
    # Import libraries
    libraries = c('data.table', 'dplyr', 'readxl', 'tidyr',
                  'ggplot2', 'gridExtra', 'plotly', 'RColorBrewer',
                  'corrplot', 'minpack.lm', 'lubridate', 'pbapply', 'Rsolnp')
    invisible(lapply(libraries, require, character.only = T))

# Define useful for MMM functions -----------------------------------------

    # Adstock function
    AdStock <- function(impressions_vector, lambda=0)
    {
      # Create empty vector for adstock
      adstock <- c()
      
      # Fill vector with values
      adstock[1] <- impressions_vector[1]
      for(i in 2:length(impressions_vector)){
        adstock[i] <- impressions_vector[i] + lambda*adstock[i-1]
      }
      
      # Return adstocked vector
      return(adstock)
    }
    
    # negative-exponential functon (concave)
    neg_exponential_form <- function(grp, alpha=0, beta, gamma, c=0){
      return(alpha + gamma*(1 - exp(-beta*grp)))
    }
    
    # Calculate R-squared for nlsLM object
    R2 <- function(nls_object, data, dep_var, adjusted=F){
      
      # Calculate basic R-squared on supplied data
      r2 <- 1 - sum(nls_object$m$resid()^2)/sum((data[[dep_var]] - mean(data[[dep_var]]))^2)
      
      # Calculate Adjusted R-squared on supplied data
      r2_adjusted <- 1 - (1 - r2)*((length(nls_object$m$predict()) - 1)/(length(nls_object$m$predict()) - length(nls_object$m$getPars())))
      
      # Return either R-squared or Adjusted R-squared
      if(adjusted == T) return(cat('R-squared adjusted: ', r2_adjusted)) else return(cat('R-squared: ', r2))
    }
    
    # Function create a dataset of useful for MMM form. A user has to supply vectors of variable names and the initial dataset
    MMM_dataset <- function(data, dep_var, ad_vars, market_vars, extra_vars){
      # Turn dataset to a data.table
      data <- as.data.table(data) %>% setorder(period)
      
      # Create a dataset with needed data
      DT <- data[, c('period', dep_var, ad_vars, setdiff(market_vars, c('baseline', 'seasonality', 'trend')), extra_vars), with=F]
      
      # Add Baseline to a dataset
      if('baseline' %in% market_vars) DT[, baseline := 1]
      
      # Add seasonality to the dataset
      if('seasonality' %in% market_vars) DT[, seasonality := decompose(ts(DT[[dep_var]], frequency = 12), 'additive')$seasonal %>% as.vector()]
      
      # Add trend to the dataset
      if('trend' %in% market_vars) DT[, trend := seq(1, dim(DT)[1])]
      
      # Return dataset for the model
      return(DT)
    }
    
    # Function creating MMM formula for the estimation
    MMM_formula <- function(dep_var, ad_vars, market_vars, extra_vars){
      # Return formuls for the case when there is no extra variables
      if(length(extra_vars) == 0){
        return(
          as.formula(paste0(dep_var, ' ~ ',
                            paste0(c(
                              paste0('AdStock(neg_exponential_form(', ad_vars,
                                     ', beta = beta_', seq(1, length(ad_vars)),
                                     ', alpha = 0',
                                     ', gamma = gamma_',  seq(1, length(ad_vars)), ')',
                                     ', lambda = lambda_', seq(1, length(ad_vars)), ')'),
                              paste0('market_', seq(1, length(market_vars)),'*', market_vars)
                            ), collapse = ' + ')
          )
          )
        )} else {
          return(
            as.formula(paste0(dep_var, ' ~ ',
                              paste0(c(
                                paste0('AdStock(neg_exponential_form(', ad_vars,
                                       ', beta = beta_', seq(1, length(ad_vars)),
                                       ', alpha = 0',
                                       ', gamma = gamma_',  seq(1, length(ad_vars)), ')',
                                       ', lambda = lambda_', seq(1, length(ad_vars)), ')'
                                ),
                                paste0('extra_', seq(1, length(extra_vars)),'*', extra_vars),
                                paste0('market_', seq(1, length(market_vars)),'*', market_vars)
                              ), collapse = ' + ')
            )
            )
          )
        }
    }

    
    # Function creating transformed series and returning new dataset which is useful for plotting the results
    MMM_transformed_series <- function(data, nls_object, dep_var, ad_vars, market_vars, extra_vars, coefficients){
      
      # Add modelled data series for advertisement variables into one dataframe
      for(var in ad_vars){
        # Extract parameters of the model
        pos <- match(var, ad_vars)
        lambda_coef <- coefficients$Estimate[match(paste0('lambda_', pos), coefficients$parameter)]
        beta_coef <- coefficients$Estimate[match(paste0('beta_', pos), coefficients$parameter)]
        gamma_coef <- coefficients$Estimate[match(paste0('gamma_', pos), coefficients$parameter)]
        
        # Get values of advertisement after application of a FuNction
        data[[paste0(var, '_FT')]] <- neg_exponential_form(data[[var]], beta = beta_coef, gamma = gamma_coef)
        
        # Get values of advertisement in AdStocked form
        data[[paste0(var, '_AS')]] <- AdStock(data[[paste0(var, '_FT')]], lambda_coef)
      }
      
      # Add modelled data series for extra variables into one dataframe
      for(var in extra_vars){
        # Extract parameters of the model
        pos <- match(var, extra_vars)
        coef <- coefficients$Estimate[match(paste0('extra_', pos), coefficients$parameter)]
        
        # Get values extra variables
        data[[paste0(var, '_AS')]] <- coef*data[[var]]
      }
      
      # Add modelled data series for market variables into one dataframe
      for(var in market_vars){
        # Extract parameters of the model
        pos <- match(var, market_vars)
        coef <- coefficients$Estimate[match(paste0('market_', pos), coefficients$parameter)]
        
        # Get values extra variables
        data[[paste0(var, '_AS')]] <- coef*data[[var]]
      }
      
      # Save model predictions to a dataframe
      data[['Fitted']] <- nls_object$m$predict()
      
      # Return the new dataset
      return(data)
    }

    
    # Function plotting decomposition of factors
    MMM_decomposition <- function(data, dep_var, market_vars){
      
      # Prepare datasets
      plot_lines <- data.table(period = as.Date(data$period), Actual = data[[dep_var]], Fitted = data[['Fitted']]) %>%
        melt.data.table(id.vars = 'period', value.name = 'data', variable.name = 'Value')
      
      plot_factors <- data %>% transform(Market = data[, paste0(market_vars, '_AS'), with=F] %>% rowSums(),
                                             period = as.Date(period)) %>%
        subset(select = c(setdiff(names(data)[grep('period|_AS', names(data))], c(paste0(market_vars, '_AS'))), 'Market')) %>%
        melt.data.table(id.vars = 'period', value.name = 'data', variable.name = 'Value')
      plot_factors[, Value := ifelse(plot_factors$Value %in% (plot_factors %>% group_by(Value) %>% summarise(sign = mean(data)) %>% as.data.table())[sign > 0]$Value,
                                     paste0('(+) ', Value), paste0('(-) ', Value))]
      
      # Set colour palettes
      cols <- colorRampPalette(brewer.pal(8, 'Set1'))(length(plot_factors$Value %>% unique()))
      cols2 <- c('yellow2', 'seagreen4', 'seagreen3', 'tan2', 'gray90', 'rosybrown', 'red3', 'red', 'blue', 'black', 'green')
      
      # Plot decompositions
      plot1 = ggplot() +
        geom_line(data = plot_lines, aes(x=period, y=data, colour=Value, linetype=Value), size=1.2) +
        scale_color_manual(values=cols) +
        ggtitle('Actual and fitted values of the model') +
        theme_classic() +
        scale_x_date(breaks = plot_lines$period[as.logical(seq(1, dim(plot_lines)[1]) %% 2 == 0)], date_labels = '%b-%Y')
      # annotate('text',
      #          x = plot_lines$period[dim(plot_lines)[1]-10],
      #          y = 1.75*min(plot_lines[Value == 'Actual']$data), label = 'italic(R) ^ 2', parse = TRUE) +
      # annotate('text',
      #          x = plot_lines$period[dim(plot_lines)[1]-9],
      #          y = 1.75*min(plot_lines[Value == 'Actual']$data), label = paste(' = ', round(r2, 2))) +
      # annotate('text',
      #          x = plot_lines$period[dim(plot_lines)[1]-11],
      #          y = 1.55*min(plot_lines[Value == 'Actual']$data), label = 'italic(R) ^ 2-adjusted', parse = TRUE) +
      # annotate('text',
      #          x = plot_lines$period[dim(plot_lines)[1]-9],
      #          y = 1.55*min(plot_lines[Value == 'Actual']$data), label = paste(' = ', round(r2_adjusted, 2)))
      
      plot2 = ggplot() +
        geom_area(data=plot_factors, aes(x=period, y=data, fill=Value), alpha = 0.75, position='stack') +
        scale_fill_manual(values=cols2) +
        geom_line(data=plot_lines, aes(x=period, y=data, colour=Value, linetype=Value), size=0.75) +
        theme_classic() +
        scale_color_manual(values=cols) +
        scale_x_date(breaks = plot_factors$period[as.logical(seq(1, dim(plot_factors)[1]) %% 2)], date_labels = '%b-%Y') +
        geom_hline(yintercept=0, linetype='dashed', color = 'black', size=0.5) +
        ggtitle('Decomposition of factors by its input into dependent variable') +
        theme(legend.position='right')
      
      return(grid.arrange(plot1, plot2))
    }
    
    
    # Function calculating impact sizes for specified number of periods from the end
    MMM_impacts <- function(data, n_periods){
      
      # Subset relevant columns for impacts dataset
      impacts <- data[(length(data$period) - n_periods + 1):length(data$period)] %>% transform(Market = data[(length(data$period) - n_periods + 1):length(data$period)][, paste0(market_vars, '_AS'), with=F] %>% rowSums()) %>%
        subset(select = c(setdiff(names(data)[grep('period|_AS', names(data))], c(paste0(market_vars, '_AS'))), 'Market'))
      
      # Calculate impact sizes
      for(var in names(impacts)[-1]){
        impacts[[paste0(var, '_IMP')]] <- sum(impacts[[var]])/(impacts[, names(impacts)[grep('_AS$|Market$', names(impacts))], with=F] %>% abs() %>% sum())
      }
      
      # Return table of impacts
      return(impacts)
    }
    
    
    # Plot impacts
    MMM_impacts_plot <- function(impacts, n_periods){
      return(ggplot(data = impacts[, grep('_IMP', names(impacts)), with=F] %>% colMeans() %>% t() %>% as.data.table() %>% transform(period = paste0('Last ', n_periods, ' periods')) %>%
               melt.data.table(id.vars = 'period', value.name = 'data', variable.name = 'Value') %>% transform(data = round(data*100, 2)),
             aes(x=period, y=data, fill=Value)) +
        geom_bar(stat = 'identity', position = 'stack') +
        geom_text(stat = 'identity', position = position_stack(vjust = .5), aes(label = data))
      )
    }

    # Function calculating response curves for BHT for all media channels
    MMM_BHT_response_curves <- function(max_investments, media_channels, media_costs, coefficients, product_price, periods){
      
      # Create a vector of investments with step of 1 mio
      response_curves <- data.table(investments = seq(0, max_investments, 10^6))
      
      # Add modelled data series for advertisement variables into one dataframe
      for(var in media_channels){
        # Extract parameters of the model
        lambda_coef <- coefficients[variable==var]$Estimate[1]
        beta_coef <- coefficients[variable==var]$Estimate[2]
        gamma_coef <- coefficients[variable==var]$Estimate[3]
        
        # Calculate expected yearly revenue (response) curve for considered media
        for(row in 1:dim(response_curves)[1]){
          response_curves[[paste0(var, '_R')]][row] <- mean(AdStock(
            neg_exponential_form(rep(response_curves[row]$investments/periods, periods)/media_costs[media==var]$cost, 
                                 beta = beta_coef, gamma = gamma_coef)
            , lambda = lambda_coef)*product_price) # calculate how much money we will get from selling products
        }
        
        # Print status
        cat('\nChannel ', var, ' is completed')
      }
      
      # Return new dataset
      return(response_curves)
    }

    # Function plotting response curves
    MMM_BHT_response_curves_plot <- function(response_curves, periods){
      
      # Plot graph
      return(ggplot(data = response_curves[, c('investments', names(response_curves)[grep('_R$', names(response_curves))]), with = F] %>%
                      melt.data.table(id.vars = 'investments', value.name = 'Value', variable.name = 'Media'),
                    aes(x=investments/10^6, y=Value, color=Media)) + 
               geom_line() +
               xlab('Yearly investments (mln rub)') +
               ylab('Average increase in BHT metrics per period (% point)') +
               theme_classic()
             )
    }
    
    # Function calculating DIRECT response curves for all media channels
    MMM_direct_response_curves <- function(max_investments, media_channels, media_costs, coefficients, product_price, periods){
      
      # Create a vector of investments with step of 1 mio
      response_curves <- data.table(investments = seq(0, max_investments, 10^6))
      
      # Add modelled data series for advertisement variables into one dataframe
      for(var in media_channels){
        # Extract parameters of the model
        lambda_coef <- coefficients[variable==var]$Estimate[1]
        beta_coef <- coefficients[variable==var]$Estimate[2]
        gamma_coef <- coefficients[variable==var]$Estimate[3]
        
        # Calculate expected yearly revenue (response) curve for considered media
        for(row in 1:dim(response_curves)[1]){
          response_curves[[paste0(var, '_R')]][row] <- sum(AdStock(
            neg_exponential_form(rep(response_curves[row]$investments/periods, periods)/media_costs[media==var]$cost, 
                                 beta = beta_coef, gamma = gamma_coef)
            , lambda = lambda_coef)) # calculate how much money we will get from selling products
        }
        
        # Calculate ROAS
        response_curves[[paste0(var, '_ROAS')]] <- response_curves[[paste0(var, '_R')]]*product_price - response_curves$investments
        
        # Print status
        cat('\nChannel ', var, ' is completed')
      }
      
      # Return new dataset
      return(response_curves)
    }
    
    # Function plotting DIRECT response curves
    MMM_direct_response_curves_plot <- function(response_curves, periods, roas=F){
      
      # Plot graph
      if(roas == T){
        return(ggplot(data = response_curves[, c('investments', names(response_curves)[grep('_ROAS$', names(response_curves))]), with = F] %>%
                        melt.data.table(id.vars = 'investments', value.name = 'Value', variable.name = 'Media'),
                      aes(x=investments/10^6, y=Value/10^6, color=Media)) + 
                 geom_line() +
                 xlab('Yearly investments (mln rub)') +
                 ylab('Expected yearly ROAS (mln rub)') +
                 theme_classic()
        )
      } else {
        return(ggplot(data = response_curves[, c('investments', names(response_curves)[grep('_R$', names(response_curves))]), with = F] %>%
                        melt.data.table(id.vars = 'investments', value.name = 'Value', variable.name = 'Media'),
                      aes(x=investments/10^6, y=Value, color=Media)) + 
                 geom_line() +
                 xlab('Yearly investments (mln rub)') +
                 ylab('Expected yearly response (units)') +
                 theme_classic()
        )
      }
    }
    
    # Function calculating TOTAL response curves for all media channels
    MMM_total_response_curves <- function(max_investments, media_channels, media_costs, coefficients, product_price, periods){
      
      # Create a vector of investments with step of 1 mio
      response_curves <- data.table(investments = seq(0, max_investments, 10^6))
      
      # Add modelled data series for advertisement variables into one dataframe
      for(var in media_channels){
        
        # Get list of models where the media was used
        models <- coefficients[variable == var]$model %>% unique()
        
        # For every model calculate the impact for every media on applications
        for(m in models){
          # Extract parameters of the model
          lambda_coef <- coefficients[variable==var & model == m]$Estimate[1]
          beta_coef <- coefficients[variable==var & model == m]$Estimate[2]
          gamma_coef <- coefficients[variable==var & model == m]$Estimate[3]
          fin_coef <- coefficients[variable==var & model == m]$final_coef[1]
        
          # Calculate expected yearly revenue (response) curve for considered media
          for(row in 1:dim(response_curves)[1]){
            response_curves[[paste0(var, '_', m, '_R')]][row] <- sum(AdStock(
              neg_exponential_form(rep(response_curves[row]$investments/periods, periods)/media_costs[media==var]$cost, 
                                   beta = beta_coef, gamma = gamma_coef)
              , lambda = lambda_coef)*fin_coef) # calculate how much money we will get from selling products
          }
        }
        
        # Create total response curve
        response_curves[[paste0(var, '_TOTAL_R')]] <- (response_curves[, names(response_curves)[grep(paste0(var, '_*.*_R'), names(response_curves))], with=F] %>% rowSums())
        
        # Calculate ROAS
        response_curves[[paste0(var, '_ROAS')]] <- response_curves[[paste0(var, '_TOTAL_R')]]*product_price - response_curves$investments
        
        # Print status
        cat('\nChannel ', var, ' is completed')
      }
      
      # Return new dataset
      return(response_curves)
    }
    
    # Function plotting TOTAL response curves
    MMM_total_response_curves_plot <- function(response_curves, weekly=F, roas=F){
      # Set number of periods
      if(weekly == T){periods = 52} else {periods = 12}
      
      # Plot graph
      if(roas == T){
        return(ggplot(data = response_curves[, c('investments', names(response_curves)[grep('_ROAS$', names(response_curves))]), with = F] %>%
                        melt.data.table(id.vars = 'investments', value.name = 'Value', variable.name = 'Media'),
                      aes(x=investments/10^6, y=Value/10^6, color=Media)) + 
                 geom_line() +
                 xlab('Yearly investments (mln rub)') +
                 ylab('Expected yearly ROAS (mln rub)') +
                 theme_classic()
        )
      } else {
        return(ggplot(data = response_curves[, c('investments', names(response_curves)[grep('_TOTAL_R$', names(response_curves))]), with = F] %>%
                        melt.data.table(id.vars = 'investments', value.name = 'Value', variable.name = 'Media'),
                      aes(x=investments/10^6, y=Value, color=Media)) + 
                 geom_line() +
                 xlab('Yearly investments (mln rub)') +
                 ylab('Expected yearly response (units)') +
                 theme_classic()
        )
      }
    }
    
    # Get coefficients of revenue function for every channel 
    MMM_revenue_coefficients <- function(response_curves){
      # Extract Revenues
      ROAS <- cbind(response_curves[, 'investments'],
                    response_curves[, names(response_curves)[grep('_R$', names(response_curves))], with=F] + response_curves$investments)
      
      # Create a table of coefficients allowing to fit the data
      lapply(names(ROAS[, -'investments']), function(channel){
        # Fit the model
        model <- nlsLM(formula = as.formula('ROAS[[channel]] ~ neg_exponential_form(grp = investments, beta = beta_1, gamma = gamma_1)'),
                       data = ROAS,
                       start = list(beta_1 = 0.001, gamma_1 = mean(ROAS[[channel]])),
                       lower = c(-Inf, -Inf),
                       upper = c(Inf, Inf),
                       control = list(maxiter = 800))
        
        # Return the coefficients
        return(data.table(media = gsub('', '', channel), beta = model$m$getPars()[1], gamma = model$m$getPars()[2]))
      }) %>% rbindlist(fill=T)
    }
    
    
    
    resp_poly <- function(response_curves, channel, value){
      response_curves<-data.frame(response_curves)
      a <- response_curves[,channel]
      x <- response_curves[,'investments']
      new <- data.frame(x = seq(0,3990000,10000))
      model <- lm(a ~ poly(x, 10))
      
      prd<-predict(model, data.frame(x=value))
      #matplot(new$x, prd$fit,lty = c(1,2,2,3,3), type = "l", ylab = "predicted y")
      return(prd)
    }
    
    predict_poly <- function(model_dict, channel, value){
      prd<-predict(model_dict[[channel]], data.frame(x=value))
      return(prd)
    }
    
    # Derive combinations of optimal budget distribution across channels
    optimize_budget <- function(param, budget, coefficients, response_curves){
      
      # Calculate revenues for all channels -> sum it up
      revenue <- sapply(seq(1, length(param)), function(ID){
        # Calculate revenue for the channel
        #revenue = neg_exponential_form(budget * param[ID], beta=coefficients$beta[ID], gamma=coefficients$gamma[ID])
        #print(1)
        revenue = predict_poly(response_curves, coefficients$media[ID], budget * param[ID])
        #print(2)
      }) %>% sum()
      #print(3)
      #print(revenue)
      # Set optimisation function
      return(-(revenue))
    }
    
    
    # Function optimizing budgets for channels for given media budget
    MMM_optimize_budget_split <- function(max_investments, revenue_coef, response_curves){
      # Calculate optimal split for all budget sizes
      splits <- pblapply(seq(0, max_investments, 10^5), function(budg, coef, response_curves1){
        # Set optimisation function
        optimum <- solnp(
          pars = rep(1/length(coef$media), length(coef$media)),
          fun = optimize_budget,
          eqfun = function(x, budget, coefficients, response_curves){sum(x)}, # sum of weights
          eqB = 1, # sum of weights equals 1
          LB = rep(0, length(coef$media)),
          UB = rep(1, length(coef$media)),
          budget = budg,
          coefficients = coef,
          response_curves = response_curves1,
          control = list(outer.iter = 5, inner.iter=10)
        )
        
        # Create a table to return
        optimization_results <- data.table(budget = budg, profit = -optimum$values[length(optimum$values)])
        for(i in 1:length(optimum$pars)){
          optimization_results[[coef$media[i]]] <- budg*optimum$par[i]
        }
        
        # Return data table with optimisation results
        return(optimization_results)
      }, coef = revenue_coef, response_curves1=response_curves) %>% rbindlist(fill=T)
      
      # Return optimised splits
      return(splits)
    }
    
    MMM_plot_optimal_splits <- function(splits_table, revenue_coef){
      # Create a table of results for the plot
      optimal_budg <- (splits_table[, -'profit'] %>% melt.data.table(id.vars = 'budget', value.name = 'data', variable.name = 'Value'))[
        , c('budget', 'data', 'Value') := list(budget/10^6, data/budget*100, factor(Value, levels = revenue_coef$media))]
      
      # Plot the graph
      plot_opt_budget_split <- ggplot() +
        geom_area(data=optimal_budg,
                  aes(x=budget, y=data, fill=Value), alpha = 0.6, position='stack') +
        scale_y_continuous(breaks = seq(0, 100, 5)) +
        scale_x_continuous(breaks = seq(0, max(optimal_budg$budget), max(optimal_budg$budget)/50)) +
        theme_classic() +
        ggtitle('Optimal splits for different levels of total media budget') +
        xlab('Total media budget (mln rub)') +
        ylab('Optimal split by media (%)') +
        theme(legend.position='right')
      
      # Return the graph
      return(plot_opt_budget_split)
    }

    # Plot optimal ROAS curve
    MMM_plot_optimal_ROAS <- function(optimised_budget){
      ggplot(data=optimised_budget, aes(x=budget/10^6, y=profit/10^6)) +
        geom_line(color='blue') +
        geom_hline(yintercept = 0, color = 'black') +
        xlab('Total budget for all media in the model (mln rub)') +
        ylab('ROAS from investments (mln rub)') +
        theme_classic()
    }
    
    
    
    ############
    
    # Plot budget decomposition
    # budgets <- data.table(
    #   'Media' = factor(c('TV', 'DIG', 'OOH'), levels=c('TV', 'DIG', 'OOH'), ordered=T),
    #   'Actual budget' = c(round(sum(model_dt[Date > '2017-03-01'][['KS_TV']])*average_cost[media=='KS_TV']$cost/10^6),
    #                       round(sum(model_dt[Date > '2017-03-01'][['KS_DIG']])*average_cost[media=='KS_DIG']$cost/10^6),
    #                       round(sum(model_dt[Date > '2017-03-01'][['KS_OOH']])*average_cost[media=='KS_OOH']$cost/10^6)),
    #   'Optimised budget' = optimised_budget[budget_total == sum(
    #     c(round(sum(model_dt[Date > '2017-03-01'][['KS_TV']])*average_cost[media=='KS_TV']$cost/10^6),
    #       round(sum(model_dt[Date > '2017-03-01'][['KS_DIG']])*average_cost[media=='KS_DIG']$cost/10^6),
    #       round(sum(model_dt[Date > '2017-03-01'][['KS_OOH']])*average_cost[media=='KS_OOH']$cost/10^6))
    #   )][, c('budget_TV', 'budget_DIG', 'budget_OOH'), with=F] %>% round() %>% as.numeric(),
    #   'Budget (max profit)' = optimised_budget[profit == max(profit)][, c('budget_TV', 'budget_DIG', 'budget_OOH'), with=F] %>% round() %>% as.numeric(),
    #   'Budget (break-even)' = optimised_budget[abs(profit) == min(abs(profit))][, c('budget_TV', 'budget_DIG', 'budget_OOH'), with=F] %>% round() %>% as.numeric()
    # )  %>% setcolorder(rev(colnames(.))) %>% melt.data.table(id.vars = 'Media', value.name = 'Value', variable.name = 'Type')
    # 
    # budgets[, c('Share', 'Total') := list(
    #   round((lapply(budgets$Type %>% unique(), function(x){
    #     values <- budgets[Type == x]$Value
    #     return(values/sum(values))
    #   }) %>% unlist())*100, 1),
    #   (budgets %>% group_by(Type) %>% summarise(Total = round(sum(Value))) %>% data.table())$Total %>% rep(each=3)
    # )
    # ][, Label := paste0(Share, '% \n (', Value, ' mln rub)')]
    # 
    # budgets[, Position := (lapply(budgets$Type %>% unique(), function(x){
    #   shares <- budgets[Type == x]$Share
    #   return(shares/2 + c(0, cumsum(shares)[-3]))
    # }) %>% unlist())
    # ]
    # 
    # plot_budgets <- ggplot(budgets, aes(x=Type, y=Share, fill=Media)) +
    #   scale_fill_manual(values=c('red', 'blue', 'grey40')) +
    #   geom_bar(width = 1, stat = 'identity', alpha=0.6, color='white', size=0.25, position = position_stack(reverse = T)) +
    #   coord_flip() +
    #   theme_minimal() +
    #   theme(axis.title.x = element_blank(),
    #         axis.title.y = element_blank(),
    #         panel.border = element_blank(),
    #         panel.grid=element_blank(),
    #         axis.ticks = element_blank(),
    #         legend.title = element_blank(),
    #         plot.title=element_text(color = 'grey', size = 14),
    #         legend.position = 'bottom') +
    #   geom_text(aes(y = Position, label = Label), size=3, color='white') +
    #   geom_text(aes(y = rep(107, 12), label = paste0('Total: \n', Total, ' mln rub')), size=3, color='black')
    # 
    
    
    
    
    
    
