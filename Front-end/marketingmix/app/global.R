# Attach libraries

# libraries = c('data.table', 'dplyr', 'ggplot2', 'gridExtra', 'plotly', 'shiny', 'shinyWidgets', 'plyr')
# invisible(lapply(libraries, require, character.only = T))
# rm(list = ls())
library('data.table')
library('ggplot2')
library('gridExtra')
library('plotly')
library('shiny')
library('shinyWidgets')
library('plyr')
library('dplyr')
library("minpack.lm")
library('readxl')
library('corrplot')
library('lubridate')
library('pbapply')
library('Rsolnp')

options(scipen=999) # prevent scientific notation
source('./Additional scripts/MMM_negative_exponential_1.R')

formatter1000 <- function(){
  function(x)x/1000
}
process_data <- function() {
  hide("main_content")
  show("secondary_content")
}
load_data <- function() {
  # show("secondary_content")
  
  Sys.sleep(2)
  hide("secondary_content")
  show("main_content")
}

process_data_sep <- function() {
  hide("main_content_sep")
  show("secondary_content_sep")
}
load_data_sep <- function() {
  # show("secondary_content")
  
  Sys.sleep(2)
  hide("secondary_content_sep")
  show("main_content_sep")
}

process_data_2 <- function() {
  show("secondary_content_2")
  hide("main_content_2")
}
load_data_2 <- function() {
  # show("secondary_content")
  
  Sys.sleep(2)
  hide("secondary_content_2")
  show("main_content_2")
}

process_data_2_sep <- function() {
  show("secondary_content_2_sep")
  hide("main_content_2_sep")
}
load_data_2_sep <- function() {
  # show("secondary_content")
  
  Sys.sleep(2)
  hide("secondary_content_2_sep")
  show("main_content_2_sep")
}

reverse_legend_labels <- function(plotly_plot) {
  n_labels <- length(plotly_plot$x$data)
  plotly_plot$x$data[1:n_labels] <- plotly_plot$x$data[n_labels:1]
  plotly_plot
}

ColdBarsColour <- c("#3c8dbc", "#77add1") 
names(ColdBarsColour) <- c("Jan-Aug 2019", "Jan-Aug 2020")
# Set variables names for visualisation -----------------------------------

re_vars_BRAND_old <- c('Date', 'actual', 'fitted', 'Audio_TOTAL_Clicks', 'DD_TOTAL_Impressions',
                       'OLV_TOTAL_Clicks', 'OTT_TOTAL_Impressions', 'PDM_TOTAL_Impressions',
                       'POLV_TOTAL_Clicks', 'Partnership_TOTAL_Impressions',
                       'YT_DD_TOTAL_2019_Clicks', 'YT_DD_TOTAL_2020_Clicks',
                       'YT_OLV_TOTAL_Impressions', 'Primary_Search_Impressions',
                       'Secondary_Search_Impressions', 'DOOH_TOTAL_Impressions',
                       'Social_TOTAL_Impressions', 'Facebook_Impressions',
                       'ALL_Mobile_CLICKED', 'NASDAQ_Opens', 'Business_Uncertainty',
                       'Unemployed', 'Samsung_Relative_Price', 'Category_Sellout',
                       'Black_friday', 'Christmas', 'New_launch_1', 'New_launch_2',
                       'New_launch_3', 'New_launch_4', 'New_launch_5', 'New_launch_6',
                       'New_launch_SG', 'Valentins_day_neg', 'Valentins_day_pos', 'New_Year',
                       'Birthday_of_Martin_Luther_King', 'Washington_Birthday', 'Memorial_Day',
                       'Independence_Day', 'Labor_Day', 'Columbus_Day', 'Veterans_Day',
                       'Thanksgiving_Day', 'Christmas_Day', 'Covid', 'past_covid_weekday_0',
                       'past_covid_weekday_1', 'past_covid_weekday_2', 'past_covid_weekday_3',
                       'past_covid_weekday_4', 'pre_covid_weekday_0', 'pre_covid_weekday_1',
                       'pre_covid_weekday_2', 'pre_covid_weekday_3', 'pre_covid_weekday_4',
                       'Month_1', 'Month_2', 'Month_3', 'Month_4', 'Month_5', 'Month_7',
                       'Month_8', 'Month_9', 'Month_10', 'Month_11', 'Month_12', 'baseline') 

re_vars_BRAND_new <- c('Date', 'actual', 'fitted', 'Audio_TOTAL_Clicks', 'DD_TOTAL_Impressions',
                       'OLV_TOTAL_Clicks', 'OTT_TOTAL_Impressions', 'PDM_TOTAL_Impressions',
                       'POLV_TOTAL_Clicks', 'Partnership_TOTAL_Impressions',
                       'YT_DD_TOTAL_2019_Clicks', 'YT_DD_TOTAL_2020_Clicks',
                       'YT_OLV_TOTAL_Impressions', 'Primary_Search_Impressions',
                       'Secondary_Search_Impressions', 'DOOH_TOTAL_Impressions',
                       'Social_TOTAL_Impressions', 'Facebook_Impressions',
                       'ALL_Mobile_CLICKED', 'NASDAQ_Opens', 'Business_Uncertainty',
                       'Unemployed', 'Samsung_Relative_Price', 'Category_Sellout',
                       'Black_friday', 'Christmas', 'New_launch_1', 'New_launch_2',
                       'New_launch_3', 'New_launch_4', 'New_launch_5', 'New_launch_6',
                       'New_launch_SG', 'Valentins_day_neg', 'Valentins_day_pos', 'New_Year',
                       'Birthday_of_Martin_Luther_King', 'Washington_Birthday', 'Memorial_Day',
                       'Independence_Day', 'Labor_Day', 'Columbus_Day', 'Veterans_Day',
                       'Thanksgiving_Day', 'Christmas_Day', 'Covid', 'past_covid_weekday_0',
                       'past_covid_weekday_1', 'past_covid_weekday_2', 'past_covid_weekday_3',
                       'past_covid_weekday_4', 'pre_covid_weekday_0', 'pre_covid_weekday_1',
                       'pre_covid_weekday_2', 'pre_covid_weekday_3', 'pre_covid_weekday_4',
                       'Month_1', 'Month_2', 'Month_3', 'Month_4', 'Month_5', 'Month_7',
                       'Month_8', 'Month_9', 'Month_10', 'Month_11', 'Month_12', 'baseline')

media_vars <- c('Audio_TOTAL_Clicks', 'DD_TOTAL_Impressions',
                'OLV_TOTAL_Clicks', 'OTT_TOTAL_Impressions', 'PDM_TOTAL_Impressions',
                'POLV_TOTAL_Clicks', 'Partnership_TOTAL_Impressions',
                'YT_DD_TOTAL_Clicks',
                'YT_OLV_TOTAL_Impressions', 'Primary_Search_Impressions',
                'Secondary_Search_Impressions', 'DOOH_TOTAL_Impressions',
                'Social_TOTAL_Impressions', 'Facebook_Impressions')

media_vars_names <- c('Audio', 'DD',
                      'OLV', 'OTT', 'PDM',
                      'POLV', 'Partnership',
                      'YT_DD',
                      'YT_OLV', 'Primary_Search',
                      'Secondary_Search', 'Digital OOH',
                      'Social', 'Facebook', 'Budget')


media_vars_names_sep <- c('Audio_cold',
                          'YouTube_DD_cold',
                          'YouTube_DD_warm',
                          'YouTube_OLV_cold',
                          'YouTube_OLV_warm',
                          'OTT_cold',
                          'OTT_warm',
                          'PDM_cold',
                          'PDM_hot',
                          'PDM_warm',
                          'POLV_cold',
                          'POLV_hot',
                          'POLV_warm',
                          'OOH_warm',
                          'Partnership_cold',
                          'Partnership_warm',
                          'Primary_Search_hot',
                          'Secondary_Search_hot',
                          'Facebook_cold',
                          'Facebook_hot',
                          'Facebook_warm',
                          'Social_cold',
                          'Social_warm',
                          'Social_hot',
                          'DD_cold',
                          'DD_hot',
                          'DD_warm',
                          'OLV_cold',
                          'OLV_warm')


media_vars_names_cold <- c('Audio', 'DD',
                           'OLV', 'OTT', 'PDM',
                           'POLV', 'Partnership',
                           'YT_DD',
                           'YT_OLV',
                           'Social', 'Facebook', 'Budget')
media_vars_names_pl_cold <- c('Audio', 'Direct Display',
                              'OLV', 'OTT', 'Prog Display',
                              'Prog OLV', 'Partnership',
                              'Youtube DD',
                              'Youtube OLV', 
                              'Other Social', 'Facebook', 'Budget')

media_vars_names_warm <- c('DD',
                           'OLV', 'OTT', 'PDM',
                           'POLV', 'Partnership',
                           'YT_DD',
                           'YT_OLV',
                           'Digital OOH',
                           'Social', 'Facebook', 'Budget')
media_vars_names_pl_warm <- c('Direct Display',
                              'OLV', 'OTT', 'Prog Display',
                              'Prog OLV', 'Partnership',
                              'Youtube DD',
                              'Youtube OLV', 'Digital OOH',
                              'Other Social', 'Facebook', 'Budget')

media_vars_names_hot <- c('PDM',
                          'POLV',
                          'YT_DD',
                          'Primary_Search',
                          'Secondary_Search',
                          'Social', 'Facebook', 'Budget')
media_vars_names_pl_hot <- c('Prog Display',
                             'Prog OLV',
                             'Youtube DD', 'Primary Search',
                             'Secondary Search',
                             'Other Social', 'Facebook', 'Budget')



media_vars_names_pl <- c('Audio', 'Direct Display',
                         'OLV', 'OTT', 'Prog Display',
                         'Prog OLV', 'Partnership',
                         'Youtube DD',
                         'Youtube OLV', 'Primary Search',
                         'Secondary Search', 'Digital OOH',
                         'Other Social', 'Facebook', 'Budget')



# Import data ---------------------------------------------



# Import response curves
re_response_BRAND <- fread(paste0('./inputs/inputs/', list.files('./inputs/inputs/', pattern='s_profits.csv')))[, c(media_vars_names) := list(Audio_TOTAL_Clicks, DD_TOTAL_Impressions, OLV_TOTAL_Clicks, OTT_TOTAL_Impressions, PDM_TOTAL_Impressions, POLV_TOTAL_Clicks, Partnership_TOTAL_Impressions, YT_DD_TOTAL_Clicks, YT_OLV_TOTAL_Impressions, Primary_Search_Impressions, Secondary_Search_Impressions, DOOH_TOTAL_Impressions, Social_TOTAL_Impressions, Facebook_Impressions, round(Investment))]
re_response_BRAND$Budget=re_response_BRAND$Budget/1000000



re_response_BRAND_cold <- fread(paste0('./inputs/inputs/', list.files('./inputs/inputs/', pattern='s_profits_cold.csv')))[, c(media_vars_names_cold) := list(Audio_Clicks, DD_Impressions, OLV_Clicks, OTT_Impressions, PDM_Impressions, POLV_Clicks, Partnership_Impressions, YouTube_DD_Clicks, YouTube_OLV_Impressions, Social_Impressions, Facebook_Impressions, round(Investment))]
re_response_BRAND_cold$Budget=re_response_BRAND_cold$Budget/1000000



re_response_BRAND_warm <- fread(paste0('./inputs/inputs/', list.files('./inputs/inputs/', pattern='s_profits_warm.csv')))[, c(media_vars_names_warm) := list(DD_Impressions, OLV_Clicks, OTT_Impressions, PDM_Impressions, POLV_Clicks, Partnership_Impressions, YouTube_DD_Clicks, YouTube_OLV_Impressions, OOH_Impressions, Social_Impressions, Facebook_Impressions, round(Investment))]
re_response_BRAND_warm$Budget=re_response_BRAND_warm$Budget/1000000



re_response_BRAND_hot <- fread(paste0('./inputs/inputs/', list.files('./inputs/inputs/', pattern='s_profits_hot.csv')))[, c(media_vars_names_hot) := list(PDM_Impressions, POLV_Clicks, YouTube_DD_Clicks, Primary_Search_Impressions, Secondary_Search_Impressions, Social_Impressions, Facebook_Impressions, round(Investment))]
re_response_BRAND_hot$Budget=re_response_BRAND_hot$Budget/1000000



# Import ROAS curves
pr_response_BRAND <- fread(paste0('./inputs/inputs/', list.files('./inputs/inputs/', pattern='s_profits.csv')))[, c(media_vars_names) := list(Audio_TOTAL_Clicks_ROAS, DD_TOTAL_Impressions_ROAS, OLV_TOTAL_Clicks_ROAS, OTT_TOTAL_Impressions_ROAS, PDM_TOTAL_Impressions_ROAS, POLV_TOTAL_Clicks_ROAS, Partnership_TOTAL_Impressions_ROAS, YT_DD_TOTAL_Clicks_ROAS, YT_OLV_TOTAL_Impressions_ROAS, Primary_Search_Impressions_ROAS, Secondary_Search_Impressions_ROAS, DOOH_TOTAL_Impressions_ROAS, Social_TOTAL_Impressions_ROAS, Facebook_Impressions_ROAS, round(Investment))]
pr_response_BRAND$Budget=pr_response_BRAND$Budget/1000000

# Import decomposition data
re_decomp_BRAND <- fread(paste0('./inputs/inputs/', list.files('./inputs/inputs/', pattern='s_predictions.csv'))) 
re_decomp_BRAND$Date <- as.Date(re_decomp_BRAND$Date, format = "%d.%m.%Y")

# Import dataset with optimised budgets
ad_barchart_BRAND <- fread(paste0('./inputs/inputs/', list.files('./inputs/inputs/', pattern='s_budgets.csv')))[
  , Media := factor(rep(c('TV_hienz_grp', 'TV_sponsor_hienz_grp', 'IMP_display_hienz_budget', 'IMP_olv_hienz_budget', 'IMP_agents_budget', 'IMP_social_hienz_budget'), 3), levels=c('TV_hienz_grp', 'TV_sponsor_hienz_grp', 'IMP_display_hienz_budget', 'IMP_olv_hienz_budget', 'IMP_agents_budget', 'IMP_social_hienz_budget'), ordered=T)
]

# Import dataset with optimal splits
ad_optimal_splits_BRAND <- fread(paste0('./inputs/inputs/', list.files('./inputs/inputs/', pattern='s_optimal_splits.csv')))[
  , Value := factor(Value, levels = media_vars_names)
]

ad_optimal_splits_BRAND_sep <- fread(paste0('./inputs/inputs/', list.files('./inputs/inputs/', pattern='s_optimal_splits_sep.csv')))[
  , Value := factor(Value, levels = media_vars_names_sep)
]


ad_optimal_splits_BRAND_zzzz <- fread(paste0('./inputs/inputs/', list.files('./inputs/inputs/', pattern='s_optimal_splits.csv')))
# Import dataset with optimal ROAS
ad_optimal_ROAS <- fread(paste0('./inputs/inputs/', list.files('./inputs/inputs/', pattern='s_optimised_budget.csv')))

# Prepare datasets for predictions and decomposition plots (do not change the order of code blocks) ----------------------------------------------

# Actual and fitted data
re_lines_BRAND <- re_decomp_BRAND[, c('Date', 'actual', 'fitted')]  %>% melt.data.table(id.vars = 'Date', value.name = 'data', variable.name = 'Value')

# Decomposition plots
re_decomp_BRAND <- re_decomp_BRAND[, -c('actual', 'fitted'), with=F] %>% melt.data.table(id.vars = 'Date', value.name = 'data', variable.name = 'Value')
# re_decomp_BRAND[Value %in% c('Digital, competitors')]$data <- re_decomp_BRAND[Value %in% c('Digital, competitors')]$data - 0.001 

ad_optimal_ROAS$profit2 = ad_optimal_ROAS$profit/10
pr_response_BRAND2 = pr_response_BRAND[, c('Audio', 'DD','OLV', 'OTT', 'PDM','POLV', 'Partnership','YT_DD','YT_OLV', 'Primary_Search','Secondary_Search', 'Digital OOH','Social', 'Facebook', 'Budget')]

re_response_BRAND2 = re_response_BRAND[, c('Audio', 'DD','OLV', 'OTT', 'PDM','POLV', 'Partnership','YT_DD','YT_OLV', 'Primary_Search','Secondary_Search', 'Digital OOH','Social', 'Facebook', 'Budget')]
re_response_BRAND2_cold = re_response_BRAND_cold[, c('Audio', 'DD','OLV', 'OTT', 'PDM','POLV', 'Partnership','YT_DD','YT_OLV', 'Social', 'Facebook', 'Budget')]
re_response_BRAND2_warm = re_response_BRAND_warm[, c('DD','OLV', 'OTT', 'PDM','POLV', 'Partnership','YT_DD','YT_OLV', 'Digital OOH','Social', 'Facebook', 'Budget')]
re_response_BRAND2_hot = re_response_BRAND_hot[, c('PDM','POLV','YT_DD', 'Primary_Search','Secondary_Search', 'Social', 'Facebook', 'Budget')]


re_response_BRAND2_plot <- re_response_BRAND2
names(re_response_BRAND2_plot)[names(re_response_BRAND2_plot) == "Social"] <- "Other Social"
names(re_response_BRAND2_plot)[names(re_response_BRAND2_plot) == "DD"] <- "Direct Display"
names(re_response_BRAND2_plot)[names(re_response_BRAND2_plot) == "PDM"] <- "Prog Display"
names(re_response_BRAND2_plot)[names(re_response_BRAND2_plot) == "Secondary_Search"] <- "Secondary Search"
names(re_response_BRAND2_plot)[names(re_response_BRAND2_plot) == "Primary_Search"] <- "Primary Search"
names(re_response_BRAND2_plot)[names(re_response_BRAND2_plot) == "YT_OLV"] <- "Youtube OLV"
names(re_response_BRAND2_plot)[names(re_response_BRAND2_plot) == "YT_DD"] <- "Youtube DD"
names(re_response_BRAND2_plot)[names(re_response_BRAND2_plot) == "POLV"] <- "Prog OLV"
# names(re_response_BRAND2_plot)[names(re_response_BRAND2_plot) == "DOOH"] <- "Digital OOH"

re_response_BRAND2_plot_cold <- re_response_BRAND2_cold
names(re_response_BRAND2_plot_cold)[names(re_response_BRAND2_plot_cold) == "Social"] <- "Other Social"
names(re_response_BRAND2_plot_cold)[names(re_response_BRAND2_plot_cold) == "DD"] <- "Direct Display"
names(re_response_BRAND2_plot_cold)[names(re_response_BRAND2_plot_cold) == "PDM"] <- "Prog Display"
names(re_response_BRAND2_plot_cold)[names(re_response_BRAND2_plot_cold) == "Secondary_Search"] <- "Secondary Search"
names(re_response_BRAND2_plot_cold)[names(re_response_BRAND2_plot_cold) == "Primary_Search"] <- "Primary Search"
names(re_response_BRAND2_plot_cold)[names(re_response_BRAND2_plot_cold) == "YT_OLV"] <- "Youtube OLV"
names(re_response_BRAND2_plot_cold)[names(re_response_BRAND2_plot_cold) == "YT_DD"] <- "Youtube DD"
names(re_response_BRAND2_plot_cold)[names(re_response_BRAND2_plot_cold) == "POLV"] <- "Prog OLV"

re_response_BRAND2_plot_warm <- re_response_BRAND2_warm
names(re_response_BRAND2_plot_warm)[names(re_response_BRAND2_plot_warm) == "Social"] <- "Other Social"
names(re_response_BRAND2_plot_warm)[names(re_response_BRAND2_plot_warm) == "DD"] <- "Direct Display"
names(re_response_BRAND2_plot_warm)[names(re_response_BRAND2_plot_warm) == "PDM"] <- "Prog Display"
names(re_response_BRAND2_plot_warm)[names(re_response_BRAND2_plot_warm) == "Secondary_Search"] <- "Secondary Search"
names(re_response_BRAND2_plot_warm)[names(re_response_BRAND2_plot_warm) == "Primary_Search"] <- "Primary Search"
names(re_response_BRAND2_plot_warm)[names(re_response_BRAND2_plot_warm) == "YT_OLV"] <- "Youtube OLV"
names(re_response_BRAND2_plot_warm)[names(re_response_BRAND2_plot_warm) == "YT_DD"] <- "Youtube DD"
names(re_response_BRAND2_plot_warm)[names(re_response_BRAND2_plot_warm) == "POLV"] <- "Prog OLV"

re_response_BRAND2_plot_hot <- re_response_BRAND2_hot
names(re_response_BRAND2_plot_hot)[names(re_response_BRAND2_plot_hot) == "Social"] <- "Other Social"
names(re_response_BRAND2_plot_hot)[names(re_response_BRAND2_plot_hot) == "DD"] <- "Direct Display"
names(re_response_BRAND2_plot_hot)[names(re_response_BRAND2_plot_hot) == "PDM"] <- "Prog Display"
names(re_response_BRAND2_plot_hot)[names(re_response_BRAND2_plot_hot) == "Secondary_Search"] <- "Secondary Search"
names(re_response_BRAND2_plot_hot)[names(re_response_BRAND2_plot_hot) == "Primary_Search"] <- "Primary Search"
names(re_response_BRAND2_plot_hot)[names(re_response_BRAND2_plot_hot) == "YT_OLV"] <- "Youtube OLV"
names(re_response_BRAND2_plot_hot)[names(re_response_BRAND2_plot_hot) == "YT_DD"] <- "Youtube DD"
names(re_response_BRAND2_plot_hot)[names(re_response_BRAND2_plot_hot) == "POLV"] <- "Prog OLV"


ad_optimal_ROAS$budget_total=as.numeric((ad_optimal_ROAS$budget_total))
ad_optimal_ROAS$profit=as.numeric((ad_optimal_ROAS$profit))

ad_optimal_splits_BRAND$data <- ad_optimal_splits_BRAND$data/ad_optimal_splits_BRAND$budget_total*100
ad_optimal_splits_BRAND$budget_total=as.numeric((ad_optimal_splits_BRAND$budget_total))
ad_optimal_splits_BRAND$data=as.numeric((ad_optimal_splits_BRAND$data))


ad_optimal_splits_BRAND_sep$data <- ad_optimal_splits_BRAND_sep$data/ad_optimal_splits_BRAND_sep$budget_total*100
ad_optimal_splits_BRAND_sep$budget_total=as.numeric((ad_optimal_splits_BRAND_sep$budget_total))
ad_optimal_splits_BRAND_sep$data=as.numeric((ad_optimal_splits_BRAND_sep$data))

ad_optimal_splits_BRAND_initial <- ad_optimal_splits_BRAND
ad_optimal_splits_BRAND_initial$Value <- revalue(ad_optimal_splits_BRAND_initial$Value, c("DD"="Direct Display", "PDM" = "Prog Display", "Prog OLV",
                                                                                          "YT_DD" = "Youtube DD", "YT_OLV" = "Youtube OLV",
                                                                                          "Primary_Search" = "Primary Search", "Secondary_Search" = "Secondary Search",
                                                                                          "Social" = "Other Social", "DOOH" = "Digital OOH"))


get_optbug_spot <- function(df, budget){
  return(df[Budget == budget])
}
#opt_spot <- get_optbug_spot(pr_response_BRAND2, 0.07)



opt_spot <- fread(paste0('./inputs/inputs/', list.files('./inputs/inputs/', pattern='historical_roas.csv')))
opt_spot_1 <- opt_spot[1]
opt_spot_bar <- opt_spot
opt_spot_bar$Budget = as.factor(opt_spot$Budget)
names(opt_spot_bar)[names(opt_spot_bar) == "Other Social Social"] <- "Other Social"
names(opt_spot_bar)[names(opt_spot_bar) == "DD"] <- "Direct Display"
names(opt_spot_bar)[names(opt_spot_bar) == "PDM"] <- "Prog Display"
names(opt_spot_bar)[names(opt_spot_bar) == "Secondary_search"] <- "Secondary Search"
names(opt_spot_bar)[names(opt_spot_bar) == "Primary_search"] <- "Primary Search"
names(opt_spot_bar)[names(opt_spot_bar) == "YT_OLV"] <- "Youtube OLV"
names(opt_spot_bar)[names(opt_spot_bar) == "YT_DD"] <- "Youtube DD"
names(opt_spot_bar)[names(opt_spot_bar) == "POLV"] <- "Prog OLV"
names(opt_spot_bar)[names(opt_spot_bar) == "DOOH"] <- "Digital OOH"
opt_spot_bar = opt_spot_bar %>% 
  # rename_at(vars(names$wrong), ~ names$right) %>%
  melt.data.table(id.vars = "Budget", value.name = "Value", variable.name = "Media")
levels(opt_spot_bar$Media) <- gsub(" ", "\n", levels(opt_spot_bar$Media))

opt_spot$Budget = as.factor(opt_spot$Budget)

opt_spot_cold <- fread(paste0('./inputs/inputs/', list.files('./inputs/inputs/', pattern='ROAS_cold.csv')))
opt_spot_cold_1 <- opt_spot_cold[1]
opt_spot_cold_bar <- opt_spot_cold
opt_spot_cold_bar$Budget = as.factor(opt_spot_cold$Budget)
names(opt_spot_cold_bar)[names(opt_spot_cold_bar) == "Other Social Social"] <- "Other Social"
names(opt_spot_cold_bar)[names(opt_spot_cold_bar) == "DD"] <- "Direct Display"
names(opt_spot_cold_bar)[names(opt_spot_cold_bar) == "PDM"] <- "Prog Display"
names(opt_spot_cold_bar)[names(opt_spot_cold_bar) == "Secondary_search"] <- "Secondary Search"
names(opt_spot_cold_bar)[names(opt_spot_cold_bar) == "Primary_search"] <- "Primary Search"
names(opt_spot_cold_bar)[names(opt_spot_cold_bar) == "YT_OLV"] <- "Youtube OLV"
names(opt_spot_cold_bar)[names(opt_spot_cold_bar) == "YT_DD"] <- "Youtube DD"
names(opt_spot_cold_bar)[names(opt_spot_cold_bar) == "POLV"] <- "Prog OLV"
names(opt_spot_cold_bar)[names(opt_spot_cold_bar) == "DOOH"] <- "Digital OOH"
opt_spot_cold_bar = opt_spot_cold_bar %>% 
  # rename_at(vars(names$wrong), ~ names$right) %>%
  melt.data.table(id.vars = "Budget", value.name = "Value", variable.name = "Media")
levels(opt_spot_cold_bar$Media) <- gsub(" ", "\n", levels(opt_spot_cold_bar$Media))

opt_spot_cold$Budget = as.factor(opt_spot_cold$Budget)

opt_spot_warm <- fread(paste0('./inputs/inputs/', list.files('./inputs/inputs/', pattern='ROAS_warm.csv')))
opt_spot_warm_1 <- opt_spot_warm[1]
opt_spot_warm_bar <- opt_spot_warm
opt_spot_warm_bar$Budget = as.factor(opt_spot_warm$Budget)
names(opt_spot_warm_bar)[names(opt_spot_warm_bar) == "Other Social Social"] <- "Other Social"
names(opt_spot_warm_bar)[names(opt_spot_warm_bar) == "DD"] <- "Direct Display"
names(opt_spot_warm_bar)[names(opt_spot_warm_bar) == "PDM"] <- "Prog Display"
names(opt_spot_warm_bar)[names(opt_spot_warm_bar) == "Secondary_search"] <- "Secondary Search"
names(opt_spot_warm_bar)[names(opt_spot_warm_bar) == "Primary_search"] <- "Primary Search"
names(opt_spot_warm_bar)[names(opt_spot_warm_bar) == "YT_OLV"] <- "Youtube OLV"
names(opt_spot_warm_bar)[names(opt_spot_warm_bar) == "YT_DD"] <- "Youtube DD"
names(opt_spot_warm_bar)[names(opt_spot_warm_bar) == "POLV"] <- "Prog OLV"
names(opt_spot_warm_bar)[names(opt_spot_warm_bar) == "DOOH"] <- "Digital OOH"
opt_spot_warm_bar = opt_spot_warm_bar %>% 
  # rename_at(vars(names$wrong), ~ names$right) %>%
  melt.data.table(id.vars = "Budget", value.name = "Value", variable.name = "Media")
levels(opt_spot_warm_bar$Media) <- gsub(" ", "\n", levels(opt_spot_warm_bar$Media))

opt_spot_warm$Budget = as.factor(opt_spot_warm$Budget)

opt_spot_hot <- fread(paste0('./inputs/inputs/', list.files('./inputs/inputs/', pattern='ROAS_hot.csv')))
opt_spot_hot_1 <- opt_spot_hot[1]
opt_spot_hot_bar <- opt_spot_hot
opt_spot_hot_bar$Budget = as.factor(opt_spot_hot$Budget)
names(opt_spot_hot_bar)[names(opt_spot_hot_bar) == "Other Social Social"] <- "Other Social"
names(opt_spot_hot_bar)[names(opt_spot_hot_bar) == "DD"] <- "Direct Display"
names(opt_spot_hot_bar)[names(opt_spot_hot_bar) == "PDM"] <- "Prog Display"
names(opt_spot_hot_bar)[names(opt_spot_hot_bar) == "Secondary_search"] <- "Secondary Search"
names(opt_spot_hot_bar)[names(opt_spot_hot_bar) == "Primary_search"] <- "Primary Search"
names(opt_spot_hot_bar)[names(opt_spot_hot_bar) == "YT_OLV"] <- "Youtube OLV"
names(opt_spot_hot_bar)[names(opt_spot_hot_bar) == "YT_DD"] <- "Youtube DD"
names(opt_spot_hot_bar)[names(opt_spot_hot_bar) == "POLV"] <- "Prog OLV"
names(opt_spot_hot_bar)[names(opt_spot_hot_bar) == "DOOH"] <- "Digital OOH"
opt_spot_hot_bar = opt_spot_hot_bar %>% 
  # rename_at(vars(names$wrong), ~ names$right) %>%
  melt.data.table(id.vars = "Budget", value.name = "Value", variable.name = "Media")
levels(opt_spot_hot_bar$Media) <- gsub(" ", "\n", levels(opt_spot_hot_bar$Media))

opt_spot_hot$Budget = as.factor(opt_spot_hot$Budget)



# Import decomposition data
re_decomp_bar_BRAND <- fread(paste0('./inputs/inputs/', list.files('./inputs/inputs/', pattern='decomp_bar.csv'))) 
re_decomp_bar_BRAND$Date <- as.Date(re_decomp_bar_BRAND$Date, format = "%d.%m.%Y")

# Decomposition plots
re_decomp_bar_BRAND <- re_decomp_bar_BRAND %>% melt.data.table(id.vars = 'Date', value.name = 'data', variable.name = 'Value')




mroas_table <- fread(paste0('./inputs/inputs/', list.files('./inputs/inputs/', pattern='mroas_hist.csv')))
mroas_table$Value=as.numeric((mroas_table$Value))

mroas_cold <- fread(paste0('./inputs/inputs/', list.files('./inputs/inputs/', pattern='mroas_cold.csv')))
mroas_cold$Value=as.numeric((mroas_cold$Value))
mroas_cold$Value=(mroas_cold$Value/635)*1000000

mroas_warm <- fread(paste0('./inputs/inputs/', list.files('./inputs/inputs/', pattern='mroas_warm.csv')))
mroas_warm$Value=as.numeric((mroas_warm$Value))
mroas_warm$Value=(mroas_warm$Value/635)*1000000

mroas_hot <- fread(paste0('./inputs/inputs/', list.files('./inputs/inputs/', pattern='mroas_hot.csv')))
mroas_hot$Value=as.numeric((mroas_hot$Value))
mroas_hot$Value=(mroas_hot$Value/635)*1000000



table_2019 <- fread(paste0('./inputs/inputs/', list.files('./inputs/inputs/', pattern='response1.csv')))
table_2019$Value=as.numeric((table_2019$Value))
table_2020 <- fread(paste0('./inputs/inputs/', list.files('./inputs/inputs/', pattern='response2.csv')))
table_2020$Value=as.numeric((table_2020$Value))


media_vars <- c('Audio_TOTAL_Clicks', 'DD_TOTAL_Impressions',
                'OLV_TOTAL_Clicks', 'OTT_TOTAL_Impressions', 'PDM_TOTAL_Impressions',
                'POLV_TOTAL_Clicks', 'Partnership_TOTAL_Impressions',
                'YT_DD_TOTAL_Clicks',
                'YT_OLV_TOTAL_Impressions', 'Primary_Search_Impressions',
                'Secondary_Search_Impressions', 'DOOH_TOTAL_Impressions',
                'Social_TOTAL_Impressions', 'Facebook_Impressions')
# Заменить с приставкой _R и _ROAS

media_vars_names_resp <- c('Audio_R', 'Direct Display_R',
                           'OLV_R', 'OTT_R', 'Prog Display_R',
                           'Prog OLV_R', 'Partnership_R',
                           'Youtube DD_R',
                           'Youtube OLV_R', 'Primary Search_R',
                           'Secondary Search_R', 'Digital OOH_R',
                           'Other Social_R', 'Facebook_R', 'investments')

# media_vars_names_resp_sep <<- lapply(names(choiceVec_0_sep), function(i) {paste0(i, '_R')})

media_vars_names_ROAS<- c('Audio_ROAS', 'Direct Display_ROAS',
                          'OLV_ROAS', 'OTT_ROAS', 'Prog Display_ROAS',
                          'Prog OLV_ROAS', 'Partnership_ROAS',
                          'Youtube DD_ROAS',
                          'Youtube OLV_ROAS', 'Primary Search_ROAS',
                          'Secondary Search_ROAS', 'Digital OOH_ROAS',
                          'Other Social_ROAS', 'Facebook_ROAS')


resp_poly <- function(response_curves, channel) {
  a <- c(response_curves[, ..channel])[[1]]
  x <- c(response_curves[, "investments"])[[1]]
  #model <- lm(a ~ poly(x, 5))
  if (channel == 'POLV_warm_R'){
    model <- lm(a ~ poly(exp(-0.00001*x) , 1))}
  if (channel != 'POLV_warm_R'){
    model <- lm(a ~ poly(exp(-1/(x+10**6)), 5))}
  return(model)
}

# resp_poly(total_response_BRAND, "Direct Display_R")
# 
# response_curves<-total_response_BRAND
# a <- c(response_curves[,c("Audio_R")])[[1]]
# x <- c(response_curves[,'investments'])[[1]]
# model <- lm(a ~ poly(exp(-1/(x+10**6)), 15))
# return(model)

predict_poly <- function(model_dict, channel, value){
  prd<-predict(model_dict[[channel]], data.frame(x=value))
  return(prd)
}


recalc_opt <- function(slid_list, budget_corr, temp) {
  
  len_channels <- length(slid_list)/2
  slid_list_temp <- c()
  slid_list_temp_2 <- c()
  channels_list <- list()
  correct_budg <- 0
  
  for (i in seq(1,len_channels,1)){
    if (slid_list[2*i] > 0){
      if (temp == 'cold' && media_vars_names_resp_sep[i] %in% lapply(names(choiceVec_0_sep_cold), function(x) {paste0(x, '_R')})){ 
        print('lev')
        channels_list <- append(channels_list,media_vars_names_resp_sep[i])
        slid_list_temp[[media_vars_names_resp_sep[i]]] <- list(slid_list[2*i-1]*1000000,slid_list[2*i]*1000000)
        slid_list_temp_2[[media_vars_names_resp_sep[i]]] <- list(slid_list[2*i-1]*1000000,slid_list[2*i]*1000000)
        if (slid_list_temp_2[[media_vars_names_resp_sep[i]]][[2]]>sat_dict[[media_vars_names_resp_sep[i]]]){
          slid_list_temp_2[[media_vars_names_resp_sep[i]]][[2]]=sat_dict[[media_vars_names_resp_sep[i]]]
        }
        correct_budg <- correct_budg + (slid_list_temp_2[[media_vars_names_resp_sep[i]]][[2]])
        #print(correct_budg)
      }
    }
  }
  
  for (i in seq(1,len_channels,1)){
    if (slid_list[2*i] > 0){
      if (temp == 'warm' && media_vars_names_resp_sep[i] %in% lapply(names(choiceVec_0_sep_warm), function(x) {paste0(x, '_R')})){ 
        print('lev')
        channels_list <- append(channels_list,media_vars_names_resp_sep[i])
        slid_list_temp[[media_vars_names_resp_sep[i]]] <- list(slid_list[2*i-1]*1000000,slid_list[2*i]*1000000)
        slid_list_temp_2[[media_vars_names_resp_sep[i]]] <- list(slid_list[2*i-1]*1000000,slid_list[2*i]*1000000)
        if (slid_list_temp_2[[media_vars_names_resp_sep[i]]][[2]]>sat_dict[[media_vars_names_resp_sep[i]]]){
          slid_list_temp_2[[media_vars_names_resp_sep[i]]][[2]]=sat_dict[[media_vars_names_resp_sep[i]]]
        }
        correct_budg <- correct_budg + (slid_list_temp_2[[media_vars_names_resp_sep[i]]][[2]])
        #print(correct_budg)
      }
    }
  }
  
  for (i in seq(1,len_channels,1)){
    if (slid_list[2*i] > 0){
      if (temp == 'hot' && media_vars_names_resp_sep[i] %in% lapply(names(choiceVec_0_sep_hot), function(x) {paste0(x, '_R')})){ 
        print('lev')
        channels_list <- append(channels_list,media_vars_names_resp_sep[i])
        slid_list_temp[[media_vars_names_resp_sep[i]]] <- list(slid_list[2*i-1]*1000000,slid_list[2*i]*1000000)
        slid_list_temp_2[[media_vars_names_resp_sep[i]]] <- list(slid_list[2*i-1]*1000000,slid_list[2*i]*1000000)
        if (slid_list_temp_2[[media_vars_names_resp_sep[i]]][[2]]>sat_dict[[media_vars_names_resp_sep[i]]]){
          slid_list_temp_2[[media_vars_names_resp_sep[i]]][[2]]=sat_dict[[media_vars_names_resp_sep[i]]]
        }
        correct_budg <- correct_budg + (slid_list_temp_2[[media_vars_names_resp_sep[i]]][[2]])
        #print(correct_budg)
      }
    }
  }
  
  
  channels <- channels_list
  revenue_coefficients_temp <- subset(revenue_coefficients, media %in% channels)
  model_list_temp <- list()
  for (col in channels){
    model_list_temp[[col]] <- model_dict[[col]]}
  
  
  # correct_budg <- Reduce(`+`, sat_dict)
  ad_optimal_splits_BRAND_2 <- MMM_optimize_budget_split(budget_corr, revenue_coefficients_temp, model_list_temp, slid_list_temp)
  ad_optimal_splits_ROAS_2 <- ad_optimal_splits_BRAND_2[,'profit']
  ad_optimal_splits_ROAS_2$budget_total <- ad_optimal_splits_BRAND_2$budget/1000000
  ad_optimal_splits_BRAND_2 <- subset(ad_optimal_splits_BRAND_2, select = -c(profit))
  names(ad_optimal_splits_BRAND_2)[names(ad_optimal_splits_BRAND_2) == 'budget'] <- 'Budget'
  ad_optimal_splits_BRAND_2 <- melt.data.table(data= ad_optimal_splits_BRAND_2, id.vars = 'Budget', value.name = 'Value', variable.name = 'Media')
  ad_optimal_splits_BRAND_2$budget_total <- (ad_optimal_splits_BRAND_2$Budget/1000000)
  ad_optimal_splits_BRAND_2$data <- ad_optimal_splits_BRAND_2$Value
  ad_optimal_splits_BRAND_2$Value <- ad_optimal_splits_BRAND_2$Media
  ad_optimal_splits_BRAND_2$data <- ad_optimal_splits_BRAND_2$data/ad_optimal_splits_BRAND_2$Budget
  ad_optimal_splits_BRAND_2[is.na(ad_optimal_splits_BRAND_2)] <- 0
  ad_optimal_splits_BRAND_2 <<- ad_optimal_splits_BRAND_2
  ad_optimal_splits_BRAND <<- ad_optimal_splits_BRAND_2
  
  ad_optimal_splits_BRAND$Value <- as.character(ad_optimal_splits_BRAND$Value)
  ad_optimal_splits_BRAND$Value <- substr(ad_optimal_splits_BRAND$Value, 1, nchar(ad_optimal_splits_BRAND$Value)-2)
  
  return(list(ad_optimal_splits_BRAND,ad_optimal_splits_ROAS_2))
}














recalc_opt_sep <- function(slid_list, budget_corr) {
  
  
  
  # slid_list <- channles_budgets_sep_input
  # budget_corr <- 10^7
  len_channels <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25)
  slid_list_temp <- c()
  slid_list_temp_2 <- c()
  channels_list <- list()
  correct_budg <- 0
  
  
  
  

  
  
  for (i in len_channels){
    if (slid_list[2*i] > 0){
      channels_list <- append(channels_list,media_vars_names_resp_sep[i])
      slid_list_temp[[media_vars_names_resp_sep[i]]] <- list(slid_list[2*i-1]*1000000,slid_list[2*i]*1000000)
      slid_list_temp_2[[media_vars_names_resp_sep[i]]] <- list(slid_list[2*i-1]*1000000,slid_list[2*i]*1000000)
      if (slid_list_temp_2[[media_vars_names_resp_sep[i]]][[2]]>sat_dict[[media_vars_names_resp_sep[i]]]){
        slid_list_temp_2[[media_vars_names_resp_sep[i]]][[2]]=sat_dict[[media_vars_names_resp_sep[i]]]
      }
      correct_budg <- correct_budg + (slid_list_temp_2[[media_vars_names_resp_sep[i]]][[2]])
      #print(correct_budg)
    }
  }
  
  
  
  channels <- channels_list
  revenue_coefficients_temp <- subset(revenue_coefficients, media %in% channels)
  model_list_temp <- list()
  for (col in channels){
    model_list_temp[[col]] <- model_dict[[col]]}
  
  
  # correct_budg <- Reduce(`+`, sat_dict)
  print('slid_list_temp')
  print(slid_list_temp)
  ad_optimal_splits_BRAND_2 <- MMM_optimize_budget_split(budget_corr, revenue_coefficients_temp, model_list_temp, slid_list_temp)
  whaaaat <<- ad_optimal_splits_BRAND_2
  ad_optimal_splits_ROAS_2 <- ad_optimal_splits_BRAND_2[,'profit']
  ad_optimal_splits_ROAS_2$budget_total <- ad_optimal_splits_BRAND_2$budget/1000000
  ad_optimal_splits_BRAND_2 <- subset(ad_optimal_splits_BRAND_2, select = -c(profit))
  names(ad_optimal_splits_BRAND_2)[names(ad_optimal_splits_BRAND_2) == 'budget'] <- 'Budget'
  ad_optimal_splits_BRAND_2 <- melt.data.table(data= ad_optimal_splits_BRAND_2, id.vars = 'Budget', value.name = 'Value', variable.name = 'Media')
  ad_optimal_splits_BRAND_2$budget_total <- (ad_optimal_splits_BRAND_2$Budget/1000000)
  ad_optimal_splits_BRAND_2$data <- ad_optimal_splits_BRAND_2$Value
  ad_optimal_splits_BRAND_2$Value <- ad_optimal_splits_BRAND_2$Media
  ad_optimal_splits_BRAND_2$data <- ad_optimal_splits_BRAND_2$data/ad_optimal_splits_BRAND_2$Budget
  ad_optimal_splits_BRAND_2[is.na(ad_optimal_splits_BRAND_2)] <- 0
  ad_optimal_splits_BRAND_2 <<- ad_optimal_splits_BRAND_2
  ad_optimal_splits_BRAND <<- ad_optimal_splits_BRAND_2
  
  ad_optimal_splits_BRAND$Value <- as.character(ad_optimal_splits_BRAND$Value)
  ad_optimal_splits_BRAND$Value <- substr(ad_optimal_splits_BRAND$Value, 1, nchar(ad_optimal_splits_BRAND$Value)-2)
  
  for (j in unique(ad_optimal_splits_BRAND$Value)) {
    tryCatch({
      
      
      
      ad_optimal_splits_BRAND$Value[ad_optimal_splits_BRAND$Value == j] <-  names(choiceVec_0_sep_help[grep(paste0('^', j), names(choiceVec_0_sep_4opt))])
    })
  }
  
  return(list(ad_optimal_splits_BRAND,ad_optimal_splits_ROAS_2))
}










recalc_opt_2 <- function(channels_list, budget_corr) {
  
  for (i in seq(1, length(channels_list))) {
    channels_list[i] = paste0(channels_list[i], "_R")
  }
  
  len_channels <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29)
  slid_list_temp <- c()
  slid_list_temp_2 <- c()
  correct_budg <- 12*10**6
  
  for (channel in channels_list) {
    
    slid_list_temp_2[[channel]] <- list(0, 100*10^6)
  }
  
  
  channels <- channels_list
  revenue_coefficients_temp <- subset(revenue_coefficients, media %in% channels)
  model_list_temp <- list()
  for (col in channels){
    model_list_temp[[col]] <- model_dict[[col]]}
  
  
  # correct_budg <- Reduce(`+`, sat_dict)
  ad_optimal_splits_BRAND_2 <- MMM_optimize_budget_split_2(budget_corr, revenue_coefficients_temp, model_list_temp, slid_list_temp_2)
  ad_optimal_splits_ROAS_2 <- ad_optimal_splits_BRAND_2[,'profit']
  ad_optimal_splits_ROAS_2$budget_total <- ad_optimal_splits_BRAND_2$budget/1000000
  ad_optimal_splits_BRAND_2 <- subset(ad_optimal_splits_BRAND_2, select = -c(profit))
  names(ad_optimal_splits_BRAND_2)[names(ad_optimal_splits_BRAND_2) == 'budget'] <- 'Budget'
  ad_optimal_splits_BRAND_2 <- melt.data.table(data= ad_optimal_splits_BRAND_2, id.vars = 'Budget', value.name = 'Value', variable.name = 'Media')
  ad_optimal_splits_BRAND_2$budget_total <- (ad_optimal_splits_BRAND_2$Budget/1000000)
  ad_optimal_splits_BRAND_2$data <- ad_optimal_splits_BRAND_2$Value
  ad_optimal_splits_BRAND_2$Value <- ad_optimal_splits_BRAND_2$Media
  ad_optimal_splits_BRAND_2$data <- ad_optimal_splits_BRAND_2$data/ad_optimal_splits_BRAND_2$Budget
  ad_optimal_splits_BRAND_2[is.na(ad_optimal_splits_BRAND_2)] <- 0
  ad_optimal_splits_BRAND_2 <<- ad_optimal_splits_BRAND_2
  ad_optimal_splits_BRAND <<- ad_optimal_splits_BRAND_2
  
  ad_optimal_splits_BRAND$Value <- as.character(ad_optimal_splits_BRAND$Value)
  ad_optimal_splits_BRAND$Value <- substr(ad_optimal_splits_BRAND$Value, 1, nchar(ad_optimal_splits_BRAND$Value)-2)
  
  return(list(ad_optimal_splits_BRAND,ad_optimal_splits_ROAS_2))
}





recalc_opt_2_sep <- function(channels_list, budget_corr) {
  
  m1 <<- channels_list
  
  b = list()
  for (i in test2_1) {
    
    a = choiceVec_0_sep_4opt[grep(paste0('^', i), names(choiceVec_0_sep_help))]  
    b = append(b, a)
    
  }
  channels_list = names(b)
  m2 <<- channels_list
  for (i in seq(1, length(channels_list))) {
    channels_list[i] = paste0(channels_list[i], "_R")
  }
  
  len_channels <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25)
  slid_list_temp <- c()
  slid_list_temp_2 <- c()
  correct_budg <- 12*10**6
  
  for (channel in channels_list) {
    
    slid_list_temp_2[[channel]] <- list(0, 100*10^6)
  }
  
  
  channels <- channels_list
  revenue_coefficients_temp <- subset(revenue_coefficients, media %in% channels)
  model_list_temp <- list()
  for (col in channels){
    model_list_temp[[col]] <- model_dict[[col]]}
  
  # correct_budg <- Reduce(`+`, sat_dict)
  ad_optimal_splits_BRAND_2 <- MMM_optimize_budget_split_2(budget_corr, revenue_coefficients_temp, model_list_temp, slid_list_temp_2)
  ad_optimal_splits_ROAS_2 <- ad_optimal_splits_BRAND_2[,'profit']
  ad_optimal_splits_ROAS_2$budget_total <- ad_optimal_splits_BRAND_2$budget/1000000
  ad_optimal_splits_BRAND_2 <- subset(ad_optimal_splits_BRAND_2, select = -c(profit))
  names(ad_optimal_splits_BRAND_2)[names(ad_optimal_splits_BRAND_2) == 'budget'] <- 'Budget'
  ad_optimal_splits_BRAND_2 <- melt.data.table(data= ad_optimal_splits_BRAND_2, id.vars = 'Budget', value.name = 'Value', variable.name = 'Media')
  ad_optimal_splits_BRAND_2$budget_total <- (ad_optimal_splits_BRAND_2$Budget/1000000)
  ad_optimal_splits_BRAND_2$data <- ad_optimal_splits_BRAND_2$Value
  ad_optimal_splits_BRAND_2$Value <- ad_optimal_splits_BRAND_2$Media
  ad_optimal_splits_BRAND_2$data <- ad_optimal_splits_BRAND_2$data/ad_optimal_splits_BRAND_2$Budget
  ad_optimal_splits_BRAND_2[is.na(ad_optimal_splits_BRAND_2)] <- 0
  ad_optimal_splits_BRAND_2 <<- ad_optimal_splits_BRAND_2
  ad_optimal_splits_BRAND <<- ad_optimal_splits_BRAND_2
  
  ad_optimal_splits_BRAND$Value <- as.character(ad_optimal_splits_BRAND$Value)
  ad_optimal_splits_BRAND$Value <- substr(ad_optimal_splits_BRAND$Value, 1, nchar(ad_optimal_splits_BRAND$Value)-2)
  
  alfa <<- ad_optimal_splits_BRAND
  
  for (j in unique(ad_optimal_splits_BRAND$Value)) {
    tryCatch({
      
      
      
      ad_optimal_splits_BRAND$Value[ad_optimal_splits_BRAND$Value == j] <-  names(choiceVec_0_sep_help[grep(paste0('^', j), names(choiceVec_0_sep_4opt))])
    })
  }
  
  
  return(list(ad_optimal_splits_BRAND,ad_optimal_splits_ROAS_2))
}





total_response_BRAND <- fread(paste0('./inputs/inputs/', list.files('./inputs/inputs/', pattern='cereals_profits_ok.csv')))[, c(media_vars_names_resp,media_vars_names_ROAS) := list(Audio_TOTAL_Clicks, DD_TOTAL_Impressions, OLV_TOTAL_Clicks, OTT_TOTAL_Impressions, PDM_TOTAL_Impressions, POLV_TOTAL_Clicks, Partnership_TOTAL_Impressions, YT_DD_TOTAL_Clicks, YT_OLV_TOTAL_Impressions, Primary_Search_Impressions, Secondary_Search_Impressions, DOOH_TOTAL_Impressions, Social_TOTAL_Impressions, Facebook_Impressions, round(Investment), Audio_TOTAL_Clicks_ROAS, DD_TOTAL_Impressions_ROAS, OLV_TOTAL_Clicks_ROAS, OTT_TOTAL_Impressions_ROAS, PDM_TOTAL_Impressions_ROAS, POLV_TOTAL_Clicks_ROAS, Partnership_TOTAL_Impressions_ROAS, YT_DD_TOTAL_Clicks_ROAS, YT_OLV_TOTAL_Impressions_ROAS, Primary_Search_Impressions_ROAS, Secondary_Search_Impressions_ROAS, DOOH_TOTAL_Impressions_ROAS, Social_TOTAL_Impressions_ROAS, Facebook_Impressions_ROAS)]

total_response_BRAND <- fread(paste0('./inputs/inputs/', list.files('./inputs/inputs/', pattern='separated_roas.csv')))
# total_response_BRAND <- total_response_BRAND %>% select(c(media_vars_names_resp,media_vars_names_ROAS))


# colnames(total_response_BRAND)

# total_response_BRAND_sep <- fread(paste0('./inputs/inputs/', list.files('./inputs/inputs/', pattern='separated_roas.csv')))
# colnames(total_response_BRAND_sep)

fix_response <- function(table, invest, gran) {
  invest_0 <- table[nrow(table)][['investments']] + gran
  num_0 <- table[nrow(table)][['V1']]
  last_row <- table[nrow(table)]
  i <- 0 
  for (inv in seq(invest_0, invest, gran)){
    new_row <- last_row
    new_row[['investments']] <- inv
    new_row[['V1']] <- num_0 + i
    table <- rbind(table, new_row) 
    i <- i+1
  }
  return(table)
}



total_response_BRAND <- fix_response(total_response_BRAND, 11000000, 10000)
revenue_coefficients <- MMM_revenue_coefficients(total_response_BRAND)

# revenue_coefficients_sep <- MMM_revenue_coefficients(total_response_BRAND_sep)


model_dict=list()
for (col in revenue_coefficients$media){
  model_dict[[col]]=resp_poly(total_response_BRAND, col)
}


sat_dict <- list()
for (col in revenue_coefficients$media){
  temp <- (total_response_BRAND[[col]]-shift(total_response_BRAND[[col]]))>1
  temp[is.na(temp)] <- 1
  sat_dict[[col]]=sum(temp)*10000
}

final_table <- function(input_budget) {
  ad_optimal_splits_BRAND$Value <- as.character(ad_optimal_splits_BRAND$Value)
  ad_optimal_splits_BRAND$Value <- substr(ad_optimal_splits_BRAND$Value, 1, nchar(ad_optimal_splits_BRAND$Value)-2)
  temp = ad_optimal_splits_BRAND
  names(temp)[1] = "Budget"
  names(temp)[2] = "Media"
  fintab<-temp[Budget==input_budget*1000000] %>%
    select(Media, data)
  fintab$abs<-fintab$data*(input_budget*1000000)
  colnames(fintab) <- c("Media", "Share", "Budget")
  fintab=fintab[as.numeric(Budget)>1]
  fintab$Media <- as.character(fintab$Media)
  fintab$Media <- substr(fintab$Media, 1, nchar(fintab$Media)-2)
  fintab2<<-fintab
  return(fintab2)
}

final_table_2 <- function(input_budget) {
  ad_optimal_splits_BRAND$Value <- as.character(ad_optimal_splits_BRAND$Value)
  ad_optimal_splits_BRAND$Value <- substr(ad_optimal_splits_BRAND$Value, 1, nchar(ad_optimal_splits_BRAND$Value)-2)
  temp = ad_optimal_splits_BRAND
  names(temp)[1] = "Budget"
  names(temp)[2] = "Media"
  fintab<-temp[Budget==input_budget*1000000] %>%
    select(Media, data)
  fintab$abs<-fintab$data*(input_budget*1000000)
  colnames(fintab) <- c("Media", "Share", "Budget")
  fintab=fintab[as.numeric(Budget)>1]
  fintab$Media <- as.character(fintab$Media)
  fintab$Media <- substr(fintab$Media, 1, nchar(fintab$Media)-2)
  fintab2_2<<-fintab
  return(fintab2_2)
}







choiceVec_0_sep_4opt <- list('Audio_cold' = '1',
                             'YouTube_DD_cold' = '2',
                             'YouTube_DD_warm' = '3',
                             'YouTube_OLV_cold' = '4',
                             'YouTube_OLV_warm' = '5',
                             # 'OTT_cold' = '6',
                             'OTT_warm' = '6',
                             'PDM_cold' = '7',
                             'PDM_hot' = '8',
                             'PDM_warm' = '9',
                             'POLV_cold' = '10',
                             # 'POLV_hot' = '12',
                             'POLV_warm' = '11',
                             # 'Partnership - Cold' = '14',
                             'Partnership_cold' = '12',
                             # 'Partnership_warm' = '13',
                             'Primary_Search_hot' = '13',
                             'Secondary_Search_hot' = '14',
                             'Facebook_cold' = '15',
                             'Facebook_hot' = '16',
                             'Facebook_warm' = '17',
                             'Social_cold' = '18',
                             'Social_warm' = '19',
                             'Social_hot' = '20',
                             'DD_cold' = '21',
                             'DD_hot' = '22',
                             'DD_warm' = '23',
                             'OLV_cold' = '24',
                             'OLV_warm' = '25')

choiceVec_0_sep <- list('Audio - Cold' = '1',
                        'YouTube DD - Cold' = '2',
                        'YouTube DD - Warm' = '3',
                        'YouTube OLV - Cold' = '4',
                        'YouTube OLV - Warm' = '5',
                        'OTT - Cold' = '6',
                        'OTT - Warm' = '7',
                        'Prog Display - Cold' = '8',
                        'Prog Display - Hot' = '9',
                        'Prog Display - Warm' = '10',
                        'Prog OLV - Cold' = '11',
                        'Prog OLV - Hot' = '12',
                        'Prog OLV - Warm' = '13',
                        'Partnership - Cold' = '14',
                        'Partnership - Cold' = '15',
                        'Partnership - Warm' = '16',
                        'Primary Search - Hot' = '17',
                        'Secondary Search - Hot' = '18',
                        'Facebook - Cold' = '19',
                        'Facebook - Hot' = '20',
                        'Facebook - Warm' = '21',
                        'Other Social - Cold' = '22',
                        'Other Social - Warm' = '23',
                        'Other Social - Hot' = '24',
                        'Direct Display - Cold' = '25',
                        'Direct Display - Hot' = '26',
                        'Direct Display - Warm' = '27',
                        'OLV - Cold' = '28',
                        'OLV - Warm' = '29')


choiceVec_0_sep_help <- list('Audio - Cold' = '1',
                        'YouTube DD - Cold' = '2',
                        'YouTube DD - Warm' = '3',
                        'YouTube OLV - Cold' = '4',
                        'YouTube OLV - Warm' = '5',
                        'OTT - Warm' = '6',
                        'Prog Display - Cold' = '7',
                        'Prog Display - Hot' = '8',
                        'Prog Display - Warm' = '9',
                        'Prog OLV - Cold' = '10',
                        'Prog OLV - Warm' = '11',
                        'Partnership - Cold' = '12',
                        'Primary Search - Hot' = '13',
                        'Secondary Search - Hot' = '14',
                        'Facebook - Cold' = '15',
                        'Facebook - Hot' = '16',
                        'Facebook - Warm' = '17',
                        'Other Social - Cold' = '18',
                        'Other Social - Warm' = '19',
                        'Other Social - Hot' = '20',
                        'Direct Display - Cold' = '21',
                        'Direct Display - Hot' = '22',
                        'Direct Display - Warm' = '23',
                        'OLV - Cold' = '24',
                        'OLV - Warm' = '25')








choiceVec_0_sep_cold_4opt <- list('Audio_cold' = '1',
                                  'YouTube_DD_cold' = '2',
                                  'YouTube_OLV_cold' = '3',
                                  'OTT_cold' = '4',
                                  'PDM_cold' = '5',
                                  'POLV_cold' = '6',
                                  'Partnership_cold' = '7',
                                  'Facebook_cold' = '8',
                                  'Social_cold' = '9',
                                  'DD_cold' = '10',
                                  'OLV_cold' = '11')



choiceVec_0_sep_warm_4opt <- list('YouTube_DD_warm' = '1',
                                  'YouTube_OLV_warm' = '2',
                                  'OTT_warm' = '3',
                                  'PDM_warm' = '4',
                                  'POLV_warm' = '5',
                                  'OOH_warm' = '6',
                                  'Partnership_warm' = '7',
                                  'Facebook_warm' = '8',
                                  'Social_warm' = '9',
                                  'DD_warm' = '10',
                                  'OLV_warm' = '11')



choiceVec_0_sep_hot_4opt <- list('PDM_hot' = '1',
                                 'POLV_hot' = '2',
                                 'Primary_Search_hot' = '3',
                                 'Secondary_Search_hot' = '4',
                                 'Facebook_hot' = '5',
                                 'Social_hot' = '6',
                                 'DD_hot' = '7')



choiceVec_0_sep_cold<- list('Audio - Cold' = '1',
                            'YouTube DD - Cold' = '2',
                            'YouTube OLV - Cold' = '3',
                            'OTT - Cold' = '4',
                            'Prog Display - Cold' = '5',
                            'Prog OLV - Cold' = '6',
                            'Partnership - Cold' = '7',
                            'Facebook - Cold' = '8',
                            'Other Social - Cold' = '9',
                            'Direct Display - Cold' = '10',
                            'OLV - Cold' = '11')

choiceVec_0_sep_cold_help<- list('Audio - Cold' = '1',
                            'YouTube DD - Cold' = '2',
                            'YouTube OLV - Cold' = '3',
                            'Prog Display - Cold' = '4',
                            'Prog OLV - Cold' = '5',
                            'Partnership - Cold' = '6',
                            'Facebook - Cold' = '7',
                            'Other Social - Cold' = '8',
                            'Direct Display - Cold' = '9',
                            'OLV - Cold' = '10')



choiceVec_0_sep_cold_1<- list('Audio' = '1',
                              'YouTube DD' = '2',
                              'YouTube OLV' = '3',
                              'Prog Display' = '4',
                              'Prog OLV' = '5',
                              'Partnership' = '6',
                              'Facebook' = '7',
                              'Other Social' = '8',
                              'Direct Display' = '9',
                              'OLV' = '10')



choiceVec_0_sep_warm <- list('YouTube DD - Warm' = '1',
                             'YouTube OLV - Warm' = '2',
                             'OTT - Warm' = '3',
                             'Prog Display - Warm' = '4',
                             'Prog OLV - Warm' = '5',
                             'Partnership - Warm' = '6',
                             'Facebook - Warm' = '7',
                             'Other Social - Warm' = '8',
                             'Direct Display - Warm' = '9',
                             'OLV - Warm' = '10')

  choiceVec_0_sep_warm_help <- list('YouTube DD - Warm' = '1',
                               'YouTube OLV - Warm' = '2',
                               'OTT - Warm' = '3',
                               'Prog Display - Warm' = '4',
                               'Prog OLV - Warm' = '5',
                               'Facebook - Warm' = '6',
                               'Other Social - Warm' = '7',
                               'Direct Display - Warm' = '8',
                               'OLV - Warm' = '9')


choiceVec_0_sep_warm_1 <- list('YouTube DD' = '1',
                               'YouTube OLV' = '2',
                               'OTT' = '3',
                               'Prog Display' = '4',
                               'Prog OLV' = '5',
                               'Facebook' = '6',
                               'Other Social' = '7',
                               'Direct Display' = '8',
                               'OLV' = '9')



choiceVec_0_sep_hot <- list('Prog Display - Hot' = '1',
                            'Prog OLV - Hot' = '2',
                            'Primary Search - Hot' = '3',
                            'Secondary Search - Hot' = '4',
                            'Facebook - Hot' = '5',
                            'Other Social - Hot' = '6',
                            'Direct Display - Hot' = '7')

choiceVec_0_sep_hot_help <- list('Prog Display - Hot' = '1',
                            'Primary Search - Hot' = '2',
                            'Secondary Search - Hot' = '3',
                            'Facebook - Hot' = '4',
                            'Other Social - Hot' = '5',
                            'Direct Display - Hot' = '6')




choiceVec_0_sep_hot_1 <- list('Prog Display' = '1',
                              'Primary Search' = '2',
                              'Secondary Search' = '3',
                              'Facebook' = '4',
                              'Other Social' = '5',
                              'Direct Display' = '6')






dd <- names(choiceVec_0_sep)
dd.col <- hcl.colors(length(dd), palette = 'GnBu')
names(dd.col)  <- dd


dd_cold <- names(choiceVec_0_sep_cold)
dd_cold.col <- hcl.colors(length(dd_cold), palette = 'GnBu')
names(dd_cold.col)  <- dd_cold

dd_warm <-names(choiceVec_0_sep_warm)
dd_warm.col <- hcl.colors(length(dd_warm), palette = 'GnBu')
names(dd_warm.col)  <- dd_warm

dd_hot <-names(choiceVec_0_sep_hot)
dd_hot.col <- hcl.colors(length(dd_hot), palette = 'GnBu')
names(dd_hot.col)  <- dd_hot


# Decomposition<-read_excel('./inputs/inputs/decomposition_excel.xlsx')
Decomposition<-read_excel('./inputs/inputs/decomposition_processed.xlsx')
Decomposition<-Decomposition[c(0:nrow(Decomposition)),c(-2,-3)]
ActFit <- Decomposition[c(0:nrow(Decomposition)),c(1:2,3)]


Value<-read_excel('./inputs/inputs/Incremental_value.xlsx')%>% as.data.table() %>%melt.data.table()
Volume<-read_excel('./inputs/inputs/Incremental_volume.xlsx')%>% as.data.table() %>%melt.data.table()
Volume<-Volume[Volume$value>=0.01]
Value$value = Value$value*100
Volume$value = Volume$value*100
Volume_cold<-read_excel('./inputs/inputs/Incremental_volume_cold.xlsx')%>% as.data.table() %>%melt.data.table()
Volume_cold<-Volume_cold[Volume_cold$value>=0.01]
Volume_cold$value = Volume_cold$value*100
Volume_warm<-read_excel('./inputs/inputs/Incremental_volume_warm.xlsx')%>% as.data.table() %>%melt.data.table()
Volume_warm<-Volume_warm[Volume_warm$value>=0.01]
Volume_warm$value = Volume_warm$value*100
Volume_hot<-read_excel('./inputs/inputs/Incremental_volume_hot.xlsx')%>% as.data.table() %>%melt.data.table()
Volume_hot<-Volume_hot[Volume_hot$value>=0.01]
Volume_hot$value = Volume_hot$value*100

tt <- unique(Volume$variable)
tt.col <- hcl.colors(length(tt), palette = 'Blues')
names(tt.col)  <- tt

tt_response_cold <- media_vars_names_pl_cold
tt_response_cold.col <- hcl.colors(length(tt_response_cold), palette = 'Blues')
names(tt_response_cold.col)  <- tt_response_cold


tt_warm <- unique(Volume$variable)
tt_warm.col <- hcl.colors(length(tt_warm), palette = 'Oranges')
names(tt_warm.col)  <- tt_warm

tt_response_warm <- media_vars_names_pl_warm
tt_response_warm.col <- hcl.colors(length(tt_response_warm), palette = 'Oranges')
names(tt_response_warm.col)  <- tt_response_warm

tt_hot <- unique(Volume$variable)
tt_hot.col <- hcl.colors(length(tt_hot), palette = 'Reds')
names(tt_hot.col)  <- tt_hot

tt_response_hot <- media_vars_names_pl_hot
tt_response_hot.col <- hcl.colors(length(tt_response_hot), palette = 'Reds')
names(tt_response_hot.col)  <- tt_response_hot

yy <- unique(Value$variable)
yy.col <- hcl.colors(length(yy), palette = 'Blues')
names(yy.col)  <- yy

Value_rev = Value
Value_rev[3:4] = Value[1:2]
Value_rev[1:2] = Value[3:4]


ggplot(Value, aes(fill = reorder(Value$variable, desc(Value$variable)), y = Date, x =  value)) +
  # ggtitle("Incremental Volume - Registrations") +
  geom_bar(stat = "identity") +
  geom_col(colour = "black", position = position_stack()) +
  geom_text(aes(label = paste0(round(value, 1), "%")),
            position = position_stack(vjust = .3)
  ) +
  annotate("text", x = 105, y = 1, label = "17.6 MN Units", color = 'white', fontface = "bold") +
  annotate("text", x = 105, y = 2, label = "19.1 MN Units", color = 'white', fontface = "bold") +
  theme_classic() +
  theme(
    panel.background = element_rect(fill = "transparent") # bg of the panel
    , plot.background = element_rect(fill = "transparent", color = NA) # bg of the plot
    , panel.grid.major = element_blank() # get rid of major grid
    , panel.grid.minor = element_blank() # get rid of minor grid
    , legend.background = element_rect(fill = "transparent") # get rid of legend bg
    , legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
  ) +
  theme(axis.text.x = element_text(colour = "#ba9765")) +
  theme(legend.title = element_blank()) +
  theme(axis.text.y = element_text(colour = "#ba9765")) +
  theme(axis.title.x = element_text(colour = "#cdcdcd")) +
  theme(axis.title.y = element_text(colour = "#cdcdcd")) +
  theme(legend.title = element_text(colour = "#cdcdcd")) +
  theme(legend.title = element_blank()) +
  theme(legend.text = element_text(colour = "#cdcdcd", size = 12)) +
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank()) +
  # theme(legend.text = element_blank())+
  theme(axis.line.x = element_blank()) +
  theme(axis.text.x = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  theme(axis.title.x = element_blank()) +
  theme(plot.title = element_text(color = "#cdcdcd")) +
  theme(axis.title = element_text(size = 15)) +
  theme(axis.text.y = element_text(size = 12)) + 
  theme(plot.title = element_text(size = 16)) +
  scale_fill_manual(values = yy.col) +
  guides(fill = guide_legend(reverse = F)) +
  theme(axis.title.y = element_blank())
ggplotly()%>%
  layout(legend = list(orientation = "h", x = 0.3, y = 0))



str1 <- "Step 1: Specify budget"
str2 <- "Step 2: Specify time period in weeks"
str3 <- "Step 3: Select channels"
str4 <- "Step 4: Optional, Select upper and lower bound"
str5 <- "Step 5: Press ‘Calculate!’"
str6 <- "Step 6: Wait until the message 'Calculation Done' appears. This might take a few minutes"

str1_2 <- "Step 1: Specify budget"
str2_2 <- "Step 2: Specify time period in weeks"
str3_2<- "Step 3: Select channels"
str4_2 <- "Step 4: Press ‘Calculate!’"
str5_2 <- "Step 5: Wait until the message 'Calculation Done' appears. This might take a few minutes"
# Тут тоже

choiceVec_0 <- list("Audio" = '1' ,
                    "Direct Display" = '2',
                    "OLV"='3' ,
                    "OTT"='4',
                    "Prog Display"='5',
                    "Prog OLV"='6',
                    "Partnership"='7',
                    "Youtube DD"='8',
                    "Youtube OLV"='9',
                    "Primary Search"='10',
                    "Secondary Search"='11',
                    "Digital OOH"='12',
                    "Other Social"='13',
                    "Facebook"='14')







choiceVec_0_2 <- list("Audio" = '1' ,
                      "DD" = '2',
                      "OLV"='3' ,
                      "OTT"='4',
                      "PDM"='5',
                      "POLV"='6',
                      "Partnership"='7',
                      "YT_DD"='8',
                      "YT_OLV"='9',
                      "Primary_Search"='10',
                      "Secondary_Search"='11',
                      "DOOH"='12',
                      "Social"='13',
                      "Facebook"='14')

choiceVec_0_2_sep <- list('Audio_cold' = '1',
                          'YouTube_DD_cold' = '2',
                          'YouTube_DD_warm' = '3',
                          'YouTube_OLV_cold' = '4',
                          'YouTube_OLV_warm' = '5',
                          'OTT_cold' = '6',
                          'OTT_warm' = '7',
                          'PDM_cold' = '8',
                          'PDM_hot' = '9',
                          'PDM_warm' = '10',
                          'POLV_cold' = '11',
                          'POLV_hot' = '12',
                          'POLV_warm' = '13',
                          'OOH_warm' = '14',
                          'Partnership_cold' = '15',
                          'Partnership_warm' = '16',
                          'Primary_Search_hot' = '17',
                          'Secondary_Search_hot' = '18',
                          'Facebook_cold' = '19',
                          'Facebook_hot' = '20',
                          'Facebook_warm' = '21',
                          'Social_cold' = '22',
                          'Social_warm' = '23',
                          'Social_hot' = '24',
                          'DD_cold' = '25',
                          'DD_hot' = '26',
                          'DD_warm' = '27',
                          'OLV_cold' = '28',
                          'OLV_warm' = '29')

media_vars_names_resp_sep <<- unlist(lapply(names(choiceVec_0_sep_4opt), function(i) {paste0(i, '_R')}))

fit <- fread(paste0('./inputs/inputs/', list.files('./inputs/inputs/', pattern='cereals_predictions.csv'))) 
fitted<-select(fit, Date, actual, fitted)
fitted$Date<-as.Date(fitted$Date, format = "%d.%m.%Y")

ROAS_table <- fread(paste0('./inputs/inputs/', list.files('./inputs/inputs/', pattern='ROAS_table.csv')),header = T) 
ROAS_table_cold <- fread(paste0('./inputs/inputs/', list.files('./inputs/inputs/', pattern='ROAS_table_cold.csv')),header = T) 
ROAS_table_warm <- fread(paste0('./inputs/inputs/', list.files('./inputs/inputs/', pattern='ROAS_table_warm.csv')),header = T) 
ROAS_table_hot <- fread(paste0('./inputs/inputs/', list.files('./inputs/inputs/', pattern='ROAS_table_hot.csv')),header = T) 


cols <- c("MROAS = 1"="#f04546","Jan-Aug 2019"="#3591d1","Jan-Aug 2020"="#35ced1")


levels(ad_optimal_splits_BRAND_sep$Value) <- list(
  `Audio - Cold` = 'Audio_cold', `YouTube DD - Cold` = 'YouTube_DD_cold', `YouTube DD - Warm` = 'YouTube_DD_warm', `YouTube OLV - Cold` = 'YouTube_OLV_cold',
  `YouTube OLV - Warm` = 'YouTube_OLV_warm', `OTT - Cold` = 'OTT_cold', `OTT - Warm` = 'OTT_warm', `Prog Display - Cold` = 'PDM_cold', `Prog Display - Hot` = 'PDM_hot',
  `Prog Display - Warm` ='PDM_warm', `Prog OLV - Cold` = 'POLV_cold', `Prog OLV - Hot` = 'POLV_hot', `Prog OLV - Warm` = 'POLV_warm', OOH = 'OOH_warm', `Partnership - Cold` = 'Partnership_cold', 
  `Partnership - Warm` = 'Partnership_warm', `Primary Search - Hot` = 'Primary_Search_hot',`Secondary Search - Hot` =  'Secondary_Search_hot', 
  `Facebook - Cold` = 'Facebook_cold', `Facebook - Hot` = 'Facebook_hot', `Facebook - Warm` = 'Facebook_warm', `Other Social - Cold` = 'Social_cold', 
  `Other Social - Warm` = 'Social_warm' , `Other Social - Hot` = 'Social_hot', `Direct Display - Cold` = 'DD_cold', `Direct Display - Hot` = 'DD_hot', `Direct Display - Warm` = 'DD_warm', 
  `OLV - Cold` = 'OLV_cold', `OLV - Warm` = 'OLV_warm')


ggplot(Value_rev, aes(fill = reorder(Value$variable, desc(Value$variable)), y = Date, x = value)) +
  # ggtitle("Incremental Volume - Registrations") +
  geom_bar(stat = "identity") +
  geom_col(colour = "black", position = position_stack(reverse = F)) +
  geom_text(aes(label = paste0(round(value, 1), "%")),
            position = position_stack(vjust = .3, reverse = F)
  ) +
  annotate("text", x = 105, y = 1, label = "17.6 MN Units", color = 'white', fontface = "bold") +
  annotate("text", x = 105, y = 2, label = "19.1 MN Units", color = 'white', fontface = "bold") +
  theme_classic() +
  theme(
    panel.background = element_rect(fill = "transparent") # bg of the panel
    , plot.background = element_rect(fill = "transparent", color = NA) # bg of the plot
    , panel.grid.major = element_blank() # get rid of major grid
    , panel.grid.minor = element_blank() # get rid of minor grid
    , legend.background = element_rect(fill = "transparent") # get rid of legend bg
    , legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
  ) +
  theme(axis.text.x = element_text(colour = "#ba9765")) +
  theme(legend.title = element_blank()) +
  theme(axis.text.y = element_text(colour = "#ba9765")) +
  theme(axis.title.x = element_text(colour = "#cdcdcd")) +
  theme(axis.title.y = element_text(colour = "#cdcdcd")) +
  theme(legend.title = element_text(colour = "#cdcdcd")) +
  theme(legend.title = element_blank()) +
  theme(legend.text = element_text(colour = "#cdcdcd", size = 12)) +
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank()) +
  # theme(legend.text = element_blank())+
  theme(axis.line.x = element_blank()) +
  theme(axis.text.x = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  theme(axis.title.x = element_blank()) +
  theme(plot.title = element_text(color = "#cdcdcd")) +
  theme(axis.title = element_text(size = 15)) +
  theme(axis.text.y = element_text(size = 12)) + 
  theme(plot.title = element_text(size = 16)) +
  scale_fill_manual(values = yy.col) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme(axis.title.y = element_blank())
ggplotly()%>%
  layout(legend = list(orientation = "h", x = 0.3, y = 0))


