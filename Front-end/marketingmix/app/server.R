# Define libraries required by the dashboard
# libraries <- c(
#   "data.table", "ggplot2", "gridExtra", "plotly", "shiny",
#   "shinydashboard", "tableHTML", "dashboardthemes",
#   "shinyWidgets", "shinyjs", "shinymanager", "shinycssloaders"
# )
#
# invisible(lapply(libraries, require, character.only = T))

library("data.table")
library("ggplot2")
library("gridExtra")
library("plotly")
library("shiny")
library("shinydashboard")
library("tableHTML")
library("dashboardthemes")
library("shinyWidgets")
library("shinyjs")
library("shinymanager")
library("shinycssloaders")
library("minpack.lm")
library('corrplot')
library('lubridate')
library('pbapply')
library('Rsolnp')
library('shinyalert')
library('scales')

# credentials for auth
credentials <- data.frame(
  user = c("admin"), # mandatory
  password = c("PubP@sswd!"), # mandatory
  stringsAsFactors = FALSE
)
# Define server logic
shinyServer(function(input, output, session) {

  # # AUTH PART
  # res_auth <- secure_server(
  #   check_credentials = check_credentials(credentials)
  # )
  # output$auth_output <- renderPrint({
  #   reactiveValuesToList(res_auth)
  # })
  #
  observe({
    print(input$shinymanager_where)
    print(input$shinymanager_language)
  })

  useShinyjs()



  output$Re_model_value <- renderPlotly({

    ggplot(Value_rev, aes(fill = reorder(Value$variable, desc(Value$variable)), y = Date, x = value)) +
      # ggtitle("Incremental Volume - Registrations") +
      geom_bar(stat = "identity", show.legend = FALSE) +
      geom_col(colour = "black", position = position_stack(reverse = F), show.legend = FALSE) +
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
      theme(legend.text = element_blank())+
      theme(axis.line.x = element_blank()) +
      theme(axis.text.x = element_blank()) +
      theme(axis.ticks.x = element_blank()) +
      theme(axis.title.x = element_blank()) +
      theme(plot.title = element_text(color = "#cdcdcd")) +
      theme(axis.title = element_text(size = 15)) +
      theme(axis.text.y = element_text(size = 12)) +
      theme(plot.title = element_text(size = 16)) +
      scale_fill_manual(values = yy.col)
      guides(fill = guide_legend(reverse = TRUE)) +
      theme(axis.title.y = element_blank())
    ggplotly()%>%
      layout(legend = list(orientation = "h", x = 0.3, y = 0))%>%
      reverse_legend_labels()
  })

  output$Re_model_volume <- renderPlotly({
    ggplot(Volume, aes(fill = variable, y = Date, x = value)) +
      # ggtitle("Incremental Volume - By Channels (%)") +
      geom_bar(stat = "identity", show.legend = FALSE) +
      geom_col(colour = "black", position = position_stack(reverse = F), show.legend = FALSE) +
      geom_text(aes(label = paste0(round(value), "%")),
                position = position_stack(vjust = .3, reverse = F)
      ) +
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
      theme(axis.line.x = element_blank()) +
      theme(axis.text.x = element_blank()) +
      theme(axis.ticks.x = element_blank()) +
      theme(axis.title.x = element_blank()) +
      theme(plot.title = element_text(color = "#cdcdcd")) +
      theme(axis.title = element_text(size = 15)) +
      theme(axis.text.y = element_text(size = 12)) +
      theme(plot.title = element_text(size = 16)) +
      scale_fill_manual(values = tt.col) +
      guides(fill = guide_legend(reverse = F)) +
      theme(axis.title.y = element_blank())
    ggplotly()%>%
      layout(legend = list(orientation = "h", x = 0, y = 0))%>%
      reverse_legend_labels()
  })

  output$Re_model_volume_cold <- renderPlotly({
    ggplot(Volume_cold, aes(fill = variable, y = value, x = Date)) +
      theme(legend.position = "none",
            panel.grid = element_blank(),
            axis.title = element_blank(),
            axis.text.y = element_blank()) +
      # ggtitle("Cold") +
      theme(plot.title = element_text(hjust = 0.5)) +
      geom_bar(stat = "identity", show.legend = FALSE) +
      geom_col(colour = "black", position = position_stack(reverse = F), aes(label = paste0(round(value), "%")), show.legend = FALSE) +
      geom_text(aes(label = paste0(round(value), "%")),
                position = position_stack(vjust = .3, reverse = F)
      ) +
      geom_text(
        aes(label = paste0(round(stat(y)), ' %'), group = Date),
        stat = 'summary', fun = sum, vjust = 0, colour = 'white'
      ) +
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
      theme(axis.title.x = element_text(colour = "#cdcdcd")) +
      theme(legend.title = element_text(colour = "#cdcdcd")) +
      theme(legend.title = element_blank()) +
      theme(legend.text = element_text(colour = "#cdcdcd", size = 12)) +
      theme(legend.position = "bottom") +
      theme(legend.title = element_blank()) +
      theme(axis.line.x = element_blank()) +
      theme(axis.text.x = element_text(size = 12)) +
      theme(axis.ticks.x = element_blank()) +
      theme(axis.title.x = element_blank()) +
      theme(plot.title = element_text(color = "#cdcdcd")) +
      # theme(axis.title = element_text(size = 15)) +
      theme(plot.title = element_text(size = 16)) +
      scale_fill_manual(values = tt.col) +
      guides(fill = guide_legend(reverse = TRUE))
    # theme(panel.border = element_rect(fill = NA, colour = 'blue1', size = 2))
    # ggplotly()%>%
    #   layout(legend = list(orientation = "h", x = 0, y = -0.1))%>%
    #   style(textposition = "top")
  })

  output$Re_model_volume_warm <- renderPlotly({
    ggplot(Volume_warm, aes(fill = variable, y = value, x = Date)) +
      theme(legend.position = "none",
            panel.grid = element_blank(),
            axis.title = element_blank(),
            axis.text.y = element_blank()) +
      # ggtitle("Warm") +
      theme(plot.title = element_text(hjust = 0.5)) +
      geom_bar(stat = "identity", show.legend = FALSE) +
      geom_col(colour = "black", position = position_stack(reverse = F), aes(label = paste0(round(value), "%")), show.legend = FALSE) +
      geom_text(aes(label = paste0(round(value), "%")),
                position = position_stack(vjust = .3, reverse = F)
      )+
      geom_text(
        aes(label = paste0(round(stat(y)), ' %'), group = Date),
        stat = 'summary', fun = sum, vjust = 0, colour = 'white'
      )+
      theme(
        panel.background = element_rect(fill = "transparent") # bg of the panel
        , plot.background = element_rect(fill = "transparent", color = NA) # bg of the plot
        , panel.grid.major = element_blank() # get rid of major grid
        , panel.grid.minor = element_blank() # get rid of minor grid
        , legend.background = element_rect(fill = "transparent") # get rid of legend bg
        , legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
      ) +
      theme(axis.text.x = element_text(colour = "#ba9765")) +
      # theme(legend.title = element_blank()) +
      theme(axis.title.x = element_text(colour = "#cdcdcd")) +
      theme(legend.title = element_text(colour = "#cdcdcd")) +
      theme(legend.title = element_blank()) +
      theme(legend.text = element_text(colour = "#cdcdcd", size = 12)) +
      theme(legend.position = "bottom") +
      theme(legend.title = element_blank()) +
      theme(axis.line.x = element_blank()) +
      theme(axis.text.x = element_text(size = 12)) +
      theme(axis.ticks.x = element_blank()) +
      theme(axis.title.x = element_blank()) +
      theme(plot.title = element_text(color = "#cdcdcd")) +
      # theme(axis.title = element_text(size = 15)) +
      # theme(axis.text.y = element_text(size = 12)) +
      theme(plot.title = element_text(size = 16)) +
      scale_fill_manual(values = tt_warm.col) +
      guides(fill = guide_legend(reverse = TRUE)
             )
    # theme(panel.border = element_rect(fill = NA, colour = 'goldenrod3', size = 2))
    # ggplotly()%>%
    #   layout(legend = list(orientation = "h", x = 0, y = -0.1))%>%
    #   style(textposition = "top")
  })

  output$Re_model_volume_hot <- renderPlotly({
      ggplot(Volume_hot, aes(fill = variable, y = value, x = Date)) +
          theme(legend.position = "none",
                panel.grid = element_blank(),
                axis.title = element_blank(),
                axis.text.y = element_blank()) +
          # ggtitle("Warm") +
          theme(plot.title = element_text(hjust = 0.5)) +
          geom_bar(stat = "identity", show.legend = FALSE) +
          geom_col(colour = "black", position = position_stack(reverse = F), aes(label = paste0(round(value), "%")), show.legend = FALSE) +
          geom_text(aes(label = paste0(round(value), "%")),
                    position = position_stack(vjust = .3, reverse = F)
          )+
          geom_text(
              aes(label = paste0(round(stat(y)), ' %'), group = Date),
              stat = 'summary', fun = sum, vjust = 0, colour = 'white'
          )+
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
          theme(axis.title.x = element_text(colour = "#cdcdcd")) +
          theme(legend.title = element_text(colour = "#cdcdcd")) +
          theme(legend.title = element_blank()) +
          theme(legend.text = element_text(colour = "#cdcdcd", size = 12)) +
          theme(legend.position = "bottom") +
          theme(legend.title = element_blank()) +
          theme(axis.line.x = element_blank()) +
          theme(axis.text.x = element_text(size = 12)) +
          theme(axis.ticks.x = element_blank()) +
          theme(axis.title.x = element_blank()) +
          theme(plot.title = element_text(color = "#cdcdcd")) +
          # theme(axis.title = element_text(size = 15)) +
          # theme(axis.text.y = element_text(size = 12)) +
          theme(plot.title = element_text(size = 16)) +
          scale_fill_manual(values = tt_hot.col) +
          guides(fill = guide_legend(reverse = TRUE)
          )
      # theme(panel.border = element_rect(fill = NA, colour = 'goldenrod3', size = 2))
      ggplotly()
        # layout(legend = list(orientation = "h", x = 0, y = -0.1))
        # style(textposition = "top")
  })

  output$ks_curves <- renderUI({
    fluidPage(
      # fluidRow(box(
      #   # title = "Response curves",
      #   solidHeader = FALSE, collapsible = TRUE,
      #   plotlyOutput("Response_BRAND", height = 700), width = 12
      # )),
      fluidRow(box(
        # title = "Response curves",
        solidHeader = FALSE, collapsible = FALSE,
        valueBox(
          value = tags$p("Response Curves - Hot", style = "font-size: 50%; text-align:center;"),
          subtitle = NULL,
          color = "red",
          icon = NULL,
          width = 12
        ),
        fluidRow(
          plotlyOutput("Response_BRAND_hot", height = 700), width = 12, style = "padding-top: 50px; "), width = 12),
        box(
          solidHeader = FALSE, collapsible = FALSE,
          valueBox(
            value = tags$p("Response Curves - Warm", style = "font-size: 50%; text-align:center;"),
            subtitle = NULL,
            color = "yellow",
            icon = NULL,
            width = 12
          ),
          fluidRow(
            plotlyOutput("Response_BRAND_warm", height = 700), width = 12, style = "padding-top: 50px; "), width = 12),
        box(
          solidHeader = FALSE, collapsible = FALSE,
          valueBox(
            value = tags$p("Response Curves - Cold", style = "font-size: 50%; text-align:center;"),
            subtitle = NULL,
            color = "blue",
            icon = NULL,
            width = 12
          ),
          fluidRow(
            plotlyOutput("Response_BRAND_cold", height = 700),width = 12, style = "padding-top: 50px; "), width = 12
        )
      )
    )
  })
  output$ks_curves_2 <- renderUI({
    fluidPage(
      fluidRow(box(
        valueBox(
          value = tags$p("ROAS by Channels - Overall", style = "font-size: 50%;text-align:center;"),
          subtitle = NULL,
          color = "aqua",
          icon = NULL,
          width = 12
        ), width = 12,
        br(),
        # title = "ROAS Overall",
        solidHeader = FALSE, collapsible = FALSE,
        plotlyOutput("ROAS_BRAND", height = 500),
        tableHTML_output("ROAS_table")
      )),
      fluidRow(box(
        valueBox(
          value = tags$p("ROAS by Channels - Hot", style = "font-size: 50%;text-align:center;"),
          subtitle = NULL,
          color = "red",
          icon = NULL,
          width = 12
        ), width = 12,
        br(),
        fluidRow(
          # title = "ROAS barplot",
          solidHeader = FALSE, collapsible = TRUE,
          plotlyOutput("ROAS_BRAND_hot", height = 500),
          tableHTML_output("ROAS_table_hot"), width = 12, style = "padding-top: 20px; padding-left: 12px;")),
        box(
          valueBox(
            value = tags$p("ROAS by Channels - Warm", style = "font-size: 50%; text-align:center"),
            subtitle = NULL,
            color = "yellow",
            icon = NULL,
            width = 12
          ), width = 12,
          fluidRow(
            solidHeader = FALSE, collapsible = TRUE,
            plotlyOutput("ROAS_BRAND_warm", height = 500),
            tableHTML_output("ROAS_table_warm"), width = 12, style = "padding-top: 50px; padding-left: 12px;")),
        box(
          valueBox(
            value = tags$p("ROAS by Channels - Cold", style = "font-size: 50%; text-align:center"),
            subtitle = NULL,
            color = "blue",
            icon = NULL,
            width = 12), width = 12,

          fluidRow(
            plotlyOutput("ROAS_BRAND_cold", height = 500),
            tableHTML_output("ROAS_table_cold"), width = 12, style = "padding-top: 50px; padding-left: 12px;"
          )))
    )
  })

  # NAMES ONE
  output$ks_slider_22th <- renderUI({box(
    fluidPage(
      fluidRow(
        chooseSliderSkin("Nice"),
        h2('Welcome to the Dynamic Optimization page!'),
        h4(HTML(paste0("This segment uses model results to calculate optimal budget split to maximize short-term digital paid media sales. To calculate the optimal split, please use the instructions in ‘How To Use’ box")), div(style = "height:25px;")),


        column(

          sliderInput("twBudget_cold", "Choose Budget for cold channels:",
                      min = 1, max = 150, value = 1, step = 1
          ),
          sliderInput("Daterange_cold", "Choose Period for cold channels (Weeks):",
                      min = 1, max = 54, value = 1, step = 1
          ),
          h6(strong('Select cold Channels: '), style = "font-size: 105%; text-align:center;"),
          multiInput('selector_cold',
                     choices=names(choiceVec_0_sep_cold), label = NULL),




          sliderInput("twBudget_warm", "Choose Budget for warm channels:",
                      min = 1, max = 150, value = 1, step = 1
          ),
          sliderInput("Daterange_warm", "Choose Period for warm channels (Weeks):",
                      min = 1, max = 54, value = 1, step = 1
          ),
          h6(strong('Select warm Channels: '), style = "font-size: 105%; text-align:center;"),
          multiInput('selector_warm',
                     choices=names(choiceVec_0_sep_warm), label = NULL),



          sliderInput("twBudget_hot", "Choose Budget for hot channels:",
                      min = 1, max = 150, value = 1, step = 1
          ),
          sliderInput("Daterange_hot", "Choose Period for hot channels (Weeks):",
                      min = 1, max = 54, value = 1, step = 1
          ),
          h6(strong('Select hot Channels: '), style = "font-size: 105%; text-align:center;"),
          multiInput('selector_hot',
                     choices=names(choiceVec_0_sep_hot), label = NULL),



          h6(strong(em('The total budget should not exceed the sum of upperbounds for channels!')), style = "font-size: 110%;
           color: #ba9765;"),
          width = 3),
        column(uiOutput("times"),width = 3),
        chooseSliderSkin("Nice"),
        # column(sliderInput("Daterange_2_cold", "Choose Period (Weeks):",
        #                    min = 1, max = 54, value = 1, step = 1
        # ),
        # sliderInput("Daterange_2_warm", "Choose Period (Weeks):",
        #             min = 1, max = 54, value = 1, step = 1
        # ),
        # sliderInput("Daterange_2_hot", "Choose Period (Weeks):",
        #             min = 1, max = 54, value = 1, step = 1
        # ),
        # uiOutput("times21"),width = 3),
        column(valueBox(
          value = tags$p("How To Use", style = "font-size: 75%;"),
          subtitle = tags$p(HTML(paste(str1, str2, str3, str4, str5, str6, sep = "<br/>")), style = "font-size: 130%;"),
          color = "teal",
          icon = icon("book-reader"),
          width = NULL
        ), width = 5)
      ),


      fluidRow(column(actionBttn("Button", "Calculate!", style = "simple"), width = 4, div(style = "height:25px;", "This might take a few minutes")),
               column(
                 shinyjs::hidden(
                   div(
                     id = "secondary_content",
                     "Processing...", style = "font-size: 130%;"
                   )
                 ),
                 shinyjs::hidden(
                   div(
                     id = "main_content",
                     "Calculation Done!", style = "font-size: 130%;"
                   )
                 )
                 , width = 6)
      )), width = 12, collapsible = TRUE)
  })




  output$ks_slider_22th_sep <- renderUI({box(
    fluidPage(
      fluidRow(
        chooseSliderSkin("Nice"),
        # h2('Welcome to the Dynamic Optimization page!'),
        # h4(HTML(paste0("This segment uses model results to calculate optimal budget split to maximize short-term digital paid media sales. To calculate the optimal split, please use the instructions in ‘How To Use’ box")), div(style = "height:25px;")),
        column(sliderInput("twBudget_2_sep", "Choose Budget:",
                           min = 1, max = 150, value = 1, step = 1
        ),




        width = 3),
        chooseSliderSkin("Nice"),
        column(sliderInput("Daterange_2_sep", "Choose Period (Weeks):",
                           min = 1, max = 54, value = 1, step = 1
        ),
        width = 3),
        column(valueBox(
          value = tags$p("How To Use", style = "font-size: 75%;"),
          subtitle = tags$p(HTML(paste(str1, str2, str3, str4, str5, str6, sep = "<br/>")), style = "font-size: 130%;"),
          color = "teal",
          icon = icon("book-reader"),
          width = NULL
        ), width = 5)
      ),
      fluidRow(

        column(3,
               # h6(strong('Select cold Channels: '), style = "font-size: 105%; height:0px"),
               valueBox(
                 value = tags$p("Select Cold Channels: ", style = "font-size: 40%; text-align:center;"),
                 subtitle = NULL,
                 color = "blue",
                 icon = NULL,
                 width = NULL
               ),
               multiInput('selector_2_sep_cold',
                          choices=names(choiceVec_0_sep_cold_1), label = NULL),
               uiOutput("times_sep_cold")),

        column(3,
               # h6(strong('Select warm Channels: '), style = "font-size: 105%; height:0px"),
               valueBox(
                 value = tags$p("Select Warm Channels: ", style = "font-size: 40%; text-align:center;"),
                 subtitle = NULL,
                 color = "yellow",
                 icon = NULL,
                 width = NULL
               ),
               multiInput('selector_2_sep_warm',
                          choices=names(choiceVec_0_sep_warm_1), label = NULL),
               uiOutput("times_sep_warm")),

        column(3,
               # h6(strong('Select hot Channels: '), style = "font-size: 105%; height:0px"),
               valueBox(
                 value = tags$p("Select Hot Channels: ", style = "font-size: 40%; text-align:center;"),
                 subtitle = NULL,
                 color = "red",
                 icon = NULL,
                 width = NULL
               ),
               multiInput('selector_2_sep_hot',
                          choices=names(choiceVec_0_sep_hot_1), label = NULL),
               uiOutput("times_sep_hot"))),



      # h6(strong(em('The total budget should not exceed the sum of upperbounds for channels!')), style = "font-size: 110%;
      #      color: #ba9765;"),


      fluidRow(column(actionBttn("Button_2_sep", "Calculate!", style = "simple"), width = 4),# div(style = "height:25px;", "This might take a few minutes")),
               column(
                 shinyjs::hidden(
                   div(
                     id = "secondary_content_2_sep",
                     "Processing...", style = "font-size: 130%;"
                   )
                 ),
                 shinyjs::hidden(
                   div(
                     id = "main_content_2_sep",
                     "Calculation Done!", style = "font-size: 130%;"
                   )
                 )
                 , width = 6)
      )), width = 12, collapsible = F)
  })










  output$ks_slider_12th <- renderUI({box(
    fluidPage(
      fluidRow(
        chooseSliderSkin("Nice"),
        h2('Welcome to the Dynamic Optimization page!'),
        h4(HTML(paste0("This segment uses model results to calculate optimal budget split to maximize short-term digital paid media sales. To calculate the optimal split, please use the instructions in ‘How To Use’ box")), div(style = "height:25px;")),


        column(

          sliderInput("twBudget_2_cold", "Choose Budget for cold channels:",
                      min = 1, max = 150, value = 1, step = 1
          ),
          sliderInput("Daterange_2_cold", "Choose Period for cold channels (Weeks):",
                      min = 1, max = 54, value = 1, step = 1
          ),
          h6(strong('Select cold Channels: '), style = "font-size: 105%; height:0px"),
          multiInput('selector_2_cold',
                     choices=names(choiceVec_0_sep_cold), label = NULL),




          sliderInput("twBudget_2_warm", "Choose Budget for warm channels:",
                      min = 1, max = 150, value = 1, step = 1
          ),
          sliderInput("Daterange_2_warm", "Choose Period for warm channels (Weeks):",
                      min = 1, max = 54, value = 1, step = 1
          ),
          h6(strong('Select warm Channels: '), style = "font-size: 105%; height:0px"),
          multiInput('selector_2_warm',
                     choices=names(choiceVec_0_sep_warm), label = NULL),



          sliderInput("twBudget_2_hot", "Choose Budget for hot channels:",
                      min = 1, max = 150, value = 1, step = 1
          ),
          sliderInput("Daterange_2_hot", "Choose Period for hot channels (Weeks):",
                      min = 1, max = 54, value = 1, step = 1
          ),
          h6(strong('Select hot Channels: '), style = "font-size: 105%; height:0px"),
          multiInput('selector_2_hot',
                     choices=names(choiceVec_0_sep_hot), label = NULL),



          h6(strong(em('The total budget should not exceed the sum of upperbounds for channels!')), style = "font-size: 110%;
           color: #ba9765;"),
          width = 3),
        chooseSliderSkin("Nice"),
        # column(sliderInput("Daterange_2_cold", "Choose Period (Weeks):",
        #                    min = 1, max = 54, value = 1, step = 1
        # ),
        # sliderInput("Daterange_2_warm", "Choose Period (Weeks):",
        #             min = 1, max = 54, value = 1, step = 1
        # ),
        # sliderInput("Daterange_2_hot", "Choose Period (Weeks):",
        #             min = 1, max = 54, value = 1, step = 1
        # ),
        # uiOutput("times21"),width = 3),
        column(valueBox(
          value = tags$p("How To Use", style = "font-size: 75%;"),
          subtitle = tags$p(HTML(paste(str1_2, str2_2, str3_2, str4_2, str5_2, sep = "<br/>")), style = "font-size: 130%;"),
          color = "teal",
          icon = icon("book-reader"),
          width = NULL
        ), width = 5)
      ),


      fluidRow(column(actionBttn("Button_2", "Calculate!", style = "simple"), width = 4, div(style = "height:25px;", "This might take a few minutes")),
               column(
                 shinyjs::hidden(
                   div(
                     id = "secondary_content_2",
                     "Processing...", style = "font-size: 130%;"
                   )
                 ),
                 shinyjs::hidden(
                   div(
                     id = "main_content_2",
                     "Calculation Done!", style = "font-size: 130%;"
                   )
                 )
                 , width = 6)
      )), width = 12, collapsible = TRUE)
  })




  output$ks_slider_12th_sep <- renderUI({box(
    fluidPage(
      fluidRow(
        chooseSliderSkin("Nice"),
        # h2('Welcome to the Dynamic Optimization page!'),
        # h4(HTML(paste0("This segment uses model results to calculate optimal budget split to maximize short-term digital paid media sales. To calculate the optimal split, please use the instructions in ‘How To Use’ box")), div(style = "height:25px;")),
        column(sliderInput("twBudget_sep", "Choose Budget:",
                           min = 1, max = 150, value = 1, step = 1
        ), width = 3),




        chooseSliderSkin("Nice"),
        column(sliderInput("Daterange_sep", "Choose Period (Weeks):",
                           min = 1, max = 54, value = 1, step = 1
        ),
        uiOutput("times21_sep"),width = 3),
        column(valueBox(
          value = tags$p("How To Use", style = "font-size: 75%;"),
          subtitle = tags$p(HTML(paste(str1_2, str2_2, str3_2, str4_2, str5_2, sep = "<br/>")), style = "font-size: 130%;"),
          color = "teal",
          icon = icon("book-reader"),
          width = NULL
        ), width = 5)
      ),
      fluidRow(

        column(3,
               # h6(strong('Select cold Channels: '), style = "font-size: 105%; height:0px"),
               valueBox(
                 value = tags$p("Select Cold Channels: ", style = "font-size: 40%; text-align:center;"),
                 subtitle = NULL,
                 color = "blue",
                 icon = NULL,
                 width = NULL
               ),
               multiInput('selector_sep_cold',
                          choices=names(choiceVec_0_sep_cold_1), label = NULL)),

        column(3,
               # h6(strong('Select warm Channels: '), style = "font-size: 105%; height:0px"),
               valueBox(
                 value = tags$p("Select Warm Channels: ", style = "font-size: 40%; text-align:center;"),
                 subtitle = NULL,
                 color = "yellow",
                 icon = NULL,
                 width = NULL
               ),
               multiInput('selector_sep_warm',
                          choices=names(choiceVec_0_sep_warm_1), label = NULL)),

        column(3,
               # h6(strong('Select hot Channels: '), style = "font-size: 105%; height:0px"),
               valueBox(
                 value = tags$p("Select Hot Channels: ", style = "font-size: 40%; text-align:center;"),
                 subtitle = NULL,
                 color = "red",
                 icon = NULL,
                 width = NULL
               ),
               multiInput('selector_sep_hot',
                          choices=names(choiceVec_0_sep_hot_1), label = NULL))),



      # h6(strong(em('The total budget should not exceed the sum of upperbounds for channels!')), style = "font-size: 110%;
      #      color: #ba9765;"),


      fluidRow(column(actionBttn("Button_sep", "Calculate!", style = "simple"), width = 4),# div(style = "height:25px;", "This might take a few minutes")),
               column(
                 shinyjs::hidden(
                   div(
                     id = "secondary_content_sep",
                     "Processing...", style = "font-size: 130%;"
                   )
                 ),
                 shinyjs::hidden(
                   div(
                     id = "main_content_sep",
                     "Calculation Done!", style = "font-size: 130%;"
                   )
                 )
                 , width = 6)
      )), width = 12, collapsible = F)
  })


  # NAMES TWO
  output$times <- renderUI({
    test1_cold<<-input$selector_cold
    test1_warm<<-input$selector_warm
    test1_hot<<-input$selector_hot

    hh_cold<<-length(test1_cold)
    hh_warm<<-length(test1_warm)
    hh_hot<<-length(test1_hot)

    if((hh_cold + hh_warm + hh_hot)!=0)
      lapply(c(test1_cold, test1_warm, test1_hot), function(i) {
        list(sliderInput(inputId = paste0('slider', choiceVec_0_sep_help[[i]]),
                         label = i, min = 0,
                         max = 50, value = c(0, 50)))
      })

    else
      textOutput('Something went wrong')




  })


  observe({
    test1_cold<<-input$selector_2_cold
    test1_warm<<-input$selector_2_warm
    test1_hot<<-input$selector_2_hot
    hh_cold<<-length(test1_cold)
    hh_warm<<-length(test1_warm)
    hh_hot<<-length(test1_hot)
  })


  #
  #




  output$times_sep <- renderUI({
    selector_sep <<- c(input$selector_2_sep_cold, input$selector_2_sep_hot, input$selector_2_sep_warm)
    test2_1 <<- selector_sep
    hh2<<-length(test2_1)
    if(hh2!=0)
      lapply(test2_1, function(i) {
        list(sliderInput(inputId = paste0('slider', choiceVec_0_sep_help[[i]], '_sep'),
                         label = i, min = 0,
                         max = 50, value = c(0, 50)))
      })

    else
      textOutput('Something went wrong')


  })

  output$times_sep_cold <- renderUI({
    selector_sep_cold <<- c(input$selector_2_sep_cold)
    test2_1_cold <<- selector_sep_cold
    test2_1_cold <<- lapply(test2_1_cold, function(i) {paste0(i, ' - Cold')})
    hh2_cold<<-length(test2_1_cold)
    if(hh2_cold!=0)
      lapply(test2_1_cold, function(i) {
        list(sliderInput(inputId = paste0('slider', choiceVec_0_sep_help[[i]], '_sep'),
                         label = substr(i, 1, nchar(i)-7), min = 0,
                         max = 50, value = c(0, 50)))
      })

    else
      textOutput('Something went wrong')


  })

  output$times_sep_warm <- renderUI({
    selector_sep_warm <<- c(input$selector_2_sep_warm)
    test2_1_warm <<- selector_sep_warm
    test2_1_warm <<- lapply(test2_1_warm, function(i) {paste0(i, ' - Warm')})
    hh2_warm<<-length(test2_1_warm)
    if(hh2_warm!=0)
      lapply(test2_1_warm, function(i) {
        list(sliderInput(inputId = paste0('slider', choiceVec_0_sep_help[[i]], '_sep'),
                         label = substr(i, 1, nchar(i)-7), min = 0,
                         max = 50, value = c(0, 50)))
      })

    else
      textOutput('Something went wrong')


  })

  output$times_sep_hot <- renderUI({
    selector_sep_hot <<- c(input$selector_2_sep_hot)
    test2_1_hot <<- selector_sep_hot
    test2_1_hot <<- lapply(test2_1_hot, function(i) {paste0(i, ' - Hot')})
    hh2_hot<<-length(test2_1_hot)
    if(hh2_hot!=0)
      lapply(test2_1_hot, function(i) {
        list(sliderInput(inputId = paste0('slider', choiceVec_0_sep_help[[i]], '_sep'),
                         label = substr(i, 1, nchar(i)-6), min = 0,
                         max = 50, value = c(0, 50)))
      })

    else
      textOutput('Something went wrong')


  })



  observe({
    selector_2_sep_cold <<- c(input$selector_sep_cold)
    selector_2_sep_cold <<- lapply(selector_2_sep_cold, function(i) {paste0(i, ' - Cold')})

    selector_2_sep_warm <<- c(input$selector_sep_warm)
    selector_2_sep_warm <<- lapply(selector_2_sep_warm, function(i) {paste0(i, ' - Warm')})

    selector_2_sep_hot <<- c(input$selector_sep_hot)
    selector_2_sep_hot <<- lapply(selector_2_sep_hot, function(i) {paste0(i, ' - Hot')})

    selector_2_sep <<- c(selector_2_sep_cold, selector_2_sep_hot, selector_2_sep_warm)
    test2_1<<-selector_2_sep
    hh2<<-length(test2_1)
    # if(hh!=0)
    #   lapply(test1, function(i) {
    #     list(sliderInput(inputId = paste0('slider', choiceVec_0_2[[i]],'_2'),
    #                      label = i, min = 0,
    #                      max = 15, value = c(0, 15)))
    #   })
    #
    # else
    #   textOutput('Something went wrong')


  })




  # SHINYJS HIDING ----------------------------------------------------------



  observe({
    shinyjs::hide("ks_final_table")
    shinyjs::hide("ks_final_table_one")
    shinyjs::hide("Budget_BRAND_optimal_splits_cold")
    shinyjs::hide("Budget_BRAND_optimal_splits_warm")
    shinyjs::hide("Budget_BRAND_optimal_splits_hot")
    shinyjs::hide("Budget_BRAND_optimal_ROAS")
    hide('spinner_12th')
    hide('spinner_12th_cold')
    hide('spinner_12th_warm')
    hide('spinner_12th_hot')

    if (v_2$fintable == 1) {
      show('spinner_12th')
      show('spinner_12th_cold')
      show('spinner_12th_warm')
      show('spinner_12th_hot')
      shinyjs::show("ks_final_table")
      shinyjs::show("ks_final_table_one")
      shinyjs::show("Budget_BRAND_optimal_splits_cold")
      shinyjs::show("Budget_BRAND_optimal_splits_warm")
      shinyjs::show("Budget_BRAND_optimal_splits_hot")
      shinyjs::show("Budget_BRAND_optimal_ROAS")
    }
  })


  observe({
    shinyjs::hide("ks_final_table_2")
    shinyjs::hide("ks_final_table_2_one")
    shinyjs::hide("Budget_BRAND_optimal_splits_cold_2")
    shinyjs::hide("Budget_BRAND_optimal_splits_warm_2")
    shinyjs::hide("Budget_BRAND_optimal_splits_hot_2")
    shinyjs::hide("Budget_BRAND_optimal_ROAS_2")
    hide('spinner_22th')
    hide('spinner_22th_cold')
    hide('spinner_22th_warm')
    hide('spinner_22th_hot')

    if (v$fintable == 1) {
      show('spinner_22th')
      show('spinner_22th_cold')
      show('spinner_22th_warm')
      show('spinner_22th_hot')
      shinyjs::show("ks_final_table_2")
      shinyjs::show("ks_final_table_2_one")
      shinyjs::show("Budget_BRAND_optimal_splits_cold_2")
      shinyjs::show("Budget_BRAND_optimal_splits_warm_2")
      shinyjs::show("Budget_BRAND_optimal_splits_hot_2")
      shinyjs::show("Budget_BRAND_optimal_ROAS_2")
    }
  })







  observe({
    shinyjs::hide("ks_final_table_sep")
    shinyjs::hide("ks_final_table_one_sep")
    shinyjs::hide("ks_final_table_sep_title")
    shinyjs::hide("ks_final_table_sep_one_title")
    if (v_sep$fintable == 1) {
      shinyjs::show("ks_final_table_sep")
      shinyjs::show("ks_final_table_one_sep")
      shinyjs::show("ks_final_table_sep_title")
      shinyjs::show("ks_final_table_sep_one_title")
    }
  })


  observe({
    shinyjs::hide("ks_final_table_2_sep")
    shinyjs::hide("ks_final_table_2_one_sep")
    shinyjs::hide("ks_final_table_2_sep_title")
    shinyjs::hide("ks_final_table_2_sep_one_title")
    if (v_2_sep$fintable == 1) {
      shinyjs::show("ks_final_table_2_sep")
      shinyjs::show("ks_final_table_2_one_sep")
      shinyjs::show("ks_final_table_2_sep_title")
      shinyjs::show("ks_final_table_2_sep_one_title")
    }
  })



  # BOTTOM PLOTS SECTIONS ---------------------------------------------------


  output$ks_final_table_sep_title <- renderText({
    print("Optimal Budget Allocation - Weekly")
  })
  output$ks_final_table_sep_one_title <- renderText({
    print(paste0("Optimal Budget Allocation - ", input$Daterange_sep, " Week(s)"))
  })
  output$ks_final_table_2_sep_title <- renderText({
    print("Optimal Budget Allocation - Weekly")
  })
  output$ks_final_table_2_sep_one_title <- renderText({
    print(paste0("Optimal Budget Allocation - ", input$Daterange_2_sep, " Week(s)"))
  })


  output$ks_final_table_sep <- renderUI({
    box(
      # title = h3("Optimal Budget Allocation - Weekly" , div(style = "height:25px;")),
      fluidRow(
        valueBox(
          value = (paste0(round(input$twBudget_sep/input$Daterange_sep, 2) , " MN$")),
          subtitle = "Budget",
          color = "light-blue",
          icon = icon("credit-card"),
          width = 4
        ),
        valueBox(
          value = paste0(format(round((round(v_sep$data[[2]][budget_total == round_any(input$twBudget_sep/input$Daterange_sep, 0.5, f = floor)]$profit))), big.mark=",") , " Units"),
          subtitle = "Mobile Registrations From Digital Paid Media",
          color = "light-blue",
          icon = icon("mobile"),
          width = 4
        ),
        valueBox(
          value = paste0(format(round((round((v_sep$data[[2]][budget_total == round_any(input$twBudget_sep/input$Daterange_sep, 0.5, f = floor)]$profit * 635)/ 1000000))), big.mark=","), " MN$"),
          subtitle = "Revenue From Expected  Mobile Registrations",
          color = "light-blue",
          icon = icon("coins"),
          width = 4
        )
      ),
      fluidRow(plotlyOutput("rys_sep")),
      fluidRow(column(12, align = "center", tableOutput("fintab_sep"))),
      width = 12,
      h5('*Channels with with share >= 1% are included')
    )})



  output$ks_final_table_one_sep <- renderUI({
    box(
      # title = h3(paste0("Optimal Budget Allocation - ", input$Daterange_sep, " Week(s)"), div(style = "height:25px;")),
      fluidRow(
        valueBox(
          value = (paste0(input$twBudget_sep, " MN$")),
          subtitle = "Budget",
          color = "maroon",
          icon = icon("credit-card"),
          width = 4
        ),
        valueBox(
          value = paste0(format(round((round(v_sep$data[[2]][budget_total == round_any(input$twBudget_sep/input$Daterange_sep, 0.5, f = floor)]$profit) * input$Daterange_sep)), big.mark=","), " Units"),
          subtitle = "Mobile Registrations From Digital Paid Media",
          color = "maroon",
          icon = icon("mobile"),
          width = 4
        ),
        valueBox(
          value = paste0(format(round((round((v_sep$data[[2]][budget_total == round_any(input$twBudget_sep/input$Daterange_sep, 0.5, f = floor)]$profit * 635) * input$Daterange_sep / 1000000))), big.mark=","), " MN$"),
          subtitle = "Revenue From Expected  Mobile Registrations",
          color = "maroon",
          icon = icon("coins"),
          width = 4
        )
      ),
      fluidRow(plotlyOutput("rys_one_sep")),
      fluidRow(column(12, align = "center", tableOutput("fintab_one_sep"))),
      width = 12,
      h5('*Channels with with share >= 1% are included')
    )})



  # output$times21 <- renderUI({
  #   box(
  #     uiOutput("times_2"), width = 12)
  # })
  output$ks_final_table_2_sep <- renderUI({
    box(
      # title = h3("Optimal Budget Allocation - Weekly" , div(style = "height:25px;")),
      fluidRow(
        valueBox(
          value = (paste0(round(input$twBudget_2_sep/input$Daterange_2_sep, 2), " MN$")),
          subtitle = "Budget",
          color = "light-blue",
          icon = icon("credit-card"),
          width = 4
        ),
        valueBox(
          value = paste0(format(round(as.numeric(round(v_2_sep$data[[2]][budget_total == round_any(input$twBudget_2_sep/input$Daterange_2_sep, 0.5, f = floor)]$profit))), big.mark=","), " Units"),
          subtitle = "Mobile Registrations From Digital Paid Media",
          color = "light-blue",
          icon = icon("mobile"),
          width = 4
        ),
        valueBox(
          value = paste0(format(round(as.numeric(round((v_2_sep$data[[2]][budget_total == round_any(input$twBudget_2_sep/input$Daterange_2_sep, 0.5, f = floor)]$profit * 635) / 1000000))), big.mark=","), " MN$"),
          subtitle = "Revenue From Expected Mobile Registrations",
          color = "light-blue",
          icon = icon("coins"),
          width = 4
        )
      ),
      fluidRow(plotlyOutput("rys_2_sep")),
      fluidRow(column(12, align = "center", tableOutput("fintab_2_sep"))),
      width = 12,
      h5('*Channels with with share >= 1% are included')
    )})

  output$ks_final_table_2_one_sep <- renderUI({
    box(
      # title = h3(paste0("Optimal Budget Allocation - ", input$Daterange_2_sep, " Week(s)") , div(style = "height:25px;")),
      fluidRow(
        valueBox(
          value = (paste0(input$twBudget_2_sep, " MN$")),
          subtitle = "Budget",
          color = "maroon",
          icon = icon("credit-card"),
          width = 4
        ),
        valueBox(
          value = paste0(format(round(as.numeric(round(v_2_sep$data[[2]][budget_total == round_any(input$twBudget_2_sep/input$Daterange_2_sep, 0.5, f = floor)]$profit) * input$Daterange_2_sep)), big.mark=","), " Units"),
          subtitle = "Mobile Registrations From Digital Paid Media",
          color = "maroon",
          icon = icon("mobile"),
          width = 4
        ),
        valueBox(
          value = paste0(format(round(as.numeric(round((v_2_sep$data[[2]][budget_total == round_any(input$twBudget_2_sep/input$Daterange_2_sep, 0.5, f = floor)]$profit * 635) / 1000000)* input$Daterange_2_sep)), big.mark=","), " MN$"),
          subtitle = "Revenue From Expected Mobile Registrations",
          color = "maroon",
          icon = icon("coins"),
          width = 4
        )
      ),
      fluidRow(plotlyOutput("rys_2_one_sep")),
      fluidRow(column(12, align = "center", tableOutput("fintab_2_one_sep"))),
      width = 12,
      h5('*Channels with with share >= 1% are included')
    )})




  # BOTTOM BARPLOTS ---------------------------------------------------------

  output$rys_sep <- renderPlotly({
    ggplot(data = v_sep$data[[1]][v_sep$data[[1]]$budget_total== floor((input$twBudget_sep/input$Daterange_sep*10)/10)][v_sep$data[[1]][v_sep$data[[1]]$budget_total== floor((input$twBudget_sep/input$Daterange_sep*10)/10)]$data>0.01], aes(x = reorder(Value, -data), y = data * 1000000 * input$twBudget_sep * input$Daterange_sep, fill = Value), colour = "white") +
      ggtitle("Channel Split (‘000 $)") +
      geom_bar(stat = "identity", colour = "white") +
      scale_fill_manual("Legend", values = dd.col) +
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
      theme(
        axis.text.y = element_text(colour = "#ba9765"),
        axis.title.x = element_text(colour = "#cdcdcd")
      ) +
      # theme(axis.title.x=element_text(colour="#cdcdcd")) +
      theme(axis.title.y = element_text(colour = "#cdcdcd")) +
      theme(legend.title = element_text(colour = "#cdcdcd")) +
      theme(legend.title = element_blank()) +
      theme(legend.text = element_text(colour = "#cdcdcd")) +
      # panel.grid.minor = element_blank()+
      theme(axis.line.y = element_blank()) +
      theme(axis.text.y = element_blank()) +
      theme(axis.ticks.y = element_blank()) +
      theme(axis.text.x = element_text(size = 12)) +
      theme(legend.position = "none") +
      theme(axis.title.x = element_blank()) +
      theme(axis.title.y = element_blank()) +
      theme(plot.title = element_text(color = "#cdcdcd")) +
      # theme(axis.text.x = element_blank())+
      # theme(axis.ticks.x = element_blank())+
      geom_col(colour = "white") +
      geom_text(aes(Value, label = format(round(data * 1000 * input$twBudget_sep/input$Daterange_sep, 1), big.mark = ",")),
                position = position_stack(vjust = 1.15),
                col = "white"
      )

    ggplotly()

  })



  output$rys_one_sep <- renderPlotly({
    ggplot(data = v_sep$data[[1]][v_sep$data[[1]]$budget_total==floor((input$twBudget_sep/input$Daterange_sep*10)/10)][v_sep$data[[1]][v_sep$data[[1]]$budget_total==floor((input$twBudget_sep/input$Daterange_sep*10)/10)]$data>0.01], aes(x = reorder(Value, -data), y = data * 1000000 * input$twBudget_sep/input$Daterange_sep, fill = Value), colour = "white") +
      ggtitle(paste0("Channel Split (‘000 $)")) +
      geom_bar(stat = "identity", colour = "white") +
      scale_fill_manual("Legend", values = dd.col) +
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
      theme(
        axis.text.y = element_text(colour = "#ba9765"),
        axis.title.x = element_text(colour = "#cdcdcd")
      ) +
      # theme(axis.title.x=element_text(colour="#cdcdcd")) +
      theme(axis.title.y = element_text(colour = "#cdcdcd")) +
      theme(legend.title = element_text(colour = "#cdcdcd")) +
      theme(legend.title = element_blank()) +
      theme(legend.text = element_text(colour = "#cdcdcd")) +
      # panel.grid.minor = element_blank()+
      theme(axis.line.y = element_blank()) +
      theme(axis.text.y = element_blank()) +
      theme(axis.ticks.y = element_blank()) +
      theme(axis.text.x = element_text(size = 12)) +
      theme(legend.position = "none") +
      theme(axis.title.x = element_blank()) +
      theme(axis.title.y = element_blank()) +
      theme(plot.title = element_text(color = "#cdcdcd")) +
      # theme(axis.text.x = element_blank())+
      # theme(axis.ticks.x = element_blank())+
      geom_col(colour = "white") +
      geom_text(aes(Value, label = format((round(data * 1000 * input$twBudget_sep/input$Daterange_sep, 1)* input$Daterange_sep), big.mark = ",")),
                position = position_stack(vjust = 1.15),
                col = "white"
      )

    ggplotly()

  })

  output$rys_2_sep <- renderPlotly({

    ggplot(data = v_2_sep$data[[1]][v_2_sep$data[[1]]$budget_total== ((input$twBudget_2_sep/input$Daterange_2_sep*10)/10)][v_2_sep$data[[1]][v_2_sep$data[[1]]$budget_total== ((input$twBudget_2_sep/input$Daterange_2_sep*10)/10)]$data>0.01], aes(x = reorder(Value, -data), y = data * 1000000 * input$twBudget_2_sep * input$Daterange_2_sep, fill = Value), colour = "white") +
      ggtitle("Channel Split (‘000 $)") +
      geom_bar(stat = "identity", colour = "white") +
      scale_fill_manual("Legend", values = dd.col) +
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
      theme(
        axis.text.y = element_text(colour = "#ba9765"),
        axis.title.x = element_text(colour = "#cdcdcd")
      ) +
      # theme(axis.title.x=element_text(colour="#cdcdcd")) +
      theme(axis.title.y = element_text(colour = "#cdcdcd")) +
      theme(legend.title = element_text(colour = "#cdcdcd")) +
      theme(legend.title = element_blank()) +
      theme(legend.text = element_text(colour = "#cdcdcd")) +
      # panel.grid.minor = element_blank()+
      theme(axis.line.y = element_blank()) +
      theme(axis.text.y = element_blank()) +
      theme(axis.ticks.y = element_blank()) +
      theme(axis.text.x = element_text(size = 12)) +
      theme(legend.position = "none") +
      theme(axis.title.x = element_blank()) +
      theme(axis.title.y = element_blank()) +
      theme(plot.title = element_text(color = "#cdcdcd")) +
      # theme(axis.text.x = element_blank())+
      # theme(axis.ticks.x = element_blank())+
      geom_col(colour = "white") +
      geom_text(aes(Value, label = format(round(data * 1000 * input$twBudget_2_sep/input$Daterange_2_sep, 1), big.mark = ",")),
                position = position_stack(vjust = 1.15),
                col = "white"
      )


    ggplotly()
  })



  output$rys_2_one_sep <- renderPlotly({

    ggplot(data = v_2_sep$data[[1]][v_2_sep$data[[1]]$budget_total==floor((input$twBudget_2_sep/input$Daterange_2_sep*10)/10)][v_2_sep$data[[1]][v_2_sep$data[[1]]$budget_total==floor((input$twBudget_2_sep/input$Daterange_2_sep*10)/10)]$data>0.01], aes(x = reorder(Value, -data), y = data * 1000000 * input$twBudget_2_sep/input$Daterange_2_sep, fill = Value), colour = "white") +
      ggtitle("Channel Split (‘000 $)") +
      geom_bar(stat = "identity", colour = "white") +
      scale_fill_manual("Legend", values = dd.col) +
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
      theme(
        axis.text.y = element_text(colour = "#ba9765"),
        axis.title.x = element_text(colour = "#cdcdcd")
      ) +
      # theme(axis.title.x=element_text(colour="#cdcdcd")) +
      theme(axis.title.y = element_text(colour = "#cdcdcd")) +
      theme(legend.title = element_text(colour = "#cdcdcd")) +
      theme(legend.title = element_blank()) +
      theme(legend.text = element_text(colour = "#cdcdcd")) +
      # panel.grid.minor = element_blank()+
      theme(axis.line.y = element_blank()) +
      theme(axis.text.y = element_blank()) +
      theme(axis.ticks.y = element_blank()) +
      theme(axis.text.x = element_text(size = 12)) +
      theme(legend.position = "none") +
      theme(axis.title.x = element_blank()) +
      theme(axis.title.y = element_blank()) +
      theme(plot.title = element_text(color = "#cdcdcd")) +
      # theme(axis.text.x = element_blank())+
      # theme(axis.ticks.x = element_blank())+
      geom_col(colour = "white") +
      geom_text(aes(Value, label = format((round(data * 1000 * input$twBudget_2_sep/input$Daterange_2_sep, 1) * input$Daterange_2_sep), big.mark = ",")),
                position = position_stack(vjust = 1.15),
                col = "white"
      )



    ggplotly()
  })


  # BOTTOM TABLES -----------------------------------------------------------


  # output$ROAS_table <- renderUI({
  #   tableHTML(ROAS_table, rownames = FALSE, widths=rep(300, 9)) %>%
  #     add_css_header(css = list(c("text-align", "background-color"), c("center", "black")),
  #                    headers = 1:ncol(ROAS_table)) %>%
  #     add_css_row(css = list("text-align", "center"),
  #                 rows = 1:4) %>%
  #     add_css_column(css = list(c("background-color"), c("black")),
  #                    columns = c(1))
  # })
  #

  output$fintab_sep <- renderUI({
    # v_sep$data[[1]][v_sep$data[[1]]$budget_total==input$twBudget_sep][v_sep$data[[1]][v_sep$data[[1]]$budget_total==input$twBudget_sep]$data>0.01]$Value <- as.character(v_sep$data[[1]][v_sep$data[[1]]$budget_total==input$twBudget_sep][v_sep$data[[1]][v_sep$data[[1]]$budget_total==input$twBudget_sep]$data>0.01]$Value)
    # v_sep$data[[1]][v_sep$data[[1]]$budget_total==input$twBudget_sep][v_sep$data[[1]][v_sep$data[[1]]$budget_total==input$twBudget_sep]$data>0.01]$Value <- substr(v_sep$data[[1]][v_sep$data[[1]]$budget_total==input$twBudget_sep][v_sep$data[[1]][v_sep$data[[1]]$budget_total==input$twBudget_sep]$data>0.01]$Value, 1, nchar(v_sep$data[[1]][v_sep$data[[1]]$budget_total==input$twBudget_sep][v_sep$data[[1]][v_sep$data[[1]]$budget_total==input$twBudget_sep]$data>0.01]$Value)-2)
    temp = v_sep$data[[1]][v_sep$data[[1]]$budget_total==floor((input$twBudget_sep/input$Daterange_sep)*10)/10][v_sep$data[[1]][v_sep$data[[1]]$budget_total==floor((input$twBudget_sep/input$Daterange_sep*10)/10)]$data>0.01]
    names(temp)[5] = "Share"
    fintab<-temp %>%
      select(Value, Share)
    fintab$Budget<-fintab$Share*input$twBudget_sep/input$Daterange_sep
    fintab=fintab[as.numeric(Budget)>=0.01]
    fintab$Value <- as.character(fintab$Value)
    # fintab$Value <- substr(fintab$Value, 1, nchar(fintab$Value)-2)
    fintab2<-fintab
    fintab2 <- fintab2[order(-Budget),]
    n <- fintab2$Value
    fintab2$Share <- round(fintab2$Share * 100, 2)
    fintab2$Budget <- round(fintab2$Budget, 2)
    names(fintab2)[2] = "Share (%)"
    names(fintab2)[3] = "Budget (MN$)"
    fintab2 <- as.data.frame(t(fintab2[, -1]))
    colnames(fintab2) <- n
    fintab2$Value <- factor(row.names(fintab2))
    # fintab2 <- fintab2 %>% rename(Type = Value)
    fintab2 <- fintab2 %>% select(Value, everything())
    fintab2 <- arrange(fintab2, Value)

    tableHTML(fintab2, rownames = FALSE, widths=rep('1000px', ncol(fintab2))) %>%
      add_css_header(css = list(c("text-align", "background-color", "color"), c("center", "#3c8dbc", "white")),
                     headers = 1:ncol(fintab2)) %>%
      add_css_row(css = list(c("text-align", "color"), c("center", "white")),
                  rows = 1:nrow(fintab2)+1) %>%
      add_css_column(css = list(c("background-color"), c("#3c8dbc")),
                     columns = c(1))
  })

  output$fintab_one_sep <- renderUI({
    # v_sep$data[[1]][v_sep$data[[1]]$budget_total==input$twBudget_sep][v_sep$data[[1]][v_sep$data[[1]]$budget_total==input$twBudget_sep]$data>0.01]$Value <- as.character(v_sep$data[[1]][v_sep$data[[1]]$budget_total==input$twBudget_sep][v_sep$data[[1]][v_sep$data[[1]]$budget_total==input$twBudget_sep]$data>0.01]$Value)
    # v_sep$data[[1]][v_sep$data[[1]]$budget_total==input$twBudget_sep][v_sep$data[[1]][v_sep$data[[1]]$budget_total==input$twBudget_sep]$data>0.01]$Value <- substr(v_sep$data[[1]][v_sep$data[[1]]$budget_total==input$twBudget_sep][v_sep$data[[1]][v_sep$data[[1]]$budget_total==input$twBudget_sep]$data>0.01]$Value, 1, nchar(v_sep$data[[1]][v_sep$data[[1]]$budget_total==input$twBudget_sep][v_sep$data[[1]][v_sep$data[[1]]$budget_total==input$twBudget_sep]$data>0.01]$Value)-2)
    temp = v_sep$data[[1]][v_sep$data[[1]]$budget_total==floor((input$twBudget_sep/input$Daterange_sep*10)/10)][v_sep$data[[1]][v_sep$data[[1]]$budget_total==floor((input$twBudget_sep/input$Daterange_sep*10)/10)]$data>0.01]
    names(temp)[5] = "Share"
    fintab<-temp %>%
      select(Value, Share)
    fintab$Budget<-fintab$Share*input$twBudget_sep/input$Daterange_sep
    fintab=fintab[as.numeric(Budget)>=0.01]
    fintab$Value <- as.character(fintab$Value)
    # fintab$Value <- substr(fintab$Value, 1, nchar(fintab$Value)-2)
    fintab2<-fintab
    fintab2 <- fintab2[order(-Budget),]
    n <- fintab2$Value
    fintab2$Share <- round(fintab2$Share * 100, 2)
    fintab2$Budget <- round(fintab2$Budget, 2)
    names(fintab2)[2] = "Share (%)"
    names(fintab2)[3] = "Budget (MN$)"
    fintab2 <- as.data.frame(t(fintab2[, -1]))
    colnames(fintab2) <- n
    fintab2$Value <- factor(row.names(fintab2))
    # fintab2 <- fintab2 %>% rename(Type = Value)
    fintab2 <- fintab2 %>% select(Value, everything())

    fintab2[2,-c(1)] <- fintab2[2,-c(1)] * input$Daterange_sep
    fintab2 <- arrange(fintab2, Value)

    tableHTML(fintab2, rownames = FALSE, widths=rep('1000px', ncol(fintab2))) %>%
      add_css_header(css = list(c("text-align", "background-color", "color"), c("center", "#d81b60", "white")),
                     headers = 1:ncol(fintab2)) %>%
      add_css_row(css = list(c("text-align", "color"), c("center", "white")),
                  rows = 1:nrow(fintab2)+1) %>%
      add_css_column(css = list(c("background-color"), c("#d81b60")),
                     columns = c(1))

  })

  output$fintab_2_sep <- renderUI({
    # v_2_sep$data[[1]][v_2_sep$data[[1]]$budget_total==input$twBudget_2_sep][v_2_sep$data[[1]][v_2_sep$data[[1]]$budget_total==input$twBudget_2_sep]$data>0.01]$Value <- as.character(v_2_sep$data[[1]][v_2_sep$data[[1]]$budget_total==input$twBudget_2_sep][v_2_sep$data[[1]][v_2_sep$data[[1]]$budget_total==input$twBudget_2_sep]$data>0.01]$Value)
    # v_2_sep$data[[1]][v_2_sep$data[[1]]$budget_total==input$twBudget_2_sep][v_2_sep$data[[1]][v_2_sep$data[[1]]$budget_total==input$twBudget_2_sep]$data>0.01]$Value <- substr(v_2_sep$data[[1]][v_2_sep$data[[1]]$budget_total==input$twBudget_2_sep][v_2_sep$data[[1]][v_2_sep$data[[1]]$budget_total==input$twBudget_2_sep]$data>0.01]$Value, 1, nchar(v_2_sep$data[[1]][v_2_sep$data[[1]]$budget_total==input$twBudget_2_sep][v_2_sep$data[[1]][v_2_sep$data[[1]]$budget_total==input$twBudget_2_sep]$data>0.01]$Value)-2)
    temp = v_2_sep$data[[1]][v_2_sep$data[[1]]$budget_total==floor((input$twBudget_2_sep/input$Daterange_2_sep*10)/10)][v_2_sep$data[[1]][v_2_sep$data[[1]]$budget_total==floor((input$twBudget_2_sep/input$Daterange_2_sep*10)/10)]$data>0.01]
    names(temp)[5] = "Share"
    fintab_2<-temp %>%
      select(Value, Share)
    fintab_2$Budget<-fintab_2$Share*input$twBudget_2_sep/input$Daterange_2_sep
    fintab_2=fintab_2[as.numeric(Budget)>=0.01]
    fintab_2$Value <- as.character(fintab_2$Value)
    # fintab_2$Value <- substr(fintab_2$Value, 1, nchar(fintab_2$Value)-2)
    fintab2_2<-fintab_2
    fintab2_2 <- fintab2_2[order(-Budget),]
    n <- fintab2_2$Value
    fintab2_2$Share <- round(fintab2_2$Share * 100, 2)
    fintab2_2$Budget <- round(fintab2_2$Budget, 2)
    names(fintab2_2)[2] = "Share (%)"
    names(fintab2_2)[3] = "Budget (MN$)"
    fintab2_2 <- as.data.frame(t(fintab2_2[, -1]))
    colnames(fintab2_2) <- n
    fintab2_2$Value <- factor(row.names(fintab2_2))
    # fintab2_2 <- fintab2_2 %>% rename(Type = Value)
    fintab2_2 <- fintab2_2 %>% select(Value, everything())
    fintab2_2 <- arrange(fintab2_2, Value)

    tableHTML(fintab2_2, rownames = FALSE, widths=rep('1000px', ncol(fintab2_2))) %>%
      add_css_header(css = list(c("text-align", "background-color", "color"), c("center", "#3c8dbc", "white")),
                     headers = 1:ncol(fintab2_2)) %>%
      add_css_row(css = list(c("text-align", "color"), c("center", "white")),
                  rows = 1:nrow(fintab2_2)+1) %>%
      add_css_column(css = list(c("background-color"), c("#3c8dbc")),
                     columns = c(1))
  })

  output$fintab_2_one_sep <- renderUI({
    # v_2_sep$data[[1]][v_2_sep$data[[1]]$budget_total==input$twBudget_2_sep][v_2_sep$data[[1]][v_2_sep$data[[1]]$budget_total==input$twBudget_2_sep]$data>0.01]$Value <- as.character(v_2_sep$data[[1]][v_2_sep$data[[1]]$budget_total==input$twBudget_2_sep][v_2_sep$data[[1]][v_2_sep$data[[1]]$budget_total==input$twBudget_2_sep]$data>0.01]$Value)
    # v_2_sep$data[[1]][v_2_sep$data[[1]]$budget_total==input$twBudget_2_sep][v_2_sep$data[[1]][v_2_sep$data[[1]]$budget_total==input$twBudget_2_sep]$data>0.01]$Value <- substr(v_2_sep$data[[1]][v_2_sep$data[[1]]$budget_total==input$twBudget_2_sep][v_2_sep$data[[1]][v_2_sep$data[[1]]$budget_total==input$twBudget_2_sep]$data>0.01]$Value, 1, nchar(v_2_sep$data[[1]][v_2_sep$data[[1]]$budget_total==input$twBudget_2_sep][v_2_sep$data[[1]][v_2_sep$data[[1]]$budget_total==input$twBudget_2_sep]$data>0.01]$Value)-2)
    temp = v_2_sep$data[[1]][v_2_sep$data[[1]]$budget_total==floor((input$twBudget_2_sep/input$Daterange_2_sep*10)/10)][v_2_sep$data[[1]][v_2_sep$data[[1]]$budget_total==floor((input$twBudget_2_sep/input$Daterange_2_sep*10)/10)]$data>0.01]
    names(temp)[5] = "Share"
    fintab_2<-temp %>%
      select(Value, Share)
    fintab_2$Budget<-fintab_2$Share*input$twBudget_2_sep/input$Daterange_2_sep
    fintab_2=fintab_2[as.numeric(Budget)>=0.01]
    fintab_2$Value <- as.character(fintab_2$Value)
    # fintab_2$Value <- substr(fintab_2$Value, 1, nchar(fintab_2$Value)-2)
    fintab2_2<-fintab_2
    fintab2_2 <- fintab2_2[order(-Budget),]
    n <- fintab2_2$Value
    fintab2_2$Share <- round(fintab2_2$Share * 100, 2)
    fintab2_2$Budget <- round(fintab2_2$Budget, 2)
    names(fintab2_2)[2] = "Share (%)"
    names(fintab2_2)[3] = "Budget (MN$)"
    fintab2_2 <- as.data.frame(t(fintab2_2[, -1]))
    colnames(fintab2_2) <- n
    fintab2_2$Value <- factor(row.names(fintab2_2))
    # fintab2_2 <- fintab2_2 %>% rename(Type = Value)
    fintab2_2 <- fintab2_2 %>% select(Value, everything())

    fintab2_2[2,-c(1)] <- fintab2_2[2,-c(1)] * input$Daterange_2_sep
    fintab2_2 <- arrange(fintab2_2, Value)

    tableHTML(fintab2_2, rownames = FALSE, widths=rep('1000px', ncol(fintab2_2))) %>%
      add_css_header(css = list(c("text-align", "background-color", "color"), c("center", "#d81b60", "white")),
                     headers = 1:ncol(fintab2_2)) %>%
      add_css_row(css = list(c("text-align", "color"), c("center", "white")),
                  rows = 1:nrow(fintab2_2)+1) %>%
      add_css_column(css = list(c("background-color"), c("#d81b60")),
                     columns = c(1))

  })






  output$Re_model_KS_prediction <- renderPlotly({
    ggplot() +
      geom_line(data = re_lines_BRAND, aes(x = Date, y = data, colour = Value, linetype = Value), size = 0.5) +
      scale_color_manual(values = c("red", "white")) +
      scale_x_date(breaks = unique(re_decomp_BRAND$Date)[seq(1, length(unique(re_decomp_BRAND$Date))) %% 40 == 0], date_labels = "%b-%Y") +
      geom_hline(yintercept = 0, linetype = "dashed", color = "white", size = 0.5) +
      xlab("Date") +
      ylab("Mobile registrations") +
      theme(legend.position = "right") +
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
      guides(fill = guide_legend(title = NULL)) +
      theme(legend.title = element_text(colour = "#cdcdcd")) +
      theme(legend.title = element_blank()) +
      theme(legend.text = element_text(colour = "#cdcdcd"))
    ggplotly()
  })



  output$Budget_KS_key_points <- renderPlotly({
    ggplot(ad_barchart_BRAND, aes(x = Type, y = Share, fill = Media)) +
      scale_fill_manual(values = c("red", "blue", "grey40", "green", "yellow", "purple")) +
      geom_bar(width = 1, stat = "identity", alpha = 0.6, color = "white", size = 0.25, position = position_stack(reverse = T)) +
      coord_flip() +
      theme_minimal() +
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(color = "grey", size = 14),
        legend.position = "bottom"
      ) +
      theme(
        panel.background = element_rect(fill = "transparent") # bg of the panel
        , plot.background = element_rect(fill = "transparent", color = NA) # bg of the plot
        , panel.grid.major = element_blank() # get rid of major grid
        , panel.grid.minor = element_blank() # get rid of minor grid
      ) +
      theme(axis.text.x = element_text(colour = "#ba9765")) +
      theme(axis.text.y = element_text(colour = "#ba9765")) +
      theme(legend.title = element_text(colour = "#cdcdcd")) +
      theme(legend.text = element_text(colour = "#cdcdcd"))
    ggplotly()
  })
  re_response_BRAND[, c(media_vars, "Budget")]

  output$Response_BRAND <- renderPlotly({
    ggplot(
      data = re_response_BRAND2_plot %>% select(media_vars_names_pl)  %>%
        melt.data.table(id.vars = "Budget", value.name = "Value", variable.name = "Media"),
      aes(x = Budget, y = (Value*635)/1000000, color = Media)
    ) +
      ggtitle('Response Curves - Overall')+
      scale_x_continuous(breaks = seq(0, 4, 0.5)) +
      scale_y_continuous(breaks = seq(0, 12, 1)) +
      geom_line() +
      # geom_point(data = mroas_table, shape = 8) +
      # geom_point(data = table_2019) +
      # geom_point(data = table_2020) +
      scale_color_manual(values = c("Audio" = "#4B0055", "Direct Display" = "#471F68", "Digital OOH" = "#393A7A", "Facebook" = "#0D5288", "OLV" = "#006992", "Other Social" = "#007E97",
                                    "OTT" = "#009297", "Partnership" = "#00A492", "Primary Search" = "#00B587", "Prog Display" = "#00C377", "Prog OLV" = "#65CF61",
                                    "Secondary Search" = "#9FD946", "Youtube DD" = "#D1E02E", "Youtube OLV" = "#FDE333", "MROAS = 1" = "red",
                                    "Jan-Aug 2019" = "lightblue", "Jan-Aug 2020" = "blue4")) +
      xlab("Weekly Investments (MN$)") +
      ylab("Weekly Revenue (MN$)

           ") +
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
      theme(legend.text = element_text(colour = "#cdcdcd")) +
      theme(axis.title = element_text(size = 15)) +
      theme(axis.text = element_text(size = 12)) +
      # annotate("text", x = 1.8, y = 12, label = "Diminishing Return Marginal ROAS: 1
      #          Point after which return per dollar is less than 1", color = 'white', fontface = "bold") +
      # annotate("rect", xmin = 1.03, xmax = 2.96, ymin = 11.5, ymax = 12.5,
      #            alpha = .5, color = "white") +
      theme(plot.title = element_text(color = "#cdcdcd", size = 16)) +
      guides(fill = guide_legend(title = NULL))
    ggplotly()
  })

  output$Response_BRAND_cold <- renderPlotly({
    ggplot(
      data = re_response_BRAND2_plot_cold %>% select(media_vars_names_pl_cold)  %>%
        melt.data.table(id.vars = "Budget", value.name = "Value", variable.name = "Media"),
      aes(x = Budget, y = (Value*635)/1000000, color = Media)
    ) +
      # ggtitle('Response Curves - Cold')+
      scale_x_continuous(breaks = seq(0, 8, 0.5)) +
      # scale_y_continuous(breaks = seq(0, 12, 1)) +
      geom_line() +
      geom_point(data = mroas_cold, shape = 8) +
      # geom_point(data = table_2019) +
      # geom_point(data = table_2020) +
      scale_color_manual(values = c(tt_response_cold.col, "MROAS = 1" = "red")) +
      xlab("Weekly Investments (MN$)") +
      ylab("Weekly Revenue (MN$)

           ") +
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
      theme(legend.text = element_text(colour = "#cdcdcd")) +
      theme(axis.title = element_text(size = 15)) +
      theme(axis.text = element_text(size = 12)) +
      # annotate("text", x = 1.8, y = 12, label = "Diminishing Return Marginal ROAS: 1
      #          Point after which return per dollar is less than 1", color = 'white', fontface = "bold") +
      # annotate("rect", xmin = 1.03, xmax = 2.96, ymin = 11.5, ymax = 12.5,
      #          alpha = .5, color = "white") +
      theme(plot.title = element_text(color = "#cdcdcd", size = 16)) +
      guides(fill = guide_legend(title = NULL))
    # theme(panel.border = element_rect(fill = NA, colour = 'blue1', size = 2))
    ggplotly()
  })

  output$Response_BRAND_warm <- renderPlotly({
    ggplot(
      data = re_response_BRAND2_plot_warm %>% select(media_vars_names_pl_warm)  %>%
        melt.data.table(id.vars = "Budget", value.name = "Value", variable.name = "Media"),
      aes(x = Budget, y = (Value*635)/1000000, color = Media)
    ) +
      # ggtitle('Response Curves - Warm')+
      scale_x_continuous(breaks = seq(0, 8, 0.5)) +
      # scale_y_continuous(breaks = seq(0, 12, 1)) +
      geom_line() +
      geom_point(data = mroas_warm, shape = 8) +
      # geom_point(data = table_2019) +
      # geom_point(data = table_2020) +
      scale_color_manual(values = c(tt_response_warm.col, "MROAS = 1" = "red")) +
      xlab("Weekly Investments (MN$)") +
      ylab("Weekly Revenue (MN$)

           ") +
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
      theme(legend.text = element_text(colour = "#cdcdcd")) +
      theme(axis.title = element_text(size = 15)) +
      theme(axis.text = element_text(size = 12)) +
      # annotate("text", x = 1.8, y = 12, label = "Diminishing Return Marginal ROAS: 1
      #          Point after which return per dollar is less than 1", color = 'white', fontface = "bold") +
      # annotate("rect", xmin = 1.03, xmax = 2.96, ymin = 11.5, ymax = 12.5,
      #          alpha = .5, color = "white") +
      theme(plot.title = element_text(color = "#cdcdcd", size = 16)) +
      guides(fill = guide_legend(title = NULL))
    # theme(panel.border = element_rect(fill = NA, colour = 'goldenrod3', size = 2))
    ggplotly()
  })

  output$Response_BRAND_hot <- renderPlotly({
    ggplot(
      data = re_response_BRAND2_plot_hot %>% select(media_vars_names_pl_hot)  %>%
        melt.data.table(id.vars = "Budget", value.name = "Value", variable.name = "Media"),
      aes(x = Budget, y = (Value*635)/1000000, color = Media)
    ) +
      # ggtitle('Response Curves - Hot')+
      scale_x_continuous(breaks = seq(0, 8, 0.5)) +
      # scale_y_continuous(breaks = seq(0, 12, 1)) +
      geom_line() +
      geom_point(data = mroas_hot) +
      # geom_point(data = table_2019) +
      # geom_point(data = table_2020) +
      scale_color_manual(values = c(tt_response_hot.col, "MROAS = 1" = "red")) +
      xlab("Weekly Investments (MN$)") +
      ylab("Weekly Revenue (MN$)

           ") +
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
      theme(legend.text = element_text(colour = "#cdcdcd")) +
      theme(axis.title = element_text(size = 15)) +
      theme(axis.text = element_text(size = 12)) +
      # annotate("text", x = 1.8, y = 12, label = "Diminishing Return Marginal ROAS: 1
      #          Point after which return per dollar is less than 1", color = 'white', fontface = "bold") +
      # annotate("rect", xmin = 1.03, xmax = 2.96, ymin = 11.5, ymax = 12.5,
      #          alpha = .5, color = "white") +
      theme(plot.title = element_text(color = "#cdcdcd", size = 16)) +
      guides(fill = guide_legend(title = NULL))
    # theme(panel.border = element_rect(fill = NA, colour = 'orangered1', size = 2))
    ggplotly()
  })



  # Generate a plot with Profit responses ----

  output$ROAS_BRAND <- renderPlotly({
    ggplot(
      data = opt_spot_bar,
      aes(Media, Value, group = Budget)
    ) +
      # ggtitle('ROAS by Channels - Overall')+
      geom_col(aes(fill = Budget, color = Budget), colour = 'white', position = "dodge") +
      geom_text(
        aes(label = Value, y = Value + 0.25),
        position = position_dodge(0.9),
        vjust = 0,
        col = "white"
      ) +
      scale_y_continuous(limits = c(0, 10)) +
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
      theme(axis.title.x = element_blank()) +
      theme(axis.title.y = element_blank()) +
      theme(legend.title = element_text(colour = "#cdcdcd")) +
      theme(legend.title = element_blank()) +
      theme(legend.text = element_text(colour = "#cdcdcd")) +
      theme(axis.title = element_text(size=14)) +
      theme(axis.text.x = element_text(size = 12)) +
      theme(axis.text.y = element_text(size = 12)) +
      theme(axis.line.y = element_blank())+
      theme(axis.ticks.y = element_blank())+
      theme(axis.text.y = element_blank())+
      theme(legend.position = "bottom") +
      # theme(axis.text.x = element_text(angle = 90)) +
      scale_fill_manual(values = c('azure4', 'azure3')) +
      theme(plot.title = element_text(color = "#cdcdcd", size = 15)) +
      theme(plot.margin = unit(c(0,0,0,2), "cm"))
    guides(fill = guide_legend(title = NULL))

    ggplotly()%>%
      layout(legend = list(orientation = "h", x = 0.6, y = 0.9))
  })



  output$ROAS_BRAND_cold <- renderPlotly({
    ggplot(
      data = opt_spot_cold_bar,
      aes(Media, Value, group = Budget)
    ) +
      # ggtitle('ROAS by Channels - Cold')+
      geom_col(aes(fill = Budget, color = Budget), colour = 'white', position = "dodge") +
      geom_text(
        aes(label = Value, y = Value + 0.25),
        position = position_dodge(0.9),
        vjust = 0,
        col = "white"
      ) +
      scale_y_continuous(limits = c(0, 10)) +
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
      theme(axis.title.x = element_blank()) +
      theme(axis.title.y = element_blank()) +
      theme(legend.title = element_text(colour = "#cdcdcd")) +
      theme(legend.title = element_blank()) +
      theme(legend.text = element_text(colour = "#cdcdcd")) +
      theme(axis.title = element_text(size=14)) +
      theme(axis.text.x = element_text(size = 12)) +
      theme(axis.text.y = element_text(size = 12)) +
      theme(axis.line.y = element_blank())+
      theme(axis.ticks.y = element_blank())+
      theme(axis.text.y = element_blank())+
      theme(legend.position = "bottom") +
      # theme(axis.text.x = element_text(angle = 90)) +
      scale_fill_manual(values = ColdBarsColour) +
      theme(plot.title = element_text(color = "#cdcdcd", size = 15)) +
      theme(plot.margin = unit(c(0,0,0,2), "cm"))
    # theme(panel.border = element_rect(fill = NA, colour = 'blue1', size = 2))
    guides(fill = guide_legend(title = NULL))

    ggplotly()%>%
      layout(legend = list(orientation = "h", x = 0.6, y = 0.9))
  })



  output$ROAS_BRAND_warm <- renderPlotly({
    ggplot(
      data = opt_spot_warm_bar,
      aes(Media, Value, group = Budget)
    ) +
      # ggtitle('ROAS by Channels - Warm')+
      geom_col(aes(fill = Budget, color = Budget), colour = 'white', position = "dodge") +
      geom_text(
        aes(label = Value, y = Value + 0.5),
        position = position_dodge(0.9),
        vjust = 0,
        col = "white"
      ) +
      scale_y_continuous(limits = c(0, NA)) +
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
      theme(axis.title.x = element_blank()) +
      theme(axis.title.y = element_blank()) +
      theme(legend.title = element_text(colour = "#cdcdcd")) +
      theme(legend.title = element_blank()) +
      theme(legend.text = element_text(colour = "#cdcdcd")) +
      theme(axis.title = element_text(size=14)) +
      theme(axis.text.x = element_text(size = 12)) +
      theme(axis.text.y = element_text(size = 12)) +
      theme(axis.line.y = element_blank())+
      theme(axis.ticks.y = element_blank())+
      theme(axis.text.y = element_blank())+
      theme(legend.position = "bottom") +
      # theme(axis.text.x = element_text(angle = 90)) +
      scale_fill_manual(values = c('chocolate1', 'burlywood1')) +
      theme(plot.title = element_text(color = "#cdcdcd", size = 15)) +
      theme(plot.margin = unit(c(0,0,0,2), "cm"))
    # theme(panel.border = element_rect(fill = NA, colour = 'goldenrod3', size = 2))
    guides(fill = guide_legend(title = NULL))

    ggplotly()%>%
      layout(legend = list(orientation = "h", x = 0.6, y = 0.9))
  })


  output$ROAS_BRAND_hot <- renderPlotly({
    ggplot(
      data = opt_spot_hot_bar,
      aes(Media, Value, group = Budget)
    ) +
      # ggtitle('ROAS by Channels - Hot')+
      geom_col(aes(fill = Budget, color = Budget), colour = 'white', position = "dodge") +
      geom_text(
        aes(label = Value, y = Value + 0.35),
        position = position_dodge(0.9),
        vjust = 0,
        col = "white"
      ) +
      scale_y_continuous(limits = c(0, 13)) +
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
      theme(axis.title.x = element_blank()) +
      theme(axis.title.y = element_blank()) +
      theme(legend.title = element_text(colour = "#cdcdcd")) +
      theme(legend.title = element_blank()) +
      theme(legend.text = element_text(colour = "#cdcdcd")) +
      theme(axis.title = element_text(size=14)) +
      theme(axis.text.x = element_text(size = 12)) +
      theme(axis.text.y = element_text(size = 12)) +
      theme(axis.line.y = element_blank())+
      theme(axis.ticks.y = element_blank())+
      theme(axis.text.y = element_blank())+
      theme(legend.position = "bottom") +
      # theme(axis.text.x = element_text(angle = 90)) +
      scale_fill_manual(values = c('Coral1', 'cornsilk1')) +
      theme(plot.title = element_text(color = "#cdcdcd", size = 15)) +
      theme(plot.margin = unit(c(0,0,0,2), "cm"))
    # theme(panel.border = element_rect(fill = NA, colour = 'orangered1', size = 2))
    guides(fill = guide_legend(title = NULL))

    ggplotly()%>%
      layout(legend = list(orientation = "h", x = 0.6, y = 0.9))
  })



  ggplot(data = opt_spot %>% melt.data.table(id.vars = "Budget", value.name = "Value", variable.name = "Media"),
         aes(Media, Value, group = Budget)) +
    geom_col(aes(fill = Budget), position = "dodge") +
    geom_text(
      aes(label = Value, y = Value + 0.05),
      position = position_dodge(0.9),
      vjust = 0
    )

  plot_frame <- ggplot() +
    theme(
      panel.background = element_rect(fill = "transparent") # bg of the panel
      , plot.background = element_rect(fill = "transparent", color = NA) # bg of the plot
      , panel.grid.major = element_blank() # get rid of major grid
      , panel.grid.minor = element_blank() # get rid of minor grid
      , legend.background = element_rect(fill = "transparent") # get rid of legend bg
      , legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
    ) +
    theme(axis.text.x = element_text(colour = "#ba9765")) +
    theme(axis.text.y = element_text(colour = "#ba9765")) +
    theme(axis.title.x = element_text(colour = "#cdcdcd")) +
    theme(axis.title.y = element_text(colour = "#cdcdcd")) +
    theme(legend.title = element_text(colour = "#cdcdcd")) +
    theme(legend.title = element_blank()) +
    theme(legend.text = element_text(colour = "#cdcdcd")) +
    theme(legend.position = "top") +
    theme(legend.title = element_blank()) +
    scale_fill_manual(values = dd.col) +
    theme(axis.text.y = element_text(size = 12)) +
    theme(axis.text.x = element_text(size = 10)) +
    theme(axis.title = element_text(size = 15)) +
    theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))+
    theme(plot.title = element_text(color = "#cdcdcd"))





  v <- reactiveValues(data = ggplot(), roas_data = ggplot(), plot = ggplot(), plot_ROAS = ggplot(), fintable = 0,
                      data_cold = ggplot(), roas_data_cold = ggplot(), plot_cold  = plot_frame, plot_ROAS_cold = plot_frame,
                      data_warm = ggplot(), roas_data_warm = ggplot(), plot_warm = plot_frame, plot_ROAS_warm = plot_frame,
                      data_hot = ggplot(), roas_data_hot = ggplot(), plot_hot = plot_frame, plot_ROAS_hot = plot_frame)

  v_sep <- reactiveValues(data = ggplot(), roas_data = ggplot(), plot = ggplot(), plot_ROAS = ggplot(), fintable = 0)


  v_2 <- reactiveValues(data = ggplot(), roas_data = ggplot(), plot = ggplot(), plot_ROAS = ggplot(), fintable = 0,
                        data_cold = ggplot(), roas_data_cold = ggplot(), plot_cold  = plot_frame, plot_ROAS_cold = plot_frame,
                        data_warm = ggplot(), roas_data_warm = ggplot(), plot_warm = plot_frame, plot_ROAS_warm = plot_frame,
                        data_hot = ggplot(), roas_data_hot = ggplot(), plot_hot = plot_frame, plot_ROAS_hot = plot_frame)

  v_2_sep <- reactiveValues(data = ad_optimal_splits_BRAND_sep, roas_data = ad_optimal_ROAS, plot = NULL, plot_ROAS = NULL, fintable = 0)


  v$plot <- ggplot()
  # geom_area(
  #   data = ad_optimal_splits_BRAND_initial[, Value := gsub("budget_", "", Value)],
  #   aes(x = budget_total, y = data, fill = Value), colour = "black", alpha = 0.6, position = "stack"
  # ) +
  # ggtitle("Optimal Split - Weekly")+
  # scale_x_continuous(breaks = seq(0, 15, 0.5)) +
  # theme_classic() +
  # theme(
  #   panel.background = element_rect(fill = "transparent") # bg of the panel
  #   , plot.background = element_rect(fill = "transparent", color = NA) # bg of the plot
  #   , panel.grid.major = element_blank() # get rid of major grid
  #   , panel.grid.minor = element_blank() # get rid of minor grid
  #   , legend.background = element_rect(fill = "transparent") # get rid of legend bg
  #   , legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
  # ) +
  # theme(axis.text.x = element_text(colour = "#ba9765")) +
  # theme(legend.title = element_blank()) +
  # theme(axis.text.y = element_text(colour = "#ba9765")) +
  # theme(axis.title.x = element_text(colour = "#cdcdcd")) +
  # theme(axis.title.y = element_text(colour = "#cdcdcd")) +
  # theme(legend.title = element_text(colour = "#cdcdcd")) +
  # theme(legend.title = element_blank()) +
  # theme(legend.text = element_text(colour = "#cdcdcd")) +
  # theme(legend.position = "top") +
  # theme(legend.title = element_blank()) +
  # scale_fill_manual(values = dd.col) +
  # theme(axis.text.y = element_text(size = 12)) +
  # theme(axis.text.x = element_text(size = 10)) +
  # theme(axis.title = element_text(size = 15)) +
  # theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))+
  # theme(plot.title = element_text(color = "#cdcdcd")) +
  # xlab("Media Budget (MN$)") +
  # ylab("Channel Share (%)
  #
  #          ") +
  # guides(fill = guide_legend(title = NULL))



  v_sep$plot <- ggplot() +
    geom_area(
      data = ad_optimal_splits_BRAND_sep[, Value := gsub("budget_", "", Value)],
      aes(x = budget_total, y = data, fill = Value), colour = "black", alpha = 0.6, position = "stack"
    ) +
    ggtitle("Optimal Split - Weekly")+
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    scale_x_continuous(breaks = seq(0, 15, 0.5)) +
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
    theme(legend.text = element_text(colour = "#cdcdcd")) +
    theme(legend.position = "top") +
    theme(legend.title = element_blank()) +
    # scale_fill_manual(values = dd) +
    theme(axis.text.y = element_text(size = 12)) +
    theme(axis.text.x = element_text(size = 10)) +
    theme(axis.title = element_text(size = 15)) +
    theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))+
    theme(plot.title = element_text(color = "#cdcdcd")) +
    xlab("Media Budget (MN$)") +
    ylab("Channel Share (%)

             ") +
    guides(fill = guide_legend(title = NULL))





  v_2$plot <- ggplot()
  # geom_area(
  #   data = ad_optimal_splits_BRAND_initial[, Value := gsub("budget_", "", Value)],
  #   aes(x = budget_total, y = data, fill = Value), colour = "black", alpha = 0.6, position = "stack"
  # ) +
  # ggtitle("Optimal Split - Weekly")+
  # scale_x_continuous(breaks = seq(0, 15, 0.5)) +
  # theme_classic() +
  # theme(
  #   panel.background = element_rect(fill = "transparent") # bg of the panel
  #   , plot.background = element_rect(fill = "transparent", color = NA) # bg of the plot
  #   , panel.grid.major = element_blank() # get rid of major grid
  #   , panel.grid.minor = element_blank() # get rid of minor grid
  #   , legend.background = element_rect(fill = "transparent") # get rid of legend bg
  #   , legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
  # ) +
  # theme(axis.text.x = element_text(colour = "#ba9765")) +
  # theme(legend.title = element_blank()) +
  # theme(axis.text.y = element_text(colour = "#ba9765")) +
  # theme(axis.title.x = element_text(colour = "#cdcdcd")) +
  # theme(axis.title.y = element_text(colour = "#cdcdcd")) +
  # theme(legend.title = element_text(colour = "#cdcdcd")) +
  # theme(legend.title = element_blank()) +
  # theme(legend.text = element_text(colour = "#cdcdcd")) +
  # theme(legend.position = "top") +
  # theme(legend.title = element_blank()) +
  # scale_fill_manual(values = dd.col) +
  # theme(axis.text.y = element_text(size = 12)) +
  # theme(axis.text.x = element_text(size = 10)) +
  # theme(axis.title = element_text(size = 15)) +
  # theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))+
  # theme(plot.title = element_text(color = "#cdcdcd")) +
  # xlab("Media Budget (MN$)") +
  # ylab("Channel Share (%)
  #
  #          ") +
  # guides(fill = guide_legend(title = NULL))



  v_2_sep$plot <- ggplot() +
    geom_area(
      data = ad_optimal_splits_BRAND_sep[, Value := gsub("budget_", "", Value)],
      aes(x = budget_total, y = data, fill = Value), colour = "black", alpha = 0.6, position = "stack"
    ) +
    ggtitle("Optimal Split - Weekly")+
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    scale_x_continuous(breaks = seq(0, 15, 0.5)) +
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
    theme(legend.text = element_text(colour = "#cdcdcd")) +
    theme(legend.position = "top") +
    theme(legend.title = element_blank()) +
    # scale_fill_manual(values = dd.col) +
    theme(axis.text.y = element_text(size = 12)) +
    theme(axis.text.x = element_text(size = 10)) +
    theme(axis.title = element_text(size = 15)) +
    theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))+
    theme(plot.title = element_text(color = "#cdcdcd")) +
    xlab("Media Budget (MN$)") +
    ylab("Channel Share (%)

             ") +
    guides(fill = guide_legend(title = NULL))








  v$plot_ROAS <- ggplot()
  # ggplot(data = ad_optimal_ROAS, aes(x = budget_total, y = profit)) +
  # geom_line(color = "white") +
  # ggtitle("Response Curve - Weekly") +
  #
  # scale_x_continuous(breaks = seq(0, 15, 0.5)) +
  # scale_y_continuous(labels = formatter1000()) +
  # theme_classic() +
  # theme(
  #   panel.background = element_rect(fill = "transparent") # bg of the panel
  #   , plot.background = element_rect(fill = "transparent", color = NA) # bg of the plot
  #   , panel.grid.major = element_blank() # get rid of major grid
  #   , panel.grid.minor = element_blank() # get rid of minor grid
  #   , legend.background = element_rect(fill = "transparent") # get rid of legend bg
  #   , legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
  # ) +
  # theme(axis.text.x = element_text(colour = "#ba9765")) +
  # theme(legend.title = element_blank()) +
  # theme(axis.text.y = element_text(colour = "#ba9765")) +
  # theme(axis.title.x = element_text(colour = "#cdcdcd")) +
  # theme(axis.title.y = element_text(colour = "#cdcdcd")) +
  # theme(legend.title = element_text(colour = "#cdcdcd")) +
  # theme(legend.title = element_blank()) +
  # theme(legend.text = element_text(colour = "#cdcdcd")) +
  # theme(plot.title = element_text(color = "#cdcdcd")) +
  # theme(axis.text.y = element_text(size = 12)) +
  # theme(axis.text.x = element_text(size = 10)) +
  # theme(axis.title = element_text(size = 15)) +
  # theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))+
  # xlab("Media Budget (MN$)
  #
  #      ") +
  # ylab("Mobile Registrations (‘000 Units)
  #
  #          ")



  v_sep$plot_ROAS <- ggplot(data = ad_optimal_ROAS[ad_optimal_ROAS$budget_total <= 10], aes(x = budget_total, y = profit)) +
    geom_line(color = "white") +
    ggtitle("Response Curve - Weekly") +

    scale_x_continuous(breaks = seq(0, 10, 0.5)) +
    scale_y_continuous(labels = formatter1000()) +
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
    theme(legend.text = element_text(colour = "#cdcdcd")) +
    theme(plot.title = element_text(color = "#cdcdcd")) +
    theme(axis.text.y = element_text(size = 12)) +
    theme(axis.text.x = element_text(size = 10)) +
    theme(axis.title = element_text(size = 15)) +
    theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))+
    xlab("Media Budget (MN$)

         ") +
    ylab("Mobile Registrations (‘000 Units)

             ")













  v_2$plot_ROAS <- ggplot()

  # ggplot(data = ad_optimal_ROAS, aes(x = budget_total, y = profit)) +
  # geom_line(color = "white") +
  # ggtitle("Response Curve - Weekly") +
  #
  # scale_x_continuous(breaks = seq(0, 15, 0.5)) +
  # scale_y_continuous(labels = formatter1000()) +
  # theme_classic() +
  # theme(
  #   panel.background = element_rect(fill = "transparent") # bg of the panel
  #   , plot.background = element_rect(fill = "transparent", color = NA) # bg of the plot
  #   , panel.grid.major = element_blank() # get rid of major grid
  #   , panel.grid.minor = element_blank() # get rid of minor grid
  #   , legend.background = element_rect(fill = "transparent") # get rid of legend bg
  #   , legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
  # ) +
  # theme(axis.text.x = element_text(colour = "#ba9765")) +
  # theme(legend.title = element_blank()) +
  # theme(axis.text.y = element_text(colour = "#ba9765")) +
  # theme(axis.title.x = element_text(colour = "#cdcdcd")) +
  # theme(axis.title.y = element_text(colour = "#cdcdcd")) +
  # theme(legend.title = element_text(colour = "#cdcdcd")) +
  # theme(legend.title = element_blank()) +
  # theme(legend.text = element_text(colour = "#cdcdcd")) +
  # theme(plot.title = element_text(color = "#cdcdcd")) +
  # theme(axis.text.y = element_text(size = 12)) +
  # theme(axis.text.x = element_text(size = 10)) +
  # theme(axis.title = element_text(size = 15)) +
  # theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))+
  # xlab("Media Budget (MN$)
  #
  #      ") +
  # ylab("Mobile Registrations (‘000 Units)
  #
  #          ")



  v_2_sep$plot_ROAS <- ggplot(data = ad_optimal_ROAS[ad_optimal_ROAS$budget_total <= 10], aes(x = budget_total, y = profit)) +
    geom_line(color = "white") +
    ggtitle("Response Curve - Weekly") +

    scale_x_continuous(breaks = seq(0, 15, 0.5)) +
    scale_y_continuous(labels = formatter1000()) +
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
    theme(legend.text = element_text(colour = "#cdcdcd")) +
    theme(plot.title = element_text(color = "#cdcdcd")) +
    theme(axis.text.y = element_text(size = 12)) +
    theme(axis.text.x = element_text(size = 10)) +
    theme(axis.title = element_text(size = 15)) +
    theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))+
    xlab("Media Budget (MN$)

         ") +
    ylab("Mobile Registrations (‘000 Units)

             ")









  s <- reactiveValues(one = c(0,0), two = c(0,0), three = c(0,0) ,four = c(0,0), five = c(0,0), six = c(0,0), seven = c(0,0),
                      eight = c(0,0), nine = c(0,0), ten = c(0,0), eleven = c(0,0), twelve = c(0,0), thirteen = c(0,0), fourteen = c(0,0),
                      fifthteen = c(0,0), sixteen = c(0,0), seventeen = c(0,0), eightteen = c(0,0), nineteen = c(0,0), s20 = c(0,0), s21 = c(0,0),
                      s22 = c(0,0), s23 = c(0,0), s24 = c(0,0), s25 = c(0,0), s26 = c(0,0) ,s27 = c(0,0) ,s28 = c(0,0),
                      s29 = c(0,0))


  s_sep <- reactiveValues(one = c(0,0), two = c(0,0), three = c(0,0) ,four = c(0,0), five = c(0,0), six = c(0,0), seven = c(0,0),
                          eight = c(0,0), nine = c(0,0), ten = c(0,0), eleven = c(0,0), twelve = c(0,0), thirteen = c(0,0), fourteen = c(0,0),
                          fifthteen = c(0,0), sixteen = c(0,0), seventeen = c(0,0), eightteen = c(0,0), nineteen = c(0,0), s20 = c(0,0), s21 = c(0,0),
                          s22 = c(0,0), s23 = c(0,0), s24 = c(0,0), s25 = c(0,0))


  observeEvent(input$slider1 | input$slider2 | input$slider3 | input$slider4 | input$slider5 | input$slider6 |
                 input$slider7 | input$slider8 | input$slider9| input$slider10| input$slider11| input$slider12 |
                 input$slider13 | input$slider14 | input$slider15 | input$slider16 | input$slider17 | input$slider18 | input$slider19 |
                 input$slider20 | input$slider21 | input$slider22 | input$slider23 | input$slider24 | input$slider25 | input$slider26 |
                 input$slider27 | input$slider28 | input$slider29, {

                   if (is.null(input$slider1)){
                     s$one <<- c(0,0)
                   } else{s$one <<- input$slider1}

                   if (is.null(input$slider2)){
                     s$two <<- c(0,0)
                   } else{s$two <<- input$slider2}

                   if (is.null(input$slider3)){
                     s$three <<- c(0,0)
                   } else{s$three <<- input$slider3}

                   if (is.null(input$slider4)){
                     s$four <<- c(0,0)
                   } else{s$four <<- input$slider4}

                   if (is.null(input$slider5)){
                     s$five <<- c(0,0)
                   } else{s$five <<- input$slider5}

                   if (is.null(input$slider6)){
                     s$six <<- c(0,0)
                   } else{s$six <<- input$slider6}

                   if (is.null(input$slider7)){
                     s$seven <<- c(0,0)
                   } else{s$seven <<- input$slider7}

                   if (is.null(input$slider8)){
                     s$eight <<- c(0,0)
                   } else{s$eight <<- input$slider8}

                   if (is.null(input$slider9)){
                     s$nine <<- c(0,0)
                   } else{s$nine <<- input$slider9}

                   if (is.null(input$slider10)){
                     s$ten <<- c(0,0)
                   } else{s$ten <<- input$slider10}

                   if (is.null(input$slider11)){
                     s$eleven <<- c(0,0)
                   } else{s$eleven <<- input$slider11}

                   if (is.null(input$slider12)){
                     s$twelve <<- c(0,0)
                   } else{s$twelve <<- input$slider12}

                   if (is.null(input$slider13)){
                     s$thirteen <<- c(0,0)
                   } else{s$thirteen <<- input$slider13}

                   if (is.null(input$slider14)){
                     s$fourteen <<- c(0,0)
                   } else{s$fourteen <<- input$slider14}

                   if (is.null(input$slider15)){
                     s$fiveteen <<- c(0,0)
                   } else{s$fiveteen <<- input$slider15}

                   if (is.null(input$slider16)){
                     s$sixteen <<- c(0,0)
                   } else{s$sixteen <<- input$slider16}

                   if (is.null(input$slider17)){
                     s$seventeen <<- c(0,0)
                   } else{s$seventeen <<- input$slider17}

                   if (is.null(input$slider18)){
                     s$eightteen <<- c(0,0)
                   } else{s$eightteen <<- input$slider18}

                   if (is.null(input$slider19)){
                     s$nineteen <<- c(0,0)
                   } else{s$nineteen <<- input$slider19}

                   if (is.null(input$slider20)){
                     s$s20 <<- c(0,0)
                   } else{s$s20 <<- input$slider20}

                   if (is.null(input$slider21)){
                     s$s21 <<- c(0,0)
                   } else{s$s21 <<- input$slider21}

                   if (is.null(input$slider22)){
                     s$s22 <<- c(0,0)
                   } else{s$s22 <<- input$slider22}

                   if (is.null(input$slider23)){
                     s$s23 <<- c(0,0)
                   } else{s$s23 <<- input$slider23}

                   if (is.null(input$slider24)){
                     s$s24 <<- c(0,0)
                   } else{s$s24 <<- input$slider24}

                   if (is.null(input$slider25)){
                     s$s25 <<- c(0,0)
                   } else{s$s25 <<- input$slider25}

                   if (is.null(input$slider26)){
                     s$s26 <<- c(0,0)
                   } else{s$s26 <<- input$slider26}

                   if (is.null(input$slider27)){
                     s$s27 <<- c(0,0)
                   } else{s$s27 <<- input$slider27}

                   if (is.null(input$slider28)){
                     s$s28 <<- c(0,0)
                   } else{s$s28 <<- input$slider28}

                   if (is.null(input$slider29)){
                     s$s29 <<- c(0,0)
                   } else{s$s29 <<- input$slider29}

                 })





  observeEvent(input$slider1_sep | input$slider2_sep | input$slider3_sep | input$slider4_sep | input$slider5_sep | input$slider6_sep |
                 input$slider7_sep | input$slider8_sep | input$slider9_sep| input$slider10_sep| input$slider11_sep| input$slider12_sep |
                 input$slider13_sep | input$slider14_sep | input$slider15_sep | input$slider16_sep | input$slider17_sep | input$slider18_sep | input$slider19_sep |
                 input$slider20_sep | input$slider21_sep | input$slider22_sep | input$slider23_sep | input$slider24_sep | input$slider25_sep,
                 {

                   if (is.null(input$slider1_sep)){
                     s_sep$one <<- c(0,0)
                   } else{s_sep$one <<- input$slider1_sep}

                   if (is.null(input$slider2_sep)){
                     s_sep$two <<- c(0,0)
                   } else{s_sep$two <<- input$slider2_sep}

                   if (is.null(input$slider3_sep)){
                     s_sep$three <<- c(0,0)
                   } else{s_sep$three <<- input$slider3_sep}

                   if (is.null(input$slider4_sep)){
                     s_sep$four <<- c(0,0)
                   } else{s_sep$four <<- input$slider4_sep}

                   if (is.null(input$slider5_sep)){
                     s_sep$five <<- c(0,0)
                   } else{s_sep$five <<- input$slider5_sep}

                   if (is.null(input$slider6_sep)){
                     s_sep$six <<- c(0,0)
                   } else{s_sep$six <<- input$slider6_sep}

                   if (is.null(input$slider7_sep)){
                     s_sep$seven <<- c(0,0)
                   } else{s_sep$seven <<- input$slider7_sep}

                   if (is.null(input$slider8_sep)){
                     s_sep$eight <<- c(0,0)
                   } else{s_sep$eight <<- input$slider8_sep}

                   if (is.null(input$slider9_sep)){
                     s_sep$nine <<- c(0,0)
                   } else{s_sep$nine <<- input$slider9_sep}

                   if (is.null(input$slider10_sep)){
                     s_sep$ten <<- c(0,0)
                   } else{s_sep$ten <<- input$slider10_sep}

                   if (is.null(input$slider11_sep)){
                     s_sep$eleven <<- c(0,0)
                   } else{s_sep$eleven <<- input$slider11_sep}

                   if (is.null(input$slider12_sep)){
                     s_sep$twelve <<- c(0,0)
                   } else{s_sep$twelve <<- input$slider12_sep}

                   if (is.null(input$slider13_sep)){
                     s_sep$thirteen <<- c(0,0)
                   } else{s_sep$thirteen <<- input$slider13_sep}

                   if (is.null(input$slider14_sep)){
                     s_sep$fourteen <<- c(0,0)
                   } else{s_sep$fourteen <<- input$slider14_sep}

                   if (is.null(input$slider15_sep)){
                     s_sep$fiveteen <<- c(0,0)
                   } else{s_sep$fiveteen <<- input$slider15_sep}

                   if (is.null(input$slider16_sep)){
                     s_sep$sixteen <<- c(0,0)
                   } else{s_sep$sixteen <<- input$slider16_sep}

                   if (is.null(input$slider17_sep)){
                     s_sep$seventeen <<- c(0,0)
                   } else{s_sep$seventeen <<- input$slider17_sep}

                   if (is.null(input$slider18_sep)){
                     s_sep$eightteen <<- c(0,0)
                   } else{s_sep$eightteen <<- input$slider18_sep}

                   if (is.null(input$slider19_sep)){
                     s_sep$nineteen <<- c(0,0)
                   } else{s_sep$nineteen <<- input$slider19_sep}

                   if (is.null(input$slider20_sep)){
                     s_sep$s20 <<- c(0,0)
                   } else{s_sep$s20 <<- input$slider20_sep}

                   if (is.null(input$slider21_sep)){
                     s_sep$s21 <<- c(0,0)
                   } else{s_sep$s21 <<- input$slider21_sep}

                   if (is.null(input$slider22_sep)){
                     s_sep$s22 <<- c(0,0)
                   } else{s_sep$s22 <<- input$slider22_sep}

                   if (is.null(input$slider23_sep)){
                     s_sep$s23 <<- c(0,0)
                   } else{s_sep$s23 <<- input$slider23_sep}

                   if (is.null(input$slider24_sep)){
                     s_sep$s24 <<- c(0,0)
                   } else{s_sep$s24 <<- input$slider24_sep}

                   if (is.null(input$slider25_sep)){
                     s_sep$s25 <<- c(0,0)
                   } else{s_sep$s25 <<- input$slider25_sep}


                 })


  observeEvent(input$Daterange, {
    channles_budgets_input <<- c(s$one,
                                 s$two,
                                 s$three,
                                 s$four,
                                 s$five,
                                 s$six,
                                 s$seven,
                                 s$eight,
                                 s$nine,
                                 s$ten,
                                 s$eleven,
                                 s$twelve,
                                 s$thirteen,
                                 s$fourteen,
                                 s$fiveteen,
                                 s$sixteen,
                                 s$seventeen,
                                 s$eightteen,
                                 s$nineteen,
                                 s$s20,
                                 s$s21,
                                 s$s22,
                                 s$s23,
                                 s$s24,
                                 s$s25,
                                 s$s26,
                                 s$s27,
                                 s$s28,
                                 s$s29)
  })


  observeEvent(input$Daterange_sep, {
    channles_budgets_sep_input <<- c(s_sep$one,
                                     s_sep$two,
                                     s_sep$three,
                                     s_sep$four,
                                     s_sep$five,
                                     s_sep$six,
                                     s_sep$seven,
                                     s_sep$eight,
                                     s_sep$nine,
                                     s_sep$ten,
                                     s_sep$eleven,
                                     s_sep$twelve,
                                     s_sep$thirteen,
                                     s_sep$fourteen,
                                     s_sep$fiveteen,
                                     s_sep$sixteen,
                                     s_sep$seventeen,
                                     s_sep$eightteen,
                                     s_sep$nineteen,
                                     s_sep$s20,
                                     s_sep$s21,
                                     s_sep$s22,
                                     s_sep$s23,
                                     s_sep$s24,
                                     s_sep$s25
)
  })



  observeEvent(input$Button, {
    channles_budgets_input <<- c(s$one,
                                 s$two,
                                 s$three,
                                 s$four,
                                 s$five,
                                 s$six,
                                 s$seven,
                                 s$eight,
                                 s$nine,
                                 s$ten,
                                 s$eleven,
                                 s$twelve,
                                 s$thirteen,
                                 s$fourteen,
                                 s$fiveteen,
                                 s$sixteen,
                                 s$seventeen,
                                 s$eightteen,
                                 s$nineteen,
                                 s$s20,
                                 s$s21,
                                 s$s22,
                                 s$s23,
                                 s$s24,
                                 s$s25)


    process_data()


    v$data_cold <- recalc_opt(channles_budgets_input, input$twBudget_cold*10^6/input$Daterange_cold, 'cold')
    v$data_warm <- recalc_opt(channles_budgets_input, input$twBudget_warm*10^6/input$Daterange_warm, 'warm')
    v$data_hot <- recalc_opt(channles_budgets_input, input$twBudget_hot*10^6/input$Daterange_hot, 'hot')

    # xx <<- recalc_opt(channles_budgets_input)
    load_data()
    v$fintable <- 1
    v$plot_cold <- ggplot() +
      geom_area(
        data = v$data_cold[[1]][, Value := gsub("budget_", "", Value)],
        aes(x = budget_total, y = data, fill = Value), colour = "black", alpha = 0.6, position = "stack"
      ) +
      ggtitle("Optimal Split cold - Weekly")+
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
      theme(legend.text = element_text(colour = "#cdcdcd")) +
      theme(legend.position = "bottom") +
      theme(legend.title = element_blank()) +
      theme(axis.text = element_text(size = 12)) +
      theme(axis.title = element_text(size = 15)) +
      scale_fill_manual(values = dd.col) +
      theme(legend.position = "bottom", legend.title = element_blank()) +
      theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))+
      theme(plot.title = element_text(color = "#cdcdcd")) +
      xlab("Media Budget (MN$)

           ") +
      ylab("Channel Share (%)

             ") +
      guides(fill = guide_legend(title = NULL))

    v$plot_ROAS_cold <- ggplot(data = v$data_cold[[2]], aes(x = budget_total, y = profit)) +
      geom_line(color = "white") +
      ggtitle("Response Curve - Weekly") +
      theme(plot.title = element_text(hjust = 0.5))+
      scale_y_continuous(labels = formatter1000()) +
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
      theme(legend.text = element_text(colour = "#cdcdcd")) +
      theme(legend.position = "bottom") +
      theme(legend.title = element_blank()) +
      theme(axis.text = element_text(size = 12)) +
      theme(axis.title = element_text(size = 15)) +
      theme(plot.title = element_text(color = "#cdcdcd")) +
      theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))+
      xlab("Media Budget (MN$)

           ") +
      ylab("Mobile Registrations (‘000 Units)
             ")

    v$roas_data_cold <- v$data_cold[[2]]





    v$plot_warm <- ggplot() +
      geom_area(
        data = v$data_warm[[1]][, Value := gsub("budget_", "", Value)],
        aes(x = budget_total, y = data, fill = Value), colour = "black", alpha = 0.6, position = "stack"
      ) +
      ggtitle("Optimal Split cold - Weekly")+
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
      theme(legend.text = element_text(colour = "#cdcdcd")) +
      theme(legend.position = "bottom") +
      theme(legend.title = element_blank()) +
      theme(axis.text = element_text(size = 12)) +
      theme(axis.title = element_text(size = 15)) +
      scale_fill_manual(values = dd.col) +
      theme(legend.position = "bottom", legend.title = element_blank()) +
      theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))+
      theme(plot.title = element_text(color = "#cdcdcd")) +
      xlab("Media Budget (MN$)

           ") +
      ylab("Channel Share (%)

             ") +
      guides(fill = guide_legend(title = NULL))

    v$plot_ROAS_warm <- ggplot(data = v$data_warm[[2]], aes(x = budget_total, y = profit)) +
      geom_line(color = "white") +
      ggtitle("Response Curve - Weekly") +
      theme(plot.title = element_text(hjust = 0.5))+
      scale_y_continuous(labels = formatter1000()) +
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
      theme(legend.text = element_text(colour = "#cdcdcd")) +
      theme(legend.position = "bottom") +
      theme(legend.title = element_blank()) +
      theme(axis.text = element_text(size = 12)) +
      theme(axis.title = element_text(size = 15)) +
      theme(plot.title = element_text(color = "#cdcdcd")) +
      theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))+
      xlab("Media Budget (MN$)

           ") +
      ylab("Mobile Registrations (‘000 Units)
             ")

    v$roas_data_warm <- v$data_warm[[2]]

    v$roas_data_hot <- v$data_hot[[2]]


    v$plot_hot <- ggplot() +
      geom_area(
        data = v$data_hot[[1]][, Value := gsub("budget_", "", Value)],
        aes(x = budget_total, y = data, fill = Value), colour = "black", alpha = 0.6, position = "stack"
      ) +
      ggtitle("Optimal Split cold - Weekly")+
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
      theme(legend.text = element_text(colour = "#cdcdcd")) +
      theme(legend.position = "bottom") +
      theme(legend.title = element_blank()) +
      theme(axis.text = element_text(size = 12)) +
      theme(axis.title = element_text(size = 15)) +
      scale_fill_manual(values = dd.col) +
      theme(legend.position = "bottom", legend.title = element_blank()) +
      theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))+
      theme(plot.title = element_text(color = "#cdcdcd")) +
      xlab("Media Budget (MN$)

           ") +
      ylab("Channel Share (%)

             ") +
      guides(fill = guide_legend(title = NULL))

    v$plot_ROAS_hot <- ggplot(data = v$data_hot[[2]], aes(x = budget_total, y = profit)) +
      geom_line(color = "white") +
      ggtitle("Response Curve - Weekly") +
      theme(plot.title = element_text(hjust = 0.5))+
      scale_y_continuous(labels = formatter1000()) +
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
      theme(legend.text = element_text(colour = "#cdcdcd")) +
      theme(legend.position = "bottom") +
      theme(legend.title = element_blank()) +
      theme(axis.text = element_text(size = 12)) +
      theme(axis.title = element_text(size = 15)) +
      theme(plot.title = element_text(color = "#cdcdcd")) +
      theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))+
      xlab("Media Budget (MN$)

           ") +
      ylab("Mobile Registrations (‘000 Units)
             ")







  })





  observeEvent(input$Button_sep, {
    if (length(test2_1) != 0) {


    # channles_budgets_sep_input <<- c(s_sep$one,
    #                              s_sep$two,
    #                              s_sep$three,
    #                              s_sep$four,
    #                              s_sep$five,
    #                              s_sep$six,
    #                              s_sep$seven,
    #                              s_sep$eight,
    #                              s_sep$nine,
    #                              s_sep$ten,
    #                              s_sep$eleven,
    #                              s_sep$twelve,
    #                              s_sep$thirteen,
    #                              s_sep$fiveteen,
    #                              s_sep$fourteen,
    #                              s_sep$sixteen,
    #                              s_sep$seventeen,
    #                              s_sep$eightteen,
    #                              s_sep$nineteen,
    #                              s_sep$s20,
    #                              s_sep$s21,
    #                              s_sep$s22,
    #                              s_sep$s23,
    #                              s_sep$s24,
    #                              s_sep$s25,
    #                              s_sep$s26,
    #                              s_sep$s27,
    #                              s_sep$s28,
    #                              s_sep$s29)
    process_data_sep()
    v_sep$data <- recalc_opt_2_sep(test2_1, input$twBudget_sep*10^6/input$Daterange_sep * 1.2)

    maxxx_budg <- subset(v_sep$data[[2]], profit > max(v_sep$data[[2]][['profit']])*0.99, select = c('budget_total'))[1][['budget_total']]
    maxxx_budg <<- floor(maxxx_budg*10)/10

    opt_split_low <- subset(v_sep$data[[1]], Budget %in% seq(0,maxxx_budg*1000000, 10000))


    if ((maxxx_budg+0.1) < (input$twBudget_sep/input$Daterange_sep) * 1.2) {
      sat_ratio <- subset(opt_split_low, budget_total == maxxx_budg)
      for (invest in seq((maxxx_budg+0.1)*1000000, input$twBudget_sep*10^6/input$Daterange_sep * 1.2, 10000)){
        table_temp <- sat_ratio
        table_temp[['Budget']] = invest
        table_temp[['budget_total']] = invest/1000000
        opt_split_low <- rbind(opt_split_low, table_temp)
      }
      v_sep$data[[1]] <- opt_split_low

      v_sep$data[[2]]$profit[v_sep$data[[2]]$budget_total>maxxx_budg] <- subset(v_sep$data[[2]], budget_total == maxxx_budg)[['profit']]
    }

    # xx <<- recalc_opt(channles_budgets_input)
    load_data_sep()
    v_sep$fintable <- 1
    xx<<- v_sep$data
    v_sep$roas_data <- v_sep$data[[2]]
    Dater_sep<-1
    Budget_sep <- input$twBudget_sep
    Dater_sep <- input$Daterange_sep

    v_sep$plot <- ggplot() +
      geom_area(
        data = v_sep$data[[1]][, Value := gsub("budget_", "", Value)],
        aes(x = budget_total, y = data, fill = Value), colour = "black", alpha = 0.6, position = "stack"
      ) +
      ggtitle("Optimal Split - Weekly")+
      scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
      scale_y_continuous(labels = function(x) paste0(x*100, "%")) +
      geom_vline(xintercept = round(Budget_sep/Dater_sep, digits = 1), linetype = "dotted", colour = "lightgreen", size = 0.8) +
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
      theme(legend.text = element_text(colour = "#cdcdcd")) +
      theme(legend.position = "bottom") +
      theme(legend.title = element_blank()) +
      theme(axis.text = element_text(size = 12)) +
      theme(axis.title = element_text(size = 15)) +
      scale_fill_manual(values = dd.col) +
      theme(legend.position = "bottom", legend.title = element_blank()) +
      theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))+
      theme(plot.title = element_text(color = "#cdcdcd")) +
      xlab("Media Budget (MN$)

           ") +
      ylab("Channel Share (%)

             ") +
      guides(fill = guide_legend(title = NULL))

    v_sep$plot_ROAS <- ggplot(data = v_sep$data[[2]], aes(x = budget_total, y = profit)) +
      geom_line(color = "white") +
      ggtitle("Response Curve - Weekly") +
      theme(plot.title = element_text(hjust = 0.5))+
      scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
      scale_y_continuous(labels = formatter1000()) +
      geom_point(x = round(Budget_sep/Dater_sep, digits = 1), y = v_sep$roas_data[budget_total == round(Budget_sep/Dater_sep, digits = 1)]$profit, color = "lightgreen", size = 2) +
      geom_segment(aes(x = round(Budget_sep/Dater_sep, digits = 1), y = 0, xend = round(Budget_sep/Dater_sep, digits = 1), yend = v_sep$roas_data[budget_total == round(Budget_sep/Dater_sep, digits = 1)]$profit),
                   linetype = "dotted", colour = "lightgreen", size = 0.8
      ) +
      geom_segment(aes(
        x = 0, y = v_sep$roas_data[budget_total == round(Budget_sep/Dater_sep, digits = 1)]$profit,
        xend = round(Budget_sep/Dater_sep, digits = 1), yend = v_sep$roas_data[budget_total == round(Budget_sep/Dater_sep, digits = 1)]$profit
      ),
      linetype = "dotted", colour = "lightgreen", size = 0.8
      )+

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
      theme(legend.text = element_text(colour = "#cdcdcd")) +
      theme(legend.position = "bottom") +
      theme(legend.title = element_blank()) +
      theme(axis.text = element_text(size = 12)) +
      theme(axis.title = element_text(size = 15)) +
      theme(plot.title = element_text(color = "#cdcdcd")) +
      theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))+
      xlab("Media Budget (MN$)

           ") +
      ylab("Mobile Registrations (‘000 Units)
             ")
    }else {
      shinyalert("Error", "No channels selected!", type = "error")
    }


  })

  ############################################################################



  observeEvent(input$Button_2, {

    # channles_budgets_input_2 <- c(s_2$one,
    #                               s_2$two,
    #                               s_2$three,
    #                               s_2$four,
    #                               s_2$five,
    #                               s_2$six,
    #                               s_2$seven,
    #                               s_2$eight,
    #                               s_2$nine,
    #                               s_2$ten,
    #                               s_2$elven,
    #                               s_2$twelve,
    #                               s_2$thirteen,
    #                               s_2$fourteen)
    # xx<<-(channles_budgets_input_2)
    process_data_2()
    v_2$data_cold <- recalc_opt_2(test1_cold, input$twBudget_2_cold*10^6/input$Daterange_2_cold)
    v_2$data_warm <- recalc_opt_2(test1_warm, input$twBudget_2_warm*10^6/input$Daterange_2_warm)
    v_2$data_hot <- recalc_opt_2(test1_hot, input$twBudget_2_hot*10^6/input$Daterange_2_hot)
    load_data_2()
    v_2$fintable <- 1
    # xx<<-v_2$data
    v_2$plot_cold <- ggplot() +
      geom_area(
        data = v_2$data_cold[[1]][, Value := gsub("budget_", "", Value)],
        aes(x = budget_total, y = data, fill = Value), colour = "black", alpha = 0.6, position = "stack"
      ) +
      ggtitle("Optimal Split - Weekly")+
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
      theme(legend.text = element_text(colour = "#cdcdcd")) +
      theme(legend.position = "bottom") +
      theme(legend.title = element_blank()) +
      theme(axis.text = element_text(size = 12)) +
      theme(axis.title = element_text(size = 15)) +
      scale_fill_manual(values = dd.col) +
      theme(legend.position = "bottom", legend.title = element_blank()) +
      theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))+
      theme(plot.title = element_text(color = "#cdcdcd")) +
      xlab("Media Budget (MN$)

           ") +
      ylab("Channel Share (%)

             ") +
      guides(fill = guide_legend(title = NULL))

    v_2$plot_ROAS_cold <- ggplot(data = v_2$data_cold[[2]], aes(x = budget_total, y = profit)) +
      geom_line(color = "white") +
      ggtitle("Response Curve - Weekly") +
      theme(plot.title = element_text(hjust = 0.5))+
      scale_y_continuous(labels = formatter1000()) +
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
      theme(legend.text = element_text(colour = "#cdcdcd")) +
      theme(legend.position = "bottom") +
      theme(legend.title = element_blank()) +
      theme(axis.text = element_text(size = 12)) +
      theme(axis.title = element_text(size = 15)) +
      theme(plot.title = element_text(color = "#cdcdcd")) +
      theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))+
      xlab("Media Budget (MN$)

           ") +
      ylab("Mobile Registrations (‘000 Units)
             ")

    v_2$roas_data_cold <- v_2$data_cold[[2]]








    v_2$plot_warm <- ggplot() +
      geom_area(
        data = v_2$data_warm[[1]][, Value := gsub("budget_", "", Value)],
        aes(x = budget_total, y = data, fill = Value), colour = "black", alpha = 0.6, position = "stack"
      ) +
      ggtitle("Optimal Split - Weekly")+
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
      theme(legend.text = element_text(colour = "#cdcdcd")) +
      theme(legend.position = "bottom") +
      theme(legend.title = element_blank()) +
      theme(axis.text = element_text(size = 12)) +
      theme(axis.title = element_text(size = 15)) +
      scale_fill_manual(values = dd.col) +
      theme(legend.position = "bottom", legend.title = element_blank()) +
      theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))+
      theme(plot.title = element_text(color = "#cdcdcd")) +
      xlab("Media Budget (MN$)

           ") +
      ylab("Channel Share (%)

             ") +
      guides(fill = guide_legend(title = NULL))

    v_2$plot_ROAS_warm <- ggplot(data = v_2$data_cold[[2]], aes(x = budget_total, y = profit)) +
      geom_line(color = "white") +
      ggtitle("Response Curve - Weekly") +
      theme(plot.title = element_text(hjust = 0.5))+
      scale_y_continuous(labels = formatter1000()) +
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
      theme(legend.text = element_text(colour = "#cdcdcd")) +
      theme(legend.position = "bottom") +
      theme(legend.title = element_blank()) +
      theme(axis.text = element_text(size = 12)) +
      theme(axis.title = element_text(size = 15)) +
      theme(plot.title = element_text(color = "#cdcdcd")) +
      theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))+
      xlab("Media Budget (MN$)

           ") +
      ylab("Mobile Registrations (‘000 Units)
             ")

    v_2$roas_data_warm <- v_2$data_warm[[2]]









    v_2$plot_hot <- ggplot() +
      geom_area(
        data = v_2$data_hot[[1]][, Value := gsub("budget_", "", Value)],
        aes(x = budget_total, y = data, fill = Value), colour = "black", alpha = 0.6, position = "stack"
      ) +
      ggtitle("Optimal Split - Weekly")+
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
      theme(legend.text = element_text(colour = "#cdcdcd")) +
      theme(legend.position = "bottom") +
      theme(legend.title = element_blank()) +
      theme(axis.text = element_text(size = 12)) +
      theme(axis.title = element_text(size = 15)) +
      scale_fill_manual(values = dd.col) +
      theme(legend.position = "bottom", legend.title = element_blank()) +
      theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))+
      theme(plot.title = element_text(color = "#cdcdcd")) +
      xlab("Media Budget (MN$)

           ") +
      ylab("Channel Share (%)

             ") +
      guides(fill = guide_legend(title = NULL))

    v_2$plot_ROAS_hot <- ggplot(data = v_2$data_hot[[2]], aes(x = budget_total, y = profit)) +
      geom_line(color = "white") +
      ggtitle("Response Curve - Weekly") +
      theme(plot.title = element_text(hjust = 0.5))+
      scale_y_continuous(labels = formatter1000()) +
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
      theme(legend.text = element_text(colour = "#cdcdcd")) +
      theme(legend.position = "bottom") +
      theme(legend.title = element_blank()) +
      theme(axis.text = element_text(size = 12)) +
      theme(axis.title = element_text(size = 15)) +
      theme(plot.title = element_text(color = "#cdcdcd")) +
      theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))+
      xlab("Media Budget (MN$)

           ") +
      ylab("Mobile Registrations (‘000 Units)
             ")

    v_2$roas_data_hot <- v_2$data_hot[[2]]



  })




  observeEvent(input$Button_2_sep, {

    # for (i in seq(1,length(channles_budgets_sep_input),1)){
    #   channles_budgets_sep_input[i]=0
    # }

    channles_budgets_sep_input <<- c(s_sep$one,
                                     s_sep$two,
                                     s_sep$three,
                                     s_sep$four,
                                     s_sep$five,
                                     s_sep$six,
                                     s_sep$seven,
                                     s_sep$eight,
                                     s_sep$nine,
                                     s_sep$ten,
                                     s_sep$eleven,
                                     s_sep$twelve,
                                     s_sep$thirteen,
                                     s_sep$fiveteen,
                                     s_sep$fourteen,
                                     s_sep$sixteen,
                                     s_sep$seventeen,
                                     s_sep$eightteen,
                                     s_sep$nineteen,
                                     s_sep$s20,
                                     s_sep$s21,
                                     s_sep$s22,
                                     s_sep$s23,
                                     s_sep$s24,
                                     s_sep$s25)

    channles_budgets_sep_input <- channles_budgets_sep_input/input$Daterange_2_sep

    loloas <<- channles_budgets_sep_input


    null_names <<- c(names(choiceVec_0_sep_cold_help[!names(choiceVec_0_sep_cold_help) %in% test2_1_cold]), names(choiceVec_0_sep_warm_help[!names(choiceVec_0_sep_warm_help) %in% test2_1_warm]), names(choiceVec_0_sep_hot_help[!names(choiceVec_0_sep_hot_help) %in% test2_1_hot]))
    for (i in null_names){
      j <- as.numeric(choiceVec_0_sep_help[[i]])
      channles_budgets_sep_input[2*j] <- 0
      channles_budgets_sep_input[2*j-1] <- 0 }


    min_sum <- 0
    for (i in seq(1,length(channles_budgets_sep_input),2)){
      min_sum <- min_sum + channles_budgets_sep_input[i]
    }

    max_sum <- 0
    for (i in seq(2,length(channles_budgets_sep_input),2)){
      max_sum <- max_sum + channles_budgets_sep_input[i]
    }


    if ((sum(channles_budgets_sep_input) != 0)&(min_sum < input$twBudget_2_sep/input$Daterange_2_sep)&(max_sum >= input$twBudget_2_sep/input$Daterange_2_sep)) {


    # null_names <<- c(names(choiceVec_0_sep_cold[!names(choiceVec_0_sep_cold) %in% test2_1_cold]), names(choiceVec_0_sep_warm[!names(choiceVec_0_sep_warm) %in% test2_1_warm]), names(choiceVec_0_sep_hot[!names(choiceVec_0_sep_hot) %in% test2_1_hot]))
    # for (i in null_names){
    #   j <- as.numeric(choiceVec_0_sep[[i]])
    #   channles_budgets_sep_input[2*j] <- 0
    #   channles_budgets_sep_input[2*j-1] <- 0 }

    # xx<<-(channles_budgets_input_2)
    min_sum <- 0
    for (i in seq(1, length(channles_budgets_sep_input)/2, 1)){
      min_sum <- min_sum + channles_budgets_sep_input[2*i-1]*1000000
    }
    process_data_2_sep()
    v_2_sep$data <- recalc_opt_sep(channles_budgets_sep_input, input$twBudget_2_sep*10^6/input$Daterange_2_sep * 1.2)

    fdfd <<- v_2_sep$data

    maxxx_budg <- subset(v_2_sep$data[[2]], profit > max(v_2_sep$data[[2]][['profit']])*0.99, select = c('budget_total'))[1][['budget_total']]
    maxxx_budg <<- floor(maxxx_budg*10)/10

    opt_split_low <- subset(v_2_sep$data[[1]], Budget %in% seq(0,maxxx_budg*1000000, 10000))
    opt_split_low_temp <<- subset(fdfd[[1]], Budget %in% seq(0,maxxx_budg*1000000, 10000))
    opt_roas_low <- subset(v_2_sep$data[[2]], budget_total %in% seq(0,maxxx_budg, 0.1))

    sat_ratio <- subset(opt_split_low, budget_total == maxxx_budg)
    sat_ratio_temp <<- subset(opt_split_low, budget_total == maxxx_budg)




    channles_budgets_sep_input_temp <<- channles_budgets_sep_input
    len_channels <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25)
    cho_chan <- list()
    rest_dict <- c()

    for (i in len_channels){
      if (channles_budgets_sep_input_temp[2*i] > 0){
        cho_chan <- append(cho_chan,media_vars_names_resp_sep[i])
        rest_dict[[media_vars_names_resp_sep[i]]] <- list(channles_budgets_sep_input_temp[2*i-1]*1000000,channles_budgets_sep_input_temp[2*i]*1000000)
      }
    }


    if ((maxxx_budg+0.1) < (input$twBudget_2_sep/input$Daterange_2_sep) * 1.2) {
      for (invest in seq((maxxx_budg+0.1)*1000000, (input$twBudget_2_sep*10^6/input$Daterange_2_sep) * 1.2, 10000)){
        table_temp <- sat_ratio
        table_temp[['Budget']] = invest
        table_temp[['budget_total']] = invest/1000000
        delt <- 0

        for (i in cho_chan){
          bad_chan <- list()
          calc_budg <- table_temp[table_temp[['Media']] == i][['data']]*table_temp[table_temp[['Media']] == i][['Budget']]

          if (calc_budg > rest_dict[[i]][[2]]){
            bad_chan <- append(bad_chan, i)
            delt <- delt + (calc_budg - rest_dict[[i]][[2]])
            table_temp[table_temp[['Media']] == i][['data']] <- rest_dict[[i]][[2]]/invest
          }
          }

        for (col in cho_chan[!(cho_chan %in% bad_chan)]){
          calc_budg <- table_temp[table_temp[['Media']] == col][['data']]*table_temp[table_temp[['Media']] == col][['Budget']]
          second_bad_channel <- list()
          if ((calc_budg + (delt/length(cho_chan[!(cho_chan %in% bad_chan)]))) > rest_dict[[col]][[2]]){
            second_bad_channel <- append(second_bad_channel, col)
            table_temp[table_temp[['Media']] == col][['data']] <- rest_dict[[col]][[2]]/invest
            delt <- delt - ((calc_budg + (delt/length(cho_chan[!(cho_chan %in% bad_chan)]))) - rest_dict[[col]][[2]])
          }
        }


        table_temp[!(table_temp[['Media']] %in% c(bad_chan, second_bad_channel))][['data']] <- table_temp[!(table_temp[['Media']] %in% c(bad_chan, second_bad_channel))][['data']] + (delt / length(cho_chan[!(cho_chan %in% c(bad_chan, second_bad_channel))]))/invest


        opt_split_low <- rbind(opt_split_low, table_temp)
      }
      v_2_sep$data[[1]] <- opt_split_low

      sat_roas <- subset(v_2_sep$data[[2]], budget_total == maxxx_budg)

      for (invest in seq((maxxx_budg+0.1)*1000000, input$twBudget_2_sep*10^6/input$Daterange_2_sep * 1.2, 10000)){
        table_temp <- sat_roas
        table_temp[['budget_total']] = invest/1000000
        opt_roas_low <- rbind(opt_roas_low, table_temp)
      }
      v_2_sep$data[[2]] <- opt_roas_low
    }

    kkk <<- v_2_sep$data[[2]]

    # v_2_sep$data <- subset(v_2_sep$data[[1]], Budget %in% seq(200000,100000000, 10000))
    load_data_2_sep()
    v_2_sep$fintable <- 1
    xx_lo<<-v_2_sep$data

    # xx_lo[[1]][xx_lo[[1]]$budget_total== floor((18/4*10)/10)][xx_lo[[1]][xx_lo[[1]]$budget_total== floor((18/4*10)/10)]$data>0.01]
    # xx_lo[[1]][xx_lo[[1]]$budget_total== ((18/4*10)/10)][xx_lo[[1]][xx_lo[[1]]$budget_total== ((18/4*10)/10)]$data>0.01]

    # v_2_sep$data[[1]] <- subset(xx_lo[[1]], Budget %in% seq(min_sum,100000000, 10000))
    v_2_sep$roas_data <- v_2_sep$data[[2]]
    Budget_sep_2 <- 1
    Budget_sep_2 <- input$twBudget_2_sep
    Dater_sep_2 <- input$Daterange_2_sep
    v_2_sep$plot <- ggplot() +
      geom_area(
        data = v_2_sep$data[[1]][, Value := gsub("budget_", "", Value)],
        aes(x = budget_total, y = data, fill = Value), colour = "black", alpha = 0.6, position = "stack"
      ) +
      ggtitle("Optimal Split - Weekly")+
      scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
      scale_y_continuous(labels = function(x) paste0(x*100, "%")) +
      geom_vline(xintercept = round(Budget_sep_2/Dater_sep_2, digits = 1), linetype = "dotted", colour = "lightgreen", size = 0.8) +
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
      theme(legend.text = element_text(colour = "#cdcdcd")) +
      theme(legend.position = "bottom") +
      theme(legend.title = element_blank()) +
      theme(axis.text = element_text(size = 12)) +
      theme(axis.title = element_text(size = 15)) +
      scale_fill_manual(values = dd.col) +
      theme(legend.position = "bottom", legend.title = element_blank()) +
      theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))+
      theme(plot.title = element_text(color = "#cdcdcd")) +
      xlab("Media Budget (MN$)

           ") +
      ylab("Channel Share (%)

             ") +
      guides(fill = guide_legend(title = NULL))




    v_2_sep$plot_ROAS <- ggplot(data = v_2_sep$data[[2]], aes(x = budget_total, y = profit)) +
      geom_line(color = "white") +
      ggtitle("Response Curve - Weekly") +
      theme(plot.title = element_text(hjust = 0.5))+
      scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
      scale_y_continuous(labels = formatter1000()) +
      geom_point(x = round(Budget_sep_2/Dater_sep_2, digits = 1), y = v_2_sep$roas_data[budget_total == round(Budget_sep_2/Dater_sep_2, digits = 1)]$profit, color = "lightgreen", size = 2)+
      geom_segment(aes(x = round(Budget_sep_2/Dater_sep_2, digits = 1), y = 0, xend = round(Budget_sep_2/Dater_sep_2, digits = 1), yend = v_2_sep$roas_data[budget_total == round(Budget_sep_2/Dater_sep_2, digits = 1)]$profit),
                   linetype = "dotted", colour = "lightgreen", size = 0.8
      ) +
      geom_segment(aes(
        x = 0, y = v_2_sep$roas_data[budget_total == round(Budget_sep_2/Dater_sep_2, digits = 1)]$profit,
        xend = round(Budget_sep_2/Dater_sep_2, digits = 1), yend = v_2_sep$roas_data[budget_total == round(Budget_sep_2/Dater_sep_2, digits = 1)]$profit),
        linetype = "dotted", colour = "lightgreen", size = 0.8
      ) +
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
      theme(legend.text = element_text(colour = "#cdcdcd")) +
      theme(legend.position = "bottom") +
      theme(legend.title = element_blank()) +
      theme(axis.text = element_text(size = 12)) +
      theme(axis.title = element_text(size = 15)) +
      theme(plot.title = element_text(color = "#cdcdcd")) +
      theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))+
      xlab("Media Budget (MN$)

           ") +
      ylab("Mobile Registrations (‘000 Units)
             ")

    }else {
      if (sum(channles_budgets_sep_input) == 0){
        shinyalert("Error", "No channels selected!", type = "error")}
      if (min_sum >= input$twBudget_2_sep/input$Daterange_2_sep){
        shinyalert("Error", "The amount of limits minimums is greater than selected budget", type = "error")}
      if ((max_sum < input$twBudget_2_sep/input$Daterange_2_sep) & (sum(channles_budgets_sep_input) > 0)){
        shinyalert("Error", "The amount of limits maximums is less than selected budget", type = "error")}

    }

  })









  output$Budget_BRAND_optimal_splits_cold <- renderPlotly({
    ggplotly(v_2$plot_cold)
    #          scale_x_continuous(breaks = seq(0, 151, 0.5)) +
    #          geom_vline(xintercept = round_any(input$twBudget/input$Daterange, 0.5, f = floor), linetype = "dotted", colour = "lightgreen", size = 0.8)) %>%
    # layout(legend = list(orientation = "h", y = -0.2))
  })

  output$Budget_BRAND_optimal_splits_warm <- renderPlotly({
    ggplotly(v_2$plot_warm)
    #          scale_x_continuous(breaks = seq(0, 151, 0.5)) +
    #          geom_vline(xintercept = round_any(input$twBudget/input$Daterange, 0.5, f = floor), linetype = "dotted", colour = "lightgreen", size = 0.8)) %>%
    # layout(legend = list(orientation = "h", y = -0.2))
  })

  output$Budget_BRAND_optimal_splits_hot <- renderPlotly({
    ggplotly(v_2$plot_hot)
    #          scale_x_continuous(breaks = seq(0, 151, 0.5)) +
    #          geom_vline(xintercept = round_any(input$twBudget/input$Daterange, 0.5, f = floor), linetype = "dotted", colour = "lightgreen", size = 0.8)) %>%
    # layout(legend = list(orientation = "h", y = -0.2))
  })



  output$Budget_BRAND_optimal_splits_sep <- renderPlotly({
    ggplotly(v_sep$plot)%>%
    #            scale_x_continuous(breaks = seq(0, 151, 0.5)) +
    #            geom_vline(xintercept = round_any(input$twBudget_sep/input$Daterange_sep, 0.5, f = floor), linetype = "dotted", colour = "lightgreen", size = 0.8))
      layout(legend = list(orientation = "h", y = -0.2))
  })




  output$Budget_BRAND_optimal_splits_cold_2 <- renderPlotly({
    ggplotly(v$plot_cold
             #          scale_x_continuous(breaks = seq(0, 151, 0.5)) +
             #          geom_vline(xintercept = round_any(input$twBudget_2/input$Daterange_2, 0.5, f = floor), linetype = "dotted", colour = "lightgreen", size = 0.8)) %>%
             # layout(legend = list(orientation = "h", y = -0.2))
    )})

  output$Budget_BRAND_optimal_splits_warm_2 <- renderPlotly({
    ggplotly(v$plot_warm
             #          scale_x_continuous(breaks = seq(0, 151, 0.5)) +
             #          geom_vline(xintercept = round_any(input$twBudget_2/input$Daterange_2, 0.5, f = floor), linetype = "dotted", colour = "lightgreen", size = 0.8)) %>%
             # layout(legend = list(orientation = "h", y = -0.2))
    )})


  output$Budget_BRAND_optimal_splits_hot_2 <- renderPlotly({
    ggplotly(v$plot_hot
             #          scale_x_continuous(breaks = seq(0, 151, 0.5)) +
             #          geom_vline(xintercept = round_any(input$twBudget_2/input$Daterange_2, 0.5, f = floor), linetype = "dotted", colour = "lightgreen", size = 0.8)) %>%
             # layout(legend = list(orientation = "h", y = -0.2))
    )})


  output$Budget_BRAND_optimal_splits_2_sep <- renderPlotly({
    ggplotly(v_2_sep$plot) %>%
    #          scale_x_continuous(breaks = seq(0, 151, 0.5)) +
    #          geom_vline(xintercept = round_any(input$twBudget_2_sep/input$Daterange_2_sep, 0.5, f = floor), linetype = "dotted", colour = "lightgreen", size = 0.8))
    layout(legend = list(orientation = "h", y = -0.2))
  })


  output$ROAS_table <- renderUI({
    tableHTML(ROAS_table, rownames = FALSE, widths=rep(300, ncol(ROAS_table))) %>%
      add_css_header(css = list(c("text-align", "background-color", "color"), c("center", "#848c8c", "white")),
                     headers = 1:ncol(ROAS_table)) %>%
      add_css_row(css = list(c("text-align", "color"), c("center", "white")),
                  rows = 1:4) %>%
      add_css_column(css = list(c("background-color"), c("#848c8c")),
                     columns = c(1))%>%
      add_css_footer(css = list(c('text-align', 'color'), c('left', 'black')))
  })

  output$ROAS_table_hot <- renderUI({
    tableHTML(ROAS_table_hot, rownames = FALSE, widths = rep('315px', ncol(ROAS_table_hot))) %>%
      add_css_header(css = list(c("text-align", "background-color", "color"), c("center", "#ff7256", "white")),
                     headers = 1:ncol(ROAS_table_hot)) %>%
      add_css_row(css = list(c("text-align", "color"), c("center", "white")),
                  rows = 1:4) %>%
      add_css_column(css = list(c("background-color"), c("#ff7256")),
                     columns = c(1))
  })

  output$ROAS_table_warm <- renderUI({
    tableHTML(ROAS_table_warm, rownames = FALSE, widths=rep(200, ncol(ROAS_table_warm))) %>%
      add_css_header(css = list(c("text-align", "background-color", "color"), c("center", "#ff7f24", "white")),
                     headers = 1:ncol(ROAS_table_warm)) %>%
      add_css_row(css = list(c("text-align", "color"), c("center", "white")),
                  rows = 1:4) %>%
      add_css_column(css = list(c("background-color"), c("#ff7f24")),
                     columns = c(1))
  })

  output$ROAS_table_cold <- renderUI({
    tableHTML(ROAS_table_cold, rownames = FALSE, widths=rep('182px', ncol(ROAS_table_cold))) %>%
      add_css_header(css = list(c("text-align", "background-color", "color"), c("center", "#3c8dbc", "white")),
                     headers = 1:ncol(ROAS_table_cold)) %>%
      add_css_row(css = list(c("text-align", "color"), c("center", "white")),
                  rows = 1:4) %>%
      add_css_column(css = list(c("background-color"), c("#3c8dbc")),
                     columns = c(1))
  })


  output$Budget_BRAND_optimal_ROAS <- renderPlotly({
    ggplotly(v_2$plot_ROAS_cold + ggtitle("The chart below shows cumulative response from selected channels and budget")
             + theme(plot.title = element_text(size = 13, face = "plain", hjust = 0))
             # scale_x_continuous(breaks = seq(0, 151, 0.5)) +
             # geom_point(x = round_any(input$twBudget/input$Daterange, 0.5, f = floor), y = v$roas_data[budget_total == round_any(input$twBudget/input$Daterange, 0.5, f = floor)]$profit, color = "lightgreen", size = 2) +
             # geom_segment(aes(x = round_any(input$twBudget/input$Daterange, 0.5, f = floor), y = 0, xend = round_any(input$twBudget/input$Daterange, 0.5, f = floor), yend = v$roas_data[budget_total == round_any(input$twBudget/input$Daterange, 0.5, f = floor)]$profit),
             #              linetype = "dotted", colour = "lightgreen", size = 0.8
             # ) +
             # geom_segment(aes(
             #   x = 0, y = v$roas_data[budget_total == round_any(input$twBudget/input$Daterange, 0.5, f = floor)]$profit,
             #   xend = round_any(input$twBudget/input$Daterange, 0.5, f = floor), yend = v$roas_data[budget_total == round_any(input$twBudget/input$Daterange, 0.5, f = floor)]$profit
             # ),
             # linetype = "dotted", colour = "lightgreen", size = 0.8
             # )
    )
  })



  output$Budget_BRAND_optimal_ROAS_sep <- renderPlotly({
    ggplotly(v_sep$plot_ROAS)
             # scale_x_continuous(breaks = seq(0, 151, 0.5)) +
             # geom_point(x = round_any(input$twBudget_sep/input$Daterange_sep, 0.5, f = floor), y = v_sep$roas_data[budget_total == round_any(input$twBudget_sep/input$Daterange_sep, 0.5, f = floor)]$profit, color = "lightgreen", size = 2) +
             # geom_segment(aes(x = round_any(input$twBudget_sep/input$Daterange_sep, 0.5, f = floor), y = 0, xend = round_any(input$twBudget_sep/input$Daterange_sep, 0.5, f = floor), yend = v_sep$roas_data[budget_total == round_any(input$twBudget_sep/input$Daterange_sep, 0.5, f = floor)]$profit),
             #              linetype = "dotted", colour = "lightgreen", size = 0.8
             # ) +
             # geom_segment(aes(
             #   x = 0, y = v_sep$roas_data[budget_total == round_any(input$twBudget_sep/input$Daterange_sep, 0.5, f = floor)]$profit,
             #   xend = round_any(input$twBudget_sep/input$Daterange_sep, 0.5, f = floor), yend = v_sep$roas_data[budget_total == round_any(input$twBudget_sep/input$Daterange_sep, 0.5, f = floor)]$profit
             # ),
             # linetype = "dotted", colour = "lightgreen", size = 0.8
             # ))

  })



  output$Budget_BRAND_optimal_ROAS_2 <- renderPlotly({
    ggplotly(v$plot_ROAS_cold + ggtitle("The chart below shows cumulative response from selected channels and budget")
             + theme(plot.title = element_text(size = 13, face = "plain", hjust = 0))
             # scale_x_continuous(breaks = seq(0, 151, 0.5)) +
             # geom_point(x = round_any(input$twBudget_2/input$Daterange_2, 0.5, f = floor), y = v_2$roas_data[budget_total == round_any(input$twBudget_2/input$Daterange_2, 0.5, f = floor)]$profit, color = "lightgreen", size = 2)+
             # geom_segment(aes(x = round_any(input$twBudget_2/input$Daterange_2, 0.5, f = floor), y = 0, xend = round_any(input$twBudget_2/input$Daterange_2, 0.5, f = floor), yend = v_2$roas_data[budget_total == round_any(input$twBudget_2/input$Daterange_2, 0.5, f = floor)]$profit),
             #              linetype = "dotted", colour = "lightgreen", size = 0.8
             # ) +
             # geom_segment(aes(
             #   x = 0, y = v_2$roas_data[budget_total == round_any(input$twBudget_2/input$Daterange_2, 0.5, f = floor)]$profit,
             #   xend = round_any(input$twBudget_2/input$Daterange_2, 0.5, f = floor), yend = v_2$roas_data[budget_total == round_any(input$twBudget_2/input$Daterange_2, 0.5, f = floor)]$profit),
             #   linetype = "dotted", colour = "lightgreen", size = 0.8
             # )
    )
  })



  output$Budget_BRAND_optimal_ROAS_2_sep <- renderPlotly({
    ggplotly(v_2_sep$plot_ROAS)
    # scale_x_continuous(breaks = seq(0, 151, 0.5)) +
    # geom_point(x = round_any(input$twBudget_2_sep/input$Daterange_2_sep, 0.5, f = floor), y = v_2_sep$roas_data[budget_total == round_any(input$twBudget_2_sep/input$Daterange_2_sep, 0.5, f = floor)]$profit, color = "lightgreen", size = 2)+
    # geom_segment(aes(x = round_any(input$twBudget_2_sep/input$Daterange_2_sep, 0.5, f = floor), y = 0, xend = round_any(input$twBudget_2_sep/input$Daterange_2_sep, 0.5, f = floor), yend = v_2_sep$roas_data[budget_total == round_any(input$twBudget_2_sep/input$Daterange_2_sep, 0.5, f = floor)]$profit),
    #              linetype = "dotted", colour = "lightgreen", size = 0.8
    # ) +
    # geom_segment(aes(
    #   x = 0, y = v_2_sep$roas_data[budget_total == round_any(input$twBudget_2_sep/input$Daterange_2_sep, 0.5, f = floor)]$profit,
    #   xend = round_any(input$twBudget_2_sep/input$Daterange_2_sep, 0.5, f = floor), yend = v_2_sep$roas_data[budget_total == round_any(input$twBudget_2_sep/input$Daterange_2_sep, 0.5, f = floor)]$profit),
    #   linetype = "dotted", colour = "lightgreen", size = 0.8
    # ))
  })






  get_optbug_spot <- function(df, budget) {
    return(df[Budget == budget])
  }
})




