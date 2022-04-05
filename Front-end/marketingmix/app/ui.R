# Define libraries required by the dashboard
# libraries <- c(
#   "data.table", "ggplot2", "gridExtra", "plotly", "shiny",
#   "shinydashboard", "tableHTML", "dashboardthemes",
#   "shinyWidgets", "shinymanager", "shinythemes", "shinyjs", "shinycssloaders",
#   "data.table", "ggplot2", "gridExtra", "plotly", "shiny",
#   "shinydashboard", "tableHTML", "dashboardthemes",
#   "shinyWidgets", "shinyjs", "shinymanager", "shinycssloaders",
#   'data.table', 'dplyr', 'ggplot2', 'gridExtra', 'plotly', 'shiny', 'shinyWidgets', 'plyr'
# )
#
# invisible(lapply(libraries, require, character.only = T))
# rm(list = ls())
library("data.table")
library("data.table")
library("ggplot2")
library("gridExtra")
library("plotly")
library("shiny")
library("shinydashboard")
library("tableHTML")
library("dashboardthemes")
library("shinyWidgets")
library("shinymanager")
library("shinythemes")
library("shinyjs")
library("shinycssloaders")
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
library('data.table')
library('dplyr')
library('ggplot2')
library('gridExtra')
library('plotly')
library('shiny')
library('shinyWidgets')
library('plyr')
library('shinyalert')

# Set side bar menu
sidebar <- dashboardSidebar(
  width = 260,
  sidebarMenu(
    menuItem("Incremental Volume", tabName = "model_fit", icon = icon("chart-bar")),
    # menuItem("Impacts", tabName = "model_fit", icon = icon("funnel-dollar"))),
    menuItem("Response Curves", tabName = "responses", icon = icon("funnel-dollar"),
             menuItem("ROAS", tabName = "response_curves_2", icon = icon("funnel-dollar")),
             menuItem("Model Response", tabName = "response_curves", icon = icon("funnel-dollar"))),
    menuItem("Short Term Sales Optimization", tabName = "dynamic_opt", icon = icon("sliders-h"),
             menuSubItem("Without Restrictions",icon = icon("flask"), tabName = "dynamic_optimization_sep"),
             menuSubItem("With Restrictions - Optional",icon = icon("fire"), tabName = "dynamic_optimization_sep_2"))
    # menuItem('Split by temperature', icon = icon("funnel-dollar"),
    #          menuSubItem("Without Restrictions",icon = icon("flask"), tabName = "dynamic_optimization"),
    #          menuSubItem("With Restrictions - Optional",icon = icon("fire"), tabName = "dynamic_optimization_2")))
    # tags$style(HTML("
    #   .main-sidebar{
    #     width: 250px;
    #   }
    # "))

  )
)

# Structure the body of the dashboard
body <- dashboardBody(
  # Define dashboard theme
  tags$style(make_css(list(c('.skin-blue .main-sidebar .sidebar .sidebar-menu a',' .skin-blue .main-header .logo','.skin-blue .main-sidebar .sidebar .sidebar-menu .treeview-menu','p','.nav > li > a','.box-header .box-title'),
                           'color',
                           '#ba9765'))),
  tags$style(make_css(list('.nav-tabs',"border-bottom-color","#ba9765"))),
  tags$style(make_css(list(c('.btn','.btn.focus', '.btn:focus','.btn:hover','.skin-blue .main-header .navbar .sidebar-toggle'),'color',"#ba9765"))),
  tags$style(make_css(list(c('.irs-slider.single',
                             '.nav-tabs > li.active > a, .nav-tabs > li.active > a:focus, .nav-tabs > li.active > a:hover'),
                           c('color'),
                           c('white')

                           ))),
  tags$style(make_css(list('.nav > li > a:focus, .nav > li > a:hover',c('background-color','color','border-color'),c('rgb(70, 80, 90)','#ba9765','#ba9765')))),
  tags$style(make_css(list(c('.well','.irs-bar','.irs-bar-edge'),c('background-color','color','border-color'),c('rgb(70, 80, 90)','#ba9765','#ba9765')))),
  tags$style(make_css(list(c('.btn','.irs-min','.irs-max','.irs-single'),c('background-color','color','border-color'),c('rgb(70, 80, 90)','#ba9765','#ba9765')))),
  tags$head(tags$style(HTML('.small-box .icon-large {top: 5px;}'))),
  tags$style(HTML("
      .my_table .table>tbody>tr>td, .table>tbody>tr>th, .table>tfoot>tr>td, .table>tfoot>tr>th, .table>thead>tr>td, .table>thead>tr>th {
        padding: 8px;
        line-height: 1.42857143;
        vertical-align: top;
        border-top: 3px solid blue;
      }
    ")
),
  tags$style(".small-box.bg-aqua { background-color: #7d7a7a !important; color: #000000 !important; }"),
  tags$style(HTML('.small-box .main-header {top: 125px;}')),
  tags$style("#ks_final_table_sep_title{;
                                   font-size: 25px;
                                   padding-left: 15px;
                                   }"
  ),
  tags$style("#ks_final_table_sep_one_title{;
                                     font-size: 25px;
                                     padding-left: 15px;
                                     }"
  ),
  tags$style("#ks_final_table_2_sep_title{;
                                       font-size: 25px;
                                       padding-left: 15px;
                                       }"
  ),
  tags$style("#ks_final_table_2_sep_one_title{;
                                       font-size: 25px;
                                       padding-left: 15px;
                                       }"
  ),
  tags$style(".small-box.bg-blue { background-color: #3c8dbc !important; color: #000000 !important; }"),
  tags$style(".small-box.bg-yellow { background-color: #ff7f24 !important; color: #000000 !important; }"),
  tags$style(".small-box.bg-red { background-color: #ff7256 !important; color: #000000 !important; }"),

  shinyDashboardThemes(theme = "grey_dark"),

  useShinyjs(),

  useShinyalert(),

  tabItems(
    # Tab with model fit and decomposition barplot
    tabItem(
      tabName = "model_fit",
      fluidPage(
        h3('Incremental Volume - Registrations', style = "padding-left: 0px;"),
        box(
          solidHeader = FALSE, collapsible = F,
          plotlyOutput("Re_model_value", height = 400),
          width = NULL),
        h3('Incremental Volume - By Channels (%)', style = "padding-left: 0px;"),
        box(
          solidHeader = FALSE, collapsible = F,
          plotlyOutput("Re_model_volume", height = 400),
          h5('*Channels with >= 1% contribution are included'),
          width = NULL),
        h3('Incremental Volume - By Temperature (%)', style = "padding-left: 0px;"),
        # box(h3('Incremental Volume - By Temperature (%)'), solidHeader = TRUE, collapsible = TRUE,
        fluidRow(
          box(valueBox(
            value = tags$p("Hot", style = "font-size: 75%; text-align:center"),
            subtitle = NULL,
            color = "red",
            icon = NULL,
            width = NULL
          ),
          plotlyOutput("Re_model_volume_hot", height = 700), width = 4),
          box(valueBox(
            value = tags$p("Warm", style = "font-size: 75%; text-align:center"),
            subtitle = NULL,
            color = "yellow",
            icon = NULL,
            width = NULL
          ),
          plotlyOutput("Re_model_volume_warm", height = 700), width = 4),
          box(valueBox(
            value = tags$p("Cold", style = "font-size: 75%; text-align:center"),
            subtitle = NULL,
            color = "blue",
            icon = NULL,
            width = NULL
          ),
          plotlyOutput("Re_model_volume_cold", height = 700), width = 4)),
        h5('*Channels with >= 1% contribution are included')
        # box(
        #   solidHeader = FALSE, collapsible = TRUE,
        #   plotlyOutput("Re_model_volume_warm", height = 400), width = 12),
        # box(
        #   solidHeader = FALSE, collapsible = TRUE,
        #   plotlyOutput("Re_model_volume_hot", height = 400), width = 12),
        # h5('*Channels with >1% contribution are included'),
        # width = 12)
      )
    ),



    # Tab with response curves and ROASes
    tabItem(
      tabName = "response_curves",
      uiOutput("ks_curves")
    ),

    tabItem(
      tabName = "response_curves_2",
      uiOutput("ks_curves_2")
    ),











    # Optimisation tool tab
    tabItem(
      tabName = "dynamic_optimization",
      uiOutput("ks_slider_12th"),
      # uiOutput("sliders"),
      box(
        #   solidHeader = FALSE, collapsible = TRUE,
        # h4('The chart below shows cumulative response from selected channels and budget'),
        plotlyOutput("Budget_BRAND_optimal_ROAS", height = 700) %>%
          withSpinner(color="#0dc5c1", id = 'spinner_12th'), width = 12, collapsible = TRUE),
      box(
        plotlyOutput("Budget_BRAND_optimal_splits_cold", height = 700) %>% withSpinner(color="#0dc5c1", id = 'spinner_12th_cold'),
        width = 12, collapsible = TRUE
      ),
      box(
        plotlyOutput("Budget_BRAND_optimal_splits_warm", height = 700) %>% withSpinner(color="#0dc5c1", id = 'spinner_12th_warm'),
        width = 12, collapsible = TRUE
      ),
      box(
        plotlyOutput("Budget_BRAND_optimal_splits_hot", height = 700) %>% withSpinner(color="#0dc5c1", id = 'spinner_12th_hot'),
        width = 12, collapsible = TRUE
      ),
      box(uiOutput("ks_final_table"), width = 12, collapsible = TRUE),
      uiOutput("ks_12th"),
      box(uiOutput("ks_final_table_one"), width = 12, collapsible = TRUE)
    ),








    tabItem(
      tabName = "dynamic_optimization_2",
      uiOutput("ks_slider_22th"),
      # uiOutput("sliders"),
      box(
        #   solidHeader = FALSE, collapsible = TRUE,
        # h4('The chart below shows cumulative response from selected channels and budget'),
        plotlyOutput("Budget_BRAND_optimal_ROAS_2", height = 700) %>%
          withSpinner(color="#0dc5c1", id = 'spinner_22th'), width = 12, collapsible = TRUE),
      box(
        plotlyOutput("Budget_BRAND_optimal_splits_cold_2", height = 700) %>% withSpinner(color="#0dc5c1", id = 'spinner_22th_cold'),
        width = 12, collapsible = TRUE
      ),
      box(
        plotlyOutput("Budget_BRAND_optimal_splits_warm_2", height = 700) %>% withSpinner(color="#0dc5c1", id = 'spinner_22th_warm'),
        width = 12, collapsible = TRUE
      ),
      box(
        plotlyOutput("Budget_BRAND_optimal_splits_hot_2", height = 700) %>% withSpinner(color="#0dc5c1", id = 'spinner_22th_hot'),
        width = 12, collapsible = TRUE
      ),
      box(uiOutput("ks_final_table_2"),  width = 12, collapsible = TRUE),
      uiOutput("ks_22th"),
      box(uiOutput("ks_final_table_2_one"),  width = 12, collapsible = TRUE),
      width = 12
    ),












    tabItem(
      tabName = "dynamic_optimization_sep",
      fluidRow(

        h2('Welcome to the Dynamic Optimization page!', style = "padding-left: 15px;"),
        h4(HTML(paste0("This segment uses model results to calculate optimal budget split to maximize short-term digital paid media sales. To calculate the optimal split, please use the instructions in ‘How To Use’ box")), style = "padding-left: 15px;", div(style = "height:25px;")),

        uiOutput("ks_slider_12th_sep"),
        # uiOutput("sliders"),

        box(
          #   solidHeader = FALSE, collapsible = TRUE,
          h4('The chart below shows cumulative response from selected channels and budget'),
          plotlyOutput("Budget_BRAND_optimal_ROAS_sep", height = 700) %>%
            withSpinner(color="#0dc5c1"), width = 12, collapsible = F),
        box(
          plotlyOutput("Budget_BRAND_optimal_splits_sep", height = 700) %>% withSpinner(color="#0dc5c1"),
          width = 12, collapsible = F
        ),
        textOutput('ks_final_table_sep_title'),
        uiOutput("ks_final_table_sep"),
        textOutput('ks_final_table_sep_one_title'),
        uiOutput("ks_final_table_one_sep"),
      )),


    tabItem(
      tabName = "dynamic_optimization_sep_2",
      fluidRow(
        h2('Welcome to the Dynamic Optimization page!', style = "padding-left: 15px;"),
        h4(HTML(paste0("This segment uses model results to calculate optimal budget split to maximize short-term digital paid media sales. To calculate the optimal split, please use the instructions in ‘How To Use’ box")), style = "padding-left: 15px;", div(style = "height:25px;")),
        uiOutput("ks_slider_22th_sep"),
        # uiOutput("sliders"),

        box(
          #   solidHeader = FALSE, collapsible = TRUE,
          h4('The chart below shows cumulative response from selected channels and budget'),
          plotlyOutput("Budget_BRAND_optimal_ROAS_2_sep", height = 700) %>% withSpinner(color="#0dc5c1"), width = 12, collapsible = F),
        box(
          plotlyOutput("Budget_BRAND_optimal_splits_2_sep", height = 700) %>% withSpinner(color="#0dc5c1"),
          width = 12, collapsible = F
        ),
        textOutput('ks_final_table_2_sep_title'),
        uiOutput("ks_final_table_2_sep"),
        textOutput('ks_final_table_2_sep_one_title'),
        uiOutput("ks_final_table_2_one_sep"),
        width = 12
      ))





  )
)


# Define UI for application
# secure_app(theme = shinythemes::shinytheme("flatly"),
dashboardPage(
  # Define title of the dashboard
  dashboardHeader(title = "Digital ROAS - Mobile"),
  sidebar,
  body

)
