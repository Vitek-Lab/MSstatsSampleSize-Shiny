###############################################################
## MSstatsSampleSize Shiny app
###############################################################
dashboardPage(
    skin="black",
    ##### Header #####
    header =  dashboardHeader(
      title = "MSstatsSampleSize - Shiny",
      titleWidth = 300
    ),
    #### SideBar ####
    sidebar =  dashboardSidebar(
      shinyjs::useShinyjs(),
      width = 300,
      sidebarMenu(
        id="tabs",
        ##### Home Tab ####
        menuItem("Home", tabName = "home", icon = icon("home")),
        #### Data Import Tab ####
        menuItem("Import Data", tabName = "import_data",
                 icon = icon("file-import"),
                 ##### Select Input for Type of Data ####
                 selectInput("data_format", "Select Data Type",
                             choice = list("Protein-level quantification" = "standard", 
                                           "Example from MSstatsSampleSize" = "examples")),
                 ##### File Input for Protein Abundance File ####
                 fileInput("standard_count", "Select Protein Abundance File",
                           multiple = FALSE,
                           accept = c("text/csv",
                                      "text/comma-separated-values,text/plain",
                                      ".csv", "text/tab-separated-values", ".tsv")),
                 ##### File Input for Annotation File #####
                 fileInput("standard_annot", "Select sample annotation file",
                           multiple = FALSE,
                           accept = c("text/csv",
                                      "text/comma-separated-values,text/plain",
                                      ".csv", "text/tab-separated-values", ".tsv")),
                 ##### Action Button To load data #####
                 actionButton("import_data", "Import dataset", 
                              icon = icon("file-import"))
        ),
        menuItem("Explore Data", tabName = "explore_data", icon = icon("tv")),
        #### Data Simulation Tab ####
        menuItem("Simulate datasets", 
                 tabName = "explore_simulated", icon = icon("project-diagram")),
        #### Analyze Simulation Tab ####
        menuItem("Analyze the simulated datasets", 
                 tabName = "analyse_simulated", icon = icon("vial"))
      )
    ),
    ##### Body ####
    body = dashboardBody(
      shinyjs::useShinyjs(),
      tabItems(
        #### Home Tab ####
        tabItem(
          tabName = "home",
          includeMarkdown("www/Welcome.md")
        ),
        #### Import Data Tab ####
        tabItem(
          tabName = "import_data"
        ),
        ##### Exploration Visuals #####
        tabItem(
          tabName = "explore_data",
          h1(textOutput("dataset_name")),
          br(),
          fluidRow(
            column(6, DT::dataTableOutput("sum_table")),
            column(6, DT::dataTableOutput("cond_sum_table"))
          ),
          br(),
          fluidRow(
            box(title = "QC Box Plot",
                width = 6,
                plotly::plotlyOutput("global_boxplot")
            ),
            box(title = "Mean-Variance Plot",
                width = 6,
                plotOutput("mean_sd_plot")
            )
          )
        ),
        #### Data Simulation Tab #####
        tabItem(
          tabName = "explore_simulated",
          h1("Simulate datasets"),
          htmlOutput("explore_simulated_content")
        ),
        #### Analyse Simulation Tab ####
        tabItem(
          tabName = "analyse_simulated",
          h1("Analyze the simulated datasets"),
          htmlOutput("analyse_simulated_content")
        )
      )
    )
)