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
      width = 230,
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
          #### Summary Tables ####
          fluidRow(
            column(6, DT::dataTableOutput("sum_table")),
            column(6, DT::dataTableOutput("cond_sum_table"))
          ),
          br(),
          #### Box plots and HeatMap ####
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
          ##### Checkboxes for file Input and set seed options ####
          fluidRow(
            column(3, checkboxInput(inputId = "upload_params",
                                    label = "Upload Simulation Parameters from csv")),
            column(3, checkboxInput(input = "set_seed",
                                    label = "Set Seed value 1212"))
          ),
          #### File Input to upload a simulation parameter csv ####
          fileInput(input = "param_input", label = "Upload Parameters in specified format",
                    multiple = F, accept = c("text/csv",
                                             "text/comma-separated-values,text/plain",
                                             ".csv", "text/tab-separated-values", ".tsv")),
          fluidRow(
            #### Box to input simulation parameters from the dashboard ######
            box(id = "param_box",
                width = 3,
                h4("Parameters For Simulating DataSets"),
                numericInput(inputId = "n_sim", label = "Number of Simulations",
                             value = 10, min = 10, max = 500, step = 1),
                textInput(inputId = "exp_fc", label = "Expected fold changes of proteins",
                          value = "data", placeholder = "0,1,2"),
                textInput(inputId = "exp_fc_name", label = "Names of Expected Fold Change",
                          value = NULL),
                textInput(inputId = "diff_prot", label = "List Diff Protein",
                          value = NULL),
                selectInput(inputId = "sel_sim_prot", label = "Select Simulated Proteins",
                            choices = c("proportion", "number"),
                            selected = "proportion"),
                numericInput(inputId = "prot_prop", label = "Protein Proportion",
                             value = 1, min = 0, max = 1, step = 0.01),
                numericInput(inputId = "prot_num", label = "Protein Number",
                             value = 1000, max = 1000, min = 25),
                numericInput(inputId = "n_samp_grp", label = "Samples per group",
                             value = 50, min = 50, max = 1000, step = 50),
                selectInput(inputId = "sim_val", label = "Simulate Validation Set",
                            choices = c(T,F), selected = F),
                numericInput(inputId = "n_val_samp_grp", label = "Valid samples per group",
                             value = 50, min = 50, max = 1000, step = 50),
                actionButton(inputId = "simulate", label = "Simulate Data",
                             icon =  icon("project-diagram"))
            ),
            #### Simulation details section #####
            box(title = "Simulated Datasets",
                width = 9,
                fluidRow(
                  ##### Select individual simulation, previous and next buttons #####
                  column(4, selectInput(inputId = "simulations", label = "Simulations", choices = NULL)),
                  column(2, actionButton(inputId = "back", label = "Previous",
                                         icon = icon("arrow-left"),
                                         style = "margin-top: 25px;",
                                         width = '100px')),
                  column(2, actionButton(inputId = "fwd", label = "Next",
                                         icon = icon("arrow-right"),
                                         style = "margin-top: 25px;",
                                         width = '100px')),
                  column(2, actionButton(input = "download_pca", label = "Download All",
                                         icon = icon("download"),
                                         style = "margin-top: 25px;",
                                         width = '150px'))
                ),
                plotOutput("pca_plot")
            )
          )
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