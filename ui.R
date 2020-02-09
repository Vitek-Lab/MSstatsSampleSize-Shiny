## MSstatsSampleSize Shiny UI
###############################################################
dashboardPage(
  skin="black",
  ##### Header #####
  header =  dashboardHeader(
    title = "MSstatsSampleSize",
    titleWidth = 300
  ),
  #### SideBar ####
  sidebar =  dashboardSidebar(
    shinyjs::useShinyjs(),
    width = 250,
    sidebarMenu(
      id="tabs",
      ##### Home Tab ####
      menuItem("Home", tabName = "home", icon = icon("home")),
      #### Data Import Tab ####
      menuItem("Import Data", icon = icon("file-import"),
               ##### Select Input for Type of Data ####
               menuSubItem(selectInput("data_format", "Select Data Type",
                                       choice = list("Protein-level quantification" = "standard", 
                                                     "Example from MSstatsSampleSize" = "examples")
                                       ),
                           tabName = "import_data"),
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
        tabName = "import_data",
        includeHTML("www/test.rmd")
      ),
      ##### Exploration Visuals #####
      tabItem(
        tabName = "explore_data",
        h1(textOutput("dataset_name")),
        br(),
        #### Summary Tables ####
        fluidRow(
          # Data table output for summary table
          box(title = "Summary", width = 6, solidHeader = T, status = "primary",
              DT::dataTableOutput("sum_table")),
          # Data table output for condition summary
          box(title = "Condition Summary", width = 6, solidHeader = T,
              status = "primary", 
              DT::dataTableOutput("cond_sum_table"))
        ),
        br(),
        #### Box plots and HeatMap ####
        fluidRow(
          # Box plot 
          box(title = "QC Box Plot", solidHeader = T, status = "primary",
              width = 7,
              plotly::plotlyOutput("global_boxplot")
          ),
          # heatmap
          box(title = "Mean-Variance Plot", solidHeader = T, status = "primary",
              width = 5,
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
          # csv file uploads for parameters --- disabled not completely scoped out
          column(3, shinyjs::disabled(checkboxInput(inputId = "upload_params",
                                  label = "Upload Simulation Parameters from csv"))),
          # set seed disabled not completely scoped out
          column(3, shinyjs::disabled(checkboxInput(input = "set_seed",
                                  label = "Set Seed value 1212")))
        ),
        #### File Input to upload a simulation parameter csv ####
        fileInput(input = "param_input", label = "Upload Parameters in specified format",
                  multiple = F, accept = c("text/csv",
                                           "text/comma-separated-values,text/plain",
                                           ".csv", "text/tab-separated-values", ".tsv")),
        fluidRow(
          #### Box to input simulation parameters from the dashboard ######
          box(id = "param_box", title = "Parameters For Simulating DataSets",
              width = 4, solidHeader = T, status = "primary",
              # Number of simulations
              numericInput(inputId = "n_sim", label = "Number of Simulations",
                           value = 10, min = 10, max = 500, step = 1) %>%
                shinyhelper::helper(type = "markdown", content = "n_sim"),
              # Use default fold change values?
              checkboxInput(inputId = "exp_fc", label = "Use Default Fold Change?",
                            value = T),
              selectInput(inputId = "b_group", label = "Baseline Group",
                          choices = NULL)%>%
                shinyhelper::helper(type = "markdown", content = "b_group"),
              # Render editable data.table for fold change values
              DT::DTOutput('fc_values'),
              br(),
              # Input vector off different proteins
              textInput(inputId = "diff_prot", label = "List Diff Protein",
                        value = NULL,
                        placeholder = "List of comma separated Proteins") %>%
                shinyhelper::helper(type = "markdown", content = "diff_prot"),
              # Select number/proportion the proteins be simulated with
              selectInput(inputId = "sel_sim_prot", label = "Select Simulated Proteins",
                          choices = c("proportion", "number"),
                          selected = "proportion") %>%
                shinyhelper::helper(type = "markdown", content = "sel_sim_prot"),
              # define the protein proportion
              sliderInput(inputId = "prot_prop", label = "Protein Proportion",
                           value = 1, min = 0, max = 1, step = 0.01) %>%
                shinyhelper::helper(type = "markdown", content = "prot_prop"),
              # define number of proteins to be selected
              sliderInput(inputId = "prot_num", label = "Protein Number",
                           value = 1, max = 1000, min = 1) %>%
                shinyhelper::helper(type = "markdown", content = "prot_num"),
              # number of sample to be simulated
              textInput(inputId = "n_samp_grp", label = "Samples per group",
                        value = NULL, placeholder = "50,60,70") %>%
                shinyhelper::helper(type = "markdown", content = "n_samp_grp"),
              # should validation set be simulated as well?
              selectInput(inputId = "sim_val", label = "Simulate Validation Set",
                          choices = c(T,F), selected = F) %>%
                shinyhelper::helper(type = "markdown", content = "sim_val"),
              # Define number of samples in the validation groups
              numericInput(inputId = "n_val_samp_grp", label = "Valid samples per group",
                           value = 50, min = 50, max = 1000, step = 50) %>%
                shinyhelper::helper(type = "markdown", content = "n_val_samp_grp"),
              # Simulate the dataset provided inputs, gets enabled dynamically
              shinyjs::disabled(
                actionButton(inputId = "simulate", label = "Simulate Data",
                             icon =  icon("project-diagram")))
          ),
          #### Simulation details section #####
          box(title = "Simulated Datasets", solidHeader = T, status = "primary",
              width = 8,
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
                column(2, downloadButton(outputId = "download_pca", label = "Download",
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
        fluidRow(
          column(width = 3, 
                 checkboxInput(inputId = "use_h2o", label = "Use H2O Package")
          ),
          column(width = 2, offset = 4,
                 selectInput(inputId = "s_size", label = "Sample Size",
                             choices = NULL)
          ),
          actionButton(inputId = "back_varimp", label = "Prev.",
                       icon = icon("arrow-left"), style = "margin-top: 25px;"),
          actionButton(inputId = "fwd_varimp", label = "Next",
                       icon = icon("arrow-right"), style = "margin-top: 25px;"),
          downloadButton(outputId = "download_prot_imp",
                         label = "Download PDF", 
                         style = "margin-top: 25px;", width = '75px')
        ),
        fluidRow(
          ##### Model Setup Box #####
          box(id = "model_config", width = 3, status = "primary",
              solidHeader = T, title = "Model Setup",
              selectInput(inputId = "classifier", label = "Select Model to Train",
                          choice = MODELS, width = '200px'),
              selectInput(inputId = "stop_metric", 
                          label = "Stopping Metric",
                          choices = STOPPING_METRIC),
              numericInput(inputId = "nfolds", label = "N-Folds",
                           value = 2, min = 0, max = 100, step = 1),
              selectInput(inputId = "f_assignment", label = "Fold Assignment",
                          choices = FOLD_ASSIGNMENT),
              numericInput(inputId = "iters", label = "Iterations", value = 200,
                           min = 1, max = 1000, step = 10),
              selectInput(inputId = "family", label = "Family", choices = FAMILY),
              selectInput(inputId = "link", label = "Link", choices = LINK),
              selectInput(inputId = "solver", label = "Solver", choices = SOLVER),
              #selectInput(inputId = "dist", label = "Distribution", choices = DISTRIBUTION),
              sliderInput(inputId = "laplace", label = "Laplace", min = 0,
                          max = 1, value = 0),
              sliderInput(inputId = "eps_sdev", label = "Cutoff Threshold",
                          min = 0, max = 1, step = 0.001, value = 0.01),
              sliderInput(inputId = "min_sdev", label = "Minimum SD",
                          min = 0.01, value = 0.01, max = 1),
              actionButton(inputId = "run_model", label = "Train Model",
                           width = '100px'),
              downloadButton(outputId = "download_models", label = "Download Models")
          ),
          ##### Accuracy Box ####
          box(width = 4, title = "Accuracy", status = "info", solidHeader = T,
              plotOutput(outputId = 'acc_plot')
          ),
          ##### Protein Importance Box ####
          box(width = 5, title = "Protein Importance", status = "success",
              solidHeader = T,
              plotOutput('importance_plot')
          )
        )
      )
    )
  )
)