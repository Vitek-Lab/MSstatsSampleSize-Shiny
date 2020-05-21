## MSstatsSampleSize Shiny UI
###############################################################
dashboardPage(
  skin="black",
  tags$head(
    tags$style(
      HTML(".shiny-notification {
              height: 100px;
              width: 800px;
              opacity: 1;
              position:fixed;
              top: calc(50% - 50px);;
              left: calc(55% - 400px);;
            }
           .shiny-output-error-validation {
           color: red;
           }"
      )
    )
  ),
  ##### Header #####
  header =  dashboardHeader(
    title = "MSstats - Sample Size Calculation",
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
      menuItem("1: Import Data", tabName = "import_data", icon = icon("file-import")),
      #### Data Simulation Tab ####
      menuItem("2: Simulate datasets", 
               tabName = "explore_simulated", icon = icon("project-diagram")),
      #### Analyze Simulation Tab ####
      menuItem("3: Plan Experiment", 
               tabName = "plan_experiment", icon = icon("vial"))
    )
  ),
  ##### Body ####
  body = dashboardBody(
    shinyjs::useShinyjs(),
    tabItems(
      #### Home Tab ####
      tabItem(
        tabName = "home",
        includeMarkdown("www/Welcome.md"),
        actionButton(inputId = "start_process",
                     label = "Start",
                     icon = icon("arrow-right"),
                     style = CSS_BUTTON,            
                     width = '200px')
      ),
      #### Import Data Tab ####
      tabItem(
        tabName = "import_data",
        navbarPage("", id = "myNavBar",
                   tabPanel("Import", fluidPage( 
                     box(title = "Data Import Wizard", width = 12,
                         solidHeader = T, status = 'primary',
                         includeHTML("www/data_import.Rmd"),
                         fluidRow(
                           column(width = 4, 
                                  selectInput("data_format", "Select Data Type",
                                              choice = FORMATS_LIST)),
                           column(width = 4, 
                                  fileInput("standard_count", "Select Protein Abundance File",
                                            multiple = FALSE,
                                            accept = EXTENSTIONS)),
                           column(width = 4,
                                  fileInput("standard_annot", "Select Sample Annotation File",
                                            multiple = FALSE,
                                            accept = EXTENSTIONS))
                         ),
                         fluidRow(
                           column(width = 2,
                                  actionButton("import_data", "Import dataset", 
                                               icon = icon("file-import")))
                         )
                     ),
                     box(title = "Example Data", width = 12, solidHeader = T,
                         status = 'primary',
                         includeHTML("www/test.Rmd")) 
                   )
                   ),
                   tabPanel("Explore Data", fluidPage(
                     fluidRow(
                       column(9,
                              h3(textOutput("dataset_name"), 
                                 style="display: inline-block;")),
                       column(3,
                              actionButton(inputId = "nav_to_sim",
                                    label = "Next Step",
                                    icon = icon("arrow-right"),
                                    style = CSS_BUTTON,
                                    width = '200px'))
                       ),
                     br(),
                     #### Summary Tables ####
                     fluidRow(
                       column(width = 6,
                              fluidRow(width = 12,
                                       # Data table output for summary table
                                       box(title = "Summary", solidHeader = T,
                                           status = "primary", width = 12,
                                           DT::dataTableOutput("sum_table"))),
                              br(),
                              # Data table output for condition summary
                              fluidRow(width = 12,
                                       box(title = "Condition Summary", solidHeader = T,
                                           status = "primary", width = 12,
                                           DT::dataTableOutput("cond_sum_table"))
                              )
                       ),
                       column(width = 6,# heatmap
                              box(title = "Mean-Variance Plot", solidHeader = T,
                                  status = "primary", width = 12,
                                  plotOutput("mean_sd_plot")
                              ))
                     ),
                     br(),
                     #### Box plots and HeatMap ####
                     fluidRow(
                       # Box plot 
                       box(title = "QC Box Plot", solidHeader = T, status = "primary",
                           width = 12,
                           plotly::plotlyOutput("global_boxplot")
                       )
                     )
                   )
                   )
        )
      ),
      #### Data Simulation Tab #####
      tabItem(
        tabName = "explore_simulated",
        fluidRow(
          column(9, h1("Simulate datasets",
                       style="display: inline-block;")),
          column(3,
                 shinyjs::disabled(actionButton(inputId = "nav_to_exp",
                              label = "Next Step",
                              icon = icon("arrow-right"),
                              style = CSS_BUTTON,
                              width = '200px')))
          ),
        ##### Checkboxes for file Input and set seed options ####
        fluidRow(
          # csv file uploads for parameters --- disabled not completely scoped out
          # column(3, shinyjs::disabled(checkboxInput(inputId = "upload_params",
          #                                           label = "Upload Simulation Parameters from csv"))),
          # set seed disabled not completely scoped out
          column(3, checkboxInput(input = "set_seed",
                                  label = "Set Seed value 1212"))
        ),
        #### File Input to upload a simulation parameter csv ####
        # fileInput(input = "param_input", label = "Upload Parameters in specified format",
        #           multiple = F, accept = c("text/csv",
        #                                    "text/comma-separated-values,text/plain",
        #                                    ".csv", "text/tab-separated-values", ".tsv")),
        fluidRow(
          #### Box to input simulation parameters from the dashboard ######
          box(id = "param_box", title = "Parameters For Simulating DataSets",
              width = 4, solidHeader = T, status = "primary",
              # Number of simulations
              numericInput(inputId = "n_sim", label = "Number of Simulations",
                           value = 10, min = 10, max = 500, step = 1) %>%
                shinyhelper::helper(type = "markdown", content = "n_sim"),
              # Use default fold change values?
              checkboxInput(inputId = "exp_fc", label = "Use Default Fold Change",
                            value = T)%>%
                shinyhelper::helper(type = "markdown", content = "exp_fc"),
              selectInput(inputId = "b_group", label = "Baseline Group",
                          choices = NULL)%>%
                shinyhelper::helper(type = "markdown", content = "b_group",
                                    id = "b_group_help"),
              # Render editable data.table for fold change values
              DT::DTOutput('fc_values') %>% 
                shinyhelper::helper(type = "markdown", content = "fc_values",
                                    id = "fc_values_help"),
              br(),
              # Input vector off different proteins
              textInput(inputId = "diff_prot", label = "List of Differential Abundant Proteins",
                        value = NULL,
                        placeholder = "List of comma separated Proteins") %>%
                shinyhelper::helper(type = "markdown", content = "diff_prot",
                                    id = "diff_prot_help"),
              # Select number/proportion the proteins be simulated with
              selectInput(inputId = "sel_sim_prot", label = "Select Proteins To Simulate",
                          choices = c("Proportion", "Number"), #capitalize this
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
                        value = "5,10,20", placeholder = "5,10,20") %>%
                shinyhelper::helper(type = "markdown", content = "n_samp_grp"),
              # should validation set be simulated as well?
              selectInput(inputId = "sim_val", label = "Simulate Validation Set",
                          choices = c(T,F), selected = F) %>%
                shinyhelper::helper(type = "markdown", content = "sim_val"),
              # Define number of samples in the validation groups
              numericInput(inputId = "n_val_samp_grp", label = "Valid samples per group",
                           value = 50, min = 50, max = 1000, step = 50) %>%
                shinyhelper::helper(type = "markdown", content = "n_val_samp_grp",
                                    id = "n_val_samp_grp_help"),
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
                column(2, shinyjs::disabled(actionButton(inputId = "back",
                                                         label = "Previous",
                                                         icon = icon("arrow-left"),
                                                         style = "margin-top: 25px;",
                                                         width = '100px'))),
                column(2, shinyjs::disabled(actionButton(inputId = "fwd",
                                                         label = "Next",
                                                         icon = icon("arrow-right"),
                                                         style = "margin-top: 25px;",
                                                         width = '100px'))),
                column(2, shinyjs::disabled(downloadButton(outputId = "download_pca",
                                                           label = "Download",
                                                           icon = icon("download"),
                                                           style = "margin-top: 25px;",
                                                           width = '150px')))

              ),
              plotOutput("pca_plot")
          )
        )
      ),
      #### Analyse Simulation Tab ####
      tabItem(
        tabName = "plan_experiment",
        h1("Plan Experiment"),
        h3("Calculate Sample Size For Classification"),
        #### Model Setup Box ####
        fluidRow(
          box(status = 'primary', solidHeader = T, title = "Model Setup", 
              width = 12,
              fluidRow(
                column(width = 2,
                       selectInput(inputId = "classifier", label = "Select Model to Train",
                                   choice = MODELS, width = '200px')
                ),
                column(width = 3,
                       checkboxGroupButtons(inputId = "checkbox_inputs",
                                            label = "Options",
                                            choices = c("Use h2o Package",
                                                        "Parameter Tuning"),
                                            checkIcon = list(
                                              yes = icon("check-square"),
                                              no = icon("square-o")),
                                            width = "300px")%>%
                         shinyhelper::helper(type = "markdown", 
                                             content = "checkbox_inputs",
                                             id = "checkbox_inputs_help")
                ),
                column(width = 1,
                       actionButton(inputId = "run_model", label = "Run Model",
                                    width = "100px", style = "margin-top:25px;"),
                ),
                column(width = 2,
                       downloadButton(outputId = "download_models",
                                      label = "Download Models",
                                      style = "margin-top:25px;",
                                      width = "250px")
                ),
                column(width = 1,
                       downloadButton(outputId = "download_plots",
                                      label = "Download Plots" ,
                                      style = "margin-top:25px;",
                                      width = "200px")
                )
              ),
              column(width = 3, 
                     selectInput(inputId = "stop_metric", 
                                 label = "Stopping Metric",
                                 choices = STOPPING_METRIC) %>%
                       shinyhelper::helper(content = "h2o_stopping_metric",
                                           id = "stop_metric_help"),
                     
                     numericInput(inputId = "nfolds", label = "N-Folds",
                                  value = 0, min = 0, max = 100, step = 1)%>%
                       shinyhelper::helper(content  = "h2o_nfold",
                                           id = "nfolds_help"),
                     
                     selectInput(inputId = "f_assignment", label = "Fold Assignment",
                                 choices = FOLD_ASSIGNMENT) %>%
                       shinyhelper::helper(content = "h2o_f_assignment",
                                           id = "f_assignment_help"),
                     
                     numericInput(inputId = "iters", label = "Iterations", value = 200,
                                  min = 1, max = 1000, step = 10) %>%
                       shinyhelper::helper(content = "h2o_iters",
                                           id = "iters_help"),
                     
                     selectInput(inputId = "family", label = "Family", choices = FAMILY,
                                 selected = "binomial") %>%
                       shinyhelper::helper(content = "h2o_family",
                                           id = "family_help"),
                     
                     sliderInput(inputId = "laplace", label = "Laplace", min = 0,
                                 max = 1, value = 0) %>%
                       shinyhelper::helper(content = "h2o_laplace",
                                           id = "laplace_help"),
                     
                     numericInput(inputId = "caret_rf", 
                                  label = "Variables to Randomly Sample", 
                                  value = 2, min = 1, max = 1000, step = 1)%>%
                       shinyhelper::helper(content = "caret_rf",
                                           id = "caret_rf_help")
                     ),
              column(width = 3,
                     selectInput(inputId = "link", label = "Link", 
                                 choices = LINK) %>%
                       shinyhelper::helper(content = "h2o_link",
                                           id = "link_help"),
                     
                     selectInput(inputId = "solver", label = "Solver",
                                 choices = SOLVER) %>%
                       shinyhelper::helper(content = "h2o_solver",
                                           id = "solver_help"),
                     
                     sliderInput(inputId = "eps_sdev", label = "Cutoff Threshold",
                                 min = 0, max = 1, step = 0.001, value = 0.01) %>%
                       shinyhelper::helper(content = "h2o_eps_sdev",
                                           id = "eps_sdev_help"),
                     
                     sliderInput(inputId = "min_sdev", label = "Minimum SD",
                                 min = 0.01, value = 0.01, max = 1) %>%
                       shinyhelper::helper(content = "h2o_min_sdev",
                                           id = "min_sdev_help"))
          ),
          verbatimTextOutput("v3")
        ),
        fluidRow(
          ##### Accuracy Box ####
          box(width = 6, title = "Accuracy", status = "info", solidHeader = T,
              plotOutput(outputId = 'acc_plot'),
              verbatimTextOutput("default", placeholder = F)
          ),
          ##### Protein Importance Box ####
          box(width = 6, title = "Protein Importance", status = "success",
              solidHeader = T,
              fluidRow(
                column(width = 4,
                       selectInput(inputId = "s_size", label = "Sample Size",
                                   choices = NULL)
                ),
                shinyjs::disabled(actionButton(inputId = "back_varimp", label = "Prev.",
                                               icon = icon("arrow-left"), 
                                               style = "margin-top: 25px;")),
                shinyjs::disabled(actionButton(inputId = "fwd_varimp", label = "Next",
                                               icon = icon("arrow-right"), 
                                               style = "margin-top: 25px;"))
              ),
              plotOutput('importance_plot')
          )
        )
      )
    )
  )
)