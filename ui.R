###############################################################
## MSstatsSampleSize Shiny app
###############################################################


###############################################################
## header

header <- dashboardHeader(
    title = "MSstatsSampleSize - Shiny",
    titleWidth = 300
    #tags$li(class = "dropdown",
    #        tags$li(class = "dropdown", actionLink("debug_save", label="Save Environment"))
    #)
)


###############################################################
## sidebar

side <- dashboardSidebar(
    width = 300,
    sidebarMenu(id="tabs",
                menuItem("Home", tabName="home", icon = icon("home")),
                menuItem("Import Data",
                         tabName="import_data", icon = icon("file-import")
                ),
                ## Use of conditionalPanel in place of subMenuItem, ref. https://github.com/rstudio/shinydashboard/issues/86
                conditionalPanel("input.tabs === 'import_data'", 
                         #startExpanded=TRUE,
                         div(class="custom-sidebar-output",
                             selectInput("data_format", "Select data type", 
                                         choices=list("Protein-level quantification"="standard", 
                                                      "Example from MSstatsSampleSize"="examples"))),
                         htmlOutput("select_files", class="custom-sidebar-output",)
                ),
                menuItem("Simulate datasets", 
                        tabName="explore_simulated", icon=icon("project-diagram")),
                menuItem("Analyze the simulated datasets", 
                        tabName="analyse_simulated", icon=icon("vial"))
   )
)


###############################################################
## body

body <- dashboardBody(
    useShinyjs(),
    tags$head(
        tags$style(HTML("
            .box {
                overflow-y: scroll;
            }
            
            /* Styling for elements nested inside menuItems */
            .treeview-menu .shiny-input-container {
                margin-bottom:0px !important;
                padding-top:0px !important;
                padding-bottom:0px !important;
            }
            
            .custom-sidebar-output>.shiny-input-container .progress{
                margin-bottom:0px !important;
            }
            
            .custom-sidebar-output>b {
                padding-left:15px;
            }
            
            .custom-sidebar-output {
                margin: 10px 0 10px 0;
            }
            
            .treeview-menu {
                padding:5px 0px 5px 5px !important;
            }

            #main_output {
                font-family: Monaco, Consolas, 'Andale Mono', 'DejaVu Sans Mono', monospace;
                font-size: 80%;
                margin-top: 5px;
                white-space: pre;
                white-space: pre-wrap;
                white-space: -moz-pre-wrap;
                white-space: -o-pre-wrap;
                background: #222D32;
                color: #FFFFFF;
                border-radius: 2px;
            }
      
            .content-wrapper, .right-side {
                background-color: #F7F7F7;
            }
      
            h1 {
                margin-top: 0px;
            }
      
            * {
                word-break: break-word;
            }
            
            .custom-box-title {
                display: inline-block;
                font-size: 18px;
                margin: 0;
                line-height: 1;
            }
            ")
        )
    ),
    
    tabItems(
        Tab1 <- tabItem(
            tabName="home",
            includeMarkdown("www/Welcome.md")
        ),

        Tab2 <- tabItem(
            tabName="import_data",
            htmlOutput("import_data_content")
        ),

        Tab3 <- tabItem(
            tabName="explore_simulated",
            h1("Simulate datasets"),
            htmlOutput("explore_simulated_content")
        ),
    
        Tab4 <- tabItem(
            tabName = "analyse_simulated",
            h1("Analyze the simulated datasets"),
            htmlOutput("analyse_simulated_content")
        )
    )
)


###############################################################
## Finally, boad

dashboardPage(
    skin="black",
    header,
    side,
    body
)