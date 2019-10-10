
function(session, input, output) {
  
    ## Set maximum size of uploaded files to 300mb
    options(shiny.maxRequestSize = 300*1024^2)
  
    ## Create reactiveValue object
    values <- reactiveValues()
    
    ## Reactive expression to check status 
    output$sim <- reactive({!is.null(values$result)})
    outputOptions(output, "sim", suspendWhenHidden = FALSE)
  
    # Debug functions
    # output$res<-renderText({
    #   paste("You've selected:", input$tabs)
    # })
    
    observeEvent(input$debug_save, {
        try ({
            message("Starting to output environment")
            
            # T1+2 Data
            debug_quant_data <<- values$quant_data
            debug_prot_abundance <<- values$prot_abundance
            debug_sample_annotation <<- values$sample_annotation
            debug_pca <<- values$pca
            debug_parameters <<- values$parameters
            debug_s_summary <<- values$s_summary
            
            # T3+4 Data
            debug_simulated_raw <<- values$simulated_raw
            debug_valid_y<<-values$valid_y
            debug_valid_x<<-values$valid_x
            debug_chosen_dataset<<-values$chosen_dataset
            debug_result <<- values$result
            debug_meandata<<- meandata
            debug_chosen_model <<- values$chosen_model
            debug_chosen_model.pred <<- values$chosen_model.pred
            debug_confusionMatrix <<- values$confusionMatrix
            debug_chosen_model.metrics <<- values$chosen_model.metrics
            debug_gg_meanPA <<- values$gg_meanPA
            message("Environment output complete")
            showNotification("Environment Saved", duration=10, closeButton = TRUE, type="message")
            
        })
    })
  
    ################################################
    ################################################
    ## Tab 1 Sidebar Data Import
    ################################################
    ################################################
    
    ################################################
    ## L1 Options Menu
    ################################################
    
    ## toDo : do we need this option? otherwise, remove this option menu
    #output$options <- renderUI({
    #    if (is.null(input$data_format)) {
    #        tags$b("Please select a data format")
    #    } else {
    #        switch(input$data_format,
    #               "standard" = tagList(
    #                 selectInput("separator", "Separator", choices = list("Commas"=",", "Tabs"="\t"))),
    #               "examples" = tagList(
    #                 tags$b("placeholder")),
    #               tags$b("No available options for selected data format.")
    #              )
    #    }
    #})  
    
    #outputOptions(output, "options", suspendWhenHidden = FALSE)
    
    ################################################
    ## L1 Dataset Upload and Import Menu | Contingent on Options & Advanced Options loading
    ################################################
    
    output$select_files <- renderUI({
      
        if (is.null(input$data_format)) {
          
            tags$b("Please import dataset.")
          
        } else {
          
          switch(input$data_format,
                 "standard" = tagList(
                     fileInput("standard_count", "Select protein abundance file",
                               multiple = FALSE,
                               accept = c("text/csv",
                                          "text/comma-separated-values,text/plain",
                                          ".csv", "text/tab-separated-values", ".tsv")),
                     fileInput("standard_annot", "Select sample annotation file",
                               multiple = FALSE,
                               accept = c("text/csv",
                                          "text/comma-separated-values,text/plain",
                                          ".csv", "text/tab-separated-values", ".tsv")),
                     actionButton("import_data", "Import dataset", 
                                  icon=icon("file-import"))
                   ),
                 "examples" = tagList(
                   ## Use the examples from MSstatsSampleSize
                   ## OV_SRM_train
                   ## OV_SRM_train_annotation
                   actionButton("import_data", "Import the example dataset", 
                                icon=icon("file-import"))
                 ),
                 tags$b("No import method for selected data format.")
          )
        }
    })
    
    outputOptions(output, "select_files", suspendWhenHidden = FALSE)
  
    
    ################################################
    ## Actual Data import function
    ################################################
    
    observeEvent(input$import_data, {
        if (is.null(input$data_format)) {
          
            showNotification("No imported dataset found", duration=10,
                             closeButton = TRUE, type="message")
          
        } else {
            switch(input$data_format,
                   "standard" = {
                     tryCatch(
                       { ## Load data from user-input filepath
                          values$wide_data <- read.csv(input$standard_count$datapath, check.names = FALSE, row.names = 1) # check.names set to false because R gets upset with column names starting with a number; row.names set to 1 to match matrix specification
                          values$annot_data <- read.csv(input$standard_annot$datapath, check.names = FALSE)
                          values$dataset_name <- input$standard_count$name
                          wide_data <<- values$wide_data
                          annot_data <<- values$annot_data
                          values$is_imported <- TRUE
                          showNotification("Data import complete", duration=10, 
                                           closeButton = TRUE, type="message")
                        },
                        error = function(e) {
                            showNotification("Error", duration=10, 
                                             closeButton = TRUE, type="message")
                        }
                     )
                   },
                   "examples" = { 
                     tryCatch(
                       { ## use example file from MSstatsSampleSize
                         ## OV_SRM_train
                         ## OV_SRM_train_annotation
                          values$wide_data <- as.data.frame(OV_SRM_train)               ## toDo : need to check
                          values$annot_data <- as.data.frame(OV_SRM_train_annotation)     ## toDo : need to check
                          values$dataset_name <- 'Ovarian cancer SRM study'
                          wide_data <<- values$wide_data
                          annot_data <<- values$annot_data
                          values$is_imported <- TRUE
                          showNotification("Data import complete", duration=10, 
                                          closeButton = TRUE, type="message")
                       },
                       error = function(e) {
                          showNotification("Error", duration=10, 
                                           closeButton = TRUE, type="message")
                       }
                    )
                }
            )
          # Recast wide_data into long format
          data <- values$wide_data
          annot <- values$annot_data
          
          ## change to long-format
          data <- cbind(Protein=rownames(data), data)
          data <- gather(data, "BioReplicate", "Abundance", 2:ncol(data))
          
          ## need to merge with annotation
          data <- left_join(data, annot, by='BioReplicate')
          
          values$long_data <- data
          long_data <<- values$long_data
        }
    })
    
    
    ################################################
    ## Explore Dataset Tab
    ################################################
    output$import_data_content <- renderUI({
        if (is.null(values$is_imported)) {
            tagList(
                h1("Summarize dataset"),
                tags$b("Please use the 'Import data' menu to import a dataset.")
            )
        } else {
            n_prot <- length(unique(rownames(values$wide_data)))
            n_group <- length(unique(values$annot_data$Condition))
            #n_biorep <- nrow(unique(values$quant_data[["RunlevelData"]]["SUBJECT"]))
            tagList(
                h1(paste0("Summary of dataset: ", values$dataset_name)),
                fluidRow(
                    ## Preliminary check : counts of proteins, groups, ...
                    valueBox("Proteins Quantified", value = n_prot, icon=icon("dna"), color="purple", width=6),
                    valueBox("Groups", value = n_group, icon=icon("layer-group"), color="green", width=6)
                    ## !! toDo : add small table for # bioReplicates per group
                ),
                fluidRow(
                    box(
                      width = 12,
                      div(style = 'overflow-x:scroll;',
                          DT::dataTableOutput("summary_table")
                      )
                    )
                ),
                fluidRow(
                    box(
                      width = 6,
                      div(style = 'overflow-x:scroll;',
                          plotlyOutput("global_boxplot")
                      )
                    ),
                    box(
                      width = 6,
                      div(style = 'overflow-x:scroll;',
                          plotOutput("mean_sd_plot")
                      )
                    )
                )
            )
        }
    })
  
    ################################################
    # L1 Explore Dataset / Data Tables
    ################################################
    
    #output$prot_abundance_table <- DT::renderDataTable({
    #    values$prot_abundance
    #})
    
    #output$sample_annotation_table <- DT::renderDataTable({
    #    values$sample_annotation
    #})
    
    output$summary_table <- DT::renderDataTable({
        data <- values$long_data
        summary.s <- matrix(NA,ncol=nlevels(data$Condition), nrow=3)
        ## # of MS runs
        msruns <- unique(data[, c("BioReplicate", "Condition")]) # Placeholder as the example dataset lacks technical replicates
        msruns <- xtabs(~Condition, data=msruns)
        summary.s[1,] <- msruns
        ## # of biological replicates
        biorep <- unique(data[, c("BioReplicate", "Condition")])
        biorep <- xtabs(~Condition, data=biorep)
        summary.s[2,] <- biorep
        ## # of technical replicates
        c.tech <- round(summary.s[1,] / summary.s[2,])
        summary.s[3,] <- c.tech
        colnames(summary.s) <- unique(data$Condition)
        rownames(summary.s) <- c("# of MS runs","# of Biological Replicates", "# of Technical Replicates")
        summary.s_debug <<- summary.s
        
        summary.s
    })
  
    ################################################
    # L1 Explore Dataset / Plots
    ################################################
    
    ## Global QC Box Plot
    output$global_boxplot <- renderPlotly({
      
        data <- values$long_data
        
        ## to get the upper limit of y-axis in box plot
        ylimup <- max(data$Abundance, rm.na=TRUE)
        
        plot_ly(data, y=~Abundance, x=~BioReplicate, color=~Condition, type = "box") %>%
          layout(xaxis=list(title="Biological Replicate"), 
                 yaxis=list(range = c(0, ylimup+2), title="Protein abundance")) ## not sure about log scale
    })
  
    ## Mean-variance Plot
    output$mean_sd_plot <- renderPlot({
      
        variance_estimation <- estimateVar(values$wide_data, 
                                           values$annot_data)
        ## In order not to save it as pdf
        meanSDplot(variance_estimation, address=FALSE)
      
    })
  
    
    ################################################
    ################################################
    ## Tab 2 Simulate datasets Tab : simulateDataset()
    ################################################
    ################################################
    
    output$explore_simulated_content<-renderUI({
        if (is.null(values$is_imported)) {
            tags$b("Please use the 'Import Data' menu to import a proteome dataset.")
        } else {
            tagList(
                fluidRow(
                  box(title = paste("Selected Dataset:",values$selected_dataset_name), 
                      width=9,
                      htmlOutput("inspect_simulated")
                  ),
                  box(title = "Simulated Datasets",
                      width = 3, 
                      plotlyOutput("simulated_grid"),
                      br(),
                      h3("Parameters", class="custom-box-title"),
                      numericInput("n_sample", label="Number of Different Sample Sizes to Simulate", value=5),
                      numericInput("sample_incr", label="Step Size Between Simulated Sample Sizes", value=20),
                      numericInput("n_protein", label="Number of Different Protein Counts to Simulate", value=5)
                  )
               )
            )
        }
    })
    
    outputOptions(output, "explore_simulated_content", suspendWhenHidden = FALSE)
    
    ################################################
    ## L1 Explore Simulated Data / Process and generate tabset for specified simulated dataset
    ################################################
    output$inspect_simulated<-renderUI({
      values$selected_dataset <- event_data("plotly_click", source = "simulated_grid_plot")
      if (length(values$selected_dataset)) {
        protein_desc <- 1/input$n_protein
        
        mu <- values$parameters$mu
        sigma <- values$parameters$sigma
        promean <- values$parameters$promean
  
        # Use mean and variance components to generate dataset on the fly
        ## No. of proteins is vars[1], No. of samples is vars[2]
        vars <- c(values$selected_dataset[["x"]], values$selected_dataset[["y"]])
        values$selected_dataset_name<-paste(vars[1]," Proteins, ",vars[2]," BioReplicates",sep="")
        
        selectedProt<-order(promean, decreasing=TRUE)[1:vars[1]]
        mu_selected <- mu[selectedProt,]
        sigma_selected <- sigma[selectedProt,]
        simulated_data <- .sampleSimulation(as.integer(vars[2]), mu_selected, sigma_selected)
  
        # values$s_prot_abundance <- t(simulated_data[["X"]])
        # values$s_sample_annotation <- simulated_data[["Y"]]
        # summary.s <- matrix(NA,ncol=nlevels(values$s_sample_annotation), nrow=1)
        # values$summary <- summary.s
        # summary <<- values$summary
        
        
        
        # merged<-melt(simulated_data[["X"]], value.name = "LogIntensities", varnames = c('originalRUN', 'Protein'))
        # merged$LogIntensities <- suppressWarnings(as.numeric(paste(merged$LogIntensities)))
        # values$s_groups<-data.frame("originalRUN"=1:length(simulated_data[["Y"]]), "Group"=simulated_data[["Y"]])
        # merged<-merge(merged, values$s_groups, by="originalRUN")
        # values$s_quant_data<-merged
        
        ## # of biological replicates
        # temp <- unique(merged[, c("Group", "originalRUN")])
        # temp1 <- xtabs(~Group, data=temp)
        # summary.s[1,] <- temp1
        # 
        # colnames(summary.s) <- unique(values$quant_data[["RunlevelData"]]$GROUP_ORIGINAL)
        # rownames(summary.s) <- c("# of Biological Replicates")
        # values$s_summary <- summary.s
        showNotification("Data processing complete", duration=10, closeButton = TRUE, type="message")
        
        tprot <- t(values$s_prot_abundance)
        class(tprot) <- "numeric"
        
        remove_cols<-nearZeroVar(tprot, names=TRUE, freqCut=19, uniqueCut=10)
        keep_cols<-colnames(tprot)
        tprot<-tprot[,setdiff(keep_cols,remove_cols)]
        
        tprot_pca1 <- prcomp(tprot, center = TRUE,scale. = TRUE)
        values$s_pca<-tprot_pca1
        showNotification("PCA complete")
        
        tabBox(id="simulated_data_wrapper", width="100%",
               tabPanel("Raw Data",
                        div(style = 'overflow-x:scroll;',
                            DT::dataTableOutput("s_sample_annotation_table"),
                            DT::dataTableOutput("s_prot_abundance_table")
                        )
               ),
               tabPanel("Summary",
                        div(style = 'overflow-x:scroll;',
                            DT::dataTableOutput("s_summary_table")
                        )
               ),
               tabPanel("PCA",
                        div(style = 'overflow-x:scroll;',
                            plotOutput("s_pcabiplot"),
                            plotOutput("s_pcascreeplot")
                        )
               ),
               tabPanel("QC Box Plots",
                        div(style = 'overflow-x:scroll;',
                            plotlyOutput("s_global_boxplot")
                        )
               )
        )
      } else {
        values$selected_dataset_name<-"No dataset selected"
        div("View and explore a simulated dataset by clicking on the tile on the heatmap that corresponds to the requested number of proteins and biological replicates. If required, vary the range of datasets to be simulated by modifying the parameters.")
      }
      
    })
  
    ################################################
    ## L2 Explore simulated data / Specified dataset / Data tables
    ################################################
    #output$s_prot_abundance_table = DT::renderDataTable({
    #    values$s_prot_abundance
    #})
    
    #output$s_sample_annotation_table = DT::renderDataTable({
    #    values$s_groups
    #})
    
    output$s_summary_table = DT::renderDataTable({
        values$s_summary
    })
  
    ################################################
    ## L2 Explore simulated data / Specified dataset / Plot: simulated_grid
    ################################################
    output$simulated_grid <- renderPlotly({
        protein_desc <- 1/input$n_protein
        nproteins <- length(unique(values$quant_data$ProcessedData$PROTEIN))
        m_prot <- floor(nproteins*protein_desc)
        ngroup <- length(unique(values$quant_data$ProcessedData$GROUP_ORIGINAL))
        sample_incr <- input$sample_incr
        n_sample <- input$n_sample
        train_size <- seq.int(from = sample_incr, 
                              to = sample_incr * n_sample, 
                              length.out = n_sample)
        train_size <- train_size * ngroup
        protein_num <- seq.int(from = m_prot, to = nproteins, by = m_prot)
        
        rownames <- as.character(train_size)
        colnames <- as.character(protein_num)
        
        simulated_grid <- matrix(NA, 
                                 nrow=length(rownames), 
                                 ncol=length(colnames), 
                                 dimnames = list(rownames,colnames))
        for (m in rownames(simulated_grid)) {
            for (n in colnames(simulated_grid)) {
                simulated_grid[m,n] <- as.numeric(m)*as.numeric(n) 
            }
        }
        
        plot_ly(
            x=colnames(simulated_grid),
            y=rownames(simulated_grid),
            z=simulated_grid,
            type="heatmap",
            source="simulated_grid_plot"
        ) %>%
            layout(xaxis=list(title="Number of Proteins", type="category"), 
                   yaxis=list(title="Number of Biological Replicates", type="category"))
    })
  
    ################################################
    ################################################
    # Tab 3 Analyze the simulated datasets: designSampleSizeClassification()
    ################################################
    ################################################
 
    ## Main Content
    output$analyse_simulated_content<-renderUI({
        if (is.null(values$is_imported)) {
          
            tags$b("Please use the 'Import data' menu to import a proteome dataset.")
          
        } else {
            tagList(
                fluidRow(
                    box(title = values$simulated_classification_title,
                        width = 9, 
                        htmlOutput("simulated_classification_heatmap")
                        ),
                    box(title="Train Models", 
                        width = 3,
                        radioButtons("train_type", label="Select type of analysis", 
                                     choices=list("Between datasets (Single classifier)"="between_datasets", 
                                                  "Between classifiers (Single dataset)"="between_classifiers")
                        ),
                        htmlOutput("train_options"),
                        numericInput("iter", 
                                     label="Iteration Count", value=10),
                        actionButton("start", 
                                     label="Begin Simulation")
                    )
                ),
                fluidRow(
                    htmlOutput("inspect_model")
                )
            )
        }
    })
  
    ################################################
    ## L2 Experiment Simulation / Training Options
    ################################################
    
    output$train_options <- renderUI({
        if (!is.null(input$train_type)) {
            switch(input$train_type,
                   "between_datasets" = {
                   # numericInput("n_sample", "Number of proteins", value=input$s_n_sample),
                   # 
                   # n_sample = input$n_sample,
                   # sample_incr = input$sample_incr,
                   # protein_desc = 1/input$protein_desc,
                      selectInput("classifier", 
                                  "Select Classifier", 
                                  choices=list("Random Forest" = "rf", 
                                               "SVM" = "svmLinear", 
                                               "Naive Bayes" = "naive_bayes", 
                                               "Partial Least Squares" = "pls", 
                                               "Neural Net (3 layers)" = "nnet"))
                   },
                   "between_classifiers" = {
                        values$chosen_dataset <- event_data("plotly_click", source = "model_plot")
                        
                        if (length(values$chosen_dataset)) {
                            values$between_classifiers_prot <- values$chosen_dataset[["x"]]
                            values$between_classifiers_samples <- values$chosen_dataset[["y"]]
                        }
                        
                        tagList(
                            tags$b("Choose a dataset by clicking on its tile on the heatmap"),
                            actionButton("between_classifiers_declare", "Select Dataset"),
                            tags$p(paste("Selected dataset:",
                                         values$between_classifiers_prot,
                                         " Proteins, ",
                                         values$between_classifiers_samples,
                                         " BioReplicates"))
                        )
                   }
            )
        }
    })

    ################################################
    ## L2 Experiment Simulation / Heatmap
    ################################################
    
    output$simulated_classification_heatmap<-renderUI({
        if (is.null(values$simulated_classification_status)) {
            values$simulated_classification_title = "Waiting for trained models..."
            tagList(
              div('Train classifiers on generated fake datasets by clicking "Begin Simulation" on the right. Optionally, change the classifier to be used, or choose to analyse cross-classifier performance for a single dataset on the right.'),
              textOutput("main_output")
            )
        } else {
            values$simulated_classification_title = "Trained Models"
            tabBox(id="simulated_classification_wrapper", width="100%",
                   tabPanel("Heatmap",
                            plotlyOutput("model_heatmap")),
                   tabPanel("Line Graphs",
                            htmlOutput("lineplot_xvar_selector"),
                            htmlOutput("lineplot_trendline_selector"),
                            plotOutput("lineplot") )
            )
        }
    })

    ################################################
    # L2 Experiment Simulation / Lineplot Type Selectors
    ################################################
    
    output$lineplot_xvar_selector <- renderUI({
        selectInput("lineplot_xvar", "X variable:", 
                    choices = c("Protein number", "Sample size"))
    })
    
    output$lineplot_trendline_selector <- renderUI({
        selectInput("lineplot_trendline", "Trendline Type", 
                    choices = c("None", "Linear", "Polynomial"))
    })
  
    ################################################
    # L1 Inspect Model
    ################################################
    
    output$inspect_model<-renderUI({
    values$chosen_dataset <- event_data("plotly_click", source="model_plot")
    if (length(values$chosen_dataset)) {
      
        chosen_prot <- as.numeric(values$chosen_dataset[["x"]])
        chosen_rep <- as.numeric(values$chosen_dataset[["y"]])
        message("2 Done")
        
        prot_index <- which(values$result$paramlog$protein_num==chosen_prot)
        
        train_index <- which(values$result$paramlog$train_size==chosen_rep)
        message("3 Done")
        message(prot_index, train_index)
        # Get model with the highest accuracy from all iterations on the given dataset
        
        iter <- length(values$result$results)
        accuracy <- vector()
        
        for (i in 1:iter) {
            message(i, " Model Checked")
            currentmodel <- values$result$results[[i]][[prot_index]][[train_index]][[2]]
            currentmodel.pred <- predict(currentmodel, values$valid_x) #Predict validation data
            accuracy[i] <- sum(diag(table(currentmodel.pred, values$valid_y))) / length(currentmodel.pred)
            highestacc <- which(accuracy==max(accuracy))[[1]]
        }
        message("highestacc: ", highestacc)
        message("4 Done")
      
        values$chosen_model <- values$result$results[[highestacc]][[prot_index]][[train_index]][[2]]
        values$chosen_model.pred <- predict(values$chosen_model, values$valid_x)
        values$confusionMatrix <- confusionMatrix(data = as.factor(as.numeric(values$chosen_model.pred)), 
                                                  reference = as.factor(values$valid_y))
        message("5 Done")
        
        d1 <- as.data.frame(values$confusionMatrix$overall)
        colnames(d1) <- "Value"
        d2 <- as.data.frame(values$confusionMatrix$byClass)
        colnames(d2) <- "Value"
        # c2 <- rbind(d1, d2)
        # c1 <- rownames(c2)
        values$chosen_model.metrics <- rbind(d1, d2)
        message("6 Done")
      
        tabBox(id="inspect_model_wrapper", width = 12,
             tabPanel("Confusion Matrix",
                      div(style="display: inline-block;vertical-align:top; width: 75%;",
                          plotOutput("confusion_matrix")),
                      div(style="display: inline-block;vertical-align:top; width: 24%;",
                          DT::dataTableOutput("metrics_table"))),
             tabPanel("Predictive Features",
                      plotOutput("predictive_features")),
             tabPanel("LIME",
                      pickerInput("test_set_picker", 
                                  label = "Select observations to test model with", 
                                  choices = as.character(rownames(values$valid_x)),
                                  selected=1,
                                  options = list(
                                    `actions-box` = TRUE, 
                                    size = 10),
                                  multiple = TRUE),
                      sliderInput("feature_picker",
                                   label = "Number of features to display",
                                   value = 4,
                                   min = 1,
                                   max = 10,
                                   step = 1,
                                   round = TRUE),
                      plotOutput("lime_plot", height = "auto"))
             )
        }
    })
  
    ################################################
    # L2 Inspect model / LIME
    ################################################
    output$lime_plot <- renderPlot({
        test_set <- as.integer(input$test_set_picker)
        explainer <- lime(values$valid_x[-test_set,], 
                          as_classifier(values$chosen_model), 
                          bin_continuous = TRUE, 
                          quantile_bins = FALSE)
        explanation <- explain(values$valid_x[test_set, ], 
                               explainer, 
                               n_labels = 1,
                               n_features = input$feature_picker)
        plot_features(explanation, ncol = 1)}, 
        height = function(){
            (70 + input$feature_picker * 50) * length(input$test_set_picker)
    })
  
    ## Plots & Tables / confusion_matrix
    output$confusion_matrix <- renderPlot(
        ggplot(as.data.frame(values$confusionMatrix$table), 
               aes(x=Prediction, y=Reference, fill=Freq)) +
            geom_tile() +
            geom_text(aes(label=Freq))
    )
  
    ## Plots & Tables / metrics 
    output$metrics_table = DT::renderDataTable({
        values$chosen_model.metrics
    })
    
    ## Plots & Tables / variable importance
    output$predictive_features <- renderPlot({
        plot(varImp(values$chosen_model), 
             top=10)
    })
    
    ## Actual Simulation Function
    observeEvent(input$start, {
        withCallingHandlers({
            shinyjs::html("text", "")
            shinyjs::html(id = "main_output", html = "")
            # shinyjs::runjs(
            #   var element = document.getElementById("mainOutput");
            #   element.scrollTo = element.scrollHeight;
            # )
            values$result <- ParSimulatedClassificationCrossDatasets(values$quant_data,
                                                   values$parameters,
                                                   n_sample = input$n_sample,
                                                   sample_incr = input$sample_incr,
                                                   n_protein = input$n_protein,
                                                   iter = input$iter,
                                                   classifier = input$classifier,
                                                   use_caret = TRUE)
            rownames(values$result$meanPA) <- gsub("prot", "", rownames(values$result$meanPA))
            colnames(values$result$meanPA) <- gsub("tra", "", colnames(values$result$meanPA))
            values$gg_meanPA<-melt(values$result$meanPA)
            
            # Clean up the validation set as output from parameters
            values$valid_x <- as.data.frame(values$parameters$X)
            rownames(values$valid_x) <- as.numeric(as.factor(rownames(values$valid_x)))              
            values$valid_y <- as.factor(values$parameters$Y)
            values$valid_y <- as.numeric(values$valid_y)
            message("1 Done")
            
            values$simulated_classification_status <- "completed"
            
        },
        message = function(m) {
           shinyjs::html(id = "main_output", html = paste(m$message,"<br>", sep=" "), add = TRUE)
        })
    })
  
    ## For Simulated
    ## PCA
    output$s_pcabiplot <- renderPlot({
        ggbiplot(values$s_pca, 
                 ellipse=TRUE, 
                 var.axes=FALSE, 
                 groups=values$s_sample_annotation)
    })
    
    output$s_pcascreeplot <- renderPlot({
        ggscreeplot(values$s_pca)
    })
    
    ## Global QC Box Plot
    output$s_global_boxplot <- renderPlotly({
        plot_ly(values$s_quant_data, 
                y=~LogIntensities, 
                x=~as.character(originalRUN), 
                color=~Group, type = "box") %>%
        layout(xaxis=list(title="Biological Replicate"), 
               yaxis=list(title="Log Intensities"))
    })
  
    ## Experiment Plots
    output$model_heatmap<-renderPlotly({
        plot_ly(
            y=colnames(values$result$meanPA),
            x=rownames(values$result$meanPA),
            z=values$result$meanPA,
            type="heatmap",
            source="model_plot"
        ) %>%
            layout(xaxis=list(title="Number of Proteins", 
                              type="category"), 
                   yaxis=list(title="Number of Biological Replicates",
                              type="category"))
      # 
      # hm.palette <- colorRampPalette(rev(brewer.pal(9, 'RdBu')), space='Lab')
      # ggplot(data=values$gg_meanPA, aes(x=Var1, y=Var2, fill=value)) +
      #  geom_tile() +
      #  # geom_text(aes(label=round(value,3))) +
      #  coord_fixed(ratio=max(values$gg_meanPA$Var1, na.rm = TRUE)/max(values$gg_meanPA$Var2, na.rm = TRUE)) +
      #  scale_fill_viridis() +
      #  labs(title="Heatmap", x="Protein number", y='Mean accuracy')
  
      # plot_ly(z=~values$result$meanPA, type="heatmap")
    })
  
    output$lineplot<-renderPlot({
        ## ggplot needs a long format dataframe
        ## get the mean accuracy
        meandata <- melt(values$result$meanPA)
        colnames(meandata) <- c("protein_num", "sample_size", "mean_acc")
        
        switch(input$lineplot_xvar,
               "Protein number" =
                    p <- ggplot(data = meandata, 
                                aes(x= protein_num, 
                                    y = mean_acc, 
                                    group = sample_size, 
                                    colour = sample_size)) +
                        geom_point() +
                        labs(title="Sample size estimation", 
                             x="Protein number", 
                             y="Mean accuracy") +
                        guides(color = guide_legend(title="Sample size")),
              "Sample size" =
                    p <- ggplot(data = meandata, 
                                aes(x= sample_size, 
                                    y = mean_acc, 
                                    group = protein_num, 
                                    colour = protein_num)) +
                        geom_point() +
                        labs(title="Sample size estimation", 
                             x="Sample size", 
                             y='Mean accuracy') +
                        guides(color = guide_legend(title="Protein number")),
        )
        
        switch(input$lineplot_trendline,
               "None" =
                    p <- p + geom_line(),
               "Linear" =
                    p <- p + geom_smooth(method = lm, se = FALSE),
               "Polynomial" =
                    p <- p + geom_smooth(method = lm, se = FALSE, formula = y ~ poly(x, 3))
        )
  
        ## make the plot
        p + geom_line()
        return(p)
  
    })
}