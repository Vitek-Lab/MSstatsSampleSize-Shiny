
function(session, input, output) {
  
  ## Set maximum size of uploaded files to 300mb
  options(shiny.maxRequestSize = 300*1024^2)
  #### Toggle control for sidebar ####
  observeEvent(input$data_format,{
    shinyjs::toggleElement(id = "standard_count",
                           condition = input$data_format == "standard")
    shinyjs::toggleElement(id = "standard_annot",
                           condition = input$data_format == "standard")
  })
  
  #### Import data, action click ####
  data <- eventReactive(input$import_data, {
    withProgress(
      data <- show_faults(
        expr = explore_data(format = input$data_format,
                            count = input$standard_count,
                            annot = input$standard_annot,
                            session = session),
        session = session
      ),
    message = "Progress:", value = 0.2, detail = "Loading Data...")
    return(data)
  })
  
  observeEvent(input$import_data,{
    updateTabItems(session = session, "tabs", selected = "explore_data")
  })
  
  #### Visualize EDA ####
  output$dataset_name <- renderText(
    paste("Data Set Name:",data()$dataset_name)
  )
  output$cond_sum_table <- DT::renderDataTable(
    DT::datatable(data()$cond_sum_table, options = list(dom = 't'))
  )
  
  output$sum_table <- DT::renderDataTable(
    DT::datatable(data()$sum_table, options = list(dom = 't'))
  )
  
  output$global_boxplot <- plotly::renderPlotly(
    data()$boxplot
  )

  output$mean_sd_plot <- renderPlot(
    data()$meanSDplot
  )
  
  
  
  
  # 
  # ################################################
  # ################################################
  # ## Tab 2 Simulate datasets Tab : simulateDataset()
  # ################################################
  # ################################################
  # 
  # output$explore_simulated_content<-renderUI({
  #   if (is.null(values$is_imported)) {
  #     tags$b("Please use the 'Import Data' menu to import a proteome dataset.")
  #   } else {
  #     tagList(
  #       fluidRow(
  #         box(title = paste("Selected Dataset:",values$selected_dataset_name), 
  #             width=9,
  #             htmlOutput("inspect_simulated")
  #         ),
  #         box(title = "Simulated Datasets",
  #             width = 3, 
  #             plotlyOutput("simulated_grid"),
  #             br(),
  #             h3("Parameters", class="custom-box-title"),
  #             numericInput("n_sample", label="Number of Different Sample Sizes to Simulate", value=5),
  #             numericInput("sample_incr", label="Step Size Between Simulated Sample Sizes", value=20),
  #             numericInput("n_protein", label="Number of Different Protein Counts to Simulate", value=5)
  #         )
  #       )
  #     )
  #   }
  # })
  # 
  # outputOptions(output, "explore_simulated_content", suspendWhenHidden = FALSE)
  # 
  # ################################################
  # ## L1 Explore Simulated Data / Process and generate tabset for specified simulated dataset
  # ################################################
  # output$inspect_simulated<-renderUI({
  #   values$selected_dataset <- event_data("plotly_click", source = "simulated_grid_plot")
  #   if (length(values$selected_dataset)) {
  #     protein_desc <- 1/input$n_protein
  #     
  #     mu <- values$parameters$mu
  #     sigma <- values$parameters$sigma
  #     promean <- values$parameters$promean
  #     
  #     # Use mean and variance components to generate dataset on the fly
  #     ## No. of proteins is vars[1], No. of samples is vars[2]
  #     vars <- c(values$selected_dataset[["x"]], values$selected_dataset[["y"]])
  #     values$selected_dataset_name<-paste(vars[1]," Proteins, ",vars[2]," BioReplicates",sep="")
  #     
  #     selectedProt<-order(promean, decreasing=TRUE)[1:vars[1]]
  #     mu_selected <- mu[selectedProt,]
  #     sigma_selected <- sigma[selectedProt,]
  #     simulated_data <- .sampleSimulation(as.integer(vars[2]), mu_selected, sigma_selected)
  #     
  #     # values$s_prot_abundance <- t(simulated_data[["X"]])
  #     # values$s_sample_annotation <- simulated_data[["Y"]]
  #     # summary.s <- matrix(NA,ncol=nlevels(values$s_sample_annotation), nrow=1)
  #     # values$summary <- summary.s
  #     # summary <<- values$summary
  #     
  #     
  #     
  #     # merged<-melt(simulated_data[["X"]], value.name = "LogIntensities", varnames = c('originalRUN', 'Protein'))
  #     # merged$LogIntensities <- suppressWarnings(as.numeric(paste(merged$LogIntensities)))
  #     # values$s_groups<-data.frame("originalRUN"=1:length(simulated_data[["Y"]]), "Group"=simulated_data[["Y"]])
  #     # merged<-merge(merged, values$s_groups, by="originalRUN")
  #     # values$s_quant_data<-merged
  #     
  #     ## # of biological replicates
  #     # temp <- unique(merged[, c("Group", "originalRUN")])
  #     # temp1 <- xtabs(~Group, data=temp)
  #     # summary.s[1,] <- temp1
  #     # 
  #     # colnames(summary.s) <- unique(values$quant_data[["RunlevelData"]]$GROUP_ORIGINAL)
  #     # rownames(summary.s) <- c("# of Biological Replicates")
  #     # values$s_summary <- summary.s
  #     showNotification("Data processing complete", duration=10, closeButton = TRUE, type="message")
  #     
  #     tprot <- t(values$s_prot_abundance)
  #     class(tprot) <- "numeric"
  #     
  #     remove_cols<-nearZeroVar(tprot, names=TRUE, freqCut=19, uniqueCut=10)
  #     keep_cols<-colnames(tprot)
  #     tprot<-tprot[,setdiff(keep_cols,remove_cols)]
  #     
  #     tprot_pca1 <- prcomp(tprot, center = TRUE,scale. = TRUE)
  #     values$s_pca<-tprot_pca1
  #     showNotification("PCA complete")
  #     
  #     tabBox(id="simulated_data_wrapper", width="100%",
  #            tabPanel("Raw Data",
  #                     div(style = 'overflow-x:scroll;',
  #                         DT::dataTableOutput("s_sample_annotation_table"),
  #                         DT::dataTableOutput("s_prot_abundance_table")
  #                     )
  #            ),
  #            tabPanel("Summary",
  #                     div(style = 'overflow-x:scroll;',
  #                         DT::dataTableOutput("s_summary_table")
  #                     )
  #            ),
  #            tabPanel("PCA",
  #                     div(style = 'overflow-x:scroll;',
  #                         plotOutput("s_pcabiplot"),
  #                         plotOutput("s_pcascreeplot")
  #                     )
  #            ),
  #            tabPanel("QC Box Plots",
  #                     div(style = 'overflow-x:scroll;',
  #                         plotlyOutput("s_global_boxplot")
  #                     )
  #            )
  #     )
  #   } else {
  #     values$selected_dataset_name<-"No dataset selected"
  #     div("View and explore a simulated dataset by clicking on the tile on the heatmap that corresponds to the requested number of proteins and biological replicates. If required, vary the range of datasets to be simulated by modifying the parameters.")
  #   }
  #   
  # })
  # 
  # ################################################
  # ## L2 Explore simulated data / Specified dataset / Data tables
  # ################################################
  # #output$s_prot_abundance_table = DT::renderDataTable({
  # #    values$s_prot_abundance
  # #})
  # 
  # #output$s_sample_annotation_table = DT::renderDataTable({
  # #    values$s_groups
  # #})
  # 
  # output$s_summary_table = DT::renderDataTable({
  #   values$s_summary
  # })
  # 
  # ################################################
  # ## L2 Explore simulated data / Specified dataset / Plot: simulated_grid
  # ################################################
  # output$simulated_grid <- renderPlotly({
  #   protein_desc <- 1/input$n_protein
  #   nproteins <- length(unique(values$quant_data$ProcessedData$PROTEIN))
  #   m_prot <- floor(nproteins*protein_desc)
  #   ngroup <- length(unique(values$quant_data$ProcessedData$GROUP_ORIGINAL))
  #   sample_incr <- input$sample_incr
  #   n_sample <- input$n_sample
  #   train_size <- seq.int(from = sample_incr, 
  #                         to = sample_incr * n_sample, 
  #                         length.out = n_sample)
  #   train_size <- train_size * ngroup
  #   protein_num <- seq.int(from = m_prot, to = nproteins, by = m_prot)
  #   
  #   rownames <- as.character(train_size)
  #   colnames <- as.character(protein_num)
  #   
  #   simulated_grid <- matrix(NA, 
  #                            nrow=length(rownames), 
  #                            ncol=length(colnames), 
  #                            dimnames = list(rownames,colnames))
  #   for (m in rownames(simulated_grid)) {
  #     for (n in colnames(simulated_grid)) {
  #       simulated_grid[m,n] <- as.numeric(m)*as.numeric(n) 
  #     }
  #   }
  #   
  #   plot_ly(
  #     x=colnames(simulated_grid),
  #     y=rownames(simulated_grid),
  #     z=simulated_grid,
  #     type="heatmap",
  #     source="simulated_grid_plot"
  #   ) %>%
  #     layout(xaxis=list(title="Number of Proteins", type="category"), 
  #            yaxis=list(title="Number of Biological Replicates", type="category"))
  # })
  # 
  # ################################################
  # ################################################
  # # Tab 3 Analyze the simulated datasets: designSampleSizeClassification()
  # ################################################
  # ################################################
  # 
  # ## Main Content
  # output$analyse_simulated_content<-renderUI({
  #   if (is.null(values$is_imported)) {
  #     
  #     tags$b("Please use the 'Import data' menu to import a proteome dataset.")
  #     
  #   } else {
  #     tagList(
  #       fluidRow(
  #         box(title = values$simulated_classification_title,
  #             width = 9, 
  #             htmlOutput("simulated_classification_heatmap")
  #         ),
  #         box(title="Train Models", 
  #             width = 3,
  #             radioButtons("train_type", label="Select type of analysis", 
  #                          choices=list("Between datasets (Single classifier)"="between_datasets", 
  #                                       "Between classifiers (Single dataset)"="between_classifiers")
  #             ),
  #             htmlOutput("train_options"),
  #             numericInput("iter", 
  #                          label="Iteration Count", value=10),
  #             actionButton("start", 
  #                          label="Begin Simulation")
  #         )
  #       ),
  #       fluidRow(
  #         htmlOutput("inspect_model")
  #       )
  #     )
  #   }
  # })
  # 
  # ################################################
  # ## L2 Experiment Simulation / Training Options
  # ################################################
  # 
  # output$train_options <- renderUI({
  #   if (!is.null(input$train_type)) {
  #     switch(input$train_type,
  #            "between_datasets" = {
  #              # numericInput("n_sample", "Number of proteins", value=input$s_n_sample),
  #              # 
  #              # n_sample = input$n_sample,
  #              # sample_incr = input$sample_incr,
  #              # protein_desc = 1/input$protein_desc,
  #              selectInput("classifier", 
  #                          "Select Classifier", 
  #                          choices=list("Random Forest" = "rf", 
  #                                       "SVM" = "svmLinear", 
  #                                       "Naive Bayes" = "naive_bayes", 
  #                                       "Partial Least Squares" = "pls", 
  #                                       "Neural Net (3 layers)" = "nnet"))
  #            },
  #            "between_classifiers" = {
  #              values$chosen_dataset <- event_data("plotly_click", source = "model_plot")
  #              
  #              if (length(values$chosen_dataset)) {
  #                values$between_classifiers_prot <- values$chosen_dataset[["x"]]
  #                values$between_classifiers_samples <- values$chosen_dataset[["y"]]
  #              }
  #              
  #              tagList(
  #                tags$b("Choose a dataset by clicking on its tile on the heatmap"),
  #                actionButton("between_classifiers_declare", "Select Dataset"),
  #                tags$p(paste("Selected dataset:",
  #                             values$between_classifiers_prot,
  #                             " Proteins, ",
  #                             values$between_classifiers_samples,
  #                             " BioReplicates"))
  #              )
  #            }
  #     )
  #   }
  # })
  # 
  # ################################################
  # ## L2 Experiment Simulation / Heatmap
  # ################################################
  # 
  # output$simulated_classification_heatmap<-renderUI({
  #   if (is.null(values$simulated_classification_status)) {
  #     values$simulated_classification_title = "Waiting for trained models..."
  #     tagList(
  #       div('Train classifiers on generated fake datasets by clicking "Begin Simulation" on the right. Optionally, change the classifier to be used, or choose to analyse cross-classifier performance for a single dataset on the right.'),
  #       textOutput("main_output")
  #     )
  #   } else {
  #     values$simulated_classification_title = "Trained Models"
  #     tabBox(id="simulated_classification_wrapper", width="100%",
  #            tabPanel("Heatmap",
  #                     plotlyOutput("model_heatmap")),
  #            tabPanel("Line Graphs",
  #                     htmlOutput("lineplot_xvar_selector"),
  #                     htmlOutput("lineplot_trendline_selector"),
  #                     plotOutput("lineplot") )
  #     )
  #   }
  # })
  # 
  # ################################################
  # # L2 Experiment Simulation / Lineplot Type Selectors
  # ################################################
  # 
  # output$lineplot_xvar_selector <- renderUI({
  #   selectInput("lineplot_xvar", "X variable:", 
  #               choices = c("Protein number", "Sample size"))
  # })
  # 
  # output$lineplot_trendline_selector <- renderUI({
  #   selectInput("lineplot_trendline", "Trendline Type", 
  #               choices = c("None", "Linear", "Polynomial"))
  # })
  # 
  # ################################################
  # # L1 Inspect Model
  # ################################################
  # 
  # output$inspect_model<-renderUI({
  #   values$chosen_dataset <- event_data("plotly_click", source="model_plot")
  #   if (length(values$chosen_dataset)) {
  #     
  #     chosen_prot <- as.numeric(values$chosen_dataset[["x"]])
  #     chosen_rep <- as.numeric(values$chosen_dataset[["y"]])
  #     message("2 Done")
  #     
  #     prot_index <- which(values$result$paramlog$protein_num==chosen_prot)
  #     
  #     train_index <- which(values$result$paramlog$train_size==chosen_rep)
  #     message("3 Done")
  #     message(prot_index, train_index)
  #     # Get model with the highest accuracy from all iterations on the given dataset
  #     
  #     iter <- length(values$result$results)
  #     accuracy <- vector()
  #     
  #     for (i in 1:iter) {
  #       message(i, " Model Checked")
  #       currentmodel <- values$result$results[[i]][[prot_index]][[train_index]][[2]]
  #       currentmodel.pred <- predict(currentmodel, values$valid_x) #Predict validation data
  #       accuracy[i] <- sum(diag(table(currentmodel.pred, values$valid_y))) / length(currentmodel.pred)
  #       highestacc <- which(accuracy==max(accuracy))[[1]]
  #     }
  #     message("highestacc: ", highestacc)
  #     message("4 Done")
  #     
  #     values$chosen_model <- values$result$results[[highestacc]][[prot_index]][[train_index]][[2]]
  #     values$chosen_model.pred <- predict(values$chosen_model, values$valid_x)
  #     values$confusionMatrix <- confusionMatrix(data = as.factor(as.numeric(values$chosen_model.pred)), 
  #                                               reference = as.factor(values$valid_y))
  #     message("5 Done")
  #     
  #     d1 <- as.data.frame(values$confusionMatrix$overall)
  #     colnames(d1) <- "Value"
  #     d2 <- as.data.frame(values$confusionMatrix$byClass)
  #     colnames(d2) <- "Value"
  #     # c2 <- rbind(d1, d2)
  #     # c1 <- rownames(c2)
  #     values$chosen_model.metrics <- rbind(d1, d2)
  #     message("6 Done")
  #     
  #     tabBox(id="inspect_model_wrapper", width = 12,
  #            tabPanel("Confusion Matrix",
  #                     div(style="display: inline-block;vertical-align:top; width: 75%;",
  #                         plotOutput("confusion_matrix")),
  #                     div(style="display: inline-block;vertical-align:top; width: 24%;",
  #                         DT::dataTableOutput("metrics_table"))),
  #            tabPanel("Predictive Features",
  #                     plotOutput("predictive_features")),
  #            tabPanel("LIME",
  #                     pickerInput("test_set_picker", 
  #                                 label = "Select observations to test model with", 
  #                                 choices = as.character(rownames(values$valid_x)),
  #                                 selected=1,
  #                                 options = list(
  #                                   `actions-box` = TRUE, 
  #                                   size = 10),
  #                                 multiple = TRUE),
  #                     sliderInput("feature_picker",
  #                                 label = "Number of features to display",
  #                                 value = 4,
  #                                 min = 1,
  #                                 max = 10,
  #                                 step = 1,
  #                                 round = TRUE),
  #                     plotOutput("lime_plot", height = "auto"))
  #     )
  #   }
  # })
  # 
  # ################################################
  # # L2 Inspect model / LIME
  # ################################################
  # output$lime_plot <- renderPlot({
  #   test_set <- as.integer(input$test_set_picker)
  #   explainer <- lime(values$valid_x[-test_set,], 
  #                     as_classifier(values$chosen_model), 
  #                     bin_continuous = TRUE, 
  #                     quantile_bins = FALSE)
  #   explanation <- explain(values$valid_x[test_set, ], 
  #                          explainer, 
  #                          n_labels = 1,
  #                          n_features = input$feature_picker)
  #   plot_features(explanation, ncol = 1)}, 
  #   height = function(){
  #     (70 + input$feature_picker * 50) * length(input$test_set_picker)
  #   })
  # 
  # ## Plots & Tables / confusion_matrix
  # output$confusion_matrix <- renderPlot(
  #   ggplot(as.data.frame(values$confusionMatrix$table), 
  #          aes(x=Prediction, y=Reference, fill=Freq)) +
  #     geom_tile() +
  #     geom_text(aes(label=Freq))
  # )
  # 
  # ## Plots & Tables / metrics 
  # output$metrics_table = DT::renderDataTable({
  #   values$chosen_model.metrics
  # })
  # 
  # ## Plots & Tables / variable importance
  # output$predictive_features <- renderPlot({
  #   plot(varImp(values$chosen_model), 
  #        top=10)
  # })
  # 
  # ## Actual Simulation Function
  # observeEvent(input$start, {
  #   withCallingHandlers({
  #     shinyjs::html("text", "")
  #     shinyjs::html(id = "main_output", html = "")
  #     # shinyjs::runjs(
  #     #   var element = document.getElementById("mainOutput");
  #     #   element.scrollTo = element.scrollHeight;
  #     # )
  #     values$result <- ParSimulatedClassificationCrossDatasets(values$quant_data,
  #                                                              values$parameters,
  #                                                              n_sample = input$n_sample,
  #                                                              sample_incr = input$sample_incr,
  #                                                              n_protein = input$n_protein,
  #                                                              iter = input$iter,
  #                                                              classifier = input$classifier,
  #                                                              use_caret = TRUE)
  #     rownames(values$result$meanPA) <- gsub("prot", "", rownames(values$result$meanPA))
  #     colnames(values$result$meanPA) <- gsub("tra", "", colnames(values$result$meanPA))
  #     values$gg_meanPA<-melt(values$result$meanPA)
  #     
  #     # Clean up the validation set as output from parameters
  #     values$valid_x <- as.data.frame(values$parameters$X)
  #     rownames(values$valid_x) <- as.numeric(as.factor(rownames(values$valid_x)))              
  #     values$valid_y <- as.factor(values$parameters$Y)
  #     values$valid_y <- as.numeric(values$valid_y)
  #     message("1 Done")
  #     
  #     values$simulated_classification_status <- "completed"
  #     
  #   },
  #   message = function(m) {
  #     shinyjs::html(id = "main_output", html = paste(m$message,"<br>", sep=" "), add = TRUE)
  #   })
  # })
  # 
  # ## For Simulated
  # ## PCA
  # output$s_pcabiplot <- renderPlot({
  #   ggbiplot(values$s_pca, 
  #            ellipse=TRUE, 
  #            var.axes=FALSE, 
  #            groups=values$s_sample_annotation)
  # })
  # 
  # output$s_pcascreeplot <- renderPlot({
  #   ggscreeplot(values$s_pca)
  # })
  # 
  # ## Global QC Box Plot
  # output$s_global_boxplot <- renderPlotly({
  #   plot_ly(values$s_quant_data, 
  #           y=~LogIntensities, 
  #           x=~as.character(originalRUN), 
  #           color=~Group, type = "box") %>%
  #     layout(xaxis=list(title="Biological Replicate"), 
  #            yaxis=list(title="Log Intensities"))
  # })
  # 
  # ## Experiment Plots
  # output$model_heatmap<-renderPlotly({
  #   plot_ly(
  #     y=colnames(values$result$meanPA),
  #     x=rownames(values$result$meanPA),
  #     z=values$result$meanPA,
  #     type="heatmap",
  #     source="model_plot"
  #   ) %>%
  #     layout(xaxis=list(title="Number of Proteins", 
  #                       type="category"), 
  #            yaxis=list(title="Number of Biological Replicates",
  #                       type="category"))
  #   # 
  #   # hm.palette <- colorRampPalette(rev(brewer.pal(9, 'RdBu')), space='Lab')
  #   # ggplot(data=values$gg_meanPA, aes(x=Var1, y=Var2, fill=value)) +
  #   #  geom_tile() +
  #   #  # geom_text(aes(label=round(value,3))) +
  #   #  coord_fixed(ratio=max(values$gg_meanPA$Var1, na.rm = TRUE)/max(values$gg_meanPA$Var2, na.rm = TRUE)) +
  #   #  scale_fill_viridis() +
  #   #  labs(title="Heatmap", x="Protein number", y='Mean accuracy')
  #   
  #   # plot_ly(z=~values$result$meanPA, type="heatmap")
  # })
  # 
  # output$lineplot<-renderPlot({
  #   ## ggplot needs a long format dataframe
  #   ## get the mean accuracy
  #   meandata <- melt(values$result$meanPA)
  #   colnames(meandata) <- c("protein_num", "sample_size", "mean_acc")
  #   
  #   switch(input$lineplot_xvar,
  #          "Protein number" =
  #            p <- ggplot(data = meandata, 
  #                        aes(x= protein_num, 
  #                            y = mean_acc, 
  #                            group = sample_size, 
  #                            colour = sample_size)) +
  #            geom_point() +
  #            labs(title="Sample size estimation", 
  #                 x="Protein number", 
  #                 y="Mean accuracy") +
  #            guides(color = guide_legend(title="Sample size")),
  #          "Sample size" =
  #            p <- ggplot(data = meandata, 
  #                        aes(x= sample_size, 
  #                            y = mean_acc, 
  #                            group = protein_num, 
  #                            colour = protein_num)) +
  #            geom_point() +
  #            labs(title="Sample size estimation", 
  #                 x="Sample size", 
  #                 y='Mean accuracy') +
  #            guides(color = guide_legend(title="Protein number")),
  #   )
  #   
  #   switch(input$lineplot_trendline,
  #          "None" =
  #            p <- p + geom_line(),
  #          "Linear" =
  #            p <- p + geom_smooth(method = lm, se = FALSE),
  #          "Polynomial" =
  #            p <- p + geom_smooth(method = lm, se = FALSE, formula = y ~ poly(x, 3))
  #   )
  #   
  #   ## make the plot
  #   p + geom_line()
  #   return(p)
  #   
  # })
}