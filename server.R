
function(session, input, output) {
  
  ## Set maximum size of uploaded files to 300mb
  options(shiny.maxRequestSize = 300*1024^2)
  rv <- reactiveValues()
  #### Toggle control for sidebar ####
  # Enable or disable fileInputs based on type of data selected
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
  #switches to the data exploration tabs which are populated with the EDA
  observeEvent(input$import_data,{
    updateTabItems(session = session, "tabs", selected = "explore_data")
  })
  
  #### Visualize EDA ####
  output$dataset_name <- renderText(
    paste("Data Set Name:",data()$dataset_name)
  )
  # Condition Summary Table
  output$cond_sum_table <- DT::renderDataTable(
    DT::datatable(data()$cond_sum_table, options = list(dom = 't'),
                  selection = 'none')
  )
  # Data Summary Table
  output$sum_table <- DT::renderDataTable(
    DT::datatable(data()$sum_table, options = list(dom = 't'),
                  selection = 'none')
  )
  # Boxplot for Proteins
  output$global_boxplot <- plotly::renderPlotly(
    data()$boxplot
  )
  # Mean and Standard Deviation Plot
  output$mean_sd_plot <- renderPlot(
    data()$meanSDplot
  )
  
  #### Toggle control for simulate data tab ####
  observeEvent(input$exp_fc,{
    shinyjs::toggleElement(id = "diff_prot",
                           condition = input$exp_fc != "data")
    shinyjs::toggleElement(id = "exp_fc_name",
                           condition = input$exp_fc != "data")
  })

  observeEvent(input$sel_sim_prot,{
    shinyjs::toggleElement(id = "prot_prop",
                           condition = input$sel_sim_prot == "proportion")
    shinyjs::toggleElement(id = "prot_num",
                           condition = input$sel_sim_prot != "proportion")
  })
  
  observeEvent(input$sim_val,{
    shinyjs::toggleElement(id = "n_val_samp_grp",
                           condition = input$sim_val == T)
  })
  
  observeEvent(input$upload_params, {
    shinyjs::toggleElement(id = "param_input",
                           condition = input$upload_params == T)
    shinyjs::toggleElement(id = "param_box",
                           condition = input$upload_params != T)
  })
  
  #### Simulate Data Button Click ####
  simulations <- eventReactive(input$simulate,{
    validate(need(nrow(data()$wide_data) != 0, "Import Data using the Import Data Menu"))
    withProgress(
      data <- show_faults(
        expr = simulate_grid(data = data()$wide_data,
                             annot = data()$annot_data,
                             num_simulation = input$n_sim,
                             exp_fc = input$exp_fc,
                             fc_name = input$exp_fc_name,
                             list_diff_proteins = input$diff_prot,
                             sel_simulated_proteins = input$sel_sim_prot,
                             prot_proportion = input$prot_prop,
                             prot_number = input$prot_num,
                             samples_per_group = input$n_samp_grp,
                             sim_valid = input$sim_val,
                             valid_samples_per_grp = input$n_val_samp_grp,
                             session = session),
        session = session
      ),
      message = "Progress:", value = 0.2, detail = "Simulating Data...")
    return(data)
  })
  
  #### Toggle Switch for previous/next/download buttons, updates select input ####
  observeEvent(input$simulate, {
    validate(need(nrow(data()$wide_data) != 0, "Import Data using the Import Data Menu"))
    sc <- sprintf("Simulation %s", seq(1, input$n_sim))
    sim_choices <<- do.call('c',lapply(sc, function(x){
      sprintf("%s Sample Size %s", x,
              as.numeric(unlist(strsplit(input$n_samp_grp, ','))))
      }))
    updateSelectInput(session = session, inputId ="simulations", label = "Simulations",
                      choices = sim_choices)
    shinyjs::toggleState(id = "fwd", condition = length(sim_choices) > 1)
    shinyjs::toggleState(id = "back", condition = length(sim_choices) > 1)
  })
  
  #### Backend for previous and next buttons ####
  observeEvent(input$back, {
    curr <- which(sim_choices == input$simulations)
    if(curr > 1){
      updateSelectInput(session = session, "simulations",
                        choices = sim_choices, selected = sim_choices[curr - 1])
    }
  })
  
  observeEvent(input$fwd,{
    curr <- which(sim_choices == input$simulations)
    if(curr >= 1){
      updateSelectInput(session = session, "simulations",
                        choices = sim_choices, selected = sim_choices[curr + 1])
    }
  })
  
  #### Backend for download all plots ####
  output$download_pca <- downloadHandler(
    filename = sprintf("PCA_plots_%s.pdf", format(Sys.time(), "%Y%m%d%H%M%S")),
    content = function(file){
      withProgress({
        pdf(file = file)
        for(i in seq_along(sim_choices)){
          status(sprintf("Plotting %s plot", i), value = i/length(sim_choices), session = session)
          vals <- unlist(stringr::str_extract_all(sim_choices[i],'\\d+'))
          sim <- sprintf("simulation%s",vals[1])
          print(make_pca_plots(simulations()[[vals[2]]], which = sim)+
                  labs(title = sim_choices[i]))
        }
        dev.off()
      }
      , session = session)
    }
  )
  
  #### Render PCA plots for selected simulations ####
  output$pca_plot <- renderPlot(
    if(!is.null(input$simulations)){
      validate(need(nchar(input$simulations) != 0, 'No Sims'))
      vals <- unlist(stringr::str_extract_all(input$simulations,'\\d+'))
      sim <- sprintf("simulation%s",vals[1])
      show_faults({
        make_pca_plots(simulations()[[vals[2]]], which = sim)},
        session = session)
    }else{
      shiny::showNotification("No Simulations Found", duration = NULL)
    }
  )
 
  
  #### Run Classification #####
  observeEvent(input$n_samp_grp,{
    vals <- unlist(strsplit(input$n_samp_grp, ","))
    vals <- sprintf("Sample%s", vals)
    updateSelectInput(session = session, inputId = "s_size", label = "Sample Size",
                      choices = vals)
  })
  
  observeEvent(input$use_h2o, {
    shinyjs::toggleElement(id = "model_config", condition = input$use_h2o == T)
    shinyjs::toggleElement(id = "p_imp", condition = input$use_h2o != T)
    shinyjs::toggleElement(id = "pred_acc", condition = input$use_h2o != T)
  })
  
  observeEvent(input$classifier, {
    shinyjs::toggleElement(id = "stop_metric",
                           condition = (input$use_h2o == T && input$classifier %in% c("rf","logreg")))
    shinyjs::toggleElement(id = "nfolds",
                           condition = (input$use_h2o == T && input$classifier == "rf"))
    shinyjs::toggleElement(id = "f_assignment",
                           condition = (input$use_h2o == T && input$classifier %in% c("rf", "naive_bayes")))
    shinyjs::toggleElement(id = "iters",
                           condition = (input$use_h2o == T && input$classifier == "svmLinear"))
    shinyjs::toggleElement(id = "link",
                           condition = (input$use_h2o == T && input$classifier == "logreg"))
    shinyjs::toggleElement(id = "family",
                           condition = (input$use_h2o == T && input$classifier == "logreg"))
    shinyjs::toggleElement(id = "solver",
                           condition = (input$use_h2o == T && input$classifier == "logreg"))
    shinyjs::toggleElement(id = "laplace",
                           condition = (input$use_h2o == T && input$classifier == "naive_bayes"))
    shinyjs::toggleElement(id = "eps_sdev",
                           condition = (input$use_h2o == T && input$classifier == "naive_bayes"))
    shinyjs::toggleElement(id = "min_sdev",
                           condition = (input$use_h2o == T && input$classifier == "naive_bayes"))
  })
  
  observeEvent(input$run_model,{
    withProgress({
      browser()
      rv$classification <- show_faults(
        run_classification(sim = simulations(), inputs = input, session = session)
      )
      if(input$use_h2o){
        output$acc_plot <- renderPlot(
          rv$classification$acc_plot
        )
      }else{
        output$importance_plot <- renderPlot({
          MSstatsSampleSize::designSampleSizeClassificationPlots(data = rv$classification$res,
                                                                 rv$classification$samp,
                                                                 predictive_accuracy_plot = F,
                                                                 address = F)
        })
        output$acc_plot <- renderPlot({
          MSstatsSampleSize::designSampleSizeClassificationPlots(data = rv$classification$res,
                                                                 rv$classification$samp,
                                                                 protein_importance_plot = F,
                                                                 address = F)
        })
      }
    },message = "Progress:", value = 0.2, detail = "Training"
    )
  })
  
  output$importance_plot <- renderPlot({
    validate(need(!is.null(rv$classification$models), "No Trained Models Found"))
    plot_var_imp(data = rv$classification$models, 
                 sample = input$s_size)
  })
  
  
  output$download_prot_imp <- downloadHandler(
    filename = sprintf("Protein_Importance_plots_%s.pdf",
                       format(Sys.time(), "%Y%m%d%H%M%S")),
    content = function(file){
      plots <- plot_var_imp(data = rv$classification$models, sample = 'all',
                            prots = nrow(rv$classification$models[[1]]$var_imp))
      withProgress({
        pdf(file = file, height = 10, width = 8)
        for(i in seq_along(plots)){
          status(sprintf("Plottint %s plot", i), value = i/length(plots),
                 session = session)
          print(plots[[i]])
        }
        dev.off()
      }, session = session)
    }
  )
  
  
  observeEvent(input$download_models, {
    fileName <- sprintf("Models_%s_%s.rds", input$classifier, format(Sys.time(),"%Y%m%d%H%M%S"))
    saveRDS(classification(), fileName)
    showNotification(sprintf("File Downloaded at %s --- File Name %s", getwd(), fileName),
                     duration = 20, type = 'message')
  })
  
}