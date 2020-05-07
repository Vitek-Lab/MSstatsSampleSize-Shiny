
function(session, input, output) {
  
  # enables the helper functionality in the UI
  shinyhelper::observe_helpers(help_dir = "help_mds")
  
  # stop the h2o cluster once the app is shutdown
  onStop(function() {
    try({
      h2o::h2o.shutdown(prompt = F)
      }, silent = T)
  })
  
  # List of reactive values
  rv <- reactiveValues()
  rv$seed <- -1
  rv$use_h2o <- F
  
  ## Set maximum size of uploaded files to 300mb
  options(shiny.maxRequestSize = 300*1024^2)
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
    withProgress({
      data <- show_faults(
        expr = format_data(format = input$data_format,
                            count = input$standard_count,
                            annot = input$standard_annot,
                            session = session),
        session = session
      )
      # Create a global variable for dynamic ui element
      B_GROUP <<- data$annot_data[,unique(Condition)] 
      # update the slider input with the maximum number of proteins found in the dataset
      updateSliderInput(session = session, inputId = "prot_num",
                        min = 1, max = data$n_prot, value = data$n_prot)
      updateSelectInput(session = session, inputId = "b_group",
                        choices = B_GROUP)
      },
    message = "Progress:", value = 0.2, detail = "Loading Data...")
    shinyjs::enable("simulate")
    return(data)
  })
  
  #### Visualize EDA ####
  output$dataset_name <- renderText(
    paste("Dataset Filename:",data()$dataset_name)
  )
  # Condition Summary Table
  output$cond_sum_table <- DT::renderDataTable({
    validate(need(!is.null(data()), "Data Formatting in Progress"))
    validate(need(!is.null(data()$cond_sum_table), "Data Not Imported"))
    DT::datatable(data()$cond_sum_table,
                  options = list(dom = 't', autoWidth = TRUE,
                                 selection = 'none'))
  })
  # Data Summary Table
  output$sum_table <- DT::renderDataTable(
    DT::datatable(data()$sum_table,
                  options = list(dom = 't', autoWidth = TRUE,
                                 selection = 'none'))
  )
  # Boxplot for Proteins
  output$global_boxplot <- plotly::renderPlotly({
    shiny::validate(shiny::need(data()$box_plot, 'No Data Found to visualize'))
    data()$box_plot
  })
  # Mean and Standard Deviation Plot
  output$mean_sd_plot <- renderPlot({
    meanSDplot(data = data()$est_var)
  })
  
  observeEvent(input$set_seed,{
    if(input$set_seed == T){
      rv$seed <- 1212
    }else{
      rm(.Random.seed, envir = globalenv())
      rv$seed <- -1
    }
  }, ignoreInit = F)
  #switches to the data exploration tabs which are populated with the EDA
  observeEvent(input$import_data,{
    updateTabsetPanel(session = session, "myNavBar", selected = "Explore Data")
  })
  #### Toggle control for simulate data tab ####
  # toggle the elements that are needed when default fold change is not selected
  observeEvent(input$exp_fc,{
    shinyjs::toggleElement(id = "diff_prot",
                           condition = input$exp_fc != T)
    shinyjs::toggleElement(id = "diff_prot_help",
                           condition = input$exp_fc != T)
    shinyjs::toggleElement(id = "b_group",
                           condition = input$exp_fc != T)
    shinyjs::toggleElement(id = "b_group_help",
                           condition = input$exp_fc != T)
    shinyjs::toggleElement(id = "fc_values", 
                           condition  = input$exp_fc != T)
    shinyjs::toggleElement(id = "fc_values_help", 
                           condition = input$exp_fc != T)
  })
  
  # toggle between number and proportion of the proteins
  observeEvent(input$sel_sim_prot,{
    shinyjs::toggleElement(id = "prot_prop",
                           condition = input$sel_sim_prot == "Proportion")
    shinyjs::toggleElement(id = "prot_num",
                           condition = input$sel_sim_prot != "Proportion")
  })
  
  # Fold Change Editable table #
  # render the fold change datatable, and update the baseline groups as selected
  # from the drop down menu provide in the UI
  observeEvent(input$b_group,{
    validate(need(input$b_group, "No Baseline Group"))
    choices <- B_GROUP[!B_GROUP %in% input$b_group]
    
    group <- sprintf("%s - %s", choices, input$b_group)
    #create a global table
    fc_values <<- data.table(Group = group,
                             `Fold Change Value` = rep(NA, length(group)-1),
                             orig_group = as.character(choices))
    # render editable table to ui
    output$fc_values <- DT::renderDT(fc_values, rownames = F,
                                     options = list(dom = 't',
                                                    columnDefs = list(list(targets = c(2),
                                                           visible = F))),
                                     selection = 'none',
                                     editable = list(target = 'cell',
                                                     disable = list(columns = 0)),
                                     class = 'cell-border stripe')
  })
  
  proxy = DT::dataTableProxy('fc_values')
  
  # Record the changes to the cell and update the table
  observeEvent(input$fc_values_cell_edit,{
    info <- input$fc_values_cell_edit
    info$col <- 2 # hardcoded, since renderDT take column indexed 0 to N
    info$value <- as.numeric(info$value)
    
    if(is.na(info$value)){
      er <- sprintf("Only Numeric Values accepted as Fold Change values, found: %s",
                    info$value)
      showNotification(er, duration = 15, id = "error",
                       type = "error", session = session)
      validate(need(info$value, "Only Numeric Values accepted"))
    }
    
    if(info$value <= 1){
      showNotification("Please enter values greater than baseline group", duration = 15,
                       type = "error", id = "error", session = session)
      validate(need(as.numeric(info$value) > 1,
                    "Please enter values greater than baseline group, i.e values > 1"))
    }
    fc_values <<- DT::editData(fc_values,info)
    DT::replaceData(proxy, fc_values, resetPaging = F, rownames = F)  # important
  })
  
  # toggle the validation input ui 
  observeEvent(input$sim_val,{
    shinyjs::toggleElement(id = "n_val_samp_grp",
                           condition = input$sim_val == T)
    shinyjs::toggleElement(id = "n_val_samp_grp_help",
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
    #validate(need(nrow(data()$wide_data) != 0, "Import Data using the Import Data Menu"))
    withProgress({
      exp_fc <- ifelse(input$exp_fc, 'data', '')
      if(exp_fc == ''){
        baseline <- data.table(Group = input$b_group,
                               `Fold Change Value` = 1,
                               orig_group = input$b_group)
        exp_fc <- rbind(baseline, fc_values)
      }
      data <- show_faults({
        simulate_grid(data = data()$wide_data,
                      annot = data()$annot_data,
                      num_simulation = input$n_sim,
                      exp_fc = exp_fc,
                      list_diff_proteins = input$diff_prot,
                      sel_simulated_proteins = tolower(input$sel_sim_prot),
                      prot_proportion = input$prot_prop,
                      prot_number = input$prot_num,
                      samples_per_group = input$n_samp_grp,
                      sim_valid = input$sim_val,
                      valid_samples_per_grp = input$n_val_samp_grp,
                      seed = rv$seed,
                      session = session)
        },session = session
      )
      },
      message = "Progress:", value = 0.2, detail = "Simulating Data...")
    shinyjs::enable("run_model")
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
    shinyjs::enable(id = "fwd")#, condition = length(sim_choices) > 1)
    shinyjs::enable(id = "back")#, condition = length(sim_choices) > 1)
    shinyjs::enable(id = "download_pca")#, condition = length(sim_choices) > 1)
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
    if(curr >= 1 && curr <= length(sim_choices)-1){
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
      validate(need(nchar(input$simulations) != 0, 'No Simulations Run yet'))
      vals <- unlist(stringr::str_extract_all(input$simulations,'\\d+'))
      sim <- sprintf("simulation%s",vals[1])
      show_faults({
        make_pca_plots(simulations()[[vals[2]]], which = sim)},
        session = session)
    }else{
      shiny::showNotification("No Simulations Found", duration = NULL)
    }
  )
 
  
  #### Toggle switches and control for selectInputs in Analyze Tab ####
  observeEvent(input$n_samp_grp,{
    vals <- unlist(strsplit(input$n_samp_grp, ","))
    samp_size <<- sprintf("Sample%s", vals)
    updateSelectInput(session = session, inputId = "s_size", label = "Sample Size",
                      choices = samp_size)
  })
  
  ##### Toggle switches based on input classifier for H2o #####
  #TODO make this chunk simplier
  observeEvent(input$checkbox_inputs, {
    rv$use_h2o <- ifelse(TUNING[1] %in% input$checkbox_inputs, T, F)
    shinyjs::toggleElement(id = "stop_metric",
                           condition = (all(TUNING %in% input$checkbox_inputs) &&
                                          input$classifier %in% c("rf","logreg")))
    shinyjs::toggleElement(id = "stop_metric_help",
                           condition = (all(TUNING %in% input$checkbox_inputs) &&
                                          input$classifier %in% c("rf","logreg")))
    
    shinyjs::toggleElement(id = "nfolds",
                           condition = (all(TUNING %in% input$checkbox_inputs) &&
                                          input$classifier == "rf"))
    shinyjs::toggleElement(id = "nfolds_help",
                           condition = (all(TUNING %in% input$checkbox_inputs) &&
                                          input$classifier == "rf"))
    
    shinyjs::toggleElement(id = "f_assignment",
                           condition = (all(TUNING %in% input$checkbox_inputs) &&
                                          input$classifier %in% c("rf", "naive_bayes")))
    shinyjs::toggleElement(id = "f_assignment_help",
                           condition = (all(TUNING %in% input$checkbox_inputs) &&
                                         input$classifier %in% c("rf", "naive_bayes")))
    
    shinyjs::toggleElement(id = "iters",
                           condition = (all(TUNING %in% input$checkbox_inputs) &&
                                          input$classifier == "svmLinear"))
    shinyjs::toggleElement(id = "iters_help", 
                           condition = (all(TUNING %in% input$checkbox_inputs) &&
                                          input$classifier == "svmLinear"))
    
    shinyjs::toggleElement(id = "link",
                           condition = (all(TUNING %in% input$checkbox_inputs) &&
                                          input$classifier == "logreg"))
    shinyjs::toggleElement(id = "link_help",
                           condition = (all(TUNING %in% input$checkbox_inputs) &&
                                          input$classifier == "logreg"))
    
    shinyjs::toggleElement(id = "family",
                           condition = (all(TUNING %in% input$checkbox_inputs) &&
                                          input$classifier == "logreg"))
    shinyjs::toggleElement(id = "family_help",
                           condition = (all(TUNING %in% input$checkbox_inputs) &&
                                          input$classifier == "logreg"))
    
    shinyjs::toggleElement(id = "solver",
                           condition = (all(TUNING %in% input$checkbox_inputs) &&
                                          input$classifier == "logreg"))
    shinyjs::toggleElement(id = "solver_help",
                           condition = (all(TUNING %in% input$checkbox_inputs) &&
                                          input$classifier == "logreg"))
    
    shinyjs::toggleElement(id = "laplace",
                           condition = (all(TUNING %in% input$checkbox_inputs) &&
                                          input$classifier == "naive_bayes"))
    shinyjs::toggleElement(id = "laplace_help",
                           condition = (all(TUNING %in% input$checkbox_inputs) &&
                                          input$classifier == "naive_bayes"))
    
    shinyjs::toggleElement(id = "eps_sdev",
                           condition = (all(TUNING %in% input$checkbox_inputs) &&
                                          input$classifier == "naive_bayes"))
    shinyjs::toggleElement(id = "eps_sdev_help",
                           condition = (all(TUNING %in% input$checkbox_inputs) &&
                                          input$classifier == "naive_bayes"))
    
    shinyjs::toggleElement(id = "min_sdev",
                           condition = (all(TUNING %in% input$checkbox_inputs) &&
                                          input$classifier == "naive_bayes"))
    shinyjs::toggleElement(id = "min_sdev_help",
                           condition = (all(TUNING %in% input$checkbox_inputs) &&
                                          input$classifier == "naive_bayes"))
    
    shinyjs::toggleElement(id = "caret_rf",
                           condition = (TUNING[2] == input$checkbox_inputs &&
                                          TUNING[1] !=  input$checkbox_inputs &&
                                          input$classifier == 'rf'))
    shinyjs::toggleElement(id = "caret_rf_help",
                           condition = (TUNING[2] == input$checkbox_inputs &&
                                          TUNING[1] !=  input$checkbox_inputs &&
                                          input$classifier == 'rf'))
  }, ignoreNULL = FALSE)
  
  observeEvent(input$classifier, {
    shinyjs::toggleElement(id = "stop_metric",
                           condition = (all(TUNING %in% input$checkbox_inputs) &&
                                          input$classifier %in% c("rf","logreg")))
    shinyjs::toggleElement(id = "nfolds",
                           condition = (all(TUNING %in% input$checkbox_inputs) &&
                                          input$classifier == "rf"))
    shinyjs::toggleElement(id = "f_assignment",
                           condition = (all(TUNING %in% input$checkbox_inputs) &&
                                          input$classifier %in% c("rf", "naive_bayes")))
    shinyjs::toggleElement(id = "iters",
                           condition = (all(TUNING %in% input$checkbox_inputs) &&
                                          input$classifier == "svmLinear"))
    shinyjs::toggleElement(id = "link",
                           condition = (all(TUNING %in% input$checkbox_inputs) &&
                                          input$classifier == "logreg"))
    shinyjs::toggleElement(id = "family",
                           condition = (all(TUNING %in% input$checkbox_inputs) &&
                                          input$classifier == "logreg"))
    shinyjs::toggleElement(id = "solver",
                           condition = (all(TUNING %in% input$checkbox_inputs) &&
                                          input$classifier == "logreg"))
    shinyjs::toggleElement(id = "laplace",
                           condition = (all(TUNING %in% input$checkbox_inputs) &&
                                          input$classifier == "naive_bayes"))
    shinyjs::toggleElement(id = "eps_sdev",
                           condition = (all(TUNING %in% input$checkbox_inputs) &&
                                          input$classifier == "naive_bayes"))
    shinyjs::toggleElement(id = "min_sdev",
                           condition = (all(TUNING %in% input$checkbox_inputs) &&
                                          input$classifier == "naive_bayes"))
  })
  #### Run Classification #####
  observeEvent(input$run_model,{
    withProgress({
      st <- Sys.time()
      message(sprintf("Start Time: %s", Sys.time()))
      rv$classification <- show_faults(
        run_classification(sim = simulations(), inputs = input, seed = rv$seed,
                           use_h2o = rv$use_h2o, session = session),
        session = session
      )
    et <- Sys.time()
    message(sprintf("End Time: %s", Sys.time()))
    output$default <- renderText({ 
      sprintf("Classification Completed in %s minutes",
              round(difftime(et, st, units = 'min'), digits = 2))
    })
    
    },message = "Progress:", value = 0.2, detail = "Training"
    )
    shinyjs::enable("download_models")
    shinyjs::enable("download_prot_imp")
    shinyjs::enable("back_varimp")
    shinyjs::enable("fwd_varimp")
  })
  
  
  observeEvent(input$back_varimp, {
    curr <- which(samp_size == input$s_size)
    if(curr > 1){
      updateSelectInput(session = session, "s_size",
                        choices = samp_size, selected = samp_size[curr - 1])
    }
  })
  
  observeEvent(input$fwd_varimp,{
    curr <- which(samp_size == input$s_size)
    if(curr >= 1){
      updateSelectInput(session = session, "s_size",
                        choices = samp_size, selected = samp_size[curr + 1])
    }
  })
  
  
  ##### Render Model training plots ####
  output$acc_plot <- renderPlot({
    shiny::validate(shiny::need(rv$classification, "No Trained Models Found"))
    if(rv$use_h2o)
      shiny::validate(shiny::need(rv$classification$models, "No Trained Models Found"))
    show_faults(plot_acc(data = rv$classification, use_h2o = rv$use_h2o,
                         alg = names(MODELS)[which(MODELS %in% input$classifier)]),
                session = session)
  })
  
  output$importance_plot <- renderPlot({
    shiny::validate(shiny::need(rv$classification, "No Trained Models Found"))
    if(rv$use_h2o)
      shiny::validate(shiny::need(rv$classification$models, "No Trained Models Found"))
    
    show_faults(plot_var_imp(data = rv$classification, sample = input$s_size,
                             alg = input$classifier,use_h2o = rv$use_h2o),
                session = session)

  })
  
  #### Download buttons for models plots/and data #####
  output$download_plots <- downloadHandler(
    filename = sprintf("plots_%s.pdf",
                       format(Sys.time(), "%Y%m%d%H%M%S")),
    content = function(file){
      plots <- plot_var_imp(data = rv$classification, sample = 'all',
                            use_h2o = rv$use_h2o, alg = input$classifier,
                            prots = 'all')
      withProgress({
        pdf(file = file, height = 9, width = 6.5)
        print(plot_acc(data = rv$classification, use_h2o = rv$use_h2o,
                       alg = names(MODELS)[which(MODELS %in% input$classifier)]))
        for(i in seq_along(plots)){
          status(sprintf("Plotting %s plot", i), value = i/length(plots),
                 session = session)
          print(plots[[i]])
        }
        dev.off()
      }, session = session)
    }
  )
  
  
  output$download_models <- downloadHandler(
    filename =  sprintf("Models_%s_%s.rds", input$classifier,
                        format(Sys.time(), "%Y%m%d%H%M%S")),
    content = function(file){
      saveRDS(rv$classification, file = file)
    }
  )
  
  observeEvent(input$debug,{
    rdsList <- list(cl = rv$classification)
    saveRDS(rdsList, 'test.rds')
  })
}