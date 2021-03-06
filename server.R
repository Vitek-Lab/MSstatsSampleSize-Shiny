
function(session, input, output) {
  
  # enables the helper functionality in the UI
  shinyhelper::observe_helpers(help_dir = "help_mds")
  
  # stop the h2o cluster once the app is shutdown
  onStop(function() {
    try({
      h2o::h2o.shutdown(prompt = F)
      }, silent = T)
    status("Shutting Down Application")
    close(FILE_CONN)
    stopApp()
  })
  
  # List of reactive values
  rv <- reactiveValues()
  rv$seed <- -1
  rv$use_h2o <- F
  
  observeEvent(input$start_process,{
    updateTabsetPanel(session = session, inputId = "tabs", "import_data")
  })
  ## Set maximum size of uploaded files to 300mb
  options(shiny.maxRequestSize = 300*1024^2)
  #### Toggle control for sidebar ####
  # Enable or disable fileInputs based on type of data selected
  observeEvent(input$data_format,{
    shinyjs::toggleElement(id = "standard_count", anim = T, animType = "fade",
                           condition = input$data_format == "standard")
    shinyjs::toggleElement(id = "standard_annot", anim = T, animType = "fade",
                           condition = input$data_format == "standard")
  })

  #### Import data, action click ####
  data <- eventReactive(input$import_data, {
    SIM_CHOICES <<-0
    rv$seed <- -1
    rv$use_h2o <- F
    rv$classification <- NULL
    
    disable_btns <- c("download_plots", "back_varimp", "fwd_varimp", 
                      "generate_report", "run_model","nav_to_exp",
                      "fwd","back","download_pca", "run_model")
    lapply(disable_btns, shinyjs::disable)
    updateSelectInput(session = session, inputId ="simulations", label = "Simulations",
                      choices = SIM_CHOICES)
    output$default <- renderText("")
    
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
  
  observeEvent(input$nav_to_sim,{
    updateTabItems(session = session, inputId = "tabs", "explore_simulated")
  })
  
  #switches to the data exploration tabs which are populated with the EDA
  observeEvent(input$import_data,{
    shiny::need(!is.null(data()), "Data Processing")
    updateTabsetPanel(session = session, "myNavBar", selected = "Explore Data")
  })
  #### Toggle control for simulate data tab ####
  # toggle the elements that are needed when default fold change is not selected
  observeEvent(input$set_seed,{
    shinyjs::toggleElement(id = "seed")
  })
  
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
    withProgress({
      exp_fc <- ifelse(input$exp_fc, 'data', '')
      if(exp_fc == ''){
        baseline <- data.table(Group = input$b_group,
                               `Fold Change Value` = 1,
                               orig_group = input$b_group)
        exp_fc <- rbind(baseline, fc_values)
      }
      
      if(input$set_seed){
        rv$seed <- input$seed
      }
      data <- show_faults({
        simulate_grid(data = data()$wide_data,
                      annot = data()$annot_data,
                      est_var = data()$est_var,
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
    
    enable_btns <- c("run_model","nav_to_exp","fwd","back","download_pca",
                     "run_model")
    lapply(enable_btns, shinyjs::enable)
    
    return(data)
  })
  
  #### Toggle Switch for previous/next/download buttons, updates select input ####
  observeEvent(input$simulate, {
    rv$classification <- NULL
    disable_btns <- c("download_plots", "back_varimp", "fwd_varimp", 
                      "generate_report", "run_model")
    lapply(disable_btns, shinyjs::disable)
    
    validate(need(nrow(data()$wide_data) != 0, "Import Data using the Import Data Menu"))
    sc <- sprintf("Simulation %s", seq(1, input$n_sim))
    SIM_CHOICES <<- do.call('c',lapply(sc, function(x){
      sprintf("%s Sample Size %s", x,
              as.numeric(unlist(strsplit(input$n_samp_grp, ','))))
      }))
    updateSelectInput(session = session, inputId ="simulations", label = "Simulations",
                      choices = SIM_CHOICES)
  })
  
  #### Backend for previous and next buttons ####
  observeEvent(input$back, {
    curr <- which(SIM_CHOICES == input$simulations)
    if(curr > 1){
      updateSelectInput(session = session, "simulations",
                        choices = SIM_CHOICES, selected = SIM_CHOICES[curr - 1])
    }
  })
  
  observeEvent(input$fwd,{
    curr <- which(SIM_CHOICES == input$simulations)
    if(curr >= 1 && curr <= length(SIM_CHOICES)-1){
      updateSelectInput(session = session, "simulations",
                        choices = SIM_CHOICES, selected = SIM_CHOICES[curr + 1])
    }
  })
  
  # observeEvent(input$debug,{
  #   saveRDS(list(sim = simulations(), choices = SIM_CHOICES),"debug.rds")
  # })
  
  #### Backend for download all plots ####
  output$download_pca <- downloadHandler(
    filename = sprintf("PCA_plots_%s.pdf", format(Sys.time(), "%Y%m%d%H%M%S")),
    content = function(file){
      withProgress({
        p <- list()
        library(gridExtra)
        pdf(file = file)
        for(i in seq_along(SIM_CHOICES)){
          status(sprintf("Plotting %s plot", i), value = i/length(SIM_CHOICES), session = session)
          vals <- unlist(stringr::str_extract_all(SIM_CHOICES[i],'\\d+'))
          sim <- sprintf("simulation%s",vals[1])
          p <- append(p, list(make_pca_plots(simulations()[[vals[2]]], 
                                             which = sim, dot_size=1,
                                             title = SIM_CHOICES[i],
                                             x.axis.size = 4, y.axis.size = 4,
                                             margin = 0.2, legend.size = 7,
                                             download = T)
                              )
                      )
          
          if(length(p)== 4 || i == max(seq_along(SIM_CHOICES))){
            do.call("grid.arrange", c(p, ncol=2, nrow=2))
            p <- list()
          }
        }
        dev.off()
      }
      , session = session)
    }
  )
  
  #### Render PCA plots for selected simulations ####
  output$pca_plot <- renderPlot(
    if(!is.null(input$simulations)){
      validate(need(SIM_CHOICES !=0, 'No Simulations run yet'))
      vals <- unlist(stringr::str_extract_all(input$simulations,'\\d+'))
      sim <- sprintf("simulation%s",vals[1])
      show_faults({
        make_pca_plots(simulations()[[vals[2]]], which = sim, download=F)},
        session = session)
    }else{
      shiny::showNotification("No Simulations Found", duration = NULL)
    }
  )

  observeEvent(input$nav_to_exp,{
    updateTabItems(session = session, inputId = "tabs", "plan_experiment")
  })
  
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
                                          input$classifier == 'rf' &&
                                          !is.null(input$checkbox_inputs)))
    shinyjs::toggleElement(id = "caret_rf_help",
                           condition = (TUNING[2] == input$checkbox_inputs &&
                                          TUNING[1] !=  input$checkbox_inputs &&
                                          input$classifier == 'rf' &&
                                          !is.null(input$checkbox_inputs)))
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
    
    shinyjs::toggleElement(id = "caret_rf",
                           condition = (TUNING[2] == input$checkbox_inputs &&
                                          TUNING[1] !=  input$checkbox_inputs &&
                                          input$classifier == 'rf' &&
                                          !is.null(input$checkbox_inputs)))
    shinyjs::toggleElement(id = "caret_rf_help",
                           condition = (TUNING[2] == input$checkbox_inputs &&
                                          TUNING[1] !=  input$checkbox_inputs &&
                                          input$classifier == 'rf' &&
                                          !is.null(input$checkbox_inputs)))
    
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
    CURRMODEL <<- input$classifier
    enable_btns <- c("download_plots", "back_varimp", "fwd_varimp", 
                     "generate_report")
    lapply(enable_btns, shinyjs::enable)
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
    validate(need(SIM_CHOICES != 0, "No Simulations run yet, Click 'Simulate'"),
             need(rv$classification,
                  "No Trained Models Found, Click 'Run Model'"),
             need(CURRMODEL == input$classifier,
                  sprintf("No Trained Models Found for %s, Click 'Run Model'", 
                          input$classifier)))
    if(rv$use_h2o)
      validate(need(SIM_CHOICES !=0, "No Simulations run yet, Click 'Simulate'"),
               need(rv$classification$models, "No Trained Models Found"))
    show_faults(plot_acc(data = rv$classification, use_h2o = rv$use_h2o,
                         alg = names(MODELS)[which(MODELS %in% input$classifier)],
                         session = session),
                session = session)
  })
  
  output$importance_plot <- renderPlot({
    validate(need(!is.null(rv$classification), "No Trained Models Found"),
             need(CURRMODEL == input$classifier, "No Trained Models Found"))
    if(rv$use_h2o)
      validate(need(!is.null(rv$classification$models), "No Trained Models Found"),
               need(CURRMODEL == input$classifier, "No Trained Models Found"))
    
    show_faults(plot_var_imp(data = rv$classification, sample = input$s_size,
                             alg = input$classifier,use_h2o = rv$use_h2o),
                session = session)

  })
  
  #### Download buttons for models plots/and data #####
  output$download_plots <- downloadHandler(
    filename = sprintf("classification_plot_%s.pdf",
                       format(Sys.time(), "%Y%m%d%H%M%S")),
    content = function(file){
      plots <-list(plot_acc(data = rv$classification, use_h2o = rv$use_h2o,
                            alg = names(MODELS)[which(MODELS %in% input$classifier)],
                            x.axis.size = 4, y.axis.size = 4,margin = 0.5))
      plots <- append(plots, plot_var_imp(data = rv$classification, sample = 'all',
                                          use_h2o = rv$use_h2o, alg = input$classifier,
                                          prots = 'all', x.axis.size = 4, 
                                          y.axis.size = 5,margin = 0.5))
      
      seqs <- seq(4,length(plots), 4)
      library(gridExtra)
      withProgress({
        pdf(file = file)
        for(i in seqs){
          status(sprintf("Plotting %s plot", i), value = i/length(plots),
                 session = session)
          do.call("grid.arrange", c(plots[(i-3):i], ncol=2, nrow=2))
        }
        dev.off()
      }, session = session)
    }
  )
  
  
  output$generate_report <- downloadHandler(
    filename =  sprintf("Report_%s.pdf", format(Sys.time(), "%Y%m%d%H%M%S")),
    content = function(file){
      tempReport <- file.path(tempdir(), "Report.Rmd")
      file.copy("Report.Rmd", tempReport, overwrite = T)
      
      params <- list(data = rv$classification, use_h2o= rv$use_h2o,
                     alg = names(MODELS)[which(MODELS %in% input$classifier)],
                     sample = input$s_size,
                     annot = ifelse(is.null(input$standard_annot$datapath),
                                    "MSstatsSampleSize Package",
                                    input$standard_annot$datapath),
                     count = ifelse(is.null(input$standard_count$datapath),
                                    "MSstatsSampleSize Package",
                                    input$standard_count$datapath),
                     n_sim = input$n_sim,
                     fc = input$exp_fc,
                     sim_by = input$sel_sim_prot,
                     fc_values = input$fc_values,
                     baseline = input$b_group,
                     list_diff_prots = input$diff_prot,
                     set_seed = input$set_seed,
                     seed = input$seed,
                     proportion = input$prot_prop,
                     number = input$prot_num,
                     valid = input$sim_val,
                     valid_sample = input$n_val_samp_grp)
      
      rmarkdown::render(tempReport, output_file = file, params = params,
                        envir = new.env(parent = globalenv()))
    }
  )
}