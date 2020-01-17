
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
    DT::datatable(data()$cond_sum_table, options = list(dom = 't'),
                  selection = 'none')
  )
  
  output$sum_table <- DT::renderDataTable(
    DT::datatable(data()$sum_table, options = list(dom = 't'),
                  selection = 'none')
  )
  
  output$global_boxplot <- plotly::renderPlotly(
    data()$boxplot
  )

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
  observeEvent(input$download_pca,{
    showNotification(sprintf("Beginning to Plot PCA for %s Simulations",
                             length(sim_choices)), duration = 5, type = 'message',
                     session = session)
    lapply(simulations(), function(sim){
      show_faults(expr = MSstatsSampleSize::designSampleSizePCAplot(sim),
                  session = session)
    })
    
    showNotification(sprintf("Plots Downloaded at: '%s'", getwd()), duration = 10,
                     session = session, type = "message")
  })
  
  #### Render PCA plots for selected simulations ####
  output$pca_plot <- renderPlot(
    if(!is.null(simulations())){
      vals <- unlist(stringr::str_extract_all(input$simulations,'\\d+'))
      p <- sprintf("simulation%s",vals[1])
      show_faults({
        MSstatsSampleSize::designSampleSizePCAplot(simulations()[[vals[2]]],
                                                   which.PCA = p,
                                                   address = F)
      },
        session = session)
    }else{
      h1('No Simulations Found')
    }
  )
 
  
  #### Run Classification #####
  classification <- eventReactive(input$run_model,{
    withProgress(
      classification <- show_faults(
        sample_size_classification(n_samp = input$n_samp_grp,
                                          sim_data = simulations(),
                                          classifier = input$classifier,
                                          session = session),
        session = session
      ),
      message = "Progress:", value = 0.2, detail = "Training")
  })
  
  output$importance_plot <- renderPlot(
    MSstatsSampleSize::designSampleSizeClassificationPlots(data = classification()$res,
                                                           classification()$samp,
                                                           predictive_accuracy_plot = F,
                                                           address = F)
  )
  output$acc_plot <- renderPlot(
    MSstatsSampleSize::designSampleSizeClassificationPlots(data = classification()$res,
                                                           classification()$samp,
                                                           protein_importance_plot = F,
                                                           address = F)
  )
  
}