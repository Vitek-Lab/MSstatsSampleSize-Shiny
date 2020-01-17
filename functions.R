#### Utility Wrappers ####

#' @title Show faults of an expression interactively
#' @description Evaluates the given expression, function and its arguements
#' return the output of the expression. If any errors/warnings are found a message
#' is logged in the console. If used in a shiny App a notificatio is shown, for any
#' errors process from the shiny app is stopped. Warning are shown and the shiny
#' process continues as developed.
#' @param ... Arguements passed to conditions, the expression to execure
#' @param session A shiny object of shiny web app, defaults to NULL
#' @return A return object of type as specified by the expression being executed
#' @references https://github.com/SumedhSankhe/workBench/blob/fd5e808e86e849a933a924735d30348974212fc3/functions.R#L1-L37
show_faults <- function(..., session = NULL){
  warn <- err <- NULL
  res <- withCallingHandlers(tryCatch(..., error = function(e) {
    err <<- conditionMessage(e)
    NULL
  }), warning = function(w) {
    warn <<- append(warn, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  
  if(!is.null(session) & !is.null(err)){
    shiny::showNotification(as.character(err), duration = 20, type = 'error',
                            session = session)
    shiny::validate(shiny::need(is.null(err), as.character(err)))
  } else if(is.null(session) & !is.null(err)) {
    stop(Sys.time(),": ",err)
  } else if (!is.null(session) & !is.null(warn)){
    shiny::showNotification(as.character(err), duration = 20, type = 'Warning',
                            session = session)
  } else if(is.null(session) & !is.null(warn)) {
    warning(Sys.time(),": ",warn)
    return(res)
  } else {
    return(res)
  }
}


#' @title Display Status messages to the console of the shiny-session
#' @description A utility function which uses the native R and shiny reporting functions
#' to display status of a particular process to the console or the shiny message
#' bar with progress
#' @param detail A character string of message to be displayed inthe console/shiny progress
#' @param value A numeric value 0-1 which depicts the progress of the task at hand
#' @param session A shiny session object
#' @references https://github.com/SumedhSankhe/workBench/blob/fd5e808e86e849a933a924735d30348974212fc3/functions.R#L51
status <- function(detail, value, session = NULL){
  if(!is.null(session))
    shiny::setProgress(value = value, message = "Progress:", detail = detail,
                       session = session)
  message(Sys.time(),": ",detail,"...")
}

#### Data Exploration ####

explore_data <- function(format, count = NULL, annot = NULL, session = NULL){
  
  formatted_data <- show_faults(
    format_data(format = format, count = count, annot = annot, session = session),
    session = session
  )
  
  plots <- show_faults(
    generate_plots_explore(formatted_data = formatted_data, session = session),
    session = session
  )
  
  sum_table <- data.frame(Parameter =  c("Number of Proteins", "Number of Groups"),
                          Values = c(formatted_data$n_prot,
                                     formatted_data$n_group))
  cond_sum_table <- show_faults(
    expr = format_summary_table(data = formatted_data$long_data), session = session
  )
  data <- append(formatted_data, plots)
  data <- append(data, list('sum_table' = sum_table, 'cond_sum_table' = cond_sum_table))
  return(data)
}

#' @title Formats data to required longer format
#' @description 
#' @param  format A decided which data to use, user provided or default examples
#' @param count A path to the data to be read in, any csv formats are acceptable
#' @param annot An annotation file path as csv
#' @param session A shiny session variable to make the notifications interactive
#' @return A named list of the data an other objects as required
format_data <- function(format, count = NULL, annot = NULL, session = NULL){
  
  shiny::validate(shiny::need(format %in% FORMATS, 'Undefined Format'))
  
  if(format == 'standard'){
    status(detail = 'Importing Protein Abundance file', value = 0.4,
           session = session)
    wide <- fread(count$datapath, keepLeadingZeros = T)
    setnames(wide, 'V1', 'Protein')
    status(detail = 'Importing Annotations file', value = 0.5,
           session = session)
    annot <- fread(annot$datapath, keepLeadingZeros = T)
    name <- count$name
  }else if(format == 'examples'){
    status(detail = 'Importing Data from MSstatsSampleSize Package', value = 0.5,
           session = session)
    
    wide <- as.data.table(MSstatsSampleSize::OV_SRM_train,
                                      keep.rownames = T)
    annot <- as.data.table(MSstatsSampleSize::OV_SRM_train_annotation)
    setnames(wide,'rn','Protein')
    name <- "Ovarian Cancer SRM study"
  }else{
    stop('Not Defined')
  }
  
  status(detail = 'Stretching the data', value = 0.6, session = session)
  data <- tidyr::gather(wide, 'BioReplicate', 'Abundance', 2:ncol(wide))
  
  status(detail = 'Merging Abundance & Spectral Counts with Annotations', 
         value = 0.7, session = session)
  data <- merge(as.data.table(data), annot, by = 'BioReplicate')
  
  if(!is.null(session)){
    shiny::showNotification('Data Import Completed', duration = 5, 
                            type = 'message')
  }
  
  return(list('long_data' = data, 'wide_data' = wide, 'annot_data' = annot,
              'n_prot' = nrow(wide[,.N, Protein]), 
              'n_group' = nrow(annot[,.N,Condition]),
              'is_imported' = T, 'dataset_name' = name))
}


generate_plots_explore <- function(formatted_data = NULL, session = NULL){
  shiny::validate(shiny::need(formatted_data, 'No Data Found to visualize'))
  status(detail = 'Creating Boxplot for Protein Abundace', value = 0.8,
         session = session)
  
  box_plot <- plotly::plot_ly(data = formatted_data$long_data[!is.na(Abundance)],
                       y = ~log(Abundance), x = ~BioReplicate, color = ~Condition,
                       type = "box") %>%
    plotly::layout(xaxis = list(title="Biological Replicate"), 
                   yaxis = list(title="Log Protein abundance"))
  
  status(detail = 'Estimating Variance', value = 0.85, session = session)
  est_var <- MSstatsSampleSize::estimateVar(data = formatted_data$wide_data,
                                            annot = formatted_data$annot_data)
  
  status(detail = 'Creating Mean/SD plots', value = 0.95, session = session)
  meansd_plot <- meanSDplot(data = est_var)
  
  return(list('boxplot' = box_plot, 'meanSDplot' = meansd_plot))
}


format_summary_table <- function(data = NULL){
  shiny::validate(need(data, 'No Data Provided'))
  
  msruns <- unique(data[,.(BioReplicate, Condition)]) # Placeholder
  msruns <- xtabs(~Condition, data = msruns) # Placeholder
  
  biorep <- unique(data[,.(BioReplicate, Condition)])
  biorep <- xtabs(~Condition, data = biorep)
  
  summary <- rbind(msruns, biorep)
  rownames(summary) <- c("# of MS runs","# of Biological Replicates")
  return(summary)
}



meanSDplot <- function (data, x.axis.size = 10, y.axis.size = 10, smoother_size = 1, 
                        xlimUp = 30, ylimUp = 3){
  
  plotdata <- data.table(mean = as.vector(data$mu), sd = as.vector(data$sigma))
  plot.lowess <- lowess(cbind(plotdata$mean, plotdata$sd))
  plot.lowess <- data.table(x = plot.lowess$x, y = plot.lowess$y)
  meansdplot <- ggplot(data = plotdata, aes(mean, sd)) +
    stat_density2d(aes(fill = ..density..^0.25), geom = "tile", contour = FALSE,
                   n = xlimUp * 10) +
    scale_fill_continuous(low = "white", high = "#0072B2") +
    geom_point(alpha = 0.02, shape = 20) + 
    geom_line(data = plot.lowess, aes(x, y), color = "orange", size = smoother_size) +
    labs(x = "Mean protein abundance per condition", 
         y = "Standard deviation per condition") +
    scale_y_continuous(expand = c(0,0), limits = c(0, ylimUp)) +
    scale_x_continuous(expand = c(0,0), limits = c(0, xlimUp)) +
    theme(panel.background = element_rect(fill = "white", colour = "black"),
          panel.grid.major = element_line(colour = "black"),
          panel.grid.minor = element_blank(),
          strip.background = element_rect(fill = "gray95"),
          strip.text.x = element_text(colour = c("#00B0F6"), size = 14),
          axis.text.x = element_text(size = x.axis.size, colour = "black"),
          axis.text.y = element_text(size = y.axis.size, colour = "black"), 
          axis.ticks = element_line(colour = "black"), 
          axis.title.x = element_text(size = x.axis.size + 5, vjust = -0.4),
          axis.title.y = element_text(size = y.axis.size + 5, vjust = 0.3),
          legend.position = "none")
  return(meansdplot)
}

#### Data Simulation ####

simulate_grid <- function(data = NULL, annot = NULL, num_simulation, exp_fc, fc_name,
                          list_diff_proteins, sel_simulated_proteins, 
                          prot_proportion, prot_number, samples_per_group, sim_valid,
                          valid_samples_per_grp, session = NULL){
  status(detail = "Setting Up Data Simulation Runs", value = 0.1, session = session)
  
  if(exp_fc != 'data'){
    status(detail = "Extracting Fold Change Informations", value = 0.15, session = session)
    diff_prots <- unlist(strsplit(list_diff_proteins, ","))
    exp_fc <- as.numeric(unlist(strsplit(exp_fc,",")))
    names(exp_fc) <- unlist(strsplit(fc_name, ","))
  } else{
    diff_prots <- NULL
  }
  
  status(detail = "Extracting Number of Samples Information", value = 0.2, session = session)
  samp <- unlist(strsplit(samples_per_group, ','))
  
  if(sim_valid){
    status(detail = "Validation Simulation requested", value = 0.2, session = session)
  }
  
  status(detail = "Running Simulation", value = 0.5, session = session)
  
  data_mat <- as.matrix(data[,-1])
  rownames(data_mat) <- data$Protein
  
  sim <- list()
  for(i in samp){
    sim[[i]] <- MSstatsSampleSize::simulateDataset(data = data_mat,
                                                   annotation = annot,
                                                   num_simulations = num_simulation,
                                                   expected_FC = exp_fc,
                                                   list_diff_proteins =  diff_prots,
                                                   select_simulated_proteins = sel_simulated_proteins,
                                                   protein_proportion = prot_proportion,
                                                   protein_number = prot_number,
                                                   samples_per_group = as.numeric(i),
                                                   simulate_valid = as.logical(sim_valid),
                                                   valid_samples_per_group = valid_samples_per_grp)
  }
  
  status(detail = "Simulation Complete", value = 0.9, session = session)
  return(sim)
}

#### Classification #####

sample_size_classification <- function(n_samp, sim_data, classifier, session = NULL){
  samp <- unlist(strsplit(n_samp,','))
  df <- data.table(Parameter = c("Sample Size", "Pred Accuracy", "Feature Importance"),
                   Value = c(NA, NA, NA))
  resi <- f_imp <- pred_acc <- list()
  
  for(i in seq_along(samp)){
    val <- i/length(samp)
    status(detail = sprintf("Classifying Sample Size %s of %s", i, length(samp)),
           session = session, value = val)
    res <- MSstatsSampleSize::designSampleSizeClassification(
      simulations = sim_data[[samp[i]]], classifier = classifier, parallel = F 
    )
    resi[[as.character(samp[i])]] <- res
    f_imp[[as.character(samp[i])]] <- res$feature_importance
    pred_acc[[as.character(samp[i])]] <- res$predictive_accuracy
  }
  
  return(list('res' = resi, 'samp' = as.numeric(samp)))
  }
