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
  
  if(!is.null(err)){
    if(!is.null(session)){
      shiny::validate(shiny::need(is.null(err), as.character(err)))
    } else {
      stop(Sys.time(),": ",err)
    }
  } else if (!is.null(warn)){
    warn <- paste(unique(warn), collapse = ", ")
    if(!is.null(session)){
      shiny::showNotification(as.character(warn), duration = 20, type = 'warning',
                              session = session) 
      return(res)
    } else {
      warning(Sys.time(),": ",warn)
      return(res)
    }
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
#' @title Explore Data
#' @description A wrapper function for the explore, plot and summary tables
#' @param format A character vector passed to the `format_data` function 
#' @param count A Protein abundance file passed to the `format_data` function
#' @param annot A annotation file for the protein abundance
#' @param session A shiny session object passed for progress bar
#' @return A list object which contains a named list containing the following
#' information
#' - data = Contains the formatted input files 
#' - plots = Contains the boxplot and meanSD plots 
#' - sum_table = Contains the summary of the data fed
#' - cond_sum_table = Contains the summary table for the conditions in the data fed
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
  data <- append(data, list('sum_table' = as.matrix(sum_table),
                            'cond_sum_table' = cond_sum_table))
  return(data)
}


#' @title Formats data to required longer format
#' @description Formats the input data in the required long and wide format
#' depending on input data
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
              'n_group' = nrow(annot[,.N,Condition]), 'dataset_name' = name))
}

#' @title Generate Exploration Plots and Tables
#' @description This function generate some data exploration visuals, like the
#' boxplot of the different proteins in the dataset and a mean/Standard deviation
#' plot of the given data
#' @param formatted_data A list object as generated by the `format_data` function
#' @param session A shiny session object to make the notifications interactive
#' @return A named list of the boxplot and meanSD plot
generate_plots_explore <- function(formatted_data = NULL, session = NULL){
  shiny::validate(shiny::need(formatted_data, 'No Data Found to visualize'))
  status(detail = 'Creating Boxplot for Protein Abundace', value = 0.8,
         session = session)
  
  box_plot <- plotly::plot_ly(data = formatted_data$long_data[!is.na(Abundance)],
                       y = ~log(Abundance), x = ~BioReplicate, color = ~Condition,
                       type = "box") %>%
    plotly::layout(xaxis = list(title="Biological Replicate"), 
                   yaxis = list(title="Log Protein abundance"),
                   legend = list(orientation = "h",
                                 xanchor = "center",
                                 x = 0.5, y = 1.1)) %>%
    plotly::config(displayModeBar = F)
  
  status(detail = 'Estimating Variance', value = 0.85, session = session)
  est_var <- MSstatsSampleSize::estimateVar(data = formatted_data$wide_data,
                                            annot = formatted_data$annot_data)
  
  status(detail = 'Creating Mean/SD plots', value = 0.95, session = session)
  meansd_plot <- meanSDplot(data = est_var)
  
  return(list('boxplot' = box_plot, 'meanSDplot' = meansd_plot))
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


format_summary_table <- function(data = NULL){
  shiny::validate(need(data, 'No Data Provided'))
  
  msruns <- unique(data[,.(BioReplicate, Condition)]) # Placeholder
  msruns <- xtabs(~Condition, data = msruns) # Placeholder
  
  biorep <- unique(data[,.(BioReplicate, Condition)])
  biorep <- xtabs(~Condition, data = biorep)
  
  summary <- rbind(msruns, biorep)
  rownames(summary) <- c("# of MS runs","# of Biological Replicates")
  return(summary[,which(colSums(summary) > 0)])
}

#' @title Make pca plots
#' @description  A wrapper function which formats all the data to pass to the 
#' `pca_plot()` function
make_pca_plots <- function(simulations, which = 'all', address = NA,
                           width = 3, height = 3, session = NULL){
  
  pc_plot <- list()
  if(which %in% c("all", "allonly")){
    iter <- length(simulations$simulation_train_Xs)+1
    pr_comp <- do_prcomp(sim = simulations)
    pc_plot[[iter]] <- pca_plot(data = pr_comp$pc.result, exp_var = pr_comp$exp.var)
  }
  
  if(grep('simulation', which) | which == 'all'){
    index <- length(simulations$simulation_train_Xs)
    iter <- as.numeric(gsub('[[:alpha:]]','',which))
    iters <- ifelse(!is.na(iter), iter, seq_along(index))
    for(i in iters){
      pr_comp <- do_prcomp(sim = simulations$simulation_train_Xs[[i]],
                           sim_y = simulations$simulation_train_Ys[[i]])
      pc_plot[[i]] <- pca_plot(data = pr_comp$pc.result, exp_var = pr_comp$exp.var)
    }
  }
  
  if(!is.na(address) | which == 'all'){
    address <- ifelse(is.na(address), getwd(), address)
    file_name <- sprintf("%s/PCA_Plot_%s.pdf",address, format(Sys.time(), "%Y%m%d%H%M"))
    pdf(file = file_name, width = width, height = height)
    for(i in seq_along(pc_plot)){
      print(pc_plot)
    }
    dev.off()
  } else{
    return(pc_plot[[iter]])
  }
}

#' @title Plot PCA outputs
#' @description A utility wrapper for plotting pca data as returned by the `do_prcomp()`
#' @param data A data frame containing the Principal components to be plotted
#' @param exp_car A vector containing numeric values of the expected variance
#' @dot_size A aesthetics field to be passed to the ggplot2 functions
pca_plot <- function(data, exp_var, dot_size = 3){
  p <- ggplot(data = data,
              aes(x = PC1, y = PC2, color = group)) +
    geom_point(size = dot_size) + 
    labs(title = "Input dataset",
         x = sprintf("PC1 (%s%% explained var.)",exp_var[1]),
         y = sprintf("PC2 (%s%% explained var.)",exp_var[2])) +
    theme_MSstats()
  return(p)
}


#' @title MSstats Theme
#' @description A utility function that standardized all the ggplot themes
#' @param x.axis.size A numeric value for size for the elements on the x-axis
#' @param y.axis.size A numeric value for size for the elements on the y-axis
#' @param legend.size A numeric value fot the size of the legend
theme_MSstats <- function(x.axis.size = 10, y.axis.size = 10, legend.size = 7){
  
  th <- ggplot2::theme(panel.background = element_rect(fill = "white", 
                                                       colour = "black"),
                       panel.grid.major = element_line(colour = "gray95"), 
                       panel.grid.minor = element_blank(), 
                       strip.background = element_rect(fill = "gray95"), 
                       strip.text.x = element_text(colour = c("#00B0F6"), 
                                                   size = 14),
                       axis.text.x = element_text(size = x.axis.size, 
                                                  colour = "black"), 
                       axis.text.y = element_text(size = y.axis.size, 
                                                  colour = "black"),
                       axis.ticks = element_line(colour = "black"), 
                       axis.title.x = element_text(size = x.axis.size + 5,
                                                   vjust = -0.4), 
                       axis.title.y = element_text(size = y.axis.size + 5,
                                                   vjust = 0.3),
                       title = element_text(size = x.axis.size + 8,
                                            vjust = 1.5),
                       legend.key = element_rect(fill = "white", 
                                                 colour = "white"),
                       legend.position = "right", 
                       legend.text = element_text(size = legend.size), 
                       legend.title = element_blank())
  return(th)
}

#' @title Do Principal Component Analysis
#' @description A wrapper function to the base `prcomp()` formats the results 
#' in a required output format
#' @param sim_x A dataframe of the feature variables
#' @param sim_y A dataframe of the predictor vairable
#' @return A named list of the outputs 
do_prcomp <- function(sim_x, sim_y){
  result.pca <- prcomp(sim_x, scale. = TRUE)
  summary.pca <- summary(result.pca)
  important.pc <- result.pca$x[, 1:2]
  pc.result <- data.frame(important.pc, group = sim_y)
  exp.var <- summary.pca$importance
  exp.var <- format(exp.var[2, 1:2] * 100, digits = 3)
  return(list("pc.result" = pc.result, "exp.var" = exp.var))
}




#### Data Simulation ####

#' @title Simulate datasets to be tested out
#' @description A wrapper function for the `simulateDataset` function from the 
#' MSstatsSampleSize package which enables simulating datasets for running experiments
#' @param data 
simulate_grid <- function(data = NULL, annot = NULL, num_simulation, exp_fc, fc_name,
                          list_diff_proteins, sel_simulated_proteins, 
                          prot_proportion, prot_number, samples_per_group, sim_valid,
                          valid_samples_per_grp, session = NULL){
  status(detail = "Setting Up Data Simulation Runs", value = 0.1, session = session)
  
  if(exp_fc != 'data'){
    status(detail = "Extracting Fold Change Informations", value = 0.15, session = session)
    diff_prots <- unlist(strsplit(list_diff_proteins, ","))
    #exp_fc <- as.numeric(unlist(strsplit(exp_fc,",")))
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


####### H2o Application for classification #######

ss_classify_h2o <- function(n_samp, sim_data, classifier, stopping_metric = "AUC",
                            seed = -1, nfolds = 10, fold_assignment = "AUTO", iters = 200,
                            alpha = 0, family, solver, link, min_sdev, laplace, eps,
                            session = NULL){
  
  samp <- unlist(strsplit(n_samp,','))
  models <- list()
  status(detail = "Getting parameters for H2O", value = 0.1, session = session)
  status(detail = "Initiating H2O Cluster", value = 0.1 , session = session)
  
  status(detail = sprintf("Running Classifier %s", classifier), session = session,
         value = 0.1)
  for(i in seq_along(samp)){
    val <- i/length(samp)
    # shiny::showNotification(sprintf("Classifying Sample %s of %s", i, length(samp)),
    #                         session = session, type = 'message')
    status(detail = sprintf("Classifying Sample %s of %s", i, length(samp)),
           session = session, value = val)
    
    valid_x <- sim_data[[i]]$valid_X
    valid_y <- sim_data[[i]]$valid_Y
    valid_x <- as.data.table(valid_x, keep.rownames = T)
    valid_x <- data.table(valid_x, condition =  valid_y)
    valid <- h2o::as.h2o(valid_x)
    train_x_list <- sim_data[[i]]$simulation_train_Xs 
    train_y_list = sim_data[[i]]$simulation_train_Ys
    
    for(index in seq_along(train_x_list)){
      new_val <- index/length(train_x_list)
      status(detail = sprintf("Classifying %s of %s", names(train_x_list)[index],
             length(train_x_list)), session = session, value = new_val)
      train <- data.table(train_x_list[[index]],
                          condition = as.factor(train_y_list[[index]]))
      train <- h2o::as.h2o(train)
      y <- "condition"
      x <- setdiff(names(train), y)
      
      
      #train <- dcast(test, Condition+BioReplicate~Protein, value.var = "Abundance")
      if(classifier == "rf"){
        model <- h2o::h2o.randomForest(x = x, y = y, training_frame = train,
                                       validation_frame = valid,
                                       stopping_rounds = 5, stopping_tolerance = 0.001, 
                                       stopping_metric = stopping_metric, seed = seed, 
                                       balance_classes = FALSE, nfolds = nfolds,
                                       fold_assignment = fold_assignment)
        
        l <- labs(subtitle = "Model: Random Forest (h2o package)")
        
      } else if (classifier == "nnet"){
        l1 = 0.5
        l2 = 0
        rate = 0.010
        rho = 0.99
        epochs = 10
        hidden = c(150,150)
        activation = "Rectifier"
        model <- h2o::h2o.deeplearning(x = x, y = y, training_frame = train)
      } else if (classifier == "svmLinear"){
        model <- h2o::h2o.psvm(x = x, y = y, training_frame = train, max_iterations = iters,
                               seed = seed, disable_training_metrics = F)
        l <- labs(subtitle = "Model: SVM (h2o package)")
      } else if (classifier == "logreg"){
        model <- h2o::h2o.glm(x = x, y = y, training_frame = train, seed = seed,
                              family = family, lambda_search = TRUE, alpha = alpha, 
                              nfolds = nfolds, solver = solver, link = link)
        l <- labs(subtitle = "Model: Regression (h2o package)")
      } else if (classifier == "naive_bayes"){
        model <- h2o::h2o.naiveBayes(x = x, y = y, training_frame = train, ignore_const_cols = TRUE,
                                     nfolds = nfolds, fold_assignment = fold_assignment,
                                     seed = seed, laplace = laplace, min_sdev = min_sdev,
                                     eps_sdev = eps, max_runtime_secs = 0)
        l <- labs(subtitle = "Model: Naive Bayes (h2o package)")
      } else{
        stop("Not defined")
      }
      
      status(detail = "Model training complete", value = new_val+0.2, session = session)
      perf <- h2o::h2o.performance(model = model, newdata = train)
      
      name_val <- sprintf("Sample%s %s", samp[i],  names(train_x_list)[index])
      var_imp <- h2o::h2o.varimp(model)
      models[[name_val]] <- list('model'= model, 'lab'=labs, 'perf' = perf,
                                 'var_imp' = var_imp)
    }
  }
  return(list('models' = models))
}

h2o_config <- function(){
  threads <- as.numeric(Sys.getenv('nthreads'))
  max_mem <- NULL
  mem <- Sys.getenv("max_mem")
  max_mem <- ifelse(grep('g',mem), mem, max_mem)
  log_dir <- Sys.getenv("log_dir")
  log_level <- Sys.getenv("log_level")
  
  threads <- ifelse(is.na(threads), -1, threads)
  max_mem <- ifelse(as.logical(grep("g",max_mem)), max_mem, NULL)
  
  log_dir <- ifelse(log_dir == "", getwd(), log_dir)
  log_level <- ifelse(log_level == "", "INFO", log_level)
  
  return(list("threads" = threads, "max_mem" = max_mem, "log_dir" = log_dir,
              "log_level" = log_level)) 
}

plot_acc <- function(data, xlim = c(0,1)){
  model_data <- data$models
  df <- rbindlist(lapply(names(model_data), function(x){
    z <- model_data[[x]]
    strs <- unlist(strsplit(x,' '))
    alg <- unique(z$model@algorithm)
    err <- z$model@model$training_metrics@metrics$mean_per_class_error
    data.table(sim  = as.numeric(gsub("[[:alpha:]]",'',strs[2])),
               sample = as.numeric(gsub("[[:alpha:]]",'',strs[1])),
               algorithm = alg,
               err = err,
               Mean_acc = mean(z$model@model$training_metrics@metrics$thresholds_and_metric_scores$accuracy))
  }))
  
  df <- df[,.(acc = mean(Mean_acc)),.(sample, algorithm)]
  p <- ggplot(data = df, aes(x = sample , y = acc))+
    labs(x = "Simulation Set", y = "Mean Accuracy",
         title = sprintf("Accuracy for classifier %s", df[, unique(algorithm)]))+
    geom_line()+
    geom_point()+
    scale_y_continuous(limits = xlim) +
    scale_x_continuous(breaks = df[,sample])+
    theme_MSstats()
  
  return(p)
}

plot_var_imp <- function(data, sample = 'all', alg = '', prots = 10){
  
  samp <- names(data)
  if(alg == "svmLinear"){
    samp <- names(data$models)
  }
    
  if(sample != 'all'){
    req_samp <- unique(unlist(strsplit(samp, ' ')))
    req_samp <- req_samp[grep("Sample", req_samp)]
    req_samp <- req_samp[grep(sample, req_samp)]
    samp <- samp[grep(req_samp, samp)]
  }
  
  df <- rbindlist(lapply(samp, function(x){
    dt <- as.data.table(data[[x]]$var_imp)
    setorder(dt, -scaled_importance)
    dt$name <- x
    dt
  }))
  
  df[, c('sample_size', 'simulation') := tstrsplit(name, " ", fixed = T)]
  df <- df[, lapply(.SD, mean), .SDcols = 2:4, by = c("variable", "sample_size")]
  
  dt <- df %>%
    mutate(variable = reorder(variable, relative_importance)) %>%
    group_by(sample_size, variable) %>%
    arrange(desc(relative_importance)) %>%
    ungroup() %>%
    mutate(variable = factor(paste(variable, sample_size, sep = '_'),
                             levels = rev(paste(variable, 
                                                sample_size, sep ='_'))))%>%
    as.data.table()
  
  g <- lapply(dt[,unique(sample_size)], function(x){
    ggplot(data = head(dt[sample_size == x], prots), aes(variable, relative_importance))+
      geom_col()+
      labs(x = "Protein", y = "Relative Importance", title = x)+
      scale_x_discrete(breaks = dt$variable,
                       labels = gsub("_.*",'',as.character(dt$variable)))+
      theme_MSstats()+
      coord_flip()
  })
  names(g) <- dt[,unique(sample_size)]
  
  return(g)
}

#### WRAPPER FOR CLASSIFICATION ######

run_classification <- function(sim, inputs, session = session){
  if(inputs$use_h2o){
    classification <- ss_classify_h2o(n_samp = inputs$n_samp_grp, sim_data = sim,
                                      classifier = inputs$classifier, stopping_metric = inputs$stop_metric,
                                      seed = -1, nfolds = inputs$nfolds,
                                      fold_assignment = inputs$f_assignment, iters = inputs$iters,
                                      family = inputs$family, solver = inputs$solver,
                                      link = inputs$link, min_sdev = inputs$min_sdev,
                                      laplace = inputs$laplace, eps = inputs$eps_sdev,
                                      session = session)
    plots <- plot_acc(data = classification)
    #plot_imp <- plot_var_imp(data = classification$models)
    classification <- append(classification, list('acc_plot' = plots))
  }else{
    classification <- sample_size_classification(n_samp = inputs$n_samp_grp,
                                                 sim_data = sim,
                                                 classifier = inputs$classifier,
                                                 session = session)
  }
  return(classification)
}