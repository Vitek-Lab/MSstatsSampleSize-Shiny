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
      shiny::showNotification(as.character(err), duration = 20, type = 'warning',
                              session = session, id = "error") 
      shiny::validate(shiny::need(is.null(err), as.character(err)))
    } else {
      stop(Sys.time(),": ",err)
    }
  } else if (!is.null(warn)){
    warn <- paste(unique(warn), collapse = ", ")
    if(!is.null(session)){
      shiny::showNotification(as.character(warn), duration = 3, type = 'warning',
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
  
  var <- estimateVar(data = formatted_data$wide_data,
                         annot = formatted_data$annot_data)
  
  plots <- show_faults(
    generate_plots_explore(formatted_data = formatted_data, est_var = var,
                           session = session),
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
    #read abundance data from the file path provided
    wide <- fread(count$datapath, keepLeadingZeros = T)
    #No column names expected for the protein columns
    #TODO make this name agnostic 
    setnames(wide, 'V1', 'Protein')
    status(detail = 'Importing Annotations file', value = 0.5,
           session = session)
    #read annotations from the file path provided
    annot <- fread(annot$datapath, keepLeadingZeros = T)
    name <- count$name
    
  }else if(format == 'examples'){
    status(detail = 'Importing Data from MSstatsSampleSize Package', value = 0.5,
           session = session)
    #example data from the package
    wide <- as.data.table(MSstatsSampleSize::OV_SRM_train,
                                      keep.rownames = T)
    #examples data from the package
    annot <- as.data.table(MSstatsSampleSize::OV_SRM_train_annotation)
    setnames(wide,'rn','Protein')
    name <- "Ovarian Cancer SRM study"
  }else{
    stop('Not Defined')
  }
  
  status(detail = 'Stretching the data', value = 0.6, session = session)
  #convert data to the long form
  data <- melt(wide, id.vars = 'Protein', variable.name = 'BioReplicate',
               value.name = 'Abundance')
  status(detail = 'Merging Abundance & Spectral Counts with Annotations', 
         value = 0.7, session = session)
  #merge the abundance data with the annotation data with the correct runs/bioreps
  if("Run" %in% names(annot)){
    data <- merge(data, annot[, BioReplicate := NULL],
                  by.y = "Run", by.x = "BioReplicate")
    annot[, BioReplicate := NULL]
    setnames(annot, 'Run', 'BioReplicate')
  }else{
    data <- merge(data, annot, by = 'BioReplicate') 
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
generate_plots_explore <- function(formatted_data = NULL, est_var = NULL,
                                   session = NULL){
  shiny::validate(shiny::need(formatted_data, 'No Data Found to visualize'))
  status(detail = 'Creating Boxplot for Protein Abundace', value = 0.8,
         session = session)
  #create the interactive boxplot for all the different proteins found in the data
  box_plot <- plotly::plot_ly(data = formatted_data$long_data[!is.na(Abundance)],
                       y = ~log(Abundance), x = ~BioReplicate, color = ~Condition,
                       type = "box") %>%
    plotly::layout(xaxis = list(title="Biological Replicate"), 
                   yaxis = list(title="Log Protein abundance"),
                   legend = list(orientation = "h", #position and of the legend
                                 xanchor = "center",
                                 x = 0.5, y = 1.1)) %>%
    plotly::config(displayModeBar = F) #hide controls of the plotly chart
  
  status(detail = 'Estimating Variance', value = 0.85, session = session)
  
  status(detail = 'Creating Mean/SD plots', value = 0.95, session = session)
  #plot the mean and variance plots
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
    theme_MSstats()+
    theme(legend.position = 'none')
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
                       title = element_text(size = x.axis.size + 4,
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
simulate_grid <- function(data = NULL, annot = NULL, var = NULL, num_simulation, exp_fc,
                          list_diff_proteins, sel_simulated_proteins, 
                          prot_proportion, prot_number, samples_per_group, sim_valid,
                          valid_samples_per_grp, seed, session = NULL){
  status(detail = "Setting Up Data Simulation Runs", value = 0.1, session = session)
  if(seed != -1)
    set.seed(seed)
  
  if(exp_fc != 'data'){
    status(detail = "Extracting Fold Change Informations", value = 0.15, session = session)
    diff_prots <- unlist(strsplit(list_diff_proteins, ","))
    fc <- exp_fc$`Fold Change Value`
    names(fc) <- exp_fc$orig_group
  } else{
    diff_prots <- NULL
    fc <- exp_fc
  }
  status(detail = "Extracting Number of Samples Information", value = 0.2, session = session)
  samp <- as.numeric(unlist(strsplit(samples_per_group, ',')))
  shiny::validate(shiny::need(all(!is.na(samp)),
                              sprintf("Samples Per Group need to be numeric values, Found : %s",
                                      samples_per_group)),
                  shiny::need(all(samp >= 1), "All samples Need to be >= 1"))
  
  if(sim_valid){
    status(detail = "Validation Simulation requested", value = 0.2, session = session)
  }
  
  status(detail = "Starting Simulation", value = 0.3, session = session)
  
  data_mat <- data.table:::as.matrix.data.table(data, rownames = 'Protein')
  
  sim <- list()
  for(i in samp){
    status(detail = sprintf("Running Simulation for sample %s of %s", which(i == samp),
                            length(samp)),
           value = which(i==samp)/length(samp), 
           session = session)
    
    sim[[paste(i)]] <- simulateDataset(data = data_mat,
                                       annotation = annot,
                                       #parameters = var,
                                       num_simulations = num_simulation,
                                       expected_FC = fc,
                                       list_diff_proteins =  diff_prots,
                                       select_simulated_proteins = sel_simulated_proteins,
                                       protein_proportion = prot_proportion,
                                       protein_number = prot_number,
                                       samples_per_group = i,
                                       simulate_valid = as.logical(sim_valid),
                                       valid_samples_per_group = valid_samples_per_grp)
    gc()
  }
  
  status(detail = "Simulation Complete", value = 0.9, session = session)
  return(sim)
}

#### Classification #####

sample_size_classification <- function(n_samp, sim_data, classifier, k = 10,
                                       family = 'binomial', session = NULL){
  samp <- unlist(strsplit(n_samp,','))
  pred_acc <- list()
  f_imp <- list()
  models <- list()
  max_val <- 0
  iter <- 0
  
  for(i in seq_along(samp)){
    
    res <- list()
    imp <- list()
    model <- list()
    list_x <- sim_data[[i]]$simulation_train_Xs
    list_y <- sim_data[[i]]$simulation_train_Ys

    valid <- cbind('condition' = sim_data[[i]]$valid_Y,
                   sim_data[[i]]$valid_X)

    if(length(unique(sim_data[[i]]$input_Y)) > 2){
      family <- 'multinomial'
    }
    
    tryCatch({
    for(j in seq_along(list_x)){
      if(max_val == 0){
        max_val <- length(list_x) * length(samp)
      }
      iter = iter + 1/max_val
      
      status(detail = sprintf("Classifying Sample Size %s of %s, Simulation %s of %s",
                              i, length(samp), j, length(list_x)),
             session = session, value = iter)
      
      df <- cbind('condition' = list_y[[j]],
                  list_x[[j]])
      res[[paste0('Sim',j)]] <- classify(df = cbind('condition' = list_y[[j]],
                                                    list_x[[j]]), 
                                         val = valid, alg = classifier,
                                         family = family, k = k)
    }
    }, error = function(e){
      print(e)
    })
    
    for(j in seq_along(res)){
      acc <- data.frame('Sample' = samp[i],'accuracy' = res[[j]]$accuracy)
      pred_acc <- append(list(acc), pred_acc)

      imp[[j]] <- data.table('Simulation' = j, res[[j]]$f_imp[1:k])
      model[[j]] <- res[[j]]$model
    }
    
    imp <- do.call('rbind', imp)
    imp <- dcast(imp, rn~Simulation, value.var = 'Overall')
    imp <- cbind('protein' = imp[,1],
                 'importance' = rowSums(imp[,-1], na.rm = T))
    imp[, importance := (importance-min(importance))/(max(importance) - min(importance))]

    models[[as.character(samp[i])]] <- model
    f_imp[[as.character(samp[i])]] <- imp
  }
  return(list('res' = models, 'samp' = as.numeric(samp), 'pred_acc' = pred_acc,
              'f_imp' = f_imp))
}


classify <- function(df, val, alg, family, k){
  
  if(alg == 'logreg'){
    alg = 'glm'
  }
  
  if(alg == 'glm'){
    if(family == 'multinomial'){
      model <- nnet::multinom(condition~., data, maxit, MaxNWts = 84581)
      f_imp <- caret::varImp(model, scale = T)
      sel_imp <- rownames(f_imp)[1:k]
      sel_imp <- gsub('`','',sel_imp)
      model <- nnet::multinom(condition~., data = df[, c('condition', sel_imp)],
                              maxit=1000,MaxNWts=84581)
    } else {
      model <- caret::train(make.names(condition)~.,data = df,
                            method = alg, family = family,
                            trControl = caret::trainControl(method = "none",
                                                            classProbs = TRUE))
      f_imp <- caret::varImp(model, scale = TRUE)
      i_ff <- data.table::as.data.table(f_imp$importance, keep.rownames = T)
      setorder(i_ff, -Overall)
      sel_imp <- i_ff[1:k, rn]
      model <- caret::train(x = x[,sel_imp], y = make.names(y), 
                            method = alg,
                            trControl = caret::trainControl(method = "none",
                                                            classProbs = TRUE))
    }
  } else {
    model <- caret::train(make.names(condition)~., data = df,
                          method = alg, 
                          trControl = caret::trainControl(method = "none", 
                                                          classProbs = TRUE)) 
    
    f_imp <- caret::varImp(model, scale = TRUE)
    i_ff <- data.table::as.data.table(f_imp$importance, keep.rownames = T)
    setorder(i_ff, -Overall)
    sel_imp <- i_ff[1:k, rn]
    
    if(!all(sel_imp %in% names(df))){
      sel_imp <- gsub('`','',sel_imp)
    }
    model <- caret::train(make.names(condition)~.,
                          data = df[, c('condition', sel_imp)],
                          method = alg, 
                          trControl = caret::trainControl(method = "none", 
                                                          classProbs = TRUE)) 
    
  }
  
  pred <- predict(model, val[-1])
  cm <- table(pred, val$condition)
  
  acc <- sum(diag(cm))/sum(cm)
  r_list <- list('accuracy' = acc, 'model' = model, 'f_imp' = i_ff)
  return(r_list)
}



####### H2o Application for classification #######

ss_classify_h2o <- function(n_samp, sim_data, classifier, stopping_metric = "AUTO",
                            seed = -1, nfolds = 0, fold_assignment = "AUTO", iters = 200,
                            alpha = 0, family, solver, link, min_sdev, laplace, eps,
                            session = NULL){
  samp <- unlist(strsplit(n_samp,','))
  config <- h2o_config()
  h2o::h2o.init(nthreads = -1, max_mem_size = '1g',
                log_dir = config$log_dir, log_level = config$log_level)
  max_val <- 0
  iter <- 0
  
  modelz <- list()
  for(i in samp){
    
    valid_x <- sim_data[[i]]$valid_X
    valid_y <- sim_data[[i]]$valid_Y
    valid <- h2o::as.h2o(cbind('condition' = valid_y, valid_x),
                         destination_frame = 'valid')
    
    train_x_list <- sim_data[[i]]$simulation_train_Xs 
    train_y_list = sim_data[[i]]$simulation_train_Ys
    for(index in seq_along(train_x_list)){
      if(max_val == 0){
        max_val <- length(train_x_list) * length(samp)
      }
      iter = iter + 1/max_val
      
      status(detail = sprintf("Classifying Sample Size %s of %s, Simulation %s of %s",
                              which(samp == i), length(samp), index, length(train_x_list)),
             session = session, value = iter)
      
      x <- train_x_list[[index]]
      y <- train_y_list[[index]]
      train <- h2o::as.h2o(cbind('condition' = y, x), destination_frame = 'train')
      
      if(classifier == "rf"){
        model <- h2o::h2o.randomForest(y = 1, training_frame = train,
                                       stopping_metric = stopping_metric, seed = seed, 
                                       balance_classes = FALSE, nfolds = nfolds,
                                       fold_assignment = fold_assignment)
        var_imp <- h2o::h2o.varimp(model)
        sel_imp <- var_imp$variable[1:10]
        h2o::h2o.rm(train)
        train <- h2o::as.h2o(cbind('condition'=y,x[,sel_imp]), destination_frame = 'train')
        model <- h2o::h2o.randomForest(y = 1, training_frame = train,
                                       validation_frame = valid,
                                       stopping_metric = stopping_metric, seed = seed, 
                                       balance_classes = FALSE, nfolds = nfolds,
                                       fold_assignment = fold_assignment)
        
      } else if (classifier == "nnet"){
        l1 = 0
        l2 = 0
        rho = 0.99
        epochs = 10
        hidden = c(250,250)
        activation = "Rectifier"
        model <- h2o::h2o.deeplearning(y = 1, 
                                       training_frame = sim_env,
                                       l1= l1, l2=l2,
                                       activation = activation,
                                       hidden = hidden,
                                       epochs = epochs)
        found_imp <- h2o::h2o.varimp(model)
        sel_imp <- found_imp$variable[1:10]
        train <- h2o::as.h2o(cbind('condition'=y,x[,sel_imp]), destination_frame = 'train')
        model <- h2o::h2o.deeplearning(y = 1, training_frame = sim_env,
                                       validation_frame = val, l1 = l1, l2 = l2,
                                       activation = activation, hidden = hidden,
                                       epochs = epochs)
        
      } else if (classifier == "svmLinear"){
        model <- h2o::h2o.psvm(x = x, y = y, training_frame = train, 
                               max_iterations = iters,
                               seed = seed, validation_frame = valid,
                               disable_training_metrics = F)
      } else if (classifier == "logreg"){
        model <- h2o::h2o.glm(y = 1, training_frame = train,
                              seed = seed, family = family, alpha = alpha, 
                              nfolds = nfolds, solver = solver,
                              link = link)
        
        var_imp <- h2o.varimp(model)
        sel_imp <- var_imp$variable[1:10]
        h2o::h2o.rm(train)
        train <- h2o::as.h2o(cbind('condition'=y,x[,sel_imp]), destination_frame = 'train')
        model <- h2o::h2o.glm(y = 1, training_frame = train, seed = seed,
                              family = family, alpha = alpha, nfolds = nfolds, 
                              solver = solver, link = link, validation_frame = valid)
      } else if (classifier == "naive_bayes"){
        model <- h2o::h2o.naiveBayes(y = 1, training_frame = train,
                                       ignore_const_cols = TRUE,
                                     nfolds = nfolds,
                                     fold_assignment = fold_assignment,
                                     seed = seed, laplace = laplace,
                                     min_sdev = min_sdev,
                                     eps_sdev = eps)
      } else{
        stop("Not defined")
      }
      perf <- h2o::h2o.performance(model = model, newdata = train)
      name_val <- sprintf("Sample%s %s", i,  names(train_x_list)[index])
      var_imp <-  h2o::h2o.varimp(model)
      modelz[[name_val]] <- list('model'= model, 'perf' = perf,
                                 'var_imp' = var_imp)
    }
  }
  h2o::h2o.shutdown(prompt = F)
  gc()
  return(list('models' = modelz))
}

h2o_config <- function(){
  config <- list()
  config$threads <- as.numeric(Sys.getenv('nthreads'))
  config$max_mem <- NULL
  mem <- Sys.getenv("max_mem")
  if(mem!= '')
    config$max_mem <- mem
  config$log_dir <- Sys.getenv("log_dir")
  config$log_level <- Sys.getenv("log_level")
  
  config$threads <- ifelse(is.na(config$threads), -1, config$threads)
  
  config$log_dir <- ifelse(config$log_dir == "", getwd(), config$log_dir)
  config$log_level <- ifelse(config$log_level == "", "INFO", config$log_level)
  
  return(config) 
}

plot_acc <- function(data, use_h2o, alg = NA){
  if(use_h2o){
    shiny::validate(shiny::need(data$models, "No Models Run Yet"))
    #loop through the object returned by classification to extract accuracy
    model_data <- data$models
    df <- rbindlist(lapply(names(model_data), function(x){
      z <- model_data[[x]]
      strs <- unlist(strsplit(x,' '))
      
      cm <- z$model@model$validation_metrics@metrics$cm$table
      cm <- cm[1:(dim(cm)[1]-1),1:(dim(cm)[2] -2)]
      cm <- as.matrix(sapply(cm, as.numeric))
      acc <- sum(diag(cm))/sum(cm)
      
      data.table(sim  = as.numeric(gsub("[[:alpha:]]",'',strs[2])),
                 sample = as.factor(gsub("[[:alpha:]]",'',strs[1])),
                 mean_acc = acc)
    }))
  }else{
    shiny::validate(shiny::need(data$samp, "No Trained Models Found"))
    df <- rbindlist(data$pred_acc)
    names(df) <- c("sample","mean_acc")
  }
  
  df[, acc := mean(mean_acc), sample]
  setorder(df, -sample)
  # following logic is flawed only workds if the data.table is arrange by sample
  # size in increasing order
  #######
  mean_PA <- df$acc
  sample_size <- df$sample
  dydx <- -diff(mean_PA)/-diff(as.numeric(as.character(sample_size)))
  
  if(any(dydx >= 0.0001)){
    inter_dydx <- dydx[which(dydx >= 0.0001)]
    min_dydx <- inter_dydx[which.min(inter_dydx)]
    optimal_index <- which(dydx == min_dydx)
    optimal_sample_size_per_group <- sample_size[optimal_index]
  } else{
    optimal_sample_size_per_group <- sample_size[1]
  }
  
  y_lim <- c(df[,min(acc, na.rm = T)]-0.1, 1)
  df[sample == optimal_sample_size_per_group, fill_col := 'red']
  ######

  p <- ggplot(data = df, aes(x = reorder(sample)))+
    geom_boxplot(aes(y = mean_acc, group = sample, fill = fill_col), alpha = 0.5)+
    scale_fill_identity()+
    # geom_vline(xintercept = optimal_sample_size_per_group, color = 'red', size= 0.75)+
    geom_point(aes(y = acc))+
    geom_line(aes(y = acc, group = 1), size = 0.75, color = "blue")+
    labs(x = "Simulated Sample Size", y = "Predictive Accuracy",
         title = sprintf("Classifier %s", alg),
         subtitle = sprintf("Optimum accuracy achieved when sample size is : %s",
                           optimal_sample_size_per_group))+
    ylim(y_lim)+
    theme_MSstats()+
    theme(plot.subtitle = element_text(face = 'italic', color = 'red'))
  
  return(p)
}

plot_var_imp <- function(data, sample = 'all', alg = NA, use_h2o, prots = 10){
  if(use_h2o){
    if(prots == 'all'){
      prots <- nrow(data$models[[1]]$var_imp)
    }
    
    shiny::validate(shiny::need(data$models, "No Trained Models Found"))
    data <- data$models
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
    df[, relative_importance := scaled_importance]
    
  }else{
    if(sample == 'all'){
      sample <- as.character(data$samp)
    }else{
      sample <- gsub("Sample","", sample)
    }
    
    df <- rbindlist(lapply(sample, function(x){
      d <- data$f_imp[[x]]
      d[, sample_size := paste("SampleSize",x)]
      setnames(d, c('protein.rn', 'importance'),
               c('variable', 'relative_importance'),
               skip_absent = T)
    }))
    
    if(prots == 'all'){
      prots <- max(df[,.N,sample_size][,unique(N)], na.rm = T)
    }
  }
  
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

run_classification <- function(sim, inputs, use_h2o, seed, session = session){
  if(seed != -1)
    set.seed(seed)
  
  if(use_h2o){
    classification <- ss_classify_h2o(n_samp = inputs$n_samp_grp, sim_data = sim,
                                      classifier = inputs$classifier,
                                      stopping_metric = inputs$stop_metric,
                                      nfolds = inputs$nfolds,
                                      fold_assignment = inputs$f_assignment, iters = inputs$iters,
                                      family = inputs$family, solver = inputs$solver,
                                      link = inputs$link, min_sdev = inputs$min_sdev,
                                      laplace = inputs$laplace, eps = inputs$eps_sdev,
                                      seed = -1, session = session)
  }else{
    
    classification <- sample_size_classification(n_samp = inputs$n_samp_grp,
                                                 sim_data = sim,
                                                 #alg = inputs$
                                                 classifier = inputs$classifier,
                                                 session = session)
  }
  return(classification)
}







