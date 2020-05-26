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
  #initiate variables to null
  warn <- err <- NULL
  #check for error and warnings with handlers
  res <- withCallingHandlers(tryCatch(..., error = function(e) {
    err <<- conditionMessage(e)
    NULL
  }), warning = function(w) {
    warn <<- append(warn, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  #format the error and warnings for shiny/console logging
  if(!is.null(err)){
    if(!is.null(session)){
      shiny::showNotification(as.character(err), duration = 20, type = "warning",
                              session = session, id = "error") 
      shiny::validate(shiny::need(is.null(err), as.character(err)))
    } else {
      stop(Sys.time(),": ",err)
    }
  } else if (!is.null(warn)){
    warn <- paste(unique(warn), collapse = ", ")
    if(!is.null(session)){
      shiny::showNotification(as.character(warn), duration = 3, type = "warning",
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


#' @title H2o configuration
#' @description Get configuration details of the h2o instance to start, the details
#' can be specified in the Rprofile.site file in the R installation path
#' @param NONE
#' @return A named list containing required h2o configuration details, if none 
#' provided, defaults are used
h2o_config <- function(){
  options("h2o.use.data.table" = T)
  options('h2o.fwrite' = T)
  config <- list()
  config$threads <- as.numeric(Sys.getenv("nthreads"))
  config$max_mem <- NULL
  mem <- Sys.getenv("max_mem")
  if(mem!= "")
    config$max_mem <- mem
  config$log_dir <- Sys.getenv("log_dir")
  config$log_level <- Sys.getenv("log_level")
  
  config$threads <- ifelse(is.na(config$threads), -1, config$threads)
  
  config$log_dir <- ifelse(config$log_dir == "", getwd(), config$log_dir)
  config$log_level <- ifelse(config$log_level == "", "INFO", config$log_level)
  
  return(config) 
}


#' @title MSstats Theme
#' @description A utility function that standardized all the ggplot themes
#' @param x.axis.size A numeric value for size for the elements on the x-axis
#' @param y.axis.size A numeric value for size for the elements on the y-axis
#' @param legend.size A numeric value fot the size of the legend
theme_MSstats <- function(x.axis.size = 10, y.axis.size = 10, 
                          legend.size = 11, margin = 0.5, leg.dir="horizontal"){
  
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
                       legend.direction = leg.dir,
                       legend.text = element_text(size = legend.size), 
                       legend.title = element_blank(),
                       plot.margin = unit(rep(margin,4), "cm"),
                       legend.position=c(1, 1.05),
                       legend.justification="right")
  return(th)
}


#' @title Plot Accuracy for Classification Models
#' @description Extracts information from the classification wrapper, and identifies
#' the optimal sample size and returns a ggplot object  
#' @param data A named list as returned by `run_classification()`
#' @param use_h2o A logical inputs which detemines if h2o was used for classification
#' @param alg A character vector detemining the name of the classifier used
#' @return A ggplot2 object
plot_acc <- function(data, use_h2o, alg = NA, session = NULL){
  if(use_h2o){
    #check if required data exists
    shiny::validate(shiny::need(data$models, "No Models Run Yet"))
    #loop through the object returned by classification to extract accuracy
    model_data <- data$models
    df <- rbindlist(lapply(names(model_data), function(x){
      acc <- model_data[[x]]$acc
      strs <- unlist(strsplit(x," "))
      data.table(sim  = as.numeric(gsub("[[:alpha:]]","",strs[2])),
                 sample = as.factor(gsub("[[:alpha:]]","",strs[1])),
                 mean_acc = acc)
    }))
  }else{
    shiny::validate(shiny::need(data$samp, "No Trained Models Found"))
    df <- rbindlist(data$pred_acc)
    names(df) <- c("sample","mean_acc")
  }
  #calculate the mean accuracy for the sample
  df[, acc := mean(mean_acc), sample]
  ####### Identify the optimal sample size ####
  opt_df <- unique(df[,.(sample, acc)])
  setorder(opt_df, -sample)
  if(use_h2o)
    setorder(opt_df, sample)
  
  opt_df[, sample := as.numeric(as.character(sample))]
  opt_df[, lg := (acc - shift(acc))/(sample - shift(sample))]
  opt_df[, optimal := ifelse(lg >= 0.001, T, F)]
  if(nrow(opt_df[, .N, optimal][optimal == T]) != 0){
    optimal_sample_size <- opt_df[optimal == T][which.min(lg), sample]
  } else {
    optimal_sample_size <- opt_df[which.min(sample), sample]
  }
  
  y_lim <- c(df[,min(acc, na.rm = T)]-0.1, 1)
  df[sample == optimal_sample_size, fill_col := "red"]
  
  if(!is.null(session)){
  OPTIMAL_SIZE <- paste0("Sample",optimal_sample_size) #uses global variable
  updateSelectInput(session = session, inputId = "s_size", label = "Sample Size",
                    choices = samp_size, selected = OPTIMAL_SIZE)
  }
  ######
  #Plot the accuracy plot
  p <- ggplot(data = df, aes(x = reorder(sample)))+
    geom_boxplot(aes(y = mean_acc, group = sample, fill = fill_col), alpha = 0.5)+
    scale_fill_identity()+
    geom_point(aes(y = acc))+
    geom_line(aes(y = acc, group = 1), size = 0.75, color = "blue")+
    labs(x = "Simulated Sample Size Per Group", y = "Predictive Accuracy",
         title = sprintf("Classifier %s", alg),
         subtitle = sprintf("Optimum accuracy achieved when sample size per group is : %s",
                            optimal_sample_size))+
    ylim(y_lim)+
    theme_MSstats()+
    theme(plot.subtitle = element_text(face = "italic", color = "red"))
  
  return(p)
}


#' @title Plot Variable Importances
#' @description Extract information from the classification wrapper and 
#' retuns the protein importance plots for the selected sample size
#' @param data A names list as returned by `run_classification()`
#' @param sample A character vector specifying the sample size
#' @param alg A vector input with the name of the classifier
#' @param use_h2o A logical input to extract h2o based data model
#' @param prots Number of proteins to plots, takes "all" to select all proteins
#' @return A ggplot2 object
plot_var_imp <- function(data, sample = "all", alg = NA, use_h2o, prots = 10){
  if(use_h2o){
    #identify number of proteins if complete variable imporatnaces are requested
    if(prots == "all"){
      prots <- nrow(data$models[[1]]$var_imp)
    }
    shiny::validate(shiny::need(data$models, "No Trained Models Found"))
    shiny::validate(
      shiny::need(!alg %in% c("svmLinear", "naive_bayes"),
                  sprintf("Protein Importances for Classifier %s in H2o unavailable",
                          alg)))
    data <- data$models
    samp <- names(data)
    #extract only those simulations which correspond to the selected sample
    if(sample != "all"){
      samp <- samp[like(samp, sample)]
    }
    
    #extract data from the h2o model structure
    df <- rbindlist(lapply(samp, function(x){
      dt <- as.data.table(data[[x]]$var_imp)
      setorder(dt, -scaled_importance)
      dt$name <- x
      dt
    }))
    df[, c("sample_size", "simulation") := tstrsplit(name, " ", fixed = T)]
    df <- df[, lapply(.SD, mean), .SDcols = 2:4, by = c("variable", "sample_size")]
    df[, relative_importance := scaled_importance]
    
  }else{
    if(sample == "all"){
      sample <- as.character(data$samp)
    }else{
      sample <- gsub("Sample","", sample)
    }
    
    df <- rbindlist(lapply(sample, function(x){
      d <- data$f_imp[[x]]
      d[, sample_size := paste("SampleSize",x)]
      setnames(d, c("protein.rn", "importance"),
               c("variable", "relative_importance"),
               skip_absent = T)
      d[!is.na(variable)]
    }))
    
    if(prots == "all"){
      prots <- max(df[,.N,sample_size][,unique(N)], na.rm = T)
    }
  }
  #TODO This is ugly needs to be re-thought, partial implementation on dev
  dt <- df %>%
    mutate(variable = reorder(variable, relative_importance)) %>%
    group_by(sample_size, variable) %>%
    arrange(desc(relative_importance)) %>%
    ungroup() %>%
    mutate(variable = factor(paste(variable, sample_size, sep = "_"),
                             levels = rev(paste(variable, 
                                                sample_size, sep ="_"))))%>%
    as.data.table()
  
  g <- lapply(dt[,unique(sample_size)], function(x){
    ggplot(data = head(dt[sample_size == x], prots),
           aes(variable, relative_importance))+
      geom_col()+
      labs(x = "Protein", y = "Relative Importance", title = x)+
      scale_x_discrete(breaks = dt$variable,
                       labels = gsub("_.*","",as.character(dt$variable)))+
      theme_MSstats()+
      coord_flip()
  })
  names(g) <- dt[,unique(sample_size)]
  return(g)
}


#' @title Plot Mean Variance of data
#' @description This function plots the estimated mean and variance of the data
#' as a scatter plot with trendline overlayed
#' @param data A dataframe containing the output from the function `estimateVar()`
#' @return A ggplot2 object 
meanSDplot <- function (data, x.axis.size = 10, y.axis.size = 10, smoother_size = 1, 
                        xlimUp = 30, ylimUp = 3){
  
  plotdata <- data.table(mean = as.vector(data$mu), sd = as.vector(data$sigma))
  plot.lowess <- lowess(cbind(plotdata$mean, plotdata$sd))
  plot.lowess <- data.table(x = plot.lowess$x, y = plot.lowess$y)
  
  ggplot(data = plotdata, aes(mean, sd)) +
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
    theme(legend.position = "none")
}


#' @title Plot QC boxplots
#' @description Plots an interactive plotly graphic with boxplots for all the 
#' proteins and their respective conditions with abundance indicated in the log
#' scale on the y axis
#' @param data A formatted data.frame with abundance, bioreplicate and conditions 
#' information
#' @return A plotly object
qc_boxplot <- function(data = NULL){
  #create the interactive boxplot for all the different proteins found in the data
  box_plot <- plotly::plot_ly(data = data[!is.na(Abundance)],
                              y = ~log(Abundance), x = ~BioReplicate, color = ~Condition,
                              type = "box") %>%
    plotly::layout(xaxis = list(title="Biological Replicate",showticklabels = TRUE,
                                tickangle = -45 ), 
                   yaxis = list(title="Log Protein abundance"),
                   legend = list(orientation = "h", #position and of the legend
                                 xanchor = "center",
                                 x = 0.5, y = 1.1)) %>%
    plotly::config(displayModeBar = F) #hide controls of the plotly chart
  
  box_plot
}


#### Data Exploration ####

#' @title Formats data to required longer format
#' @description Formats the input data in the required long and wide format
#' depending on input data
#' @param  format A decided which data to use, user provided or default examples
#' @param count A path to the data to be read in, any csv formats are acceptable
#' @param annot An annotation file path as csv
#' @param session A shiny session variable to make the notifications interactive
#' @return A named list of the data an other objects as required
format_data <- function(format, count = NULL, annot = NULL, session = NULL){
  shiny::validate(shiny::need(format %in% FORMATS, "Undefined Format"))
  if(format == "standard"){
    status(detail = "Importing Protein Abundance file", value = 0.4,
           session = session)
    #read abundance data from the file path provided
    wide <- fread(count$datapath, keepLeadingZeros = T)
    #No column names expected for the protein columns
    setnames(wide, "V1", "Protein")
    uniq_prots <- unique(wide[, Protein])
    prots_combinations <- uniq_prots[grep(",|;",uniq_prots)]
    if(length(prots_combinations) > 0){
      single_prots <- uniq_prots[-grep(",|;", uniq_prots)]
      v <- do.call("c", lapply(prots_combinations, function(x){
        comb <- unlist(strsplit(x,",|;"))
        val <- any(comb %in% single_prots)
        if(!val){
          return(x)
        }
      }))
      single_prots <- c(single_prots,v)
      message(Sys.time()," Old Proteins ", length(uniq_prots),
              ", New Proteins ", length(single_prots))
      
      wide <- wide[Protein %in% single_prots]
    }
    name <- count$name
    status(detail = "Importing Annotations file", value = 0.5,
           session = session)
    #read annotations from the file path provided
    annot <- fread(annot$datapath, keepLeadingZeros = T)
    name <- count$name
    
  }else if(format == "examples"){
    status(detail = "Importing Data from MSstatsSampleSize Package", value = 0.5,
           session = session)
    #example data from the package
    wide <- as.data.table(MSstatsSampleSize::OV_SRM_train,
                                      keep.rownames = T)
    #examples data from the package
    annot <- as.data.table(MSstatsSampleSize::OV_SRM_train_annotation)
    setnames(wide,"rn","Protein")
    name <- "Ovarian Cancer SRM study"
  }else{
    stop("Not Defined")
  }
  #get summary about the bioreplicates and runs of the data
  data_summary <- format_summary_table(data = annot)
  
  status(detail = "Stretching the data", value = 0.6, session = session)
  #convert data to the long form
  data <- melt(wide, id.vars = "Protein", variable.name = "BioReplicate",
               value.name = "Abundance")
  status(detail = "Merging Abundance & Spectral Counts with Annotations", 
         value = 0.7, session = session)
  #merge the abundance data with the annotation data with the correct runs/bioreps
  if("Run" %in% names(annot)){
    data <- merge(data, annot[, BioReplicate := NULL],
                  by.y = "Run", by.x = "BioReplicate")
    setnames(annot, "Run", "BioReplicate")
  }else{
    data <- merge(data, annot, by = "BioReplicate") 
  }

  status(detail = "Estimating Variance", value = 0.8, session = session)
  var <- estimateVar(data = wide, annot = annot)
  
  status(detail = "Creating Summary Table", value = 0.9, session = session)
  sum_table <- data.frame(Parameter =  c("Number of Proteins", "Number of Groups"),
                          Values = c(nrow(wide[,.N, Protein]),
                                     nrow(annot[,.N,Condition])))
  status(detail = "Creating Box Plots", value = 0.95, session = session)
  box_plot <- qc_boxplot(data = data)
  
  return(list("wide_data" = wide, "annot_data" = annot, "box_plot" = box_plot,
              "n_prot" = nrow(wide[,.N, Protein]), "cond_sum_table" = data_summary,
              "n_group" = nrow(annot[,.N,Condition]), "dataset_name" = name,
              "sum_table" = as.matrix(sum_table), "est_var" = var))
}


#' @title Get summary table for the annotation data
#' @description Get the summary for the unique bioreplicates and number of
#' MS runs for the data that is provided
#' @param data A data.frame with the annotation data containing the Bioreplicates
#' Runs and condition information
#' @return A data.frame with the counts of bioreplicates for each conditions
#' and the number of runs as well
format_summary_table <- function(data = NULL){
  #create crosstable for the conditions vs bioreplicates
  tryCatch({
    biorep <- unique(data[,.(BioReplicate, Condition)])
    biorep <- xtabs(~Condition, data = biorep)
    
    #create crosstable for the conditions vs runs if runs data exists
    if(any(c("run","Run") %in% names(data))){
      msruns <- unique(data[,.(Run, Condition)])
      msruns <- xtabs(~Condition, data = msruns)
    }else{
      msruns <- rep(0, length(names(biorep)))
      names(msruns) <- names(biorep) #make runs data 0 if not found
    }
    #format it correctly
    #summary <- rbind(biorep, msruns)
    summary <-matrix(biorep, nrow = 1)
    colnames(summary) <- names(biorep)
    summary <- summary[,which(colSums(summary, na.rm = T) > 0)]
    sum_table <- matrix(summary, nrow=1)
    #rownames(summary) <- c("# of Biological Replicates", "# of MS runs")
    dimnames(sum_table) <- list("# of Biological Replicates", names(summary))
    return(sum_table)
  }, error = function(e){
    return(e)
  })
}


#' @title Make pca plots
#' @description  A wrapper function which formats all the data to pass to the 
#' `pca_plot()` function
#' @param simulations A list object with all the simulations that are runn
#' @param which A character vector specifying which simulations to perform PCA for
#' `all` specifies that perform pca on all simulations
#' @param address A file path to be provided where to save the pca plots
#' @param width Page width parameter for the pdf
#' @param height Page height parameter for the pdf
#' @session A session object for the shiny implementation
make_pca_plots <- function(simulations, which = "all", address = NA,
                           width = 3, height = 3, dot_size = 3, session = NULL){
  pc_plot <- list()
  if(which %in% c("all", "allonly")){
    iter <- length(simulations$simulation_train_Xs)+1
    pr_comp <- do_prcomp(sim = simulations)
    pc_plot[[iter]] <- pca_plot(data = pr_comp$pc.result, exp_var = pr_comp$exp.var,
                                dot_size = dot_size)
  }
  
  if(grep("simulation", which) | which == "all"){
    index <- length(simulations$simulation_train_Xs)
    iter <- as.numeric(gsub("[[:alpha:]]","",which))
    iters <- ifelse(!is.na(iter), iter, seq_along(index))
    for(i in iters){
      pr_comp <- do_prcomp(sim = simulations$simulation_train_Xs[[i]],
                           sim_y = simulations$simulation_train_Ys[[i]])
      pc_plot[[i]] <- pca_plot(data = pr_comp$pc.result, exp_var = pr_comp$exp.var,
                               dot_size = dot_size)
    }
  }else{
    stop(Sys.time()," Improper which arguement provided")
  }
  
  if(!is.na(address) | which == "all"){
    address <- ifelse(is.na(address), getwd(), address)
    file_name <- file.path(address, sprintf("PCA_Plot_%s.pdf",
                                            format(Sys.time(), "%Y%m%d%H%M")))
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
#' @param annot
#' @param num_simulations
#' @param exp_fc
#' @param list_diff_proteins
#' @param sel_simulated_proteins
#' @param prot_proportion
#' @param prot_number
#' @param samples_per_group
#' @param sim_valid
#' @param valid_samples_per_grp
#' @param seed
#' @param session
#' @return 
simulate_grid <- function(data = NULL, annot = NULL, num_simulation, exp_fc,
                          list_diff_proteins, sel_simulated_proteins, 
                          prot_proportion, prot_number, samples_per_group, sim_valid,
                          valid_samples_per_grp, seed, session = NULL){
  #check if seed value required
  status(detail = "Setting Up Data Simulation Runs", value = 0.1, session = session)
  if(seed != -1)
    set.seed(seed)
  
  if(exp_fc != "data"){
    status(detail = "Extracting Fold Change Informations", value = 0.15, session = session)
    diff_prots <- unlist(strsplit(list_diff_proteins, ","))
    fc <- exp_fc$`Fold Change Value`
    names(fc) <- exp_fc$orig_group
  } else{
    diff_prots <- NULL
    fc <- exp_fc
  }
  status(detail = "Extracting Number of Samples Information", value = 0.2, session = session)
  samp <- as.numeric(unlist(strsplit(samples_per_group, ",")))
  shiny::validate(shiny::need(all(!is.na(samp)),
                              sprintf("Samples Per Group need to be numeric values, Found : %s",
                                      samples_per_group)),
                  shiny::need(all(samp >= 1), "All samples Need to be >= 1"))
  
  if(sim_valid){
    status(detail = "Validation Simulation requested", value = 0.2, session = session)
  }
  
  status(detail = "Starting Simulation", value = 0.3, session = session)
  
  data_mat <- data.table:::as.matrix.data.table(data, rownames = "Protein")
  
  sim <- list()
  for(i in samp){
    status(detail = sprintf("Running Simulation for sample %s of %s", which(i == samp),
                            length(samp)),
           value = which(i==samp)/length(samp), 
           session = session)
    
    sim[[paste(i)]] <- simulateDataset(data = data_mat,
                                       annotation = annot,
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
  saveRDS(sim, "h20_test.rds")
  status(detail = "Simulation Complete", value = 0.9, session = session)
  return(sim)
}

#### Classification #####
#' @title Run classification Algorithms
#' @description A wrapper function to either caret of h2o based classification 
#' algorithm, this function removes the overhead of having to do decision making
#' in a reactive environment of the shiny app and easier to debug
#' @param sim A list object containing the simulated data as returned by the
#' `simulate_grid()` function
#' @param inputs A shiny inputs objects containing user inputs, can also be a list
#' of named inputs 
#' @param use_h2o A logical value determining if to use h2o or not
#' @param seed A numeric value setting seed for reproducible experiments
#' @param session A session object for shiny, making interactive status updates
#' @return A classification object containing training models for each s
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
    
    classification <- ss_classify_caret(n_samp = inputs$n_samp_grp,
                                        sim_data = sim,
                                        classifier = inputs$classifier,
                                        session = session)
  }
  return(classification)
}


#' @title 
#' @description 
#' @param n_samp
#' @param sim_data
#' @param classifier
#' @param k
#' @param family
#' @param session
ss_classify_caret <- function(n_samp, sim_data, classifier, k = 10,
                              family = "binomial", session = NULL){
  samp <- unlist(strsplit(n_samp,","))
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

    valid <- cbind("condition" = sim_data[[i]]$valid_Y,
                   sim_data[[i]]$valid_X)

    if(length(unique(sim_data[[i]]$input_Y)) > 2){
      family <- "multinomial"
    }
    
    # use for loop instead of lapply or a parallel implementation of lapply
    # to provide the user with visual aid of the progress bar
    # TODO figure out a vectorized approach with status outputs to the UI 
    for(j in seq_along(list_x)){
      if(max_val == 0){
        max_val <- length(list_x) * length(samp)
      }
      iter = iter + 1/max_val
      
      status(detail = sprintf("Classifying Sample Size %s of %s, Simulation %s of %s",
                              i, length(samp), j, length(list_x)),
             session = session, value = iter)
      
      df <- cbind("condition" = list_y[[j]],
                  list_x[[j]])
      res[[paste0("Sim",j)]] <- classify(df = cbind("condition" = list_y[[j]],
                                                    list_x[[j]]), 
                                         val = valid, alg = classifier,
                                         family = family, k = k)
    }
    
    for(j in seq_along(res)){
      acc <- data.frame("Sample" = samp[i],"accuracy" = res[[j]]$accuracy)
      pred_acc <- append(list(acc), pred_acc)

      imp[[j]] <- data.table("Simulation" = j, res[[j]]$f_imp[1:k])
      model[[j]] <- res[[j]]$model
    }
    
    imp <- do.call("rbind", imp)
    # Get the top k most frequent proteins across all simulation runs
    imp_prots <- imp[,.N,rn]
    setorder(imp_prots,-N)
    imp_prots <- imp_prots[1:k, rn]
    imp <- imp[rn %in% imp_prots]
    
    #spreat out the protein importances and scale them 
    imp <- dcast(imp, rn~Simulation, value.var = "Overall")
    imp <- cbind("protein" = imp[,1],
                 "importance" = rowSums(imp[,-1], na.rm = T))
    imp[, importance := (importance-min(importance))/(max(importance) - min(importance))]

    models[[as.character(samp[i])]] <- model
    f_imp[[as.character(samp[i])]] <- imp
  }
  
  rm(list_x, list_y, valid, df, max_val, acc)
  
  return(list("samp" = as.numeric(samp), "pred_acc" = pred_acc, "f_imp" = f_imp))
}


#' @title Fit a classification Model
#' @description Fits a given classification model to the data provided
#' @param df A data.frame with the protein abundances and group information for 
#' training the model
#' @param val A data.frame with the protein abundances and group information for
#' validation of the model
#' @param alg The classifier to apply
#' @param family Family of function to apply (only for Logistic Regression)
#' @param k Number of features to select for model training
#' @return A named list containing the trained model, accuracy and raw features
#' importances
classify <- function(df, val, alg, family, k){
  
  switch(alg, rf = {
    tunegrid = data.frame(mtry = 2)
  }, nnet = {
    tunegrid = data.frame(size = 5, decay = 0.1)
  }, svmLinear = {
    tunegrid = data.frame(C = 1)
  }, naive_bayes = {
    tunegrid = data.frame(laplace = 0, usekernel = FALSE, 
                          adjust = 1)
  }, logreg = {
    tunegrid = data.frame(decay = 0.2)
  })
  
  if(family != "multinomial" && alg == 'logreg'){
    model <- caret::train(make.names(condition)~.,data = df,
                          method = 'glm', family = family,
                          trControl = caret::trainControl(method = "none",
                                                          classProbs = TRUE),
                          maxit = 1000)
    
    f_imp <- caret::varImp(model, scale = TRUE)
    i_ff <- data.table::as.data.table(f_imp$importance, keep.rownames = T)
    setorder(i_ff, -Overall)
    sel_imp <- i_ff[1:k][!is.na(rn), rn]
    if(!all(sel_imp %in% names(df))){
      sel_imp <- gsub("`|\\\\","",sel_imp)
    }
    model <- caret::train(make.names(condition)~.,
                          data = df[ ,c("condition", sel_imp)],
                          method = "glm",
                          trControl = caret::trainControl(method = "none",
                                                          classProbs = TRUE),
                          maxit = 1000)
  } else {
    if(alg == 'logreg'){
      alg <- 'multinom'
    }
    model <- caret::train(make.names(condition)~.,
                          data = df,
                          method = alg, 
                          trControl = caret::trainControl(method = "none", 
                                                          classProbs = TRUE),
                          tuneGrid = tunegrid, MaxNWts = 80000, maxit = 1000) 
    
    f_imp <- caret::varImp(model, scale = TRUE)
    i_ff <- data.table::as.data.table(f_imp$importance, keep.rownames = T)
    if(alg %in% c("svmLinear", "naive_bayes")){
      i_ff[, Overall := rowMeans(i_ff[, -1], na.rm = T)]
    }
    setorder(i_ff, -Overall)
    sel_imp <- i_ff[1:k][!is.na(rn), rn]
    
    # TODO figure out why make.names add certains special characters to the 
    # proteins check consistency of protein names
    if(!all(sel_imp %in% names(df))){
      sel_imp <- gsub("`|\\\\","",sel_imp)
    }
    model <- caret::train(make.names(condition)~.,
                          data = df[ ,c("condition", sel_imp)],
                          method = alg, 
                          trControl = caret::trainControl(method = "none", 
                                                          classProbs = TRUE),
                          tuneGrid = tunegrid) 
    
  }
  
  pred <- predict(model, val[-1])
  cm <- table(pred, val$condition)
  
  acc <- sum(diag(cm))/sum(cm)
  r_list <- list("accuracy" = acc, "model" = model, "f_imp" = i_ff)
  return(r_list)
}



#' @title Classify using h2o classification algorithms
#' @description This function enables the shiny application to apply various 
#' algorithms as defined by the user
#' @param n_samp A comma separated chracter vector with number of sample sizes 
#' @param sim_data A list object containing simulated data
#' @param classifier Classifier to use as selected by the user can use one of the
#' following `Logistic Regression`, `Random Forest`, `Support Vector Machine`,
#' `Naive Bayes`, `Neural Network`
#' @param stopping_metric 
#' @param seed
#' @param nfolds
#' @param fold_assignment
#' @param iters
#' @param alpha
#' @param family
#' @param solver
#' @param link
#' @param min_sdev
#' @param laplace
#' @param eps
#' @param session 
ss_classify_h2o <- function(n_samp, sim_data, classifier, stopping_metric = "AUTO",
                            seed = -1, nfolds = 0, fold_assignment = "AUTO", iters = 200,
                            alpha = 0, family, solver, link, min_sdev, laplace, eps,
                            session = NULL){
  samp <- unlist(strsplit(n_samp,","))
  config <- h2o_config()
  h2o::h2o.init(nthreads = config$threads, max_mem_size = config$max_mem,
                log_dir = config$log_dir, log_level = config$log_level)
  max_val <- 0
  iter <- 0
  modelz <- list()
  
  for(i in samp){
    
    valid_x <- sim_data[[i]]$valid_X
    valid_y <- sim_data[[i]]$valid_Y
    valid <- h2o::as.h2o(cbind("condition" = valid_y, valid_x),
                         destination_frame = "valid")
    
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
      train <- h2o::as.h2o(cbind("condition" = y, x), destination_frame = "train")
      
      if(classifier == "rf"){
        model <- h2o::h2o.randomForest(y = 1, training_frame = train,
                                       stopping_metric = stopping_metric, seed = seed, 
                                       balance_classes = FALSE, nfolds = nfolds,
                                       fold_assignment = fold_assignment,
                                       build_tree_one_node = T)
        var_imp <- h2o::h2o.varimp(model)
        sel_imp <- var_imp$variable[1:10]
        h2o::h2o.rm(train)
        train <- h2o::as.h2o(cbind("condition"=y,x[,sel_imp]), destination_frame = "train")
        model <- h2o::h2o.randomForest(y = 1, training_frame = train,
                                       validation_frame = valid,
                                       stopping_metric = stopping_metric, seed = seed, 
                                       balance_classes = FALSE, nfolds = nfolds,
                                       fold_assignment = fold_assignment,
                                       build_tree_one_node = T)
        
      } else if (classifier == "nnet"){
        l1 = 0
        l2 = 0
        rho = 0.99
        epochs = 10
        hidden = c(250,250)
        activation = "Rectifier"
        model <- h2o::h2o.deeplearning(y = 1, 
                                       training_frame = train,
                                       l1= l1, l2=l2,
                                       activation = activation,
                                       hidden = hidden,
                                       epochs = epochs)
        found_imp <- h2o::h2o.varimp(model)
        sel_imp <- found_imp$variable[1:10]
        train <- h2o::as.h2o(cbind("condition"=y,x[,sel_imp]), destination_frame = "train")
        model <- h2o::h2o.deeplearning(y = 1, training_frame = train,
                                       validation_frame = valid, l1 = l1, l2 = l2,
                                       activation = activation, hidden = hidden,
                                       epochs = epochs)
        
      } else if (classifier == "svmLinear"){
        model <- h2o::h2o.psvm(y = 1, training_frame = train, 
                               max_iterations = iters,
                               seed = seed, validation_frame = valid,
                               disable_training_metrics = F)
      } else if (classifier == "logreg"){
        if(length(unique(as.vector(train[,1]))) > 2){
          family <- "multinomial"
        }
        model <- h2o::h2o.glm(y = 1, training_frame = train,
                              seed = seed, family = family, alpha = alpha, 
                              nfolds = nfolds, solver = solver,
                              link = link)
        
        var_imp <- h2o::h2o.varimp(model)
        sel_imp <- var_imp$variable[1:10]
        h2o::h2o.rm(train)
        train <- h2o::as.h2o(cbind("condition"=y,x[,sel_imp]), destination_frame = "train")
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
      perf <- h2o::h2o.performance(model = model, newdata = valid)
      cm <- perf@metrics$cm$table[1:length(unique(y)),
                                  1:length(unique(y))]
      cm <- as.matrix(sapply(cm, as.numeric))
      #accuracy of current simulation
      acc <- sum(diag(cm))/sum(cm)
      
      name_val <- sprintf("Sample%s %s", i,  names(train_x_list)[index])
      var_imp <-  h2o::h2o.varimp(model)
      
      rm(train, perf, cm)
      
      modelz[[name_val]] <- list("model"= model, "acc" = acc,
                                 "var_imp" = var_imp)
    }
  }
  rm(valid, valid_x, valid_y, train_x_list, train_y_list, model)
  gc()
  h2o:::.h2o.garbageCollect()
  h2o::h2o.shutdown(prompt = F)
  
  return(list("models" = modelz))
}










