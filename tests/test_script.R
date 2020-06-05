source("global.R")
source("functions.R")

session <- NULL

rv <- list()
rv$seed <- -1
rv$use_h2o <- T

input <- list()
input$data_format <- "standard"
input$n_sim <- 50
input$prot_prop <- 1
input$n_samp_grp <- "5,10,20,50,100,200"
input$sim_val <- F
input$sel_sim_prot <- "proportion"
#input$classifier <- "rf"
input$family <- "binomial"
input$solver <- "AUTO"
input$link <- "family_default"
input$min_sdev <- 1
input$laplace <- 1
input$eps <- 1

exp_fc <- 'data'
annot <- count <- list()

classifier <- c("svmLinear","nnet")

library(h2o)
files <- list.files(paste0(getwd(),"/MSstatsSampleSize-Data/"), full.names = T)
x <- files[6]


lapply(classifier, function(y){
    lapply(files[c(2:3,5)], function(x){
      file <- list.files(x, full.names = T)
      if(length(file) > 2){
        annot$datapath <- file[grepl('train_annotation',file)]
        count$datapath <- file[grepl('train_proteinAbundance', file)]
      }else{
        annot$datapath <- file[grepl('annotation',file)]
        count$datapath <- file[grepl('proteinAbundance', file)]
      }
      
      data <- explore_data(format = input$data_format, count = count,
                           annot = annot)
      B_GROUP <- data$annot_data[,unique(Condition)] 
      
      
      
      sim_data <- simulate_grid(data = data$wide_data,
                                annot = data$annot_data,
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
      
      st <- Sys.time()
      d <- sample_size_classification(n_samp = input$n_samp_grp,
                                      sim_data = sim_data,
                                      classifier = 'rf',
                                      par =T,
                                      session = session)
      et <- Sys.time()
      pl1 <- plot_acc(data = d, use_h2o = F, alg = y)
      t1 <- difftime(et,st, units = 'mins')
      rm(d)
      st <- Sys.time()
      d <- ss_classify_h2o(n_samp = input$n_samp_grp, sim_data = sim_data,
                           classifier = 'rf',
                           stopping_metric = input$stop_metric,
                           nfolds = 0,
                           fold_assignment = input$f_assignment, iters = input$iters,
                           family = input$family, solver = input$solver,
                           link = input$link, min_sdev = input$min_sdev,
                           laplace = input$laplace, eps = input$eps_sdev,
                           seed = -1, session = session)
      et <- Sys.time()
      t2 <- difftime(et,st,units = 'mins')
      pl2 <- plot_acc(data = d, use_h2o = T, alg = y)
      rm(d)
      gc()
      rdslist <- list(pl1 = pl1, pl2 = pl2, t1 = t1, t2 = t2)
      name <- unlist(strsplit(x,'/'))
      saveRDS(rdslist, sprintf("results/%s_%s", y , name[length(name)]))
    })
#  }, error = function(e) NULL)
})

#assign(paste("orca", i, sep = ""), list_name[[i]])
# 
# system.time({
#   d <- sample_size_classification(n_samp = input$n_samp_grp,
#                                   sim_data = sim_data,
#                                   classifier = input$classifier,
#                                   session = session)
# })na
# # # user  system elapsed 
# # # 22.20    0.32   22.89 
# # 
# system.time({
#   d <- ss_classify_h2o(n_samp = input$n_samp_grp, sim_data = sim_data,
#                        classifier = input$classifier,
#                        stopping_metric = input$stop_metric,
#                        nfolds = 0,
#                        fold_assignment = input$f_assignment, iters = input$iters,
#                        family = input$family, solver = input$solver,
#                        link = input$link, min_sdev = input$min_sdev,
#                        laplace = input$laplace, eps = input$eps_sdev,
#                        seed = -1, session = session)
# })
# h2o::h2o.shutdown(prompt = F)
# # user  system elapsed 
# # 13.91    0.52   54.53 
