source("global.R")
source("functions.R")

session <- NULL

rv <- list()
rv$seed <- -1
rv$use_h2o <- F

input <- list()
input$data_format <- "standard"
input$n_sim <- 10
input$prot_prop <- 1
input$n_samp_grp <- "5,10,20"
input$sim_val <- F
input$sel_sim_prot <- "proportion"
input$classifier <- "rf"
input$family <- "binomial"
input$solver <- "AUTO"
input$link <- "family_default"
input$min_sdev <- 1
input$laplace <- 1
input$eps <- 1

files <- list.files(paste0(getwd(),"/MSstatsSampleSize-Data/CRC-DDA/"), full.names = T)
exp_fc <- 'data'
count <- list(files[2])
annot <- list(files[1])
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

#assign(paste("orca", i, sep = ""), list_name[[i]])

system.time({
  d <- sample_size_classification(n_samp = input$n_samp_grp,
                                  sim_data = sim_data,
                                  classifier = input$classifier,
                                  session = session)
})na
# # user  system elapsed 
# # 22.20    0.32   22.89 
# 
system.time({
  d <- ss_classify_h2o(n_samp = input$n_samp_grp, sim_data = sim_data,
                       classifier = input$classifier,
                       stopping_metric = input$stop_metric,
                       nfolds = 0,
                       fold_assignment = input$f_assignment, iters = input$iters,
                       family = input$family, solver = input$solver,
                       link = input$link, min_sdev = input$min_sdev,
                       laplace = input$laplace, eps = input$eps_sdev,
                       seed = -1, session = session)
})
h2o::h2o.shutdown(prompt = F)
# user  system elapsed 
# 13.91    0.52   54.53 
