
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(ggplot2)
library(data.table)
library(dplyr)
library(naivebayes)
library(randomForest)
library(kernlab)
library(e1071)

source('functions.R')
FORMATS <- c("examples", "standard")
MODELS <- c('','rf','nnet','svmLinear','logreg','naive_bayes')
names(MODELS) <- c('',"Random Forest", "Neural Network", "Support Vector Machines with Linear Kernel",
                   "Logistic Regression", "Naive Bayes")

STOPPING_METRIC <- c("AUTO", "deviance", "logloss", "MSE", "RMSE", "MAE", "RMSLE",
                     "AUC", "lift_top_group", "misclassification", "AUCPR",
                     "mean_per_class_error", "custom", "custom_increasing")

FOLD_ASSIGNMENT <- c("AUTO", "Random", "Modulo", "Stratified")

DISTRIBUTION <- c("AUTO", "bernoulli", "multinomial", "gaussian", "poisson",
                  "gamma", "tweedie", "laplace", "quantile", "huber")

FAMILY <-  c("gaussian", "binomial", "quasibinomial", "ordinal", "multinomial",
           "poisson", "gamma", "tweedie", "negativebinomial")

SOLVER <- c("AUTO", "IRLSM", "L_BFGS", "COORDINATE_DESCENT_NAIVE", "COORDINATE_DESCENT",
           "GRADIENT_DESCENT_LH", "GRADIENT_DESCENT_SQERR")

LINK <- c("family_default", "identity", "logit", "log", "inverse", "tweedie", "ologit")

B_GROUP <- ""

# config <- h2o_config()
# h2o::h2o.init(nthreads = config$threads, max_mem_size = config$max_mem,
#               log_dir = config$log_dir, log_level = config$log_level)