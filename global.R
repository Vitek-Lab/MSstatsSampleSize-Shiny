suppressMessages({
  library(shiny)
  library(shinydashboard)
  library(shinyWidgets)
  library(shinycssloaders)
  library(ggplot2)
  library(data.table)
  library(dplyr)
  library(MSstatsSampleSize)
  #### Caret package dependencies ####
  library(e1071)
  library(kernlab)
  library(naivebayes)
  library(randomForest)
})
source('functions.R')


#### logging file ####
LOG_DIR <- file.path(getwd(),'logs')
dir.create(LOG_DIR, showWarnings = F)
LOG_FILE <- sprintf("MSstats_SS_%s.Rout",
                    format(Sys.time(),"%Y%m%d%H%M%S"))
LOG_FILE <- file.path(LOG_DIR,LOG_FILE)
FILE_CONN <- file(LOG_FILE, open='wt')
writeLines(capture.output(sessionInfo()), FILE_CONN)
writeLines("\n\n ############## LOG ############# \n\n", FILE_CONN)

#### GLOBAL VARS ####
FORMATS_LIST <- list("Protein-level quantification" = "standard", 
                     "Example from MSstatsSampleSize" = "examples")
FORMATS <- c("examples", "standard")
EXTENSTIONS <- c("text/csv",
                 "text/comma-separated-values,text/plain",
                 ".csv", "text/tab-separated-values", ".tsv")

MODELS <- c('rf','nnet','svmLinear','logreg','naive_bayes')
names(MODELS) <- c("Random Forest", "Neural Network",
                   "Support Vector Machines with Linear Kernel",
                   "Logistic Regression", "Naive Bayes")

STOPPING_METRIC <- c("AUTO", "deviance", "logloss", "MSE", "RMSE", "MAE", "RMSLE",
                     "AUC", "lift_top_group", "misclassification", "AUCPR",
                     "mean_per_class_error")

FOLD_ASSIGNMENT <- c("AUTO", "Random", "Modulo", "Stratified")


FAMILY <-  c("gaussian", "binomial", "quasibinomial", "ordinal", "multinomial",
           "poisson", "gamma", "tweedie", "negativebinomial")

SOLVER <- c("AUTO", "IRLSM", "L_BFGS", "COORDINATE_DESCENT_NAIVE", 
            "COORDINATE_DESCENT", "GRADIENT_DESCENT_LH", "GRADIENT_DESCENT_SQERR")

LINK <- c("family_default", "identity", "logit", "log", "inverse", "tweedie",
          "ologit")

B_GROUP <- ""
TUNING <- c("Use h2o Package", "Parameter Tuning")
CURRMODEL <- ""
SIM_CHOICES <- 0

CSS_BUTTON <- "margin-top: 25px;
              display: inline-block;
              color: #fff;
              background-color: orange;
              border-color: black;
              font-size : 20px;"

CSS_BUTTON_REG <- "background-color: orange;
                  border-color: black;
                  color: #fff;"

CSS_BUTTON_RUN <- "background-color: orange;
                  border-color: black;
                  color: #fff;
                  margin-top:25px;"
