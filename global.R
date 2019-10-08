
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(markdown)
library(shinyjs)
library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(viridis)
library(MSstatsBioData)
library(MSstats)
library(caret)
library(plotly)
library(future)
library(promises)
library(doParallel)

MSstatsPackages <- c("lme4", "marray", "limma", "gplots", "ggplot2", "methods", "grid", "ggrepel",
                   "preprocessCore", "reshape2", "survival", "statmod", "minpack.lm", "utils",
                   "grDevices", "graphics", "stats", "doSNOW", "snow", "foreach", "data.table",
                   "MASS", "dplyr", "tidyr", "broom", "purrr", "stringr", "randomForest")
lapply(MSstatsPackages, library, character.only = TRUE)
#source("scripts/convert_to_MSstats.R")
#source("scripts/ParSampleSizeCalculationClassification.R")
#source("scripts/GroupComparison.R")
#source("scripts/methods.R")

library(lime)
