library(caret)
library(doParallel)
library(ggplot2)
library(gridExtra)

# load('data/sample.annotation.rda')
# ## !! sample annotation : required 'BioReplicate' and 'Condition'
# CRC_DDA_annotation <- sample.annotation
# save(CRC_DDA_annotation, file = "CRC_DDA_annotation.rda")
#
# load('data/spectral.data.rda')
# ## !! note : need to make 'protein id' in rownames in the default
# ## !! and should be matched with 'BioReplicate' column in annotation
# CRC_DDA <- spectral.data
# save(CRC_DDA, file = "CRC_DDA.rda")
#
# data <- spectral.data
# annotation <- sample.annotation
##########################################################################################
## todo : this part should be in datasetSimulation function
#data <- data[, sample.annotaion$CPTAC.sample.ID]
#group <- sample.annotaion$Cancer

library(MSstatsSampleSize)
data <- OV_SRM_train
# write.csv(data, file = "OV_SRM_train.csv")
# data <- read.csv(file = "OV_SRM_train.csv")
# rownames(data) <- data$Protein
# data <- data[, colnames(data)!="Protein"]

annotation <- OV_SRM_train_annotation

variance_estimation <- estimateVar(data, annotation)
save(variance_estimation, file = "variance_estimation.rda" )
meanSDplot(variance_estimation)

designSampleSizeHypothesisTesting(data = OV_SRM_train,
                                  annotation= OV_SRM_train_annotation,
                                  desiredFC = "data",
                                  select_testing_proteins = "proportion",
                                  protein_proportion = 1.0,
                                  protein_number = 1000,
                                  FDR=0.05,
                                  power=0.9)


##################################################
############## simulation ########################
##################################################
# fold change estimated from data
simulated_datasets <- simulateDataset(data,
                                      annotation,
                                      num_simulations = 10,
                                      expected_FC = "data",
                                      list_diff_proteins =  NULL,
                                      select_simulated_proteins = "proportion",
                                      protein_proportion = 1.0,
                                      protein_number = 1000,
                                      samples_per_group = 50,
                                      simulate_valid = FALSE,
                                      valid_samples_per_group = 50)

save(simulated_datasets, file = "simulated_datasets.rda")

# names(simulated_datasets)
# attributes(simulated_datasets$simulation_train_Xs)
#
# class(simulated_datasets$simulation_train_Xs$Simulation1)
# head(simulated_datasets$simulation_train_Xs$Simulation1)
#
# attributes(simulated_datasets$simulation_train_Ys)
# class(simulated_datasets$simulation_train_Ys$Simulation1)
#
# attributes(simulated_datasets$valid_X)
# head(simulated_datasets$valid_X[1, ])
# dim(simulated_datasets$valid_X)
# View(simualted_datasets$valid_X)
#
# attributes(simulated_datasets$valid_Y)
# head(simulated_datasets$valid_Y)

classification_results <- designSampleSizeClassification(simulations = simualted_datasets)
save(classification_results, file = "classification_results.rda")

classification_results_parallel <- designSampleSizeClassification(simulations = simualted_datasets,
                                       threads = 4)
save(classification_results_parallel, file = "classification_results_parallel.rda")

##################################################
################### PCA plot #####################
##################################################
designSampleSizePCAplot(simulated_datasets)

##################################################
#######user defined fold change simulation########
##################################################
# user defined fold change
unique(annotation$Condition)
expected_FC <- c(1, 1.2, 2)
names(expected_FC) <- c("benign", "control", "ovarian cancer")
set.seed(1212)
diff_proteins <- sample(rownames(data), 20)
simualted_datasets_predefined_FC <- simulateDataset(data,
                                      annotation,
                                      num_simulations = 10,
                                      expected_FC = expected_FC,
                                      list_diff_proteins =  diff_proteins,
                                      select_simulated_proteins = "proportion",
                                      protein_proportion = 1.0,
                                      protein_number = 1000,
                                      samples_per_group = 50,
                                      simulate_valid = FALSE,
                                      valid_samples_per_group = 50)

save(simualted_datasets_predefined_FC, file = "simualted_datasets_predefined_FC.rda")

##########################################################################################
#### sample size classification ####
multiple_sample_sizes <- list()
list_samples_per_group <- c(25, 50, 75, 100)

## toTing, list protein number can be happened??
for(i in 1:length(list_samples_per_group)){
    simualted_datasets <- simulateDataset(data,
                                          annotation,
                                          num_simulations = 10,
                                          expected_FC = "data",
                                          list_diff_proteins =  NULL,
                                          select_simulated_proteins = "proportion",
                                          protein_proportion = 1.0,
                                          protein_number = 1000,
                                          samples_per_group = 50,
                                          simulate_valid = FALSE,
                                          valid_samples_per_group = 50)

    res <- designSampleSizeClassification(simulations = simualted_datasets)
    multiple_sample_sizes[[i]] <- res
}
save(multiple_sample_sizes, file = "data/multiple_sample_sizes.rda")

designSampleSizeClassificationPlots(multiple_sample_sizes,
                                    list_samples_per_group,
                                    )


