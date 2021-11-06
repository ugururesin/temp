# LIBRARIES
##########################
library(caret)
library(e1071)
library(randomForest)

# CREATING IMBALANCED DATA
##########################
# Creating the feature vectors
vec1 <- rep("YES",100)
vec2 <- rep("YES",100)
vec3 <- rep("OK",100)

# Randomly assigning some "NO"s and "NOK"s into the Vectors to generate imbalanced data
vec1[sample(1:100,5)] <- "NO"
vec2[sample(1:100,5)] <- "NO"
vec3[sample(1:100,5)] <- "NOK"

# Creating the dataframe
dataset <- data.frame(X1=vec1, X2=vec2, Y=vec3)
summary(dataset)

# Sourcing ML model functions
setwd("/Users/ugururessn/Desktop/MYGIT/temp")
source("./caret_functions.R")

# Splitting the dataset
set.seed(1000)
dataset_splitted <- partition(dataset, factor=dataset$Y, trainsize=0.8)
summary(dataset_splitted$train)
summary(dataset_splitted$test)

# Creating ML model
ml_algorithm  <- "rf"
num_cv_rep    <- 5
tune_length   <- 20
show_train    <- TRUE

### TRAINING THE MODEL
msg_training = paste("\n", ml_algorithm, "algoritması egitiliyor...\n", sep=" ")
model_ml <- train_model(dataset_splitted$train, ml_algorithm, num_cv_rep, tune_length, show_train, msg_training)
plot(model_ml)

### ACCURACY CALCULATION & PRINTING
acc_model <- 100*get_accuracy(model_ml, dataset_splitted$test)
cat(sprintf("MODEL SONUCLARI:
            Model Egitim Basarisi: %%%#.1f\nModel Test Basarisi: %%%#.1f\n",
            acc_model[1], acc_model[2]))
