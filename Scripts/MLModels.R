#First run preprocessing data

#Required Packages
library(keras)
library(splitstackshape)

#Setting working directory
setwd("/Users/hwarden/Documents/University/Maths/Year4/Project/RCode/MachineLearningCode")

#Loading required data
#TODO convert data to RData files
load("Data/MouseFeatures")
load("Data/HumanFeatures")
load("Data/MouseIdentity")
load("Data/HumanIdentity")
load("Data/MouseSpecimen")
load("Data/HumanSpecimen")

#Splitting data by specimen
x.mouse <- list(spec1 = x.mouse[which(specimen.mouse == "M1"),], spec2 = x.mouse[which(specimen.mouse == "M2"),], spec3 = x.mouse[which(specimen.mouse == "M3"),], full = x.mouse)
x.human <- list(spec1 = x.human[which(specimen.human == "H1"),], spec2 = x.human[which(specimen.human == "H2"),], spec3 = x.human[which(specimen.human == "H3"),], full = x.human)

#Generating training and testing indices via stratified sampling
train_ind <- list(mouse = list(), human = list())
train_ind$mouse[[1]] <- stratified(data.frame(index = 1:nrow(x.mouse$spec1), identity = y.mouse[which(specimen.mouse == "M1")]), group = 2, size = 0.8)$index
train_ind$mouse[[2]] <- stratified(data.frame(index = 1:nrow(x.mouse$spec2), identity = y.mouse[which(specimen.mouse == "M2")]), group = 2, size = 0.8)$index
train_ind$mouse[[3]] <- stratified(data.frame(index = 1:nrow(x.mouse$spec3), identity = y.mouse[which(specimen.mouse == "M3")]), group = 2, size = 0.8)$index
train_ind$human[[1]] <- stratified(data.frame(index = 1:nrow(x.human$spec1), identity = y.human[which(specimen.human == "H1")]), group = 2, size = 0.8)$index
train_ind$human[[2]] <- stratified(data.frame(index = 1:nrow(x.human$spec2), identity = y.human[which(specimen.human == "H2")]), group = 2, size = 0.8)$index
train_ind$human[[3]] <- stratified(data.frame(index = 1:nrow(x.human$spec3), identity = y.human[which(specimen.human == "H3")]), group = 2, size = 0.8)$index

#Splitting the data sets into testing and training data
x.mouse$spec1 <- list(train = x.mouse$spec1[train_ind$mouse[[1]],], test = x.mouse$spec1[-train_ind$mouse[[1]],])
x.mouse$spec2 <- list(train = x.mouse$spec2[train_ind$mouse[[2]],], test = x.mouse$spec2[-train_ind$mouse[[2]],])
x.mouse$spec3 <- list(train = x.mouse$spec3[train_ind$mouse[[3]],], test = x.mouse$spec3[-train_ind$mouse[[3]],])
x.human$spec1 <- list(train = x.human$spec1[train_ind$human[[1]],], test = x.human$spec1[-train_ind$human[[1]],])
x.human$spec2 <- list(train = x.human$spec2[train_ind$human[[2]],], test = x.human$spec2[-train_ind$human[[2]],])
x.human$spec3 <- list(train = x.human$spec3[train_ind$human[[3]],], test = x.human$spec3[-train_ind$human[[3]],])

#Splitting the ground truth by specimen
y.mouse.class <- list(spec1 = y.mouse[which(specimen.mouse == "M1")], spec2 = y.mouse[which(specimen.mouse == "M2")], spec3 = y.mouse[which(specimen.mouse == "M3")], full = y.mouse)
y.human.class <- list(spec1 = y.human[which(specimen.human == "H1")], spec2 = y.human[which(specimen.human == "H2")], spec3 = y.human[which(specimen.human == "H3")], full = y.human)

#Splitting the ground truth into testing and training sets
y.mouse.class$spec1 <- list(train = y.mouse.class$spec1[train_ind$mouse[[1]]], test = y.mouse.class$spec1[-train_ind$mouse[[1]]])
y.mouse.class$spec2 <- list(train = y.mouse.class$spec2[train_ind$mouse[[2]]], test = y.mouse.class$spec2[-train_ind$mouse[[2]]])
y.mouse.class$spec3 <- list(train = y.mouse.class$spec3[train_ind$mouse[[3]]], test = y.mouse.class$spec3[-train_ind$mouse[[3]]])
y.human.class$spec1 <- list(train = y.human.class$spec1[train_ind$human[[1]]], test = y.human.class$spec1[-train_ind$human[[1]]])
y.human.class$spec2 <- list(train = y.human.class$spec2[train_ind$human[[2]]], test = y.human.class$spec2[-train_ind$human[[2]]])
y.human.class$spec3 <- list(train = y.human.class$spec3[train_ind$human[[3]]], test = y.human.class$spec3[-train_ind$human[[3]]])

#Splitting the ground truth by specimen - as a categorical variable
y.mouse <- list(spec1 = to_categorical(as.numeric(y.mouse[which(specimen.mouse == "M1")])-1, 11), spec2 = to_categorical(as.numeric(y.mouse[which(specimen.mouse == "M2")])-1, 11), spec3 = to_categorical(as.numeric(y.mouse[which(specimen.mouse == "M3")])-1, 11), full = to_categorical(as.numeric(y.mouse)-1, 11))
y.human <- list(spec1 = to_categorical(as.numeric(y.human[which(specimen.human == "H1")])-1, 11), spec2 = to_categorical(as.numeric(y.human[which(specimen.human == "H2")])-1, 11), spec3 = to_categorical(as.numeric(y.human[which(specimen.human == "H3")])-1, 11), full = to_categorical(as.numeric(y.human)-1, 11))

#Splitting the ground truth into testing and training sets - as a categorical variable
y.mouse$spec1 <- list(train = y.mouse$spec1[train_ind$mouse[[1]],], test = y.mouse$spec1[-train_ind$mouse[[1]],])
y.mouse$spec2 <- list(train = y.mouse$spec2[train_ind$mouse[[2]],], test = y.mouse$spec2[-train_ind$mouse[[2]],])
y.mouse$spec3 <- list(train = y.mouse$spec3[train_ind$mouse[[3]],], test = y.mouse$spec3[-train_ind$mouse[[3]],])
y.human$spec1 <- list(train = y.human$spec1[train_ind$human[[1]],], test = y.human$spec1[-train_ind$human[[1]],])
y.human$spec2 <- list(train = y.human$spec2[train_ind$human[[2]],], test = y.human$spec2[-train_ind$human[[2]],])
y.human$spec3 <- list(train = y.human$spec3[train_ind$human[[3]],], test = y.human$spec3[-train_ind$human[[3]],])

#Saving data sets
save(y.mouse.class, y.human.class, file = "Data/MLClasses.RData")

#Creating ML models
ml.models <- list(mouse = list(), human = list())

#Function to build required ML model
build_model <- function(in_shape) {
  # define model
  model <- keras_model_sequential() %>%
    layer_dense(units = 11,
                activation = "softmax",
                input_shape = in_shape)
  
  # compile model
  model %>% compile(
    loss = "categorical_crossentropy",
    optimizer = "rmsprop",
    metrics = list("accuracy") )
  
  # return model
  return(model)
}

#Building model for each individual
for (i in 1:3){
  ml.models$mouse[[i]] <- build_model(4311)
  ml.models$human[[i]] <- build_model(4311)
}

#Fitting each of the models
for (i in 1:3){
  ml.models$mouse[[i]] %>% fit(x.mouse[[i]]$train, y.mouse[[i]]$train)
  ml.models$human[[i]] %>% fit(x.human[[i]]$train, y.human[[i]]$train)
}

#Lists to save softmax and class outputs
ml.sftmx <- list(mouse = list(list(), list(), list()), human = list(list(), list(), list()), mouse_human = list(list(), list(), list()), human_mouse = list(list(), list(), list()))
ml.class <- list(mouse = list(list(), list(), list()), human = list(list(), list(), list()), mouse_human = list(list(), list(), list()), human_mouse = list(list(), list(), list()))

#Filling in the softmax outputs and classification estimates
for (i in 1:3){
  for (j in 1:3){
    ml.sftmx$mouse[[i]][[j]] <- ml.models$mouse[[i]] %>% predict_proba(x.mouse[[j]]$test)
    ml.sftmx$human[[i]][[j]] <- ml.models$human[[i]] %>% predict_proba(x.human[[j]]$test)
    ml.sftmx$mouse_human[[i]][[j]] <- ml.models$mouse[[i]] %>% predict_proba(x.human[[j]]$test)
    ml.sftmx$human_mouse[[i]][[j]] <- ml.models$human[[i]] %>% predict_proba(x.mouse[[j]]$test)
    
    ml.class$mouse[[i]][[j]] <- ml.models$mouse[[i]] %>% predict_classes(x.mouse[[j]]$test)
    ml.class$human[[i]][[j]] <- ml.models$human[[i]] %>% predict_classes(x.human[[j]]$test)
    ml.class$mouse_human[[i]][[j]] <- ml.models$mouse[[i]] %>% predict_classes(x.human[[j]]$test)
    ml.class$human_mouse[[i]][[j]] <- ml.models$human[[i]] %>% predict_classes(x.mouse[[j]]$test)
  }
}

#Saving softmax outputs and classification estimates
save(ml.sftmx, ml.class, file = "Data/MLResults.RData")
save(x.mouse, y.mouse, x.human, y.human, train_ind, file = "Data/MLData.RData")

#Saving the ML models
for (i in 1:3){
  save_model_tf(ml.models$mouse[[i]], file = paste0("Models/Mouse",i))
  save_model_tf(ml.models$human[[i]], file = paste0("Models/Human",i))
}

