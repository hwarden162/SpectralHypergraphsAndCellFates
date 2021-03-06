#Loading required libraries
library(yardstick)
library(caret)

#Loading data
load("Data/MLResults.RData")
load("Data/MLClasses.RData")

#Generating list to store balanced accuracies
bal_accs <- list(mm = list(list(), list(), list()), mh = list(list(), list(), list()), hm = list(list(), list(), list()), hh = list(list(), list(), list()))

#Generating list to store confusion matrices
conf_mat <- list(mm = list(list(), list(), list()), mh = list(list(), list(), list()), hm = list(list(), list(), list()), hh = list(list(), list(), list()))

#Creating confusion matrices
for (i in 1:3){
  for (j in 1:3){
    conf_mat$mm[[i]][[j]] <- confusionMatrix(as.factor(levels(y.mouse.class$full)[ml.class$mouse[[i]][[j]]+1]), y.mouse.class[[j]]$test)
    conf_mat$mh[[i]][[j]] <- confusionMatrix(as.factor(levels(y.mouse.class$full)[ml.class$mouse_human[[i]][[j]]+1]), y.human.class[[j]]$test)
    conf_mat$hm[[i]][[j]] <- confusionMatrix(as.factor(levels(y.human.class$full)[ml.class$human_mouse[[i]][[j]]+1]), y.mouse.class[[j]]$test)
    conf_mat$hh[[i]][[j]] <- confusionMatrix(as.factor(levels(y.human.class$full)[ml.class$human[[i]][[j]]+1]), y.human.class[[j]]$test)
  }
}

#Finding balanced accuracies
for (i in 1:3){
  for (j in 1:3){
    bal_accs$mm[[i]][[j]] <- bal_accuracy(conf_mat$mm[[i]][[j]]$table)$.estimate
    bal_accs$mh[[i]][[j]] <- bal_accuracy(conf_mat$mh[[i]][[j]]$table)$.estimate
    bal_accs$hm[[i]][[j]] <- bal_accuracy(conf_mat$hm[[i]][[j]]$table)$.estimate
    bal_accs$hh[[i]][[j]] <- bal_accuracy(conf_mat$hh[[i]][[j]]$table)$.estimate
  }
}
