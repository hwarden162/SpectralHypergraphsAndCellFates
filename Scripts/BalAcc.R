library(yardstick)
library(caret)

load("/Users/hugh/Documents/University/Maths/Year4/Project/RCode/MachineLearningCode/Data/MLResults.RData")
load("/Users/hugh/Documents/University/Maths/Year4/Project/RCode/MachineLearningCode/Data/MLClasses.RData")

bal_accs <- list(mm = list(list(), list(), list()), mh = list(list(), list(), list()), hm = list(list(), list(), list()), hh = list(list(), list(), list()))

conf_mat <- list(mm = list(list(), list(), list()), mh = list(list(), list(), list()), hm = list(list(), list(), list()), hh = list(list(), list(), list()))

for (i in 1:3){
  for (j in 1:3){
    conf_mat$mm[[i]][[j]] <- confusionMatrix(as.factor(levels(y.mouse.class$full)[ml.class$mouse[[i]][[j]]+1]), y.mouse.class[[j]]$test)
    conf_mat$mh[[i]][[j]] <- confusionMatrix(as.factor(levels(y.mouse.class$full)[ml.class$mouse_human[[i]][[j]]+1]), y.human.class[[j]]$test)
    conf_mat$hm[[i]][[j]] <- confusionMatrix(as.factor(levels(y.human.class$full)[ml.class$human_mouse[[i]][[j]]+1]), y.mouse.class[[j]]$test)
    conf_mat$hh[[i]][[j]] <- confusionMatrix(as.factor(levels(y.human.class$full)[ml.class$human[[i]][[j]]+1]), y.human.class[[j]]$test)
  }
}

for (i in 1:3){
  for (j in 1:3){
    bal_accs$mm[[i]][[j]] <- bal_accuracy(conf_mat$mm[[i]][[j]]$table)$.estimate
    bal_accs$mh[[i]][[j]] <- bal_accuracy(conf_mat$mh[[i]][[j]]$table)$.estimate
    bal_accs$hm[[i]][[j]] <- bal_accuracy(conf_mat$hm[[i]][[j]]$table)$.estimate
    bal_accs$hh[[i]][[j]] <- bal_accuracy(conf_mat$hh[[i]][[j]]$table)$.estimate
  }
}
