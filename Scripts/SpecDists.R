load("/Users/hugh/Documents/University/Maths/Year4/Project/RCode/MachineLearningCode/Data/MLResults.RData")

library(RSpectra)

get_degree_matrix <- function(inc_mat) {
  return(diag(nrow(inc_mat))*apply(inc_mat^2, 1, sum))
}

get_hyperedge_normalised_laplacian <- function(inc_mat) {
  deg_mat <- get_degree_matrix(inc_mat)
  diag(deg_mat) <- diag(deg_mat)^-1
  return(t(inc_mat)%*%deg_mat%*%inc_mat)
}

get_eigenvalues <- function(inc_mat) {
  inc_mat <- as.matrix(inc_mat)
  lap_mat <- get_hyperedge_normalised_laplacian(inc_mat)
  num <- min(dim(inc_mat))
  eigen <- eigs_sym(lap_mat, num)
  return(eigen$values)
}

get_spectral_distance <- function(ev1, ev2) {
  ev1 <- sort(ev1, decreasing = TRUE)
  ev2 <- sort(ev2, decreasing = TRUE)
  n_1 <- length(ev1)
  n_2 <- length(ev2)
  if (n_1 > n_2) {
    diff <- n_1 - n_2
    ev2 <- c(ev2, rep(0, diff))
  }
  if (n_2 > n_1) {
    diff <- n_2 - n_1
    ev1 <- c(ev1, rep(0, diff))
  }
  ev <- sum(abs(ev1 - ev2))
  return(ev)
}

get_spectral_distance_disconnected <- function(ev){
  return(get_spectral_distance(ev, rep(1, length(ev))))
}

hypergraphs <- list(mouse_mouse = list(primal = list(list(), list(), list()), dual = list(list(), list(), list())), mouse_human = list(primal = list(list(), list(), list()), dual = list(list(), list(), list())), human_mouse = list(primal = list(list(), list(), list()), dual = list(list(), list(), list())), human_human = list(primal = list(list(), list(), list()), dual = list(list(), list(), list())))

for (i in 1:3){
  for (j in 1:3){
    hypergraphs$mouse_mouse$primal[[i]][[j]] <- ml.sftmx$mouse[[i]][[j]]
    hypergraphs$mouse_human$primal[[i]][[j]] <- ml.sftmx$mouse_human[[i]][[j]]
    hypergraphs$human_mouse$primal[[i]][[j]] <- ml.sftmx$human_mouse[[i]][[j]]
    hypergraphs$human_human$primal[[i]][[j]] <- ml.sftmx$human[[i]][[j]]
    
    hypergraphs$mouse_mouse$dual[[i]][[j]] <- t(ml.sftmx$mouse[[i]][[j]])
    hypergraphs$mouse_human$dual[[i]][[j]] <- t(ml.sftmx$mouse_human[[i]][[j]])
    hypergraphs$human_mouse$dual[[i]][[j]] <- t(ml.sftmx$human_mouse[[i]][[j]])
    hypergraphs$human_human$dual[[i]][[j]] <- t(ml.sftmx$human[[i]][[j]])
  }
}

eigenvalues <- list(mouse_mouse = list(primal = list(list(), list(), list()), dual = list(list(), list(), list())), mouse_human = list(primal = list(list(), list(), list()), dual = list(list(), list(), list())), human_mouse = list(primal = list(list(), list(), list()), dual = list(list(), list(), list())), human_human = list(primal = list(list(), list(), list()), dual = list(list(), list(), list())))

for (i in 1:3){
  for (j in 1:3){
    eigenvalues$mouse_mouse$primal[[i]][[j]] <- get_eigenvalues(hypergraphs$mouse_mouse$primal[[i]][[j]])
    eigenvalues$mouse_human$primal[[i]][[j]] <- get_eigenvalues(hypergraphs$mouse_human$primal[[i]][[j]])
    eigenvalues$human_mouse$primal[[i]][[j]] <- get_eigenvalues(hypergraphs$human_mouse$primal[[i]][[j]])
    eigenvalues$human_human$primal[[i]][[j]] <- get_eigenvalues(hypergraphs$human_human$primal[[i]][[j]])
    
    eigenvalues$mouse_mouse$dual[[i]][[j]] <- get_eigenvalues(hypergraphs$mouse_mouse$dual[[i]][[j]])
    eigenvalues$mouse_human$dual[[i]][[j]] <- get_eigenvalues(hypergraphs$mouse_human$dual[[i]][[j]])
    eigenvalues$human_mouse$dual[[i]][[j]] <- get_eigenvalues(hypergraphs$human_mouse$dual[[i]][[j]])
    eigenvalues$human_human$dual[[i]][[j]] <- get_eigenvalues(hypergraphs$human_human$dual[[i]][[j]])
  }
}

spectral_distances <- list(primal = list(), dual = list(), disconnected = list())

spectral_distances$primal$mm <- list(mm = list(list(),list(),list()), mh = list(list(),list(),list()), hm = list(list(),list(),list()), hh = list(list(),list(),list()))
spectral_distances$primal$mh <- list(mm = list(list(),list(),list()), mh = list(list(),list(),list()), hm = list(list(),list(),list()), hh = list(list(),list(),list()))
spectral_distances$primal$hm <- list(mm = list(list(),list(),list()), mh = list(list(),list(),list()), hm = list(list(),list(),list()), hh = list(list(),list(),list()))
spectral_distances$primal$hh <- list(mm = list(list(),list(),list()), mh = list(list(),list(),list()), hm = list(list(),list(),list()), hh = list(list(),list(),list()))

spectral_distances$dual$mm <- list(mm = list(list(),list(),list()), mh = list(list(),list(),list()), hm = list(list(),list(),list()), hh = list(list(),list(),list()))
spectral_distances$dual$mh <- list(mm = list(list(),list(),list()), mh = list(list(),list(),list()), hm = list(list(),list(),list()), hh = list(list(),list(),list()))
spectral_distances$dual$hm <- list(mm = list(list(),list(),list()), mh = list(list(),list(),list()), hm = list(list(),list(),list()), hh = list(list(),list(),list()))
spectral_distances$dual$hh <- list(mm = list(list(),list(),list()), mh = list(list(),list(),list()), hm = list(list(),list(),list()), hh = list(list(),list(),list()))

spectral_distances$disconnected <- list(mm = list(), mh = list(), hm = list(), hh = list())
spectral_distances$disconnected$mm <- list(list(), list(), list())
spectral_distances$disconnected$mh <- list(list(), list(), list())
spectral_distances$disconnected$hm <- list(list(), list(), list())
spectral_distances$disconnected$hh <- list(list(), list(), list())

for (i in 1:3){
  for (ii in 1:3){
    spectral_distances$primal$mm$mm[[i]][[ii]] <- list(list(),list(),list())
    spectral_distances$primal$mm$mh[[i]][[ii]] <- list(list(),list(),list())
    spectral_distances$primal$mm$hm[[i]][[ii]] <- list(list(),list(),list())
    spectral_distances$primal$mm$hh[[i]][[ii]] <- list(list(),list(),list())
    spectral_distances$dual$mm$mm[[i]][[ii]] <- list(list(),list(),list())
    spectral_distances$dual$mm$mh[[i]][[ii]] <- list(list(),list(),list())
    spectral_distances$dual$mm$hm[[i]][[ii]] <- list(list(),list(),list())
    spectral_distances$dual$mm$hh[[i]][[ii]] <- list(list(),list(),list())
    
    spectral_distances$primal$mh$mm[[i]][[ii]] <- list(list(),list(),list())
    spectral_distances$primal$mh$mh[[i]][[ii]] <- list(list(),list(),list())
    spectral_distances$primal$mh$hm[[i]][[ii]] <- list(list(),list(),list())
    spectral_distances$primal$mh$hh[[i]][[ii]] <- list(list(),list(),list())
    spectral_distances$dual$mh$mm[[i]][[ii]] <- list(list(),list(),list())
    spectral_distances$dual$mh$mh[[i]][[ii]] <- list(list(),list(),list())
    spectral_distances$dual$mh$hm[[i]][[ii]] <- list(list(),list(),list())
    spectral_distances$dual$mh$hh[[i]][[ii]] <- list(list(),list(),list())
    
    spectral_distances$primal$hm$mm[[i]][[ii]] <- list(list(),list(),list())
    spectral_distances$primal$hm$mh[[i]][[ii]] <- list(list(),list(),list())
    spectral_distances$primal$hm$hm[[i]][[ii]] <- list(list(),list(),list())
    spectral_distances$primal$hm$hh[[i]][[ii]] <- list(list(),list(),list())
    spectral_distances$dual$hm$mm[[i]][[ii]] <- list(list(),list(),list())
    spectral_distances$dual$hm$mh[[i]][[ii]] <- list(list(),list(),list())
    spectral_distances$dual$hm$hm[[i]][[ii]] <- list(list(),list(),list())
    spectral_distances$dual$hm$hh[[i]][[ii]] <- list(list(),list(),list())
    
    spectral_distances$primal$hh$mm[[i]][[ii]] <- list(list(),list(),list())
    spectral_distances$primal$hh$mh[[i]][[ii]] <- list(list(),list(),list())
    spectral_distances$primal$hh$hm[[i]][[ii]] <- list(list(),list(),list())
    spectral_distances$primal$hh$hh[[i]][[ii]] <- list(list(),list(),list())
    spectral_distances$dual$hh$mm[[i]][[ii]] <- list(list(),list(),list())
    spectral_distances$dual$hh$mh[[i]][[ii]] <- list(list(),list(),list())
    spectral_distances$dual$hh$hm[[i]][[ii]] <- list(list(),list(),list())
    spectral_distances$dual$hh$hh[[i]][[ii]] <- list(list(),list(),list())
    
    for (iii in 1:3){
      for (iv in 1:3){
        spectral_distances$primal$mm$mm[[i]][[ii]][[iii]][[iv]] <- get_spectral_distance(eigenvalues$mouse_mouse$primal[[i]][[ii]],eigenvalues$mouse_mouse$primal[[iii]][[iv]])
        spectral_distances$primal$mm$mh[[i]][[ii]][[iii]][[iv]] <- get_spectral_distance(eigenvalues$mouse_mouse$primal[[i]][[ii]],eigenvalues$mouse_human$primal[[iii]][[iv]])
        spectral_distances$primal$mm$hm[[i]][[ii]][[iii]][[iv]] <- get_spectral_distance(eigenvalues$mouse_mouse$primal[[i]][[ii]],eigenvalues$human_mouse$primal[[iii]][[iv]])
        spectral_distances$primal$mm$hh[[i]][[ii]][[iii]][[iv]] <- get_spectral_distance(eigenvalues$mouse_mouse$primal[[i]][[ii]],eigenvalues$human_human$primal[[iii]][[iv]])
        spectral_distances$dual$mm$mm[[i]][[ii]][[iii]][[iv]] <- get_spectral_distance(eigenvalues$mouse_mouse$dual[[i]][[ii]],eigenvalues$mouse_mouse$dual[[iii]][[iv]])
        spectral_distances$dual$mm$mh[[i]][[ii]][[iii]][[iv]] <- get_spectral_distance(eigenvalues$mouse_mouse$dual[[i]][[ii]],eigenvalues$mouse_human$dual[[iii]][[iv]])
        spectral_distances$dual$mm$hm[[i]][[ii]][[iii]][[iv]] <- get_spectral_distance(eigenvalues$mouse_mouse$dual[[i]][[ii]],eigenvalues$human_mouse$dual[[iii]][[iv]])
        spectral_distances$dual$mm$hh[[i]][[ii]][[iii]][[iv]] <- get_spectral_distance(eigenvalues$mouse_mouse$dual[[i]][[ii]],eigenvalues$human_human$dual[[iii]][[iv]])
        
        spectral_distances$primal$mh$mm[[i]][[ii]][[iii]][[iv]] <- get_spectral_distance(eigenvalues$mouse_human$primal[[i]][[ii]],eigenvalues$mouse_mouse$primal[[iii]][[iv]])
        spectral_distances$primal$mh$mh[[i]][[ii]][[iii]][[iv]] <- get_spectral_distance(eigenvalues$mouse_human$primal[[i]][[ii]],eigenvalues$mouse_human$primal[[iii]][[iv]])
        spectral_distances$primal$mh$hm[[i]][[ii]][[iii]][[iv]] <- get_spectral_distance(eigenvalues$mouse_human$primal[[i]][[ii]],eigenvalues$human_mouse$primal[[iii]][[iv]])
        spectral_distances$primal$mh$hh[[i]][[ii]][[iii]][[iv]] <- get_spectral_distance(eigenvalues$mouse_human$primal[[i]][[ii]],eigenvalues$human_human$primal[[iii]][[iv]])
        spectral_distances$dual$mh$mm[[i]][[ii]][[iii]][[iv]] <- get_spectral_distance(eigenvalues$mouse_human$dual[[i]][[ii]],eigenvalues$mouse_mouse$dual[[iii]][[iv]])
        spectral_distances$dual$mh$mh[[i]][[ii]][[iii]][[iv]] <- get_spectral_distance(eigenvalues$mouse_human$dual[[i]][[ii]],eigenvalues$mouse_human$dual[[iii]][[iv]])
        spectral_distances$dual$mh$hm[[i]][[ii]][[iii]][[iv]] <- get_spectral_distance(eigenvalues$mouse_human$dual[[i]][[ii]],eigenvalues$human_mouse$dual[[iii]][[iv]])
        spectral_distances$dual$mh$hh[[i]][[ii]][[iii]][[iv]] <- get_spectral_distance(eigenvalues$mouse_human$dual[[i]][[ii]],eigenvalues$human_human$dual[[iii]][[iv]])
        
        spectral_distances$primal$hm$mm[[i]][[ii]][[iii]][[iv]] <- get_spectral_distance(eigenvalues$human_mouse$primal[[i]][[ii]],eigenvalues$mouse_mouse$primal[[iii]][[iv]])
        spectral_distances$primal$hm$mh[[i]][[ii]][[iii]][[iv]] <- get_spectral_distance(eigenvalues$human_mouse$primal[[i]][[ii]],eigenvalues$mouse_human$primal[[iii]][[iv]])
        spectral_distances$primal$hm$hm[[i]][[ii]][[iii]][[iv]] <- get_spectral_distance(eigenvalues$human_mouse$primal[[i]][[ii]],eigenvalues$human_mouse$primal[[iii]][[iv]])
        spectral_distances$primal$hm$hh[[i]][[ii]][[iii]][[iv]] <- get_spectral_distance(eigenvalues$human_mouse$primal[[i]][[ii]],eigenvalues$human_human$primal[[iii]][[iv]])
        spectral_distances$dual$hm$mm[[i]][[ii]][[iii]][[iv]] <- get_spectral_distance(eigenvalues$human_mouse$dual[[i]][[ii]],eigenvalues$mouse_mouse$dual[[iii]][[iv]])
        spectral_distances$dual$hm$mh[[i]][[ii]][[iii]][[iv]] <- get_spectral_distance(eigenvalues$human_mouse$dual[[i]][[ii]],eigenvalues$mouse_human$dual[[iii]][[iv]])
        spectral_distances$dual$hm$hm[[i]][[ii]][[iii]][[iv]] <- get_spectral_distance(eigenvalues$human_mouse$dual[[i]][[ii]],eigenvalues$human_mouse$dual[[iii]][[iv]])
        spectral_distances$dual$hm$hh[[i]][[ii]][[iii]][[iv]] <- get_spectral_distance(eigenvalues$human_mouse$dual[[i]][[ii]],eigenvalues$human_human$dual[[iii]][[iv]])
        
        spectral_distances$primal$hh$mm[[i]][[ii]][[iii]][[iv]] <- get_spectral_distance(eigenvalues$human_human$primal[[i]][[ii]],eigenvalues$mouse_mouse$primal[[iii]][[iv]])
        spectral_distances$primal$hh$mh[[i]][[ii]][[iii]][[iv]] <- get_spectral_distance(eigenvalues$human_human$primal[[i]][[ii]],eigenvalues$mouse_human$primal[[iii]][[iv]])
        spectral_distances$primal$hh$hm[[i]][[ii]][[iii]][[iv]] <- get_spectral_distance(eigenvalues$human_human$primal[[i]][[ii]],eigenvalues$human_mouse$primal[[iii]][[iv]])
        spectral_distances$primal$hh$hh[[i]][[ii]][[iii]][[iv]] <- get_spectral_distance(eigenvalues$human_human$primal[[i]][[ii]],eigenvalues$human_human$primal[[iii]][[iv]])
        spectral_distances$dual$hh$mm[[i]][[ii]][[iii]][[iv]] <- get_spectral_distance(eigenvalues$human_human$dual[[i]][[ii]],eigenvalues$mouse_mouse$dual[[iii]][[iv]])
        spectral_distances$dual$hh$mh[[i]][[ii]][[iii]][[iv]] <- get_spectral_distance(eigenvalues$human_human$dual[[i]][[ii]],eigenvalues$mouse_human$dual[[iii]][[iv]])
        spectral_distances$dual$hh$hm[[i]][[ii]][[iii]][[iv]] <- get_spectral_distance(eigenvalues$human_human$dual[[i]][[ii]],eigenvalues$human_mouse$dual[[iii]][[iv]])
        spectral_distances$dual$hh$hh[[i]][[ii]][[iii]][[iv]] <- get_spectral_distance(eigenvalues$human_human$dual[[i]][[ii]],eigenvalues$human_human$dual[[iii]][[iv]])
      }
    }
  }
}

for (i in 1:3){
  for (j in 1:3){
    spectral_distances$disconnected$mm[[i]][[j]] <- get_spectral_distance_disconnected(eigenvalues$mouse_mouse$dual[[i]][[j]])
    spectral_distances$disconnected$mh[[i]][[j]] <- get_spectral_distance_disconnected(eigenvalues$mouse_human$dual[[i]][[j]])
    spectral_distances$disconnected$hm[[i]][[j]] <- get_spectral_distance_disconnected(eigenvalues$human_mouse$dual[[i]][[j]])
    spectral_distances$disconnected$hh[[i]][[j]] <- get_spectral_distance_disconnected(eigenvalues$human_human$dual[[i]][[j]])
  }
}


