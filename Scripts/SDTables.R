sd.p <- matrix(0, 36,36)
sd.d <- matrix(0, 36,36)

sd.p[1:9,1:9] <- matrix(unlist(spectral_distances$primal$mm$mm), 9, 9, byrow = TRUE)
sd.d[1:9,1:9] <- matrix(unlist(spectral_distances$dual$mm$mm), 9, 9, byrow = TRUE)

sd.p[1:9,10:18] <- matrix(unlist(spectral_distances$primal$mm$mh), 9, 9, byrow = TRUE)
sd.d[1:9,10:18] <- matrix(unlist(spectral_distances$dual$mm$mh), 9, 9, byrow = TRUE)
sd.p[10:18,1:9] <- matrix(unlist(spectral_distances$primal$mm$mh), 9, 9, byrow = FALSE)
sd.d[10:18,1:9] <- matrix(unlist(spectral_distances$dual$mm$mh), 9, 9, byrow = FALSE)

sd.p[1:9,19:27] <- matrix(unlist(spectral_distances$primal$mm$hm), 9, 9, byrow = TRUE)
sd.d[1:9,19:27] <- matrix(unlist(spectral_distances$dual$mm$hm), 9, 9, byrow = TRUE)
sd.p[19:27,1:9] <- matrix(unlist(spectral_distances$primal$mm$hm), 9, 9, byrow = FALSE)
sd.d[19:27,1:9] <- matrix(unlist(spectral_distances$dual$mm$hm), 9, 9, byrow = FALSE)

sd.p[1:9,28:36] <- matrix(unlist(spectral_distances$primal$mm$hh), 9, 9, byrow = TRUE)
sd.d[1:9,28:36] <- matrix(unlist(spectral_distances$dual$mm$hh), 9, 9, byrow = TRUE)
sd.p[28:36,1:9] <- matrix(unlist(spectral_distances$primal$mm$hh), 9, 9, byrow = FALSE)
sd.d[28:36,1:9] <- matrix(unlist(spectral_distances$dual$mm$hh), 9, 9, byrow = FALSE)

sd.p[10:18,10:18] <- matrix(unlist(spectral_distances$primal$mh$mh), 9, 9, byrow = TRUE)
sd.d[10:18,10:18] <- matrix(unlist(spectral_distances$dual$mh$mh), 9, 9, byrow = TRUE)

sd.p[10:18,19:27] <- matrix(unlist(spectral_distances$primal$mh$hm), 9, 9, byrow = TRUE)
sd.d[10:18,19:27] <- matrix(unlist(spectral_distances$dual$mh$hm), 9, 9, byrow = TRUE)
sd.p[19:27,10:18] <- matrix(unlist(spectral_distances$primal$mh$hm), 9, 9, byrow = FALSE)
sd.d[19:27,10:18] <- matrix(unlist(spectral_distances$dual$mh$hm), 9, 9, byrow = FALSE)

sd.p[10:18,28:36] <- matrix(unlist(spectral_distances$primal$mh$hh), 9, 9, byrow = TRUE)
sd.d[10:18,28:36] <- matrix(unlist(spectral_distances$dual$mh$hh), 9, 9, byrow = TRUE)
sd.p[28:36,10:18] <- matrix(unlist(spectral_distances$primal$mh$hh), 9, 9, byrow = FALSE)
sd.d[28:36, 10:18] <- matrix(unlist(spectral_distances$dual$mh$hh), 9, 9, byrow = FALSE)

sd.p[19:27,19:27] <- matrix(unlist(spectral_distances$primal$hm$hm), 9, 9, byrow = TRUE)
sd.d[19:27,19:27] <- matrix(unlist(spectral_distances$dual$hm$hm), 9, 9, byrow = TRUE)

sd.p[19:27,28:36] <- matrix(unlist(spectral_distances$primal$hm$hh), 9, 9, byrow = TRUE)
sd.d[19:27,28:36] <- matrix(unlist(spectral_distances$dual$hm$hh), 9, 9, byrow = TRUE)
sd.p[28:36,19:27] <- matrix(unlist(spectral_distances$primal$hm$hh), 9, 9, byrow = FALSE)
sd.d[28:36,19:27] <- matrix(unlist(spectral_distances$dual$hm$hh), 9, 9, byrow = FALSE)

sd.p[28:36,28:36] <- matrix(unlist(spectral_distances$primal$hh$hh), 9, 9, byrow = TRUE)
sd.d[28:36,28:36] <- matrix(unlist(spectral_distances$dual$hh$hh), 9, 9, byrow = TRUE)

train_num <- rep(1:3, 4, each = 3)
test_num <- rep(1:3, 12)
train_spec <- rep(c("M", "H"), 1, each = 18)
test_spec <- rep(c("M", "H"), 2, each = 9)
id <- paste0(train_spec, train_num, "/", test_spec, test_num)

rownames(sd.p) <- id
rownames(sd.d) <- id
colnames(sd.p) <- id
colnames(sd.d) <- id





