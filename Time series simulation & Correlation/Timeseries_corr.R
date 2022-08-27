library(ppcor)
library(corrplot)


######### COLLIDER

# Triplets of time series
sd <- c(0.2, 0.5, 1, 1.5, 2)
Xtot <- list()

for (i in sd){
  set.seed(333)
  x <- rnorm(1000)
  y <- rnorm(1000)
  z <- x + y + rnorm(1000, 0, i)
  
  Xtot[[length(Xtot) + 1]] <- cbind(x, y, z) 
  # list with 5 elements: each is a matrix of 3 variables x, y, z that differ for
  # the standard deviation of the noise around the collider z (0.2, 0.5, 1, 1.5, 2)
}  

# plot layout
mat <- matrix(c(1,4,2,5,3,0), 2)
layout(mat)

# Pearson's and partial correlation
part_corrX <- cor_matX <- vector(mode = "list", length = length(sd))  

for (i in 1:length(sd)) {
  ### PAIRWISE CORRELATION 
  (cor_matX[[i]] <- round(cor(Xtot[[i]], use = "pairwise.complete.obs" ), 3))
  ### PARTIAL CORRELATION
  (part_corrX[[i]] <- round(pcor(Xtot[[i]])$estimate, 3))
  
  # Merge pairwise (lower tri) and partial (upper tri) correlation matrices
  m <- cor_matX[[i]]
  m[upper.tri(m)] <- part_corrX[[i]][upper.tri(part_corrX[[i]])]
  diag(m)=0
  
  corrplot(m,
           method = "color",
           tl.col = "black",                                    
           tl.cex = 0.6,                                        
           tl.srt = 45)
} 

cor_matX
part_corrX

##################################### TAB #####################################

t1 <- round(cbind(c(cor_matX[[1]][4], cor_matX[[1]][7], cor_matX[[1]][8]),
                  c(cor_matX[[2]][4], cor_matX[[2]][7], cor_matX[[2]][8]), 
                  c(cor_matX[[3]][4], cor_matX[[3]][7], cor_matX[[3]][8]),  
                  c(cor_matX[[4]][4], cor_matX[[4]][7], cor_matX[[4]][8]),   
                  c(cor_matX[[5]][4], cor_matX[[5]][7], cor_matX[[5]][8])), 2)

rownames(t1) <- c("x~y", "x~z", "y~z")
colnames(t1) <- c("0.2", "0.5", "1", "1.5", "2")

knitr::kable(t1)

t2 <- round(cbind(c(part_corrX[[1]][4], part_corrX[[1]][7], part_corrX[[1]][8]),
                  c(part_corrX[[2]][4], part_corrX[[2]][7], part_corrX[[2]][8]),
                  c(part_corrX[[3]][4], part_corrX[[3]][7], part_corrX[[3]][8]),
                  c(part_corrX[[4]][4], part_corrX[[4]][7], part_corrX[[4]][8]), 
                  c(part_corrX[[5]][4], part_corrX[[5]][7], part_corrX[[5]][8])), 2)

rownames(t2) <- c("x~y;z", "x~z;y", "y~z;x")
colnames(t2) <- c("0.2", "0.5", "1", "1.5", "2")

knitr::kable(t2)


######### CONFOUNDER
# Triplets of time series
Ytot <- list()

for (i in sd){
  set.seed(333)
  z <- rnorm(1000, 0, i)
  x <- rnorm(1000) + z
  y <- rnorm(1000) + z
  
  Ytot[[length(Ytot) + 1]] <- cbind(x, y, z) 
  # list with 5 elements: each is a matrix of 3 variables x, y, z that differ for
  # the standard deviation of the noise around the  confounder z (0.2, 0.5, 1, 1.5, 2)
}

# Pearson's and partial correlation
part_corrY <- cor_matY <- vector(mode = "list", length = length(sd))  

for (i in 1:length(sd)) {
  ### PAIRWISE CORRELATION 
  (cor_matY[[i]] <- round(cor(Ytot[[i]], use = "pairwise.complete.obs" ), 3))
  ### PARTIAL CORRELATION
  (part_corrY[[i]] <- round(pcor(Ytot[[i]])$estimate, 3))
  
  # Merge pairwise (lower tri) and partial (upper tri) correlation matrices
  m <- cor_matY[[i]]
  m[upper.tri(m)] <- part_corrY[[i]][upper.tri(part_corrY[[i]])]
  diag(m)=0
  
  corrplot(m,
           method = "color",
           tl.col = "black",                                    
           tl.cex = 0.6,                                        
           tl.srt = 45)
} 

cor_matY
part_corrY

##################################### TAB #####################################

t1 <- round(cbind(c(cor_matY[[1]][4], cor_matY[[1]][7], cor_matY[[1]][8]),  
                  c(cor_matY[[2]][4], cor_matY[[2]][7], cor_matY[[2]][8]),  
                  c(cor_matY[[3]][4], cor_matY[[3]][7], cor_matY[[3]][8]),  
                  c(cor_matY[[4]][4], cor_matY[[4]][7], cor_matY[[4]][8]),  
                  c(cor_matY[[5]][4], cor_matY[[5]][7], cor_matY[[5]][8])), 2)

rownames(t1) <- c("x~y", "x~z", "y~z")
colnames(t1) <- c("0.2", "0.5", "1", "1.5", "2")

knitr::kable(t1)

t2 <- round(cbind(c(part_corrY[[1]][4], part_corrY[[1]][7], part_corrY[[1]][8]),
                  c(part_corrY[[2]][4], part_corrY[[2]][7], part_corrY[[2]][8]),
                  c(part_corrY[[3]][4], part_corrY[[3]][7], part_corrY[[3]][8]),
                  c(part_corrY[[4]][4], part_corrY[[4]][7], part_corrY[[4]][8]),
                  c(part_corrY[[5]][4], part_corrY[[5]][7], part_corrY[[5]][8])), 2)

rownames(t2) <- c("x~y;z", "x~z;y", "y~z;x")
colnames(t2) <- c("0.2", "0.5", "1", "1.5", "2")

knitr::kable(t2)


######### REDUNDANCY 1, varying x, first element of the chain

# Triplets of time series
Wtot1 <- list()

for (i in sd){
  set.seed(333)
  x <- rnorm(1000, 0, i)
  y <- 0.5*rnorm(1000) + 0.5*x
  z <- 0.5*rnorm(1000) + 0.5*y

  Wtot1[[length(Wtot1) + 1]] <- cbind(x, y, z) 
  # list with 5 elements: each is a matrix of 4 variables w, x, y, z that differ 
  # for the standard deviation of the noise around the first variable w (0.2, 0.5, 1, 1.5, 2)
}

# Pearson's and partial correlation
part_corrW1 <- cor_matW1 <- vector(mode = "list", length = length(sd))  

for (i in 1:length(sd)) {
  ### PAIRWISE CORRELATION 
  (cor_matW1[[i]] <- round(cor(Wtot1[[i]], use = "pairwise.complete.obs" ), 3))
  ### PARTIAL CORRELATION
  (part_corrW1[[i]] <- round(pcor(Wtot1[[i]])$estimate, 3))
  
  # Merge pairwise (lower tri) and partial (upper tri) correlation matrices
  m <- cor_matW1[[i]]
  m[upper.tri(m)] <- part_corrW1[[i]][upper.tri(part_corrW1[[i]])]
  diag(m)=0
  
  corrplot(m,
           method = "color",
           tl.col = "black",                                    
           tl.cex = 0.6,                                        
           tl.srt = 45)
} 

cor_matW1
part_corrW1

##################################### TAB #####################################

t1 <- round(cbind(c(cor_matW1[[1]][4], cor_matW1[[1]][7], cor_matW1[[1]][8]),  
                  c(cor_matW1[[2]][4], cor_matW1[[2]][7], cor_matW1[[2]][8]),  
                  c(cor_matW1[[3]][4], cor_matW1[[3]][7], cor_matW1[[3]][8]),  
                  c(cor_matW1[[4]][4], cor_matW1[[4]][7], cor_matW1[[4]][8]),  
                  c(cor_matW1[[5]][4], cor_matW1[[5]][7], cor_matW1[[5]][8])), 2)

rownames(t1) <- c("x~y", "x~z", "y~z")
colnames(t1) <- c("0.2", "0.5", "1", "1.5", "2")

knitr::kable(t1)

t2 <- round(cbind(c(part_corrW1[[1]][4], part_corrW1[[1]][7], part_corrW1[[1]][8]),
                  c(part_corrW1[[2]][4], part_corrW1[[2]][7], part_corrW1[[2]][8]),
                  c(part_corrW1[[3]][4], part_corrW1[[3]][7], part_corrW1[[3]][8]),
                  c(part_corrW1[[4]][4], part_corrW1[[4]][7], part_corrW1[[4]][8]),
                  c(part_corrW1[[5]][4], part_corrW1[[5]][7], part_corrW1[[5]][8])), 2)

rownames(t2) <- c("x~y;z", "x~z;y", "y~z;x")
colnames(t2) <- c("0.2", "0.5", "1", "1.5", "2")

knitr::kable(t2)


######### REDUNDANCY 2, varying y, second element of the chain

# Triplets of time series
Wtot2 <- list()

for (i in sd){
  set.seed(333)
  x <- rnorm(1000)
  y <- 0.5*rnorm(1000, 0, i) + 0.5*x
  z <- 0.5*rnorm(1000) + 0.5*y
  
  Wtot2[[length(Wtot2) + 1]] <- cbind(x, y, z)
  # list with 5 elements: each is a matrix of 4 variables w, x, y, z that differ for 
  # the standard deviation of the noise around the firsvariable w (0.2, 0.5, 1, 1.5, 2)
}

# Pearson's and partial correlation
part_corrW2 <- cor_matW2 <- vector(mode = "list", length = length(sd))  

for (i in 1:length(sd)) {
  ### PAIRWISE CORRELATION 
  (cor_matW2[[i]] <- round(cor(Wtot2[[i]], use = "pairwise.complete.obs" ), 3))
  ### PARTIAL CORRELATION
  (part_corrW2[[i]] <- round(pcor(Wtot2[[i]])$estimate, 3))
  
  # Merge pairwise (lower tri) and partial (upper tri) correlation matrices
  m <- cor_matW2[[i]]
  m[upper.tri(m)] <- part_corrW2[[i]][upper.tri(part_corrW2[[i]])]
  diag(m)=0
  
  corrplot(m,
           method = "color",
           tl.col = "black",                                    
           tl.cex = 0.6,                                        
           tl.srt = 45)
} 

cor_matW2
part_corrW2

##################################### TAB #####################################

t1 <- round(cbind(c(cor_matW2[[1]][4], cor_matW2[[1]][7], cor_matW2[[1]][8]),  
                  c(cor_matW2[[2]][4], cor_matW2[[2]][7], cor_matW2[[2]][8]),  
                  c(cor_matW2[[3]][4], cor_matW2[[3]][7], cor_matW2[[3]][8]),  
                  c(cor_matW2[[4]][4], cor_matW2[[4]][7], cor_matW2[[4]][8]),  
                  c(cor_matW2[[5]][4], cor_matW2[[5]][7], cor_matW2[[5]][8])), 2)

rownames(t1) <- c("x~y", "x~z", "y~z")
colnames(t1) <- c("0.2", "0.5", "1", "1.5", "2")

knitr::kable(t1)

t2 <- round(cbind(c(part_corrW2[[1]][4], part_corrW2[[1]][7], part_corrW2[[1]][8]),
                  c(part_corrW2[[2]][4], part_corrW2[[2]][7], part_corrW2[[2]][8]),
                  c(part_corrW2[[3]][4], part_corrW2[[3]][7], part_corrW2[[3]][8]),
                  c(part_corrW2[[4]][4], part_corrW2[[4]][7], part_corrW2[[4]][8]),
                  c(part_corrW2[[5]][4], part_corrW2[[5]][7], part_corrW2[[5]][8])), 2)

rownames(t2) <- c("x~y;z", "x~z;y", "y~z;x")
colnames(t2) <- c("0.2", "0.5", "1", "1.5", "2")

knitr::kable(t2)


######### REDUNDANCY 3, varying z, third element of the chain

# Triplets of time series
Wtot3 <- list()

for (i in sd){
  set.seed(333)
  x <- rnorm(1000)
  y <- 0.5*rnorm(1000) + 0.5*x
  z <- 0.5*rnorm(1000, 0, i) + 0.5*y
  
  
  Wtot3[[length(Wtot3) + 1]] <- cbind(x, y, z)
  # list with 5 elements: each is a matrix of 4 variables w, x, y, z that differ
  # for the standard deviation of the noise around the third variable y (0.2, 0.5, 1, 1.5, 2)
}

# Pearson's and partial correlation
part_corrW3 <- cor_matW3 <- vector(mode = "list", length = length(sd))  

for (i in 1:length(sd)) {
  ### PAIRWISE CORRELATION 
  (cor_matW3[[i]] <- round(cor(Wtot3[[i]], use = "pairwise.complete.obs" ), 3))
  ### PARTIAL CORRELATION
  (part_corrW3[[i]] <- round(pcor(Wtot3[[i]])$estimate, 3))
  
  # Merge pairwise (lower tri) and partial (upper tri) correlation matrices
  m <- cor_matW3[[i]]
  m[upper.tri(m)] <- part_corrW3[[i]][upper.tri(part_corrW3[[i]])]
  diag(m)=0
  
  corrplot(m,
           method = "color",
           tl.col = "black",                                    
           tl.cex = 0.6,                                        
           tl.srt = 45)
} 

cor_matW3
part_corrW3

##################################### TAB #####################################

t1 <- round(cbind(c(cor_matW3[[1]][4], cor_matW3[[1]][7], cor_matW3[[1]][8]),  
                  c(cor_matW3[[2]][4], cor_matW3[[2]][7], cor_matW3[[2]][8]),  
                  c(cor_matW3[[3]][4], cor_matW3[[3]][7], cor_matW3[[3]][8]),  
                  c(cor_matW3[[4]][4], cor_matW3[[4]][7], cor_matW3[[4]][8]),  
                  c(cor_matW3[[5]][4], cor_matW3[[5]][7], cor_matW3[[5]][8])), 2)

rownames(t1) <- c("x~y", "x~z", "y~z")
colnames(t1) <- c("0.2", "0.5", "1", "1.5", "2")

knitr::kable(t1)

t2 <- round(cbind(c(part_corrW3[[1]][4], part_corrW3[[1]][7], part_corrW3[[1]][8]),
                  c(part_corrW3[[2]][4], part_corrW3[[2]][7], part_corrW3[[2]][8]),
                  c(part_corrW3[[3]][4], part_corrW3[[3]][7], part_corrW3[[3]][8]),
                  c(part_corrW3[[4]][4], part_corrW3[[4]][7], part_corrW3[[4]][8]),
                  c(part_corrW3[[5]][4], part_corrW3[[5]][7], part_corrW3[[5]][8])), 2)

rownames(t2) <- c("x~y;z", "x~z;y", "y~z;x")
colnames(t2) <- c("0.2", "0.5", "1", "1.5", "2")

knitr::kable(t2)


######### SUPPRESSOR1, varying x, first element 

# Triplets of time series
Ztot1 <- list()

for (i in sd){
  set.seed(333)
  x <- rnorm(1000, 0, i)
  y <- rnorm(1000)
  z <- 0.3*x + 0.6*x*y + 0.1 + rnorm(1000)
  
  Ztot1[[length(Ztot1) + 1]] <- cbind(x, y, z) 
  # list with 5 elements: each is a matrix of 4 variables w, x, y, z that differ 
  # for the standard deviation of the noise around the first variable w (0.2, 0.5, 1, 1.5, 2)
}

# Pearson's and partial correlation
part_corrZ1 <- cor_matZ1 <- vector(mode = "list", length = length(sd))  

for (i in 1:length(sd)) {
  ### PAIRWISE CORRELATION 
  (cor_matZ1[[i]] <- round(cor(Ztot1[[i]], use = "pairwise.complete.obs" ), 3))
  ### PARTIAL CORRELATION
  (part_corrZ1[[i]] <- round(pcor(Ztot1[[i]])$estimate, 3))
  
  # Merge pairwise (lower tri) and partial (upper tri) correlation matrices
  m <- cor_matZ1[[i]]
  m[upper.tri(m)] <- part_corrZ1[[i]][upper.tri(part_corrZ1[[i]])]
  diag(m)=0
  
  corrplot(m,
           method = "color",
           tl.col = "black",                                    
           tl.cex = 0.6,                                        
           tl.srt = 45)
} 

cor_matZ1
part_corrZ1

##################################### TAB #####################################

t1 <- round(cbind(c(cor_matZ1[[1]][4], cor_matZ1[[1]][7], cor_matZ1[[1]][8]),  
                  c(cor_matZ1[[2]][4], cor_matZ1[[2]][7], cor_matZ1[[2]][8]),  
                  c(cor_matZ1[[3]][4], cor_matZ1[[3]][7], cor_matZ1[[3]][8]),  
                  c(cor_matZ1[[4]][4], cor_matZ1[[4]][7], cor_matZ1[[4]][8]),  
                  c(cor_matZ1[[5]][4], cor_matZ1[[5]][7], cor_matZ1[[5]][8])), 2)

rownames(t1) <- c("x~y", "x~z", "y~z")
colnames(t1) <- c("0.2", "0.5", "1", "1.5", "2")

knitr::kable(t1)

t2 <- round(cbind(c(part_corrZ1[[1]][4], part_corrZ1[[1]][7], part_corrZ1[[1]][8]),
                  c(part_corrZ1[[2]][4], part_corrZ1[[2]][7], part_corrZ1[[2]][8]),
                  c(part_corrZ1[[3]][4], part_corrZ1[[3]][7], part_corrZ1[[3]][8]),
                  c(part_corrZ1[[4]][4], part_corrZ1[[4]][7], part_corrZ1[[4]][8]),
                  c(part_corrZ1[[5]][4], part_corrZ1[[5]][7], part_corrZ1[[5]][8])), 2)

rownames(t2) <- c("x~y;z", "x~z;y", "y~z;x")
colnames(t2) <- c("0.2", "0.5", "1", "1.5", "2")

knitr::kable(t2)


######### SUPPRESSOR2, varying y, second element 

# Triplets of time series
Ztot2 <- list()

for (i in sd){
  set.seed(333)
  x <- rnorm(1000)
  y <- rnorm(1000, 0, i)
  z <- 0.3*x + 0.6*x*y + 0.1 + rnorm(1000)
  
  Ztot2[[length(Ztot2) + 1]] <- cbind(x, y, z) 
  # list with 5 elements: each is a matrix of 4 variables w, x, y, z that differ 
  # for the standard deviation of the noise around the second variable x (0.2, 0.5, 1, 1.5, 2)
}

# Pearson's and partial correlation
part_corrZ2 <- cor_matZ2 <- vector(mode = "list", length = length(sd))  

for (i in 1:length(sd)) {
  ### PAIRWISE CORRELATION 
  (cor_matZ2[[i]] <- round(cor(Ztot2[[i]], use = "pairwise.complete.obs" ), 3))
  ### PARTIAL CORRELATION
  (part_corrZ2[[i]] <- round(pcor(Ztot2[[i]])$estimate, 3))
  
  # Merge pairwise (lower tri) and partial (upper tri) correlation matrices
  m <- cor_matZ2[[i]]
  m[upper.tri(m)] <- part_corrZ2[[i]][upper.tri(part_corrZ2[[i]])]
  diag(m)=0
  
  corrplot(m,
           method = "color",
           tl.col = "black",                                    
           tl.cex = 0.6,                                        
           tl.srt = 45)
} 

cor_matZ2
part_corrZ2

##################################### TAB #####################################

t1 <- round(cbind(c(cor_matZ2[[1]][4], cor_matZ2[[1]][7], cor_matZ2[[1]][8]),  
                  c(cor_matZ2[[2]][4], cor_matZ2[[2]][7], cor_matZ2[[2]][8]),  
                  c(cor_matZ2[[3]][4], cor_matZ2[[3]][7], cor_matZ2[[3]][8]),  
                  c(cor_matZ2[[4]][4], cor_matZ2[[4]][7], cor_matZ2[[4]][8]),  
                  c(cor_matZ2[[5]][4], cor_matZ2[[5]][7], cor_matZ2[[5]][8])), 2)

rownames(t1) <- c("x~y", "x~z", "y~z")
colnames(t1) <- c("0.2", "0.5", "1", "1.5", "2")

knitr::kable(t1)

t2 <- round(cbind(c(part_corrZ2[[1]][4], part_corrZ2[[1]][7], part_corrZ2[[1]][8]),
                  c(part_corrZ2[[2]][4], part_corrZ2[[2]][7], part_corrZ2[[2]][8]),
                  c(part_corrZ2[[3]][4], part_corrZ2[[3]][7], part_corrZ2[[3]][8]),
                  c(part_corrZ2[[4]][4], part_corrZ2[[4]][7], part_corrZ2[[4]][8]),
                  c(part_corrZ2[[5]][4], part_corrZ2[[5]][7], part_corrZ2[[5]][8])), 2)

rownames(t2) <- c("x~y;z", "x~z;y", "y~z;x")
colnames(t2) <- c("0.2", "0.5", "1", "1.5", "2")

knitr::kable(t2)


######### SUPPRESSOR3, varying z, third element

# Triplets of time series
Ztot3 <- list()

for (i in sd){
  set.seed(333)
  x <- rnorm(1000)
  y <- rnorm(1000)
  z <- 0.3*x + 0.6*x*y + 0.1 + rnorm(1000, 0, i)
  
  Ztot3[[length(Ztot3) + 1]] <- cbind(x, y, z) 
  # list with 5 elements: each is a matrix of 4 variables w, x, y, z that differ 
  # for the standard deviation of the noise around the third variable y (0.2, 0.5, 1, 1.5, 2)
}

# Pearson's and partial correlation
part_corrZ3 <- cor_matZ3 <- vector(mode = "list", length = length(sd))  

for (i in 1:length(sd)) {
  ### PAIRWISE CORRELATION 
  (cor_matZ3[[i]] <- round(cor(Ztot3[[i]], use = "pairwise.complete.obs" ), 3))
  ### PARTIAL CORRELATION
  (part_corrZ3[[i]] <- round(pcor(Ztot3[[i]])$estimate, 3))
  
  # Merge pairwise (lower tri) and partial (upper tri) correlation matrices
  m <- cor_matZ3[[i]]
  m[upper.tri(m)] <- part_corrZ3[[i]][upper.tri(part_corrZ3[[i]])]
  diag(m)=0
  
  corrplot(m,
           method = "color",
           tl.col = "black",                                    
           tl.cex = 0.6,                                        
           tl.srt = 45)
} 

cor_matZ3
part_corrZ3

##################################### TAB #####################################

t1 <- round(cbind(c(cor_matZ3[[1]][4], cor_matZ3[[1]][7], cor_matZ3[[1]][8]),  
                  c(cor_matZ3[[2]][4], cor_matZ3[[2]][7], cor_matZ3[[2]][8]),  
                  c(cor_matZ3[[3]][4], cor_matZ3[[3]][7], cor_matZ3[[3]][8]),  
                  c(cor_matZ3[[4]][4], cor_matZ3[[4]][7], cor_matZ3[[4]][8]),  
                  c(cor_matZ3[[5]][4], cor_matZ3[[5]][7], cor_matZ3[[5]][8])), 2)

rownames(t1) <- c("x~y", "x~z", "y~z")
colnames(t1) <- c("0.2", "0.5", "1", "1.5", "2")

knitr::kable(t1)

t2 <- round(cbind(c(part_corrZ3[[1]][4], part_corrZ3[[1]][7], part_corrZ3[[1]][8]),
                  c(part_corrZ3[[2]][4], part_corrZ3[[2]][7], part_corrZ3[[2]][8]),
                  c(part_corrZ3[[3]][4], part_corrZ3[[3]][7], part_corrZ3[[3]][8]),
                  c(part_corrZ3[[4]][4], part_corrZ3[[4]][7], part_corrZ3[[4]][8]),
                  c(part_corrZ3[[5]][4], part_corrZ3[[5]][7], part_corrZ3[[5]][8])), 2)

rownames(t2) <- c("x~y;z", "x~z;y", "y~z;x")
colnames(t2) <- c("0.2", "0.5", "1", "1.5", "2")

knitr::kable(t2)

