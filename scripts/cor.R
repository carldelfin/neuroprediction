# create empty list
cList <- list()

# add spearman's rho correlation results to list
for (i in spectList) {
  cList[[i]] <- cor.test(data$age, data[[i]], method = "spearman", exact = FALSE)
}

# unlist into data frame
corDat <- data.frame(matrix(unlist(cList), nrow = 8, byrow = TRUE), stringsAsFactors = FALSE)

# remove columns we don't want
corDat <- corDat[, -c(1, 4:7)]

# rename columns
colnames(corDat) <- c("p", "rho")

# round numbers
corDat$p <- round(as.numeric(corDat$p), 3)
corDat$rho <- round(as.numeric(corDat$rho), 2)

# create new var 'var', set to 0
corDat$var <- 0

# loop through variable names and add to 'var'
for (i in 1:8) {
  corDat[i,"var"] = names(cList[i])
}

# rearrange columns
corDat <- corDat[, c(3, 2, 1)]