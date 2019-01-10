# categorical only
stillundercare <- c("stillundercare")
catdata <- data[, c(outcomeVariable, baselineList, pharmaList, primaryList, stillundercare)]
catnames <- names(catdata[, sapply(catdata, is.factor)][-1])

# create empty vectors
countsall <- NULL
percall <- NULL
countsnonreoff <- NULL
percnonreoff <- NULL
countsreoff <- NULL
percreoff <- NULL

# store 
for (i in catnames) {
  countsall[i]      <- sum(catdata[i] == "1")
  percall[i]        <- round(sum(catdata[i] == "1")/nrow(catdata)*100, 0)
  countsnonreoff[i] <- sum(catdata[i] == "1" & catdata[outcomeVariable] == "no") 
  percnonreoff[i]   <- round(sum(catdata[i] == "1" & catdata[outcomeVariable] == "no")/sum(catdata[outcomeVariable] == "no")*100, 0)
  countsreoff[i]    <- sum(catdata[i] == "1" & catdata[outcomeVariable] == "yes")
  percreoff[i]      <- round(sum(catdata[i] == "1" & catdata[outcomeVariable] == "yes")/sum(catdata[outcomeVariable] == "yes")*100, 0)
}

catTable <- as.data.frame(cbind(countsall, percall, countsnonreoff, percnonreoff, countsreoff, percreoff))

# do Barnard's test
# store z-score and p-values in separate vectors
# (df is redundant so just fill with zeroes for the time being)

barnardTest <- NULL
confusionMatrix <- NULL
comparison <- NULL
p <- NULL

for (i in catnames) {
  confusionMatrix <- table(unlist(catdata[i]), unlist(catdata[outcomeVariable]))
  barnardTest <- barnard.test(confusionMatrix[1], confusionMatrix[2],
                              confusionMatrix[3], confusionMatrix[4])
  comparison <- round(append(comparison, (as.numeric(barnardTest$statistic))), 2)
  p <- round(append(p, as.numeric(barnardTest$p.value[2])), 3)
}

# create combined columns (with n and percent)
catTable <- within(catTable, all <- paste(countsall, percall, sep = " ("))
catTable <- within(catTable, nonreoffenders <- paste(countsnonreoff, percnonreoff, sep = " ("))
catTable <- within(catTable, reoffenders <- paste(countsreoff, percreoff, sep = " ("))

# add closing parenthesis
catTable$all <- paste0(catTable$all, "\u0025)")
catTable$nonreoffenders <- paste0(catTable$nonreoffenders, "\u0025)")
catTable$reoffenders <- paste0(catTable$reoffenders, "\u0025)")

# remove "old" columns
catTable <- catTable[-c(1:6)]

# add z- and p-values
catTable <- cbind(catTable, comparison, p)