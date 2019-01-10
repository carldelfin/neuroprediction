# numerical variables, full sample
nums <- sapply(data, is.numeric)
table_all <- data.frame(describe(data[, nums]))

# numerical variables, by reoffending
table_temp <- describeBy(data[, nums], group = data[[outcomeVariable]])

# remove columns we don't want; only keep mean and SD
table_all <- round(table_all[3:4], 2)
table_temp[[1]] <- round(table_temp[[1]][3:4], 2)
table_temp[[2]] <- round(table_temp[[2]][3:4], 2)

# create separate data frames
table_reoffendno <- as.data.frame(table_temp[[1]])
table_reoffendyes <- as.data.frame(table_temp[[2]])

# create temporary storage for t-tests
ttests <- NULL

# loop through t-tests using lapply, store in 'temp'
temp <- lapply(names(data)[nums], function(x)
  t.test(as.formula(paste(x, outcomeVariable, sep = "~")), data = data))

# append 'ttests' with t and p-values
for (i in 1:length(temp)) {
  ttests$comparison = round(append(ttests$comparison, as.numeric(temp[[i]]$statistic)), 2)
  ttests$p = round(append(ttests$p, as.numeric(temp[[i]]$p.value)), 3)
}

# convert 'ttests' to data frame
ttests <- as.data.frame(ttests)

# bind data frames together to form a single table (rather, single data frame)
table1 <- cbind(table_all, table_reoffendno, table_reoffendyes)

# rename columns
names(table1) <- c("mean1", "sd1", "mean2", "sd2", "mean3", "sd3")

# create combined columns (with mean and SD)
table1 <- within(table1, all <- paste(mean1, sd1, sep = " (\u00B1 "))
table1 <- within(table1, nonreoffenders <- paste(mean2, sd2, sep = " (\u00B1 "))
table1 <- within(table1, reoffenders <- paste(mean3, sd3, sep = " (\u00B1 "))

# add closing parenthesis
table1$all <- paste0(table1$all, ")")
table1$nonreoffenders <- paste0(table1$nonreoffenders, ")")
table1$reoffenders <- paste0(table1$reoffenders, ")")

# remove "old" columns
table1 <- table1[-c(1:6)]

# add t-tests
table1 <- cbind(table1, ttests)

# bind categorical table (sourced in analysis.Rmd) with table2
table1 <- rbind(table1, catTable)

# rename columns (again)
names(table1) <- c("Mean (\u00B1 SD)/n (\u0025)", "Mean (\u00B1 SD)/n (\u0025)", "Mean (\u00B1 SD)/n (\u0025)", "t/z value", "p")

# remove temporary stuff
remove(table_temp, table_all, table_reoffendno, table_reoffendyes, 
       ttests, temp, catTable, catdata, catnames, barnardTest,
       confusionMatrix, i, nums, p, percall, percnonreoff, 
       percreoff, countsall, countsnonreoff, countsreoff, comparison)