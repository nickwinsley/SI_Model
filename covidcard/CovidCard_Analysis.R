library(ergm)
library(network)
library(sna)
library(irr)
library(caret)
library(pROC)
library(MASS)
library(gdata)
library(tnet)

library(dplyr)
library(lubridate)

# Read in CovidCard data.
cc.data <-read.csv("data/nz_latest_extract.csv", stringsAsFactors = FALSE)
# Read in case investigation data.
ci.data <-read.csv("data/contact_events_for_matching_436obs.csv", stringsAsFactors = FALSE)
# Read in IDs of CovidCard users who have case investigation data.
real.ids <- read.csv("data/card_ids_for_analysis.csv", stringsAsFactors = FALSE)


cc.data <-read_excel("/Users/admirary/Dropbox/Research/Masters/Jules Millen/Data/Card Data Raw.xlsx", sheet = 1)


# # Read in CovidCard data.
 cc.data <-read.csv("/Users/admirary/Dropbox/Research/Masters/Jules Millen/network_app/data/nz_latest_extract.csv", stringsAsFactors = FALSE)
7# # Read in case investigation data.
 ci.data <-read.csv("/Users/admirary/Dropbox/Research/Masters/Jules Millen/network_app/data/contact_events_for_matching_436obs.csv", stringsAsFactors = FALSE)
# # Read in IDs of CovidCard users who have case investigation data.
 real.ids <- read.csv("/Users/admirary/Dropbox/Research/Masters/Jules Millen/network_app/data/card_ids_for_analysis.csv", stringsAsFactors = FALSE)
# # Read in separate file with dates and times in a useable format.
 dates.and.times <- read.csv("/Users/admirary/Dropbox/Research/Masters/Jules Millen/network_app/data/date_time.csv", stringsAsFactors = FALSE)
# 
# # Remove date columns from original CovidCard dataset and overwrite with those from the "dates.and.times" file.
 cc.data <- cc.data[, -c(1, 4)]
 cc.data <- cbind(cc.data, dates.and.times)

#############################
## Overwrite and/or create ##
## new variables.          ##
#############################

# Reformat ExposureDateTime and UploadDateTime.
cc.data$ExposureDateTime <- as_datetime(cc.data$ExposureDateTime)
cc.data$UploadDateTime <- as_datetime(cc.data$UploadDateTime)
cc.data$ExposureDateTime_unclass <- unclass(cc.data$ExposureDateTime)

# Split date and time, and also create a character version for the date stamp.
cc.data$date_stamp <- as.Date(cc.data$ExposureDateTime)
cc.data$date_stamp_char <- as.character(cc.data$date_stamp)
cc.data$time_stamp <- format(cc.data$ExposureDateTime,"%H:00:00") # Truncate time to simulate hourly clustering
cc.data$upload_date <- as.Date(cc.data$UploadDateTime)
cc.data$upload_date_char <- as.character(cc.data$upload_date)

# reformat date for ci.data
ci.data$contact_date_char <- strptime(as.character(ci.data$contact_date), "%d/%m/%Y")
ci.data$contact_date_char <- format(ci.data$contact_date_char, "%Y-%m-%d")

# Calculate time variable from counts of each class (0, 1, 2) and create new variables.
cc.data$Class0Time <- cc.data$Class0Count * 2
cc.data$Class1Time <- (cc.data$Class1Count - cc.data$Class0Count) * 2
cc.data$Class2Time <- (cc.data$Class2Count - cc.data$Class1Count) * 2

###############################
## Subset the CovidCard data ##
###############################

# List the days for which the trial ran and so valid CovidCard data exists.
trial_days <- c("2020-11-09","2020-11-10","2020-11-11", "2020-11-12","2020-11-13","2020-11-14", "2020-11-15")

# Subset data to only include data from days of the trial.
cc.data.sub <- filter(cc.data, date_stamp_char %in% trial_days)
# If data were uploaded on Nov. 15 (i.e., CovidCard was returned then), then remove data for Nov. 15.
cc.data.sub <- filter(cc.data.sub, !(upload_date_char == "2020-11-15" & date_stamp_char == "2020-11-15"))

# Filter cc.data to only have "good" IDs.  (Some CovidCard IDs correspond to a number of cards that were not used.)
cc.data.sub <- cc.data.sub %>% filter((ObservingQRCode %in% real.ids$card_id) & (ObservedQRCode %in% real.ids$card_id))

# Confirm that all case investigation data IDs are contained in the set of "good" IDs.
length(unique(c(ci.data$card_id, ci.data$contact_id))) # Number of unique IDs for case investigation data.
sum(unique(c(ci.data$card_id, ci.data$contact_id)) %in% unique(c(cc.data.sub$ObservingQRCode, cc.data.sub$ObservedQRCode))) # Number that are in reduced subset.

####################
## Re-number IDs. ##
####################

# create new node ID columns
cc.data.sub$Sender <- cc.data.sub$ObservingQRCode
cc.data.sub$Receiver <- cc.data.sub$ObservedQRCode
ci.data$Sender <- ci.data$card_id
ci.data$Receiver <- ci.data$contact_id

# Determine the number of unique IDs (i.e., QR codes) for CovidCards.
n <- length(unique(c(cc.data.sub$ObservingQRCode, cc.data.sub$ObservedQRCode)))
# Create a vector of the unique IDs, sorted.
IDs <- sort(unique(c(cc.data.sub$ObservingQRCode, cc.data.sub$ObservedQRCode)))

# Recode all card IDs to be 1 to n.
for(i in 1 : n)
{
  cc.data.sub$Sender[cc.data.sub$Sender == IDs[i]] <- i
  cc.data.sub$Receiver[cc.data.sub$Receiver == IDs[i]] <- i
  ci.data$Sender[ci.data$Sender == IDs[i]] <- i
  ci.data$Receiver[ci.data$Receiver == IDs[i]] <- i
}

#########################################
## Create a dataset where interactions ##
## are grouped by day.                 ##
#########################################

# Group data by day (CovidCard data only).
cc.data.day <- group_by(cc.data.sub, date_stamp_char, Sender, Receiver) %>% summarise(Class0Total = sum(Class0Time), Class1Total = sum(Class1Time), Class2Total = sum(Class2Time))

# Examine number of unique (Observing, Observed) interactions by day.
table(cc.data.day$date_stamp_char)
table(ci.data$contact_date_char)

#~~~~~ Why is Nov. 9 so high for CovidCard data?  Should this be eliminated?  Why the jump again on Nov. 15? ~~~~ #

################################################
## Produce adjacency matrices of interactions ##
## by day.  Additionally, record durations    ##
## according to classes.                      ##
################################################

adjacency.matrices.cc <- list(NULL)
adjacency.matrices.ci <- list(NULL)
class0.matrices <- list(NULL)
class1.matrices <- list(NULL)
class2.matrices <- list(NULL)
class01.matrices <- list(NULL)
class12.matrices <- list(NULL)
class012.matrices <- list(NULL)

for(i in 1 : length(trial_days))
{
  # Construct an empty adjacency matrix for CovidCard interactions.
  a.mat <- matrix(0, nrow = n, ncol = n)
  # Construct an empty adjacency matrix for case investigation interactions.
  a.mat.ci <- matrix(0, nrow = n, ncol = n)
  
  # Construct matrices with missing durations for all classes for CovidCard interactions.
  #	b.mat <- c.mat <- d.mat <- e.mat <- f.mat <- g.mat <- matrix(NA, nrow = n, ncol = n)
  b.mat <- c.mat <- d.mat <- e.mat <- f.mat <- g.mat <- matrix(0, nrow = n, ncol = n)
  
  #~~~~ The second version, where cell entries are initialised to 0   ~~~~#
  #~~~~ can be used to not remove cases where one CovidCard does not  ~~~~#
  #~~~~ register an interaction.  This should reduce intraclass       ~~~~#
  #~~~~ to some degree, as it does not exclude these cases due to NA. ~~~~#          
  
  # Extract only data for a given day for CovidCard data.
  cc.data.day.sub <- filter(cc.data.day, date_stamp_char == trial_days[i])
  # Extract only data for a given day for case investigation data.
  ci.data.day <- filter(ci.data, contact_date_char == trial_days[i])
  
  # Record reported interactions from edgelist data for CovidCard.  Record durations in matrices as well.
  for(j in 1 : nrow(cc.data.day.sub))
  {
    a.mat[cc.data.day.sub$Sender[j], cc.data.day.sub$Receiver[j]] <- 1
    b.mat[cc.data.day.sub$Sender[j], cc.data.day.sub$Receiver[j]] <- cc.data.day.sub$Class0Total[j]
    c.mat[cc.data.day.sub$Sender[j], cc.data.day.sub$Receiver[j]] <- cc.data.day.sub$Class1Total[j]
    d.mat[cc.data.day.sub$Sender[j], cc.data.day.sub$Receiver[j]] <- cc.data.day.sub$Class2Total[j]
    e.mat[cc.data.day.sub$Sender[j], cc.data.day.sub$Receiver[j]] <- cc.data.day.sub$Class0Total[j] + cc.data.day.sub$Class1Total[j]
    f.mat[cc.data.day.sub$Sender[j], cc.data.day.sub$Receiver[j]] <- cc.data.day.sub$Class1Total[j] + cc.data.day.sub$Class2Total[j]
    g.mat[cc.data.day.sub$Sender[j], cc.data.day.sub$Receiver[j]] <- cc.data.day.sub$Class0Total[j] + cc.data.day.sub$Class1Total[j] + cc.data.day.sub$Class2Total[j]
  }
  
  # Record reported interactions from edgelist data for case investigation data.
  for(j in 1 : nrow(ci.data.day))
  {
    a.mat.ci[ci.data.day$Sender[j], ci.data.day$Receiver[j]] <- 1
  }
  
  # Check for repeated entries.  What to do if different? (Most second entries are 300 or 600 minutes)
  #	for(k in 1 : (nrow(ci.data.day) - 1))
  #	{
  #		for(l in (k + 1) : nrow(ci.data.day))
  #			if(sum(c(ci.data.day$Sender[k], ci.data.day$Receiver[k])  == c(ci.data.day$Sender[l], ci.data.day$Receiver[l])) == 2)
  #				print(c(k, l))
  #	}
  
  # Store matrices in corresponding lists.
  adjacency.matrices.cc[[i]] <- a.mat
  class0.matrices[[i]] <- b.mat
  class1.matrices[[i]] <- c.mat
  class2.matrices[[i]] <- d.mat
  class01.matrices[[i]] <- e.mat
  class12.matrices[[i]] <- f.mat
  class012.matrices[[i]] <- g.mat
  adjacency.matrices.ci[[i]] <- a.mat.ci
}

# Confirm that the number of edges reflected in the adjacency matrix for each day matches edgelist data.
for(i in 1 : length(trial_days))
{
  print(sum(adjacency.matrices.cc[[i]]))
  print(sum(adjacency.matrices.ci[[i]]))
}

# Tally number of mutual, asymmetric, and null dyads for CovidCard data for each day, and record percentage of reciprocated interactions.
dyad.state.tally.cc <- matrix(0, nrow = length(trial_days), ncol = 3)
daily.reciprocity.prop.cc <- matrix(0, nrow = length(trial_days), ncol = 2)

# Tally number of mutual, asymmetric, and null dyads for case investigation data for each day, and record percentage of reciprocated interactions.
dyad.state.tally.ci <- matrix(0, nrow = length(trial_days), ncol = 3)
daily.reciprocity.prop.ci <- matrix(0, nrow = length(trial_days), ncol = 2)

for(i in 1 : length(trial_days))
{
  # Construct a directed network object for CovidCard interactions for the day.
  day.net.cc <- network(adjacency.matrices.cc[[i]], directed = TRUE)
  # Construct a directed network object for case investigation interactions for the day.  The adjacency matrix needs to be restricted to rows and columns corresponding to "Sender" IDs in the case investigation dataset.
  day.net.ci <- network(adjacency.matrices.ci[[i]][sort(unique(ci.data$Sender)), sort(unique(ci.data$Sender))], directed = TRUE)
  # Carry out a dyad census for states.
  dyad.state.tally.cc[i, ] <- dyad.census(day.net.cc)
  daily.reciprocity.prop.cc[i, ] <- c(dyad.census(day.net.cc)[, 1] / sum(dyad.census(day.net.cc)[, 1 : 2]), sum(dyad.census(day.net.cc)[, 1 : 2]))
  dyad.state.tally.ci[i, ] <- dyad.census(day.net.ci)
  daily.reciprocity.prop.ci[i, ] <- c(dyad.census(day.net.ci)[, 1] / sum(dyad.census(day.net.ci)[, 1 : 2]), sum(dyad.census(day.net.ci)[, 1 : 2]))
}

# # Start a graphics device.
# pdf("Figures/Figures.pdf")

# Visualise proportion of reciprocated interactions by day for CoviCard data.  Include 95% Wald confidence bounds.
plot(1 : length(trial_days), daily.reciprocity.prop.cc[, 1], ylim = c(0.7, 0.9), type = "b", main = "Reciprocity (CovidCard Data)", xlab = "Day", ylab = "Proportion", pch = 16, cex = 2)
for(i in 1 : length(trial_days))
{
  p <- daily.reciprocity.prop.cc[i, 1]
  n <- daily.reciprocity.prop.cc[i, 2]
  points(rep(i, 2), c(p - qnorm(0.975) * sqrt(p *(1 - p) / n), p + qnorm(0.975) * sqrt(p *(1 - p) / n)), type = "l")
  points(c(i - 0.05, i + 0.05), rep(p - qnorm(0.975) * sqrt(p *(1 - p) / n), 2), type = "l")
  points(c(i - 0.05, i + 0.05), rep(p + qnorm(0.975) * sqrt(p *(1 - p) / n), 2), type = "l")
}

# Visualise proportion of reciprocated interactions by day for case investigation data.  Include 95% Wald confidence bounds.
plot(1 : length(trial_days), daily.reciprocity.prop.ci[, 1], ylim = c(0, 1), type = "b", main = "Reciprocity (Case Investigation Data)", xlab = "Day", ylab = "Proportion", pch = 16, cex = 2)
for(i in 1 : length(trial_days))
{
  p <- daily.reciprocity.prop.ci[i, 1]
  n <- daily.reciprocity.prop.ci[i, 2]
  points(rep(i, 2), c(p - qnorm(0.975) * sqrt(p *(1 - p) / n), p + qnorm(0.975) * sqrt(p *(1 - p) / n)), type = "l")
  points(c(i - 0.05, i + 0.05), rep(p - qnorm(0.975) * sqrt(p *(1 - p) / n), 2), type = "l")
  points(c(i - 0.05, i + 0.05), rep(p + qnorm(0.975) * sqrt(p *(1 - p) / n), 2), type = "l")
}

# Output tables of counts and proportions of reciprocal dyads.
colnames(dyad.state.tally.cc) <- c("Mutual", "Asymmetric", "Null")
colnames(daily.reciprocity.prop.cc) <- c("Proportion", "n")
dyad.state.tally.cc
daily.reciprocity.prop.cc

# Output tables of counts and proportions of reciprocal dyads.
colnames(dyad.state.tally.ci) <- c("Mutual", "Asymmetric", "Null")
colnames(daily.reciprocity.prop.ci) <- c("Proportion", "n")
dyad.state.tally.ci
daily.reciprocity.prop.ci

# Initialise matrices in which to store intraclass correlations and lower and upper bounds for 95% confidence intervals.
class0.icc.mat <- class1.icc.mat <- class2.icc.mat <- class01.icc.mat <- class12.icc.mat <- class012.icc.mat <- matrix(0, nrow = length(trial_days), ncol = 3) 

for(i in 1 : length(trial_days))
{
  # Extract upper triangular region cells and corresponding lower triangular region cells for each possible duration class or grouping.
  class0 <- cbind((class0.matrices[[i]])[upper.tri(class0.matrices[[i]], diag = FALSE)], (t(class0.matrices[[i]]))[upper.tri(t(class0.matrices[[i]]), diag = FALSE)])
  class1 <- cbind((class1.matrices[[i]])[upper.tri(class1.matrices[[i]], diag = FALSE)], (t(class1.matrices[[i]]))[upper.tri(t(class1.matrices[[i]]), diag = FALSE)])
  class2 <- cbind((class2.matrices[[i]])[upper.tri(class2.matrices[[i]], diag = FALSE)], (t(class2.matrices[[i]]))[upper.tri(t(class2.matrices[[i]]), diag = FALSE)])
  class01 <- cbind((class01.matrices[[i]])[upper.tri(class01.matrices[[i]], diag = FALSE)], (t(class01.matrices[[i]]))[upper.tri(t(class01.matrices[[i]]), diag = FALSE)])
  class12 <- cbind((class12.matrices[[i]])[upper.tri(class12.matrices[[i]], diag = FALSE)], (t(class12.matrices[[i]]))[upper.tri(t(class12.matrices[[i]]), diag = FALSE)])
  class012 <- cbind((class012.matrices[[i]])[upper.tri(class012.matrices[[i]], diag = FALSE)], (t(class012.matrices[[i]]))[upper.tri(t(class012.matrices[[i]]), diag = FALSE)])
  
  # Remove cases where neither card report any durations (i.e., do not interact).
  class0 <- class0[apply(class0, 1, sum, na.rm = TRUE) > 0, ]
  class1 <- class1[apply(class1, 1, sum, na.rm = TRUE) > 0, ]
  class2 <- class2[apply(class2, 1, sum, na.rm = TRUE) > 0, ]
  class01 <- class01[apply(class01, 1, sum, na.rm = TRUE) > 0, ]
  class12 <- class12[apply(class12, 1, sum, na.rm = TRUE) > 0, ]
  class012 <- class012[apply(class012, 1, sum, na.rm = TRUE) > 0, ]
  
  # Visualise reported durations for corresponding CovidCards in an interaction for each day.
  plot(class0[, 1], class0[, 2], xlab = "Duration in min. (First ID)", ylab = "Duration in min. (Second ID)", main = paste("Total Time (Class 0), Day", i))
  plot(class1[, 1], class1[, 2], xlab = "Duration in min. (First ID)", ylab = "Duration in min. (Second ID)", main = paste("Total Time (Class 1), Day", i))
  plot(class2[, 1], class2[, 2], xlab = "Duration in min. (First ID)", ylab = "Duration in min. (Second ID)", main = paste("Total Time (Class 2), Day", i))
  plot(class01[, 1], class01[, 2], xlab = "Duration in min. (First ID)", ylab = "Duration in min. (Second ID)", main = paste("Total Time (Class 01), Day", i))
  plot(class12[, 1], class12[, 2], xlab = "Duration in min. (First ID)", ylab = "Duration in min. (Second ID)", main = paste("Total Time (Class 12), Day", i))
  plot(class012[, 1], class012[, 2], xlab = "Duration in min. (First ID)", ylab = "Duration in min. (Second ID)", main = paste("Total Time (Class 012), Day", i))
  
  # Calculate intraclass correlations for classes for each day.  
  class0.icc <- icc(class0)
  class1.icc <- icc(class1)
  class2.icc <- icc(class2)
  class01.icc <- icc(class01)
  class12.icc <- icc(class12)
  class012.icc <- icc(class012)
  
  # Store point estimates and 95% confidence bounds for intraclass correlations.
  class0.icc.mat[i, ] <- c(class0.icc$value, class0.icc$lbound, class0.icc$ubound)
  class1.icc.mat[i, ] <- c(class1.icc$value, class1.icc$lbound, class1.icc$ubound)
  class2.icc.mat[i, ] <- c(class2.icc$value, class2.icc$lbound, class2.icc$ubound)
  class01.icc.mat[i, ] <- c(class01.icc$value, class01.icc$lbound, class01.icc$ubound)
  class12.icc.mat[i, ] <- c(class12.icc$value, class12.icc$lbound, class12.icc$ubound)
  class012.icc.mat[i, ] <- c(class012.icc$value, class012.icc$lbound, class012.icc$ubound)
}

# Plots of intraclass correlation by day.
for(i in 1 : length(trial_days))
{
  plot(1 : 6, c(class0.icc.mat[i, 1], class1.icc.mat[i, 1], class2.icc.mat[i, 1], class01.icc.mat[i, 1], class12.icc.mat[i, 1], class012.icc.mat[i, 1]), ylim = c(0.9, 1), xaxt = "n", main = paste("Intraclass Correlations (Day ", i, ")", sep = ""), xlab = "Classes", ylab = "ICC", pch = 16, cex = 2)
  points(rep(1, 2), class0.icc.mat[i, 2 : 3], type = "l")
  points(rep(2, 2), class1.icc.mat[i, 2 : 3], type = "l")
  points(rep(3, 2), class2.icc.mat[i, 2 : 3], type = "l")
  points(rep(4, 2), class01.icc.mat[i, 2 : 3], type = "l")
  points(rep(5, 2), class12.icc.mat[i, 2 : 3], type = "l")
  points(rep(6, 2), class012.icc.mat[i, 2 : 3], type = "l")
  axis(side = 1, at = 1 : 6, labels = c("0", "1", "2", "0 + 1", "1 + 2", "0 + 1 + 2"))
}

##############################
## Repeat for weekly totals ##
##############################

# Initialise objects in which to store aggregated durations.
class0.tot <- class1.tot <- class2.tot <- class01.tot <- class12.tot <- class012.tot <- 0

for(i in 1 : length(trial_days))
{
  class0.tot <- class0.tot + class0.matrices[[i]]
  class1.tot <- class1.tot + class1.matrices[[i]]
  class2.tot <- class2.tot + class2.matrices[[i]]
  class01.tot <- class01.tot + class01.matrices[[i]]
  class12.tot <- class12.tot + class12.matrices[[i]]
  class012.tot <- class012.tot + class012.matrices[[i]]
}

# Initialise a matrix in which to store weekly data intraclass correlations.
icc.mat <- matrix(0, nrow = 6, ncol = 3)

# Extract upper triangular region cells and corresponding lower triangular region cells for each possible duration class or grouping.
class0 <- cbind(class0.tot[upper.tri(class0.tot, diag = FALSE)], (t(class0.tot))[upper.tri(t(class0.tot), diag = FALSE)])
class1 <- cbind(class1.tot[upper.tri(class1.tot, diag = FALSE)], (t(class1.tot))[upper.tri(t(class1.tot), diag = FALSE)])
class2 <- cbind(class2.tot[upper.tri(class2.tot, diag = FALSE)], (t(class2.tot))[upper.tri(t(class2.tot), diag = FALSE)])
class01 <- cbind(class01.tot[upper.tri(class01.tot, diag = FALSE)], (t(class01.tot))[upper.tri(t(class01.tot), diag = FALSE)])
class12 <- cbind(class12.tot[upper.tri(class12.tot, diag = FALSE)], (t(class12.tot))[upper.tri(t(class12.tot), diag = FALSE)])
class012 <- cbind(class012.tot[upper.tri(class012.tot, diag = FALSE)], (t(class012.tot))[upper.tri(t(class012.tot), diag = FALSE)])

# Remove cases where neither card report any durations (i.e., do not interact).
class0 <- class0[apply(class0, 1, sum, na.rm = TRUE) > 0, ]
class1 <- class1[apply(class1, 1, sum, na.rm = TRUE) > 0, ]
class2 <- class2[apply(class2, 1, sum, na.rm = TRUE) > 0, ]
class01 <- class01[apply(class01, 1, sum, na.rm = TRUE) > 0, ]
class12 <- class12[apply(class12, 1, sum, na.rm = TRUE) > 0, ]
class012 <- class012[apply(class012, 1, sum, na.rm = TRUE) > 0, ]

# Calculate intraclass correlations for each classes for weekly data.  
class0.icc <- icc(class0)
class1.icc <- icc(class1)
class2.icc <- icc(class2)
class01.icc <- icc(class01)
class12.icc <- icc(class12)
class012.icc <- icc(class012)

# Store point estimates and 95% confidence bounds for intraclass correlations.
icc.mat[1, ] <- c(class0.icc$value, class0.icc$lbound, class0.icc$ubound)
icc.mat[2, ] <- c(class1.icc$value, class1.icc$lbound, class1.icc$ubound)
icc.mat[3, ] <- c(class2.icc$value, class2.icc$lbound, class2.icc$ubound)
icc.mat[4, ] <- c(class01.icc$value, class01.icc$lbound, class01.icc$ubound)
icc.mat[5, ] <- c(class12.icc$value, class12.icc$lbound, class12.icc$ubound)
icc.mat[6, ] <- c(class012.icc$value, class012.icc$lbound, class012.icc$ubound)

# Plot of intraclass correlation for weekly data.
plot(1 : 6, icc.mat[, 1], ylim = range(icc.mat), xaxt = "n", main = "Intraclass Correlations (Weekly Data)", xlab = "Classes", ylab = "ICC", pch = 16, cex = 2)
for(i in 1 : 6)
{
  points(rep(i, 2), icc.mat[i, 2 : 3], type = "l")
  points(c(i - 0.05, i + 0.05), rep(icc.mat[i, 2], 2), type = "l")
  points(c(i - 0.05, i + 0.05), rep(icc.mat[i, 3], 2), type = "l")
}
axis(side = 1, at = 1 : 6, labels = c("0", "1", "2", "0 + 1", "1 + 2", "0 + 1 + 2"))

# Output point estimates and 95% confidence bounds.
icc.mat

##############################################
## Further analysis of internal consistency ##
## of case investigation reports.           ##
##############################################

# For each adjacency matrix, decompose into two matrices representing mutual and asymmetric relations.  (If these are added, they should produce the original adjacency matrix.)
mutual.adjacency.matrices.ci <- asymmetric.adjacency.matrices.ci <- list(
)

for(i in 1 : length(trial_days))
{
  mutual.adjacency.matrices.ci[[i]] <- (adjacency.matrices.ci[[i]]) * t(adjacency.matrices.ci[[i]])
  asymmetric.adjacency.matrices.ci[[i]] <- adjacency.matrices.ci[[i]] - mutual.adjacency.matrices.ci[[i]]
}

# Compare consecutive days for asymmetric relations to see if symmetric elements report interactions
for(i in 1 : (length(trial_days) - 1))
{
  # Determine symmetric elements on consecutive days that both report interactions.
  to.be.reassigned <- t(asymmetric.adjacency.matrices.ci[[i]]) * (asymmetric.adjacency.matrices.ci[[i + 1]])
  
  # Print to the screen how many interactions can be reassigned.
  print(sum(to.be.reassigned))
  
  # Add 1s to adjacency matrix for the first day corresponding to symmetric elements from the second day.
  asymmetric.adjacency.matrices.ci[[i]] <- asymmetric.adjacency.matrices.ci[[i]] + to.be.reassigned
  # Remove 1s from the adjacency matrix for the second day corresponding to symmetric elements from the first day.
  asymmetric.adjacency.matrices.ci[[i + 1]] <- asymmetric.adjacency.matrices.ci[[i + 1]] - to.be.reassigned
}

# Combine mutual and updated asymmetric matrices for an "improved" consistency adjacency matrix for each day.
improved.adjacency.matrices.ci <- list()

for(i in 1 : length(trial_days))
{
  improved.adjacency.matrices.ci[[i]] <- mutual.adjacency.matrices.ci[[i]] + asymmetric.adjacency.matrices.ci[[i]]
}

# Tally number of mutual, asymmetric, and null dyads for case investigation data for each day, and record percentage of reciprocated interactions.
improved.dyad.state.tally.ci <- matrix(0, nrow = length(trial_days), ncol = 3)
improved.daily.reciprocity.prop.ci <- matrix(0, nrow = length(trial_days), ncol = 2)

for(i in 1 : length(trial_days))
{
  # Construct a directed network object for case investigation interactions for the day.  The adjacency matrix needs to be restricted to rows and columns corresponding to "Sender" IDs in the case investigation dataset.
  day.net.ci <- network(improved.adjacency.matrices.ci[[i]][sort(unique(ci.data$Sender)), sort(unique(ci.data$Sender))], directed = TRUE)
  # Carry out a dyad census for states.
  improved.dyad.state.tally.ci[i, ] <- dyad.census(day.net.ci)
  improved.daily.reciprocity.prop.ci[i, ] <- c(dyad.census(day.net.ci)[, 1] / sum(dyad.census(day.net.ci)[, 1 : 2]), sum(dyad.census(day.net.ci)[, 1 : 2]))
}

# Output tables of counts and proportions of reciprocal dyads.
colnames(improved.dyad.state.tally.ci) <- c("Mutual", "Asymmetric", "Null")
colnames(improved.daily.reciprocity.prop.ci) <- c("Proportion", "n")
improved.dyad.state.tally.ci
improved.daily.reciprocity.prop.ci

# For reference, original totals were as follows:
dyad.state.tally.ci
daily.reciprocity.prop.ci

# Weekly totals (original data).
c(sum(dyad.state.tally.ci[, 1]) / sum(dyad.state.tally.ci[, 1] + dyad.state.tally.ci[, 2]), sum(dyad.state.tally.ci[, 1] + dyad.state.tally.ci[, 2]))

# Weekly totals ("improved" data).
c(sum(improved.dyad.state.tally.ci[, 1]) / sum(improved.dyad.state.tally.ci[, 1] + improved.dyad.state.tally.ci[, 2]), sum(improved.dyad.state.tally.ci[, 1] + improved.dyad.state.tally.ci[, 2]))

###################################################
## Assess consistency between case investigation ##
## reports and CovidCard reports.                ##
###################################################

## Symmetrise adjacency matrices under two scenarios:
##   1) if either card/person notes an interaction, then it exists ("best").
##   2) if both cards/people note an interaction, then it exists ("worst").
best.adjacency.matrices.cc <- best.adjacency.matrices.ci <- worst.adjacency.matrices.cc <- worst.adjacency.matrices.ci <- list()

for(i in 1 : length(trial_days))
{
  best.adjacency.matrices.cc[[i]] <- adjacency.matrices.cc[[i]] * t(adjacency.matrices.cc[[i]]) + (adjacency.matrices.cc[[i]] - adjacency.matrices.cc[[i]] * t(adjacency.matrices.cc[[i]])) + t(adjacency.matrices.cc[[i]] - adjacency.matrices.cc[[i]] * t(adjacency.matrices.cc[[i]]))
  worst.adjacency.matrices.cc[[i]] <- adjacency.matrices.cc[[i]] * t(adjacency.matrices.cc[[i]])
  best.adjacency.matrices.ci[[i]] <- adjacency.matrices.ci[[i]] * t(adjacency.matrices.ci[[i]]) + (adjacency.matrices.ci[[i]] - adjacency.matrices.ci[[i]] * t(adjacency.matrices.ci[[i]])) + t(adjacency.matrices.ci[[i]] - adjacency.matrices.ci[[i]] * t(adjacency.matrices.ci[[i]]))
  worst.adjacency.matrices.ci[[i]] <- adjacency.matrices.ci[[i]] * t(adjacency.matrices.ci[[i]])
}

# For a given day, assess the consistency between case investigation and CovidCard reports.
agreement.best <- agreement.worst <- list()

for(i in 1 : length(trial_days))
{
  agreement.best[[i]] <- ifelse(best.adjacency.matrices.cc[[i]] == 0 & best.adjacency.matrices.ci[[i]] == 0, "Null", ifelse(best.adjacency.matrices.cc[[i]] == 1 & best.adjacency.matrices.ci[[i]] == 1, "Agree", ifelse(best.adjacency.matrices.cc[[i]] == 1 & best.adjacency.matrices.ci[[i]] == 0, "CC but not CI", "CI but not CC")))
  agreement.worst[[i]] <- ifelse(worst.adjacency.matrices.cc[[i]] == 0 & worst.adjacency.matrices.ci[[i]] == 0, "Null", ifelse(worst.adjacency.matrices.cc[[i]] == 1 & worst.adjacency.matrices.ci[[i]] == 1, "Agree", ifelse(worst.adjacency.matrices.cc[[i]] == 1 & worst.adjacency.matrices.ci[[i]] == 0, "CC but not CI", "CI but not CC")))
}

# Calculate agreement and disagreement by day.
agreement.best.mat <- agreement.worst.mat <- matrix(0, nrow = length(trial_days), ncol = 4)

for(i in 1 : length(trial_days))
{
  # Version 1: Consider all possible interactions reported by case investigation participants
  agreement.best.mat[i, ] <- c(sum(agreement.best[[i]][sort(unique(ci.data$Sender)),] == "Agree") - sum(agreement.best[[i]][sort(unique(ci.data$Sender)), sort(unique(ci.data$Sender))] == "Agree") / 2, sum(agreement.best[[i]][sort(unique(ci.data$Sender)),] == "CC but not CI") - sum(agreement.best[[i]][sort(unique(ci.data$Sender)), sort(unique(ci.data$Sender))] == "CC but not CI") / 2, sum(agreement.best[[i]][sort(unique(ci.data$Sender)),] == "CI but not CC") - sum(agreement.best[[i]][sort(unique(ci.data$Sender)), sort(unique(ci.data$Sender))] == "CI but not CC") / 2, sum(agreement.best[[i]][sort(unique(ci.data$Sender)),] == "Null") - sum(agreement.best[[i]][sort(unique(ci.data$Sender)), sort(unique(ci.data$Sender))] == "Null") / 2)
  
  # Version 2: Only consider possible interactions between case investigation participants
  #	agreement.best.mat[i, ] <- c(sum(agreement.best[[i]][sort(unique(ci.data$Sender)), sort(unique(ci.data$Sender))] == "Agree") / 2, sum(agreement.best[[i]][sort(unique(ci.data$Sender)), sort(unique(ci.data$Sender))] == "CC but not CI") / 2, sum(agreement.best[[i]][sort(unique(ci.data$Sender)), sort(unique(ci.data$Sender))] == "CI but not CC") / 2, sum(agreement.best[[i]][sort(unique(ci.data$Sender)), sort(unique(ci.data$Sender))] == "Null") / 2)
  
  
  # Version 1: Consider all possible interactions reported by case investigation participants
  agreement.worst.mat[i, ] <- c(sum(agreement.worst[[i]][sort(unique(ci.data$Sender)),] == "Agree") - sum(agreement.worst[[i]][sort(unique(ci.data$Sender)), sort(unique(ci.data$Sender))] == "Agree") / 2, sum(agreement.worst[[i]][sort(unique(ci.data$Sender)),] == "CC but not CI") - sum(agreement.worst[[i]][sort(unique(ci.data$Sender)), sort(unique(ci.data$Sender))] == "CC but not CI") / 2, sum(agreement.worst[[i]][sort(unique(ci.data$Sender)),] == "CI but not CC") - sum(agreement.worst[[i]][sort(unique(ci.data$Sender)), sort(unique(ci.data$Sender))] == "CI but not CC") / 2, sum(agreement.worst[[i]][sort(unique(ci.data$Sender)),] == "Null") - sum(agreement.worst[[i]][sort(unique(ci.data$Sender)), sort(unique(ci.data$Sender))] == "Null") / 2)
  
  # Version 2: Only consider possible interactions between case investigation participants
  #	agreement.worst.mat[i, ] <- c(sum(agreement.worst[[i]][sort(unique(ci.data$Sender)), sort(unique(ci.data$Sender))] == "Agree") / 2, sum(agreement.worst[[i]][sort(unique(ci.data$Sender)), sort(unique(ci.data$Sender))] == "CC but not CI") / 2, sum(agreement.worst[[i]][sort(unique(ci.data$Sender)), sort(unique(ci.data$Sender))] == "CI but not CC") / 2, sum(agreement.worst[[i]][sort(unique(ci.data$Sender)), sort(unique(ci.data$Sender))] == "Null") / 2)
}

colnames(agreement.best.mat) <- c("Agree", "CC but not CI", "CI but not CC", "Null")
colnames(agreement.worst.mat) <- c("Agree", "CC but not CI", "CI but not CC", "Null")
agreement.best.mat
agreement.worst.mat

#####################################
## CovidCards will pick up on      ##
## interactions that may not be    ##
## deemed "close" and so go        ##
## unreported.  Change sensitivity ##
## of class0 + class1 durations to ##
## try to maximise agreement       ##
## between case investigation data ##
## and CovidCard data.             ##
#####################################

# # Create a vector of duration thresholds to consider
# duration <- c(seq(0, 30, by = 2), seq(35, 60, by = 5))
# 
# proc.time()
# 
# #duration <- c(0, 2)
# 
# # Create an empty list in which to store CovidCard adjacency matrices imputed from class0 + class2 duration times.
# imputed.adjacency.matrices.cc <- best.imputed.adjacency.matrices.cc <- worst.imputed.adjacency.matrices.cc <- list()
# 
# # Calculate agreement and disagreement by day and duration value.  Dimensions correspond to (in order) day, agreement outcome, and duration value.
# agreement.best.array <- agreement.worst.array <- array(0, dim = c(length(trial_days), 4, length(duration)))
# 
# for(k in 1 : length(duration))
# {
# 	for(i in 1 : length(trial_days))
# 	{
# 		# Create an adjacency matrix of the imputed interactions based on interaction data.
# 		imputed.adjacency.matrices.cc[[i]] <- ifelse(class01.matrices[[i]] > duration[k], 1, 0)
# 		
# 		best.imputed.adjacency.matrices.cc[[i]] <- imputed.adjacency.matrices.cc[[i]] * t(imputed.adjacency.matrices.cc[[i]]) + (imputed.adjacency.matrices.cc[[i]] - imputed.adjacency.matrices.cc[[i]] * t(imputed.adjacency.matrices.cc[[i]])) + t(imputed.adjacency.matrices.cc[[i]] - imputed.adjacency.matrices.cc[[i]] * t(imputed.adjacency.matrices.cc[[i]]))
# 		worst.imputed.adjacency.matrices.cc[[i]] <- imputed.adjacency.matrices.cc[[i]] * t(imputed.adjacency.matrices.cc[[i]])
# 	}
# 
# 	# For a given day, assess the consistency between case investigation and CovidCard reports.
# 	for(i in 1 : length(trial_days))
# 	{
# 		agreement.best[[i]] <- ifelse(best.imputed.adjacency.matrices.cc[[i]] == 0 & best.adjacency.matrices.ci[[i]] == 0, "Null", ifelse(best.imputed.adjacency.matrices.cc[[i]] == 1 & best.adjacency.matrices.ci[[i]] == 1, "Agree", ifelse(best.imputed.adjacency.matrices.cc[[i]] == 1 & best.adjacency.matrices.ci[[i]] == 0, "CC but not CI", "CI but not CC")))
# 		agreement.worst[[i]] <- ifelse(worst.imputed.adjacency.matrices.cc[[i]] == 0 & worst.adjacency.matrices.ci[[i]] == 0, "Null", ifelse(worst.imputed.adjacency.matrices.cc[[i]] == 1 & worst.adjacency.matrices.ci[[i]] == 1, "Agree", ifelse(worst.imputed.adjacency.matrices.cc[[i]] == 1 & worst.adjacency.matrices.ci[[i]] == 0, "CC but not CI", "CI but not CC")))
# 	}
# 
# 	for(i in 1 : length(trial_days))
# 	{
# 		# Version 1: Consider all possible interactions reported by case investigation participants
# 		agreement.best.array[i, , k] <- c(sum(agreement.best[[i]][sort(unique(ci.data$Sender)),] == "Agree") - sum(agreement.best[[i]][sort(unique(ci.data$Sender)), sort(unique(ci.data$Sender))] == "Agree") / 2, sum(agreement.best[[i]][sort(unique(ci.data$Sender)),] == "CC but not CI") - sum(agreement.best[[i]][sort(unique(ci.data$Sender)), sort(unique(ci.data$Sender))] == "CC but not CI") / 2, sum(agreement.best[[i]][sort(unique(ci.data$Sender)),] == "CI but not CC") - sum(agreement.best[[i]][sort(unique(ci.data$Sender)), sort(unique(ci.data$Sender))] == "CI but not CC") / 2, sum(agreement.best[[i]][sort(unique(ci.data$Sender)),] == "Null") - sum(agreement.best[[i]][sort(unique(ci.data$Sender)), sort(unique(ci.data$Sender))] == "Null") / 2)
# 
# 		# Version 2: Only consider possible interactions between case investigation participants
# #		agreement.best.array[i, , k] <- c(sum(agreement.best[[i]][sort(unique(ci.data$Sender)), sort(unique(ci.data$Sender))] == "Agree") / 2, sum(agreement.best[[i]][sort(unique(ci.data$Sender)), sort(unique(ci.data$Sender))] == "CC but not CI") / 2, sum(agreement.best[[i]][sort(unique(ci.data$Sender)), sort(unique(ci.data$Sender))] == "CI but not CC") / 2, sum(agreement.best[[i]][sort(unique(ci.data$Sender)), sort(unique(ci.data$Sender))] == "Null") / 2)
# 	
# 		# Version 1: Consider all possible interactions reported by case investigation participants
# 		agreement.worst.array[i, , k] <- c(sum(agreement.worst[[i]][sort(unique(ci.data$Sender)),] == "Agree") - sum(agreement.worst[[i]][sort(unique(ci.data$Sender)), sort(unique(ci.data$Sender))] == "Agree") / 2, sum(agreement.worst[[i]][sort(unique(ci.data$Sender)),] == "CC but not CI") - sum(agreement.worst[[i]][sort(unique(ci.data$Sender)), sort(unique(ci.data$Sender))] == "CC but not CI") / 2, sum(agreement.worst[[i]][sort(unique(ci.data$Sender)),] == "CI but not CC") - sum(agreement.worst[[i]][sort(unique(ci.data$Sender)), sort(unique(ci.data$Sender))] == "CI but not CC") / 2, sum(agreement.worst[[i]][sort(unique(ci.data$Sender)),] == "Null") - sum(agreement.worst[[i]][sort(unique(ci.data$Sender)), sort(unique(ci.data$Sender))] == "Null") / 2)
# 
# 		# Version 2: Only consider possible interactions between case investigation participants
# #		agreement.worst.array[i, , k] <- c(sum(agreement.worst[[i]][sort(unique(ci.data$Sender)), sort(unique(ci.data$Sender))] == "Agree") / 2, sum(agreement.worst[[i]][sort(unique(ci.data$Sender)), sort(unique(ci.data$Sender))] == "CC but not CI") / 2, sum(agreement.worst[[i]][sort(unique(ci.data$Sender)), sort(unique(ci.data$Sender))] == "CI but not CC") / 2, sum(agreement.worst[[i]][sort(unique(ci.data$Sender)), sort(unique(ci.data$Sender))] == "Null") / 2)
# 	}
# }
# 
# dimnames(agreement.best.array) <- list(c("Day 1", "Day 2", "Day 3", "Day 4", "Day 5", "Day 6", "Day 7"), c("Agree", "CC but not CI", "CI but not CC", "Null"), NULL)
# dimnames(agreement.worst.array) <- list(c("Day 1", "Day 2", "Day 3", "Day 4", "Day 5", "Day 6", "Day 7"), c("Agree", "CC but not CI", "CI but not CC", "Null"), NULL)
# 
# # Calculate true positive and false positive rates
# tpr <- fpr <- matrix(0, nrow = k, ncol = length(trial_days))
# for(k in 1 : length(duration))
# {
# 	for(i in 1 : length(trial_days))
# 	{
# 		tpr[k, i] <- agreement.best.array[i, 1, k] / (agreement.best.array[i, 1, k] + agreement.best.array[i, 3, k])
# 		fpr[k, i] <- agreement.best.array[i, 2, k] / (agreement.best.array[i, 2, k] + agreement.best.array[i, 4, k])
# 	}
# }
# 
# # This isn't really a true ROC curve.  
# for(i in 1 : length(trial_days))
# {
# 	plot(fpr[, i], tpr[, i], xlab = "FPR", ylab = "TPR", type = "l")
# }
# 
# # Turn off graphics device.
# dev.off()
# 
# proc.time()

#######################
## Predictive models ##
#######################

Y <- X0 <- X1 <- X2 <- reg.data.upper <- reg.data.lower <- list()

for(i in 1 : length(trial_days))
{
  Y[[i]] <- c((best.adjacency.matrices.ci[[i]][sort(unique(ci.data$Sender)), sort(unique(ci.data$Sender))])[upper.tri(best.adjacency.matrices.ci[[i]][sort(unique(ci.data$Sender)), sort(unique(ci.data$Sender))], diag = FALSE)], (best.adjacency.matrices.ci[[i]][sort(unique(ci.data$Sender)), -sort(unique(ci.data$Sender))]))
  X0[[i]] <- c((class0.matrices[[i]][sort(unique(ci.data$Sender)), sort(unique(ci.data$Sender))])[upper.tri(class0.matrices[[i]][sort(unique(ci.data$Sender)), sort(unique(ci.data$Sender))], diag = FALSE)], (class0.matrices[[i]][sort(unique(ci.data$Sender)), -sort(unique(ci.data$Sender))]))
  X1[[i]] <- c((class1.matrices[[i]][sort(unique(ci.data$Sender)), sort(unique(ci.data$Sender))])[upper.tri(class1.matrices[[i]][sort(unique(ci.data$Sender)), sort(unique(ci.data$Sender))], diag = FALSE)], (class1.matrices[[i]][sort(unique(ci.data$Sender)), -sort(unique(ci.data$Sender))]))
  X2[[i]] <- c((class2.matrices[[i]][sort(unique(ci.data$Sender)), sort(unique(ci.data$Sender))])[upper.tri(class2.matrices[[i]][sort(unique(ci.data$Sender)), sort(unique(ci.data$Sender))], diag = FALSE)], (class2.matrices[[i]][sort(unique(ci.data$Sender)), -sort(unique(ci.data$Sender))]))
  
  # Combine response and explanatory variables into separate data frames for each day.
  reg.data.upper[[i]] <- data.frame(Y.reg = Y[[i]], X0.reg = X0[[i]], X1.reg = X1[[i]], X2.reg = X2[[i]])
}

for(i in 1 : length(trial_days))
{
  Y[[i]] <- c((best.adjacency.matrices.ci[[i]][sort(unique(ci.data$Sender)), sort(unique(ci.data$Sender))])[upper.tri(best.adjacency.matrices.ci[[i]][sort(unique(ci.data$Sender)), sort(unique(ci.data$Sender))], diag = FALSE)], (best.adjacency.matrices.ci[[i]][sort(unique(ci.data$Sender)), -sort(unique(ci.data$Sender))]))
  X0[[i]] <- c((t(class0.matrices[[i]])[sort(unique(ci.data$Sender)), sort(unique(ci.data$Sender))])[upper.tri(t(class0.matrices[[i]])[sort(unique(ci.data$Sender)), sort(unique(ci.data$Sender))], diag = FALSE)], (t(class0.matrices[[i]])[sort(unique(ci.data$Sender)), -sort(unique(ci.data$Sender))]))
  X1[[i]] <- c((t(class1.matrices[[i]])[sort(unique(ci.data$Sender)), sort(unique(ci.data$Sender))])[upper.tri(t(class1.matrices[[i]])[sort(unique(ci.data$Sender)), sort(unique(ci.data$Sender))], diag = FALSE)], (t(class1.matrices[[i]])[sort(unique(ci.data$Sender)), -sort(unique(ci.data$Sender))]))
  X2[[i]] <- c((t(class2.matrices[[i]])[sort(unique(ci.data$Sender)), sort(unique(ci.data$Sender))])[upper.tri(t(class2.matrices[[i]])[sort(unique(ci.data$Sender)), sort(unique(ci.data$Sender))], diag = FALSE)], (t(class2.matrices[[i]])[sort(unique(ci.data$Sender)), -sort(unique(ci.data$Sender))]))
  # Y[[i]] <- c((best.adjacency.matrices.ci[[i]][sort(unique(ci.data$Sender)), sort(unique(ci.data$Sender))])[lower.tri(best.adjacency.matrices.ci[[i]][sort(unique(ci.data$Sender)), sort(unique(ci.data$Sender))], diag = FALSE)], (best.adjacency.matrices.ci[[i]][-sort(unique(ci.data$Sender)), sort(unique(ci.data$Sender))]))
  # X0[[i]] <- c((class0.matrices[[i]][sort(unique(ci.data$Sender)), sort(unique(ci.data$Sender))])[lower.tri(class0.matrices[[i]][sort(unique(ci.data$Sender)), sort(unique(ci.data$Sender))], diag = FALSE)], (class0.matrices[[i]][-sort(unique(ci.data$Sender)), sort(unique(ci.data$Sender))]))
  # X1[[i]] <- c((class1.matrices[[i]][sort(unique(ci.data$Sender)), sort(unique(ci.data$Sender))])[lower.tri(class1.matrices[[i]][sort(unique(ci.data$Sender)), sort(unique(ci.data$Sender))], diag = FALSE)], (class1.matrices[[i]][-sort(unique(ci.data$Sender)), sort(unique(ci.data$Sender))]))
  # X2[[i]] <- c((class2.matrices[[i]][sort(unique(ci.data$Sender)), sort(unique(ci.data$Sender))])[lower.tri(class2.matrices[[i]][sort(unique(ci.data$Sender)), sort(unique(ci.data$Sender))], diag = FALSE)], (class2.matrices[[i]][-sort(unique(ci.data$Sender)), sort(unique(ci.data$Sender))]))
  
  # Combine response and explanatory variables into separate data frames for each day.
  reg.data.lower[[i]] <- data.frame(Y.reg = Y[[i]], X0.reg = X0[[i]], X1.reg = X1[[i]], X2.reg = X2[[i]])
}

# create dataset for full week
reg.data.week.upper <- NULL
reg.data.week.lower <- NULL

for(i in 1 : length(trial_days))
{
  reg.data.week.upper <- rbind(reg.data.week.upper, reg.data.upper[[i]])
  reg.data.week.lower <- rbind(reg.data.week.lower, reg.data.lower[[i]])
}

# random diag
X0.reg.both <- cbind(reg.data.week.upper$X0.reg, reg.data.week.lower$X0.reg)
X1.reg.both <- cbind(reg.data.week.upper$X1.reg, reg.data.week.lower$X1.reg)
X2.reg.both <- cbind(reg.data.week.upper$X2.reg, reg.data.week.lower$X2.reg)

choice1 <- sample(c(0,1), nrow(reg.data.week.upper), replace = TRUE)
choice2 <- abs(choice1-1)
choice <- cbind(choice1,choice2)

reg.data.week <- data.frame(Y.reg = reg.data.week.upper$Y.reg,
                                 X0.reg = apply(X0.reg.both*choice, 1, sum),
                                 X1.reg = apply(X1.reg.both*choice, 1, sum),
                                 X2.reg = apply(X2.reg.both*choice, 1, sum))

# # upper diag
# reg.data.week <- reg.data.week.upper
# 
# # lower diag
# reg.data.week <- reg.data.week.lower

# remove cases where neither case investigation data nor CovidCard data recorded an interaction.
reg.data.sub.week <- reg.data.week[apply(reg.data.week, 1, sum) > 0,]

# rename reponse levels so that AUC can be calculated
reg.data.sub.week$Y.reg <- factor(reg.data.sub.week$Y.reg)
levels(reg.data.sub.week$Y.reg) <- c("not_close", "close")

# filter to get a more sensical dataset
reg.data.sub.week <- reg.data.sub.week %>% filter(!((Y.reg=="not_close") & (X0.reg+X1.reg>20))&
                                                    !((Y.reg=="close") & (X0.reg+X1.reg<2)))


##### train logistic regression model #####

# 10-fold repeated CV calc AUC, Sens and Spec
fitControl <- trainControl(
  method="repeatedcv", 
  number = 10,
  repeats = 10,
  summaryFunction=twoClassSummary, 
  classProbs=TRUE,
  savePredictions = TRUE)

#### fit null model as reference point - not working ####
# mod_null <- train(Y.reg ~ 1,
#                   data = reg.data.sub[[i]], 
#                   trControl = fitControl,
#                   method = "glm",
#                   family = "binomial")
# 
# # results
# mod_null$results
# summary(mod_null)
# 
# # Plot:
# plot.roc(mod_null$pred$obs,
#          mod_null$pred$close)


#### train the model on raw subsetted data ####

# ## full model
# mod_full <- train(Y.reg ~ X0.reg +
#                     X1.reg + X2.reg,
#                   data = reg.data.sub[[i]], 
#                   trControl = fitControl,
#                   method = "glm",
#                   family = "binomial")
# 
# # results
# mod_full$results
# summary(mod_full)
# 
# # Plot:
# plot.roc(mod_full$pred$obs,
#          mod_full$pred$close)
# 
# ## 01 model
# mod_01 <- train(Y.reg ~ X0.reg +
#                   X1.reg,
#                 data = reg.data.sub[[i]], 
#                 trControl = fitControl,
#                 method = "glm",
#                 family = "binomial")
# 
# # results
# mod_01$results
# summary(mod_01)
# 
# # Plot:
# plot.roc(mod_01$pred$obs,
#          mod_01$pred$close)
# 
# ## 02 model
# mod_02 <- train(Y.reg ~ X0.reg +
#                   X2.reg,
#                 data = reg.data.sub[[i]], 
#                 trControl = fitControl,
#                 method = "glm",
#                 family = "binomial")
# 
# # results
# mod_02$results
# summary(mod_02)
# 
# # Plot:
# plot.roc(mod_02$pred$obs,
#          mod_02$pred$close)
# 
# ## 12 model
# mod_12 <- train(Y.reg ~ X1.reg +
#                   X2.reg,
#                 data = reg.data.sub[[i]], 
#                 trControl = fitControl,
#                 method = "glm",
#                 family = "binomial")
# 
# # results
# mod_12$results
# summary(mod_12)
# 
# # Plot:
# plot.roc(mod_12$pred$obs,
#          mod_12$pred$close)
# 
# ## 0 model
# mod_0 <- train(Y.reg ~ X0.reg,
#                data = reg.data.sub[[i]], 
#                trControl = fitControl,
#                method = "glm",
#                family = "binomial")
# 
# # results
# mod_0$results
# summary(mod_0)
# 
# # Plot:
# plot.roc(mod_0$pred$obs,
#          mod_0$pred$close)
# 
# ## 1 model
# mod_1 <- train(Y.reg ~ X1.reg,
#                data = reg.data.sub[[i]], 
#                trControl = fitControl,
#                method = "glm",
#                family = "binomial")
# 
# # results
# mod_1$results
# summary(mod_1)
# 
# # Plot:
# plot.roc(mod_1$pred$obs,
#          mod_1$pred$close)
# 
# ## 2 model
# mod_2 <- train(Y.reg ~ X2.reg,
#                data = reg.data.sub[[i]], 
#                trControl = fitControl,
#                method = "glm",
#                family = "binomial")
# 
# # results
# mod_2$results
# summary(mod_2)
# 
# # Plot:
# plot.roc(mod_2$pred$obs,
#          mod_2$pred$close)
# 
# 
# #### train the model on raw subsetted full week data
# 
# ## full model
# mod_full <- train(Y.reg ~ X0.reg +
#                     X1.reg + X2.reg,
#                   data = reg.data.sub.week, 
#                   trControl = fitControl,
#                   method = "glm",
#                   family = "binomial")
# 
# # results
# mod_full$results
# summary(mod_full)
# 
# # Plot:
# plot.roc(mod_full$pred$obs,
#          mod_full$pred$close)
# 
# ## 01 model
# mod_01 <- train(Y.reg ~ X0.reg +
#                   X1.reg,
#                 data = reg.data.sub.week, 
#                 trControl = fitControl,
#                 method = "glm",
#                 family = "binomial")
# 
# # results
# mod_01$results
# summary(mod_01)
# 
# # Plot:
# plot.roc(mod_01$pred$obs,
#          mod_01$pred$close)
# 
# ## 02 model
# mod_02 <- train(Y.reg ~ X0.reg +
#                   X2.reg,
#                 data = reg.data.sub.week, 
#                 trControl = fitControl,
#                 method = "glm",
#                 family = "binomial")
# 
# # results
# mod_02$results
# summary(mod_02)
# 
# # Plot:
# plot.roc(mod_02$pred$obs,
#          mod_02$pred$close)
# 
# ## 12 model
# mod_12 <- train(Y.reg ~ X1.reg +
#                   X2.reg,
#                 data = reg.data.sub.week, 
#                 trControl = fitControl,
#                 method = "glm",
#                 family = "binomial")
# 
# # results
# mod_12$results
# summary(mod_12)
# 
# # Plot:
# plot.roc(mod_12$pred$obs,
#          mod_12$pred$close)
# 
# ## 0 model
# mod_0 <- train(Y.reg ~ X0.reg,
#                data = reg.data.sub.week, 
#                trControl = fitControl,
#                method = "glm",
#                family = "binomial")
# 
# # results
# mod_0$results
# summary(mod_0)
# 
# # Plot:
# plot.roc(mod_0$pred$obs,
#          mod_0$pred$close)
# 
# ## 1 model
# mod_1 <- train(Y.reg ~ X1.reg,
#                data = reg.data.sub.week, 
#                trControl = fitControl,
#                method = "glm",
#                family = "binomial")
# 
# # results
# mod_1$results
# summary(mod_1)
# 
# # Plot:
# plot.roc(mod_1$pred$obs,
#          mod_1$pred$close)
# 
# ## 2 model
# mod_2 <- train(Y.reg ~ X2.reg,
#                data = reg.data.sub.week, 
#                trControl = fitControl,
#                method = "glm",
#                family = "binomial")
# 
# # results
# mod_2$results
# summary(mod_2)
# 
# # Plot:
# plot.roc(mod_2$pred$obs,
#          mod_2$pred$close)
# 
# #### train the model on transformed subsetted data
# 
# ## full model
# mod_full <- train(Y.reg ~ I(log(X0.reg+1)) +
#                     I(log(X1.reg+1)) + I(log(X2.reg+1)),
#                   data = reg.data.sub[[i]], 
#                   trControl = fitControl,
#                   method = "glm",
#                   family = "binomial")
# 
# # results
# mod_full$results
# summary(mod_full)
# 
# # Plot:
# plot.roc(mod_full$pred$obs,
#          mod_full$pred$close)
# 
# ## 01 model
# mod_01 <- train(Y.reg ~ I(log(X0.reg+1)) +
#                   I(log(X1.reg+1)),
#                 data = reg.data.sub[[i]], 
#                 trControl = fitControl,
#                 method = "glm",
#                 family = "binomial")
# 
# # results
# mod_01$results
# summary(mod_01)
# 
# # Plot:
# plot.roc(mod_01$pred$obs,
#          mod_01$pred$close)
# 
# ## 02 model
# mod_02 <- train(Y.reg ~ I(log(X0.reg+1)) +
#                   I(log(X2.reg+1)),
#                 data = reg.data.sub[[i]], 
#                 trControl = fitControl,
#                 method = "glm",
#                 family = "binomial")
# 
# # results
# mod_02$results
# summary(mod_02)
# 
# # Plot:
# plot.roc(mod_02$pred$obs,
#          mod_02$pred$close)
# 
# ## 12 model
# mod_12 <- train(Y.reg ~ I(log(X1.reg+1)) +
#                   I(log(X2.reg+1)),
#                 data = reg.data.sub[[i]], 
#                 trControl = fitControl,
#                 method = "glm",
#                 family = "binomial")
# 
# # results
# mod_12$results
# summary(mod_12)
# 
# # Plot:
# plot.roc(mod_12$pred$obs,
#          mod_12$pred$close)
# 
# ## 0 model
# mod_0 <- train(Y.reg ~ I(log(X0.reg+1)),
#                data = reg.data.sub[[i]], 
#                trControl = fitControl,
#                method = "glm",
#                family = "binomial")
# 
# # results
# mod_0$results
# summary(mod_0)
# 
# # Plot:
# plot.roc(mod_0$pred$obs,
#          mod_0$pred$close)
# 
# ## 1 model
# mod_1 <- train(Y.reg ~ I(log(X1.reg+1)),
#                data = reg.data.sub[[i]], 
#                trControl = fitControl,
#                method = "glm",
#                family = "binomial")
# 
# # results
# mod_1$results
# summary(mod_1)
# 
# # Plot:
# plot.roc(mod_1$pred$obs,
#          mod_1$pred$close)
# 
# ## 2 model
# mod_2 <- train(Y.reg ~ I(log(X2.reg+1)),
#                data = reg.data.sub[[i]], 
#                trControl = fitControl,
#                method = "glm",
#                family = "binomial")
# 
# # results
# mod_2$results
# summary(mod_2)
# 
# # Plot:
# plot.roc(mod_2$pred$obs,
#          mod_2$pred$close)

#### train the model on transformed subsetted full week data ####

# set seed
set.seed(123)

## full model
mod_full <- train(Y.reg ~ I(log(X0.reg+1)) +
                    I(log(X1.reg+1)) + I(log(X2.reg+1)),
                  data = reg.data.sub.week, 
                  trControl = fitControl,
                  method = "glm",
                  family = "binomial")

# results
mod_full$results
summary(mod_full)

# Plot:
plot.roc(mod_full$pred$obs,
         mod_full$pred$close)

# ## 01 model
# mod_01 <- train(Y.reg ~ I(log(X0.reg+1)) +
#                   I(log(X1.reg+1)),
#                 data = reg.data.sub.week, 
#                 trControl = fitControl,
#                 method = "glm",
#                 family = "binomial")
# 
# # results
# mod_01$results
# summary(mod_01)
# 
# # Plot:
# plot.roc(mod_01$pred$obs,
#          mod_01$pred$close)
# 
# ## 02 model
# mod_02 <- train(Y.reg ~ I(log(X0.reg+1)) +
#                   I(log(X2.reg+1)),
#                 data = reg.data.sub.week, 
#                 trControl = fitControl,
#                 method = "glm",
#                 family = "binomial")
# 
# # results
# mod_02$results
# summary(mod_02)
# 
# # Plot:
# plot.roc(mod_02$pred$obs,
#          mod_02$pred$close)
# 
# ## 12 model
# mod_12 <- train(Y.reg ~ I(log(X1.reg+1)) +
#                   I(log(X2.reg+1)),
#                 data = reg.data.sub.week, 
#                 trControl = fitControl,
#                 method = "glm",
#                 family = "binomial")
# 
# # results
# mod_12$results
# summary(mod_12)
# 
# # Plot:
# plot.roc(mod_12$pred$obs,
#          mod_12$pred$close)
# 
# ## 0 model
# mod_0 <- train(Y.reg ~ I(log(X0.reg+1)),
#                data = reg.data.sub.week, 
#                trControl = fitControl,
#                method = "glm",
#                family = "binomial")
# 
# # results
# mod_0$results
# summary(mod_0)
# 
# # Plot:
# plot.roc(mod_0$pred$obs,
#          mod_0$pred$close)
# 
# ## 1 model
# mod_1 <- train(Y.reg ~ I(log(X1.reg+1)),
#                data = reg.data.sub.week, 
#                trControl = fitControl,
#                method = "glm",
#                family = "binomial")
# 
# # results
# mod_1$results
# summary(mod_1)
# 
# # Plot:
# plot.roc(mod_1$pred$obs,
#          mod_1$pred$close)
# 
# ## 2 model
# mod_2 <- train(Y.reg ~ I(log(X2.reg+1)),
#                data = reg.data.sub.week, 
#                trControl = fitControl,
#                method = "glm",
#                family = "binomial")
# 
# # results
# mod_2$results
# summary(mod_2)
# 
# # Plot:
# plot.roc(mod_2$pred$obs,
#          mod_2$pred$close)


#############################################################################
################### Linear discriminant analysis ############################
#############################################################################

# set.seed(1)
# 
# ## full model
# mod_full <- train(Y.reg ~ I(log(X0.reg+1)) +
#                     I(log(X1.reg+1)) + I(log(X2.reg+1)),
#                   data = reg.data.sub.week, 
#                   trControl = fitControl,
#                   method = "lda")
# 
# mod_full_reg <- train(Y.reg ~ I(log(X0.reg+1)) +
#                     I(log(X1.reg+1)) + I(log(X2.reg+1)),
#                   data = reg.data.sub.week, 
#                   trControl = fitControl,
#                   method = "glm",
#                   family = "binomial")
# 
# # results
# mod_full$results
# 
# # Plot:
# plot.roc(mod_full$pred$obs,
#          mod_full$pred$close)
# 
# ## 01 model
# mod_01 <- train(Y.reg ~ I(log(X0.reg+1)) +
#                   I(log(X1.reg+1)),
#                 data = reg.data.sub.week, 
#                 trControl = fitControl,
#                 method = "lda")
# 
# # results
# mod_01$results
# 
# # Plot:
# plot.roc(mod_01$pred$obs,
#          mod_01$pred$close)
# 
# ## 02 model
# mod_02 <- train(Y.reg ~ I(log(X0.reg+1)) +
#                   I(log(X2.reg+1)),
#                 data = reg.data.sub.week, 
#                 trControl = fitControl,
#                 method = "lda")
# 
# # results
# mod_02$results
# 
# # Plot:
# plot.roc(mod_02$pred$obs,
#          mod_02$pred$close)
# 
# ## 12 model
# mod_12 <- train(Y.reg ~ I(log(X1.reg+1)) +
#                   I(log(X2.reg+1)),
#                 data = reg.data.sub.week, 
#                 trControl = fitControl,
#                 method = "lda")
# 
# # results
# mod_12$results
# 
# # Plot:
# plot.roc(mod_12$pred$obs,
#          mod_12$pred$close)
# 
# ## 0 model
# mod_0 <- train(Y.reg ~ I(log(X0.reg+1)),
#                data = reg.data.sub.week, 
#                trControl = fitControl,
#                method = "lda")
# 
# # results
# mod_0$results
# 
# # Plot:
# plot.roc(mod_0$pred$obs,
#          mod_0$pred$close)
# 
# ## 1 model
# mod_1 <- train(Y.reg ~ I(log(X1.reg+1)),
#                data = reg.data.sub.week, 
#                trControl = fitControl,
#                method = "lda")
# 
# # results
# mod_1$results
# 
# # Plot:
# plot.roc(mod_1$pred$obs,
#          mod_1$pred$close)
# 
# ## 2 model
# mod_2 <- train(Y.reg ~ I(log(X2.reg+1)),
#                data = reg.data.sub.week, 
#                trControl = fitControl,
#                method = "lda")
# 
# # results
# mod_2$results
# 
# # Plot:
# plot.roc(mod_2$pred$obs,
#          mod_2$pred$close)


#######################################################################
############### Calculate edge weights using model ####################
#######################################################################
n <- length(unique(c(cc.data.sub$ObservingQRCode, cc.data.sub$ObservedQRCode)))
weighted.matrices <- list()

for(i in 1 : length(trial_days))
{
  # Construct matrices with missing durations for all classes for CovidCard interactions.
  #	b.mat <- c.mat <- d.mat <- e.mat <- f.mat <- g.mat <- matrix(NA, nrow = n, ncol = n)
  weighted.mat <- matrix(0, nrow = n, ncol = n)
  
  #~~~~ The second version, where cell entries are initialised to 0   ~~~~#
  #~~~~ can be used to not remove cases where one CovidCard does not  ~~~~#
  #~~~~ register an interaction.  This should reduce intraclass       ~~~~#
  #~~~~ to some degree, as it does not exclude these cases due to NA. ~~~~#          
  
  # Extract only data for a given day for CovidCard data.
  cc.data.day.sub <- filter(cc.data.day, date_stamp_char == trial_days[i])
  
  # Record reported interactions from edgelist data for CovidCard.  Record durations in matrices as well.
  for(j in 1 : nrow(cc.data.day.sub))
  {
    # temp <- data.frame(X0.reg=I(log(cc.data.day.sub$Class0Total[j]+1)),
    #                    X1.reg=I(log(cc.data.day.sub$Class1Total[j]+1)),
    #                    X2.reg=I(log(cc.data.day.sub$Class2Total[j]+1)))
    temp <- data.frame(X0.reg=cc.data.day.sub$Class0Total[j],
                       X1.reg=cc.data.day.sub$Class1Total[j],
                       X2.reg=cc.data.day.sub$Class2Total[j])
    
    weighted.mat[cc.data.day.sub$Sender[j], cc.data.day.sub$Receiver[j]] <- as.numeric(predict(mod_full, newdata = temp, type = "prob")[2])
  }
  
  # Store matrix in list
  weighted.matrices[[i]] <- weighted.mat
}


################################################################################
############################ Network analysis ##################################
################################################################################

# create network object for which to carry out network section of analysis
weighted.net <- list()
for(i in 1 : length(trial_days))
{
  # copy upper tri over lower tri to symmetrise matrix - IS THIS THE BEST OPTION?? 
  lowerTriangle(weighted.matrices[[i]]) <- upperTriangle(weighted.matrices[[i]], byrow=TRUE)
  # created network object with contact_risk as edge weights
  weighted.net[[i]] <- network(weighted.matrices[[i]],
                                      directed = FALSE,
                                      ignore.eval = FALSE, 
                                      names.eval = "contact_risk")
  
}




