# Load the "caret" package for model training and validation.
library(caret)
# Load the "doParallel" package for parallel processing.
library(doParallel)
# Load the "foreach" package for splitting loops across multiple cores.
library(foreach)

# Set random number generator seed for replicability of results.
set.seed(0)

# Read in the Taipei house prices dataset.
houseprice <- read.csv("/Users/admirary/Documents/Dropbox/Courses/DATA303/Data/houseprice.csv", header = TRUE)


# Convert the "year" variable to categorical.
houseprice$year <- as.factor(houseprice$year)

# Specify the indices of predictors to be considered.
variable.indices <- 2 : 7

# Remove any observations with missing data for any of the predictors of interest.
houseprice <- houseprice[complete.cases(houseprice[, variable.indices]), ]

#####################################
## Train linear regression models. ##
#####################################
 
# Produce a matrix that represents all possible combinations of predictors.
all.comb <- expand.grid(as.data.frame(matrix(rep(0 : 1, length(variable.indices)), nrow = 2)))[-1, ]

# Fire up 75% of computer cores for parallel processing.
nclust <- makeCluster(detectCores() * 0.75)
registerDoParallel(nclust)

#########################
## MSE: The bootstrap. ##
#########################

# Specify the number of bootstrap samples to use.
nboot <- 100

fitControl <- trainControl(method = "boot", number = nboot, seeds = 1 : (nboot + 1), savePredictions = TRUE)

all.model.fits.bs <- foreach(i = 1 : nrow(all.comb), .packages = "caret") %dopar%
{
  model.equation <- as.formula(paste("price ~", paste(names(houseprice)[variable.indices][all.comb[i,] == 1], collapse = " + ")))
  train(model.equation, data = houseprice, method = "lm", trControl = fitControl)
}

##########################
## MSE: Bootstrap .632. ##
##########################

fitControl <- trainControl(method = "boot632", number = nboot, seeds = 1 : (nboot + 2), savePredictions = TRUE)

all.model.fits.bs.632 <- foreach(i = 1 : nrow(all.comb), .packages = "caret") %dopar%
{
  model.equation <- as.formula(paste("price ~", paste(names(houseprice)[variable.indices][all.comb[i,] == 1], collapse = " + ")))
  train(model.equation, data = houseprice, method = "lm", trControl = fitControl)
}

###############################################
## MSE: One run of 10-fold cross-validation. ##
###############################################

# Specify the number of folds for k-fold cross-validation.
folds <- 10

fitControl <- trainControl(method = "cv", number = folds, seeds = 1 : (folds + 1), savePredictions = TRUE)

all.model.fits.cv <- foreach(i = 1 : nrow(all.comb), .packages = "caret") %dopar%
{
  model.equation <- as.formula(paste("price ~", paste(names(houseprice)[variable.indices][all.comb[i,] == 1], collapse = " + ")))
  train(model.equation, data = houseprice, method = "lm", trControl = fitControl)
}

#############################################
## MSE: Repeated 10-fold cross-validation. ##
#############################################

# Specify the number of repetitions of k-fold cross-validation.
reps <- 10

fitControl <- trainControl(method = "repeatedcv", number = folds, repeats = reps, seeds = 1 : (folds * reps + 1), savePredictions = TRUE)

all.model.fits.rep.cv <- foreach(i = 1 : nrow(all.comb), .packages = "caret") %dopar%
{
  model.equation <- as.formula(paste("price ~", paste(names(houseprice)[variable.indices][all.comb[i,] == 1], collapse = " + ")))
  train(model.equation, data = houseprice, method = "lm", trControl = fitControl)
}

# Shut down cores.
stopCluster(nclust)

# Write a custom function that will extract RMSE for each candidate model 
# and square it to produce the estimate of MSE.
MSE.extract <- function(x)
{
  return(as.numeric(x$results[2]) ^ 2)
}

# Apply the function to all of the candidate models that were fit using
# repeated 10-fold cross-validation.
MSE.rep.cv <- sapply(all.model.fits.rep.cv, MSE.extract)

# See the ranking of models in terms of the objective of minimising MSE.
order(MSE.rep.cv)

# Construct a matrix in which to store information on which variables are included in the 10 best models.
best.models <- matrix(NA, nrow = 10, ncol = length(variable.indices), dimnames = list(NULL, names(houseprice)[variable.indices]))

# Cycle through the top 10 models and save TRUEs and FALSEs for columns of variables included and not included in the best models.
for(i in 1 : 10)
{
  best.models[i, ] <- all.comb[order(MSE.rep.cv)[i], ] == 1
}

pander(best.models)

dotplot(resamples(all.model.fits.rep.cv), metric = "RMSE")

# Carry out t-tests for differences between pairs of models.  Use a Bonferroni adjustment to p-values.
MSE.differences <- diff(resamples(all.model.fits.rep.cv), adjustment = "bonferroni")
# Extract a matrix of differences and p-values for tests carried out on RMSE.
benchmark.experiment <- summary(MSE.differences)$table$RMSE
# View differences and p-values corresponding to comparisons between models for the top four models.
pander(benchmark.experiment[sort(order(MSE.rep.cv)[1 : 4]), sort(order(MSE.rep.cv)[1 : 4])])
