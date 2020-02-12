# **Caret** assumes that all of the data are numeric, i.e. factors have been converted to dummy variables via model.matrix, dummyVars or other means. But first, we will convert brand variable to numeric (0: Belkin and 1: Elago).


dataSetProcessed$brand <- ifelse(dataSetProcessed$brand == 'Belkin', 0, 1)

# Converting every categorical variable to numerical using dummy variables
dmy <- dummyVars(" ~ .", data = dataSetProcessed)
dataSetTransformed <- data.frame(predict(dmy, newdata = dataSetProcessed))


# Identification of near zero variance predictors
kable(head(nearZeroVar(dataSetTransformed, saveMetrics= TRUE))) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

# All variables do not have zero-variance, although some variables associated with car models have near zero-variance.
# We also test linear dependency among variables. For each linear combination, one variable will be removed.

# Linear Dependencies
(linearDepend <- findLinearCombos(dataSetTransformed))

dataSetTransformed <- dataSetTransformed[, -linearDepend$remove]

# convert brand variable into factors
dataSetTransformed$brand <- ifelse(dataSetTransformed$brand == 0, 'Belkin', 'Elago')
dataSetTransformed$brand <- as.factor(dataSetTransformed$brand)

# The function shows that there are two linear combinations and indicates that we should remove two variables (Opel car model and East-Central Europe zipcode).

# Feature selection
# Now we will be using Recursive Feature elimination (RFE) which is a wrapper method to find the best subset of features to use for modeling.

# Feature selection using rfe in caret
subsets <- c(1:5, 10, 15, 20, 25)
rfControl <- rfeControl(functions = rfFuncs, method = "repeatedcv", 
                        repeats = 3, verbose = FALSE)
#lmControl <- rfeControl(functions = lmFuncs, method = "repeatedcv", 
#                        repeats = 3, verbose = FALSE)

predictors <- names(dataSetTransformed)[!names(dataSetTransformed) %in% 'brand']

rfProfile <- rfe(dataSetTransformed[, predictors], dataSetTransformed[,'brand'], 
                 sizes = subsets, rfeControl = rfControl)
rfProfile

#lmProfile <- rfe(dataSetTransformed[, predictors], dataSetTransformed[,'brand'], 
#                 sizes = subsets, rfeControl = lmControl)
#lmProfile

trellis.par.set(caretTheme())
plot(rfProfile, type = c("g", "o"))