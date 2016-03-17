install.packages("caret")
require(oneClass)

## a synthetic data set
data(bananas)

## this is the default setting of trControl in trainOcc
cntrl <- trainControl(method = "cv",
                      number = 10,
                      summaryFunction = puSummary, #!
                      classProbs = TRUE,           #!
                      savePredictions = TRUE,      #!
                      returnResamp = "all",        #!
                      allowParallel = TRUE)
tuneGrid <- data.frame(sigma=3, cNeg=1, cMultiplier=10)

# DEFAULT CALL
time_def <- 
  system.time(
    tocc_def <- trainOcc(x=bananas$tr[, -1], y=bananas$tr[, 1],
                     trControl=cntrl, tuneGrid=tuneGrid))

cntrl_noMe <- trainControl(method = "none",
                      number = 10,
                      summaryFunction = puSummary, #!
                      classProbs = TRUE,           #!
                      savePredictions = TRUE,      #!
                      returnResamp = "all",        #!
                      allowParallel = TRUE)
time_noMe <- 
  system.time(
    tocc_noMe <- trainOcc(x=bananas$tr[, -1], y=bananas$tr[, 1],
                     trControl=cntrl_noMe, tuneGrid=tuneGrid))

# %% Compare the final models
tocc_def$finalModel()
