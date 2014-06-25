### borrowed from caret: 
### https://github.com/topepo/caret/tree/master/models
wd <- getwd()
setwd("inst/models/files/")
modelFiles <- list.files(pattern = "\\.R$")

models <- vector(mode = "list", length = length(modelFiles))
names(models) <- gsub("\\.R", "", modelFiles)

for(i in seq(along = modelFiles)) {
  source(modelFiles[i])
  models[[i]] <- modelInfo
  rm(modelInfo)
}

setwd(wd)
save(models, file = "inst/models/models.RData")
