
data(bananas)

model <- trainOcc(x=bananas$tr[, -1], y=bananas$tr[, 1], method="ocsvm")
model <- update(model, aggregatePredictions=TRUE)





model <- update(model, aggregatePredictions=TRUE, 
                puSummaryFunction = puSummary_multiTh, 
                metric = "puF@thAP")
colnames( model$results)
pairs( model$results[, c("puF", "puFAP", "puF@thAP")])


model <- trainOcc(x=bananas$tr[, -1], y=bananas$tr[, 1], method="ocsvm")

hop <- holdOutPredictions(model, partition=1)
dataForSummaryFunction(hop)
