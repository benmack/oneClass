### Example for evaluateAtTh

# get an ModelEvaluation object
ev <- dismo::evaluate( p=rnorm(25, mean = 1), a=rnorm(100, mean = -1) )
# use evaluateAtTh in order to get an 
evAt0 <- evaluateAtTh(ev, th=0)
evAtMaxK <- evaluateAtTh(ev, th='max.kappa')
# print the confusion matrices and some model evaluation statistics 
evAt0
evAtMaxK