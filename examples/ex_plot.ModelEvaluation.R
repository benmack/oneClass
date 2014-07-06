### Example for evaluateAtTh

# get an ModelEvaluation object
ev <- dismo::evaluate( p=rnorm(25, mean = 1), a=rnorm(100, mean = -1) )
# plot 
plot(ev)
