context('Test modelPosition')

toy <- threeGaussians(seed=123)

### generate reproducible objects (store somewhere and load for testing!?)
set.seed(123)
index <- createFolds(toy$tr$y, k=2)
x <- trainOcc(x = toy$tr[,-1], y = toy$tr[,1], positive = 1,
              index = index, tuneGrid = expand.grid( sigma=c(.1,1), 
                                                     cNeg=c(1, 10), 
                                                     cMultiplier=c(2, 8) ) )

mp <- modelPosition(x, modRow=1:8)

expect_that ( all(mp$row == 1:8), is_true() )
expect_that ( all(mp$rank == c(8,1,6,2,5,3,7,4)), is_true() )
