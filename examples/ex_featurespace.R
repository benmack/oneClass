### Example for featurespace

tr <- bananas$tr
### underfitted model
oc <- trainOcc ( x = tr[, -1], y = tr[, 1], 
                 tuneGrid=expand.grid(sigma=0.1, 
                                      cNeg=.1, 
                                      cMultiplier=100))
featurespace(oc, th=0) 

### overfitted model
oc <- trainOcc ( x = tr[, -1], y = tr[, 1], 
                 tuneGrid=expand.grid(sigma=10, 
                                      cNeg=.1, 
                                      cMultiplier=100))
featurespace(oc, th=0) 

