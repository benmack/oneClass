### ----------------------------------------------------------------------------
### prediction of raster data with a mask using spatial.tools
### adjusted from example found at 
### http://www.geog.illinois.edu/~jgrn/software-and-datasets/rasterengine-tutorial/
### and from a 
### note: even though the mask is applied all samples will be read from the file, 
### but - if masked - not processed 
.trainOcc_raster_predict <- function(inraster,ocModel,type="prob",mask,...)
{
  # We need to load randomForest (note this is only
  # loaded once per worker on most cluster setups):
  lapply(c("caret", ocModel$modelInfo$library), require, character.only = TRUE)

  # First, preserve the names:
  band_names <- dimnames(inraster)[3][[1]]
  
  # This will “flatten” the array to a matrix (we lose the names here):
  inraster_matrix <- inraster
  dim(inraster_matrix) <- c(dim(inraster)[1]*dim(inraster)[2],dim(inraster)[3])
  # Do the same on the mask
  dim(mask) <- c(dim(mask)[1]*dim(mask)[2],dim(mask)[3])

  # And get the pixels which are not masked
  not_masked <- !is.na(mask)
  
  # Now, let’s coerce this to a data.frame:
  inraster.df <- as.data.frame(inraster_matrix)
  
  # We need to re-set the names because the model might require it:
  names(inraster.df) <- band_names
  
  # Now we can use this in a predict statement:
  out_predictions <- rep(NA, length(not_masked))
  out_predictions[not_masked] <- predict.train(ocModel, inraster.df[not_masked,, drop=FALSE], 
                                                 type=type)$pos 
  # We must return this as a numeric array (a raster cannot
  # use character classes), and set the dims to match the input.
  # Also, right now rasterEngine requires ALL outputs to be double
  # precision floating point, so we cannot use “as.numeric” on the
  # factor, because that will return an integer.
  
  out_predictions_array <- array(as.double(out_predictions),dim=c(dim(inraster)[1:2],1))
  
  return(out_predictions_array)
}
