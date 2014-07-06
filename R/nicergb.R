#' @name nicergb
#' @aliases nicergb
#' @title Pretty Red-Green-Blue plot based on three raster* layers.
#'
#' @description The \code{nicergb} function is a wrapper for \code{\link{plotRGB}} 
#' in package package \code{raster}.
#' 
#' @param img RasterBrick or RasterStack
#' @param r integer. Index of the Red channel, between 1 and nlayers(x)
#' @param g integer. Index of the Green channel, between 1 and nlayers(x)
#' @param b integer. Index of the Blue channel, between 1 and nlayers(x)
#' @param stretch numeric. linear stretch to be applied. Values between 0 and 50
#' describe proportion (in percent) to be cut away from lower and upper quantile.
#' @param readjust logical. If \code{TRUE}, the channel with the largest range is scaled
#' to its maximum. The ranges of the other two channels are stretched but
#' maintaining the relative proportion.
#' @param fixed matrix of 3 x 2 with thresholds for RGB
#' @param nodata numeric. no data value to be ignored.
#' 
#' @return A list with the following objects:
#' \itemize{
#' \item{original.range}{original range of greyscale values}
#' \item{plotted.range}{the plotted range of greyscale values}
#' \item{color.stretch}{minimum and maximum color values}
#' }
#' @author Sebastian Schmidtlein
#' @seealso \code{\link{plotRGB}}
#' @examples
#' \dontrun{
#' ## Load image
#' img <- brick(system.file("external/rlogo.grd", package="raster"))
#' ## Plot as RGB
#' nicergb (img)
#' ## Cut the upper and lower 20% of the histogram and stretch the rest
#' nicergb (img, stretch = 20)
#' }
#' @export
nicergb <- function (img, r = 1, g = 2, b = 3, stretch = 2, readjust = FALSE, 
          fixed = NULL, nodata=NULL) {
  
  ext <- extent(img)
  subs <- raster::subset(img, c(r, g, b))										# subset ->raster::subset                             
  val <- getValues(subs)
  
  if (!is.null(nodata)) {   													# additional no data value
    for(ii in 1:3) 
      val[val[,ii]==nodata,ii] <- NA
  }
  
  original.range <- matrix(NA, 3, 2)
  original.range[1, ] <- range(val[, 1], na.rm=TRUE) # , na.rm=TRUE
  original.range[2, ] <- range(val[, 2], na.rm=TRUE) # , na.rm=TRUE
  original.range[3, ] <- range(val[, 3], na.rm=TRUE) # , na.rm=TRUE
  rownames(original.range) <- c("R", "G", "B")
  colnames(original.range) <- c("min", "max")
  prb <- c(stretch/100, 1 - stretch/100)
  if (is.null(fixed)) {
    rlim <- quantile(val[, 1], probs = prb, na.rm=TRUE) # , na.rm=TRUE
    glim <- quantile(val[, 2], probs = prb, na.rm=TRUE) # , na.rm=TRUE
    blim <- quantile(val[, 3], probs = prb, na.rm=TRUE) # , na.rm=TRUE
  }
  else {
    rlim <- fixed[1, ]
    glim <- fixed[2, ]
    blim <- fixed[3, ]
  }
  croprange <- function(x, limits) {
    x[x < limits[1]] <- limits[1]
    x[x > limits[2]] <- limits[2]
    x
  }
  rpic <- croprange(val[, 1], rlim)
  gpic <- croprange(val[, 2], glim)
  bpic <- croprange(val[, 3], blim)
  lims <- matrix(c(min(rpic, na.rm=TRUE), max(rpic, na.rm=TRUE), 
                   min(gpic, na.rm=TRUE), max(gpic, na.rm=TRUE), 
                   min(bpic, na.rm=TRUE), max(bpic, na.rm=TRUE)), 2, 3)
  colnames(lims) <- c("R", "G", "B")
  rownames(lims) <- c("min", "max")
  if (readjust) {
    gmn <- min(lims[1, ])
    adjlims <- lims - gmn
    gmx <- max(adjlims[2, ])
    scalerange1 <- function(x) {
      x <- x - gmn
      x <- x/gmx
      x <- round(x * 255, 0)
      x
    }
    rpic <- scalerange1(rpic)
    gpic <- scalerange1(gpic)
    bpic <- scalerange1(bpic)
  }
  else {
    scalerange2 <- function(x, emplimits) {
      x <- x - emplimits[1]
      x <- x/(emplimits[2] - emplimits[1])
      x <- round(x * 255, 0)
      x
    }
    rpic <- scalerange2(rpic, lims[, 1])
    gpic <- scalerange2(gpic, lims[, 2])
    bpic <- scalerange2(bpic, lims[, 3])
  }
  
  subs <- setValues(subs, rpic, layer = 1)
  subs <- setValues(subs, gpic, layer = 2)
  subs <- setValues(subs, bpic, layer = 3)
  color.stretch <- cbind(minValue(subs), maxValue(subs))
  rownames(color.stretch) <- c("R", "G", "B")
  colnames(color.stretch) <- c("min", "max")
  suppressWarnings(plotRGB(subs))
  invisible(list(original.range = original.range, plotted.range = t(lims), 
                 color.stretch = color.stretch))
}
