#' @title One-class classification
#'
#' @description The purpose of the package is to provide i) (an interface to) one-class classifiers, 
#' ii) easy access to critical information and diagnostic plots, and iii) an environment 
#' for developers to define new methods. 
#' 
#' @details One-class classifiers need to solve a binary classification problem 
#' but for the training of the classifier labeled training samples are only available for the class of interest.
#' Model and threshold selection are two critical issues in one-class classification and it is strongly recommended 
#' that critical decision of the modelling process are examined carefully by the user. \cr
#' The package builds upon the powerful \code{caret} package (see \url{http://caret.r-forge.r-project.org/}).
#' The most important function \code{\link{trainOcc}} calls \code{\link[caret]{train}} and returns an object 
#' of class \code{trainOcc} which is a child of the class \code{train}. 
#' Therefore, many options, e.g. different resampling and pre-processing methods, 
#' and tools, e.g. investigation of resampling distributions, from the \code{caret} package are 
#' also available in the package \code{oneClass}.
#' 
#' @seealso The package vignette gives a illustrative introduction to one-class 
#' classification and the package \code{oneClass} (\url{../doc/oneClassIntro.html}) 
#' 
#' @import caret
#' @import raster
#' @importFrom dismo evaluate
#' @importFrom pROC roc
#' @importFrom spatial.tools rasterEngine
#' @docType package
#' @name oneClass
NULL



