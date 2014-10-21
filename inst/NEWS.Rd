\name{NEWS}
\title{News for Package \pkg{oneClass}}
                          
\section{Version 0.1-2}{
\itemize{
\item Changes in update.trainOcc. The returned model is now updated if argumente aggregatePredictions is set to TRUE.
\item Function createFoldsPu: Examples added to help. Argument indep.index changed to indepUn. Can now also be a fraction.  
\item In vignette: Information about parallel processing added. 
\item Bugfix in puSummary. If not yet attached but available, pROC attached and puAuc calculated.
}

\section{Version 0.1-1}{
\itemize{
\item Bugfix. When pu-partition is used the final model is now trained with the U-training set only.
}

\section{Version 0.1-0}{
\itemize{
\item First release with basic functionalities as described in the manual.
}
}

