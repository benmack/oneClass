.onLoad <- function(...) {
  
  hints <- c(
    "This package is still in developement.\n", 
    "You are welcome to use and try it but be aware that there might be still some bugs and some changes in the the next time.\n",
    "Feel free to complain to the author if something does not work properly.\n"#,
    #"If you are not sure what one-class classification is good for or how to use this package you may want to read \'trainOcc/docs/trainOcc_intro.pdf\' in your library.\n",
    #"Use suppressPackageStartupMessages to eliminate package startup messages."
  )
  packageStartupMessage(hints)
}