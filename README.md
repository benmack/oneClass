oneClass
========

One-class classification in the absence of test data.

The R packages oneClass implements the one-class classifiers one-class SVM, biased SVM, and Maxent, as custom functions for the train function of the package caret. Thus, the extensive infrastructure of the caret package can be used for training and analyzing one-class classification models. The infrastructure is further extended by one-class classification specific tools which should help the which are useful to understand and thus improve one-class classifier outcomes in the absence of a representative and complete test data. 
The package is developed for one-class land cover classification. A part of classical data frames it also contains mothods to predict raster data.

Note that the package is in developement and needs further testing. Also some documentation might still be incomplete.

However the most important functionalities should work well and are described in a tutorial which you can find in the doc directory of the installed package directory. 
It can also be downloaded here: 
https://github.com/benmack/oneClass/blob/master/inst/doc/oneClassIntro.pdf?raw=true

If you encounter a bug or have any problem with the package please do not hesistate to contact me (knecmab@gmail.com). 


Installation
========
The package can be downloaded here:
https://github.com/benmack/oneClass/releases

Or you can install it from within R with the package devtools and the following command:
install_github('benmack/oneClass')
