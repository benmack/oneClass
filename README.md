oneClass: An R Package for One-Class Classification.
====================================================

One-class classification in the absence of test data.

The R packages oneClass implements the one-class classifiers one-class SVM, biased SVM, and Maxent, as custom functions for the train function of the package caret. 
Thus, the extensive infrastructure of the caret package can be used for training and analyzing one-class classification models. The infrastructure is further extended by one-class classification specific tools which may help to understand and thus improve one-class classifier outcomes in the absence of a representative and complete test data. 
The package is developed for one-class land cover classification. 
Therefore, a part of classical data frames it also contains methods to predict raster data.

Note that the package is in developement and needs further testing. 
Also some documentation might still be incomplete.

However the most important functionalities should work well and are described in the [introductory vignette](https://github.com/benmack/oneClass/blob/master/notebooks/oneClassIntro.ipynb). 

If you encounter a bug, unclear/lacking documentation or any other problem with the package please use the [issue tracker](https://github.com/benmack/oneClass/issues) or contact me (knecmab@gmail.com). 


Installation
============

You can install it from within R with the package devtools and the following command:

```
install_github('benmack/oneClass')
```
