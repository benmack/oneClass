## oneClass: An R Package for One-Class Classification.

The R packages **oneClass** implements the one-class classifiers one-class SVM, biased SVM, and Maxent, as custom functions for the train function of the package **caret**. 
Thus, the extensive infrastructure of **caret** can be used for training and analyzing one-class classification models. 
The infrastructure is further extended by one-class classification specific tools which may help to understand and thus improve one-class classifier outcomes in the absence of a representative and complete test data. 
The package is developed for one-class land cover classification with remote sensing data. 
Therefore, a part of classical data frames it also contains methods to predict raster data.

The most important functionalities are described in the [introductory vignette](https://github.com/benmack/oneClass/blob/master/notebooks/oneClassIntro.ipynb). 


Note that the package is still in developement and needs further testing. 
Also some documentation might still be incomplete.

If you encounter a bug, unclear/lacking documentation or any other problem with the package please use the [issue tracker](https://github.com/benmack/oneClass/issues) or contact me. 


## Installation

The package is available on [GitHub](https://github.com/benmack/oneClass).
You can install it from within R with the package **devtools** and the following command:

```
install_github('benmack/oneClass')
```
