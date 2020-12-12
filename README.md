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
## Development

You can develop and use the **oneClass** source code in a Docker container build from the 
[*benmack/r-oneclass-deps*](https://github.com/benmack/r-oneclass-deps) image. 
Prerequisites are that you have Docker installed and local copy of the **oneClass** source code on your machine.

Run the following command in a terminal:

    docker run -i -p 8787:8787 -e PASSWORD=<password of your choice> -v <path containing the local copy of the oneClass repository>:/home/rstudio/oneClass benmack/r-oneclass-deps:3.5.3

And go to *http://localhost:8787/* in your browser where you can log in with the user *rstudio* and the password of your choice to connect to RStudio. 
In RStudio you should find the *oneClass* folder under the files in which you can start the *oneClass.Rproj*.

**Note that the Dockerimage is optimized and for not as slim as it could be and that *maxent.jar* is not yet included in the image (see https://hub.docker.com/r/benmack/r-oneclass-deps).**

The following package looks like a very interesting future base image on top of which oneClass might be installed without much of a setup cost because it contains most if not all dependencies, including maxent:

    docker run -rm -p 8888:8888 -e JUPYTER_ENABLE_LAB=yes -v ${PWD}:/home/jovyan/work scioquiver/notebooks:cgspatial-notebook
