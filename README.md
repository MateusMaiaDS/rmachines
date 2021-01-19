# Random Machines <img src="rmachines-hex-01.png" width="120" align="right" />
Random Machines: a package for a support vector ensemble based on random kernel space. This package use the [kernlab](https://github.com/cran/kernlab) as dependency to calculate the support vector base learners.


## Installation

To install the **Random Machines** package `rmachines` in the development version from GitHub, just use the command:

```r
# install.packages("devtools")
devtools::install_github("MateusMaiaDS/rmachines")
```

## Examples

The ensemble method was developed for both classifiction and regression tasks. The examples of use are described below

### Classification

```r
# Classification example

mod_classification<-random_machines(formula=Species~.,#Formula that will be used
                  train=iris,#The Training set
                  test=iris,#The Test set
                  boots_size=100, #B correspoding to the number of bootstrap samples
                  cost=1,#Cost parameter of SVM
                  seed.bootstrap=NULL, #Set.seed for bootstrap samples
                  automatic_tuning=FALSE, #Automatic tuning of kernel function's hyperparameters proposed by kernlab::ksvm
                  poly_scale=1, #Scale parameter of Polynomial kernel function,
                  gamma_rbf=1,#Gamma of Gaussian Kernel Function,
                  gamma_lap=1,#Gamma of Laplacian Kernel Function,
                  degree=2,# Polynomial Degree of Kernel Function,
                  offset=0)


predict.rm_model(mod_classification,newdata=iris)
```


### Regression

```r
# Regression example
mod_regression<-regression_random_machines(formula = dist~.,#Formula that will be used
                              train=cars,#The Training set
                              test=cars,#The test set
                              boots_size=25, #B correspoding to the number of bootstrap samples
                              cost=1,#Cost parameter of SVM
                              gamma_rbf=1,#Gamma of Gaussian Kernel Function
                              gamma_lap=1,#Gamma of Laplacian Kernel Function
                              degree=2,# Polynomial Degree of Kernel Function
                              epsilon=0.1, #Epsilon parameter of SVR model
                              beta=2, #Beta parameter of Regression Random Machines
                              seed.bootstrap=NULL, #Set.seed for bootstrap samples
                              loss_function=RMSE, #Loss functions that will be used the options are: RMSE, hubber, SRMSE, e_sensitive
                              automatic_tuning=FALSE, #Automatic tuning of kernel function's hyperparameters proposed by kernlab::ksvm
                              poly_scale=1) #Scale parameter of Polynomial kernel function.

predict.rrm_model(mod_regression,newdata=cars)
```

## Acknowledgments

The development of this ensemble algorithm and the r-package was builten jointly with the [Dr. Anderson Ara](http://www.mwstat.com/andersonara/) and [Dr. Samuel Macêdo](https://samuelmacedo.netlify.com/) 


## Bibliography

To learn more about the methods and how they are work please check.

**Ara, Anderson, et al. "Random Machines Regression Approach: an ensemble support vector regression model with free kernel choice." arXiv preprint arXiv:2003.12643 (2020).** [ArXiV Link](https://arxiv.org/abs/2003.12643)

**Ara, Anderson, et al. "Random Machines: A bagged-weighted support vector model with free kernel choice." arXiv preprint arXiv:1911.09411 (2019).** [ArXiV Link](https://arxiv.org/abs/1911.09411)

## About me

For more resources about Statistics and Data Science please access [my site](https://learningfromdata.netlify.com/)
