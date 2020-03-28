# Random Machines
Random Machines: a package for a support vector ensemble based on random kernel space.


## Installation

To install the **Random Machines** package `rm` in the development version from GitHub, just use the command:

```r
# install.packages("devtools")
devtools::install_github("r-spatial/leafpop")
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
                  degree=2, #Degree used in Table 1.,
                  seed.bootstrap=NULL, #Set.seed for bootstrap samples
                  automatic_tuning=FALSE, #Automatic tuning of kernel function's hyperparameters proposed by kernlab::ksvm
                  poly_scale=1 #Scale parameter of Polynomial kernel function,
                  gamma_rbf=1,#Gamma of Gaussian Kernel Function
                  gamma_lap=1,#Gamma of Laplacian Kernel Function
                  degree=2,# Polynomial Degree of Kernel Function,
                  offset=0)


predict(mod_classification,newdata=iris)
```


### Regression

```r
# Regression example
mod_regresssion<-regression_random_machines(formula = dist~.,#Formula that will be used
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
                              poly_scale=1 #Scale parameter of Polynomial kernel function)

predict(mod_regression,newdata=cars)
```
