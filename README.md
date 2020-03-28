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
                  test=iris,#The test set
                  # class_name,#The string corresponding to the variable that will be predicted
                  boots_size=100, #B correspoding to the number of bootstrap samples
                  cost=1,#Cost parameter of SVM
                  degree=2, #Degree used in Table 1.,
                  seed.bootstrap=NULL,automatic_tuning=FALSE,gamma_rbf=1,gamma_lap=1,poly_scale=1,offset=0)


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
                              gamma_rbf=1,#Gamma used in Table 1.
                              gamma_lap=1,
                              degree=2,#Degree used in Table 1.
                              epsilon=0.1,beta=2,seed.bootstrap=NULL,
                              loss_function=RMSE,automatic_tuning=FALSE, #Choose a loss-fucntion
                              poly_scale=1)

predict(mod_regression,newdata=cars)
```
