# ============ SIMULATIONS FUNCTIONS ================ #

# Function for the first simulation scenario
class_sim_scenarion_one <- function(n,p,ratio,seed=NULL){
  
  # Setting the seed
  set.seed(seed)
  
  # Setting the number of observations from the first data set
  n_a <- round(n*abs(1-ratio))
  n_b <- round(n*ratio)
  
  # Generating values from the X observations
  x_a <- replicate(p,rnorm(n_a,mean = 0,sd = 1))
  colnames(x_a) <- paste("x",1:p)
  
  x_b <- replicate(p,rnorm(n_b,mean = 4,sd = 1))
  colnames(x_b) <- paste("x",1:p)
  
  # Formating the complete dataset
  x <- rbind(x_a,x_b)
  y <- as.factor(c(rep("A",n_a),rep("B",n_b)))
  
  simulated_data <- data.frame(x,y)
  
  return(simulated_data[sample(nrow(simulated_data)),])
}

# Function for the first simulation scenario
class_sim_scenarion_two <- function(n,p,ratio,seed=NULL){
  
  # Setting the seed
  set.seed(seed)
  
  # Setting the number of observations from the first data set
  n_a <- round(n*abs(1-ratio))
  n_b <- round(n*ratio)
  
  # Generating values from the X observations
  x_a <- replicate(p,rnorm(n_a,mean = 0,sd = 1))
  colnames(x_a) <- paste("x",1:p)
  
  x_b <- replicate(p,rnorm(n_b,mean = 2,sd = 1))
  colnames(x_b) <- paste("x",1:p)
  
  # Formating the complete dataset
  x <- rbind(x_a,x_b)
  y <- as.factor(c(rep("A",n_a),rep("B",n_b)))
  
  simulated_data <- data.frame(x,y)
  
  return(simulated_data[sample(nrow(simulated_data)),])
}


# Circle scenario
class_sim_scenario_three <- function(n, d, ratio, seed =  NULL){
  
  # Setting the seed
  set.seed(seed)
  
  # Calculating the radius of the circle 
  r <- (2^(d - 1) * gamma(1 + d/2)/(pi^(d/2)))^(1/d)
  
  # Setting the number of observations from the first data set
  n_a <- round(n*abs(1-ratio))
  n_b <- round(n*ratio)
  
  # Generating values from the X observations
  x_a <- replicate(d,runif(min = -1,max = 1,n = n*100))
  colnames(x_a) <- paste("x",1:p)
  
  x_b <- replicate(d,runif(min = -1,max = 1,n = n*100))
  colnames(x_b) <- paste("x",1:p)
  
  # Formating the complete dataset
  x <- rbind(x_a,x_b)
  y <- rep("A",nrow(x))
  y[apply(x,1,function(w){sum(w^2) > r^2 })] <- "B" 
  y <- as.factor(y)
  
  

  simulated_data <- data.frame(x,y)
  
  # Only class A
  class_a <- simulated_data[simulated_data[,"y"]=="A",]
  class_b <- simulated_data[simulated_data[,"y"]=="B",]
  
  # Only class b
  class_a <- class_a[ sample(1:nrow(class_a),size = n_a),]
  class_b <- class_b[ sample(1:nrow(class_b),size = n_b),]
  
  # Merging all of them
  
  return(rbind(class_a,class_b))
  
}

### Regressions cases
reg_sim_scenario_one <- function(n,seed= NULL){
  
  # Setting the seed.
  set.seed(seed)
    
  # Generating the x
  x <- replicate(2,runif(n,min = -1,max = 1))
  colnames(x) <- paste0("x.",1:2)
  
  # Generating the y
  y  <- x[,1]^2 + exp(-x[,2]^2) + rnorm(n = n,mean = 0,sd = sqrt(0.25))
  
  return(data.frame(x,y=y))
}

reg_sim_scenario_two <- function(n,seed= NULL){
  
  # Setting the seed.
  set.seed(seed)
  
  # Generating the x
  x <- replicate(8,runif(n,min = -1,max = 1))
  colnames(x) <- paste0("x.",1:8)
  
  # Generating the y
  y  <- x[,1]*x[,2] + x[,3]^2 -x[,4]*x[,7] +x[,5]*x[,8] -x[,6]^2 + rnorm(n = n,mean = 0,sd = sqrt(0.5))
  
  return(data.frame(x,y=y))
}


reg_sim_scenario_three <- function(n,seed= NULL){
  
  # Setting the seed.
  set.seed(seed)
  
  # Generating the x
  x <- replicate(4,runif(n,min = -1,max = 1))
  colnames(x) <- paste0("x.",1:4)
  
  # Generating the y
  y  <- -sin(x[,1]) + x[,4]^2 + x[,3] - exp(-x[,4]^2) + rnorm(n = n,mean = 0,sd = sqrt(0.5))
  
  return(data.frame(x,y=y))
}

reg_sim_scenario_four <- function(n,seed= NULL){
  
  # Setting the seed.
  set.seed(seed)
  
  # Generating the x
  x <- replicate(6,runif(n,min = -1,max = 1))
  colnames(x) <- paste0("x.",1:6)
  
  # Generating the y
  y  <- x[,1]^2 + (x[,2]^2)*x[,3]*exp(-abs(x[,4])) + x[,6] - x[,5]  + rnorm(n = n,mean = 0,sd = sqrt(0.5))
  
  return(data.frame(x,y=y))
}


reg_sim_scenario_five <- function(n,seed= NULL){
  
  # Setting the seed.
  set.seed(seed)
  
  # Generating the x
  x <- replicate(6,rnorm(n = n))
  colnames(x) <- paste0("x.",1:6)
  
  # Generating the y
  y  <- x[,1] + 0.707*x[,2]^2 + 2*ifelse(x[,3]>0,1,0) + 0.873*log(abs(x[,1]))*abs(x[,3]) + 0.894*x[,2]*x[,4] +
    2*ifelse(x[,5]>0,1,0) + 0.464*exp(x[,6]) + rnorm(n = n,mean = 0,sd = sqrt(1))
  
  return(data.frame(x,y=y))
}

# ====================================================== #



#Dependencies
library(purrr)
library(mlbench)
library(magrittr)
library(kernlab)
#Data to test the function
#(This observations are unnecessary to the package, this is only used here to test the function)
# data<-mlbench.circle(100,d=20) %>% as.data.frame()
#
#
#I used this to test the function
# split.ratio<-0.7
# train_index<-sample(1:nrow(data),size = split.ratio*nrow(data))
# train<-data[train_index,]
# test<-data[-train_index,]
# i=1
# formula=class~.
# train<-list_data[[k]]$training
# test<-list_data[[k]]$test
# class_name='class'
# boots_size=100
# cost=1
# gamma=1#Gamma used in Table 1.
# degree=2 #Degree used in Table 1.
# #
# seed.bootstrap=NULL
# automatic_tuning=FALSE
# gamma_rbf=1
# gamma_lap=1
# poly_scale=1
# offset=0

# offset=0

#Root Mean Squared Error Function
RMSE<-function(predicted,observed,epsilon=NULL){
  min<-min(observed)
  max<-max(observed)
  sqrt(mean(unlist((predicted-observed)^2)))
}

hubber<-function(epsilon,observed,predicted){
  mean( ifelse(abs(predicted-observed)>=epsilon,
               epsilon*abs(predicted-observed)-(epsilon^2)/2,#hubber condition #1
               0.5*(predicted-observed)^2) ) #hubber condition 2
}


#Standard Root Mean Squared Error Function
SRMSE<-function(predicted,observed,epsilon=NULL){
  
  mean(((predicted-observed)/observed)^2)
}


#E-senstive loss function (Vapnik)
e_sensitive<-function(predicted,observed,epsilon){
  mean( ifelse(abs(predicted-observed)>=epsilon,
               abs(predicted-observed)-epsilon,
               0) )
}

#METRICS FOR BINARY CASE
mcc<-function(observed,predicted){
  levels(observed)<-c(1,-1)
  levels(predicted)<-c(1,-1)
  confusion_matrix<-table(observed,predicted)
  TP=confusion_matrix[1,1]
  TN=confusion_matrix[2,2]
  FP=confusion_matrix[2,1]
  FN=confusion_matrix[1,2]
  mcc<-(TP*TN-FP*FN)/sqrt((TP+FP+1e-5)*(TP+FN+1e-5)*(TN+FP+1e-5)*(TN+FN+1e-5))
  return(mcc)
}

acc<-function(observed,predicted){
  levels(observed)<-c(1,-1)
  levels(predicted)<-c(1,-1)
  confusion_matrix<-table(observed,predicted)
  acc<-sum(diag(confusion_matrix))/sum(confusion_matrix)
  return(acc)
}

#METRICS FOR MULTICLASS CASES

# mcc<-function(observed,predicted){
#
#       levels<-levels(observed)
#       mcc<-numeric(0)
#       for(i in 1:length(levels)){
#             aux_obs<-ifelse(observed==levels[i],"1","-1")
#             aux_pred<-ifelse(predicted==levels[i],"1","-1")
#             confusion_matrix<-table(aux_obs,aux_pred)
#             TP=confusion_matrix[1,1]
#             TN=confusion_matrix[2,2]
#             FP=confusion_matrix[2,1]
#             FN=confusion_matrix[1,2]
#             mcc[i]<-(TP*TN-FP*FN)/sqrt((TP+FP+1e-5)*(TP+FN+1e-5)*(TN+FP+1e-5)*(TN+FN+1e-5))
#       }
#       return(mean(mcc))
# }

# acc<-function(observed,predicted){
#       # levels(observed)<-c(1,-1)
#       # levels(predicted)<-c(1,-1)
#       confusion_matrix<-table(observed,predicted)
#       acc<-sum(diag(confusion_matrix))/sum(confusion_matrix)
#       return(acc)
# }

#RM Code
random_machines<-function(formula,#Formula that will be used
                          train,#The Training set
                          validation,#The validation set
                          boots_size=100, #B correspoding to the number of bootstrap samples
                          cost=1,#Cost parameter of SVM
                          degree=2, #Degree used in Table 1.,
                          seed.bootstrap=NULL,automatic_tuning=FALSE,
                          gamma_rbf=1,gamma_lap=1,poly_scale=1,offset=0
){
  
  # Gambiarra para nao precisar mudar a funcao toda
  test<-validation
  #Probability associated with each kernel function
  class_name<- as.character(formula[[2]])
  
  prob_weights<-list()
  
  #The Kernel types used in the algorithm
  kernel_type<-c('rbfdot','polydot','laplacedot','vanilladot')
  
  # Automatic Tuning
  if(automatic_tuning){
    
    early_model<- purrr::map(kernel_type,~kernlab::ksvm(formula,data=train,type="C-svc",
                                                        kernel=if(.x=="vanilladot"){
                                                          "polydot"
                                                        }else{
                                                          .x
                                                        },
                                                        C=cost,
                                                        kpar=if(.x=='laplacedot' ||.x=='rbfdot')
                                                        {
                                                          "automatic"
                                                        }else if(.x=='polydot'){
                                                          list(degree=2,scale=poly_scale,offset=0)
                                                        }else{
                                                          list(degree=1,scale=poly_scale,offset=0)
                                                        }))
  }else{
    #The early model that will calculate the probabilities that will be used during the sort process
    early_model<-purrr::map(kernel_type,~kernlab::ksvm(formula,data=train,type="C-svc",
                                                       kernel=if(.x=="vanilladot"){
                                                         "polydot"
                                                       }else{
                                                         .x
                                                       },
                                                       C=cost,
                                                       kpar=if(.x=='laplacedot')
                                                       {
                                                         list(sigma=gamma_lap)
                                                       }else if(.x=='rbfdot'){
                                                         
                                                         list(sigma=gamma_rbf)
                                                         
                                                       }else if(.x=='polydot'){
                                                         list(degree=2,scale=poly_scale,offset=0)
                                                       }else{
                                                         list(degree=1,scale=poly_scale,offset=0)
                                                       }))
  }
  #Calculando o predict para cada modelo
  predict<-purrr::map(early_model,~kernlab::predict(.x,newdata=test))
  
  
  #Calculating the weights (Equation 9)
  accuracy<-purrr::map(predict,~table(.x,unlist(test[,class_name]))) %>%
    purrr::map(~sum(diag(.x))/sum(.x)) %>% unlist()
  log_acc<-log(accuracy/(1-accuracy))
  log_acc[is.infinite(log_acc)]<-1 #Sometimes the accuracy can be equal to 1, so this line certify to not produce any NA
  prob_weights<-log_acc/sum(log_acc)
  prob_weights<-ifelse(prob_weights<0,0,prob_weights)#To not heve negative values of probabilities
  
  
  #----Defining the variables----
  models<-rep(list(0),boots_size)#Creating the list of models
  boots_sample<-list(rep(boots_size)) #Argument that will be passed in the purrr::map function
  out_of_bag<-list(rep(boots_size)) #OOB samples object
  boots_index_row<-list(nrow(train)) %>% rep(boots_size)
  
  #====================================================
  
  #======Selecting the Bootstraping samples============
  #
  #Creating a indicator varaible to verify if at least one observation of each class is verified
  at_least_one<-NULL
  # p=0
  #Defining which rows will be sampled
  if(is.null(seed.bootstrap)){
    #At least's condition
    while(is.null(at_least_one)){
      boots_index_row_new<-purrr::map(boots_index_row,~sample(1:.x,.x,replace=TRUE))#Generating the boots_sample index
      #Defining the Boots samples
      boots_sample<-purrr::map(boots_index_row_new,~train[.x,]) #Without feature susection
      #Defining out_of the bags_sample
      out_of_bag<-purrr::map(boots_index_row_new,~train[-unique(.x),])
      if(any(unlist(lapply(boots_sample,function(x){table(x[[class_name]])==0})))){
        at_least_one<-NULL
      }else{
        at_least_one<-1
      }
    }
  }else{
    set.seed(seed.bootstrap)
    #At least's condition
    while(is.null(at_least_one)){
      boots_index_row_new<-purrr::map(boots_index_row,~sample(1:.x,.x,replace=TRUE))#Generating the boots_sample index
      #Defining the Boots samples
      boots_sample<-purrr::map(boots_index_row_new,~train[.x,]) #Without feature susection
      #Defining out_of the bags_sample
      out_of_bag<-purrr::map(boots_index_row_new,~train[-unique(.x),])
      
      #Verifying any zero
      if(any(unlist(lapply(boots_sample,function(x){table(x[[class_name]])==0})))){
        at_least_one<-NULL
      }else{
        at_least_one<-1
      }
      # p<-p+1
      # print(paste("Procurando amostra:",p))
    }
  }
  
  #=====================================================
  
  #=================Generating the models===============
  #Calculating the models
  
  #Here is defined which kernel will be used to heach model
  random_kernel<-sample(c('rbfdot','polydot','laplacedot','vanilladot'),
                        boots_size,replace = TRUE,prob = prob_weights)
  
  
  if(automatic_tuning){
    models<-purrr::map2(boots_sample,random_kernel,~kernlab::ksvm(formula, data=.x,type="C-svc",
                                                                  kernel=if(.y=="vanilladot"){
                                                                    "polydot"
                                                                  }else{
                                                                    .y
                                                                  },
                                                                  C=cost,
                                                                  kpar=if(.y=='laplacedot' ||.y=='rbfdot')
                                                                  {
                                                                    "automatic"
                                                                  }else if(.y=='polydot'){
                                                                    list(degree=2,scale=poly_scale,offset=0)
                                                                  }else{
                                                                    list(degree=1,scale=poly_scale,offset=0)
                                                                  }))
    
  }else{
    models<-purrr::map2(boots_sample,random_kernel,~kernlab::ksvm(formula, data=.x,type="C-svc",
                                                                  kernel=if(.y=="vanilladot"){
                                                                    "polydot"
                                                                  }else{
                                                                    .y
                                                                  },
                                                                  C=cost,
                                                                  kpar=if(.y=='laplacedot')
                                                                  {
                                                                    list(sigma=gamma_lap)
                                                                  }else if(.y=='rbfdot'){
                                                                    list(sigma=gamma_rbf)
                                                                  }else if(.y=='polydot'){
                                                                    list(degree=2,scale=poly_scale,offset=0)
                                                                  }else{
                                                                    list(degree=1,scale=poly_scale,offset=0)
                                                                  }))
    
  }
  
  
  
  #Prediction of each mode
  predict<-purrr::map(models,~kernlab::predict(.x,newdata=test))
  
  #Prediction of OOB samples
  predict_oobg<-purrr::map2(models,out_of_bag,~kernlab::predict(.x,newdata=.y))
  
  #Calculating weights from equation 10
  kernel_weight<-purrr::map2(predict_oobg,out_of_bag,~table(.x,unlist(.y[,class_name]))) %>%
    purrr::map_dbl(~sum(diag(.x))/sum(.x))
  
  
  model_result<- list(train = train, # Train data used during the training.
                      class_name = class_name,
                      kernel_weight = kernel_weight, # Kernel Weight of each function
                      lambda_values=list(Lin_Kern=prob_weights[1],
                                                               Pol_Kern=prob_weights[2],
                                                               RBF_Kern=prob_weights[3],
                                                               LAP_Kern=prob_weights[4]),
                      model_params=list(class_name=class_name,
                                        boots_size=boots_size,
                                        cost=cost,
                                        gamma_rbf=gamma_rbf,
                                        gamma_lap=gamma_lap,
                                        degree=degree),bootstrap_models=models,bootstrap_samples=boots_sample)
  
  attr(model_result,"class")<-"rm_model"
  #=============================
  return(model_result)
}


# result<-random_machines(formula=class~.,#Formula that will be used
#                         train=train_data,#The Training set
#                         validation = validation_data,#The test set
#                         test_new = test_data,
#                         boots_size=100, #B correspoding to the number of bootstrap samples
#                         cost=1,#Cost parameter of SVM
#                         gamma_rbf = 1,#Gamma used in Table 1.
#                         degree=2, #Degree used in Table 1.
#                         gamma_lap = 1,automatic_tuning = TRUE
# )

# Return rpredictions from the testset
predict_rm_model<-function(mod,newdata,agreement=FALSE){
  
  
  # Setting the objecsts fro the trained model 
  models <- mod$bootstrap_models
  train <- mod$train
  class_name <- mod$class_name
  kernel_weight <- mod$kernel_weight
  
  #Prediction of each mode newdata
  predict_new<-purrr::map(models,~kernlab::predict(.x,newdata=newdata))
  
  
  #Predictions finals
  predict_df<-predict_new %>%                         #Generating a matrix with where the the rows are each bootstrap sample
    unlist %>%                         #and the columns are each observation from test set
    matrix(ncol=nrow(newdata),byrow = TRUE)
  
  
  predict_df_new<-purrr::map(seq(1:nrow(newdata)),~predict_df[,.x])#Transposing the matrix
  
  pred_df_fct<-purrr::map(predict_df_new,~ifelse(.x==unlist(levels(train[[class_name]]))[1],1,-1)) %>% #Verifying the monst commmon prediction in the boostrap samples for each obs
    purrr::map(~.x/((1+1e-10)-kernel_weight)^2) %>% #Multiplying the weights
    purrr::map(sum) %>% purrr::map(sign) %>% purrr::map(~ifelse(.x==1,levels(dplyr::pull(train,class_name))[1],levels(unlist(train[,class_name]))[2])) %>%
    unlist %>% as.factor()
  
  #AVG_AGR(para calcular iremos transformar o vetor das matrizes de fatores)
  levels_class<-levels(train[[class_name]])
  
  # Transform the matrix to calculate the agreement
  pred_df_standard<-ifelse(predict_df==levels_class[[1]],1,-1)

  # Calculate the agreement of trees but not return it
  agreement_trees<-tcrossprod(pred_df_standard)
  
  #Padroniza a contagem de ocorrencia
  agreement_trees<-(agreement_trees+agreement_trees[1,1])/(2*agreement_trees[1,1])
  
  # Ue the average values
  avg_agreement<-mean(agreement_trees[lower.tri(agreement_trees,diag = FALSE)])
  
  # If else to return the agreement, 
  if(agreement){
    return( list(prediction = pred_df_fct, agreement = avg_agreement) )  
  } else {
    return(pred_df_fct)
  }
}



# 
RMSE_function<-function(predicted,observed,epsilon=NULL){
  min<-min(observed)
  max<-max(observed)
  sqrt(mean(unlist((predicted-observed)^2)))
}

hubber<-function(epsilon,observed,predicted){
  mean( ifelse(abs(predicted-observed)>=epsilon,
               epsilon*abs(predicted-observed)-(epsilon^2)/2,#hubber condition #1
               0.5*(predicted-observed)^2) ) #hubber condition 2
}


#Standard Root Mean Squared Error Function
SRMSE_function<-function(predicted,observed,epsilon=NULL){
  
  mean(((predicted-observed)/observed)^2)
}


#E-senstive loss function (Vapnik)
e_sensitive<-function(predicted,observed,epsilon){
  mean( ifelse(abs(predicted-observed)>=epsilon,
               abs(predicted-observed)-epsilon,
               0) )
}



predict_rrm_model<-function(mod){
  
  # Getting the prediction back from the model
  prediction <- mod$predicted
  
  #=============================
  return(prediction)
}



regression_random_machines<-function(formula,#Formula that will be used
                                     train,#The Training set
                                     validation,#The validation set
                                     test, #New TEST
                                     class_name,#The string corresponding to the variable that will be predicted
                                     boots_size=25, #B correspoding to the number of bootstrap samples
                                     cost=1,#Cost parameter of SVM
                                     gamma_rbf=1,#Gamma used in Table 1.
                                     gamma_lap=1,
                                     degree=2,#Degree used in Table 1.
                                     epsilon=0.1,beta=2,seed.bootstrap=NULL,
                                     loss_function,automatic_tuning=FALSE, #Choose a loss-fucntion
                                     poly_scale
                                     
){
  
  # Creating the class name variable
  class_name <- as.character(formula[[2]])
  
  #Probability associated with each kernel function
  
  #Root Mean Squared Error Function
  RMSE<-function(predicted,observed,epsilon=NULL){
    min<-min(observed)
    max<-max(observed)
    sqrt(mean(unlist((predicted-observed)^2)))
  }
  
  
  prob_weights<-list()
  
  #The Kernel types used in the algorithm
  kernel_type<-c('rbfdot','polydot','laplacedot','vanilladot')
  
  #TUNING AUTOMÁTICO
  if(automatic_tuning){
    
    early_model<- map(kernel_type,~ksvm(formula,data=train,type="eps-svr",
                                        kernel=if(.x=="vanilladot"){
                                          "polydot"
                                        }else{
                                          .x
                                        },
                                        C=cost,
                                        kpar=if(.x=='laplacedot' ||.x=='rbfdot')
                                        {
                                          "automatic"
                                        }else if(.x=='polydot'){
                                          list(degree=2,scale=poly_scale,offset=0)
                                        }else{
                                          list(degree=1,scale=poly_scale,offset=0)
                                        },
                                        epsilon=epsilon))  
  }else{   
    #The early model that will calculate the probabilities that will be used during the sort process
    early_model<-map(kernel_type,~ksvm(formula,data=train,type="eps-svr",
                                       kernel=if(.x=="vanilladot"){
                                         "polydot"
                                       }else{
                                         .x
                                       },
                                       C=cost,
                                       kpar=if(.x=='laplacedot')
                                       {
                                         list(sigma=gamma_lap)
                                       }else if(.x=='rbfdot'){
                                         
                                         list(sigma=gamma_rbf)
                                         
                                       }else if(.x=='polydot'){
                                         list(degree=2,scale=poly_scale,offset=0)
                                       }else{
                                         list(degree=1,scale=poly_scale,offset=0)
                                       },
                                       epsilon=epsilon))  
  }
  #Calculando o predict para cada modelo
  predict<-lapply(early_model,function(x)predict(x,newdata=validation))
  
  
  #Calculating the weights (Equation 9)
  rmse<-lapply(predict,function(x){loss_function(predicted=x,observed=validation[,class_name],epsilon)}) %>% unlist
  rmse<-rmse/sd(rmse)
  # std_rmse<-rmse/(range(validation[,class_name])[2]-range(validation[,class_name])[1])
  inv_rmse<-(exp(-rmse*beta))
  
  prob_weights<-inv_rmse/sum(inv_rmse)
  prob_weights<-ifelse(prob_weights<0,0,prob_weights)#To not heve negative values of probabilities
  
  
  #----Defining the variables----
  models<-rep(list(0),boots_size)#Creating the list of models
  boots_sample<-list(rep(boots_size)) #Argument that will be passed in the map function
  out_of_bag<-list(rep(boots_size)) #OOB samples object
  boots_index_row<-list(nrow(train)) %>% rep(boots_size)
  
  #====================================================
  
  #======Selecting the Bootstraping samples============
  #Defining which rows will be sampled
  if(is.null(seed.bootstrap)){
    boots_index_row<-map(boots_index_row,~sample(1:.x,.x,replace=TRUE))#Generating the boots_sample index
  }else{
    set.seed(seed.bootstrap)
    boots_index_row<-map(boots_index_row,~sample(1:.x,.x,replace=TRUE))#Generating the boots_sample index
  }
  
  #Defining out_of the bags_sample
  #Defining the Boots samples
  boots_sample<-map(boots_index_row,~train[.x,]) #Without feature susection
  out_of_bag<-map(boots_index_row,~train[-unique(.x),])
  
  #=====================================================
  
  #=================Generating the models===============
  #Calculating the models
  
  #Here is defined which kernel will be used to heach model
  random_kernel<-sample(c('rbfdot','polydot','laplacedot','vanilladot'),
                        boots_size,replace = TRUE,prob = prob_weights)
  
  if(automatic_tuning){
    models<-map2(boots_sample,random_kernel,~ksvm(formula, data=.x,type="eps-svr",
                                                  kernel=if(.y=="vanilladot"){
                                                    "polydot"
                                                  }else{
                                                    .y
                                                  },
                                                  C=cost,
                                                  kpar=if(.y=='laplacedot' ||.y=='rbfdot')
                                                  {
                                                    "automatic"
                                                  }else if(.y=='polydot'){
                                                    list(degree=2,scale=poly_scale,offset=0)
                                                  }else{
                                                    list(degree=1,scale=poly_scale,offset=0)
                                                  },
                                                  epsilon=epsilon))
    
  }else{
    models<-map2(boots_sample,random_kernel,~ksvm(formula, data=.x,type="eps-svr",
                                                  kernel=if(.y=="vanilladot"){
                                                    "polydot"
                                                  }else{
                                                    .y
                                                  },
                                                  C=cost,
                                                  kpar=if(.y=='laplacedot')
                                                  {
                                                    list(sigma=gamma_lap)
                                                  }else if(.y=='rbfdot'){
                                                    list(sigma=gamma_rbf)
                                                  }else if(.y=='polydot'){
                                                    list(degree=2,scale=poly_scale,offset=0)
                                                  }else{
                                                    list(degree=1,scale=poly_scale,offset=0)
                                                  },epsilon=epsilon))
    
  }
  
  
  #Prediction of each mode
  predict<-map(models,~predict(.x,newdata=test))
  
  #Prediction of OOB samples
  predict_oobg<-map2(models,out_of_bag,~predict(.x,newdata=.y))
  
  #Calculating weights from equation 10
  kernel_weight<-map2_dbl(predict_oobg,out_of_bag,~loss_function(predicted = .x,observed = .y[,class_name],epsilon)) #%>% 
  #./sd(.) #%>% 
  #map_dbl(~1/(1-exp(-.x*beta))^2)
  
  boots_error<-map2_dbl(predict_oobg,out_of_bag,~loss_function(predicted = .x,observed = .y[,class_name],epsilon)) #%>% 
  
  
  kernel_weight<-(kernel_weight/sd(kernel_weight)) %>% map_dbl(~exp(-.x*beta))
  
  kernel_weight<-kernel_weight/sum((kernel_weight))
  
  
  #Predictions finals
  predict_df<-predict %>%                         #Generating a matrix with where the the rows are each bootstrap sample
    unlist %>%                         #and the columns are each observation from test set
    matrix(ncol=nrow(test),byrow = TRUE)
  
  
  correlation_models<-cor(t(predict_df))
  
  correlation_measure<-mean(correlation_models[upper.tri(correlation_models)])
  
  predict_df_new<-map(seq(1:nrow(test)),~predict_df[,.x])#Transposing the matrix
  
  
  #Como associar o peso!
  
  pred_df_fct<-map(predict_df_new,~.x*kernel_weight) %>% #Multiplying the weights
    map_dbl(sum) 
  
  
  model_result <- list(predicted=pred_df_fct,lambda_values=list(Lin_Kern=prob_weights[4],
                                                                Pol_Kern=prob_weights[2],
                                                                RBF_Kern=prob_weights[1],
                                                                LAP_Kern=prob_weights[3]),
                       model_params=list(class_name=class_name,
                                         boots_size=boots_size,
                                         cost=cost,
                                         gamma=gamma,
                                         degree=degree),bootstrap_models=models,bootstrap_samples=boots_sample,probabilities=prob_weights,
                       init_rmse=rmse,kernel_weight=kernel_weight,
                       correlation_measure=correlation_measure,list_kernels=random_kernel,predict_oob=predict_oobg,botse=boots_error)                                                
  attr(model_result,"class")<-"rrm_model"                                                
  #=============================
  return(model_result)
}


