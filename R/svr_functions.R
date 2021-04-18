# #Root Mean Squared Error Function
# RMSE<-function(predicted,observed,epsilon=NULL){
#   min<-min(observed)
#   max<-max(observed)
#   sqrt(mean(unlist((predicted-observed)^2)))
# }
# 
# hubber<-function(epsilon,observed,predicted){
#   mean( ifelse(abs(predicted-observed)>=epsilon,
#                epsilon*abs(predicted-observed)-(epsilon^2)/2,#hubber condition #1
#                0.5*(predicted-observed)^2) ) #hubber condition 2
# }
# 
# 
# #Standard Root Mean Squared Error Function
# SRMSE<-function(predicted,observed,epsilon=NULL){
#   
#   mean(((predicted-observed)/observed)^2)
# }
# 
# 
# #E-senstive loss function (Vapnik)
# e_sensitive<-function(predicted,observed,epsilon){
#   mean( ifelse(abs(predicted-observed)>=epsilon,
#                abs(predicted-observed)-epsilon,
#                0) )
# }
# 
# #RM Code
# random_machines_svr<-function(formula,#Formula that will be used
#                               train,#The Training set
#                               test,#The test set
#                               boots_size=25, #B correspoding to the number of bootstrap samples
#                               cost=1,#Cost parameter of SVM
#                               gamma_rbf=1,#Gamma used in Table 1.
#                               gamma_lap=1,
#                               degree=2,#Degree used in Table 1.
#                               epsilon=0.1,beta=2,seed.bootstrap=NULL,
#                               loss_function,automatic_tuning=FALSE, #Choose a loss-fucntion
#                               poly_scale
#                               
# ){
#   
#   class_name<-as.character(formula[[2]])#The string corresponding to the variable that will be predicted
#   
#   
#   
#   #Root Mean Squared Error Function
#   RMSE<-function(predicted,observed,epsilon=NULL){
#     min<-min(observed)
#     max<-max(observed)
#     sqrt(mean(unlist((predicted-observed)^2)))
#   }
#   
#   #Probability associated with each kernel function
#   
#   
#   prob_weights<-list()
#   
#   #The Kernel types used in the algorithm
#   kernel_type<-c('rbfdot','polydot','laplacedot','vanilladot')
#   
#   #TUNING AUTOMÁTICO
#   if(automatic_tuning){
#     
#     early_model<- map(kernel_type,~ksvm(formula,data=train,type="eps-svr",
#                                         kernel=if(.x=="vanilladot"){
#                                           "polydot"
#                                         }else{
#                                           .x
#                                         },
#                                         C=cost,
#                                         kpar=if(.x=='laplacedot' ||.x=='rbfdot')
#                                         {
#                                           "automatic"
#                                         }else if(.x=='polydot'){
#                                           list(degree=2,scale=poly_scale,offset=0)
#                                         }else{
#                                           list(degree=1,scale=poly_scale,offset=0)
#                                         },
#                                         epsilon=epsilon))  
#   }else{   
#     #The early model that will calculate the probabilities that will be used during the sort process
#     early_model<-map(kernel_type,~ksvm(formula,data=train,type="eps-svr",
#                                        kernel=if(.x=="vanilladot"){
#                                          "polydot"
#                                        }else{
#                                          .x
#                                        },
#                                        C=cost,
#                                        kpar=if(.x=='laplacedot')
#                                        {
#                                          list(sigma=gamma_lap)
#                                        }else if(.x=='rbfdot'){
#                                          
#                                          list(sigma=gamma_rbf)
#                                          
#                                        }else if(.x=='polydot'){
#                                          list(degree=2,scale=poly_scale,offset=0)
#                                        }else{
#                                          list(degree=1,scale=poly_scale,offset=0)
#                                        },
#                                        epsilon=epsilon))  
#   }
#   #Calculando o predict para cada modelo
#   predict<-lapply(early_model,function(x)predict(x,newdata=test))
#   
#   
#   #Calculating the weights (Equation 9)
#   rmse<-lapply(predict,function(x){loss_function(predicted=x,observed=test[,class_name],epsilon)}) %>% unlist
#   rmse<-rmse/sd(rmse)
#   # std_rmse<-rmse/(range(test[,class_name])[2]-range(test[,class_name])[1])
#   inv_rmse<-(exp(-rmse*beta))
#   
#   prob_weights<-inv_rmse/sum(inv_rmse)
#   prob_weights<-ifelse(prob_weights<0,0,prob_weights)#To not heve negative values of probabilities
#   
#   
#   #----Defining the variables----
#   models<-rep(list(0),boots_size)#Creating the list of models
#   boots_sample<-list(rep(boots_size)) #Argument that will be passed in the map function
#   out_of_bag<-list(rep(boots_size)) #OOB samples object
#   boots_index_row<-list(nrow(train)) %>% rep(boots_size)
#   
#   #====================================================
#   
#   #======Selecting the Bootstraping samples============
#   #Defining which rows will be sampled
#   if(is.null(seed.bootstrap)){
#     boots_index_row<-map(boots_index_row,~sample(1:.x,.x,replace=TRUE))#Generating the boots_sample index
#   }else{
#     set.seed(seed.bootstrap)
#     boots_index_row<-map(boots_index_row,~sample(1:.x,.x,replace=TRUE))#Generating the boots_sample index
#   }
#   
#   #Defining out_of the bags_sample
#   #Defining the Boots samples
#   boots_sample<-map(boots_index_row,~train[.x,]) #Without feature susection
#   out_of_bag<-map(boots_index_row,~train[-unique(.x),])
#   
#   #=====================================================
#   
#   #=================Generating the models===============
#   #Calculating the models
#   
#   #Here is defined which kernel will be used to heach model
#   random_kernel<-sample(c('rbfdot','polydot','laplacedot','vanilladot'),
#                         boots_size,replace = TRUE,prob = prob_weights)
#   
#   if(automatic_tuning){
#     models<-map2(boots_sample,random_kernel,~ksvm(formula, data=.x,type="eps-svr",
#                                                   kernel=if(.y=="vanilladot"){
#                                                     "polydot"
#                                                   }else{
#                                                     .y
#                                                   },
#                                                   C=cost,
#                                                   kpar=if(.y=='laplacedot' ||.y=='rbfdot')
#                                                   {
#                                                     "automatic"
#                                                   }else if(.y=='polydot'){
#                                                     list(degree=2,scale=poly_scale,offset=0)
#                                                   }else{
#                                                     list(degree=1,scale=poly_scale,offset=0)
#                                                   },
#                                                   epsilon=epsilon))
#     
#   }else{
#     models<-map2(boots_sample,random_kernel,~ksvm(formula, data=.x,type="eps-svr",
#                                                   kernel=if(.y=="vanilladot"){
#                                                     "polydot"
#                                                   }else{
#                                                     .y
#                                                   },
#                                                   C=cost,
#                                                   kpar=if(.y=='laplacedot')
#                                                   {
#                                                     list(sigma=gamma_lap)
#                                                   }else if(.y=='rbfdot'){
#                                                     list(sigma=gamma_rbf)
#                                                   }else if(.y=='polydot'){
#                                                     list(degree=2,scale=poly_scale,offset=0)
#                                                   }else{
#                                                     list(degree=1,scale=poly_scale,offset=0)
#                                                   },epsilon=epsilon))
#     
#   }
#   
#   
#   #Prediction of each mode
#   predict<-map(models,~predict(.x,newdata=test))
#   
#   #Prediction of OOB samples
#   predict_oobg<-map2(models,out_of_bag,~predict(.x,newdata=.y))
#   
#   #Calculating weights from equation 10
#   kernel_weight<-map2_dbl(predict_oobg,out_of_bag,~loss_function(predicted = .x,observed = .y[,class_name],epsilon)) #%>% 
#   #./sd(.) #%>% 
#   #map_dbl(~1/(1-exp(-.x*beta))^2)
#   
#   boots_error<-map2_dbl(predict_oobg,out_of_bag,~loss_function(predicted = .x,observed = .y[,class_name],epsilon)) #%>% 
#   
#   
#   kernel_weight<-(kernel_weight/sd(kernel_weight)) %>% map_dbl(~exp(-.x*beta))
#   
#   kernel_weight<-kernel_weight/sum((kernel_weight))
#   
#   
#   #Predictions finals
#   predict_df<-predict %>%                         #Generating a matrix with where the the rows are each bootstrap sample
#     unlist %>%                         #and the columns are each observation from test set
#     matrix(ncol=nrow(test),byrow = TRUE)
#   
#   
#   correlation_models<-cor(t(predict_df))
#   
#   correlation_measure<-mean(correlation_models[upper.tri(correlation_models)])
#   
#   predict_df_new<-map(seq(1:nrow(test)),~predict_df[,.x])#Transposing the matrix
#   
#   
#   #Como associar o peso!
#   
#   pred_df_fct<-map(predict_df_new,~.x*kernel_weight) %>% #Multiplying the weights
#     map_dbl(sum) 
#   
#   
#   #=============================
#   return(list(predicted=pred_df_fct,lambda_values=list(Lin_Kern=prob_weights[4],
#                                                        Pol_Kern=prob_weights[2],
#                                                        RBF_Kern=prob_weights[1],
#                                                        LAP_Kern=prob_weights[3]),
#               model_params=list(class_name=class_name,
#                                 boots_size=boots_size,
#                                 cost=cost,
#                                 gamma=gamma,
#                                 degree=degree),bootstrap_models=models,bootstrap_samples=boots_sample,probabilities=prob_weights,
#               init_rmse=rmse,kernel_weight=kernel_weight,
#               correlation_measure=correlation_measure,list_kernels=random_kernel,predict_oob=predict_oobg,botse=boots_error))
# }
# 
# bagged_svr<-function(formula,#Formula that will be used
#                      train,#The Training set
#                      test,#The test set
#                      class_name,#The string corresponding to the variable that will be predicted
#                      boots_size=100, #B correspoding to the number of bootstrap samples
#                      cost=1,poly_scale,#Cost parameter of SVM
#                      kernel,#Kernel Function
#                      gamma=1,offset=1,#Gamma used in Table 1.  
#                      degree=2,
#                      epsilon,seed.bootstrap=NULL,automatic_tuning=FALSE
# ){
#   #Probability associated with each kernel function
#   
#   
#   #----Defining the variables----
#   models<-rep(list(0),boots_size)#Creating the list of models
#   boots_sample<-list(rep(boots_size)) #Argument that will be passed in the map function
#   out_of_bag<-list(rep(boots_size)) #OOB samples object
#   boots_index_row<-list(nrow(train)) %>% rep(boots_size)
#   
#   #=============Col_Sample============
#   
#   #======Selecting the Bootstraping samples============
#   #Defining which rows will be sampled
#   if(is.null(seed.bootstrap)){
#     boots_index_row<-map(boots_index_row,~sample(1:.x,.x,replace=TRUE))#Generating the boots_sample index
#   }else{
#     set.seed(seed.bootstrap)
#     boots_index_row<-map(boots_index_row,~sample(1:.x,.x,replace=TRUE))#Generating the boots_sample index
#   }
#   
#   #Defining out_of the bags_sample
#   #Defining the Boots samples
#   boots_sample<-map(boots_index_row,~train[.x,]) #Without feature susection
#   out_of_bag<-map(boots_index_row,~train[-unique(.x),])
#   
#   #=====================================================
#   
#   #=================Generating the models===============
#   #Calculating the models
#   
#   if(automatic_tuning){
#     models<-map(boots_sample,~ksvm(formula, data=.x,type="eps-svr",
#                                    kernel=if(kernel=="vanilladot"){
#                                      "polydot"
#                                    }else{
#                                      kernel
#                                    },
#                                    kpar=if(kernel=='laplacedot' ||kernel=='rbfdot')
#                                    {
#                                      "automatic"
#                                    }else if(kernel=='polydot'){
#                                      list(degree=degree,scale=poly_scale,offset=offset)
#                                    }else{
#                                      list(degree=1,scale=poly_scale,offset=offset)
#                                    },C=cost,
#                                    ,epsilon=epsilon))
#     
#     
#   }else{
#     models<-map(boots_sample,~ksvm(formula, data=.x,type="eps-svr",
#                                    kernel=if(kernel=="vanilladot"){
#                                      "polydot"
#                                    }else{
#                                      kernel
#                                    },
#                                    C=cost,
#                                    kpar=if(kernel=='polydot'){
#                                      list(degree=degree,scale=poly_scale,offset=offset)
#                                    }else if(kernel=='vanilladot'){
#                                      list(degree=1,scale=poly_scale,offset=offset)
#                                    }else{
#                                      list(sigma=gamma)
#                                    },epsilon=epsilon))
#     
#   }
#   
#   
#   #Prediction of each mode
#   predict<-map(models,~predict(.x,newdata=test))
#   
#   
#   #Predictions finals
#   predict_df<-predict %>%                         #Generating a matrix with where the the rows are each bootstrap sample
#     unlist %>%                         #and the columns are each observation from test set
#     matrix(ncol=nrow(test),byrow = TRUE)
#   
#   
#   correlation_models<-cor(t(predict_df))
#   
#   correlation_measure<-mean(correlation_models[upper.tri(correlation_models)])
#   
#   
#   predict_df_new<-map(seq(1:nrow(test)),~predict_df[,.x])#Transposing the matrix
#   
#   
#   #Como associar o peso!
#   
#   pred_df_fct<-map_dbl(predict_df_new,mean) 
#   
#   
#   
#   #=============================
#   return(list(predicted=pred_df_fct,
#               model_params=list(class_name=class_name,
#                                 boots_size=boots_size,
#                                 cost=cost,
#                                 gamma=gamma,
#                                 degree=degree),bootstrap_models=models,bootstrap_samples=boots_sample,
#               correlation_measure=correlation_measure))
# }
# 
# 
# calculate_error<-function(list,resultado,metrica_error){
#   #Funcao e metrica de erro
#   aux<-resultado
#   split(aux$predicted,ceiling(seq_along(aux$predicted)/nrow(list[[1]]$test))) %>% 
#     map2_dbl(list,~metrica_error(.x,.y$test$y)) %>% mean(na.rm=TRUE)
# }
# 
# calculate_error_single<-function(list,modelo,metrica_error){
#   predictions<-map2(modelo,list,~predict(.x,newdata=.y$test))
#   result_error<-map2_dbl(predictions,list,~(RMSE(predicted =.x,observed = .y$test$y)))# %>% mean()
#   return(result_error)
# }
# 
# 
# #Root Mean Squared Error Function
# RMSE<-function(predicted,observed,epsilon=NULL){
#   min<-min(observed)
#   max<-max(observed)
#   sqrt(mean(unlist((predicted-observed)^2)))
# }
# 
# hubber<-function(epsilon,observed,predicted){
#   mean( ifelse(abs(predicted-observed)>=epsilon,
#                epsilon*abs(predicted-observed)-(epsilon^2)/2,#hubber condition #1
#                0.5*(predicted-observed)^2) ) #hubber condition 2
# }
# 
# 
# #Standard Root Mean Squared Error Function
# SRMSE<-function(predicted,observed,epsilon=NULL){
#   
#   mean(((predicted-observed)/observed)^2)
# }
# 
# 
# #E-senstive loss function (Vapnik)
# e_sensitive<-function(predicted,observed,epsilon){
#   mean( ifelse(abs(predicted-observed)>=epsilon,
#                abs(predicted-observed)-epsilon,
#                0) )
# }
# 
# #RM Code
# random_machines_svr<-function(formula,#Formula that will be used
#                               train,#The Training set
#                               test,#The test set
#                               boots_size=25, #B correspoding to the number of bootstrap samples
#                               cost=1,#Cost parameter of SVM
#                               gamma_rbf=1,#Gamma used in Table 1.
#                               gamma_lap=1,
#                               degree=2,#Degree used in Table 1.
#                               epsilon=0.1,beta=2,seed.bootstrap=NULL,
#                               loss_function,automatic_tuning=FALSE, #Choose a loss-fucntion
#                               poly_scale
#                               
# ){
#   
#   class_name<-as.character(formula[[2]])#The string corresponding to the variable that will be predicted
#   
#   
#   
#   #Root Mean Squared Error Function
#   RMSE<-function(predicted,observed,epsilon=NULL){
#     min<-min(observed)
#     max<-max(observed)
#     sqrt(mean(unlist((predicted-observed)^2)))
#   }
#   
#   #Probability associated with each kernel function
#   
#   
#   prob_weights<-list()
#   
#   #The Kernel types used in the algorithm
#   kernel_type<-c('rbfdot','polydot','laplacedot','vanilladot')
#   
#   #TUNING AUTOMÁTICO
#   if(automatic_tuning){
#     
#     early_model<- map(kernel_type,~ksvm(formula,data=train,type="eps-svr",
#                                         kernel=if(.x=="vanilladot"){
#                                           "polydot"
#                                         }else{
#                                           .x
#                                         },
#                                         C=cost,
#                                         kpar=if(.x=='laplacedot' ||.x=='rbfdot')
#                                         {
#                                           "automatic"
#                                         }else if(.x=='polydot'){
#                                           list(degree=2,scale=poly_scale,offset=0)
#                                         }else{
#                                           list(degree=1,scale=poly_scale,offset=0)
#                                         },
#                                         epsilon=epsilon))  
#   }else{   
#     #The early model that will calculate the probabilities that will be used during the sort process
#     early_model<-map(kernel_type,~ksvm(formula,data=train,type="eps-svr",
#                                        kernel=if(.x=="vanilladot"){
#                                          "polydot"
#                                        }else{
#                                          .x
#                                        },
#                                        C=cost,
#                                        kpar=if(.x=='laplacedot')
#                                        {
#                                          list(sigma=gamma_lap)
#                                        }else if(.x=='rbfdot'){
#                                          
#                                          list(sigma=gamma_rbf)
#                                          
#                                        }else if(.x=='polydot'){
#                                          list(degree=2,scale=poly_scale,offset=0)
#                                        }else{
#                                          list(degree=1,scale=poly_scale,offset=0)
#                                        },
#                                        epsilon=epsilon))  
#   }
#   #Calculando o predict para cada modelo
#   predict<-lapply(early_model,function(x)predict(x,newdata=test))
#   
#   
#   #Calculating the weights (Equation 9)
#   rmse<-lapply(predict,function(x){loss_function(predicted=x,observed=test[,class_name],epsilon)}) %>% unlist
#   rmse<-rmse/sd(rmse)
#   # std_rmse<-rmse/(range(test[,class_name])[2]-range(test[,class_name])[1])
#   inv_rmse<-(exp(-rmse*beta))
#   
#   prob_weights<-inv_rmse/sum(inv_rmse)
#   prob_weights<-ifelse(prob_weights<0,0,prob_weights)#To not heve negative values of probabilities
#   
#   
#   #----Defining the variables----
#   models<-rep(list(0),boots_size)#Creating the list of models
#   boots_sample<-list(rep(boots_size)) #Argument that will be passed in the map function
#   out_of_bag<-list(rep(boots_size)) #OOB samples object
#   boots_index_row<-list(nrow(train)) %>% rep(boots_size)
#   
#   #====================================================
#   
#   #======Selecting the Bootstraping samples============
#   #Defining which rows will be sampled
#   if(is.null(seed.bootstrap)){
#     boots_index_row<-map(boots_index_row,~sample(1:.x,.x,replace=TRUE))#Generating the boots_sample index
#   }else{
#     set.seed(seed.bootstrap)
#     boots_index_row<-map(boots_index_row,~sample(1:.x,.x,replace=TRUE))#Generating the boots_sample index
#   }
#   
#   #Defining out_of the bags_sample
#   #Defining the Boots samples
#   boots_sample<-map(boots_index_row,~train[.x,]) #Without feature susection
#   out_of_bag<-map(boots_index_row,~train[-unique(.x),])
#   
#   #=====================================================
#   
#   #=================Generating the models===============
#   #Calculating the models
#   
#   #Here is defined which kernel will be used to heach model
#   random_kernel<-sample(c('rbfdot','polydot','laplacedot','vanilladot'),
#                         boots_size,replace = TRUE,prob = prob_weights)
#   
#   if(automatic_tuning){
#     models<-map2(boots_sample,random_kernel,~ksvm(formula, data=.x,type="eps-svr",
#                                                   kernel=if(.y=="vanilladot"){
#                                                     "polydot"
#                                                   }else{
#                                                     .y
#                                                   },
#                                                   C=cost,
#                                                   kpar=if(.y=='laplacedot' ||.y=='rbfdot')
#                                                   {
#                                                     "automatic"
#                                                   }else if(.y=='polydot'){
#                                                     list(degree=2,scale=poly_scale,offset=0)
#                                                   }else{
#                                                     list(degree=1,scale=poly_scale,offset=0)
#                                                   },
#                                                   epsilon=epsilon))
#     
#   }else{
#     models<-map2(boots_sample,random_kernel,~ksvm(formula, data=.x,type="eps-svr",
#                                                   kernel=if(.y=="vanilladot"){
#                                                     "polydot"
#                                                   }else{
#                                                     .y
#                                                   },
#                                                   C=cost,
#                                                   kpar=if(.y=='laplacedot')
#                                                   {
#                                                     list(sigma=gamma_lap)
#                                                   }else if(.y=='rbfdot'){
#                                                     list(sigma=gamma_rbf)
#                                                   }else if(.y=='polydot'){
#                                                     list(degree=2,scale=poly_scale,offset=0)
#                                                   }else{
#                                                     list(degree=1,scale=poly_scale,offset=0)
#                                                   },epsilon=epsilon))
#     
#   }
#   
#   
#   #Prediction of each mode
#   predict<-map(models,~predict(.x,newdata=test))
#   
#   #Prediction of OOB samples
#   predict_oobg<-map2(models,out_of_bag,~predict(.x,newdata=.y))
#   
#   #Calculating weights from equation 10
#   kernel_weight<-map2_dbl(predict_oobg,out_of_bag,~loss_function(predicted = .x,observed = .y[,class_name],epsilon)) #%>% 
#   #./sd(.) #%>% 
#   #map_dbl(~1/(1-exp(-.x*beta))^2)
#   
#   boots_error<-map2_dbl(predict_oobg,out_of_bag,~loss_function(predicted = .x,observed = .y[,class_name],epsilon)) #%>% 
#   
#   
#   kernel_weight<-(kernel_weight/sd(kernel_weight)) %>% map_dbl(~exp(-.x*beta))
#   
#   kernel_weight<-kernel_weight/sum((kernel_weight))
#   
#   
#   #Predictions finals
#   predict_df<-predict %>%                         #Generating a matrix with where the the rows are each bootstrap sample
#     unlist %>%                         #and the columns are each observation from test set
#     matrix(ncol=nrow(test),byrow = TRUE)
#   
#   
#   correlation_models<-cor(t(predict_df))
#   
#   correlation_measure<-mean(correlation_models[upper.tri(correlation_models)])
#   
#   predict_df_new<-map(seq(1:nrow(test)),~predict_df[,.x])#Transposing the matrix
#   
#   
#   #Como associar o peso!
#   
#   pred_df_fct<-map(predict_df_new,~.x*kernel_weight) %>% #Multiplying the weights
#     map_dbl(sum) 
#   
#   
#   #=============================
#   return(list(predicted=pred_df_fct,lambda_values=list(Lin_Kern=prob_weights[4],
#                                                        Pol_Kern=prob_weights[2],
#                                                        RBF_Kern=prob_weights[1],
#                                                        LAP_Kern=prob_weights[3]),
#               model_params=list(class_name=class_name,
#                                 boots_size=boots_size,
#                                 cost=cost,
#                                 gamma=gamma,
#                                 degree=degree),bootstrap_models=models,bootstrap_samples=boots_sample,probabilities=prob_weights,
#               init_rmse=rmse,kernel_weight=kernel_weight,
#               correlation_measure=correlation_measure,list_kernels=random_kernel,predict_oob=predict_oobg,botse=boots_error))
# }
# 
# bagged_svr<-function(formula,#Formula that will be used
#                      train,#The Training set
#                      test,#The test set
#                      class_name,#The string corresponding to the variable that will be predicted
#                      boots_size=100, #B correspoding to the number of bootstrap samples
#                      cost=1,poly_scale,#Cost parameter of SVM
#                      kernel,#Kernel Function
#                      gamma=1,offset=1,#Gamma used in Table 1.  
#                      degree=2,
#                      epsilon,seed.bootstrap=NULL,automatic_tuning=FALSE
# ){
#   #Probability associated with each kernel function
#   
#   
#   #----Defining the variables----
#   models<-rep(list(0),boots_size)#Creating the list of models
#   boots_sample<-list(rep(boots_size)) #Argument that will be passed in the map function
#   out_of_bag<-list(rep(boots_size)) #OOB samples object
#   boots_index_row<-list(nrow(train)) %>% rep(boots_size)
#   
#   #=============Col_Sample============
#   
#   #======Selecting the Bootstraping samples============
#   #Defining which rows will be sampled
#   if(is.null(seed.bootstrap)){
#     boots_index_row<-map(boots_index_row,~sample(1:.x,.x,replace=TRUE))#Generating the boots_sample index
#   }else{
#     set.seed(seed.bootstrap)
#     boots_index_row<-map(boots_index_row,~sample(1:.x,.x,replace=TRUE))#Generating the boots_sample index
#   }
#   
#   #Defining out_of the bags_sample
#   #Defining the Boots samples
#   boots_sample<-map(boots_index_row,~train[.x,]) #Without feature susection
#   out_of_bag<-map(boots_index_row,~train[-unique(.x),])
#   
#   #=====================================================
#   
#   #=================Generating the models===============
#   #Calculating the models
#   
#   if(automatic_tuning){
#     models<-map(boots_sample,~ksvm(formula, data=.x,type="eps-svr",
#                                    kernel=if(kernel=="vanilladot"){
#                                      "polydot"
#                                    }else{
#                                      kernel
#                                    },
#                                    kpar=if(kernel=='laplacedot' ||kernel=='rbfdot')
#                                    {
#                                      "automatic"
#                                    }else if(kernel=='polydot'){
#                                      list(degree=degree,scale=poly_scale,offset=offset)
#                                    }else{
#                                      list(degree=1,scale=poly_scale,offset=offset)
#                                    },C=cost,
#                                    ,epsilon=epsilon))
#     
#     
#   }else{
#     models<-map(boots_sample,~ksvm(formula, data=.x,type="eps-svr",
#                                    kernel=if(kernel=="vanilladot"){
#                                      "polydot"
#                                    }else{
#                                      kernel
#                                    },
#                                    C=cost,
#                                    kpar=if(kernel=='polydot'){
#                                      list(degree=degree,scale=poly_scale,offset=offset)
#                                    }else if(kernel=='vanilladot'){
#                                      list(degree=1,scale=poly_scale,offset=offset)
#                                    }else{
#                                      list(sigma=gamma)
#                                    },epsilon=epsilon))
#     
#   }
#   
#   
#   #Prediction of each mode
#   predict<-map(models,~predict(.x,newdata=test))
#   
#   
#   #Predictions finals
#   predict_df<-predict %>%                         #Generating a matrix with where the the rows are each bootstrap sample
#     unlist %>%                         #and the columns are each observation from test set
#     matrix(ncol=nrow(test),byrow = TRUE)
#   
#   
#   correlation_models<-cor(t(predict_df))
#   
#   correlation_measure<-mean(correlation_models[upper.tri(correlation_models)])
#   
#   
#   predict_df_new<-map(seq(1:nrow(test)),~predict_df[,.x])#Transposing the matrix
#   
#   
#   #Como associar o peso!
#   
#   pred_df_fct<-map_dbl(predict_df_new,mean) 
#   
#   
#   
#   #=============================
#   return(list(predicted=pred_df_fct,
#               model_params=list(class_name=class_name,
#                                 boots_size=boots_size,
#                                 cost=cost,
#                                 gamma=gamma,
#                                 degree=degree),bootstrap_models=models,bootstrap_samples=boots_sample,
#               correlation_measure=correlation_measure))
# }
# 
# 
# calculate_error<-function(list,resultado,metrica_error){
#   #Funcao e metrica de erro
#   aux<-resultado
#   split(aux$predicted,ceiling(seq_along(aux$predicted)/nrow(list[[1]]$test))) %>% 
#     map2_dbl(list,~metrica_error(.x,.y$test$y)) %>% mean(na.rm=TRUE)
# }
# 
# calculate_error_single<-function(list,modelo,metrica_error){
#   predictions<-map2(modelo,list,~predict(.x,newdata=.y$test))
#   result_error<-map2_dbl(predictions,list,~(RMSE(predicted =.x,observed = .y$test$y)))# %>% mean()
#   return(result_error)
# }
# 
# 
# random_machines_svr_new<-function(formula,#Formula that will be used
#                                   train,#The Training set
#                                   test,#The test set
#                                   test_new, #New TEST
#                                   class_name,#The string corresponding to the variable that will be predicted
#                                   boots_size=25, #B correspoding to the number of bootstrap samples
#                                   cost=1,#Cost parameter of SVM
#                                   gamma_rbf=1,#Gamma used in Table 1.
#                                   gamma_lap=1,
#                                   degree=2,#Degree used in Table 1.
#                                   epsilon=0.1,beta=2,seed.bootstrap=NULL,
#                                   loss_function,automatic_tuning=FALSE, #Choose a loss-fucntion
#                                   poly_scale
#                                   
# ){
#   #Probability associated with each kernel function
#   
#   #Root Mean Squared Error Function
#   RMSE<-function(predicted,observed,epsilon=NULL){
#     min<-min(observed)
#     max<-max(observed)
#     sqrt(mean(unlist((predicted-observed)^2)))
#   }
#   
#   
#   prob_weights<-list()
#   
#   #The Kernel types used in the algorithm
#   kernel_type<-c('rbfdot','polydot','laplacedot','vanilladot')
#   
#   #TUNING AUTOMÁTICO
#   if(automatic_tuning){
#     
#     early_model<- map(kernel_type,~ksvm(formula,data=train,type="eps-svr",
#                                         kernel=if(.x=="vanilladot"){
#                                           "polydot"
#                                         }else{
#                                           .x
#                                         },
#                                         C=cost,
#                                         kpar=if(.x=='laplacedot' ||.x=='rbfdot')
#                                         {
#                                           "automatic"
#                                         }else if(.x=='polydot'){
#                                           list(degree=2,scale=poly_scale,offset=0)
#                                         }else{
#                                           list(degree=1,scale=poly_scale,offset=0)
#                                         },
#                                         epsilon=epsilon))  
#   }else{   
#     #The early model that will calculate the probabilities that will be used during the sort process
#     early_model<-map(kernel_type,~ksvm(formula,data=train,type="eps-svr",
#                                        kernel=if(.x=="vanilladot"){
#                                          "polydot"
#                                        }else{
#                                          .x
#                                        },
#                                        C=cost,
#                                        kpar=if(.x=='laplacedot')
#                                        {
#                                          list(sigma=gamma_lap)
#                                        }else if(.x=='rbfdot'){
#                                          
#                                          list(sigma=gamma_rbf)
#                                          
#                                        }else if(.x=='polydot'){
#                                          list(degree=2,scale=poly_scale,offset=0)
#                                        }else{
#                                          list(degree=1,scale=poly_scale,offset=0)
#                                        },
#                                        epsilon=epsilon))  
#   }
#   #Calculando o predict para cada modelo
#   predict<-lapply(early_model,function(x)predict(x,newdata=test))
#   
#   
#   #Calculating the weights (Equation 9)
#   rmse<-lapply(predict,function(x){loss_function(predicted=x,observed=test[,class_name],epsilon)}) %>% unlist
#   rmse<-rmse/sd(rmse)
#   # std_rmse<-rmse/(range(test[,class_name])[2]-range(test[,class_name])[1])
#   inv_rmse<-(exp(-rmse*beta))
#   
#   prob_weights<-inv_rmse/sum(inv_rmse)
#   prob_weights<-ifelse(prob_weights<0,0,prob_weights)#To not heve negative values of probabilities
#   
#   
#   #----Defining the variables----
#   models<-rep(list(0),boots_size)#Creating the list of models
#   boots_sample<-list(rep(boots_size)) #Argument that will be passed in the map function
#   out_of_bag<-list(rep(boots_size)) #OOB samples object
#   boots_index_row<-list(nrow(train)) %>% rep(boots_size)
#   
#   #====================================================
#   
#   #======Selecting the Bootstraping samples============
#   #Defining which rows will be sampled
#   if(is.null(seed.bootstrap)){
#     boots_index_row<-map(boots_index_row,~sample(1:.x,.x,replace=TRUE))#Generating the boots_sample index
#   }else{
#     set.seed(seed.bootstrap)
#     boots_index_row<-map(boots_index_row,~sample(1:.x,.x,replace=TRUE))#Generating the boots_sample index
#   }
#   
#   #Defining out_of the bags_sample
#   #Defining the Boots samples
#   boots_sample<-map(boots_index_row,~train[.x,]) #Without feature susection
#   out_of_bag<-map(boots_index_row,~train[-unique(.x),])
#   
#   #=====================================================
#   
#   #=================Generating the models===============
#   #Calculating the models
#   
#   #Here is defined which kernel will be used to heach model
#   random_kernel<-sample(c('rbfdot','polydot','laplacedot','vanilladot'),
#                         boots_size,replace = TRUE,prob = prob_weights)
#   
#   if(automatic_tuning){
#     models<-map2(boots_sample,random_kernel,~ksvm(formula, data=.x,type="eps-svr",
#                                                   kernel=if(.y=="vanilladot"){
#                                                     "polydot"
#                                                   }else{
#                                                     .y
#                                                   },
#                                                   C=cost,
#                                                   kpar=if(.y=='laplacedot' ||.y=='rbfdot')
#                                                   {
#                                                     "automatic"
#                                                   }else if(.y=='polydot'){
#                                                     list(degree=2,scale=poly_scale,offset=0)
#                                                   }else{
#                                                     list(degree=1,scale=poly_scale,offset=0)
#                                                   },
#                                                   epsilon=epsilon))
#     
#   }else{
#     models<-map2(boots_sample,random_kernel,~ksvm(formula, data=.x,type="eps-svr",
#                                                   kernel=if(.y=="vanilladot"){
#                                                     "polydot"
#                                                   }else{
#                                                     .y
#                                                   },
#                                                   C=cost,
#                                                   kpar=if(.y=='laplacedot')
#                                                   {
#                                                     list(sigma=gamma_lap)
#                                                   }else if(.y=='rbfdot'){
#                                                     list(sigma=gamma_rbf)
#                                                   }else if(.y=='polydot'){
#                                                     list(degree=2,scale=poly_scale,offset=0)
#                                                   }else{
#                                                     list(degree=1,scale=poly_scale,offset=0)
#                                                   },epsilon=epsilon))
#     
#   }
#   
#   
#   #Prediction of each mode
#   predict<-map(models,~predict(.x,newdata=test_new))
#   
#   #Prediction of OOB samples
#   predict_oobg<-map2(models,out_of_bag,~predict(.x,newdata=.y))
#   
#   #Calculating weights from equation 10
#   kernel_weight<-map2_dbl(predict_oobg,out_of_bag,~loss_function(predicted = .x,observed = .y[,class_name],epsilon)) #%>% 
#   #./sd(.) #%>% 
#   #map_dbl(~1/(1-exp(-.x*beta))^2)
#   
#   boots_error<-map2_dbl(predict_oobg,out_of_bag,~loss_function(predicted = .x,observed = .y[,class_name],epsilon)) #%>% 
#   
#   
#   kernel_weight<-(kernel_weight/sd(kernel_weight)) %>% map_dbl(~exp(-.x*beta))
#   
#   kernel_weight<-kernel_weight/sum((kernel_weight))
#   
#   
#   #Predictions finals
#   predict_df<-predict %>%                         #Generating a matrix with where the the rows are each bootstrap sample
#     unlist %>%                         #and the columns are each observation from test set
#     matrix(ncol=nrow(test_new),byrow = TRUE)
#   
#   
#   correlation_models<-cor(t(predict_df))
#   
#   correlation_measure<-mean(correlation_models[upper.tri(correlation_models)])
#   
#   predict_df_new<-map(seq(1:nrow(test_new)),~predict_df[,.x])#Transposing the matrix
#   
#   
#   #Como associar o peso!
#   
#   pred_df_fct<-map(predict_df_new,~.x*kernel_weight) %>% #Multiplying the weights
#     map_dbl(sum) 
#   
#   
#   #=============================
#   return(list(predicted=pred_df_fct,lambda_values=list(Lin_Kern=prob_weights[4],
#                                                        Pol_Kern=prob_weights[2],
#                                                        RBF_Kern=prob_weights[1],
#                                                        LAP_Kern=prob_weights[3]),
#               model_params=list(class_name=class_name,
#                                 boots_size=boots_size,
#                                 cost=cost,
#                                 gamma=gamma,
#                                 degree=degree),bootstrap_models=models,bootstrap_samples=boots_sample,probabilities=prob_weights,
#               init_rmse=rmse,kernel_weight=kernel_weight,
#               correlation_measure=correlation_measure,list_kernels=random_kernel,predict_oob=predict_oobg,botse=boots_error))
# }
# 
# 
# 
# 
