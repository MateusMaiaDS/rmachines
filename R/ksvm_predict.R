## Support Vector Machines
## author : alexandros karatzoglou
## updated : 08.02.06
## 
## I (MATEUS MAIA) DO NOT OWN THIS TYPE OF CODE PLEASE ACCESS FOR THE ORIGINAL AND COMPLETE CODE: https://github.com/cran/kernlab/pulls

setMethod("predict", signature(object = "ksvm"),
          function (object, newdata, type = "response", coupler = "minpair")
          {
                type <- match.arg(type,c("response","probabilities","votes","decision"))
                if (missing(newdata) && type=="response" & !is.null(fitted(object)))
                      return(fitted(object))
                else if(missing(newdata))
                      stop("Missing data !")
                
                if(!is(newdata,"list")){
                      if (!is.null(terms(object)) & !is(newdata,"kernelMatrix"))
                      {
                            if(!is.matrix(newdata))
                                  newdata <- model.matrix(delete.response(terms(object)), as.data.frame(newdata), na.action = n.action(object))
                      }
                      else
                            newdata  <- if (is.vector(newdata)) t(t(newdata)) else as.matrix(newdata)
                      
                      
                      newnrows <- nrow(newdata)
                      newncols <- ncol(newdata)
                      if(!is(newdata,"kernelMatrix") && !is.null(xmatrix(object))){
                            if(is(xmatrix(object),"list") && is(xmatrix(object)[[1]],"matrix")) oldco <- ncol(xmatrix(object)[[1]])
                            if(is(xmatrix(object),"matrix")) oldco <- ncol(xmatrix(object))
                            if (oldco != newncols) stop ("test vector does not match model !")
                      }
                }
                else
                      newnrows <- length(newdata)
                
                p <- 0
                
                if (is.list(scaling(object)))
                      newdata[,scaling(object)$scaled] <-
                      scale(newdata[,scaling(object)$scaled, drop = FALSE],
                            center = scaling(object)$x.scale$"scaled:center", scale  = scaling(object)$x.scale$"scaled:scale")
                
                if(type == "response" || type =="decision" || type=="votes")
                {
                      if(type(object)=="C-svc"||type(object)=="nu-svc"||type(object)=="C-bsvc")
                      {
                            predres <- 1:newnrows
                            if(type=="decision")
                                  votematrix <- matrix(0,nclass(object)*(nclass(object)-1)/2,newnrows)
                            else
                                  votematrix <- matrix(0,nclass(object),newnrows)
                            
                            for(i in 1:(nclass(object)-1))
                            {
                                  jj <- i+1
                                  for(j in jj:nclass(object))
                                  {
                                        p <- p+1
                                        
                                        if(is(newdata,"kernelMatrix"))
                                              ret <- newdata[,which(SVindex(object)%in%alphaindex(object)[[p]]), drop=FALSE] %*% coef(object)[[p]] - b(object)[p]
                                        else
                                              ret <- kernelMult(kernelf(object),newdata,xmatrix(object)[[p]],coef(object)[[p]]) - b(object)[p]
                                        
                                        if(type=="decision")
                                              votematrix[p,] <- ret
                                        else{
                                              votematrix[i,ret<0] <- votematrix[i,ret<0] + 1
                                              votematrix[j,ret>0] <- votematrix[j,ret>0] + 1
                                        }
                                  }
                            }
                            if(type == "decision")
                                  predres <-  t(votematrix)
                            else
                                  predres <- sapply(predres, function(x) which.max(votematrix[,x]))
                      }
                      
                      if(type(object) == "spoc-svc")
                      {
                            predres <- 1:newnrows
                            votematrix <- matrix(0,nclass(object),newnrows)
                            for(i in 1:nclass(object)){
                                  if(is(newdata,"kernelMatrix"))
                                        votematrix[i,] <- newdata[,which(SVindex(object)%in%alphaindex(object)[[i]]), drop=FALSE] %*% coef(object)[[i]]
                                  else if (is(newdata,"list"))
                                        votematrix[i,] <- kernelMult(kernelf(object),newdata,xmatrix(object)[alphaindex(object)[[i]]],coef(object)[[i]])
                                  else
                                        votematrix[i,] <- kernelMult(kernelf(object),newdata,xmatrix(object)[alphaindex(object)[[i]],,drop=FALSE],coef(object)[[i]])
                            }
                            predres <- sapply(predres, function(x) which.max(votematrix[,x]))
                      }
                      
                      if(type(object) == "kbb-svc")
                      {
                            predres <- 1:newnrows
                            votematrix <- matrix(0,nclass(object),newnrows)
                            A <- rowSums(alpha(object))
                            
                            for(i in 1:nclass(object))
                            {
                                  for(k in (1:i)[-i])
                                        if(is(newdata,"kernelMatrix"))
                                              votematrix[k,] <- votematrix[k,] - (newdata[,which(SVindex(object)%in%alphaindex(object)[[i]]), drop=FALSE] %*% alpha(object)[,k][alphaindex(object)[[i]]] + sum(alpha(object)[,k][alphaindex(object)[[i]]]))
                                        else if (is(newdata,"list"))
                                              votematrix[k,] <- votematrix[k,] - (kernelMult(kernelf(object),newdata,xmatrix(object)[alphaindex(object)[[i]]],alpha(object)[,k][alphaindex(object)[[i]]]) + sum(alpha(object)[,k][alphaindex(object)[[i]]]))
                                        else
                                              votematrix[k,] <- votematrix[k,] - (kernelMult(kernelf(object),newdata,xmatrix(object)[alphaindex(object)[[i]],,drop=FALSE],alpha(object)[,k][alphaindex(object)[[i]]]) + sum(alpha(object)[,k][alphaindex(object)[[i]]]))
                                        
                                        if(is(newdata,"kernelMatrix"))
                                              votematrix[i,] <- votematrix[i,] + (newdata[,which(SVindex(object)%in%alphaindex(object)[[i]]), drop=FALSE] %*% A[alphaindex(object)[[i]]] + sum(A[alphaindex(object)[[i]]]))
                                        else if (is(newdata,"list"))
                                              votematrix[i,] <- votematrix[i,] + (kernelMult(kernelf(object),newdata,xmatrix(object)[alphaindex(object)[[i]]],A[alphaindex(object)[[i]]]) + sum(A[alphaindex(object)[[i]]]))
                                        else
                                              votematrix[i,] <- votematrix[i,] + (kernelMult(kernelf(object),newdata,xmatrix(object)[alphaindex(object)[[i]],,drop=FALSE],A[alphaindex(object)[[i]]]) + sum(A[alphaindex(object)[[i]]]))
                                        
                                        if(i <= (nclass(object)-1))
                                              for(kk in i:(nclass(object)-1))
                                                    if(is(newdata,"kernelMatrix"))
                                                          votematrix[kk+1,] <- votematrix[kk+1,] - (newdata[,which(SVindex(object)%in%alphaindex(object)[[i]]), drop=FALSE] %*% alpha(object)[,kk][alphaindex(object)[[i]]] + sum(alpha(object)[,kk][alphaindex(object)[[i]]]))
                                                    else if (is(newdata,"list"))
                                                          votematrix[kk+1,] <- votematrix[kk+1,] - (kernelMult(kernelf(object),newdata,xmatrix(object)[alphaindex(object)[[i]]],alpha(object)[,kk][alphaindex(object)[[i]]]) + sum(alpha(object)[,kk][alphaindex(object)[[i]]]))
                                                    else
                                                          votematrix[kk+1,] <- votematrix[kk+1,] - (kernelMult(kernelf(object),newdata,xmatrix(object)[alphaindex(object)[[i]],,drop=FALSE],alpha(object)[,kk][alphaindex(object)[[i]]]) + sum(alpha(object)[,kk][alphaindex(object)[[i]]]))
                            }
                            predres <- sapply(predres, function(x) which.max(votematrix[,x]))
                      }
                }
                
                if(type == "probabilities")
                {
                      if(is.null(prob.model(object)[[1]]))
                            stop("ksvm object contains no probability model. Make sure you set the paramater prob.model in ksvm during training.")
                      
                      if(type(object)=="C-svc"||type(object)=="nu-svc"||type(object)=="C-bsvc")
                      {
                            binprob <- matrix(0, newnrows, nclass(object)*(nclass(object) - 1)/2)
                            for(i in 1:(nclass(object)-1))
                            {
                                  jj <- i+1
                                  for(j in jj:nclass(object))
                                  {
                                        p <- p+1
                                        if(is(newdata,"kernelMatrix"))
                                              binprob[,p] <- 1 - .SigmoidPredict(as.vector(newdata[,which(SVindex(object)%in%alphaindex(object)[[p]]), drop=FALSE] %*% coef(object)[[p]] - b(object)[p]), prob.model(object)[[p]]$A, prob.model(object)[[p]]$B)
                                        else
                                              binprob[,p] <- 1 - .SigmoidPredict(as.vector(kernelMult(kernelf(object),newdata,xmatrix(object)[[p]],coef(object)[[p]]) - b(object)[p]), prob.model(object)[[p]]$A, prob.model(object)[[p]]$B)
                                  }
                            }
                            multiprob <- couple(binprob, coupler = coupler)
                      }
                      else
                            stop("probability estimates only supported for C-svc, C-bsvc and nu-svc")
                }
                
                if(type(object) == "one-svc")
                {
                      if(is(newdata,"kernelMatrix"))
                            ret <- newdata %*% coef(object) - b(object)
                      else
                            ret <- kernelMult(kernelf(object),newdata,xmatrix(object),coef(object)) - b(object)
                      ##one-class-classification: return TRUE/FALSE (probabilities ?)
                      if(type=="decision")
                            return(ret)
                      else
                      {
                            ret[ret>0]<-1
                            return(ret == 1)
                      }
                }
                else {
                      if(type(object)=="eps-svr"||type(object)=="nu-svr"||type(object)=="eps-bsvr")
                      {
                            if(is(newdata,"kernelMatrix"))
                                  predres <- newdata %*% coef(object) - b(object)
                            else
                                  predres <- kernelMult(kernelf(object),newdata,xmatrix(object),coef(object)) - b(object)
                      }
                      else {
                            ##classification & votes : return votematrix
                            if(type == "votes")
                                  return(votematrix)
                            
                            ##classification & probabilities : return probability matrix
                            if(type == "probabilities")
                            {
                                  colnames(multiprob) <- lev(object)
                                  return(multiprob)
                            }
                            
                            if(is.numeric(lev(object)) && type == "response")
                                  return(lev(object)[predres])
                            
                            if (is.character(lev(object)) && type!="decision")
                            {
                                  ##classification & type response: return factors
                                  if(type == "response")
                                        return(factor (lev(object)[predres], levels = lev(object)))
                            }
                      }
                }
                
                if (!is.null(scaling(object)$y.scale) & !is(newdata,"kernelMatrix") & !is(newdata,"list"))
                      ## return raw values, possibly scaled back
                      return(predres * scaling(object)$y.scale$"scaled:scale" + scaling(object)$y.scale$"scaled:center")
                else
                      ##else: return raw values
                      return(predres)
          })


