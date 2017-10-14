#'A multiple ridge regression model (RC)
#'
#'@description  Class for the convenient multiple ridge regression 
#'
#'@param formula Contains dependent and independent variables for linear regression
#'@param data A data.frame to conduct linear regression
#'@param lambda The ridge constant, a numeric scalar
#'
#'@importFrom methods new
#'@exportClass ridgereg
#'@export ridgereg
#
#ridgereg_mod <- ridgereg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda = 1)

ridgereg <- setRefClass("ridgereg", 
                        fields = list(formula="formula",
                                      data="data.frame",
                                      reg_coe="matrix",
                                      fit_val="matrix",
                                      data_name="character",
                                      lambda="numeric"),
                        
                        methods = list(
                          initialize = function(formula, data, lambda=0){
                            
                            formula <<- formula
                            data <<- data
                            lambda <<- lambda
                            
                            data_name <<- deparse(substitute(data))
                            
                            #getting y variable from the formula
                            get_y <- all.vars(formula)[1]
                            
                            #defining X and y
                            y <- data[[get_y]]
                            X <- model.matrix(formula, data)
                            
                            #Normalize
                            p <- ncol(X)
                            X <- cbind(X[,1], scale(X[,-1]))
                            
                            #Ridge regression
                            # reg_coe <<- solve(t(X)%*%X+lambda*diag(p)) %*% t(X)%*%y
                            # dimnames(reg_coe) <<- list(c("(Intercept)", all.vars(formula)[-1]), NULL)

                            # fit_val <<- X%*%reg_coe
                            
                            #QR Decomposition
                            y<-as.matrix(data[get_y],ncol=1)
                            X_qr<-rbind(X,sqrt(lambda)*diag(p))
                            y_qr<-rbind(y,matrix(data = 0, nrow = p, ncol = 1))
                            
                            QR <- qr((X_qr))
                            Q <- qr.Q(QR)
                            R <- qr.R(QR)
                            
                            reg_coe <<- qr.solve(R) %*% t(Q) %*% as.matrix(y_qr)
                            dimnames(reg_coe) <<- list(c("(Intercept)", all.vars(formula)[-1]), NULL)
                            
                            fit_val <<- X %*% reg_coe
                          },
                          
                          results = function(){
                            results <- list()
                            
                            results$coef <- reg_coe
                            results$fitted <- fit_val
                            return(results)
                            
                          },
                          
                          print = function(){
                            "Print out the coefficients and coefficient names"
                            
                            cat("Call: \n ")
                            cat(paste0("ridgereg(formula = ",format(formula),", data = ",data_name,", lambda = ",lambda,")\n\n"))
                            cat("Coefficients: \n")
                            cat(" ",row.names(reg_coe), "\n    ", sep = "  ")
                            cat(t(reg_coe[,1]), sep="    ")
                            
                          },
                          
                          
                          predict = function(newdata=NULL){ #if newdata is used, it should be a data frame
                            "Prints out the predicted values or if newdata is used, prints out predicted values for the new data set"
                            
                            if(is.null(newdata)){
                              result <- structure(c(results()[[2]]), names=(1:length(results()[[2]])))
                            } else{
                              X<-model.matrix(object=formula, data=newdata)
                              X[,2:ncol(X)] <- scale(X[,-1])
                              result <- (X %*% results()[[1]])[,1]
                            }
                            return(result)
                          },
                          
                          # predict = function(){
                          #   "Return the predicted values y_hat"
                          #   return(fit_val)
                          # },
                          
                          coef = function(){
                            "Return the coefficients as a named vector"
                            return(reg_coe)
                          }
                        )
)