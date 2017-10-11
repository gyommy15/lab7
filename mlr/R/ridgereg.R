#'A multiple ridge regression model (RC)
#'
#'@description  Class for the convenient multiple ridge regression 
#'
#'@param formula Contains dependent and independent variables for linear regression
#'@param data A data.frame to conduct linear regression
#'
#'@exportClass ridgereg
#'@export ridgereg
#
#formula=Petal.Length~Sepal.Width+Sepal.Length
#data=iris
#ridgereg_mod <- ridgereg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda = 1)

ridgereg <- setRefClass("ridgereg", 
                        fields = list(formula="formula",
                                      data="data.frame",
                                      reg_coe="matrix",
                                      fit_val="matrix",
                                      residu="matrix",
                                      dof="numeric",
                                      res_var="matrix",
                                      var_reg_coe="numeric",
                                      t_val="matrix",
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
                            reg_coe <<- solve(t(X)%*%X+lambda*diag(p)) %*% t(X)%*%y
                            dimnames(reg_coe) <<- list(c("(Intercept)", all.vars(formula)[-1]), NULL)
                            
                            fit_val <<- X%*%reg_coe
                            
                          },
                          
                          print = function(){
                            "Print out the coefficients and coefficient names"
                            
                            cat("Call: \n ")
                            cat(paste0("ridgereg(formula = ",format(formula),", data = ",data_name,", lambda = ",lambda,")\n\n"))
                            cat("Coefficients: \n")
                            cat(" ",row.names(reg_coe), "\n    ", sep = "  ")
                            cat(t(reg_coe[,1]), sep="    ")
                            
                          },

                          predict = function(){
                            "Return the predicted values y_hat"
                            return(fit_val)
                          },
                          
                          coef = function(){
                            "Return the coefficients as a named vector"
                            return(reg_coe)
                          }
                        )
)