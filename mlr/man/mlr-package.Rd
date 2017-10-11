\name{mlr-package}
\alias{mlr-package}
\alias{mlr}
\docType{package}
\title{
mlr packagae
}
\description{
\packageDescription{mlr}
}
\details{
More details can be found on package vignette.
}
\author{
\packageAuthor{mlr}

Maintainer: \packageMaintainer{mlr}
}
\references{
\url{http://math.stackexchange.com/questions/299481/qr-factorization-for-ridge-regression}
}

\keyword{ package }

\seealso{
\url{https://www.rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf}  
 
\url{http://topepo.github.io/caret/Linear Regression.html}  
 
\url{http://topepo.github.io/caret/custom models.html}  
 
\url{https://cran.r-project.org/web/packages/caret/vignettes/caret.pdf}   
 
\url{http://topepo.github.io/caret/training.html}   
}  

\examples{
ridgereg_mod <- ridgereg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda = 1)
}
