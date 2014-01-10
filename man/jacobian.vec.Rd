\name{jacobian.vec}
\alias{jacobian.vec}

\title{
Gradient of a vector valued function vectorized using foreach 
}
\description{
Calculate the m by n numerical approximation of the gradient of a real m-vector valued function with n-vector argument.
}
\usage{
jacobian.vec(func, x, method = "simple", method.args = list(), vec = FALSE, core= 2)
}

\arguments{
\item{func}{a function with a real (vector) result.}
    \item{x}{a real or real vector argument to func, indicating the point 
       at which the gradient is to be calculated.}
    \item{method}{one of \code{"simple"}, or 
       \code{"complex"} indicating the method to use for the approximation.}
        \item{method.args}{arguments passed to method. 
       (Arguments not specified remain with their default values.)}
       

  \item{vec}{
To specify the way you want to vectorize the function. See Details.
}
  \item{core}{
If you choose the foreach way, you need to set the number of cores for foreach. See Details.
}
}
\details{
For \eqn{f:R^n -> R^m}{f:R^n -> R^m} calculate the \eqn{m x n}{m x n} 
   Jacobian \eqn{dy/dx}{dy/dx}.
   The function \code{jacobian} calculates a numerical approximation of the 
   first derivative of \code{func} at the point \code{x}. Any additional 
   arguments in \dots are also passed to \code{func}, but the gradient is not
   calculated with respect to these additional arguments.
   
This function is modified from \code{\link[numDeriv]{jacobian}}, and we add some vectorized options. The default one is "FALSE", so there is no difference from the original one; "apply" means using the \code{\link[base]{apply}} to vectorize; "foreach" uses \code{\link[foreach]{foreach}}.

}
\value{
A real m by n matrix.
}

\author{
Qiang Kou, Yann Richet
}


\note{
However, there is one point we want to emphasize, if the number of possible parralel evaluations is not enough high, the gain will be very thin, even negative, since the messaging process between cores costs much time. This is typically can be seen in Nelder Mead results.

Also, the gain in running time can be seen when the objective function is CPU-expensive. And in our test only when the evaluating time overruns 0.1 second, the vectorized or parallel is meaningful.
}




\seealso{
\code{\link[numDeriv]{jacobian}}, \code{\link[base]{apply}}, \code{\link[foreach]{foreach}}
}
\examples{

   func <- function(x) c(sin(x), cos(x))
   x <- (0:1)*2*pi
   jacobian.vec(func, x)
   \dontrun{
   jacobian.vec(func, x, vec="foreach")
}
}

\keyword{ optimization }
\keyword{ foreach }
