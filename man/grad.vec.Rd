\name{grad.vec}
\alias{grad.vec}

\title{
Numerical Gradient of a Function vectorized using foreach
}
\description{
Calculate the gradient of a function by numerical approximation and vectorized using foreach
}
\usage{
grad.vec(func, x, method = "simple", method.args = list(), vec = FALSE, core = 2)
}

\arguments{
    \item{func}{a function with a scalar real result (see details).}
    \item{x}{a real scalar or vector argument to func, indicating the 
      point(s) at which the gradient is to be calculated.}
    \item{method}{one of \code{"simple"}, or \code{"complex"} indicating the method to use for the approximation.}
    \item{method.args}{arguments passed to method. Arguments not specified 
       remain with their default values as specified in details}
\item{vec}{
To specify the way you want to vectorize the function. See Details.
}
  \item{core}{
If you choose the foreach way, you need to set the number of cores for foreach. See Details.
}
}
\details{
   The function \code{grad.vec} calculates a numerical approximation of the first derivative of \code{func} at the point \code{x}. It is assumed \code{func}  is a scalar value function.
}
\value{
A real scalar or vector of the approximated gradient(s).
}

\author{
Qiang Kou, Yann Richet
}

\note{
However, there is one point we want to emphasize, if the number of possible parralel evaluations is not enough high, the gain will be very thin, even negative, since the messaging process between cores costs much time. This is typically can be seen in Nelder Mead results.

Also, the gain in running time can be seen when the objective function is CPU-expensive. And in our test only when the evaluating time overruns 0.1 second, the vectorized or parallel is meaningful.
}

\seealso{
\code{\link[numDeriv]{grad}}, \code{\link[base]{apply}}, \code{\link[foreach]{foreach}}
}
\examples{
  grad.vec(sin, pi)
  grad.vec(sin, (0:10)*2*pi/10)
  func0 <- function(x){ sum(sin(x))  }
  grad.vec(func0 , (0:10)*2*pi/10, vec="foreach")

}

\keyword{ gradient }
\keyword{ foreach }
