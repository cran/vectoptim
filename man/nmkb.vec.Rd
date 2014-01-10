\name{nmkb.vec}
\alias{nmkb.vec}

\title{
Nelder-Mead optimziation algorithm for derivative-free optimization 
}
\description{
An implementation of the Nelder-Mead algorithm for derivative-free optimization and vectorized using foreach.
}
\usage{
nmkb.vec(par, fn, lower = -Inf, upper = Inf, control = list(), vec = FALSE, core = 2, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{par}{A starting vector of parameter values. Must be feasible, i.e. lie strictly between lower and upper bounds.}
\item{fn}{
Nonlinear objective function that is to be optimized. 
      A scalar function that takes a real vector as argument and 
      returns a scalar that is the value of the function at that point 
      (see details).}
\item{lower}{Lower bounds on the parameters.  A vector of the same length as the parameters.  If a single value is specified, it is assumed that the same lower bound applies to all parameters.} 
\item{upper}{Upper bounds on the parameters.  A vector of the same length as the parameters.  If a single value is specified, it is assumed that the same upper bound applies to all parameters.} 
  \item{control}{A list of control parameters.  See *Details* for more information.
}
  \item{vec}{
To specify the way you want to vectorize the function. See Details.
}
  \item{core}{
If you choose the foreach way, you need to set the number of cores for foreach. See Details.
}
  \item{\dots}{
    pieces of data required to evaluate the objective function. See Details.
  }
}
\details{
Argument \code{control} is a list specifing any changes to default values of algorithm control parameters for the outer loop.  Note that the names of these must be specified completely.  Partial matching will not work.  The list items are as follows:

\code{tol} Convergence tolerance.  Iteration is terminated when the absolute difference in function value between successive iteration is below \code{tol}.  Default is 1.e-06.

\code{maxfeval}: Maximum number of objective function evaluations allowed.  Default is min(5000, max(1500, 20*length(par)^2)).

\code{regsimp} A logical variable indicating whether the starting parameter configuration is a regular simplex.  Default is TRUE.

\code{maximize} A logical variable indicating whether the objective function should be maximized.  Default is FALSE.

\code{restarts.max} Maximum number of times the algorithm should be restarted before declaring failure. Default is 3.

\code{trace} A logical variable indicating whether the starting parameter configuration is a regular simplex.  Default is FALSE.

}
\value{
A list with the following components:
\item{par}{Best estimate of the parameter vector found by the algorithm.}

\item{value}{The value of the objective function at termination.}

\item{feval}{The number of times the objective \code{fn} was evaluated.
}

\item{restarts}{The number of times the algorithm had to be restarted when it stagnated.
}

\item{convergence}{An integer code indicating type of convergence.  \code{0} indicates successful convergence. Positive integer codes indicate failure to converge.    
}

\item{message}{Text message indicating the type of convergence or failure.  
}
}
\references{
C.T. Kelley (1999), Iterative Methods for Optimization, SIAM.
}
\author{

Qiang Kou, Yann Richet, Ravi Varadhan
}
\note{
However, there is one point we want to emphasize, if the number of possible parralel evaluations is not enough high, the gain will be very thin, even negative, since the messaging process between cores costs much time. This is typically can be seen in Nelder Mead results.

Also, the gain in running time can be seen when the objective function is CPU-expensive. And in our test only when the evaluating time overruns 0.1 second, the vectorized or parallel is meaningful.
}



\seealso{
\code{\link[dfoptim]{nmkb}}, \code{\link[base]{apply}}, \code{\link[foreach]{foreach}}
}
\examples{

nmkb.vec(par=rep(3, 30), fn=rastr, lower=2, upper=4, vec="apply")

}

\keyword{ optimization }
\keyword{ foreach }
