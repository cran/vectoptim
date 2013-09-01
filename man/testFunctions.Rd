\name{testFunctions}
\alias{testFunctions}
\alias{ackley}
\alias{levy}
\alias{powell}
\alias{rastr}
\alias{rosenbrock}
\alias{schwef}

\concept{Testing functions for optimization}
\title{
  Classical Test Functions for Unconstrained Optimization
}
\description{
Some classical testing functions as benchmarks for optimization.
}
\usage{
ackley(xx, delay=0, a = 20, b = 0.2, c = 2 * pi)
levy(xx, delay=0)
rastr(xx, delay=0)
rosenbrock(xx, delay=0)
schwef(xx, delay=0)
}

\arguments{
  \item{xx}{
    a numeric vector of arguments
  }
  \item{delay}{
 the delay time for evaluating the testing function, it is used to simulate the function which is CPU-expensive.
}
  \item{a}{
    constant with default value 20
}
  \item{b}{
    constant with default value 0.2
}
  \item{c}{
    constant with default value 2*pi
}
}
\details{
  All functions take as argument only one variable, a numeric vector \code{x} whose length determines the dimensionality of the problem.

}
\value{
  The objective function evaluated at \code{x} (a numeric vector of length one).
}
\references{

  Derek Bingham \emph{Virtual Library of Simulation Experiments: Test Functions and Datasets}.
  \url{http://www.sfu.ca/~ssurjano/optimization.html}

}
\author{
  Qiang Kou, Derek Bingham
}


\seealso{
  \code{\link{DEopt.vec}}, \code{\link{PSopt.vec}}, \code{\link{GAopt.vec}}
}
\examples{
x<-rep(3,20)
rosenbrock(x, delay=0.1)
}
\keyword{optimize}
