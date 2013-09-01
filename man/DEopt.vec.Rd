\name{DEopt.vec}
\alias{DEopt.vec}
\title{
Optimization with Differential Evolution
}
\description{
The function implements the standard Differential Evolution algorithm and vectorized by foreach
}
\usage{
DEopt.vec(fun, algo = list(), vec = FALSE, core = 2, ...)
}
\arguments{
  \item{fun}{
The objective function, to be minimized.
}
  \item{algo}{
A list with the settings for algorithm.
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
The function implements the standard Differential Evolution (no jittering or other features). Differential Evolution (\acronym{DE}) is a population-based optimisation heuristic proposed by Storn and Price (1997). \acronym{DE} evolves several solutions over a number of iterations. In a given generation, new solutions are   created and evaluated; better solutions replace inferior ones in the population. Finally, the best solution of the population is returned. See the references for more details on the mechanisms.

This function is modified from \code{\link[NMOF]{DEopt}}, and we add some vectorized options. The default one is "FALSE", so there is no difference from the original one; "apply" means using the \code{\link[base]{apply}} to vectorize; "foreach" uses \code{\link[foreach]{foreach}}. And if the function has been vectorized, you can use "fun" option.

}
\value{
A list:
  \item{\code{par}}{the solution (the best member of the population), which is
    a numeric vector}
  \item{\code{value}}{objective function value of best solution}
  \item{\code{popF}}{a vector. The objective function values in the final population.}
  \item{\code{Fmat}}{if \code{algo$storeF} is \code{TRUE}, a matrix of size
    \code{algo$nG} times \code{algo$nP} containing the objective function
    values of all solutions over the generations; else \code{NA}.}
  \item{\code{xlist}}{if \code{algo$storeSolutions} is \code{TRUE}, a list that
    contains a list \code{P} of matrices; else \code{NA}.}
  \item{\code{initial.state}}{the value of \code{\link{.Random.seed}}
    when the function was called.}
}
\references{
Gilli, M., Maringer, D. and Schumann, E. (2011) \emph{Numerical Methods and Optimization in Finance}. Elsevier.
}
\author{
Qiang Kou, Yann Richet, Enrico Schumann
}

\note{
However, there is one point we want to emphasize, if the number of possible parralel evaluations is not enough high, the gain will be very thin, even negative, since the messaging process between cores costs much time. This is typically can be seen in Nelder Mead results.

Also, the gain in running time can be seen when the objective function is CPU-expensive. And in our test only when the evaluating time overruns 0.1 second, the vectorized or parallel is meaningful.
}


\seealso{
\code{\link[NMOF]{DEopt}}, \code{\link[base]{apply}}, \code{\link[foreach]{foreach}}
}
\examples{
algo <- list(nP = 30L,				### population size
				nG = 300L,			### number of generations
				F = 0.6,			### step size
				CR = 0.9,			### prob of crossover
				min = c(-2, 20),	### range for initial population
				max = c( 4, 20))
set.seed(123)
sol <- DEopt.vec(fun = rosenbrock, algo = algo, vec = "foreach")

}

\keyword{ optimization }
\keyword{ foreach }
