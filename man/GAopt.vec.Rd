\name{GAopt.vec}
\alias{GAopt.vec}

\title{
Optimization with a Genetic Algorithm
}
\description{
A simple Genetic Algorithm for minimizing a function and vectorized by foreach.
}
\usage{
GAopt.vec(fun, algo = list(), vec = FALSE, core=2, ...)
}

\arguments{
  \item{fun}{
The objective function, to be minimised. See Details.
}
  \item{algo}{
A list with the settings for algorithm. See Details and Examples.
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
The function implements a simple Genetic Algorithm (\acronym{GA}). A \acronym{GA} evolves a collection of solutions (the so-called population), all of which are coded as vectors containing only zeros and ones. The algorithm starts with randomly-chosen or user-supplied population and aims to iteratively improve this population by mixing solutions and by switching single bits in solutions, both at random. In each iteration, such randomly-changed solutions are compared with the original population and better solutions replace inferior ones. In this function, the population size is kept constant.

This function is modified from \code{\link[NMOF]{GAopt}}, and we add some vectorized options. The default one is "FALSE", so there is no difference from the original one; "apply" means using the \code{\link[base]{apply}} to vectorize; "foreach" uses \code{\link[foreach]{foreach}}. And if the function has been vectorized, you can use "fun" option.

}
\value{
  A list:
  \item{par}{the solution (the best member of the population)}
  \item{value}{objective function value of best solution}
  \item{popF}{a vector. The objective function values in the final population.}
  \item{Fmat}{if \code{algo$storeF} is \code{TRUE}, a matrix of size
    \code{algo$nG} times \code{algo$nP} containing 
    the objective function values of all solutions over the generations;
    else \code{NA}}
  \item{xlist}{if \code{algo$storeSolutions} is \code{TRUE}, a list that
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
\code{\link[NMOF]{GAopt}}, \code{\link[base]{apply}}, \code{\link[foreach]{foreach}}
}
\examples{

algo <- list(nB = 20, 
			 nP = 20L, 
			 nG = 100L, 
			 prob = 0.002, 
			 printBar = FALSE
			 )
set.seed(123)
sol.foreach <- GAopt.vec(rosenbrock, algo = algo, vec = "foreach")


}

\keyword{ optimization }
\keyword{ foreach }
