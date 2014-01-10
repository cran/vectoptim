\name{PSopt.vec}
\alias{PSopt.vec}

\title{
Particle Swarm Optimisation
}
\description{
The function implements Particle Swarm Optimisation and vectorized using foreach.
}
\usage{
PSopt.vec(fun, algo = list(), vec = FALSE, core = 2, ...)
}

\arguments{
  \item{fun}{
the objective function to be minimised. See Details.
}
  \item{algo}{
a list with the settings for algorithm. See Details and Examples.
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
The function implements Particle Swarm Optimisation (\acronym{PS}); see the references for details on the implementation. \acronym{PS} is a population-based optimisation heuristic. It develops several solutions over a number of iterations. \acronym{PS} is directly applicable to continuous problems since the population is stored in real-valued vectors. In each iteration, a solution is updated by adding another vector called velocity. The algorithm stops after a fixed number of iterations.

This function is modified from \code{\link[NMOF]{PSopt}}, and we add some vectorized options. The default one is "FALSE", so there is no difference from the original one; "apply" means using the \code{\link[base]{apply}} to vectorize; "foreach" uses \code{\link[foreach]{foreach}}. And if the function has been vectorized, you can use "fun" option.

}
\value{
  Returns a list:
  \item{par}{the solution}
  \item{value}{objective function value of best solution}
  \item{popF}{a vector: the objective function values in the final population}
  \item{Fmat}{if \code{algo$storeF} is \code{TRUE}, a matrix of size \code{algo$nG}
    times \code{algo$nP}. Each column contains the best objective function value found by the particular solution.}
  \item{xlist}{if \code{algo$storeSolutions} is \code{TRUE}, a list that contains two
    lists \code{P} and \code{Pbest} of matrices; else \code{NA}.
  }
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
\code{\link[NMOF]{PSopt}}, \code{\link[base]{apply}}, \code{\link[foreach]{foreach}}
}

\examples{
algo <- list(min = rep(-2, 20), max = rep(4, 20),
             c1 = 1.0, c2 = 2.0,
             iner = 0.7, initV = 1, maxV = 3, 
             nP = 20L, nG = 300L, printBar = FALSE)
set.seed(123)
sol <- PSopt.vec(rosenbrock, algo = algo, vec="apply")

}

\keyword{ optimization }
\keyword{ foreach }
