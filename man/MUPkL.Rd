\name{MUPkL}
\alias{MUPkL}
\title{
Calculations of one discrete model in several time steps
}
\usage{
MUPkL(A, P, U, n, k, sta)
}
\arguments{
  \item{A}{starting square matrix a process at time 0}
  \item{P}{basic transition matrix chain}
  \item{U}{correction matrix chain}
  \item{n}{The number of steps. The length of the steps depends on the value of \code{k}.}
  \item{k}{\code{k == 0} ... step length \code{i} is equal to \eqn{2^(i-1), i = 1, 2, .. , n}. \cr
  \code{k == 1} ... step length \code{i} is equal to 1. \cr
  \code{k > 1} ... The first \code{n} steps has a length equal to 1. Other then have a length of twice the previous step.}
  \item{sta}{Vector whose values are the indices of the columns
  of the \code{A} matrix.}
}
\details{
Both \code{n} and \code{k} are single positive integers.
}
\value{
A list with following components: \cr
  \tabular{rl}{
  \code{N} \tab{sum values of entries into state} \cr
  \code{Navg} \tab{average N in interval \code{(i - 1, i]}} \cr
  \code{Tavg} \tab{\eqn{1/Navg}} \cr
  \code{x} \tab{steps vector}
  }
}
\author{
Josef Brejcha
}
\description{
\code{M[i + 1] = (I + Q)} \code{*} \code{M[i]} process in several selected steps.\cr
  \eqn{Q = P * U}, matrix multiplication. \cr
  Computation process only in the following steps \code{i}: \cr
  \tabular{r}{
  \eqn{c(1:k, k * 2^(1:(n-k)))} where \code{k > 1}; \cr
  \eqn{c(2^((1:n)-1))} for \code{k == 0}; \cr
  \eqn{seq(1, n, 1)} for \code{k == 1}.
  }
  \eqn{M[2*i] = (I + Q^i) * M[i]} for \code{k == 0}.
}
\examples{
A <- array(c(2, 3, 1, 4, 2, 1, 3, 1, 2), c(3, 3))
P <- array(c(0.9, 0.6, 0.8, 0.05, 0.2, 0.05, 0.05, 0.2, 0.15),
					 c(3, 3))
U <- array(c(0.8, 0.8, 0.7, 0.06, 0.02, 0.2, 0.14, 0.18, 0.1),
					 c(3, 3))
sta <- c(1, 3)
k <- 3
n <- 8
M33 <- MUPkL(A, P, U, n, k, sta)
print(M33$N)
k <- 1
n <- 24
M11 <- MUPkL(A, P, U, n, k, sta)
print(M11$N)
k <- 0
n <- 6
M00 <- MUPkL(A, P, U, n, k, sta)
print(M00$N)
}
\keyword{MUPkL}

