\name{MPKlMatrix}
\alias{MPKlMatrix}
\title{
Creates a matrix of specified row of output \code{\link{MUPkLo}}
}
\usage{
MPKlMatrix(Mx, step, nc, sta)
}
\arguments{
  \item{Mx}{
output matrix of \code{\link{MUPkLo}}
}
  \item{step}{
row name of matrix \code{Mx}
}
  \item{nc}{
number of columns of matrix \code{Mx}
}
  \item{sta}{
vector with column indices of input matrices into \code{\link{MUPkLo}}
}
}
\value{
The matrix with \code{nc} rows and columnes.
}
\author{
Josef Brejcha
}
\description{
Specified row of output \code{\link{MUPkLo}} is a number step of
  process which computes \code{\link{MUPkLo}} function.
}
\examples{
A <- array(c(0.9, 0.6, 0.8, 0.05, 0.2, 0.05, 0.05, 0.2, 0.15), c(3, 3))
P <- array(c(0.9, 0.6, 0.8, 0.05, 0.2, 0.05, 0.05, 0.2, 0.15), c(3, 3))
U <- array(c(0.8, 0.8, 0.7, 0.06, 0.02, 0.2, 0.14, 0.18, 0.1), c(3, 3))
sta <- c(1, 2, 3)
k <- c(1, 0, 1, 0)
n <- c(5, 7, 12, 17)
Mx <- MUPkLo(A, P, U, n, k, sta)
M100 = MPKlMatrix(Mx, step = 100, nc = 3, sta = c(1, 2, 3))
}
\keyword{MPKlMatrix}

