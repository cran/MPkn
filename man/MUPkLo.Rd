\name{MUPkLo}
\alias{MUPkLo}
\title{
Calculations of one discrete model in several time steps
}
\usage{
MUPkLo(A, P, U, n, k, sta)
}
\arguments{
  \item{A}{an initial square matrix a process at time 0}
  \item{P}{a basic transition matrix chain}
  \item{U}{a correction matrix chain}
  \item{n}{
An integer vector cumulative number of individual process steps. \cr
\code{n[1] > 0}, \code{n[i] > n[i-1]}.
  }
  \item{k}{
A vector of 0 and 1 identifying the mode of calculation in
  the stretch step. \cr
  \code{k[i] = 1} for \code{rn[j] = rn[j-1]+1}, \cr
  \code{k[i] = 0} for \code{rn[j] = 2*rn[j-1]}, \cr
  where \code{rn[j]} is the j-th row name of the output value
  matrix.
  }
  \item{sta}{Vector of indices of the columns of the matrix
  \code{M}. The matrix \code{M} contains the cumulative number
  of inputs \code{mij} from the state of the \code{i}
  to the state \code{j}.}
}
\details{
Relationship between \code{k} and \code{n}: \cr
  \code{length(k)} \code{==} \code{lentgh(n)}. \cr
  It is recommended to determine the value of well vectors \code{n}
  and \code{k}.
}
\value{
An array (r x slp x sta) where \cr
  \tabular{rl}{
  \code{r} \tab \eqn{r = n[length(n)]} \cr
  \code{slp} \tab Vector of column indices of the matrix P \cr
  \code{sta} \tab Vector of column indices of the matrix M \cr
  }
	 Row of the output matrix (array) is the column in the matrix
	  \code{M} and whose number is specified in the \code{sta}.
  The matrix \code{M} contains the cumulative number of inputs
  \code{mij} from the state of the \code{i} to the state \code{j}.
}
\author{
Josef Brejcha
}
\description{
\eqn{M[i + 1] = (I + Q) * M[i]} process in several selected
  steps. \cr
  \eqn{Q = P * U}, matrix multiplication. \cr
  The calculation is performed in steps determined by integer \cr
  vectors \code{k} and \code{n}. The sections defined by integers \cr
  \code{k }and \code{n} are applied as follows: \cr
  \tabular{lll}{
  \code{k[i] == 1} \tab{\ldots \eqn{M[n] = sum(i=0,n-1)(Q^i)*A}}
    \tab{,  for \eqn{n = 0, 1, 2, ... }} \cr
  \code{k[i] == 0} \tab{\ldots \eqn{M[2n] = (I + Q^n)*M[n]}}
  \tab{, for \eqn{n = r*2^i,  i=1,2,3,..}}
  }
  where \code{r} is the last step before section with \code{k[i] == 0}
}
\examples{
A = array(c(-2, -3, 1, 4, -2, 1, 3, -1, -2), c(3, 3))
P <- array(c(0.9, 0.6, 0.8, 0.05, 0.2, 0.05, 0.05, 0.2, 0.15), c(3, 3))
U <- array(c(0.8, 0.8, 0.7, 0.06, 0.02, 0.2, 0.14, 0.18, 0.1), c(3, 3))
sta <- 3
Ao <- A
k <- c(1, 0, 1, 0)
n <- c(5, 7, 12, 17)
# Steps, in which will compute the value of the Mx:
# 1, 2, 3, 4, 5, 10, 20, 21,  22, 23, 24, 25, 50, 100, 200, 400, 800
Mx <- MUPkLo(A, P, U, n, k, sta)
print(Mx)
A <- Ao
Mb <- MUPkLo(A, P, U, n = 100, k = 1, sta)
Mb[100,,]
}
\keyword{MUPkLo}

