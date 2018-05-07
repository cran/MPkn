\name{radekW}
\alias{radekW}
\title{
The Numbers of Rows of the Output Matrix
}
\usage{
radekW(n, k)
}
\arguments{
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
}
\value{
Matrix size \code{n[length(n)] x 1}. \cr
The values of the rows of the matrix are the numbers of steps of the chain.
}
\author{
Josef Brejcha
}
\description{
The numbers of rows of the output matrix. These numbers are
determined by the vectors of \code{n} and \code{k}.
}
\examples{
  radekW(n = c(3, 5, 8, 9, 11), k = c(1, 0, 1, 0, 0))
}
\keyword{radekW}

