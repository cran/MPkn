\name{MPkn-package}
\alias{MPkn-package}
\docType{package}
\title{
Calculations of One Discrete Model in Several Time Steps
}
\details{
	\tabular{ll}{
		Package: \tab MPkn\cr
		Type: \tab Package\cr
		Version: \tab 0.1.0\cr
		Date: \tab 2018-05-03 \cr
		License: \tab GPL (>= 3)\cr
	}
}
\author{
Josef Brejcha

Maintainer: Josef Brejcha <brchjo@gmail.com>
}
\description{
A matrix discrete model having the form \code{M[i+1] = (I + Q)*M[i]}.
The calculation of the values of \code{M[i]} only for pre-selected values of \code{i}. The method of calculation is presented in the vignette 'Fundament' ('Base'). Maybe it`s own idea of the author of the package. A weakness is that the method gives information only in selected steps of the process. It mainly refers to cases with matrices that are not Markov chain. \cr
If \code{Q} is markov transition matrix, then \code{MUPkL} may be
used to calculate the steady-state distribution \code{p} for
\eqn{p = Q*p}. See example bottom. \cr
Matrix power of non integer (\code{matrix.powerni}) gives the same results as a \code{mpower} from package \code{matlib}.
}
\references{
Ton van den Boom, "Discrete-time systems analysis" (2006), Additional Lecture Notes for the course SC4090, \url{www.dcsc.tudelft.nl/~sc4060/transp/discreteNOTES.pdf} \cr
Richard Weber, "Markov Chains" (2011), \url{http://www.statslab.cam.ac.uk/~rrw1/markov/M.pdf} \cr
"Examples of Markov chains", \url{https://en.wikipedia.org/wiki/Examples_of_Markov_chains} \cr
"Markov chains", \url{https://en.wikipedia.org/wiki/Markov_chain#Expected_number_of_visits} \cr
Donald R. Burleson, Ph.D.
"ON NON-INTEGER POWERS OF A SQUARE MATRIX", (2005),
\url{http://www.blackmesapress.com/Eigenvalues.htm}
}
\examples{
require(MPkn)
require(markovchain)
options(digits = 14)
n = 12
k = 2
rz = 11
P = array(0, c(rz, rz))
for (i in 1:rz){
  po = runif(rz)
  P[i, ] = po/sum(po)
}
I = diag(1, rz, rz)
Myy = MUPkL(P, P, I, n, k, c(1:rz))
StSy = NULL
for (i in 1:rz) StSy = c(StSy, Myy$Navg[,,i][n])
mrkv = new("markovchain", transitionMatrix = P)
StSx = steadyStates(mrkv)
print("MPkn");  print(StSy)
print("markovchain"); print(StSx)

}
