#' @export MUPkL
#'
MUPkL <- function(A, P, U, n, k, sta){
  # A, P, U are square matrices
  # n ... see xn bottom, n >= k
  # xn ... vector of discret times, (steps of the process)
  # k ... number of singl steps of the process
  # sta ... vector whose values are indices of the rows of the matrix P
  nsta = length(sta)
  slp = length(P[1, ])
  if (k > 1) {
  	xn <- c(1:(k-1), k * 2^((1:(n-k+1)-1)))
  } else {
  	if (k == 0) {
  		xn <- 2^((1:n)-1)
  	} else { xn <- c(1:n)}
  }
  M = matrix(0, nrow=slp, ncol=slp)
  Mn = M
  W = array(0, c(length(xn), slp, length(sta)),
  					dimnames = list(xn, 1:slp, sta))
  Nn = Tn = W
############################
  I = diag(1, slp, slp)
  Q = P %*% U
  Qn = Q %*% A
  Qp = Q

##########################
  j = 1
  M = A
  iii = 0
  for (ii in sta) {
  	iii = iii + 1
  	W[j, , iii] = M[, ii]
  	if (j == 1) Nn[1, ,iii] = W[1, ,iii] else
  		Nn[j, , iii] = (W[j, , iii] - W[j - 1, , iii])/(xn[j] - xn[j - 1])
  	if (j == 1) Tn[1, , iii] = 1/Nn[1, , iii] else
  		Tn[j, , iii] = 1/Nn[j, , iii]
  }
#########################
  for (j in 2:length(xn)) {
  	if (j > 1) {
  		if (k == 1) {
  			Mn = M + Qn
  			Qp = Qp %*% Q
  			Qn = Qp %*% A
  			M = Mn
  		} else {
  			if (k == 0) {
  				Mn = (I + Q) %*% M
  				M = Mn
  				Q = Q %*% Q
  			} else {
  				# k > 1
  				if (j <= k) {
  					Mn = M + Qn
  					Qp = Qp %*% Q
  					Qn = Qp %*% A
  					M = Mn
  				} else {
  				#	if (ne) { Qn = Q %*% Qn; ne = FALSE }
  					Mn = M + Qp %*% M
  					Qp = Qp %*% Qp
  					M = Mn
  				}
  			}
  		}
  	}
  	iii = 0
  	for (ii in sta) {
  		iii = iii + 1
  		W[j, , iii] = M[, ii ]
			if (j == 1) Nn[1, ,iii] = W[1, ,iii] else
				Nn[j, , iii] = (W[j, , iii] - W[j - 1, , iii])/(xn[j] - xn[j - 1])
			if (j == 1) Tn[1, , iii] = 1/Nn[1, , iii] else
  					Tn[j, , iii] = 1/Nn[j, , iii]
  	}
  }
  return(list(N = W, Navg = Nn, Tavg = Tn, x = xn))
}
