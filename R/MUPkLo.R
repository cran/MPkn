#' @export MUPkLo
MUPkLo <- function(A, P, U, n, k, sta){
	nsta = length(sta)
	slp = length(P[1, ])
	poc = TRUE
	xnb = 0
	xno = 1
	xn = NULL
	for (ik in 1:length(n)) {
#		print(paste(ik, k[ik], n[ik], xno))
		if (ik == 1) {
			if (k[ik] == 0) {
				xny <- 2^((1:n[ik]) - 1)
			} else {
				xny <- c(1:n[ik])
			}
		} else {
			if (k[ik] == 0) {
			xny <- 2*xno * 2^(1:(n[ik] - n[ik - 1]) - 1)
		} else {
			xny <- xnb + c(1:(n[ik] - n[ik - 1]))
		}}
	xn = c(xn, xny )
	nxn = length(xn)
	xnb = xn[nxn]
	xno = xnb
	}
#	print(xn)
	M = matrix(0, nrow=slp, ncol=slp)
	Mn = M
	W = array(0, c(length(xn), slp, length(sta)),
						dimnames = list(xn, 1:slp, sta))
	############################
	I = diag(1, slp, slp)
	Q = P %*% U
	Qn = Q %*% A
	Qp = Q
	##########################
	for (ijk in 1:length(n)) {
	if (poc) { nijk = 1; poc = FALSE } else nijk = n[ijk - 1] + 1
	for (j in nijk:n[ijk]) {
		if (j == 1) M = A
	 	#########################
			if (j > 1) {
				if (k[ijk] == 1 & j <= n[ijk]) {
					Mn = M + Qn
					Qp = Qp %*% Q
					Qn = Qp %*% A
					M = Mn
				} else {
					if (k[ijk] == 0 & j <= n[ijk]) {
						Mn = (I + Qp) %*% M
						M = Mn
						Qp = Qp %*% Qp
						Qn = Qp %*% A
					}
				}
			}
			iii = 0
			for (ii in sta) {
				iii = iii + 1
				W[j, , iii] = M[, ii]
			}
		}
	}
	return(W)
}
