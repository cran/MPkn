#' @export MPKlMatrix
#'
MPKlMatrix <- function(Mx, step, nc, sta)
{
	M <- matrix(NA, nrow = nc, ncol = nc)
	k = which(step == rownames(Mx))
	for (i in sta) {
		M[, i] = Mx[k, , i]
	}
	return(M)
}
