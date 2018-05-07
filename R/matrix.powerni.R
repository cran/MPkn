#' @export matrix.powerni
#'
matrix.powerni <- function(A, p){
	G = eigen(A)
	# G$values = rep(0, length(G$values))
	# print(G)
	if (all(G$values >= 0) | is.integer(p)){
		h = as.vector(G$values)
		C = solve(a = t(G$vectors), b = h^p * t(G$vectors))
		return(C)
	} else {
		return(paste("G = eigen(A):",
"all(G$values) >= 0 == FALSE | (all(G$values) >= 0 & !is.integer(p))"))
	}
}
