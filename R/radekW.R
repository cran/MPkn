#' @export radekW
#'
radekW <- function(n, k){
	nvc = length(n)
	rdk = integer(n[nvc])
	rdx = 0
	if (k[1] == 1) rdk[1:n[1]] = 1:n[1] else {
		for (j in 1:n[1]) rdk[j] = 2^(j - 1)
	}
	rdx = rdk[n[1]]
	if (nvc > 1){
		for (i in 2:nvc){
			# print(paste(i, nvc)); print(rdk)
			if (i < nvc){
				if (k[i] == 1){
					for (j in (n[i-1]+1):n[i]){
						rdx = rdx + 1
						rdk[j] = rdx
					}
				} else {
					for (j in (n[i-1]+1):n[i]){
					  rdx = 2*rdx
					  rdk[j] = rdx
					}
				}
			} else {
				if (k[i] == 1){
					for (j in (n[i-1]+1):n[i]){
						rdx = rdx + 1
						rdk[j] = rdx
					}
				} else {
					for (j in (n[i-1]+1):n[i]){
						# print(paste(i, j))
						rdx = 2*rdx
						rdk[j] = rdx
					}
				}
			}
		}
	}
	return(as.matrix(rdk))
}
