#' @family p-value calculating functions
#'
#' These functions calculate p-values for coevolution
#' scores assuming they come from the null distribution
#' or a given theoretical distribution.
#' @seealso \itemize{
#'      \item \code{\link{Pempirical}}
#'      \item \code{\link{Pnormal}}
#'      \item \code{\link{Pgamma}}
#' }

#' P-values given standard normal null
#'
#' \code{Pnormal} computes p-values for coevolution scores
#' assuming the null distribution is \code{N(\mu = 0,\sigma = 1)}
#'
#' @param x A numeric vector of finite valued coevolution scores
#' @return A numeric vector of p-values
#' @section Usage note:
#' \code{Pnormal(x)} assumes high values are good scores
#' and \code{x} is finite valued (eg. no NAs, NaNs)
Pnormal = function(x){
    z_x = scale(x)
    p_x = pnorm(z_x, lower.tail = FALSE)
    return(p_x)
}

#' P-values given empirical null
#'
#' \code{Pempirical} computes p-values for coevolution scores
#' assuming they are drawn from the null distribution.
#'
#' @param x A numeric vector of finite valued coevolution scores
#' @return A numeric vector of p-values
#' @section Usage note:
#' \code{Pempirical(x)} assumes high values are good scores
#' and \code{x} is finite valued (eg. no NAs, NaNs)
Pempirical = function(x){
    p_x = 1 - ecdf(x)(x)
    return(p_x)
}

#' Converts given scores in a data.table to p-values
#'
#' \code{calculatePvalues} converts given columns in a data.table to p-values
#' using a \link{\code{Pnormal}}, \link{\code{Pempirical}}, or a
#' user-defined function.
#'
#' @param stab A table containing scores
#' @param pfunc An R function that calculates p-values from scores
#' @param prefix A character string prepended to score names used to name p-value columns
#' @return A data.table containing p-values for given scores from \code{stab}.
calculatePvalues = function(stab, pfunc, prefix){
    snames = colnames(stab)
    cleantab = cleanColumns(stab)
    ptab = cleantab[, lapply(.SD, pfunc)]
    pnames = paste(prefix, snames, sep = '')
    setnames(ptab, snames, pnames)
    return(ptab)
}






    
