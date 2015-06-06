# P-value calculating functions
#
# These functions calculate p-values for coevolution
# scores assuming they come from the null distribution
# or a given theoretical distribution.
#   p_empirical
#   p_normal
#   p_gamma

#' P-values given standard normal null
#'
#' \code{p_normal} computes p-values for coevolution scores
#' assuming the null distribution is \eqn{N(\mu = 0, \sigma = 1)}
#'
#' @param x A numeric vector of finite valued coevolution scores
#' @return A numeric vector of p-values
#' @section Usage note:
#' \code{p_normal(x)} assumes high values are good scores
#' and \code{x} is finite valued (eg. no NAs, NaNs)
#'
#' @export
p_normal = function(x){
    z_x = scale(x)
    p_x = pnorm(z_x, lower.tail = FALSE)
    return(p_x)
}

#' P-values given empirical null
#'
#' \code{p_empirical} computes p-values for coevolution scores
#' assuming they are drawn from the null distribution.
#'
#' @param x A numeric vector of finite valued coevolution scores
#' @return A numeric vector of p-values
#' @section Usage note:
#' \code{p_empirical(x)} assumes high values are good scores
#' and \code{x} is finite valued (eg. no NAs, NaNs)
#'
#' @export
p_empirical = function(x){
    p_x = 1 - ecdf(x)(x)
    return(p_x)
}

#' Converts given scores in a data.table to p-values
#'
#' \code{calculate_p} converts given columns in a data.table to p-values
#' using a \link{\code{p_normal}}, \link{\code{p_empirical}}, or a
#' user-defined function.
#'
#' @param stab A table containing scores
#' @param pfunc An R function that calculates p-values from scores
#' @param prefix A character string prepended to score names used to name p-value columns
#' @return A data.table containing p-values for given scores from \code{stab}.
#'
#' @export
calculate_p = function(stab, pfunc, prefix){
    snames = colnames(stab)
    cleantab = clean_columns(stab)
    ptab = cleantab[, lapply(.SD, pfunc)]
    pnames = paste(prefix, snames, sep = '')
    setnames(ptab, snames, pnames)
    return(ptab)
}

#' P-values given specific null score distribution
#'
#' \code{p_null} returns a function for computing empirical p-values
#' given a specific null score distribution
#'
#' @param x A numeric vector of finite valued coevolution scores
#' @return A function for computing p-values
#' @section Usage note:
#' Assumes high values are good scores and \code{x} is finite valued (eg. no NAs, NaNs)
#'
#' @export
p_null = function(x){
    e = ecdf(x)
    f = function(y){
        p_x = 1 - e(y)
        return(p_x)
    }
    return(f)
}

