#R
# Calculate theoretical and empirical pvalues 
#   - Pempirical
#   - Pnormal
#   - #Pgamma

require(plyr)

Pnormal = function(x){
    # assumes high values are good scores
    # and finite values
    z_x = scale(x)
    p_x = pnorm(z_x, lower.tail = FALSE)
    return(p_x)
}

Pempirical = function(x){
    # assumes high values are good scores
    # and finite values
    p_x = 1 - ecdf(x)(x)
    return(p_x)
}

makePvalues = function(xtab, xnames, Ptype, prefix){
    cleantab = cleanColumns(xtab[, xnames], xnames)
    ptab = colwise(Ptype)(cleantab)
    pnames = paste(prefix, xnames, sep = '')
    colnames(ptab) = pnames
    return(ptab)
}






    
