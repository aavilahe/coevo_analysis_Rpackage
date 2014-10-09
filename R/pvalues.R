#R
# Calculate theoretical and empirical pvalues 
#   - Pempirical
#   - Pnormal
#   - #Pgamma

Pnormal = function(x){
    # assumes high values are good scores
    # and finite values
    z_x = scale(x)
    p_x = pnorm(zscores, lower.tail = FALSE)
    return(p_x)
}

Pempirical = function(x){
    # assumes high values are good scores
    # and finite values
    p_x = 1 - ecdf(x)(x)
    return(p_x)
}





    
