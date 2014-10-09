Pempirical <-
function(x){
    # assumes high values are good scores
    # and finite values
    p_x = 1 - ecdf(x)(x)
    return(p_x)
}
