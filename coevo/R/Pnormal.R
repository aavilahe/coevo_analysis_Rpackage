Pnormal <-
function(x){
    # assumes high values are good scores
    # and finite values
    z_x = scale(x)
    p_x = pnorm(z_x, lower.tail = FALSE)
    return(p_x)
}
