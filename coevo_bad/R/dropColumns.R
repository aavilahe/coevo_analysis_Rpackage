dropColumns <-
function(tab, drop_these){
    return(tab[, colnames(tab) %ni% drop_these])
}
