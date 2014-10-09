removeAllNAColumns <-
function(tab){
    numRows = nrow(tab)
    keepThese = (colSums(is.na(tab)) < numRows)
    return(tab[, keepThese])
}
