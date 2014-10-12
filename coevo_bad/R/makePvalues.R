makePvalues <-
function(xtab, xnames, Ptype, prefix){
    cleantab = cleanColumns(xtab[, xnames], xnames)
    ptab = colwise(Ptype)(cleantab)
    pnames = paste(prefix, xnames, sep = '')
    colnames(ptab) = pnames
    return(ptab)
}
