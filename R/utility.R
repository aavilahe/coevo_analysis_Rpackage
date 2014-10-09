#R
# Utility functions for scores data frames
#   - flipping
#   - 

# hack for now. figure out how to configure flip-logic later
flipRequired = function(column_name){
    prefix = substr(column_name, 1, 2)
    if(prefix == 'r_' | prefix == 'z_'){
        return(TRUE)
    }
    if(column_name %in% c('VI')){
       return(TRUE)
    }
    return(FALSE)
}

flip = function(x){
    return(-x)
}

removeAllNAColumns = function(tab){
    numRows = nrow(tab)
    keepThese = (colSums(is.na(tab)) < numRows)
    return(tab[, keepThese])
}

dropColumns = function(tab, drop_these){
    return(tab[, colnames(tab) %ni% drop_these])
}

naToBottom = function(x){
    xmin = min(x, na.rm = TRUE)
    xmax = max(x, na.rm = TRUE)
    xrange = xmax - xmin 
    nx = length(x)
    xbottom = xmin - ( xrange / nx )
	x[is.na(x)] = xbottom
	return(x)
}

cleanColumns = function(tab, column_names){
    for(column_name in column_names){
        tmpCol = tab[ , column_name]
        if(flipRequired(column_name)){
            tmpCol = flip(tmpCol)
        }
        tab[, column_name] = naToBottom(tmpCol)
    }
    return(tab)
}







