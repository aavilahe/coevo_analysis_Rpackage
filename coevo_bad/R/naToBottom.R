naToBottom <-
function(x){
    xmin = min(x, na.rm = TRUE)
    xmax = max(x, na.rm = TRUE)
    xrange = xmax - xmin 
    nx = length(x)
    xbottom = xmin - ( xrange / nx )
	x[is.na(x)] = xbottom
	return(x)
}
