loadTab <-
function(fn){
	# generic load tab
	tab = read.table(fn, sep='\t', header = TRUE)
	return(tab)
}
