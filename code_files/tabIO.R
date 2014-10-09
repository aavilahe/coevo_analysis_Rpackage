#R
# Reads an already formatted *.tab file
#   - tab separated
#   - first row is header
#   - first two columns are alignment column indices
#   - column names do not start with '[r|z]_'

loadTab = function(fn){
	# generic load tab
	tab = read.table(fn, sep='\t', header = TRUE)
	return(tab)
}

