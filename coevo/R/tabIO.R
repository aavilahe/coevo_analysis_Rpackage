#R
# Reads an already formatted *.tab file
#   - tab separated
#   - first row is header
#   - first two columns are alignment column indices
#   - column names do not start with '[r|z]_'

#requires(data.table)

loadTab = function(fn){
	# generic load tab
    tab = data.table::fread(fn, header = TRUE, sep = '\t')
    tab = data.frame(tab)
	return(tab)
}

