getAUPR <-
function(pred, column_names){
	# computes area under precision recall curve for column_names
	perf = performance(pred, 'prec', 'rec')
    nauprs = length(perf@y.values)
    auprs = vector(mode = 'numeric', length = nauprs)
    for(i in 1:nauprs){
	    perf.mat = na.omit(cbind(perf@x.values[[i]], perf@y.values[[i]]))
	    auprs[i] = trapz(perf.mat[,1], perf.mat[,2])
    }
    names(auprs) = column_names
	return(auprs)
}
