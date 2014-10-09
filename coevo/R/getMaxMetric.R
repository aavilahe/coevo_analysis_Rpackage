getMaxMetric <-
function(pred, metric, column_names){
	# gets maximum cutoff-dependent metric (f, phi); also works with auc
	perf = performance(pred, metric)
    nmets = length(perf@y.values)
    mets = vector(mode = 'numeric', length = nmets)
    for(i in 1:nmets){
	    mets[i] = max(perf@y.values[[i]], na.rm=T)
    }
    names(mets) = column_names
	return(mets)
}
