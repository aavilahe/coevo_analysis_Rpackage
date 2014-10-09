getCutoffsThatControlFPR <-
function(pred, targetFPR, column_names){
    perf = performance(pred, 'fpr')
    ncutoffs = length(perf@y.values)
    cutoffs = vector(mode = 'numeric', length = ncutoffs)
    for(i in 1:ncutoffs){
        cutoffs[i] = tail(perf@x.values[[i]][perf@y.values[[i]] < targetFPR], 1)
    }
    names(cutoffs) = column_names
    return(cutoffs)
}
