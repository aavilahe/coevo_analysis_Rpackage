getTPRAtControlledFPR <-
function(pred, targetFPR, column_names){
    perf = performance(pred, 'tpr', 'fpr')
    ntprs = length(perf@y.values)
    tprs = vector(mode = 'numeric', length = ntprs)
    for(i in 1:ntprs){
        tprs[i] = tail(perf@y.values[[i]][perf@x.values[[i]] < targetFPR], 1)
    }
    names(tprs) = column_names
    return(tprs)
}
