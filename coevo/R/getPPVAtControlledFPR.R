getPPVAtControlledFPR <-
function(pred, targetFPR, column_names){
    perf = performance(pred, 'prec', 'fpr')
    nprecs = length(perf@y.values)
    precs = vector(mode = 'numeric', length = nprecs)
    for(i in 1:nprecs){
        precs[i] = tail(perf@y.values[[i]][perf@x.values[[i]] < targetFPR], 1)
    }
    names(precs) = column_names
    return(precs)
}
