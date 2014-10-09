getCutoffIndependentMetrics <-
function(pred, metric, column_names){
    # auc, aupr, (f, phi --> max f, max phi)
    if(metric == 'aupr'){
        return(getAUPR(pred, column_names))
    }
    return(getMaxMetric(pred, metric, column_names))
}
