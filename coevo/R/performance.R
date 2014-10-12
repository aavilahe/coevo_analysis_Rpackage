#R
# calculate performance with ROCR package

getROCRPredObj = function(cleantab, the_labels){
    # cleantab: data.frame containing predictions
    # the_labels: vector containing classes (TRUE/FALSE) (eg. data.frame(Labels = c(T, F, T, F)))
    the_labels = data.frame(matrix(the_labels, length(the_labels), ncol(cleantab), byrow = FALSE))
    pred = ROCR::prediction(predictions = cleantab, labels = the_labels)
    return(pred)
}

#' cutoffs that control fpr
getCutoffsThatControlFPR = function(pred, targetFPR, column_names){
    perf = ROCR::performance(pred, 'fpr')
    ncutoffs = length(perf@y.values)
    cutoffs = vector(mode = 'numeric', length = ncutoffs)
    for(i in 1:ncutoffs){
        cutoffs[i] = tail(perf@x.values[[i]][perf@y.values[[i]] < targetFPR], 1)
    }
    names(cutoffs) = column_names
    return(cutoffs)
}

getTPRAtControlledFPR = function(pred, targetFPR, column_names){
    perf = ROCR::performance(pred, 'tpr', 'fpr')
    ntprs = length(perf@y.values)
    tprs = vector(mode = 'numeric', length = ntprs)
    for(i in 1:ntprs){
        tprs[i] = tail(perf@y.values[[i]][perf@x.values[[i]] < targetFPR], 1)
    }
    names(tprs) = column_names
    return(tprs)
}

getFPRAtControlledFPR = function(pred, targetFPR, column_names){
    perf = ROCR::performance(pred, 'fpr')
    nfprs = length(perf@y.values)
    fprs = vector(mode = 'numeric', length = nfprs)
    for(i in 1:nfprs){
        fprs[i] = tail(perf@y.values[[i]][perf@y.values[[i]] < targetFPR], 1)
    }
    names(fprs) = column_names
    return(fprs)
}

getPPVAtControlledFPR = function(pred, targetFPR, column_names){
    perf = ROCR::performance(pred, 'prec', 'fpr')
    nprecs = length(perf@y.values)
    precs = vector(mode = 'numeric', length = nprecs)
    for(i in 1:nprecs){
        precs[i] = tail(perf@y.values[[i]][perf@x.values[[i]] < targetFPR], 1)
    }
    names(precs) = column_names
    return(precs)
}

getAUPR = function(pred, column_names){
	# computes area under precision recall curve for column_names
	perf = ROCR::performance(pred, 'prec', 'rec')
    nauprs = length(perf@y.values)
    auprs = vector(mode = 'numeric', length = nauprs)
    for(i in 1:nauprs){
	    perf.mat = na.omit(cbind(perf@x.values[[i]], perf@y.values[[i]]))
	    auprs[i] = pracma::trapz(perf.mat[,1], perf.mat[,2])
    }
    names(auprs) = column_names
	return(auprs)
}

getMaxMetric = function(pred, metric, column_names){
	# gets maximum cutoff-dependent metric (f, phi); also works with auc
	perf = ROCR::performance(pred, metric)
    nmets = length(perf@y.values)
    mets = vector(mode = 'numeric', length = nmets)
    for(i in 1:nmets){
	    mets[i] = max(perf@y.values[[i]], na.rm=T)
    }
    names(mets) = column_names
	return(mets)
}

getCutoffIndependentMetrics = function(pred, metric, column_names){
    # auc, aupr, (f, phi --> max f, max phi)
    if(metric == 'aupr'){
        return(getAUPR(pred, column_names))
    }
    return(getMaxMetric(pred, metric, column_names))
}
