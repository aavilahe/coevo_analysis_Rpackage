#' @family calculate performance with ROCR package
#'

#' Useless wrapper for ROCR::prediction().
#'
#' Accepts a single named list of predictions and labels instead.
#'
#' @param pred_lab_list A list with named entries 'predictions' and 'labels'
#' @return A ROCR::prediction-class object
#' @seealso \link{\code{pred_lab_prep}}
#' @export
get_ROCR_prediction = function(pred_lab_list){
    predictions = pred_lab_list$predictions
    labels = pred_lab_list$labels
    pred = ROCR::prediction(predictions = predictions, labels = labels)
    return(pred)
}

#' Prepares score data.table and labels for \code{ROCR::prediction()}.
#'
#' Repeats labels for each column in scores data.table and removes NAs in
#' labels.
#'
#' @param cleantab A data.table that has been cleaned by \code{cleanColumns()}
#' @param the_labels A logical vector containing the classes for each row
#' @return A list containing prediction and label data.tables
#' @export
pred_lab_prep = function(cleantab, the_labels){
    nonas = !is.na(the_labels)

    the_labels = the_labels[nonas]
    cleantab = cleantab[nonas, ]

    nr = length(the_labels)
    nc = ncol(cleantab)

    the_labels = as.data.table(matrix(the_labels, nr, nc, byrow = FALSE))
    return(list('predictions' = cleantab, 'labels' = the_labels))
}


#' Get score cutoffs that control FPR below a given target rate.
#'
#' Uses ROCR to calculate FPR vs score cutoff curve, then chooses
#' largest score such that FPR is less than given target rate.
#'
#' @param pred A ROCR prediction object
#' @param target_FPR A target FPR
#' @return A numeric vector of score cutoffs ordered like ROCR prediction columns
#' @export
get_scores_at_FPR = function(pred, target_FPR){
    perf = ROCR::performance(pred, 'fpr')
    num_cutoffs = length(perf@y.values)
    cutoffs = vector(mode = 'numeric', length = num_cutoffs)
    for(i in 1:num_cutoffs){
        # get last x of (x,y) where y < target_FPR
        cutoffs[i] = tail(perf@x.values[[i]][perf@y.values[[i]] < target_FPR], 1)
    }
    return(cutoffs)
}

#' Get TPRs at given FPR.
#'
#' Uses ROCR to calculate TPR vs FPR curve, then chooses
#' largest TPR such that FPR is less than given target rate.
#'
#' @param pred A ROCR prediction object
#' @param target_FPR, A target FPR
#' @return A numeric vector of TPRs ordered like ROCR prediction columns
#' @export
get_TPRs_at_FPR = function(pred, target_FPR){
    perf = ROCR::performance(pred, 'tpr', 'fpr')
    num_tprs = length(perf@y.values)
    tprs = vector(mode = 'numeric', length = num_tprs)
    for(i in 1:num_tprs){
        # get last y of (x,y) where x < target_FPR
        tprs[i] = tail(perf@y.values[[i]][perf@x.values[[i]] < target_FPR], 1)
    }
    return(tprs)
}

#' Get nominal FPR at a given target FPR.
#'
#' Uses ROCR to calculate FPR vs score curve, then chooses largest
#' FPR < target FPR.
#'
#' @param pred A ROCR prediction object
#' @param target_FPR, A target FPR
#' @return A numeric vector of nominal FPRs ordered like ROCR prediction columns
#' @export
get_nomFPR_at_FPR = function(pred, target_FP){
    perf = ROCR::performance(pred, 'fpr')
    num_fprs = length(perf@y.values)
    fprs = vector(mode = 'numeric', length = num_fprs)
    for(i in 1:num_fprs){
        # get last y of (x,y) where y < target_FPR
        fprs[i] = tail(perf@y.values[[i]][perf@y.values[[i]] < target_FPR], 1)
    }
    return(fprs)
}

#' Get PPVs at given FPR.
#'
#' Uses ROCR to calculate PPV vs FPR curve, then chooses
#' largest PPV such that FPR is less than given target rate.
#'
#' @param pred A ROCR prediction object
#' @param target_FPR, A target FPR
#' @return A numeric vector of PPVs ordered like ROCR prediction columns
#' @export
get_PPV_at_FPR = function(pred, target_FPR){
    perf = ROCR::performance(pred, 'ppv', 'fpr')
    num_ppvs = length(perf@y.values)
    ppvs = vector(mode = 'numeric', length = num_ppvs)
    for(i in 1:num_ppvs){
        ppvs[i] = tail(perf@y.values[[i]][perf@x.values[[i]] < target_FPR], 1)
    }
    return(ppvs)
}

#' Get area under the precision-recall curve.
#'
#' Uses ROCR to calculate precision vs recall curve, then calculates area under it.
#'
#' @param pred A ROCR prediction object
#' @return A numeric vector of areas under prec-rec curve ordered like ROCR prediction columns
#' @export
get_auPR = function(pred){
	perf = ROCR::performance(pred, 'prec', 'rec')
    num_auprs = length(perf@y.values)
    auprs = vector(mode = 'numeric', length = num_auprs)
    for(i in 1:num_auprs){
	    perf.mat = na.omit(cbind(perf@x.values[[i]], perf@y.values[[i]]))
	    auprs[i] = pracma::trapz(perf.mat[,1], perf.mat[,2])
    }
	return(auprs)
}

#' Get maximum of cutoff-dependent metric.
#'
#' Calculates ROCR metric vs cutoff curve, and keeps maximum.
#'
#' @param pred A ROCR prediction object
#' @param metric A character string that is one of ROCR's cutoff-dependent performance metrics
#' @return A numeric vector of metrics ordered like ROCR prediction columns
#'
#' @section Misuse:
#' Can be misused to calculate auROC when \code{metric = 'auc'},
#' for example, \link{\code{get_cutoff_independent_metric}}.
#'
#' @examples
#' get_max_metric(pred = pred, metric = 'f')  # gets fmax
#' @export
get_max_metric = function(pred, metric){
	perf = ROCR::performance(pred, metric)
    num_mets = length(perf@y.values)
    mets = vector(mode = 'numeric', length = num_mets)
    for(i in 1:num_mets){
	    mets[i] = max(perf@y.values[[i]], na.rm=T)
    }
	return(mets)
}

#' Get cutoff-independent metric.
#'
#' Calculates cutoff-independent metric using ROCR, for example
#' \code{auc}, \code{aupr}, \code{mccmax}, \code{fmax}.
#'
#' @param pred A ROCR prediction object
#' @param metric a character string that is one of ROCR's performance metrics
#'
#' @examples
#' get_cutoff_independent_metric(pred = pred, metric = 'f')  # gets fmax
#' get_cutoff_independent_metric(pred = pred, metric = 'aupr')  # gets auPR
#' @export
get_cutoff_independent_metric = function(pred, metric){
    if(metric == 'aupr'){
        return(get_auPR(pred))
    }
    return(get_max_metric(pred, metric))
}
