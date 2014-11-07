# Convenience functions used in scripts
#'
#'

#' Loads tab and removes problematic rows and columns.
#'
#' Loads tab and removes problematic rows and columns.
#'
#' @param fn A filename.
#' @param essential_names A character vector of columns where no NAs are allowed.
#' @return A santized tab ready to work with.
#'
#' @export
load_sanitized = function(fn, essential_names = character(0)){
    tab = load_tab(fn)
    tab = drop_all_NA_columns(tab)
    tab = drop_NA_rows_in_columns(tab, essential_names)
    return(tab)
}

#' Gets ROCR object from predictions and labels
#'
#' Another useless wrapper
#'
#' @param cleantab A data.table that has been cleaned by \code{cleanColumns()}
#' @param the_labels A logical vector containing the classes for each row
#'
#' @return A ROCR::prediction-class object
#'
#' @export
ROCR_pred = function(cleantab, the_labels){
    return(get_ROCR_prediction(pred_lab_prep(cleantab, the_labels)))
}




