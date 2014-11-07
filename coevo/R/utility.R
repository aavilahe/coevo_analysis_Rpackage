# score data.table utilities
#
# These utility functions help clean up score tabs (data.tables)
#      is_flip
#      flip
#      drop_all_NA_columns
#      drop_columns
#      drop_NA_rows_in_columns
#      na_to_bottom
#      clean_columns


#' Checks if columns should be \code{flip()}ed
#'
#' Checks column names against a hardcoded list of distances, dissimilarities,
#' and p-value prefixes where small small numbers are associated with
#' the positive class in a prediction problem.
#'
#' hack for now. figure out how to configure flip-logic later
#'
#' @param column_names A character vector of column names.
#' @return A logical vector indicating if \code{flip()} is required.
#'
#' @export
is_flip = function(column_names){
    pnames = grepl("^[rz]_", column_names)
    vis = grepl("^VI", column_names)
    comapPs = grepl("^CoMapP", column_names)
    flip_these = pnames | vis | comapPs
    return(flip_these)
}

#' Makes small scores big and vice versa
#'
#' Used before using distances or p-values to predict
#' the positive class with ROCR.
#' (eg. small information distances predict contancts)
#'
#' @param x A numeric vector
#' @return \code{-x}
#'
#' @export
flip = function(x){
    return(-x)
}

#' Drops columns that are all NA
#'
#' @param tab A data.table
#' @return A data.table with all-NA columns dropped
#'
#' @export
drop_all_NA_columns = function(tab){
    numRows = nrow(tab)
    keepThese = (colSums(is.na(tab)) < numRows)
    return(tab[, keepThese, with = FALSE])
}

#' Drops specified columns
#'
#' @param tab A data.table
#' @param drop_these A character vector of columns to drop
#' @return A data.table with columns dropped
#'
#' @export
drop_columns = function(tab, drop_these){
    #return(tab[, names(tab) %ni% drop_these, with = FALSE])
    return(tab[, !drop_these, with = FALSE])
}

#' Drops rows with NAs in given columns
#'
#' Drops rows with NAs in existing given columns.
#'
#' @param tab A data.table
#' @param column_names A character vector of column names to check for NAs
#' @return A data.table with rows dropped
#'
#' @export
drop_NA_rows_in_columns = function(tab, column_names){
    tnames = colnames(tab)
    column_names = column_names[column_names %in% tnames]
    if(length(column_names) == 0){
        return(tab)
    }
    return(tab[complete.cases(tab[, column_names, with = FALSE])])
}

#' Sets NA scores to lowest value
#'
#' Sets NA to a \code{xbottom}, a value lower than
#' \code{min(x, na.rm = TRUE)}
#' @param x A numeric vector
#' @return A numeric vector with NA values last
#' 
#' @export
na_to_bottom = function(x){
    xmin = min(x, na.rm = TRUE)
    xmax = max(x, na.rm = TRUE)
    xrange = xmax - xmin 
    nx = length(x)
    xbottom = xmin - (xrange / nx)
	x[is.na(x)] = xbottom
	return(x)
}

#' Flip and handle NAs in data.table columns
#'
#' \code{clean_columns} checks columns by column name
#' to determine if flipping big and small values is
#' required, then \code{flip()}s those columns.
#'
#' Typically used before calculating performance with ROCR
#' and before computing p-values.
#'
#' @param tab A data.table with columns to clean
#' @return A data.table with finite valued, ascending score columns
#' @seealso \link{\code{flip()}} and \link{\code{na_to_bottom()}}
#'
#' @export
clean_columns = function(tab){
    flip_these = which(is_flip(colnames(tab)))
    flipped = data.table::copy(tab)
    flipped[, (flip_these) := lapply(.SD, flip), .SDcols = flip_these]
    cleaned = flipped[, lapply(.SD, na_to_bottom)]
    return(cleaned)
}

