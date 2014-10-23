#' @family tab loading functions
#' @seealso \code{\link{loadTab}}

#' Reads a *.tab formatted file and returns a data.table.
#'
#' @param fn A filename to read.
#' @return A data.table or NULL.
#' @examples
#' loadTab('results.tab')
#' list_of_tables = lapply(list_of_filenames, loadTab)

#' @section *.tab file format:
#' *.tab files are:
#' \itemize{
#'     \item tab separated
#'     \item first row is header
#'     \item first two columns are alignment column indices
#'     \item column names do not start with '[r|z]_'
#' }
loadTab = function(fn){
    tab = try(
        data.table::fread(fn, header = TRUE, sep = '\t')
    )
    if(class(tab) == 'try-error'){
        warning(paste('loadTab: Read no lines from', fn))
        return(NULL)
    }
	return(tab)
}

