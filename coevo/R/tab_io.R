# tab loading functions

#' Reads a *.tab formatted file and returns a data.table.
#'
#' @param fn A filename to read.
#' @return A data.table or NULL.
#' @examples
#' load_tab('results.tab')
#' list_of_tables = lapply(list_of_filenames, load_tab)
#'
#' @section *.tab file format:
#' *.tab files are:
#' \itemize{
#'     \item tab separated
#'     \item first row is header
#'     \item first two columns are alignment column indices
#'     \item column names do not start with '[r|z]_'
#' }
#'
#' @export
load_tab = function(fn){
    tab = try(
        data.table::fread(fn, header = TRUE, sep = '\t')
    )
    if(any('try-error' %in% class(tab))){
        warning(paste('load_tab: Read no lines from', fn))
        return(NULL)
    }
	return(tab)
}
