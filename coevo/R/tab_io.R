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

#' Saves a data.table or data.frame as an unquoted tsv
#'
#' Wrapper for \code{write.table()}, does not save row.names,
#' and prints floats to 6 significant digits
#'
#' @param tab A data.table (or data.frame) to save
#' @param fn Filename to write to
#' @return Whatever \code{write.table()} returns
#'
#' @export
save_tab = function(tab, fn){
    write.table(format(tab, digits = 6), fn, sep = '\t',
                row.names = FALSE,
                quote = FALSE)
}

