cleanColumns <-
function(tab, column_names){
    # flip() and naToBottom()
    # use before calculating p-values and performance
    for(column_name in column_names){
        tmpCol = tab[ , column_name]
        if(flipRequired(column_name)){
            tmpCol = flip(tmpCol)
        }
        tab[, column_name] = naToBottom(tmpCol)
    }
    return(tab)
}
