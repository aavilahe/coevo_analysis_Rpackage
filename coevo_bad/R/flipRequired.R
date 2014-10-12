flipRequired <-
function(column_name){
    prefix = substr(column_name, 1, 2)
    if(prefix == 'r_' | prefix == 'z_'){
        return(TRUE)
    }
    if(column_name %in% c('VI')){
       return(TRUE)
    }
    return(FALSE)
}
