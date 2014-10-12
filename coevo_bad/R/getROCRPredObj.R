getROCRPredObj <-
function(cleantab, the_labels){
    # cleantab: data.frame containing predictions
    # the_labels: vector containing classes (TRUE/FALSE) (eg. data.frame(Labels = c(T, F, T, F)))
    the_labels = data.frame(matrix(the_labels, length(the_labels), ncol(cleantab), byrow = FALSE))
    pred = prediction(predictions = cleantab, labels = the_labels)
    return(pred)
}
