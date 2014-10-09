#R
# Make coevolution predictions based on user specified cutoff

# requires tabs to be loaded


makePValuePrediction = function(tab, p_stat, cutoff){
	predict_tab = tab[,c('Virus_Column', 'Mammal_Column', p_stat)]
	predict_tab[,p_stat] = tab[,p_stat] < cutoff
	colnames(predict_tab)[3] = paste(p_stat,'Prediction', sep='_')
	return(predict_tab)
}

makePrediction = function(tab, stat, cutoff, cmp_func){
	predict_tab = tab[,c('Virus_Column', 'Mammal_Column', stat)]
	predict_tab[,stat] = do.call(cmp_func, list(tab[,stat], cutoff))
	colnames(predict_tab)[3] = paste(stat,'Prediction', sep='_')
	return(predict_tab)
}

