#R read various types of tab files

# TODO: write loaders for PSICOV, CAPS, plmDCA
#cat('loadTab_mfdca() is hard coded to assume 1000 bootstraps\n', file=stderr())
#cat('loadTab_plmdca() is hard coded to assume 1000 bootstraps\n', file=stderr())
#cat('loadTab_psicov() is hard coded to assume 1000 bootstraps\n', file=stderr())
#cat('loadTab_hpdca() is hard coded to assume 1000 bootstraps\n', file=stderr())

loadTab = function(fn, column_labels){
	# generic load tab
	tab = read.table(fn, sep='\t')
	colnames(tab) = c('Virus_Column', 'Mammal_Column', column_labels)

	return(tab)
}

fixNAs = function(tab, column_labels){
	# set p_stat = 1 if stat == NA
	for(stat in column_labels){
		p_stat = paste('p',stat,sep='_')
		if(p_stat %in% column_labels){
			tab[is.na(tab[,stat]), p_stat] = 1
#			tab[,p_stat] = p.adjust(tab[,p_stat], method='holm') # fwer control (better than bonferroni)
		}
	}
	return(tab)
}

cleanWhats = function(Whats){
	minWhat = min(Whats, na.rm=TRUE)
	Whats[!is.finite(Whats)] = minWhat
	return(Whats)
}

set_whatCmp = function(whatName, opt_what){
# if we're looking at Pvalues or VarInf small numbers means coevolution
# VarInf is a distance
	if(opt_what == 'Pvalue' | opt_what == 'Rank' | whatName == 'VarInf'){
		return('<')
	}
	return('>')
}

addRank = function(tab, column_labels){
	rank_these_labels = grep('^p_', column_labels, invert=TRUE, value=TRUE)
	for(label in rank_these_labels){
		flip = (set_whatCmp(label, 'Score') == '<')
		rlabel = paste('r', label, sep='_')
		x = cleanWhats(tab[, label])
		if(flip){
			R = 1 - ecdf(-x)(-x)
		} else {
			R = 1 - ecdf(x)(x)
		}
		tab[, rlabel] = R
	}
	return(tab)
}

addZscore = function(tab, column_labels){
	zscore_these_labels = grep('^p_', column_labels, invert=TRUE, value=TRUE)
	for(label in zscore_these_labels){
		flip = (set_whatCmp(label, 'Score') == '<')
		zlabel = paste('z', label, sep='_')
		x = cleanWhats(tab[, label])
		if(flip){
			Z = -scale(x)
		} else {
			Z = scale(x)
		}
		tab[, zlabel] = Z
	}
	return(tab)
}

loadTab_infStats = function(fn, suff=NULL){
	column_labels = c('Vir_Entropy', 'Mam_Entropy', 'Joint_Entropy',
						'MutInf', 'VarInf',
						'Zmin_MutInf', 'Zjoint_MutInf',
						'p_MutInf', 'p_VarInf',
						'p_Zmin_MutInf', 'p_Zjoint_MutInf')
	if(!is.null(suff)){
		column_labels = paste(column_labels, suff, sep='_')
	}
	tab = loadTab(fn, column_labels)
	tab = fixNAs(tab, column_labels)
	tab = addRank(tab, column_labels)
	tab = addZscore(tab, column_labels)
	return(tab)
}

loadTab_mfdca = function(fn, suff=NULL){
	column_labels = c('dcaMI', 'dcaDI', 'p_dcaMI', 'p_dcaDI')
	if(!is.null(suff)){
		column_labels = paste(column_labels, suff, sep='_')
	}
	tab = loadTab(fn, column_labels)
#	tab[, column_labels[c(3,4)]] = tab[, column_labels[c(3,4)]] / 1000 # hard coded
	tab = fixNAs(tab, column_labels)
	tab = addRank(tab, column_labels)
	#tab = addZscore(tab, column_labels[1]) # add zscores for dcaMI only
	tab = addZscore(tab, column_labels) # add zscores for dcaMI and dcaDI
	return(tab)
}

loadTab_plmdca = function(fn, suff=NULL){
	column_labels = c('plmDCA', 'p_plmDCA')
	if(!is.null(suff)){
		column_labels = paste(column_labels, suff, sep='_')
	}
	tab = loadTab(fn, column_labels)
#	tab[ , column_labels[2] ]= tab[, column_labels[2] / 1000 # hard coded
	tab = fixNAs(tab, column_labels)
	tab = addRank(tab, column_labels)
	return(tab)
}

loadTab_psicov = function(fn, suff=NULL){
	column_labels = c('psicov', 'p_psicov')
	if(!is.null(suff)){
		column_labels = paste(column_labels, suff, sep='_')
	}
	tab = loadTab(fn, column_labels)
#	tab[, column_labels[2]] = tab[, column_labels[2]] / 1000 # hard coded
	tab = fixNAs(tab, column_labels)
	tab = addRank(tab, column_labels)
	return(tab)
}

loadTab_hpdca = function(fn, suff=NULL){
	column_labels = c('hpDCA', 'p_hpDCA')
	if(!is.null(suff)){
		column_labels = paste(column_labels, suff, sep='_')
	}
	tab = loadTab(fn, column_labels)
#	tab[, column_labels[2] ] = tab[, column_labels[2] ] / 1000 # hard coded
	tab = fixNAs(tab, column_labels)
	tab = addRank(tab, column_labels)
	return(tab)
}

loadTab_dists = function(fn, DistStr='Distance'){
	column_labels = c(DistStr)
	tab = loadTab(fn, column_labels)
	return(tab)
}



