#R
# Define contact labels based on literature cutoffs

makeCB_labels = function(dist_cb){
	cutoff = 8
	dist_cb[,3] = dist_cb[,3] < 8
	colnames(dist_noh)[3] = 'CB_Labels'
	return(dist_cb)
}

makeNoH_labels = function(dist_noh){
	cutoff = 6
	dist_noh[,3] = dist_noh[,3] < 6
	colnames(dist_noh)[3] = 'noH_Labels'
	return(dist_noh)
}

makeSpecDef_labels = function(Vir_spec_def_fn, Mam_spec_def_fn, dist_lab){
	# intersection of dist_lab and specificity determining residues in 
	# Vir_spec_def_fn and Mam_spec_def_fn
	Vir = read.table(Vir_spec_def_fn, sep='\t')$V1
	Mam = read.table(Mam_spec_def_fn, sep='\t')$V1
	virsel = dist_lab[,'Virus_Column'] %in% Vir
	mamsel = dist_lab[,'Mammal_Column'] %in% Mam
	virmamsel = apply(cbind(mamsel, virsel), 1, any)
	virmamdistsel = apply(cbind(virmamsel, dist_lab[,3]), 1, all)

	spec_def = cbind(dist_lab[,c('Virus_Column', 'Mammal_Column')],
					virmamdistsel)
	colnames(spec_def)[3] = 'SpecDef_Labels'
	return(spec_def)
}

