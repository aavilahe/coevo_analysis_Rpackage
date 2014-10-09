# r
source('makePredictions.R')
source('defineLabels.R')

# for given prediction and label tabs,
# and create confusion matrix
# calculate given performance measure


getConfusionMatrix = function(predict_tab, label_tab){
	merged_tab = merge(predict_tab, label_tab, c("Virus_Column", "Mammal_Column"), all.y=TRUE)
	merged_tab[!is.finite(merged_tab[,3]),3] = FALSE
	TP = sum(merged_tab[,3] == TRUE & merged_tab[,4] == TRUE)
	FP = sum(merged_tab[,3] == TRUE & merged_tab[,4] == FALSE)
	TN = sum(merged_tab[,3] == FALSE & merged_tab[,4] == FALSE)
	FN = sum(merged_tab[,3] == FALSE & merged_tab[,4] == TRUE)

	return(matrix(c(TP, FP,
					FN, TN), 2, 2, byrow=TRUE))
}

getTPR = function(confMat){
	# TPR = TP/(TP+FN)
	TP = confMat[1,1]
	FN = confMat[2,1]
	return(TP/(TP+FN))
}
getPPV = function(confMat){
	# PPV = TP/(TP+FP)
	TP = confMat[1,1]
	FP = confMat[1,2]
	return(TP/(TP+FP))
}
getFPR = function(confMat){
	# FPR = FP/(TN+FP)
	FP = confMat[1,2]
	TN = confMat[2,2]
	return(FP/(TN+FP))
}

getPerf = function(perfName, confMat){
	return(do.call(perfName, list(confMat)))
}

getRateCatBreaks = function(llTabsWithRates, numVirRates, numMamRates){
	# determine rate categories (breaks actually) for tabs of the same sample size

	# this function extracts rates for each site from a TabWithRates data.frame
	singleSiteRates = function(TabWithRates, whatRates){
		if(whatRates == "Virus"){
			whatCols = "Virus_Column"
			whatEnt = "Vir_Entropy"
		} else {
			whatCols = "Mammal_Column"
			whatEnt = "Mam_Entropy"
		}

		return(as.numeric(
			TabWithRates[!duplicated(TabWithRates[,whatCols]), whatEnt]
		))
	}


	vprobs = (0:numVirRates)/numVirRates
	mprobs = (0:numMamRates)/numMamRates
	the_breaks = list()
	for(samp_size in names(llTabsWithRates)){
		replicates = llTabsWithRates[[samp_size]]
		# sapply fails if replicates don't all have same num rows
		#vrates = unlist(lapply(replicates, subset, TRUE, Vir_Entropy))
		#mrates = unlist(lapply(replicates, subset, TRUE, Mam_Entropy))
		vrates = unlist(lapply(replicates, singleSiteRates, "Virus"))
		mrates = unlist(lapply(replicates, singleSiteRates, "Mammal"))
		vbreaks = unique(quantile(vrates, probs=vprobs))
		mbreaks = unique(quantile(mrates, probs=mprobs))
		the_breaks[[samp_size]] = list(vbreaks=vbreaks, mbreaks=mbreaks)
	}
	return(the_breaks)
}

binTabByRateCat = function(tabWithRates, vbreaks, mbreaks){
	# annotate each tabWithRates with rate categories in mbreak and vbreaks
	require(plyr)
	tabWithRates$Vir_RateCat = cut(tabWithRates$Vir_Entropy, breaks=vbreaks, include.lowest=TRUE)
	tabWithRates$Mam_RateCat = cut(tabWithRates$Mam_Entropy, breaks=mbreaks, include.lowest=TRUE)
	return(dlply(tabWithRates, .(Vir_RateCat, Mam_RateCat)))
}

getControlledConfMat = function(tab, controlName, controlCmp, threshold,
										whatName, whatCmp, whatBounds, label_tab){
#eg. getControlledConfMat(tab, 'getFPR', '<', 0.1, 'MutInf', '>', c(0,NA), label_tab)


	BestToWorst = (whatCmp == '>')

	if(controlName == 'getFPR'){
		merged_tab = merge(tab[, c("Virus_Column","Mammal_Column",whatName)], label_tab, c("Virus_Column", "Mammal_Column"), all.y=TRUE)
		negatives = merged_tab[merged_tab[, 4] == FALSE, 3]
		negatives = sort(negatives, decreasing=BestToWorst, na.last=TRUE)
		idx = ceiling(threshold * length(negatives)) #idx of best scoring TN at FPR < thresh
		bestTN = negatives[idx]
	} else {
		stop(paste('controlling', controlName, 'not implemented yet'))
	}

	predict_tab = makePrediction(tab, whatName, bestTN, whatCmp)
	confMat = getConfusionMatrix(predict_tab, label_tab)

	return(list(critVal=bestTN, confMat=confMat))

}








