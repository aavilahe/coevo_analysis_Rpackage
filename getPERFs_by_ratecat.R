#!/usr/bin/Rscript

###############################################################################
# getopts stuff
spec = matrix(c(
				'help', 'h', 0, 'logical',
								'display this help',
				'perf', 'p', 1, 'character',
								'TPR, FPR, PPV, etc...',
				'threshold', 't', 1, 'double',
								'max/min "what" value to control "control" or to call coevolution',
				'what', 'w', 1, 'character',
								'what to threshold: Score, Pvalue, Rank, Zscore',
				'contact_label', 'l' , 2, 'character',
								'CB_labels, NoH_labels, SpecDef_labels',
				'p_adjust', 'a', 2, 'character',
								'how to adjust p-vals: none, holm, fdr',
				'control', 'c', 2, 'character',
								'"nomFPR" is only nominal perf implemented yet'

		), byrow=TRUE, ncol=5)

parse_the_args = function(spec){
	# seems useful.. maybe put in other Rscripts?
	require(getopt)
	usage = function(spec){
		cat(getopt(spec, usage=TRUE), file=stderr())
		q(status=1)
	}
	opts = tryCatch(
		getopt(spec),
		error = function(cond){
			myMessage = paste("Error:", conditionMessage(cond))
			write(myMessage, stderr())
			usage(spec)
		}
	)
	if(!is.null(opts$help) | length(names(opts))==1){
		usage(spec)
	}
	return(opts)
}

set_opts_defaults = function(opts){
# hardcode for now
	if(is.null(opts$p_adjust)){
		opts$p_adjust = 'none'
	} else if(opts$p_adjust %ni% c('fdr','holm')){
		opts$p_adjust = 'none'
	}
	if(is.null(opts$contact_label)){
		opts$contact_label = 'SpecDef'
	}
	if(opts$what %ni% c('Score', 'Pvalue', 'Rank', 'Zscore')){
		cat('Warning: thresholding on p-values', file=stderr())
		opts$what = 'Pvalue'
	}
	return(opts)
}

# Aux functions
get_whatNames = function(any_tab, opt_what){
	columnNames = colnames(any_tab)
	
	# get p-value names
	pNames = grep('^p_', columnNames, value=TRUE)

	# get score names ## breaks if there are scores without p-values ##
	scoreNames = sub('^p_','',pNames)

	# get score rank names ## breaks if there are scores without p-values ##
	rankNames = sub('^p_', 'r_', pNames)

	# get zscore names ## breaks if there are scores without p-values ##
	zscoreNames = sub('^p_', 'z_', pNames)

	if(opt_what == 'Score'){
		return(scoreNames)
	} else if(opt_what == 'Rank'){
		return(rankNames)
	} else if(opt_what == 'Zscore'){
		return(zscoreNames)
	}
	return(pNames)
}

set_whatBounds = function(whatName, opt_what){
	if(opt_what == 'Pvalue' | whatName %in% c('Zmin_MutInf', 'Zjoint_MutInf')){
		return(c(0,1))
	}
	return(c(0,NA))
}

set_controlCmp = function(opt_control){
	if(opt_control == 'nomFPR'){
		return('<')
	}
	stop(paste("controlling for '", opt_control, "' is not implemented yet", sep=''))
}

###############################################################################
# Parse and load
opts = parse_the_args(spec)
opts = set_opts_defaults(opts)

source('data_loader.R')
source('calculatePerformance.R')
cat('\n\n\n\n\n', file=stderr())
#options(error=recover)

### Z-score hotfix
# convert quantile threshold to z-score if not controlling FPR
if(opts$what == 'Zscore' & is.null(opts$control)){
	opts$threshold = qnorm(1-opts$threshold)
}
###

switch(opts$contact_label,
	NoH_labels={
		the_labels=makeNoH_labels(dist_noh)
	},
	CB_labels={
		the_labels=makeCB_labels(dist_cb)
	},
	{
		the_labels = makeSpecDef_labels(spec_def_HK_fn,spec_def_RR_fn,makeCB_labels(dist_cb))
	}
)

if(!is.null(opts$control)){
	controlCmp = set_controlCmp(opts$control) # set how to control nominal performance measure
}

the_breaks = getRateCatBreaks(infStat_samps, 2, 2) # get entropy 50%iles for each sample size

##############################################################################
# MAIN:
# for each samp
#   for each tab
#     get performance
#     print performance and tab data

#
#MasterSampleList = list(infStat_samps, mfdca_samps)

# print headers
cat(paste(
	'Method', opts$perf, 'Replicate', 'Alignment_Size', 'PD', 'CritVal', 
	'RateCat1', 'RateCat2', #rcVir, rcMam
	sep='\t')
); cat('\n')

for(TABTYPE in MasterSampleList){
	for(samp_size in names(TABTYPE)){
		replicates = TABTYPE[[samp_size]]
		if(length(replicates) < 1){
			next
		}
		pds = pd_samps[[samp_size]]
	
		vbreaks = the_breaks[[samp_size]][['vbreaks']] # get breaks for each samp_size
		mbreaks = the_breaks[[samp_size]][['mbreaks']]

		whatNames = get_whatNames(replicates[[1]], opts$what)
		for(whatName in whatNames){
			whatCmp = set_whatCmp(whatName, opts$what)
			whatBounds = set_whatBounds(whatName, opts$what)
			for(tab_bfn in names(replicates)){
				tab = replicates[[tab_bfn]]
				pd = pds[[tab_bfn]]
				
				if(whatName %ni% names(tab)){
					next
				}

				if(opts$what == 'Pvalue' | opts$what == 'Rank'){
					tab[, whatName] = p.adjust(tab[, whatName], method=opts$p_adjust)
				}

				#### split tab into list of tabs by rate category
				## If tab isn't an infStat_samps, then merge the infStat_samps marginal entropies
				if(any(c('Vir_Entropy', 'Mam_Entropy') %ni% colnames(tab))){
					the_rates = infStat_samps[[samp_size]][[tab_bfn]][c('Virus_Column', 'Mammal_Column',
																		'Vir_Entropy', 'Mam_Entropy')]
					tab = merge(the_rates, tab, c('Virus_Column', 'Mammal_Column'), all.x=TRUE)
				}
				tabsByRateCat_list = binTabByRateCat(tab, vbreaks, mbreaks) # make tabs by rate category

				####
				for(rcTab in tabsByRateCat_list){
					rcVir = rcTab$Vir_RateCat[1] # get rate categories
					rcMam = rcTab$Mam_RateCat[1]

					#### recalculate zscores and ranks for each pair of rate categories
					prefix = substr(whatName,1,2)
					if(prefix == 'r_'){
						theColName = substr(whatName, 3, nchar(whatName))
						rcTab = addRank(rcTab, theColName)
					}
					if(prefix == 'z_'){
						theColName = substr(whatName, 3, nchar(whatName))
						rcTab = addZscore(rcTab, theColName)
					}
					########

					if(!is.null(opts$control)){
						#controlCmp = set_controlCmp(opts$control) # could possibly move this out
						critVal_and_confMat = getControlledConfMat(rcTab, sub('^nom','get',opts$control),
														controlCmp, opts$threshold,
														whatName, whatCmp, whatBounds, the_labels)
						critVal = critVal_and_confMat[['critVal']]
						confMat = critVal_and_confMat[['confMat']]
					} else {
						critVal = opts$threshold
						predict_tab = makePrediction(rcTab, whatName, critVal, whatCmp)
						confMat = getConfusionMatrix(predict_tab, the_labels)
					}
					perf_value = getPerf(paste('get',opts$perf,sep=''), confMat)
					cat(paste(
						whatName, perf_value, tab_bfn, samp_size, pd, critVal, rcVir, rcMam, sep='\t'
					)); cat('\n')
				}
			}
		}
	}	
}
