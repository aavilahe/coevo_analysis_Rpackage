#!/usr/bin/Rscript
# mkDatFig.R -- makes ggplot2 figures
#
#

############################################################################
# getopts 

args = commandArgs(TRUE)
#args = c('--Xaxis', 'Alignment_Size', '--Yaxis', 'TPR', '--GrpBy', 'Method',
#					'--Title', 'TPR for P < 0.1', '--strip_sizes',
#					'--DatFile', 'ALL_METHODS_JAT_P-TPR-0.1.dat')

spec = matrix(c(
				'help', 'h', 0, 'logical',
						'display this help',
				'DatFile', 'D', 1, 'character',
						'ALL_METHOD_${tag}-${prf}-0.1.dat, etc...',
				'Xaxis', 'X', 1, 'character',
						'Alignment_Size, RateCat1, etc..',
				'Yaxis', 'Y', 1, 'character',
						'TPR, RateCat2, etc...',
				'Zaxis', 'Z', 1, 'character',
						'TPR, FPR, etc...',
				'GrpBy', 'G', 1, 'character',
						'Method, etc...',
				'PngPre', 'P', 1, 'character',
						'prefix for png output filename',
				'MainTitle', 'M', 2, 'character',
						'Main Title',
				'Xlab', 'x', 1, 'character',
						'Xaxis label',
				'Ylab', 'y', 1, 'character',
						'Yaxis label',
				'Type', 'T', 1, 'character',
						'Line, Facet, Squish, MultiHeat',
				'addHLine', 'L', 1, 'numeric',
						'0.05, 0.001, etc..',
				'SetYRange', 'R', 1, 'character',
						'(min,max)',
				'NaNToZero', 'A', 0, 'logical',
						'sets Y or Z NaNs to 0 before aggregating',
				'dump_table', 'd', 0, 'logical',
						'dumps ag_dat to table',
				'FilterDummy', 'F', 1, 'character',
						'removes methods with dummy values',
				'no_XoverAS', 'a', 0, 'logical',
						'divide Xaxis by Alignment Size',
				'Wrap', 'W', 0, 'logical',
						'use facet_wrap instead of facet_grid',
				'smoother', 's', 2, 'character',
						'use loess smoother in --Type Facet',
				'nopng', 'n', 0, 'logical',
						'disables png output',
				'pdf', 'v', 0, 'logical',
						'enables pdf output',
				'no_pdSamp', 'N', 0, 'logical',
						'removes pdSamp',
				'strip_sizes', 'S', 0, 'logical',
						'samp250 -> 250; pdSamp250 -> 250'
		), byrow=TRUE, ncol=5)

usage = function(spec){
	# print usage and exit
	cat(getopt(spec, opt=args, usage=TRUE), file=stderr())
	q(status=1)
}

parse_the_args = function(spec, args){
	# seems useful.. maybe put in other Rscripts?
	require(getopt)
	opts = tryCatch(
		# parse options, prints error and usage on error
		getopt(spec, opt=args),
		error = function(cond){
			myMessage = paste("Error:", conditionMessage(cond))
			write(myMessage, stderr())
			usage(spec)
		}
	)
	if(!is.null(opts$help) | length(names(opts))==1){
		# prints usage if --help or run without args
		usage(spec)
	}
	if(is.null(opts$Xaxis) | is.null(opts$Yaxis) | is.null(opts$Type) |
			is.null(opts$DatFile) | is.null(opts$PngPre)) {
		write(paste("Error:", "--Xaxis, --Yaxis, --DatFile", 
					"--PngPre, --Type  must be defined"), file=stderr())
		usage(spec)
	}
	return(opts)
}

parseTheYRange = function(opts_SetYRange){
	if(!is.null(opts_SetYRange)){
		# remove parens
		opts_SetYRange = gsub('[()]', '', opts_SetYRange)
		theYRange = as.numeric(unlist(strsplit(opts_SetYRange, ',')))
	} else {
		theYRange = c(0,1)
	}
	return(theYRange)
}

parseTheFilter = function(opts_FilterDummy){
	if(!is.null(opts_FilterDummy)){
		removeThese = unlist(strsplit(opts_FilterDummy, ','))
	} else {
		removeThese = c()
	}
	return(removeThese)
}

opts = parse_the_args(spec, args)
theYRange = parseTheYRange(opts$SetYRange)
removeThese = parseTheFilter(opts$FilterDummy)

### Aux
#setFixedYrange = function(the_plot, the_data, the_Yax){
#	maxY = max(the_data[, paste(the_Yax, 'upBar', sep='.')])
#	the_plot = the_plot + ylim(0,max(1.05*maxY,1))
#	return(the_plot)
#}

setYRange = function(the_plot, theYRange){
	#return(the_plot + ylim(theYRange))
	return(the_plot + coord_cartesian(ylim=theYRange))
}

filterDummies = function(dat, removeThese){
	dat = subset(dat, Method %ni% removeThese)
	return(dat)
}

doFaceting = function(opts_Wrap, Variable){
	if(!is.null(opts_Wrap)){
		return(facet_wrap(as.formula(paste('~', Variable)), scale='free', nrow=2))
	} else {
		return(facet_grid(as.formula(paste('. ~', Variable)), scale='free'))
	}
}

#### Main
source('plotDat_functions.R')
require(plyr)
require(grid)

dat = read.table(opts$DatFile, sep='\t', header=TRUE)
dat = filterDummies(dat, removeThese)
dat = methodFactor(dat) # sets names(method_colors)

# preserve the input order of Alignment_Size column
dat$Alignment_Size = factor(dat$Alignment_Size,
					levels=unique(dat$Alignment_Size))

# strip pdSamp
if(!is.null(opts$no_pdSamp)){
	dat = subset(dat, substring(Alignment_Size, 1, 6) != 'pdSamp')
}

# remove leading letters from Alignment_Size (and enforce factor order again)
if(!is.null(opts$strip_sizes)){
	dat$Alignment_Size = gsub("^[^0-9]*([0-9]+)$", "\\1", dat$Alignment_Size)
	dat$Alignment_Size = factor(dat$Alignment_Size,
							levels=sort(unique(as.numeric(dat$Alignment_Size))))
}

#### ``fig 1'' ~ eg. TPR vs Alignment_Size by Method
if(opts$Type == "Line"){
	ag_dat = aggDat(dat, opts$Yaxis, c(opts$Xaxis, opts$GrpBy), opts$NaNToZero)

	# turns stripped alignment sizes into numbers
	#if(!is.null(opts$strip_sizes)){
	#	ag_dat$Alignment_Size = as.numeric(as.character(ag_dat$Alignment_Size))
	#}

	## make line plot ##
	YaxMean = paste(opts$Yaxis, 'median', sep='.') # plotDatLines(ag_dat) expects colname: Yax'.median'
	the_plot = plotDatLines(ag_dat, opts$Xaxis, YaxMean, opts$GrpBy)

	## add error bars ##
	YaxLo = paste(opts$Yaxis, 'loBar', sep='.') # addErrorBars() expects ag_dat colname: Yax'.loBar'
	YaxUp = paste(opts$Yaxis, 'upBar', sep='.') # addErrorBars() expects ag_dat colname: Yax'.upBar'
	the_plot = the_plot + addErrorBars(YaxLo, YaxUp)

	## fixed y-range ##
	the_plot = setYRange(the_plot, theYRange)

	## add proper title and labels ##
	the_plot = addTitlesAndLabs(the_plot, opts$MainTitle, opts$Xlab, opts$Ylab)

	## black and white grid
	the_plot = the_plot + theme_bw()

	## add horizontal line
	if(!is.null(opts$addHLine)){
		the_plot = the_plot + geom_abline(slope=0, intercept=opts$addHLine, linetype='dashed', color='grey')
	}

	## save ##
	if(is.null(opts$nopng)){
		# default png 
		ggsave(plot=the_plot, filename=getPngName(opts$DatFile, opts$PngPre, opts$Type))
	}
	if(!is.null(opts$dump_table)){
		dumpTable(fn=getTableDumpName(opts$DatFile, opts$PngPre, opts$Type), df=ag_dat)
	}
	if(!is.null(opts$nopng) | !is.null(opts$pdf)){
		ggsave(plot=the_plot, filename=getPDFName(opts$DatFile, opts$PngPre, opts$Type), useDingbats=FALSE)
	}
}

#### ``fig 2'' ~ eg. TPR vs PD by Method
# facet by Alignment_Size
if(opts$Type == "Facet"){	
	if(!is.null(opts$no_XoverAS)){
		the_plot = plotDatSmooth(dat, opts$Xaxis, opts$Yaxis, opts$GrpBy, opts$smoother)
	} else {
		# divide Xaxis by Alignment Size
		XoverAS = paste(opts$Xaxis, 'Alignment_Size', sep='.')
		dat[, XoverAS] = dat[,opts$Xaxis] / as.numeric(as.character(dat[,'Alignment_Size']))
		the_plot = plotDatSmooth(dat, XoverAS, opts$Yaxis, opts$GrpBy, opts$smoother)
	}

	## add facet grid
	the_plot = the_plot + doFaceting(opts$Wrap, 'Alignment_Size')

	## fixed y-range ##
	the_plot = setYRange(the_plot, theYRange)

	## add proper title and labels ##
	the_plot = addTitlesAndLabs(the_plot, opts$MainTitle, opts$Xlab, opts$Ylab)

	## black and white grid
	the_plot = the_plot + theme_bw()

	## flip Xaxis labels
	the_plot = the_plot + theme(axis.text.x=element_text(angle = 45, vjust = 0.5)) 

	## add horizontal line
	if(!is.null(opts$addHLine)){
		the_plot = the_plot + geom_abline(slope=0, intercept=opts$addHLine, linetype='dashed', color='grey')
	}
	
	## save ##
	if(is.null(opts$nopng)){
		# default png
		ggsave(plot=the_plot, filename=getPngName(opts$DatFile, opts$PngPre, opts$Type),
				width=12, height=6, units='in')
	}
	if(!is.null(opts$nopng) | !is.null(opts$pdf)){
		ggsave(plot=the_plot, filename=getPDFName(opts$DatFile, opts$PngPre, opts$Type),
				width=12, height=6, units='in', useDingbats=FALSE)
	}

}

#### ``fig 3'' ~ rate category heatmap multiplot
if(opts$Type == "MultiHeat"){
	if(!is.null(opts$Zaxis)){
		newGrpBy = c('Alignment_Size', 'Method', opts$Xaxis, opts$Yaxis)
		ag_dat = aggDat(dat, opts$Zaxis, newGrpBy, opts$NaNToZero)
		numAsizes = length(unique(ag_dat$Alignment_Size))
		# for each Alignment_Size, run plotDatHeat and modify it to fit in
		# a multiplot. returns list of modified heatmaps
		the_plotlist = dlply(ag_dat, .(Alignment_Size), function(X){
							method_name = X$Method[1]
							align_size = X$Alignment_Size[1]
							a_plot = plotDatHeat(X, opts$Xaxis, opts$Yaxis, opts$Zaxis) +
									# title and xlab for each 'portrait' style plot
									ggtitle(opts$MainTitle) +
									xlab(opts$Xlab) +
									# no ylab
									ylab(NULL) +
									# overwrite legend. limits are [0,1] because we plot FPR/TPR/PPV
									scale_fill_gradient(limits=c(0,1), guide="none") +
									# shrink vertical space, remove left/right margins
									coord_fixed(0.8) + theme(plot.margin=unit(c(0,-0.1, 0, -0.1),'in')) +
									# rows are methods; columns are alignment sizes
									facet_grid(Method~Alignment_Size) +
									# add bw theme
									theme_bw()
							return(a_plot)
							}
						)
		# add ylab to the first columnar plot
		the_plotlist[[1]] = the_plotlist[[1]] + ylab(opts$Ylab)
		# add legend to the last columnar plot. limits are [0,1] because we plot FPR/TPR/PPV
		the_plotlist[[length(the_plotlist)]] = the_plotlist[[length(the_plotlist)]] +
													scale_fill_gradient(limits=c(0,1), guide="colourbar")
		if(is.null(opts$nopng)){
			png(filename=getPngName(opts$DatFile, opts$PngPre, opts$Type), h=1200, w=1920)
				multiplot(plotlist=the_plotlist, cols=numAsizes)
			dev.off()
		}
		if(!is.null(opts$nopng) | !is.null(opts$pdf)){
			pdf(file=getPDFName(opts$DatFile, opts$PngPre, opts$Type), h=12, w=24, useDingbats=FALSE)
				multiplot(plotlist=the_plotlist, cols=numAsizes)
			dev.off()
		}
		if(!is.null(opts$dump_table)){
			dumpTable(fn=getTableDumpName(opts$DatFile, opts$PngPre, opts$Type), df=ag_dat)
		}
	} else {
		write('Error: "--Zaxis" not defined for "--Type MultiHeat"\n', file=stderr())
	}
}


