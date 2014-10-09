#!/usr/bin/Rscript
# updated for the mkDatFig.R

# aggregate and plot the output of a .dat file
# 

# globals
require(RColorBrewer)
hardcoded_methods <<- c('VarInf', 'MutInf', 'Zmin_MutInf', 'Zjoint_MutInf',
						'dcaMI', 'dcaDI',
						'hpDCA_p32', 'hpDCA_p256',
						'plmDCA',
						'psicov'
					)
method_colors <<- brewer.pal(length(hardcoded_methods), 'Paired')


### aux functions
getQuantileBars = function(x,quant){
	loQ = (1 - quant)/2
	upQ = 1 - loQ
	x = unlist(x) # median won't work on lists or data.frames
	x = as.numeric(na.omit(x))
	xsummary = c(median(x), quantile(x, c(loQ, upQ)))
	names(xsummary) = c('median', 'loBar', 'upBar')
	return(xsummary)
}

aggDat = function(dat, Yax, GrpBy, NaNToZeroResponse=NULL){
	#### Aggregate Yax values by GrpBy factors
	## dat is a data.frame
	## ag_dat is a data.frame

	## set NaNs to 0 before aggregating () 
	if(!is.null(NaNToZeroResponse)){
		dat[is.nan(dat[ ,Yax]), Yax] = 0
	}

	grouping = list()
	for(nm in GrpBy){
		grouping[[nm]] = dat[,nm]
	}
	ag_dat = aggregate(subset(dat,T,Yax),
					by=grouping,
					FUN=getQuantileBars, .50
				)

	## ``unlist'' vector column
	yaxmat = ag_dat[,Yax]
	colnames(yaxmat) = paste(Yax,colnames(yaxmat),sep='.')
	ag_dat = cbind(ag_dat[,which(colnames(ag_dat) %ni% Yax)],yaxmat)
	return(ag_dat)
}

getPngName = function(fn, pngPrefix, Type){
	## Format: [pngPrefix]_[Type]--[fn].png
	bname = basename(fn)
	pngOut = paste(pngPrefix,'_',Type,'--',bname,'.png', sep='') 
	return(pngOut)
}

getPDFName = function(fn, pngPrefix, Type){
	pngOut = getPngName(fn, pngPrefix, Type)
	pdfOut = sub('.png$', '.pdf', pngOut)
	return(pdfOut)
}

getTableDumpName = function(fn, pngPrefix, Type){
	pngOut = getPngName(fn, pngPrefix, Type)
	tableOut = sub('.png$', '.tableDump', pngOut)
	return(tableOut)
}

dumpTable = function(fn, df){
	cat("dumping table\n", file=stderr())
	write.table(df, fn, sep='\t', quote=FALSE, row.names=FALSE)
}

addTitlesAndLabs = function(plt, title=NULL, xLab=NULL, yLab=NULL){
	if(!is.null(title)){
		plt = plt + ggtitle(title)
	}
	if(!is.null(xLab)){
		plt = plt + xlab(xLab)
	}
	if(!is.null(yLab)){
		plt = plt + ylab(yLab)
	}
	return(plt)
}

### plot functions
plotDatLines = function(dat, Xax, Yax, GrpBy){
	## makes a line plot from dat or ag_dat
	## Expects colnames: Xax Yax GrpBy
	require(ggplot2)
	lineSize = 1
	plt = ggplot(data=dat,
			aes_string(x=Xax, y=Yax, colour=GrpBy)
			) + 
			geom_line(aes_string(group=GrpBy), size=lineSize) +
			geom_point()
	plt = plt + scale_color_manual(breaks=names(method_colors), values=method_colors)
	return(plt)
}

addErrorBars = function(YaxLo, YaxUp){
	# adds error bars to a plotDatLines() plt made from an ag_dat
	require(ggplot2)
	errBarWidth = 0.1
	bars = geom_errorbar(aes_string(ymin=YaxLo, ymax=YaxUp), width=errBarWidth)
	return(bars)
}

plotDatSmooth = function(dat, Xax, Yax, GrpBy, Smoother='lm'){
	## plots points from dat or ag_dat with a smoother (lm or loess)
	## Expects colnames: Xax Yax GrpBy
	require(ggplot2)

	if(is.null(Smoother)){
		Smoother = 'lm'
	}

	plt = ggplot(data=dat,
			aes_string(x=Xax, y=Yax, colour=GrpBy)
			) + stat_smooth(method=Smoother, alpha=0.2) +
			geom_point()
	plt = plt + scale_color_manual(breaks=names(method_colors), values=method_colors)
	return(plt)
}

plotDatHeat = function(ag_dat, Xax, Yax, Zax){
	require(ggplot2)
	Zax = paste(Zax,'median',sep='.')
	plt = ggplot(data=ag_dat,
			aes_string(x=Xax, y=Yax, fill=Zax), environment = environment()
			) + 
			geom_tile(colour='black') + scale_fill_gradient(limits=c(0,1)) +
			geom_text(aes(label=ifelse(is.nan(get(Zax)),
										"", 
										sprintf('%.2f', get(Zax))
										)
						), colour="white"
			) +
			theme(axis.text.x=element_text(angle = 90, vjust = 0.5)) +
			coord_fixed()
	return(plt)
}

methodFactor = function(dat){
	if('Method' %in% names(dat)){
		dat_methods = unique(dat$Method)
		prefix = substr(dat_methods[1],1,2)
		if(prefix %in% c('p_','r_','z_')){
			factor_names = paste(prefix, hardcoded_methods, sep='')
		} else {
			factor_names = hardcoded_methods
		}
		dat$Method = factor(dat$Method, ordered=TRUE, levels=factor_names)
		names(method_colors) <<- factor_names
	}
	return(dat)
}

############## 3rd party aux plot functions #############
# http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_%28ggplot2%29/
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
require(grid)

	# Make a list from the ... arguments and plotlist
	plots <- c(list(...), plotlist)

	numPlots = length(plots)

	# If layout is NULL, then use 'cols' to determine layout
	if (is.null(layout)) {
		# Make the panel
		# ncol: Number of columns of plots
		# nrow: Number of rows needed, calculated from # of cols
		layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
										ncol = cols, nrow = ceiling(numPlots/cols))
	}

	if(numPlots==1) {
		print(plots[[1]])
	} else {
		# Set up the page
		grid.newpage()
		pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

		# Make each plot, in the correct location
		for (i in 1:numPlots) {
			# Get the i,j matrix positions of the regions that contain this subplot
			matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

			print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
																			layout.pos.col = matchidx$col))
		}
	}
}

