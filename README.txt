This is a codebase to analyze ALREADY cleaned and fixed .tab files.

The codebase has four purposes:

1 load .tab files
2 generate predictions for each method
3 evaluate performance
4 plot performance

TODO: be able to switch between CB and Spec_def
TODO: be able to load Spec_def residues before making labels
TODO: adjust p-vals in getPERF...

executables should run from anywhere


Tab file FORMAT:
	tab delimited file
	Rows:
	pairs of interprotein alignment positions (0-indexed)

	Columns (columns are unfortunately not named):

	1  : alignment position of first protein
	2  : alignment postition of second protein
	3+ : score or p-value for various methods



getPERF_at_cutoff.R generates a .dat file for all loaded tabs
Dat file FORMAT:
	tab delimited file
	Rows:
	performance for a particular .tab

	1  : Method -- method name (eg. p_VarInf, p_dcaDI)
	2  : Perf (TPR, FPR, PPV) -- Perf value
	3  : PD -- phylogenetic distance
	4  : Alignment_Size -- (eg. samp5 samp50...)
	5  : Replicate number -- Columns 1 and 2 make a key
	6+ : other groupings
