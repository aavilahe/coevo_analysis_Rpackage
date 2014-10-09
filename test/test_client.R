#!/usr/bin/Rscript
# load one tab
# clean
# get performance
#   - tpr @ fpr cutoff
#   - ppv @ fpr cutoff
#   - max f
#   - max phi
#   - aupr
#   - auc

source('../R/tabIO.R')
source('../R/utility.R')
source('../R/pvalues.R')
source('../R/performance.R')


tabfn = "~/db_projects_aram/PDB_CoevoLearning/BakerGroupData/anal2/1RM6_A_1RM6_B.tab"
indices = c('Left_Column', 'Right_Column')
covars = c('Left_Entropy', 'Right_Entropy', 'Joint_Entropy')
dist_col = 'Dist'

drop_these = c(indices, covars, dist_col)


cat(paste('loading', tabfn, '\n'))
tab = loadTab(tabfn)
print(dim(tab))

cat('removing all-NA-columns\n')
tab = removeAllNAColumns(tab)
print(dim(tab))


cat('dropping non-score columns\n')
s_tab = dropColumns(tab, drop_these)  # scores only
print(dim(s_tab))

score_columns = colnames(s_tab)

cat('making Pempirical tab\n')
r_tab = makePvalues(s_tab, score_columns, Pempirical, 'r_')
print(dim(r_tab))

cat('making Pnormal tab\n')
z_tab = makePvalues(s_tab, score_columns, Pnormal, 'z_')
print(dim(z_tab))

### get r_tab performance
cat('cleaning r_tab for ROCR\n')
clean_r_tab = cleanColumns(r_tab, colnames(r_tab))
print(dim(clean_r_tab))
labels = tab[, dist_col] < 8

cat('making ROCR pred\n')
pred = getROCRPredObj(clean_r_tab, labels)

targetFPR = 0.01
cat(paste('cuts controlling fpr at', targetFPR, '\n'))
FPRcuts = getCutoffsThatControlFPR(pred, targetFPR, colnames(r_tab))
print(FPRcuts)

cat('unflip\n')
for(i in 1:length(FPRcuts)){
    column_name = names(FPRcuts[i])
    if(flipRequired(column_name)){
        FPRcuts[i] = flip(FPRcuts[i])
    }
}
print(FPRcuts)















