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

#source('../R/tabIO.R')
#source('../R/utility.R')
#source('../R/pvalues.R')
#source('../R/performance.R')
library(coevo)

#tabfn = "~/db_projects_aram/PDB_CoevoLearning/BakerGroupData/anal2/1RM6_A_1RM6_B.tab"
tabfn = "~/db_projects_aram/PDB_CoevoLearning/BakerGroupData/anal2/1I1Q_A_1I1Q_B.tab"
indices = c('Left_Column', 'Right_Column')
covars = c('Left_Entropy', 'Right_Entropy', 'Joint_Entropy')
dist_col = 'Dist'

drop_these = c(indices, covars, dist_col, 'CoMapP')


cat(paste('loading', tabfn, '\n'))
tab = loadTab(tabfn)
print(dim(tab))

cat('removing all-NA-columns\n')
tab = removeAllNAColumns(tab)
print(dim(tab))

cat('removing Dist-NA-rows\n')
tab = dropNARowsInCol(tab, dist_col)
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
ROCRready = ROCRprep(clean_r_tab, labels)
pred = getROCRPredObj(ROCRready$predictions, ROCRready$labels)

targetFPR = 0.01
cat(paste('cutoff-dependent metrics at targetFPR =', targetFPR, '\n'))
FPRcuts = getCutoffsThatControlFPR(pred, targetFPR, colnames(r_tab))
TPRatFPR = getTPRAtControlledFPR(pred, targetFPR, colnames(r_tab))
FPRatFPR = getFPRAtControlledFPR(pred, targetFPR, colnames(r_tab))
PPVatFPR = getPPVAtControlledFPR(pred, targetFPR, colnames(r_tab))

RES1 = data.frame(rbind(FPRcuts, TPRatFPR, FPRatFPR, PPVatFPR))
print(RES1)

cat('cutoff-indep metrics\n')
AUC = getCutoffIndependentMetrics(pred, 'auc', colnames(r_tab))
AUPR = getCutoffIndependentMetrics(pred, 'aupr', colnames(r_tab))
FMAX = getCutoffIndependentMetrics(pred, 'f', colnames(r_tab))
PHIMAX = getCutoffIndependentMetrics(pred, 'phi', colnames(r_tab))

RES2 = data.frame(rbind(AUC, AUPR, FMAX, PHIMAX))
print(RES2)

cat('unflip\n')

for(cn in colnames(RES1)){
    if(flipRequired(cn)){
        RES1['FPRcuts', cn] = flip(RES1['FPRcuts', cn])
    }
}
print(RES1)
cat('\n\n')
print(RES2)















