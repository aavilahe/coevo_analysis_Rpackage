#!/usr/bin/Rscript

#' An external testing script to test coevobenchr package
#'
#' @section Tests:
#' \enumerate{
#'     \item load one tab
#'     \item clean rows and columns
#'     \item get performance
#'     \item \enumerate{
#'         \item tpr
#'         \item ppv
#'         \item max f
#'         \item max phi
#'         \item auPR
#'         \item auROC
#'     }
#'     \item write p-value tabs
#' }

library(data.table)
library(coevo)

# Names, labels, etc...
tabfn = "~/db_projects_aram/PDB_CoevoLearning/BakerGroupData/anal2/1RM6_A_1RM6_B.tab"
indices = c('Left_Column', 'Right_Column')
covars = c('Left_Entropy', 'Right_Entropy', 'Joint_Entropy')
dist_col = 'Dist'

drop_these = c(indices, covars, dist_col)  # these are not scores

# Begin tests
cat(paste('loading', tabfn, '\n'))
tab = load_tab(tabfn)
print(dim(tab))

cat('removing all-NA-columns\n')
tab = drop_all_NA_columns(tab)
print(dim(tab))

cat('removing Dist-NA-rows\n')  # partially redundant with pred_lab_prep()
tab = drop_NA_rows_in_columns(tab, dist_col)
print(dim(tab))

cat('dropping non-score columns\n')
s_tab = drop_columns(tab, drop_these)  # scores only
print(dim(s_tab))

score_columns = colnames(s_tab)

cat('making p_empirical tab\n')
r_tab = calculate_p(s_tab, p_empirical, 'r_')
print(dim(r_tab))

cat('making p_normal tab\n')
z_tab = calculate_p(s_tab, p_normal, 'z_')
print(dim(z_tab))

# get r_tab performance
cat('cleaning r_tab for ROCR\n')
clean_r_tab = clean_columns(r_tab)
print(dim(clean_r_tab))
labels = tab[[dist_col]] < 8

cat('making ROCR pred on empirical p-values\n')
pred_lab = pred_lab_prep(clean_r_tab, labels)
pred = get_ROCR_prediction(pred_lab)

target_FPR = 0.01
cat(paste('cutoff-dependent metrics at target_FPR =', target_FPR, '\n'))
FPRcuts = get_scores_at_FPR(pred, target_FPR)
TPRatFPR = get_TPR_at_FPR(pred, target_FPR)
FPRatFPR = get_nomFPR_at_FPR(pred, target_FPR)
PPVatFPR = get_PPV_at_FPR(pred, target_FPR)

RES1 = data.table(rbind(FPRcuts, TPRatFPR, FPRatFPR, PPVatFPR), keep.rownames = TRUE)
setnames(RES1, c('Metric', colnames(r_tab)))
print(RES1)

target_p = 0.01
cat(paste('cutoff-dependent metrics at target_p =', target_p, '\n'))
Pcuts = get_P_at_p(pred, target_p)
TPRatP = get_TPR_at_p(pred, target_p)
FPRatP = get_FPR_at_p(pred, target_p)
PPVatP = get_PPV_at_p(pred, target_p)

RES3 = data.table(rbind(Pcuts, TPRatP, FPRatP, PPVatP), keep.rownames = TRUE)
setnames(RES3, c('Metric', colnames(r_tab)))
print(RES3)


cat('cutoff-indep metrics\n')
AUC = get_cutoff_independent_metric(pred, 'auc')
AUPR = get_cutoff_independent_metric(pred, 'aupr')
FMAX = get_cutoff_independent_metric(pred, 'f')
PHIMAX = get_cutoff_independent_metric(pred, 'phi')

RES2 = data.table(rbind(AUC, AUPR, FMAX, PHIMAX), keep.rownames = TRUE)
setnames(RES2, c('Metric', colnames(r_tab)))
print(RES2)

cat('unflip\n')
unflip_these = which(is_flip(colnames(RES1)))
RES1[ Metric == 'FPRcuts', (unflip_these) := lapply(.SD, flip), .SDcols = unflip_these]
print(RES1)

unflip_these = which(is_flip(colnames(RES3)))
RES3[ Metric == 'Pcuts', (unflip_these) := lapply(.SD, flip), .SDcols = unflip_these]
print(RES3)


# save r_tab, z_tab with indices, covars, and distances

r_ctab = cbind(tab[, drop_these, with = FALSE], r_tab)
z_ctab = cbind(tab[, drop_these, with = FALSE], z_tab)

save_tab(r_ctab, 'r_ctab')
save_tab(z_ctab, 'z_ctab')
