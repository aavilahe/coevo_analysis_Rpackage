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
tab = drop_NA_rows_in_col(tab, dist_col)
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
labels = tab[, dist_col, with = FALSE] < 8

cat('making ROCR pred on empirical p-values\n')
pred_lab = pred_lab_prep(clean_r_tab, labels)
pred = get_ROCR_prediction(pred_lab)

target_FPR = 0.01
cat(paste('cutoff-dependent metrics at target_FPR =', target_FPR, '\n'))
FPRcuts = get_scores_at_FPR(pred, target_FPR)
TPRatFPR = get_TPRs_at_FPR(pred, target_FPR)
FPRatFPR = get_nomFPR_at_FPR(pred, target_FPR)
PPVatFPR = get_PPV_at_FPR(pred, target_FPR)

RES1 = data.table(rbind(FPRcuts, TPRatFPR, FPRatFPR, PPVatFPR))
setnames(RES1, colnames(r_tab))
print(RES1)

cat('cutoff-indep metrics\n')
AUC = getCutoffIndependentMetrics(pred, 'auc', colnames(r_tab))
AUPR = getCutoffIndependentMetrics(pred, 'aupr', colnames(r_tab))
FMAX = getCutoffIndependentMetrics(pred, 'f', colnames(r_tab))
PHIMAX = getCutoffIndependentMetrics(pred, 'phi', colnames(r_tab))

RES2 = data.table(rbind(AUC, AUPR, FMAX, PHIMAX))
setnames(RES2, colnames(r_tab))
print(RES2)

cat('unflip\n')
unflip_these = is_flip(colnames(RES1))
RES1[, (unflip_these) := lapply(.SD, flip), .SDcols = unflip_these]

print(RES1)


# save r_tab, z_tab with indices, covars, and distances

r_ctab = cbind(tab[, c(drop_these)], r_tab)
z_ctab = cbind(tab[, c(drop_these)], z_tab)


