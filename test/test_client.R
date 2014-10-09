#!/usr/bin/Rscript

source('../R/tabIO.R')
source('../R/utility.R')


tabfn = "~/db_projects_aram/PDB_CoevoLearning/BakerGroupData/anal2/1RM6_A_1RM6_B.tab"

ogtab = loadTab(tabfn)
tab = removeAllNAColumns(ogtab)

dropTheseColumns = c('Left_Column', 'Right_Column',
                        'Left_Entropy', 'Right_Entropy',
                        'Joint_Entropy'
                    )

tab = dropColumns(tab, dropTheseColumns)


