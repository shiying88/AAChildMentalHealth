############################################
# Select LCMM models of interest
#  Format posterior classifications
#  Join model object & classifications with input data (mdata_imp)
# Purpose of output RDS objects is for exploratory analysis and evaluation of clustering

############################################

library(here)
library(tidyverse)

source(here('code/library/functions.r'))

############################################

# Data
bl <- readRDS(here('data/bl.rds'))
# LCMM model data
mdata_imp <- readRDS(here('data/lcmm_mdata_imp.rds'))

# Fitted LCMM models
lc4_list_imp <- readRDS(here('models/lcmm/lc4_list_imp.rds'))

# Selected models
# EXT: n2_spl4q, n4_spl4q
# INT: n2_spl4q, n3_spl3q, n4_spl4q
ext_n2_spl4q <- joinCl('ext', n=2, model='spl4q', mlist=lc4_list_imp, .bl=bl, df_long=mdata_imp) %>%
    labelCl(class_labels=c(
        `2`='Low',
        `1`='High'
    ), joined_cl=.)
ext_n4_spl4q <- joinCl('ext', n=4, model='spl4q', mlist=lc4_list_imp, .bl=bl, df_long=mdata_imp) %>%
    labelCl(class_labels=c(
        `2`='Low',
        `4`='High',
        `3`='Decreasing',
        `1`='Increasing'
    ), joined_cl=.)

int_n2_spl4q <- joinCl('int', n=2, model='spl4q', mlist=lc4_list_imp, .bl=bl, df_long=mdata_imp) %>%
    labelCl(class_labels=c(
        `2`='Low',
        `1`='High'
    ), joined_cl=.)
int_n3_spl3q <- joinCl('int', n=3, model='spl3q', mlist=lc4_list_imp, .bl=bl, df_long=mdata_imp) %>%
    labelCl(class_labels=c(
        `2`='Low',
        `1`='Med',
        `3`='High'
    ), joined_cl=.)
int_n4_spl4q <- joinCl('int', n=4, model='spl4q', mlist=lc4_list_imp, .bl=bl, df_long=mdata_imp) %>%
    labelCl(class_labels=c(
        `2`='Low',
        `1`='Decreasing low',
        `3`='Increasing',
        `4`='Increasing high'
    ), joined_cl=.)

# Rename class from each LCMM model, join together
lcmm_imp_dflist <- list(
    ext_n2_spl4q=ext_n2_spl4q, ext_n4_spl4q=ext_n4_spl4q,
    int_n2_spl4q=int_n2_spl4q, int_n3_spl3q=int_n3_spl3q, int_n4_spl4q=int_n4_spl4q
)


############################################
cat('...saving datasets...')
# Save selected lcmm model data with predicted and labeled clusters
saveRDS(lcmm_imp_dflist, here('data/lcmm_imp_dflist.rds'))
cat('\nfin!')
