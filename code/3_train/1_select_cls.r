############################################
# Select LCMM models for outcome classes in predictive modeling
#   Re-label arbitrary numeric class labels into meaningful labels
#   Save as inputs for predictive modeling steps

############################################
# Libraries

library(here)
library(tidyverse)

source(here('code/library/functions.r'))
############################################

# Data
bl <- readRDS(here('data/bl.rds'))
mdata_imp <- readRDS(here('data/lcmm_mdata_imp.rds'))
# Subset baseline predictors to those in LCMM analysis
mdata_imp_bl <- bl %>%
    filter(childid %in% unique(mdata_imp$childid))

# LCMM models
lc4_list_imp <- readRDS(here('models/lcmm/lc4_list_imp.rds'))

# selected best modesl by BIC


ext_n2_spl4q <- joinCl('ext', n=2, model='spl4q', mlist=lc4_list_imp, .bl=bl, df_long=mdata_imp) %>%
    labelCl(class_labels=c(
        `2`='Low',
        `1`='High'
    ), joined_cl=.)

int_n2_spl4q <- joinCl('int', n=2, model='spl4q', mlist=lc4_list_imp, .bl=bl, df_long=mdata_imp) %>%
    labelCl(class_labels=c(
        `2`='Low',
        `1`='High'
    ), joined_cl=.)

# Create full data for prediction analysis (everything should be the same...)
pdata <- full_join(
    rename_with(ext_n2_spl4q$bl, ~paste0(., '_ext'), .cols=c('class', 'class_lbl', 'prob1', 'prob2')),
    rename_with(int_n2_spl4q$bl, ~paste0(., '_int'), .cols=c('class', 'class_lbl', 'prob1', 'prob2'))
)

############################################
# Save

saveRDS(pdata, here('data/pdata.rds'))
