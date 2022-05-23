############################################
# Prediction of g=2 unsupervised clusters using baseline (kindergarten) covariates

############################################

library(here)
library(tidyverse)
library(gglasso)
library(randomForest)
library(SuperLearner)
library(parallel)
library(vtable)
library(naniar)
library(glmnet)

source(here('code/library/functions.r'))

############################################
# Data
bl_origin <- readRDS(here('data/bl.rds'))
# lcmm_mdata_imp: longitudinal dataset (AAPI) w/ LOCF imputation of internal/external social behavior problem measures, for clustering analysis
mdata_imp_origin <- readRDS(here('data/lcmm_mdata_imp.rds'))

# drop columns with missingness over 30%
bl <- bl_origin %>%
  select(
    -c(p1_pinv11_sc, p1_pinv11,
       p1_read4_sc, p1_read4,
       x1prnapp_sc, x1prnapp,
       x1prnsoc_sc, x1prnsoc,
       x1tchper_sc, x1tchper,
       x1prncon_sc, x1prncon,
       x1tchcon_sc, x1tchcon,
       p1hscale_sc, p1hscale)
  )
mdata_imp <- mdata_imp_origin %>%
  select(
    -c(p1_pinv11_sc, p1_pinv11,
       p1_read4_sc, p1_read4,
       x1prnapp_sc, x1prnapp,
       x1prnsoc_sc, x1prnsoc,
       x1tchper_sc, x1tchper,
       x1prncon_sc, x1prncon,
       x1tchcon_sc, x1tchcon,
       p1hscale_sc, p1hscale)
  )
# Filter wide data to students in modeling datasets
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

########################################
# Random forest (get feature importance)

matrix_ext_n2_spl4q <- ext_n2_spl4q$bl %>%
  select(class_lbl, all_of(demvars)) %>%
  filter(across(everything(), ~!is.na(.)))
df_ext_n2_spl4q <- data.frame(matrix_ext_n2_spl4q)

matrix_int_n2_spl4q <- int_n2_spl4q$bl %>%
  select(class_lbl, all_of(demvars)) %>%
  filter(across(everything(), ~!is.na(.)))
df_int_n2_spl4q <- data.frame(matrix_int_n2_spl4q)

# external
set.seed(9)
rf_ext_n2_spl4q_mtry3_ntree100 <- randomForest(
  class_lbl ~ .,
  data=df_ext_n2_spl4q,
  importance=TRUE,
  mtry=3,
  ntree=100
)

set.seed(9)
rf_ext_n2_spl4q_mtry3_ntree500 <- randomForest(
  class_lbl ~ .,
  data=df_ext_n2_spl4q,
  importance=TRUE,
  mtry=3,
  ntree=500
)

set.seed(9)
rf_ext_n2_spl4q_mtry3_ntree700 <- randomForest(
  class_lbl ~ .,
  data=df_ext_n2_spl4q,
  importance=TRUE,
  mtry=3,
  ntree=700
)

set.seed(9)
rf_ext_n2_spl4q_mtry5_ntree100 <- randomForest(
  class_lbl ~ .,
  data=df_ext_n2_spl4q,
  importance=TRUE,
  mtry=5,
  ntree=100
)

set.seed(9)
rf_ext_n2_spl4q_mtry5_ntree500 <- randomForest(
  class_lbl ~ .,
  data=df_ext_n2_spl4q,
  importance=TRUE,
  mtry=5,
  ntree=500
)

set.seed(9)
rf_ext_n2_spl4q_mtry5_ntree700 <- randomForest(
  class_lbl ~ .,
  data=df_ext_n2_spl4q,
  importance=TRUE,
  mtry=5,
  ntree=700
)

set.seed(9)
rf_ext_n2_spl4q_mtry7_ntree100 <- randomForest(
  class_lbl ~ .,
  data=df_ext_n2_spl4q,
  importance=TRUE,
  mtry=7,
  ntree=100
)

set.seed(9)
rf_ext_n2_spl4q_mtry7_ntree500 <- randomForest(
  class_lbl ~ .,
  data=df_ext_n2_spl4q,
  importance=TRUE,
  mtry=7,
  ntree=500
)

set.seed(9)
rf_ext_n2_spl4q_mtry7_ntree700 <- randomForest(
  class_lbl ~ .,
  data=df_ext_n2_spl4q,
  importance=TRUE,
  mtry=7,
  ntree=700
)

# internal
set.seed(9)
rf_int_n2_spl4q_mtry3_ntree100 <- randomForest(
  class_lbl ~ .,
  data=df_int_n2_spl4q,
  importance=TRUE,
  mtry=3,
  ntree=100
)

set.seed(9)
rf_int_n2_spl4q_mtry3_ntree500 <- randomForest(
  class_lbl ~ .,
  data=df_int_n2_spl4q,
  importance=TRUE,
  mtry=3,
  ntree=500
)

set.seed(9)
rf_int_n2_spl4q_mtry3_ntree700 <- randomForest(
  class_lbl ~ .,
  data=df_int_n2_spl4q,
  importance=TRUE,
  mtry=3,
  ntree=700
)

set.seed(9)
rf_int_n2_spl4q_mtry5_ntree100 <- randomForest(
  class_lbl ~ .,
  data=df_int_n2_spl4q,
  importance=TRUE,
  mtry=5,
  ntree=100
)

set.seed(9)
rf_int_n2_spl4q_mtry5_ntree500 <- randomForest(
  class_lbl ~ .,
  data=df_int_n2_spl4q,
  importance=TRUE,
  mtry=5,
  ntree=500
)

set.seed(9)
rf_int_n2_spl4q_mtry5_ntree700 <- randomForest(
  class_lbl ~ .,
  data=df_int_n2_spl4q,
  importance=TRUE,
  mtry=5,
  ntree=700
)

set.seed(9)
rf_int_n2_spl4q_mtry7_ntree100 <- randomForest(
  class_lbl ~ .,
  data=df_int_n2_spl4q,
  importance=TRUE,
  mtry=7,
  ntree=100
)

set.seed(9)
rf_int_n2_spl4q_mtry7_ntree500 <- randomForest(
  class_lbl ~ .,
  data=df_int_n2_spl4q,
  importance=TRUE,
  mtry=7,
  ntree=500
)

set.seed(9)
rf_int_n2_spl4q_mtry7_ntree700 <- randomForest(
  class_lbl ~ .,
  data=df_int_n2_spl4q,
  importance=TRUE,
  mtry=7,
  ntree=700
)

# external
saveRDS(rf_ext_n2_spl4q_mtry3_ntree100, here('models/rf/rf_ext_n2_spl4q_mtry3_ntree100.rds'))
saveRDS(rf_ext_n2_spl4q_mtry3_ntree500, here('models/rf/rf_ext_n2_spl4q_mtry3_ntree500.rds'))
saveRDS(rf_ext_n2_spl4q_mtry3_ntree700, here('models/rf/rf_ext_n2_spl4q_mtry3_ntree700.rds'))
saveRDS(rf_ext_n2_spl4q_mtry5_ntree100, here('models/rf/rf_ext_n2_spl4q_mtry5_ntree100.rds'))
saveRDS(rf_ext_n2_spl4q_mtry5_ntree500, here('models/rf/rf_ext_n2_spl4q_mtry5_ntree500.rds'))
saveRDS(rf_ext_n2_spl4q_mtry5_ntree700, here('models/rf/rf_ext_n2_spl4q_mtry5_ntree700.rds'))
saveRDS(rf_ext_n2_spl4q_mtry7_ntree100, here('models/rf/rf_ext_n2_spl4q_mtry7_ntree100.rds'))
saveRDS(rf_ext_n2_spl4q_mtry7_ntree500, here('models/rf/rf_ext_n2_spl4q_mtry7_ntree500.rds'))
saveRDS(rf_ext_n2_spl4q_mtry7_ntree700, here('models/rf/rf_ext_n2_spl4q_mtry7_ntree700.rds'))

# internal
saveRDS(rf_int_n2_spl4q_mtry3_ntree100, here('models/rf/rf_int_n2_spl4q_mtry3_ntree100.rds'))
saveRDS(rf_int_n2_spl4q_mtry3_ntree500, here('models/rf/rf_int_n2_spl4q_mtry3_ntree500.rds'))
saveRDS(rf_int_n2_spl4q_mtry3_ntree700, here('models/rf/rf_int_n2_spl4q_mtry3_ntree700.rds'))
saveRDS(rf_int_n2_spl4q_mtry5_ntree100, here('models/rf/rf_int_n2_spl4q_mtry5_ntree100.rds'))
saveRDS(rf_int_n2_spl4q_mtry5_ntree500, here('models/rf/rf_int_n2_spl4q_mtry5_ntree500.rds'))
saveRDS(rf_int_n2_spl4q_mtry5_ntree700, here('models/rf/rf_int_n2_spl4q_mtry5_ntree700.rds'))
saveRDS(rf_int_n2_spl4q_mtry7_ntree100, here('models/rf/rf_int_n2_spl4q_mtry7_ntree100.rds'))
saveRDS(rf_int_n2_spl4q_mtry7_ntree500, here('models/rf/rf_int_n2_spl4q_mtry7_ntree500.rds'))
saveRDS(rf_int_n2_spl4q_mtry7_ntree700, here('models/rf/rf_int_n2_spl4q_mtry7_ntree700.rds'))








