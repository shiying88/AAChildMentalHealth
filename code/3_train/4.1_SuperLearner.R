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
library(kernlab)

source(here('code/library/functions.r'))
############################################
# Data
bl_origin <- readRDS(here('data/bl.rds'))
mdata_imp_origin <- readRDS(here('data/lcmm_mdata_imp.rds'))
# lcmm_mdata_imp: longitudinal dataset (AAPI) w/ LOCF imputation of internal/external social behavior problem measures, for clustering analysis

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

# save dataset
saveRDS(ext_n2_spl4q, here('data/df_ext_n2_spl4q.rds'))
saveRDS(int_n2_spl4q, here('data/df_int_n2_spl4q.rds'))

############################################
# Model fitting
# Pre-processing

# ext n=2 spl4q
xy_ext_n2_spl4q <- prepMdata(ext_n2_spl4q$bl, demvars)
# int n=2 spl4q
xy_int_n2_spl4q <- prepMdata(int_n2_spl4q$bl, demvars)

############################################
# SuperLearner: grouped lasso, full logistic regression, mean prediction
# super-learner and gglasso only allow for gaussian or binomial outcomes

outcomes <- list('ext_n2_spl4q', 'int_n2_spl4q')
names(outcomes) <- outcomes

options(mc.cores=20)
detectCores()


sl_fits <- map(outcomes, function(yvar) {
  y <- get(paste0('xy_', yvar))$y$y_01
  print('length of y')
  print(length(y))
  x <- as.data.frame(get(paste0('xy_', yvar))$x)
  x_grp <- get(paste0('xy_', yvar))$x_grp
  
  
  # SuperLearner wrappers
  
  #' SL.glasso wrapper with specific group ids
  SL.glassoGrp <- function(...) {
    SL.glasso(..., groupid=x_grp)
  }
  
  # SL.randomForest wrapper with mtry from repeated-CV tuning
  SL.randomForest_mtry3_ntree100 <- function(...) {
    SL.randomForest(..., mtry=3, ntree=100)
  }
  
  SL.randomForest_mtry5_ntree100 <- function(...) {
    SL.randomForest(..., mtry=5, ntree=100)
  }
  
  SL.randomForest_mtry7_ntree100 <- function(...) {
    SL.randomForest(..., mtry=7, ntree=100)
  }
  
  SL.randomForest_mtry3_ntree500 <- function(...) {
    SL.randomForest(..., mtry=3, ntree=500)
  }
  
  SL.randomForest_mtry5_ntree500 <- function(...) {
    SL.randomForest(..., mtry=5, ntree=500)
  }
  
  SL.randomForest_mtry7_ntree500 <- function(...) {
    SL.randomForest(..., mtry=7, ntree=500)
  }
  
  SL.randomForest_mtry3_ntree700 <- function(...) {
    SL.randomForest(..., mtry=3, ntree=700)
  }
  
  SL.randomForest_mtry5_ntree700 <- function(...) {
    SL.randomForest(..., mtry=5, ntree=700)
  }
  
  SL.randomForest_mtry7_ntree700 <- function(...) {
    SL.randomForest(..., mtry=7, ntree=700)
  }
  
  SL.glmnetNoStandardize = function(...){
    SL.glmnet(...,standardize=FALSE)
  }
  
  set.seed(2)
  system.time({
    cvSL <- CV.SuperLearner(
      Y=y, X=x,
      SL.library=c('SL.mean', 
                   'SL.glm',
                   'SL.glassoGrp',
                   'SL.glmnetNoStandardize',
                   'SL.randomForest_mtry3_ntree100',
                   'SL.randomForest_mtry3_ntree500',
                   'SL.randomForest_mtry3_ntree700',
                   'SL.randomForest_mtry5_ntree100',
                   'SL.randomForest_mtry5_ntree500',
                   'SL.randomForest_mtry5_ntree700',
                   'SL.randomForest_mtry7_ntree100',
                   'SL.randomForest_mtry7_ntree500',
                   'SL.randomForest_mtry7_ntree700',
                   'SL.ksvm'
      ),
      family=binomial(),
      method='method.AUC',
      cvControl=list(V=10, stratifyCV=TRUE),
      innerCvControl=list(list(V=10, stratifyCV=TRUE)),
      parallel='multicore',
      verbose=TRUE
    )
  })
  
  cvSL
})

# Save fitted SuperLearners
saveRDS(sl_fits$ext_n2_spl4q, here('models/superlearner/ext_n2_spl4q.rds'))
saveRDS(sl_fits$int_n2_spl4q, here('models/superlearner/int_n2_spl4q.rds'))

