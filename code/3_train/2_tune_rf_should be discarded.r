############################################
# Tune randomForest hyperparameters

############################################
# Libraries

library(here)
library(tidyverse)
library(caret)
library(randomForest)

source(here('code/library/functions.r'))

############################################
# Data

# demographic variables (for group summary stats and modeling) come from code/library/functions_model.r

# selected models
ext_cl2 <- readRDS(here('inputs/lcmm_select/ext_n2_spl4q.rds'))
int_cl2 <- readRDS(here('inputs/lcmm_select/int_n2_spl4q.rds'))

xy_ext_cl2 <- ext_cl2$bl %>%
    select(class_lbl, all_of(demvars)) %>%
    filter(across(everything(), ~!is.na(.)))
xy_int_cl2 <-int_cl2$bl %>%
    select(class_lbl, all_of(demvars)) %>%
    filter(across(everything(), ~!is.na(.)))


# Setup repeated-CV to tune mtry, ntree

tune_ntree <- list(nt100=100, nt500=500, nt1000=1000)

# train controls
control <- trainControl(
    method='repeatedcv',
    number=10,
    repeats=5,
    search='grid'
)

# hyperparameter grid search
tunegrid <- expand.grid(mtry=c(1:16))

# tune hyperparameters
set.seed(9)
ext_rf_gridsearch <- map(tune_ntree, function(nt) {
    train(
        class_lbl ~ .,
        data=xy_ext_cl2,
        ntree=nt,
        method='rf',
        metric='Kappa',
        tuneGrid=tunegrid,
        trControl=control
    )
})

set.seed(9)
int_rf_gridsearch <- map(tune_ntree, function(int) {
    train(
        class_lbl ~ .,
        data=xy_int_cl2,
        method='rf',
        metric='Kappa',
        tuneGrid=tunegrid,
        trControl=control
    )
})

############################################
# Save models
saveRDS(ext_rf_gridsearch, here('models/rf/ext_n2_spl4q_gridsearch.rds'))
saveRDS(int_rf_gridsearch, here('models/rf/int_n2_spl4q_gridsearch.rds'))


map(ext_rf_gridsearch, function(gs) {
    as_tibble(gs$results)
}) %>%
    bind_rows(.id='ntree') %>%
    arrange(desc(Accuracy), desc(Kappa))


map(int_rf_gridsearch, function(gs) {
    as_tibble(gs$results)
}) %>%
    bind_rows(.id='ntree') %>%
    arrange(desc(Accuracy), desc(Kappa))
