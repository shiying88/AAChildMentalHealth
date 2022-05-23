############################################
# Univariate predictions of LCMM (imputed) classes

############################################

library(here)
library(tidyverse)
library(nnet)

source(here('code/library/functions_model.r'))

############################################
# Data, lcmm imputed prediction data
lcmm_imp_dflist <- readRDS(here('data/lcmm_imp_dflist.rds'))
list2env(lcmm_imp_dflist, envir=.GlobalEnv)

# Model fitting

# manually set reference levels for model outcomes
############################################
set.seed(2)

# ext n2 spl4q - 1: low, 2: high
binom_ext_n2_spl4q <- map(setNames(demvars, demvars), function(x) {
    glm(as.formula(paste('class_lbl ~', x)), family=binomial(link='logit'), data=ext_n2_spl4q$bl)
})

# ext n4 spl4q - 1: increase, 2: low, 3: decrease, 4: high
multinom_ext_n4_spl4q <- map(setNames(demvars, demvars), function(x) {
    multinom(as.formula(paste('class_lbl ~', x)), data=ext_n4_spl4q$bl)
})

# int g2 spl3q - 1:high, 2:low
binom_int_n2_spl4q <- map(setNames(demvars, demvars), function(x) {
    glm(as.formula(paste('class_lbl ~', x)), family=binomial(link='logit'), data=int_n2_spl4q$bl)
})

# int n3 spl3q - 1: med, 2: low, 3: high
multinom_int_n3_spl3q <- map(setNames(demvars, demvars), function(x) {
    multinom(as.formula(paste('class_lbl ~', x)), data=int_n3_spl3q$bl)
})

# int n4 spl4q - 1: med, 2: low, 3: increase1, 4: increase2
multinom_int_n4_spl4q <- map(setNames(demvars, demvars), function(x) {
    multinom(as.formula(paste('class_lbl ~', x)), data=int_n4_spl4q$bl)
})

############################################
# Save
cat('...saving fitted models...')
saveRDS(binom_ext_n2_spl4q, here('models/univariate/binom_ext_n2_spl4q.rds'))
saveRDS(multinom_ext_n4_spl4q, here('models/univariate/multinom_ext_n4_spl4q.rds'))
saveRDS(binom_int_n2_spl4q, here('models/univariate/binom_int_n2_spl4q.rds'))
saveRDS(multinom_int_n3_spl3q, here('models/univariate/multinom_int_n3_spl3q.rds'))
saveRDS(multinom_int_n4_spl4q, here('models/univariate/multinom_int_n4_spl4q.rds'))
cat('\nfin!')
