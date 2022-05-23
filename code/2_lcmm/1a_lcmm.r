############################################
# LCMM clustering of asian-american students

############################################
# Library

library(here)
library(tidyverse)
library(lcmm)

source(here('code/library/functions.r'))

############################################
# Data

mdata <- readRDS(here('data/lcmm_mdata.rds'))

############################################
# modeling

set.seed(1)

# base single class models
lc1_list <- map(list(int='int', ext='ext'), function(y, df=mdata) {
    # base linear model
    mod_lin <- lcmm(
        as.formula(paste(y, '~ time2')), random=~time2, subject='childkey',
        ng=1, link='linear', data=as.data.frame(df)
    )
    # return list of models
    list(
        lin=mod_lin,
        quad=update(mod_lin, fixed=as.formula(paste(y, '~ time2 + I(time2^2)'))),
        beta=update(mod_lin, link='beta'),
        spl3e=update(mod_lin, link='3-equi-splines'),
        spl4e=update(mod_lin, link='4-equi-splines'),
        spl3q=update(mod_lin, link='3-quant-splines'),
        spl4q=update(mod_lin, link='4-quant-splines')
    )
})

# n=2:4 cluster solutions
lc4_list <- map(list(int='int', ext='ext'), function(y, df=mdata) {
    # return nested list
    map(list(n2=2, n3=3, n4=4), function(n) {
        # base linear model
        mod_lin <- lcmm(
            as.formula(paste(y, '~time2')), random=~time2, mixture=~time2, subject='childkey',
            ng=n, link='linear', data=as.data.frame(df)
        )
        # inner model list
        list(
            lin=mod_lin,
            quad=update(mod_lin, fixed=as.formula(paste(y, '~ time2 + I(time2^2)'))),
            beta=update(mod_lin, link='beta'),
            spl3e=update(mod_lin, link='3-equi-splines'),
            spl4e=update(mod_lin, link='4-equi-splines'),
            spl3q=update(mod_lin, link='3-quant-splines'),
            spl4q=update(mod_lin, link='4-quant-splines')
        )
    })
})

############################################
# Save model lists
saveRDS(lc1_list, here('models/lcmm/lc1_list.rds'))
saveRDS(lc4_list, here('models/lcmm/lc4_list.rds'))
