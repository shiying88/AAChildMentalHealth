############################################
# Extract select variables from ECLS-K 2011-2016 data

############################################

library(tidyverse)

eclsk <- readRDS("data/eclsk2011_k5puf.rds")
names(eclsk) <- str_to_lower(names(eclsk))

############################################

#' Look for variable name in eclsk data
#' Usage: interactive helper function
#' 
#' @param name chr: string of variable to lookup
findVar <- function(name, dat = eclsk) {
    dat %>%
        select(matches(!!name))
}


#' Reshape family of variables long
#' 
#' @param cols_pat character: regex for wide variable names to reshape
#' @param names_pat character: regex for pivot_longer
#' @param names_to character list: names for pivot_longer; default is c('survey', 'measure')
#' @return tibble data reshape long, unique childid and survey
reshape_long <- function(cols_pat, names_pat, names_to=c('survey', 'measure')) {
    # get variable prefix (most are x[0-9])
    var_prefix <- str_extract(cols_pat, '^[a-z][0-9]')

    dem %>%
        select(childid, matches(!!cols_pat)) %>%
        pivot_longer(cols=matches(col_start))
}


#' Count missing across variables
#'
#' @param ... unquoted column names
countRowNA <- function(...) {
    eclsk %>%
        select(...) %>%
        mutate_all(~case_when(. %in% -9:-1 ~ NA_real_, TRUE ~ .)) %>%
        mutate(x=rowSums(across(everything(), is.na))) %>%
        count(x)
}



testQuo <- function(...) {
    select(eclsk, ...)
}

############################################
# Pull original wide variables
dem <- eclsk %>%
    select(childid,
        matches("x[1-9]k{0,1}age(_r){0,1}$"),
        x_chsex_r,
        x1firkdg,
        matches("x_(hisp|white|black|asian|aminan|hawpi|multr)_r"),
        x_raceth_r, x_racethp_r,
        matches("x[1-9]height"),
        matches("x[1-9]weight"),
        matches("x[1-9]bmi"),
        matches("x[246789]disabl"),
        # Family/Household characteristics
        matches("x(12|[469])langst"),
        matches("x(12|[4789])par[12]ed"),
        matches("x[1469]par[12]emp"),
        matches("x[1469]par[12]occ"),
        matches("x[246789]inccat_i"),
        matches("x[246789]povty(?:_i)?$"),
        matches("x(12|[49])sesl(_i)?$"),
        matches('^x[1246-9](htotal|numsib)$'),
        matches('^x[249]fsstat2$'),
        # Child reported social topics
        matches("^c[789](teased|liesabt|pushch|excldch)$"),
        matches("^c[789](wrythk|wrydtlk|afrdntalk)$"),
        # Teacher reported social skills
        matches("x[1-9]tchcon"),
        matches("x[1-9]tchper"),
        matches("x[1-9]tchext"),
        matches("x[1-9]tchint"),
        # Teacher reported on peer relationships (item-level, no composite score)
            # peer victimization (child as victim)
        matches('^(t[67]|g[89])os(teas|lies|push|lfto)$'),
            # peer vicitimization (child as aggressor)
        matches('^(t[67]|g[89])ts(teas|lies|push|lfto)$'),
            # excluded by peers
        matches('^(t7|g[89])(plymte|pavoid|exclued|ignred|)$'),
            # prosocial with peers
        matches('^(t7|g[89])(tdist|skind|coprtv|cnmorl|hlpups)$'),
            # positive peer group
        matches('^g[89](goodgp|worygp|badinf|supvis|trblgp|excstu|hrdwkr|fungrp|kindgp)$'),
            # social skills with peers 
        matches('g[89](undfel|intper|solint|efebev)$'),
        # Parent reported social skills
        matches("x[124]prncon"),
        matches("x[124]prnsoc"),
        matches("x[124]prnsad"),
        matches("x[124]prnimp"),
        # Parent reported on peer relationships (item-level, no composite score)
            # peer victimization (child as victim)
        matches('^p[67]oth(tea|lie|hit|exc)$'),
        matches('^p[67]oft(tea|lie|hit|exc)$'),
            # number of close friends
        matches('^p[89]numfrd$'),
            # influence of best friend
        matches('^p[89]frinfl$'),
        # Reading/Math/Science Scaled scores
        matches("x[1-9][rms]scalk5$"),
        # Parent's age
        matches('^x[1246-9]par[12]age(_r)?$'),
        x1primnw, x1basc,
        # community support
        s4spprt,
        # child attention/learning behaviors: parent report approaches to learning, teacher report approaches to learning, teacher report attentional focus
        x1prnapp, x1tchapp, x1attnfs,
        # child social behaviors: parent report social interaction, teacher report interpersonal
        x1prnsoc, x1tchper,
        # child behavioral regulation: parent report self-control, teacher report self-control, teacher report inhibitory control
        x1prncon, x1tchcon, x1inbcnt,
        # child health
        p1hscale,
        # variables for creating additional scales
        p1tellst,p1singso,p1hlpart,p1chores,p1games,p1nature,p1build,p1sport,p1numbrs,p1readbk,p1picbks,
        p1tellst, p1numbrs, p1readbk, p1picbks,
        p2howchd, p2childr, p2chancv, p2hlplrn, p2commun,
        s4theft, s4conflc, s4vandal, s4bully,
        p2ethnic, p2relig
    ) %>%
    # set missing, refuse, don't response codes to NA
    mutate(across(where(~is.numeric(.)), ~case_when(. %in% c(-7:-9, -1) ~ NA_real_, TRUE ~ .))) %>%
    mutate(
        # Child care 
        x1primnw=factor(x1primnw, levels=0:8, labels=c('No arrangement', 'Relative, at home', 'Relative, another home', 'Relative, loc varies', 'Nonrelative, home', 'Nonrelative, another home', 'Nonrelative, loc varies', 'Center-based program', 'Two+ types')),
        x1basc=factor(x1basc, levels=1:3, labels=c('Relative', 'Nonrelative', 'Center-based')),
        # home parent-child interaction
        p1_pinv11=rowMeans(across(c(p1tellst,p1singso,p1hlpart,p1chores,p1games,p1nature,p1build,p1sport,p1numbrs,p1readbk,p1picbks))),
        # reading together
        p1_read4=rowMeans(across(c(p1tellst, p1numbrs, p1readbk, p1picbks))),
        # school-home connection
        p2_hscon5=rowMeans(across(c(p2howchd, p2childr, p2chancv, p2hlplrn, p2commun))),
        # community violence
        com_violence=rowMeans(across(c(s4theft, s4conflc, s4vandal, s4bully))),
        # culture heritage
        p2_cul2=rowMeans(across(c(p2ethnic, p2relig)))
    )
    

############################################
# Reshape variables long
if (interactive()) {
    dem %>%
        select(childid, matches('x[1-9]k{0,1}age(_r){0,1}$')) %>%
        pivot_longer(cols=matches('^x'), names_pattern='^(x[1-9])k?(age)', names_to=c('survey', 'measure'), values_to='val') %>%
        pivot_wider(id_cols=c(childid, survey), names_from=measure, values_from=val)
}

############################################
# Save
saveRDS(dem, "data/eclsk2011_dem.rds")
