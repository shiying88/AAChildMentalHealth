############################################
# Read in extracted raw ECLS-K data
# Clean and format for analysis:
#  1. bl.rds: "baseline" dataset (i.e. kindergarten) variables
#  2. lcmm_mdata.rds: longitudinal dataset (AAPI) for LCMM clustering analysis
#  3. lcmm_mdata_imp: longitudinal dataset (AAPI) w/ LOCF imputation of internal/external social behavior problem measures, for clustering analysis

############################################
# Libraries

library(here)
library(tidyverse)

source(here('code/library/functions.r'))

############################################
# Data

dem <- readRDS(here('data/eclsk2011_dem.rds'))
# full dataset for LCMM analysis
full <- dem %>%
    formatRaceth() %>%
    arrange(childid) %>%
    group_by(childid) %>%
        mutate(childkey=as.numeric(childid)) %>%
        ungroup() %>%
    select(childid, childkey, raceth, matches('tchint'), matches('tchext'))
    # tchint: teacher report internal prob behaviors; tchext: teacher report external prob behaviors

# check childkey is unique as childid
if (n_distinct(full$childkey) != n_distinct(full$childid)) {
    stop('!CHILDKEY and CHILDID are not mutually unique!')
}

# baseline characteristics
bl <- dem %>%
    formatRaceth() %>%
    mutate(
        female=factor(x_chsex_r==2, levels=c(F,T)),
        disabl=factor(x2disabl==1, levels=c(F,T)),
        first_yr_kg=factor(x1firkdg==1, levels=c(F,T)),
        langst=factor(x12langst, levels=1:3,
            labels=c('Non-english', 'English', 'English + other')
        ),
        langst2=factor(x12langst, levels=1:3,
            labels=c('Non-english', 'English', 'English')
        ),
        paredu=pmax(x12par1ed_i, x12par2ed_i, na.rm=TRUE),
        paredu=factor(paredu, levels=1:8,
            labels=c('lths', 'lths', 'hs', 'some college', 'some college', 'bs', 'some graduate', 'ms or higher')
        ),
        hhinc=factor(x2inccat_i, levels=1:18,
            labels=c(
                "5K or less",
                "(5K, 10K]",
                "(10K, 15K]",
                "(15K, 20K]",
                "(20K, 25K]",
                "(25K, 30K]",
                "(30K, 35K]",
                "(35K, 40K]",
                "(40K, 45K]",
                "(45K, 50K]",
                "(50K, 55K]",
                "(55K, 60K]",
                "(60K, 65K]",
                "(65K, 70K]",
                "(70K, 75K]",
                "(75K, 100K]",
                "(100K, 200K]",
                "200K or more"
            )
        ),
        foodsec=factor(x2fsstat2, levels=1:3, labels=c('Secure', 'Low', 'Very low')),
        foodsec2=fct_recode(foodsec, 'Low'='Very low'),
        # highest level completed education
        paredu2=factor(case_when(
            paredu=='lths' ~ 'lths',
            paredu %in% c('hs', 'some college') ~ 'hs',
            paredu %in% c('bs', 'some graduate') ~ 'college',
            paredu=='ms or higher' ~ 'graduate'
            ), levels=c('lths', 'hs', 'college', 'graduate')
        ),
        # collate income levels
        hhinc2=factor(case_when(
            hhinc %in% c("5K or less", "(5K, 10K]", "(10K, 15K]", "(15K, 20K]") ~ '[0,20K]',
            hhinc %in% c("(20K, 25K]", "(25K, 30K]", "(30K, 35K]", "(35K, 40K]", "(40K, 45K]") ~ '(20K,45K]',
            hhinc %in% c("(45K, 50K]", "(50K, 55K]", "(55K, 60K]", "(60K, 65K]", "(65K, 70K]", "(70K, 75K]", "(75K, 100K]") ~ '(45K,100K]',
            hhinc %in% "(100K, 200K]" ~ '(100K,200K]',
            hhinc=='200K or more' ~ '(200K,+)'
            ), levels=c('[0,20K]', '(20K,45K]', '(45K,100K]', '(100K,200K]', '(200K,+)')
        ),
        # household number
        hhnum=factor(case_when(
          x2htotal == 1 ~ '1',
          x2htotal == 2 ~ '2',
          x2htotal == 3 ~ '3',
          x2htotal == 4 ~ '4',
          x2htotal == 5 ~ '5',
          x2htotal == 6 ~ '6',
          x2htotal >= 7 ~ '7 and above'
          ), levels=c('1', '2', '3', '4', '5', '6', '7 and above')
        ),
        # number of siblings
        siblings=factor(case_when(
          x2numsib == 0 ~ '0',
          x2numsib == 1 ~ '1',
          x2numsib == 2 ~ '2',
          x2numsib == 3 ~ '3',
          x2numsib >= 4 ~ '4 and above',
          ), levels=c('0', '1', '2', '3', '4 and above')
        ),
        childcare2=factor(case_when(
            x1primnw=='No arrangement' ~ 'Other',
            x1primnw %in% c('Relative, at home', 'Relative, another home', 'Relative, loc varies') ~ 'Relative',
            x1primnw %in% c('Nonrelative, home', 'Nonrelative, another home', 'Nonrelative, loc varies') ~ 'Other',
            x1primnw=='Center-based program' ~ 'Other',
            x1primnw=='Two+ types' ~ 'Other'
            ), levels=c('None', 'Other', 'Relative')
        ),
        # teacher rated self-control
        tchcon=x2tchcon,
        # teacher rated interpersonal skils
        tchper=x2tchper
    ) %>%
    # Create explicit NA levels for all factors (except female)
    mutate(
      across(
        where(is.factor) & -c(female),
        fct_explicit_na,
        na_level='na'
      )
    ) %>%
    select(
        childid, raceth,
        age_months=x2kage_r, female, bmi=x2bmi, disabl, first_yr_kg,
        read=x2rscalk5, math=x2mscalk5, sci=x2sscalk5,
        langst, langst2, par1age=x2par1age, par2age=x2par2age, paredu, paredu2,
        hhinc, hhinc2, ses=x12sesl, foodsec, foodsec2,
        # hhnum=x2htotal, siblings=x2numsib,
        hhnum, siblings,
        childcare=x1primnw, childcare2, x1basc,

        s4spprt,
        # com_support=s4spprt,

        # child attention/learning behaviors: parent report approaches to learning, teacher report approaches to learning, teacher report attentional focus
        x1prnapp, x1tchapp, x1attnfs,
        # appro_learn_p=x1prnapp, appro_learn_t=x1tchapp, focus_t=x1attnfs,

        # child social behaviors: parent report social interaction, teacher report interpersonal
        x1prnsoc, x1tchper,
        # social_p=x1prnsoc, social_t=x1tchper,

        # child behavioral regulation: parent report self-control, teacher report self-control, teacher report inhibitory control
        x1prncon, x1tchcon, x1inbcnt,
        # self_con_p=x1prncon, self_con_t=x1tchcon, inhb_con_t=x1inbcnt,

        # child health
        p1hscale,
        # health=p1hscale,

        # Created scales (from Yen)
        p1_pinv11, p1_read4, p2_hscon5, com_violence, p2_cul2
        # h_inter=p1_pinv11, h_read=p1_read4, h_s_con=p2_hscon5, com_violence, cul_heritage=p2_cul2
    ) %>%
    group_by(raceth) %>%
      # mean imputation
      mutate(
        age_months = impute.mean(age_months),
        par1age = impute.mean(par1age), # parent 1 age (new)
        par2age = impute.mean(par2age), # parent 2 age (new)
        s4spprt = impute.mean(s4spprt), # community support (new)
        bmi = impute.mean(bmi),
        read = impute.mean(read),
        math = impute.mean(math),
        sci = impute.mean(sci),
        com_violence = impute.mean(com_violence),
        ses = impute.mean(ses),
        p2_cul2 = impute.mean(p2_cul2), # cultural heritage
        p2_hscon5 = impute.mean(p2_hscon5), # school-home connection
        x1tchapp = impute.mean(x1tchapp), # appraoch to learning (teacher)
        disabl = impute.mean(disabl), # disability
        x1inbcnt = impute.mean(x1inbcnt), # inhibitory control
        x1attnfs = impute.mean(x1attnfs) # attention focus
      ) %>%
    ungroup() %>%
    # normalize continuous variables
    mutate(across(c('par1age', 'par2age', 's4spprt', 'x1prnapp', 'x1tchapp', 'x1attnfs', 'x1prnsoc', 'x1tchper', 'x1prncon', 'x1tchcon', 'x1inbcnt', 'p1hscale', 'p1_pinv11', 'p1_read4', 'p2_hscon5', 'com_violence', 'p2_cul2', 'age_months', 'bmi', 'read', 'math', 'sci'), ~scale(., center=TRUE, scale=TRUE)[,1], .names='{.col}_sc')) %>%
    left_join(distinct(full, childid, childkey), by='childid')

# list
mdata_list <- list(
    # Full sample
    full=full,
    # Completers across both scales
    completers=filter(full, across(everything(), ~!is.na(.))),
    # Completers (internalizing score)
    completers_int=filter(full, across(matches('tchint'), ~!is.na(.))),
    # Completers (externalizing score)
    completers_ext=filter(full, across(matches('tchext'), ~!is.na(.)))
)
# pivot data long, join baseline covariates
mdata_long_list <- map(mdata_list, function(df) {
    df %>%
        pivot_longer(cols=matches('tch(int|ext)$'), names_sep='tch', names_to=c('survey', 'type'), values_to='score') %>%
        formatSurvey(grade=TRUE, order=TRUE) %>%
        mutate(
            time=case_when(
                survey=='K F' ~ 0,
                survey=='K S' ~ 0.5,
                survey=='1 F' ~ 1,
                survey=='1 S' ~ 1.5,
                survey=='2 F' ~ 2,
                survey=='2 S' ~ 2.5,
                survey=='3 S' ~ 3.5,
                survey=='4 S' ~ 4.5,
                survey=='5 S' ~ 5.5
            ),
            time2=case_when(
                survey=='K F' ~ NA_real_,
                survey=='K S' ~ 0,
                survey=='1 F' ~ NA_real_,
                survey=='1 S' ~ 1,
                survey=='2 F' ~ NA_real_,
                survey=='2 S' ~ 2,
                survey=='3 S' ~ 3,
                survey=='4 S' ~ 4,
                survey=='5 S' ~ 5
            )
        ) %>%
        pivot_wider(id_cols=c(childid, childkey, raceth, survey, time, time2), names_from='type', values_from='score') %>%
    left_join(bl, by=c('childid', 'childkey', 'raceth'))
})


# drop measurements in fall of 1st and 2nd grade (over 70% missingness)
# keep all children w/ >= 2 measurements in both scores for lcmm modeling
lcmm_mdata <- mdata_long_list$full %>%
    filter(raceth=='Asian') %>%
    filter(!is.na(time2)) %>%
    group_by(childid) %>%
        filter(sum(!is.na(int))>=2, sum(!is.na(ext))>=2) %>%
        ungroup()

# impute data: carryforward fall measurement (K, 1st, 2nd grades) if available
# drop measurements in fall of 1st and 2nd grade (over 70% missingness)
# keep all children w/ >= 2 measurements in both scores for lcmm modeling
lcmm_mdata_imp <- mdata_long_list$full %>%
    filter(raceth=='Asian') %>%
    mutate(grade=str_remove(survey, ' [FS]$')) %>%
    group_by(childid, grade) %>%
        arrange(time) %>%
        fill(int, ext, .direction='down') %>%
        ungroup() %>%
    arrange(childid, survey) %>%
    filter(!is.na(time2)) %>%
    group_by(childid) %>%
        filter(sum(!is.na(int))>=2, sum(!is.na(ext))>=2) %>%
        ungroup()


############################################
# Save
cat('...saving datasets...')
saveRDS(bl, file=here('data/bl.rds'))
saveRDS(lcmm_mdata, here('data/lcmm_mdata.rds'))
saveRDS(lcmm_mdata_imp, here('data/lcmm_mdata_imp.rds'))
cat('\nfin!')
