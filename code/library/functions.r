############################################
# Functions

############################################

library(here)

source(here('code/library/functions_prep.r'))
source(here('code/library/functions_model.r'))


# IMPUTE (mean)
impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))


# PLOT FUNCTIONS
############################################


#' Plot model classified observed external/internal scores
#' 
#' @param tchtype chr ['ext', 'int']
#' @param joined_cl joinCl()
#' @param class_var character: ['class', 'class_lbl']
#' @param covs character vector of covariates for completers
plotClTrajs <- function(tchtype, joined_cl, class_var='class_lbl', covs=NULL) {
    if (!tchtype %in% c('ext', 'int')) {
        stop('tchtype must be one of {ext, int}')
    }
    df <- joined_cl$mdata
    labs1 <- labs(x='Time', y='Score', title=paste('Observed', ifelse(tchtype=='ext', 'External', 'Internal'), 'problem behavior score'), subtitle=paste0('n=', n_distinct(df$childid)))
    if (!is.null(covs)) {
        if (any(!covs %in% names(df))) {
            stop('one or more covariates not in baseline data!')
        }
        df <- filter(df, across(all_of(covs), ~!is.na(.)))
        labs1 <- labs(x='Time', y='Score', title=paste('Observed', ifelse(tchtype=='ext', 'External', 'Internal'), 'problem behavior score'), subtitle=paste0('Completers n=', n_distinct(df$childid)))        
    }
    p1 <- ggplot(df, aes(time2, get(tchtype), col=get(class_var), group=childid)) +
        geom_point(size=1, alpha=0.3) +
        geom_line(alpha=0.2) +
        geom_smooth(aes(group=get(class_var)), size=1.4, col='black', method='loess', formula=y~x, se=FALSE) +
        geom_smooth(aes(group=get(class_var)), method='loess', formula=y~x, se=FALSE) +
        # scale_color_discrete('Class') +
        scale_color_brewer(name='Class', type='qual', palette='Set1') +
        labs1 +
        guides(col='none')
    p2 <- df %>%
        group_by(!!rlang::sym(class_var), time2) %>%
            summarise(across(!!rlang::sym(tchtype),
                list(
                    m=~mean(., na.rm=T),
                    se=~sd(boot(., statistic=sampleMean, R=1e3)$t)
                ), .names='{fn}')
            ) %>%
        mutate(
            cil=m - 1.96 * se,
            ciu=m + 1.96 * se
        ) %>%
    ggplot(aes(time2, m, col=get(class_var))) +
        geom_ribbon(aes(ymin=cil, ymax=ciu), alpha=0.3) +
        geom_line() +
        scale_y_continuous(limits=c(1,4)) +
        # scale_color_discrete('Class') +
        scale_color_brewer(name='Class', type='qual', palette='Set1') +
        labs(x='Time', y='Score', title=paste('Group mean', ifelse(tchtype=='ext', 'External', 'Internal'), 'problem behavior score'), caption='bootstrapped 95%CI')

    return(p1+p2)
}


#' Format raceth fill/col
#' @param type chr: "fill" or "color"
#' @param asian logical: group by asian or not
scale_raceth <- function(type) {
    name = "Race/Ethnicity"
    values = c(
        "White" = "#8DD3C7",
        "Black/African-American" = "#FFFFB3",
        "Hispanic" = "#FB8072",
        "Asian" = "#80B1D3",
        "American Indian/Alaska Native" = "#B3DE69",
        "Multiracial" = "#FCCDE5",
        "Not ascertained" = "#D9D9D9")
    if (type == "fill") {
        scale_fill_manual(name = name, values = values)
    } else if (type == "color") {
        scale_color_manual(name = name, values = values)
    }
}


#' Format raceth fill/col
#' @param type chr: "fill" or "color"
#' @param asian logical: group by asian or not
scale_raceth_old <- function(type, asian = FALSE) {
    name = "Race/Ethnicity"
    if (asian) {
        values = c("Asian" = "salmon4", "All others" = "black")
        breaks = c("Asian", "All others")
    } else {
        values = c(
            "White, non-hisp" = "#8DD3C7",
            "Black/African-American, non-hisp" = "#FFFFB3",
            "Hispanic, race specified" = "#BEBADA",
            "Hispanic, no race specified" = "#FB8072",
            "Asian, non-hisp" = "#80B1D3",
            "Native Hawaiian/Pacific Isl., non-hisp" = "#FDB462",
            "American Indian/Alaska Native, non-hisp" = "#B3DE69",
            "Two or more races, non-hisp" = "#FCCDE5",
            "Not ascertained" = "#D9D9D9")
        breaks = c(
            "White, non-hisp",
            "Black/African-American, non-hisp",
            "Hispanic, race specified",
            "Hispanic, no race specified",
            "Asian, non-hisp",
            "Native Hawaiian/Pacific Isl., non-hisp",
            "American Indian/Alaska Native, non-hisp",
            "Two or more races, non-hisp",
            "Not ascertained")
    }
    if (type == "fill") {
        scale_fill_manual(name = name, values = values, breaks = breaks)
    } else if (type == "color") {
        scale_color_manual(name = name, values = values, breaks = breaks)
    }
}




# STATISTICAL FUNCTIONS
############################################


#' Create aAge-, sex- specific BMI classification
#' Source CDC: https://www.cdc.gov/obesity/childhood/defining.html
#' dem.rds needs to be in environment
#' 
#' @return tibble: long data with childids and bmi age-, sex- adjusted BMI categories
calcBMICat <- function() {
    if (!exists('dem')) {
        stop('\'dem\' object does not exist in environment')
    }
    if (!is.data.frame(dem)) {
        stop('\'dem\' object is not a data.frame or tibble')
    }
    dem %>%
        select(childid, x_chsex_r, matches("x[1-9]k{0,1}age(_r){0,1}$"), matches('^x[246-9]bmi$')) %>%
        rename(sex=x_chsex_r) %>%
        # reshape age and bmi long
        pivot_longer(cols=matches('^x[2-9]'), names_pattern='^(x[1-9])k?(age|bmi)', names_to=c('survey',  'measure'), values_to='val') %>%
        pivot_wider(id_cols=c(childid, sex, survey), names_from=measure, values_from=val) %>%
        mapSurveyYear() %>%
        # create age in years
        mutate(age_year=round(age/12)) %>%
        group_by(age_year, sex) %>%
            mutate(
                bmi_pc5=quantile(bmi, probs=0.05, na.rm=T, names=F),
                bmi_pc85=quantile(bmi, probs=0.85, na.rm=T, names=F),
                bmi_pc95=quantile(bmi, probs=0.95, na.rm=T, names=F)
            ) %>%
            ungroup() %>%
        # create age-, sex- child BMI categories
        mutate(bmi_cat=factor(case_when(
            bmi < bmi_pc5 ~ 'Underweight',
            bmi >= bmi_pc5 & bmi < bmi_pc85 ~ 'Healthy',
            bmi >= bmi_pc85 & bmi < bmi_pc95 ~ 'Overweight',
            bmi >= bmi_pc95 ~ 'Obsese'),
            levels=c('Underweight', 'Healthy', 'Overweight', 'Obese')
        )) %>%
        select(childid, survey, bmi_cat)
}

#' Summary stats and tests by class
#' 
#' @param xvars list chr: name of variables in data
#' @param xnames list chr: name of variables for labels
#' @param dflist output of joinCl()
#' @return tibble
classStat <- function(xvars, xnames, dflist) {
    df <- dflist$bl
    names(xvars) <- xnames
    # map through xvars
    map(xvars, function(x) {
        if (is.numeric(pull(df, !!x))) {
        # Continuous variable
            df %>%
                group_by(class_lbl) %>%
                    summarise(across(!!x, ~sprintf('%.2f (%.2f)', mean(., na.rm=T), sd(., na.rm=T))), .groups='drop') %>%
                pivot_wider(names_from=class_lbl, values_from=!!x)
        } else {
            if (is.factor(pull(df, !!x)) & length(levels(pull(df, !!x)))>2) {
            # Multi-level factor
                df %>%
                    group_by(class_lbl) %>%
                        count(!!rlang::sym(x)) %>%
                        mutate(val=sprintf('%.0f (%.1f%%)', n, n / sum(n) * 100)) %>%
                        ungroup() %>%
                    select(class_lbl, !!x, val) %>%
                    pivot_wider(names_from=class_lbl, values_from=val, values_fill='') %>%
                    rename_with(~paste('x_level'), .cols=!!x) %>%
                    mutate(x_level=replace_na(as.character(x_level), 'NA'))
            } else {
            # Binary
                df %>%
                    group_by(class_lbl) %>%
                        summarise(
                            n=sum(!!rlang::sym(x)==TRUE, na.rm=T),
                            pc=n * 100 / sum(!is.na(!!rlang::sym(x))),
                            .groups='drop'
                        ) %>%
                    mutate(val=sprintf('%.0f (%.1f%%)', n, pc)) %>%
                    select(class_lbl, val) %>%
                    pivot_wider(names_from=class_lbl, values_from=val)
            }
        }
    }) %>%
        bind_rows(.id='x') %>%
        mutate(x=case_when(!is.na(x_level) ~ paste(x, x_level, sep=': '), TRUE ~ x)) %>%
        select(-x_level)
}


#' Sample mean function for boot::boot()
#' 
#' @param x numeric: vector of data
#' @param i numeric: vector of indices
sampleMean <- function(x, i) {
    return(mean(x[i], na.rm=T))
}


#' Tabulate categoricals with n (%)
#' 
#' @param xvar chr: name of categorical variable to tabulate
#' @param dat data.frame
tableCat <- function(xvar, dat = dem) {
    dat %>%
        select(!!xvar)
}


#' wald test for multinomial logistic model
#' 
#' @param mod nnet::multinom model
waldTest <- function(mod) {
    z <- summary(mod)$coefficient / summary(mod)$standard.error
    (1 - pnorm(abs(z), 0, 1)) * 2
}

