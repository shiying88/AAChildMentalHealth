############################################
# FUNCTIONS: modeling

############################################

demvars <- c(
    'Age (scaled)'='age_months_sc',
    'Female'='female',
    'BMI (scaled)'='bmi_sc',
    'Disability'='disabl',
    '1st yr in kindergarten'='first_yr_kg',
    'Reading score (scaled)'='read_sc',
    'Math score (scaled)'='math_sc',
    'Science score (scaled)'='sci_sc',
    'Lang at home'='langst2',
    'Parent edu'='paredu2',
    'HH income'='hhinc2',
    'SES'='ses',
    'Food security'='foodsec2',
    'HH members'='hhnum',
    'Siblings'='siblings',
    'Childcare'='childcare2',
    # Created scales (from Yen)
    # 'Home parent-child int. (scaled)'='p1_pinv11_sc', # missingness > 30%
    # 'Reading together (scaled)'='p1_read4_sc', # missingness > 30%
    'School-home connection (scaled)'='p2_hscon5_sc',
    'Community violence (scaled)'='com_violence_sc',
    'Cultural heritage (scaled)'='p2_cul2_sc',
    # Additional predictors (from Yen)
    # 'Approach learning - parent (scaled)'='x1prnapp_sc', # missingness > 30%
    'Approach learning - teacher (scaled)'='x1tchapp_sc',
    'Attentional focus (scaled)'='x1attnfs_sc',
    # 'Social interaction (scaled)'='x1prnsoc_sc', # missingness > 30%
    # 'Interpersonal (scaled)'='x1tchper_sc', # missingness > 30%
    # 'Self-control - parent (scaled)'='x1prncon_sc', # missingness > 30%
    # 'Self-control - teacher (scaled)'='x1tchcon_sc', # missingness > 30%
    'Inhibitory control (scaled)'='x1inbcnt_sc',
    # 'Child health (scaled)'='p1hscale_sc', # missingness > 30%
    'Community support (scaled)'='s4spprt_sc',  # newly added
    'Parent1 Age (scaled)'='par1age_sc'  # newly added
)
demnames <- names(demvars)


#' Generate confusion matrix for SuperLearner SL.glasso
#' 
#' @param slfit fitted SuperLearner with SL.glasso

#' Helper function call for lcmm::summarytable() on list of models
#' 
#' @param modlist list of lcmm models
#' @param modstats vector of which arg for summarytable
#' @return tibble
doSummarytable <- function(modlist, modstats=c('G', 'loglik', 'npm', 'AIC', 'BIC', '%class', 'entropy')) {
    require(lcmm)
    
    ms <- unname(modlist)
    ms$which <- modstats
    sumtb <- do.call(lcmm::summarytable, ms)
    as_tibble(sumtb) %>%
        mutate(model=names(modlist)) %>%
        rename_with(~str_replace(., '^%', 'pc_'), .cols=matches('class')) %>%
        select(model, g=G, loglik, npm, aic=AIC, bic=BIC, entropy, everything())
}


#' Format glm logit results
#' 
#' @param mod glm logit model
#' @param returnKbl logical: return data.frame or kable output
formatEsts <- function(mod, returnKbl=TRUE) {
    ests <- as_tibble(summary(mod)$coefficients, rownames='x') %>%
        rename(est=Estimate, stder=`Std. Error`, zval=`z value`, pval=`Pr(>|z|)`) %>%
        mutate(or=exp(est), cil=exp(est - 1.96*stder), ciu=exp(est + 1.96*stder))
    if (returnKbl) {
        ests %>%
            mutate(across(c(cil, ciu), ~case_when(
                abs(.) > 1e3 ~ sprintf('%.2e', .),
                TRUE ~ sprintf('%.2f', .)
            ))) %>%
            mutate(
                or=sprintf('%.2f', or),
                ci=sprintf('[%.2f, %.2f]', cil, ciu),
                pval=case_when(pval<0.05 ~ sprintf('%.3f', pval), 
                TRUE ~ sprintf('%.2f', pval))
            ) %>%
            select(x, or, ci, pval) %>%
        kable(booktabs=TRUE, col.names=c('x', 'OR', '95%CI', 'p-value')) %>%
            kable_styling(bootstrap_options=c('condensed'), full_width=FALSE)
    } else {
        return(ests)
    }
}


#' Join LCMM posterior classification with demographic and longitudinal model data
#' 
#' @param type chr: ['ext', 'int']
#' @param n numeric: number of clusters
#' @param model chr: name of LCMM model link
#' @param mlist list of LCMM models
#' @param df_dem data.frame: wide demographic dataset
#' @param df_long data.frame: longitudinal model dataset
#' @return list of data.frames
joinCl <- function(type, n, model, mlist=lc4_list, .bl=bl, df_long=mdata) {
    if (!type %in% c('ext', 'int')) {
        stop('!type %in% c(\'ext\', \'int\')')
    }
    if (n<0 & n%%1==0) {
        stop('n must be positive integer')
    }
    if (!model %in% c('lin', 'quad', 'beta', 'spl3q', 'spl3e', 'spl4q', 'spl4e')) {
        stop('check model arg!')
    }
    # pull model posterior classifications
    pprob <- mlist[[type]][[paste0('n', n)]][[model]]$pprob %>%
        mutate(class=as.factor(class))
    # return list of datasets
    list(
        bl=right_join(.bl, pprob, by='childkey'),
        mdata=left_join(df_long, pprob, by='childkey')
    )
}


#' Create labeled class factor in joinCl() output
#' 
#' @param class_labels chr: named character vector c(class=label)
#' @param joined_cl joinCl() output
labelCl <- function(class_labels, joined_cl) {
    if (length(intersect(unique(joined_cl$bl$class), names(class_labels))) != length(class_labels)) {
        stop('Labels provided do not completely match labels in data')
    }
    if (length(class_labels) > length(unique(joined_cl$bl$class))) {
        warning('more classes defined than in data!')
    }
    level_order <- paste0(class_labels, ' (', names(class_labels), ')')
    map(joined_cl, function(df) {
        left_join(
            df %>%
                select(-matches('^class_lbl$')),
            tibble(
                class=names(class_labels),
                class_lbl=class_labels
            ) %>%
                mutate(
                    class=as_factor(class),
                    class_lbl=factor(paste0(class_labels, ' (', as.character(class), ')'), levels=level_order)
                ),
            by='class'
        )
    })
}


#' Predict for SL.glasso()
#' 
#' @param object fitted SL.glasso()
#' @param newdata data.frame
predict.SL.glasso = function(object, newdata,...) {
    predict.glasso = predict(object,
        newdata,
        s="lambda.min",
        type='link')
    pred = sapply(1:length(predict.gglasso),
    function(x)1/(1+exp(-predict.gglasso[x])))
    return(pred)
}


#' Prep data for model fitting
#' Only for binomial or multinomial outcomes
#' 
#' @param dat data.frame model data.frame/tibble
#' @param covs character: character list of covariates
prepMdata <- function(dat, covs, outcome='binomial') {
    if (!outcome %in% c('binomial', 'multinomial')) {
        stop('This helper function is only suitable for {binomial, multinomial} outcomes!')
    }
    # completers
    # mat <- dat %>%
    #     select(childid, class_lbl, all_of(covs)) %>%
    #     filter(across(all_of(covs), ~!is.na(.)))
    sub <- dat %>% select(childid, class_lbl, all_of(covs)) # subset columns
    covs <- names(sub)[-(1:2)]
    mat <- sub %>% filter(across(all_of(covs), ~!is.na(.))) # delete obs with missing values
    # model matrix for covariates
    x_df <- select(mat, all_of(covs))
    x_mat <- model.matrix(~ -1 + ., select(mat, all_of(covs)))
    # x <- list(x_df=x_df, x_mat=x_mat)
    # set group indices for covariates
    x_reps <- unlist(map(covs, ~ifelse(is.factor(mat[[.]]), length(levels(mat[[.]]))-1, 1)))
    # add group for intercept
    x_grp <- c(1, rep(seq_along(covs)+1, times=x_reps))
    if (outcome=='binomial') {
        # standard {0,1} outcome
        y_01 <- ifelse(as.numeric(mat$class_lbl)==2, 1, 0)
        # set reference level to -1; value to {-1,1} for gglasso
        y_gglasso <- ifelse(as.numeric(mat$class_lbl)==2, 1, -1)
        y <- list(y_01=y_01, y_gglasso=y_gglasso)
    } else if (outcome=='multinomial') {
        y <- list(y=mat$class_lbl)
    }
    # return list
    list(x=x_mat, x_grp=x_grp, y=y)
}


#' Prep CV.SuperLearner ROCR prediction and performance objects
#'
#' @param cvsl fitted CV.SuperLearner object
prepCVSLperf <- function(cvsl) {
    preds <- as_tibble(cvsl$library.predict)
    preds$SuperLearner = cvsl$SL.predict
    preds$DiscreteSL = cvsl$discreteSL.predict

    rocr_preds <- apply(preds, MARGIN=2, FUN=function(p) {
        ROCR::prediction(p, cvsl$Y)
    })

    rocr_perf <- map(rocr_preds, ROCR::performance, measure='sens', x.measure='spec')
    rocr_prec_rec <- map(rocr_preds, ROCR::performance, measure='prec', x.measure='rec')
    rocr_acc <- map(rocr_preds, ROCR::performance, measure='acc')

    roc <- map(rocr_perf, function(p) {
        tibble(
            sens=p@y.values[[1]],
            spec=p@x.values[[1]]
        )
    }) %>%
        bind_rows(.id='alg')
    prec_rec <- map(rocr_prec_rec, function(p) {
        tibble(
            prec=p@y.values[[1]],
            rec=p@x.values[[1]]
        )
    }) %>%
        bind_rows(.id='alg')
    acc <- map(rocr_acc, function(p) {
        tibble(
            acc=p@y.values[[1]],
            cutoff=p@x.values[[1]]
        )
    }) %>%
        bind_rows(.id='alg')

    aucs <- map(rocr_preds, ROCR::performance, measure='auc') %>%
        map(function(p) {
            tibble(
                auc=p@y.values[[1]]
            )
        }) %>%
        bind_rows(.id='alg')
    aucspr <- map(rocr_preds, ROCR::performance, measure='aucpr') %>%
        map(function(p) {
            tibble(
                aucpr=p@y.values[[1]]
            )
        }) %>%
        bind_rows(.id='alg')

    return(
        list(
            roc=roc,
            prec_rec=prec_rec,
            aucs=aucs,
            aucspr=aucspr,
            acc=acc,
            preds=preds,
            rocr_preds=rocr_preds,
            rocr_perf=rocr_perf
        )
    )
}


#' SuperLearner wrapper for gglasso::cv.gglasso()
#' 
#' @param Y vector for outcome; {0,1} coding for binary
#' @param X data.frame or matrix: predictors
#' @param newX data.frame or matrix: predictors for prediction
#' @param family gaussian() or binomial()
#' @param groupid vector numeric: covariate group indices for group lasso
#' @param nfolds numeric: nfolds for cross-validation
SL.glasso <- function(Y, X, newX, family, groupid=1:dim(X)[2], nfolds=10, ...) {
    require(gglasso) #package to fit group lasso
    if(family$family == "gaussian") {
        stop("SL.glasso only available for family = binomial()")
    }
    if(length(groupid)!= dim(X)[2]){
        stop("Number of covariates is different than the lenght of groupid")
    }
    if (!is.matrix(X)) {
        X <- model.matrix(~ -1 + ., X)
        newX <- model.matrix(~ -1 + ., newX)
    }
    Y[which(Y == 0)] = -1
    fit.CVglasso = gglasso::cv.gglasso(
        x = X, y = Y,
        group = groupid,
        loss='logit', pred.loss = 'loss', nfolds = nfolds
    )
                    
    predict.gglasso = predict(fit.CVglasso, newX,  s = "lambda.min", type = 'link')
    
    pred = sapply(1:length(predict.gglasso), function(x) 1/(1 + exp(-predict.gglasso[x])))
    fit = list(object = fit.CVglasso)
    out = list(pred = pred, fit = fit)
    class(out$fit) <- 'SL.glasso'
    return(out)
}

