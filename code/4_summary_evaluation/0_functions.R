# Objective : functions used for model performance evaluation

prepDat <- function(df, covariates){
  sub <- df %>% select(childid, class_lbl, all_of(covariates)) # subset columns
  covs <- names(sub)[-(1:2)]  # name of covariates
  mat <- sub %>% filter(across(all_of(covs), ~!is.na(.))) # delete obs with missing values
  mat$class_lbl <- ifelse(as.numeric(mat$class_lbl)==2, 1, 0) # change outcome to [0,1] scale
  return(mat)
}

get_calib_info <- function(df_hat, func, num_qtl=10){
  # df_hat: dataframe of interest
  # func: the predicted values based on which function(algorithm) used
  # num_qtl: number of quntile intervals
  df_hat_sorted <- df_hat[order(df_hat[,func]),] # sort based on pij_hat
  bound_seq <- seq(from=0, to=1, by=1/num_qtl) # quantile boundaries (from 0 to 1)
  qtl_group_seq <- seq(from=1, to=num_qtl, by=1) # quantile group number
  qtl <- unique(quantile(df_hat_sorted[,func], probs=bound_seq)) # get pij_hat boundary values
  if ((num_qtl+1) != length(qtl)){
    qtl_group_seq <- seq(from=1, to=length(qtl)-1, by=1)
  }
  # add quantile info
  qtl
  qtl_group_seq
  df_hat_sorted$qtl_group <- cut(df_hat_sorted[,func], qtl, qtl_group_seq,  include.lowest = T)
  # calculate events rate within each quantile
  aggre_true <- aggregate(x=df_hat_sorted$class_lbl, by=list(df_hat_sorted$qtl_group), FUN=mean)
  aggre_pred <- aggregate(x=df_hat_sorted[,func], by=list(df_hat_sorted$qtl_group), FUN=mean)
  # add aggregate mean (true events rate with each qtl group) into dataframe
  df_hat_sorted$qtl_group_mean <- ave(df_hat_sorted$class_lbl, df_hat_sorted$qtl_group)
  # outcome_list <- list('df_hat_sorted' = df_hat_sorted, 'aggre_true' = aggre_true, 'aggre_pred' = aggre_pred)
  outcome_list <- list('aggre_true' = aggre_true, 'aggre_pred' = aggre_pred)
  return(outcome_list)
}

get_x_labels <- function(aggre_pred){
  length_x <- length(aggre_pred)
  x_text_bounds <- seq(from=0, to=1, by=1/length_x)
  x_text <- NULL
  for (i in 1:(length_x)){
    x_text <- c(x_text, paste(round(x_text_bounds[i],2), round(x_text_bounds[i+1],2), sep='-'))
  }
  return(x_text)
}
