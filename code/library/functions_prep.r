############################################
# FUNCTIONS: data prep/cleaning

############################################


#' Format edu: max of edu2 (parent's highest education)
#' Equates to highest education between both parents
#' 
#' @param dat data.frame
formatEdu <- function(dat) {
    dat %>%
        mutate(edu = pmax(x12par1ed_i, x12par2ed_i, na.rm = T)) %>%
        mutate(edu = case_when(
            edu %in% 1:2 ~ "Less than high school",
            edu == 3 ~ "High school/equiv.",
            edu %in% 4:5 ~ "Some college, voc./tech program",
            edu == 6 ~ "Bachelor's degree",
            edu == 7 ~ "Some graduate/prof.",
            edu == 8 ~ "MS or higher",
            edu == -9 ~ NA_character_,
            TRUE ~ NA_character_))%>%
        mutate(edu = factor(edu,
            levels = c(
                'None',
                "Less than high school",
                "High school/equiv.",
                "Some college, voc./tech program",
                "Bachelor's degree",
                "Some graduate/prof.",
                "MS or higher",
                NA),
            ordered = TRUE,
            exclude = NULL))
}


#' Format household income bracket
#' 
#' @param dat data.frame
formatInc <- function(dat) {
    dat %>%
        mutate_at(vars(matches("x[246789]inccat_i"), matches("^inc$")),
            ~case_when(
                . == 1 ~ "$5K or less",
                . == 2 ~ "($5K, $10K]",
                . == 3 ~ "($10K, $15K]",
                . == 4 ~ "($15K, $20K]",
                . == 5 ~ "($20K, $25K]",
                . == 6 ~ "($25K, $30K]",
                . == 7 ~ "($30K, $35K]",
                . == 8 ~ "($35K, $40K]",
                . == 9 ~ "($40K, $45K]",
                . == 10 ~ "($45K, $50K]",
                . == 11 ~ "($50K, $55K]",
                . == 12 ~ "($55K, $60K]",
                . == 13 ~ "($60K, $65K]",
                . == 14 ~ "($65K, $70K]",
                . == 15 ~ "($70K, $75K]",
                . == 16 ~ "($75K, $100K]",
                . == 17 ~ "($100K, $200K]",
                . == 18 ~ "$200K or more",
                . == -9 ~ NA_character_,
                is.na(.) ~ NA_character_)
        ) %>%
        mutate_at(vars(matches("x[246789]inccat_i"), matches("^inc$")),
            ~factor(.,
                levels = c("$5K or less",
                "($5K, $10K]",
                "($10K, $15K]",
                "($15K, $20K]",
                "($20K, $25K]",
                "($25K, $30K]",
                "($30K, $35K]",
                "($35K, $40K]",
                "($40K, $45K]",
                "($45K, $50K]",
                "($50K, $55K]",
                "($55K, $60K]",
                "($60K, $65K]",
                "($65K, $70K]",
                "($70K, $75K]",
                "($75K, $100K]",
                "($100K, $200K]",
                "$200K or more",
                NA),
                ordered = TRUE,
                exclude = NULL)
        )
}



#' Format x_raceth_p
#' Combine hispanic categories; combine Asian & Hawaiian/Pacific Isl.
#' 
#' @param dat data.frame
formatRaceth <- function(dat) {
    dat %>%
        mutate(
            raceth = factor(case_when(
                x_raceth_r == 1 ~ "White",
                x_raceth_r == 2 ~ "Black/African-American",
                x_raceth_r == 3 ~ "Hispanic",
                x_raceth_r == 4 ~ "Hispanic",
                x_raceth_r == 5 ~ "Asian",
                x_raceth_r == 6 ~ "Asian",
                x_raceth_r == 7 ~ "American Indian/Alaska Native",
                x_raceth_r == 8 ~ "Multiracial",
                x_raceth_r == -9  ~ "Not ascertained")
            , levels = c("Asian", "American Indian/Alaska Native", "Black/African-American", "Hispanic", "White", "Multiracial", "Not ascertained"))
        )
}


#' Format x_raceth_p (original survey version)
#' 
#' @param dat data.frame
#' @param asian logical: group by asian or not
formatRaceth_old <- function(dat, asian = FALSE) {
    temp <- dat %>%
        mutate(raceth = case_when(
            x_raceth_r == 1 ~ "White, non-hisp",
            x_raceth_r == 2 ~ "Black/African-American, non-hisp",
            x_raceth_r == 3 ~ "Hispanic, race specified",
            x_raceth_r == 4 ~ "Hispanic, no race specified",
            x_raceth_r == 5 ~ "Asian, non-hisp",
            x_raceth_r == 6 ~ "Native Hawaiian/Pacific Isl., non-hisp",
            x_raceth_r == 7 ~ "American Indian/Alaska Native, non-hisp",
            x_raceth_r == 8 ~ "Two or more races, non-hisp",
            x_raceth_r == -9  ~ "Not ascertained")
    )
    if (asian) {
        mutate(temp, raceth = case_when(
            grepl("Asian", raceth) ~ "Asian", TRUE ~ "All others"))
    } else {
        temp
    }
}


#' Format survey variable
#' 
#' @param dat data.frame: must have "survey" variable
#' @param order lgl: factor or ordered factor
#' @param grade logical: whether to label by grade (K, 1st, 2nd) or by calendar year
formatSurvey <- function(dat, order = TRUE, grade=FALSE) {
    if (grade) {
        survey_labs <- c(
            '2010, Fall'='K F', '2011, Spring'='K S', '2011'='K',
            '2011, Fall'='1 F', '2012, Spring'='1 S',
            '2012, Fall'='2 F', '2013, Spring'='2 S',
            '2014, Spring'='3 S',
            '2015, Spring'='4 S',
            '2016, Spring'='5 S'
        )
    } else {
        survey_labs <- c(
            '2010, Fall'='2010 F', '2011, Spring'='2011 S', '2011'='2011',
            '2011, Fall'='2011 F', '2012, Spring'='2012 S',
            '2012, Fall'='2012 F', '2013, Spring'='2013 S',
            '2014, Spring'='2014 S',
            '2015, Spring'='2015 S',
            '2016, Spring'='2016 S'
        )
    }
    dat %>%
        mutate(survey = case_when(
            grepl("^x12", survey) ~ "2011",
            grepl("^x1", survey) ~ "2010, Fall",
            grepl("^x2", survey) ~ "2011, Spring",
            grepl("^x3", survey) ~ "2011, Fall",
            grepl("^x4", survey) ~ "2012, Spring",
            grepl("^x5", survey) ~ "2012, Fall",
            grepl("^x6", survey) ~ "2013, Spring",
            grepl("^x7", survey) ~ "2014, Spring",
            grepl("^x8", survey) ~ "2015, Spring",
            grepl("^x9", survey) ~ "2016, Spring",
            TRUE ~ "Error"
    )) %>%
        mutate(survey = factor(survey,
            levels = c(
                "2010, Fall", "2011, Spring", "2011",
                "2011, Fall", "2012, Spring",
                "2012, Fall", "2013, Spring",
                "2014, Spring", "2015, Spring", "2016, Spring"),
            labels = survey_labs,
            ordered = !!order))
}


#' Format survey as numeric years
#' 
#' @param df data.frame: variables must be reshape long with a 'survey' variable that starts with x[1-9]. Ignores Fall sub-sample survey measurements
mapSurveyYear <- function(df) {
    df %>%
        mutate(
            year = case_when(
                grepl('^x12', survey) ~ 2011,
                grepl('^x2', survey) ~ 2011,
                grepl('^x4', survey) ~ 2012,
                grepl('^x6', survey) ~ 2013,
                grepl('^x7', survey) ~ 2014,
                grepl('^x8', survey) ~ 2015,
                grepl('^x9', survey) ~ 2016
            )
        )
}

