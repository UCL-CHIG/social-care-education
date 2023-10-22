
# LOAD --------------------------------------------------------------------

setwd("P:/Working/Matt")

lib_base = "P:/Working/Matt/r-library/"
assign(".lib.loc", c(.libPaths(), lib_base), envir = environment(.libPaths))
rm(lib_base)

# data management
library(data.table)
library(reshape2)
library(tableone)
library(tidyr)

# graphics
library(ggplot2)
library(grid)
library(gridExtra)

# regrssion modelling
library(lme4)
library(lmtest)
library(geepack)

load("PROCESSED DATA/cohort1_npd_clean_sch_ad_qual_cscobs_outcomes_misc.rda")
load("PROCESSED DATA/cohort2_npd_clean_sch_ad_qual_cscobs_outcomes_misc.rda")

# load("PROCESSED DATA/cohort1_npd_clean_sch_ad_qual_cscobs_outcomes_misc_csclca.rda")
# load("PROCESSED DATA/cohort2_npd_clean_sch_ad_qual_cscobs_outcomes_misc_csclca.rda")

cohort1$cohort <- 1
cohort2$cohort <- 2
table(names(cohort1) == names(cohort2))
cohort_all <- rbind(cohort1, cohort2)
rm(cohort1, cohort2)

# custom functions --------------------------------------------------------

describe_outcome <- function(exposures, outcome = NULL, dat, n_round = 0, p_round = 1) {
  
  if (!is.null(outcome)) {
    tmp_table <- round(table(dat[[exposures[1]]],
                             dat[[outcome]], useNA = "always"), n_round)
    prop_tmp_table <- round(prop.table(tmp_table, 1) * 100, p_round)
    output <- matrix(
      rep(NA, nrow(tmp_table) * 1),
      nrow = nrow(tmp_table),
      ncol = 1
    )
    colnames(output) <- outcome
    rownames(output) <- paste0(exposures[1], "_", names(tmp_table[, 2]))
    output[, 1] <- paste0(tmp_table[, 2], " (", prop_tmp_table[, 2], "%)")
    
    if (length(exposures) > 1) {
      for (j in 2:length(exposures)) {
        tmp_table <- round(table(dat[[exposures[j]]],
                                 dat[[outcome]], useNA = "always"), n_round)
        prop_tmp_table <- round(prop.table(tmp_table, 1) * 100, p_round)
        tmp_matrix <- matrix(
          rep(NA, nrow(tmp_table) * 1),
          nrow = nrow(tmp_table),
          ncol = 1
        )
        colnames(tmp_matrix) <- outcome
        rownames(tmp_matrix) <- paste0(exposures[j], "_", names(tmp_table[, 2]))
        tmp_matrix[, 1] <- paste0(tmp_table[, 2], " (", prop_tmp_table[, 2], "%)")
        output <- rbind(output, tmp_matrix)
      }
    }
    
    return(output)
    
  } else {
    tmp_table <- round(table(dat[[exposures[1]]], useNA = "always"), n_round)
    prop_tmp_table <- round(prop.table(tmp_table) * 100, p_round)
    output <- matrix(
      rep(NA, nrow(tmp_table) * 1),
      nrow = nrow(tmp_table),
      ncol = 1
    )
    colnames(output) <- outcome
    rownames(output) <- paste0(exposures[1], "_", names(tmp_table))
    output[, 1] <- paste0(tmp_table, " (", prop_tmp_table, "%)")
    
    
    if (length(exposures) > 1) {
      for (j in 2:length(exposures)) {
        tmp_table <- round(table(dat[[exposures[j]]], useNA = "always"), n_round)
        prop_tmp_table <- round(prop.table(tmp_table) * 100, p_round)
        tmp_matrix <- matrix(
          rep(NA, nrow(tmp_table) * 1),
          nrow = nrow(tmp_table),
          ncol = 1
        )
        colnames(tmp_matrix) <- outcome
        rownames(tmp_matrix) <- paste0(exposures[j], "_", names(tmp_table))
        tmp_matrix[, 1] <- paste0(tmp_table, " (", prop_tmp_table, "%)")
        output <- rbind(output, tmp_matrix)
        
      }
    }
    
    return(output)
    
  }
}

create_ci_data <- function(subgroups = T, pmr_vector, outcome_var, factor_var = NULL) {
  
  if (!subgroups) {
    
    ci_tab <- data.frame(
      pupilmatchingrefanonymous = pmr_vector
    )
    
    ci_tab$y7 <- as.integer(NA)
    ci_tab$y8 <- as.integer(NA)
    ci_tab$y9 <- as.integer(NA)
    ci_tab$y10 <- as.integer(NA)
    ci_tab$y11 <- as.integer(NA)
    
    for (i in 1:5) {
      ci_tab[, i + 1] <- ci_tab$pupilmatchingrefanonymous %in%
        cohort_all[year <= i + 6 & outcome_var == 1]$pupilmatchingrefanonymous
    }
    
    return(data.table(ci_tab))
    
  } else {
    
    if (!is.factor(factor_var)) {
      factor_var <- factor(factor_var)
    }
    
    factor_var_int <- as.integer(factor_var)
    loop_length <- dim(table(factor_var))
    
    ci_list <- list()
    for (i in 1:loop_length) {
      ci_list[[i]] <- data.frame(
        pupilmatchingrefanonymous = pmr_vector[factor_var_int == i]
      )
      
      ci_list[[i]]$y7 <- as.integer(NA)
      ci_list[[i]]$y8 <- as.integer(NA)
      ci_list[[i]]$y9 <- as.integer(NA)
      ci_list[[i]]$y10 <- as.integer(NA)
      ci_list[[i]]$y11 <- as.integer(NA)
      
      for (j in 1:5) {
        ci_list[[i]][, j + 1] <- ci_list[[i]]$pupilmatchingrefanonymous %in%
          cohort_all[year <= j + 6 & outcome_var == 1]$pupilmatchingrefanonymous
      }
      
      ci_list[[i]] <- data.table(ci_list[[i]])
    }
    names(ci_list) <- levels(factor_var)
    return(ci_list)
  }
}

create_ci_output <- function(subgroups = T, ci_data, names_for_cols = NULL, n_round = 0, perc_round = 1) {
  
  if (!subgroups) {
    
    ci_output <- data.frame(
      year = 7:11,
      n = rep(NA, 5),
      p = rep(NA, 5)
    )
    
    for (i in 2:6) {
      ci_output[i - 1, 2] <- round(table(ci_data[, i, with = F])[2], n_round)
      ci_output[i - 1, 3] <- round(prop.table(table(ci_data[, i, with = F])) * 100, perc_round)[2]
    }
    
    return(ci_output)
    
  } else {
    for (i in 1:length(ci_data)) {
      ci_output <- matrix(
        rep(NA, 5 * (length(ci_data) * 4)),
        nrow = 5,
        ncol = (length(ci_data) * 4)
      )
      for (j in 2:6) {
        for (k in 1:length(ci_data)) {
          ci_output[j - 1, k] <- round(table(ci_data[[k]][, j, with = F])[2], n_round)
          ci_output[j - 1, length(ci_data) + k] <- round(prop.table(table(ci_data[[k]][, j, with = F])) * 100, perc_round)[2]
          n <- ci_output[j - 1, k]
          p <- ci_output[j - 1, length(ci_data) + k]
          se <- sqrt((p * (100 - p)) / n)
          ci_output[j - 1, length(ci_data) * 2 + k] <- ci_output[j - 1, length(ci_data) + k] - 1.96 * se
          ci_output[j - 1, length(ci_data) * 3 + k] <- ci_output[j - 1, length(ci_data) + k] +  1.96 * se
        }
      }
    }
    
    colnames(ci_output) <- rep(names_for_cols, length(ci_data))
    
    return(ci_output)
    
  }
}

# MISSING ---------------------------------------------
# 
# table(cohort_all[year == 7 & sch_n_in_year == 1]$cohort)
# 
# nrow(cohort_all[year == 7 & sch_n_in_year == 1])

# summary(cohort_all[year == 7 & sch_n_in_year == 1, c("pupilmatchingrefanonymous", "cohort",
#                                                      "female_clean", "eth_major", "language_clean", "highest_ever_sen_y09",
#                                                      "ever_sen_y09",
#                                                      "idaci_quintiles", "fsmeligible", "idaci_fsm", "region_major", "la",
#                                                      "exposure_highest_4_grp_y46",
#                                                      "school_type_with_secondary_5_groups", "appru_y09", "special_school",
#                                                      "not_enrolled_y8", "not_enrolled_y9", "not_enrolled_y10", "not_enrolled_y11",
#                                                      "not_enrolled_y89", "not_enrolled_y1011", "no_gcses",
#                                                      "not_enrolled_y1011_nogcses")])

cohort_all$any_nas <- apply(cohort_all[, c("female_clean", "eth_major", "language_clean", "idaci_quintiles")], 1, anyNA)
cohort_all[, drop := year == 7 & any_nas & sch_n_in_year == 1]
cohort_all[, drop := max(drop), by = .(pupilmatchingrefanonymous)]
table(cohort_all[year == 7 & sch_n_in_year == 1]$drop) 
cohort_all <- cohort_all[!cohort_all$drop]
cohort_all[, any_nas := NULL]
cohort_all[, drop := NULL]

# DEMOGRAPHICS ------------------------------------------------------------

table(cohort_all[year == 7 & sch_n_in_year == 1]$cohort)

nrow(cohort_all[year == 7 & sch_n_in_year == 1])

exposures <- c("exposure_highest_4_grp_y49",
               "female_clean", "eth_major", "language_clean", "idaci_quintiles", "fsmeligible", "idaci_fsm", "region_major",
               "school_type_with_secondary_5_groups", "appru", "appru_y79", "appru_y09", "special_school",
               "ever_sen_y09", "highest_ever_sen_y09")

final_output <- cbind(describe_outcome(exposure = exposures, outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & cohort == 1]),
                      describe_outcome(exposure = exposures, outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & cohort == 2]),
                      describe_outcome(exposure = exposures, outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1]))
colnames(final_output) <- c("Cohort 1", "Cohort 2", "Combined")
write.csv(final_output, file = "OUTPUTS/CHAPTER 6/chars.csv")

# by special school or not
table(cohort_all[year == 7 & sch_n_in_year == 1]$special_school)

exposures <- c("exposure_highest_4_grp_y49",
               "female_clean", "eth_major", "language_clean", "idaci_quintiles", "fsmeligible", "idaci_fsm", "region_major",
               "school_type_with_secondary_5_groups", "appru", "appru_y79",  "appru_y09",
               "ever_sen_y09", "highest_ever_sen_y09")

final_output <- cbind(describe_outcome(exposure = exposures, outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T]),
                      describe_outcome(exposure = exposures, outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F]),
                      describe_outcome(exposure = exposures, outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1]))
colnames(final_output) <- c("Special school yr 7", "Not special school yr 7", "Combined")
write.csv(final_output, file = "OUTPUTS/CHAPTER 6/chars-by-special-school.csv")

rm(exposures, final_output)

table(cohort_all[sch_n_in_year == 1 & year == 7]$exposure_highest_4_grp_y49,
      cohort_all[sch_n_in_year == 1 & year == 7]$ever_sen_y09)

# OUTCOME DESC ------------------------------------------------------------

table(cohort_all[year == 7 & sch_n_in_year == 1]$not_enrolled_y1011)
prop.table(table(cohort_all[year == 7 & sch_n_in_year == 1]$not_enrolled_y1011))

table(cohort_all[year == 7 & sch_n_in_year == 1]$no_gcses)
prop.table(table(cohort_all[year == 7 & sch_n_in_year == 1]$no_gcses))

table(cohort_all[year == 7 & sch_n_in_year == 1]$not_enrolled_y1011_nogcses)
prop.table(table(cohort_all[year == 7 & sch_n_in_year == 1]$not_enrolled_y1011_nogcses))

# grouped
outcomes <- c("not_enrolled_y1011",
              "no_gcses",
              "not_enrolled_y1011_nogcses")

final_output <- cbind(rbind(describe_outcome(exposure = outcomes[1], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F], p_round = 1),
                            describe_outcome(exposure = outcomes[2], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F], p_round = 1),
                            describe_outcome(exposure = outcomes[3], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F], p_round = 1)),
                      rbind(describe_outcome(exposure = outcomes[1], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T], p_round = 1),
                            describe_outcome(exposure = outcomes[2], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T], p_round = 1),
                            describe_outcome(exposure = outcomes[3], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T], p_round = 1)))
colnames(final_output) <- c("Not special school yr 7", "Special school yr 7")
write.csv(final_output, file = "OUTPUTS/CHAPTER 6/outcomes-total.csv")

# * grouped by RFs not special schools --------------------------------------------

exposures <- c("exposure_highest_4_grp_y49",
               "female_clean", "eth_major", "language_clean", "idaci_quintiles", "fsmeligible", "idaci_fsm", "region_major",
               "appru_y09", "ever_sen_y09", "highest_ever_sen_y09")

final_output <- cbind(describe_outcome(exposure = exposures, outcome = outcomes[1], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F], p_round = 1),
                      describe_outcome(exposure = exposures, outcome = outcomes[2], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F], p_round = 1),
                      describe_outcome(exposure = exposures, outcome = outcomes[3], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F], p_round = 1))
write.csv(final_output, file = "OUTPUTS/CHAPTER 6/outcomes-by-RFs-not-special.csv")

# by cSC status
final_output <- cbind(describe_outcome(exposure = exposures, outcome = outcomes[1],
                                       dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F & exposure_highest_4_grp_y49 == "None"], p_round = 1),
                      describe_outcome(exposure = exposures, outcome = outcomes[2],
                                       dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F & exposure_highest_4_grp_y49 == "None"], p_round = 1),
                      describe_outcome(exposure = exposures, outcome = outcomes[3],
                                       dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F & exposure_highest_4_grp_y49 == "None"], p_round = 1))
write.csv(final_output, file = "OUTPUTS/CHAPTER 6/outcomes-by-RFs-not-special-nocsc.csv")

final_output <- cbind(describe_outcome(exposure = exposures, outcome = outcomes[1],
                                       dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F & exposure_highest_4_grp_y49 == "CiN"], p_round = 1),
                      describe_outcome(exposure = exposures, outcome = outcomes[2],
                                       dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F & exposure_highest_4_grp_y49 == "CiN"], p_round = 1),
                      describe_outcome(exposure = exposures, outcome = outcomes[3],
                                       dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F & exposure_highest_4_grp_y49 == "CiN"], p_round = 1))
write.csv(final_output, file = "OUTPUTS/CHAPTER 6/outcomes-by-RFs-not-special-cin.csv")

final_output <- cbind(describe_outcome(exposure = exposures, outcome = outcomes[1],
                                       dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F & exposure_highest_4_grp_y49 == "CPP"], p_round = 1),
                      describe_outcome(exposure = exposures, outcome = outcomes[2],
                                       dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F & exposure_highest_4_grp_y49 == "CPP"], p_round = 1),
                      describe_outcome(exposure = exposures, outcome = outcomes[3],
                                       dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F & exposure_highest_4_grp_y49 == "CPP"], p_round = 1))
write.csv(final_output, file = "OUTPUTS/CHAPTER 6/outcomes-by-RFs-not-special-cpp.csv")

final_output <- cbind(describe_outcome(exposure = exposures, outcome = outcomes[1],
                                       dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F & exposure_highest_4_grp_y49 == "CLA"], p_round = 1),
                      describe_outcome(exposure = exposures, outcome = outcomes[2],
                                       dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F & exposure_highest_4_grp_y49 == "CLA"], p_round = 1),
                      describe_outcome(exposure = exposures, outcome = outcomes[3],
                                       dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F & exposure_highest_4_grp_y49 == "CLA"], p_round = 1))
write.csv(final_output, file = "OUTPUTS/CHAPTER 6/outcomes-by-RFs-not-special-cla.csv")

# * by RFs special schools ------------------------------------------------

final_output <- cbind(describe_outcome(exposure = exposures, outcome = outcomes[1], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T], p_round = 1),
                      describe_outcome(exposure = exposures, outcome = outcomes[2], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T], p_round = 1),
                      describe_outcome(exposure = exposures, outcome = outcomes[3], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T], p_round = 1))
write.csv(final_output, file = "OUTPUTS/CHAPTER 6/outcomes-by-RFs-special.csv")

# by CSC
final_output <- cbind(describe_outcome(exposure = exposures, outcome = outcomes[1],
                                       dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T & exposure_highest_4_grp_y49 == "None"], p_round = 1),
                      describe_outcome(exposure = exposures, outcome = outcomes[2],
                                       dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T & exposure_highest_4_grp_y49 == "None"], p_round = 1),
                      describe_outcome(exposure = exposures, outcome = outcomes[3],
                                       dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T & exposure_highest_4_grp_y49 == "None"], p_round = 1))
write.csv(final_output, file = "OUTPUTS/CHAPTER 6/outcomes-by-RFs-special-nocsc.csv")

final_output <- cbind(describe_outcome(exposure = exposures, outcome = outcomes[1],
                                       dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T & exposure_highest_4_grp_y49 == "CiN"], p_round = 1),
                      describe_outcome(exposure = exposures, outcome = outcomes[2],
                                       dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T & exposure_highest_4_grp_y49 == "CiN"], p_round = 1),
                      describe_outcome(exposure = exposures, outcome = outcomes[3],
                                       dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T & exposure_highest_4_grp_y49 == "CiN"], p_round = 1))
write.csv(final_output, file = "OUTPUTS/CHAPTER 6/outcomes-by-RFs-special-cin.csv")

final_output <- cbind(describe_outcome(exposure = exposures, outcome = outcomes[1],
                                       dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T & exposure_highest_4_grp_y49 == "CPP"], p_round = 1),
                      describe_outcome(exposure = exposures, outcome = outcomes[2],
                                       dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T & exposure_highest_4_grp_y49 == "CPP"], p_round = 1),
                      describe_outcome(exposure = exposures, outcome = outcomes[3],
                                       dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T & exposure_highest_4_grp_y49 == "CPP"], p_round = 1))
write.csv(final_output, file = "OUTPUTS/CHAPTER 6/outcomes-by-RFs-special-cpp.csv")

final_output <- cbind(describe_outcome(exposure = exposures, outcome = outcomes[1],
                                       dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T & exposure_highest_4_grp_y49 == "CLA"], p_round = 1),
                      describe_outcome(exposure = exposures, outcome = outcomes[2],
                                       dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T & exposure_highest_4_grp_y49 == "CLA"], p_round = 1),
                      describe_outcome(exposure = exposures, outcome = outcomes[3],
                                       dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T & exposure_highest_4_grp_y49 == "CLA"], p_round = 1))
write.csv(final_output, file = "OUTPUTS/CHAPTER 6/outcomes-by-RFs-special-cla.csv")

rm(exposures, outcomes, final_output)

# WHICH YEARS NOT ENROLLED ------------------------------------------------

table(cohort_all[year == 7 & sch_n_in_year == 1]$not_enrolled_y1011_nogcses) #
nrow(cohort_all[year == 7 & sch_n_in_year == 1 & not_enrolled_y1011_nogcses == T & not_enrolled_y10 == T]) # 
nrow(cohort_all[year == 7 & sch_n_in_year == 1 & not_enrolled_y1011_nogcses == T & not_enrolled_y11 == T]) # 
nrow(cohort_all[year == 7 & sch_n_in_year == 1 & not_enrolled_y1011_nogcses == T & not_enrolled_y10 == T & not_enrolled_y11 == T]) # 


table(cohort_all[year == 7 & sch_n_in_year == 1]$not_enrolled_y1011) # 
nrow(cohort_all[year == 7 & sch_n_in_year == 1 & not_enrolled_y1011 == T & not_enrolled_y10 == T]) # 
nrow(cohort_all[year == 7 & sch_n_in_year == 1 & not_enrolled_y1011 == T & not_enrolled_y11 == T]) # 
nrow(cohort_all[year == 7 & sch_n_in_year == 1 & not_enrolled_y1011 == T & not_enrolled_y10 == T & not_enrolled_y11 == T]) # 

cohort_all[, not_enrolled_y10and11 := not_enrolled_y10 == T & not_enrolled_y11 == T]
cohort_all[, not_enrolled_y10only := not_enrolled_y10 == T & not_enrolled_y11 == F]
cohort_all[, not_enrolled_y11only := not_enrolled_y10 == F & not_enrolled_y11 == T]

outcomes <- c("not_enrolled_y10", "not_enrolled_y11", "not_enrolled_y10and11", "not_enrolled_y10only", "not_enrolled_y11only")

final_output <-cbind(rbind(describe_outcome(exposure = outcomes[1], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & not_enrolled_y1011 == T & special_school == F & exposure_highest_4_grp_y49 == "None"]),
                           describe_outcome(exposure = outcomes[2], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & not_enrolled_y1011 == T & special_school == F & exposure_highest_4_grp_y49 == "None"]),
                           describe_outcome(exposure = outcomes[3], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & not_enrolled_y1011 == T & special_school == F & exposure_highest_4_grp_y49 == "None"]),
                           describe_outcome(exposure = outcomes[4], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & not_enrolled_y1011 == T & special_school == F & exposure_highest_4_grp_y49 == "None"]),
                           describe_outcome(exposure = outcomes[5], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & not_enrolled_y1011 == T & special_school == F & exposure_highest_4_grp_y49 == "None"])),
                     rbind(describe_outcome(exposure = outcomes[1], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & not_enrolled_y1011 == T & special_school == F & exposure_highest_4_grp_y49 == "CiN"]),
                           describe_outcome(exposure = outcomes[2], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & not_enrolled_y1011 == T & special_school == F & exposure_highest_4_grp_y49 == "CiN"]),
                           describe_outcome(exposure = outcomes[3], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & not_enrolled_y1011 == T & special_school == F & exposure_highest_4_grp_y49 == "CiN"]),
                           describe_outcome(exposure = outcomes[4], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & not_enrolled_y1011 == T & special_school == F & exposure_highest_4_grp_y49 == "CiN"]),
                           describe_outcome(exposure = outcomes[5], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & not_enrolled_y1011 == T & special_school == F & exposure_highest_4_grp_y49 == "CiN"])),
                     rbind(describe_outcome(exposure = outcomes[1], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & not_enrolled_y1011 == T & special_school == F & exposure_highest_4_grp_y49 == "CPP"]),
                           describe_outcome(exposure = outcomes[2], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & not_enrolled_y1011 == T & special_school == F & exposure_highest_4_grp_y49 == "CPP"]),
                           describe_outcome(exposure = outcomes[3], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & not_enrolled_y1011 == T & special_school == F & exposure_highest_4_grp_y49 == "CPP"]),
                           describe_outcome(exposure = outcomes[4], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & not_enrolled_y1011 == T & special_school == F & exposure_highest_4_grp_y49 == "CPP"]),
                           describe_outcome(exposure = outcomes[5], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & not_enrolled_y1011 == T & special_school == F & exposure_highest_4_grp_y49 == "CPP"])),
                     rbind(describe_outcome(exposure = outcomes[1], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & not_enrolled_y1011 == T & special_school == F & exposure_highest_4_grp_y49 == "CLA"]),
                           describe_outcome(exposure = outcomes[2], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & not_enrolled_y1011 == T & special_school == F & exposure_highest_4_grp_y49 == "CLA"]),
                           describe_outcome(exposure = outcomes[3], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & not_enrolled_y1011 == T & special_school == F & exposure_highest_4_grp_y49 == "CLA"]),
                           describe_outcome(exposure = outcomes[4], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & not_enrolled_y1011 == T & special_school == F & exposure_highest_4_grp_y49 == "CLA"]),
                           describe_outcome(exposure = outcomes[5], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & not_enrolled_y1011 == T & special_school == F & exposure_highest_4_grp_y49 == "CLA"]))) 
colnames(final_output) <- c("None", "CiN", "CPP", "CLA")
write.csv(final_output, file = "OUTPUTS/CHAPTER 6/notenrolledy1011-missing-which-year-non-special.csv")  

final_output <-cbind(rbind(describe_outcome(exposure = outcomes[1], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & not_enrolled_y1011_nogcses == T & special_school == F & exposure_highest_4_grp_y49 == "None"]),
                           describe_outcome(exposure = outcomes[2], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & not_enrolled_y1011_nogcses == T & special_school == F & exposure_highest_4_grp_y49 == "None"]),
                           describe_outcome(exposure = outcomes[3], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & not_enrolled_y1011_nogcses == T & special_school == F & exposure_highest_4_grp_y49 == "None"]),
                           describe_outcome(exposure = outcomes[4], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & not_enrolled_y1011_nogcses == T & special_school == F & exposure_highest_4_grp_y49 == "None"]),
                           describe_outcome(exposure = outcomes[5], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & not_enrolled_y1011_nogcses == T & special_school == F & exposure_highest_4_grp_y49 == "None"])),
                     rbind(describe_outcome(exposure = outcomes[1], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & not_enrolled_y1011_nogcses == T & special_school == F & exposure_highest_4_grp_y49 == "CiN"]),
                           describe_outcome(exposure = outcomes[2], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & not_enrolled_y1011_nogcses == T & special_school == F & exposure_highest_4_grp_y49 == "CiN"]),
                           describe_outcome(exposure = outcomes[3], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & not_enrolled_y1011_nogcses == T & special_school == F & exposure_highest_4_grp_y49 == "CiN"]),
                           describe_outcome(exposure = outcomes[4], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & not_enrolled_y1011_nogcses == T & special_school == F & exposure_highest_4_grp_y49 == "CiN"]),
                           describe_outcome(exposure = outcomes[5], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & not_enrolled_y1011_nogcses == T & special_school == F & exposure_highest_4_grp_y49 == "CiN"])),
                     rbind(describe_outcome(exposure = outcomes[1], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & not_enrolled_y1011_nogcses == T & special_school == F & exposure_highest_4_grp_y49 == "CPP"]),
                           describe_outcome(exposure = outcomes[2], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & not_enrolled_y1011_nogcses == T & special_school == F & exposure_highest_4_grp_y49 == "CPP"]),
                           describe_outcome(exposure = outcomes[3], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & not_enrolled_y1011_nogcses == T & special_school == F & exposure_highest_4_grp_y49 == "CPP"]),
                           describe_outcome(exposure = outcomes[4], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & not_enrolled_y1011_nogcses == T & special_school == F & exposure_highest_4_grp_y49 == "CPP"]),
                           describe_outcome(exposure = outcomes[5], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & not_enrolled_y1011_nogcses == T & special_school == F & exposure_highest_4_grp_y49 == "CPP"])),
                     rbind(describe_outcome(exposure = outcomes[1], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & not_enrolled_y1011_nogcses == T & special_school == F & exposure_highest_4_grp_y49 == "CLA"]),
                           describe_outcome(exposure = outcomes[2], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & not_enrolled_y1011_nogcses == T & special_school == F & exposure_highest_4_grp_y49 == "CLA"]),
                           describe_outcome(exposure = outcomes[3], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & not_enrolled_y1011_nogcses == T & special_school == F & exposure_highest_4_grp_y49 == "CLA"]),
                           describe_outcome(exposure = outcomes[4], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & not_enrolled_y1011_nogcses == T & special_school == F & exposure_highest_4_grp_y49 == "CLA"]),
                           describe_outcome(exposure = outcomes[5], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & not_enrolled_y1011_nogcses == T & special_school == F & exposure_highest_4_grp_y49 == "CLA"])))  
colnames(final_output) <- c("None", "CiN", "CPP", "CLA")
write.csv(final_output, file = "OUTPUTS/CHAPTER 6/notenrolledy1011nogcses-missing-which-year-non-special.csv")  

final_output <-cbind(rbind(describe_outcome(exposure = outcomes[1], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & not_enrolled_y1011 == T & special_school == T & exposure_highest_4_grp_y49 == "None"]),
                           describe_outcome(exposure = outcomes[2], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & not_enrolled_y1011 == T & special_school == T & exposure_highest_4_grp_y49 == "None"]),
                           describe_outcome(exposure = outcomes[3], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & not_enrolled_y1011 == T & special_school == T & exposure_highest_4_grp_y49 == "None"]),
                           describe_outcome(exposure = outcomes[4], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & not_enrolled_y1011 == T & special_school == T & exposure_highest_4_grp_y49 == "None"]),
                           describe_outcome(exposure = outcomes[5], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & not_enrolled_y1011 == T & special_school == T & exposure_highest_4_grp_y49 == "None"])),
                     rbind(describe_outcome(exposure = outcomes[1], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & not_enrolled_y1011 == T & special_school == T & exposure_highest_4_grp_y49 == "CiN"]),
                           describe_outcome(exposure = outcomes[2], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & not_enrolled_y1011 == T & special_school == T & exposure_highest_4_grp_y49 == "CiN"]),
                           describe_outcome(exposure = outcomes[3], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & not_enrolled_y1011 == T & special_school == T & exposure_highest_4_grp_y49 == "CiN"]),
                           describe_outcome(exposure = outcomes[4], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & not_enrolled_y1011 == T & special_school == T & exposure_highest_4_grp_y49 == "CiN"]),
                           describe_outcome(exposure = outcomes[5], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & not_enrolled_y1011 == T & special_school == T & exposure_highest_4_grp_y49 == "CiN"])),
                     rbind(describe_outcome(exposure = outcomes[1], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & not_enrolled_y1011 == T & special_school == T & exposure_highest_4_grp_y49 == "CPP"]),
                           describe_outcome(exposure = outcomes[2], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & not_enrolled_y1011 == T & special_school == T & exposure_highest_4_grp_y49 == "CPP"]),
                           describe_outcome(exposure = outcomes[3], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & not_enrolled_y1011 == T & special_school == T & exposure_highest_4_grp_y49 == "CPP"]),
                           describe_outcome(exposure = outcomes[4], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & not_enrolled_y1011 == T & special_school == T & exposure_highest_4_grp_y49 == "CPP"]),
                           describe_outcome(exposure = outcomes[5], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & not_enrolled_y1011 == T & special_school == T & exposure_highest_4_grp_y49 == "CPP"])),
                     rbind(describe_outcome(exposure = outcomes[1], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & not_enrolled_y1011 == T & special_school == T & exposure_highest_4_grp_y49 == "CLA"]),
                           describe_outcome(exposure = outcomes[2], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & not_enrolled_y1011 == T & special_school == T & exposure_highest_4_grp_y49 == "CLA"]),
                           describe_outcome(exposure = outcomes[3], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & not_enrolled_y1011 == T & special_school == T & exposure_highest_4_grp_y49 == "CLA"]),
                           describe_outcome(exposure = outcomes[4], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & not_enrolled_y1011 == T & special_school == T & exposure_highest_4_grp_y49 == "CLA"]),
                           describe_outcome(exposure = outcomes[5], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & not_enrolled_y1011 == T & special_school == T & exposure_highest_4_grp_y49 == "CLA"]))) 
colnames(final_output) <- c("None", "CiN", "CPP", "CLA")
write.csv(final_output, file = "OUTPUTS/CHAPTER 6/notenrolledy1011-missing-which-year-special.csv")

rm(final_output, outcomes)


# GCSEs IN SUBSEQUENT YEAR ------------------------------------------------

# cohort 1
# subsequent year
ks4_c1 <- data.table(read.table(
  "P:/Working/WORKING DATA/KS4/KS4Pupil_2017_KS2_KS1.txt",
  header = T,
  sep = "\t",
  stringsAsFactors = F,
  skipNul = T
))

ks4_c1 <- ks4_c1[KS4_PupilMatchingRefAnonymous %in% cohort_all[cohort == 1]$pupilmatchingrefanonymous]
names(ks4_c1) <- tolower(gsub("KS4_", "", names(ks4_c1)))

keep <- c("pupilmatchingrefanonymous", "entry_5_3ng_ptq_ee")
ks4_c1 <- ks4_c1[, keep, with = F]
rm(keep)
ks4_c1[, entry_5_3ng_ptq_ee := max(entry_5_3ng_ptq_ee, na.rm = T), by = "pupilmatchingrefanonymous"]
ks4_c1 <- ks4_c1[!duplicated(ks4_c1)]
names(ks4_c1) <- c("pupilmatchingrefanonymous", "sat_5_gcses_subs_year")

cohort_all <- merge(cohort_all,
                    ks4_c1,
                    by = "pupilmatchingrefanonymous",
                    all.x = T,
                    sort = F)

rm(ks4_c1)

# previous year
ks4_c1 <- data.table(read.table(
  "P:/Working/WORKING DATA/KS4/KS4Pupil_2015_to_2016_KS2_KS1.txt",
  header = T,
  sep = "\t",
  stringsAsFactors = F,
  skipNul = T
))

ks4_c1 <- ks4_c1[KS4_ACADYR == "2014/2015"]
ks4_c1 <- ks4_c1[KS4_PupilMatchingRefAnonymous %in% cohort_all[cohort == 1]$pupilmatchingrefanonymous]
names(ks4_c1) <- tolower(gsub("KS4_", "", names(ks4_c1)))

keep <- c("pupilmatchingrefanonymous", "entry_5_ptq_ee")
ks4_c1 <- ks4_c1[, keep, with = F]
rm(keep)
ks4_c1[, entry_5_ptq_ee := max(entry_5_ptq_ee, na.rm = T), by = "pupilmatchingrefanonymous"]
ks4_c1 <- ks4_c1[!duplicated(ks4_c1)]
names(ks4_c1) <- c("pupilmatchingrefanonymous", "sat_5_gcses_prev_year_c1")

cohort_all <- merge(cohort_all,
                    ks4_c1,
                    by = "pupilmatchingrefanonymous",
                    all.x = T,
                    sort = F)
rm(ks4_c1)

# cohort 2 (can only do previous year)
ks4_c2 <- data.table(read.table(
  "P:/Working/WORKING DATA/KS4/KS4Pupil_2015_to_2016_KS2_KS1.txt",
  header = T,
  sep = "\t",
  stringsAsFactors = F,
  skipNul = T
))

ks4_c2 <- ks4_c2[KS4_ACADYR == "2015/2016"]
ks4_c2 <- ks4_c2[KS4_PupilMatchingRefAnonymous %in% cohort_all[cohort == 2]$pupilmatchingrefanonymous]
names(ks4_c2) <- tolower(gsub("KS4_", "", names(ks4_c2)))

keep <- c("pupilmatchingrefanonymous", "entry_5_3ng_ptq_ee")
ks4_c2 <- ks4_c2[, keep, with = F]
rm(keep)
ks4_c2[, entry_5_3ng_ptq_ee := max(entry_5_3ng_ptq_ee, na.rm = T), by = "pupilmatchingrefanonymous"]
ks4_c2 <- ks4_c2[!duplicated(ks4_c2)]
names(ks4_c2) <- c("pupilmatchingrefanonymous", "sat_5_gcses_prev_year_c2")

cohort_all <- merge(cohort_all,
                    ks4_c2,
                    by = "pupilmatchingrefanonymous",
                    all.x = T,
                    sort = F)
rm(ks4_c2)

cohort_all[, sat_5_gcses_prev_year := sat_5_gcses_prev_year_c1]
cohort_all[!is.na(sat_5_gcses_prev_year_c2), sat_5_gcses_prev_year := sat_5_gcses_prev_year_c2]
cohort_all[is.na(sat_5_gcses_prev_year), sat_5_gcses_prev_year := 0]
cohort_all[, sat_5_gcses_prev_year_c1 := NULL]
cohort_all[, sat_5_gcses_prev_year_c2 := NULL]

cohort_all[cohort == 1 & is.na(sat_5_gcses_subs_year), sat_5_gcses_subs_year := 0]

# analyse
table(cohort_all[year == 7 & sch_n_in_year == 1]$sat_5_gcses_prev_year)
table(cohort_all[year == 7 & sch_n_in_year == 1 & not_enrolled_y1011_nogcses == T]$sat_5_gcses_prev_year)
table(cohort_all[year == 7 & sch_n_in_year == 1 & not_enrolled_y1011_nogcses == T]$sat_5_gcses_prev_year,
      cohort_all[year == 7 & sch_n_in_year == 1 & not_enrolled_y1011_nogcses == T]$exposure_highest_4_grp_y49)

table(cohort_all[year == 7 & sch_n_in_year == 1 & cohort == 1]$sat_5_gcses_subs_year)
table(cohort_all[year == 7 & sch_n_in_year == 1 & not_enrolled_y1011_nogcses == T & cohort == 1]$sat_5_gcses_subs_year)
table(cohort_all[year == 7 & sch_n_in_year == 1 & not_enrolled_y1011_nogcses == T & cohort == 1]$sat_5_gcses_subs_year,
      cohort_all[year == 7 & sch_n_in_year == 1 & not_enrolled_y1011_nogcses == T & cohort == 1]$exposure_highest_4_grp_y49)

# CREATE MODEL DATA -------------------------------------------------------

model_data_non_special <- cohort_all[year == 7 & sch_n_in_year == 1  & special_school == F,
                         c("pupilmatchingrefanonymous", "cohort",
                           "female_clean", "eth_major", "language_clean", "ever_sen_y09", "highest_ever_sen_y09", "ever_aaps_y09",
                           "idaci_fsm", "region_major", "la", "idaci_quintiles", "fsmeligible",
                           "exposure_highest_4_grp_y49",
                           "appru", "appru_y09",
                           "not_enrolled_y1011",
                           "no_gcses",
                           "not_enrolled_y1011_nogcses")]

model_data_non_special[, region_major := relevel(region_major, ref = "South East")]
model_data_non_special <- model_data_non_special[order(la)]

model_data_special <- cohort_all[year == 7 & sch_n_in_year == 1  & special_school == T,
                                 c("pupilmatchingrefanonymous", "cohort",
                                   "female_clean", "eth_major", "language_clean",
                                   "idaci_fsm", "region_major", "la", "idaci_quintiles", "fsmeligible",
                                   "exposure_highest_4_grp_y49",
                                   "appru_y09",
                                   "not_enrolled_y1011",
                                   "no_gcses",
                                   "not_enrolled_y1011_nogcses")]

model_data_special[, region_major := relevel(region_major, ref = "South East")]
model_data_special <- model_data_special[order(la)]


# ** get LA vars -----------------------------------------------------------

# *** from LAIT -------------------------------------------------------------

per_ag_sw <- data.table(read.csv("P:/Working/WORKING DATA/LAIT/Percentage_agency_SW.csv"))
per_sw_turnover <- data.table(read.csv("P:/Working/WORKING DATA/LAIT/Percentage_SW_turnover.csv"))
sw_s251 <- data.table(read.csv("P:/Working/WORKING DATA/LAIT/SW_s251_OT_weekly_unit_costs.csv"))

# need to fill in NAs using nextd available value
per_ag_sw$per_ag_sw_year_used <- as.integer(rep(NA, nrow(per_ag_sw)))
per_sw_turnover$per_sw_turnover_year_used <- as.integer(rep(NA, nrow(per_ag_sw)))
sw_s251$sw_s251_year_used <- as.integer(rep(NA, nrow(per_ag_sw)))

per_ag_sw$y2014 <- as.double(per_ag_sw$y2014)
per_sw_turnover$y2014 <- as.double(per_sw_turnover$y2014)
sw_s251$y2014 <- as.double(sw_s251$y2014)

per_ag_sw$per_ag_sw <- per_ag_sw$y2014
per_sw_turnover$per_sw_turnover <- per_sw_turnover$y2014
sw_s251$sw_s251 <- sw_s251$y2014

for (i in 1:nrow(per_ag_sw)) {
  
  if (is.na(per_ag_sw[i]$per_ag_sw)) {
    y <- which(!is.na(per_ag_sw[i, 3:8]))[1]
    per_ag_sw[i]$per_ag_sw <- per_ag_sw[i, 2 + y, with = F]
    per_ag_sw$per_ag_sw_year_used[i] <- as.integer(substr(names(per_ag_sw)[2 + y], 2, 5))
  } else {
    per_ag_sw$per_ag_sw_year_used[i] <- 2014
  }
  
  if (is.na(per_sw_turnover[i]$per_sw_turnover)) {
    y <- which(!is.na(per_sw_turnover[i, 3:8]))[1]
    per_sw_turnover[i]$per_sw_turnover <- per_sw_turnover[i, 2 + y, with = F]
    per_sw_turnover$per_sw_turnover_year_used[i] <- as.integer(substr(names(per_sw_turnover)[2 + y], 2, 5))
  } else {
    per_sw_turnover$per_sw_turnover_year_used[i] <- 2014
  }
  
  if (is.na(sw_s251[i]$sw_s251)) {
    if (any(!is.na(sw_s251[i, 3:8]))) {
      y <- which(!is.na(sw_s251[i, 3:8]))[1]
      sw_s251[i]$sw_s251 <- sw_s251[i, 2 + y, with = F]
      sw_s251$sw_s251_year_used[i] <- as.integer(substr(names(sw_s251)[2 + y], 2, 5))
    }
  } else {
    sw_s251$sw_s251_year_used[i] <- 2014
  }
}

rm(i, y)

per_ag_sw <- per_ag_sw[la_id %in% model_data_non_special$la | la_id %in% model_data_special$la]
per_sw_turnover <- per_sw_turnover[la_id %in% model_data_non_special$la | la_id %in% model_data_special$la]
sw_s251 <- sw_s251[la_id %in% model_data_non_special$la | la_id %in% model_data_special$la]

table(per_ag_sw$per_ag_sw_year_used)
table(per_sw_turnover$per_sw_turnover_year_used)
table(sw_s251$sw_s251_year_used)

per_ag_sw <- per_ag_sw[, c("la_id", "per_ag_sw")]
per_sw_turnover <- per_sw_turnover[, c("la_id", "per_sw_turnover")]
sw_s251 <- sw_s251[, c("la_id", "sw_s251")]

lait_vars <- merge(per_ag_sw,
                   per_sw_turnover)
lait_vars <- merge(lait_vars,
                   sw_s251)

rm(sw_s251, per_ag_sw, per_sw_turnover)

#lait_vars <- lait_vars[la_id %in% model_data_non_special$la | la_id %in% model_data_special$la]

model_data_non_special <- merge(model_data_non_special,
                                lait_vars,
                                by.x = "la",
                                by.y = "la_id",
                                all.x = T)

# *** from NPD ----------------------------------------------------------------

c1 <- data.table(read.table(
  "P:/Working/WORKING DATA/Spring_Census_2012.txt",
  header = T,
  sep = "\t",
  stringsAsFactors = F
))

names(c1) <- tolower(gsub("_S.*", "", names(c1)))
c1 <- c1[!duplicated(pupilmatchingrefanonymous)]
c1 <- c1[ageatstartofacademicyear >= 5 & ageatstartofacademicyear <= 15]
c1 <- c1[, c("pupilmatchingrefanonymous", "la", "fsmeligible", "urn")]

pru <- data.table(read.table(
  "P:/Working/WORKING DATA/PRU_Census_2010_to_2013.txt",
  header = T,
  sep = "\t",
  stringsAsFactors = F
))

names(pru) <- tolower(gsub("PRU_", "", names(pru)))
pru$censusdate <- as.Date(pru$censusdate, format = "%Y-%m-%d")
pru <- pru[censusdate == as.Date("2012-01-19")]
pru <- pru[ageatstartofacademicyear >= 5 & ageatstartofacademicyear <= 15]
pru <- pru[!duplicated(pupilmatchingrefanonymous)]
pru <- pru[, c("pupilmatchingrefanonymous", "la", "fsmeligible", "urn")]
c1 <- rbind(c1, pru)
rm(pru)

ap <- data.table(read.table(
  "P:/Working/WORKING DATA/AP_Census_2008_to_2013.txt",
  header = T,
  sep = "\t",
  stringsAsFactors = F
))

names(ap) <- tolower(gsub("AP_", "", names(ap)))
ap <- ap[ap$acadyr == "2011/2012"]
ap <- ap[age_start >= 5 & age_start <= 15]
ap <- ap[!duplicated(pupilmatchingrefanonymous)]
ap <- ap[, c("pupilmatchingrefanonymous", "la", "fsmeligibility")]
names(ap) <- c("pupilmatchingrefanonymous", "la", "fsmeligible")
c1 <- rbind(c1, ap, fill = T)
rm(ap)

c1 <- c1[!duplicated(pupilmatchingrefanonymous)]
c1[is.na(fsmeligible)]$fsmeligible <- 0

c2 <- data.table(read.table(
  "P:/Working/WORKING DATA/Spring_Census_2013.txt",
  header = T,
  sep = "\t",
  stringsAsFactors = F
))

names(c2) <- tolower(gsub("_S.*", "", names(c2)))
c2 <- c2[!duplicated(pupilmatchingrefanonymous)]
c2 <- c2[ageatstartofacademicyear >= 5 & ageatstartofacademicyear <= 15]
c2 <- c2[, c("pupilmatchingrefanonymous", "la", "fsmeligible", "urn")]

pru <- data.table(read.table(
  "P:/Working/WORKING DATA/PRU_Census_2010_to_2013.txt",
  header = T,
  sep = "\t",
  stringsAsFactors = F
))

names(pru) <- tolower(gsub("PRU_", "", names(pru)))
pru$censusdate <- as.Date(pru$censusdate, format = "%Y-%m-%d")
pru <- pru[censusdate == as.Date("2013-01-17")]
pru <- pru[ageatstartofacademicyear >= 5 & ageatstartofacademicyear <= 15]
pru <- pru[!duplicated(pupilmatchingrefanonymous)]
pru <- pru[, c("pupilmatchingrefanonymous", "la", "fsmeligible", "urn")]
c2 <- rbind(c2, pru)
rm(pru)

ap <- data.table(read.table(
  "P:/Working/WORKING DATA/AP_Census_2008_to_2013.txt",
  header = T,
  sep = "\t",
  stringsAsFactors = F
))

names(ap) <- tolower(gsub("AP_", "", names(ap)))
ap <- ap[ap$acadyr == "2012/2013"]
ap <- ap[age_start >= 5 & age_start <= 15]
ap <- ap[!duplicated(pupilmatchingrefanonymous)]
ap <- ap[, c("pupilmatchingrefanonymous", "la", "fsmeligibility")]
names(ap) <- c("pupilmatchingrefanonymous", "la", "fsmeligible")
c2 <- rbind(c2, ap, fill = T)
rm(ap)

c2 <- c2[!duplicated(pupilmatchingrefanonymous)]
c2[is.na(fsmeligible)]$fsmeligible <- 0

# now get LA vars
# total NPD population in the year 7 year
c1[, la_n := .N, by = .(la)]
c2[, la_n := .N, by = .(la)]

# prop of FSM in the year 7 year divided by population in the year 7 year
c1[, la_fsm_n := sum(fsmeligible == 1), by = .(la)]
c2[, la_fsm_n := sum(fsmeligible == 1), by = .(la)]

c1[, la_fsm_prop := la_fsm_n / la_n]
c2[, la_fsm_prop := la_fsm_n / la_n]

# % children in special schools
## first get school type
dfe <- data.table(read.csv("P:/Working/WORKING DATA/SCHOOLS/DfE.csv", stringsAsFactors = F))
names(dfe) <- tolower(gsub("name", "", names(dfe)))

dfe1 <- dfe[urn %in% c1$urn]
dfe2 <- dfe[urn %in% c2$urn]

c1 <- merge(c1, dfe1[, c("urn", "establishmenttypegroup")],
            all.x = T,
            by = "urn")

c2 <- merge(c2, dfe2[, c("urn", "establishmenttypegroup")],
            all.x = T,
            by = "urn")

c1[, la_special_schools_n := sum(establishmenttypegroup == "Special schools", na.rm = T), by = .(la)]
c2[, la_special_schools_n := sum(establishmenttypegroup == "Special schools", na.rm = T), by = .(la)]

c1[, la_special_schools_prop := la_special_schools_n / la_n]
c2[, la_special_schools_prop := la_special_schools_n / la_n]

# merge
la_df_c1 <- data.table(
  la = c1$la,
  la_n_c1 = c1$la_n,
  la_fsm_n_c1 = c1$la_fsm_n,
  la_fsm_prop_c1 = c1$la_fsm_prop,
  la_special_schools_n_c1 = c1$la_special_schools_n,
  la_special_schools_prop_c1 = c1$la_special_schools_prop
)

la_df_c1 <- la_df_c1[!duplicated(la_df_c1)]

la_df_c2 <- data.table(
  la = c2$la,
  la_n_c2 = c2$la_n,
  la_fsm_n_c2 = c2$la_fsm_n,
  la_fsm_prop_c2 = c2$la_fsm_prop,
  la_special_schools_n_c2 = c2$la_special_schools_n,
  la_special_schools_prop_c2 = c2$la_special_schools_prop
)

la_df_c2 <- la_df_c2[!duplicated(la_df_c2)]

rm(c1, c2)

# import into model data
model_data_non_special <- merge(model_data_non_special,
                    la_df_c1,
                    by = "la",
                    all.x = T)

model_data_non_special <- merge(model_data_non_special,
                    la_df_c2,
                    by = "la",
                    all.x = T)

model_data_non_special[, la_n := la_n_c1]
model_data_non_special[cohort == 2, la_n := la_n_c2]
model_data_non_special[, la_fsm_n := la_fsm_n_c1]
model_data_non_special[cohort == 2, la_fsm_n := la_fsm_n_c2]
model_data_non_special[, la_fsm_prop := la_fsm_prop_c1]
model_data_non_special[cohort == 2, la_fsm_prop := la_fsm_prop_c2]
model_data_non_special[, la_special_schools_n := la_special_schools_n_c1]
model_data_non_special[cohort == 2, la_special_schools_n := la_special_schools_n_c2]
model_data_non_special[, la_special_schools_prop := la_special_schools_prop_c1]
model_data_non_special[cohort == 2, la_special_schools_prop := la_special_schools_prop_c2]

model_data_non_special[, la_n_c1 := NULL]
model_data_non_special[, la_n_c2 := NULL]
model_data_non_special[, la_fsm_n_c1 := NULL]
model_data_non_special[, la_fsm_n_c2 := NULL]
model_data_non_special[, la_fsm_prop_c1 := NULL]
model_data_non_special[, la_fsm_prop_c2 := NULL]
model_data_non_special[, la_special_schools_n_c1 := NULL]
model_data_non_special[, la_special_schools_n_c2 := NULL]
model_data_non_special[, la_special_schools_prop_c1 := NULL]
model_data_non_special[, la_special_schools_prop_c2 := NULL]

# *** from CiN -------------------------------------------------------------

cin <- data.table(read.table("P:/Working/WORKING DATA/CIN_2009_2017.txt",
                             header = T,
                             sep = "\t",
                             skipNul = T,
                             stringsAsFactors = F,
                             na.strings = ""
))

cin[is.na(CIN_LA), CIN_LA := CIN_CIN_LA]
cin[, CIN_CIN_LA := NULL]
names(cin) <- tolower(gsub("CIN_", "", names(cin)))

cin <- cin[acadyr %in% c("2011/2012", "2012/2013")]
cin$dob <- as.Date(cin$dob, format = "%Y-%m-%d")
cin$aystart <- as.Date(paste0(as.integer(substr(cin$acadyr, 1, 4)), "-09-01"))
cin$cinclosuredate <- as.Date(cin$cinclosuredate, format = "%Y-%m-%d")
cin$ageatstartofacademicyear <- as.double(difftime(cin$aystart, cin$dob, units = "days") / 365.25)
cin <- cin[ageatstartofacademicyear >= 5 & ageatstartofacademicyear <= 15]
cin <- cin[referralnfa == 0 & (reasonforclosure != "RC8" | is.na(reasonforclosure))]
cin <- cin[, c("pupilmatchingrefanonymous", "lachildid_anon", "la", "acadyr", "referraldate", "cinclosuredate")]

cin <- cin[order(la, referraldate, cinclosuredate)]

drops <- cin$acadyr %in% c("2011/2012", "2012/2013") & is.na(cin$referraldate)
cin <- cin[!drops]
rm(drops)

cin_c1 <- cin[acadyr == "2011/2012"]
cin_c2 <- cin[acadyr == "2012/2013"]

rm(cin)

# number of open cases and number of new cases
cin_c1[, open_cin_n := .N, by = .(la)]
cin_c2[, open_cin_n := .N, by = .(la)]

cin_c1[, new_cin_count := referraldate >= as.Date("2011-09-01")]
cin_c1[, new_cin_n := sum(new_cin_count), by = .(la)]
cin_c2[, new_cin_count := referraldate >= as.Date("2011-09-01")]
cin_c2[, new_cin_n := sum(new_cin_count), by = .(la)]

cin_c1 <- cin_c1[, c("la", "open_cin_n", "new_cin_n")]
cin_c1 <- cin_c1[!duplicated(cin_c1)]

cin_c2 <- cin_c2[, c("la", "open_cin_n", "new_cin_n")]
cin_c2 <- cin_c2[!duplicated(cin_c2)]

# 311 and 316 no data so use data from next year
cin_c1 <- rbind(cin_c1,
                cbind(la = 311, open_cin_n = cin_c2[la == 311]$open_cin_n, new_cin_n = cin_c2[la == 311]$new_cin_n),
                cbind(la = 316, open_cin_n = cin_c2[la == 316]$open_cin_n, new_cin_n = cin_c2[la == 316]$new_cin_n))

names(cin_c1) <- c("la", "open_cin_n_c1", "new_cin_n_c1")
names(cin_c2) <- c("la", "open_cin_n_c2", "new_cin_n_c2")

la_df_c1 <- merge(la_df_c1,
                  cin_c1,
                  by = "la",
                  all.x = T)

la_df_c2 <- merge(la_df_c2,
                  cin_c2,
                  by = "la",
                  all.x = T)

rm(cin_c1, cin_c2)

la_df_c1[, open_cin_prop_c1 := open_cin_n_c1 / la_n_c1]
la_df_c2[, open_cin_prop_c2 := open_cin_n_c2 / la_n_c2]

la_df_c1[, new_cin_prop_c1 := new_cin_n_c1 / la_n_c1]
la_df_c2[, new_cin_prop_c2 := new_cin_n_c2 / la_n_c2]

# import into model data
model_data_non_special <- merge(model_data_non_special,
                    la_df_c1[, c("la", "open_cin_n_c1", "open_cin_prop_c1", "new_cin_n_c1", "new_cin_prop_c1")],
                    by = "la",
                    all.x = T)

model_data_non_special <- merge(model_data_non_special,
                    la_df_c2[, c("la", "open_cin_n_c2", "open_cin_prop_c2", "new_cin_n_c2", "new_cin_prop_c2")],
                    by = "la",
                    all.x = T)

model_data_non_special[, open_cin_n := open_cin_n_c1]
model_data_non_special[cohort == 2, open_cin_n := open_cin_n_c2]
model_data_non_special[, new_cin_n := new_cin_n_c1]
model_data_non_special[cohort == 2, new_cin_n := new_cin_n_c2]

model_data_non_special[, open_cin_prop := open_cin_prop_c1]
model_data_non_special[cohort == 2, open_cin_prop := open_cin_prop_c2]
model_data_non_special[, new_cin_prop := new_cin_prop_c1]
model_data_non_special[cohort == 2, new_cin_prop := new_cin_prop_c2]

model_data_non_special[, open_cin_n_c1 := NULL]
model_data_non_special[, open_cin_n_c2 := NULL]
model_data_non_special[, open_cin_prop_c1 := NULL]
model_data_non_special[, open_cin_prop_c2 := NULL]
model_data_non_special[, new_cin_n_c1 := NULL]
model_data_non_special[, new_cin_n_c2 := NULL]
model_data_non_special[, new_cin_prop_c1 := NULL]
model_data_non_special[, new_cin_prop_c2 := NULL]

# * add LA variables to special ------------------------------------------------------

model_data_special <- merge(model_data_special,
                            lait_vars,
                            by.x = "la",
                            by.y = "la_id",
                            all.x = T)

model_data_special <- merge(model_data_special,
                            la_df_c1,
                            by = "la",
                            all.x = T)

model_data_special <- merge(model_data_special,
                            la_df_c2,
                            by = "la",
                            all.x = T)

model_data_special[, la_n := la_n_c1]
model_data_special[cohort == 2, la_n := la_n_c2]
model_data_special[, la_fsm_n := la_fsm_n_c1]
model_data_special[cohort == 2, la_fsm_n := la_fsm_n_c2]
model_data_special[, la_fsm_prop := la_fsm_prop_c1]
model_data_special[cohort == 2, la_fsm_prop := la_fsm_prop_c2]
model_data_special[, la_special_schools_n := la_special_schools_n_c1]
model_data_special[cohort == 2, la_special_schools_n := la_special_schools_n_c2]
model_data_special[, la_special_schools_prop := la_special_schools_prop_c1]
model_data_special[cohort == 2, la_special_schools_prop := la_special_schools_prop_c2]

model_data_special[, la_n_c1 := NULL]
model_data_special[, la_n_c2 := NULL]
model_data_special[, la_fsm_n_c1 := NULL]
model_data_special[, la_fsm_n_c2 := NULL]
model_data_special[, la_fsm_prop_c1 := NULL]
model_data_special[, la_fsm_prop_c2 := NULL]
model_data_special[, la_special_schools_n_c1 := NULL]
model_data_special[, la_special_schools_n_c2 := NULL]
model_data_special[, la_special_schools_prop_c1 := NULL]
model_data_special[, la_special_schools_prop_c2 := NULL]

model_data_special[, open_cin_n := open_cin_n_c1]
model_data_special[cohort == 2, open_cin_n := open_cin_n_c2]
model_data_special[, new_cin_n := new_cin_n_c1]
model_data_special[cohort == 2, new_cin_n := new_cin_n_c2]

model_data_special[, open_cin_prop := open_cin_prop_c1]
model_data_special[cohort == 2, open_cin_prop := open_cin_prop_c2]
model_data_special[, new_cin_prop := new_cin_prop_c1]
model_data_special[cohort == 2, new_cin_prop := new_cin_prop_c2]

model_data_special[, open_cin_n_c1 := NULL]
model_data_special[, open_cin_n_c2 := NULL]
model_data_special[, open_cin_prop_c1 := NULL]
model_data_special[, open_cin_prop_c2 := NULL]
model_data_special[, new_cin_n_c1 := NULL]
model_data_special[, new_cin_n_c2 := NULL]
model_data_special[, new_cin_prop_c1 := NULL]
model_data_special[, new_cin_prop_c2 := NULL]

# DESCRIBE LA VARS --------------------------------------------------------

# LAIT vars are commonn to both cohorts so should get their own table
lait_vars <- lait_vars[la_id %in% la_df_c1$la]

# plot(lait_vars[, 2:4])
# lapply(lait_vars[, 2:4], FUN = hist) # all quite skewed

vars <- c("per_ag_sw", "per_sw_turnover", "sw_s251")
write.csv(print(CreateTableOne(data = lait_vars,
                               vars = vars,
                               test = F,
                               includeNA = T),
                nonnormal = vars,
                showAllLevels = T,
                printToggle = F,
                noSpaces = T,
                minMax = T),
          file = "OUTPUTS/CHAPTER 6/lait-vars-la-level-minmax.csv")
write.csv(print(CreateTableOne(data = lait_vars,
                               vars = vars,
                               test = F,
                               includeNA = T),
                nonnormal = vars,
                showAllLevels = T,
                printToggle = F,
                noSpaces = T),
          file = "OUTPUTS/CHAPTER 6/lait-vars-la-level.csv")
rm(vars)

# vars from NPD
# plot(la_df_c1[, 2:8])
# lapply(la_df_c1[, 2:8], FUN = hist) # all quite skewed

# LA level
la_df_c1 <- la_df_c1[la %in% cohort_all[cohort == 1 & sch_n_in_year == 1 & year == 7]$la]
la_df_c2 <- la_df_c2[la %in% cohort_all[cohort == 2 & sch_n_in_year == 1 & year == 7]$la]

# convert to percentage
la_df_c1$la_fsm_pc_c1 <- la_df_c1$la_fsm_prop_c1 * 100
la_df_c1$open_cin_pc_c1 <- la_df_c1$open_cin_prop_c1 * 100
la_df_c1$new_cin_pc_c1 <- la_df_c1$new_cin_prop_c1 * 100
la_df_c1$la_special_schools_pc_c1 <- la_df_c1$la_special_schools_prop_c1 * 100

la_df_c2$la_fsm_pc_c2 <- la_df_c2$la_fsm_prop_c2 * 100
la_df_c2$open_cin_pc_c2 <- la_df_c2$open_cin_prop_c2 * 100
la_df_c2$new_cin_pc_c2 <- la_df_c2$new_cin_prop_c2 * 100
la_df_c2$la_special_schools_pc_c2 <- la_df_c2$la_special_schools_prop_c2 * 100

vars <- c("la_n_c1", "la_fsm_pc_c1", "open_cin_pc_c1", "new_cin_pc_c1", "la_special_schools_pc_c1")
write.csv(print(CreateTableOne(data = la_df_c1,
                               vars = vars,
                               test = F,
                               includeNA = T),
                nonnormal = vars,
                showAllLevels = T,
                printToggle = F,
                noSpaces = T),
          file = "OUTPUTS/CHAPTER 6/la-npd-vars-cohort1.csv")
write.csv(print(CreateTableOne(data = la_df_c1,
                               vars = vars,
                               test = F,
                               includeNA = T),
                nonnormal = vars,
                showAllLevels = T,
                printToggle = F,
                noSpaces = T,
                minMax = T),
          file = "OUTPUTS/CHAPTER 6/la-npd-vars-cohort1-minmax.csv")

vars <- c("la_n_c2", "la_fsm_pc_c2", "open_cin_pc_c2", "new_cin_pc_c2", "la_special_schools_pc_c2")
write.csv(print(CreateTableOne(data = la_df_c2,
                               vars = vars,
                               test = F,
                               includeNA = T),
                nonnormal = vars,
                showAllLevels = T,
                printToggle = F,
                noSpaces = T),
          file = "OUTPUTS/CHAPTER 6/la-npd-vars-cohort2.csv")
write.csv(print(CreateTableOne(data = la_df_c2,
                               vars = vars,
                               test = F,
                               includeNA = T),
                nonnormal = vars,
                showAllLevels = T,
                printToggle = F,
                noSpaces = T,
                minMax = T),
          file = "OUTPUTS/CHAPTER 6/la-npd-vars-cohort2-minmax.csv")

rm(vars, la_df_c1, la_df_c2, lait_vars)

# OUTCOMES DESC BY LA VARS ------------------------------------------------

# * not special -----------------------------------------------------------

# categorise for illustration purposes
# non special
model_data_non_special$per_ag_sw_cat <- as.integer(cut(model_data_non_special$per_ag_sw,
                                                       breaks = quantile(model_data_non_special$per_ag_sw,
                                                                         probs = seq(0, 1, 0.25)),
                                                       include.lowest = T,
                                                       right = T))

model_data_non_special$per_sw_turnover_cat <- as.integer(cut(model_data_non_special$per_sw_turnover,
                                                             breaks = quantile(model_data_non_special$per_sw_turnover,
                                                                               probs = seq(0, 1, 0.25)),
                                                             include.lowest = T,
                                                             right = T))

model_data_non_special$sw_s251_cat <- as.integer(cut(model_data_non_special$sw_s251,
                                                     breaks = quantile(model_data_non_special$sw_s251,
                                                                       probs = seq(0, 1, 0.25)),
                                                     include.lowest = T,
                                                     right = T))

model_data_non_special$la_n_cat <- as.integer(cut(model_data_non_special$la_n,
                                                  breaks = quantile(model_data_non_special$la_n,
                                                                    probs = seq(0, 1, 0.25)),
                                                  include.lowest = T,
                                                  right = T))

model_data_non_special$la_fsm_prop_cat <- as.integer(cut(model_data_non_special$la_fsm_prop,
                                                         breaks = quantile(model_data_non_special$la_fsm_prop,
                                                                           probs = seq(0, 1, 0.25)),
                                                         include.lowest = T,
                                                         right = T))

model_data_non_special$open_cin_prop_cat <- as.integer(cut(model_data_non_special$open_cin_prop,
                                                           breaks = quantile(model_data_non_special$open_cin_prop,
                                                                             probs = seq(0, 1, 0.25)),
                                                           include.lowest = T,
                                                           right = T))

model_data_non_special$new_cin_prop_cat <- as.integer(cut(model_data_non_special$new_cin_prop,
                                                          breaks = quantile(model_data_non_special$new_cin_prop,
                                                                            probs = seq(0, 1, 0.25)),
                                                          include.lowest = T,
                                                          right = T))

model_data_non_special$la_special_schools_prop_cat <- as.integer(cut(model_data_non_special$la_special_schools_prop,
                                                                     breaks = quantile(model_data_non_special$la_special_schools_prop,
                                                                                       probs = seq(0, 1, 0.25)),
                                                                     include.lowest = T,
                                                                     right = T))

exposures <- c("per_ag_sw_cat",
               "per_sw_turnover_cat",
               "sw_s251_cat",
               "la_n_cat",
               "la_fsm_prop_cat",
               "open_cin_prop_cat",
               "new_cin_prop_cat",
               "la_special_schools_prop_cat")

outcomes <- c("not_enrolled_y1011",
              "no_gcses",
              "not_enrolled_y1011_nogcses")

final_output <- cbind(describe_outcome(exposure = exposures, outcome = outcomes[1], dat = model_data_non_special, p_round = 1),
                      describe_outcome(exposure = exposures, outcome = outcomes[2], dat = model_data_non_special, p_round = 1),
                      describe_outcome(exposure = exposures, outcome = outcomes[3], dat = model_data_non_special, p_round = 1))
write.csv(final_output, file = "OUTPUTS/CHAPTER 6/outcomes-by-LA-RFs-not-special.csv")

final_output <- cbind(describe_outcome(exposure = exposures, outcome = outcomes[1], dat = model_data_non_special[exposure_highest_4_grp_y49 == "None"], p_round = 1),
                      describe_outcome(exposure = exposures, outcome = outcomes[2], dat = model_data_non_special[exposure_highest_4_grp_y49 == "None"], p_round = 1),
                      describe_outcome(exposure = exposures, outcome = outcomes[3], dat = model_data_non_special[exposure_highest_4_grp_y49 == "None"], p_round = 1))
write.csv(final_output, file = "OUTPUTS/CHAPTER 6/outcomes-by-LA-RFs-not-special-nocsc.csv")

final_output <- cbind(describe_outcome(exposure = exposures, outcome = outcomes[1], dat = model_data_non_special[exposure_highest_4_grp_y49 == "CiN"], p_round = 1),
                      describe_outcome(exposure = exposures, outcome = outcomes[2], dat = model_data_non_special[exposure_highest_4_grp_y49 == "CiN"], p_round = 1),
                      describe_outcome(exposure = exposures, outcome = outcomes[3], dat = model_data_non_special[exposure_highest_4_grp_y49 == "CiN"], p_round = 1))
write.csv(final_output, file = "OUTPUTS/CHAPTER 6/outcomes-by-LA-RFs-not-special-cin.csv")

final_output <- cbind(describe_outcome(exposure = exposures, outcome = outcomes[1], dat = model_data_non_special[exposure_highest_4_grp_y49 == "CPP"], p_round = 1),
                      describe_outcome(exposure = exposures, outcome = outcomes[2], dat = model_data_non_special[exposure_highest_4_grp_y49 == "CPP"], p_round = 1),
                      describe_outcome(exposure = exposures, outcome = outcomes[3], dat = model_data_non_special[exposure_highest_4_grp_y49 == "CPP"], p_round = 1))
write.csv(final_output, file = "OUTPUTS/CHAPTER 6/outcomes-by-LA-RFs-not-special-cpp.csv")

final_output <- cbind(describe_outcome(exposure = exposures, outcome = outcomes[1], dat = model_data_non_special[exposure_highest_4_grp_y49 == "CLA"], p_round = 1),
                      describe_outcome(exposure = exposures, outcome = outcomes[2], dat = model_data_non_special[exposure_highest_4_grp_y49 == "CLA"], p_round = 1),
                      describe_outcome(exposure = exposures, outcome = outcomes[3], dat = model_data_non_special[exposure_highest_4_grp_y49 == "CLA"], p_round = 1))
write.csv(final_output, file = "OUTPUTS/CHAPTER 6/outcomes-by-LA-RFs-not-special-cla.csv")

# * special ---------------------------------------------------------------

model_data_special$per_ag_sw_cat <- as.integer(cut(model_data_special$per_ag_sw,
                                                   breaks = quantile(model_data_special$per_ag_sw,
                                                                     probs = seq(0, 1, 0.25)),
                                                   include.lowest = T,
                                                   right = T))

model_data_special$per_sw_turnover_cat <- as.integer(cut(model_data_special$per_sw_turnover,
                                                         breaks = quantile(model_data_special$per_sw_turnover,
                                                                           probs = seq(0, 1, 0.25)),
                                                         include.lowest = T,
                                                         right = T))

model_data_special$sw_s251_cat <- as.integer(cut(model_data_special$sw_s251,
                                                 breaks = quantile(model_data_special$sw_s251,
                                                                   probs = seq(0, 1, 0.25)),
                                                 include.lowest = T,
                                                 right = T))

model_data_special$la_n_cat <- as.integer(cut(model_data_special$la_n,
                                              breaks = quantile(model_data_special$la_n,
                                                                probs = seq(0, 1, 0.25)),
                                              include.lowest = T,
                                              right = T))

model_data_special$la_fsm_prop_cat <- as.integer(cut(model_data_special$la_fsm_prop,
                                                     breaks = quantile(model_data_special$la_fsm_prop,
                                                                       probs = seq(0, 1, 0.25)),
                                                     include.lowest = T,
                                                     right = T))

model_data_special$open_cin_prop_cat <- as.integer(cut(model_data_special$open_cin_prop,
                                                       breaks = quantile(model_data_special$open_cin_prop,
                                                                         probs = seq(0, 1, 0.25)),
                                                       include.lowest = T,
                                                       right = T))

model_data_special$new_cin_prop_cat <- as.integer(cut(model_data_special$new_cin_prop,
                                                      breaks = quantile(model_data_special$new_cin_prop,
                                                                        probs = seq(0, 1, 0.25)),
                                                      include.lowest = T,
                                                      right = T))

model_data_special$la_special_schools_prop_cat <- as.integer(cut(model_data_special$new_cin_prop,
                                                                 breaks = quantile(model_data_special$new_cin_prop,
                                                                                   probs = seq(0, 1, 0.25)),
                                                                 include.lowest = T,
                                                                 right = T))

final_output <- cbind(describe_outcome(exposure = exposures, outcome = outcomes[1], dat = model_data_special, p_round = 1),
                      describe_outcome(exposure = exposures, outcome = outcomes[2], dat = model_data_special, p_round = 1),
                      describe_outcome(exposure = exposures, outcome = outcomes[3], dat = model_data_special, p_round = 1))
write.csv(final_output, file = "OUTPUTS/CHAPTER 6/outcomes-by-LA-RFs-special.csv")

final_output <- cbind(describe_outcome(exposure = exposures, outcome = outcomes[1], dat = model_data_special[exposure_highest_4_grp_y49 == "None"], p_round = 1),
                      describe_outcome(exposure = exposures, outcome = outcomes[2], dat = model_data_special[exposure_highest_4_grp_y49 == "None"], p_round = 1),
                      describe_outcome(exposure = exposures, outcome = outcomes[3], dat = model_data_special[exposure_highest_4_grp_y49 == "None"], p_round = 1))
write.csv(final_output, file = "OUTPUTS/CHAPTER 6/outcomes-by-LA-RFs-special-nocsc.csv")

final_output <- cbind(describe_outcome(exposure = exposures, outcome = outcomes[1], dat = model_data_special[exposure_highest_4_grp_y49 == "CiN"], p_round = 1),
                      describe_outcome(exposure = exposures, outcome = outcomes[2], dat = model_data_special[exposure_highest_4_grp_y49 == "CiN"], p_round = 1),
                      describe_outcome(exposure = exposures, outcome = outcomes[3], dat = model_data_special[exposure_highest_4_grp_y49 == "CiN"], p_round = 1))
write.csv(final_output, file = "OUTPUTS/CHAPTER 6/outcomes-by-LA-RFs-special-cin.csv")

final_output <- cbind(describe_outcome(exposure = exposures, outcome = outcomes[1], dat = model_data_special[exposure_highest_4_grp_y49 == "CPP"], p_round = 1),
                      describe_outcome(exposure = exposures, outcome = outcomes[2], dat = model_data_special[exposure_highest_4_grp_y49 == "CPP"], p_round = 1),
                      describe_outcome(exposure = exposures, outcome = outcomes[3], dat = model_data_special[exposure_highest_4_grp_y49 == "CPP"], p_round = 1))
write.csv(final_output, file = "OUTPUTS/CHAPTER 6/outcomes-by-LA-RFs-special-cpp.csv")

final_output <- cbind(describe_outcome(exposure = exposures, outcome = outcomes[1], dat = model_data_special[exposure_highest_4_grp_y49 == "CLA"], p_round = 1),
                      describe_outcome(exposure = exposures, outcome = outcomes[2], dat = model_data_special[exposure_highest_4_grp_y49 == "CLA"], p_round = 1),
                      describe_outcome(exposure = exposures, outcome = outcomes[3], dat = model_data_special[exposure_highest_4_grp_y49 == "CLA"], p_round = 1))
write.csv(final_output, file = "OUTPUTS/CHAPTER 6/outcomes-by-LA-RFs-special-cla.csv")

rm(exposures, outcomes, dfe, dfe1, dfe2, final_output)


# SAVE AND LOAD MODEL DATA ------------------------------------------------

# recode IDACI
model_data_non_special[, idaci_quintiles := factor(idaci_quintiles, levels = 5:1)]
model_data_special[, idaci_quintiles := factor(idaci_quintiles, levels = 5:1)]

save(model_data_non_special, file = "PROCESSED DATA/OFFROLLING_model_data_non_special.rda")
save(model_data_special, file = "PROCESSED DATA/OFFROLLING_model_data_special.rda")

load("PROCESSED DATA/OFFROLLING_model_data_non_special.rda")
load("PROCESSED DATA/OFFROLLING_model_data_special.rda")

# MODELLING - NOT SPECIAL ------------------------------------------------------

# * describe size of units ---------------------------------------------------

# how many pupils per LA
model_data_non_special[, pupils_per_la := length(pupilmatchingrefanonymous), by = .(la)]

tmp <- data.table(
  la = model_data_non_special$la,
  pupils_per_la = model_data_non_special$pupils_per_la
)

tmp <- tmp[!duplicated(tmp)]

hist(tmp$pupils_per_la, breaks = 15)
summary(tmp$pupils_per_la)

# how many pupils and LAs per region
model_data_non_special[, pupils_per_region := length(pupilmatchingrefanonymous), by = .(region_major)]
model_data_non_special[, las_per_region := length(unique(la)), by = .(region_major)]

tmp <- data.table(
  region_major = model_data_non_special$region_major,
  pupils_per_region = model_data_non_special$pupils_per_region,
  las_per_region = model_data_non_special$las_per_region
)

tmp <- tmp[!duplicated(tmp)]

tmp
summary(tmp$pupils_per_region)
summary(tmp$las_per_region)

rm(tmp)

# * LA and regional variation ----------------------------------------------

# ** graphical -------------------------------------------------------------

tmp <- data.table(
  la = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F]$la,
  region = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F]$region_major,
  not_enrolled_y1011_nogcses = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F]$not_enrolled_y1011_nogcses
)

tmp <- tmp[order(la)]
tmp[, la_pop_n := max(seq_len(.N)), by = la]

tmp[, la_not_enrolled_y1011_nogcses_n := sum(not_enrolled_y1011_nogcses), by = la]
tmp[, la_not_enrolled_y1011_nogcses_prop := mean(not_enrolled_y1011_nogcses), by = la]
tmp[, la_not_enrolled_y1011_nogcses_se := sqrt((la_not_enrolled_y1011_nogcses_prop * (1 - la_not_enrolled_y1011_nogcses_prop)) / la_pop_n)]

tmp <- tmp[, c(1:2, 4:7)]
tmp <- tmp[!duplicated(tmp)]

tmp <- tmp[order(region, la_not_enrolled_y1011_nogcses_prop)]
tmp$la_int <- 1:151
nrow(tmp[la_not_enrolled_y1011_nogcses_n < 10])

tiff("OUTPUTS/CHAPTER 6/offrolling-by-la-y1011.tiff",
     width = 12, height = 6, units = "in", res = 300)
ggplot(tmp[la_not_enrolled_y1011_nogcses_n >= 10], aes(x = la_int, y = la_not_enrolled_y1011_nogcses_prop, colour = region)) +
  geom_point() +
  geom_errorbar(ymin = tmp[la_not_enrolled_y1011_nogcses_n >= 10]$la_not_enrolled_y1011_nogcses_prop - 1.96 * tmp[la_not_enrolled_y1011_nogcses_n >= 10]$la_not_enrolled_y1011_nogcses_se,
                ymax = tmp[la_not_enrolled_y1011_nogcses_n >= 10]$la_not_enrolled_y1011_nogcses_prop + 1.96 * tmp[la_not_enrolled_y1011_nogcses_n >= 10]$la_not_enrolled_y1011_nogcses_se) +
  scale_y_continuous(limits = c(0, 0.1)) +
  xlab("") +
  ylab("Not enrolled yr 10/11 and sat <5 GCSEs\n(proportion)") +
  ggtitle("Not special schools") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text = element_text(colour = "black"),
        legend.key = element_blank(),
        legend.position = c(0.5, 0.9)) +
  guides(colour = guide_legend(nrow = 3)) +
  labs(colour = "Region")
dev.off()

write.csv(tmp, file = "OUTPUTS/CHAPTER 6/offrolling-by-la-y1011-underlying-data.csv")

# ** numerical -------------------------------------------------------------

# LA level
summary(tmp$la_not_enrolled_y1011_nogcses_n)
summary(tmp$la_not_enrolled_y1011_nogcses_prop)
hist(tmp$la_not_enrolled_y1011_nogcses_n, breaks = 15)
hist(tmp$la_not_enrolled_y1011_nogcses_prop, breaks = 15)
mean(tmp$la_not_enrolled_y1011_nogcses_n < 10)

# regional level
#tmp <- tmp[la_not_enrolled_y1011_nogcses_n >= 6]

tmp[, region_pop_n := sum(la_pop_n), by = .(region)]
tmp[, region_not_enrolled_y1011_nogcses_n := sum(la_not_enrolled_y1011_nogcses_n), by = .(region)]
tmp[, region_not_enrolled_y1011_nogcses_prop := region_not_enrolled_y1011_nogcses_n / region_pop_n]

tmp[, region_min_y1011_nogcses_prop := min(la_not_enrolled_y1011_nogcses_prop), by = .(region)]
tmp[, region_min_y1011_nogcses_n := min(la_not_enrolled_y1011_nogcses_n), by = .(region)]

tmp[, region_25c_y1011_nogcses_prop := quantile(la_not_enrolled_y1011_nogcses_prop, 0.25), by = .(region)]
tmp[, region_25c_y1011_nogcses_n := quantile(la_not_enrolled_y1011_nogcses_n, 0.25), by = .(region)]

tmp[, region_50c_y1011_nogcses_prop := quantile(la_not_enrolled_y1011_nogcses_prop, 0.5), by = .(region)]
tmp[, region_50c_y1011_nogcses_n := quantile(la_not_enrolled_y1011_nogcses_n, 0.50), by = .(region)]

tmp[, region_75c_y1011_nogcses_prop := quantile(la_not_enrolled_y1011_nogcses_prop, 0.75), by = .(region)]
tmp[, region_75c_y1011_nogcses_n := quantile(la_not_enrolled_y1011_nogcses_n, 0.75), by = .(region)]

tmp[, region_max_y1011_nogcses_prop := max(la_not_enrolled_y1011_nogcses_prop), by = .(region)]

tmp <- tmp[, c("region", "region_pop_n", "region_not_enrolled_y1011_nogcses_n", "region_not_enrolled_y1011_nogcses_prop",
               "region_min_y1011_nogcses_prop", "region_min_y1011_nogcses_n",
               "region_25c_y1011_nogcses_prop", "region_25c_y1011_nogcses_n",
               "region_50c_y1011_nogcses_prop", "region_50c_y1011_nogcses_n",
               "region_75c_y1011_nogcses_prop", "region_75c_y1011_nogcses_n",
               "region_max_y1011_nogcses_prop")]
tmp <- tmp[!duplicated(tmp)]
tmp

write.csv(tmp, file = "OUTPUTS/CHAPTER 6/outcomes-region-ns.csv")

summary(tmp$region_not_enrolled_y1011_nogcses_n)
summary(tmp$region_not_enrolled_y1011_nogcses_prop)

rm(tmp)

# * estimate hierarchical ----------------------------------------------------------

# ** univar models --------------------------------------------------------

gc()
um1 <- glmer(not_enrolled_y1011_nogcses ~ exposure_highest_4_grp_y49 +
               (1 | la) + (1 | region_major),
             data = model_data_non_special,
             family = binomial)

gc()
um2 <- glmer(not_enrolled_y1011_nogcses ~ highest_ever_sen_y09 +
               (1 | la) + (1 | region_major),
             data = model_data_non_special,
             family = binomial)

gc()
um3 <- glmer(not_enrolled_y1011_nogcses ~ ever_sen_y09 +
               (1 | la) + (1 | region_major),
             data = model_data_non_special,
             family = binomial)

gc()
um4 <- glmer(not_enrolled_y1011_nogcses ~ female_clean +
               (1 | la) + (1 | region_major),
             data = model_data_non_special,
             family = binomial)

gc()
um5 <- glmer(not_enrolled_y1011_nogcses ~ eth_major +
               (1 | la) + (1 | region_major),
             data = model_data_non_special,
             family = binomial)

gc()
um6 <- glmer(not_enrolled_y1011_nogcses ~ language_clean +
               (1 | la) + (1 | region_major),
             data = model_data_non_special,
             family = binomial)

gc()
um7 <- glmer(not_enrolled_y1011_nogcses ~ idaci_fsm +
               (1 | la) + (1 | region_major),
             data = model_data_non_special,
             family = binomial)

gc()
um8 <- glmer(not_enrolled_y1011_nogcses ~ appru_y09 +
               (1 | la) + (1 | region_major),
             data = model_data_non_special,
             family = binomial,
             control = glmerControl(optimizer = "Nelder_Mead",
                                    optCtrl = list(maxfun = 1e6)))

gc()
um9 <- glmer(not_enrolled_y1011_nogcses ~ idaci_quintiles +
               (1 | la) + (1 | region_major),
             data = model_data_non_special,
             family = binomial)

gc()
um10 <- glmer(not_enrolled_y1011_nogcses ~ fsmeligible +
                (1 | la) + (1 | region_major),
              data = model_data_non_special,
              family = binomial)

gc()
um11 <- glmer(not_enrolled_y1011_nogcses ~ I(per_ag_sw / 10) +
                (1 | la) + (1 | region_major),
              data = model_data_non_special,
              family = binomial)

gc()
um12 <- glmer(not_enrolled_y1011_nogcses ~ I(per_sw_turnover / 10) +
                (1 | la) + (1 | region_major),
              data = model_data_non_special,
              family = binomial)

gc()
um13 <- glmer(not_enrolled_y1011_nogcses ~ I(la_n / 10000) +
                (1 | la) + (1 | region_major),
              data = model_data_non_special,
              family = binomial)

gc()
um14 <- glmer(not_enrolled_y1011_nogcses ~ I(la_fsm_prop * 10) +
                (1 | la) + (1 | region_major),
              data = model_data_non_special,
              family = binomial)

gc()
um15 <- glmer(not_enrolled_y1011_nogcses ~ I(open_cin_prop * 100) +
                (1 | la) + (1 | region_major),
              data = model_data_non_special,
              family = binomial)

gc()
um16 <- glmer(not_enrolled_y1011_nogcses ~ I(new_cin_prop * 100) +
                (1 | la) + (1 | region_major),
              data = model_data_non_special,
              family = binomial)

gc()
um17 <- glmer(not_enrolled_y1011_nogcses ~ I(sw_s251 / 100) +
                (1 | la) + (1 | region_major),
              data = model_data_non_special,
              family = binomial)

gc()
um18 <- glmer(not_enrolled_y1011_nogcses ~ I(la_special_schools_prop * 100) +
                (1 | la) + (1 | region_major),
              data = model_data_non_special,
              family = binomial)

gc()
models_offrolling_univar_glm_ns <- list()
models_offrolling_univar_glm_ns[[1]] <- um1
models_offrolling_univar_glm_ns[[2]] <- um2
models_offrolling_univar_glm_ns[[3]] <- um3
models_offrolling_univar_glm_ns[[4]] <- um4
models_offrolling_univar_glm_ns[[5]] <- um5
models_offrolling_univar_glm_ns[[6]] <- um6
models_offrolling_univar_glm_ns[[7]] <- um7
models_offrolling_univar_glm_ns[[8]] <- um8
models_offrolling_univar_glm_ns[[9]] <- um9
models_offrolling_univar_glm_ns[[10]] <- um10
models_offrolling_univar_glm_ns[[11]] <- um11
models_offrolling_univar_glm_ns[[12]] <- um12
models_offrolling_univar_glm_ns[[13]] <- um13
models_offrolling_univar_glm_ns[[14]] <- um14
models_offrolling_univar_glm_ns[[15]] <- um15
models_offrolling_univar_glm_ns[[16]] <- um16
models_offrolling_univar_glm_ns[[17]] <- um17
models_offrolling_univar_glm_ns[[18]] <- um18

save(models_offrolling_univar_glm_ns, file = "PROCESSED DATA/MODELS/MODELS_OFFROLLING_UNIVAR_GLM_NOT_SPECIAL.RDA")
rm(um2, um3, um4, um5, um6, um7, um8, um9, um10, um11, um12, um13, um14, um15, um16, um17, um18)

summary(models_offrolling_univar_glm_ns[[1]])
summary(models_offrolling_univar_glm_ns[[2]])
summary(models_offrolling_univar_glm_ns[[3]])
summary(models_offrolling_univar_glm_ns[[4]])
summary(models_offrolling_univar_glm_ns[[5]])
summary(models_offrolling_univar_glm_ns[[6]])
summary(models_offrolling_univar_glm_ns[[7]])
summary(models_offrolling_univar_glm_ns[[8]])
summary(models_offrolling_univar_glm_ns[[9]])
summary(models_offrolling_univar_glm_ns[[10]])
summary(models_offrolling_univar_glm_ns[[11]])
summary(models_offrolling_univar_glm_ns[[12]])
summary(models_offrolling_univar_glm_ns[[13]])
summary(models_offrolling_univar_glm_ns[[14]])
summary(models_offrolling_univar_glm_ns[[15]])
summary(models_offrolling_univar_glm_ns[[16]])
summary(models_offrolling_univar_glm_ns[[17]])
summary(models_offrolling_univar_glm_ns[[18]])

rm(models_offrolling_univar_glm_ns)

# test new_cin and open_cin for collineraity
gc()
cinm1 <- glmer(not_enrolled_y1011_nogcses ~ I(new_cin_prop * 100) + I(open_cin_prop * 100) +
                 (1 | la) + (1 | region_major),
               data = model_data_non_special,
               family = binomial)

summary(cinm1)
rm(cinm1) # seems pretty collinear so just go with new cin

# ** main models ----------------------------------------------------------

gc()
hm1 <- glmer(not_enrolled_y1011_nogcses ~ 1 +
               (1 | la) + (1 | region_major),
             data = model_data_non_special,
             family = binomial)

gc()
hm3 <- glmer(not_enrolled_y1011_nogcses ~ exposure_highest_4_grp_y49 + ever_sen_y09 +
               (1 | la) + (1 | region_major),
             data = model_data_non_special,
             family = binomial)

gc()
hm4 <- glmer(not_enrolled_y1011_nogcses ~ exposure_highest_4_grp_y49 * ever_sen_y09 +
               (1 | la) + (1 | region_major),
             data = model_data_non_special,
             family = binomial)

gc()
hm5 <- glmer(not_enrolled_y1011_nogcses ~ exposure_highest_4_grp_y49 * ever_sen_y09 +
               female_clean + eth_major + language_clean + idaci_fsm + appru_y09 +
               (1 | la) + (1 | region_major),
             data = model_data_non_special,
             family = binomial,
             control = glmerControl(optimizer = "Nelder_Mead",
                                    optCtrl = list(maxfun = 2e6)))

gc()
hm5a <- glmer(not_enrolled_y1011_nogcses ~ exposure_highest_4_grp_y49 * ever_sen_y09 +
                female_clean + eth_major + language_clean + idaci_quintiles + appru_y09 +
                (1 | la) + (1 | region_major),
              data = model_data_non_special,
              family = binomial,
              control = glmerControl(optimizer = "Nelder_Mead",
                                     optCtrl = list(maxfun = 1e6)))

gc()
hm5b <- glmer(not_enrolled_y1011_nogcses ~ exposure_highest_4_grp_y49 * ever_sen_y09 +
               female_clean + eth_major + language_clean + idaci_fsm + appru_y09 +
               (1 | la) + (1 | region_major),
             data = model_data_non_special,
             family = binomial,
             control = glmerControl(optimizer = "Nelder_Mead",
                                    check.conv.grad = .makeCC("warning", tol = 2e-3, relTol = NULL),
                                    optCtrl = list(maxfun = 2e6)))


gc()
hm6 <- glmer(not_enrolled_y1011_nogcses ~ exposure_highest_4_grp_y49 * ever_sen_y09 +
               female_clean + eth_major + language_clean + idaci_fsm + appru_y09 +
               I(per_ag_sw / 10) + I(per_sw_turnover / 10) + I(sw_s251 / 100) + I(la_n / 10000) + I(la_fsm_prop * 10) +
               I(new_cin_prop * 100) + I(la_special_schools_prop * 100) +
               (1 | la) + (1 | region_major),
             data = model_data_non_special,
             family = binomial,
             control = glmerControl(optimizer = "Nelder_Mead",
                                    optCtrl = list(maxfun = 2e6))) # FTC

gc()
hm6a <- glmer(not_enrolled_y1011_nogcses ~ exposure_highest_4_grp_y49 * ever_sen_y09 +
                female_clean + eth_major + language_clean + idaci_fsm + appru_y09 +
                I(per_sw_turnover / 10) + I(la_fsm_prop * 10) +
                I(new_cin_prop * 100) +
                (1 | la) + (1 | region_major),
              data = model_data_non_special,
              family = binomial,
              control = glmerControl(optimizer = "Nelder_Mead",
                                     optCtrl = list(maxfun = 2e6))) # FTC

gc()
hm6b <- glmer(not_enrolled_y1011_nogcses ~ exposure_highest_4_grp_y49 * ever_sen_y09 +
                female_clean + eth_major + language_clean + idaci_fsm + appru_y09 +
                I(per_sw_turnover / 10) + I(la_fsm_prop * 10) +
                I(new_cin_prop * 100) +
                (1 | la) + (1 | region_major),
              data = model_data_non_special,
              family = binomial,
              control = glmerControl(optimizer = "Nelder_Mead",
                                     check.conv.grad = .makeCC("warning", tol = 2e-3, relTol = NULL),
                                     optCtrl = list(maxfun = 2e6)))

gc()
hm6_posthoc <- glmer(not_enrolled_y1011_nogcses ~ exposure_highest_4_grp_y49 * ever_sen_y09 +
                       I(per_ag_sw / 10) + I(per_sw_turnover / 10) + I(sw_s251 / 100) + I(la_n / 10000) + I(la_fsm_prop * 10) +
                       I(new_cin_prop * 100) + I(la_special_schools_prop * 100) +
                       (1 | la) + (1 | region_major),
                     data = model_data_non_special,
                     family = binomial,
                     control = glmerControl(#optimizer = "Nelder_Mead",
                                            check.conv.grad = .makeCC("warning", tol = 2e-3, relTol = NULL),
                                            #optCtrl = list(maxfun = 1e6)
                                            )) # ftc def 0.00107368 / 1 mil 0.00107368 / nm 0.00523208

models_offrolling_glm_ns <- list()
models_offrolling_glm_ns[[1]] <- hm1
models_offrolling_glm_ns[[2]] <- um1
models_offrolling_glm_ns[[3]] <- hm3
models_offrolling_glm_ns[[4]] <- hm4
models_offrolling_glm_ns[[5]] <- hm5b
models_offrolling_glm_ns[[6]] <- hm6b
models_offrolling_glm_ns[[7]] <- hm6_posthoc
save(models_offrolling_glm_ns, file = "PROCESSED DATA/MODELS/MODELS_OFFROLLING_GLM_NOT_SPECIAL.RDA")
rm(hm1, um1, hm3, hm4, hm5, hm5a, hm5b, hm6, hm6a, hm6b, hm6_posthoc)

summary(models_offrolling_glm_ns[[1]])
summary(models_offrolling_glm_ns[[2]])
summary(models_offrolling_glm_ns[[3]])
summary(models_offrolling_glm_ns[[4]])
summary(models_offrolling_glm_ns[[5]])
summary(models_offrolling_glm_ns[[6]])

lrtest(models_offrolling_glm_ns[[2]], models_offrolling_glm_ns[[1]])
lrtest(models_offrolling_glm_ns[[3]], models_offrolling_glm_ns[[2]])
lrtest(models_offrolling_glm_ns[[4]], models_offrolling_glm_ns[[3]])
lrtest(models_offrolling_glm_ns[[5]], models_offrolling_glm_ns[[4]])
lrtest(models_offrolling_glm_ns[[6]], models_offrolling_glm_ns[[5]])

summary(models_offrolling_glm_ns[[7]])
lrtest(models_offrolling_glm_ns[[7]], models_offrolling_glm_ns[[4]])

rm(models_offrolling_glm_ns)

# ** random slopes --------------------------------------------------------

model_data_non_special[, any_csc := exposure_highest_4_grp_y49 != "None"]
table(model_data_non_special$any_csc)
prop.table(table(model_data_non_special$any_csc))
table(model_data_non_special$any_csc,
      model_data_non_special$exposure_highest_4_grp_y49,
      useNA = "always")

gc()
hm_posthoc_random_slopes_1 <- glmer(not_enrolled_y1011_nogcses ~ any_csc + 
                                    (1 | la) + (1 | region_major),
                                  data = model_data_non_special,
                                  family = binomial,
                                  control = glmerControl(optimizer = "Nelder_Mead"))

gc()
hm_posthoc_random_slopes_2 <- glmer(not_enrolled_y1011_nogcses ~ any_csc + 
                                    (any_csc | la) + (1 | region_major),
                                  data = model_data_non_special,
                                  family = binomial,
                                  control = glmerControl(optimizer = "Nelder_Mead"))

hm_posthoc_random_slopes <- list()
hm_posthoc_random_slopes[[1]] <- hm_posthoc_random_slopes_1
hm_posthoc_random_slopes[[2]] <- hm_posthoc_random_slopes_2
save(hm_posthoc_random_slopes, file = "PROCESSED DATA/MODELS/MODELS_OFFROLLING_GLM_RANDOM_SLOPES_NOT_SPECIAL.RDA")

summary(hm_posthoc_random_slopes[[1]])
summary(hm_posthoc_random_slopes[[2]])
lrtest(hm_posthoc_random_slopes[[2]], hm_posthoc_random_slopes[[1]])

gc()
hm_posthoc_random_slopes_3 <- glmer(not_enrolled_y1011_nogcses ~ any_csc + 
                                      ever_sen_y09 + female_clean + eth_major + language_clean + idaci_fsm + appru_y09 +
                                      (any_csc | la) + (1 | region_major),
                                    data = model_data_non_special,
                                    family = binomial,
                                    control = glmerControl(optimizer = "Nelder_Mead")) # FTC 1.4183

gc()
hm_posthoc_random_slopes_4 <- glmer(not_enrolled_y1011_nogcses ~ exposure_highest_4_grp_y49 + 
                                      (exposure_highest_4_grp_y49 | la) + (1 | region_major),
                                    data = model_data_non_special,
                                    family = binomial,
                                    control = glmerControl(optimizer = "Nelder_Mead")) # ftc 0.532965


# ** LA models ------------------------------------------------------------

# gc()
# hm6h <- glmer(not_enrolled_y1011_nogcses ~ I(per_ag_sw / 10) + I(per_sw_turnover / 10) + I(sw_s251 / 100) +
#                 I(la_n / 10000) + I(la_fsm_prop * 10) + I(new_cin_prop * 100) +
#                 (1 | la) + (1 | region_major),
#               data = model_data_non_special,
#               family = binomial,
#               control = glmerControl(optimizer = "Nelder_Mead",
#                                      optCtrl = list(maxfun = 2e6))) # FTC
#
# gc()
# hm6i <- glmer(not_enrolled_y1011_nogcses ~ I(per_ag_sw / 10) + I(per_sw_turnover / 10) + I(sw_s251 / 100) +
#                 I(la_fsm_prop * 10) + I(new_cin_prop * 100) +
#                 (1 | la) + (1 | region_major),
#               data = model_data_non_special,
#               family = binomial,
#               control = glmerControl(optimizer = "Nelder_Mead",
#                                      optCtrl = list(maxfun = 2e6)))
# 
# gc()
# hm6j <- glmer(not_enrolled_y1011_nogcses ~ exposure_highest_4_grp_y49 +
#                 I(per_ag_sw / 10) + I(per_sw_turnover / 10) + I(sw_s251 / 100) +
#                 I(la_fsm_prop * 10) + I(new_cin_prop * 100) +
#                 (1 | la) + (1 | region_major),
#               data = model_data_non_special,
#               family = binomial,
#               control = glmerControl(optimizer = "Nelder_Mead",
#                                      optCtrl = list(maxfun = 2e6)))
# 
# 
# models_offrolling_la_level_glm_ns <- list()
# models_offrolling_la_level_glm_ns[[1]] <- hm6h
# models_offrolling_la_level_glm_ns[[2]] <- hm6i
# models_offrolling_la_level_glm_ns[[3]] <- hm6j
# 
# save(hm6i, file = "PROCESSED DATA/MODELS/MODELS_OFFROLLING_GLM_NOT_SPECIAL_LA_VARS_ONLY.RDA")
# rm(models_offrolling_la_level_glm_ns)


# ** cross-level interactions ---------------------------------------------

x1a <- glmer(not_enrolled_y1011_nogcses ~ exposure_highest_4_grp_y49 + I(per_ag_sw / 10) +
               (1 | la) + (1 | region_major),
             data = model_data_non_special,
             family = binomial)
x1b <- glmer(not_enrolled_y1011_nogcses ~ exposure_highest_4_grp_y49 * I(per_ag_sw / 10) +
               (1 | la) + (1 | region_major),
             data = model_data_non_special,
             family = binomial)

x2a <- glmer(not_enrolled_y1011_nogcses ~ exposure_highest_4_grp_y49 + I(per_sw_turnover / 10) +
               (1 | la) + (1 | region_major),
             data = model_data_non_special,
             family = binomial)
x2b <- glmer(not_enrolled_y1011_nogcses ~ exposure_highest_4_grp_y49 * I(per_sw_turnover / 10) +
               (1 | la) + (1 | region_major),
             data = model_data_non_special,
             family = binomial,
             control = glmerControl(optimizer = "Nelder_Mead",
                                    check.conv.grad = .makeCC("warning", tol = 2e-3, relTol = NULL)))

x3a <- glmer(not_enrolled_y1011_nogcses ~ exposure_highest_4_grp_y49 + I(sw_s251 / 100) +
               (1 | la) + (1 | region_major),
             data = model_data_non_special,
             family = binomial,
             control = glmerControl(optimizer = "Nelder_Mead",
                                    optCtrl = list(maxfun = 1e6)))
x3b <- glmer(not_enrolled_y1011_nogcses ~ exposure_highest_4_grp_y49 * I(sw_s251 / 100) +
               (1 | la) + (1 | region_major),
             data = model_data_non_special,
             family = binomial,
             control = glmerControl(optimizer = "Nelder_Mead",
                                    check.conv.grad = .makeCC("warning", tol = 2e-3, relTol = NULL)))

x4a <- glmer(not_enrolled_y1011_nogcses ~ exposure_highest_4_grp_y49 + I(la_fsm_prop * 10) +
               (1 | la) + (1 | region_major),
             data = model_data_non_special,
             family = binomial)
x4b <- glmer(not_enrolled_y1011_nogcses ~ exposure_highest_4_grp_y49 * I(la_fsm_prop * 10) +
               (1 | la) + (1 | region_major),
             data = model_data_non_special,
             family = binomial,
             control = glmerControl(optimizer = "Nelder_Mead",
                                    check.conv.grad = .makeCC("warning", tol = 2e-3, relTol = NULL),
                                    optCtrl = list(maxfun = 1e6)))

x5a <- glmer(not_enrolled_y1011_nogcses ~ exposure_highest_4_grp_y49 + I(new_cin_prop * 100) +
               (1 | la) + (1 | region_major),
             data = model_data_non_special,
             family = binomial)
x5b <- glmer(not_enrolled_y1011_nogcses ~ exposure_highest_4_grp_y49 * I(new_cin_prop * 100) +
               (1 | la) + (1 | region_major),
             data = model_data_non_special,
             family = binomial,
             control = glmerControl(optCtrl = list(maxfun = 2e6))) #  0.00354779, 0.00727101 1e6 and NM and 2e6

x6a <- glmer(not_enrolled_y1011_nogcses ~ exposure_highest_4_grp_y49 + I(la_special_schools_prop * 10) +
               (1 | la) + (1 | region_major),
             data = model_data_non_special,
             family = binomial)
x6b <- glmer(not_enrolled_y1011_nogcses ~ exposure_highest_4_grp_y49 * I(la_special_schools_prop * 10) +
               (1 | la) + (1 | region_major),
             data = model_data_non_special,
             family = binomial,
             control = glmerControl(optimizer = "Nelder_Mead",
                                    optCtrl = list(maxfun = 1e6)))

models_offrolling_x_int_glm_ns <- list()
models_offrolling_x_int_glm_ns[[1]] <- x1a
models_offrolling_x_int_glm_ns[[2]] <- x1b
models_offrolling_x_int_glm_ns[[3]] <- x2a
models_offrolling_x_int_glm_ns[[4]] <- x2b
models_offrolling_x_int_glm_ns[[5]] <- x3a
models_offrolling_x_int_glm_ns[[6]] <- x3b
models_offrolling_x_int_glm_ns[[7]] <- x4a
models_offrolling_x_int_glm_ns[[8]] <- x4b
models_offrolling_x_int_glm_ns[[9]] <- x5a
models_offrolling_x_int_glm_ns[[10]] <- x5b
models_offrolling_x_int_glm_ns[[11]] <- x6a
models_offrolling_x_int_glm_ns[[12]] <- x6b
save(models_offrolling_x_int_glm_ns, file = "PROCESSED DATA/MODELS/MODELS_OFFROLLING_GLM_NOT_SPECIAL_XINT.RDA")
rm(x1a, x1b, x2a, x2b, x3a, x3b, x4a, x4b, x5a, x5b, x6a, x6b)

summary(models_offrolling_x_int_glm_ns[[1]])
summary(models_offrolling_x_int_glm_ns[[2]])
lrtest(models_offrolling_x_int_glm_ns[[2]], models_offrolling_x_int_glm_ns[[1]])

summary(models_offrolling_x_int_glm_ns[[3]])
summary(models_offrolling_x_int_glm_ns[[4]])
lrtest(models_offrolling_x_int_glm_ns[[4]], models_offrolling_x_int_glm_ns[[3]])

summary(models_offrolling_x_int_glm_ns[[5]])
summary(models_offrolling_x_int_glm_ns[[6]])
lrtest(models_offrolling_x_int_glm_ns[[6]], models_offrolling_x_int_glm_ns[[5]])

summary(models_offrolling_x_int_glm_ns[[7]])
summary(models_offrolling_x_int_glm_ns[[8]])
lrtest(models_offrolling_x_int_glm_ns[[8]], models_offrolling_x_int_glm_ns[[7]])

summary(models_offrolling_x_int_glm_ns[[9]])
summary(models_offrolling_x_int_glm_ns[[10]])
lrtest(models_offrolling_x_int_glm_ns[[10]], models_offrolling_x_int_glm_ns[[9]])

summary(models_offrolling_x_int_glm_ns[[11]])
summary(models_offrolling_x_int_glm_ns[[12]])
lrtest(models_offrolling_x_int_glm_ns[[12]], models_offrolling_x_int_glm_ns[[11]])

# ** cross-level with adjustments -----------------------------------------

gc()
x1b_adj <- glmer(not_enrolled_y1011_nogcses ~ exposure_highest_4_grp_y49 * I(per_ag_sw / 10) +
                   ever_sen_y09 + female_clean + eth_major + language_clean + idaci_fsm + appru_y09 +
                   (1 | la) + (1 | region_major),
                 data = model_data_non_special,
                 family = binomial,
                 control = glmerControl(optimizer = "Nelder_Mead",
                                        check.conv.grad = .makeCC("warning", tol = 2e-3, relTol = NULL),
                                        optCtrl = list(maxfun = 2e6)))


gc()
x2b_adj <- glmer(not_enrolled_y1011_nogcses ~ exposure_highest_4_grp_y49 * I(per_sw_turnover / 10) +
                   ever_sen_y09 + female_clean + eth_major + language_clean + idaci_fsm + appru_y09 +
                   (1 | la) + (1 | region_major),
                 data = model_data_non_special,
                 family = binomial,
                 control = glmerControl(optimizer = "Nelder_Mead",
                                        check.conv.grad = .makeCC("warning", tol = 2e-3, relTol = NULL),
                                        optCtrl = list(maxfun = 2e6)))


gc()
x3b_adj <- glmer(not_enrolled_y1011_nogcses ~ exposure_highest_4_grp_y49 * I(sw_s251 / 100) +
                   ever_sen_y09 + female_clean + eth_major + language_clean + idaci_fsm + appru_y09 +
                   (1 | la) + (1 | region_major),
                 data = model_data_non_special,
                 family = binomial,
                 control = glmerControl(optimizer = "Nelder_Mead",
                                        check.conv.grad = .makeCC("warning", tol = 2e-3, relTol = NULL),
                                        optCtrl = list(maxfun = 2e6)))

gc()
x4b_adj <- glmer(not_enrolled_y1011_nogcses ~ exposure_highest_4_grp_y49 * I(la_fsm_prop * 10) +
                   ever_sen_y09 + female_clean + eth_major + language_clean + idaci_fsm + appru_y09 +
                   (1 | la) + (1 | region_major),
                 data = model_data_non_special,
                 family = binomial,
                 control = glmerControl(optimizer = "Nelder_Mead",
                                        check.conv.grad = .makeCC("warning", tol = 2e-3, relTol = NULL),
                                        optCtrl = list(maxfun = 2e6)))

x5b_adj <- glmer(not_enrolled_y1011_nogcses ~ exposure_highest_4_grp_y49 * I(new_cin_prop * 100) +
                   ever_sen_y09 + female_clean + eth_major + language_clean + idaci_fsm + appru_y09 +
                   (1 | la) + (1 | region_major),
                 data = model_data_non_special,
                 family = binomial,
                 control = glmerControl(optimizer = "Nelder_Mead",
                                        check.conv.grad = .makeCC("warning", tol = 2e-3, relTol = NULL),
                                        optCtrl = list(maxfun = 2e6)))

gc()
x6b_adj <- glmer(not_enrolled_y1011_nogcses ~ exposure_highest_4_grp_y49 * I(la_special_schools_prop * 10) +
                   ever_sen_y09 + female_clean + eth_major + language_clean + idaci_fsm + appru_y09 +
                   (1 | la) + (1 | region_major),
                 data = model_data_non_special,
                 family = binomial,
                 control = glmerControl(optimizer = "Nelder_Mead",
                                        check.conv.grad = .makeCC("warning", tol = 2e-3, relTol = NULL),
                                        optCtrl = list(maxfun = 2e6)))

models_offrolling_x_int_adj_glm_ns <- list()
models_offrolling_x_int_adj_glm_ns[[1]] <- x1b_adj
models_offrolling_x_int_adj_glm_ns[[2]] <- x2b_adj
models_offrolling_x_int_adj_glm_ns[[3]] <- x3b_adj
models_offrolling_x_int_adj_glm_ns[[4]] <- x4b_adj
models_offrolling_x_int_adj_glm_ns[[5]] <- x5b_adj
models_offrolling_x_int_adj_glm_ns[[6]] <- x6b_adj
save(models_offrolling_x_int_adj_glm_ns, file = "PROCESSED DATA/MODELS/MODELS_OFFROLLING_GLM_NOT_SPECIAL_XINT_ADJ.RDA")
rm(x1b_adj, x2b_adj, x3b_adj, x4b_adj, x5b_adj, x6b_adj)

summary(models_offrolling_x_int_adj_glm_ns[[1]])
lrtest(models_offrolling_x_int_adj_glm_ns[[1]], models_offrolling_x_int_glm_ns[[2]])

summary(models_offrolling_x_int_adj_glm_ns[[2]])
lrtest(models_offrolling_x_int_adj_glm_ns[[2]], models_offrolling_x_int_glm_ns[[4]])

summary(models_offrolling_x_int_adj_glm_ns[[3]])
lrtest(models_offrolling_x_int_adj_glm_ns[[3]], models_offrolling_x_int_glm_ns[[6]])

summary(models_offrolling_x_int_adj_glm_ns[[4]])
lrtest(models_offrolling_x_int_adj_glm_ns[[4]], models_offrolling_x_int_glm_ns[[8]])

summary(models_offrolling_x_int_adj_glm_ns[[5]])
lrtest(models_offrolling_x_int_adj_glm_ns[[5]], models_offrolling_x_int_glm_ns[[10]])

summary(models_offrolling_x_int_adj_glm_ns[[6]])
lrtest(models_offrolling_x_int_adj_glm_ns[[6]], models_offrolling_x_int_glm_ns[[12]])

# la adjustment
gc()
x1b_adj_la <- glmer(not_enrolled_y1011_nogcses ~ exposure_highest_4_grp_y49 * I(per_ag_sw / 10) +
                      ever_sen_y09 + female_clean + eth_major + language_clean + idaci_fsm + appru_y09 +
                      I(per_sw_turnover / 10) + I(sw_s251 / 100) + I(la_fsm_prop * 10) + I(new_cin_prop * 100) + I(la_special_schools_prop * 10) +
                      (1 | la) + (1 | region_major),
                    data = model_data_non_special,
                    family = binomial,
                    control = glmerControl(optimizer = "Nelder_Mead",
                                           check.conv.grad = .makeCC("warning", tol = 2e-3, relTol = NULL),
                                           optCtrl = list(maxfun = 2e6))) # FTC 0.15

gc()
x1b_adj_la_2 <- glmer(not_enrolled_y1011_nogcses ~ exposure_highest_4_grp_y49 * I(per_ag_sw / 10) +
                      ever_sen_y09 + female_clean + eth_major + language_clean + idaci_fsm + appru_y09 +
                      I(per_sw_turnover / 10) + I(sw_s251 / 100) + I(la_fsm_prop * 10) + I(new_cin_prop * 100) + I(la_special_schools_prop * 10) +
                      (1 | la) + (1 | region_major),
                    data = model_data_non_special,
                    family = binomial,
                    control = glmerControl(optimizer = "bobyqa",
                                           check.conv.grad = .makeCC("warning", tol = 2e-3, relTol = NULL),
                                           optCtrl = list(maxfun = 2e6)))

gc()
x1b_adj_la_3 <- glmer(not_enrolled_y1011_nogcses ~ exposure_highest_4_grp_y49 * I(per_ag_sw / 10) +
                        ever_sen_y09 + female_clean + eth_major + language_clean + idaci_fsm + appru_y09 +
                        I(per_sw_turnover / 10) + I(la_fsm_prop * 10) + I(new_cin_prop * 100) +
                        (1 | la) + (1 | region_major),
                      data = model_data_non_special,
                      family = binomial,
                      control = glmerControl(optimizer = "bobyqa",
                                             check.conv.grad = .makeCC("warning", tol = 2e-3, relTol = NULL),
                                             optCtrl = list(maxfun = 2e6)))

gc()
x1b_adj_la_4 <- glmer(not_enrolled_y1011_nogcses ~ exposure_highest_4_grp_y49 * I(per_ag_sw / 10) +
                        ever_sen_y09 + female_clean + eth_major + language_clean + idaci_fsm + appru_y09 +
                        I(per_sw_turnover / 10) + I(la_fsm_prop * 10) + I(new_cin_prop * 100) +
                        (1 | la) + (1 | region_major),
                      data = model_data_non_special,
                      family = binomial,
                      control = glmerControl(optimizer = "Nelder_Mead",
                                             check.conv.grad = .makeCC("warning", tol = 2e-3, relTol = NULL),
                                             optCtrl = list(maxfun = 2e6))) # ftc 0.004

gc()
x1b_adj_la_5 <- glmer(not_enrolled_y1011_nogcses ~ exposure_highest_4_grp_y49 * I(per_ag_sw / 10) +
                        I(per_sw_turnover / 10) + I(la_fsm_prop * 10) + I(new_cin_prop * 100) +
                        (1 | la) + (1 | region_major),
                      data = model_data_non_special,
                      family = binomial,
                      control = glmerControl(optimizer = "Nelder_Mead",
                                              optCtrl = list(maxfun = 2e6)))


gc()
x2b_adj_la <- glmer(not_enrolled_y1011_nogcses ~ exposure_highest_4_grp_y49 * I(per_sw_turnover / 10) +
                      I(la_fsm_prop * 10) + I(new_cin_prop * 100) +
                      (1 | la) + (1 | region_major),
                    data = model_data_non_special,
                    family = binomial,
                    control = glmerControl(optimizer = "Nelder_Mead",
                                           #check.conv.grad = .makeCC("warning", tol = 2e-3, relTol = NULL),
                                           optCtrl = list(maxfun = 2e6))) # FTC 0.0031754 NM / 0.0064 bobyqa

gc()
x3b_adj_la <- glmer(not_enrolled_y1011_nogcses ~ exposure_highest_4_grp_y49 * I(sw_s251/100) +
                      I(per_sw_turnover/100) + I(la_fsm_prop * 10) + I(new_cin_prop * 100) +
                      (1 | la) + (1 | region_major),
                    data = model_data_non_special,
                    family = binomial,
                    control = glmerControl(optimizer = "Nelder_Mead",
                                           check.conv.grad = .makeCC("warning", tol = 2e-3, relTol = NULL),
                                           optCtrl = list(maxfun = 2e6)))

gc()
x4b_adj_la <- glmer(not_enrolled_y1011_nogcses ~ exposure_highest_4_grp_y49 * I(la_fsm_prop * 10) +
                      I(per_sw_turnover / 100) + I(new_cin_prop * 100) +
                      (1 | la) + (1 | region_major),
                    data = model_data_non_special,
                    family = binomial,
                    control = glmerControl(optimizer = "Nelder_Mead",
                                           #check.conv.grad = .makeCC("warning", tol = 2e-3, relTol = NULL),
                                           optCtrl = list(maxfun = 2e6))) # ftc 0.0021, 0.027 BOBYQA

gc()
x5b_adj_la <- glmer(not_enrolled_y1011_nogcses ~ exposure_highest_4_grp_y49 * I(new_cin_prop * 100) +
                      I(per_sw_turnover / 100) + I(la_fsm_prop * 100) +
                      (1 | la) + (1 | region_major),
                    data = model_data_non_special,
                    family = binomial,
                    control = glmerControl(optimizer = "Nelder_Mead",
                                           check.conv.grad = .makeCC("warning", tol = 2e-3, relTol = NULL),
                                           optCtrl = list(maxfun = 2e6)))

gc()
x6b_adj_la <- glmer(not_enrolled_y1011_nogcses ~ exposure_highest_4_grp_y49 * I(la_special_schools_prop * 10) +
                      I(per_sw_turnover / 100) + I(la_fsm_prop * 100) + I(new_cin_prop * 10) +
                      (1 | la) + (1 | region_major),
                    data = model_data_non_special,
                    family = binomial,
                    control = glmerControl(optimizer = "Nelder_Mead",
                                           check.conv.grad = .makeCC("warning", tol = 2e-3, relTol = NULL),
                                           optCtrl = list(maxfun = 3e6))) # FTC 0.01 nm, 3E6, 0.02 bobyqa

models_offrolling_x_int_adj_la_glm_ns <- list()
models_offrolling_x_int_adj_la_glm_ns[[1]] <- x1b_adj_la_5
models_offrolling_x_int_adj_la_glm_ns[[2]] <- x2b_adj_la
models_offrolling_x_int_adj_la_glm_ns[[3]] <- x3b_adj_la
models_offrolling_x_int_adj_la_glm_ns[[4]] <- x4b_adj_la
models_offrolling_x_int_adj_la_glm_ns[[5]] <- x5b_adj_la
models_offrolling_x_int_adj_la_glm_ns[[6]] <- x6b_adj_la

save(models_offrolling_x_int_adj_la_glm_ns, file = "PROCESSED DATA/MODELS/MODELS_OFFROLLING_GLM_NOT_SPECIAL_XINT_ADJ_LA.RDA")
rm(x1b_adj_la_5, x2b_adj_la, x3b_adj_la, x4b_adj_la, x5b_adj_la, x6b_adj_la)

summary(models_offrolling_x_int_adj_la_glm_ns[[1]])
lrtest(models_offrolling_x_int_adj_la_glm_ns[[1]], models_offrolling_x_int_glm_ns[[2]])

summary(models_offrolling_x_int_adj_la_glm_ns[[2]])
lrtest(models_offrolling_x_int_adj_la_glm_ns[[2]], models_offrolling_x_int_glm_ns[[4]])

summary(models_offrolling_x_int_adj_la_glm_ns[[3]])
lrtest(models_offrolling_x_int_adj_la_glm_ns[[3]], models_offrolling_x_int_glm_ns[[6]])

summary(models_offrolling_x_int_adj_la_glm_ns[[4]])
lrtest(models_offrolling_x_int_adj_la_glm_ns[[4]], models_offrolling_x_int_glm_ns[[8]])

summary(models_offrolling_x_int_adj_la_glm_ns[[5]])
lrtest(models_offrolling_x_int_adj_la_glm_ns[[5]], models_offrolling_x_int_glm_ns[[10]])

# not outputting 6 as failed to converge

rm(models_offrolling_glm_ns, models_offrolling_x_int_glm_ns,
   models_offrolling_x_int_adj_glm_ns, models_offrolling_x_int_adj_la_glm_ns)

# MODELLING - SPECIAL -----------------------------------------------------

# * describe size of units ---------------------------------------------------

# how many pupils per LA
model_data_special[, pupils_per_la := length(pupilmatchingrefanonymous), by = .(la)]

tmp <- data.table(
  la = model_data_special$la,
  pupils_per_la = model_data_special$pupils_per_la
)

tmp <- tmp[!duplicated(tmp)]

hist(tmp$pupils_per_la, breaks = 15)
summary(tmp$pupils_per_la)

# how many pupils and LAs per region
model_data_special[, pupils_per_region := length(pupilmatchingrefanonymous), by = .(region_major)]
model_data_special[, las_per_region := length(unique(la)), by = .(region_major)]

tmp <- data.table(
  region_major = model_data_special$region_major,
  pupils_per_region = model_data_special$pupils_per_region,
  las_per_region = model_data_special$las_per_region
)

tmp <- tmp[!duplicated(tmp)]

tmp
summary(tmp$pupils_per_region)
summary(tmp$las_per_region)

rm(tmp)

# * describe LA variation -------------------------------------------------

# ** graphical ------------------------------------------------------------

# tmp <- data.table(
#   la = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T]$la,
#   region = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T]$region_major,
#   not_enrolled_y1011_nogcses = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T]$not_enrolled_y1011_nogcses
# )

tmp <- data.table(
  la = model_data_special$la,
  region = model_data_special$region_major,
  not_enrolled_y1011 = model_data_special$not_enrolled_y1011
)


tmp <- tmp[order(la)]
tmp[, la_pop_n := max(seq_len(.N)), by = la]

tmp[, la_not_enrolled_y1011_n := sum(not_enrolled_y1011), by = la]
tmp[, la_not_enrolled_y1011_prop := mean(not_enrolled_y1011), by = la]
tmp[, la_not_enrolled_y1011_se := sqrt((la_not_enrolled_y1011_prop * (1 - la_not_enrolled_y1011_prop)) / la_pop_n)]

tmp <- tmp[, c(1:2, 4:7)]
tmp <- tmp[!duplicated(tmp)]

tmp <- tmp[order(region, la_not_enrolled_y1011_prop)]
tmp$la_int <- 1:148
nrow(tmp[la_not_enrolled_y1011_n < 10])

# ** numerical ------------------------------------------------------------

# LA level
summary(tmp$la_not_enrolled_y1011_n)
summary(tmp$la_not_enrolled_y1011_prop)
hist(tmp$la_not_enrolled_y1011_n, breaks = 20)
hist(tmp$la_not_enrolled_y1011_prop, breaks = 15)
mean(tmp$la_not_enrolled_y1011_n < 6) 
mean(tmp$la_not_enrolled_y1011_n == 0) 

# regional level
#tmp <- tmp[la_not_enrolled_y1011_n >= 6]

tmp[, region_pop_n := sum(la_pop_n), by = .(region)]
tmp[, region_not_enrolled_y1011_n := sum(la_not_enrolled_y1011_n), by = .(region)]
tmp[, region_not_enrolled_y1011_prop := region_not_enrolled_y1011_n / region_pop_n]

tmp[, region_min_y1011_prop := min(la_not_enrolled_y1011_prop), by = .(region)]
tmp[, region_min_y1011_n := min(la_not_enrolled_y1011_n), by = .(region)]

tmp[, region_25c_y1011_prop := quantile(la_not_enrolled_y1011_prop, 0.25), by = .(region)]
tmp[, region_25c_y1011_n := quantile(la_not_enrolled_y1011_n, 0.25), by = .(region)]

tmp[, region_50c_y1011_prop := quantile(la_not_enrolled_y1011_prop, 0.5), by = .(region)]
tmp[, region_50c_y1011_n := quantile(la_not_enrolled_y1011_n, 0.50), by = .(region)]

tmp[, region_75c_y1011_prop := quantile(la_not_enrolled_y1011_prop, 0.75), by = .(region)]
tmp[, region_75c_y1011_n := quantile(la_not_enrolled_y1011_n, 0.75), by = .(region)]

tmp[, region_max_y1011_prop := max(la_not_enrolled_y1011_prop), by = .(region)]

tmp <- tmp[, c("region", "region_pop_n", "region_not_enrolled_y1011_n", "region_not_enrolled_y1011_prop",
               "region_min_y1011_prop", "region_min_y1011_n",
               "region_25c_y1011_prop", "region_25c_y1011_n",
               "region_50c_y1011_prop", "region_50c_y1011_n",
               "region_75c_y1011_prop", "region_75c_y1011_n",
               "region_max_y1011_prop")]
tmp <- tmp[!duplicated(tmp)]
tmp

write.csv(tmp, file = "OUTPUTS/CHAPTER 6/outcomes-region-s.csv")

summary(tmp$region_not_enrolled_y1011_n)
summary(tmp$region_not_enrolled_y1011_prop)

rm(tmp)

# * hierarchical ------------------------------------------------------------

# ** univar models --------------------------------------------------------

gc()
um1 <- glmer(not_enrolled_y1011~ exposure_highest_4_grp_y49 +
               (1 | la),
             data = model_data_special,
             family = binomial)

gc()
um2 <- glmer(not_enrolled_y1011 ~ female_clean +
               (1 | la),
             data = model_data_special,
             family = binomial)

gc()
um3 <- glmer(not_enrolled_y1011 ~ eth_major +
               (1 | la),
             data = model_data_special,
             family = binomial)

gc()
um4 <- glmer(not_enrolled_y1011 ~ language_clean +
               (1 | la),
             data = model_data_special,
             family = binomial)

gc()
um5 <- glmer(not_enrolled_y1011 ~ appru_y09 +
               (1 | la),
             data = model_data_special,
             family = binomial)

gc()
um6 <- glmer(not_enrolled_y1011 ~ idaci_quintiles +
               (1 | la),
             data = model_data_special,
             family = binomial)

gc()
um7 <- glmer(not_enrolled_y1011 ~ fsmeligible +
               (1 | la),
             data = model_data_special,
             family = binomial)

gc()
um8 <- glmer(not_enrolled_y1011 ~ per_ag_sw +
                (1 | la),
              data = model_data_special,
              family = binomial)

gc()
um9 <- glmer(not_enrolled_y1011 ~ per_sw_turnover +
                (1 | la),
              data = model_data_special,
              family = binomial)


gc()
um10 <- glmer(not_enrolled_y1011 ~ I(la_n / 10000) +
                (1 | la),
              data = model_data_special,
              family = binomial,
              control = glmerControl(optimizer = "Nelder_Mead",
                                     optCtrl = list(maxfun = 1e6)))

gc()
um11 <- glmer(not_enrolled_y1011 ~ I(la_fsm_prop * 10) +
                (1 | la),
              data = model_data_special,
              family = binomial)

gc()
um12 <- glmer(not_enrolled_y1011 ~ I(open_cin_prop * 100) +
                (1 | la),
              data = model_data_special,
              family = binomial)

gc()
um13 <- glmer(not_enrolled_y1011 ~ I(new_cin_prop * 100) +
                (1 | la),
              data = model_data_special,
              family = binomial)

gc()
um14 <- glmer(not_enrolled_y1011 ~ I(sw_s251 / 100) +
                (1 | la),
              data = model_data_special,
              family = binomial)

gc()
um15 <- glmer(not_enrolled_y1011 ~ I(la_special_schools_prop * 100) +
                (1 | la),
              data = model_data_special,
              family = binomial)

gc()
um16 <- glmer(not_enrolled_y1011 ~ idaci_fsm +
                (1 | la),
              data = model_data_special,
              family = binomial,
              control = glmerControl(optimizer = "Nelder_Mead",
                                     optCtrl = list(maxfun = 1e6)))

gc()
models_offrolling_univar_glm_s <- list()
models_offrolling_univar_glm_s[[1]] <- um1
models_offrolling_univar_glm_s[[2]] <- um2
models_offrolling_univar_glm_s[[3]] <- um3
models_offrolling_univar_glm_s[[4]] <- um4
models_offrolling_univar_glm_s[[5]] <- um5
models_offrolling_univar_glm_s[[6]] <- um6
models_offrolling_univar_glm_s[[7]] <- um7
models_offrolling_univar_glm_s[[8]] <- um8
models_offrolling_univar_glm_s[[9]] <- um9
models_offrolling_univar_glm_s[[10]] <- um10
models_offrolling_univar_glm_s[[11]] <- um11
models_offrolling_univar_glm_s[[12]] <- um12
models_offrolling_univar_glm_s[[13]] <- um13
models_offrolling_univar_glm_s[[14]] <- um14
models_offrolling_univar_glm_s[[15]] <- um15
models_offrolling_univar_glm_s[[16]] <- um16

save(models_offrolling_univar_glm_s,
     file = "PROCESSED DATA/MODELS/MODELS_OFFROLLING_UNIVAR_GLM_SPECIAL.RDA")
rm(um2, um3, um4, um5, um6, um7, um8, um9, um10, um11, um12, um13, um14, um15, um16)

summary(models_offrolling_univar_glm_s[[1]])
summary(models_offrolling_univar_glm_s[[2]])
summary(models_offrolling_univar_glm_s[[3]])
summary(models_offrolling_univar_glm_s[[4]])
summary(models_offrolling_univar_glm_s[[5]])
summary(models_offrolling_univar_glm_s[[6]])
summary(models_offrolling_univar_glm_s[[7]])
summary(models_offrolling_univar_glm_s[[8]])
summary(models_offrolling_univar_glm_s[[9]])
summary(models_offrolling_univar_glm_s[[10]])
summary(models_offrolling_univar_glm_s[[11]])
summary(models_offrolling_univar_glm_s[[12]])
summary(models_offrolling_univar_glm_s[[13]])
summary(models_offrolling_univar_glm_s[[14]])
summary(models_offrolling_univar_glm_s[[15]])
summary(models_offrolling_univar_glm_s[[16]])

# ** main models ----------------------------------------------------------

gc()
hm1 <- glmer(not_enrolled_y1011 ~ 1 +
               (1 | la),
             data = model_data_special,
             family = binomial)

gc()
hm3 <- glmer(not_enrolled_y1011 ~ exposure_highest_4_grp_y49 +
               female_clean + eth_major + language_clean + fsmeligible + appru_y09 +
               (1 | la),
             data = model_data_special,
             family = binomial)

gc()
hm3a <- glmer(not_enrolled_y1011 ~ exposure_highest_4_grp_y49 +
                female_clean + eth_major + language_clean + fsmeligible + appru_y09 +
                (1 | la),
              data = model_data_special,
              family = binomial,
              control = glmerControl(check.conv.grad = .makeCC("warning", tol = 2e-3, relTol = NULL)))

gc()
hm4 <- glmer(not_enrolled_y1011 ~ exposure_highest_4_grp_y49 +
               female_clean + eth_major + language_clean + fsmeligible + appru_y09 +
               per_sw_turnover + I(new_cin_prop * 100) +
                (1 | la),
              data = model_data_special,
              family = binomial,
              control = glmerControl(optimizer = "Nelder_Mead",
                                     optCtrl = list(maxfun = 2e6))) # FTC 0.385

gc()
hm4a <- glmer(not_enrolled_y1011 ~ exposure_highest_4_grp_y49 +
               female_clean + eth_major + language_clean + fsmeligible + appru_y09 +
               per_sw_turnover + I(new_cin_prop * 100) +
               (1 | la),
             data = model_data_special,
             family = binomial,
             control = glmerControl(optimizer = "bobyqa",
                                    #check.conv.grad = .makeCC("warning", tol = 2e-3, relTol = NULL),
                                    optCtrl = list(maxfun = 2e6)))


gc()
hm4_posthoc <- glmer(not_enrolled_y1011_nogcses ~ exposure_highest_4_grp_y49 +
                       I(per_ag_sw / 10) + I(per_sw_turnover / 10) + I(sw_s251 / 100) + I(la_n / 10000) + I(la_fsm_prop * 10) +
                       I(new_cin_prop * 100) + I(la_special_schools_prop * 100) +
                       (1 | la),
                     data = model_data_special,
                     family = binomial,
                     control = glmerControl(optimizer = "bobyqa")) # def 0.0122964 / bobyqa 

models_offrolling_glm_s <- list()
models_offrolling_glm_s[[1]] <- hm1
models_offrolling_glm_s[[2]] <- um1
models_offrolling_glm_s[[3]] <- hm3a
models_offrolling_glm_s[[4]] <- hm4a
models_offrolling_glm_s[[5]] <- hm4_posthoc
save(models_offrolling_glm_s, file = "PROCESSED DATA/MODELS/MODELS_OFFROLLING_GLM_SPECIAL.RDA")
rm(hm1, um1, hm3, hm3a, hm4, hm4a, hm4_posthoc)

summary(models_offrolling_glm_s[[1]])
summary(models_offrolling_glm_s[[2]])
summary(models_offrolling_glm_s[[3]])
summary(models_offrolling_glm_s[[4]])
summary(models_offrolling_glm_s[[5]])

lrtest(models_offrolling_glm_s[[2]], models_offrolling_glm_s[[1]])
lrtest(models_offrolling_glm_s[[3]], models_offrolling_glm_s[[2]])
lrtest(models_offrolling_glm_s[[4]], models_offrolling_glm_s[[3]])
lrtest(models_offrolling_glm_s[[5]], models_offrolling_glm_s[[2]])

# ** random slopes --------------------------------------------------------

model_data_special[, any_csc := exposure_highest_4_grp_y49 != "None"]
table(model_data_special$any_csc)
prop.table(table(model_data_special$any_csc))
table(model_data_special$any_csc,
      model_data_special$exposure_highest_4_grp_y49,
      useNA = "always")

gc()
hm_posthoc_random_slopes_1 <- glmer(not_enrolled_y1011_nogcses ~ any_csc + 
                                      (1 | la),
                                    data = model_data_special,
                                    family = binomial)

gc()
hm_posthoc_random_slopes_2 <- glmer(not_enrolled_y1011_nogcses ~ any_csc + 
                                      (any_csc | la),
                                    data = model_data_special,
                                    family = binomial)

rm(hm_posthoc_random_slopes_1, hm_posthoc_random_slopes_2)

# ** LA models ------------------------------------------------------------

# gc()
# hm4a <- glmer(not_enrolled_y1011 ~ per_ag_sw + per_sw_turnover + I(sw_s251 / 100) +
#                      I(la_n / 10000) + I(la_fsm_prop * 100) + I(new_cin_prop * 100) +
#                      (1 | la),
#                    data = model_data_special,
#                    family = binomial,
#               control = glmerControl(optimizer = "Nelder_Mead",
#                                      optCtrl = list(maxfun = 1e6))) # ftc 0.006
# # FTC. All were insig except turnover. Already estimated that model.
# 
# rm(hm4a)
# 
# lrtest(models_offrolling_univar_glm_s[[9]], models_offrolling_glm_s[[1]])

# ** cross-level interactions ---------------------------------------------

x1a <- glmer(not_enrolled_y1011 ~ exposure_highest_4_grp_y49 + I(per_ag_sw / 10) +
               (1 | la),
             data = model_data_special,
             family = binomial)
x1b <- glmer(not_enrolled_y1011 ~ exposure_highest_4_grp_y49 * I(per_ag_sw / 10) +
               (1 | la),
             data = model_data_special,
             family = binomial)

x2a <- glmer(not_enrolled_y1011 ~ exposure_highest_4_grp_y49 + I(per_sw_turnover / 10) +
               (1 | la),
             data = model_data_special,
             family = binomial)
x2b <- glmer(not_enrolled_y1011 ~ exposure_highest_4_grp_y49 * I(per_sw_turnover / 10) +
               (1 | la),
             data = model_data_special,
             family = binomial,
             control = glmerControl(optimizer = "bobyqa",
                                    optCtrl = list(maxfun = 1e6))) # default ftc 0.0044 / NM 1e6 ftc 0.0097 / 1e6 0.004 / bobyqa 1e6 CONVERGED

x3a <- glmer(not_enrolled_y1011 ~ exposure_highest_4_grp_y49 + I(sw_s251 / 100) +
               (1 | la),
             data = model_data_special,
             family = binomial)
x3b <- glmer(not_enrolled_y1011 ~ exposure_highest_4_grp_y49 * I(sw_s251 / 100) +
               (1 | la),
             data = model_data_special,
             family = binomial,
             control = glmerControl(optimizer = "bobyqa",
                                    optCtrl = list(maxfun = 1e6))) # default ftc 0.02 / NM 1e6 ftc 0.06 / 1e6 ftc 0.02 / bobyqa 1e6 CONVERGED

x4a <- glmer(not_enrolled_y1011 ~ exposure_highest_4_grp_y49 + I(la_fsm_prop * 10) +
               (1 | la),
             data = model_data_special,
             family = binomial)
x4b <- glmer(not_enrolled_y1011 ~ exposure_highest_4_grp_y49 * I(la_fsm_prop * 10) +
               (1 | la),
             data = model_data_special,
             family = binomial)

x5a <- glmer(not_enrolled_y1011 ~ exposure_highest_4_grp_y49 + I(new_cin_prop * 100) +
               (1 | la),
             data = model_data_special,
             family = binomial)
x5b <- glmer(not_enrolled_y1011 ~ exposure_highest_4_grp_y49 * I(new_cin_prop * 100) +
               (1 | la),
             data = model_data_special,
             family = binomial)

x6a <- glmer(not_enrolled_y1011 ~ exposure_highest_4_grp_y49 + I(la_special_schools_prop * 10) +
               (1 | la),
             data = model_data_special,
             family = binomial)
x6b <- glmer(not_enrolled_y1011 ~ exposure_highest_4_grp_y49 * I(la_special_schools_prop * 10) +
               (1 | la),
             data = model_data_special,
             family = binomial)

models_offrolling_x_int_glm_s <- list()
models_offrolling_x_int_glm_s[[1]] <- x1a
models_offrolling_x_int_glm_s[[2]] <- x1b
models_offrolling_x_int_glm_s[[3]] <- x2a
models_offrolling_x_int_glm_s[[4]] <- x2b
models_offrolling_x_int_glm_s[[5]] <- x3a
models_offrolling_x_int_glm_s[[6]] <- x3b
models_offrolling_x_int_glm_s[[7]] <- x4a
models_offrolling_x_int_glm_s[[8]] <- x4b
models_offrolling_x_int_glm_s[[9]] <- x5a
models_offrolling_x_int_glm_s[[10]] <- x5b
models_offrolling_x_int_glm_s[[11]] <- x6a
models_offrolling_x_int_glm_s[[12]] <- x6b
save(models_offrolling_x_int_glm_s, file = "PROCESSED DATA/MODELS/MODELS_OFFROLLING_GLM_SPECIAL_XINT.RDA")
rm(x1a, x1b, x2a, x2b, x3a, x3b, x4a, x4b, x5a, x5b, x6a, x6b)

summary(models_offrolling_x_int_glm_s[[1]])
summary(models_offrolling_x_int_glm_s[[2]])
lrtest(models_offrolling_x_int_glm_s[[2]], models_offrolling_x_int_glm_s[[1]])

summary(models_offrolling_x_int_glm_s[[3]])
summary(models_offrolling_x_int_glm_s[[4]])
lrtest(models_offrolling_x_int_glm_s[[4]], models_offrolling_x_int_glm_s[[3]])

summary(models_offrolling_x_int_glm_s[[5]])
summary(models_offrolling_x_int_glm_s[[6]])
lrtest(models_offrolling_x_int_glm_s[[6]], models_offrolling_x_int_glm_s[[5]])

summary(models_offrolling_x_int_glm_s[[7]])
summary(models_offrolling_x_int_glm_s[[8]])
lrtest(models_offrolling_x_int_glm_s[[8]], models_offrolling_x_int_glm_s[[7]])

summary(models_offrolling_x_int_glm_s[[9]])
summary(models_offrolling_x_int_glm_s[[10]])
lrtest(models_offrolling_x_int_glm_s[[10]], models_offrolling_x_int_glm_s[[9]])

summary(models_offrolling_x_int_glm_s[[11]])
summary(models_offrolling_x_int_glm_s[[12]])
lrtest(models_offrolling_x_int_glm_s[[12]], models_offrolling_x_int_glm_s[[11]])

# ** cross-level with adjustments -----------------------------------------

gc()
x1b_adj <- glmer(not_enrolled_y1011 ~ exposure_highest_4_grp_y49 * I(per_ag_sw / 10) +
                   female_clean + eth_major + language_clean + fsmeligible + appru_y09 +
                   (1 | la),
                 data = model_data_special,
                 family = binomial,
                 control = glmerControl(optimizer = "bobyqa",
                                        optCtrl = list(maxfun = 1e6)))


gc()
x2b_adj <- glmer(not_enrolled_y1011 ~ exposure_highest_4_grp_y49 * I(per_sw_turnover / 10) +
                   female_clean + eth_major + language_clean + fsmeligible + appru_y09 +
                   (1 | la),
                 data = model_data_special,
                 family = binomial,
                 control = glmerControl(optimizer = "bobyqa",
                                        optCtrl = list(maxfun = 1e6)))


gc()
x3b_adj <- glmer(not_enrolled_y1011 ~ exposure_highest_4_grp_y49 * I(sw_s251 / 100) +
                   female_clean + eth_major + language_clean + fsmeligible + appru_y09 +
                   (1 | la),
                 data = model_data_special,
                 family = binomial,
                 control = glmerControl(optimizer = "bobyqa",
                                        optCtrl = list(maxfun = 1e6)))

gc()
x4b_adj <- glmer(not_enrolled_y1011 ~ exposure_highest_4_grp_y49 * I(la_fsm_prop * 10) +
                   female_clean + eth_major + language_clean + fsmeligible + appru_y09 +
                   (1 | la),
                 data = model_data_special,
                 family = binomial,
                 control = glmerControl(optimizer = "bobyqa",
                                        optCtrl = list(maxfun = 1e6)))

gc()
x5b_adj <- glmer(not_enrolled_y1011 ~ exposure_highest_4_grp_y49 * I(new_cin_prop * 100) +
                   female_clean + eth_major + language_clean + fsmeligible + appru_y09 +
                   (1 | la),
                 data = model_data_special,
                 family = binomial,
                 control = glmerControl(optimizer = "bobyqa",
                                        optCtrl = list(maxfun = 1e6)))

gc()
x6b_adj <- glmer(not_enrolled_y1011 ~ exposure_highest_4_grp_y49 * I(la_special_schools_prop * 10) +
                   female_clean + eth_major + language_clean + fsmeligible + appru_y09 +
                   (1 | la),
                 data = model_data_special,
                 family = binomial,
                 control = glmerControl(optimizer = "bobyqa",
                                        optCtrl = list(maxfun = 1e6)))

models_offrolling_x_int_adj_glm_s <- list()
models_offrolling_x_int_adj_glm_s[[1]] <- x1b_adj
models_offrolling_x_int_adj_glm_s[[2]] <- x2b_adj
models_offrolling_x_int_adj_glm_s[[3]] <- x3b_adj
models_offrolling_x_int_adj_glm_s[[4]] <- x4b_adj
models_offrolling_x_int_adj_glm_s[[5]] <- x5b_adj
models_offrolling_x_int_adj_glm_s[[6]] <- x6b_adj
save(models_offrolling_x_int_adj_glm_s, file = "PROCESSED DATA/MODELS/MODELS_OFFROLLING_GLM_SPECIAL_XINT_ADJ.RDA")
rm(x1b_adj, x2b_adj, x3b_adj, x4b_adj, x5b_adj, x6b_adj)

summary(models_offrolling_x_int_adj_glm_s[[1]])
lrtest(models_offrolling_x_int_adj_glm_s[[1]], models_offrolling_x_int_glm_s[[2]])

summary(models_offrolling_x_int_adj_glm_s[[2]])
lrtest(models_offrolling_x_int_adj_glm_s[[2]], models_offrolling_x_int_glm_s[[4]])

summary(models_offrolling_x_int_adj_glm_s[[3]])
lrtest(models_offrolling_x_int_adj_glm_s[[3]], models_offrolling_x_int_glm_s[[6]])

summary(models_offrolling_x_int_adj_glm_s[[4]])
lrtest(models_offrolling_x_int_adj_glm_s[[4]], models_offrolling_x_int_glm_s[[8]])

summary(models_offrolling_x_int_adj_glm_s[[5]])
lrtest(models_offrolling_x_int_adj_glm_s[[5]], models_offrolling_x_int_glm_s[[10]])

summary(models_offrolling_x_int_adj_glm_s[[6]])
lrtest(models_offrolling_x_int_adj_glm_s[[6]], models_offrolling_x_int_glm_s[[12]])

rm(models_offrolling_x_int_adj_glm_s)

# la adjustment
gc()
x1b_adj_la <- glmer(not_enrolled_y1011 ~ exposure_highest_4_grp_y49 * I(per_ag_sw / 10) +
                      female_clean + eth_major + language_clean + fsmeligible + appru_y09 +
                      per_sw_turnover + I(new_cin_prop * 100) +
                      (1 | la),
                    data = model_data_special,
                    family = binomial,
                    control = glmerControl(optimizer = "bobyqa",
                                           optCtrl = list(maxfun = 1e6)))

gc()
x2b_adj_la <- glmer(not_enrolled_y1011_nogcses ~ exposure_highest_4_grp_y49 * I(per_sw_turnover / 10) +
                      female_clean + eth_major + language_clean + fsmeligible + appru_y09 +
                      I(new_cin_prop * 100) +
                      (1 | la),
                    data = model_data_special,
                    family = binomial,
                    control = glmerControl(optimizer = "bobyqa",
                                           optCtrl = list(maxfun = 1e6)))

gc()
x3b_adj_la <- glmer(not_enrolled_y1011_nogcses ~ exposure_highest_4_grp_y49 * I(sw_s251 / 100) +
                      female_clean + eth_major + language_clean + fsmeligible + appru_y09 +
                      I(per_sw_turnover / 100) + I(new_cin_prop * 100) +
                      (1 | la),
                    data = model_data_special,
                    family = binomial,
                    control = glmerControl(optimizer = "bobyqa",
                                           optCtrl = list(maxfun = 1e6)))

gc()
x4b_adj_la <- glmer(not_enrolled_y1011_nogcses ~ exposure_highest_4_grp_y49 * I(la_fsm_prop * 10) +
                      female_clean + eth_major + language_clean + fsmeligible + appru_y09 +
                      I(per_sw_turnover / 100) + I(new_cin_prop * 100) + 
                      (1 | la),
                    data = model_data_special,
                    family = binomial,
                    control = glmerControl(optimizer = "bobyqa",
                                           optCtrl = list(maxfun = 1e6)))

gc()
x5b_adj_la <- glmer(not_enrolled_y1011_nogcses ~ exposure_highest_4_grp_y49 * I(new_cin_prop * 100) +
                      female_clean + eth_major + language_clean + fsmeligible + appru_y09 +
                      I(per_sw_turnover / 100) +
                      (1 | la),
                    data = model_data_special,
                    family = binomial,
                    control = glmerControl(optimizer = "bobyqa",
                                           optCtrl = list(maxfun = 1e6)))

gc()
x6b_adj_la <- glmer(not_enrolled_y1011_nogcses ~ exposure_highest_4_grp_y49 * I(la_special_schools_prop * 10) +
                      female_clean + eth_major + language_clean + fsmeligible + appru_y09 +
                      I(per_sw_turnover / 100) + I(new_cin_prop * 10) +
                      (1 | la),
                    data = model_data_special,
                    family = binomial,
                    control = glmerControl(optimizer = "bobyqa",
                                           optCtrl = list(maxfun = 1e6)))

models_offrolling_x_int_adj_la_glm_s <- list()
models_offrolling_x_int_adj_la_glm_s[[1]] <- x1b_adj_la
models_offrolling_x_int_adj_la_glm_s[[2]] <- x2b_adj_la
models_offrolling_x_int_adj_la_glm_s[[3]] <- x3b_adj_la
models_offrolling_x_int_adj_la_glm_s[[4]] <- x4b_adj_la
models_offrolling_x_int_adj_la_glm_s[[5]] <- x5b_adj_la
models_offrolling_x_int_adj_la_glm_s[[6]] <- x6b_adj_la
save(models_offrolling_x_int_adj_la_glm_s, file = "PROCESSED DATA/MODELS/MODELS_OFFROLLING_GLM_SPECIAL_XINT_ADJ_LA.RDA")
rm(x1b_adj_la, x2b_adj_la, x3b_adj_la, x4b_adj_la, x5b_adj_la, x6b_adj_la)

summary(models_offrolling_x_int_adj_la_glm_s[[1]])
lrtest(models_offrolling_x_int_adj_la_glm_s[[1]], models_offrolling_x_int_adj_glm_s[[1]])

summary(models_offrolling_x_int_adj_la_glm_s[[2]])
lrtest(models_offrolling_x_int_adj_la_glm_s[[2]], models_offrolling_x_int_adj_glm_s[[2]])

summary(models_offrolling_x_int_adj_la_glm_s[[3]])
lrtest(models_offrolling_x_int_adj_la_glm_s[[3]], models_offrolling_x_int_adj_glm_s[[3]])

summary(models_offrolling_x_int_adj_la_glm_s[[4]])
lrtest(models_offrolling_x_int_adj_la_glm_s[[4]], models_offrolling_x_int_adj_glm_s[[4]])

summary(models_offrolling_x_int_adj_la_glm_s[[5]])
lrtest(models_offrolling_x_int_adj_la_glm_s[[5]], models_offrolling_x_int_adj_glm_s[[5]])

summary(models_offrolling_x_int_adj_la_glm_s[[6]])
lrtest(models_offrolling_x_int_adj_la_glm_s[[6]], models_offrolling_x_int_adj_glm_s[[6]])
