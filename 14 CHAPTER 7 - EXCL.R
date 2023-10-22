# LOAD --------------------------------------------------------------------

setwd("P:/Working/Matt")

lib_base = "P:/Working/Matt/r-library/"
assign(".lib.loc", c(.libPaths(), lib_base), envir = environment(.libPaths))
rm(lib_base)

# data management
library(data.table)
library(tableone)

# graphics
library(ggplot2)
library(gridExtra)

# regrssion modelling
library(lme4)
library(lmtest)

# load("PROCESSED DATA/cohort1_npd_clean_sch_ad_qual_cscobs_outcomes_misc.rda")
# load("PROCESSED DATA/cohort2_npd_clean_sch_ad_qual_cscobs_outcomes_misc.rda")

load("PROCESSED DATA/cohort1_npd_clean_sch_ad_qual_cscobs_outcomes_misc_csclca.rda")
load("PROCESSED DATA/cohort2_npd_clean_sch_ad_qual_cscobs_outcomes_misc_csclca.rda")

cohort1$cohort <- 1
cohort2$cohort <- 2
table(names(cohort1) == names(cohort2))
cohort_all <- rbind(cohort1, cohort2)
rm(cohort1, cohort2)

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

# table(cohort_all[year == 7 & sch_n_in_year == 1]$ever_not_enrolled)
# summary(cohort_all[year == 7 & sch_n_in_year == 1, c("pupilmatchingrefanonymous", "cohort",
#                                                      "female_clean", "eth_major", "language_clean", "highest_ever_sen_primary",
#                                                      "ever_sen_primary", "ever_sen_y09", "highest_ever_sen_y09",
#                                                      "idaci_quintiles", "fsmeligible", "idaci_fsm", "region_major", "la",
#                                                      "exposure_highest_4_grp_y46", "exposure_highest_4_grp_y49",
#                                                      "school_type_with_secondary_5_groups", "appru", "appru_y06", "appru_y09",
#                                                      #"persistent_absence_auth_y7", "persistent_absence_auth_y8", "persistent_absence_auth_y9",
#                                                      #"persistent_absence_auth_y10", "persistent_absence_auth_y11",
#                                                      #"persistent_absence_unauth_y7", "persistent_absence_unauth_y8", "persistent_absence_unauth_y9",
#                                                      #"persistent_absence_unauth_y10", "persistent_absence_unauth_y11",
#                                                      #"persistent_absence_overall_y7", "persistent_absence_overall_y8", "persistent_absence_overall_y9",
#                                                      #"persistent_absence_overall_y10", "persistent_absence_overall_y11",
#                                                      "ft_excl_y7", "ft_excl_y8", "ft_excl_y9",
#                                                      "ft_excl_y10", "ft_excl_y11",
#                                                      "perm_excl_y7", "perm_excl_y8", "perm_excl_y9",
#                                                      "perm_excl_y10", "perm_excl_y11")])

cohort_all$any_nas <- apply(cohort_all[, c("female_clean", "eth_major", "language_clean", "idaci_quintiles")], 1, anyNA)
cohort_all[, drop := year == 7 & any_nas & sch_n_in_year == 1]
cohort_all[, drop := max(drop), by = .(pupilmatchingrefanonymous)]
table(cohort_all[year == 7 & sch_n_in_year == 1]$drop)
cohort_all <- cohort_all[!cohort_all$drop]
cohort_all[, any_nas := NULL]
cohort_all[, drop := NULL]

# COMPLETE DATA ONLY ------------------------------------------------------

cohort_all$complete_record_secondary <-
  cohort_all$pupilmatchingrefanonymous %in% cohort_all[year == 7]$pupilmatchingrefanonymous &
  cohort_all$pupilmatchingrefanonymous %in% cohort_all[year == 8]$pupilmatchingrefanonymous &
  cohort_all$pupilmatchingrefanonymous %in% cohort_all[year == 9]$pupilmatchingrefanonymous &
  cohort_all$pupilmatchingrefanonymous %in% cohort_all[year == 10]$pupilmatchingrefanonymous &
  cohort_all$pupilmatchingrefanonymous %in% cohort_all[year == 11]$pupilmatchingrefanonymous

table(cohort_all[year == 7 & sch_n_in_year == 1]$cohort)

nrow(cohort_all[year == 7 & sch_n_in_year == 1])

table(cohort_all[year == 7 & sch_n_in_year == 1]$complete_record_secondary)

exposures <- c("exposure_highest_4_grp_y46", "exposure_highest_4_grp_y49",
               "female_clean", "eth_major", "language_clean", "idaci_quintiles", "fsmeligible", "idaci_fsm", "region_major",
               "school_type_with_secondary_5_groups", "appru_y06", "appru_y09", "special_school",
               "ever_sen_primary", "highest_ever_sen_primary", "ever_sen_y09", "highest_ever_sen_y09")

final_output <- cbind(describe_outcome(exposure = exposures, outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & complete_record_secondary == T]),
                      describe_outcome(exposure = exposures, outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & complete_record_secondary == F]),
                      describe_outcome(exposure = exposures, outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1]))
colnames(final_output) <- c("Complete secondary school records", "Incomplete records", "Combined")
write.csv(final_output, file = "OUTPUTS/CHAPTER 7/chars-complete-records.csv")
rm(exposures, final_output)

cohort_all <- cohort_all[complete_record_secondary == T]

# COMBINE EXCLUSIONS ------------------------------------------------------

cohort_all[, any_excl_y7 := perm_excl_y7 | ft_excl_y7]
cohort_all[, any_excl_y8 := perm_excl_y8 | ft_excl_y8]
cohort_all[, any_excl_y9 := perm_excl_y9 | ft_excl_y9]
cohort_all[, any_excl_y10 := perm_excl_y10 | ft_excl_y10]
cohort_all[, any_excl_y11 := perm_excl_y11 | ft_excl_y11]

cohort_all[, any_excl_y79 := perm_excl_y79 | ft_excl_y79]
cohort_all[, any_excl_y1011 := perm_excl_y1011 | ft_excl_y1011]
cohort_all[, any_excl_y711 := perm_excl_y711 | ft_excl_y711]

cohort_all[, ft_excluded_year := F]
cohort_all[year == 7, ft_excluded_year := ft_excl_y7]
cohort_all[year == 8, ft_excluded_year := ft_excl_y8]
cohort_all[year == 9, ft_excluded_year := ft_excl_y9]
cohort_all[year == 10, ft_excluded_year := ft_excl_y10]
cohort_all[year == 11, ft_excluded_year := ft_excl_y11]

cohort_all[, perm_excluded_year := F]
cohort_all[year == 7, perm_excluded_year := perm_excl_y7]
cohort_all[year == 8, perm_excluded_year := perm_excl_y8]
cohort_all[year == 9, perm_excluded_year := perm_excl_y9]
cohort_all[year == 10, perm_excluded_year := perm_excl_y10]
cohort_all[year == 11, perm_excluded_year := perm_excl_y11]

cohort_all[, any_excluded_year := F]
cohort_all[year == 7, any_excluded_year := any_excl_y7]
cohort_all[year == 8, any_excluded_year := any_excl_y8]
cohort_all[year == 9, any_excluded_year := any_excl_y9]
cohort_all[year == 10, any_excluded_year := any_excl_y10]
cohort_all[year == 11, any_excluded_year := any_excl_y11]

# DEMOGRAPHICS ------------------------------------------------------------

# by special school or not
table(cohort_all[year == 7 & sch_n_in_year == 1]$special_school)

# create interaction var
# primary
table(cohort_all[year == 7 & sch_n_in_year == 1]$exposure_highest_4_grp_y46,
      cohort_all[year == 7 & sch_n_in_year == 1]$highest_ever_sen_primary)

cohort_all[, exposure_sen_primary := factor("None-None", levels = c("None-None",
                                                                    "CiN-None",
                                                                    "CPP-None",
                                                                    "CLA-None",
                                                                    "None-Any",
                                                                    "CiN-Any",
                                                                    "CPP-Any",
                                                                    "CLA-Any"))]

cohort_all[exposure_highest_4_grp_y46 == "CiN" & highest_ever_sen_primary == "None", exposure_sen_primary := "CiN-None"]
cohort_all[exposure_highest_4_grp_y46 == "CPP" & highest_ever_sen_primary == "None", exposure_sen_primary := "CPP-None"]
cohort_all[exposure_highest_4_grp_y46 == "CLA" & highest_ever_sen_primary == "None", exposure_sen_primary := "CLA-None"]

cohort_all[exposure_highest_4_grp_y46 == "None" & highest_ever_sen_primary != "None", exposure_sen_primary := "None-Any"]
cohort_all[exposure_highest_4_grp_y46 == "CiN" & highest_ever_sen_primary != "None", exposure_sen_primary := "CiN-Any"]
cohort_all[exposure_highest_4_grp_y46 == "CPP" & highest_ever_sen_primary != "None", exposure_sen_primary := "CPP-Any"]
cohort_all[exposure_highest_4_grp_y46 == "CLA" & highest_ever_sen_primary != "None", exposure_sen_primary := "CLA-Any"]

table(cohort_all[year == 7 & sch_n_in_year == 1]$exposure_sen_primary)

# y 49
table(cohort_all[year == 7 & sch_n_in_year == 1]$exposure_highest_4_grp_y49,
      cohort_all[year == 7 & sch_n_in_year == 1]$highest_ever_sen_y09)

cohort_all[, exposure_sen_y09 := factor("None-None", levels = c("None-None",
                                                                "CiN-None",
                                                                "CPP-None",
                                                                "CLA-None",
                                                                "None-Any",
                                                                "CiN-Any",
                                                                "CPP-Any",
                                                                "CLA-Any"))]

cohort_all[exposure_highest_4_grp_y49 == "CiN" & highest_ever_sen_y09 == "None", exposure_sen_y09 := "CiN-None"]
cohort_all[exposure_highest_4_grp_y49 == "CPP" & highest_ever_sen_y09 == "None", exposure_sen_y09 := "CPP-None"]
cohort_all[exposure_highest_4_grp_y49 == "CLA" & highest_ever_sen_y09 == "None", exposure_sen_y09 := "CLA-None"]

cohort_all[exposure_highest_4_grp_y49 == "None" & highest_ever_sen_y09 != "None", exposure_sen_y09 := "None-Any"]
cohort_all[exposure_highest_4_grp_y49 == "CiN" & highest_ever_sen_y09 != "None", exposure_sen_y09 := "CiN-Any"]
cohort_all[exposure_highest_4_grp_y49 == "CPP" & highest_ever_sen_y09 != "None", exposure_sen_y09 := "CPP-Any"]
cohort_all[exposure_highest_4_grp_y49 == "CLA" & highest_ever_sen_y09 != "None", exposure_sen_y09 := "CLA-Any"]

table(cohort_all[year == 7 & sch_n_in_year == 1]$exposure_sen_y09)

# output
exposures <- c("exposure_highest_4_grp_y46", "exposure_highest_4_grp_y49",
               "female_clean", "eth_major", "language_clean", "idaci_quintiles", "fsmeligible", "idaci_fsm", "region_major",
               "school_type_with_secondary_5_groups", "appru_y06", "appru_y09",
               "ever_sen_primary", "highest_ever_sen_primary", "ever_sen_y09", "highest_ever_sen_y09",
               "exposure_sen_primary", "exposure_sen_y09")

final_output <- cbind(describe_outcome(exposure = exposures, outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F]),
                      describe_outcome(exposure = exposures, outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T]),
                      describe_outcome(exposure = exposures, outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1]))
colnames(final_output) <- c("Not special school yr 7", "Special school yr 7", "Combined")
write.csv(final_output, file = "OUTPUTS/CHAPTER 7/chars-by-special-school.csv")

rm(exposures, final_output)

# CROSS-SECTIONAL ---------------------------------------------------------

# in toto
outcomes <- c("ft_excl_y7", "ft_excl_y8", "ft_excl_y9", "ft_excl_y10", "ft_excl_y11",
              "perm_excl_y7", "perm_excl_y8", "perm_excl_y9", "perm_excl_y10", "perm_excl_y11",
              "any_excl_y7", "any_excl_y8", "any_excl_y9", "any_excl_y10", "any_excl_y11")

final_output <- cbind(rbind(describe_outcome(exposure = outcomes[1], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F]),
                            describe_outcome(exposure = outcomes[2], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F]),
                            describe_outcome(exposure = outcomes[3], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F]),
                            describe_outcome(exposure = outcomes[4], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F]),
                            describe_outcome(exposure = outcomes[5], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F]),
                            describe_outcome(exposure = outcomes[6], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F]),
                            describe_outcome(exposure = outcomes[7], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F]),
                            describe_outcome(exposure = outcomes[8], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F]),
                            describe_outcome(exposure = outcomes[9], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F]),
                            describe_outcome(exposure = outcomes[10], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F]),
                            describe_outcome(exposure = outcomes[11], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F]),
                            describe_outcome(exposure = outcomes[12], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F]),
                            describe_outcome(exposure = outcomes[13], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F]),
                            describe_outcome(exposure = outcomes[14], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F]),
                            describe_outcome(exposure = outcomes[15], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F])),
                      rbind(describe_outcome(exposure = outcomes[1], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T]),
                            describe_outcome(exposure = outcomes[2], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T]),
                            describe_outcome(exposure = outcomes[3], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T]),
                            describe_outcome(exposure = outcomes[4], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T]),
                            describe_outcome(exposure = outcomes[5], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T]),
                            describe_outcome(exposure = outcomes[6], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T]),
                            describe_outcome(exposure = outcomes[7], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T]),
                            describe_outcome(exposure = outcomes[8], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T]),
                            describe_outcome(exposure = outcomes[9], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T]),
                            describe_outcome(exposure = outcomes[10], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T]),
                            describe_outcome(exposure = outcomes[11], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T]),
                            describe_outcome(exposure = outcomes[12], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T]),
                            describe_outcome(exposure = outcomes[13], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T]),
                            describe_outcome(exposure = outcomes[14], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T]),
                            describe_outcome(exposure = outcomes[15], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T])))
colnames(final_output) <- c("Not special school yr 7", "Special school yr 7")
write.csv(final_output, file = "OUTPUTS/CHAPTER 7/excl-cross-sectional-total-yearly.csv")
                       
# grouped years
outcomes <- c("ft_excl_y79", "ft_excl_y1011", "ft_excl_y711",
              "perm_excl_y79", "perm_excl_y1011", "perm_excl_y711",
              "any_excl_y79", "any_excl_y1011", "any_excl_y711")

final_output <- cbind(rbind(describe_outcome(exposure = outcomes[1], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F]),
                            describe_outcome(exposure = outcomes[2], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F]),
                            describe_outcome(exposure = outcomes[3], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F]),
                            describe_outcome(exposure = outcomes[4], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F]),
                            describe_outcome(exposure = outcomes[5], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F]),
                            describe_outcome(exposure = outcomes[6], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F]),
                            describe_outcome(exposure = outcomes[7], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F]),
                            describe_outcome(exposure = outcomes[8], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F]),
                            describe_outcome(exposure = outcomes[9], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F])),
                      rbind(describe_outcome(exposure = outcomes[1], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T]),
                            describe_outcome(exposure = outcomes[2], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T]),
                            describe_outcome(exposure = outcomes[3], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T]),
                            describe_outcome(exposure = outcomes[4], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T]),
                            describe_outcome(exposure = outcomes[5], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T]),
                            describe_outcome(exposure = outcomes[6], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T]),
                            describe_outcome(exposure = outcomes[7], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T]),
                            describe_outcome(exposure = outcomes[8], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T]),
                            describe_outcome(exposure = outcomes[9], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T])))
colnames(final_output) <- c("Not special school yr 7", "Special school yr 7")
write.csv(final_output, file = "OUTPUTS/CHAPTER 7/excl-cross-sectional-grouped-years.csv")

# by RFs
exposures <- c("exposure_highest_4_grp_y46", "exposure_highest_4_grp_y49",
               "female_clean", "eth_major", "language_clean", "idaci_quintiles", "fsmeligible", "idaci_fsm", "region_major",
               "school_type_with_secondary_5_groups", "appru_y06", "appru_y09", "special_school",
               "ever_sen_primary", "highest_ever_sen_primary", "ever_sen_y09", "highest_ever_sen_y09",
               "exposure_sen_primary", "exposure_sen_y09")

# by RFs
final_output <- cbind(describe_outcome(exposure = exposures, outcome = outcomes[1], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F]),
                      describe_outcome(exposure = exposures, outcome = outcomes[2], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F]),
                      describe_outcome(exposure = exposures, outcome = outcomes[3], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F]),
                      describe_outcome(exposure = exposures, outcome = outcomes[4], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F]),
                      describe_outcome(exposure = exposures, outcome = outcomes[5], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F]),
                      describe_outcome(exposure = exposures, outcome = outcomes[6], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F]),
                      describe_outcome(exposure = exposures, outcome = outcomes[7], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F]),
                      describe_outcome(exposure = exposures, outcome = outcomes[8], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F]),
                      describe_outcome(exposure = exposures, outcome = outcomes[9], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F]))
write.csv(final_output, file = "OUTPUTS/CHAPTER 7/excl-cross-sectional-grouped-years-by-RFs-not-special.csv")

final_output <- cbind(describe_outcome(exposure = exposures, outcome = outcomes[1], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T]),
                      describe_outcome(exposure = exposures, outcome = outcomes[2], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T]),
                      describe_outcome(exposure = exposures, outcome = outcomes[3], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T]),
                      describe_outcome(exposure = exposures, outcome = outcomes[4], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T]),
                      describe_outcome(exposure = exposures, outcome = outcomes[5], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T]),
                      describe_outcome(exposure = exposures, outcome = outcomes[6], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T]),
                      describe_outcome(exposure = exposures, outcome = outcomes[7], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T]),
                      describe_outcome(exposure = exposures, outcome = outcomes[8], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T]),
                      describe_outcome(exposure = exposures, outcome = outcomes[9], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T]))
write.csv(final_output, file = "OUTPUTS/CHAPTER 7/excl-cross-sectional-grouped-years-by-RFs-special.csv")

rm(exposures, outcomes, final_output)

# CUMULATIVE --------------------------------------------------------------

# * not special ----------------------------------------------------------

# ** ft -------------------------------------------------------------------

ci_data <- create_ci_data(subgroups = F,
                          pmr_vector = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F]$pupilmatchingrefanonymous,
                          outcome_var = cohort_all$ft_excluded_year)

ci_output <- create_ci_output(subgroups = F, ci_data = ci_data)
ci_output

write.csv(ci_output, "OUTPUTS/CHAPTER 7/ci/ci-ft-excl-ns.csv")
rm(ci_data, ci_output)

ci_data <- create_ci_data(pmr_vector = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F]$pupilmatchingrefanonymous,
                          outcome_var = cohort_all$ft_excluded_year,
                          factor_var = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F]$exposure_highest_4_grp_y46)

ci_output <- create_ci_output(ci_data = ci_data, names_for_cols = levels(cohort_all$exposure_highest_4_grp_y46))
ci_output
write.csv(ci_output, "OUTPUTS/CHAPTER 7/ci/ci-ft-excl-by-exp-ns.csv")

ci_output_p <- data.table(ci_output[, 5:8])
ci_output_ll <- data.table(ci_output[, 9:12])
ci_output_ul <- data.table(ci_output[, 13:16])

ci_output_p$year <- 7:11
ci_output_p <- melt(ci_output_p,
                    variable.name = "Exposure",
                    measure.vars = levels(cohort_all$exposure_highest_4_grp_y46),
                    value.name = "Perc")

ci_output_ll$year <- 7:11
ci_output_ll <- melt(ci_output_ll,
                     variable.name = "Exposure",
                     measure.vars = levels(cohort_all$exposure_highest_4_grp_y46),
                     value.name = "Perc")

ci_output_ul$year <- 7:11
ci_output_ul <- melt(ci_output_ul,
                     variable.name = "Exposure",
                     measure.vars = levels(cohort_all$exposure_highest_4_grp_y46),
                     value.name = "Perc")

ci_output_p$ll <- ci_output_ll$Perc
ci_output_p$ul <- ci_output_ul$Perc
rm(ci_output_ll, ci_output_ul)

p1 <- ggplot(data = ci_output_p) +
  geom_line(aes(x = year, y = Perc, colour = Exposure), size = 1) +
  geom_point(aes(x = year, y = Perc, colour = Exposure), size = 2,
             position = position_dodge(0.05)) +
  geom_errorbar(aes(x = year, ymin = ll, ymax = ul, colour = Exposure),
                width = 0.3,
                position = position_dodge(0.05)) +
  xlab("Year") +
  ylab("Cumulative fixed-term exclusion (%)") +
  ggtitle("Not special schools") +
  scale_y_continuous(limits = c(0, 100),
                     expand = c(0, 0)) +
  scale_x_continuous(breaks = 7:11,
                     labels = 7:11,
                     expand = c(0, 0)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        legend.key = element_blank(),
        legend.position = "none")

rm(ci_data, ci_output, ci_output_p)

# ** perm -----------------------------------------------------------------

ci_data <- create_ci_data(subgroups = F,
                          pmr_vector = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F]$pupilmatchingrefanonymous,
                          outcome_var = cohort_all$perm_excluded_year)

ci_output <- create_ci_output(subgroups = F, ci_data = ci_data)
ci_output

write.csv(ci_output, "OUTPUTS/CHAPTER 7/ci/ci-perm-excl-ns.csv")
rm(ci_data, ci_output)

ci_data <- create_ci_data(pmr_vector = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F]$pupilmatchingrefanonymous,
                          outcome_var = cohort_all$perm_excluded_year,
                          factor_var = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F]$exposure_highest_4_grp_y46)

ci_output <- create_ci_output(ci_data = ci_data, names_for_cols = levels(cohort_all$exposure_highest_4_grp_y46))
ci_output
write.csv(ci_output, "OUTPUTS/CHAPTER 7/ci/ci-perm-excl-by-exp-ns.csv")

ci_output_p <- data.table(ci_output[, 5:8])
ci_output_ll <- data.table(ci_output[, 9:12])
ci_output_ul <- data.table(ci_output[, 13:16])

ci_output_p$year <- 7:11
ci_output_p <- melt(ci_output_p,
                    variable.name = "Exposure",
                    measure.vars = levels(cohort_all$exposure_highest_4_grp_y46),
                    value.name = "Perc")

ci_output_ll$year <- 7:11
ci_output_ll <- melt(ci_output_ll,
                     variable.name = "Exposure",
                     measure.vars = levels(cohort_all$exposure_highest_4_grp_y46),
                     value.name = "Perc")

ci_output_ul$year <- 7:11
ci_output_ul <- melt(ci_output_ul,
                     variable.name = "Exposure",
                     measure.vars = levels(cohort_all$exposure_highest_4_grp_y46),
                     value.name = "Perc")

ci_output_p$ll <- ci_output_ll$Perc
ci_output_p$ul <- ci_output_ul$Perc
rm(ci_output_ll, ci_output_ul)

ci_output_p[ll < 0]$ll <- 0

p2 <- ggplot(data = ci_output_p) +
  geom_line(aes(x = year, y = Perc, colour = Exposure), size = 1) +
  geom_point(aes(x = year, y = Perc, colour = Exposure), size = 2,
             position = position_dodge(0.05)) +
  geom_errorbar(aes(x = year, ymin = ll, ymax = ul, colour = Exposure),
                width = 0.3,
                position = position_dodge(0.05)) +
  xlab("Year") +
  ylab("Cumulative permanent exclusion (%)") +
  ggtitle("") +
  scale_y_continuous(limits = c(0, 100),
                     expand = c(0, 0)) +
  scale_x_continuous(breaks = 7:11,
                     labels = 7:11,
                     expand = c(0, 0)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        legend.key = element_blank(),
        legend.position = "none")

rm(ci_data, ci_output, ci_output_p)

# ** any ------------------------------------------------------------------

ci_data <- create_ci_data(subgroups = F,
                          pmr_vector = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F]$pupilmatchingrefanonymous,
                          outcome_var = cohort_all$any_excluded_year)

ci_output <- create_ci_output(subgroups = F, ci_data = ci_data)
ci_output

write.csv(ci_output, "OUTPUTS/CHAPTER 7/ci/ci-any-excl-ns.csv")
rm(ci_data, ci_output)

ci_data <- create_ci_data(pmr_vector = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F]$pupilmatchingrefanonymous,
                          outcome_var = cohort_all$any_excluded_year,
                          factor_var = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F]$exposure_highest_4_grp_y46)

ci_output <- create_ci_output(ci_data = ci_data, names_for_cols = levels(cohort_all$exposure_highest_4_grp_y46))
ci_output
write.csv(ci_output, "OUTPUTS/CHAPTER 7/ci/ci-any-excl-by-exp-ns.csv")

ci_output_p <- data.table(ci_output[, 5:8])
ci_output_ll <- data.table(ci_output[, 9:12])
ci_output_ul <- data.table(ci_output[, 13:16])

ci_output_p$year <- 7:11
ci_output_p <- melt(ci_output_p,
                    variable.name = "Exposure",
                    measure.vars = levels(cohort_all$exposure_highest_4_grp_y46),
                    value.name = "Perc")

ci_output_ll$year <- 7:11
ci_output_ll <- melt(ci_output_ll,
                     variable.name = "Exposure",
                     measure.vars = levels(cohort_all$exposure_highest_4_grp_y46),
                     value.name = "Perc")

ci_output_ul$year <- 7:11
ci_output_ul <- melt(ci_output_ul,
                     variable.name = "Exposure",
                     measure.vars = levels(cohort_all$exposure_highest_4_grp_y46),
                     value.name = "Perc")

ci_output_p$ll <- ci_output_ll$Perc
ci_output_p$ul <- ci_output_ul$Perc
rm(ci_output_ll, ci_output_ul)

p3 <- ggplot(data = ci_output_p) +
  geom_line(aes(x = year, y = Perc, colour = Exposure), size = 1) +
  geom_point(aes(x = year, y = Perc, colour = Exposure), size = 2,
             position = position_dodge(0.05)) +
  geom_errorbar(aes(x = year, ymin = ll, ymax = ul, colour = Exposure),
                width = 0.3,
                position = position_dodge(0.05)) +
  xlab("Year") +
  ylab("Cumulative any exclusion (%)") +
  ggtitle("") +
  scale_y_continuous(limits = c(0, 100),
                     expand = c(0, 0)) +
  scale_x_continuous(breaks = 7:11,
                     labels = 7:11,
                     expand = c(0, 0)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        legend.key = element_blank(),
        legend.position = c(0.25, 0.8))

rm(ci_data, ci_output, ci_output_p)

# ** combine plot ---------------------------------------------------------

tiff("OUTPUTS/CHAPTER 7/ci/ci-excl-not-special.tiff",
     width = 12, height = 6, units = "in", res = 300)
grid.arrange(p1, p2, p3, nrow = 1)
dev.off()
rm(p1, p2, p3)

# * special --------------------------------------------------------------

# ** ft -------------------------------------------------------------------

ci_data <- create_ci_data(subgroups = F,
                          pmr_vector = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T]$pupilmatchingrefanonymous,
                          outcome_var = cohort_all$ft_excluded_year)

ci_output <- create_ci_output(subgroups = F, ci_data = ci_data)
ci_output

write.csv(ci_output, "OUTPUTS/CHAPTER 7/ci/ci-ft-excl-s.csv")
rm(ci_data, ci_output)

ci_data <- create_ci_data(pmr_vector = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T]$pupilmatchingrefanonymous,
                          outcome_var = cohort_all$ft_excluded_year,
                          factor_var = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T]$exposure_highest_4_grp_y46)

ci_output <- create_ci_output(ci_data = ci_data, names_for_cols = levels(cohort_all$exposure_highest_4_grp_y46))
ci_output
write.csv(ci_output, "OUTPUTS/CHAPTER 7/ci/ci-ft-excl-by-exp-s.csv")
rm(ci_data, ci_output)

# ** perm -----------------------------------------------------------------

ci_data <- create_ci_data(subgroups = F,
                          pmr_vector = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T]$pupilmatchingrefanonymous,
                          outcome_var = cohort_all$perm_excluded_year)

ci_output <- create_ci_output(subgroups = F, ci_data = ci_data)
ci_output

write.csv(ci_output, "OUTPUTS/CHAPTER 7/ci/ci-perm-excl-s.csv")
rm(ci_data, ci_output)

ci_data <- create_ci_data(pmr_vector = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T]$pupilmatchingrefanonymous,
                          outcome_var = cohort_all$perm_excluded_year,
                          factor_var = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T]$exposure_highest_4_grp_y46)

ci_output <- create_ci_output(ci_data = ci_data, names_for_cols = levels(cohort_all$exposure_highest_4_grp_y46))
ci_output
write.csv(ci_output, "OUTPUTS/CHAPTER 7/ci/ci-perm-excl-by-exp-s.csv")
rm(ci_data, ci_output)

# ** any ------------------------------------------------------------------

ci_data <- create_ci_data(subgroups = F,
                          pmr_vector = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T]$pupilmatchingrefanonymous,
                          outcome_var = cohort_all$any_excluded_year)

ci_output <- create_ci_output(subgroups = F, ci_data = ci_data)
ci_output

write.csv(ci_output, "OUTPUTS/CHAPTER 7/ci/ci-any-excl-s.csv")
rm(ci_data, ci_output)

ci_data <- create_ci_data(pmr_vector = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T]$pupilmatchingrefanonymous,
                          outcome_var = cohort_all$any_excluded_year,
                          factor_var = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T]$exposure_highest_4_grp_y46)

ci_output <- create_ci_output(ci_data = ci_data, names_for_cols = levels(cohort_all$exposure_highest_4_grp_y46))
ci_output
write.csv(ci_output, "OUTPUTS/CHAPTER 7/ci/ci-any-excl-by-exp-s.csv")

ci_output_p <- data.table(ci_output[, 5:8])
ci_output_ll <- data.table(ci_output[, 9:12])
ci_output_ul <- data.table(ci_output[, 13:16])

ci_output_p$year <- 7:11
ci_output_p <- melt(ci_output_p,
                    variable.name = "Exposure",
                    measure.vars = levels(cohort_all$exposure_highest_4_grp_y46),
                    value.name = "Perc")

ci_output_ll$year <- 7:11
ci_output_ll <- melt(ci_output_ll,
                     variable.name = "Exposure",
                     measure.vars = levels(cohort_all$exposure_highest_4_grp_y46),
                     value.name = "Perc")

ci_output_ul$year <- 7:11
ci_output_ul <- melt(ci_output_ul,
                     variable.name = "Exposure",
                     measure.vars = levels(cohort_all$exposure_highest_4_grp_y46),
                     value.name = "Perc")

ci_output_p$ll <- ci_output_ll$Perc
ci_output_p$ul <- ci_output_ul$Perc
rm(ci_output_ll, ci_output_ul)

tiff("OUTPUTS/CHAPTER 7/ci/ci-excl-special.tiff",
     width = 6, height = 6, units = "in", res = 300)
ggplot(data = ci_output_p) +
  geom_line(aes(x = year, y = Perc, colour = Exposure), size = 1) +
  geom_point(aes(x = year, y = Perc, colour = Exposure), size = 2,
             position = position_dodge(0.05)) +
  geom_errorbar(aes(x = year, ymin = ll, ymax = ul, colour = Exposure),
                width = 0.3,
                position = position_dodge(0.05)) +
  xlab("Year") +
  ylab("Cumulative any exclusion (%)") +
  ggtitle("Special schools") +
  scale_y_continuous(limits = c(0, 100),
                     expand = c(0, 0)) +
  scale_x_continuous(breaks = 7:11,
                     labels = 7:11,
                     expand = c(0, 0)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        legend.key = element_blank(),
        legend.position = c(0.25, 0.8))
dev.off()

rm(ci_data, ci_output, ci_output_p)

# FIXED-TERM DURATION -----------------------------------------------------

# * continuous measures ---------------------------------------------------

# average duration per excl per year
cohort_all[, average_ft_dur := as.double(NA)]
cohort_all[!is.na(totalfixedexclusions), average_ft_dur := totalfixedsessions / totalfixedexclusions]

# total exclusions across y711
cohort_all[, total_ft_n_y711 := as.double(NA)]
cohort_all[!is.na(totalfixedexclusions), total_ft_n_y711 := sum(totalfixedexclusions), by = .(pupilmatchingrefanonymous)]
cohort_all[, total_ft_n_y711 := total_ft_n_y711[!is.na(total_ft_n_y711)][1], by = .(pupilmatchingrefanonymous)]

# total duration across y711
cohort_all[, total_ft_dur_y711 := as.double(NA)]
cohort_all[!is.na(totalfixedexclusions), total_ft_dur_y711 := sum(totalfixedsessions), by = .(pupilmatchingrefanonymous)]
cohort_all[, total_ft_dur_y711 := total_ft_dur_y711[!is.na(total_ft_dur_y711)][1], by = .(pupilmatchingrefanonymous)]

# average duration across y711
cohort_all[, average_ft_dur_y711 := as.double(NA)]
cohort_all[!is.na(totalfixedexclusions), average_ft_dur_y711 := total_ft_dur_y711 / total_ft_n_y711, by = .(pupilmatchingrefanonymous)]
cohort_all[, average_ft_dur_y711 := average_ft_dur_y711[!is.na(average_ft_dur_y711)][1], by = .(pupilmatchingrefanonymous)]

# total average duration across children across school - by RFs
vars <- c("total_ft_n_y711", "total_ft_dur_y711", "average_ft_dur_y711")

# not special
write.csv(print(CreateTableOne(data = cohort_all[sch_n_in_year == 1 &
                                                   year == 7 &
                                                   special_school == F &
                                                   ft_excl_y711 == T],
                               vars = vars,
                               test = F,
                               includeNA = T),
                nonnormal = vars,
                showAllLevels = T,
                printToggle = F,
                noSpaces = T),
          file = "OUTPUTS/CHAPTER 7/ft-durations-overall-not-special.csv")

write.csv(print(CreateTableOne(data = cohort_all[sch_n_in_year == 1 &
                                                   year == 7 &
                                                   special_school == F &
                                                   ft_excl_y711 == T],
                               vars = vars,
                               test = F,
                               includeNA = T,
                               strata = "exposure_highest_4_grp_y46"),
                nonnormal = vars,
                showAllLevels = T,
                printToggle = F,
                noSpaces = T),
          file = "OUTPUTS/CHAPTER 7/ft-durations-csc46-not-special.csv")

write.csv(print(CreateTableOne(data = cohort_all[sch_n_in_year == 1 &
                                                   year == 7 &
                                                   special_school == F &
                                                   ft_excl_y711 == T],
                               vars = vars,
                               test = F,
                               includeNA = T,
                               strata = c("exposure_highest_4_grp_y46", "highest_ever_sen_primary")),
                nonnormal = vars,
                showAllLevels = T,
                printToggle = F,
                noSpaces = T),
          file = "OUTPUTS/CHAPTER 7/ft-durations-csc46-highest-sen-not-special.csv")



# special
write.csv(print(CreateTableOne(data = cohort_all[sch_n_in_year == 1 &
                                                   year == 7 &
                                                   special_school == T &
                                                   ft_excl_y711 == T],
                               vars = vars,
                               test = F,
                               includeNA = T),
                nonnormal = vars,
                showAllLevels = T,
                printToggle = F,
                noSpaces = T),
          file = "OUTPUTS/CHAPTER 7/ft-durations-overall-special.csv")

write.csv(print(CreateTableOne(data = cohort_all[sch_n_in_year == 1 &
                                                   year == 7 &
                                                   special_school == T &
                                                   ft_excl_y711 == T],
                               vars = vars,
                               test = F,
                               includeNA = T,
                               strata = "exposure_highest_4_grp_y46"),
                nonnormal = vars,
                showAllLevels = T,
                printToggle = F,
                noSpaces = T),
          file = "OUTPUTS/CHAPTER 7/ft-durations-csc46-special.csv")

# OUTCOMES BY LA ----------------------------------------------------------

# * not special ----------------------------------------------------------

# ** graphical ------------------------------------------------------------

tmp <- data.table(
  la = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F]$la,
  region = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F]$region_major,
  any_excl_y79 = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F]$any_excl_y79,
  any_excl_y1011 = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F]$any_excl_y1011
)

tmp <- tmp[order(la)]
tmp[, la_pop_n := max(seq_len(.N)), by = la]

tmp[, la_any_excl_y79_n := sum(any_excl_y79), by = la]
tmp[, la_any_excl_y79_prop := mean(any_excl_y79), by = la]
tmp[, la_any_excl_y79_se := sqrt((la_any_excl_y79_prop * (1 - la_any_excl_y79_prop)) / la_pop_n)]
tmp[, la_any_excl_y1011_n := sum(any_excl_y1011), by = la]
tmp[, la_any_excl_y1011_prop := mean(any_excl_y1011), by = la]
tmp[, la_any_excl_y1011_se := sqrt((la_any_excl_y1011_prop * (1 - la_any_excl_y1011_prop)) / la_pop_n)]

tmp <- tmp[, c(1:2, 5:11)]
tmp <- tmp[!duplicated(tmp)]

tmp <- tmp[order(region, la_any_excl_y79_prop)]
tmp$la_int <- 1:151
table(tmp$la_any_excl_y79_n < 10)

p1 <- ggplot(tmp[la_any_excl_y79_n >= 10], aes(x = la_int, y = la_any_excl_y79_prop, colour = region)) +
  geom_point() +
  geom_errorbar(ymin = tmp[la_any_excl_y79_n >= 10]$la_any_excl_y79_prop - 1.96 * tmp[la_any_excl_y79_n >= 10]$la_any_excl_y79_se,
                ymax = tmp[la_any_excl_y79_n >= 10]$la_any_excl_y79_prop + 1.96 * tmp[la_any_excl_y79_n >= 10]$la_any_excl_y79_se) +
  scale_y_continuous(limits = c(0, 0.3),
                     breaks = c(0, 0.1, 0.2, 0.3),
                     labels = c("0%", "10%", "20%", "30%")) +
  xlab("Local authority") +
  ylab("Percentage of children excluded\n(fixed term or permanent) across years 7 to 9") +
  ggtitle("A: Percentage of children excluded across years 7 to 9") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text = element_text(colour = "black"),
        legend.key = element_blank(),
        legend.position = c(0.5, 0.75)) +
  guides(colour = guide_legend(nrow = 3)) +
  labs(colour = "Region") +
  geom_hline(yintercept = mean(tmp[la_any_excl_y79_n >= 10]$la_any_excl_y79_prop),
             linetype = "dashed",
             colour = "red")

tmp <- tmp[order(region, la_any_excl_y1011_prop)]
tmp$la_int <- 1:151
table(tmp$la_any_excl_y1011_n < 10)

p2 <- ggplot(tmp[la_any_excl_y1011_n >= 10], aes(x = la_int, y = la_any_excl_y1011_prop, colour = region)) +
  geom_point() +
  geom_errorbar(ymin = tmp[la_any_excl_y1011_n >= 10]$la_any_excl_y1011_prop - 1.96 * tmp[la_any_excl_y1011_n >= 10]$la_any_excl_y1011_se,
                ymax = tmp[la_any_excl_y1011_n >= 10]$la_any_excl_y1011_prop + 1.96 * tmp[la_any_excl_y1011_n >= 10]$la_any_excl_y1011_se) +
  scale_y_continuous(limits = c(0, 0.3),
                     breaks = c(0, 0.1, 0.2, 0.3),
                     labels = c("0%", "10%", "20%", "30%")) +
  xlab("Local authority") +
  ylab("Percentage of children excluded\n(fixed term or permanent) across years 10 to 11") +
  ggtitle("A: Percentage of children excluded across years 10 to 11") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text = element_text(colour = "black"),
        legend.position = "none") +
  geom_hline(yintercept = mean(tmp[la_any_excl_y79_n >= 10]$la_any_excl_y1011_prop),
             linetype = "dashed",
             colour = "red")

write.csv(tmp, file = "OUTPUTS/CHAPTER 7/la-cat-plots-underlying-data-not-special.csv", row.names = F)

# ** numerical -----------------------------------------------------------

# 79
# LA level
summary(tmp$la_any_excl_y79_n)
summary(tmp$la_any_excl_y79_prop)
hist(tmp$la_any_excl_y79_n, breaks = 15)
hist(tmp$la_any_excl_y79_prop, breaks = 15)

# regional level
# tmp <- tmp[la_any_excl_y79_n >= 10]

tmp[, region_pop_n := sum(la_pop_n), by = .(region)]
tmp[, region_any_excl_y79_n := sum(la_any_excl_y79_n), by = .(region)]
tmp[, region_any_excl_y79_prop := region_any_excl_y79_n / region_pop_n]

tmp[, region_min_y79_prop := min(la_any_excl_y79_prop), by = .(region)]
tmp[, region_min_y79_n := min(la_any_excl_y79_n), by = .(region)]

tmp[, region_25c_y79_prop := quantile(la_any_excl_y79_prop, 0.25), by = .(region)]
tmp[, region_25c_y79_n := quantile(la_any_excl_y79_n, 0.25), by = .(region)]

tmp[, region_50c_y79_prop := quantile(la_any_excl_y79_prop, 0.5), by = .(region)]
tmp[, region_50c_y79_n := quantile(la_any_excl_y79_n, 0.50), by = .(region)]

tmp[, region_75c_y79_prop := quantile(la_any_excl_y79_prop, 0.75), by = .(region)]
tmp[, region_75c_y79_n := quantile(la_any_excl_y79_n, 0.75), by = .(region)]

tmp[, region_max_y79_prop := max(la_any_excl_y79_prop), by = .(region)]
tmp[, region_max_y79_n := max(la_any_excl_y79_n), by = .(region)]

tmp_2 <- tmp[, c("region", "region_pop_n", "region_any_excl_y79_n", "region_any_excl_y79_prop",
               "region_min_y79_prop", "region_min_y79_n",
               "region_25c_y79_prop", "region_25c_y79_n",
               "region_50c_y79_prop", "region_50c_y79_n",
               "region_75c_y79_prop", "region_75c_y79_n",
               "region_max_y79_prop")]
tmp_2 <- tmp_2[!duplicated(tmp_2)]
tmp_2

write.csv(tmp_2, file = "OUTPUTS/CHAPTER 7/excl-y79-region-ns.csv")

summary(tmp_2$region_any_excl_y79_n)
summary(tmp_2$region_any_excl_y79_prop)
rm(tmp_2)

# 1011
# LA level
summary(tmp$la_any_excl_y1011_n)
summary(tmp$la_any_excl_y1011_prop)
hist(tmp$la_any_excl_y1011_n, breaks = 15)
hist(tmp$la_any_excl_y1011_prop, breaks = 15)

# regional level
# tmp <- tmp[la_any_excl_y1011_n >= 10]

tmp[, region_pop_n := sum(la_pop_n), by = .(region)]
tmp[, region_any_excl_y1011_n := sum(la_any_excl_y1011_n), by = .(region)]
tmp[, region_any_excl_y1011_prop := region_any_excl_y1011_n / region_pop_n]

tmp[, region_min_y1011_prop := min(la_any_excl_y1011_prop), by = .(region)]
tmp[, region_min_y1011_n := min(la_any_excl_y1011_n), by = .(region)]

tmp[, region_25c_y1011_prop := quantile(la_any_excl_y1011_prop, 0.25), by = .(region)]
tmp[, region_25c_y1011_n := quantile(la_any_excl_y1011_n, 0.25), by = .(region)]

tmp[, region_50c_y1011_prop := quantile(la_any_excl_y1011_prop, 0.5), by = .(region)]
tmp[, region_50c_y1011_n := quantile(la_any_excl_y1011_n, 0.50), by = .(region)]

tmp[, region_75c_y1011_prop := quantile(la_any_excl_y1011_prop, 0.75), by = .(region)]
tmp[, region_75c_y1011_n := quantile(la_any_excl_y1011_n, 0.75), by = .(region)]

tmp[, region_max_y1011_prop := max(la_any_excl_y1011_prop), by = .(region)]
tmp[, region_max_y1011_n := max(la_any_excl_y1011_n), by = .(region)]

tmp_2 <- tmp[, c("region", "region_pop_n", "region_any_excl_y1011_n", "region_any_excl_y1011_prop",
                 "region_min_y1011_prop", "region_min_y1011_n",
                 "region_25c_y1011_prop", "region_25c_y1011_n",
                 "region_50c_y1011_prop", "region_50c_y1011_n",
                 "region_75c_y1011_prop", "region_75c_y1011_n",
                 "region_max_y1011_prop")]

tmp_2 <- tmp_2[!duplicated(tmp_2)]
tmp_2

write.csv(tmp_2, file = "OUTPUTS/CHAPTER 7/excl-y1011-region-ns.csv")

summary(tmp_2$region_any_excl_y1011_n)
summary(tmp_2$region_any_excl_y1011_prop)

rm(tmp, tmp_2)

# * special ----------------------------------------------------------

# ** graphical ------------------------------------------------------------

tmp <- data.table(
  la = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T]$la,
  region = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T]$region_major,
  any_excl_y711 = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T]$any_excl_y711
)

tmp <- tmp[order(la)]
tmp[, la_pop_n := max(seq_len(.N)), by = la]

tmp[, la_any_excl_y711_n := sum(any_excl_y711), by = la]
tmp[, la_any_excl_y711_prop := mean(any_excl_y711), by = la]
tmp[, la_any_excl_y711_se := sqrt((la_any_excl_y711_prop * (1 - la_any_excl_y711_prop)) / la_pop_n)]

tmp <- tmp[, c(1:2, 4:7)]
tmp <- tmp[!duplicated(tmp)]

tmp <- tmp[order(region, la_any_excl_y711_prop)]
tmp$la_int <- 1:148
table(tmp$la_any_excl_y711_n < 10)

p3 <- ggplot(tmp[la_any_excl_y711_n >= 10], aes(x = la_int, y = la_any_excl_y711_prop, colour = region)) +
  geom_point() +
  geom_errorbar(ymin = tmp[la_any_excl_y711_n >= 10]$la_any_excl_y711_prop - 1.96 * tmp[la_any_excl_y711_n >= 10]$la_any_excl_y711_se,
                ymax = tmp[la_any_excl_y711_n >= 10]$la_any_excl_y711_prop + 1.96 * tmp[la_any_excl_y711_n >= 10]$la_any_excl_y711_se) +
  scale_y_continuous(limits = c(0, 0.8)) +
  xlab("Local authority") +
  ylab("Percentage of children excluded\n(fixed term or permanent) across years 7 to 11") +
  ggtitle("A: percentage of children excluded across years 7 to 11") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text = element_text(colour = "black"),
        legend.key = element_blank(),
        legend.position = c(0.5, 0.8)) +
  guides(colour = guide_legend(nrow = 3)) +
  labs(colour = "Region") +
  geom_hline(yintercept = mean(tmp[la_any_excl_y711_n >= 10]$la_any_excl_y711_prop),
             linetype = "dashed",
             colour = "red")

write.csv(tmp, file = "OUTPUTS/CHAPTER 7/la-cat-plots-underlying-data-special.csv", row.names = F)
  
# ** numerical -----------------------------------------------------------

# 79
# LA level
summary(tmp$la_any_excl_y711_n)
summary(tmp$la_any_excl_y711_prop)
hist(tmp$la_any_excl_y711_n, breaks = 15)
hist(tmp$la_any_excl_y711_prop, breaks = 15)

# regional level
# tmp <- tmp[la_any_excl_y711_n >= 10]

tmp[, region_pop_n := sum(la_pop_n), by = .(region)]
tmp[, region_any_excl_y711_n := sum(la_any_excl_y711_n), by = .(region)]
tmp[, region_any_excl_y711_prop := region_any_excl_y711_n / region_pop_n]

tmp[, region_min_y711_prop := min(la_any_excl_y711_prop), by = .(region)]
tmp[, region_min_y711_n := min(la_any_excl_y711_n), by = .(region)]

tmp[, region_25c_y711_prop := quantile(la_any_excl_y711_prop, 0.25), by = .(region)]
tmp[, region_25c_y711_n := quantile(la_any_excl_y711_n, 0.25), by = .(region)]

tmp[, region_50c_y711_prop := quantile(la_any_excl_y711_prop, 0.5), by = .(region)]
tmp[, region_50c_y711_n := quantile(la_any_excl_y711_n, 0.50), by = .(region)]

tmp[, region_75c_y711_prop := quantile(la_any_excl_y711_prop, 0.75), by = .(region)]
tmp[, region_75c_y711_n := quantile(la_any_excl_y711_n, 0.75), by = .(region)]

tmp[, region_max_y711_prop := max(la_any_excl_y711_prop), by = .(region)]
tmp[, region_max_y711_n := max(la_any_excl_y711_n), by = .(region)]

tmp_2 <- tmp[, c("region", "region_pop_n", "region_any_excl_y711_n", "region_any_excl_y711_prop",
                 "region_min_y711_prop", "region_min_y711_n",
                 "region_25c_y711_prop", "region_25c_y711_n",
                 "region_50c_y711_prop", "region_50c_y711_n",
                 "region_75c_y711_prop", "region_75c_y711_n",
                 "region_max_y711_prop")]
tmp_2 <- tmp_2[!duplicated(tmp_2)]
tmp_2

write.csv(tmp_2, file = "OUTPUTS/CHAPTER 7/excl-y711-region-s.csv")

summary(tmp_2$region_any_excl_y711_n)
summary(tmp_2$region_any_excl_y711_prop)
rm(tmp, tmp_2)

# ** combine plot --------------------------------------------------------

# thesis
tiff("OUTPUTS/CHAPTER 7/la/any-excl-by-la.tiff",
     width = 12, height = 6, units = "in", res = 300)
grid.arrange(p1, p2, p3, nrow = 2)
dev.off()

# paper
tiff("OUTPUTS/CHAPTER 7/la/any-excl-by-la-paper-not-special.tiff",
     width = 12, height = 12, units = "in", res = 300)
grid.arrange(p1, p2, nrow = 2)
dev.off()

tiff("OUTPUTS/CHAPTER 7/la/any-excl-by-la-paper-special.tiff",
     width = 12, height = 6, units = "in", res = 300)
p3
dev.off()

rm(p1, p2, p3)

# CREATE MODELLING DATA ---------------------------------------------

model_data_non_special <- cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F,
                                     c("pupilmatchingrefanonymous", "cohort",
                                       "female_clean", "eth_major", "language_clean",
                                       "special_school", "idaci_quintiles", "fsmeligible",
                                       "idaci_fsm", "region_major", "la",
                                       "ever_sen_primary", "highest_ever_sen_primary",
                                       "ever_sen_y09", "highest_ever_sen_y09",
                                       "exposure_highest_4_grp_y46", "exposure_highest_4_grp_y49",
                                       "appru_y06", "appru_y09",
                                       "ft_excl_y79",
                                       "ft_excl_y1011",
                                       "ft_excl_y711",
                                       "perm_excl_y79",
                                       "perm_excl_y1011",
                                       "perm_excl_y711",
                                       "any_excl_y79",
                                       "any_excl_y1011",
                                       "any_excl_y711")]

model_data_non_special[, region_major := relevel(region_major, ref = "South East")]
model_data_non_special <- model_data_non_special[order(la)]

model_data_special <- cohort_all[year == 7 & sch_n_in_year == 1  & special_school == T,
                                 c("pupilmatchingrefanonymous", "cohort",
                                   "female_clean", "eth_major", "language_clean",
                                   "special_school", "idaci_quintiles", "fsmeligible",
                                   "idaci_fsm", "region_major", "la",
                                   "exposure_highest_4_grp_y46",
                                   "appru_y06",
                                   "any_excl_y711")]

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

# ** add LA variables to special ------------------------------------------------------

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
          file = "OUTPUTS/CHAPTER 7/lait-vars-la-level-minmax.csv")
write.csv(print(CreateTableOne(data = lait_vars,
                               vars = vars,
                               test = F,
                               includeNA = T),
                nonnormal = vars,
                showAllLevels = T,
                printToggle = F,
                noSpaces = T),
          file = "OUTPUTS/CHAPTER 7/lait-vars-la-level.csv")
rm(vars)

# vars from NPD
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
          file = "OUTPUTS/CHAPTER 7/la-npd-vars-cohort1.csv")
write.csv(print(CreateTableOne(data = la_df_c1,
                               vars = vars,
                               test = F,
                               includeNA = T),
                nonnormal = vars,
                showAllLevels = T,
                printToggle = F,
                noSpaces = T,
                minMax = T),
          file = "OUTPUTS/CHAPTER 7/la-npd-vars-cohort1-minmax.csv")

vars <- c("la_n_c2", "la_fsm_pc_c2", "open_cin_pc_c2", "new_cin_pc_c2", "la_special_schools_pc_c2")
write.csv(print(CreateTableOne(data = la_df_c2,
                               vars = vars,
                               test = F,
                               includeNA = T),
                nonnormal = vars,
                showAllLevels = T,
                printToggle = F,
                noSpaces = T),
          file = "OUTPUTS/CHAPTER 7/la-npd-vars-cohort2.csv")
write.csv(print(CreateTableOne(data = la_df_c2,
                               vars = vars,
                               test = F,
                               includeNA = T),
                nonnormal = vars,
                showAllLevels = T,
                printToggle = F,
                noSpaces = T,
                minMax = T),
          file = "OUTPUTS/CHAPTER 7/la-npd-vars-cohort2-minmax.csv")

rm(vars, la_df_c1, la_df_c2, lait_vars)

# OUTCOMES DESC BY LA VARS ------------------------------------------------

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

outcomes <- c("ft_excl_y79",
              "ft_excl_y1011",
              "ft_excl_y711",
              "perm_excl_y79",
              "perm_excl_y1011",
              "perm_excl_y711",
              "any_excl_y79",
              "any_excl_y1011",
              "any_excl_y711")

final_output <- cbind(describe_outcome(exposure = exposures, outcome = outcomes[1], dat = model_data_non_special),
                      describe_outcome(exposure = exposures, outcome = outcomes[2], dat = model_data_non_special),
                      describe_outcome(exposure = exposures, outcome = outcomes[3], dat = model_data_non_special),
                      describe_outcome(exposure = exposures, outcome = outcomes[4], dat = model_data_non_special),
                      describe_outcome(exposure = exposures, outcome = outcomes[5], dat = model_data_non_special),
                      describe_outcome(exposure = exposures, outcome = outcomes[6], dat = model_data_non_special),
                      describe_outcome(exposure = exposures, outcome = outcomes[7], dat = model_data_non_special),
                      describe_outcome(exposure = exposures, outcome = outcomes[8], dat = model_data_non_special),
                      describe_outcome(exposure = exposures, outcome = outcomes[9], dat = model_data_non_special))
write.csv(final_output, file = "OUTPUTS/CHAPTER 7/outcomes-by-LA-RFs-not-special.csv")

# special
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

model_data_special$la_special_schools_prop_cat <- as.integer(cut(model_data_special$la_special_schools_prop,
                                                                 breaks = quantile(model_data_special$la_special_schools_prop,
                                                                                   probs = seq(0, 1, 0.25)),
                                                                 include.lowest = T,
                                                                 right = T))

outcomes <- c("any_excl_y711")

final_output <- cbind(describe_outcome(exposure = exposures, outcome = outcomes, dat = model_data_special))
write.csv(final_output, file = "OUTPUTS/CHAPTER 7/outcomes-by-LA-RFs-special.csv")

rm(exposures, outcomes, final_output)

# SAVE AND LOAD MODEL DATA ------------------------------------------------

save(model_data_non_special, file = "PROCESSED DATA/EXCL_model_data_non_special.rda")
save(model_data_special, file = "PROCESSED DATA/EXCL_model_data_special.rda")

load("PROCESSED DATA/EXCL_model_data_non_special.rda")
load("PROCESSED DATA/EXCL_model_data_special.rda")

# MODELLING - not special --------------------------------------------------------------

# * describe size of units ---------------------------------------------------------------

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

# * estimate models -------------------------------------------------------

# ** 79 --------------------------------------------------------------------

# *** univariable models ---------------------------------------------------

gc()
um1 <- glmer(any_excl_y79 ~ exposure_highest_4_grp_y46 +
               (1 | la),
             data = model_data_non_special,
             family = binomial)

gc()
um2 <- glmer(any_excl_y79 ~ highest_ever_sen_primary +
               (1 | la),
             data = model_data_non_special,
             family = binomial)

gc()
um3 <- glmer(any_excl_y79 ~ ever_sen_primary +
               (1 | la),
             data = model_data_non_special,
             family = binomial)

gc()
um4 <- glmer(any_excl_y79 ~ female_clean +
               (1 | la),
             data = model_data_non_special,
             family = binomial)

gc()
um5 <- glmer(any_excl_y79 ~ eth_major +
               (1 | la),
             data = model_data_non_special,
             family = binomial,
             control = glmerControl(check.conv.grad = .makeCC("warning", tol = 2e-3, relTol = NULL))) # default ftc 0.00103 / 1 mil 0.00103  / nm 1 mil 0.00161123

gc()
um6 <- glmer(any_excl_y79 ~ language_clean +
               (1 | la),
             data = model_data_non_special,
             family = binomial)

gc()
um7 <- glmer(any_excl_y79 ~ idaci_fsm +
               (1 | la),
             data = model_data_non_special,
             family = binomial)

gc()
um8 <- glmer(any_excl_y79 ~ appru_y06 +
               (1 | la),
             data = model_data_non_special,
             family = binomial,
             control = glmerControl(check.conv.grad = .makeCC("warning", tol = 2e-3, relTol = NULL))) # default 0.001233 / 1 mil 0.001233 / nm 1 mil 0.001000915

gc()
um9 <- glmer(any_excl_y79 ~ idaci_quintiles +
               (1 | la),
             data = model_data_non_special,
             family = binomial)

gc()
um10 <- glmer(any_excl_y79 ~ fsmeligible +
                (1 | la),
              data = model_data_non_special,
              family = binomial)

gc()
um11 <- glmer(any_excl_y79 ~ I(per_ag_sw / 10) +
                (1 | la),
              data = model_data_non_special,
              family = binomial,
              control = glmerControl(optimizer = "Nelder_Mead",
                                     check.conv.grad = .makeCC("warning", tol = 2e-3, relTol = NULL),
                                     optCtrl = list(maxfun = 2e6))) # default ftc 0.00216 / 1 mil 0.00216 / nm 1 mil 0.00130414

gc()
um12 <- glmer(any_excl_y79 ~ I(per_sw_turnover / 10) +
                (1 | la),
              data = model_data_non_special,
              family = binomial,
              control = glmerControl(check.conv.grad = .makeCC("warning", tol = 2e-3, relTol = NULL))) # default ftc 0.00121979 / 1 mil 0.00121979 / nm 1 mil 0.00210957

gc()
um13 <- glmer(any_excl_y79 ~ I(la_n / 10000) +
                (1 | la),
              data = model_data_non_special,
              family = binomial)

gc()
um14 <- glmer(any_excl_y79 ~ I(la_fsm_prop * 10) +
                (1 | la),
              data = model_data_non_special,
              family = binomial)

gc()
um15 <- glmer(any_excl_y79 ~ I(open_cin_prop * 100) +
                (1 | la),
              data = model_data_non_special,
              family = binomial,
              control = glmerControl(check.conv.grad = .makeCC("warning", tol = 2e-3, relTol = NULL))) # default ftc 0.001186 / 1 mil 0.001186 / nm 1 mil 0.00192352

gc()
um16 <- glmer(any_excl_y79 ~ I(new_cin_prop * 100) +
                (1 | la),
              data = model_data_non_special,
              family = binomial)

gc()
um17 <- glmer(any_excl_y79 ~ I(sw_s251 / 100) +
                (1 | la),
              data = model_data_non_special,
              family = binomial)

gc()
um18 <- glmer(any_excl_y79 ~ I(la_special_schools_prop * 100) +
                (1 | la) + (1 | region_major),
              data = model_data_non_special,
              family = binomial,
              control = glmerControl(check.conv.grad = .makeCC("warning", tol = 2e-3, relTol = NULL))) # default 0.0015

gc()
models_any_y79_univar_glm_ns <- list()
models_any_y79_univar_glm_ns[[1]] <- um1
models_any_y79_univar_glm_ns[[2]] <- um2
models_any_y79_univar_glm_ns[[3]] <- um3
models_any_y79_univar_glm_ns[[4]] <- um4
models_any_y79_univar_glm_ns[[5]] <- um5
models_any_y79_univar_glm_ns[[6]] <- um6
models_any_y79_univar_glm_ns[[7]] <- um7
models_any_y79_univar_glm_ns[[8]] <- um8
models_any_y79_univar_glm_ns[[9]] <- um9
models_any_y79_univar_glm_ns[[10]] <- um10
models_any_y79_univar_glm_ns[[11]] <- um11
models_any_y79_univar_glm_ns[[12]] <- um12
models_any_y79_univar_glm_ns[[13]] <- um13
models_any_y79_univar_glm_ns[[14]] <- um14
models_any_y79_univar_glm_ns[[15]] <- um15
models_any_y79_univar_glm_ns[[16]] <- um16
models_any_y79_univar_glm_ns[[17]] <- um17
models_any_y79_univar_glm_ns[[18]] <- um18

save(models_any_y79_univar_glm_ns, file = "PROCESSED DATA/MODELS/MODELS_EXCLANY_Y79_UNIVAR_GLM_NOT_SPECIAL.RDA")
rm(um2, um3, um4, um5, um6, um7, um8, um9, um10, um11, um12, um13, um14, um15, um16, um17, um18)

summary(models_any_y79_univar_glm_ns[[1]])
summary(models_any_y79_univar_glm_ns[[2]])
summary(models_any_y79_univar_glm_ns[[3]])
summary(models_any_y79_univar_glm_ns[[4]])
summary(models_any_y79_univar_glm_ns[[5]])
summary(models_any_y79_univar_glm_ns[[6]])
summary(models_any_y79_univar_glm_ns[[7]])
summary(models_any_y79_univar_glm_ns[[8]])
summary(models_any_y79_univar_glm_ns[[9]])
summary(models_any_y79_univar_glm_ns[[10]])
summary(models_any_y79_univar_glm_ns[[11]])
summary(models_any_y79_univar_glm_ns[[12]])
summary(models_any_y79_univar_glm_ns[[13]])
summary(models_any_y79_univar_glm_ns[[14]])
summary(models_any_y79_univar_glm_ns[[15]])
summary(models_any_y79_univar_glm_ns[[16]])
summary(models_any_y79_univar_glm_ns[[17]])
summary(models_any_y79_univar_glm_ns[[18]])

# *** main models ---------------------------------------------------------

gc()
hm1 <- glmer(any_excl_y79 ~ 1 +
               (1 | la),
             data = model_data_non_special,
             family = binomial,
             control = glmerControl(check.conv.grad = .makeCC("warning", tol = 2e-3, relTol = NULL))) # ftc 0.0014 / nm 1 mil 0.00102
             

gc()
hm3 <- glmer(any_excl_y79 ~ 1 + exposure_highest_4_grp_y46 + ever_sen_primary +
               (1 | la),
             data = model_data_non_special,
             family = binomial)

gc()
hm4 <- glmer(any_excl_y79 ~ 1 + exposure_highest_4_grp_y46 * ever_sen_primary +
               (1 | la),
             data = model_data_non_special,
             family = binomial,
             control = glmerControl(check.conv.grad = .makeCC("warning", tol = 2e-3, relTol = NULL))) # ftc 0.0013 / nm 1 mil 0.00196

gc()
hm5 <- glmer(any_excl_y79 ~ 1 + exposure_highest_4_grp_y46 * ever_sen_primary +
               female_clean + eth_major + language_clean + idaci_quintiles + appru_y06 + # idaci quintiles bc convergence probs in m6
               (1 | la),
             data = model_data_non_special,
             family = binomial,
             control = glmerControl(check.conv.grad = .makeCC("warning", tol = 2e-3, relTol = NULL)))

gc()
hm6 <- glmer(any_excl_y79 ~ 1 + exposure_highest_4_grp_y46 * ever_sen_primary +
               female_clean + eth_major + language_clean + idaci_fsm + appru_y06 +
               I(per_ag_sw / 10) + I(per_sw_turnover / 10) + I(sw_s251 / 100) + I(la_n / 10000) + I(la_fsm_prop * 10) +
               I(new_cin_prop * 100) + I(la_special_schools_prop * 100) +
               (1 | la),
             data = model_data_non_special,
             family = binomial,
             control = glmerControl(optimizer = "Nelder_Mead",
                                    check.conv.grad = .makeCC("warning", tol = 2e-3, relTol = NULL),
                                    optCtrl = list(maxfun = 2e6))) # 2e3 tol, 0.00453205 / 2 mil 0.00453205 / nm 2 mil 0.00809301

gc()
hm6a <- glmer(any_excl_y79 ~ 1 + exposure_highest_4_grp_y46 * ever_sen_primary +
                female_clean + eth_major + language_clean + idaci_fsm + appru_y06 +
                I(la_n / 10000) + I(la_fsm_prop * 10) +
                (1 | la),
              data = model_data_non_special,
              family = binomial,
              control = glmerControl(optimizer = "Nelder_Mead",
                                     check.conv.grad = .makeCC("warning", tol = 2e-3, relTol = NULL),
                                     optCtrl = list(maxfun = 2e6))) # default 0.00891253 / nm 2 mil 0.00626653

gc()
hm6b <- glmer(any_excl_y79 ~ 1 + exposure_highest_4_grp_y46 * ever_sen_primary +
                female_clean + eth_major + language_clean + idaci_quintiles + appru_y06 +
                I(la_n / 10000) + I(la_fsm_prop * 10) +
                (1 | la),
              data = model_data_non_special,
              family = binomial,
              control = glmerControl(optimizer = "Nelder_Mead",
                                     check.conv.grad = .makeCC("warning", tol = 2e-3, relTol = NULL),
                                     optCtrl = list(maxfun = 2e6))) # def 0.00106196


models_any_y79_glm_ns <- list()
models_any_y79_glm_ns[[1]] <- hm1
models_any_y79_glm_ns[[2]] <- um1
models_any_y79_glm_ns[[3]] <- hm3
models_any_y79_glm_ns[[4]] <- hm4
models_any_y79_glm_ns[[5]] <- hm5
models_any_y79_glm_ns[[6]] <- hm6b
save(models_any_y79_glm_ns, file = "PROCESSED DATA/MODELS/MODELS_EXCLANY_Y79_GLM_NOT_SPECIAL.RDA")
rm(hm1, um1, hm3, hm4, hm5, hm6, hm6a, hm6b, hm6c)

summary(models_any_y79_glm_ns[[1]])
summary(models_any_y79_glm_ns[[2]])
lrtest(models_any_y79_glm_ns[[2]], models_any_y79_glm_ns[[1]])

summary(models_any_y79_glm_ns[[3]])
lrtest(models_any_y79_glm_ns[[3]], models_any_y79_glm_ns[[2]])

summary(models_any_y79_glm_ns[[4]])
lrtest(models_any_y79_glm_ns[[4]], models_any_y79_glm_ns[[3]])

summary(models_any_y79_glm_ns[[5]])
lrtest(models_any_y79_glm_ns[[5]], models_any_y79_glm_ns[[4]])

summary(models_any_y79_glm_ns[[6]])
lrtest(models_any_y79_glm_ns[[6]], models_any_y79_glm_ns[[5]])

# ** post hoc -------------------------------------------------------------

gc()
hm6_all_la_vars <- glmer(any_excl_y79 ~ 1 + exposure_highest_4_grp_y46 +
                           I(per_ag_sw / 10) + I(per_sw_turnover / 10) + I(sw_s251 / 100) + I(la_n / 10000) + I(la_fsm_prop * 10) +
                           I(new_cin_prop * 100) + I(la_special_schools_prop * 100) +
                           (1 | la),
                         data = model_data_non_special,
                         family = binomial,
                         control = glmerControl(optimizer = "Nelder_Mead",
                                                check.conv.grad = .makeCC("warning", tol = 2e-3, relTol = NULL))) # def 0.00960937 / 0.00127437

model_data_non_special[, any_csc_y46 := exposure_highest_4_grp_y46 != "None"]

gc()
hm_posthoc_random_slopes_1 <- glmer(any_excl_y79 ~ any_csc_y46 + 
                                      (1 | la),
                                    data = model_data_non_special,
                                    family = binomial) # def conv

gc()
hm_posthoc_random_slopes_2 <- glmer(any_excl_y79 ~ any_csc_y46 + 
                                      (any_csc_y46 | la),
                                    data = model_data_non_special,
                                    family = binomial,
                                    control = glmerControl(check.conv.grad = .makeCC("warning", tol = 2e-3, relTol = NULL))) # def 0.00137255

gc()
hm_posthoc_random_slopes_3 <- glmer(any_excl_y79 ~ any_csc_y46 + 
                                      ever_sen_primary + female_clean + eth_major + language_clean + idaci_quintiles + appru_y06 +
                                      (any_csc_y46 | la),
                                    data = model_data_non_special,
                                    family = binomial) # def converged

gc()
hm_posthoc_random_slopes_4 <- glmer(any_excl_y79 ~ any_csc_y46 + 
                                      ever_sen_primary + female_clean + eth_major + language_clean + idaci_quintiles + appru_y06 +
                                      I(la_n / 10000) + I(la_fsm_prop * 10) +
                                      (any_csc_y46 | la),
                                    data = model_data_non_special,
                                    family = binomial,
                                    control = glmerControl(optimizer = "Nelder_Mead")) # def 0.0153475 / nm 0.00792633

models_any_y79_glm_ns_posthoc <- list()
models_any_y79_glm_ns_posthoc[[1]] <- hm6_all_la_vars
models_any_y79_glm_ns_posthoc[[2]] <- hm_posthoc_random_slopes_1
models_any_y79_glm_ns_posthoc[[3]] <- hm_posthoc_random_slopes_2
models_any_y79_glm_ns_posthoc[[4]] <- hm_posthoc_random_slopes_3
#models_any_y79_glm_ns_posthoc[[5]] <- hm_posthoc_random_slopes_4
save(models_any_y79_glm_ns_posthoc, file = "PROCESSED DATA/MODELS/MODELS_EXCLANY_y79_GLM_NOT_SPECIAL_POSTHOC.RDA")
rm(hm6_all_la_vars, hm_posthoc_random_slopes_1, hm_posthoc_random_slopes_2, hm_posthoc_random_slopes_3,
   hm_posthoc_random_slopes_4)

summary(models_any_y79_glm_ns_posthoc[[1]])

summary(models_any_y79_glm_ns_posthoc[[2]])
summary(models_any_y79_glm_ns_posthoc[[3]])
lrtest(models_any_y79_glm_ns_posthoc[[3]], models_any_y79_glm_ns_posthoc[[2]])

summary(models_any_y79_glm_ns_posthoc[[4]])
lrtest(models_any_y79_glm_ns_posthoc[[4]], models_any_y79_glm_ns_posthoc[[3]])

# ** cross-level interactions ---------------------------------------------

x1a <- glmer(any_excl_y79 ~ exposure_highest_4_grp_y46 + I(per_ag_sw / 10) +
               (1 | la),
             data = model_data_non_special,
             family = binomial)
x1b <- glmer(any_excl_y79 ~ exposure_highest_4_grp_y46 * I(per_ag_sw / 10) +
               (1 | la),
             data = model_data_non_special,
             family = binomial,
             control = glmerControl(check.conv.grad = .makeCC("warning", tol = 3e-3, relTol = NULL))) # default 0.00278457 / nm 2 mil 0.00272927 / bobyqa 2 mil 0.01

x2a <- glmer(any_excl_y79 ~ exposure_highest_4_grp_y46 + I(per_sw_turnover / 10) +
               (1 | la),
             data = model_data_non_special,
             family = binomial,
             control = glmerControl(check.conv.grad = .makeCC("warning", tol = 2e-3, relTol = NULL))) # default 0.00103
x2b <- glmer(any_excl_y79 ~ exposure_highest_4_grp_y46 * I(per_sw_turnover / 10) +
               (1 | la),
             data = model_data_non_special,
             family = binomial,
             control = glmerControl(check.conv.grad = .makeCC("warning", tol = 2e-3, relTol = NULL))) # default 0.00195

x3a <- glmer(any_excl_y79 ~ exposure_highest_4_grp_y46 + I(sw_s251 / 100) +
               (1 | la),
             data = model_data_non_special,
             family = binomial,
             control = glmerControl(optimizer = "Nelder_Mead",
                                    check.conv.grad = .makeCC("warning", tol = 2e-3, relTol = NULL),
                                    optCtrl = list(maxfun = 2e6))) # default 0.00247
x3b <- glmer(any_excl_y79 ~ exposure_highest_4_grp_y46 * I(sw_s251 / 100) +
               (1 | la),
             data = model_data_non_special,
             family = binomial,
             control = glmerControl(check.conv.grad = .makeCC("warning", tol = 2e-3, relTol = NULL))) # default 0.001004

x4a <- glmer(any_excl_y79 ~ exposure_highest_4_grp_y46 + I(la_fsm_prop * 10) +
               (1 | la),
             data = model_data_non_special,
             family = binomial,
             control = glmerControl(check.conv.grad = .makeCC("warning", tol = 2e-3, relTol = NULL))) # default 0.00119
x4b <- glmer(any_excl_y79 ~ exposure_highest_4_grp_y46 * I(la_fsm_prop * 10) +
               (1 | la),
             data = model_data_non_special,
             family = binomial,
             control = glmerControl(optimizer = "Nelder_Mead",
                                    check.conv.grad = .makeCC("warning", tol = 3e-3, relTol = NULL),
                                    optCtrl = list(maxfun = 2e6))) # default 0.0032 / nm 2 mil 0.00216 / bobyqa 0.014

x5a <- glmer(any_excl_y79 ~ exposure_highest_4_grp_y46 + I(new_cin_prop * 100) +
               (1 | la),
             data = model_data_non_special,
             family = binomial)
x5b <- glmer(any_excl_y79 ~ exposure_highest_4_grp_y46 * I(new_cin_prop * 100) +
               (1 | la),
             data = model_data_non_special,
             family = binomial,
             control = glmerControl(check.conv.grad = .makeCC("warning", tol = 4e-3, relTol = NULL))) # default 0.0032 / nm 2 mil 0.0039 / bobyqa 2 mil 0.0265

gc()
x6a <- glmer(any_excl_y79 ~ exposure_highest_4_grp_y46 + I(la_special_schools_prop * 100) +
               (1 | la),
             data = model_data_non_special,
             family = binomial) 

gc()
x6b <- glmer(any_excl_y79 ~ exposure_highest_4_grp_y46 * I(la_special_schools_prop * 100) +
               (1 | la),
             data = model_data_non_special,
             family = binomial,
             control = glmerControl(check.conv.grad = .makeCC("warning", tol = 2e-3, relTol = NULL))) # def 0.00147499

models_any_excl_y79_x_int_glm_ns <- list()
models_any_excl_y79_x_int_glm_ns[[1]] <- x1a
models_any_excl_y79_x_int_glm_ns[[2]] <- x1b
models_any_excl_y79_x_int_glm_ns[[3]] <- x2a
models_any_excl_y79_x_int_glm_ns[[4]] <- x2b
models_any_excl_y79_x_int_glm_ns[[5]] <- x3a
models_any_excl_y79_x_int_glm_ns[[6]] <- x3b
models_any_excl_y79_x_int_glm_ns[[7]] <- x4a
models_any_excl_y79_x_int_glm_ns[[8]] <- x4b
models_any_excl_y79_x_int_glm_ns[[9]] <- x5a
models_any_excl_y79_x_int_glm_ns[[10]] <- x5b
models_any_excl_y79_x_int_glm_ns[[11]] <- x6a
models_any_excl_y79_x_int_glm_ns[[12]] <- x6b
save(models_any_excl_y79_x_int_glm_ns, file = "PROCESSED DATA/MODELS/MODELS_any_excl_y79_GLM_NOT_SPECIAL_XINT.RDA")
rm(x1a, x1b, x2a, x2b, x3a, x3b, x4a, x4b, x5a, x5b, x6a, x6b)

summary(models_any_excl_y79_x_int_glm_ns[[1]])
summary(models_any_excl_y79_x_int_glm_ns[[2]])
lrtest(models_any_excl_y79_x_int_glm_ns[[2]], models_any_excl_y79_x_int_glm_ns[[1]])

summary(models_any_excl_y79_x_int_glm_ns[[3]])
summary(models_any_excl_y79_x_int_glm_ns[[4]])
lrtest(models_any_excl_y79_x_int_glm_ns[[4]], models_any_excl_y79_x_int_glm_ns[[3]])

summary(models_any_excl_y79_x_int_glm_ns[[5]])
summary(models_any_excl_y79_x_int_glm_ns[[6]])
lrtest(models_any_excl_y79_x_int_glm_ns[[6]], models_any_excl_y79_x_int_glm_ns[[5]])

summary(models_any_excl_y79_x_int_glm_ns[[7]])
summary(models_any_excl_y79_x_int_glm_ns[[8]])
lrtest(models_any_excl_y79_x_int_glm_ns[[8]], models_any_excl_y79_x_int_glm_ns[[7]])

summary(models_any_excl_y79_x_int_glm_ns[[9]])
summary(models_any_excl_y79_x_int_glm_ns[[10]])
lrtest(models_any_excl_y79_x_int_glm_ns[[10]], models_any_excl_y79_x_int_glm_ns[[9]])

summary(models_any_excl_y79_x_int_glm_ns[[11]])
summary(models_any_excl_y79_x_int_glm_ns[[12]])
lrtest(models_any_excl_y79_x_int_glm_ns[[12]], models_any_excl_y79_x_int_glm_ns[[11]])

# ** cross-level with adjustments -----------------------------------------

gc()
x1b_adj <- glmer(any_excl_y79 ~ exposure_highest_4_grp_y46 * I(per_ag_sw / 10) +
                   ever_sen_primary + female_clean + eth_major + language_clean + idaci_quintiles + appru_y06 +
                   (1 | la),
                 data = model_data_non_special,
                 family = binomial,
                 control = glmerControl(optimizer = "Nelder_Mead",
                                        check.conv.grad = .makeCC("warning", tol = 2e-3, relTol = NULL),
                                        optCtrl = list(maxfun = 2e6))) 

gc()
x2b_adj <- glmer(any_excl_y79 ~ exposure_highest_4_grp_y46 * I(per_sw_turnover / 10) +
                   ever_sen_primary + female_clean + eth_major + language_clean + idaci_quintiles + appru_y06 +
                   (1 | la),
                 data = model_data_non_special,
                 family = binomial,
                 control = glmerControl(check.conv.grad = .makeCC("warning", tol = 3e-3, relTol = NULL),
                                        optCtrl = list(maxfun = 2e6))) # ftc nm 2 mil 0.00483102 / 2 mil def 0.0023463

gc()
x3b_adj <- glmer(any_excl_y79 ~ exposure_highest_4_grp_y46 * I(sw_s251 / 100) +
                   ever_sen_primary + female_clean + eth_major + language_clean + idaci_quintiles + appru_y06 +
                   (1 | la),
                 data = model_data_non_special,
                 family = binomial,
                 control = glmerControl(check.conv.grad = .makeCC("warning", tol = 3e-3, relTol = NULL))) # nm 2 mil 0.00328584, def 0.00257505

gc()
x4b_adj <- glmer(any_excl_y79 ~ exposure_highest_4_grp_y46 * I(la_fsm_prop * 10) +
                   ever_sen_primary + female_clean + eth_major + language_clean + idaci_quintiles + appru_y06 +
                   (1 | la),
                 data = model_data_non_special,
                 family = binomial,
                 control = glmerControl(check.conv.grad = .makeCC("warning", tol = 3e-3, relTol = NULL))) # nm 2 mil 0.00274039, def 0.00272186

gc()
x5b_adj <- glmer(any_excl_y79 ~ exposure_highest_4_grp_y46 * I(new_cin_prop * 100) +
                   ever_sen_primary + female_clean + eth_major + language_clean + idaci_quintiles + appru_y06 +
                   (1 | la),
                 data = model_data_non_special,
                 family = binomial,
                 control = glmerControl(check.conv.grad = .makeCC("warning", tol = 3e-3, relTol = NULL))) # nm 2 mil 0.00288268, def 0.00266946

gc()
x6b_adj <- glmer(any_excl_y79 ~ exposure_highest_4_grp_y46 * I(la_special_schools_prop * 100) +
                   ever_sen_primary + female_clean + eth_major + language_clean + idaci_quintiles + appru_y06 +
                   (1 | la),
                 data = model_data_non_special,
                 family = binomial,
                 control = glmerControl(check.conv.grad = .makeCC("warning", tol = 2e-3, relTol = NULL))) # def 0.00174494

models_any_excl_y79_x_int_adj_glm_ns <- list()
models_any_excl_y79_x_int_adj_glm_ns[[1]] <- x1b_adj
models_any_excl_y79_x_int_adj_glm_ns[[2]] <- x2b_adj
models_any_excl_y79_x_int_adj_glm_ns[[3]] <- x3b_adj
models_any_excl_y79_x_int_adj_glm_ns[[4]] <- x4b_adj
models_any_excl_y79_x_int_adj_glm_ns[[5]] <- x5b_adj
models_any_excl_y79_x_int_adj_glm_ns[[6]] <- x6b_adj
save(models_any_excl_y79_x_int_adj_glm_ns, file = "PROCESSED DATA/MODELS/MODELS_any_excl_y79_GLM_NOT_SPECIAL_XINT_ADJ.RDA")
rm(x1b_adj, x2b_adj, x3b_adj, x4b_adj, x5b_adj, x6b_adj)

summary(models_any_excl_y79_x_int_adj_glm_ns[[1]])
lrtest(models_any_excl_y79_x_int_adj_glm_ns[[1]], models_any_excl_y79_x_int_glm_ns[[2]])

summary(models_any_excl_y79_x_int_adj_glm_ns[[2]])
lrtest(models_any_excl_y79_x_int_adj_glm_ns[[2]], models_any_excl_y79_x_int_glm_ns[[4]])

summary(models_any_excl_y79_x_int_adj_glm_ns[[3]])
lrtest(models_any_excl_y79_x_int_adj_glm_ns[[3]], models_any_excl_y79_x_int_glm_ns[[6]])

summary(models_any_excl_y79_x_int_adj_glm_ns[[4]])
lrtest(models_any_excl_y79_x_int_adj_glm_ns[[4]], models_any_excl_y79_x_int_glm_ns[[8]])

summary(models_any_excl_y79_x_int_adj_glm_ns[[5]])
lrtest(models_any_excl_y79_x_int_adj_glm_ns[[5]], models_any_excl_y79_x_int_glm_ns[[10]])

summary(models_any_excl_y79_x_int_adj_glm_ns[[6]])
lrtest(models_any_excl_y79_x_int_adj_glm_ns[[6]], models_any_excl_y79_x_int_glm_ns[[12]])

# la adjustment
gc()
x1b_adj_la <- glmer(any_excl_y79 ~ exposure_highest_4_grp_y46 * I(per_ag_sw / 10) +
                      I(la_n / 10000) + I(la_fsm_prop * 10) +
                      (1 | la),
                    data = model_data_non_special,
                    family = binomial,
                    control = glmerControl(optimizer = "bobyqa")) # default 0.0027 / bobyqa 0.0154844 / nm 2 mil 0.0145

gc()
x2b_adj_la <- glmer(any_excl_y79 ~ exposure_highest_4_grp_y46 * I(per_sw_turnover / 10) +
                      I(la_n / 10000) + I(la_fsm_prop * 10) +
                      (1 | la),
                    data = model_data_non_special,
                    family = binomial,
                    control = glmerControl(optimizer = "bobyqa")) # default 0.0103 / bobyqa 0.0046983 / nm 2 mil 0.00165652

gc()
x3b_adj_la <- glmer(any_excl_y79 ~ exposure_highest_4_grp_y46 * I(sw_s251/100) +
                      I(la_n / 10000) + I(la_fsm_prop * 10) +
                      (1 | la),
                    data = model_data_non_special,
                    family = binomial,
                    control = glmerControl(optimizer = "bobyqa")) # default0.00663424 / bobyqa 0.0170387 / nm 2 mil 0.00752929

gc()
x4b_adj_la <- glmer(any_excl_y79 ~ exposure_highest_4_grp_y46 * I(la_fsm_prop * 10) +
                      I(la_n / 10000) +
                      (1 | la),
                    data = model_data_non_special,
                    family = binomial,
                    control = glmerControl(optimizer = "bobyqa")) # default 0.0109 / bobyqa 0.016757 / nm 2 mil 0.00749596

gc()
x5b_adj_la <- glmer(any_excl_y79 ~ exposure_highest_4_grp_y46 * I(new_cin_prop * 100) +
                      I(la_n / 10000) + I(la_fsm_prop * 10) +
                      (1 | la),
                    data = model_data_non_special,
                    family = binomial,
                    control = glmerControl(optimizer = "bobyqa")) # default 0.005 / bobyqa 0.00601649 / nm 2 mil 0.0075

gc()
x6b_adj_la <- glmer(any_excl_y79 ~ exposure_highest_4_grp_y46 * I(la_special_schools_prop * 100) +
                      I(la_n / 10000) + I(la_fsm_prop * 10) +
                      (1 | la),
                    data = model_data_non_special,
                    family = binomial,
                    control = glmerControl(optimizer = "bobyqa")) # default 0.00355 / bobyqa 0.0123449

models_any_excl_y79_x_int_adj_la_glm_ns <- list()
models_any_excl_y79_x_int_adj_la_glm_ns[[1]] <- x1b_adj_la
models_any_excl_y79_x_int_adj_la_glm_ns[[2]] <- x2b_adj_la
models_any_excl_y79_x_int_adj_la_glm_ns[[3]] <- x3b_adj_la
models_any_excl_y79_x_int_adj_la_glm_ns[[4]] <- x4b_adj_la
models_any_excl_y79_x_int_adj_la_glm_ns[[5]] <- x5b_adj_la
models_any_excl_y79_x_int_adj_la_glm_ns[[6]] <- x6b_adj_la

save(models_any_excl_y79_x_int_adj_la_glm_ns, file = "PROCESSED DATA/MODELS/MODELS_any_excl_y79_GLM_NOT_SPECIAL_XINT_ADJ_LA.RDA")

# nothing converged

rm(x1b_adj_la, x2b_adj_la, x3b_adj_la, x4b_adj_la, x5b_adj_la, x6b_adj_la,
   models_any_excl_y79_glm_ns, models_any_excl_y79_x_int_glm_ns,
   models_any_excl_y79_x_int_adj_glm_ns, models_any_excl_y79_x_int_adj_la_glm_ns)

# ** y1011 -----------------------------------------------------------------

# *** univariable models ---------------------------------------------------

gc()
um1 <- glmer(any_excl_y1011 ~ exposure_highest_4_grp_y49 +
               (1 | la),
             data = model_data_non_special,
             family = binomial,
             control = glmerControl(check.conv.grad = .makeCC("warning", tol = 2e-3, relTol = NULL)))

gc()
um2 <- glmer(any_excl_y1011 ~ highest_ever_sen_y09 +
               (1 | la),
             data = model_data_non_special,
             family = binomial)

gc()
um3 <- glmer(any_excl_y1011 ~ ever_sen_y09 +
               (1 | la),
             data = model_data_non_special,
             family = binomial)

gc()
um4 <- glmer(any_excl_y1011 ~ female_clean +
               (1 | la),
             data = model_data_non_special,
             family = binomial)

gc()
um5 <- glmer(any_excl_y1011 ~ eth_major +
               (1 | la),
             data = model_data_non_special,
             family = binomial) 

gc()
um6 <- glmer(any_excl_y1011 ~ language_clean +
               (1 | la),
             data = model_data_non_special,
             family = binomial)

gc()
um7 <- glmer(any_excl_y1011 ~ idaci_fsm +
               (1 | la),
             data = model_data_non_special,
             family = binomial,
             control = glmerControl(check.conv.grad = .makeCC("warning", tol = 2e-3, relTol = NULL))) # 0.0015

gc()
um8 <- glmer(any_excl_y1011 ~ appru_y09 +
               (1 | la),
             data = model_data_non_special,
             family = binomial) 

gc()
um9 <- glmer(any_excl_y1011 ~ idaci_quintiles +
               (1 | la),
             data = model_data_non_special,
             family = binomial,
             control = glmerControl(check.conv.grad = .makeCC("warning", tol = 2e-3, relTol = NULL))) # 0.00175

gc()
um10 <- glmer(any_excl_y1011 ~ fsmeligible +
                (1 | la),
              data = model_data_non_special,
              family = binomial)

gc()
um11 <- glmer(any_excl_y1011 ~ I(per_ag_sw / 10) +
                (1 | la),
              data = model_data_non_special,
              family = binomial) 

gc()
um12 <- glmer(any_excl_y1011 ~ I(per_sw_turnover / 10) +
                (1 | la),
              data = model_data_non_special,
              family = binomial,
              control = glmerControl(optimizer = "Nelder_Mead",
                                     check.conv.grad = .makeCC("warning", tol = 2e-3, relTol = NULL),
                                     optCtrl = list(maxfun = 2e6))) # 0.0025 / nm 2 mil converged

gc()
um13 <- glmer(any_excl_y1011 ~ I(la_n / 10000) +
                (1 | la),
              data = model_data_non_special,
              family = binomial,
              control = glmerControl(check.conv.grad = .makeCC("warning", tol = 2e-3, relTol = NULL))) # def 0.0014

gc()
um14 <- glmer(any_excl_y1011 ~ I(la_fsm_prop * 10) +
                (1 | la),
              data = model_data_non_special,
              family = binomial,
              control = glmerControl(check.conv.grad = .makeCC("warning", tol = 0.0025, relTol = NULL))) # 0.0023 / nm 2 mil 0.00377 / bobyqa 2 mil 0.0029

gc()
um15 <- glmer(any_excl_y1011 ~ I(open_cin_prop * 100) +
                (1 | la),
              data = model_data_non_special,
              family = binomial,
              control = glmerControl(optimizer = "Nelder_Mead",
                                     check.conv.grad = .makeCC("warning", tol = 0.0035, relTol = NULL),
                                     optCtrl = list(maxfun = 2e6))) # 0.0043 / nm 2 mil 0.00347 / bobyqa 2 mil 0.012

gc()
um16 <- glmer(any_excl_y1011 ~ I(new_cin_prop * 100) +
                (1 | la),
              data = model_data_non_special,
              family = binomial)

gc()
um17 <- glmer(any_excl_y1011 ~ I(sw_s251 / 100) +
                (1 | la),
              data = model_data_non_special,
              family = binomiald,
              control = glmerControl(check.conv.grad = .makeCC("warning", tol = 2e-3, relTol = NULL))) # def 0.0011

gc()
um18 <- glmer(any_excl_y1011 ~ I(la_special_schools_prop * 100) +
                (1 | la) + (1 | region_major),
              data = model_data_non_special,
              family = binomial,
              control = glmerControl(check.conv.grad = .makeCC("warning", tol = 2e-3, relTol = NULL)))

gc()
models_any_y1011_univar_glm_ns <- list()
models_any_y1011_univar_glm_ns[[1]] <- um1
models_any_y1011_univar_glm_ns[[2]] <- um2
models_any_y1011_univar_glm_ns[[3]] <- um3
models_any_y1011_univar_glm_ns[[4]] <- um4
models_any_y1011_univar_glm_ns[[5]] <- um5
models_any_y1011_univar_glm_ns[[6]] <- um6
models_any_y1011_univar_glm_ns[[7]] <- um7
models_any_y1011_univar_glm_ns[[8]] <- um8
models_any_y1011_univar_glm_ns[[9]] <- um9
models_any_y1011_univar_glm_ns[[10]] <- um10
models_any_y1011_univar_glm_ns[[11]] <- um11
models_any_y1011_univar_glm_ns[[12]] <- um12
models_any_y1011_univar_glm_ns[[13]] <- um13
models_any_y1011_univar_glm_ns[[14]] <- um14
models_any_y1011_univar_glm_ns[[15]] <- um15
models_any_y1011_univar_glm_ns[[16]] <- um16
models_any_y1011_univar_glm_ns[[17]] <- um17
models_any_y1011_univar_glm_ns[[18]] <- um18

save(models_any_y1011_univar_glm_ns, file = "PROCESSED DATA/MODELS/MODELS_EXCLANY_y1011_UNIVAR_GLM_NOT_SPECIAL.RDA")
rm(um2, um3, um4, um5, um6, um7, um8, um9, um10, um11, um12, um13, um14, um15, um16, um17, um18)

summary(models_any_y1011_univar_glm_ns[[1]])
summary(models_any_y1011_univar_glm_ns[[2]])
summary(models_any_y1011_univar_glm_ns[[3]])
summary(models_any_y1011_univar_glm_ns[[4]])
summary(models_any_y1011_univar_glm_ns[[5]])
summary(models_any_y1011_univar_glm_ns[[6]])
summary(models_any_y1011_univar_glm_ns[[7]])
summary(models_any_y1011_univar_glm_ns[[8]])
summary(models_any_y1011_univar_glm_ns[[9]])
summary(models_any_y1011_univar_glm_ns[[10]])
summary(models_any_y1011_univar_glm_ns[[11]])
summary(models_any_y1011_univar_glm_ns[[12]])
summary(models_any_y1011_univar_glm_ns[[13]])
summary(models_any_y1011_univar_glm_ns[[14]])
summary(models_any_y1011_univar_glm_ns[[15]])
summary(models_any_y1011_univar_glm_ns[[16]])
summary(models_any_y1011_univar_glm_ns[[17]])
summary(models_any_y1011_univar_glm_ns[[18]])

# *** main models ---------------------------------------------------------

gc()
hm1 <- glmer(any_excl_y1011 ~ 1 +
               (1 | la),
             data = model_data_non_special,
             family = binomial,
             control = glmerControl(check.conv.grad = .makeCC("warning", tol = 2e-3, relTol = NULL)))

gc()
hm3 <- glmer(any_excl_y1011 ~ 1 + exposure_highest_4_grp_y49 + ever_sen_y09 +
               (1 | la),
             data = model_data_non_special,
             family = binomial,
             control = glmerControl(check.conv.grad = .makeCC("warning", tol = 2e-3, relTol = NULL)))

gc()
hm4 <- glmer(any_excl_y1011 ~ 1 + exposure_highest_4_grp_y49 * ever_sen_y09 +
               (1 | la),
             data = model_data_non_special,
             family = binomial,
             control = glmerControl(check.conv.grad = .makeCC("warning", tol = 2e-3, relTol = NULL)))

gc()
hm5 <- glmer(any_excl_y1011 ~ 1 + exposure_highest_4_grp_y49 * ever_sen_y09 +
               female_clean + eth_major + language_clean + idaci_quintiles + appru_y09 +
               (1 | la),
             data = model_data_non_special,
             family = binomial,
             control = glmerControl(#optimizer = "Nelder_Mead",
                                    check.conv.grad = .makeCC("warning", tol = 2e-3, relTol = NULL)#,
                                    #optCtrl = list(maxfun = 2e6)
                                    )) # 

gc()
hm6 <- glmer(any_excl_y1011 ~ 1 + exposure_highest_4_grp_y49 * ever_sen_y09 +
               female_clean + eth_major + language_clean + idaci_quintiles + appru_y09 +
               I(per_ag_sw / 10) + I(per_sw_turnover / 10) + I(sw_s251 / 100) + I(la_n / 10000) + I(la_fsm_prop * 10) +
               I(new_cin_prop * 100) + I(la_special_schools_prop * 100) +
               (1 | la),
             data = model_data_non_special,
             family = binomial) # ftc 0.00643767

gc()
hm6a <- glmer(any_excl_y1011 ~ 1 + exposure_highest_4_grp_y49 * ever_sen_y09 +
               female_clean + eth_major + language_clean + idaci_quintiles + appru_y09 +
               I(la_n / 10000) + I(la_fsm_prop * 10) + I(new_cin_prop * 100) +
               (1 | la),
             data = model_data_non_special,
             family = binomial,
             control = glmerControl(check.conv.grad = .makeCC("warning", tol = 5e-3, relTol = NULL))
                                    ) # def 0.00476141 / nm 0.0101619 / def 2 mil 0.00475141 / nm 2 mil 0.0101619

models_any_y1011_glm_ns <- list()
models_any_y1011_glm_ns[[1]] <- hm1
models_any_y1011_glm_ns[[2]] <- um1
models_any_y1011_glm_ns[[3]] <- hm3
models_any_y1011_glm_ns[[4]] <- hm4
models_any_y1011_glm_ns[[5]] <- hm5
models_any_y1011_glm_ns[[6]] <- hm6a
save(models_any_y1011_glm_ns, file = "PROCESSED DATA/MODELS/MODELS_EXCLANY_y1011_GLM_NOT_SPECIAL.RDA")
rm(hm1, um1, hm3, hm4, hm5, hm6, hm6a)

summary(models_any_y1011_glm_ns[[1]])
summary(models_any_y1011_glm_ns[[2]])
lrtest(models_any_y1011_glm_ns[[2]], models_any_y1011_glm_ns[[1]])

summary(models_any_y1011_glm_ns[[3]])
lrtest(models_any_y1011_glm_ns[[3]], models_any_y1011_glm_ns[[2]])

summary(models_any_y1011_glm_ns[[4]])
lrtest(models_any_y1011_glm_ns[[4]], models_any_y1011_glm_ns[[3]])

summary(models_any_y1011_glm_ns[[5]])
lrtest(models_any_y1011_glm_ns[[5]], models_any_y1011_glm_ns[[4]])

summary(models_any_y1011_glm_ns[[6]])
lrtest(models_any_y1011_glm_ns[[6]], models_any_y1011_glm_ns[[5]])

# ** post hoc -------------------------------------------------------------

gc()
hm6_all_la_vars <- glmer(any_excl_y1011 ~ 1 + exposure_highest_4_grp_y49 +
               I(per_ag_sw / 10) + I(per_sw_turnover / 10) + I(sw_s251 / 100) + I(la_n / 10000) + I(la_fsm_prop * 10) +
               I(new_cin_prop * 100) + I(la_special_schools_prop * 100) +
               (1 | la),
             data = model_data_non_special,
             family = binomial,
             control = glmerControl(check.conv.grad = .makeCC("warning", tol = 2e-3, relTol = NULL)))


gc()
hm_posthoc_random_slopes_1 <- glmer(any_excl_y1011 ~ exposure_highest_4_grp_y49 +
                                      (exposure_highest_4_grp_y49 | la),
                                    data = model_data_non_special,
                                    family = binomial,
                                    control = glmerControl(optimizer = "Nelder_Mead")) # def ftc 0.00405395 / NM 4.44625

model_data_non_special[, any_csc := exposure_highest_4_grp_y49 != "None"]

gc()
hm_posthoc_random_slopes_2 <- glmer(any_excl_y1011 ~ any_csc + 
                                      (1 | la),
                                    data = model_data_non_special,
                                    family = binomial,
                                    control = glmerControl(optimizer = "Nelder_Mead")) # default ftc 0.00223902 / NM converged

gc()
hm_posthoc_random_slopes_3 <- glmer(any_excl_y1011 ~ any_csc + 
                                      (any_csc | la),
                                    data = model_data_non_special,
                                    family = binomial) # def converged

gc()
hm_posthoc_random_slopes_4 <- glmer(any_excl_y1011 ~ any_csc + 
                                      ever_sen_y09 + female_clean + eth_major + language_clean + idaci_quintiles + appru_y09 +
                                      (any_csc | la),
                                    data = model_data_non_special,
                                    family = binomial,
                                    control = glmerControl(check.conv.grad = .makeCC("warning", tol = 2e-3, relTol = NULL))) # def 0.00144498

gc()
hm_posthoc_random_slopes_5 <- glmer(any_excl_y1011 ~ any_csc + 
                                      ever_sen_y09 + female_clean + eth_major + language_clean + idaci_quintiles + appru_y09 +
                                      I(la_n / 10000) + I(la_fsm_prop * 10) + I(new_cin_prop * 100) +
                                      (any_csc | la),
                                    data = model_data_non_special,
                                    family = binomial,
                                    control = glmerControl(optimizer = "Nelder_Mead")) # def 0.00681174 / nm 0.0105602

models_any_y1011_glm_ns_posthoc <- list()
models_any_y1011_glm_ns_posthoc[[1]] <- hm6_all_la_vars
models_any_y1011_glm_ns_posthoc[[2]] <- hm_posthoc_random_slopes_1
models_any_y1011_glm_ns_posthoc[[3]] <- hm_posthoc_random_slopes_2
models_any_y1011_glm_ns_posthoc[[4]] <- hm_posthoc_random_slopes_3
models_any_y1011_glm_ns_posthoc[[5]] <- hm_posthoc_random_slopes_4
models_any_y1011_glm_ns_posthoc[[6]] <- hm_posthoc_random_slopes_5
save(models_any_y1011_glm_ns_posthoc, file = "PROCESSED DATA/MODELS/MODELS_EXCLANY_y1011_GLM_NOT_SPECIAL_POSTHOC.RDA")
rm(hm6_all_la_vars, hm_posthoc_random_slopes_1, hm_posthoc_random_slopes_2, hm_posthoc_random_slopes_3,
   hm_posthoc_random_slopes_4, hm_posthoc_random_slopes_5)

summary(models_any_y1011_glm_ns_posthoc[[1]])

summary(models_any_y1011_glm_ns_posthoc[[3]])
summary(models_any_y1011_glm_ns_posthoc[[4]])
lrtest(models_any_y1011_glm_ns_posthoc[[4]], models_any_y1011_glm_ns_posthoc[[3]])

summary(models_any_y1011_glm_ns_posthoc[[5]])
lrtest(models_any_y1011_glm_ns_posthoc[[5]], models_any_y1011_glm_ns_posthoc[[4]])

# ** cross-level interactions ---------------------------------------------

x1a <- glmer(any_excl_y1011 ~ exposure_highest_4_grp_y49 + I(per_ag_sw / 10) +
               (1 | la),
             data = model_data_non_special,
             family = binomial)
x1b <- glmer(any_excl_y1011 ~ exposure_highest_4_grp_y49 * I(per_ag_sw / 10) +
               (1 | la),
             data = model_data_non_special,
             family = binomial,
             control = glmerControl(optimizer = "Nelder_Mead",
                                    check.conv.grad = .makeCC("warning", tol = 2e-3, relTol = NULL))) # def 0.00770352 / nm conv

x2a <- glmer(any_excl_y1011 ~ exposure_highest_4_grp_y49 + I(per_sw_turnover / 10) +
               (1 | la),
             data = model_data_non_special,
             family = binomial,
             control = glmerControl(check.conv.grad = .makeCC("warning", tol = 2e-3, relTol = NULL))) # def 0.00117108
x2b <- glmer(any_excl_y1011 ~ exposure_highest_4_grp_y49 * I(per_sw_turnover / 10) +
               (1 | la),
             data = model_data_non_special,
             family = binomial,
             control = glmerControl(optimizer = "Nelder_Mead",
                                    check.conv.grad = .makeCC("warning", tol = 3e-3, relTol = NULL))) #def 0.00362935 / nm 0.00211

x3a <- glmer(any_excl_y1011 ~ exposure_highest_4_grp_y49 + I(sw_s251 / 100) +
               (1 | la),
             data = model_data_non_special,
             family = binomial)
x3b <- glmer(any_excl_y1011 ~ exposure_highest_4_grp_y49 * I(sw_s251 / 100) +
               (1 | la),
             data = model_data_non_special,
             family = binomial,
             control = glmerControl(optimizer = "Nelder_Mead",
                                    check.conv.grad = .makeCC("warning", tol = 3e-3, relTol = NULL))) # def 0.00244536 / nm 0.00268859

x4a <- glmer(any_excl_y1011 ~ exposure_highest_4_grp_y49 + I(la_fsm_prop * 10) +
               (1 | la),
             data = model_data_non_special,
             family = binomial)
x4b <- glmer(any_excl_y1011 ~ exposure_highest_4_grp_y49 * I(la_fsm_prop * 10) +
               (1 | la),
             data = model_data_non_special,
             family = binomial,
             control = glmerControl(check.conv.grad = .makeCC("warning", tol = 4e-3, relTol = NULL))) # def 0.00331798 / nm 0.00331798 / bobyqa 0.0144122

x5a <- glmer(any_excl_y1011 ~ exposure_highest_4_grp_y49 + I(new_cin_prop * 100) +
               (1 | la),
             data = model_data_non_special,
             family = binomial)
x5b <- glmer(any_excl_y1011 ~ exposure_highest_4_grp_y49 * I(new_cin_prop * 100) +
               (1 | la),
             data = model_data_non_special,
             family = binomial,
             control = glmerControl(optimizer = "Nelder_Mead",
                                    check.conv.grad = .makeCC("warning", tol = 2e-3, relTol = NULL))) # def 0.00907788 / nm conv

x6a <- glmer(any_excl_y1011 ~ exposure_highest_4_grp_y49 + I(la_special_schools_prop * 100) +
               (1 | la),
             data = model_data_non_special,
             family = binomial,
             control = glmerControl(check.conv.grad = .makeCC("warning", tol = 2e-3, relTol = NULL))) # def 0.00134342
x6b <- glmer(any_excl_y1011 ~ exposure_highest_4_grp_y49 * I(la_special_schools_prop * 100) +
               (1 | la),
             data = model_data_non_special,
             family = binomial,
             control = glmerControl(optimizer = "Nelder_Mead",
                                    check.conv.grad = .makeCC("warning", tol = 3e-3, relTol = NULL))) # def 0.0030893 / nm 0.00248

models_any_excl_y1011_x_int_glm_ns <- list()
models_any_excl_y1011_x_int_glm_ns[[1]] <- x1a
models_any_excl_y1011_x_int_glm_ns[[2]] <- x1b
models_any_excl_y1011_x_int_glm_ns[[3]] <- x2a
models_any_excl_y1011_x_int_glm_ns[[4]] <- x2b
models_any_excl_y1011_x_int_glm_ns[[5]] <- x3a
models_any_excl_y1011_x_int_glm_ns[[6]] <- x3b
models_any_excl_y1011_x_int_glm_ns[[7]] <- x4a
models_any_excl_y1011_x_int_glm_ns[[8]] <- x4b
models_any_excl_y1011_x_int_glm_ns[[9]] <- x5a
models_any_excl_y1011_x_int_glm_ns[[10]] <- x5b
models_any_excl_y1011_x_int_glm_ns[[11]] <- x6a
models_any_excl_y1011_x_int_glm_ns[[12]] <- x6b
save(models_any_excl_y1011_x_int_glm_ns, file = "PROCESSED DATA/MODELS/MODELS_any_excl_y1011_GLM_NOT_SPECIAL_XINT.RDA")
rm(x1a, x1b, x2a, x2b, x3a, x3b, x4a, x4b, x5a, x5b, x6a, x6b)

summary(models_any_excl_y1011_x_int_glm_ns[[1]])
summary(models_any_excl_y1011_x_int_glm_ns[[2]])
lrtest(models_any_excl_y1011_x_int_glm_ns[[2]], models_any_excl_y1011_x_int_glm_ns[[1]])

summary(models_any_excl_y1011_x_int_glm_ns[[3]])
summary(models_any_excl_y1011_x_int_glm_ns[[4]])
lrtest(models_any_excl_y1011_x_int_glm_ns[[4]], models_any_excl_y1011_x_int_glm_ns[[3]])

summary(models_any_excl_y1011_x_int_glm_ns[[5]])
summary(models_any_excl_y1011_x_int_glm_ns[[6]])
lrtest(models_any_excl_y1011_x_int_glm_ns[[6]], models_any_excl_y1011_x_int_glm_ns[[5]])

summary(models_any_excl_y1011_x_int_glm_ns[[7]])
summary(models_any_excl_y1011_x_int_glm_ns[[8]])
lrtest(models_any_excl_y1011_x_int_glm_ns[[8]], models_any_excl_y1011_x_int_glm_ns[[7]])

summary(models_any_excl_y1011_x_int_glm_ns[[9]])
summary(models_any_excl_y1011_x_int_glm_ns[[10]])
lrtest(models_any_excl_y1011_x_int_glm_ns[[10]], models_any_excl_y1011_x_int_glm_ns[[9]])

summary(models_any_excl_y1011_x_int_glm_ns[[11]])
summary(models_any_excl_y1011_x_int_glm_ns[[12]])
lrtest(models_any_excl_y1011_x_int_glm_ns[[12]], models_any_excl_y1011_x_int_glm_ns[[11]])

# ** cross-level with adjustments -----------------------------------------

gc()
x1b_adj <- glmer(any_excl_y1011 ~ exposure_highest_4_grp_y49 * I(per_ag_sw / 10) +
                   ever_sen_y09 + female_clean + eth_major + language_clean + idaci_quintiles + appru_y09 +
                   (1 | la),
                 data = model_data_non_special,
                 family = binomial,
                 control = glmerControl(optimizer = "Nelder_Mead",
                                        check.conv.grad = .makeCC("warning", tol = 2e-3, relTol = NULL))) # def 0.00219239 / nm conv

gc()
x2b_adj <- glmer(any_excl_y1011 ~ exposure_highest_4_grp_y49 * I(per_sw_turnover / 10) +
                   ever_sen_y09 + female_clean + eth_major + language_clean + idaci_quintiles + appru_y09 +
                   (1 | la),
                 data = model_data_non_special,
                 family = binomial,
                 control = glmerControl(check.conv.grad = .makeCC("warning", tol = 3e-3, relTol = NULL))) # def 0.00244709 / nm 0.00255132 / def 1 mil 0.00244709 / therefore conv on 0.003 tol

gc()
x3b_adj <- glmer(any_excl_y1011 ~ exposure_highest_4_grp_y49 * I(sw_s251 / 100) +
                   ever_sen_y09 + female_clean + eth_major + language_clean + idaci_quintiles + appru_y09 +
                   (1 | la),
                 data = model_data_non_special,
                 family = binomial,
                 control = glmerControl(optimizer = "Nelder_Mead",
                                        check.conv.grad = .makeCC("warning", tol = 2e-3, relTol = NULL))) # def 0.003084 / nm conv

gc()
x4b_adj <- glmer(any_excl_y1011 ~ exposure_highest_4_grp_y49 * I(la_fsm_prop * 10) +
                   ever_sen_y09 + female_clean + eth_major + language_clean + idaci_quintiles + appru_y09 +
                   (1 | la),
                 data = model_data_non_special,
                 family = binomial,
                 control = glmerControl(check.conv.grad = .makeCC("warning", tol = 2e-3, relTol = NULL))) # 0.00148924 / conv

x5b_adj <- glmer(any_excl_y1011 ~ exposure_highest_4_grp_y49 * I(new_cin_prop * 100) +
                   ever_sen_y09 + female_clean + eth_major + language_clean + idaci_quintiles + appru_y09 +
                   (1 | la),
                 data = model_data_non_special,
                 family = binomial,
                 control = glmerControl(optimizer = "Nelder_Mead",
                                        check.conv.grad = .makeCC("warning", tol = 2e-3, relTol = NULL))) # def 0.00352738 / nm conv 

gc()
x6b_adj <- glmer(any_excl_y1011 ~ exposure_highest_4_grp_y49 * I(la_special_schools_prop * 100) +
                   ever_sen_y09 + female_clean + eth_major + language_clean + idaci_quintiles + appru_y09 +
                   (1 | la),
                 data = model_data_non_special,
                 family = binomial,
                 control = glmerControl(optimizer = "Nelder_Mead",
                                        check.conv.grad = .makeCC("warning", tol = 3e-3, relTol = NULL))) # def 0.00349373 / nm 0.0027497 / default 1 mil 0.00349373 / therfore conv nm 0.003 tol

models_any_excl_y1011_x_int_adj_glm_ns <- list()
models_any_excl_y1011_x_int_adj_glm_ns[[1]] <- x1b_adj
models_any_excl_y1011_x_int_adj_glm_ns[[2]] <- x2b_adj
models_any_excl_y1011_x_int_adj_glm_ns[[3]] <- x3b_adj
models_any_excl_y1011_x_int_adj_glm_ns[[4]] <- x4b_adj
models_any_excl_y1011_x_int_adj_glm_ns[[5]] <- x5b_adj
models_any_excl_y1011_x_int_adj_glm_ns[[6]] <- x6b_adj
save(models_any_excl_y1011_x_int_adj_glm_ns, file = "PROCESSED DATA/MODELS/MODELS_any_excl_y1011_GLM_NOT_SPECIAL_XINT_ADJ.RDA")
rm(x1b_adj, x2b_adj, x3b_adj, x4b_adj, x5b_adj, x6b_adj)

summary(models_any_excl_y1011_x_int_adj_glm_ns[[1]])
lrtest(models_any_excl_y1011_x_int_adj_glm_ns[[1]], models_any_excl_y1011_x_int_glm_ns[[2]])

summary(models_any_excl_y1011_x_int_adj_glm_ns[[2]])
lrtest(models_any_excl_y1011_x_int_adj_glm_ns[[2]], models_any_excl_y1011_x_int_glm_ns[[4]])

summary(models_any_excl_y1011_x_int_adj_glm_ns[[3]])
lrtest(models_any_excl_y1011_x_int_adj_glm_ns[[3]], models_any_excl_y1011_x_int_glm_ns[[6]])

summary(models_any_excl_y1011_x_int_adj_glm_ns[[4]])
lrtest(models_any_excl_y1011_x_int_adj_glm_ns[[4]], models_any_excl_y1011_x_int_glm_ns[[8]])

summary(models_any_excl_y1011_x_int_adj_glm_ns[[5]])
lrtest(models_any_excl_y1011_x_int_adj_glm_ns[[5]], models_any_excl_y1011_x_int_glm_ns[[10]])

summary(models_any_excl_y1011_x_int_adj_glm_ns[[6]])
lrtest(models_any_excl_y1011_x_int_adj_glm_ns[[6]], models_any_excl_y1011_x_int_glm_ns[[12]])

# la adjustment
gc()
x1b_adj_la <- glmer(any_excl_y1011 ~ exposure_highest_4_grp_y49 * I(per_ag_sw / 10) +
                      I(la_n / 10000) + I(la_fsm_prop * 10) + I(new_cin_prop * 100) +
                      (1 | la),
                    data = model_data_non_special,
                    family = binomial,
                    control = glmerControl(optimizer = "bobyqa")) # def 0.0110846 / nm 0.0135962 / bobyqa 0.0062101

gc()
x2b_adj_la <- glmer(any_excl_y1011 ~ exposure_highest_4_grp_y49 * I(per_sw_turnover / 10) +
                      I(la_n / 10000) + I(la_fsm_prop * 10) + I(new_cin_prop * 100) +
                      (1 | la),
                    data = model_data_non_special,
                    family = binomial,
                    control = glmerControl(optimizer = "bobyqa")) # def 0.00720323 / nm 0.00353934 / bobyqa 0.0202939

gc()
x3b_adj_la <- glmer(any_excl_y1011 ~ exposure_highest_4_grp_y49 * I(sw_s251/100) +
                      I(la_n / 10000) + I(la_fsm_prop * 10) + I(new_cin_prop * 100) +
                      (1 | la),
                    data = model_data_non_special,
                    family = binomial,
                    control = glmerControl(optimizer = "bobyqa")) # def 0.00339581 / nm 0.00537219 / bobyqa 0.0306663

gc()
x4b_adj_la <- glmer(any_excl_y1011 ~ exposure_highest_4_grp_y49 * I(la_fsm_prop * 10) +
                      I(la_n / 10000) + I(new_cin_prop * 100) +
                      (1 | la),
                    data = model_data_non_special,
                    family = binomial,
                    control = glmerControl(optimizer = "bobyqa")) # def 0.0106997 / nm 0.00436324 / bobyqa 0.00968101

gc()
x5b_adj_la <- glmer(any_excl_y1011 ~ exposure_highest_4_grp_y49 * I(new_cin_prop * 100) +
                      I(la_n / 10000) + I(la_fsm_prop * 10) +
                      (1 | la),
                    data = model_data_non_special,
                    family = binomial,
                    control = glmerControl(optimizer = "bobyqa")) # def 0.00606086 / nm 0.00924528 / bobyqa 0.00938807

gc()
x6b_adj_la <- glmer(any_excl_y1011 ~ exposure_highest_4_grp_y49 * I(la_special_schools_prop * 100) +
                      I(la_n / 10000) + I(la_fsm_prop * 10) + I(new_cin_prop * 100) +
                      (1 | la),
                    data = model_data_non_special,
                    family = binomial,
                    control = glmerControl(optimizer = "bobyqa")) # def 0.0213992 / nm 0.00549157 / bobyqa 0.0118587

models_any_excl_y1011_x_int_adj_la_glm_ns <- list()
models_any_excl_y1011_x_int_adj_la_glm_ns[[1]] <- x1b_adj_la
models_any_excl_y1011_x_int_adj_la_glm_ns[[2]] <- x2b_adj_la
models_any_excl_y1011_x_int_adj_la_glm_ns[[3]] <- x3b_adj_la
models_any_excl_y1011_x_int_adj_la_glm_ns[[4]] <- x4b_adj_la
models_any_excl_y1011_x_int_adj_la_glm_ns[[5]] <- x5b_adj_la
models_any_excl_y1011_x_int_adj_la_glm_ns[[6]] <- x6b_adj_la

# nothing converged

save(models_any_excl_y1011_x_int_adj_la_glm_ns, file = "PROCESSED DATA/MODELS/MODELS_any_excl_y1011_GLM_NOT_SPECIAL_XINT_ADJ_LA.RDA")
rm(x1b_adj_la, x2b_adj_la, x3b_adj_la, x4b_adj_la, x5b_adj_la, x6b_adj_la,
   models_any_excl_y1011_glm_ns, models_any_excl_y1011_x_int_glm_ns,
   models_any_excl_y1011_x_int_adj_glm_ns, models_any_excl_y1011_x_int_adj_la_glm_ns)

# MODELLING - special --------------------------------------------------------------

# * describe size of units ---------------------------------------------------------------

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

# * estimate models -------------------------------------------------------

# ** 711 --------------------------------------------------------------------

# *** univariable models ---------------------------------------------------

gc()
um1 <- glmer(any_excl_y711 ~ exposure_highest_4_grp_y46 +
               (1 | la),
             data = model_data_special,
             family = binomial)

gc()
um2 <- glmer(any_excl_y711 ~ female_clean +
               (1 | la),
             data = model_data_special,
             family = binomial)

gc()
um3 <- glmer(any_excl_y711 ~ eth_major +
               (1 | la),
             data = model_data_special,
             family = binomial)

gc()
um4 <- glmer(any_excl_y711 ~ language_clean +
               (1 | la),
             data = model_data_special,
             family = binomial)

gc()
um5 <- glmer(any_excl_y711 ~ appru_y06 +
               (1 | la),
             data = model_data_special,
             family = binomial)

gc()
um6 <- glmer(any_excl_y711 ~ fsmeligible +
                (1 | la),
              data = model_data_special,
              family = binomial)

gc()
um7 <- glmer(any_excl_y711 ~ I(per_ag_sw / 10) +
                (1 | la),
              data = model_data_special,
              family = binomial)

gc()
um8 <- glmer(any_excl_y711 ~ I(per_sw_turnover / 10) +
                (1 | la),
              data = model_data_special,
              family = binomial)

gc()
um9 <- glmer(any_excl_y711 ~ I(la_n / 10000) +
                (1 | la),
              data = model_data_special,
              family = binomial)

gc()
um10 <- glmer(any_excl_y711 ~ I(la_fsm_prop * 10) +
                (1 | la),
              data = model_data_special,
              family = binomial)

gc()
um11 <- glmer(any_excl_y711 ~ I(open_cin_prop * 100) +
                (1 | la),
              data = model_data_special,
              family = binomial)

gc()
um12 <- glmer(any_excl_y711 ~ I(new_cin_prop * 100) +
                (1 | la),
              data = model_data_special,
              family = binomial)

gc()
um13 <- glmer(any_excl_y711 ~ I(sw_s251 / 100) +
                (1 | la),
              data = model_data_special,
              family = binomial)

gc()
um14 <- glmer(any_excl_y711 ~ I(la_special_schools_prop * 100) +
                (1 | la) + (1 | region_major),
              data = model_data_special,
              family = binomial)

gc()
models_any_y711_univar_glm_s <- list()
models_any_y711_univar_glm_s[[1]] <- um1
models_any_y711_univar_glm_s[[2]] <- um2
models_any_y711_univar_glm_s[[3]] <- um3
models_any_y711_univar_glm_s[[4]] <- um4
models_any_y711_univar_glm_s[[5]] <- um5
models_any_y711_univar_glm_s[[6]] <- um6
models_any_y711_univar_glm_s[[7]] <- um7
models_any_y711_univar_glm_s[[8]] <- um8
models_any_y711_univar_glm_s[[9]] <- um9
models_any_y711_univar_glm_s[[10]] <- um10
models_any_y711_univar_glm_s[[11]] <- um11
models_any_y711_univar_glm_s[[12]] <- um12
models_any_y711_univar_glm_s[[13]] <- um13
models_any_y711_univar_glm_s[[14]] <- um14

save(models_any_y711_univar_glm_s, file = "PROCESSED DATA/MODELS/MODELS_EXCLANY_y711_UNIVAR_GLM_SPECIAL.RDA")
rm(um2, um3, um4, um5, um6, um7, um8, um9, um10, um11, um12, um13, um14)

summary(models_any_y711_univar_glm_s[[1]])
summary(models_any_y711_univar_glm_s[[2]])
summary(models_any_y711_univar_glm_s[[3]])
summary(models_any_y711_univar_glm_s[[4]])
summary(models_any_y711_univar_glm_s[[5]])
summary(models_any_y711_univar_glm_s[[6]])
summary(models_any_y711_univar_glm_s[[7]])
summary(models_any_y711_univar_glm_s[[8]])
summary(models_any_y711_univar_glm_s[[9]])
summary(models_any_y711_univar_glm_s[[10]])
summary(models_any_y711_univar_glm_s[[11]])
summary(models_any_y711_univar_glm_s[[12]])
summary(models_any_y711_univar_glm_s[[13]])
summary(models_any_y711_univar_glm_s[[14]])

# *** main models ---------------------------------------------------------

gc()
hm1 <- glmer(any_excl_y711 ~ 1 +
               (1 | la),
             data = model_data_special,
             family = binomial)

gc()
hm3 <- glmer(any_excl_y711 ~ 1 + exposure_highest_4_grp_y46 +
               female_clean + eth_major + language_clean + fsmeligible + appru_y06 +
               (1 | la),
             data = model_data_special,
             family = binomial)

gc()
hm4 <- glmer(any_excl_y711 ~ 1 + exposure_highest_4_grp_y46 +
               female_clean + eth_major + language_clean + fsmeligible + appru_y06 +
               I(per_ag_sw / 10) + I(per_sw_turnover / 10) + I(sw_s251 / 100) + I(la_n / 10000) + I(la_fsm_prop * 10) +
               I(new_cin_prop * 100) + I(la_special_schools_prop * 100) +
               (1 | la),
             data = model_data_special,
             family = binomial) # 0.0546669

gc()
hm4a <- glmer(any_excl_y711 ~ 1 + exposure_highest_4_grp_y46 +
               female_clean + eth_major + language_clean + fsmeligible + appru_y06 +
               I(la_special_schools_prop * 100) +
               (1 | la),
             data = model_data_special,
             family = binomial) # 0.0034

gc()
hm4b <- glmer(any_excl_y711 ~ 1 + exposure_highest_4_grp_y46 +
                female_clean + eth_major + language_clean + fsmeligible + appru_y06 +
                I(la_special_schools_prop * 100) +
                (1 | la),
              data = model_data_special,
              family = binomial,
              control = glmerControl(optimizer = "bobyqa")) # nm 0.00253718 / bobyqa converged

models_any_y711_glm_s <- list()
models_any_y711_glm_s[[1]] <- hm1
models_any_y711_glm_s[[2]] <- um1
models_any_y711_glm_s[[3]] <- hm3
models_any_y711_glm_s[[4]] <- hm4b
save(models_any_y711_glm_s, file = "PROCESSED DATA/MODELS/MODELS_EXCLANY_y711_GLM_SPECIAL.RDA")
rm(hm1, um1, hm3, hm4, hm4a, hm4b)

summary(models_any_y711_glm_s[[1]])
summary(models_any_y711_glm_s[[2]])
lrtest(models_any_y711_glm_s[[2]], models_any_y711_glm_s[[1]])

summary(models_any_y711_glm_s[[3]])
lrtest(models_any_y711_glm_s[[3]], models_any_y711_glm_s[[2]])

summary(models_any_y711_glm_s[[4]])
lrtest(models_any_y711_glm_s[[4]], models_any_y711_glm_s[[3]])

# ** post hoc -------------------------------------------------------------

gc()
hm6_all_la_vars <- glmer(any_excl_y711 ~ 1 + exposure_highest_4_grp_y46 +
                           I(per_ag_sw / 10) + I(per_sw_turnover / 10) + I(sw_s251 / 100) + I(la_n / 10000) + I(la_fsm_prop * 10) +
                           I(new_cin_prop * 100) + I(la_special_schools_prop * 100) +
                           (1 | la),
                         data = model_data_special,
                         family = binomial,
                         control = glmerControl(optimizer = "bobyqa")) # def 0.013263 / bobyqa converged

model_data_special[, any_csc_y46 := exposure_highest_4_grp_y46 != "None"]

gc()
hm_posthoc_random_slopes_1 <- glmer(any_excl_y711 ~ any_csc_y46 + 
                                      (1 | la),
                                    data = model_data_special,
                                    family = binomial) # def converged

gc()
hm_posthoc_random_slopes_2 <- glmer(any_excl_y711 ~ any_csc_y46 + 
                                      (any_csc_y46 | la),
                                    data = model_data_special,
                                    family = binomial) # def converged

gc()
hm_posthoc_random_slopes_3 <- glmer(any_excl_y711 ~ any_csc_y46 + 
                                      female_clean + eth_major + language_clean + fsmeligible + appru_y06 +
                                      (any_csc_y46 | la),
                                    data = model_data_special,
                                    family = binomial,
                                    control = glmerControl(optimizer = "bobyqa")) # def 0.006534 / bobyqa converged

gc()
hm_posthoc_random_slopes_4 <- glmer(any_excl_y711 ~ any_csc_y46 + 
                                      female_clean + eth_major + language_clean + fsmeligible + appru_y06 +
                                      I(la_special_schools_prop * 100) +
                                      (any_csc_y46 | la),
                                    data = model_data_special,
                                    family = binomial,
                                    control = glmerControl(optimizer = "bobyqa")) # bobyqa converged

models_any_y711_glm_s_posthoc <- list()
models_any_y711_glm_s_posthoc[[1]] <- hm6_all_la_vars
models_any_y711_glm_s_posthoc[[2]] <- hm_posthoc_random_slopes_1
models_any_y711_glm_s_posthoc[[3]] <- hm_posthoc_random_slopes_2
models_any_y711_glm_s_posthoc[[4]] <- hm_posthoc_random_slopes_3
models_any_y711_glm_s_posthoc[[5]] <- hm_posthoc_random_slopes_4
save(models_any_y711_glm_s_posthoc, file = "PROCESSED DATA/MODELS/MODELS_EXCLANY_y711_GLM_SPECIAL_POSTHOC.RDA")
rm(hm6_all_la_vars, hm_posthoc_random_slopes_1, hm_posthoc_random_slopes_2, hm_posthoc_random_slopes_3,
   hm_posthoc_random_slopes_4)

summary(models_any_y711_glm_s_posthoc[[1]])

summary(models_any_y711_glm_s_posthoc[[2]])
summary(models_any_y711_glm_s_posthoc[[3]])
lrtest(models_any_y711_glm_s_posthoc[[3]], models_any_y711_glm_s_posthoc[[2]])

summary(models_any_y711_glm_s_posthoc[[4]])
lrtest(models_any_y711_glm_s_posthoc[[4]], models_any_y711_glm_s_posthoc[[3]])

summary(models_any_y711_glm_s_posthoc[[5]])
lrtest(models_any_y711_glm_s_posthoc[[5]], models_any_y711_glm_s_posthoc[[4]])

# ** cross-level interactions ---------------------------------------------

x1a <- glmer(any_excl_y711 ~ exposure_highest_4_grp_y46 + I(per_ag_sw / 10) +
               (1 | la),
             data = model_data_special,
             family = binomial)
x1b <- glmer(any_excl_y711 ~ exposure_highest_4_grp_y46 * I(per_ag_sw / 10) +
               (1 | la),
             data = model_data_special,
             family = binomial)

x2a <- glmer(any_excl_y711 ~ exposure_highest_4_grp_y46 + I(per_sw_turnover / 10) +
               (1 | la),
             data = model_data_special,
             family = binomial)
x2b <- glmer(any_excl_y711 ~ exposure_highest_4_grp_y46 * I(per_sw_turnover / 10) +
               (1 | la),
             data = model_data_special,
             family = binomial,
             control = glmerControl(optimizer = "bobyqa")) # def 0.00162925 / bobyqa conv

x3a <- glmer(any_excl_y711 ~ exposure_highest_4_grp_y46 + I(sw_s251 / 100) +
               (1 | la),
             data = model_data_special,
             family = binomial)
x3b <- glmer(any_excl_y711 ~ exposure_highest_4_grp_y46 * I(sw_s251 / 100) +
               (1 | la),
             data = model_data_special,
             family = binomial,
             control = glmerControl(optimizer = "bobyqa")) # def 0.00907334 / bobyqa conv

x4a <- glmer(any_excl_y711 ~ exposure_highest_4_grp_y46 + I(la_fsm_prop * 10) +
               (1 | la),
             data = model_data_special,
             family = binomial)
x4b <- glmer(any_excl_y711 ~ exposure_highest_4_grp_y46 * I(la_fsm_prop * 10) +
               (1 | la),
             data = model_data_special,
             family = binomial,
             control = glmerControl(optimizer = "bobyqa")) # def 0.00215102 / bobyqa conv

x5a <- glmer(any_excl_y711 ~ exposure_highest_4_grp_y46 + I(new_cin_prop * 100) +
               (1 | la),
             data = model_data_special,
             family = binomial)
x5b <- glmer(any_excl_y711 ~ exposure_highest_4_grp_y46 * I(new_cin_prop * 100) +
               (1 | la),
             data = model_data_special,
             family = binomial,
             control = glmerControl(optimizer = "bobyqa")) # def 0.00163561 / bobyqa conv

x6a <- glmer(any_excl_y711 ~ exposure_highest_4_grp_y46 + I(la_special_schools_prop * 100) +
               (1 | la),
             data = model_data_special,
             family = binomial)
x6b <- glmer(any_excl_y711 ~ exposure_highest_4_grp_y46 * I(la_special_schools_prop * 100) +
               (1 | la),
             data = model_data_special,
             family = binomial,
             control = glmerControl(optimizer = "bobyqa")) # def 0.00256507 / bobyqa conv

models_any_excl_y711_x_int_glm_s <- list()
models_any_excl_y711_x_int_glm_s[[1]] <- x1a
models_any_excl_y711_x_int_glm_s[[2]] <- x1b
models_any_excl_y711_x_int_glm_s[[3]] <- x2a
models_any_excl_y711_x_int_glm_s[[4]] <- x2b
models_any_excl_y711_x_int_glm_s[[5]] <- x3a
models_any_excl_y711_x_int_glm_s[[6]] <- x3b
models_any_excl_y711_x_int_glm_s[[7]] <- x4a
models_any_excl_y711_x_int_glm_s[[8]] <- x4b
models_any_excl_y711_x_int_glm_s[[9]] <- x5a
models_any_excl_y711_x_int_glm_s[[10]] <- x5b
models_any_excl_y711_x_int_glm_s[[11]] <- x6a
models_any_excl_y711_x_int_glm_s[[12]] <- x6b
save(models_any_excl_y711_x_int_glm_s, file = "PROCESSED DATA/MODELS/MODELS_any_excl_y711_GLM_SPECIAL_XINT.RDA")
rm(x1a, x1b, x2a, x2b, x3a, x3b, x4a, x4b, x5a, x5b, x6a, x6b)

summary(models_any_excl_y711_x_int_glm_s[[1]])
summary(models_any_excl_y711_x_int_glm_s[[2]])
lrtest(models_any_excl_y711_x_int_glm_s[[2]], models_any_excl_y711_x_int_glm_s[[1]])

summary(models_any_excl_y711_x_int_glm_s[[3]])
summary(models_any_excl_y711_x_int_glm_s[[4]])
lrtest(models_any_excl_y711_x_int_glm_s[[4]], models_any_excl_y711_x_int_glm_s[[3]])

summary(models_any_excl_y711_x_int_glm_s[[5]])
summary(models_any_excl_y711_x_int_glm_s[[6]])
lrtest(models_any_excl_y711_x_int_glm_s[[6]], models_any_excl_y711_x_int_glm_s[[5]])

summary(models_any_excl_y711_x_int_glm_s[[7]])
summary(models_any_excl_y711_x_int_glm_s[[8]])
lrtest(models_any_excl_y711_x_int_glm_s[[8]], models_any_excl_y711_x_int_glm_s[[7]])

summary(models_any_excl_y711_x_int_glm_s[[9]])
summary(models_any_excl_y711_x_int_glm_s[[10]])
lrtest(models_any_excl_y711_x_int_glm_s[[10]], models_any_excl_y711_x_int_glm_s[[9]])

summary(models_any_excl_y711_x_int_glm_s[[11]])
summary(models_any_excl_y711_x_int_glm_s[[12]])
lrtest(models_any_excl_y711_x_int_glm_s[[12]], models_any_excl_y711_x_int_glm_s[[11]])

# ** cross-level with adjustments -----------------------------------------

gc()
x1b_adj <- glmer(any_excl_y711 ~ exposure_highest_4_grp_y46 * I(per_ag_sw / 10) +
                   female_clean + eth_major + language_clean + fsmeligible + appru_y06 +
                   (1 | la),
                 data = model_data_special,
                 family = binomial,
                 control = glmerControl(optimizer = "bobyqa")) # def 0.00759135

gc()
x2b_adj <- glmer(any_excl_y711 ~ exposure_highest_4_grp_y46 * I(per_sw_turnover / 10) +
                   female_clean + eth_major + language_clean + fsmeligible + appru_y06 +
                   (1 | la),
                 data = model_data_special,
                 family = binomial,
                 control = glmerControl(optimizer = "bobyqa")) # def 0.0121226

gc()
x3b_adj <- glmer(any_excl_y711 ~ exposure_highest_4_grp_y46 * I(sw_s251 / 100) +
                   female_clean + eth_major + language_clean + fsmeligible + appru_y06 +
                   (1 | la),
                 data = model_data_special,
                 family = binomial,
                 control = glmerControl(optimizer = "bobyqa")) # def 0.0342178

gc()
x4b_adj <- glmer(any_excl_y711 ~ exposure_highest_4_grp_y46 * I(la_fsm_prop * 10) +
                   female_clean + eth_major + language_clean + fsmeligible + appru_y06 +
                   (1 | la),
                 data = model_data_special,
                 family = binomial,
                 control = glmerControl(optimizer = "bobyqa")) # def 0.069983

gc()
x5b_adj <- glmer(any_excl_y711 ~ exposure_highest_4_grp_y46 * I(new_cin_prop * 100) +
                   female_clean + eth_major + language_clean + fsmeligible + appru_y06 +
                   (1 | la),
                 data = model_data_special,
                 family = binomial,
                 control = glmerControl(optimizer = "bobyqa")) # def 0.00496774

gc()
x6b_adj <- glmer(any_excl_y711 ~ exposure_highest_4_grp_y46 * I(la_special_schools_prop * 100) +
                   female_clean + eth_major + language_clean + fsmeligible + appru_y06 +
                   (1 | la),
                 data = model_data_special,
                 family = binomial,
                 control = glmerControl(optimizer = "bobyqa")) # def 0.03077988

models_any_excl_y711_x_int_adj_glm_s <- list()
models_any_excl_y711_x_int_adj_glm_s[[1]] <- x1b_adj
models_any_excl_y711_x_int_adj_glm_s[[2]] <- x2b_adj
models_any_excl_y711_x_int_adj_glm_s[[3]] <- x3b_adj
models_any_excl_y711_x_int_adj_glm_s[[4]] <- x4b_adj
models_any_excl_y711_x_int_adj_glm_s[[5]] <- x5b_adj
models_any_excl_y711_x_int_adj_glm_s[[6]] <- x6b_adj
save(models_any_excl_y711_x_int_adj_glm_s, file = "PROCESSED DATA/MODELS/MODELS_any_excl_y711_GLM_SPECIAL_XINT_ADJ.RDA")
rm(x1b_adj, x2b_adj, x3b_adj, x4b_adj, x5b_adj, x6b_adj)

summary(models_any_excl_y711_x_int_adj_glm_s[[1]])
lrtest(models_any_excl_y711_x_int_adj_glm_s[[1]], models_any_excl_y711_x_int_glm_s[[2]])

summary(models_any_excl_y711_x_int_adj_glm_s[[2]])
lrtest(models_any_excl_y711_x_int_adj_glm_s[[2]], models_any_excl_y711_x_int_glm_s[[4]])

summary(models_any_excl_y711_x_int_adj_glm_s[[3]])
lrtest(models_any_excl_y711_x_int_adj_glm_s[[3]], models_any_excl_y711_x_int_glm_s[[6]])

summary(models_any_excl_y711_x_int_adj_glm_s[[4]])
lrtest(models_any_excl_y711_x_int_adj_glm_s[[4]], models_any_excl_y711_x_int_glm_s[[8]])

summary(models_any_excl_y711_x_int_adj_glm_s[[5]])
lrtest(models_any_excl_y711_x_int_adj_glm_s[[5]], models_any_excl_y711_x_int_glm_s[[10]])

summary(models_any_excl_y711_x_int_adj_glm_s[[6]])
lrtest(models_any_excl_y711_x_int_adj_glm_s[[6]], models_any_excl_y711_x_int_glm_s[[12]])

# la adjustment
gc()
x1b_adj_la <- glmer(any_excl_y711 ~ exposure_highest_4_grp_y46 * I(per_ag_sw / 10) +
                      female_clean + eth_major + language_clean + fsmeligible + appru_y06 +
                      I(la_special_schools_prop * 100) +
                      (1 | la),
                    data = model_data_special,
                    family = binomial,
                    control = glmerControl(optimizer = "bobyqa"))

gc()
x2b_adj_la <- glmer(any_excl_y711 ~ exposure_highest_4_grp_y46 * I(per_sw_turnover / 10) +
                      female_clean + eth_major + language_clean + fsmeligible + appru_y06 +
                      I(la_special_schools_prop * 100) +
                      (1 | la),
                    data = model_data_special,
                    family = binomial,
                    control = glmerControl(optimizer = "bobyqa"))

gc()
x3b_adj_la <- glmer(any_excl_y711 ~ exposure_highest_4_grp_y46 * I(sw_s251/100) +
                      female_clean + eth_major + language_clean + fsmeligible + appru_y06 +
                      I(la_special_schools_prop * 100) +
                      (1 | la),
                    data = model_data_special,
                    family = binomial,
                    control = glmerControl(optimizer = "bobyqa"))

gc()
x4b_adj_la <- glmer(any_excl_y711 ~ exposure_highest_4_grp_y46 * I(la_fsm_prop * 10) +
                      female_clean + eth_major + language_clean + fsmeligible + appru_y06 +
                      I(la_special_schools_prop * 100) +
                      (1 | la),
                    data = model_data_special,
                    family = binomial,
                    control = glmerControl(optimizer = "bobyqa"))

gc()
x5b_adj_la <- glmer(any_excl_y711 ~ exposure_highest_4_grp_y46 * I(new_cin_prop * 100) +
                      female_clean + eth_major + language_clean + fsmeligible + appru_y06 +
                      I(la_special_schools_prop * 100) +
                      (1 | la),
                    data = model_data_special,
                    family = binomial,
                    control = glmerControl(optimizer = "bobyqa"))

models_any_excl_y711_x_int_adj_la_glm_s <- list()
models_any_excl_y711_x_int_adj_la_glm_s[[1]] <- x1b_adj_la
models_any_excl_y711_x_int_adj_la_glm_s[[2]] <- x2b_adj_la
models_any_excl_y711_x_int_adj_la_glm_s[[3]] <- x3b_adj_la
models_any_excl_y711_x_int_adj_la_glm_s[[4]] <- x4b_adj_la
models_any_excl_y711_x_int_adj_la_glm_s[[5]] <- x5b_adj_la

save(models_any_excl_y711_x_int_adj_la_glm_s, file = "PROCESSED DATA/MODELS/MODELS_any_excl_y711_GLM_SPECIAL_XINT_ADJ_LA.RDA")
rm(x1b_adj_la, x2b_adj_la, x3b_adj_la, x4b_adj_la, x5b_adj_la)

summary(models_any_excl_y711_x_int_adj_la_glm_s[[1]])
lrtest(models_any_excl_y711_x_int_adj_la_glm_s[[1]], models_any_excl_y711_x_int_adj_glm_s[[1]])

summary(models_any_excl_y711_x_int_adj_la_glm_s[[2]])
lrtest(models_any_excl_y711_x_int_adj_la_glm_s[[2]], models_any_excl_y711_x_int_adj_glm_s[[2]])

summary(models_any_excl_y711_x_int_adj_la_glm_s[[3]])
lrtest(models_any_excl_y711_x_int_adj_la_glm_s[[3]], models_any_excl_y711_x_int_adj_glm_s[[3]])

summary(models_any_excl_y711_x_int_adj_la_glm_s[[4]])
lrtest(models_any_excl_y711_x_int_adj_la_glm_s[[4]], models_any_excl_y711_x_int_adj_glm_s[[4]])

summary(models_any_excl_y711_x_int_adj_la_glm_s[[5]])
lrtest(models_any_excl_y711_x_int_adj_la_glm_s[[5]], models_any_excl_y711_x_int_adj_glm_s[[5]])

rm(models_any_excl_y711_glm_ns, models_any_excl_y711_x_int_glm_s,
   models_any_excl_y711_x_int_adj_glm_ns, models_any_excl_y711_x_int_adj_la_glm_s)
