
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

load("PROCESSED DATA/cohort1_npd_clean_sch_ad_qual_cscobs_outcomes_misc.rda")
load("PROCESSED DATA/cohort2_npd_clean_sch_ad_qual_cscobs_outcomes_misc.rda")

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

# MISSING ---------------------------------------------

cohort_all$any_nas <- apply(cohort_all[, c("female_clean", "eth_major", "language_clean", "idaci_quintiles")], 1, anyNA)
cohort_all[, drop := year == 7 & any_nas & sch_n_in_year == 1]
cohort_all[, drop := max(drop), by = .(pupilmatchingrefanonymous)]
cohort_all <- cohort_all[!cohort_all$drop]
cohort_all[, any_nas := NULL]
cohort_all[, drop := NULL]

# COMPLETE DATA YY7 TO 9 ONLY ------------------------------------------------------

cohort_all$complete_record_y79 <-
  cohort_all$pupilmatchingrefanonymous %in% cohort_all[year == 7]$pupilmatchingrefanonymous &
  cohort_all$pupilmatchingrefanonymous %in% cohort_all[year == 8]$pupilmatchingrefanonymous &
  cohort_all$pupilmatchingrefanonymous %in% cohort_all[year == 9]$pupilmatchingrefanonymous

table(cohort_all[year == 7 & sch_n_in_year == 1]$complete_record_y79) 

exposures <- c("exposure_highest_4_grp_y46", "exposure_highest_4_grp_y49",
               "female_clean", "eth_major", "language_clean", "idaci_quintiles", "fsmeligible", "idaci_fsm", "region_major",
               "school_type_with_secondary_5_groups", "appru_y06", "appru_y09", "special_school",
               "ever_sen_primary", "highest_ever_sen_primary", "ever_sen_y09", "highest_ever_sen_y09")

final_output <- cbind(describe_outcome(exposure = exposures, outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & complete_record_y79 == T]),
                      describe_outcome(exposure = exposures, outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & complete_record_y79 == F]),
                      describe_outcome(exposure = exposures, outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1]))
colnames(final_output) <- c("Complete secondary school records", "Incomplete records", "Combined")
write.csv(final_output, file = "OUTPUTS/CHAPTER 7/posthoc/chars-complete-records-y79.csv")
rm(exposures, final_output)

cohort_all <- cohort_all[complete_record_y79 == T]

# COMBINE EXCLUSIONS ------------------------------------------------------

cohort_all[, any_excl_y7 := perm_excl_y7 | ft_excl_y7]
cohort_all[, any_excl_y8 := perm_excl_y8 | ft_excl_y8]
cohort_all[, any_excl_y9 := perm_excl_y9 | ft_excl_y9]

cohort_all[, any_excl_y79 := perm_excl_y79 | ft_excl_y79]

cohort_all[, ft_excluded_year := F]
cohort_all[year == 7, ft_excluded_year := ft_excl_y7]
cohort_all[year == 8, ft_excluded_year := ft_excl_y8]
cohort_all[year == 9, ft_excluded_year := ft_excl_y9]

cohort_all[, perm_excluded_year := F]
cohort_all[year == 7, perm_excluded_year := perm_excl_y7]
cohort_all[year == 8, perm_excluded_year := perm_excl_y8]
cohort_all[year == 9, perm_excluded_year := perm_excl_y9]

cohort_all[, any_excluded_year := F]
cohort_all[year == 7, any_excluded_year := any_excl_y7]
cohort_all[year == 8, any_excluded_year := any_excl_y8]
cohort_all[year == 9, any_excluded_year := any_excl_y9]

# ANALYSIS ----------------------------------------------------------------

# * main ------------------------------------------------------------------

table(cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F]$not_enrolled_y1011_nogcses)
table(cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T]$not_enrolled_y1011_nogcses)

table(cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F]$exposure_highest_4_grp_y46)
table(cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T]$exposure_highest_4_grp_y46)

outcomes <- c("ft_excl_y79",
              "perm_excl_y79",
              "any_excl_y79")

exposure <- "not_enrolled_y1011_nogcses"

# overall
final_output <- cbind(rbind(describe_outcome(exposure = exposure, outcome = outcomes[1], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F], p_round = 1),
                            describe_outcome(exposure = exposure, outcome = outcomes[2], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F], p_round = 1),
                            describe_outcome(exposure = exposure, outcome = outcomes[3], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F], p_round = 1)),
                      rbind(describe_outcome(exposure = exposure, outcome = outcomes[1], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T], p_round = 1),
                            describe_outcome(exposure = exposure, outcome = outcomes[2], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T], p_round = 1),
                            describe_outcome(exposure = exposure, outcome = outcomes[3], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T], p_round = 1)))
colnames(final_output) <- c("Not special school yr 7", "Special school yr 7")
write.csv(final_output, file = "OUTPUTS/CHAPTER 7/posthoc/excl-overall.csv")

# CSC none
final_output <- cbind(rbind(describe_outcome(exposure = exposure, outcome = outcomes[1], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F & exposure_highest_4_grp_y46 == "None"], p_round = 1),
                            describe_outcome(exposure = exposure, outcome = outcomes[2], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F & exposure_highest_4_grp_y46 == "None"], p_round = 1),
                            describe_outcome(exposure = exposure, outcome = outcomes[3], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F & exposure_highest_4_grp_y46 == "None"], p_round = 1)),
                      rbind(describe_outcome(exposure = exposure, outcome = outcomes[1], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T & exposure_highest_4_grp_y46 == "None"], p_round = 1),
                            describe_outcome(exposure = exposure, outcome = outcomes[2], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T & exposure_highest_4_grp_y46 == "None"], p_round = 1),
                            describe_outcome(exposure = exposure, outcome = outcomes[3], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T & exposure_highest_4_grp_y46 == "None"], p_round = 1)))
colnames(final_output) <- c("Not special school yr 7", "Special school yr 7")
write.csv(final_output, file = "OUTPUTS/CHAPTER 7/posthoc/excl-csc-none.csv")

# CSC CiN
final_output <- cbind(rbind(describe_outcome(exposure = exposure, outcome = outcomes[1], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F & exposure_highest_4_grp_y46 == "CiN"], p_round = 1),
                            describe_outcome(exposure = exposure, outcome = outcomes[2], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F & exposure_highest_4_grp_y46 == "CiN"], p_round = 1),
                            describe_outcome(exposure = exposure, outcome = outcomes[3], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F & exposure_highest_4_grp_y46 == "CiN"], p_round = 1)),
                      rbind(describe_outcome(exposure = exposure, outcome = outcomes[1], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T & exposure_highest_4_grp_y46 == "CiN"], p_round = 1),
                            describe_outcome(exposure = exposure, outcome = outcomes[2], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T & exposure_highest_4_grp_y46 == "CiN"], p_round = 1),
                            describe_outcome(exposure = exposure, outcome = outcomes[3], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T & exposure_highest_4_grp_y46 == "CiN"], p_round = 1)))
colnames(final_output) <- c("Not special school yr 7", "Special school yr 7")
write.csv(final_output, file = "OUTPUTS/CHAPTER 7/posthoc/excl-csc-cin.csv")

# CSC CPP
final_output <- cbind(rbind(describe_outcome(exposure = exposure, outcome = outcomes[1], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F & exposure_highest_4_grp_y46 == "CPP"], p_round = 1),
                            describe_outcome(exposure = exposure, outcome = outcomes[2], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F & exposure_highest_4_grp_y46 == "CPP"], p_round = 1),
                            describe_outcome(exposure = exposure, outcome = outcomes[3], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F & exposure_highest_4_grp_y46 == "CPP"], p_round = 1)),
                      rbind(describe_outcome(exposure = exposure, outcome = outcomes[1], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T & exposure_highest_4_grp_y46 == "CPP"], p_round = 1),
                            describe_outcome(exposure = exposure, outcome = outcomes[2], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T & exposure_highest_4_grp_y46 == "CPP"], p_round = 1),
                            describe_outcome(exposure = exposure, outcome = outcomes[3], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T & exposure_highest_4_grp_y46 == "CPP"], p_round = 1)))
colnames(final_output) <- c("Not special school yr 7", "Special school yr 7")
write.csv(final_output, file = "OUTPUTS/CHAPTER 7/posthoc/excl-csc-cpp.csv")

# CSC CLA
final_output <- cbind(rbind(describe_outcome(exposure = exposure, outcome = outcomes[1], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F & exposure_highest_4_grp_y46 == "CLA"], p_round = 1),
                            describe_outcome(exposure = exposure, outcome = outcomes[2], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F & exposure_highest_4_grp_y46 == "CLA"], p_round = 1),
                            describe_outcome(exposure = exposure, outcome = outcomes[3], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F & exposure_highest_4_grp_y46 == "CLA"], p_round = 1)),
                      rbind(describe_outcome(exposure = exposure, outcome = outcomes[1], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T & exposure_highest_4_grp_y46 == "CLA"], p_round = 1),
                            describe_outcome(exposure = exposure, outcome = outcomes[2], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T & exposure_highest_4_grp_y46 == "CLA"], p_round = 1),
                            describe_outcome(exposure = exposure, outcome = outcomes[3], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T & exposure_highest_4_grp_y46 == "CLA"], p_round = 1)))
colnames(final_output) <- c("Not special school yr 7", "Special school yr 7")
write.csv(final_output, file = "OUTPUTS/CHAPTER 7/posthoc/excl-csc-cla.csv")