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
library(gridExtra)

# sequence analysis
library(TraMineR)
library(fpc)

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
    
    colnames(ci_output) <- rep(names_for_cols, 4)
    
    return(ci_output)
    
  }
}

# MISSING ---------------------------------------------

table(cohort_all[year == 7 & sch_n_in_year == 1]$cohort)

nrow(cohort_all[year == 7 & sch_n_in_year == 1]) 

summary(cohort_all[year == 7 & sch_n_in_year == 1, c("pupilmatchingrefanonymous", "cohort",
                                                     "female_clean", "eth_major", "language_clean", "highest_ever_sen_y09",
                                                     "ever_sen_y09",
                                                     "idaci_quintiles", "fsmeligible", "idaci_fsm", "region_major", "la",
                                                     "exposure_highest_4_grp_y46",
                                                     "school_type_with_secondary_5_groups", "appru_y09", "special_school",
                                                     "not_enrolled_y8", "not_enrolled_y9", "not_enrolled_y10", "not_enrolled_y11",
                                                     "not_enrolled_y89", "not_enrolled_y1011", "no_gcses",
                                                     "not_enrolled_y1011_nogcses")])

cohort_all$any_nas <- apply(cohort_all[, c("female_clean", "eth_major", "language_clean", "idaci_quintiles")], 1, anyNA)
cohort_all[, missing := year == 7 & any_nas & sch_n_in_year == 1]
cohort_all[, missing := max(missing), by = .(pupilmatchingrefanonymous)]
table(cohort_all[year == 7 & sch_n_in_year == 1]$missing)

# View(cohort_all[missing == T,
#                 c("pupilmatchingrefanonymous", "year", "school_type", "school_type_with_secondary", "aptype",
#                   "female_clean", "eth_major", "language_clean", "idaci_quintiles")])
# table(cohort_all$school_type, is.na(cohort_all$idaci_quintiles))

exposures <- c("exposure_highest_4_grp_y46",
               "female_clean", "eth_major", "language_clean", "idaci_quintiles", "fsmeligible", "idaci_fsm", "region_major",
               "ever_sen_primary", "highest_ever_sen_primary",
               "school_type_with_secondary_5_groups", "appru", "special_school")

final_output <- cbind(describe_outcome(exposure = exposures, outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & missing == T]),
                      describe_outcome(exposure = exposures, outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & missing == F]),
                      describe_outcome(exposure = exposures, outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1]))
colnames(final_output) <- c("Any missing", "No missing", "All")
write.csv(final_output, file = "OUTPUTS/CHAPTER 5/missing.csv")

cohort_all <- cohort_all[!cohort_all$missing]
cohort_all[, any_nas := NULL]
cohort_all[, missing := NULL]

# DEMOGRAPHICS ------------------------------------------------------------

table(cohort_all[year == 7 & sch_n_in_year == 1]$cohort)

nrow(cohort_all[year == 7 & sch_n_in_year == 1]) 

exposures <- c("exposure_highest_4_grp_y46",
               "female_clean", "eth_major", "language_clean", "idaci_quintiles", "fsmeligible", "idaci_fsm", "region_major",
               "ever_sen_primary", "highest_ever_sen_primary",
               "school_type_with_secondary_5_groups", "appru", "special_school")

final_output <- cbind(describe_outcome(exposure = exposures, outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & cohort == 1]),
                      describe_outcome(exposure = exposures, outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & cohort == 2]),
                      describe_outcome(exposure = exposures, outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1]))
colnames(final_output) <- c("Cohort 1", "Cohort 2", "Combined")
write.csv(final_output, file = "OUTPUTS/CHAPTER 5/chars.csv")

# by special school or not
table(cohort_all[year == 7 & sch_n_in_year == 1]$special_school)

exposures <- c("exposure_highest_4_grp_y46",
               "female_clean", "eth_major", "language_clean", "idaci_quintiles", "fsmeligible", "idaci_fsm", "region_major",
               "ever_sen_primary", "highest_ever_sen_primary",
               "school_type_with_secondary_5_groups", "appru", "appru_y06", "appru_y09")

final_output <- cbind(describe_outcome(exposure = exposures, outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T]),
                      describe_outcome(exposure = exposures, outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F]),
                      describe_outcome(exposure = exposures, outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1]))
colnames(final_output) <- c("Special school yr 7", "Not special school yr 7", "Combined")
write.csv(final_output, file = "OUTPUTS/CHAPTER 5/chars-by-special-school.csv")

rm(exposures, final_output)

cohort_all[, exposure_highest_4_grp_y46_none := 0]
cohort_all[, exposure_highest_4_grp_y46_cin := 0]
cohort_all[, exposure_highest_4_grp_y46_cpp := 0]
cohort_all[, exposure_highest_4_grp_y46_cla := 0]

cohort_all[exposure_highest_4_grp_y46 == "None", exposure_highest_4_grp_y46_none := 1]
cohort_all[exposure_highest_4_grp_y46 == "CiN", exposure_highest_4_grp_y46_cin := 1]
cohort_all[exposure_highest_4_grp_y46 == "CPP", exposure_highest_4_grp_y46_cpp := 1]
cohort_all[exposure_highest_4_grp_y46 == "CLA", exposure_highest_4_grp_y46_cla := 1]

table(cohort_all$exposure_highest_4_grp_y46, cohort_all$exposure_highest_4_grp_y46_none)
table(cohort_all$exposure_highest_4_grp_y46, cohort_all$exposure_highest_4_grp_y46_cin)
table(cohort_all$exposure_highest_4_grp_y46, cohort_all$exposure_highest_4_grp_y46_cpp)
table(cohort_all$exposure_highest_4_grp_y46, cohort_all$exposure_highest_4_grp_y46_cla)

outcomes <- c("exposure_highest_4_grp_y46_none", "exposure_highest_4_grp_y46_cin",
              "exposure_highest_4_grp_y46_cpp", "exposure_highest_4_grp_y46_cla")
exposures <- c("female_clean", "eth_major", "language_clean", "idaci_quintiles", "fsmeligible", "idaci_fsm", "region_major",
              "ever_sen_primary", "highest_ever_sen_primary", "appru")

final_output <- cbind(
  describe_outcome(exposure = exposures, outcome = outcomes[1], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F], p_round = 1),
  describe_outcome(exposure = exposures, outcome = outcomes[2], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F], p_round = 1),
  describe_outcome(exposure = exposures, outcome = outcomes[3], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F], p_round = 1),
  describe_outcome(exposure = exposures, outcome = outcomes[4], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F], p_round = 1)
)

write.csv(final_output, file = "OUTPUTS/CHAPTER 5/chars-by-exposure-not-special.csv")

exposures <- c("female_clean", "eth_major", "language_clean", "idaci_quintiles", "fsmeligible", "idaci_fsm", "region_major", "appru")

final_output <- cbind(
  describe_outcome(exposure = exposures, outcome = outcomes[1], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T], p_round = 1),
  describe_outcome(exposure = exposures, outcome = outcomes[2], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T], p_round = 1),
  describe_outcome(exposure = exposures, outcome = outcomes[3], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T], p_round = 1),
  describe_outcome(exposure = exposures, outcome = outcomes[4], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T], p_round = 1)
)

write.csv(final_output, file = "OUTPUTS/CHAPTER 5/chars-by-exposure-special.csv")

rm(exposures, outcomes, final_output)

# NOT ENROLLED ------------------------------------------------------------

# * cross-sectional -------------------------------------------------------

outcomes <- c("not_enrolled_y8",
              "not_enrolled_y9",
              "not_enrolled_y10",
              "not_enrolled_y11")

final_output <- cbind(rbind(describe_outcome(exposure = outcomes[1], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F], p_round = 1),
                            describe_outcome(exposure = outcomes[2], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F], p_round = 1),
                            describe_outcome(exposure = outcomes[3], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F], p_round = 1),
                            describe_outcome(exposure = outcomes[4], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F], p_round = 1)),
                      rbind(describe_outcome(exposure = outcomes[1], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T], p_round = 1),
                            describe_outcome(exposure = outcomes[2], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T], p_round = 1),
                            describe_outcome(exposure = outcomes[3], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T], p_round = 1),
                            describe_outcome(exposure = outcomes[4], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T], p_round = 1)))
colnames(final_output) <- c("Not special school yr 7", "Special school yr 7")
write.csv(final_output, file = "OUTPUTS/CHAPTER 5/not-enrolled-cross-sectional-total-yearly.csv")

exposures <- c("exposure_highest_4_grp_y46",
               "female_clean", "eth_major", "language_clean", "idaci_quintiles", "fsmeligible", "idaci_fsm", "region_major",
               "ever_sen_primary", "highest_ever_sen_primary",
               "appru", "appru_y06")

final_output <- cbind(
  describe_outcome(exposure = exposures, outcome = outcomes[1], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F], p_round = 1),
  describe_outcome(exposure = exposures, outcome = outcomes[2], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F], p_round = 1),
  describe_outcome(exposure = exposures, outcome = outcomes[3], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F], p_round = 1),
  describe_outcome(exposure = exposures, outcome = outcomes[4], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F], p_round = 1)
)

write.csv(final_output, file = "OUTPUTS/CHAPTER 5/not-enrolled-cross-sectional-by-risk-factors-not-special-yearly.csv")

exposures <- c("exposure_highest_4_grp_y46",
               "female_clean", "eth_major", "language_clean", "idaci_quintiles", "fsmeligible", "idaci_fsm", "region_major",
               "appru", "appru_y06")

final_output <- cbind(
  describe_outcome(exposure = exposures, outcome = outcomes[1], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T], p_round = 1),
  describe_outcome(exposure = exposures, outcome = outcomes[2], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T], p_round = 1),
  describe_outcome(exposure = exposures, outcome = outcomes[3], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T], p_round = 1),
  describe_outcome(exposure = exposures, outcome = outcomes[4], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T], p_round = 1)
)

write.csv(final_output, file = "OUTPUTS/CHAPTER 5/not-enrolled-cross-sectional-by-risk-factors-special-yearly.csv")

rm(exposures, outcomes, final_output)

# * cumulative ------------------------------------------------------------

# ** by CSC ---------------------------------------------------------------

# *** not special ---------------------------------------------------------

# not special
tmp <- data.table(
  pupilmatchingrefanonymous = cohort_all[year >= 7 & sch_n_in_year == 1 & special_school_y7 == F]$pupilmatchingrefanonymous,
  year = cohort_all[year >= 7 & sch_n_in_year == 1 & special_school_y7 == F]$year,
  exposure_highest_4_grp_y46 = cohort_all[year >= 7 & sch_n_in_year == 1 & special_school_y7 == F]$exposure_highest_4_grp_y46
)

tmp$not_enrolled <- 0
tmp <- data.table(complete(tmp, pupilmatchingrefanonymous, year))
tmp[is.na(not_enrolled)]$not_enrolled <- 1
tmp[, exposure_highest_4_grp_y46 := exposure_highest_4_grp_y46[1], by = .(pupilmatchingrefanonymous)]

# modified from function above
ci_data <- data.frame(
  pupilmatchingrefanonymous = tmp[year == 7]$pupilmatchingrefanonymous
)

ci_data$y7 <- as.integer(NA)
ci_data$y8 <- as.integer(NA)
ci_data$y9 <- as.integer(NA)
ci_data$y10 <- as.integer(NA)
ci_data$y11 <- as.integer(NA)

for (i in 1:5) {
  ci_data[, i + 1] <- ci_data$pupilmatchingrefanonymous %in%
    tmp[year <= i + 6 & not_enrolled == 1]$pupilmatchingrefanonymous
}

ci_data <- data.table(ci_data)

ci_output <- create_ci_output(subgroups = F, ci_data = ci_data, perc_round = 1)
ci_output

write.csv(ci_output, "OUTPUTS/CHAPTER 5/not-enrolled-cumulative-not-special.csv")

rm(ci_data)

# now by csc - also mofidied from above
pmr_vector <- tmp[year == 7]$pupilmatchingrefanonymous
factor_var <- tmp[year == 7]$exposure_highest_4_grp_y46

factor_var <- factor(factor_var)
factor_var_int <- as.integer(factor_var)
loop_length <- dim(table(factor_var))

ci_data <- list()
for (i in 1:loop_length) {
  ci_data[[i]] <- data.frame(
    pupilmatchingrefanonymous = pmr_vector[factor_var_int == i]
  )

  ci_data[[i]]$y7 <- as.integer(NA)
  ci_data[[i]]$y8 <- as.integer(NA)
  ci_data[[i]]$y9 <- as.integer(NA)
  ci_data[[i]]$y10 <- as.integer(NA)
  ci_data[[i]]$y11 <- as.integer(NA)

  for (j in 2:5) {
    ci_data[[i]][, j + 1] <- ci_data[[i]]$pupilmatchingrefanonymous %in%
      tmp[year <= j + 6 & not_enrolled == 1]$pupilmatchingrefanonymous
  }

  ci_data[[i]] <- data.table(ci_data[[i]])
}

rm(i, j, pmr_vector, factor_var, factor_var_int, loop_length)

ci_output <- create_ci_output(ci_data = ci_data, names_for_cols = levels(cohort_all$exposure_highest_4_grp_y46), perc_round = 1)
ci_output

write.csv(ci_output, "OUTPUTS/CHAPTER 5/not-enrolled-cumulative-by-exp-not-special.csv")

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

ci_output_p[year == 8 & Exposure %in% c("CPP", "CLA"), ]$ll <- 0

p1 <- ggplot(data = ci_output_p) +
  geom_line(aes(x = year, y = Perc, colour = Exposure), size = 1) +
  geom_point(aes(x = year, y = Perc, colour = Exposure), size = 2,
             position = position_dodge(0.05)) +
  geom_errorbar(aes(x = year, ymin = ll, ymax = ul, colour = Exposure),
                width = 0.3,
                position = position_dodge(0.05)) +
  xlab("Year") +
  ylab("Cumulative not enrolled (%)") +
  ggtitle("Not special schools") +
  scale_y_continuous(limits = c(0, 20),
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
        legend.position = c(0.25, 0.7))

# *** special -------------------------------------------------------------

tmp <- data.table(
  pupilmatchingrefanonymous = cohort_all[year >= 7 & sch_n_in_year == 1 & special_school_y7 == T]$pupilmatchingrefanonymous,
  year = cohort_all[year >= 7 & sch_n_in_year == 1 & special_school_y7 == T]$year,
  exposure_highest_4_grp_y46 = cohort_all[year >= 7 & sch_n_in_year == 1 & special_school_y7 == T]$exposure_highest_4_grp_y46
)

tmp$not_enrolled <- 0
tmp <- data.table(complete(tmp, pupilmatchingrefanonymous, year))
tmp[is.na(not_enrolled)]$not_enrolled <- 1
tmp[, exposure_highest_4_grp_y46 := exposure_highest_4_grp_y46[1], by = .(pupilmatchingrefanonymous)]

# modified from function above
ci_data <- data.frame(
  pupilmatchingrefanonymous = tmp[year == 7]$pupilmatchingrefanonymous
)

ci_data$y7 <- as.integer(NA)
ci_data$y8 <- as.integer(NA)
ci_data$y9 <- as.integer(NA)
ci_data$y10 <- as.integer(NA)
ci_data$y11 <- as.integer(NA)

for (i in 1:5) {
  ci_data[, i + 1] <- ci_data$pupilmatchingrefanonymous %in%
    tmp[year <= i + 6 & not_enrolled == 1]$pupilmatchingrefanonymous
}

ci_data <- data.table(ci_data)

ci_output <- create_ci_output(subgroups = F, ci_data = ci_data, perc_round = 1)
ci_output

write.csv(ci_output, "OUTPUTS/CHAPTER 5/not-enrolled-cumulative-special.csv")

rm(ci_data)

# now by csc - also mofidied from above
pmr_vector <- tmp[year == 7]$pupilmatchingrefanonymous
factor_var <- tmp[year == 7]$exposure_highest_4_grp_y46

factor_var <- factor(factor_var)
factor_var_int <- as.integer(factor_var)
loop_length <- dim(table(factor_var))

ci_data <- list()
for (i in 1:loop_length) {
  ci_data[[i]] <- data.frame(
    pupilmatchingrefanonymous = pmr_vector[factor_var_int == i]
  )
  
  ci_data[[i]]$y7 <- as.integer(NA)
  ci_data[[i]]$y8 <- as.integer(NA)
  ci_data[[i]]$y9 <- as.integer(NA)
  ci_data[[i]]$y10 <- as.integer(NA)
  ci_data[[i]]$y11 <- as.integer(NA)
  
  for (j in 2:5) {
    ci_data[[i]][, j + 1] <- ci_data[[i]]$pupilmatchingrefanonymous %in%
      tmp[year <= j + 6 & not_enrolled == 1]$pupilmatchingrefanonymous
  }
  
  ci_data[[i]] <- data.table(ci_data[[i]])
}

rm(i, j, pmr_vector, factor_var, factor_var_int, loop_length)

ci_output <- create_ci_output(ci_data = ci_data, names_for_cols = levels(cohort_all$exposure_highest_4_grp_y46), perc_round = 1)
ci_output

write.csv(ci_output, "OUTPUTS/CHAPTER 5/not-enrolled-cumulative-by-exp-special.csv")

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

ci_output_p[ci_output_p$ll < 0, ]$ll <- 0

# now suppress first two years of CPP as N too low
ci_output_p[12:13, ] <- NA

p2 <- ggplot(data = ci_output_p) +
  geom_line(aes(x = year, y = Perc, colour = Exposure), size = 1) +
  geom_point(aes(x = year, y = Perc, colour = Exposure), size = 2,
             position = position_dodge(0.05)) +
  geom_errorbar(aes(x = year, ymin = ll, ymax = ul, colour = Exposure),
                width = 0.3,
                position = position_dodge(0.05)) +
  xlab("Year") +
  ylab("Cumulative not enrolled (%)") +
  ggtitle("Special schools") +
  scale_y_continuous(limits = c(0, 20),
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
        legend.position = "none")

tiff("OUTPUTS/CHAPTER 5/offrolling-cumulative.tiff",
     width = 12, height = 6, units = "in", res = 300)
grid.arrange(p1, p2, nrow = 1)
dev.off()

rm(ci_data, ci_output, ci_output_p, tmp, p1, p2)

# *** not special & <5 GCSEs ----------------------------------------------

tmp <- data.table(
  pupilmatchingrefanonymous = cohort_all[year >= 7 & sch_n_in_year == 1 & special_school_y7 == F]$pupilmatchingrefanonymous,
  year = cohort_all[year >= 7 & sch_n_in_year == 1 & special_school_y7 == F]$year,
  exposure_highest_4_grp_y46 = cohort_all[year >= 7 & sch_n_in_year == 1 & special_school_y7 == F]$exposure_highest_4_grp_y46,
  no_gcses = cohort_all[year >= 7 & sch_n_in_year == 1 & special_school_y7 == F]$no_gcses
)

tmp$not_enrolled <- 0
tmp <- data.table(complete(tmp, pupilmatchingrefanonymous, year))
tmp[is.na(not_enrolled)]$not_enrolled <- 1
tmp[, exposure_highest_4_grp_y46 := exposure_highest_4_grp_y46[1], by = .(pupilmatchingrefanonymous)]
tmp[, no_gcses := no_gcses[1], by = .(pupilmatchingrefanonymous)]

# modified from function above
ci_data <- data.frame(
  pupilmatchingrefanonymous = tmp[year == 7]$pupilmatchingrefanonymous
)

ci_data$y7 <- as.integer(NA)
ci_data$y8 <- as.integer(NA)
ci_data$y9 <- as.integer(NA)
ci_data$y10 <- as.integer(NA)
ci_data$y11 <- as.integer(NA)

for (i in 1:5) {
  ci_data[, i + 1] <- ci_data$pupilmatchingrefanonymous %in%
    tmp[year <= i + 6 & not_enrolled == 1 & no_gcses == 1]$pupilmatchingrefanonymous
}

ci_data <- data.table(ci_data)

ci_output <- create_ci_output(subgroups = F, ci_data = ci_data, perc_round = 1)
ci_output

write.csv(ci_output, "OUTPUTS/CHAPTER 5/not-enrolled-cumulative-not-special-no-gcses.csv")

rm(ci_data)

# now by csc - also mofidied from above
pmr_vector <- tmp[year == 7]$pupilmatchingrefanonymous
factor_var <- tmp[year == 7]$exposure_highest_4_grp_y46

factor_var <- factor(factor_var)
factor_var_int <- as.integer(factor_var)
loop_length <- dim(table(factor_var))

ci_data <- list()
for (i in 1:loop_length) {
  ci_data[[i]] <- data.frame(
    pupilmatchingrefanonymous = pmr_vector[factor_var_int == i]
  )
  
  ci_data[[i]]$y7 <- as.integer(NA)
  ci_data[[i]]$y8 <- as.integer(NA)
  ci_data[[i]]$y9 <- as.integer(NA)
  ci_data[[i]]$y10 <- as.integer(NA)
  ci_data[[i]]$y11 <- as.integer(NA)
  
  for (j in 2:5) {
    ci_data[[i]][, j + 1] <- ci_data[[i]]$pupilmatchingrefanonymous %in%
      tmp[year <= j + 6 & not_enrolled == 1 & no_gcses == 1]$pupilmatchingrefanonymous
  }
  
  ci_data[[i]] <- data.table(ci_data[[i]])
}

rm(i, j, pmr_vector, factor_var, factor_var_int, loop_length)

ci_output <- create_ci_output(ci_data = ci_data, names_for_cols = levels(cohort_all$exposure_highest_4_grp_y46), perc_round = 1)
ci_output

write.csv(ci_output, "OUTPUTS/CHAPTER 5/not-enrolled-cumulative-by-exp-not-special-no-gcses.csv")

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

ci_output_p[year %in% c(8, 9) & Exposure %in% c("CPP", "CLA"), ]$ll <- 0

ggplot(data = ci_output_p) +
  geom_line(aes(x = year, y = Perc, colour = Exposure), size = 1) +
  geom_point(aes(x = year, y = Perc, colour = Exposure), size = 2,
             position = position_dodge(0.05)) +
  geom_errorbar(aes(x = year, ymin = ll, ymax = ul, colour = Exposure),
                width = 0.3,
                position = position_dodge(0.05)) +
  xlab("Year") +
  ylab("Cumulative not enrolled & sat < 5 GCSEs year 11 (%)") +
  ggtitle("Not special schools") +
  scale_y_continuous(limits = c(0, 20),
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
        legend.position = c(0.25, 0.7))

rm(ci_data, ci_output, ci_output_p, tmp)

# ** table by all other RFs -----------------------------------------------

exposures <- c("female_clean", "eth_major", "language_clean", "idaci_quintiles", "fsmeligible", "idaci_fsm", "region_major",
               "ever_sen_primary", "highest_ever_sen_primary",
               "appru")

outcome <- "not_enrolled_y811"

final_output <- cbind(describe_outcome(exposure = exposures, outcome = outcome, dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T], p_round = 1),
                      describe_outcome(exposure = exposures, outcome = outcome, dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F], p_round = 1),
                      describe_outcome(exposure = exposures, outcome = outcome, dat = cohort_all[year == 7 & sch_n_in_year == 1], p_round = 1))
colnames(final_output) <- c("Special school yr 7", "Not special school yr 7", "Combined")
write.csv(final_output, file = "OUTPUTS/CHAPTER 5/cumulative-not-enrolled-risk-factors.csv")
rm(exposures, outcome, final_output)

# * never return ----------------------------------------------------------

cohort_all[, not_enrolled_all_y891011 := not_enrolled_y8 & not_enrolled_y9 & not_enrolled_y10 & not_enrolled_y11]
cohort_all[, not_enrolled_all_y91011 := not_enrolled_y9 & not_enrolled_y10 & not_enrolled_y11]
cohort_all[, not_enrolled_all_y1011 := not_enrolled_y10 & not_enrolled_y11]

table(cohort_all[year == 7 & sch_n_in_year == 1]$not_enrolled_all_y891011)
table(cohort_all[year == 7 & sch_n_in_year == 1]$not_enrolled_all_y91011)
table(cohort_all[year == 7 & sch_n_in_year == 1]$not_enrolled_all_y1011)
table(cohort_all[year == 7 & sch_n_in_year == 1]$not_enrolled_y11)

table(cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F]$not_enrolled_all_y891011)
table(cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F]$not_enrolled_all_y91011)
table(cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F]$not_enrolled_all_y1011)
table(cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F]$not_enrolled_y11)

table(cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T]$not_enrolled_all_y891011)
table(cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T]$not_enrolled_all_y91011)
table(cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T]$not_enrolled_all_y1011)
table(cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T]$not_enrolled_y11)

outcomes <- c("not_enrolled_all_y891011",
              "not_enrolled_all_y91011",
              "not_enrolled_all_y1011",
              "not_enrolled_y11")

final_output <- cbind(rbind(describe_outcome(exposure = outcomes[1], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F], p_round = 1),
                            describe_outcome(exposure = outcomes[2], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F], p_round = 1),
                            describe_outcome(exposure = outcomes[3], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F], p_round = 1),
                            describe_outcome(exposure = outcomes[4], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F], p_round = 1)),
                      rbind(describe_outcome(exposure = outcomes[1], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T], p_round = 1),
                            describe_outcome(exposure = outcomes[2], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T], p_round = 1),
                            describe_outcome(exposure = outcomes[3], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T], p_round = 1),
                            describe_outcome(exposure = outcomes[4], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T], p_round = 1)))
colnames(final_output) <- c("Not special school yr 7", "Special school yr 7")
write.csv(final_output, file = "OUTPUTS/CHAPTER 5/not-enrolled-not-return-total.csv")

exposures <- c("exposure_highest_4_grp_y46",
               "female_clean", "eth_major", "language_clean", "idaci_quintiles", "fsmeligible", "idaci_fsm", "region_major",
               "ever_sen_primary", "highest_ever_sen_primary",
               "appru")

final_output <- cbind(
  describe_outcome(exposure = exposures, outcome = outcomes[1], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F], p_round = 1),
  describe_outcome(exposure = exposures, outcome = outcomes[2], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F], p_round = 1),
  describe_outcome(exposure = exposures, outcome = outcomes[3], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F], p_round = 1),
  describe_outcome(exposure = exposures, outcome = outcomes[4], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F], p_round = 1)
)

write.csv(final_output, file = "OUTPUTS/CHAPTER 5/not-enrolled-not-return-by-RFs-not-special.csv")

final_output <- cbind(
  describe_outcome(exposure = exposures, outcome = outcomes[1], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T], p_round = 1),
  describe_outcome(exposure = exposures, outcome = outcomes[2], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T], p_round = 1),
  describe_outcome(exposure = exposures, outcome = outcomes[3], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T], p_round = 1),
  describe_outcome(exposure = exposures, outcome = outcomes[4], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T], p_round = 1)
)

write.csv(final_output, file = "OUTPUTS/CHAPTER 5/not-enrolled-not-return-by-RFs-special.csv")

# ENROLMENT SEQUENCE ANALYSIS ---------------------------------------------

pupilschools <- data.table(
  pupilmatchingrefanonymous = cohort_all[sch_n_in_year == 1]$pupilmatchingrefanonymous,
  year = cohort_all[sch_n_in_year == 1]$year,
  female_clean = cohort_all[sch_n_in_year == 1]$female_clean,
  eth_major = cohort_all[sch_n_in_year == 1]$eth_major,
  language_clean = cohort_all[sch_n_in_year == 1]$language_clean,
  highest_ever_sen_primary = cohort_all[sch_n_in_year == 1]$highest_ever_sen_primary,
  ever_sen_primary = cohort_all[sch_n_in_year == 1]$ever_sen_primary,
  highest_ever_sen_y09 = cohort_all[sch_n_in_year == 1]$highest_ever_sen_y09,
  ever_sen_y09 = cohort_all[sch_n_in_year == 1]$ever_sen_y09,
  idaci_quintiles = cohort_all[sch_n_in_year == 1]$idaci_quintiles,
  fsmeligible = cohort_all[sch_n_in_year == 1]$fsmeligible,
  idaci_fsm = cohort_all[sch_n_in_year == 1]$idaci_fsm,
  exposure_highest_4_grp_y46 = cohort_all[sch_n_in_year == 1]$exposure_highest_4_grp_y46,
  exposure_highest_4_grp_y49 = cohort_all[sch_n_in_year == 1]$exposure_highest_4_grp_y49,
  school_type_with_secondary = cohort_all[sch_n_in_year == 1]$school_type_with_secondary,
  appru = cohort_all[sch_n_in_year == 1]$appru,
  appru_y711 = cohort_all[sch_n_in_year == 1]$appru_y711,
  special_school = cohort_all[sch_n_in_year == 1]$special_school,
  special_school_y711 = cohort_all[sch_n_in_year == 1]$special_school_y711,
  not_enrolled_y89 = cohort_all[sch_n_in_year == 1]$not_enrolled_y89,
  not_enrolled_y1011 = cohort_all[sch_n_in_year == 1]$not_enrolled_y1011,
  no_gcses = cohort_all[sch_n_in_year == 1]$no_gcses
)

pupilschools <- pupilschools[year >= 7]
pupilschools <- data.table(complete(pupilschools, pupilmatchingrefanonymous, year))

pupilschools[is.na(school_type_with_secondary), school_type_with_secondary := "NotEnrolled"]
pupilschools[school_type_with_secondary == "Mainstream - LA", school_type_with_secondary := "MainstreamLA"]
pupilschools[school_type_with_secondary == "Mainstream - Academy etc", school_type_with_secondary := "MainstreamAc"]
pupilschools[school_type_with_secondary == "Special & AP/PRU", school_type_with_secondary := "SpecialAPPRU"]
pupilschools[school_type_with_secondary == "Mainstream - LA & AP/PRU", school_type_with_secondary := "MainstreamLAAPPRU"]
pupilschools[school_type_with_secondary == "Mainstream - Academy etc & AP/PRU", school_type_with_secondary := "MainstreamAcAPPRU"]
pupilschools[school_type_with_secondary == "AP/PRU", school_type_with_secondary := "APPRU"]
pupilschools[, school_type_with_secondary := factor(school_type_with_secondary)]

table(pupilschools$school_type_with_secondary, useNA = "always")

pupilschools[, school_type_with_secondary_6_groups := school_type_with_secondary]
pupilschools[school_type_with_secondary %in% c("MainstreamLA", "MainstreamAc"),
             school_type_with_secondary_6_groups := "Mainstream"]
pupilschools[school_type_with_secondary %in% c("MainstreamLAAPPRU", "MainstreamAcAPPRU"),
             school_type_with_secondary_6_groups := "MainstreamAPPRU"]
pupilschools[, school_type_with_secondary_6_groups := factor(school_type_with_secondary_6_groups)]

table(pupilschools$school_type_with_secondary_6_groups, useNA = "always")

pupilschools[, school_type_with_secondary_5_groups := school_type_with_secondary_6_groups]
pupilschools[school_type_with_secondary %in% c("Special", "SpecialAPPRU"),
             school_type_with_secondary_5_groups := "Special&SpecialAPPRU"]
pupilschools[, school_type_with_secondary_5_groups := factor(school_type_with_secondary_5_groups)]

table(pupilschools$school_type_with_secondary_5_groups, useNA = "always")

pupilschools[, school_type_with_secondary_4_groups := school_type_with_secondary_5_groups]
pupilschools[school_type_with_secondary_5_groups %in% c("Mainstream", "MainstreamAPPRU"),
             school_type_with_secondary_4_groups := "Mainstream&MainstreamAPPRU"]
pupilschools[, school_type_with_secondary_4_groups := factor(school_type_with_secondary_4_groups)]

table(pupilschools$school_type_with_secondary_4_groups, useNA = "always")

# * main  ------------------------------------------------------------------

# recast to wide
seq_matrix <- reshape(pupilschools[, c("pupilmatchingrefanonymous", "year", "school_type_with_secondary_4_groups")],
                      idvar = "pupilmatchingrefanonymous",
                      timevar =  "year",
                      direction = "wide")

colnames(seq_matrix) <- c("pupilmatchingrefanonymous", paste0("y", 7:11))

# 1% sample
set.seed(97)
seq_matrix$seq_sample <- rbinom(nrow(seq_matrix), 1, 0.01)
seq_matrix <- seq_matrix[seq_sample == 1]
seq_matrix[, seq_sample := NULL]

# create sequence matrix
seq <- seqdef(seq_matrix, var = 2:6)

# clustering
ccost <- seqsubm(seq, method = "CONSTANT", cval = 2)
hamming <- seqdist(seq, method = "HAM", sm = ccost)
cluster_ward_hamming <- hclust(as.dist(hamming), method = "ward.D2")

write.csv(cluster_ward_hamming$height[cluster_ward_hamming$height > 0],
          file = "OUTPUTS/CHAPTER 5/school_seq/main_4_group/dendogram_heights.csv")

plot(cluster_ward_hamming, labels = FALSE, xlab = "", sub = "")

cluster_list <- list()
for(i in 1:20) {
  cluster_list[[i]] <- cutree(cluster_ward_hamming, k = i)
}

# sequence index plots
for (i in 1:20) {
  png(paste0("OUTPUTS/CHAPTER 5/school_seq/main_4_group/si_plot_", i, "_groups_hamming_ward.png"),
      width = 530, height = 2000, units = "px")
  seqiplot(seq,
           group = cluster_list[[i]],
           idxs = "I",
           border = NA,
           space = 0,
           with.legend = T,
           sortv = "from.start")
  dev.off()
}

# ch index
ch <- vector()
pb <- txtProgressBar(min = 2, max = 20, style = 3)
for (i in 2:20) {
  ch[i - 1] <- calinhara(as.dist(hamming), cluster_list[[i]])
  setTxtProgressBar(pb, i)
}

write.csv(ch, "OUTPUTS/CHAPTER 5/school_seq/main_4_group/ch_hamming_ward.csv")

tiff("OUTPUTS/CHAPTER 5/school_seq/main_4_group/chindx_and_dendrogram.tiff",
     width = 12, height = 6, units = "in", res = 196)
par(mfrow = c(1, 2))
plot(cluster_ward_hamming, labels = FALSE, xlab = "", sub = "")
plot(ch,
     type = "l",
     main = "Cali\u0144ski-Harabasz Index",
     xaxt = "n",
     xlab = "Number of clusters",
     ylab = "Index")
axis(1, at = 1:19, labels = 2:20, cex.axis = 0.7)
dev.off()

# get decent SI plot
tiff(paste0("OUTPUTS/CHAPTER 5/school_seq/main_4_group/si_plot_4_pretty.tiff"),
    width = 6, height = 6, units = "in", res = 300)
seqiplot(seq,
         group = cluster_list[[4]],
         idxs = "I",
         border = NA,
         space = 0,
         with.legend = T,
         sortv = "from.start",
         #ylab = "Child Number",
         xlab = "School year",
         xtlab = paste0("Yr ", 7:11),
         ltext = c("AP/PRU", "Mainstream +/- AP/PRU", "Not enrolled", "Special +/- AP/PRU"))
dev.off()

# merge to pupilschools dataframe chosen clusters
seq_matrix$seq_main_4g <- cluster_list[[4]]

pupilschools <- merge(pupilschools,
                      seq_matrix[, c("pupilmatchingrefanonymous",
                                     "seq_main_10g")],
                      by = "pupilmatchingrefanonymous",
                      all.x = T)

table(cluster_list[[4]])
table(pupilschools[year == 7]$seq_main_10g, useNA = "always")

# remove
rm(ch, i, seq_matrix, seq, ccost, cluster_ward_hamming, cluster_list, hamming, pb)

# describe
output_data <- pupilschools[year == 7 & !is.na(seq_main_10g)]

vars <- c("female_clean", "eth_major", "language_clean", "idaci_quintiles",
          "school_type_with_secondary_5_groups",
          "exposure_highest_4_grp_y46",
          "highest_ever_sen_primary")

write.csv(print(CreateTableOne(data = output_data,
                               vars = vars,
                               factorVars = vars,
                               strata = "seq_main_4g",
                               test = F,
                               includeNA = T),
                showAllLevels = T,
                printToggle = F,
                noSpaces = T),
          file = "OUTPUTS/CHAPTER 5/school_seq/main_4_group/group-chars-4_unrounded.csv")

final_output <- rbind(describe_outcome(exposure = vars[1], outcome = NULL, dat = output_data[seq_main_4g == 1]),
                      describe_outcome(exposure = vars[2], outcome = NULL, dat = output_data[seq_main_4g == 1]),
                      describe_outcome(exposure = vars[3], outcome = NULL, dat = output_data[seq_main_4g == 1]),
                      describe_outcome(exposure = vars[4], outcome = NULL, dat = output_data[seq_main_4g == 1]),
                      describe_outcome(exposure = vars[5], outcome = NULL, dat = output_data[seq_main_4g == 1]),
                      describe_outcome(exposure = vars[6], outcome = NULL, dat = output_data[seq_main_4g == 1]),
                      describe_outcome(exposure = vars[7], outcome = NULL, dat = output_data[seq_main_4g == 1]))

for (i in 2:10) {
  final_output <- cbind(final_output,
                        rbind(describe_outcome(exposure = vars[1], outcome = NULL, dat = output_data[seq_main_4g == i]),
                              describe_outcome(exposure = vars[2], outcome = NULL, dat = output_data[seq_main_4g == i]),
                              describe_outcome(exposure = vars[3], outcome = NULL, dat = output_data[seq_main_4g == i]),
                              describe_outcome(exposure = vars[4], outcome = NULL, dat = output_data[seq_main_4g == i]),
                              describe_outcome(exposure = vars[5], outcome = NULL, dat = output_data[seq_main_4g == i]),
                              describe_outcome(exposure = vars[6], outcome = NULL, dat = output_data[seq_main_4g == i]),
                              describe_outcome(exposure = vars[7], outcome = NULL, dat = output_data[seq_main_4g == i])))
}


write.csv(final_output, file = "OUTPUTS/CHAPTER 5//school_seq/main_4_group/group-chars-4.csv")
rm(vars, final_output, output_data, i)

# * main random draws --------------------------------------------

set.seed(123)
seeds <- sample(1:1000, 10)

pb <- txtProgressBar(min = 0, max = 9, style = 3)
for (i in 0:9) {
  seq_matrix <- reshape(pupilschools[, c("pupilmatchingrefanonymous", "year", "school_type_with_secondary_4_groups")],
                        idvar = "pupilmatchingrefanonymous",
                        timevar =  "year",
                        direction = "wide")
  
  colnames(seq_matrix) <- c("pupilmatchingrefanonymous", paste0("y", 7:11))
  
  set.seed(seeds[i + 1])
  seq_matrix$seq_sample <- rbinom(nrow(seq_matrix), 1, 0.01)
  seq_matrix <- seq_matrix[seq_sample == 1]
  seq_matrix[, seq_sample := NULL]
  
  seq <- seqdef(seq_matrix, var = 2:6)
  ccost <- seqsubm(seq, method = "CONSTANT", cval = 2)
  hamming <- seqdist(seq, method = "HAM", sm = ccost)
  cluster_ward_hamming <- hclust(as.dist(hamming), method = "ward.D2")
  
  png(paste0("OUTPUTS/CHAPTER 5/school_seq/sens_an/0", i, "_dendrogram_hamming_ward.png"))
  plot(cluster_ward_hamming, labels = FALSE)
  dev.off()
  
  cluster_4 <- cutree(cluster_ward_hamming, k = 4)
  
  png(paste0("OUTPUTS/CHAPTER 5/school_seq/sens_an/0", i, "_si_plot_4_groups_hamming_ward.png"),
      width = 530, height = 1000, units = "px")
  seqiplot(seq,
           group = cluster_4,
           idxs = "I",
           border = NA,
           space = 0,
           with.legend = T,
           sortv = "from.start")
  dev.off()
  
  setTxtProgressBar(pb, i)
}

# remove
rm(seq_matrix, seq, ccost, cluster_ward_hamming, cluster_4, hamming, pb, i, seeds)
  
# * with non-enrolment only -----------------------------------------------

pupilschools_non_enrolled <- pupilschools[pupilmatchingrefanonymous %in%
                                            pupilschools[not_enrolled_y89 == T | not_enrolled_y1011 == T]$pupilmatchingrefanonymous]

# recast to wide
seq_matrix <- reshape(pupilschools_non_enrolled[, c("pupilmatchingrefanonymous", "year", "school_type_with_secondary_6_groups")],
                      idvar = "pupilmatchingrefanonymous",
                      timevar =  "year",
                      direction = "wide")

colnames(seq_matrix) <- c("pupilmatchingrefanonymous", paste0("y", 7:11))

# 24% sample
set.seed(97)
seq_matrix$seq_sample <- rbinom(nrow(seq_matrix), 1, 0.24)
seq_matrix <- seq_matrix[seq_sample == 1]
seq_matrix[, seq_sample := NULL]

# create sequence matrix
seq <- seqdef(seq_matrix, var = 2:6)

# clustering
ccost <- seqsubm(seq, method = "CONSTANT", cval = 2)
hamming <- seqdist(seq, method = "HAM", sm = ccost)
cluster_ward_hamming <- hclust(as.dist(hamming), method = "ward.D2")

write.csv(cluster_ward_hamming$height[cluster_ward_hamming$height > 0],
          file = "OUTPUTS/CHAPTER 5/school_seq/non_enrolled/dendogram_heights.csv")

cluster_list <- list()
for(i in 1:20) {
  cluster_list[[i]] <- cutree(cluster_ward_hamming, k = i)
}

plot(cluster_ward_hamming, labels = FALSE)

# sequence index plots
for (i in 1:20) {
  png(paste0("OUTPUTS/CHAPTER 5/school_seq/non_enrolled/si_plot_", i, "_groups_hamming_ward.png"),
      width = 530, height = 2000, units = "px")
  seqiplot(seq,
           group = cluster_list[[i]],
           idxs = "I",
           border = NA,
           space = 0,
           with.legend = T,
           sortv = "from.start")
  dev.off()
}

# ch index
ch <- vector()
pb <- txtProgressBar(min = 2, max = 20, style = 3)
for (i in 2:20) {
  ch[i - 1] <- calinhara(as.dist(hamming), cluster_list[[i]])
  setTxtProgressBar(pb, i)
}

write.csv(ch, "OUTPUTS/CHAPTER 5/school_seq/non_enrolled/ch_hamming_ward.csv")

tiff("OUTPUTS/CHAPTER 5/school_seq/non_enrolled/chindx_and_dendrogram.tiff",
     width = 12, height = 6, units = "in", res = 196)
par(mfrow = c(1, 2))
plot(cluster_ward_hamming, labels = FALSE, xlab = "", sub = "")
plot(ch,
     type = "l",
     main = "Cali\u0144ski-Harabasz Index",
     xaxt = "n",
     xlab = "Number of clusters",
     ylab = "Index")
axis(1, at = 1:19, labels = 2:20, cex.axis = 0.7)
dev.off()

# get decent SI plot
# 15 groups (final solution)
tiff(paste0("OUTPUTS/CHAPTER 5/school_seq/non_enrolled/pretty/si_plot_15_pretty_1to9.tiff"),
     width = 6, height = 14, units = "in", res = 300)
seqiplot(seq[cluster_list[[15]] %in% 1:9, ],
         group = cluster_list[[15]][cluster_list[[15]] %in% 1:9],
         idxs = "I",
         border = NA,
         space = 0,
         with.legend = T,
         sortv = "from.start",
         xlab = "School year",
         xtlab = paste0("Yr ", 7:11),
         ltext = c("AP/PRU", "Mainstream", "Mainstream & AP/PRU", "Not enrolled", "Special", "Special & AP/PRU"))
dev.off()

tiff(paste0("OUTPUTS/CHAPTER 5/school_seq/non_enrolled/pretty/si_plot_15_pretty_10to15.tiff"),
     width = 6, height = 14, units = "in", res = 300)
seqiplot(seq[cluster_list[[15]] %in% 10:15, ],
         group = cluster_list[[15]][cluster_list[[15]] %in% 10:15],
         idxs = "I",
         border = NA,
         space = 0,
         with.legend = T,
         sortv = "from.start",
         xlab = "School year",
         xtlab = paste0("Yr ", 7:11),
         ltext = c("AP/PRU", "Mainstream", "Mainstream & AP/PRU", "Not enrolled", "Special", "Special & AP/PRU"))
dev.off()

# 11
tiff(paste0("OUTPUTS/CHAPTER 5/school_seq/non_enrolled/pretty/si_plot_11_pretty_1to6.tiff"),
     width = 6, height = 14, units = "in", res = 300)
seqiplot(seq[cluster_list[[11]] %in% 1:6, ],
         group = cluster_list[[11]][cluster_list[[11]] %in% 1:6],
         idxs = "I",
         border = NA,
         space = 0,
         with.legend = T,
         sortv = "from.start",
         xlab = "School year",
         xtlab = paste0("Yr ", 7:11),
         ltext = c("AP/PRU", "Mainstream", "Mainstream & AP/PRU", "Not enrolled", "Special", "Special & AP/PRU"))
dev.off()

tiff(paste0("OUTPUTS/CHAPTER 5/school_seq/non_enrolled/pretty/si_plot_11_pretty_7to11.tiff"),
     width = 6, height = 14, units = "in", res = 300)
seqiplot(seq[cluster_list[[11]] %in% 7:11, ],
         group = cluster_list[[11]][cluster_list[[11]] %in% 7:11],
         idxs = "I",
         border = NA,
         space = 0,
         with.legend = T,
         sortv = "from.start",
         xlab = "School year",
         xtlab = paste0("Yr ", 7:11),
         ltext = c("AP/PRU", "Mainstream", "Mainstream & AP/PRU", "Not enrolled", "Special", "Special & AP/PRU"))
dev.off()

# 17
tiff(paste0("OUTPUTS/CHAPTER 5/school_seq/non_enrolled/pretty/si_plot_17_pretty_1to9.tiff"),
     width = 6, height = 14, units = "in", res = 300)
seqiplot(seq[cluster_list[[17]] %in% 1:9, ],
         group = cluster_list[[17]][cluster_list[[17]] %in% 1:9],
         idxs = "I",
         border = NA,
         space = 0,
         with.legend = T,
         sortv = "from.start",
         xlab = "School year",
         xtlab = paste0("Yr ", 7:11),
         ltext = c("AP/PRU", "Mainstream", "Mainstream & AP/PRU", "Not enrolled", "Special", "Special & AP/PRU"))
dev.off()

tiff(paste0("OUTPUTS/CHAPTER 5/school_seq/non_enrolled/pretty/si_plot_17_pretty_10to17.tiff"),
     width = 6, height = 14, units = "in", res = 300)
seqiplot(seq[cluster_list[[17]] %in% 10:17, ],
         group = cluster_list[[17]][cluster_list[[17]] %in% 10:17],
         idxs = "I",
         border = NA,
         space = 0,
         with.legend = T,
         sortv = "from.start",
         xlab = "School year",
         xtlab = paste0("Yr ", 7:11),
         ltext = c("AP/PRU", "Mainstream", "Mainstream & AP/PRU", "Not enrolled", "Special", "Special & AP/PRU"))
dev.off()

# 20
tiff(paste0("OUTPUTS/CHAPTER 5/school_seq/non_enrolled/pretty/si_plot_20_pretty_1to7.tiff"),
     width = 6, height = 14, units = "in", res = 300)
seqiplot(seq[cluster_list[[20]] %in% 1:7, ],
         group = cluster_list[[20]][cluster_list[[20]] %in% 1:7],
         idxs = "I",
         border = NA,
         space = 0,
         with.legend = T,
         sortv = "from.start",
         xlab = "School year",
         xtlab = paste0("Yr ", 7:11),
         ltext = c("AP/PRU", "Mainstream", "Mainstream & AP/PRU", "Not enrolled", "Special", "Special & AP/PRU"))
dev.off()

tiff(paste0("OUTPUTS/CHAPTER 5/school_seq/non_enrolled/pretty/si_plot_20_pretty_8to14.tiff"),
     width = 6, height = 14, units = "in", res = 300)
seqiplot(seq[cluster_list[[20]] %in% 8:14, ],
         group = cluster_list[[20]][cluster_list[[20]] %in% 8:14],
         idxs = "I",
         border = NA,
         space = 0,
         with.legend = T,
         sortv = "from.start",
         xlab = "School year",
         xtlab = paste0("Yr ", 7:11),
         ltext = c("AP/PRU", "Mainstream", "Mainstream & AP/PRU", "Not enrolled", "Special", "Special & AP/PRU"))
dev.off()

tiff(paste0("OUTPUTS/CHAPTER 5/school_seq/non_enrolled/pretty/si_plot_20_pretty_15to20.tiff"),
     width = 6, height = 14, units = "in", res = 300)
seqiplot(seq[cluster_list[[20]] %in% 15:20, ],
         group = cluster_list[[20]][cluster_list[[20]] %in% 15:20],
         idxs = "I",
         border = NA,
         space = 0,
         with.legend = T,
         sortv = "from.start",
         xlab = "School year",
         xtlab = paste0("Yr ", 7:11),
         ltext = c("AP/PRU", "Mainstream", "Mainstream & AP/PRU", "Not enrolled", "Special", "Special & AP/PRU"))
dev.off()

# merge to pupilschools dataframe chosen clusters
seq_matrix$seq_ns_15g <- cluster_list[[15]]

pupilschools <- merge(pupilschools,
                      seq_matrix[, c("pupilmatchingrefanonymous",
                                     "seq_ns_15g")],
                      by = "pupilmatchingrefanonymous",
                      all.x = T)

table(cluster_list[[15]])
table(pupilschools[year == 7]$seq_ns_15g, useNA = "always")

# remove
rm(ch, i, seq_matrix, seq, ccost, cluster_ward_hamming, cluster_list, hamming, pb)
 
# describe
output_data <- pupilschools[year == 7 & !is.na(seq_ns_15g)]
vars <- c("female_clean", "eth_major", "language_clean", "idaci_quintiles", "fsmeligible",
          "appru_y711", "special_school_y711",
          "exposure_highest_4_grp_y46",
          "highest_ever_sen_primary",
          "ever_sen_primary",
          "no_gcses")

write.csv(print(CreateTableOne(data = output_data,
                               vars = vars,
                               factorVars = vars,
                               strata = "seq_ns_15g",
                               test = F,
                               includeNA = T),
                showAllLevels = T,
                printToggle = F,
                noSpaces = T),
          file = "OUTPUTS/CHAPTER 5/school_seq/non_enrolled/group-chars-15_unrounded.csv")

rm(vars, final_output, output_data, i, pupilschools_non_enrolled)

# * CSC only --------------------------------------------------------------

pupilschools_csc <- pupilschools[pupilmatchingrefanonymous %in%
                                                pupilschools[exposure_highest_4_grp_y46 != "None"]$pupilmatchingrefanonymous]

# recast to wide
seq_matrix <- reshape(pupilschools_csc[, c("pupilmatchingrefanonymous", "year", "school_type_with_secondary_6_groups")],
                      idvar = "pupilmatchingrefanonymous",
                      timevar =  "year",
                      direction = "wide")

colnames(seq_matrix) <- c("pupilmatchingrefanonymous", paste0("y", 7:11))

# 12% sample
set.seed(1937)
seq_matrix$seq_sample <- rbinom(nrow(seq_matrix), 1, 0.12)
seq_matrix <- seq_matrix[seq_sample == 1]
seq_matrix[, seq_sample := NULL]

# create sequence matrix
seq <- seqdef(seq_matrix, var = 2:6)

# clustering
ccost <- seqsubm(seq, method = "CONSTANT", cval = 2)
hamming <- seqdist(seq, method = "HAM", sm = ccost)
cluster_ward_hamming <- hclust(as.dist(hamming), method = "ward.D2")

write.csv(cluster_ward_hamming$height[cluster_ward_hamming$height > 0],
          file = "OUTPUTS/CHAPTER 5/school_seq/csc/dendogram_heights.csv")

cluster_list <- list()
for(i in 1:20) {
  cluster_list[[i]] <- cutree(cluster_ward_hamming, k = i)
}

plot(cluster_ward_hamming, labels = FALSE)

# sequence index plots
for (i in 1:20) {
  png(paste0("OUTPUTS/CHAPTER 5/school_seq/csc/si_plot_", i, "_groups_hamming_ward.png"),
      width = 530, height = 2000, units = "px")
  seqiplot(seq,
           group = cluster_list[[i]],
           idxs = "I",
           border = NA,
           space = 0,
           with.legend = T,
           sortv = "from.start")
  dev.off()
}

# ch index
ch <- vector()
pb <- txtProgressBar(min = 2, max = 20, style = 3)
for (i in 2:20) {
  ch[i - 1] <- calinhara(as.dist(hamming), cluster_list[[i]])
  setTxtProgressBar(pb, i)
}

write.csv(ch, "OUTPUTS/CHAPTER 5/school_seq/csc/ch_hamming_ward.csv")

tiff("OUTPUTS/CHAPTER 5/school_seq/csc/chindx_and_dendrogram.tiff",
     width = 12, height = 6, units = "in", res = 196)
par(mfrow = c(1, 2))
plot(cluster_ward_hamming, labels = FALSE, xlab = "", sub = "")
plot(ch,
     type = "l",
     main = "Cali\u0144ski-Harabasz Index",
     xaxt = "n",
     xlab = "Number of clusters",
     ylab = "Index")
axis(1, at = 1:19, labels = 2:20, cex.axis = 0.7)
dev.off()

# 3, 10 and 13 all have local maxima
# get decent SI plots
# 3
tiff(paste0("OUTPUTS/CHAPTER 5/school_seq/csc/pretty/si_plot_3_pretty.tiff"),
     width = 6, height = 6, units = "in", res = 300)
seqiplot(seq[cluster_list[[3]] %in% 1:3, ],
         group = cluster_list[[3]],
         idxs = "I",
         border = NA,
         space = 0,
         with.legend = T,
         sortv = "from.start",
         xlab = "School year",
         xtlab = paste0("Yr ", 7:11),
         ltext = c("AP/PRU", "Mainstream", "Mainstream & AP/PRU", "Not enrolled", "Special", "Special & AP/PRU"))
dev.off()

# 10
tiff(paste0("OUTPUTS/CHAPTER 5/school_seq/csc/pretty/si_plot_10_pretty_1to5.tiff"),
     width = 6, height = 14, units = "in", res = 300)
seqiplot(seq[cluster_list[[10]] %in% 1:5, ],
         group = cluster_list[[10]][cluster_list[[10]] %in% 1:5],
         idxs = "I",
         border = NA,
         space = 0,
         with.legend = T,
         sortv = "from.start",
         xlab = "School year",
         xtlab = paste0("Yr ", 7:11),
         ltext = c("AP/PRU", "Mainstream", "Mainstream & AP/PRU", "Not enrolled", "Special", "Special & AP/PRU"))
dev.off()

tiff(paste0("OUTPUTS/CHAPTER 5/school_seq/csc/pretty/si_plot_10_pretty_6to10.tiff"),
     width = 6, height = 14, units = "in", res = 300)
seqiplot(seq[cluster_list[[10]] %in% 6:10, ],
         group = cluster_list[[10]][cluster_list[[10]] %in% 6:10],
         idxs = "I",
         border = NA,
         space = 0,
         with.legend = T,
         sortv = "from.start",
         xlab = "School year",
         xtlab = paste0("Yr ", 7:11),
         ltext = c("AP/PRU", "Mainstream", "Mainstream & AP/PRU", "Not enrolled", "Special", "Special & AP/PRU"))
dev.off()

# 18
tiff(paste0("OUTPUTS/CHAPTER 5/school_seq/csc/pretty/si_plot_18_pretty_1to9.tiff"),
     width = 6, height = 14, units = "in", res = 300)
seqiplot(seq[cluster_list[[18]] %in% 1:9, ],
         group = cluster_list[[18]][cluster_list[[18]] %in% 1:9],
         idxs = "I",
         border = NA,
         space = 0,
         with.legend = T,
         sortv = "from.start",
         xlab = "School year",
         xtlab = paste0("Yr ", 7:11),
         ltext = c("AP/PRU", "Mainstream", "Mainstream & AP/PRU", "Not enrolled", "Special", "Special & AP/PRU"))
dev.off()

tiff(paste0("OUTPUTS/CHAPTER 5/school_seq/csc/pretty/si_plot_15_pretty_10to18.tiff"),
     width = 6, height = 14, units = "in", res = 300)
seqiplot(seq[cluster_list[[18]] %in% 10:18, ],
         group = cluster_list[[18]][cluster_list[[18]] %in% 10:18],
         idxs = "I",
         border = NA,
         space = 0,
         with.legend = T,
         sortv = "from.start",
         xlab = "School year",
         xtlab = paste0("Yr ", 7:11),
         ltext = c("AP/PRU", "Mainstream", "Mainstream & AP/PRU", "Not enrolled", "Special", "Special & AP/PRU"))
dev.off()

# merge to pupilschools dataframe chosen clusters
seq_matrix$seq_csc_3g <- cluster_list[[3]]
seq_matrix$seq_csc_10g <- cluster_list[[10]]
seq_matrix$seq_csc_18g <- cluster_list[[18]]

pupilschools <- merge(pupilschools,
                      seq_matrix[, c("pupilmatchingrefanonymous",
                                     "seq_csc_3g",
                                     "seq_csc_10g",
                                     "seq_csc_18g")],
                      by = "pupilmatchingrefanonymous",
                      all.x = T)

table(cluster_list[[3]])
table(pupilschools[year == 7]$seq_csc_3g, useNA = "always")

table(cluster_list[[10]])
table(pupilschools[year == 7]$seq_csc_10g, useNA = "always")

table(cluster_list[[18]])
table(pupilschools[year == 7]$seq_csc_18g, useNA = "always")

# remove
rm(ch, i, seq_matrix, seq, ccost, cluster_ward_hamming, cluster_list, hamming, pb)

# describe - 3 groups
output_data <- pupilschools[year == 7 & !is.na(seq_csc_3g)]
vars <- c("female_clean", "eth_major", "language_clean", "idaci_quintiles", "fsmeligible",
          "appru_y711", "special_school_y711",
          "exposure_highest_4_grp_y46",
          "highest_ever_sen_primary",
          "ever_sen_primary",
          "no_gcses")

write.csv(print(CreateTableOne(data = output_data,
                               vars = vars,
                               factorVars = vars,
                               strata = "seq_csc_3g",
                               test = F,
                               includeNA = T),
                showAllLevels = T,
                printToggle = F,
                noSpaces = T),
          file = "OUTPUTS/CHAPTER 5/school_seq/csc/chars/group-chars-3.csv")

output_data <- pupilschools[year == 7 & !is.na(seq_csc_10g)]
write.csv(print(CreateTableOne(data = output_data,
                               vars = vars,
                               factorVars = vars,
                               strata = "seq_csc_10g",
                               test = F,
                               includeNA = T),
                showAllLevels = T,
                printToggle = F,
                noSpaces = T),
          file = "OUTPUTS/CHAPTER 5/school_seq/csc/chars/group-chars-10.csv")

output_data <- pupilschools[year == 7 & !is.na(seq_csc_18g)]
write.csv(print(CreateTableOne(data = output_data,
                               vars = vars,
                               factorVars = vars,
                               strata = "seq_csc_18g",
                               test = F,
                               includeNA = T),
                showAllLevels = T,
                printToggle = F,
                noSpaces = T),
          file = "OUTPUTS/CHAPTER 5/school_seq/csc/chars/group-chars-18.csv")

rm(vars, final_output, output_data, i, pupilschools_csc)

# * CSC with non-enrolment only -----------------------------------------------

pupilschools_csc_non_enrolled <- pupilschools[pupilmatchingrefanonymous %in%
                                            pupilschools[(not_enrolled_y89 == T | not_enrolled_y1011 == T) &
                                                           exposure_highest_4_grp_y46 != "None"]$pupilmatchingrefanonymous]

# recast to wide
seq_matrix <- reshape(pupilschools_csc_non_enrolled[, c("pupilmatchingrefanonymous", "year", "school_type_with_secondary_6_groups")],
                      idvar = "pupilmatchingrefanonymous",
                      timevar =  "year",
                      direction = "wide")

colnames(seq_matrix) <- c("pupilmatchingrefanonymous", paste0("y", 7:11))

# create sequence matrix
seq <- seqdef(seq_matrix, var = 2:6)

# clustering
ccost <- seqsubm(seq, method = "CONSTANT", cval = 2)
hamming <- seqdist(seq, method = "HAM", sm = ccost)
cluster_ward_hamming <- hclust(as.dist(hamming), method = "ward.D2")

write.csv(cluster_ward_hamming$height[cluster_ward_hamming$height > 0],
          file = "OUTPUTS/CHAPTER 5/school_seq/csc_non_enrolled/dendogram_heights.csv")

cluster_list <- list()
for(i in 1:20) {
  cluster_list[[i]] <- cutree(cluster_ward_hamming, k = i)
}

plot(cluster_ward_hamming, labels = FALSE)

# sequence index plots
for (i in 1:20) {
  png(paste0("OUTPUTS/CHAPTER 5/school_seq/csc_non_enrolled/si_plot_", i, "_groups_hamming_ward.png"),
      width = 530, height = 2000, units = "px")
  seqiplot(seq,
           group = cluster_list[[i]],
           idxs = "I",
           border = NA,
           space = 0,
           with.legend = T,
           sortv = "from.start")
  dev.off()
}

# ch index
ch <- vector()
pb <- txtProgressBar(min = 2, max = 20, style = 3)
for (i in 2:20) {
  ch[i - 1] <- calinhara(as.dist(hamming), cluster_list[[i]])
  setTxtProgressBar(pb, i)
}

write.csv(ch, "OUTPUTS/CHAPTER 5/school_seq/csc_non_enrolled/ch_hamming_ward.csv")

tiff("OUTPUTS/CHAPTER 5/school_seq/csc_non_enrolled/chindx_and_dendrogram.tiff",
     width = 12, height = 6, units = "in", res = 196)
par(mfrow = c(1, 2))
plot(cluster_ward_hamming, labels = FALSE, xlab = "", sub = "")
plot(ch,
     type = "l",
     main = "Cali\u0144ski-Harabasz Index",
     xaxt = "n",
     xlab = "Number of clusters",
     ylab = "Index")
axis(1, at = 1:19, labels = 2:20, cex.axis = 0.7)
dev.off()

# get decent SI plot
# 5 cluster (final solution)
tiff(paste0("OUTPUTS/CHAPTER 5/school_seq/csc_non_enrolled/pretty/si_plot_5_pretty.tiff"),
     width = 6, height = 14, units = "in", res = 300)
seqiplot(seq,
         group = cluster_list[[5]],
         idxs = "I",
         border = NA,
         space = 0,
         with.legend = T,
         sortv = "from.start",
         xlab = "School year",
         xtlab = paste0("Yr ", 7:11),
         ltext = c("AP/PRU", "Mainstream", "Mainstream & AP/PRU", "Not enrolled", "Special", "Special & AP/PRU"))
dev.off()

# 10
tiff(paste0("OUTPUTS/CHAPTER 5/school_seq/csc_non_enrolled/pretty/si_plot_10_pretty_1to5.tiff"),
     width = 6, height = 14, units = "in", res = 300)
seqiplot(seq[cluster_list[[10]] %in% 1:5, ],
         group = cluster_list[[10]][cluster_list[[10]] %in% 1:5],
         idxs = "I",
         border = NA,
         space = 0,
         with.legend = T,
         sortv = "from.start",
         xlab = "School year",
         xtlab = paste0("Yr ", 7:11),
         ltext = c("AP/PRU", "Mainstream", "Mainstream & AP/PRU", "Not enrolled", "Special", "Special & AP/PRU"))
dev.off()

tiff(paste0("OUTPUTS/CHAPTER 5/school_seq/csc_non_enrolled/pretty/si_plot_10_pretty_6to10.tiff"),
     width = 6, height = 14, units = "in", res = 300)
seqiplot(seq[cluster_list[[10]] %in% 6:10, ],
         group = cluster_list[[10]][cluster_list[[10]] %in% 6:10],
         idxs = "I",
         border = NA,
         space = 0,
         with.legend = T,
         sortv = "from.start",
         xlab = "School year",
         xtlab = paste0("Yr ", 7:11),
         ltext = c("AP/PRU", "Mainstream", "Mainstream & AP/PRU", "Not enrolled", "Special", "Special & AP/PRU"))
dev.off()

# 14
tiff(paste0("OUTPUTS/CHAPTER 5/school_seq/csc_non_enrolled/pretty/si_plot_14_pretty_1to7.tiff"),
     width = 6, height = 14, units = "in", res = 300)
seqiplot(seq[cluster_list[[14]] %in% 1:7, ],
         group = cluster_list[[14]][cluster_list[[14]] %in% 1:7],
         idxs = "I",
         border = NA,
         space = 0,
         with.legend = T,
         sortv = "from.start",
         xlab = "School year",
         xtlab = paste0("Yr ", 7:11),
         ltext = c("AP/PRU", "Mainstream", "Mainstream & AP/PRU", "Not enrolled", "Special", "Special & AP/PRU"))
dev.off()

tiff(paste0("OUTPUTS/CHAPTER 5/school_seq/csc_non_enrolled/pretty/si_plot_14_pretty_8to14.tiff"),
     width = 6, height = 14, units = "in", res = 300)
seqiplot(seq[cluster_list[[14]] %in% 8:14, ],
         group = cluster_list[[14]][cluster_list[[14]] %in% 8:14],
         idxs = "I",
         border = NA,
         space = 0,
         with.legend = T,
         sortv = "from.start",
         xlab = "School year",
         xtlab = paste0("Yr ", 7:11),
         ltext = c("AP/PRU", "Mainstream", "Mainstream & AP/PRU", "Not enrolled", "Special", "Special & AP/PRU"))
dev.off()

 
# merge to pupilschools dataframe chosen clusters
seq_matrix$seq_ns_5g <- cluster_list[[5]]

pupilschools <- merge(pupilschools,
                      seq_matrix[, c("pupilmatchingrefanonymous",
                                     "seq_ns_5g")],
                      by = "pupilmatchingrefanonymous",
                      all.x = T)

table(cluster_list[[5]])
table(pupilschools[year == 7]$seq_ns_5g, useNA = "always")

# remove
rm(ch, i, seq_matrix, seq, ccost, cluster_ward_hamming, cluster_list, hamming, pb)

# describe
output_data <- pupilschools[year == 7 & !is.na(seq_ns_5g)]
vars <- c("female_clean", "eth_major", "language_clean", "idaci_quintiles", "fsmeligible",
          "appru_y711", "special_school_y711",
          "exposure_highest_4_grp_y46",
          "highest_ever_sen_primary",
          "ever_sen_primary",
          "no_gcses")

write.csv(print(CreateTableOne(data = output_data,
                               vars = vars,
                               factorVars = vars,
                               strata = "seq_ns_5g",
                               test = F,
                               includeNA = T),
                showAllLevels = T,
                printToggle = F,
                noSpaces = T),
          file = "OUTPUTS/CHAPTER 5/school_seq/csc_non_enrolled/group-chars-5_unrounded.csv")

rm(vars, final_output, output_data, i, pupilschools_non_enrolled)


# NEW ANALYSES FOR PAPER --------------------------------------------------

# * appru year 7 ----------------------------------------------------------

exposures <- c("appru")
outcomes <- c("not_enrolled_y1011",
              "no_gcses",
              "not_enrolled_y1011_nogcses")

final_output <- cbind(describe_outcome(exposure = exposures, outcome = outcomes[1], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F], p_round = 1),
                      describe_outcome(exposure = exposures, outcome = outcomes[2], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F], p_round = 1),
                      describe_outcome(exposure = exposures, outcome = outcomes[3], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F], p_round = 1))
write.csv(final_output, file = "OUTPUTS/CHAPTER 5/additional/outcomes-by-appru-not-special.csv")

final_output <- cbind(describe_outcome(exposure = exposures, outcome = outcomes[1], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T], p_round = 1),
                      describe_outcome(exposure = exposures, outcome = outcomes[2], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T], p_round = 1),
                      describe_outcome(exposure = exposures, outcome = outcomes[3], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T], p_round = 1))
write.csv(final_output, file = "OUTPUTS/CHAPTER 5/additional/outcomes-by-appru-special.csv")

rm(exposures, outcomes, final_output)

# * cumulative ------------------------------------------------------------

# ** by female ---------------------------------------------------------------

# not special
tmp <- data.table(
  pupilmatchingrefanonymous = cohort_all[year >= 7 & sch_n_in_year == 1 & special_school_y7 == F]$pupilmatchingrefanonymous,
  year = cohort_all[year >= 7 & sch_n_in_year == 1 & special_school_y7 == F]$year,
  female_clean = cohort_all[year >= 7 & sch_n_in_year == 1 & special_school_y7 == F]$female_clean
)

tmp$not_enrolled <- 0
tmp <- data.table(complete(tmp, pupilmatchingrefanonymous, year))
tmp[is.na(not_enrolled)]$not_enrolled <- 1
tmp[, female_clean := female_clean[1], by = .(pupilmatchingrefanonymous)]

pmr_vector <- tmp[year == 7]$pupilmatchingrefanonymous
factor_var <- tmp[year == 7]$female_clean

factor_var <- factor(factor_var)
factor_var_int <- as.integer(factor_var)
loop_length <- dim(table(factor_var))

ci_data <- list()
for (i in 1:loop_length) {
  ci_data[[i]] <- data.frame(
    pupilmatchingrefanonymous = pmr_vector[factor_var_int == i]
  )
  
  ci_data[[i]]$y7 <- as.integer(NA)
  ci_data[[i]]$y8 <- as.integer(NA)
  ci_data[[i]]$y9 <- as.integer(NA)
  ci_data[[i]]$y10 <- as.integer(NA)
  ci_data[[i]]$y11 <- as.integer(NA)
  
  for (j in 2:5) {
    ci_data[[i]][, j + 1] <- ci_data[[i]]$pupilmatchingrefanonymous %in%
      tmp[year <= j + 6 & not_enrolled == 1]$pupilmatchingrefanonymous
  }
  
  ci_data[[i]] <- data.table(ci_data[[i]])
}

var_levels <- levels(factor_var)
rm(i, j, pmr_vector, factor_var, factor_var_int, loop_length)

ci_output <- create_ci_output(ci_data = ci_data, names_for_cols = var_levels, perc_round = 1)
ci_output

write.csv(ci_output, "OUTPUTS/CHAPTER 5/additional/ci-not-enrolled-by-female-not-special.csv")
rm(tmp, var_levels, ci_data, ci_output)

# special
tmp <- data.table(
  pupilmatchingrefanonymous = cohort_all[year >= 7 & sch_n_in_year == 1 & special_school_y7 == T]$pupilmatchingrefanonymous,
  year = cohort_all[year >= 7 & sch_n_in_year == 1 & special_school_y7 == T]$year,
  female_clean = cohort_all[year >= 7 & sch_n_in_year == 1 & special_school_y7 == T]$female_clean
)

tmp$not_enrolled <- 0
tmp <- data.table(complete(tmp, pupilmatchingrefanonymous, year))
tmp[is.na(not_enrolled)]$not_enrolled <- 1
tmp[, female_clean := female_clean[1], by = .(pupilmatchingrefanonymous)]

pmr_vector <- tmp[year == 7]$pupilmatchingrefanonymous
factor_var <- tmp[year == 7]$female_clean

factor_var <- factor(factor_var)
factor_var_int <- as.integer(factor_var)
loop_length <- dim(table(factor_var))

ci_data <- list()
for (i in 1:loop_length) {
  ci_data[[i]] <- data.frame(
    pupilmatchingrefanonymous = pmr_vector[factor_var_int == i]
  )
  
  ci_data[[i]]$y7 <- as.integer(NA)
  ci_data[[i]]$y8 <- as.integer(NA)
  ci_data[[i]]$y9 <- as.integer(NA)
  ci_data[[i]]$y10 <- as.integer(NA)
  ci_data[[i]]$y11 <- as.integer(NA)
  
  for (j in 2:5) {
    ci_data[[i]][, j + 1] <- ci_data[[i]]$pupilmatchingrefanonymous %in%
      tmp[year <= j + 6 & not_enrolled == 1]$pupilmatchingrefanonymous
  }
  
  ci_data[[i]] <- data.table(ci_data[[i]])
}

var_levels <- levels(factor_var)
rm(i, j, pmr_vector, factor_var, factor_var_int, loop_length)

ci_output <- create_ci_output(ci_data = ci_data, names_for_cols = var_levels, perc_round = 1)
ci_output

write.csv(ci_output, "OUTPUTS/CHAPTER 5/additional/ci-not-enrolled-by-female-special.csv")
rm(tmp, var_levels, ci_data, ci_output)

# ** by ethnicity ---------------------------------------------------------

# not special
tmp <- data.table(
  pupilmatchingrefanonymous = cohort_all[year >= 7 & sch_n_in_year == 1 & special_school_y7 == F]$pupilmatchingrefanonymous,
  year = cohort_all[year >= 7 & sch_n_in_year == 1 & special_school_y7 == F]$year,
  eth_major = cohort_all[year >= 7 & sch_n_in_year == 1 & special_school_y7 == F]$eth_major
)

tmp$not_enrolled <- 0
tmp <- data.table(complete(tmp, pupilmatchingrefanonymous, year))
tmp[is.na(not_enrolled)]$not_enrolled <- 1
tmp[, eth_major := eth_major[1], by = .(pupilmatchingrefanonymous)]

pmr_vector <- tmp[year == 7]$pupilmatchingrefanonymous
factor_var <- tmp[year == 7]$eth_major

factor_var <- factor(factor_var)
factor_var_int <- as.integer(factor_var)
loop_length <- dim(table(factor_var))

ci_data <- list()
for (i in 1:loop_length) {
  ci_data[[i]] <- data.frame(
    pupilmatchingrefanonymous = pmr_vector[factor_var_int == i]
  )
  
  ci_data[[i]]$y7 <- as.integer(NA)
  ci_data[[i]]$y8 <- as.integer(NA)
  ci_data[[i]]$y9 <- as.integer(NA)
  ci_data[[i]]$y10 <- as.integer(NA)
  ci_data[[i]]$y11 <- as.integer(NA)
  
  for (j in 2:5) {
    ci_data[[i]][, j + 1] <- ci_data[[i]]$pupilmatchingrefanonymous %in%
      tmp[year <= j + 6 & not_enrolled == 1]$pupilmatchingrefanonymous
  }
  
  ci_data[[i]] <- data.table(ci_data[[i]])
}

var_levels <- levels(factor_var)
rm(i, j, pmr_vector, factor_var, factor_var_int, loop_length)

ci_output <- create_ci_output(ci_data = ci_data, names_for_cols = var_levels, perc_round = 1)
ci_output

write.csv(ci_output, "OUTPUTS/CHAPTER 5/additional/ci-not-enrolled-by-eth-not-special.csv")
rm(tmp, var_levels, ci_data, ci_output)

# special
tmp <- data.table(
  pupilmatchingrefanonymous = cohort_all[year >= 7 & sch_n_in_year == 1 & special_school_y7 == T]$pupilmatchingrefanonymous,
  year = cohort_all[year >= 7 & sch_n_in_year == 1 & special_school_y7 == T]$year,
  eth_major = cohort_all[year >= 7 & sch_n_in_year == 1 & special_school_y7 == T]$eth_major
)

tmp$not_enrolled <- 0
tmp <- data.table(complete(tmp, pupilmatchingrefanonymous, year))
tmp[is.na(not_enrolled)]$not_enrolled <- 1
tmp[, eth_major := eth_major[1], by = .(pupilmatchingrefanonymous)]

pmr_vector <- tmp[year == 7]$pupilmatchingrefanonymous
factor_var <- tmp[year == 7]$eth_major

factor_var <- factor(factor_var)
factor_var_int <- as.integer(factor_var)
loop_length <- dim(table(factor_var))

ci_data <- list()
for (i in 1:loop_length) {
  ci_data[[i]] <- data.frame(
    pupilmatchingrefanonymous = pmr_vector[factor_var_int == i]
  )
  
  ci_data[[i]]$y7 <- as.integer(NA)
  ci_data[[i]]$y8 <- as.integer(NA)
  ci_data[[i]]$y9 <- as.integer(NA)
  ci_data[[i]]$y10 <- as.integer(NA)
  ci_data[[i]]$y11 <- as.integer(NA)
  
  for (j in 2:5) {
    ci_data[[i]][, j + 1] <- ci_data[[i]]$pupilmatchingrefanonymous %in%
      tmp[year <= j + 6 & not_enrolled == 1]$pupilmatchingrefanonymous
  }
  
  ci_data[[i]] <- data.table(ci_data[[i]])
}

var_levels <- levels(factor_var)
rm(i, j, pmr_vector, factor_var, factor_var_int, loop_length)

ci_output <- create_ci_output(ci_data = ci_data, names_for_cols = var_levels, perc_round = 1)
ci_output

write.csv(ci_output, "OUTPUTS/CHAPTER 5/additional/ci-not-enrolled-by-eth-special.csv")
rm(tmp, var_levels, ci_data, ci_output)

# ** by first lang --------------------------------------------------------

# not special
tmp <- data.table(
  pupilmatchingrefanonymous = cohort_all[year >= 7 & sch_n_in_year == 1 & special_school_y7 == F]$pupilmatchingrefanonymous,
  year = cohort_all[year >= 7 & sch_n_in_year == 1 & special_school_y7 == F]$year,
  language_clean = cohort_all[year >= 7 & sch_n_in_year == 1 & special_school_y7 == F]$language_clean
)

tmp$not_enrolled <- 0
tmp <- data.table(complete(tmp, pupilmatchingrefanonymous, year))
tmp[is.na(not_enrolled)]$not_enrolled <- 1
tmp[, language_clean := language_clean[1], by = .(pupilmatchingrefanonymous)]

pmr_vector <- tmp[year == 7]$pupilmatchingrefanonymous
factor_var <- tmp[year == 7]$language_clean

factor_var <- factor(factor_var)
factor_var_int <- as.integer(factor_var)
loop_length <- dim(table(factor_var))

ci_data <- list()
for (i in 1:loop_length) {
  ci_data[[i]] <- data.frame(
    pupilmatchingrefanonymous = pmr_vector[factor_var_int == i]
  )
  
  ci_data[[i]]$y7 <- as.integer(NA)
  ci_data[[i]]$y8 <- as.integer(NA)
  ci_data[[i]]$y9 <- as.integer(NA)
  ci_data[[i]]$y10 <- as.integer(NA)
  ci_data[[i]]$y11 <- as.integer(NA)
  
  for (j in 2:5) {
    ci_data[[i]][, j + 1] <- ci_data[[i]]$pupilmatchingrefanonymous %in%
      tmp[year <= j + 6 & not_enrolled == 1]$pupilmatchingrefanonymous
  }
  
  ci_data[[i]] <- data.table(ci_data[[i]])
}

var_levels <- levels(factor_var)
rm(i, j, pmr_vector, factor_var, factor_var_int, loop_length)

ci_output <- create_ci_output(ci_data = ci_data, names_for_cols = var_levels, perc_round = 1)
ci_output

write.csv(ci_output, "OUTPUTS/CHAPTER 5/additional/ci-not-enrolled-by-lang-not-special.csv")
rm(tmp, var_levels, ci_data, ci_output)

# special
tmp <- data.table(
  pupilmatchingrefanonymous = cohort_all[year >= 7 & sch_n_in_year == 1 & special_school_y7 == T]$pupilmatchingrefanonymous,
  year = cohort_all[year >= 7 & sch_n_in_year == 1 & special_school_y7 == T]$year,
  language_clean = cohort_all[year >= 7 & sch_n_in_year == 1 & special_school_y7 == T]$language_clean
)

tmp$not_enrolled <- 0
tmp <- data.table(complete(tmp, pupilmatchingrefanonymous, year))
tmp[is.na(not_enrolled)]$not_enrolled <- 1
tmp[, language_clean := language_clean[1], by = .(pupilmatchingrefanonymous)]

pmr_vector <- tmp[year == 7]$pupilmatchingrefanonymous
factor_var <- tmp[year == 7]$language_clean

factor_var <- factor(factor_var)
factor_var_int <- as.integer(factor_var)
loop_length <- dim(table(factor_var))

ci_data <- list()
for (i in 1:loop_length) {
  ci_data[[i]] <- data.frame(
    pupilmatchingrefanonymous = pmr_vector[factor_var_int == i]
  )
  
  ci_data[[i]]$y7 <- as.integer(NA)
  ci_data[[i]]$y8 <- as.integer(NA)
  ci_data[[i]]$y9 <- as.integer(NA)
  ci_data[[i]]$y10 <- as.integer(NA)
  ci_data[[i]]$y11 <- as.integer(NA)
  
  for (j in 2:5) {
    ci_data[[i]][, j + 1] <- ci_data[[i]]$pupilmatchingrefanonymous %in%
      tmp[year <= j + 6 & not_enrolled == 1]$pupilmatchingrefanonymous
  }
  
  ci_data[[i]] <- data.table(ci_data[[i]])
}

var_levels <- levels(factor_var)
rm(i, j, pmr_vector, factor_var, factor_var_int, loop_length)

ci_output <- create_ci_output(ci_data = ci_data, names_for_cols = var_levels, perc_round = 1)
ci_output

write.csv(ci_output, "OUTPUTS/CHAPTER 5/additional/ci-not-enrolled-by-lang-special.csv")
rm(tmp, var_levels, ci_data, ci_output)

# ** by IDACI fifths ------------------------------------------------------

# not special
tmp <- data.table(
  pupilmatchingrefanonymous = cohort_all[year >= 7 & sch_n_in_year == 1 & special_school_y7 == F]$pupilmatchingrefanonymous,
  year = cohort_all[year >= 7 & sch_n_in_year == 1 & special_school_y7 == F]$year,
  idaci_quintiles = cohort_all[year >= 7 & sch_n_in_year == 1 & special_school_y7 == F]$idaci_quintiles
)

tmp$not_enrolled <- 0
tmp <- data.table(complete(tmp, pupilmatchingrefanonymous, year))
tmp[is.na(not_enrolled)]$not_enrolled <- 1
tmp[, idaci_quintiles := idaci_quintiles[1], by = .(pupilmatchingrefanonymous)]

pmr_vector <- tmp[year == 7]$pupilmatchingrefanonymous
factor_var <- tmp[year == 7]$idaci_quintiles

factor_var <- factor(factor_var)
factor_var_int <- as.integer(factor_var)
loop_length <- dim(table(factor_var))

ci_data <- list()
for (i in 1:loop_length) {
  ci_data[[i]] <- data.frame(
    pupilmatchingrefanonymous = pmr_vector[factor_var_int == i]
  )
  
  ci_data[[i]]$y7 <- as.integer(NA)
  ci_data[[i]]$y8 <- as.integer(NA)
  ci_data[[i]]$y9 <- as.integer(NA)
  ci_data[[i]]$y10 <- as.integer(NA)
  ci_data[[i]]$y11 <- as.integer(NA)
  
  for (j in 2:5) {
    ci_data[[i]][, j + 1] <- ci_data[[i]]$pupilmatchingrefanonymous %in%
      tmp[year <= j + 6 & not_enrolled == 1]$pupilmatchingrefanonymous
  }
  
  ci_data[[i]] <- data.table(ci_data[[i]])
}

var_levels <- levels(factor_var)
rm(i, j, pmr_vector, factor_var, factor_var_int, loop_length)

ci_output <- create_ci_output(ci_data = ci_data, names_for_cols = var_levels, perc_round = 1)
ci_output

write.csv(ci_output, "OUTPUTS/CHAPTER 5/additional/ci-not-enrolled-by-idaci-not-special.csv")
rm(tmp, var_levels, ci_data, ci_output)

# special
tmp <- data.table(
  pupilmatchingrefanonymous = cohort_all[year >= 7 & sch_n_in_year == 1 & special_school_y7 == T]$pupilmatchingrefanonymous,
  year = cohort_all[year >= 7 & sch_n_in_year == 1 & special_school_y7 == T]$year,
  idaci_quintiles = cohort_all[year >= 7 & sch_n_in_year == 1 & special_school_y7 == T]$idaci_quintiles
)

tmp$not_enrolled <- 0
tmp <- data.table(complete(tmp, pupilmatchingrefanonymous, year))
tmp[is.na(not_enrolled)]$not_enrolled <- 1
tmp[, idaci_quintiles := idaci_quintiles[1], by = .(pupilmatchingrefanonymous)]

pmr_vector <- tmp[year == 7]$pupilmatchingrefanonymous
factor_var <- tmp[year == 7]$idaci_quintiles

factor_var <- factor(factor_var)
factor_var_int <- as.integer(factor_var)
loop_length <- dim(table(factor_var))

ci_data <- list()
for (i in 1:loop_length) {
  ci_data[[i]] <- data.frame(
    pupilmatchingrefanonymous = pmr_vector[factor_var_int == i]
  )
  
  ci_data[[i]]$y7 <- as.integer(NA)
  ci_data[[i]]$y8 <- as.integer(NA)
  ci_data[[i]]$y9 <- as.integer(NA)
  ci_data[[i]]$y10 <- as.integer(NA)
  ci_data[[i]]$y11 <- as.integer(NA)
  
  for (j in 2:5) {
    ci_data[[i]][, j + 1] <- ci_data[[i]]$pupilmatchingrefanonymous %in%
      tmp[year <= j + 6 & not_enrolled == 1]$pupilmatchingrefanonymous
  }
  
  ci_data[[i]] <- data.table(ci_data[[i]])
}

var_levels <- levels(factor_var)
rm(i, j, pmr_vector, factor_var, factor_var_int, loop_length)

ci_output <- create_ci_output(ci_data = ci_data, names_for_cols = var_levels, perc_round = 1)
ci_output

write.csv(ci_output, "OUTPUTS/CHAPTER 5/additional/ci-not-enrolled-by-idaci-special.csv")
rm(tmp, var_levels, ci_data, ci_output)

# ** by FSM ---------------------------------------------------------------

# not special
tmp <- data.table(
  pupilmatchingrefanonymous = cohort_all[year >= 7 & sch_n_in_year == 1 & special_school_y7 == F]$pupilmatchingrefanonymous,
  year = cohort_all[year >= 7 & sch_n_in_year == 1 & special_school_y7 == F]$year,
  fsmeligible = cohort_all[year >= 7 & sch_n_in_year == 1 & special_school_y7 == F]$fsmeligible
)

tmp$not_enrolled <- 0
tmp <- data.table(complete(tmp, pupilmatchingrefanonymous, year))
tmp[is.na(not_enrolled)]$not_enrolled <- 1
tmp[, fsmeligible := fsmeligible[1], by = .(pupilmatchingrefanonymous)]

pmr_vector <- tmp[year == 7]$pupilmatchingrefanonymous
factor_var <- tmp[year == 7]$fsmeligible

factor_var <- factor(factor_var)
factor_var_int <- as.integer(factor_var)
loop_length <- dim(table(factor_var))

ci_data <- list()
for (i in 1:loop_length) {
  ci_data[[i]] <- data.frame(
    pupilmatchingrefanonymous = pmr_vector[factor_var_int == i]
  )
  
  ci_data[[i]]$y7 <- as.integer(NA)
  ci_data[[i]]$y8 <- as.integer(NA)
  ci_data[[i]]$y9 <- as.integer(NA)
  ci_data[[i]]$y10 <- as.integer(NA)
  ci_data[[i]]$y11 <- as.integer(NA)
  
  for (j in 2:5) {
    ci_data[[i]][, j + 1] <- ci_data[[i]]$pupilmatchingrefanonymous %in%
      tmp[year <= j + 6 & not_enrolled == 1]$pupilmatchingrefanonymous
  }
  
  ci_data[[i]] <- data.table(ci_data[[i]])
}

var_levels <- levels(factor_var)
rm(i, j, pmr_vector, factor_var, factor_var_int, loop_length)

ci_output <- create_ci_output(ci_data = ci_data, names_for_cols = var_levels, perc_round = 1)
ci_output

write.csv(ci_output, "OUTPUTS/CHAPTER 5/additional/ci-not-enrolled-by-fsm-not-special.csv")
rm(tmp, var_levels, ci_data, ci_output)

# special
tmp <- data.table(
  pupilmatchingrefanonymous = cohort_all[year >= 7 & sch_n_in_year == 1 & special_school_y7 == T]$pupilmatchingrefanonymous,
  year = cohort_all[year >= 7 & sch_n_in_year == 1 & special_school_y7 == T]$year,
  fsmeligible = cohort_all[year >= 7 & sch_n_in_year == 1 & special_school_y7 == T]$fsmeligible
)

tmp$not_enrolled <- 0
tmp <- data.table(complete(tmp, pupilmatchingrefanonymous, year))
tmp[is.na(not_enrolled)]$not_enrolled <- 1
tmp[, fsmeligible := fsmeligible[1], by = .(pupilmatchingrefanonymous)]

pmr_vector <- tmp[year == 7]$pupilmatchingrefanonymous
factor_var <- tmp[year == 7]$fsmeligible

factor_var <- factor(factor_var)
factor_var_int <- as.integer(factor_var)
loop_length <- dim(table(factor_var))

ci_data <- list()
for (i in 1:loop_length) {
  ci_data[[i]] <- data.frame(
    pupilmatchingrefanonymous = pmr_vector[factor_var_int == i]
  )
  
  ci_data[[i]]$y7 <- as.integer(NA)
  ci_data[[i]]$y8 <- as.integer(NA)
  ci_data[[i]]$y9 <- as.integer(NA)
  ci_data[[i]]$y10 <- as.integer(NA)
  ci_data[[i]]$y11 <- as.integer(NA)
  
  for (j in 2:5) {
    ci_data[[i]][, j + 1] <- ci_data[[i]]$pupilmatchingrefanonymous %in%
      tmp[year <= j + 6 & not_enrolled == 1]$pupilmatchingrefanonymous
  }
  
  ci_data[[i]] <- data.table(ci_data[[i]])
}

var_levels <- levels(factor_var)
rm(i, j, pmr_vector, factor_var, factor_var_int, loop_length)

ci_output <- create_ci_output(ci_data = ci_data, names_for_cols = var_levels, perc_round = 1)
ci_output

write.csv(ci_output, "OUTPUTS/CHAPTER 5/additional/ci-not-enrolled-by-fsm-special.csv")
rm(tmp, var_levels, ci_data, ci_output)

# ** by IDACI/FSM ---------------------------------------------------------

# not special
tmp <- data.table(
  pupilmatchingrefanonymous = cohort_all[year >= 7 & sch_n_in_year == 1 & special_school_y7 == F]$pupilmatchingrefanonymous,
  year = cohort_all[year >= 7 & sch_n_in_year == 1 & special_school_y7 == F]$year,
  idaci_fsm = cohort_all[year >= 7 & sch_n_in_year == 1 & special_school_y7 == F]$idaci_fsm
)

tmp$not_enrolled <- 0
tmp <- data.table(complete(tmp, pupilmatchingrefanonymous, year))
tmp[is.na(not_enrolled)]$not_enrolled <- 1
tmp[, idaci_fsm := idaci_fsm[1], by = .(pupilmatchingrefanonymous)]

pmr_vector <- tmp[year == 7]$pupilmatchingrefanonymous
factor_var <- tmp[year == 7]$idaci_fsm

factor_var <- factor(factor_var)
factor_var_int <- as.integer(factor_var)
loop_length <- dim(table(factor_var))

ci_data <- list()
for (i in 1:loop_length) {
  ci_data[[i]] <- data.frame(
    pupilmatchingrefanonymous = pmr_vector[factor_var_int == i]
  )
  
  ci_data[[i]]$y7 <- as.integer(NA)
  ci_data[[i]]$y8 <- as.integer(NA)
  ci_data[[i]]$y9 <- as.integer(NA)
  ci_data[[i]]$y10 <- as.integer(NA)
  ci_data[[i]]$y11 <- as.integer(NA)
  
  for (j in 2:5) {
    ci_data[[i]][, j + 1] <- ci_data[[i]]$pupilmatchingrefanonymous %in%
      tmp[year <= j + 6 & not_enrolled == 1]$pupilmatchingrefanonymous
  }
  
  ci_data[[i]] <- data.table(ci_data[[i]])
}

var_levels <- levels(factor_var)
rm(i, j, pmr_vector, factor_var, factor_var_int, loop_length)

ci_output <- create_ci_output(ci_data = ci_data, names_for_cols = var_levels, perc_round = 1)
ci_output

write.csv(ci_output, "OUTPUTS/CHAPTER 5/additional/ci-not-enrolled-by-idacifsm-not-special.csv")
rm(tmp, var_levels, ci_data, ci_output)

# special
tmp <- data.table(
  pupilmatchingrefanonymous = cohort_all[year >= 7 & sch_n_in_year == 1 & special_school_y7 == T]$pupilmatchingrefanonymous,
  year = cohort_all[year >= 7 & sch_n_in_year == 1 & special_school_y7 == T]$year,
  idaci_fsm = cohort_all[year >= 7 & sch_n_in_year == 1 & special_school_y7 == T]$idaci_fsm
)

tmp$not_enrolled <- 0
tmp <- data.table(complete(tmp, pupilmatchingrefanonymous, year))
tmp[is.na(not_enrolled)]$not_enrolled <- 1
tmp[, idaci_fsm := idaci_fsm[1], by = .(pupilmatchingrefanonymous)]

pmr_vector <- tmp[year == 7]$pupilmatchingrefanonymous
factor_var <- tmp[year == 7]$idaci_fsm

factor_var <- factor(factor_var)
factor_var_int <- as.integer(factor_var)
loop_length <- dim(table(factor_var))

ci_data <- list()
for (i in 1:loop_length) {
  ci_data[[i]] <- data.frame(
    pupilmatchingrefanonymous = pmr_vector[factor_var_int == i]
  )
  
  ci_data[[i]]$y7 <- as.integer(NA)
  ci_data[[i]]$y8 <- as.integer(NA)
  ci_data[[i]]$y9 <- as.integer(NA)
  ci_data[[i]]$y10 <- as.integer(NA)
  ci_data[[i]]$y11 <- as.integer(NA)
  
  for (j in 2:5) {
    ci_data[[i]][, j + 1] <- ci_data[[i]]$pupilmatchingrefanonymous %in%
      tmp[year <= j + 6 & not_enrolled == 1]$pupilmatchingrefanonymous
  }
  
  ci_data[[i]] <- data.table(ci_data[[i]])
}

var_levels <- levels(factor_var)
rm(i, j, pmr_vector, factor_var, factor_var_int, loop_length)

ci_output <- create_ci_output(ci_data = ci_data, names_for_cols = var_levels, perc_round = 1)
ci_output

write.csv(ci_output, "OUTPUTS/CHAPTER 5/additional/ci-not-enrolled-by-idacifsm-special.csv")
rm(tmp, var_levels, ci_data, ci_output)

# ** by region ------------------------------------------------------------

# not special
tmp <- data.table(
  pupilmatchingrefanonymous = cohort_all[year >= 7 & sch_n_in_year == 1 & special_school_y7 == F]$pupilmatchingrefanonymous,
  year = cohort_all[year >= 7 & sch_n_in_year == 1 & special_school_y7 == F]$year,
  region_major = cohort_all[year >= 7 & sch_n_in_year == 1 & special_school_y7 == F]$region_major
)

tmp$not_enrolled <- 0
tmp <- data.table(complete(tmp, pupilmatchingrefanonymous, year))
tmp[is.na(not_enrolled)]$not_enrolled <- 1
tmp[, region_major := region_major[1], by = .(pupilmatchingrefanonymous)]

pmr_vector <- tmp[year == 7]$pupilmatchingrefanonymous
factor_var <- tmp[year == 7]$region_major

factor_var <- factor(factor_var)
factor_var_int <- as.integer(factor_var)
loop_length <- dim(table(factor_var))

ci_data <- list()
for (i in 1:loop_length) {
  ci_data[[i]] <- data.frame(
    pupilmatchingrefanonymous = pmr_vector[factor_var_int == i]
  )
  
  ci_data[[i]]$y7 <- as.integer(NA)
  ci_data[[i]]$y8 <- as.integer(NA)
  ci_data[[i]]$y9 <- as.integer(NA)
  ci_data[[i]]$y10 <- as.integer(NA)
  ci_data[[i]]$y11 <- as.integer(NA)
  
  for (j in 2:5) {
    ci_data[[i]][, j + 1] <- ci_data[[i]]$pupilmatchingrefanonymous %in%
      tmp[year <= j + 6 & not_enrolled == 1]$pupilmatchingrefanonymous
  }
  
  ci_data[[i]] <- data.table(ci_data[[i]])
}

var_levels <- levels(factor_var)
rm(i, j, pmr_vector, factor_var, factor_var_int, loop_length)

ci_output <- create_ci_output(ci_data = ci_data, names_for_cols = var_levels, perc_round = 1)
ci_output

write.csv(ci_output, "OUTPUTS/CHAPTER 5/additional/ci-not-enrolled-by-region-not-special.csv")
rm(tmp, var_levels, ci_data, ci_output)

# special
tmp <- data.table(
  pupilmatchingrefanonymous = cohort_all[year >= 7 & sch_n_in_year == 1 & special_school_y7 == T]$pupilmatchingrefanonymous,
  year = cohort_all[year >= 7 & sch_n_in_year == 1 & special_school_y7 == T]$year,
  region_major = cohort_all[year >= 7 & sch_n_in_year == 1 & special_school_y7 == T]$region_major
)

tmp$not_enrolled <- 0
tmp <- data.table(complete(tmp, pupilmatchingrefanonymous, year))
tmp[is.na(not_enrolled)]$not_enrolled <- 1
tmp[, region_major := region_major[1], by = .(pupilmatchingrefanonymous)]

pmr_vector <- tmp[year == 7]$pupilmatchingrefanonymous
factor_var <- tmp[year == 7]$region_major

factor_var <- factor(factor_var)
factor_var_int <- as.integer(factor_var)
loop_length <- dim(table(factor_var))

ci_data <- list()
for (i in 1:loop_length) {
  ci_data[[i]] <- data.frame(
    pupilmatchingrefanonymous = pmr_vector[factor_var_int == i]
  )
  
  ci_data[[i]]$y7 <- as.integer(NA)
  ci_data[[i]]$y8 <- as.integer(NA)
  ci_data[[i]]$y9 <- as.integer(NA)
  ci_data[[i]]$y10 <- as.integer(NA)
  ci_data[[i]]$y11 <- as.integer(NA)
  
  for (j in 2:5) {
    ci_data[[i]][, j + 1] <- ci_data[[i]]$pupilmatchingrefanonymous %in%
      tmp[year <= j + 6 & not_enrolled == 1]$pupilmatchingrefanonymous
  }
  
  ci_data[[i]] <- data.table(ci_data[[i]])
}

var_levels <- levels(factor_var)
rm(i, j, pmr_vector, factor_var, factor_var_int, loop_length)

ci_output <- create_ci_output(ci_data = ci_data, names_for_cols = var_levels, perc_round = 1)
ci_output

write.csv(ci_output, "OUTPUTS/CHAPTER 5/additional/ci-not-enrolled-by-region-special.csv")
rm(tmp, var_levels, ci_data, ci_output)

# ** by SEND primary ------------------------------------------------------

# not special
tmp <- data.table(
  pupilmatchingrefanonymous = cohort_all[year >= 7 & sch_n_in_year == 1 & special_school_y7 == F]$pupilmatchingrefanonymous,
  year = cohort_all[year >= 7 & sch_n_in_year == 1 & special_school_y7 == F]$year,
  ever_sen_primary = cohort_all[year >= 7 & sch_n_in_year == 1 & special_school_y7 == F]$ever_sen_primary
)

tmp$not_enrolled <- 0
tmp <- data.table(complete(tmp, pupilmatchingrefanonymous, year))
tmp[is.na(not_enrolled)]$not_enrolled <- 1
tmp[, ever_sen_primary := ever_sen_primary[1], by = .(pupilmatchingrefanonymous)]

pmr_vector <- tmp[year == 7]$pupilmatchingrefanonymous
factor_var <- tmp[year == 7]$ever_sen_primary

factor_var <- factor(factor_var)
factor_var_int <- as.integer(factor_var)
loop_length <- dim(table(factor_var))

ci_data <- list()
for (i in 1:loop_length) {
  ci_data[[i]] <- data.frame(
    pupilmatchingrefanonymous = pmr_vector[factor_var_int == i]
  )
  
  ci_data[[i]]$y7 <- as.integer(NA)
  ci_data[[i]]$y8 <- as.integer(NA)
  ci_data[[i]]$y9 <- as.integer(NA)
  ci_data[[i]]$y10 <- as.integer(NA)
  ci_data[[i]]$y11 <- as.integer(NA)
  
  for (j in 2:5) {
    ci_data[[i]][, j + 1] <- ci_data[[i]]$pupilmatchingrefanonymous %in%
      tmp[year <= j + 6 & not_enrolled == 1]$pupilmatchingrefanonymous
  }
  
  ci_data[[i]] <- data.table(ci_data[[i]])
}

var_levels <- levels(factor_var)
rm(i, j, pmr_vector, factor_var, factor_var_int, loop_length)

ci_output <- create_ci_output(ci_data = ci_data, names_for_cols = var_levels, perc_round = 1)
ci_output

write.csv(ci_output, "OUTPUTS/CHAPTER 5/additional/ci-not-enrolled-by-senprim-not-special.csv")
rm(tmp, var_levels, ci_data, ci_output)

# ** by highest SEND primary ----------------------------------------------

# not special
tmp <- data.table(
  pupilmatchingrefanonymous = cohort_all[year >= 7 & sch_n_in_year == 1 & special_school_y7 == F]$pupilmatchingrefanonymous,
  year = cohort_all[year >= 7 & sch_n_in_year == 1 & special_school_y7 == F]$year,
  highest_ever_sen_primary = cohort_all[year >= 7 & sch_n_in_year == 1 & special_school_y7 == F]$highest_ever_sen_primary
)

tmp$not_enrolled <- 0
tmp <- data.table(complete(tmp, pupilmatchingrefanonymous, year))
tmp[is.na(not_enrolled)]$not_enrolled <- 1
tmp[, highest_ever_sen_primary := highest_ever_sen_primary[1], by = .(pupilmatchingrefanonymous)]

pmr_vector <- tmp[year == 7]$pupilmatchingrefanonymous
factor_var <- tmp[year == 7]$highest_ever_sen_primary

factor_var <- factor(factor_var)
factor_var_int <- as.integer(factor_var)
loop_length <- dim(table(factor_var))

ci_data <- list()
for (i in 1:loop_length) {
  ci_data[[i]] <- data.frame(
    pupilmatchingrefanonymous = pmr_vector[factor_var_int == i]
  )
  
  ci_data[[i]]$y7 <- as.integer(NA)
  ci_data[[i]]$y8 <- as.integer(NA)
  ci_data[[i]]$y9 <- as.integer(NA)
  ci_data[[i]]$y10 <- as.integer(NA)
  ci_data[[i]]$y11 <- as.integer(NA)
  
  for (j in 2:5) {
    ci_data[[i]][, j + 1] <- ci_data[[i]]$pupilmatchingrefanonymous %in%
      tmp[year <= j + 6 & not_enrolled == 1]$pupilmatchingrefanonymous
  }
  
  ci_data[[i]] <- data.table(ci_data[[i]])
}

var_levels <- levels(factor_var)
rm(i, j, pmr_vector, factor_var, factor_var_int, loop_length)

ci_output <- create_ci_output(ci_data = ci_data, names_for_cols = var_levels, perc_round = 1)
ci_output

write.csv(ci_output, "OUTPUTS/CHAPTER 5/additional/ci-not-enrolled-by-highestsenprim-not-special.csv")
rm(tmp, var_levels, ci_data, ci_output)

# ** by APPRU yr 7 --------------------------------------------------------

# not special
tmp <- data.table(
  pupilmatchingrefanonymous = cohort_all[year >= 7 & sch_n_in_year == 1 & special_school_y7 == F]$pupilmatchingrefanonymous,
  year = cohort_all[year >= 7 & sch_n_in_year == 1 & special_school_y7 == F]$year,
  appru_y06 = cohort_all[year >= 7 & sch_n_in_year == 1 & special_school_y7 == F]$appru_y06
)

tmp$not_enrolled <- 0
tmp <- data.table(complete(tmp, pupilmatchingrefanonymous, year))
tmp[is.na(not_enrolled)]$not_enrolled <- 1
tmp[, appru_y06 := appru_y06[1], by = .(pupilmatchingrefanonymous)]

pmr_vector <- tmp[year == 7]$pupilmatchingrefanonymous
factor_var <- tmp[year == 7]$appru_y06

factor_var <- factor(factor_var)
factor_var_int <- as.integer(factor_var)
loop_length <- dim(table(factor_var))

ci_data <- list()
for (i in 1:loop_length) {
  ci_data[[i]] <- data.frame(
    pupilmatchingrefanonymous = pmr_vector[factor_var_int == i]
  )
  
  ci_data[[i]]$y7 <- as.integer(NA)
  ci_data[[i]]$y8 <- as.integer(NA)
  ci_data[[i]]$y9 <- as.integer(NA)
  ci_data[[i]]$y10 <- as.integer(NA)
  ci_data[[i]]$y11 <- as.integer(NA)
  
  for (j in 2:5) {
    ci_data[[i]][, j + 1] <- ci_data[[i]]$pupilmatchingrefanonymous %in%
      tmp[year <= j + 6 & not_enrolled == 1]$pupilmatchingrefanonymous
  }
  
  ci_data[[i]] <- data.table(ci_data[[i]])
}

var_levels <- levels(factor_var)
rm(i, j, pmr_vector, factor_var, factor_var_int, loop_length)

ci_output <- create_ci_output(ci_data = ci_data, names_for_cols = var_levels, perc_round = 1)
ci_output

write.csv(ci_output, "OUTPUTS/CHAPTER 5/additional/ci-not-enrolled-by-appru_y06-not-special.csv")
rm(tmp, var_levels, ci_data, ci_output)

# special
tmp <- data.table(
  pupilmatchingrefanonymous = cohort_all[year >= 7 & sch_n_in_year == 1 & special_school_y7 == T]$pupilmatchingrefanonymous,
  year = cohort_all[year >= 7 & sch_n_in_year == 1 & special_school_y7 == T]$year,
  appru_y06 = cohort_all[year >= 7 & sch_n_in_year == 1 & special_school_y7 == T]$appru_y06
)

tmp$not_enrolled <- 0
tmp <- data.table(complete(tmp, pupilmatchingrefanonymous, year))
tmp[is.na(not_enrolled)]$not_enrolled <- 1
tmp[, appru_y06 := appru_y06[1], by = .(pupilmatchingrefanonymous)]

pmr_vector <- tmp[year == 7]$pupilmatchingrefanonymous
factor_var <- tmp[year == 7]$appru_y06

factor_var <- factor(factor_var)
factor_var_int <- as.integer(factor_var)
loop_length <- dim(table(factor_var))

ci_data <- list()
for (i in 1:loop_length) {
  ci_data[[i]] <- data.frame(
    pupilmatchingrefanonymous = pmr_vector[factor_var_int == i]
  )
  
  ci_data[[i]]$y7 <- as.integer(NA)
  ci_data[[i]]$y8 <- as.integer(NA)
  ci_data[[i]]$y9 <- as.integer(NA)
  ci_data[[i]]$y10 <- as.integer(NA)
  ci_data[[i]]$y11 <- as.integer(NA)
  
  for (j in 2:5) {
    ci_data[[i]][, j + 1] <- ci_data[[i]]$pupilmatchingrefanonymous %in%
      tmp[year <= j + 6 & not_enrolled == 1]$pupilmatchingrefanonymous
  }
  
  ci_data[[i]] <- data.table(ci_data[[i]])
}

var_levels <- levels(factor_var)
rm(i, j, pmr_vector, factor_var, factor_var_int, loop_length)

ci_output <- create_ci_output(ci_data = ci_data, names_for_cols = var_levels, perc_round = 1)
ci_output

write.csv(ci_output, "OUTPUTS/CHAPTER 5/additional/ci-not-enrolled-by-appru_y06-special.csv")
rm(tmp, var_levels, ci_data, ci_output)

# YEARS 8/9 ---------------------------------------------------------------

outcomes <- c("not_enrolled_y89")

final_output <- cbind(describe_outcome(exposure = outcomes[1], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F]),
                      describe_outcome(exposure = outcomes[1], outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T]))
colnames(final_output) <- c("Not special school yr 7", "Special school yr 7")
write.csv(final_output, file = "OUTPUTS/CHAPTER 5/not-enrolled-y89-total.csv")

exposures <- c("exposure_highest_4_grp_y46", "exposure_highest_4_grp_y49",
               "female_clean", "eth_major", "language_clean", "idaci_quintiles", "fsmeligible", "idaci_fsm", "region_major",
               "appru_y06", "appru_y09", "ever_sen_primary", "ever_sen_y09", "highest_ever_sen_primary", "highest_ever_sen_y09")

final_output <- describe_outcome(exposure = exposures, outcome = outcomes[1], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F])
write.csv(final_output, file = "OUTPUTS/CHAPTER 5/not-enrolled-y89-by-RFs-not-special.csv")

exposures <- c("exposure_highest_4_grp_y46", "exposure_highest_4_grp_y49",
               "female_clean", "eth_major", "language_clean", "idaci_quintiles", "fsmeligible", "idaci_fsm", "region_major",
               "appru_y06", "appru_y09")

final_output <- describe_outcome(exposure = exposures, outcome = outcomes[1], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T])
write.csv(final_output, file = "OUTPUTS/CHAPTER 5/not-enrolled-y89-by-RFs-special.csv")

rm(outcomes, exposures, final_output)

# DSITRIBUTION OF NON-ENROLMENT -------------------------------------------

cohort_all[, n_years_not_enrolled := as.integer(NA)]

cohort_all[sch_n_in_year == 1 & year == 7, n_years_not_enrolled := sum(not_enrolled_y8,
                                                                       not_enrolled_y9,
                                                                       not_enrolled_y10,
                                                                       not_enrolled_y11),
           by = .(pupilmatchingrefanonymous)]

cohort_all[, n_years_not_enrolled := n_years_not_enrolled[which(!is.na(n_years_not_enrolled))],
           by = .(pupilmatchingrefanonymous)]

table(cohort_all[year == 7 & sch_n_in_year == 1]$n_years_not_enrolled)
prop.table(table(cohort_all[year == 7 & sch_n_in_year == 1]$n_years_not_enrolled))

table(cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F]$n_years_not_enrolled)
prop.table(table(cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F]$n_years_not_enrolled))

table(cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T]$n_years_not_enrolled)
prop.table(table(cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T]$n_years_not_enrolled))

write.csv(table(cohort_all[year == 7 & sch_n_in_year == 1]$exposure_highest_4_grp_y46,
                cohort_all[year == 7 & sch_n_in_year == 1]$n_years_not_enrolled),
          file = "OUTPUTS/CHAPTER 5/not-enrolled-distro-all-schools.csv")

write.csv(table(cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F]$exposure_highest_4_grp_y46,
                cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F]$n_years_not_enrolled),
          file = "OUTPUTS/CHAPTER 5/not-enrolled-distro-not-special-schools.csv")

write.csv(table(cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T]$exposure_highest_4_grp_y46,
                cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T]$n_years_not_enrolled),
          file = "OUTPUTS/CHAPTER 5/not-enrolled-distro-special-schools.csv")
