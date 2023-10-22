# LOAD --------------------------------------------------------------------

setwd("P:/Working/Matt")

lib_base = "P:/Working/Matt/r-library/"
assign(".lib.loc", c(.libPaths(), lib_base), envir = environment(.libPaths))
rm(lib_base)

# data management
library(data.table)
library(tidyr)

# graphics
library(ggplot2)
library(gridExtra)

load("PROCESSED DATA/cohort1_npd_clean_sch_ad_qual_cscobs_outcomes_misc.rda")
load("PROCESSED DATA/cohort2_npd_clean_sch_ad_qual_cscobs_outcomes_misc.rda")

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

cohort_all$any_nas <- apply(cohort_all[, c("female_clean", "eth_major", "language_clean", "idaci_quintiles")], 1, anyNA)
cohort_all[, drop := year == 7 & any_nas & sch_n_in_year == 1]
cohort_all[, drop := max(drop), by = .(pupilmatchingrefanonymous)]
table(cohort_all[year == 7 & sch_n_in_year == 1]$drop)
cohort_all <- cohort_all[!cohort_all$drop]
cohort_all[, any_nas := NULL]
cohort_all[, drop := NULL]

# MUST BE IN YEAR 1 -------------------------------------------------------

cohort_all[, enrolled_y1 := F]
cohort_all[year == 1, enrolled_y1 := T]
cohort_all[, enrolled_y1 := max(enrolled_y1), by = .(pupilmatchingrefanonymous)]
cohort_all <- cohort_all[enrolled_y1 == T]

# * CHARACTERISTICS ---------------------------------------------------------

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
write.csv(final_output, file = "OUTPUTS/CHAPTER 6/sensitivity/chars.csv")

# by special school or not
table(cohort_all[year == 7 & sch_n_in_year == 1]$special_school)

exposures <- c("exposure_highest_4_grp_y46",
               "female_clean", "eth_major", "language_clean", "idaci_quintiles", "fsmeligible", "idaci_fsm", "region_major",
               "ever_sen_primary", "highest_ever_sen_primary",
               "school_type_with_secondary_5_groups", "appru")

final_output <- cbind(describe_outcome(exposure = exposures, outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T]),
                      describe_outcome(exposure = exposures, outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F]),
                      describe_outcome(exposure = exposures, outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1]))
colnames(final_output) <- c("Special school yr 7", "Not special school yr 7", "Combined")
write.csv(final_output, file = "OUTPUTS/CHAPTER 6/sensitivity/chars-by-special-school.csv")

rm(exposures, final_output)


# * INCIDENCE ---------------------------------------------------------------

# ** cross-sectional -------------------------------------------------------

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
write.csv(final_output, file = "OUTPUTS/CHAPTER 6/sensitivity/not-enrolled-cross-sectional-total-yearly.csv")

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

write.csv(final_output, file = "OUTPUTS/CHAPTER 6/sensitivity/not-enrolled-cross-sectional-by-risk-factors-not-special-yearly.csv")

exposures <- c("exposure_highest_4_grp_y46",
               "female_clean", "eth_major", "language_clean", "idaci_quintiles", "fsmeligible", "idaci_fsm", "region_major",
               "appru")

final_output <- cbind(
  describe_outcome(exposure = exposures, outcome = outcomes[1], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T], p_round = 1),
  describe_outcome(exposure = exposures, outcome = outcomes[2], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T], p_round = 1),
  describe_outcome(exposure = exposures, outcome = outcomes[3], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T], p_round = 1),
  describe_outcome(exposure = exposures, outcome = outcomes[4], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T], p_round = 1)
)

write.csv(final_output, file = "OUTPUTS/CHAPTER 6/sensitivity/not-enrolled-cross-sectional-by-risk-factors-special-yearly.csv")

rm(exposures, outcomes, final_output)

# ** cumulative --------------------------------------------------------------

# *** by CSC ---------------------------------------------------------------

# **** not special ---------------------------------------------------------

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

write.csv(ci_output, "OUTPUTS/CHAPTER 6/sensitivity/not-enrolled-cumulative-not-special.csv")

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

write.csv(ci_output, "OUTPUTS/CHAPTER 6/sensitivity/not-enrolled-cumulative-by-exp-not-special.csv")

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

# **** special -------------------------------------------------------------

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

write.csv(ci_output, "OUTPUTS/CHAPTER 6/sensitivity/not-enrolled-cumulative-special.csv")

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

write.csv(ci_output, "OUTPUTS/CHAPTER 6/sensitivity/not-enrolled-cumulative-by-exp-special.csv")

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

tiff("OUTPUTS/CHAPTER 6/sensitivity/offrolling-cumulative.tiff",
     width = 12, height = 6, units = "in", res = 300)
grid.arrange(p1, p2, nrow = 1)
dev.off()

rm(ci_data, ci_output, ci_output_p, tmp, p1, p2)

# * LA AND REGIONAL VARIATION ---------------------------------------------

# ** not special -----------------------------------------------------------

# *** graphical ------------------------------------------------------------

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

tiff("OUTPUTS/CHAPTER 6/sensitivity/offrolling-by-la-y1011.tiff",
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

# *** numerical ------------------------------------------------------------

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

write.csv(tmp, file = "OUTPUTS/CHAPTER 6/sensitivity/outcomes-region-ns.csv")

summary(tmp$region_not_enrolled_y1011_nogcses_n)
summary(tmp$region_not_enrolled_y1011_nogcses_prop)

rm(tmp)


# ** special ---------------------------------------------------------------

# *** graphical ------------------------------------------------------------

tmp <- data.table(
  la = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T]$la,
  region = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T]$region_major,
  not_enrolled_y1011 = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T]$not_enrolled_y1011
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

# *** numerical ------------------------------------------------------------

# LA level
summary(tmp$la_not_enrolled_y1011_n)
summary(tmp$la_not_enrolled_y1011_prop)
hist(tmp$la_not_enrolled_y1011_n, breaks = 20)
hist(tmp$la_not_enrolled_y1011_prop, breaks = 15)
mean(tmp$la_not_enrolled_y1011_n < 6)
mean(tmp$la_not_enrolled_y1011_n == 0)

# regional level
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

write.csv(tmp, file = "OUTPUTS/CHAPTER 6/sensitivity//outcomes-region-s.csv")

summary(tmp$region_not_enrolled_y1011_n)
summary(tmp$region_not_enrolled_y1011_prop)

rm(tmp)

# ENGLISH FIRST LANGUAGE -------------------------------------------------------

cohort_all <- cohort_all[language_clean == "eng"]

# * CHARACTERISTICS ---------------------------------------------------------

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
write.csv(final_output, file = "OUTPUTS/CHAPTER 6/sensitivity2/chars.csv")

# by special school or not
table(cohort_all[year == 7 & sch_n_in_year == 1]$special_school)
exposures <- c("exposure_highest_4_grp_y46",
               "female_clean", "eth_major", "language_clean", "idaci_quintiles", "fsmeligible", "idaci_fsm", "region_major",
               "ever_sen_primary", "highest_ever_sen_primary",
               "school_type_with_secondary_5_groups", "appru")

final_output <- cbind(describe_outcome(exposure = exposures, outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T]),
                      describe_outcome(exposure = exposures, outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == F]),
                      describe_outcome(exposure = exposures, outcome = NULL, dat = cohort_all[year == 7 & sch_n_in_year == 1]))
colnames(final_output) <- c("Special school yr 7", "Not special school yr 7", "Combined")
write.csv(final_output, file = "OUTPUTS/CHAPTER 6/sensitivity2/chars-by-special-school.csv")

rm(exposures, final_output)

# * INCIDENCE ---------------------------------------------------------------

# ** cross-sectional -------------------------------------------------------

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
write.csv(final_output, file = "OUTPUTS/CHAPTER 6/sensitivity2/not-enrolled-cross-sectional-total-yearly.csv")

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

write.csv(final_output, file = "OUTPUTS/CHAPTER 6/sensitivity2/not-enrolled-cross-sectional-by-risk-factors-not-special-yearly.csv")

exposures <- c("exposure_highest_4_grp_y46",
               "female_clean", "eth_major", "language_clean", "idaci_quintiles", "fsmeligible", "idaci_fsm", "region_major",
               "appru")

final_output <- cbind(
  describe_outcome(exposure = exposures, outcome = outcomes[1], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T], p_round = 1),
  describe_outcome(exposure = exposures, outcome = outcomes[2], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T], p_round = 1),
  describe_outcome(exposure = exposures, outcome = outcomes[3], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T], p_round = 1),
  describe_outcome(exposure = exposures, outcome = outcomes[4], dat = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T], p_round = 1)
)

write.csv(final_output, file = "OUTPUTS/CHAPTER 6/sensitivity2/not-enrolled-cross-sectional-by-risk-factors-special-yearly.csv")

rm(exposures, outcomes, final_output)

# ** cumulative --------------------------------------------------------------

# *** by CSC ---------------------------------------------------------------

# **** not special ---------------------------------------------------------

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

write.csv(ci_output, "OUTPUTS/CHAPTER 6/sensitivity2/not-enrolled-cumulative-not-special.csv")

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

write.csv(ci_output, "OUTPUTS/CHAPTER 6/sensitivity2/not-enrolled-cumulative-by-exp-not-special.csv")

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

# **** special -------------------------------------------------------------

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

write.csv(ci_output, "OUTPUTS/CHAPTER 6/sensitivity2/not-enrolled-cumulative-special.csv")

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

write.csv(ci_output, "OUTPUTS/CHAPTER 6/sensitivity2/not-enrolled-cumulative-by-exp-special.csv")

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

tiff("OUTPUTS/CHAPTER 6/sensitivity2/offrolling-cumulative.tiff",
     width = 12, height = 6, units = "in", res = 300)
grid.arrange(p1, p2, nrow = 1)
dev.off()

rm(ci_data, ci_output, ci_output_p, tmp, p1, p2)

# * LA AND REGIONAL VARIATION ---------------------------------------------

# ** not special -----------------------------------------------------------

# *** graphical ------------------------------------------------------------

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

tiff("OUTPUTS/CHAPTER 6/sensitivity2/offrolling-by-la-y1011.tiff",
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

# *** numerical ------------------------------------------------------------

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

write.csv(tmp, file = "OUTPUTS/CHAPTER 6/sensitivity2/outcomes-region-ns.csv")

summary(tmp$region_not_enrolled_y1011_nogcses_n)
summary(tmp$region_not_enrolled_y1011_nogcses_prop)

rm(tmp)

# ** special ---------------------------------------------------------------

# *** graphical ------------------------------------------------------------

tmp <- data.table(
  la = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T]$la,
  region = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T]$region_major,
  not_enrolled_y1011 = cohort_all[year == 7 & sch_n_in_year == 1 & special_school == T]$not_enrolled_y1011
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

# *** numerical ------------------------------------------------------------

# LA level
summary(tmp$la_not_enrolled_y1011_n)
summary(tmp$la_not_enrolled_y1011_prop)
hist(tmp$la_not_enrolled_y1011_n, breaks = 20)
hist(tmp$la_not_enrolled_y1011_prop, breaks = 15)
mean(tmp$la_not_enrolled_y1011_n < 6)
mean(tmp$la_not_enrolled_y1011_n == 0)

# regional level
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

write.csv(tmp, file = "OUTPUTS/CHAPTER 6/sensitivity2//outcomes-region-s.csv")

summary(tmp$region_not_enrolled_y1011_n)
summary(tmp$region_not_enrolled_y1011_prop)

rm(tmp)
