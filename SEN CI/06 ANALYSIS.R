
# LOAD --------------------------------------------------------------------

setwd("P:/Working/Matt")

lib_base = "P:/Working/Matt/r-library/"
assign(".lib.loc", c(.libPaths(), lib_base), envir = environment(.libPaths))
rm(lib_base)

library(data.table)
library(tableone)
library(ggplot2)
library(gridExtra)

load("PROCESSED DATA/SEN CI/cohort2_npd_clean_sch.rda")
load("PROCESSED DATA/SEN CI/cla_episodes.rda")
load("PROCESSED DATA/SEN CI/cin.rda")
load("PROCESSED DATA/SEN CI/stb.rda")

dates <- c(
  as.Date("2005-09-01"),
  as.Date("2006-09-01"),
  as.Date("2007-09-01"),
  as.Date("2008-09-01"),
  as.Date("2009-09-01"),
  as.Date("2010-09-01"),
  as.Date("2011-09-01"),
  as.Date("2012-09-01"),
  as.Date("2013-09-01"),
  as.Date("2014-09-01"),
  as.Date("2015-09-01"),
  as.Date("2016-09-01"),
  as.Date("2017-09-01")
)

years <- c(
  "2005/2006",
  "2006/2007",
  "2007/2008",
  "2008/2009",
  "2009/2010",
  "2010/2011",
  "2011/2012",
  "2012/2013",
  "2013/2014",
  "2014/2015",
  "2015/2016",
  "2016/2017"
)

# IsleOfDogs1 <- c("#9986A5", "#79402E", "#CCBA72", "#0F0D0E", "#D9D0D3", "#8D8680")
# Darjeeling1 <- c("#FF0000", "#00A08A", "#F2AD00", "#F98400", "#5BBCD6")

white <- c("WBRI", "WIRI", "WIRT", "WOTH", "WROM")
black <- c("BAFR", "BCRB", "BOTH")
mixed <- c("MWAS", "MWBA", "MWBC", "MOTH")
asian <- c("ABAN", "AIND", "AOTH", "APKN", "CHNE")

cohort2$eth_broad_custom <- "White"
cohort2[eth.clean %in% c("WIRT", "WROM")]$eth_broad_custom <- "White Traveller/Roma"
cohort2[eth.clean %in% black]$eth_broad_custom <- "Black"
cohort2[eth.clean %in% mixed]$eth_broad_custom <- "Mixed"
cohort2[eth.clean %in% asian]$eth_broad_custom <- "Asian"
cohort2[eth.clean %in% c("OOTH", "unknown")]$eth_broad_custom <- "Other/Unknown"

cohort2$eth_broad_custom <- factor(cohort2$eth_broad_custom,
                                   levels = c("White", "White Traveller/Roma", "Black", "Mixed", "Asian", "Other/Unknown"))

table(cohort2$eth_broad_custom)

rm(white, black, mixed, asian)

cohort2$lang_custom <- "English"
cohort2[lang.clean %in% c("oth", "unknown")]$lang_custom <- "Other/Unknown"


# EVER CSC VARIABLES ------------------------------------------------------

cohort2$ever_referred_y3to11 <- cohort2$pupilmatchingrefanonymous %in% cin$pupilmatchingrefanonymous
cohort2$ever_assessed_y3to11 <- cohort2$pupilmatchingrefanonymous %in% cin[referralnfa == 0]$pupilmatchingrefanonymous
cohort2$ever_cin_y3to11 <- cohort2$pupilmatchingrefanonymous %in% cin[referralnfa == 0 & (reasonforclosure != "RC8" | is.na(reasonforclosure))]$pupilmatchingrefanonymous
cohort2$ever_cpp_y3to11 <- cohort2$pupilmatchingrefanonymous %in% cin[!is.na(cppstartdate)]$pupilmatchingrefanonymous
cohort2$ever_cla_y0o11 <- cohort2$pupilmatchingrefanonymous %in% cla_episodes$pmr2

cohort2$highest_ever_csc <- "None"
cohort2[ever_referred_y3to11 == T & ever_assessed_y3to11 == F & ever_cin_y3to11 == F & ever_cpp_y3to11 == F & ever_cla_y0o11 == F]$highest_ever_csc <- "Referred"
cohort2[ever_assessed_y3to11 == T & ever_cin_y3to11 == F & ever_cpp_y3to11 == F & ever_cla_y0o11 == F]$highest_ever_csc <- "Assessed"
cohort2[ever_cin_y3to11 == T & ever_cpp_y3to11 == F & ever_cla_y0o11 == F]$highest_ever_csc <- "CiN"
cohort2[ever_cpp_y3to11 == T & ever_cla_y0o11 == F]$highest_ever_csc <- "CPP"
cohort2[ever_cla_y0o11 == T]$highest_ever_csc <- "CLA"
cohort2$highest_ever_csc <- factor(cohort2$highest_ever_csc,
                                   levels = c("None", "Referred", "Assessed", "CiN", "CPP", "CLA"))

table(cohort2[year == 0 & sch_n_in_year == 1]$highest_ever_csc, useNA = "always")

output <- data.table(
  group = c("referred", "assessed", "cin", "cpp", "cla"),
  n = c(
    round(table(cohort2[year == 0 & sch_n_in_year == 1]$ever_referred_y3to11), -1)[2],
    round(table(cohort2[year == 0 & sch_n_in_year == 1]$ever_assessed_y3to11), -1)[2],
    round(table(cohort2[year == 0 & sch_n_in_year == 1]$ever_cin_y3to11), -1)[2],
    round(table(cohort2[year == 0 & sch_n_in_year == 1]$ever_cpp_y3to11), -1)[2],
    round(table(cohort2[year == 0 & sch_n_in_year == 1]$ever_cla_y0o11), -1)[2]
  )
)

output$p <- round((output$n / 525510) * 100, 0)

round(table(cohort2[year == 0 & sch_n_in_year == 1]$highest_ever_csc, useNA = "always"), -1)
prop.table(round(table(cohort2[year == 0 & sch_n_in_year == 1]$highest_ever_csc, useNA = "always"), -1))

# EVER SEN ----------------------------------------------------------------

cohort2$ever_sen <- cohort2$pupilmatchingrefanonymous %in% cohort2[sen2 %in% c("AAPS", "SEHCP")]$pupilmatchingrefanonymous
cohort2$ever_aaps <- cohort2$pupilmatchingrefanonymous %in% cohort2[sen2 == "AAPS"]$pupilmatchingrefanonymous
cohort2$ever_sehcp <- cohort2$pupilmatchingrefanonymous %in% cohort2[sen2 == "SEHCP"]$pupilmatchingrefanonymous

output <- data.table(
  group = c("ever_sen", "ever_aaps", "ever_sehcp"),
  n = c(
    round(table(cohort2[year == 0 & sch_n_in_year == 1]$ever_sen), -1)[2],
    round(table(cohort2[year == 0 & sch_n_in_year == 1]$ever_aaps), -1)[2],
    round(table(cohort2[year == 0 & sch_n_in_year == 1]$ever_sehcp), -1)[2]
  )
)

output$p <- round((output$n / 525510) * 100, 0)

cohort2$highest_ever_sen <- "None"
cohort2[ever_aaps == T & ever_sehcp == F]$highest_ever_sen <- "AAPS"
cohort2[ever_sehcp == T]$highest_ever_sen <- "SEHCP"
cohort2$highest_ever_sen <- factor(cohort2$highest_ever_sen,
                                   levels = c("None", "AAPS", "SEHCP"))

table(cohort2[year == 0 & sch_n_in_year == 1]$highest_ever_sen, useNA = "always")
prop.table(table(cohort2[year == 0 & sch_n_in_year == 1]$highest_ever_sen, useNA = "always"))

# CHARACTERISTICS ---------------------------------------------------------

vars <- c("female.clean", "eth_broad_custom", "lang_custom", "sen2",
          "idaci_quintiles", "fsmeligible")

write.csv(print(CreateTableOne(data = cohort2[year == 0 & sch_n_in_year == 1],
                               vars = vars,
                               factorVars = vars,
                               test = F,
                               includeNA = T),
                showAllLevels = F,
                printToggle = F,
                noSpaces = T,
                format = "f"),
          file = "OUTPUTS/SEN CI/y0-chars-whole-cohort.csv")

write.csv(print(CreateTableOne(data = cohort2[year == 0 & sch_n_in_year == 1],
                               vars = vars,
                               factorVars = vars,
                               strata = "highest_ever_csc",
                               test = F,
                               includeNA = T),
                showAllLevels = F,
                printToggle = F,
                noSpaces = T,
                format = "f"),
          file = "OUTPUTS/SEN CI/y0-chars-by-exp.csv")

write.csv(print(CreateTableOne(data = cohort2[year == 0 & sch_n_in_year == 1],
                               vars = vars,
                               factorVars = vars,
                               strata = "highest_ever_sen",
                               test = F,
                               includeNA = T),
                showAllLevels = F,
                printToggle = F,
                noSpaces = T,
                format = "f"),
          file = "OUTPUTS/SEN CI/y0-chars-by-sen.csv")

rm(vars)

# CSC CUMULATIVE ----------------------------------------------------------

# * referral --------------------------------------------------------------

ci_tab <- data.frame(
  pupilmatchingrefanonymous = cohort2[year == 0 & sch_n_in_year == 1]$pupilmatchingrefanonymous
)

ci_tab$y3 <- as.logical(NA)
ci_tab$y4 <- as.logical(NA)
ci_tab$y5 <- as.logical(NA)
ci_tab$y6 <- as.logical(NA)
ci_tab$y7 <- as.logical(NA)
ci_tab$y8 <- as.logical(NA)
ci_tab$y9 <- as.logical(NA)
ci_tab$y10 <- as.logical(NA)
ci_tab$y11 <- as.logical(NA)

for (i in 1:9) {
  ci_tab[, i + 1] <- ci_tab$pupilmatchingrefanonymous %in% cin[acadyr %in% years[4:(i + 3)] |
                     (referraldate >= dates[4] & referraldate < dates[i + 4])]$pupilmatchingrefanonymous
}

ci_tab <- data.table(ci_tab)

ci_mat_overall <- data.frame(
  year = 3:11,
  n = rep(NA, 9),
  p = rep(NA, 9)
)

for (i in 2:10) {
  ci_mat_overall[i - 1, 2] <- table(ci_tab[, i, with = F])[2]
  ci_mat_overall[i - 1, 3] <- round(prop.table(table(ci_tab[, i, with = F])) * 100, 0)[2]
}

ci_mat_overall
table(cohort2[year == 0 & sch_n_in_year == 1]$ever_referred_y3to11)

for (i in 2:10) {
  ci_mat_overall[i - 1, 2] <- round(table(ci_tab[, i, with = F])[2], -1)
  ci_mat_overall[i - 1, 3] <- round(prop.table(table(ci_tab[, i, with = F])) * 100, 0)[2]
}

ci_mat_overall
write.csv(ci_mat_overall, "OUTPUTS/SEN CI/csc/ci-referral.csv")

graph_df <- ci_mat_overall[, c(1, 3)]
graph_df <- rbind(c(-1, NA),
                  c(0, NA),
                  c(1, NA),
                  c(2, NA),
                  graph_df)

graph_df$exposure <- "Referral"

rm(i, ci_tab, ci_mat_overall)

# * assessment --------------------------------------------------------------

ci_tab <- data.frame(
  pupilmatchingrefanonymous = cohort2[year == 0 & sch_n_in_year == 1]$pupilmatchingrefanonymous
)

ci_tab$y3 <- as.logical(NA)
ci_tab$y4 <- as.logical(NA)
ci_tab$y5 <- as.logical(NA)
ci_tab$y6 <- as.logical(NA)
ci_tab$y7 <- as.logical(NA)
ci_tab$y8 <- as.logical(NA)
ci_tab$y9 <- as.logical(NA)
ci_tab$y10 <- as.logical(NA)
ci_tab$y11 <- as.logical(NA)

for (i in 1:9) {
  ci_tab[, i + 1] <- ci_tab$pupilmatchingrefanonymous %in% cin[referralnfa == 0 &
    (acadyr %in% years[4:(i + 3)] | (referraldate >= dates[4] & referraldate < dates[i + 4]))]$pupilmatchingrefanonymous
}

ci_tab <- data.table(ci_tab)

ci_mat_overall <- data.frame(
  year = 3:11,
  n = rep(NA, 9),
  p = rep(NA, 9)
)

for (i in 2:10) {
  ci_mat_overall[i - 1, 2] <- table(ci_tab[, i, with = F])[2]
  ci_mat_overall[i - 1, 3] <- round(prop.table(table(ci_tab[, i, with = F])) * 100, 0)[2]
}

ci_mat_overall
table(cohort2[year == 0 & sch_n_in_year == 1]$ever_assessed_y3to11)

for (i in 2:10) {
  ci_mat_overall[i - 1, 2] <- round(table(ci_tab[, i, with = F])[2], -1)
  ci_mat_overall[i - 1, 3] <- round(prop.table(table(ci_tab[, i, with = F])) * 100, 0)[2]
}

ci_mat_overall
write.csv(ci_mat_overall, "OUTPUTS/SEN CI/csc/ci-assessment.csv")

ci_mat_overall$exposure <- "Assessment"
graph_df <- rbind(graph_df,
                  c(-1, NA, "Assessment"),
                  c(0, NA, "Assessment"),
                  c(1, NA, "Assessment"),
                  c(2, NA, "Assessment"),
                  ci_mat_overall[, c(1, 3:4)])

rm(i, ci_tab, ci_mat_overall)

# * need --------------------------------------------------------------

ci_tab <- data.frame(
  pupilmatchingrefanonymous = cohort2[year == 0 & sch_n_in_year == 1]$pupilmatchingrefanonymous
)

ci_tab$y3 <- as.logical(NA)
ci_tab$y4 <- as.logical(NA)
ci_tab$y5 <- as.logical(NA)
ci_tab$y6 <- as.logical(NA)
ci_tab$y7 <- as.logical(NA)
ci_tab$y8 <- as.logical(NA)
ci_tab$y9 <- as.logical(NA)
ci_tab$y10 <- as.logical(NA)
ci_tab$y11 <- as.logical(NA)

for (i in 1:9) {
  ci_tab[, i + 1] <- ci_tab$pupilmatchingrefanonymous %in%
    cin[referralnfa == 0 & (reasonforclosure != "RC8" | is.na(reasonforclosure)) &
          (acadyr %in% years[4:(i + 3)] | (referraldate >= dates[4] & referraldate < dates[i + 4]))]$pupilmatchingrefanonymous
}

ci_tab <- data.table(ci_tab)

ci_mat_overall <- data.frame(
  year = 3:11,
  n = rep(NA, 9),
  p = rep(NA, 9)
)

for (i in 2:10) {
  ci_mat_overall[i - 1, 2] <- table(ci_tab[, i, with = F])[2]
  ci_mat_overall[i - 1, 3] <- round(prop.table(table(ci_tab[, i, with = F])) * 100, 0)[2]
}

ci_mat_overall
table(cohort2[year == 0 & sch_n_in_year == 1]$ever_cin_y3to11)

for (i in 2:10) {
  ci_mat_overall[i - 1, 2] <- round(table(ci_tab[, i, with = F])[2], -1)
  ci_mat_overall[i - 1, 3] <- round(prop.table(table(ci_tab[, i, with = F])) * 100, 0)[2]
}

ci_mat_overall
write.csv(ci_mat_overall, "OUTPUTS/SEN CI/csc/ci-need.csv")

ci_mat_overall$exposure <- "CiN"
graph_df <- rbind(graph_df,
                  c(-1, NA, "CiN"),
                  c(0, NA, "CiN"),
                  c(1, NA, "CiN"),
                  c(2, NA, "CiN"),
                  ci_mat_overall[, c(1, 3:4)])

rm(i, ci_tab, ci_mat_overall)

# * cpp -------------------------------------------------------------------

ci_tab <- data.frame(
  pupilmatchingrefanonymous = cohort2[year == 0 & sch_n_in_year == 1]$pupilmatchingrefanonymous
)

ci_tab$y3 <- as.logical(NA)
ci_tab$y4 <- as.logical(NA)
ci_tab$y5 <- as.logical(NA)
ci_tab$y6 <- as.logical(NA)
ci_tab$y7 <- as.logical(NA)
ci_tab$y8 <- as.logical(NA)
ci_tab$y9 <- as.logical(NA)
ci_tab$y10 <- as.logical(NA)
ci_tab$y11 <- as.logical(NA)

for (i in 1:9) {
  ci_tab[, i + 1] <- ci_tab$pupilmatchingrefanonymous %in%
    cin[!is.na(cppstartdate) &
          (acadyr %in% years[4:(i + 3)] | (referraldate >= dates[4] & referraldate < dates[i + 4]))]$pupilmatchingrefanonymous
}

ci_tab <- data.table(ci_tab)

ci_mat_overall <- data.frame(
  year = 3:11,
  n = rep(NA, 9),
  p = rep(NA, 9)
)

for (i in 2:10) {
  ci_mat_overall[i - 1, 2] <- table(ci_tab[, i, with = F])[2]
  ci_mat_overall[i - 1, 3] <- round(prop.table(table(ci_tab[, i, with = F])) * 100, 1)[2]
}

ci_mat_overall
table(cohort2[year == 0 & sch_n_in_year == 1]$ever_cpp_y3to11)

for (i in 2:10) {
  ci_mat_overall[i - 1, 2] <- round(table(ci_tab[, i, with = F])[2], -1)
  ci_mat_overall[i - 1, 3] <- round(prop.table(table(ci_tab[, i, with = F])) * 100, 1)[2]
}

ci_mat_overall
write.csv(ci_mat_overall, "OUTPUTS/SEN CI/csc/ci-cpp.csv")

ci_mat_overall$exposure <- "CPP"
graph_df <- rbind(graph_df,
                  c(-1, NA, "CPP"),
                  c(0, NA, "CPP"),
                  c(1, NA, "CPP"),
                  c(2, NA, "CPP"),
                  ci_mat_overall[, c(1, 3:4)])

rm(i, ci_tab, ci_mat_overall)

# * cla -------------------------------------------------------------------

ci_tab <- data.frame(
  pupilmatchingrefanonymous = cohort2[year == 0 & sch_n_in_year == 1]$pupilmatchingrefanonymous
)

ci_tab$y0_start <- as.logical(NA)
ci_tab$y0 <- as.logical(NA)
ci_tab$y1 <- as.logical(NA)
ci_tab$y2 <- as.logical(NA)
ci_tab$y3 <- as.logical(NA)
ci_tab$y4 <- as.logical(NA)
ci_tab$y5 <- as.logical(NA)
ci_tab$y6 <- as.logical(NA)
ci_tab$y7 <- as.logical(NA)
ci_tab$y8 <- as.logical(NA)
ci_tab$y9 <- as.logical(NA)
ci_tab$y10 <- as.logical(NA)
ci_tab$y11 <- as.logical(NA)

for (i in 1:13) {
  ci_tab[, i + 1] <- ci_tab$pupilmatchingrefanonymous %in%
    cla_episodes[dateepisodestarted < dates[i] &
                   (dateepisodeceased >= dates[1] | is.na(dateepisodeceased))]$pmr2
}

ci_tab <- data.table(ci_tab)

ci_mat_overall <- data.frame(
  year = -1:11,
  n = rep(NA, 13),
  p = rep(NA, 13)
)

for (i in 2:14) {
  ci_mat_overall[i - 1, 2] <- table(ci_tab[, i, with = F])[2]
  ci_mat_overall[i - 1, 3] <- round(prop.table(table(ci_tab[, i, with = F])) * 100, 1)[2]
}

ci_mat_overall
table(cohort2[year == 0 & sch_n_in_year == 1]$ever_cla_y0o11)

for (i in 2:14) {
  ci_mat_overall[i - 1, 2] <- round(table(ci_tab[, i, with = F])[2], -1)
  ci_mat_overall[i - 1, 3] <- round(prop.table(table(ci_tab[, i, with = F])) * 100, 1)[2]
}

ci_mat_overall
write.csv(ci_mat_overall, "OUTPUTS/SEN CI/csc/ci-cla.csv")

ci_mat_overall$exposure <- "CLA"
graph_df <- rbind(graph_df,
                  ci_mat_overall[, c(1, 3:4)])

rm(i, ci_tab, ci_mat_overall)

# ** graph --------------------------------------------------------

graph_df$year <- as.integer(graph_df$year)
graph_df$p <- as.numeric(graph_df$p)
graph_df$exposure <- factor(graph_df$exposure,
                            c("Referral", "Assessment",
                              "CiN", "CPP", "CLA"))

tiff("OUTPUTS/SEN CI/csc/csc_ci.tiff", width = 6, height = 6, units = "in", res = 300)
ggplot(data = graph_df) +
  geom_line(aes(x = year, y = p, colour = exposure), size = 1) +
  xlab("By end of year") +
  ylab("Proportion with CSC contct") +
  scale_y_continuous(limits = c(0, 20),
                     expand = c(0, 0)) +
  scale_x_continuous(breaks = -1:11,
                     labels = c("Start of reception", "Reception", 1:11),
                     expand = c(0, 0)) +
  labs(colour = "Exposure") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(colour = "black",
                                   angle = 45, hjust = 1),
        axis.text.y = element_text(colour = "black"),
        legend.key = element_blank(),
        plot.margin = unit(c(10, 10, 10, 25), units = "pt"))
dev.off()

rm(graph_df)

# SEN CUMULATIVE ----------------------------------------------------------

create_ci_data <- function(subgroups = T, pmr_vector, filter_vector, factor_var = NULL) {
  
  if (!subgroups) {
    
    ci_tab <- data.frame(
      pupilmatchingrefanonymous = pmr_vector
    )
    
    ci_tab$y0 <- as.integer(NA)
    ci_tab$y1 <- as.integer(NA)
    ci_tab$y2 <- as.integer(NA)
    ci_tab$y3 <- as.integer(NA)
    ci_tab$y4 <- as.integer(NA)
    ci_tab$y5 <- as.integer(NA)
    ci_tab$y6 <- as.integer(NA)
    ci_tab$y7 <- as.integer(NA)
    ci_tab$y8 <- as.integer(NA)
    ci_tab$y9 <- as.integer(NA)
    ci_tab$y10 <- as.integer(NA)
    ci_tab$y11 <- as.integer(NA)
    
    for (i in 0:11) {
      ci_tab[, i + 2] <- ci_tab$pupilmatchingrefanonymous %in%
        cohort2[year <= i & sen2 %in% filter_vector]$pupilmatchingrefanonymous
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
      ci_list[[i]]$y0 <- as.integer(NA)
      ci_list[[i]]$y1 <- as.integer(NA)
      ci_list[[i]]$y2 <- as.integer(NA)
      ci_list[[i]]$y3 <- as.integer(NA)
      ci_list[[i]]$y4 <- as.integer(NA)
      ci_list[[i]]$y5 <- as.integer(NA)
      ci_list[[i]]$y6 <- as.integer(NA)
      ci_list[[i]]$y7 <- as.integer(NA)
      ci_list[[i]]$y8 <- as.integer(NA)
      ci_list[[i]]$y9 <- as.integer(NA)
      ci_list[[i]]$y10 <- as.integer(NA)
      ci_list[[i]]$y11 <- as.integer(NA)
      
      for (j in 0:11) {
        ci_list[[i]][, j + 2] <- ci_list[[i]]$pupilmatchingrefanonymous %in%
          cohort2[year <= j & sen2 %in% filter_vector]$pupilmatchingrefanonymous
      }
      
      ci_list[[i]] <- data.table(ci_list[[i]])
    }
    return(ci_list)
  }
}



create_ci_output <- function(subgroups = T, ci_data, names_for_cols = NULL, n_round = -1, perc_round = 0) {
  
  if (!subgroups) {
    
    ci_output <- data.frame(
      year = 0:11,
      n = rep(NA, 12),
      p = rep(NA, 12)
    )
    
    for (i in 2:13) {
      ci_output[i - 1, 2] <- round(table(ci_data[, i, with = F])[2], n_round)
      ci_output[i - 1, 3] <- round(prop.table(table(ci_data[, i, with = F])) * 100, perc_round)[2]
    }
    
    return(ci_output)
    
  } else {
    for (i in 1:length(ci_data)) {
      ci_output <- matrix(
        rep(NA, 12 * (length(ci_data) * 2)),
        nrow = 12,
        ncol = (length(ci_data) * 2)
      )
      for (j in 2:13) {
        for (k in 1:length(ci_data)) {
          ci_output[j - 1, k] <- round(table(ci_data[[k]][, j, with = F])[2], n_round)
          ci_output[j - 1, length(ci_data) + k] <- round(prop.table(table(ci_data[[k]][, j, with = F])) * 100, perc_round)[2]
        }
      }
    }
    
    colnames(ci_output) <- c(names_for_cols, names_for_cols)
    
    return(ci_output)
    
  }
}

# * any sen  --------------------------------------------------------------

ci_data <- create_ci_data(subgroups = F,
                          pmr_vector = cohort2[year == 0 & sch_n_in_year == 1]$pupilmatchingrefanonymous,
                          filter_vector = c("AAPS", "SEHCP"))

ci_output <- create_ci_output(subgroups = F, ci_data = ci_data)

ci_output
write.csv(ci_output, "OUTPUTS/SEN CI/sen/any/any-sen-ci.csv")
rm(ci_data, ci_output)

# * any gender ---------------------------------------------------------------

ci_data <- create_ci_data(pmr_vector = cohort2[year == 0 & sch_n_in_year == 1]$pupilmatchingrefanonymous,
                          filter_vector = c("AAPS", "SEHCP"),
                          factor_var = cohort2[year == 0 & sch_n_in_year == 1]$female.clean + 1) # + 1 to convert boolean to 1 (M) and 2 (F)

ci_output <- create_ci_output(ci_data = ci_data, names_for_cols = c("Male", "Female"))
ci_output
write.csv(ci_output, "OUTPUTS/SEN CI/sen/any/any-ci-female.csv")

ci_output <- data.table(ci_output[, 3:4])
ci_output$year <- 0:11
ci_output <- melt(ci_output,
                      variable.name = "Gender",
                      measure.vars = c("Male", "Female"),
                      value.name = "Perc")

p1 <- ggplot(data = ci_output) +
  geom_line(aes(x = year, y = Perc, colour = Gender), size = 1) +
  xlab("Year") +
  ylab("Any SEND provision (%)") +
  scale_y_continuous(limits = c(0, 100),
                     expand = c(0, 0)) +
  scale_x_continuous(breaks = 0:11,
                     labels = c("Reception", 1:11),
                     expand = c(0, 0)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(colour = "black",
                                   angle = 45, hjust = 1),
        axis.text.y = element_text(colour = "black"),
        legend.key = element_blank())

rm(ci_data, ci_output)

# * any ethnicity ------------------------------------------------------------

ci_data <- create_ci_data(pmr_vector = cohort2[year == 0 & sch_n_in_year == 1]$pupilmatchingrefanonymous,
                          filter_vector = c("AAPS", "SEHCP"),
                          factor_var = cohort2[year == 0 & sch_n_in_year == 1]$eth_broad_custom)
ci_output <- create_ci_output(ci_data = ci_data, names_for_cols = names(table(cohort2$eth_broad_custom)))
ci_output

write.csv(ci_output, "OUTPUTS/SEN CI/sen/any/any-ci-eth.csv")

ci_output <- data.table(ci_output[, 7:12])
ci_output$year <- 0:11
ci_output <- melt(ci_output,
                  variable.name = "Ethnicity",
                  measure.vars = names(ci_output)[-7],
                  value.name = "Perc")

p2 <- ggplot() +
  geom_line(aes(x = year, y = Perc, colour = Ethnicity),
            size = 1,
            data = ci_output) +
  xlab("Year") +
  ylab("Any SEND provision (%)") +
  scale_y_continuous(limits = c(0, 100),
                     expand = c(0, 0)) +
  scale_x_continuous(breaks = 0:11,
                     labels = c("Reception", 1:11),
                     expand = c(0, 0)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(colour = "black",
                                   angle = 45, hjust = 1),
        axis.text.y = element_text(colour = "black"),
        legend.key = element_blank())

rm(ci_data, ci_output)

# * any language -------------------------------------------------------------

ci_data <- create_ci_data(pmr_vector = cohort2[year == 0 & sch_n_in_year == 1]$pupilmatchingrefanonymous,
                          filter_vector = c("AAPS", "SEHCP"),
                          factor_var = cohort2[year == 0 & sch_n_in_year == 1]$lang.clean)
ci_output <- create_ci_output(ci_data = ci_data, names_for_cols = names(table(cohort2$lang.clean)))
ci_output
write.csv(ci_output, "OUTPUTS/SEN CI/sen/any/any-ci-lang.csv")

ci_output <- data.table(ci_output[, 4:5])
ci_output$year <- 0:11
ci_output <- melt(ci_output,
                  variable.name = "Language",
                  measure.vars = names(ci_output)[-3],
                  value.name = "Perc")

p3 <- ggplot() +
  geom_line(aes(x = year, y = Perc, colour = Language),
            size = 1,
            data = ci_output) +
  xlab("Year") +
  ylab("Any SEND provision (%)") +
  scale_y_continuous(limits = c(0, 100),
                     expand = c(0, 0)) +
  scale_x_continuous(breaks = 0:11,
                     labels = c("Reception", 1:11),
                     expand = c(0, 0)) +
  labs(colour = "First language") +
  scale_color_discrete(breaks = c("eng", "oth"),
                       labels = c("English", "Other")) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(colour = "black",
                                   angle = 45, hjust = 1),
        axis.text.y = element_text(colour = "black"),
        legend.key = element_blank())

rm(ci_data, ci_output)

# * any idaci year 0 ---------------------------------------------------------

ci_data <- create_ci_data(pmr_vector = cohort2[year == 0 & sch_n_in_year == 1]$pupilmatchingrefanonymous,
                          filter_vector = c("AAPS", "SEHCP"),
                          factor_var = cohort2[year == 0 & sch_n_in_year == 1]$idaci_y0)
ci_output <- create_ci_output(ci_data = ci_data, names_for_cols = names(table(cohort2$idaci_y0)))
ci_output
write.csv(ci_output, "OUTPUTS/SEN CI/sen/any/any-ci-idaci_y0.csv")

ci_output <- data.table(ci_output[, 7:11])
ci_output$year <- 0:11
ci_output <- melt(ci_output,
                  variable.name = "IDACI_Quintile",
                  measure.vars = names(ci_output)[-6],
                  value.name = "Perc")

p4 <- ggplot() +
  geom_line(aes(x = year, y = Perc, colour = IDACI_Quintile),
            size = 1,
            data = ci_output) +
  xlab("Year") +
  ylab("Any SEND provision (%)") +
  scale_y_continuous(limits = c(0, 100),
                     expand = c(0, 0)) +
  scale_x_continuous(breaks = 0:11,
                     labels = c("Reception", 1:11),
                     expand = c(0, 0)) +
  scale_color_discrete(breaks = 1:5,
                       labels = c("1 (most deprived)", 2:4, "5 (least deprived)")) +
  labs(colour = "IDACI quintile") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(colour = "black",
                                   angle = 45, hjust = 1),
        axis.text.y = element_text(colour = "black"),
        legend.key = element_blank())

rm(ci_data, ci_output)

# * any fsm year 0 -----------------------------------------------------------

ci_data <- create_ci_data(pmr_vector = cohort2[year == 0 & sch_n_in_year == 1]$pupilmatchingrefanonymous,
                          filter_vector = c("AAPS", "SEHCP"),
                          factor_var = cohort2[year == 0 & sch_n_in_year == 1]$fsm_y0)
ci_output <- create_ci_output(ci_data = ci_data, names_for_cols = names(table(cohort2$fsm_y0)))
ci_output
write.csv(ci_output, "OUTPUTS/SEN CI/sen/any/any-ci-fsm.csv")

ci_output <- data.table(ci_output[, 3:4])
ci_output$year <- 0:11
ci_output <- melt(ci_output,
                  variable.name = "FSM_Eligible",
                  measure.vars = names(ci_output)[-3],
                  value.name = "Perc")

p5 <- ggplot() +
  geom_line(aes(x = year, y = Perc, colour = FSM_Eligible),
            size = 1,
            data = ci_output) +
  xlab("Year") +
  ylab("Any SEND provision (%)") +
  scale_y_continuous(limits = c(0, 100),
                     expand = c(0, 0)) +
  scale_x_continuous(breaks = 0:11,
                     labels = c("Reception", 1:11),
                     expand = c(0, 0)) +
  labs(colour = "Free school meals eligible") +
  scale_color_discrete(breaks = 0:1,
                       labels = c("No", "Yes")) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text = element_text(colour = "black"),
        legend.key = element_blank())

rm(ci_data, ci_output)

# * any graph -------------------------------------------------------------

tiff("OUTPUTS/SEN CI/sen/any/any-sen-graphs.tiff", width = 8, height = 12, units = "in", res = 300)
grid.arrange(p1, p2, p3, p4, p5, nrow = 3)
dev.off()

rm(p1, p2, p3, p4, p5)

# * aaps gender ---------------------------------------------------------------

ci_data <- create_ci_data(pmr_vector = cohort2[year == 0 & sch_n_in_year == 1]$pupilmatchingrefanonymous,
                          filter_vector = c("AAPS"),
                          factor_var = cohort2[year == 0 & sch_n_in_year == 1]$female.clean + 1) # + 1 to convert boolean to 1 (M) and 2 (F)

ci_output <- create_ci_output(ci_data = ci_data, names_for_cols = c("Male", "Female"))
ci_output
write.csv(ci_output, "OUTPUTS/SEN CI/sen/aaps/aaps-ci-female.csv")

ci_output <- data.table(ci_output[, 3:4])
ci_output$year <- 0:11
ci_output <- melt(ci_output,
                  variable.name = "Gender",
                  measure.vars = c("Male", "Female"),
                  value.name = "Perc")

p1 <- ggplot(data = ci_output) +
  geom_line(aes(x = year, y = Perc, colour = Gender), size = 1) +
  xlab("Year") +
  ylab("AAPS (%)") +
  scale_y_continuous(limits = c(0, 100),
                     expand = c(0, 0)) +
  scale_x_continuous(breaks = 0:11,
                     labels = c("Reception", 1:11),
                     expand = c(0, 0)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(colour = "black",
                                   angle = 45, hjust = 1),
        axis.text.y = element_text(colour = "black"),
        legend.key = element_blank())

rm(ci_data, ci_output)

# * aaps ethnicity ------------------------------------------------------------

ci_data <- create_ci_data(pmr_vector = cohort2[year == 0 & sch_n_in_year == 1]$pupilmatchingrefanonymous,
                          filter_vector = c("AAPS"),
                          factor_var = cohort2[year == 0 & sch_n_in_year == 1]$eth_broad_custom)
ci_output <- create_ci_output(ci_data = ci_data, names_for_cols = names(table(cohort2$eth_broad_custom)))
ci_output

write.csv(ci_output, "OUTPUTS/SEN CI/sen/aaps/aaps-ci-eth.csv")

ci_output <- data.table(ci_output[, 7:12])
ci_output$year <- 0:11
ci_output <- melt(ci_output,
                  variable.name = "Ethnicity",
                  measure.vars = names(ci_output)[-7],
                  value.name = "Perc")

p2 <- ggplot() +
  geom_line(aes(x = year, y = Perc, colour = Ethnicity),
            size = 1,
            data = ci_output) +
  xlab("Year") +
  ylab("AAPS (%)") +
  scale_y_continuous(limits = c(0, 100),
                     expand = c(0, 0)) +
  scale_x_continuous(breaks = 0:11,
                     labels = c("Reception", 1:11),
                     expand = c(0, 0)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(colour = "black",
                                   angle = 45, hjust = 1),
        axis.text.y = element_text(colour = "black"),
        legend.key = element_blank())

rm(ci_data, ci_output)

# * aaps language -------------------------------------------------------------

ci_data <- create_ci_data(pmr_vector = cohort2[year == 0 & sch_n_in_year == 1]$pupilmatchingrefanonymous,
                          filter_vector = c("AAPS"),
                          factor_var = cohort2[year == 0 & sch_n_in_year == 1]$lang.clean)
ci_output <- create_ci_output(ci_data = ci_data, names_for_cols = names(table(cohort2$lang.clean)))
ci_output
write.csv(ci_output, "OUTPUTS/SEN CI/sen/aaps/aaps-ci-lang.csv")

ci_output <- data.table(ci_output[, 4:5])
ci_output$year <- 0:11
ci_output <- melt(ci_output,
                  variable.name = "Language",
                  measure.vars = names(ci_output)[-3],
                  value.name = "Perc")

p3 <- ggplot() +
  geom_line(aes(x = year, y = Perc, colour = Language),
            size = 1,
            data = ci_output) +
  xlab("Year") +
  ylab("AAPS (%)") +
  scale_y_continuous(limits = c(0, 100),
                     expand = c(0, 0)) +
  scale_x_continuous(breaks = 0:11,
                     labels = c("Reception", 1:11),
                     expand = c(0, 0)) +
  labs(colour = "First language") +
  scale_colour_discrete(breaks = c("eng", "oth"),
                        labels = c("English", "Other")) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(colour = "black",
                                   angle = 45, hjust = 1),
        axis.text.y = element_text(colour = "black"),
        legend.key = element_blank())

rm(ci_data, ci_output)

# * any idaci year 0 ---------------------------------------------------------

ci_data <- create_ci_data(pmr_vector = cohort2[year == 0 & sch_n_in_year == 1]$pupilmatchingrefanonymous,
                          filter_vector = c("AAPS"),
                          factor_var = cohort2[year == 0 & sch_n_in_year == 1]$idaci_y0)
ci_output <- create_ci_output(ci_data = ci_data, names_for_cols = names(table(cohort2$idaci_y0)))
ci_output
write.csv(ci_output, "OUTPUTS/SEN CI/sen/aaps/aaps-ci-idaci_y0.csv")

ci_output <- data.table(ci_output[, 7:11])
ci_output$year <- 0:11
ci_output <- melt(ci_output,
                  variable.name = "IDACI_Quintile",
                  measure.vars = names(ci_output)[-6],
                  value.name = "Perc")

p4 <- ggplot() +
  geom_line(aes(x = year, y = Perc, colour = IDACI_Quintile),
            size = 1,
            data = ci_output) +
  xlab("Year") +
  ylab("AAPS (%)") +
  scale_y_continuous(limits = c(0, 100),
                     expand = c(0, 0)) +
  scale_x_continuous(breaks = 0:11,
                     labels = c("Reception", 1:11),
                     expand = c(0, 0)) +
  scale_colour_discrete(breaks = 1:5,
                      labels = c("1 (most deprived)", 2:4, "5 (least deprived)")) +
  labs(colour = "IDACI quintile") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(colour = "black",
                                   angle = 45, hjust = 1),
        axis.text.y = element_text(colour = "black"),
        legend.key = element_blank())

rm(ci_data, ci_output)

# * aaps fsm year 0 -----------------------------------------------------------

ci_data <- create_ci_data(pmr_vector = cohort2[year == 0 & sch_n_in_year == 1]$pupilmatchingrefanonymous,
                          filter_vector = c("AAPS"),
                          factor_var = cohort2[year == 0 & sch_n_in_year == 1]$fsm_y0)
ci_output <- create_ci_output(ci_data = ci_data, names_for_cols = names(table(cohort2$fsm_y0)))
ci_output
write.csv(ci_output, "OUTPUTS/SEN CI/sen/aaps/aaps-ci-fsm.csv")

ci_output <- data.table(ci_output[, 3:4])
ci_output$year <- 0:11
ci_output <- melt(ci_output,
                  variable.name = "FSM_Eligible",
                  measure.vars = names(ci_output)[-3],
                  value.name = "Perc")

p5 <- ggplot() +
  geom_line(aes(x = year, y = Perc, colour = FSM_Eligible),
            size = 1,
            data = ci_output) +
  xlab("Year") +
  ylab("AAPS (%)") +
  scale_y_continuous(limits = c(0, 100),
                     expand = c(0, 0)) +
  scale_x_continuous(breaks = 0:11,
                     labels = c("Reception", 1:11),
                     expand = c(0, 0)) +
  labs(colour = "Free school meals eligible") +
  scale_colour_discrete(breaks = 0:1,
                      labels = c("No", "Yes")) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text = element_text(colour = "black"),
        legend.key = element_blank())

rm(ci_data, ci_output)

# * aaps graph -------------------------------------------------------------

tiff("OUTPUTS/SEN CI/sen/aaps/aaps-sen-graphs.tiff", width = 8, height = 12, units = "in", res = 300)
grid.arrange(p1, p2, p3, p4, p5, nrow = 3)
dev.off()

rm(p1, p2, p3, p4, p5)

# * sehcp gender ---------------------------------------------------------------

ci_data <- create_ci_data(pmr_vector = cohort2[year == 0 & sch_n_in_year == 1]$pupilmatchingrefanonymous,
                          filter_vector = c("SEHCP"),
                          factor_var = cohort2[year == 0 & sch_n_in_year == 1]$female.clean + 1) # + 1 to convert boolean to 1 (M) and 2 (F)

ci_output <- create_ci_output(ci_data = ci_data, names_for_cols = c("Male", "Female"))
ci_output
write.csv(ci_output, "OUTPUTS/SEN CI/sen/sehcp/sehcp-ci-female.csv")

ci_output <- data.table(ci_output[, 3:4])
ci_output$year <- 0:11
ci_output <- melt(ci_output,
                  variable.name = "Gender",
                  measure.vars = c("Male", "Female"),
                  value.name = "Perc")

p1 <- ggplot(data = ci_output) +
  geom_line(aes(x = year, y = Perc, colour = Gender), size = 1) +
  xlab("Year") +
  ylab("SEHCP (%)") +
  scale_y_continuous(limits = c(0, 20),
                     expand = c(0, 0)) +
  scale_x_continuous(breaks = 0:11,
                     labels = c("Reception", 1:11),
                     expand = c(0, 0)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(colour = "black",
                                   angle = 45, hjust = 1),
        axis.text.y = element_text(colour = "black"),
        legend.key = element_blank())

rm(ci_data, ci_output)

# * sehcp ethnicity ------------------------------------------------------------

ci_data <- create_ci_data(pmr_vector = cohort2[year == 0 & sch_n_in_year == 1]$pupilmatchingrefanonymous,
                          filter_vector = c("SEHCP"),
                          factor_var = cohort2[year == 0 & sch_n_in_year == 1]$eth_broad_custom)
ci_output <- create_ci_output(ci_data = ci_data, names_for_cols = names(table(cohort2$eth_broad_custom)))
ci_output

write.csv(ci_output, "OUTPUTS/SEN CI/sen/sehcp/sehcp-ci-eth.csv")

ci_output <- data.table(ci_output[, 7:12])
ci_output$year <- 0:11
ci_output <- melt(ci_output,
                  variable.name = "Ethnicity",
                  measure.vars = names(ci_output)[-7],
                  value.name = "Perc")

p2 <- ggplot() +
  geom_line(aes(x = year, y = Perc, colour = Ethnicity),
            size = 1,
            data = ci_output) +
  xlab("Year") +
  ylab("SEHCP (%)") +
  scale_y_continuous(limits = c(0, 20),
                     expand = c(0, 0)) +
  scale_x_continuous(breaks = 0:11,
                     labels = c("Reception", 1:11),
                     expand = c(0, 0)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(colour = "black",
                                   angle = 45, hjust = 1),
        axis.text.y = element_text(colour = "black"),
        legend.key = element_blank())

rm(ci_data, ci_output)

# * sehcp language -------------------------------------------------------------

ci_data <- create_ci_data(pmr_vector = cohort2[year == 0 & sch_n_in_year == 1]$pupilmatchingrefanonymous,
                          filter_vector = c("SEHCP"),
                          factor_var = cohort2[year == 0 & sch_n_in_year == 1]$lang.clean)
ci_output <- create_ci_output(ci_data = ci_data, names_for_cols = names(table(cohort2$lang.clean)))
ci_output
write.csv(ci_output, "OUTPUTS/SEN CI/sen/sehcp/sehcp-ci-lang.csv")

ci_output <- data.table(ci_output[, 4:5])
ci_output$year <- 0:11
ci_output <- melt(ci_output,
                  variable.name = "Language",
                  measure.vars = names(ci_output)[-3],
                  value.name = "Perc")

p3 <- ggplot() +
  geom_line(aes(x = year, y = Perc, colour = Language),
            size = 1,
            data = ci_output) +
  xlab("Year") +
  ylab("SEHCP (%)") +
  scale_y_continuous(limits = c(0, 20),
                     expand = c(0, 0)) +
  scale_x_continuous(breaks = 0:11,
                     labels = c("Reception", 1:11),
                     expand = c(0, 0)) +
  labs(colour = "First language") +
  scale_colour_discrete(breaks = c("eng", "oth"),
                      labels = c("English", "Other")) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(colour = "black",
                                   angle = 45, hjust = 1),
        axis.text.y = element_text(colour = "black"),
        legend.key = element_blank())

rm(ci_data, ci_output)

# * sehcp idaci year 0 ---------------------------------------------------------

ci_data <- create_ci_data(pmr_vector = cohort2[year == 0 & sch_n_in_year == 1]$pupilmatchingrefanonymous,
                          filter_vector = c("SEHCP"),
                          factor_var = cohort2[year == 0 & sch_n_in_year == 1]$idaci_y0)
ci_output <- create_ci_output(ci_data = ci_data, names_for_cols = names(table(cohort2$idaci_y0)))
ci_output
write.csv(ci_output, "OUTPUTS/SEN CI/sen/sehcp/sehcp-ci-idaci_y0.csv")

ci_output <- data.table(ci_output[, 7:11])
ci_output$year <- 0:11
ci_output <- melt(ci_output,
                  variable.name = "IDACI_Quintile",
                  measure.vars = names(ci_output)[-6],
                  value.name = "Perc")

p4 <- ggplot() +
  geom_line(aes(x = year, y = Perc, colour = IDACI_Quintile),
            size = 1,
            data = ci_output) +
  xlab("Year") +
  ylab("SEHCP (%)") +
  scale_y_continuous(limits = c(0, 20),
                     expand = c(0, 0)) +
  scale_x_continuous(breaks = 0:11,
                     labels = c("Reception", 1:11),
                     expand = c(0, 0)) +
  scale_colour_discrete(breaks = 1:5,
                      labels = c("1 (most deprived)", 2:4, "5 (least deprived)")) +
  labs(colour = "IDACI quintile") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(colour = "black",
                                   angle = 45, hjust = 1),
        axis.text.y = element_text(colour = "black"),
        legend.key = element_blank())

rm(ci_data, ci_output)

# * sehcp fsm year 0 -----------------------------------------------------------

ci_data <- create_ci_data(pmr_vector = cohort2[year == 0 & sch_n_in_year == 1]$pupilmatchingrefanonymous,
                          filter_vector = c("SEHCP"),
                          factor_var = cohort2[year == 0 & sch_n_in_year == 1]$fsm_y0)
ci_output <- create_ci_output(ci_data = ci_data, names_for_cols = names(table(cohort2$fsm_y0)))
ci_output
write.csv(ci_output, "OUTPUTS/SEN CI/sen/sehcp/sehcp-ci-fsm.csv")

ci_output <- data.table(ci_output[, 3:4])
ci_output$year <- 0:11
ci_output <- melt(ci_output,
                  variable.name = "FSM_Eligible",
                  measure.vars = names(ci_output)[-3],
                  value.name = "Perc")

p5 <- ggplot() +
  geom_line(aes(x = year, y = Perc, colour = FSM_Eligible),
            size = 1,
            data = ci_output) +
  xlab("Year") +
  ylab("SEHCP (%)") +
  scale_y_continuous(limits = c(0, 20),
                     expand = c(0, 0)) +
  scale_x_continuous(breaks = 0:11,
                     labels = c("Reception", 1:11),
                     expand = c(0, 0)) +
  labs(colour = "Free school meals eligible") +
  scale_colour_discrete(breaks = 0:1,
                      labels = c("No", "Yes")) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text = element_text(colour = "black"),
        legend.key = element_blank())

rm(ci_data, ci_output)

# * sehcp graph -------------------------------------------------------------

tiff("OUTPUTS/SEN CI/sen/sehcp/sehcp-sen-graphs.tiff", width = 8, height = 12, units = "in", res = 300)
grid.arrange(p1, p2, p3, p4, p5, nrow = 3)
dev.off()

rm(p1, p2, p3, p4, p5)

# SEN BY CSC --------------------------------------------------------------

# * any -------------------------------------------------------------------

ci_data <- create_ci_data(pmr_vector = cohort2[year == 0 & sch_n_in_year == 1]$pupilmatchingrefanonymous,
                          filter_vector = c("AAPS", "SEHCP"),
                          factor_var = cohort2[year == 0 & sch_n_in_year == 1]$highest_ever_csc)
ci_output <- create_ci_output(ci_data = ci_data, names_for_cols = names(table(cohort2$highest_ever_csc)))
ci_output
write.csv(ci_output, "OUTPUTS/SEN CI/sen/csc/any-ci-highest_ever_csc.csv")

ci_output <- data.table(ci_output[, 7:12])
ci_output$year <- 0:11
ci_output <- melt(ci_output,
                  variable.name = "Exposure",
                  measure.vars = names(ci_output)[-7],
                  value.name = "Perc")

p1 <- ggplot() +
  geom_line(aes(x = year, y = Perc, colour = Exposure),
            size = 1,
            data = ci_output) +
  xlab("Year") +
  ylab("Any SEND provision (%)") +
  scale_y_continuous(limits = c(0, 100),
                     expand = c(0, 0)) +
  scale_x_continuous(breaks = 0:11,
                     labels = c("Reception", 1:11),
                     expand = c(0, 0)) +
  labs(colour = "Highest exposure level") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(colour = "black",
                                   angle = 45, hjust = 1),
        axis.text.y = element_text(colour = "black"),
        legend.position = "none")

rm(ci_data, ci_output)

# * aaps ------------------------------------------------------------------

ci_data <- create_ci_data(pmr_vector = cohort2[year == 0 & sch_n_in_year == 1]$pupilmatchingrefanonymous,
                          filter_vector = c("AAPS"),
                          factor_var = cohort2[year == 0 & sch_n_in_year == 1]$highest_ever_csc)
ci_output <- create_ci_output(ci_data = ci_data, names_for_cols = names(table(cohort2$highest_ever_csc)))
ci_output
write.csv(ci_output, "OUTPUTS/SEN CI/sen/csc/aaps-ci-highest_ever_csc.csv")

ci_output <- data.table(ci_output[, 7:12])
ci_output$year <- 0:11
ci_output <- melt(ci_output,
                  variable.name = "Exposure",
                  measure.vars = names(ci_output)[-7],
                  value.name = "Perc")

p2 <- ggplot() +
  geom_line(aes(x = year, y = Perc, colour = Exposure),
            size = 1,
            data = ci_output) +
  xlab("Year") +
  ylab("AAPS (%)") +
  scale_y_continuous(limits = c(0, 100),
                     expand = c(0, 0)) +
  scale_x_continuous(breaks = 0:11,
                     labels = c("Reception", 1:11),
                     expand = c(0, 0)) +
  labs(colour = "Highest exposure level") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(colour = "black",
                                   angle = 45, hjust = 1),
        axis.text.y = element_text(colour = "black"),
        legend.position = "none")

rm(ci_data, ci_output)

# * sehcp -----------------------------------------------------------------

ci_data <- create_ci_data(pmr_vector = cohort2[year == 0 & sch_n_in_year == 1]$pupilmatchingrefanonymous,
                          filter_vector = c("SEHCP"),
                          factor_var = cohort2[year == 0 & sch_n_in_year == 1]$highest_ever_csc)
ci_output <- create_ci_output(ci_data = ci_data, names_for_cols = names(table(cohort2$highest_ever_csc)))
ci_output
write.csv(ci_output, "OUTPUTS/SEN CI/sen/csc/sehcp-ci-highest_ever_csc.csv")

ci_output <- data.table(ci_output[, 7:12])
ci_output$year <- 0:11
ci_output <- melt(ci_output,
                  variable.name = "Exposure",
                  measure.vars = names(ci_output)[-7],
                  value.name = "Perc")

p3 <- ggplot() +
  geom_line(aes(x = year, y = Perc, colour = Exposure),
            size = 1,
            data = ci_output) +
  xlab("Year") +
  ylab("SEHCP (%)") +
  scale_y_continuous(limits = c(0, 100),
                     expand = c(0, 0)) +
  scale_x_continuous(breaks = 0:11,
                     labels = c("Reception", 1:11),
                     expand = c(0, 0)) +
  labs(colour = "Highest exposure level") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(colour = "black",
                                   angle = 45, hjust = 1),
        axis.text.y = element_text(colour = "black"),
        legend.key = element_blank(),
        legend.position = c(0.8, 0.75))

rm(ci_data, ci_output)

# * csc graph -------------------------------------------------------------

tiff("OUTPUTS/SEN CI/sen/csc/csc-sen-graphs.tiff", width = 18, height = 6, units = "in", res = 300)
grid.arrange(p1, p2, p3, nrow = 1)
dev.off()

rm(p1, p2, p3)
