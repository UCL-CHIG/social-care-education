
# LOAD --------------------------------------------------------------------

setwd("P:/Working/Matt")

lib_base = "P:/Working/Matt/r-library/"
assign(".lib.loc", c(.libPaths(), lib_base), envir = environment(.libPaths))
rm(lib_base)

# install.packages("R:/Software Add-Ons/R-library/NEW 3.6 R-Library/lifecycle_0.2.0.zip",
#                  repos = NULL,
#                  lib = "r-library",
#                  type = "win.binary")


library(data.table)
library(tidyr)
library(epiR)

load("PROCESSED DATA/cap4_appendix1/cohort1.rda")
load("PROCESSED DATA/cap4_appendix1/cohort2.rda")

cohort1$complete_record <-
  cohort1$pupilmatchingrefanonymous %in% cohort1[year == 1]$pupilmatchingrefanonymous &
  cohort1$pupilmatchingrefanonymous %in% cohort1[year == 2]$pupilmatchingrefanonymous &
  cohort1$pupilmatchingrefanonymous %in% cohort1[year == 3]$pupilmatchingrefanonymous &
  cohort1$pupilmatchingrefanonymous %in% cohort1[year == 4]$pupilmatchingrefanonymous &
  cohort1$pupilmatchingrefanonymous %in% cohort1[year == 5]$pupilmatchingrefanonymous &
  cohort1$pupilmatchingrefanonymous %in% cohort1[year == 6]$pupilmatchingrefanonymous &
  cohort1$pupilmatchingrefanonymous %in% cohort1[year == 7]$pupilmatchingrefanonymous &
  cohort1$pupilmatchingrefanonymous %in% cohort1[year == 8]$pupilmatchingrefanonymous &
  cohort1$pupilmatchingrefanonymous %in% cohort1[year == 9]$pupilmatchingrefanonymous &
  cohort1$pupilmatchingrefanonymous %in% cohort1[year == 10]$pupilmatchingrefanonymous &
  cohort1$pupilmatchingrefanonymous %in% cohort1[year == 11]$pupilmatchingrefanonymous

cohort2$complete_record <-
  cohort2$pupilmatchingrefanonymous %in% cohort2[year == 0]$pupilmatchingrefanonymous &
  cohort2$pupilmatchingrefanonymous %in% cohort2[year == 1]$pupilmatchingrefanonymous &
  cohort2$pupilmatchingrefanonymous %in% cohort2[year == 2]$pupilmatchingrefanonymous &
  cohort2$pupilmatchingrefanonymous %in% cohort2[year == 3]$pupilmatchingrefanonymous &
  cohort2$pupilmatchingrefanonymous %in% cohort2[year == 4]$pupilmatchingrefanonymous &
  cohort2$pupilmatchingrefanonymous %in% cohort2[year == 5]$pupilmatchingrefanonymous &
  cohort2$pupilmatchingrefanonymous %in% cohort2[year == 6]$pupilmatchingrefanonymous &
  cohort2$pupilmatchingrefanonymous %in% cohort2[year == 7]$pupilmatchingrefanonymous &
  cohort2$pupilmatchingrefanonymous %in% cohort2[year == 8]$pupilmatchingrefanonymous &
  cohort2$pupilmatchingrefanonymous %in% cohort2[year == 9]$pupilmatchingrefanonymous &
  cohort2$pupilmatchingrefanonymous %in% cohort2[year == 10]$pupilmatchingrefanonymous &
  cohort2$pupilmatchingrefanonymous %in% cohort2[year == 11]$pupilmatchingrefanonymous

# NON-ENROLMENT PATTERNS --------------------------------------------------

# * overall -----------------------------------------------

table(!cohort1[year == 1 & sch_n_in_year == 1]$complete_record)
prop.table(table(!cohort1[year == 1 & sch_n_in_year == 1]$complete_record))

table(!cohort2[year == 0 & sch_n_in_year == 1]$complete_record)
prop.table(table(!cohort2[year == 0 & sch_n_in_year == 1]$complete_record))

# * first non-enrolment (c1) ----------------------------------------------------

# cohort 1
seq_mat_c1 <- data.frame(
  matrix(
    rep(0, 12 * length(unique(cohort1[year == 1 & sch_n_in_year == 1]$pupilmatchingrefanonymous))),
    ncol = 12,
    nrow = length(unique(cohort1$pupilmatchingrefanonymous))
  )
)

colnames(seq_mat_c1) <- c("pupilmatchingrefanonymous", sprintf("y%02d", seq(1, 11))) 

seq_mat_c1$pupilmatchingrefanonymous <- as.character(seq_mat_c1$pupilmatchingrefanonymous)
seq_mat_c1$pupilmatchingrefanonymous <- cohort1[year == 1 & sch_n_in_year == 1]$pupilmatchingrefanonymous

cohort1$enrolled <- T # we just need a binary variable for years when enrolled so the next step gives an NA for not enrolled
tmp1 <- cohort1[sch_n_in_year == 1, c("pupilmatchingrefanonymous", "year", "enrolled")]
tmp1 <- complete(tmp1, pupilmatchingrefanonymous, year)
tmp1 <- data.table(tmp1)
tmp1 <- tmp1[!duplicated(tmp1)] # no idea why some duplication was brought in in year 9

seq_mat_c1$y01 <- tmp1[year == 1]$enrolled
seq_mat_c1$y02 <- tmp1[year == 2]$enrolled
seq_mat_c1$y03 <- tmp1[year == 3]$enrolled
seq_mat_c1$y04 <- tmp1[year == 4]$enrolled
seq_mat_c1$y05 <- tmp1[year == 5]$enrolled
seq_mat_c1$y06 <- tmp1[year == 6]$enrolled
seq_mat_c1$y07 <- tmp1[year == 7]$enrolled
seq_mat_c1$y08 <- tmp1[year == 8]$enrolled
seq_mat_c1$y09 <- tmp1[year == 9]$enrolled
seq_mat_c1$y10 <- tmp1[year == 10]$enrolled
seq_mat_c1$y11 <- tmp1[year == 11]$enrolled

rm(tmp1)

seq_mat_c1 <- data.table(seq_mat_c1)

seq_mat_c1$any_NA <- apply(seq_mat_c1[, 2:12, with = F], 1, anyNA)
seq_mat_c1$total_NA <- apply(seq_mat_c1[, 2:12, with = F], 1, function(x) sum(is.na(x)))

first_absence <- rep(NA, nrow(seq_mat_c1))
pb <- txtProgressBar(min = 1, max = nrow(seq_mat_c1), style = 3)

for (i in 1:nrow(seq_mat_c1)) {
  if (seq_mat_c1$any_NA[i]) {
    first_absence[i] <- min(which(unname(unlist(is.na(seq_mat_c1[i, 2:12])))))
  }
  setTxtProgressBar(pb, i)
}

seq_mat_c1$first_absence <- first_absence
rm(i, pb, first_absence)

write.csv(table(seq_mat_c1$first_absence),
          file = "OUTPUTS/CHAPTER 4/cap4_appendix1/first-non-enrol-c1.csv")

# * enrolment patters (c1) ------------------------------------------------

cov_pats <- epi.cp(seq_mat_c1[, c(2:12)])
seq_mat_c1$cov_pats_id <- cov_pats$id

patterns <- cov_pats$cov.pattern
patterns$perc <- round((patterns$n / sum(patterns$n)) * 100, 2)

patterns2 <- patterns[order(-patterns$n), c(1:2, 14, 3:13)]
patterns2 <- patterns2[1:20, ]
patterns2$pattern <- as.character(1:20)
patterns2 <- patterns2[, c(1, 15, 2:14)]
patterns2[21, ] <- c(NA,
                     "All Others",
                     sum(patterns[21:nrow(patterns), ]$n),
                     sum(patterns[21:nrow(patterns), ]$perc),
                     rep(NA, 11))

write.csv(patterns2,
          file = "OUTPUTS/CHAPTER 4/cap4_appendix1/attrition-patterns-c1.csv",
          row.names = F)

rm(cov_pats, patterns, patterns2)

# first non-enrolment (c2) ------------------------------------------------

seq_mat_c2 <- data.frame(
  matrix(
    rep(0, 13 * length(unique(cohort2[year == 0 & sch_n_in_year == 1]$pupilmatchingrefanonymous))),
    ncol = 13,
    nrow = length(unique(cohort2$pupilmatchingrefanonymous))
  )
)

colnames(seq_mat_c2) <- c("pupilmatchingrefanonymous", sprintf("y%02d", seq(0, 11))) 

seq_mat_c2$pupilmatchingrefanonymous <- as.character(seq_mat_c2$pupilmatchingrefanonymous)
seq_mat_c2$pupilmatchingrefanonymous <- cohort2[year == 0 & sch_n_in_year == 1]$pupilmatchingrefanonymous

cohort2$enrolled <- T # we just need a binary variable for years when enrolled so the next step gives an NA for not enrolled
tmp1 <- cohort2[sch_n_in_year == 1, c("pupilmatchingrefanonymous", "year", "enrolled")]
tmp1 <- complete(tmp1, pupilmatchingrefanonymous, year)
tmp1 <- data.table(tmp1)
tmp1 <- tmp1[!duplicated(tmp1)] # no idea why some duplication was brought in in year 8

seq_mat_c2$y00 <- tmp1[year == 0]$enrolled
seq_mat_c2$y01 <- tmp1[year == 1]$enrolled
seq_mat_c2$y02 <- tmp1[year == 2]$enrolled
seq_mat_c2$y03 <- tmp1[year == 3]$enrolled
seq_mat_c2$y04 <- tmp1[year == 4]$enrolled
seq_mat_c2$y05 <- tmp1[year == 5]$enrolled
seq_mat_c2$y06 <- tmp1[year == 6]$enrolled
seq_mat_c2$y07 <- tmp1[year == 7]$enrolled
seq_mat_c2$y08 <- tmp1[year == 8]$enrolled
seq_mat_c2$y09 <- tmp1[year == 9]$enrolled
seq_mat_c2$y10 <- tmp1[year == 10]$enrolled
seq_mat_c2$y11 <- tmp1[year == 11]$enrolled

rm(tmp1)

seq_mat_c2 <- data.table(seq_mat_c2)

seq_mat_c2$any_NA <- apply(seq_mat_c2[, 2:13, with = F], 1, anyNA)
seq_mat_c2$total_NA <- apply(seq_mat_c2[, 2:13, with = F], 1, function(x) sum(is.na(x)))

first_absence <- rep(NA, nrow(seq_mat_c2))
pb <- txtProgressBar(min = 1, max = nrow(seq_mat_c2), style = 3)

for (i in 1:nrow(seq_mat_c2)) {
  if (seq_mat_c2$any_NA[i]) {
    first_absence[i] <- min(which(unname(unlist(is.na(seq_mat_c2[i, 2:13])))))
  }
  setTxtProgressBar(pb, i)
}

seq_mat_c2$first_absence <- first_absence
rm(i, pb, first_absence)

write.csv(table(seq_mat_c2$first_absence),
          file = "OUTPUTS/CHAPTER 4/cap4_appendix1/first-non-enrol-c2.csv")

# * enrolment patters (c2) ------------------------------------------------

cov_pats <- epi.cp(seq_mat_c2[, c(2:13)])
seq_mat_c2$cov_pats_id <- cov_pats$id

patterns <- cov_pats$cov.pattern
patterns$perc <- round((patterns$n / sum(patterns$n)) * 100, 2)

patterns2 <- patterns[order(-patterns$n), c(1:2, 15, 3:14)]
patterns2 <- patterns2[1:20, ]
patterns2$pattern <- as.character(1:20)
patterns2 <- patterns2[, c(1, 16, 2:15)]
patterns2[21, ] <- c(NA,
                     "All Others",
                     sum(patterns[21:nrow(patterns), ]$n),
                     sum(patterns[21:nrow(patterns), ]$perc),
                     rep(NA, 12))

write.csv(patterns2,
          file = "OUTPUTS/CHAPTER 4/cap4_appendix1/attrition-patterns-c2.csv",
          row.names = F)

rm(cov_pats, patterns, patterns2)

# absences -------------------------------------------------

# f <- list.files("P:/Working/WORKING DATA/ABSENCE")
# abs_list <- list()
# abs_list_c1 <- list()
# abs_list_c2 <- list()
# for (i in 1:12) {
#   print(paste0("trying ", i))
#   abs_list[[i]] <- data.table(read.table(
#     paste0("P:/Working/WORKING DATA/ABSENCE/", f[i]),
#     header = T,
#     sep = "\t",
#     stringsAsFactors = F,
#     skipNul = T,
#     comment.char = ""
#   ))
#   
#   names(abs_list[[i]]) <- tolower(gsub(paste0("_ab", formatC(i + 5,
#                                                              format = "d",
#                                                              flag = "0",
#                                                              width = 2)),
#                                        "",
#                                        names(abs_list[[i]])))
#   
#   abs_list_c1[[i]] <- abs_list[[i]][pupilmatchingrefanonymous %in% cohort1$pupilmatchingrefanonymous]
#   abs_list_c2[[i]] <- abs_list[[i]][pupilmatchingrefanonymous %in% cohort2$pupilmatchingrefanonymous]
#   
#   print(paste0("done ", i))
# }
# 
# rm(f, i, abs_list)
# 
# abs_all_c1 <- rbind(abs_list_c1[[1]],
#                     abs_list_c1[[2]],
#                     abs_list_c1[[3]],
#                     abs_list_c1[[4]],
#                     abs_list_c1[[5]],
#                     abs_list_c1[[6]],
#                     abs_list_c1[[7]],
#                     abs_list_c1[[8]],
#                     abs_list_c1[[9]],
#                     abs_list_c1[[10]],
#                     abs_list_c1[[11]],
#                     abs_list_c1[[12]],
#                     fill = T)
# 
# abs_all_c2 <- rbind(abs_list_c2[[1]],
#                     abs_list_c2[[2]],
#                     abs_list_c2[[3]],
#                     abs_list_c2[[4]],
#                     abs_list_c2[[5]],
#                     abs_list_c2[[6]],
#                     abs_list_c2[[7]],
#                     abs_list_c2[[8]],
#                     abs_list_c2[[9]],
#                     abs_list_c2[[10]],
#                     abs_list_c2[[11]],
#                     abs_list_c2[[12]],
#                     fill = T)
# 
# rm(abs_list_c1, abs_list_c2)
# 
# save(abs_all_c1, file = "P:/Working/Matt/PROCESSED DATA/cap4_appendix1/abs_all_c1.rda")
# save(abs_all_c2, file = "P:/Working/Matt/PROCESSED DATA/cap4_appendix1/abs_all_c2.rda")

load("P:/Working/Matt/PROCESSED DATA/cap4_appendix1/abs_all_c1.rda")
load("P:/Working/Matt/PROCESSED DATA/cap4_appendix1/abs_all_c2.rda")

# * cohort 1 --------------------------------------------------------------

abs_all_c1 <- abs_all_c1[, c("pupilmatchingrefanonymous",
                             "academicyear",
                             names(abs_all_c1)[c(25:44, 70:80)]),
                         with = F] # removes census and flag vars
abs_all_c1 <- abs_all_c1[!duplicated(abs_all_c1)]
abs_all_c1 <- abs_all_c1[order(pupilmatchingrefanonymous, academicyear)]

abs_all_c1$sessionspossible_annual <- abs_all_c1$sessionspossible_autumn + abs_all_c1$sessionspossible_spring + abs_all_c1$sessionspossible_summer
abs_all_c1$authorisedabsence_annual <- abs_all_c1$authorisedabsence_autumn + abs_all_c1$authorisedabsence_spring + abs_all_c1$authorisedabsence_summer
abs_all_c1$unauthorisedabsence_annual <- abs_all_c1$unauthorisedabsence_autumn + abs_all_c1$unauthorisedabsence_spring + abs_all_c1$unauthorisedabsence_summer
abs_all_c1$overallabsence_annual <- abs_all_c1$overallabsence_autumn + abs_all_c1$overallabsence_spring + abs_all_c1$overallabsence_summer

absence_c1 <- abs_all_c1[, c("pupilmatchingrefanonymous", "academicyear",
                             "sessionspossible_annual", "authorisedabsence_annual",
                             "unauthorisedabsence_annual", "overallabsence_annual")]
rm(abs_all_c1)

gc()

absence_c1[, sessionspossible_annual_2 := sum(sessionspossible_annual), by = .(pupilmatchingrefanonymous, academicyear)]
absence_c1[, authorisedabsence_annual_2 := sum(authorisedabsence_annual), by = .(pupilmatchingrefanonymous, academicyear)]
absence_c1[, unauthorisedabsence_annual_2 := sum(unauthorisedabsence_annual), by = .(pupilmatchingrefanonymous, academicyear)]
absence_c1[, overallabsence_annual_2 := sum(overallabsence_annual), by = .(pupilmatchingrefanonymous, academicyear)]

absence_c1 <- absence_c1[, c(1:2, 7:10)]
absence_c1 <- absence_c1[!duplicated(absence_c1)]

seq_mat_c1$abs_y1 <- ifelse(seq_mat_c1$pupilmatchingrefanonymous %in% absence_c1[academicyear == "2005/2006"]$pupilmatchingrefanonymous, T, F)
seq_mat_c1$abs_y2 <- ifelse(seq_mat_c1$pupilmatchingrefanonymous %in% absence_c1[academicyear == "2006/2007"]$pupilmatchingrefanonymous, T, F)
seq_mat_c1$abs_y3 <- ifelse(seq_mat_c1$pupilmatchingrefanonymous %in% absence_c1[academicyear == "2007/2008"]$pupilmatchingrefanonymous, T, F)
seq_mat_c1$abs_y4 <- ifelse(seq_mat_c1$pupilmatchingrefanonymous %in% absence_c1[academicyear == "2008/2009"]$pupilmatchingrefanonymous, T, F)
seq_mat_c1$abs_y5 <- ifelse(seq_mat_c1$pupilmatchingrefanonymous %in% absence_c1[academicyear == "2009/2010"]$pupilmatchingrefanonymous, T, F)
seq_mat_c1$abs_y6 <- ifelse(seq_mat_c1$pupilmatchingrefanonymous %in% absence_c1[academicyear == "2010/2011"]$pupilmatchingrefanonymous, T, F)
seq_mat_c1$abs_y7 <- ifelse(seq_mat_c1$pupilmatchingrefanonymous %in% absence_c1[academicyear == "2011/2012"]$pupilmatchingrefanonymous, T, F)
seq_mat_c1$abs_y8 <- ifelse(seq_mat_c1$pupilmatchingrefanonymous %in% absence_c1[academicyear == "2012/2013"]$pupilmatchingrefanonymous, T, F)
seq_mat_c1$abs_y9 <- ifelse(seq_mat_c1$pupilmatchingrefanonymous %in% absence_c1[academicyear == "2013/2014"]$pupilmatchingrefanonymous, T, F)
seq_mat_c1$abs_y10 <- ifelse(seq_mat_c1$pupilmatchingrefanonymous %in% absence_c1[academicyear == "2014/2015"]$pupilmatchingrefanonymous, T, F)
seq_mat_c1$abs_y11 <- ifelse(seq_mat_c1$pupilmatchingrefanonymous %in% absence_c1[academicyear == "2015/2016"]$pupilmatchingrefanonymous, T, F)

# View(seq_mat_c1[cov_pats_id == 17]) # the secondary school disappearences
table(seq_mat_c1[cov_pats_id == 17]$abs_y1)
table(seq_mat_c1[cov_pats_id == 17]$abs_y2)
table(seq_mat_c1[cov_pats_id == 17]$abs_y3)
table(seq_mat_c1[cov_pats_id == 17]$abs_y4)
table(seq_mat_c1[cov_pats_id == 17]$abs_y5)
table(seq_mat_c1[cov_pats_id == 17]$abs_y6)
table(seq_mat_c1[cov_pats_id == 17]$abs_y7)
table(seq_mat_c1[cov_pats_id == 17]$abs_y8)
table(seq_mat_c1[cov_pats_id == 17]$abs_y9)
table(seq_mat_c1[cov_pats_id == 17]$abs_y10)
table(seq_mat_c1[cov_pats_id == 17]$abs_y11)

View(seq_mat_c1[cov_pats_id == 3]) # enrolled in every year
table(seq_mat_c1[cov_pats_id == 3]$abs_y1)
table(seq_mat_c1[cov_pats_id == 3]$abs_y2)
table(seq_mat_c1[cov_pats_id == 3]$abs_y3)
table(seq_mat_c1[cov_pats_id == 3]$abs_y4)
table(seq_mat_c1[cov_pats_id == 3]$abs_y5)
table(seq_mat_c1[cov_pats_id == 3]$abs_y6)
table(seq_mat_c1[cov_pats_id == 3]$abs_y7)
table(seq_mat_c1[cov_pats_id == 3]$abs_y8)
table(seq_mat_c1[cov_pats_id == 3]$abs_y9)
table(seq_mat_c1[cov_pats_id == 3]$abs_y10)
table(seq_mat_c1[cov_pats_id == 3]$abs_y11)

# * cohort 2 --------------------------------------------------------------

abs_all_c2 <- abs_all_c2[, c("pupilmatchingrefanonymous",
                             "academicyear",
                             names(abs_all_c2)[c(25:44, 70:80)]),
                         with = F] # removes census and flag vars
abs_all_c2 <- abs_all_c2[!duplicated(abs_all_c2)]
abs_all_c2 <- abs_all_c2[order(pupilmatchingrefanonymous, academicyear)]

abs_all_c2$sessionspossible_annual <- abs_all_c2$sessionspossible_autumn + abs_all_c2$sessionspossible_spring + abs_all_c2$sessionspossible_summer
abs_all_c2$authorisedabsence_annual <- abs_all_c2$authorisedabsence_autumn + abs_all_c2$authorisedabsence_spring + abs_all_c2$authorisedabsence_summer
abs_all_c2$unauthorisedabsence_annual <- abs_all_c2$unauthorisedabsence_autumn + abs_all_c2$unauthorisedabsence_spring + abs_all_c2$unauthorisedabsence_summer
abs_all_c2$overallabsence_annual <- abs_all_c2$overallabsence_autumn + abs_all_c2$overallabsence_spring + abs_all_c2$overallabsence_summer

absence_c2 <- abs_all_c2[, c("pupilmatchingrefanonymous", "academicyear",
                             "sessionspossible_annual", "authorisedabsence_annual",
                             "unauthorisedabsence_annual", "overallabsence_annual")]
rm(abs_all_c2)

gc()

absence_c2[, sessionspossible_annual_2 := sum(sessionspossible_annual), by = .(pupilmatchingrefanonymous, academicyear)]
absence_c2[, authorisedabsence_annual_2 := sum(authorisedabsence_annual), by = .(pupilmatchingrefanonymous, academicyear)]
absence_c2[, unauthorisedabsence_annual_2 := sum(unauthorisedabsence_annual), by = .(pupilmatchingrefanonymous, academicyear)]
absence_c2[, overallabsence_annual_2 := sum(overallabsence_annual), by = .(pupilmatchingrefanonymous, academicyear)]

absence_c2 <- absence_c2[, c(1:2, 7:10)]
absence_c2 <- absence_c2[!duplicated(absence_c2)]

seq_mat_c2$abs_y0 <- ifelse(seq_mat_c2$pupilmatchingrefanonymous %in% absence_c2[academicyear == "2005/2006"]$pupilmatchingrefanonymous, T, F)
seq_mat_c2$abs_y1 <- ifelse(seq_mat_c2$pupilmatchingrefanonymous %in% absence_c2[academicyear == "2006/2007"]$pupilmatchingrefanonymous, T, F)
seq_mat_c2$abs_y2 <- ifelse(seq_mat_c2$pupilmatchingrefanonymous %in% absence_c2[academicyear == "2007/2008"]$pupilmatchingrefanonymous, T, F)
seq_mat_c2$abs_y3 <- ifelse(seq_mat_c2$pupilmatchingrefanonymous %in% absence_c2[academicyear == "2008/2009"]$pupilmatchingrefanonymous, T, F)
seq_mat_c2$abs_y4 <- ifelse(seq_mat_c2$pupilmatchingrefanonymous %in% absence_c2[academicyear == "2009/2010"]$pupilmatchingrefanonymous, T, F)
seq_mat_c2$abs_y5 <- ifelse(seq_mat_c2$pupilmatchingrefanonymous %in% absence_c2[academicyear == "2010/2011"]$pupilmatchingrefanonymous, T, F)
seq_mat_c2$abs_y6 <- ifelse(seq_mat_c2$pupilmatchingrefanonymous %in% absence_c2[academicyear == "2011/2012"]$pupilmatchingrefanonymous, T, F)
seq_mat_c2$abs_y7 <- ifelse(seq_mat_c2$pupilmatchingrefanonymous %in% absence_c2[academicyear == "2012/2013"]$pupilmatchingrefanonymous, T, F)
seq_mat_c2$abs_y8 <- ifelse(seq_mat_c2$pupilmatchingrefanonymous %in% absence_c2[academicyear == "2013/2014"]$pupilmatchingrefanonymous, T, F)
seq_mat_c2$abs_y9 <- ifelse(seq_mat_c2$pupilmatchingrefanonymous %in% absence_c2[academicyear == "2014/2015"]$pupilmatchingrefanonymous, T, F)
seq_mat_c2$abs_y10 <- ifelse(seq_mat_c2$pupilmatchingrefanonymous %in% absence_c2[academicyear == "2015/2016"]$pupilmatchingrefanonymous, T, F)
seq_mat_c2$abs_y11 <- ifelse(seq_mat_c2$pupilmatchingrefanonymous %in% absence_c2[academicyear == "2016/2017"]$pupilmatchingrefanonymous, T, F)

# View(seq_mat_c2[cov_pats_id == 11]) # the secondary school disappearences
table(seq_mat_c2[cov_pats_id == 11]$abs_y0)
table(seq_mat_c2[cov_pats_id == 11]$abs_y1)
table(seq_mat_c2[cov_pats_id == 11]$abs_y2)
table(seq_mat_c2[cov_pats_id == 11]$abs_y3)
table(seq_mat_c2[cov_pats_id == 11]$abs_y4)
table(seq_mat_c2[cov_pats_id == 11]$abs_y5)
table(seq_mat_c2[cov_pats_id == 11]$abs_y6)
table(seq_mat_c2[cov_pats_id == 11]$abs_y7)
table(seq_mat_c2[cov_pats_id == 11]$abs_y8)
table(seq_mat_c2[cov_pats_id == 11]$abs_y9)
table(seq_mat_c2[cov_pats_id == 11]$abs_y10)
table(seq_mat_c2[cov_pats_id == 11]$abs_y11)

# View(seq_mat_c2[cov_pats_id == 3]) # enrolled in every year
table(seq_mat_c2[cov_pats_id == 3]$abs_y0)
table(seq_mat_c2[cov_pats_id == 3]$abs_y1)
table(seq_mat_c2[cov_pats_id == 3]$abs_y2)
table(seq_mat_c2[cov_pats_id == 3]$abs_y3)
table(seq_mat_c2[cov_pats_id == 3]$abs_y4)
table(seq_mat_c2[cov_pats_id == 3]$abs_y5)
table(seq_mat_c2[cov_pats_id == 3]$abs_y6)
table(seq_mat_c2[cov_pats_id == 3]$abs_y7)
table(seq_mat_c2[cov_pats_id == 3]$abs_y8)
table(seq_mat_c2[cov_pats_id == 3]$abs_y9)
table(seq_mat_c2[cov_pats_id == 3]$abs_y10)
table(seq_mat_c2[cov_pats_id == 3]$abs_y11)

# exclusions --------------------------------------------------------------

sec_years_c1 <- c("2011/2012",
                  "2012/2013",
                  "2013/2014",
                  "2014/2015",
                  "2015/2016")

sec_years_c2 <- c("2012/2013",
                  "2013/2014",
                  "2014/2015",
                  "2015/2016",
                  "2016/2017")

f <- list.files("P:/Working/WORKING DATA/EXCLUSIONS")
excl_list <- list()
for (i in 1:length(f)) {
  print(paste0("trying ", i, " of ", (length(f))))
  excl_list[[i]] <- data.table(read.table(
    paste0("P:/Working/WORKING DATA/EXCLUSIONS/", f[i]),
    header = T,
    sep = "\t",
    stringsAsFactors = F,
    skipNul = T,
    comment.char = ""
  ))
  
  names(excl_list[[i]]) <- tolower(gsub(paste0("_ex", formatC(i + 11,
                                                              format = "d",
                                                              flag = "0",
                                                              width = 2)),
                                        "",
                                        names(excl_list[[i]])))
  
  print(paste0("done ", i))
}

rm(f, i)

excl_all <- rbind(excl_list[[1]],
                  excl_list[[2]],
                  excl_list[[3]],
                  excl_list[[4]],
                  excl_list[[5]],
                  excl_list[[6]],
                  fill = T)

table(fe = is.na(excl_all$totalfixedexclusions), fs = is.na(excl_all$totalfixedsessions), pe = is.na(excl_all$permanentexclusioncount))
table(fe = is.na(excl_all$totalfixedexclusions),
      excl_all$academicyear)

table(cat = is.na(excl_all$category),
      excl_all$academicyear) # need to work out for the two earlier years

excl_all <- excl_all[, c("pupilmatchingrefanonymous", "academicyear",
                         "category", "startdate", "sessions",
                         "totalfixedexclusions", "totalfixedsessions", "permanentexclusioncount")]

table(excl_all$academicyear)
aggregate(pupilmatchingrefanonymous ~ academicyear, data = excl_all, function(x) length(unique(x))) # unique within later years - can count for the earlier

excl_all[academicyear %in% c("2011/2012", "2012/2013"),
         totalfixedexclusions := length(category[category %in% c("FIXD", "LNCH")]), by = .(pupilmatchingrefanonymous, academicyear)]

excl_all[academicyear %in% c("2011/2012", "2012/2013"),
         totalfixedsessions := sum(sessions, na.rm = T), by = .(pupilmatchingrefanonymous, academicyear)]

excl_all[academicyear %in% c("2011/2012", "2012/2013"),
         permanentexclusioncount := length(category[category == "PERM"]), by = .(pupilmatchingrefanonymous, academicyear)]

table(is.na(excl_all$totalfixedexclusions),
      excl_all$academicyear)
table(is.na(excl_all$totalfixedsessions),
      excl_all$academicyear)
table(is.na(excl_all$permanentexclusioncount),
      excl_all$academicyear)

excl_all <- excl_all[, c("pupilmatchingrefanonymous", "academicyear",
                         "totalfixedexclusions", "totalfixedsessions", "permanentexclusioncount")]

excl_all <- excl_all[!duplicated(excl_all)]

table(excl_all$academicyear)
aggregate(pupilmatchingrefanonymous ~ academicyear, data = excl_all, function(x) length(unique(x))) # unique within later years - can count for the earlier

excl_all_c1 <- excl_all[pupilmatchingrefanonymous %in% cohort1$pupilmatchingrefanonymous & academicyear %in% sec_years_c1]
excl_all_c2 <- excl_all[pupilmatchingrefanonymous %in% cohort2$pupilmatchingrefanonymous & academicyear %in% sec_years_c2]

save(excl_all_c1, file = "PROCESSED DATA/cap4_appendix1/excl_c1.rda")
save(excl_all_c2, file = "PROCESSED DATA/cap4_appendix1/excl_c2.rda")

cohort1 <- merge(cohort1,
                 excl_all_c1,
                 by = c("pupilmatchingrefanonymous", "academicyear"),
                 all.x = T)

cohort2 <- merge(cohort2,
                 excl_all_c2,
                 by = c("pupilmatchingrefanonymous", "academicyear"),
                 all.x = T)

cohort1[permanentexclusioncount == 0]$permanentexclusioncount <- NA
cohort1[totalfixedexclusions == 0]$totalfixedexclusions <- NA
cohort1[totalfixedexclusions == 0]$totalfixedsessions <- NA

cohort2[permanentexclusioncount == 0]$permanentexclusioncount <- NA
cohort2[totalfixedexclusions == 0]$totalfixedexclusions <- NA
cohort2[totalfixedexclusions == 0]$totalfixedsessions <- NA

# * cohort 1 --------------------------------------------------------------

seq_mat_c1$excl_y7 <- seq_mat_c1$pupilmatchingrefanonymous %in% cohort1[year == 7 & (!is.na(permanentexclusioncount) | !is.na(totalfixedexclusions))]$pupilmatchingrefanonymous
seq_mat_c1$excl_y8 <- seq_mat_c1$pupilmatchingrefanonymous %in% cohort1[year == 8 & (!is.na(permanentexclusioncount) | !is.na(totalfixedexclusions))]$pupilmatchingrefanonymous
seq_mat_c1$excl_y9 <- seq_mat_c1$pupilmatchingrefanonymous %in% cohort1[year == 9 & (!is.na(permanentexclusioncount) | !is.na(totalfixedexclusions))]$pupilmatchingrefanonymous
seq_mat_c1$excl_y10 <- seq_mat_c1$pupilmatchingrefanonymous %in% cohort1[year == 10 & (!is.na(permanentexclusioncount) | !is.na(totalfixedexclusions))]$pupilmatchingrefanonymous
seq_mat_c1$excl_y11 <- seq_mat_c1$pupilmatchingrefanonymous %in% cohort1[year == 11 & (!is.na(permanentexclusioncount) | !is.na(totalfixedexclusions))]$pupilmatchingrefanonymous

# View(seq_mat_c1[cov_pats_id == 17]) # the secondary school disappearences
table(seq_mat_c1[cov_pats_id == 17]$excl_y7)
table(seq_mat_c1[cov_pats_id == 17]$excl_y8)
table(seq_mat_c1[cov_pats_id == 17]$excl_y9)
table(seq_mat_c1[cov_pats_id == 17]$excl_y10)
table(seq_mat_c1[cov_pats_id == 17]$excl_y11)

View(seq_mat_c1[cov_pats_id == 3]) # enrolled in every year
table(seq_mat_c1[cov_pats_id == 3]$excl_y7)
table(seq_mat_c1[cov_pats_id == 3]$excl_y8)
table(seq_mat_c1[cov_pats_id == 3]$excl_y9)
table(seq_mat_c1[cov_pats_id == 3]$excl_y10)
table(seq_mat_c1[cov_pats_id == 3]$excl_y11)

# * cohort 2 --------------------------------------------------------------

seq_mat_c2$excl_y7 <- seq_mat_c2$pupilmatchingrefanonymous %in% cohort2[year == 7 & (!is.na(permanentexclusioncount) | !is.na(totalfixedexclusions))]$pupilmatchingrefanonymous
seq_mat_c2$excl_y8 <- seq_mat_c2$pupilmatchingrefanonymous %in% cohort2[year == 8 & (!is.na(permanentexclusioncount) | !is.na(totalfixedexclusions))]$pupilmatchingrefanonymous
seq_mat_c2$excl_y9 <- seq_mat_c2$pupilmatchingrefanonymous %in% cohort2[year == 9 & (!is.na(permanentexclusioncount) | !is.na(totalfixedexclusions))]$pupilmatchingrefanonymous
seq_mat_c2$excl_y10 <- seq_mat_c2$pupilmatchingrefanonymous %in% cohort2[year == 10 & (!is.na(permanentexclusioncount) | !is.na(totalfixedexclusions))]$pupilmatchingrefanonymous
seq_mat_c2$excl_y11 <- seq_mat_c2$pupilmatchingrefanonymous %in% cohort2[year == 11 & (!is.na(permanentexclusioncount) | !is.na(totalfixedexclusions))]$pupilmatchingrefanonymous

# View(seq_mat_c2[cov_pats_id == 17]) # the secondary school disappearences
table(seq_mat_c2[cov_pats_id == 11]$excl_y7)
table(seq_mat_c2[cov_pats_id == 11]$excl_y8)
table(seq_mat_c2[cov_pats_id == 11]$excl_y9)
table(seq_mat_c2[cov_pats_id == 11]$excl_y10)
table(seq_mat_c2[cov_pats_id == 11]$excl_y11)

View(seq_mat_c2[cov_pats_id == 3]) # enrolled in every year
table(seq_mat_c2[cov_pats_id == 3]$excl_y7)
table(seq_mat_c2[cov_pats_id == 3]$excl_y8)
table(seq_mat_c2[cov_pats_id == 3]$excl_y9)
table(seq_mat_c2[cov_pats_id == 3]$excl_y10)
table(seq_mat_c2[cov_pats_id == 3]$excl_y11)
