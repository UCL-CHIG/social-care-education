
# LOAD --------------------------------------------------------------------

setwd("P:/Working/Matt")

lib_base = "P:/Working/Matt/r-library/"
assign(".lib.loc", c(.libPaths(), lib_base), envir = environment(.libPaths))
rm(lib_base)

# install.packages("R:/Software Add-Ons/R-library/NEW 3.6 R-Library/tidyselect_1.1.0.zip",
#                  repos = NULL,
#                  lib = "r-library",
#                  type = "win.binary")

library(data.table)

load("PROCESSED DATA/cohort1_npd_clean_sch_ad_qual.rda")
load("PROCESSED DATA/cohort2_npd_clean_sch_ad_qual.rda")
load("PROCESSED DATA/cin.rda")
load("PROCESSED DATA/cla_episodes_ad.rda")
load("PROCESSED DATA/stb.rda")

actual_cin <- cin[referralnfa == 0 & (reasonforclosure != "RC8" | is.na(reasonforclosure))]
cla_episodes <- cla_episodes[is.na(dateepisodeceased) | dateepisodeceased >= as.Date("2005-09-01")]

# EXPOSURE OBSERVED VARIABLES ---------------------------------------------

prim_years_c1 <- c("2008/2009",
                   "2009/2010",
                   "2010/2011")

sec_years_c1 <- c("2011/2012",
                  "2012/2013",
                  "2013/2014",
                  "2014/2015",
                  "2015/2016")

prim_dates_c1 <- c(
  as.Date("2008-09-01"),
  as.Date("2009-09-01"),
  as.Date("2010-09-01"),
  as.Date("2011-09-01")
)

sec_dates_c1 <- c(
  as.Date("2011-09-01"),
  as.Date("2012-09-01"),
  as.Date("2013-09-01"),
  as.Date("2014-09-01"),
  as.Date("2015-09-01"),
  as.Date("2016-09-01")
)

prim_years_c2 <- c("2009/2010",
                   "2010/2011",
                   "2011/2012")

sec_years_c2 <- c("2012/2013",
                  "2013/2014",
                  "2014/2015",
                  "2015/2016",
                  "2016/2017")

prim_dates_c2 <- c(
  as.Date("2009-09-01"),
  as.Date("2010-09-01"),
  as.Date("2011-09-01"),
  as.Date("2012-09-01")
)

sec_dates_c2 <- c(
  as.Date("2012-09-01"),
  as.Date("2013-09-01"),
  as.Date("2014-09-01"),
  as.Date("2015-09-01"),
  as.Date("2016-09-01"),
  as.Date("2017-09-01")
)

# cin
cin_primary_c1 <- actual_cin[cohort == 1 & (acadyr %in% prim_years_c1 |
                                              (referraldate >= prim_dates_c1[1] & referraldate < prim_dates_c1[4]))]
cin_primary_c2 <- actual_cin[cohort == 2 & (acadyr %in% prim_years_c2 |
                                              (referraldate >= prim_dates_c2[1] & referraldate < prim_dates_c2[4]))]

cin_secondary_c1 <- actual_cin[cohort == 1 & (acadyr %in% sec_years_c1 |
                                                (referraldate >= sec_dates_c1[1] & referraldate < sec_dates_c1[6]))]
cin_secondary_c2 <- actual_cin[cohort == 2 & (acadyr %in% sec_years_c2 |
                                                (referraldate >= sec_dates_c2[1] & referraldate < sec_dates_c2[6]))]

# cpp
cin_cpp_c1 <- cin[!is.na(cppstartdate)]
cin_cpp_c2 <- cin[!is.na(cppstartdate)]
cpp_primary_c1 <- cin_cpp_c1[cohort == 1 & (acadyr %in% prim_years_c1 |
                                              (cppstartdate >= prim_dates_c1[1] & cppstartdate < prim_dates_c1[4]))]
cpp_primary_c2 <- cin_cpp_c2[cohort == 2 & (acadyr %in% prim_years_c2 |
                                              (cppstartdate >= prim_dates_c2[1] & cppstartdate < prim_dates_c2[4]))]
cpp_secondary_c1 <- cin_cpp_c1[cohort == 1 & (acadyr %in% sec_years_c1 |
                                                (cppstartdate >= sec_dates_c1[1] & cppstartdate < sec_dates_c1[5]))]
cpp_secondary_c2 <- cin_cpp_c2[cohort == 2 & (acadyr %in% sec_years_c2 |
                                                (cppstartdate >= sec_dates_c2[1] & cppstartdate < sec_dates_c2[5]))]

# stb
stb_c1 <- stb[dateepisodestarted1 >= as.Date("2008-09-01")]
stb_c2 <- stb[dateepisodestarted1 >= as.Date("2009-09-01")]
stb_primary_c1 <- stb_c1[cohort == 1 & dateepisodestarted1 < as.Date("2011-09-01")]
stb_primary_c2 <- stb_c2[cohort == 2 & dateepisodestarted1 < as.Date("2012-09-01")]
stb_secondary_c1 <- stb_c1[cohort == 1 & dateepisodestarted1 >= as.Date("2011-09-01")]
stb_secondary_c2 <- stb_c2[cohort == 2 & dateepisodestarted1 >= as.Date("2012-09-01")]

# cla
cla_primary_c1 <- cla_episodes[cohort == 1 & dateepisodestarted < as.Date("2011-09-01")]
cla_primary_c2 <- cla_episodes[cohort == 2 & dateepisodestarted < as.Date("2012-09-01")]
cla_secondary_c1 <- cla_episodes[cohort == 1 & dateepisodestarted >= as.Date("2011-09-01")]
cla_secondary_c2 <- cla_episodes[cohort == 2 & dateepisodestarted >= as.Date("2012-09-01")]

rm(cin_cpp_c1, cin_cpp_c2, stb_c1, stb_c2)

# * most basic ------------------------------------------------------------

# ** CLA all primary school -----------------------------------------------

# ever actual cin
# cohort1
cohort1$ever_cin <- cohort1$pupilmatchingrefanonymous %in% cin_primary_c1$pupilmatchingrefanonymous |
  cohort1$pupilmatchingrefanonymous %in% cin_secondary_c1$pupilmatchingrefanonymous
cohort1$ever_cin_primary <- cohort1$pupilmatchingrefanonymous %in% cin_primary_c1$pupilmatchingrefanonymous
cohort1$ever_cin_secondary <- cohort1$pupilmatchingrefanonymous %in% cin_secondary_c1$pupilmatchingrefanonymous

# cohort2
cohort2$ever_cin <- cohort2$pupilmatchingrefanonymous %in% cin_primary_c2$pupilmatchingrefanonymous | 
  cohort2$pupilmatchingrefanonymous %in% cin_secondary_c2$pupilmatchingrefanonymous
cohort2$ever_cin_primary <- cohort2$pupilmatchingrefanonymous %in% cin_primary_c2$pupilmatchingrefanonymous
cohort2$ever_cin_secondary <- cohort2$pupilmatchingrefanonymous %in% cin_secondary_c2$pupilmatchingrefanonymous

# ever stb
# cohort1
cohort1$ever_stb <- cohort1$pupilmatchingrefanonymous %in% stb_primary_c1$pmr2 | cohort1$pupilmatchingrefanonymous %in% stb_secondary_c1$pmr2
cohort1$ever_stb_primary <- cohort1$pupilmatchingrefanonymous %in% stb_primary_c1$pmr2
cohort1$ever_stb_secondary <- cohort1$pupilmatchingrefanonymous %in% stb_secondary_c1$pmr2

# cohort2
cohort2$ever_stb <- cohort2$pupilmatchingrefanonymous %in% stb_primary_c2$pmr2 | cohort2$pupilmatchingrefanonymous %in% stb_secondary_c2$pmr2
cohort2$ever_stb_primary <- cohort2$pupilmatchingrefanonymous %in% stb_primary_c2$pmr2
cohort2$ever_stb_secondary <- cohort2$pupilmatchingrefanonymous %in% stb_secondary_c2$pmr2

# ever cin or stb
# cohort1
cohort1$ever_cin_stb <- cohort1$ever_cin == 1 | cohort1$ever_stb == 1
cohort1$ever_cin_stb_primary <- cohort1$ever_cin_primary == 1 | cohort1$ever_stb_primary == 1
cohort1$ever_cin_stb_secondary <- cohort1$ever_cin_secondary == 1 | cohort1$ever_stb_secondary == 1

# cohort2
cohort2$ever_cin_stb <- cohort2$ever_cin == 1 | cohort2$ever_stb == 1
cohort2$ever_cin_stb_primary <- cohort2$ever_cin_primary == 1 | cohort2$ever_stb_primary == 1
cohort2$ever_cin_stb_secondary <- cohort2$ever_cin_secondary == 1 | cohort2$ever_stb_secondary == 1

# ever cpp
# cohort1
cohort1$ever_cpp <- cohort1$pupilmatchingrefanonymous %in% cpp_primary_c1$pupilmatchingrefanonymous |
  cohort1$pupilmatchingrefanonymous %in% cpp_secondary_c1$pupilmatchingrefanonymous
cohort1$ever_cpp_primary <- cohort1$pupilmatchingrefanonymous %in% cpp_primary_c1$pupilmatchingrefanonymous
cohort1$ever_cpp_secondary <- cohort1$pupilmatchingrefanonymous %in% cpp_secondary_c1$pupilmatchingrefanonymous

# cohort2
cohort2$ever_cpp <- cohort2$pupilmatchingrefanonymous %in% cpp_primary_c2$pupilmatchingrefanonymous |
  cohort2$pupilmatchingrefanonymous %in% cpp_secondary_c2$pupilmatchingrefanonymous
cohort2$ever_cpp_primary <- cohort2$pupilmatchingrefanonymous %in% cpp_primary_c2$pupilmatchingrefanonymous
cohort2$ever_cpp_secondary <- cohort2$pupilmatchingrefanonymous %in% cpp_secondary_c2$pupilmatchingrefanonymous

# ever care
# cohort1
cohort1$ever_care <- cohort1$pupilmatchingrefanonymous %in% cla_primary_c1$pmr2 | cohort1$pupilmatchingrefanonymous %in% cla_secondary_c1$pmr2
cohort1$ever_care_primary <- cohort1$pupilmatchingrefanonymous %in% cla_primary_c1$pmr2
cohort1$ever_care_secondary <- cohort1$pupilmatchingrefanonymous %in% cla_secondary_c1$pmr2

# cohort2
cohort2$ever_care <- cohort2$pupilmatchingrefanonymous %in% cla_primary_c2$pmr2 | cohort2$pupilmatchingrefanonymous %in% cla_secondary_c2$pmr2
cohort2$ever_care_primary <- cohort2$pupilmatchingrefanonymous %in% cla_primary_c2$pmr2
cohort2$ever_care_secondary <- cohort2$pupilmatchingrefanonymous %in% cla_secondary_c2$pmr2

# main var
cohort1$exposure_highest_5_grp <- "None"
cohort1[ever_care_primary == 0 & ever_cpp_primary == 0 & ever_stb_primary == 0 & ever_cin_primary == 1]$exposure_highest_5_grp <- "CiN"
cohort1[ever_care_primary == 0 & ever_cpp_primary == 0 & ever_stb_primary == 1]$exposure_highest_5_grp <- "STB"
cohort1[ever_care_primary == 0 & ever_cpp_primary == 1]$exposure_highest_5_grp <- "CPP"
cohort1[ever_care_primary == 1]$exposure_highest_5_grp <- "Care"

cohort2$exposure_highest_5_grp <- "None"
cohort2[ever_care_primary == 0 & ever_cpp_primary == 0 & ever_stb_primary == 0 & ever_cin_primary == 1]$exposure_highest_5_grp <- "CiN"
cohort2[ever_care_primary == 0 & ever_cpp_primary == 0 & ever_stb_primary == 1]$exposure_highest_5_grp <- "STB"
cohort2[ever_care_primary == 0 & ever_cpp_primary == 1]$exposure_highest_5_grp <- "CPP"
cohort2[ever_care_primary == 1]$exposure_highest_5_grp <- "Care"

table(cohort1[year == 7 & sch_n_in_year == 1]$exposure_highest_5_grp, useNA = "always")
table(cohort2[year == 7 & sch_n_in_year == 1]$exposure_highest_5_grp, useNA = "always")

cohort1$exposure_highest_4_grp <- "None"
cohort1[ever_care_primary == 0 & ever_cpp_primary == 0 & (ever_stb_primary == 1 | ever_cin_primary == 1)]$exposure_highest_4_grp <- "CiN/STB"
cohort1[ever_care_primary == 0 & ever_cpp_primary == 1]$exposure_highest_4_grp <- "CPP"
cohort1[ever_care_primary == 1]$exposure_highest_4_grp <- "Care"

cohort2$exposure_highest_4_grp <- "None"
cohort2[ever_care_primary == 0 & ever_cpp_primary == 0 & (ever_stb_primary == 1 | ever_cin_primary == 1)]$exposure_highest_4_grp <- "CiN/STB"
cohort2[ever_care_primary == 0 & ever_cpp_primary == 1]$exposure_highest_4_grp <- "CPP"
cohort2[ever_care_primary == 1]$exposure_highest_4_grp <- "Care"

table(cohort1[year == 7 & sch_n_in_year == 1]$exposure_highest_4_grp, useNA = "always")
table(cohort2[year == 7 & sch_n_in_year == 1]$exposure_highest_4_grp, useNA = "always")

# * count cin & stb -------------------------------------------------------

table(cohort1[year == 7 & sch_n_in_year == 1]$ever_cin_primary)
table(cohort1[year == 7 & sch_n_in_year == 1]$ever_cin_secondary)
table(cohort1[year == 7 & sch_n_in_year == 1]$ever_cin)

table(cohort1[year == 7 & sch_n_in_year == 1]$ever_stb_primary)
table(cohort1[year == 7 & sch_n_in_year == 1]$ever_stb_secondary)
table(cohort1[year == 7 & sch_n_in_year == 1]$ever_stb)

table(cohort1[year == 7 & sch_n_in_year == 1]$ever_cin_stb_primary)
table(cohort1[year == 7 & sch_n_in_year == 1]$ever_cin_stb_secondary)
table(cohort1[year == 7 & sch_n_in_year == 1]$ever_cin_stb)

table(cohort2[year == 7 & sch_n_in_year == 1]$ever_cin_primary)
table(cohort2[year == 7 & sch_n_in_year == 1]$ever_cin_secondary)
table(cohort2[year == 7 & sch_n_in_year == 1]$ever_cin)

table(cohort2[year == 7 & sch_n_in_year == 1]$ever_stb_primary)
table(cohort2[year == 7 & sch_n_in_year == 1]$ever_stb_secondary)
table(cohort2[year == 7 & sch_n_in_year == 1]$ever_stb)

table(cohort2[year == 7 & sch_n_in_year == 1]$ever_cin_stb_primary)
table(cohort2[year == 7 & sch_n_in_year == 1]$ever_cin_stb_secondary)
table(cohort2[year == 7 & sch_n_in_year == 1]$ever_cin_stb)

# ** all KS2 only ---------------------------------------------------------

cla_y46_c1 <- cla_primary_c1[dateepisodestarted >= as.Date("2008-09-01")]
cla_y46_c2 <- cla_primary_c2[dateepisodestarted >= as.Date("2009-09-01")]

cohort1$ever_care_y46 <- cohort1$pupilmatchingrefanonymous %in% cla_y46_c1$pmr2
cohort2$ever_care_y46 <- cohort2$pupilmatchingrefanonymous %in% cla_y46_c2$pmr2

cohort1$exposure_highest_5_grp_y46 <- "None"
cohort1[ever_care_y46 == 0 & ever_cpp_primary == 0 & ever_stb_primary == 0 & ever_cin_primary == 1]$exposure_highest_5_grp_y46 <- "CiN"
cohort1[ever_care_y46 == 0 & ever_cpp_primary == 0 & ever_stb_primary == 1]$exposure_highest_5_grp_y46 <- "STB"
cohort1[ever_care_y46 == 0 & ever_cpp_primary == 1]$exposure_highest_5_grp_y46 <- "CPP"
cohort1[ever_care_y46 == 1]$exposure_highest_5_grp_y46 <- "Care"

cohort2$exposure_highest_5_grp_y46 <- "None"
cohort2[ever_care_y46 == 0 & ever_cpp_primary == 0 & ever_stb_primary == 0 & ever_cin_primary == 1]$exposure_highest_5_grp_y46 <- "CiN"
cohort2[ever_care_y46 == 0 & ever_cpp_primary == 0 & ever_stb_primary == 1]$exposure_highest_5_grp_y46 <- "STB"
cohort2[ever_care_y46 == 0 & ever_cpp_primary == 1]$exposure_highest_5_grp_y46 <- "CPP"
cohort2[ever_care_y46 == 1]$exposure_highest_5_grp_y46 <- "Care"

table(cohort1[year == 7 & sch_n_in_year == 1]$exposure_highest_5_grp_y46, useNA = "always")
table(cohort2[year == 7 & sch_n_in_year == 1]$exposure_highest_5_grp_y46, useNA = "always")

cohort1$exposure_highest_4_grp_y46 <- "None"
cohort1[ever_care_y46 == 0 & ever_cpp_primary == 0 & (ever_stb_primary == 1 | ever_cin_primary == 1)]$exposure_highest_4_grp_y46 <- "CiN/STB"
cohort1[ever_care_y46 == 0 & ever_cpp_primary == 1]$exposure_highest_4_grp_y46 <- "CPP"
cohort1[ever_care_y46 == 1]$exposure_highest_4_grp_y46 <- "Care"

cohort2$exposure_highest_4_grp_y46 <- "None"
cohort2[ever_care_y46 == 0 & ever_cpp_primary == 0 & (ever_stb_primary == 1 | ever_cin_primary == 1)]$exposure_highest_4_grp_y46 <- "CiN/STB"
cohort2[ever_care_y46 == 0 & ever_cpp_primary == 1]$exposure_highest_4_grp_y46 <- "CPP"
cohort2[ever_care_y46 == 1]$exposure_highest_4_grp_y46 <- "Care"

table(cohort1[year == 7 & sch_n_in_year == 1]$exposure_highest_4_grp, useNA = "always")
table(cohort1[year == 7 & sch_n_in_year == 1]$exposure_highest_4_grp_y46, useNA = "always")

table(cohort2[year == 7 & sch_n_in_year == 1]$exposure_highest_4_grp, useNA = "always")
table(cohort2[year == 7 & sch_n_in_year == 1]$exposure_highest_4_grp_y46, useNA = "always")

rm(cla_y46_c1, cla_y46_c2)

# ** all y4 to 9 ----------------------------------------------------------

# get cla merged
cla_y49_c1 <- cla_primary_c1[dateepisodestarted >= as.Date("2008-09-01")]
cla_y49_c1_tmp <- cla_secondary_c1[dateepisodestarted < as.Date("2014-09-01")]
cla_y49_c1 <- rbind(cla_y49_c1, cla_y49_c1_tmp)
rm(cla_y49_c1_tmp)

cla_y49_c2 <- cla_primary_c2[dateepisodestarted >= as.Date("2009-09-01")]
cla_y49_c2_tmp <- cla_secondary_c2[dateepisodestarted < as.Date("2015-09-01")]
cla_y49_c2 <- rbind(cla_y49_c2, cla_y49_c2_tmp)
rm(cla_y49_c2_tmp)

# get cpp merged
cpp_y49_c1 <- cpp_primary_c1
cpp_y49_c1_tmp <- cpp_secondary_c1[acadyr %in% sec_years_c1[1:3] |
                                     (cppstartdate >= sec_dates_c1[1] & cppstartdate < sec_dates_c1[4])]
cpp_y49_c1 <- rbind(cpp_y49_c1, cpp_y49_c1_tmp)
rm(cpp_y49_c1_tmp)

cpp_y49_c2 <- cpp_primary_c2
cpp_y49_c2_tmp <- cpp_secondary_c2[acadyr %in% sec_years_c2[1:3] |
                                     (cppstartdate >= sec_dates_c2[1] & cppstartdate < sec_dates_c2[4])]
cpp_y49_c2 <- rbind(cpp_y49_c2, cpp_y49_c2_tmp)
rm(cpp_y49_c2_tmp)

# get stb merged
stb_y49_c1 <- stb_primary_c1
stb_y49_c1_tmp <- stb_secondary_c1[dateepisodestarted1 >= sec_dates_c1[1] & dateepisodestarted1 < sec_dates_c1[4]]
stb_y49_c1 <- rbind(stb_y49_c1, stb_y49_c1_tmp)
rm(stb_y49_c1_tmp)

stb_y49_c2 <- stb_primary_c2
stb_y49_c2_tmp <- stb_secondary_c2[dateepisodestarted1 >= sec_dates_c2[1] & dateepisodestarted1 < sec_dates_c2[4]]
stb_y49_c2 <- rbind(stb_y49_c2, stb_y49_c2_tmp)
rm(stb_y49_c2_tmp)

# get cin merged
cin_y49_c1 <- cin_primary_c1
cin_y49_c1_tmp <- cin_secondary_c1[acadyr %in% sec_years_c1[1:3] |
                                     (referraldate >= sec_dates_c1[1] & referraldate < sec_dates_c1[4])]
cin_y49_c1 <- rbind(cin_y49_c1, cin_y49_c1_tmp)
rm(cin_y49_c1_tmp)

cin_y49_c2 <- cin_primary_c2
cin_y49_c2_tmp <- cin_secondary_c2[acadyr %in% sec_years_c2[1:3] |
                                     (referraldate >= sec_dates_c2[1] & referraldate < sec_dates_c2[4])]
cin_y49_c2 <- rbind(cin_y49_c2, cin_y49_c2_tmp)
rm(cin_y49_c2_tmp)

cohort1$ever_cin_y49 <- cohort1$pupilmatchingrefanonymous %in% cin_y49_c1$pupilmatchingrefanonymous
cohort2$ever_cin_y49 <- cohort2$pupilmatchingrefanonymous %in% cin_y49_c2$pupilmatchingrefanonymous

cohort1$ever_stb_y49 <- cohort1$pupilmatchingrefanonymous %in% stb_y49_c1$pmr2
cohort2$ever_stb_y49 <- cohort2$pupilmatchingrefanonymous %in% stb_y49_c2$pmr2

cohort1$ever_cin_stb_y49 <- cohort1$ever_cin_y49 == 1 | cohort1$ever_stb_y49 == 1
cohort2$ever_cin_stb_y49 <- cohort2$ever_cin_y49 == 1 | cohort2$ever_stb_y49 == 1

cohort1$ever_cpp_y49 <- cohort1$pupilmatchingrefanonymous %in% cpp_y49_c1$pupilmatchingrefanonymous
cohort2$ever_cpp_y49 <- cohort2$pupilmatchingrefanonymous %in% cpp_y49_c2$pupilmatchingrefanonymous

cohort1$ever_care_y49 <- cohort1$pupilmatchingrefanonymous %in% cla_y49_c1$pmr2
cohort2$ever_care_y49 <- cohort2$pupilmatchingrefanonymous %in% cla_y49_c2$pmr2

# create var
cohort1$exposure_highest_5_grp_y49 <- "None"
cohort1[ever_care_y49 == 0 & ever_cpp_y49 == 0 & ever_stb_y49 == 0 & ever_cin_y49 == 1]$exposure_highest_5_grp_y49 <- "CiN"
cohort1[ever_care_y49 == 0 & ever_cpp_y49 == 0 & ever_stb_y49 == 1]$exposure_highest_5_grp_y49 <- "STB"
cohort1[ever_care_y49 == 0 & ever_cpp_y49 == 1]$exposure_highest_5_grp_y49 <- "CPP"
cohort1[ever_care_y49 == 1]$exposure_highest_5_grp_y49 <- "Care"

cohort2$exposure_highest_5_grp_y49 <- "None"
cohort2[ever_care_y49 == 0 & ever_cpp_y49 == 0 & ever_stb_y49 == 0 & ever_cin_y49 == 1]$exposure_highest_5_grp_y49 <- "CiN"
cohort2[ever_care_y49 == 0 & ever_cpp_y49 == 0 & ever_stb_y49 == 1]$exposure_highest_5_grp_y49 <- "STB"
cohort2[ever_care_y49 == 0 & ever_cpp_y49 == 1]$exposure_highest_5_grp_y49 <- "CPP"
cohort2[ever_care_y49 == 1]$exposure_highest_5_grp_y49 <- "Care"

table(cohort1[year == 7 & sch_n_in_year == 1]$exposure_highest_5_grp, useNA = "always")
table(cohort1[year == 7 & sch_n_in_year == 1]$exposure_highest_5_grp_y46, useNA = "always")
table(cohort1[year == 7 & sch_n_in_year == 1]$exposure_highest_5_grp_y49, useNA = "always")

table(cohort2[year == 7 & sch_n_in_year == 1]$exposure_highest_5_grp, useNA = "always")
table(cohort2[year == 7 & sch_n_in_year == 1]$exposure_highest_5_grp_y46, useNA = "always")
table(cohort2[year == 7 & sch_n_in_year == 1]$exposure_highest_5_grp_y49, useNA = "always")

cohort1$exposure_highest_4_grp_y49 <- "None"
cohort1[ever_care_y49 == 0 & ever_cpp_y49 == 0 & (ever_stb_y49 == 1 | ever_cin_y49 == 1)]$exposure_highest_4_grp_y49 <- "CiN/STB"
cohort1[ever_care_y49 == 0 & ever_cpp_y49 == 1]$exposure_highest_4_grp_y49 <- "CPP"
cohort1[ever_care_y49 == 1]$exposure_highest_4_grp_y49 <- "Care"

cohort2$exposure_highest_4_grp_y49 <- "None"
cohort2[ever_care_y49 == 0 & ever_cpp_y49 == 0 & (ever_stb_y49 == 1 | ever_cin_y49 == 1)]$exposure_highest_4_grp_y49 <- "CiN/STB"
cohort2[ever_care_y49 == 0 & ever_cpp_y49 == 1]$exposure_highest_4_grp_y49 <- "CPP"
cohort2[ever_care_y49 == 1]$exposure_highest_4_grp_y49 <- "Care"

table(cohort1[year == 7 & sch_n_in_year == 1]$exposure_highest_4_grp, useNA = "always")
table(cohort1[year == 7 & sch_n_in_year == 1]$exposure_highest_4_grp_y46, useNA = "always")
table(cohort1[year == 7 & sch_n_in_year == 1]$exposure_highest_4_grp_y49, useNA = "always")

table(cohort2[year == 7 & sch_n_in_year == 1]$exposure_highest_4_grp, useNA = "always")
table(cohort2[year == 7 & sch_n_in_year == 1]$exposure_highest_4_grp_y46, useNA = "always")
table(cohort2[year == 7 & sch_n_in_year == 1]$exposure_highest_4_grp_y49, useNA = "always")

rm(cin_secondary_c1, cin_secondary_c2, cla_secondary_c1, cla_secondary_c2,
   cpp_secondary_c1, cpp_secondary_c2, stb_secondary_c1, stb_secondary_c2,
   sec_dates_c1, sec_dates_c2, sec_years_c1, sec_years_c2,
   cla_y49_c1, cla_y49_c2, cin_y49_c1, cin_y49_c2, cpp_y49_c1, cpp_y49_c2,
   stb_y49_c1, stb_y49_c2)

# * according to analysis plan --------------------------------------------

# ** cohort1 --------------------------------------------------------------

# first get number of years CIN/STB (but not CPP or CLA)
ci_tab <- data.frame(
  pupilmatchingrefanonymous = cohort1[year == 7 & sch_n_in_year == 1]$pupilmatchingrefanonymous
)

ci_tab$y4 <- as.integer(NA)
ci_tab$y5 <- as.integer(NA)
ci_tab$y6 <- as.integer(NA)

for (i in 1:3) {
  ci_tab[, i + 1] <- ifelse(
    
    (ci_tab$pupilmatchingrefanonymous %in% cohort1[ever_cin_stb_primary == 1]$pupilmatchingrefanonymous)
    
    &
      
      (
        (ci_tab$pupilmatchingrefanonymous %in% # stb starts before year, runs into next
           stb_primary_c1[dateepisodestarted1 < prim_dates_c1[i] & (dateepisodeceased1 >= prim_dates_c1[i] | is.na(dateepisodeceased1))]$pmr2)
        
        |
          
          (ci_tab$pupilmatchingrefanonymous %in% # stb starts within year
             stb_primary_c1[dateepisodestarted1 >= prim_dates_c1[i] & dateepisodestarted1 < prim_dates_c1[i + 1]]$pmr2) 
        
        |
          
          (ci_tab$pupilmatchingrefanonymous %in% # cin active during year
             cin_primary_c1[acadyr == prim_years_c1[i] |
                              (referraldate >= prim_dates_c1[i] & referraldate < prim_dates_c1[i + 1])]$pupilmatchingrefanonymous)
      )
    
    &
      
      !(ci_tab$pupilmatchingrefanonymous %in% # not care or cpp
          cohort1[ever_care_primary == 1 | ever_cpp_primary == 1]$pupilmatchingrefanonymous),
    
    1, 0)
}

rm(i)

ci_tab <- data.table(ci_tab)

ci_tab[, sum_prim_sch_yrs := y4 + y5 + y6]

table(ci_tab$sum_prim_sch_yrs)
sum(table(ci_tab$sum_prim_sch_yrs)[2:4])
table(cohort1[year == 7 & sch_n_in_year == 1]$exposure_highest_4_grp)

# now get total duration of CLA primary school
cla_primary_c1$dur_prim <- as.integer(NA)
cla_primary_c1[dateepisodestarted < as.Date("2011-08-31") &
               (dateepisodeceased > as.Date("2011-08-31") | is.na(dateepisodeceased))]$dur_prim <-
  
  as.integer(difftime(as.Date("2011-08-31"),
                      cla_primary_c1[dateepisodestarted < as.Date("2011-08-31") &
                                     (dateepisodeceased > as.Date("2011-08-31") | is.na(dateepisodeceased))]$dateepisodestarted,
                      units = "days"))

cla_primary_c1[dateepisodestarted < as.Date("2011-08-31") & dateepisodeceased < as.Date("2011-08-31")]$dur_prim <-
  
  as.integer(difftime(cla_primary_c1[dateepisodestarted < as.Date("2011-08-31") &
                                     dateepisodeceased < as.Date("2011-08-31")]$dateepisodeceased,
                      cla_primary_c1[dateepisodestarted < as.Date("2011-08-31") &
                                     dateepisodeceased < as.Date("2011-08-31")]$dateepisodestarted,
                      units = "days"))

cla_primary_c1[, dur_prim := sum(dur_prim, na.rm = T), by = "pmr2"]

tmp <- data.table(
  pupilmatchingrefanonymous = cla_primary_c1$pmr2,
  dur_prim = cla_primary_c1$dur_prim
)

tmp <- tmp[!duplicated(tmp)]

cohort1 <- merge(cohort1,
                 tmp,
                 by = "pupilmatchingrefanonymous",
                 all.x = T)

rm(tmp)

table(cohort1[year == 7 & sch_n_in_year == 1]$exposure_highest_4_grp)
table(is.na(cohort1[year == 7 & sch_n_in_year == 1]$dur_prim))

# now get total number of placements primary school
cla_primary_c1 <- cla_primary_c1[order(pmr2, dateepisodestarted)]
cla_primary_c1$n_placements_prim <- as.integer(NA)

cla_primary_c1$count_episode_for_placement_prim <- T
flag <- cla_primary_c1$placement == shift(cla_primary_c1$placement, type = "lead") &
  cla_primary_c1$poc_start == shift(cla_primary_c1$poc_start, type = "lead") &
  cla_primary_c1$legal_status != shift(cla_primary_c1$legal_status, type = "lead") &
  cla_primary_c1$pmr2 == shift(cla_primary_c1$pmr2, type = "lead")

cla_primary_c1[flag == T]$count_episode_for_placement_prim <- F
rm(flag)

cla_primary_c1[, n_placements_prim :=  sum(count_episode_for_placement_prim), by = "pmr2"]

tmp <- data.table(
  pupilmatchingrefanonymous = cla_primary_c1$pmr2,
  n_placements_prim = cla_primary_c1$n_placements_prim
)

tmp <- tmp[!duplicated(tmp)]

cohort1 <- merge(cohort1,
                 tmp,
                 by = "pupilmatchingrefanonymous",
                 all.x = T)

rm(tmp)

cohort1[ever_care_primary == 0]$n_placements_prim <- NA

table(cohort1[year == 7 & sch_n_in_year == 1]$exposure_highest_4_grp)
table(is.na(cohort1[year == 7 & sch_n_in_year == 1]$n_placements_prim))
table(cohort1[year == 7 & sch_n_in_year == 1]$n_placements_prim)

# build exposure
cohort1$exposure_highest_8_grp <- "None"
cohort1[pupilmatchingrefanonymous %in% ci_tab[sum_prim_sch_yrs == 1]$pupilmatchingrefanonymous]$exposure_highest_8_grp <- "CIN 1 yr"
cohort1[pupilmatchingrefanonymous %in% ci_tab[sum_prim_sch_yrs == 2]$pupilmatchingrefanonymous]$exposure_highest_8_grp <- "CIN 2 yr"
cohort1[pupilmatchingrefanonymous %in% ci_tab[sum_prim_sch_yrs == 3]$pupilmatchingrefanonymous]$exposure_highest_8_grp <- "CIN 3 yr"
cohort1[ever_care_primary == 0 & ever_cpp_primary == 1]$exposure_highest_8_grp <- "CPP"
cohort1[ever_care_primary == 1 & dur_prim < 365 & n_placements_prim < 3]$exposure_highest_8_grp <- "CLA short stable"
cohort1[ever_care_primary == 1 & dur_prim < 365 & n_placements_prim >= 3]$exposure_highest_8_grp <- "CLA short unstable"
cohort1[ever_care_primary == 1 & dur_prim >= 365 & n_placements_prim < 3]$exposure_highest_8_grp <- "CLA long stable"
cohort1[ever_care_primary == 1 & dur_prim >= 365 & n_placements_prim >= 3]$exposure_highest_8_grp <- "CLA long unstable"

cohort1$exposure_highest_8_grp <- factor(cohort1$exposure_highest_8_grp, levels = c("None",
                                                          "CIN 1 yr",
                                                          "CIN 2 yr",
                                                          "CIN 3 yr",
                                                          "CPP",
                                                          "CLA short stable",
                                                          "CLA short unstable",
                                                          "CLA long stable",
                                                          "CLA long unstable"))

table(cohort1[year == 7 & sch_n_in_year == 1]$exposure_highest_4_grp, useNA = "always")
table(cohort1[year == 7 & sch_n_in_year == 1]$exposure_highest_8_grp, useNA = "always")

rm(ci_tab)

# ** cohort2 --------------------------------------------------------------

# first get number of years CIN/STB (but not CPP or CLA)
ci_tab <- data.frame(
  pupilmatchingrefanonymous = cohort2[year == 7 & sch_n_in_year == 1]$pupilmatchingrefanonymous
)

ci_tab$y4 <- as.integer(NA)
ci_tab$y5 <- as.integer(NA)
ci_tab$y6 <- as.integer(NA)

for (i in 1:3) {
  ci_tab[, i + 1] <- ifelse(
    
    (ci_tab$pupilmatchingrefanonymous %in% cohort2[ever_cin_stb_primary == 1]$pupilmatchingrefanonymous)
    
    &
      
      (
        (ci_tab$pupilmatchingrefanonymous %in% # stb starts before year, runs into next
           stb_primary_c2[dateepisodestarted1 < prim_dates_c2[i] & (dateepisodeceased1 >= prim_dates_c2[i] | is.na(dateepisodeceased1))]$pmr2)
        
        |
          
          (ci_tab$pupilmatchingrefanonymous %in% # stb starts within year
             stb_primary_c2[dateepisodestarted1 >= prim_dates_c2[i] & dateepisodestarted1 < prim_dates_c2[i + 1]]$pmr2) 
        
        |
          
          (ci_tab$pupilmatchingrefanonymous %in% # cin active during year
             cin_primary_c2[acadyr == prim_years_c2[i] |
                              (referraldate >= prim_dates_c2[i] & referraldate < prim_dates_c2[i + 1])]$pupilmatchingrefanonymous)
      )
    
    &
      
      !(ci_tab$pupilmatchingrefanonymous %in% # not cpp or care
          cohort2[ever_care_primary == 1 | ever_cpp_primary == 1]$pupilmatchingrefanonymous),
    
    1, 0)
}

rm(i)

ci_tab <- data.table(ci_tab)

ci_tab[, sum_prim_sch_yrs := y4 + y5 + y6]

table(ci_tab$sum_prim_sch_yrs)
sum(table(ci_tab$sum_prim_sch_yrs)[2:4])
table(cohort2[year == 7 & sch_n_in_year == 1]$exposure_highest_4_grp)

# now get total duration of CLA primary school
cla_primary_c2$dur_prim <- as.integer(NA)
cla_primary_c2[dateepisodestarted < as.Date("2012-08-31") &
                 (dateepisodeceased > as.Date("2012-08-31") | is.na(dateepisodeceased))]$dur_prim <-
  
  as.integer(difftime(as.Date("2012-08-31"),
                      cla_primary_c2[dateepisodestarted < as.Date("2012-08-31") &
                                       (dateepisodeceased > as.Date("2012-08-31") | is.na(dateepisodeceased))]$dateepisodestarted,
                      units = "days"))

cla_primary_c2[dateepisodestarted < as.Date("2012-08-31") & dateepisodeceased < as.Date("2012-08-31")]$dur_prim <-
  
  as.integer(difftime(cla_primary_c2[dateepisodestarted < as.Date("2012-08-31") &
                                       dateepisodeceased < as.Date("2012-08-31")]$dateepisodeceased,
                      cla_primary_c2[dateepisodestarted < as.Date("2012-08-31") &
                                       dateepisodeceased < as.Date("2012-08-31")]$dateepisodestarted,
                      units = "days"))

cla_primary_c2[, dur_prim := sum(dur_prim, na.rm = T), by = "pmr2"]

tmp <- data.table(
  pupilmatchingrefanonymous = cla_primary_c2$pmr2,
  dur_prim = cla_primary_c2$dur_prim
)

tmp <- tmp[!duplicated(tmp)]

cohort2 <- merge(cohort2,
                 tmp,
                 by = "pupilmatchingrefanonymous",
                 all.x = T)

rm(tmp)

table(cohort2[year == 7 & sch_n_in_year == 1]$exposure_highest_4_grp)
table(is.na(cohort2[year == 7 & sch_n_in_year == 1]$dur_prim))

# now get total number of placements primary school
cla_primary_c2 <- cla_primary_c2[order(pmr2, dateepisodestarted)]
cla_primary_c2$n_placements_prim <- as.integer(NA)

cla_primary_c2$count_episode_for_placement_prim <- T
flag <- cla_primary_c2$placement == shift(cla_primary_c2$placement, type = "lead") &
  cla_primary_c2$poc_start == shift(cla_primary_c2$poc_start, type = "lead") &
  cla_primary_c2$legal_status != shift(cla_primary_c2$legal_status, type = "lead") &
  cla_primary_c2$pmr2 == shift(cla_primary_c2$pmr2, type = "lead")

cla_primary_c2[flag == T]$count_episode_for_placement_prim <- F
rm(flag)

cla_primary_c2[, n_placements_prim :=  sum(count_episode_for_placement_prim), by = "pmr2"]

tmp <- data.table(
  pupilmatchingrefanonymous = cla_primary_c2$pmr2,
  n_placements_prim = cla_primary_c2$n_placements_prim
)

tmp <- tmp[!duplicated(tmp)]

cohort2 <- merge(cohort2,
                 tmp,
                 by = "pupilmatchingrefanonymous",
                 all.x = T)

rm(tmp)

cohort2[ever_care_primary == 0]$n_placements_prim <- NA

table(cohort2[year == 7 & sch_n_in_year == 1]$exposure_highest_4_grp)
table(is.na(cohort2[year == 7 & sch_n_in_year == 1]$n_placements_prim))
table(cohort2[year == 7 & sch_n_in_year == 1]$n_placements_prim)

# according to analysis plan
cohort2$exposure_highest_8_grp <- "None"
cohort2[pupilmatchingrefanonymous %in% ci_tab[sum_prim_sch_yrs == 1]$pupilmatchingrefanonymous]$exposure_highest_8_grp <- "CIN 1 yr"
cohort2[pupilmatchingrefanonymous %in% ci_tab[sum_prim_sch_yrs == 2]$pupilmatchingrefanonymous]$exposure_highest_8_grp <- "CIN 2 yr"
cohort2[pupilmatchingrefanonymous %in% ci_tab[sum_prim_sch_yrs == 3]$pupilmatchingrefanonymous]$exposure_highest_8_grp <- "CIN 3 yr"
cohort2[ever_care_primary == 0 & ever_cpp_primary == 1]$exposure_highest_8_grp <- "CPP"
cohort2[ever_care_primary == 1 & dur_prim < 365 & n_placements_prim < 3]$exposure_highest_8_grp <- "CLA short stable"
cohort2[ever_care_primary == 1 & dur_prim < 365 & n_placements_prim >= 3]$exposure_highest_8_grp <- "CLA short unstable"
cohort2[ever_care_primary == 1 & dur_prim >= 365 & n_placements_prim < 3]$exposure_highest_8_grp <- "CLA long stable"
cohort2[ever_care_primary == 1 & dur_prim >= 365 & n_placements_prim >= 3]$exposure_highest_8_grp <- "CLA long unstable"

cohort2$exposure_highest_8_grp <- factor(cohort2$exposure_highest_8_grp, levels = c("None",
                                                          "CIN 1 yr",
                                                          "CIN 2 yr",
                                                          "CIN 3 yr",
                                                          "CPP",
                                                          "CLA short stable",
                                                          "CLA short unstable",
                                                          "CLA long stable",
                                                          "CLA long unstable"))

table(cohort2[year == 7 & sch_n_in_year == 1]$exposure_highest_4_grp, useNA = "always")
table(cohort2[year == 7 & sch_n_in_year == 1]$exposure_highest_8_grp, useNA = "always")

rm(ci_tab)

# SAVE --------------------------------------------------------------------

save(cohort1, file = "P:/Working/Matt/PROCESSED DATA/cohort1_npd_clean_sch_ad_qual_cscobs.rda")
save(cohort2, file = "P:/Working/Matt/PROCESSED DATA/cohort2_npd_clean_sch_ad_qual_cscobs.rda")
rm(list = ls()); gc()