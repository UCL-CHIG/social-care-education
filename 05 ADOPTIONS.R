# LOAD DATA ----------------------------------------------------------

setwd("P:/Working/Matt")

lib_base = "P:/Working/Matt/r-library/"
assign(".lib.loc", c(.libPaths(), lib_base), envir = environment(.libPaths))
rm(lib_base)

library("data.table")

load("PROCESSED DATA/cohort1_npd_clean_sch.rda")
load("PROCESSED DATA/cohort2_npd_clean_sch.rda")
load("PROCESSED DATA/cla_episodes.rda")
load("PROCESSED DATA/stb.rda")

# WHETHER ADOPTED ---------------------------------------------------------

# cla
lt <- as.POSIXlt(cla_episodes$dateepisodeceased)
cla_episodes$epi_end_ay <- lt$year + (lt$mo >= 8) + 1900 # because mo starts at 0
rm(lt)

cla_episodes$ay_first_adopted <- as.integer(NA)
cla_episodes[reasonepisodeceased %in% c("E11", "E12")]$ay_first_adopted <- cla_episodes[reasonepisodeceased %in% c("E11", "E12")]$epi_end_ay
cla_episodes[, ay_first_adopted := ay_first_adopted[!is.na(ay_first_adopted)][1], by = pmr2]

# stb
table(stb$reasonepisodeceased) 
rm(stb)

# merge - whether adopted
cohort1$adopted <- ifelse(cohort1$pupilmatchingrefanonymous %in% cla_episodes[reasonepisodeceased %in% c("E11", "E12")]$pmr2, T, F)
cohort2$adopted <- ifelse(cohort2$pupilmatchingrefanonymous %in% cla_episodes[reasonepisodeceased %in% c("E11", "E12")]$pmr2, T, F)

# year adopted
tmp <- data.table(
  pupilmatchingrefanonymous = cla_episodes$pmr2,
  ay_first_adopted = cla_episodes$ay_first_adopted
)

tmp <- tmp[!duplicated(tmp)]

cohort1 <- merge(cohort1,
              tmp,
              all.x = T,
              by = "pupilmatchingrefanonymous")

cohort1 <- cohort1[order(pupilmatchingrefanonymous, year, sch_n_in_year)]

cohort2 <- merge(cohort2,
                 tmp,
                 all.x = T,
                 by = "pupilmatchingrefanonymous")

cohort2 <- cohort2[order(pupilmatchingrefanonymous, year, sch_n_in_year)]

rm(tmp)

cohort1$year_first_adopted <- cohort1$ay_first_adopted - 2005
cohort1[, max_year_enrolled := max(year), by = pupilmatchingrefanonymous]

cohort2$year_first_adopted <- (cohort2$ay_first_adopted - 2005) - 1 # to account for reception year (y0)
cohort2[, max_year_enrolled := max(year), by = pupilmatchingrefanonymous]

# ANALYSIS ----------------------------------------------------------------

# cohort1
table(cohort1[year == 7 & sch_n_in_year == 1]$adopted)

table(cohort1[year == 7 & sch_n_in_year == 1]$year_first_adopted)
prop.table(table(cohort1[year == 7 & sch_n_in_year == 1]$year_first_adopted))

table(cohort1[year == 7 & sch_n_in_year == 1 & adopted == T]$max_year_enrolled)
table(cohort1[year == 7 & sch_n_in_year == 1]$max_year_enrolled > cohort1[year == 7 & sch_n_in_year == 1]$year_first_adopted)

table(cohort1[year == 7 & sch_n_in_year == 1 & year_first_adopted == 11]$max_year_enrolled)

# cohort2
table(cohort2[year == 7 & sch_n_in_year == 1]$adopted)

table(cohort2[year == 7 & sch_n_in_year == 1]$year_first_adopted)
prop.table(table(cohort2[year == 1 & sch_n_in_year == 1]$year_first_adopted))

table(cohort2[year == 7 & sch_n_in_year == 1 & adopted == T]$max_year_enrolled)
table(cohort2[year == 7 & sch_n_in_year == 1]$max_year_enrolled > cohort2[year == 7 & sch_n_in_year == 1]$year_first_adopted)

table(cohort2[year == 7 & sch_n_in_year == 1 & year_first_adopted == 11]$max_year_enrolled)

# SAVE --------------------------------------------------------------------

save(cohort1, file = "PROCESSED DATA/cohort1_npd_clean_sch_ad.rda")
save(cohort2, file = "PROCESSED DATA/cohort2_npd_clean_sch_ad.rda")
save(cla_episodes, file = "PROCESSED DATA/cla_episodes_ad.rda")
rm(list = ls()); gc()
