# LOAD --------------------------------------------------------------------

setwd("P:/Working/Matt")

lib_base = "P:/Working/Matt/r-library/"
assign(".lib.loc", c(.libPaths(), lib_base), envir = environment(.libPaths))
rm(lib_base)

library(data.table)

load("P:/Working/Matt/PROCESSED DATA/cohort1_npd_clean_sch_ad_qual_cscobs.rda")
load("P:/Working/Matt/PROCESSED DATA/cohort2_npd_clean_sch_ad_qual_cscobs.rda")

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

# ABSENCE ----------------------------------------------------------------

f <- list.files("P:/Working/WORKING DATA/ABSENCES")
abs_list <- list()
abs_list_c1 <- list()
abs_list_c2 <- list()
for (i in 1:length(f)) {
  print(paste0("trying ", i, " of ", (length(f))))
  abs_list[[i]] <- data.table(read.table(
    paste0("P:/Working/WORKING DATA/ABSENCES/", f[i]),
    header = T,
    sep = "\t",
    stringsAsFactors = F,
    skipNul = T,
    comment.char = ""
  ))
  
  names(abs_list[[i]]) <- tolower(gsub(paste0("_ab", formatC(i + 11,
                                                             format = "d",
                                                             flag = "0",
                                                             width = 2)),
                                       "",
                                       names(abs_list[[i]])))
  
  abs_list_c1[[i]] <- abs_list[[i]][pupilmatchingrefanonymous %in% cohort1$pupilmatchingrefanonymous]
  abs_list_c2[[i]] <- abs_list[[i]][pupilmatchingrefanonymous %in% cohort2$pupilmatchingrefanonymous]

  print(paste0("done ", i))
}

rm(f, i, abs_list)

abs_all_c1 <- rbind(abs_list_c1[[1]],
                    abs_list_c1[[2]],
                    abs_list_c1[[3]],
                    abs_list_c1[[4]],
                    abs_list_c1[[5]],
                    abs_list_c1[[6]],
                    fill = T)

abs_all_c2 <- rbind(abs_list_c2[[1]],
                    abs_list_c2[[2]],
                    abs_list_c2[[3]],
                    abs_list_c2[[4]],
                    abs_list_c2[[5]],
                    abs_list_c2[[6]],
                    fill = T)

rm(abs_list_c1, abs_list_c2)

abs_all_c1 <- abs_all_c1[academicyear %in% sec_years_c1]
abs_all_c2 <- abs_all_c2[academicyear %in% sec_years_c2]

# * cohort 1 --------------------------------------------------------------

abs_all_c1 <- abs_all_c1[!duplicated(abs_all_c1)]
abs_all_c1 <- abs_all_c1[order(pupilmatchingrefanonymous, academicyear)]

table(abs_all_c1$sessionspossible_5halfterms > 0, abs_all_c1$academicyear, useNA = "always")
table(abs_all_c1$sessionspossible_3term > 0, abs_all_c1$academicyear, useNA = "always")
abs_all_c1[academicyear == "2011/2012"]$sessionspossible_5halfterms <- abs_all_c1[academicyear == "2011/2012"]$sessionspossible_3term
table(abs_all_c1$sessionspossible_5halfterms > 0, abs_all_c1$academicyear, useNA = "always")

table(abs_all_c1$authorisedabsence_5halfterms > 0, abs_all_c1$academicyear, useNA = "always")
table(abs_all_c1$authorisedabsence_3term > 0, abs_all_c1$academicyear, useNA = "always")
abs_all_c1[academicyear == "2011/2012"]$authorisedabsence_5halfterms <- abs_all_c1[academicyear == "2011/2012"]$authorisedabsence_3term
table(abs_all_c1$authorisedabsence_5halfterms > 0, abs_all_c1$academicyear, useNA = "always")

table(abs_all_c1$unauthorisedabsence_5halfterms > 0, abs_all_c1$academicyear, useNA = "always")
table(abs_all_c1$unauthorisedabsence_3term > 0, abs_all_c1$academicyear, useNA = "always")
abs_all_c1[academicyear == "2011/2012"]$unauthorisedabsence_5halfterms <- abs_all_c1[academicyear == "2011/2012"]$unauthorisedabsence_3term
table(abs_all_c1$unauthorisedabsence_5halfterms > 0, abs_all_c1$academicyear, useNA = "always")

table(abs_all_c1$overallabsence_5halfterms > 0, abs_all_c1$academicyear, useNA = "always")
table(abs_all_c1$overallabsence_3term > 0, abs_all_c1$academicyear, useNA = "always")
abs_all_c1[academicyear == "2011/2012"]$overallabsence_5halfterms <- abs_all_c1[academicyear == "2011/2012"]$overallabsence_3term
table(abs_all_c1$overallabsence_5halfterms > 0, abs_all_c1$academicyear, useNA = "always")

abs_all_c1 <- abs_all_c1[, c("pupilmatchingrefanonymous", "academicyear",
                      "sessionspossible_5halfterms", "authorisedabsence_5halfterms",
                      "unauthorisedabsence_5halfterms", "overallabsence_5halfterms")]

abs_all_c1[, sessionspossible_5halfterms := sum(sessionspossible_5halfterms), by = .(pupilmatchingrefanonymous, academicyear)]
abs_all_c1[, authorisedabsence_5halfterms := sum(authorisedabsence_5halfterms), by = .(pupilmatchingrefanonymous, academicyear)]
abs_all_c1[, unauthorisedabsence_5halfterms := sum(unauthorisedabsence_5halfterms), by = .(pupilmatchingrefanonymous, academicyear)]
abs_all_c1[, overallabsence_5halfterms := sum(overallabsence_5halfterms), by = .(pupilmatchingrefanonymous, academicyear)]
abs_all_c1 <- abs_all_c1[!duplicated(abs_all_c1)]

cohort1 <- merge(cohort1,
                 abs_all_c1[, c("pupilmatchingrefanonymous", "academicyear",
                                "sessionspossible_5halfterms", "authorisedabsence_5halfterms",
                                "unauthorisedabsence_5halfterms", "overallabsence_5halfterms")],
                 by = c("pupilmatchingrefanonymous", "academicyear"),
                 all.x = T)

save(abs_all_c1, file = "PROCESSED DATA/abs_all_c1.rda")
rm(abs_all_c1)

# calc % sessions missed
cohort1$auth_absence_prop <- cohort1$authorisedabsence_5halfterms / cohort1$sessionspossible_5halfterms
cohort1$unauth_absence_prop <- cohort1$unauthorisedabsence_5halfterms / cohort1$sessionspossible_5halfterms
cohort1$overall_absence_prop <- cohort1$overallabsence_5halfterms / cohort1$sessionspossible_5halfterms

cohort1$persistent_absence_auth_year <- cohort1$auth_absence_prop > 0.1
cohort1$persistent_absence_unauth_year <- cohort1$unauth_absence_prop > 0.1
cohort1$persistent_absence_overall_year <- cohort1$overall_absence_prop > 0.1

# outcome vars to be used
## authorised
cohort1$persistent_absence_auth_y7 <- NA
cohort1$persistent_absence_auth_y8 <- NA
cohort1$persistent_absence_auth_y9 <- NA
cohort1$persistent_absence_auth_y10 <- NA
cohort1$persistent_absence_auth_y11 <- NA

# year 7
cohort1[year == 7]$persistent_absence_auth_y7 <- ifelse(!is.na(cohort1[year == 7]$persistent_absence_auth_year),
                                                        cohort1[year == 7]$pupilmatchingrefanonymous %in% cohort1[year == 7 & persistent_absence_auth_year == T]$pupilmatchingrefanonymous,
                                                        NA)
cohort1[, persistent_absence_auth_y7 := persistent_absence_auth_y7[year == 7][1], by = .(pupilmatchingrefanonymous)]

# year 8
cohort1[year == 8]$persistent_absence_auth_y8 <- ifelse(!is.na(cohort1[year == 8]$persistent_absence_auth_year),
                                                        cohort1[year == 8]$pupilmatchingrefanonymous %in% cohort1[year == 8 & persistent_absence_auth_year == T]$pupilmatchingrefanonymous,
                                                        NA)
cohort1[, persistent_absence_auth_y8 := persistent_absence_auth_y8[year == 8][1], by = .(pupilmatchingrefanonymous)]

# year 9
cohort1[year == 9]$persistent_absence_auth_y9 <- ifelse(!is.na(cohort1[year == 9]$persistent_absence_auth_year),
                                                        cohort1[year == 9]$pupilmatchingrefanonymous %in% cohort1[year == 9 & persistent_absence_auth_year == T]$pupilmatchingrefanonymous,
                                                        NA)
cohort1[, persistent_absence_auth_y9 := persistent_absence_auth_y9[year == 9][1], by = .(pupilmatchingrefanonymous)]

# year 10
cohort1[year == 10]$persistent_absence_auth_y10 <- ifelse(!is.na(cohort1[year == 10]$persistent_absence_auth_year),
                                                        cohort1[year == 10]$pupilmatchingrefanonymous %in% cohort1[year == 10 & persistent_absence_auth_year == T]$pupilmatchingrefanonymous,
                                                        NA)
cohort1[, persistent_absence_auth_y10 := persistent_absence_auth_y10[year == 10][1], by = .(pupilmatchingrefanonymous)]

# year 11
cohort1[year == 11]$persistent_absence_auth_y11 <- ifelse(!is.na(cohort1[year == 11]$persistent_absence_auth_year),
                                                         cohort1[year == 11]$pupilmatchingrefanonymous %in% cohort1[year == 11 & persistent_absence_auth_year == T]$pupilmatchingrefanonymous,
                                                         NA)
cohort1[, persistent_absence_auth_y11 := persistent_absence_auth_y11[year == 11][1], by = .(pupilmatchingrefanonymous)]

table(cohort1[year == 7 & sch_n_in_year == 1]$persistent_absence_auth_y7, useNA = "always")
table(cohort1[year == 7 & sch_n_in_year == 1]$persistent_absence_auth_y8, useNA = "always")
table(cohort1[year == 7 & sch_n_in_year == 1]$persistent_absence_auth_y9, useNA = "always")
table(cohort1[year == 7 & sch_n_in_year == 1]$persistent_absence_auth_y10, useNA = "always")
table(cohort1[year == 7 & sch_n_in_year == 1]$persistent_absence_auth_y11, useNA = "always")

table(cohort1[year == 7 & sch_n_in_year == 1]$persistent_absence_auth_y7, 
      cohort1[year == 7 & sch_n_in_year == 1]$school_legal_status,
      useNA = "always")
table(cohort1[year == 8 & sch_n_in_year == 1]$persistent_absence_auth_y8, 
      cohort1[year == 8 & sch_n_in_year == 1]$school_legal_status,
      useNA = "always")
table(cohort1[year == 9 & sch_n_in_year == 1]$persistent_absence_auth_y9, 
      cohort1[year == 9 & sch_n_in_year == 1]$school_legal_status,
      useNA = "always")
table(cohort1[year == 10 & sch_n_in_year == 1]$persistent_absence_auth_y10, 
      cohort1[year == 10 & sch_n_in_year == 1]$school_legal_status,
      useNA = "always")
table(cohort1[year == 11 & sch_n_in_year == 1]$persistent_absence_auth_y11, 
      cohort1[year == 11 & sch_n_in_year == 1]$school_legal_status,
      useNA = "always")

## unauthorised
cohort1$persistent_absence_unauth_y7 <- NA
cohort1$persistent_absence_unauth_y8 <- NA
cohort1$persistent_absence_unauth_y9 <- NA
cohort1$persistent_absence_unauth_y10 <- NA
cohort1$persistent_absence_unauth_y11 <- NA

# year 7
cohort1[year == 7]$persistent_absence_unauth_y7 <- ifelse(!is.na(cohort1[year == 7]$persistent_absence_unauth_year),
                                                        cohort1[year == 7]$pupilmatchingrefanonymous %in% cohort1[year == 7 & persistent_absence_unauth_year == T]$pupilmatchingrefanonymous,
                                                        NA)
cohort1[, persistent_absence_unauth_y7 := persistent_absence_unauth_y7[year == 7][1], by = .(pupilmatchingrefanonymous)]

# year 8
cohort1[year == 8]$persistent_absence_unauth_y8 <- ifelse(!is.na(cohort1[year == 8]$persistent_absence_unauth_year),
                                                        cohort1[year == 8]$pupilmatchingrefanonymous %in% cohort1[year == 8 & persistent_absence_unauth_year == T]$pupilmatchingrefanonymous,
                                                        NA)
cohort1[, persistent_absence_unauth_y8 := persistent_absence_unauth_y8[year == 8][1], by = .(pupilmatchingrefanonymous)]

# year 9
cohort1[year == 9]$persistent_absence_unauth_y9 <- ifelse(!is.na(cohort1[year == 9]$persistent_absence_unauth_year),
                                                        cohort1[year == 9]$pupilmatchingrefanonymous %in% cohort1[year == 9 & persistent_absence_unauth_year == T]$pupilmatchingrefanonymous,
                                                        NA)
cohort1[, persistent_absence_unauth_y9 := persistent_absence_unauth_y9[year == 9][1], by = .(pupilmatchingrefanonymous)]

# year 10
cohort1[year == 10]$persistent_absence_unauth_y10 <- ifelse(!is.na(cohort1[year == 10]$persistent_absence_unauth_year),
                                                          cohort1[year == 10]$pupilmatchingrefanonymous %in% cohort1[year == 10 & persistent_absence_unauth_year == T]$pupilmatchingrefanonymous,
                                                          NA)
cohort1[, persistent_absence_unauth_y10 := persistent_absence_unauth_y10[year == 10][1], by = .(pupilmatchingrefanonymous)]

# year 11
cohort1[year == 11]$persistent_absence_unauth_y11 <- ifelse(!is.na(cohort1[year == 11]$persistent_absence_unauth_year),
                                                          cohort1[year == 11]$pupilmatchingrefanonymous %in% cohort1[year == 11 & persistent_absence_unauth_year == T]$pupilmatchingrefanonymous,
                                                          NA)
cohort1[, persistent_absence_unauth_y11 := persistent_absence_unauth_y11[year == 11][1], by = .(pupilmatchingrefanonymous)]

table(cohort1[year == 7 & sch_n_in_year == 1]$persistent_absence_unauth_y7, useNA = "always")
table(cohort1[year == 7 & sch_n_in_year == 1]$persistent_absence_unauth_y8, useNA = "always")
table(cohort1[year == 7 & sch_n_in_year == 1]$persistent_absence_unauth_y9, useNA = "always")
table(cohort1[year == 7 & sch_n_in_year == 1]$persistent_absence_unauth_y10, useNA = "always")
table(cohort1[year == 7 & sch_n_in_year == 1]$persistent_absence_unauth_y11, useNA = "always")

table(cohort1[year == 7 & sch_n_in_year == 1]$persistent_absence_unauth_y7, 
      cohort1[year == 7 & sch_n_in_year == 1]$school_legal_status,
      useNA = "always")
table(cohort1[year == 8 & sch_n_in_year == 1]$persistent_absence_unauth_y8, 
      cohort1[year == 8 & sch_n_in_year == 1]$school_legal_status,
      useNA = "always")
table(cohort1[year == 9 & sch_n_in_year == 1]$persistent_absence_unauth_y9, 
      cohort1[year == 9 & sch_n_in_year == 1]$school_legal_status,
      useNA = "always")
table(cohort1[year == 10 & sch_n_in_year == 1]$persistent_absence_unauth_y10, 
      cohort1[year == 10 & sch_n_in_year == 1]$school_legal_status,
      useNA = "always")
table(cohort1[year == 11 & sch_n_in_year == 1]$persistent_absence_unauth_y11, 
      cohort1[year == 11 & sch_n_in_year == 1]$school_legal_status,
      useNA = "always")

## overall
cohort1$persistent_absence_overall_y7 <- NA
cohort1$persistent_absence_overall_y8 <- NA
cohort1$persistent_absence_overall_y9 <- NA
cohort1$persistent_absence_overall_y10 <- NA
cohort1$persistent_absence_overall_y11 <- NA

# year 7
cohort1[year == 7]$persistent_absence_overall_y7 <- ifelse(!is.na(cohort1[year == 7]$persistent_absence_overall_year),
                                                        cohort1[year == 7]$pupilmatchingrefanonymous %in% cohort1[year == 7 & persistent_absence_overall_year == T]$pupilmatchingrefanonymous,
                                                        NA)
cohort1[, persistent_absence_overall_y7 := persistent_absence_overall_y7[year == 7][1], by = .(pupilmatchingrefanonymous)]

# year 8
cohort1[year == 8]$persistent_absence_overall_y8 <- ifelse(!is.na(cohort1[year == 8]$persistent_absence_overall_year),
                                                        cohort1[year == 8]$pupilmatchingrefanonymous %in% cohort1[year == 8 & persistent_absence_overall_year == T]$pupilmatchingrefanonymous,
                                                        NA)
cohort1[, persistent_absence_overall_y8 := persistent_absence_overall_y8[year == 8][1], by = .(pupilmatchingrefanonymous)]

# year 9
cohort1[year == 9]$persistent_absence_overall_y9 <- ifelse(!is.na(cohort1[year == 9]$persistent_absence_overall_year),
                                                        cohort1[year == 9]$pupilmatchingrefanonymous %in% cohort1[year == 9 & persistent_absence_overall_year == T]$pupilmatchingrefanonymous,
                                                        NA)
cohort1[, persistent_absence_overall_y9 := persistent_absence_overall_y9[year == 9][1], by = .(pupilmatchingrefanonymous)]

# year 10
cohort1[year == 10]$persistent_absence_overall_y10 <- ifelse(!is.na(cohort1[year == 10]$persistent_absence_overall_year),
                                                          cohort1[year == 10]$pupilmatchingrefanonymous %in% cohort1[year == 10 & persistent_absence_overall_year == T]$pupilmatchingrefanonymous,
                                                          NA)
cohort1[, persistent_absence_overall_y10 := persistent_absence_overall_y10[year == 10][1], by = .(pupilmatchingrefanonymous)]

# year 11
cohort1[year == 11]$persistent_absence_overall_y11 <- ifelse(!is.na(cohort1[year == 11]$persistent_absence_overall_year),
                                                          cohort1[year == 11]$pupilmatchingrefanonymous %in% cohort1[year == 11 & persistent_absence_overall_year == T]$pupilmatchingrefanonymous,
                                                          NA)
cohort1[, persistent_absence_overall_y11 := persistent_absence_overall_y11[year == 11][1], by = .(pupilmatchingrefanonymous)]

table(cohort1[year == 7 & sch_n_in_year == 1]$persistent_absence_overall_y7, useNA = "always")
table(cohort1[year == 7 & sch_n_in_year == 1]$persistent_absence_overall_y8, useNA = "always")
table(cohort1[year == 7 & sch_n_in_year == 1]$persistent_absence_overall_y9, useNA = "always")
table(cohort1[year == 7 & sch_n_in_year == 1]$persistent_absence_overall_y10, useNA = "always")
table(cohort1[year == 7 & sch_n_in_year == 1]$persistent_absence_overall_y11, useNA = "always")

table(cohort1[year == 7 & sch_n_in_year == 1]$persistent_absence_overall_y7, 
      cohort1[year == 7 & sch_n_in_year == 1]$school_legal_status,
      useNA = "always")
table(cohort1[year == 8 & sch_n_in_year == 1]$persistent_absence_overall_y8, 
      cohort1[year == 8 & sch_n_in_year == 1]$school_legal_status,
      useNA = "always")
table(cohort1[year == 9 & sch_n_in_year == 1]$persistent_absence_overall_y9, 
      cohort1[year == 9 & sch_n_in_year == 1]$school_legal_status,
      useNA = "always")
table(cohort1[year == 10 & sch_n_in_year == 1]$persistent_absence_overall_y10, 
      cohort1[year == 10 & sch_n_in_year == 1]$school_legal_status,
      useNA = "always")
table(cohort1[year == 11 & sch_n_in_year == 1]$persistent_absence_overall_y11, 
      cohort1[year == 11 & sch_n_in_year == 1]$school_legal_status,
      useNA = "always")

# * cohort 2 --------------------------------------------------------------

abs_all_c2 <- abs_all_c2[!duplicated(abs_all_c2)]
abs_all_c2 <- abs_all_c2[order(pupilmatchingrefanonymous, academicyear)]

table(abs_all_c2$sessionspossible_5halfterms > 0, abs_all_c2$academicyear, useNA = "always")
table(abs_all_c2$authorisedabsence_5halfterms > 0, abs_all_c2$academicyear, useNA = "always")
table(abs_all_c2$unauthorisedabsence_5halfterms > 0, abs_all_c2$academicyear, useNA = "always")
table(abs_all_c2$overallabsence_5halfterms > 0, abs_all_c2$academicyear, useNA = "always")

abs_all_c2 <- abs_all_c2[, c("pupilmatchingrefanonymous", "academicyear",
                             "sessionspossible_5halfterms", "authorisedabsence_5halfterms",
                             "unauthorisedabsence_5halfterms", "overallabsence_5halfterms")]

abs_all_c2[, sessionspossible_5halfterms := sum(sessionspossible_5halfterms), by = .(pupilmatchingrefanonymous, academicyear)]
abs_all_c2[, authorisedabsence_5halfterms := sum(authorisedabsence_5halfterms), by = .(pupilmatchingrefanonymous, academicyear)]
abs_all_c2[, unauthorisedabsence_5halfterms := sum(unauthorisedabsence_5halfterms), by = .(pupilmatchingrefanonymous, academicyear)]
abs_all_c2[, overallabsence_5halfterms := sum(overallabsence_5halfterms), by = .(pupilmatchingrefanonymous, academicyear)]
abs_all_c2 <- abs_all_c2[!duplicated(abs_all_c2)]

cohort2 <- merge(cohort2,
                 abs_all_c2[, c("pupilmatchingrefanonymous", "academicyear",
                                "sessionspossible_5halfterms", "authorisedabsence_5halfterms",
                                "unauthorisedabsence_5halfterms", "overallabsence_5halfterms")],
                 by = c("pupilmatchingrefanonymous", "academicyear"),
                 all.x = T)

save(abs_all_c2, file = "PROCESSED DATA/abs_all_c2.rda")
rm(abs_all_c2)

# calc % sessions missed
cohort2$auth_absence_prop <- cohort2$authorisedabsence_5halfterms / cohort2$sessionspossible_5halfterms
cohort2$unauth_absence_prop <- cohort2$unauthorisedabsence_5halfterms / cohort2$sessionspossible_5halfterms
cohort2$overall_absence_prop <- cohort2$overallabsence_5halfterms / cohort2$sessionspossible_5halfterms

cohort2$persistent_absence_auth_year <- cohort2$auth_absence_prop > 0.1
cohort2$persistent_absence_unauth_year <- cohort2$unauth_absence_prop > 0.1
cohort2$persistent_absence_overall_year <- cohort2$overall_absence_prop > 0.1

# outcome vars to be used
## authorised
cohort2$persistent_absence_auth_y7 <- NA
cohort2$persistent_absence_auth_y8 <- NA
cohort2$persistent_absence_auth_y9 <- NA
cohort2$persistent_absence_auth_y10 <- NA
cohort2$persistent_absence_auth_y11 <- NA

# year 7
cohort2[year == 7]$persistent_absence_auth_y7 <- ifelse(!is.na(cohort2[year == 7]$persistent_absence_auth_year),
                                                        cohort2[year == 7]$pupilmatchingrefanonymous %in% cohort2[year == 7 & persistent_absence_auth_year == T]$pupilmatchingrefanonymous,
                                                        NA)
cohort2[, persistent_absence_auth_y7 := persistent_absence_auth_y7[year == 7][1], by = .(pupilmatchingrefanonymous)]

# year 8
cohort2[year == 8]$persistent_absence_auth_y8 <- ifelse(!is.na(cohort2[year == 8]$persistent_absence_auth_year),
                                                        cohort2[year == 8]$pupilmatchingrefanonymous %in% cohort2[year == 8 & persistent_absence_auth_year == T]$pupilmatchingrefanonymous,
                                                        NA)
cohort2[, persistent_absence_auth_y8 := persistent_absence_auth_y8[year == 8][1], by = .(pupilmatchingrefanonymous)]

# year 9
cohort2[year == 9]$persistent_absence_auth_y9 <- ifelse(!is.na(cohort2[year == 9]$persistent_absence_auth_year),
                                                        cohort2[year == 9]$pupilmatchingrefanonymous %in% cohort2[year == 9 & persistent_absence_auth_year == T]$pupilmatchingrefanonymous,
                                                        NA)
cohort2[, persistent_absence_auth_y9 := persistent_absence_auth_y9[year == 9][1], by = .(pupilmatchingrefanonymous)]

# year 10
cohort2[year == 10]$persistent_absence_auth_y10 <- ifelse(!is.na(cohort2[year == 10]$persistent_absence_auth_year),
                                                          cohort2[year == 10]$pupilmatchingrefanonymous %in% cohort2[year == 10 & persistent_absence_auth_year == T]$pupilmatchingrefanonymous,
                                                          NA)
cohort2[, persistent_absence_auth_y10 := persistent_absence_auth_y10[year == 10][1], by = .(pupilmatchingrefanonymous)]

# year 11
cohort2[year == 11]$persistent_absence_auth_y11 <- ifelse(!is.na(cohort2[year == 11]$persistent_absence_auth_year),
                                                          cohort2[year == 11]$pupilmatchingrefanonymous %in% cohort2[year == 11 & persistent_absence_auth_year == T]$pupilmatchingrefanonymous,
                                                          NA)
cohort2[, persistent_absence_auth_y11 := persistent_absence_auth_y11[year == 11][1], by = .(pupilmatchingrefanonymous)]

table(cohort2[year == 7 & sch_n_in_year == 1]$persistent_absence_auth_y7, useNA = "always")
table(cohort2[year == 7 & sch_n_in_year == 1]$persistent_absence_auth_y8, useNA = "always")
table(cohort2[year == 7 & sch_n_in_year == 1]$persistent_absence_auth_y9, useNA = "always")
table(cohort2[year == 7 & sch_n_in_year == 1]$persistent_absence_auth_y10, useNA = "always")
table(cohort2[year == 7 & sch_n_in_year == 1]$persistent_absence_auth_y11, useNA = "always")

table(cohort2[year == 7 & sch_n_in_year == 1]$persistent_absence_auth_y7, 
      cohort2[year == 7 & sch_n_in_year == 1]$school_legal_status,
      useNA = "always")
table(cohort2[year == 8 & sch_n_in_year == 1]$persistent_absence_auth_y8, 
      cohort2[year == 8 & sch_n_in_year == 1]$school_legal_status,
      useNA = "always")
table(cohort2[year == 9 & sch_n_in_year == 1]$persistent_absence_auth_y9, 
      cohort2[year == 9 & sch_n_in_year == 1]$school_legal_status,
      useNA = "always")
table(cohort2[year == 10 & sch_n_in_year == 1]$persistent_absence_auth_y10, 
      cohort2[year == 10 & sch_n_in_year == 1]$school_legal_status,
      useNA = "always")
table(cohort2[year == 11 & sch_n_in_year == 1]$persistent_absence_auth_y11, 
      cohort2[year == 11 & sch_n_in_year == 1]$school_legal_status,
      useNA = "always")

## unauthorised
cohort2$persistent_absence_unauth_y7 <- NA
cohort2$persistent_absence_unauth_y8 <- NA
cohort2$persistent_absence_unauth_y9 <- NA
cohort2$persistent_absence_unauth_y10 <- NA
cohort2$persistent_absence_unauth_y11 <- NA

# year 7
cohort2[year == 7]$persistent_absence_unauth_y7 <- ifelse(!is.na(cohort2[year == 7]$persistent_absence_unauth_year),
                                                          cohort2[year == 7]$pupilmatchingrefanonymous %in% cohort2[year == 7 & persistent_absence_unauth_year == T]$pupilmatchingrefanonymous,
                                                          NA)
cohort2[, persistent_absence_unauth_y7 := persistent_absence_unauth_y7[year == 7][1], by = .(pupilmatchingrefanonymous)]

# year 8
cohort2[year == 8]$persistent_absence_unauth_y8 <- ifelse(!is.na(cohort2[year == 8]$persistent_absence_unauth_year),
                                                          cohort2[year == 8]$pupilmatchingrefanonymous %in% cohort2[year == 8 & persistent_absence_unauth_year == T]$pupilmatchingrefanonymous,
                                                          NA)
cohort2[, persistent_absence_unauth_y8 := persistent_absence_unauth_y8[year == 8][1], by = .(pupilmatchingrefanonymous)]

# year 9
cohort2[year == 9]$persistent_absence_unauth_y9 <- ifelse(!is.na(cohort2[year == 9]$persistent_absence_unauth_year),
                                                          cohort2[year == 9]$pupilmatchingrefanonymous %in% cohort2[year == 9 & persistent_absence_unauth_year == T]$pupilmatchingrefanonymous,
                                                          NA)
cohort2[, persistent_absence_unauth_y9 := persistent_absence_unauth_y9[year == 9][1], by = .(pupilmatchingrefanonymous)]

# year 10
cohort2[year == 10]$persistent_absence_unauth_y10 <- ifelse(!is.na(cohort2[year == 10]$persistent_absence_unauth_year),
                                                            cohort2[year == 10]$pupilmatchingrefanonymous %in% cohort2[year == 10 & persistent_absence_unauth_year == T]$pupilmatchingrefanonymous,
                                                            NA)
cohort2[, persistent_absence_unauth_y10 := persistent_absence_unauth_y10[year == 10][1], by = .(pupilmatchingrefanonymous)]

# year 11
cohort2[year == 11]$persistent_absence_unauth_y11 <- ifelse(!is.na(cohort2[year == 11]$persistent_absence_unauth_year),
                                                            cohort2[year == 11]$pupilmatchingrefanonymous %in% cohort2[year == 11 & persistent_absence_unauth_year == T]$pupilmatchingrefanonymous,
                                                            NA)
cohort2[, persistent_absence_unauth_y11 := persistent_absence_unauth_y11[year == 11][1], by = .(pupilmatchingrefanonymous)]

table(cohort2[year == 7 & sch_n_in_year == 1]$persistent_absence_unauth_y7, useNA = "always")
table(cohort2[year == 7 & sch_n_in_year == 1]$persistent_absence_unauth_y8, useNA = "always")
table(cohort2[year == 7 & sch_n_in_year == 1]$persistent_absence_unauth_y9, useNA = "always")
table(cohort2[year == 7 & sch_n_in_year == 1]$persistent_absence_unauth_y10, useNA = "always")
table(cohort2[year == 7 & sch_n_in_year == 1]$persistent_absence_unauth_y11, useNA = "always")

table(cohort2[year == 7 & sch_n_in_year == 1]$persistent_absence_unauth_y7, 
      cohort2[year == 7 & sch_n_in_year == 1]$school_legal_status,
      useNA = "always")
table(cohort2[year == 8 & sch_n_in_year == 1]$persistent_absence_unauth_y8, 
      cohort2[year == 8 & sch_n_in_year == 1]$school_legal_status,
      useNA = "always")
table(cohort2[year == 9 & sch_n_in_year == 1]$persistent_absence_unauth_y9, 
      cohort2[year == 9 & sch_n_in_year == 1]$school_legal_status,
      useNA = "always")
table(cohort2[year == 10 & sch_n_in_year == 1]$persistent_absence_unauth_y10, 
      cohort2[year == 10 & sch_n_in_year == 1]$school_legal_status,
      useNA = "always")
table(cohort2[year == 11 & sch_n_in_year == 1]$persistent_absence_unauth_y11, 
      cohort2[year == 11 & sch_n_in_year == 1]$school_legal_status,
      useNA = "always")

## overall
cohort2$persistent_absence_overall_y7 <- NA
cohort2$persistent_absence_overall_y8 <- NA
cohort2$persistent_absence_overall_y9 <- NA
cohort2$persistent_absence_overall_y10 <- NA
cohort2$persistent_absence_overall_y11 <- NA

# year 7
cohort2[year == 7]$persistent_absence_overall_y7 <- ifelse(!is.na(cohort2[year == 7]$persistent_absence_overall_year),
                                                           cohort2[year == 7]$pupilmatchingrefanonymous %in% cohort2[year == 7 & persistent_absence_overall_year == T]$pupilmatchingrefanonymous,
                                                           NA)
cohort2[, persistent_absence_overall_y7 := persistent_absence_overall_y7[year == 7][1], by = .(pupilmatchingrefanonymous)]

# year 8
cohort2[year == 8]$persistent_absence_overall_y8 <- ifelse(!is.na(cohort2[year == 8]$persistent_absence_overall_year),
                                                           cohort2[year == 8]$pupilmatchingrefanonymous %in% cohort2[year == 8 & persistent_absence_overall_year == T]$pupilmatchingrefanonymous,
                                                           NA)
cohort2[, persistent_absence_overall_y8 := persistent_absence_overall_y8[year == 8][1], by = .(pupilmatchingrefanonymous)]

# year 9
cohort2[year == 9]$persistent_absence_overall_y9 <- ifelse(!is.na(cohort2[year == 9]$persistent_absence_overall_year),
                                                           cohort2[year == 9]$pupilmatchingrefanonymous %in% cohort2[year == 9 & persistent_absence_overall_year == T]$pupilmatchingrefanonymous,
                                                           NA)
cohort2[, persistent_absence_overall_y9 := persistent_absence_overall_y9[year == 9][1], by = .(pupilmatchingrefanonymous)]

# year 10
cohort2[year == 10]$persistent_absence_overall_y10 <- ifelse(!is.na(cohort2[year == 10]$persistent_absence_overall_year),
                                                             cohort2[year == 10]$pupilmatchingrefanonymous %in% cohort2[year == 10 & persistent_absence_overall_year == T]$pupilmatchingrefanonymous,
                                                             NA)
cohort2[, persistent_absence_overall_y10 := persistent_absence_overall_y10[year == 10][1], by = .(pupilmatchingrefanonymous)]

# year 11
cohort2[year == 11]$persistent_absence_overall_y11 <- ifelse(!is.na(cohort2[year == 11]$persistent_absence_overall_year),
                                                             cohort2[year == 11]$pupilmatchingrefanonymous %in% cohort2[year == 11 & persistent_absence_overall_year == T]$pupilmatchingrefanonymous,
                                                             NA)
cohort2[, persistent_absence_overall_y11 := persistent_absence_overall_y11[year == 11][1], by = .(pupilmatchingrefanonymous)]

table(cohort2[year == 7 & sch_n_in_year == 1]$persistent_absence_overall_y7, useNA = "always")
table(cohort2[year == 7 & sch_n_in_year == 1]$persistent_absence_overall_y8, useNA = "always")
table(cohort2[year == 7 & sch_n_in_year == 1]$persistent_absence_overall_y9, useNA = "always")
table(cohort2[year == 7 & sch_n_in_year == 1]$persistent_absence_overall_y10, useNA = "always")
table(cohort2[year == 7 & sch_n_in_year == 1]$persistent_absence_overall_y11, useNA = "always")

table(cohort2[year == 7 & sch_n_in_year == 1]$persistent_absence_overall_y7, 
      cohort2[year == 7 & sch_n_in_year == 1]$school_legal_status,
      useNA = "always")
table(cohort2[year == 8 & sch_n_in_year == 1]$persistent_absence_overall_y8, 
      cohort2[year == 8 & sch_n_in_year == 1]$school_legal_status,
      useNA = "always")
table(cohort2[year == 9 & sch_n_in_year == 1]$persistent_absence_overall_y9, 
      cohort2[year == 9 & sch_n_in_year == 1]$school_legal_status,
      useNA = "always")
table(cohort2[year == 10 & sch_n_in_year == 1]$persistent_absence_overall_y10, 
      cohort2[year == 10 & sch_n_in_year == 1]$school_legal_status,
      useNA = "always")
table(cohort2[year == 11 & sch_n_in_year == 1]$persistent_absence_overall_y11, 
      cohort2[year == 11 & sch_n_in_year == 1]$school_legal_status,
      useNA = "always")

# EXCLUSIONS --------------------------------------------------------------

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

table(excl_all$category,
      excl_all$academicyear)

table(cat = is.na(excl_all$category),
      excl_all$academicyear) # need to work out number of sessions for the two earlier years

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

save(excl_all_c1, file = "PROCESSED DATA/excl_c1.rda")
save(excl_all_c2, file = "PROCESSED DATA/excl_c2.rda")

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

table(cohort1[year == 7 & sch_n_in_year == 1]$permanentexclusioncount,
      cohort1[year == 7 & sch_n_in_year == 1]$school_legal_status,
      useNA = "always")
table(cohort1[year == 7 & sch_n_in_year == 1]$totalfixedexclusions,
      cohort1[year == 7 & sch_n_in_year == 1]$school_legal_status,
      useNA = "always")
table(cohort1[year == 7 & sch_n_in_year == 1]$totalfixedsessions,
      cohort1[year == 7 & sch_n_in_year == 1]$school_legal_status,
      useNA = "always")

rm(excl_list, excl_all, excl_all_c1, excl_all_c2)

# * outcome vars ----------------------------------------------------------

# cohort 1
cohort1$perm_excl_y7 <- cohort1$pupilmatchingrefanonymous %in% cohort1[year == 7 & !is.na(permanentexclusioncount)]$pupilmatchingrefanonymous
cohort1$perm_excl_y8 <- cohort1$pupilmatchingrefanonymous %in% cohort1[year == 8 & !is.na(permanentexclusioncount)]$pupilmatchingrefanonymous
cohort1$perm_excl_y9 <- cohort1$pupilmatchingrefanonymous %in% cohort1[year == 9 & !is.na(permanentexclusioncount)]$pupilmatchingrefanonymous
cohort1$perm_excl_y10 <- cohort1$pupilmatchingrefanonymous %in% cohort1[year == 10 & !is.na(permanentexclusioncount)]$pupilmatchingrefanonymous
cohort1$perm_excl_y11 <- cohort1$pupilmatchingrefanonymous %in% cohort1[year == 11 & !is.na(permanentexclusioncount)]$pupilmatchingrefanonymous

table(cohort1[year == 7 & sch_n_in_year == 1]$perm_excl_y7, useNA = "always")
table(cohort1[year == 7 & sch_n_in_year == 1]$perm_excl_y8, useNA = "always")
table(cohort1[year == 7 & sch_n_in_year == 1]$perm_excl_y9, useNA = "always")
table(cohort1[year == 7 & sch_n_in_year == 1]$perm_excl_y10, useNA = "always")
table(cohort1[year == 7 & sch_n_in_year == 1]$perm_excl_y11, useNA = "always")

table(cohort1[year == 7 & sch_n_in_year == 1]$perm_excl_y7, 
      cohort1[year == 7 & sch_n_in_year == 1]$school_legal_status,
      useNA = "always")

cohort1$ft_excl_y7 <- cohort1$pupilmatchingrefanonymous %in% cohort1[year == 7 & !is.na(totalfixedexclusions)]$pupilmatchingrefanonymous
cohort1$ft_excl_y8 <- cohort1$pupilmatchingrefanonymous %in% cohort1[year == 8 & !is.na(totalfixedexclusions)]$pupilmatchingrefanonymous
cohort1$ft_excl_y9 <- cohort1$pupilmatchingrefanonymous %in% cohort1[year == 9 & !is.na(totalfixedexclusions)]$pupilmatchingrefanonymous
cohort1$ft_excl_y10 <- cohort1$pupilmatchingrefanonymous %in% cohort1[year == 10 & !is.na(totalfixedexclusions)]$pupilmatchingrefanonymous
cohort1$ft_excl_y11 <- cohort1$pupilmatchingrefanonymous %in% cohort1[year == 11 & !is.na(totalfixedexclusions)]$pupilmatchingrefanonymous

table(cohort1[year == 7 & sch_n_in_year == 1]$ft_excl_y7, useNA = "always")
table(cohort1[year == 7 & sch_n_in_year == 1]$ft_excl_y8, useNA = "always")
table(cohort1[year == 7 & sch_n_in_year == 1]$ft_excl_y9, useNA = "always")
table(cohort1[year == 7 & sch_n_in_year == 1]$ft_excl_y10, useNA = "always")
table(cohort1[year == 7 & sch_n_in_year == 1]$ft_excl_y11, useNA = "always")

# cohort 2
cohort2$perm_excl_y7 <- cohort2$pupilmatchingrefanonymous %in% cohort2[year == 7 & !is.na(permanentexclusioncount)]$pupilmatchingrefanonymous
cohort2$perm_excl_y8 <- cohort2$pupilmatchingrefanonymous %in% cohort2[year == 8 & !is.na(permanentexclusioncount)]$pupilmatchingrefanonymous
cohort2$perm_excl_y9 <- cohort2$pupilmatchingrefanonymous %in% cohort2[year == 9 & !is.na(permanentexclusioncount)]$pupilmatchingrefanonymous
cohort2$perm_excl_y10 <- cohort2$pupilmatchingrefanonymous %in% cohort2[year == 10 & !is.na(permanentexclusioncount)]$pupilmatchingrefanonymous
cohort2$perm_excl_y11 <- cohort2$pupilmatchingrefanonymous %in% cohort2[year == 11 & !is.na(permanentexclusioncount)]$pupilmatchingrefanonymous

table(cohort2[year == 7 & sch_n_in_year == 1]$perm_excl_y7, useNA = "always")
table(cohort2[year == 7 & sch_n_in_year == 1]$perm_excl_y8, useNA = "always")
table(cohort2[year == 7 & sch_n_in_year == 1]$perm_excl_y9, useNA = "always")
table(cohort2[year == 7 & sch_n_in_year == 1]$perm_excl_y10, useNA = "always")
table(cohort2[year == 7 & sch_n_in_year == 1]$perm_excl_y11, useNA = "always")

cohort2$ft_excl_y7 <- cohort2$pupilmatchingrefanonymous %in% cohort2[year == 7 & !is.na(totalfixedexclusions)]$pupilmatchingrefanonymous
cohort2$ft_excl_y8 <- cohort2$pupilmatchingrefanonymous %in% cohort2[year == 8 & !is.na(totalfixedexclusions)]$pupilmatchingrefanonymous
cohort2$ft_excl_y9 <- cohort2$pupilmatchingrefanonymous %in% cohort2[year == 9 & !is.na(totalfixedexclusions)]$pupilmatchingrefanonymous
cohort2$ft_excl_y10 <- cohort2$pupilmatchingrefanonymous %in% cohort2[year == 10 & !is.na(totalfixedexclusions)]$pupilmatchingrefanonymous
cohort2$ft_excl_y11 <- cohort2$pupilmatchingrefanonymous %in% cohort2[year == 11 & !is.na(totalfixedexclusions)]$pupilmatchingrefanonymous

table(cohort2[year == 7 & sch_n_in_year == 1]$ft_excl_y7, useNA = "always")
table(cohort2[year == 7 & sch_n_in_year == 1]$ft_excl_y8, useNA = "always")
table(cohort2[year == 7 & sch_n_in_year == 1]$ft_excl_y9, useNA = "always")
table(cohort2[year == 7 & sch_n_in_year == 1]$ft_excl_y10, useNA = "always")
table(cohort2[year == 7 & sch_n_in_year == 1]$ft_excl_y11, useNA = "always")

# OFF-ROLLING -------------------------------------------------------------

cohort1[, not_enrolled_y8 := !(8 %in% year), by = pupilmatchingrefanonymous]
cohort1[, not_enrolled_y9 := !(9 %in% year), by = pupilmatchingrefanonymous]
cohort1[, not_enrolled_y10 := !(10 %in% year), by = pupilmatchingrefanonymous]
cohort1[, not_enrolled_y11 := !(11 %in% year), by = pupilmatchingrefanonymous]

table(cohort1[year == 7 & sch_n_in_year == 1]$not_enrolled_y8, useNA = "always")
table(cohort1[year == 7 & sch_n_in_year == 1]$not_enrolled_y9, useNA = "always")
table(cohort1[year == 7 & sch_n_in_year == 1]$not_enrolled_y10, useNA = "always")
table(cohort1[year == 7 & sch_n_in_year == 1]$not_enrolled_y11, useNA = "always")

cohort2[, not_enrolled_y8 := !(8 %in% year), by = pupilmatchingrefanonymous]
cohort2[, not_enrolled_y9 := !(9 %in% year), by = pupilmatchingrefanonymous]
cohort2[, not_enrolled_y10 := !(10 %in% year), by = pupilmatchingrefanonymous]
cohort2[, not_enrolled_y11 := !(11 %in% year), by = pupilmatchingrefanonymous]

table(cohort2[year == 7 & sch_n_in_year == 1]$not_enrolled_y8, useNA = "always")
table(cohort2[year == 7 & sch_n_in_year == 1]$not_enrolled_y9, useNA = "always")
table(cohort2[year == 7 & sch_n_in_year == 1]$not_enrolled_y10, useNA = "always")
table(cohort2[year == 7 & sch_n_in_year == 1]$not_enrolled_y11, useNA = "always")

cohort1[, n_years_enrolled := length(unique(year)), by = pupilmatchingrefanonymous]
cohort2[, n_years_enrolled := length(unique(year)), by = pupilmatchingrefanonymous]

table(cohort1[year == 7 & sch_n_in_year == 1]$n_years_enrolled, useNA = "always")
table(cohort2[year == 7 & sch_n_in_year == 1]$n_years_enrolled, useNA = "always")

# SAT GCSE ----------------------------------------------------------------

# KS4_EBACC_E_PTQ_EE = entered for all pillars of the EBacc
# KS4_GCSE_ENGATT_PTQ_EE = attempted English
# KS4_GCSE_MATHATT_PTQ_EE = attempted maths
# KS4_ENTRY_5_3NG_PTQ_EE = entered at least 5 GCSEs or equiv
# KS4_ENTRY_1_3NG_PTQ_EE = entered at least 1 GCSE or equiv

# * cohort 1 --------------------------------------------------------------

ks4_c1 <- data.table(read.table(
  "P:/Working/WORKING DATA/KS4/KS4Pupil_2015_to_2016_KS2_KS1.txt",
  header = T,
  sep = "\t",
  stringsAsFactors = F,
  skipNul = T
))

ks4_c1 <- ks4_c1[KS4_PupilMatchingRefAnonymous %in% cohort1$pupilmatchingrefanonymous]
ks4_c1 <- ks4_c1[KS4_ACADYR == "2015/2016"]
names(ks4_c1) <- tolower(gsub("KS4_", "", names(ks4_c1)))

keep <- c("pupilmatchingrefanonymous", "entry_1_3ng_ptq_ee", "entry_5_3ng_ptq_ee", "ebacc_e_ptq_ee")
ks4_c1 <- ks4_c1[, keep, with = F]
rm(keep)

table(ks4_c1$entry_1_3ng_ptq_ee, useNA = "always")
table(ks4_c1$entry_5_3ng_ptq_ee, useNA = "always")
table(ks4_c1$ebacc_e_ptq_ee, useNA = "always")

ks4_c1 <- ks4_c1[!duplicated(ks4_c1)]

# there are some duplicated children who have values in one record but NA in the other (or differing values)
ks4_c1[, entry_1_3ng_ptq_ee := max(entry_1_3ng_ptq_ee, na.rm = T), by = "pupilmatchingrefanonymous"]
ks4_c1[, entry_5_3ng_ptq_ee := max(entry_5_3ng_ptq_ee, na.rm = T), by = "pupilmatchingrefanonymous"]
ks4_c1[, ebacc_e_ptq_ee := max(ebacc_e_ptq_ee, na.rm = T), by = "pupilmatchingrefanonymous"]

ks4_c1 <- ks4_c1[!duplicated(ks4_c1)]
length(unique(ks4_c1$pupilmatchingrefanonymous))

cohort1 <- merge(cohort1,
                 ks4_c1,
                 by = "pupilmatchingrefanonymous",
                 all.x = T,
                 sort = F)

rm(ks4_c1)

table(cohort1[year == 7 & sch_n_in_year == 1]$entry_1_3ng_ptq_ee, useNA = "always")
table(cohort1[year == 7 & sch_n_in_year == 1]$entry_5_3ng_ptq_ee, useNA = "always")
table(cohort1[year == 7 & sch_n_in_year == 1]$ebacc_e_ptq_ee, useNA = "always")

cohort1[is.na(entry_1_3ng_ptq_ee)]$entry_1_3ng_ptq_ee <- 0
cohort1[is.na(entry_5_3ng_ptq_ee)]$entry_5_3ng_ptq_ee <- 0
cohort1[is.na(ebacc_e_ptq_ee)]$ebacc_e_ptq_ee <- 0

table(cohort1[year == 7 & sch_n_in_year == 1]$entry_1_3ng_ptq_ee, useNA = "always")
table(cohort1[year == 7 & sch_n_in_year == 1]$entry_5_3ng_ptq_ee, useNA = "always")
table(cohort1[year == 7 & sch_n_in_year == 1]$ebacc_e_ptq_ee, useNA = "always")

# * cohort 2 --------------------------------------------------------------

ks4_c2 <- data.table(read.table(
  "P:/Working/WORKING DATA/KS4/KS4Pupil_2017_KS2_KS1.txt",
  header = T,
  sep = "\t",
  stringsAsFactors = F,
  skipNul = T
))

ks4_c2 <- ks4_c2[KS4_PupilMatchingRefAnonymous %in% cohort2$pupilmatchingrefanonymous]
names(ks4_c2) <- tolower(gsub("KS4_", "", names(ks4_c2)))

keep <- c("pupilmatchingrefanonymous", "entry_1_3ng_ptq_ee", "entry_5_3ng_ptq_ee", "ebacc_e_ptq_ee")
ks4_c2 <- ks4_c2[, keep, with = F]
rm(keep)

table(ks4_c2$entry_1_3ng_ptq_ee, useNA = "always")
table(ks4_c2$entry_5_3ng_ptq_ee, useNA = "always")
table(ks4_c2$ebacc_e_ptq_ee, useNA = "always")

ks4_c2 <- ks4_c2[!duplicated(ks4_c2)]

# there are some duplicated children who have values in one record but NA in the other (or differing values)
ks4_c2[, entry_1_3ng_ptq_ee := max(entry_1_3ng_ptq_ee, na.rm = T), by = "pupilmatchingrefanonymous"]
ks4_c2[, entry_5_3ng_ptq_ee := max(entry_5_3ng_ptq_ee, na.rm = T), by = "pupilmatchingrefanonymous"]
ks4_c2[, ebacc_e_ptq_ee := max(ebacc_e_ptq_ee, na.rm = T), by = "pupilmatchingrefanonymous"]

ks4_c2 <- ks4_c2[!duplicated(ks4_c2)]
length(unique(ks4_c2$pupilmatchingrefanonymous))

cohort2 <- merge(cohort2,
                 ks4_c2,
                 by = "pupilmatchingrefanonymous",
                 all.x = T,
                 sort = F)

rm(ks4_c2)

table(cohort2[year == 7 & sch_n_in_year == 1]$entry_1_3ng_ptq_ee, useNA = "always")
table(cohort2[year == 7 & sch_n_in_year == 1]$entry_5_3ng_ptq_ee, useNA = "always")
table(cohort2[year == 7 & sch_n_in_year == 1]$ebacc_e_ptq_ee, useNA = "always")

cohort2[is.na(entry_1_3ng_ptq_ee)]$entry_1_3ng_ptq_ee <- 0
cohort2[is.na(entry_5_3ng_ptq_ee)]$entry_5_3ng_ptq_ee <- 0
cohort2[is.na(ebacc_e_ptq_ee)]$ebacc_e_ptq_ee <- 0

table(cohort2[year == 7 & sch_n_in_year == 1]$entry_1_3ng_ptq_ee, useNA = "always")
table(cohort2[year == 7 & sch_n_in_year == 1]$entry_5_3ng_ptq_ee, useNA = "always")
table(cohort2[year == 7 & sch_n_in_year == 1]$ebacc_e_ptq_ee, useNA = "always")

# SAVE --------------------------------------------------------------------

save(cohort1, file = "PROCESSED DATA/cohort1_npd_clean_sch_ad_qual_cscobs_outcomes.rda")
save(cohort2, file = "PROCESSED DATA/cohort2_npd_clean_sch_ad_qual_cscobs_outcomes.rda")
rm(list = ls()); gc()
