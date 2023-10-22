
setwd("P:/Working/Matt")

lib_base = "P:/Working/Matt/r-library/"
assign(".lib.loc", c(.libPaths(), lib_base), envir = environment(.libPaths))
rm(lib_base)

library(data.table)

load("PROCESSED DATA/cohort1_npd_clean_sch_ad_qual_cscobs_outcomes.rda")
load("PROCESSED DATA/cohort2_npd_clean_sch_ad_qual_cscobs_outcomes.rda")

# ethnicity ---------------------------------------------------------------

white <- c("WBRI", "WIRI", "WIRT", "WOTH", "WROM")
black <- c("BAFR", "BCRB", "BOTH")
mixed <- c("MWAS", "MWBA", "MWBC", "MOTH")
asian <- c("ABAN", "AIND", "AOTH", "APKN", "CHNE")

cohort1$eth_major <- factor(NA, levels = c("White", "Black", "Mixed", "Asian", "Other"))
cohort1[eth_clean %in% white]$eth_major <- "White"
cohort1[eth_clean %in% black]$eth_major <- "Black"
cohort1[eth_clean %in% mixed]$eth_major <- "Mixed"
cohort1[eth_clean %in% asian]$eth_major <- "Asian"
cohort1[eth_clean == "OOTH"]$eth_major <- "Other"
table(cohort1$eth_major, useNA = "always")

cohort2$eth_major <- factor(NA, levels = c("White", "Black", "Mixed", "Asian", "Other"))
cohort2[eth_clean %in% white]$eth_major <- "White"
cohort2[eth_clean %in% black]$eth_major <- "Black"
cohort2[eth_clean %in% mixed]$eth_major <- "Mixed"
cohort2[eth_clean %in% asian]$eth_major <- "Asian"
cohort2[eth_clean == "OOTH"]$eth_major <- "Other"
table(cohort2$eth_major, useNA = "always")

rm(white, black, mixed, asian)

# sen ---------------------------------------------------------------------

cohort1$ever_sen <- cohort1$pupilmatchingrefanonymous %in% cohort1[senprovision_3_groups %in% c("AAPS", "SEHCP")]$pupilmatchingrefanonymous
cohort1$ever_aaps <- cohort1$pupilmatchingrefanonymous %in% cohort1[senprovision_3_groups == "AAPS"]$pupilmatchingrefanonymous
cohort1$ever_sehcp <- cohort1$pupilmatchingrefanonymous %in% cohort1[senprovision_3_groups == "SEHCP"]$pupilmatchingrefanonymous

cohort1$highest_ever_sen <- "None"
cohort1[ever_aaps == T & ever_sehcp == F]$highest_ever_sen <- "AAPS"
cohort1[ever_sehcp == T]$highest_ever_sen <- "SEHCP"
cohort1$highest_ever_sen <- factor(cohort1$highest_ever_sen,
                                   levels = c("None", "AAPS", "SEHCP"))

cohort2$ever_sen <- cohort2$pupilmatchingrefanonymous %in% cohort2[senprovision_3_groups %in% c("AAPS", "SEHCP")]$pupilmatchingrefanonymous
cohort2$ever_aaps <- cohort2$pupilmatchingrefanonymous %in% cohort2[senprovision_3_groups == "AAPS"]$pupilmatchingrefanonymous
cohort2$ever_sehcp <- cohort2$pupilmatchingrefanonymous %in% cohort2[senprovision_3_groups == "SEHCP"]$pupilmatchingrefanonymous

cohort2$highest_ever_sen <- "None"
cohort2[ever_aaps == T & ever_sehcp == F]$highest_ever_sen <- "AAPS"
cohort2[ever_sehcp == T]$highest_ever_sen <- "SEHCP"
cohort2$highest_ever_sen <- factor(cohort2$highest_ever_sen,
                                   levels = c("None", "AAPS", "SEHCP"))

table(cohort1$highest_ever_sen)
table(cohort2$highest_ever_sen)

cohort1$ever_sen_primary <- cohort1$pupilmatchingrefanonymous %in% cohort1[senprovision_3_groups %in% c("AAPS", "SEHCP") &
                                                                             year %in% 1:6]$pupilmatchingrefanonymous
cohort1$ever_aaps_primary <- cohort1$pupilmatchingrefanonymous %in% cohort1[senprovision_3_groups == "AAPS" &
                                                                              year %in% 1:6]$pupilmatchingrefanonymous
cohort1$ever_sehcp_primary <- cohort1$pupilmatchingrefanonymous %in% cohort1[senprovision_3_groups == "SEHCP" &
                                                                               year %in% 1:6]$pupilmatchingrefanonymous

cohort1$highest_ever_sen_primary <- "None"
cohort1[ever_aaps_primary == T & ever_sehcp_primary == F]$highest_ever_sen_primary <- "AAPS"
cohort1[ever_sehcp_primary == T]$highest_ever_sen_primary <- "SEHCP"
cohort1$highest_ever_sen_primary <- factor(cohort1$highest_ever_sen_primary,
                                           levels = c("None", "AAPS", "SEHCP"))

cohort2$ever_sen_primary <- cohort2$pupilmatchingrefanonymous %in% cohort2[senprovision_3_groups %in% c("AAPS", "SEHCP") &
                                                                             year %in% 0:6]$pupilmatchingrefanonymous
cohort2$ever_aaps_primary <- cohort2$pupilmatchingrefanonymous %in% cohort2[senprovision_3_groups == "AAPS" &
                                                                              year %in% 0:6]$pupilmatchingrefanonymous
cohort2$ever_sehcp_primary <- cohort2$pupilmatchingrefanonymous %in% cohort2[senprovision_3_groups == "SEHCP" &
                                                                               year %in% 0:6]$pupilmatchingrefanonymous


cohort2$highest_ever_sen_primary <- "None"
cohort2[ever_aaps_primary == T & ever_sehcp_primary == F]$highest_ever_sen_primary <- "AAPS"
cohort2[ever_sehcp_primary == T]$highest_ever_sen_primary <- "SEHCP"
cohort2$highest_ever_sen_primary <- factor(cohort2$highest_ever_sen_primary,
                                           levels = c("None", "AAPS", "SEHCP"))

table(cohort1$highest_ever_sen)
table(cohort1$highest_ever_sen_primary)

table(cohort2$highest_ever_sen)
table(cohort2$highest_ever_sen_primary)

cohort1$ever_sen_y09 <- cohort1$pupilmatchingrefanonymous %in% cohort1[senprovision_3_groups %in% c("AAPS", "SEHCP") &
                                                                             year %in% 1:9]$pupilmatchingrefanonymous
cohort1$ever_aaps_y09 <- cohort1$pupilmatchingrefanonymous %in% cohort1[senprovision_3_groups == "AAPS" &
                                                                              year %in% 1:9]$pupilmatchingrefanonymous
cohort1$ever_sehcp_y09 <- cohort1$pupilmatchingrefanonymous %in% cohort1[senprovision_3_groups == "SEHCP" &
                                                                               year %in% 1:9]$pupilmatchingrefanonymous

cohort1$highest_ever_sen_y09 <- "None"
cohort1[ever_aaps_y09 == T & ever_sehcp_y09 == F]$highest_ever_sen_y09 <- "AAPS"
cohort1[ever_sehcp_y09 == T]$highest_ever_sen_y09 <- "SEHCP"
cohort1$highest_ever_sen_y09 <- factor(cohort1$highest_ever_sen_y09,
                                           levels = c("None", "AAPS", "SEHCP"))

cohort2$ever_sen_y09 <- cohort2$pupilmatchingrefanonymous %in% cohort2[senprovision_3_groups %in% c("AAPS", "SEHCP") &
                                                                             year %in% 0:9]$pupilmatchingrefanonymous
cohort2$ever_aaps_y09 <- cohort2$pupilmatchingrefanonymous %in% cohort2[senprovision_3_groups == "AAPS" &
                                                                              year %in% 0:9]$pupilmatchingrefanonymous
cohort2$ever_sehcp_y09 <- cohort2$pupilmatchingrefanonymous %in% cohort2[senprovision_3_groups == "SEHCP" &
                                                                               year %in% 0:9]$pupilmatchingrefanonymous


cohort2$highest_ever_sen_y09 <- "None"
cohort2[ever_aaps_y09 == T & ever_sehcp_y09 == F]$highest_ever_sen_y09 <- "AAPS"
cohort2[ever_sehcp_y09 == T]$highest_ever_sen_y09 <- "SEHCP"
cohort2$highest_ever_sen_y09 <- factor(cohort2$highest_ever_sen_y09,
                                           levels = c("None", "AAPS", "SEHCP"))

table(cohort1$highest_ever_sen)
table(cohort1$highest_ever_sen_primary)
table(cohort1$highest_ever_sen_y09)

table(cohort2$highest_ever_sen)
table(cohort2$highest_ever_sen_primary)
table(cohort2$highest_ever_sen_y09)

# school type -------------------------------------------------------------

cohort1$school_legal_status_3_grp <- cohort1$school_legal_status
cohort1[school_legal_status %in% c("AP", "PRU")]$school_legal_status_3_grp <- "AP/PRU"
cohort1$school_legal_status_3_grp <- factor(cohort1$school_legal_status_3_grp)
table(cohort1$school_legal_status_3_grp, cohort1$year, useNA = "always")

cohort2$school_legal_status_3_grp <- cohort2$school_legal_status
cohort2[school_legal_status %in% c("AP", "PRU")]$school_legal_status_3_grp <- "AP/PRU"
cohort2$school_legal_status_3_grp <- factor(cohort2$school_legal_status_3_grp)
table(cohort2$school_legal_status_3_grp, cohort2$year, useNA = "always")

cohort1$school_type_with_secondary <- "Mainstream - LA"
cohort1[school_legal_status == "Academy/Free School/Independent"]$school_type_with_secondary <- "Mainstream - Academy etc"
cohort1[school_type == "Special"]$school_type_with_secondary <- "Special"
cohort1[school_type %in% c("AP", "PRU")]$school_type_with_secondary <- "AP/PRU"

cohort1[, any_ap_pru := any(school_type_with_secondary == "AP/PRU"), by = .(pupilmatchingrefanonymous, year)]
cohort1[, any_mainstream_la := any(school_type_with_secondary == "Mainstream - LA"), by = .(pupilmatchingrefanonymous, year)]
cohort1[, any_mainstream_academy := any(school_type_with_secondary == "Mainstream - Academy etc"), by = .(pupilmatchingrefanonymous, year)]
cohort1[, any_special := any(school_type_with_secondary == "Special"), by = .(pupilmatchingrefanonymous, year)]

table(cohort1$any_mainstream_la, cohort1$any_special) # such a small N, assign as special
table(cohort1$any_mainstream_academy, cohort1$any_special)
cohort1[any_mainstream_la == T & any_special == T]$any_mainstream_la <- F
cohort1[any_mainstream_academy == T & any_special == T]$any_mainstream_academy <- F

cohort1[any_mainstream_la == T & any_ap_pru == T]$school_type_with_secondary <- "Mainstream - LA & AP/PRU"
cohort1[any_mainstream_academy == T & any_ap_pru == T]$school_type_with_secondary <- "Mainstream - Academy etc & AP/PRU"
cohort1[any_special == T & any_ap_pru == T]$school_type_with_secondary <- "Special & AP/PRU"
cohort1[any_mainstream_la == F & any_mainstream_academy == F & any_special == F & any_ap_pru == T]$school_type_with_secondary <- "AP/PRU"

cohort1$school_type_with_secondary <- factor(cohort1$school_type_with_secondary,
                                             levels = c("Mainstream - LA",
                                                        "Mainstream - Academy etc",
                                                        "Special",
                                                        "Special & AP/PRU",
                                                        "Mainstream - LA & AP/PRU",
                                                        "Mainstream - Academy etc & AP/PRU",
                                                        "AP/PRU"))
table(cohort1$school_type_with_secondary, useNA = "always")
table(cohort1[sch_n_in_year == 1]$school_type_with_secondary, cohort1[sch_n_in_year == 1]$year, useNA = "always")

cohort2$school_type_with_secondary <- "Mainstream - LA"
cohort2[school_legal_status == "Academy/Free School/Independent"]$school_type_with_secondary <- "Mainstream - Academy etc"
cohort2[school_type == "Special"]$school_type_with_secondary <- "Special"
cohort2[school_type %in% c("AP", "PRU")]$school_type_with_secondary <- "AP/PRU"

cohort2[, any_ap_pru := any(school_type_with_secondary == "AP/PRU"), by = .(pupilmatchingrefanonymous, year)]
cohort2[, any_mainstream_la := any(school_type_with_secondary == "Mainstream - LA"), by = .(pupilmatchingrefanonymous, year)]
cohort2[, any_mainstream_academy := any(school_type_with_secondary == "Mainstream - Academy etc"), by = .(pupilmatchingrefanonymous, year)]
cohort2[, any_special := any(school_type_with_secondary == "Special"), by = .(pupilmatchingrefanonymous, year)]

table(cohort2$any_mainstream_la, cohort2$any_special) # such a small N, assign as special
table(cohort2$any_mainstream_academy, cohort2$any_special)
cohort2[any_mainstream_la == T & any_special == T]$any_mainstream_la <- F
cohort2[any_mainstream_academy == T & any_special == T]$any_mainstream_academy <- F

cohort2[any_mainstream_la == T & any_ap_pru == T]$school_type_with_secondary <- "Mainstream - LA & AP/PRU"
cohort2[any_mainstream_academy == T & any_ap_pru == T]$school_type_with_secondary <- "Mainstream - Academy etc & AP/PRU"
cohort2[any_special == T & any_ap_pru == T]$school_type_with_secondary <- "Special & AP/PRU"
cohort2[any_mainstream_la == F & any_mainstream_academy == F & any_special == F & any_ap_pru == T]$school_type_with_secondary <- "AP/PRU"

cohort2$school_type_with_secondary <- factor(cohort2$school_type_with_secondary,
                                             levels = c("Mainstream - LA",
                                                        "Mainstream - Academy etc",
                                                        "Special",
                                                        "Special & AP/PRU",
                                                        "Mainstream - LA & AP/PRU",
                                                        "Mainstream - Academy etc & AP/PRU",
                                                        "AP/PRU"))
table(cohort2$school_type_with_secondary, useNA = "always")
table(cohort2[sch_n_in_year == 1]$school_type_with_secondary, cohort2[sch_n_in_year == 1]$year, useNA = "always")

cohort1[, any_ap_pru := NULL]
cohort1[, any_mainstream_la := NULL]
cohort1[, any_mainstream_academy := NULL]
cohort1[, any_special := NULL]
cohort2[, any_ap_pru := NULL]
cohort2[, any_mainstream_la := NULL]
cohort2[, any_mainstream_academy := NULL]
cohort2[, any_special := NULL]

cohort1[, school_type_with_secondary_5_groups := school_type_with_secondary]
cohort1[school_type_with_secondary %in% c("Mainstream - LA", "Mainstream - Academy etc"),
        school_type_with_secondary_5_groups := "Mainstream"]
cohort1[school_type_with_secondary %in% c("Mainstream - LA & AP/PRU", "Mainstream - Academy etc & AP/PRU"),
        school_type_with_secondary_5_groups := "Mainstream & AP/PRU"]
cohort1[, school_type_with_secondary_5_groups := factor(school_type_with_secondary_5_groups,
                                                        levels = c("Mainstream",
                                                                   "Mainstream & AP/PRU",
                                                                   "Special",
                                                                   "Special & AP/PRU",
                                                                   "AP/PRU"))]

table(cohort1[year == 7 & sch_n_in_year == 1]$school_type_with_secondary)
table(cohort1[year == 7 & sch_n_in_year == 1]$school_type_with_secondary_5_groups)

cohort1[, special_school := school_type_with_secondary_5_groups %in% c("Special", "Special & AP/PRU")]
cohort1[, special_school_y711 := F]
cohort1[year %in% 7:11 & special_school == T, special_school_y711 := T]
cohort1[, special_school_y711 := max(special_school_y711), by = .(pupilmatchingrefanonymous)]

cohort1[, special_school_y7 := F]
cohort1[year == 7 & special_school == T, special_school_y7 := T]
cohort1[, special_school_y7 := max(special_school_y7), by = .(pupilmatchingrefanonymous)]

cohort1[, appru := school_type_with_secondary_5_groups %in% c("Mainstream & AP/PRU", "Special & AP/PRU", "AP/PRU")]
cohort1[, appru_y711 := F]
cohort1[year %in% 7:11 & appru == T, appru_y711 := T]
cohort1[, appru_y711 := max(appru_y711), by = .(pupilmatchingrefanonymous)]

cohort1[, appru_y79 := F]
cohort1[year %in% 7:9 & appru == T, appru_y79 := T]
cohort1[, appru_y79 := max(appru_y79), by = .(pupilmatchingrefanonymous)]

cohort1[, appru_y06 := F]
cohort1[year %in% 0:6 & appru == T, appru_y06 := T]
cohort1[, appru_y06 := max(appru_y06), by = .(pupilmatchingrefanonymous)]

cohort1[, appru_y09 := F]
cohort1[year %in% 0:9 & appru == T, appru_y09 := T]
cohort1[, appru_y09 := max(appru_y09), by = .(pupilmatchingrefanonymous)]

cohort2[, school_type_with_secondary_5_groups := school_type_with_secondary]
cohort2[school_type_with_secondary %in% c("Mainstream - LA", "Mainstream - Academy etc"),
        school_type_with_secondary_5_groups := "Mainstream"]
cohort2[school_type_with_secondary %in% c("Mainstream - LA & AP/PRU", "Mainstream - Academy etc & AP/PRU"),
        school_type_with_secondary_5_groups := "Mainstream & AP/PRU"]
cohort2[, school_type_with_secondary_5_groups := factor(school_type_with_secondary_5_groups,
                                                        levels = c("Mainstream",
                                                                   "Mainstream & AP/PRU",
                                                                   "Special",
                                                                   "Special & AP/PRU",
                                                                   "AP/PRU"))]

table(cohort2[year == 7 & sch_n_in_year == 1]$school_type_with_secondary)
table(cohort2[year == 7 & sch_n_in_year == 1]$school_type_with_secondary_5_groups)

cohort2[, special_school := school_type_with_secondary_5_groups %in% c("Special", "Special & AP/PRU")]
cohort2[, special_school_y711 := F]
cohort2[year %in% 7:11 & special_school == T, special_school_y711 := T]
cohort2[, special_school_y711 := max(special_school_y711), by = .(pupilmatchingrefanonymous)]

cohort2[, special_school_y7 := F]
cohort2[year == 7 & special_school == T, special_school_y7 := T]
cohort2[, special_school_y7 := max(special_school_y7), by = .(pupilmatchingrefanonymous)]

cohort2[, appru := school_type_with_secondary_5_groups %in% c("Mainstream & AP/PRU", "Special & AP/PRU", "AP/PRU")]
cohort2[, appru_y711 := F]
cohort2[year %in% 7:11 & appru == T, appru_y711 := T]
cohort2[, appru_y711 := max(appru_y711), by = .(pupilmatchingrefanonymous)]

cohort2[, appru_y79 := F]
cohort2[year %in% 7:9 & appru == T, appru_y79 := T]
cohort2[, appru_y79 := max(appru_y79), by = .(pupilmatchingrefanonymous)]

cohort2[, appru_y06 := F]
cohort2[year %in% 0:6 & appru == T, appru_y06 := T]
cohort2[, appru_y06 := max(appru_y06), by = .(pupilmatchingrefanonymous)]

cohort2[, appru_y09 := F]
cohort2[year %in% 0:9 & appru == T, appru_y09 := T]
cohort2[, appru_y09 := max(appru_y09), by = .(pupilmatchingrefanonymous)]

# language ----------------------------------------------------------------

cohort1$language_clean_2_grp <- "eng"
cohort1[language_clean %in% c("oth", "unknown")]$language_clean_2_grp <- "other/unknown"

cohort1[language_clean == "unknown", language_clean := NA]
cohort1[, language_clean := factor(language_clean)]

cohort2$language_clean_2_grp <- "eng"
cohort2[language_clean %in% c("oth", "unknown")]$language_clean_2_grp <- "other/unknown"

cohort2[language_clean == "unknown", language_clean := NA]
cohort2[, language_clean := factor(language_clean)]

# exposure ----------------------------------------------------------------

cohort1$exposure_highest_4_grp_y46 <- factor(cohort1$exposure_highest_4_grp_y46,
                                             levels = c("None",
                                                        "CiN/STB",
                                                        "CPP",
                                                        "Care"),
                                             labels = c("None",
                                                        "CiN",
                                                        "CPP",
                                                        "CLA"))
cohort2$exposure_highest_4_grp_y46 <- factor(cohort2$exposure_highest_4_grp_y46,
                                             levels = c("None",
                                                        "CiN/STB",
                                                        "CPP",
                                                        "Care"),
                                             labels = c("None",
                                                        "CiN",
                                                        "CPP",
                                                        "CLA"))

cohort1$exposure_highest_4_grp_y49 <- factor(cohort1$exposure_highest_4_grp_y49,
                                             levels = c("None",
                                                        "CiN/STB",
                                                        "CPP",
                                                        "Care"),
                                             labels = c("None",
                                                        "CiN",
                                                        "CPP",
                                                        "CLA"))
cohort2$exposure_highest_4_grp_y49 <- factor(cohort2$exposure_highest_4_grp_y49,
                                             levels = c("None",
                                                        "CiN/STB",
                                                        "CPP",
                                                        "Care"),
                                             labels = c("None",
                                                        "CiN",
                                                        "CPP",
                                                        "CLA"))

# region ------------------------------------------------------------------

cohort1$region_major <- factor(cohort1$region_major)
cohort2$region_major <- factor(cohort2$region_major)

# not enrolled and gcses --------------------------------------------------

cohort1[, not_enrolled_y89 := not_enrolled_y8 | not_enrolled_y9]
cohort1[, not_enrolled_y1011 := not_enrolled_y10 | not_enrolled_y11]
cohort1[, not_enrolled_y1011_nogcses := not_enrolled_y1011 & entry_5_3ng_ptq_ee == 0]
cohort1[, not_enrolled_y811 := not_enrolled_y89 | not_enrolled_y1011]
cohort1$no_gcses <- as.integer(!cohort1$entry_5_3ng_ptq_ee)

cohort2[, not_enrolled_y89 := not_enrolled_y8 | not_enrolled_y9]
cohort2[, not_enrolled_y1011 := not_enrolled_y10 | not_enrolled_y11]
cohort2[, not_enrolled_y1011_nogcses := not_enrolled_y1011 & entry_5_3ng_ptq_ee == 0]
cohort2[, not_enrolled_y811 := not_enrolled_y89 | not_enrolled_y1011]
cohort2$no_gcses <- as.integer(!cohort2$entry_5_3ng_ptq_ee)

# idaci -------------------------------------------------------------------

cohort1[, idaci_quintiles := as.factor(idaci_quintiles)]

cohort1[, idaci_fsm := factor(NA, levels = c("1_1", "1_0",
                                                "2_1", "2_0",
                                                "3_1", "3_0",
                                                "4_1", "4_0",
                                                "5_1", "5_0"))]
cohort1[idaci_quintiles == 1 & fsmeligible == 1, idaci_fsm := "1_1"]
cohort1[idaci_quintiles == 1 & fsmeligible == 0, idaci_fsm := "1_0"]
cohort1[idaci_quintiles == 2 & fsmeligible == 1, idaci_fsm := "2_1"]
cohort1[idaci_quintiles == 2 & fsmeligible == 0, idaci_fsm := "2_0"]
cohort1[idaci_quintiles == 3 & fsmeligible == 1, idaci_fsm := "3_1"]
cohort1[idaci_quintiles == 3 & fsmeligible == 0, idaci_fsm := "3_0"]
cohort1[idaci_quintiles == 4 & fsmeligible == 1, idaci_fsm := "4_1"]
cohort1[idaci_quintiles == 4 & fsmeligible == 0, idaci_fsm := "4_0"]
cohort1[idaci_quintiles == 5 & fsmeligible == 1, idaci_fsm := "5_1"]
cohort1[idaci_quintiles == 5 & fsmeligible == 0, idaci_fsm := "5_0"]

table(cohort1[year == 7 & sch_n_in_year == 1]$idaci_quintiles, useNA = "always")
table(cohort1[year == 7 & sch_n_in_year == 1]$fsmeligible, useNA = "always")
table(cohort1[year == 7 & sch_n_in_year == 1]$idaci_quintiles,
      cohort1[year == 7 & sch_n_in_year == 1]$fsmeligible, useNA = "always")
table(cohort1[year == 7 & sch_n_in_year == 1]$idaci_fsm, useNA = "always")

cohort2[, idaci_quintiles := as.factor(idaci_quintiles)]

cohort2[, idaci_fsm := factor(NA, levels = c("1_1", "1_0",
                                             "2_1", "2_0",
                                             "3_1", "3_0",
                                             "4_1", "4_0",
                                             "5_1", "5_0"))]
cohort2[idaci_quintiles == 1 & fsmeligible == 1, idaci_fsm := "1_1"]
cohort2[idaci_quintiles == 1 & fsmeligible == 0, idaci_fsm := "1_0"]
cohort2[idaci_quintiles == 2 & fsmeligible == 1, idaci_fsm := "2_1"]

cohort2[idaci_quintiles == 2 & fsmeligible == 0, idaci_fsm := "2_0"]
cohort2[idaci_quintiles == 3 & fsmeligible == 1, idaci_fsm := "3_1"]
cohort2[idaci_quintiles == 3 & fsmeligible == 0, idaci_fsm := "3_0"]
cohort2[idaci_quintiles == 4 & fsmeligible == 1, idaci_fsm := "4_1"]
cohort2[idaci_quintiles == 4 & fsmeligible == 0, idaci_fsm := "4_0"]
cohort2[idaci_quintiles == 5 & fsmeligible == 1, idaci_fsm := "5_1"]
cohort2[idaci_quintiles == 5 & fsmeligible == 0, idaci_fsm := "5_0"]

table(cohort2[year == 7 & sch_n_in_year == 1]$idaci_quintiles, useNA = "always")
table(cohort2[year == 7 & sch_n_in_year == 1]$fsmeligible, useNA = "always")
table(cohort2[year == 7 & sch_n_in_year == 1]$idaci_quintiles,
      cohort2[year == 7 & sch_n_in_year == 1]$fsmeligible, useNA = "always")
table(cohort2[year == 7 & sch_n_in_year == 1]$idaci_fsm, useNA = "always")
gc()


# grouped outcomes --------------------------------------------------------

cohort1[, persistent_absence_auth_y79 := persistent_absence_auth_y7 | persistent_absence_auth_y8 | persistent_absence_auth_y9]
cohort1[, persistent_absence_auth_y1011 := persistent_absence_auth_y10 | persistent_absence_auth_y11]
cohort1[, persistent_absence_unauth_y79 := persistent_absence_unauth_y7 | persistent_absence_unauth_y8 | persistent_absence_unauth_y9]
cohort1[, persistent_absence_unauth_y1011 := persistent_absence_unauth_y10 | persistent_absence_unauth_y11]
cohort1[, persistent_absence_overall_y79 := persistent_absence_overall_y7 | persistent_absence_overall_y8 | persistent_absence_overall_y9]
cohort1[, persistent_absence_overall_y1011 := persistent_absence_overall_y10 | persistent_absence_overall_y11]

cohort1[, ft_excl_y79 := ft_excl_y7 | ft_excl_y8 | ft_excl_y9]
cohort1[, ft_excl_y1011 := ft_excl_y10 | ft_excl_y11]
cohort1[, ft_excl_y711 := ft_excl_y79 | ft_excl_y1011]

cohort1[, perm_excl_y79 := perm_excl_y7 | perm_excl_y8 | perm_excl_y9]
cohort1[, perm_excl_y1011 := perm_excl_y10 | perm_excl_y11]
cohort1[, perm_excl_y711 := perm_excl_y79 | perm_excl_y1011]

cohort1[, ever_not_enrolled := not_enrolled_y8 | not_enrolled_y9 | not_enrolled_y10 | not_enrolled_y11]

cohort2[, persistent_absence_auth_y79 := persistent_absence_auth_y7 | persistent_absence_auth_y8 | persistent_absence_auth_y9]
cohort2[, persistent_absence_auth_y1011 := persistent_absence_auth_y10 | persistent_absence_auth_y11]
cohort2[, persistent_absence_unauth_y79 := persistent_absence_unauth_y7 | persistent_absence_unauth_y8 | persistent_absence_unauth_y9]
cohort2[, persistent_absence_unauth_y1011 := persistent_absence_unauth_y10 | persistent_absence_unauth_y11]
cohort2[, persistent_absence_overall_y79 := persistent_absence_overall_y7 | persistent_absence_overall_y8 | persistent_absence_overall_y9]
cohort2[, persistent_absence_overall_y1011 := persistent_absence_overall_y10 | persistent_absence_overall_y11]

cohort2[, ft_excl_y79 := ft_excl_y7 | ft_excl_y8 | ft_excl_y9]
cohort2[, ft_excl_y1011 := ft_excl_y10 | ft_excl_y11]
cohort2[, ft_excl_y711 := ft_excl_y79 | ft_excl_y1011]

cohort2[, perm_excl_y79 := perm_excl_y7 | perm_excl_y8 | perm_excl_y9]
cohort2[, perm_excl_y1011 := perm_excl_y10 | perm_excl_y11]
cohort2[, perm_excl_y711 := perm_excl_y79 | perm_excl_y1011]

cohort2[, ever_not_enrolled := not_enrolled_y8 | not_enrolled_y9 | not_enrolled_y10 | not_enrolled_y11]


# save --------------------------------------------------------------------

save(cohort1, file = "PROCESSED DATA/cohort1_npd_clean_sch_ad_qual_cscobs_outcomes_misc.rda")
save(cohort2, file = "PROCESSED DATA/cohort2_npd_clean_sch_ad_qual_cscobs_outcomes_misc.rda")
rm(list = ls()); gc()