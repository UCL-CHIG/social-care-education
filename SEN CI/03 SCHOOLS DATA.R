
# LOAD --------------------------------------------------------------------

setwd("P:/Working/Matt")

lib_base = "P:/Working/Matt/r-library/"
assign(".lib.loc", c(.libPaths(), lib_base), envir = environment(.libPaths))
rm(lib_base)

library("data.table")

load("PROCESSED DATA/SEN CI/cohort2_npd_clean.rda")

# DfE ---------------------------------------------------------------------

dfe <- data.table(read.csv("P:/Working/WORKING DATA/SCHOOLS/DfE.csv", stringsAsFactors = F))
names(dfe) <- tolower(gsub("name", "", names(dfe)))

length(unique(dfe$urn))
dfe <- dfe[urn %in% cohort2$urn]

length(unique(dfe$urn))

table(cohort2$urn %in% dfe$urn)
table(cohort2[!(urn %in% dfe$urn)]$sourcetable) # ditto

names(dfe)[names(dfe) == "closedate"] <- "schoolclosedate"
names(dfe)[names(dfe) == "gender"] <- "schoolgender"

dfe[schoolclosedate == ""]$schoolclosedate <- "01/01/1800"
dfe$schoolclosedate <- as.Date(dfe$schoolclosedate, format = "%d/%m/%Y")

cohort2 <- merge(cohort2, dfe[, c("urn", "typeofestablishment", "establishmenttypegroup", "phaseofeducation",
                                  "schoolgender", "boarders", "nurseryprovision", "officialsixthform",
                                  "religiouscharacter", "religiousethos", "admissionspolicy",
                                  "schoolcapacity", "specialclasses", "schoolclosedate", "reasonestablishmentclosed")],
                 all.x = T,
                 by = "urn")

rm(dfe)

cohort2[sourcetable == "AP"]$establishmenttypegroup <- "AP" # is NA for AP
cohort2[sourcetable == "AP"]$typeofestablishment <- "AP"

cohort2[sourcetable == "PRU"]$establishmenttypegroup <- "PRU" # is mostly NA for PRU
cohort2[sourcetable == "PRU"]$typeofestablishment <- "PRU"

# major group - c2
cohort2$major_type <- "mainstream"
cohort2[establishmenttypegroup == "Special schools"]$major_type <- "special"
ap <- c("Academy alternative provision converter", "Academy alternative provision sponsor led",
        "AP", "Free schools alternative provision")
cohort2[sourcetable == "AP" | typeofestablishment %in% ap]$major_type <- "AP"
cohort2[sourcetable == "PRU" | typeofestablishment == "Pupil referral unit" | phase == "PR" |
          typeofestablishment == "PRU"]$major_type <- "PRU"
rm(ap)
table(cohort2$major_type, cohort2$year, useNA = "always")

cohort2$major_type_2 <- cohort2$major_type
cohort2[major_type_2 == "PRU"]$major_type_2 <- "AP"

cohort2[, any_ap := any(major_type_2 == "AP"), by = .(pupilmatchingrefanonymous, year)]

cohort2$major_type_3 <- cohort2$major_type_2
cohort2[any_ap == T]$major_type_3 <- "AP"

# MISC CLEANING -----------------------------------------------------------

# ** religious character -----------------------------------------------------

cohort2$religiouscharacter2 <- "None/DNA"

chr <- c("Catholic", "Christian", "Greek Orthodox", "Methodist",
         "Quaker", "Roman Catholic", "Roman Catholic/Church of England",
         "Seventh Day Adventist", "United Reformed Church")
coe17 <- "Church of England"

cohort2[religiouscharacter %in% chr |
          substr(religiouscharacter, 1, 17) %in% coe17]$religiouscharacter2 <- "Christian"
cohort2[religiouscharacter == "Sikh"]$religiouscharacter2 <- "Sikh"
cohort2[religiouscharacter == "Muslim"]$religiouscharacter2 <- "Muslim"
cohort2[religiouscharacter == "Jewish"]$religiouscharacter2 <- "Jewish"

oth <- c("Sikh", "Muslim", "Jewish")
cohort2$religiouscharacter3 <- "None/DNA"
cohort2[religiouscharacter %in% chr |
          substr(religiouscharacter, 1, 17) %in% coe17]$religiouscharacter3 <- "Christian"
cohort2[religiouscharacter %in% oth]$religiouscharacter3 <- "Other"
rm(chr, oth, coe17)

# ** admissions policy -------------------------------------------------------

cohort2$admissionspolicy2 <- cohort2$admissionspolicy
cohort2[admissionspolicy %in% c("", "Not collected")]$admissionspolicy2 <- "Not applicable"
cohort2$admissionspolicy2 <- factor(cohort2$admissionspolicy2,
                                    levels = c("Comprehensive (secondary)", "Modern (secondary)",
                                               "Non-selective", "Selective (grammar)",
                                               "Not applicable"))

cohort2$admissionspolicy3 <- cohort2$admissionspolicy2
cohort2[admissionspolicy3 %in% c("Comprehensive (secondary)",
                                 "Modern (secondary)")]$admissionspolicy3 <- "Non-selective"
cohort2$admissionspolicy3 <- factor(cohort2$admissionspolicy3)

cohort2$admissionspolicy2 <- as.character(cohort2$admissionspolicy2)
cohort2[is.na(admissionspolicy2)]$admissionspolicy2 <- "Not applicable"
cohort2$admissionspolicy2 <- factor(cohort2$admissionspolicy2,
                                    levels = c("Comprehensive (secondary)", "Modern (secondary)",
                                               "Non-selective", "Selective (grammar)",
                                               "Not applicable"))

# ** boarders & special classes ---------------------------------------------------

cohort2$boarders2 <- cohort2$boarders
cohort2[boarders == ""]$boarders2 <- "Not applicable"

cohort2$specialclasses2 <- cohort2$specialclasses
cohort2[specialclasses == ""]$specialclasses2 <- "Not applicable"

# ** phase of education ------------------------------------------------------

cohort2$phaseofeducation2 <- factor(cohort2$phaseofeducation,
                                    levels = c("Nursery", "Primary", "Middle deemed primary",
                                               "Secondary", "Middle deemed secondary",
                                               "All through", "16 plus", "Not applicable"))

cohort2$phaseofeducation2 <- as.character(cohort2$phaseofeducation2)
cohort2[is.na(phaseofeducation2)]$phaseofeducation2 <- "Not applicable"
cohort2$phaseofeducation2 <- factor(cohort2$phaseofeducation2,
                                    levels = c("Nursery", "Primary", "Middle deemed primary",
                                               "Secondary", "Middle deemed secondary",
                                               "All through", "16 plus", "Not applicable"))

# ** school gender -----------------------------------------------------------

cohort2[schoolgender == ""]$schoolgender <- NA
cohort2$schoolgender2 <- factor(cohort2$schoolgender,
                                levels = c("Mixed", "Girls", "Boys"))

# ** school year -------------------------------------------------------------

cohort2$ncyearactual <- factor(cohort2$ncyearactual, # to re-order it
                               levels = c(as.character(1:11), "X"))

# ** school type -------------------------------------------------------------

cohort2[, any_pru := any(major_type == "PRU"), by = .(pupilmatchingrefanonymous, year)]
cohort2$major_type_4 <- cohort2$major_type_2
cohort2[any_ap == T]$major_type_4 <- "AP"
cohort2[any_pru == T]$major_type_4 <- "PRU" # PRU second as is subtype of AP

# SAVE --------------------------------------------------------------------

save(cohort2, file = "PROCESSED DATA/SEN CI/cohort2_npd_clean_sch.rda")
