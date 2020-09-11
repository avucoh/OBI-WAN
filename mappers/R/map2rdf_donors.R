library(plyr)

dem <- read.table("Core_Demographics.tsv", header = T, sep = "\t", na.strings = c("", "NA"), stringsAsFactors = F)

race <- plyr::revalue(dem$race[!is.na(dem$race)],
                           c(Caucasian = ":Caucasian",
                             Asian = ":Asian",
                             "Hispanic/Latino" = ":Hispanic_Latino",
                             "African Am" = ":African_American",
                             "American Indian/Alaska Native" = ":American_Indian_Alaska_Native",
                             "Multiracial" = ":Multiracial"))
race_ttl <- paste0("cid:", dem$ID[!is.na(dem$race)], " a ", race, " .")

donortype <- plyr::revalue(dem$donor.type[!is.na(dem$donor.type) & dem$donor.type != "CMA"],
                           c(Pending = ":Pending",
                             CMA = ":CMA_Donor",
                             Fulminant = ":Fulminant_Donor",
                             Transplant = ":Transplant_Donor",
                             T1D = ":T1D_Donor",
                             `Autoab Pos` = ":Autoab_Pos_Donor",
                             `No diabetes` = ":No_Diabetes_Donor",
                             `Other-No Diabetes` = ":Other_No_Diabetes_Donor",
                             T2D = ":T2D_Donor",
                             `Monogenic Diabetes` = ":Monogenic_Diabetes",
                             `T1D Medalist` = ":T1D_Medalist_Donor",
                             `Other-Diabetes` = ":Other_Diabetes_Donor",
                             `Gestational diabetes` = ":Gestational_Diabetes_Donor",
                             `Cystic fibrosis` = ":Cystic_Fibrosis_Donor",
                             Pregnancy = ":Pregnancy_Donor",
                             `Gastric Bypass` = ":Gastric_Bypass_Donor"))


donortype_ttl <- paste0("cid:", dem$ID[!is.na(dem$donor.type) & dem$donor.type != "CMA"], " a ", donortype, " .")

sex <- plyr::revalue(dem$sex[!is.na(dem$sex)],
                     c("Male" = ":Male",
                     "Female" = ":Female"))
sex_ttl <- paste0("cid:", dem$ID[!is.na(dem$sex)], " a ", sex, " .")

age_ttl <- paste0("cid:", dem$ID[!is.na(dem$age)], " :donorAge ", shQuote(dem$age[!is.na(dem$age)], "cmd"), "^^xsd:decimal .")

prefix <- c("@prefix : <http://purl.org/net/obi-wan#> .",
            "@prefix cid: <https://npoddatashare.coh.org/labkey/study/nPOD%20DataShare/participant.view?participantId=> .",
            "@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .", "")
writeLines(c(prefix, donortype_ttl), "core/donor_type.ttl")
writeLines(c(prefix, age_ttl), "core/donor_age.ttl")
writeLines(c(prefix, race_ttl), "core/donor_race.ttl")
writeLines(c(prefix, sex_ttl), "core/donor_sex.ttl")

