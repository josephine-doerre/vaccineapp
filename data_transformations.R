# load required packages
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(plotly)

# get data every day
fileURL_bund <- "https://raw.githubusercontent.com/robert-koch-institut/COVID-19-Impfungen_in_Deutschland/master/Aktuell_Deutschland_Bundeslaender_COVID-19-Impfungen.csv"
fileURL_land <- "https://raw.githubusercontent.com/josephine-doerre/COVID-19-Impfungen_in_Deutschland/master/Aktuell_Deutschland_Landkreise_COVID-19-Impfungen.csv"

# seperate list of states in germany
landkreise_id <- read.csv(".../landkreis.csv", sep = ";")

data_bund <-  read.csv(fileURL_bund)
data_landkreis <- read.csv(fileURL_land)

# merge with information about state id
data_landkreis <- as_tibble(data_landkreis)
data_landkreis <- rename(data_landkreis, landkreis_ID = LandkreisId_Impfort)
data_landkreis$landkreis_ID <- as.integer(data_landkreis$landkreis_ID)
data_landkreis <- left_join(data_landkreis,landkreise_id , by = "landkreis_ID")
data_compl <- data_landkreis[complete.cases(data_landkreis), ]
data_compl$Impfdatum <- ymd(data_compl$Impfdatum)

# calcule the counts per day per and age group
data_age <- data_compl %>%
    select(-landkreis_ID,-kreis) %>%
    group_by(Impfdatum,Altersgruppe) %>%
    summarise(Dosen=sum(Anzahl))

# calculate the cumulative sum as a new var cum_sum
data_age$cum_sum <-ave(data_age$Dosen,data_age$Altersgruppe, FUN=cumsum)
data_age <- filter(data_age,!Altersgruppe == "u")
data_age$Impfdatum <- ymd(data_age$Impfdatum)

# renaming and labeling
library(plyr)
data_age$Altersgruppe <- revalue(data_age$Altersgruppe, c("18-59" = "18-59 years", "60+" = "60+", "12-17" = "12-17 years"))
detach("package:plyr", unload=TRUE) 

data_bund$BundeslandId_Impfort <- as.integer(data_bund$BundeslandId_Impfort)
data_bund <- data_bund %>%
    mutate(Bundesland = case_when(
        BundeslandId_Impfort == 1 ~ "SH",
        BundeslandId_Impfort == 2 ~ "HH", 
        BundeslandId_Impfort == 3 ~ "NI",
        BundeslandId_Impfort == 4 ~ "HB",
        BundeslandId_Impfort == 5 ~ "NW",
        BundeslandId_Impfort == 6 ~ "HE",
        BundeslandId_Impfort == 7 ~ "RP",
        BundeslandId_Impfort == 8 ~ "BW", 
        BundeslandId_Impfort == 9 ~ "BY",
        BundeslandId_Impfort == 10 ~ "SL",
        BundeslandId_Impfort == 11 ~ "BE",
        BundeslandId_Impfort == 12 ~ "BB",
        BundeslandId_Impfort == 13 ~ "MV",
        BundeslandId_Impfort == 14 ~ "SN",
        BundeslandId_Impfort == 15 ~ "ST",
        BundeslandId_Impfort == 16 ~ "TH",
        BundeslandId_Impfort == 17 ~"Bundesressort",
        TRUE ~ "unbekannt"))

# turn the date into a date format
data_bund$Impfdatum <- ymd(data_bund$Impfdatum)

# save the transformed data files 
save(data_age, file= "federalstate.rda")
save(data_bund, file="vaccinetype_state.rda")