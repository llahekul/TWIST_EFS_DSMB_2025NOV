# make ADSV
library(dplyr)


# screening/baseline
sb <- sv_sb[c("Subject", "FolderName", "SVDAT")]

# discharge
discharge <- sv_dchg[c("Subject", "FolderName", "SVDAT")]

# follow-up visits
visits <- sv[c("Subject", "FolderName", "SVDAT", "SVOCCUR")]

# make sure both data frames have the same columns
all_cols <- union(union(colnames(sb), colnames(discharge)), colnames(visits))

# add missing columns to each df
sb[setdiff(all_cols, colnames(sb))] <- NA
discharge[setdiff(all_cols, colnames(discharge))] <- NA
visits[setdiff(all_cols, colnames(visits))] <- NA

# row-bind them together
adsv <- rbind(sb, visits, discharge)


# create VISIT and VISITNUM variable
adsv <- adsv %>%
  mutate(
    VISIT = recode(FolderName,
                   "1 Month Follow-Up"         = "30 Days",
                   "1 Year Follow-Up"          = "1 Year",
                   "2 Year Follow-Up"          = "2 Years",
                   "3 Month Follow-Up"         = "3 Months",
                   "6 Month Follow-Up"         = "6 Months",
                   "Discharge"                 = "Discharge",
                   "Screening/Baseline"        = "Screening/Baseline"
    ),
    VISITNUM = recode(FolderName,
                      "1 Month Follow-Up"   = 3,
                      "1 Year Follow-Up"    = 6,
                      "2 Year Follow-Up"    = 7,
                      "3 Month Follow-Up"   = 4,
                      "6 Month Follow-Up"   = 5,
                      "Discharge"           = 2,
                      "Screening/Baseline"  = 1
    )
  ) %>%
  select(-FolderName)

# bring in enrolled population
enrolled <- pr2[!is.na(pr2$PRSTDAT),]
enrolled <- enrolled[c("Subject")]

# merge in procedure date to all_visits (this removes patients who didn't have procedure attempted)
adsv <- left_join(enrolled, adsv, by="Subject")

# remove rows where SVOCCUR == "N"
adsv <- adsv %>%
  filter(SVOCCUR != "No" | is.na(SVOCCUR))


# add phantom rows
# create subject-by-visit grid
subjects <- unique(adsv$Subject)
visits <- unique(adsv$VISIT)

phantom_grid <- expand.grid(Subject = subjects, VISIT = visits)

# step 2: merge and assign DTYPE
adsv <- phantom_grid %>%
  left_join(adsv, by = c("Subject", "VISIT")) %>%
  mutate(
    DTYPE = if_else(is.na(SVDAT), "PHANTOM", NA_character_),
    
    VISITNUM = case_when(
      VISIT == "Screening/Baseline" ~ 1,
      VISIT == "Discharge"          ~ 2,
      VISIT == "30 Days"            ~ 3,
      VISIT == "3 Months"           ~ 4,
      VISIT == "6 Months"           ~ 5,
      VISIT == "2 Years"            ~ 7,
      VISIT == "1 Year"             ~ 6,
      TRUE ~ NA_real_  # handle any unexpected or future visits
    )
  )

# merge in procedure date to all_visits (this removes patients who didn't have procedure attempted)
enrolled <- pr2[!is.na(pr2$PRSTDAT),]
enrolled <- enrolled[c("Subject", "PRSTDAT")]

#rename procedure date
enrolled <- enrolled %>%
  rename(procedure_dt = PRSTDAT)

# merge in procedure date to all_visits (this removes patients who didn't have procedure attempted)
adsv <- left_join(enrolled, adsv, by="Subject")

# remove SVOCCUR as it could get confusing bc some visits that occured have a missing SVOCCUR
adsv <- select(adsv, -SVOCCUR)

# ANL01FL == "Y" if visit occured
#adsv$ANL01FL <- ifelse(!is.na(adsv$SVDAT), "Y", "N")

# calculates days from procedure date to visit
adsv <- adsv %>%
  mutate(VISITDY = as.numeric(difftime(SVDAT, procedure_dt, units = "days"))) %>%
  arrange(Subject, VISITNUM)

# add in USUBJID too
USUBJID <- pr2[c("Subject", "USUBJID")]
adsv <- left_join(adsv, USUBJID, by="Subject")


# get deaths
# CEC confirmed deaths
deaths_cec <- subset(fa_cec, fa_cec$FAORRES_DTHTYP %in% c("Cardiovascular", "Non-cardiovascular"))
# subset variables
deaths_cec <- deaths_cec[c("Subject", "FADAT_NEW")]

# deaths in AE log
deaths_ae <- subset(ae, ae$AESDTH == 1)
deaths_ae <- deaths_ae[c("Subject", "DDDAT")]

deaths <- full_join(deaths_ae, deaths_cec, by="Subject")

# get rid of duplicate death
deaths <- deaths %>%
  distinct(Subject, .keep_all = TRUE)

# pick death in AE log over CEC-death
# per Rafeek - most probably was a data entry error from Innovalve team when transfer data for RedCap to RAVE
deaths$death_dt <- as.POSIXct(coalesce(deaths$DDDAT, deaths$FADAT_NEW))
deaths <- deaths[c("Subject", "death_dt")]

# add deaths into adsv
adsv <- left_join(adsv, deaths, by = "Subject")

# create death day 
adsv <- adsv %>%
  mutate(DEATHDY = as.numeric(difftime(death_dt, procedure_dt, units = "days"))) %>%
  arrange(Subject, VISITNUM) 

# create variable showing days from procedure to today
adsv <- adsv %>%
  mutate(days_since_procedure = as.numeric(difftime(Sys.Date(), procedure_dt, units = "days")))


# eligible for visit flag
# will need to add explants/LTFUs/discontinuations
adsv <- adsv %>%
  mutate(ELIGIBLE_FL = case_when(
    VISIT == "30 Days" & !is.na(days_since_procedure) &
      days_since_procedure >= (30.5 * 1 - 7) & (is.na(DEATHDY) | DEATHDY >= (30.5 * 1 - 7)) ~ "Y",
    
    VISIT == "6 Months" & !is.na(days_since_procedure) &
      days_since_procedure >= (30.5 * 6 - 14) & (is.na(DEATHDY) | DEATHDY >= (30.5 * 6 - 14)) ~ "Y",
    
    VISIT == "1 Year" & !is.na(days_since_procedure) &
      days_since_procedure >= (30.5 * 12 - 30) & (is.na(DEATHDY) | DEATHDY >= (30.5 * 12 - 30)) ~ "Y",
    
    VISIT == "2 Years" & !is.na(days_since_procedure) &
      days_since_procedure >= (30.5 * 24 - 45) & (is.na(DEATHDY) | DEATHDY >= (30.5 * 24 - 45)) ~ "Y",
    
    VISIT %in% c("30 Days", "6 Months", "1 Year", "2 Years") ~ "N",
    
    TRUE ~ NA_character_
  ))

# create CRIT01FL = "Y" if the visit fell within the window
adsv <- adsv %>%
  mutate(CRIT01FL = case_when(
    VISIT == "30 Days" & !is.na(VISITDY) &
      abs(VISITDY) >= (30.5 * 1 - 7) & abs(VISITDY) <= (30.5 * 1 + 7) ~ "Y",
    VISIT == "30 Days" ~ "N",
    
    VISIT == "6 Months" & !is.na(VISITDY) &
      abs(VISITDY) >= (30.5 * 6 - 14) & abs(VISITDY) <= (30.5 * 6 + 14) ~ "Y",
    VISIT == "6 Months" ~ "N",
    
    VISIT == "1 Year" & !is.na(VISITDY) &
      abs(VISITDY) >= (30.5 * 12 - 30) & abs(VISITDY) <= (30.5 * 12 + 30) ~ "Y",
    VISIT == "1 Year" ~ "N",
    
    VISIT == "2 Years" & !is.na(VISITDY) &
      abs(VISITDY) >= (30.5 * 24 - 45) & abs(VISITDY) <= (30.5 * 24 + 45) ~ "Y",
    VISIT == "2 Years" ~ "N",
    
    VISIT %in% c("30 Days", "6 Months", "1 Year", "2 Years") ~ "N",
    
    TRUE ~ NA_character_
  ))

# create CRIT02FL = "Y" if the visit fell outside of the window
adsv <- adsv %>%
  mutate(CRIT02FL = case_when(
    VISIT == "30 Days" & !is.na(VISITDY) & CRIT01FL == "N" ~ "Y",
    VISIT == "30 Days" ~ "N",
    
    VISIT == "6 Months" & !is.na(VISITDY) & CRIT01FL == "N" ~ "Y",
    VISIT == "6 Months" ~ "N",
    
    VISIT == "1 Year" & !is.na(VISITDY) & CRIT01FL == "N" ~ "Y",
    VISIT == "1 Year" ~ "N",
    
    VISIT == "2 Years" & !is.na(VISITDY) & CRIT01FL == "N" ~ "Y",
    VISIT == "2 Years" ~ "N",
    
    VISIT %in% c("30 Days", "6 Months", "1 Year", "2 Years") ~ "N",
    
    TRUE ~ NA_character_
  ))

# visit not performaed (but eligble)
adsv <- adsv %>%
  mutate(VISIT_NOT_PERFORMED = case_when(
    VISIT %in% c("30 Days", "6 Months", "1 Year", "2 Years") &
      is.na(SVDAT) & ELIGIBLE_FL == "Y" & CRIT01FL == "N" & CRIT02FL == "N" ~ "Y",
    VISIT %in% c("30 Days", "6 Months", "1 Year", "2 Years") ~ "N",
    TRUE ~ NA_character_
  ))

# died before window flag
adsv <- adsv %>%
  mutate(DIED_BEFORE_WINDOW = case_when(
    VISIT == "30 Days" & DEATHDY < (30.5 * 1 - 7) ~ "Y",
    
    VISIT == "6 Months" &  DEATHDY < (30.5 * 6 - 14) ~ "Y",
    
    VISIT == "1 Year" & DEATHDY < (30.5 * 12 - 30) ~ "Y",
    
    VISIT == "2 Years" & DEATHDY < (30.5 * 24 - 45) ~ "Y",
    
    VISIT %in% c("30 Days", "6 Months", "1 Year", "2 Years") ~ "N",
    
    TRUE ~ NA_character_
  ))

# visit not due flag
adsv <- adsv %>%
  mutate(VISIT_NOT_DUE = case_when(
    VISIT == "30 Days" & DIED_BEFORE_WINDOW != "Y" & days_since_procedure < (30.5 * 1 - 7) ~ "Y",
    
    VISIT == "6 Months" & DIED_BEFORE_WINDOW != "Y" & days_since_procedure < (30.5 * 6 - 14) ~ "Y",
    
    VISIT == "1 Year" & DIED_BEFORE_WINDOW != "Y" & days_since_procedure < (30.5 * 12 - 30) ~ "Y",
    
    VISIT == "2 Years" & DIED_BEFORE_WINDOW != "Y" & days_since_procedure < (30.5 * 24 - 45) ~ "Y",
    
    VISIT %in% c("30 Days", "6 Months", "1 Year", "2 Years") ~ "N",
    
    TRUE ~ NA_character_
  ))

test <- subset(adsv, adsv$VISIT == "30 Days")





