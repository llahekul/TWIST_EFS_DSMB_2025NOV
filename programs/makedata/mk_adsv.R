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
adsv$ANL01FL <- ifelse(!is.na(adsv$SVDAT), "Y", "N")

# calculates days from procedure date to visit
adsv <- adsv %>%
  mutate(VISITDY = as.numeric(difftime(SVDAT, procedure_dt, units = "days"))) %>%
  arrange(Subject, VISITNUM)

# add in USUBJID too
USUBJID <- pr2[c("Subject", "USUBJID")]
adsv <- left_join(adsv, USUBJID, by="Subject")

# potentially add some crit flags?
adsv


