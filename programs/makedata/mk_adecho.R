# make BDS adam dataset for ECHO measurments

source("C:/Users/luke_hall/OneDrive - Edwards Lifesciences/lhall/TWIST_EFS_DSMB_2025NOV/programs/makedata/mk_adsl.R")
enrolled <- subset(adsl, adsl$EnrolledFl=="Y")

# baseline
mr_base <- cv_sb1 %>%
  select(Subject, CVTESTCD, CVORRES_MVLREGTS, CVDAT) %>%
  filter(CVTESTCD == "TTE",
         Subject %in% enrolled$Subject) %>%
  mutate(PARAM = "MR grade (TTE)",
         PARAMCD = "MRGRADE",
         AVISIT = "Baseline") %>%
  rename(ADT = CVDAT,
         AVALC = CVORRES_MVLREGTS) %>%
  select(-starts_with("CV"))


# follow-up
mr_fu <- cv_fu2 %>%
  select(Subject, CVTESTCD, CVORRES_TRNSVMR, CVDAT, FolderName) %>%
  filter(CVTESTCD == "TTE",
         Subject %in% enrolled$Subject) %>%
  mutate(PARAM = "MR grade (TTE)",
         PARAMCD = "MRGRADE") %>%
  rename(ADT = CVDAT,
         AVALC = CVORRES_TRNSVMR,
         AVISIT = FolderName) %>%
  select(-starts_with("CV"))

adecho <- rbind(mr_base, mr_fu)

# changes moderate->mild-moderate and flags latest observation
adecho <- adecho %>%
  mutate(AVALC = if_else(AVALC == "Moderate", "Mild-Moderate", AVALC)) %>%
  group_by(Subject, AVISIT) %>%
  mutate(ANL01FL = if_else(ADT == max(ADT, na.rm = TRUE), "Y", NA_character_)) %>%
  ungroup() %>%
  mutate(AVISIT = case_when(
    AVISIT == "1 Month Follow-Up - ECHO" ~ "30 Days",
    AVISIT == "1 Year Follow-Up - ECHO" ~ "1 Year",
    AVISIT == "2 Year Follow-Up - ECHO" ~ "2 Years",
    AVISIT == "3 Month Follow-Up - ECHO" ~ "3 Months",
    AVISIT == "6 Month Follow-Up - ECHO" ~ "6 Months",
    AVISIT == "Discharge - ECHO" ~ "Discharge",
    AVISIT == "Baseline" ~ "Baseline",
    AVISIT == "Unscheduled - ECHO" ~ "Unscheduled Visit",
    TRUE ~ AVISIT  # fallback for unmapped values
  ))

  
  
  
  
  




