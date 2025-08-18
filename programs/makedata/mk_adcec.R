# make BDS adam dataset for CEC-adjudicated events
{library(flextable)
  library(officer)
  library(dplyr)
  library(knitr)
  library(dplyr)
  library(tibble)
  library(stringr)}

# evens are as follows:
# bold composite column
# ACM
# stroke
# Myocardial infraction (MI)  
# All-cause Hospitalization
# Durable LVAD implant
# Heart transplant
# Renal complications requiring unplanned dialysis or renal replacement therapy
# Severe bleeding1 
# Non-elective mitral valve re-intervention, percutaneous or surgical
# Major access site and vascular complications
# Major cardiac structural complications 
# Device embolization



# load in ADSL
source("C:/Users/luke_hall/OneDrive - Edwards Lifesciences/lhall/TWIST_EFS_DSMB_2025NOV/programs/makedata/mk_adsl.R")
enrolled <- subset(adsl, adsl$EnrolledFl=="Y")



cec <- fa_cec %>%
  filter(Subject %in% enrolled$Subject) %>%
  select(
    Subject,
    FADAT_NEW,
    FAORRES_DTHTYP,           # death type (cardio vs. non-cardio)
    FAORRES_STTIA,            # stroke
    FAORRES_BLEEDSEV,         # life-threatening (or fatal) bleeding
    FAORRES_BLEED,
    FAORRES_CVCM,             # vascular complications
    FAORRES_HOSP,             # Hospitalization
    FAORRES_MJCVCM_SRGRQ,     # surgery required to repair?
    FAORRES_MJCARSCM,         # major cardiac structural complications
    FAORRES_MJCARSCM_SRGRQ,   # surgery required to repair?
    FAORRES_AKISTAGE_CEC,     # AKI
    FAORRES_AKISTAGE_CEC_SPEC,# AKI stage
    FAORRES_MI,               # myocardial infarction
    FAORRES_CISREINT,
    FAORRES_CRDGSHK,          # cardiogenic shock
    FAORRES_CMRQSRGI
  ) %>%
  mutate(
    PARAM = case_when(
      nzchar(FAORRES_DTHTYP)    ~ "Death",
      nzchar(FAORRES_STTIA)     ~ "Stroke",
      FAORRES_MI == "Yes"       ~ "Myocardial infarction", 
      FAORRES_MI == "Yes"       ~ "Myocardial infarction",
      FAORRES_BLEED == "Yes"    ~ "Bleed",
      TRUE                      ~ ""
    ),
    PARAMCD = case_when(
      nzchar(FAORRES_DTHTYP)    ~ "DEATH",
      nzchar(FAORRES_STTIA)     ~ "STROKE",
      FAORRES_MI == "Yes"       ~ "MI",
      FAORRES_BLEED == "Yes"    ~ "BLEED",
      TRUE                      ~ ""
    ),
    AVALC = case_when(
      PARAM == "Bleed" ~ FAORRES_BLEEDSEV,
      TRUE             ~ ""
    )
  ) %>%
  rename(ADT = FADAT_NEW)
  #select(-starts_with("FAORRES_"))

# death ----
deaths <- fa_cec %>%
  filter(Subject %in% enrolled$Subject) %>%
  filter(nzchar(FAORRES_DTHTYP)) %>%
  select(
    Subject,
    FADAT_NEW,
    FAORRES_DTHTYP 
  ) %>%
  mutate(
    PARAM   = "Death",
    PARAMCD = "DEATH",
    AVALC   = ""
  ) %>%
  rename(ADT = FADAT_NEW) %>%
  select(-starts_with("FAORRES_"))

# stroke ----
strokes <- fa_cec %>%
  filter(Subject %in% enrolled$Subject) %>%
  filter(nzchar(FAORRES_STTIA)) %>%
  select(
    Subject,
    FADAT_NEW,
    FAORRES_STTIA 
  ) %>%
  mutate(
    PARAM   = "Stroke",
    PARAMCD = "STROKE",
    AVALC    = ""
  ) %>%
  rename(ADT = FADAT_NEW) %>%
  select(-starts_with("FAORRES_"))
# all-cause hospitalization ----
ach <- fa_cec %>%
  filter(Subject %in% enrolled$Subject) %>%
  filter(FAORRES_HOSP == "Yes") %>%
  select(
    Subject,
    FADAT_NEW,
    FAORRES_HOSP 
  ) %>%
  mutate(
    PARAM   = "All-Cause Hospitalization",
    PARAMCD = "ACH",
    AVALC    = ""
  ) %>%
  rename(ADT = FADAT_NEW) %>%
  select(-starts_with("FAORRES_"))
# bleeding ----
bleed <- fa_cec %>%
  filter(Subject %in% enrolled$Subject) %>%
  filter(FAORRES_BLEED == "Yes") %>%
  select(
    Subject,
    FADAT_NEW,
    FAORRES_BLEED,
    FAORRES_BLEEDSEV
  ) %>%
  mutate(
    PARAM   = "Bleeding",
    PARAMCD = "BLEED",
    AVALC    = FAORRES_BLEEDSEV
  ) %>%
  rename(ADT = FADAT_NEW) %>%
  select(-starts_with("FAORRES_"))

bleed <- subset(bleed, bleed$AVALC != "Minor bleed")

# major vascular complications requiring surgery to repair ----
vasc <- fa_cec %>%
  filter(Subject %in% enrolled$Subject) %>%
  filter(nzchar(FAORRES_CVCM) & 
           FAORRES_MJCVCM_SRGRQ == "Yes (Please complete adjudication form for complications requiring surgery or repeat intervention)") %>%
  select(
    Subject,
    FADAT_NEW,
    FAORRES_CVCM,
    FAORRES_MJCVCM_SRGRQ
  ) %>%
  mutate(
    PARAM   = "Major Vascular Complications Requiring Surgery to Repair",
    PARAMCD = "VASCULAR",
    AVALC    = ""
  ) %>%
  rename(ADT = FADAT_NEW) %>%
  select(-starts_with("FAORRES_"))
# major cardiac complications requiring surgery to repair ----
cardiac_comp <- fa_cec %>%
  filter(Subject %in% enrolled$Subject) %>%
  filter(nzchar(FAORRES_MJCARSCM) & 
           FAORRES_MJCARSCM_SRGRQ == "Yes (Please complete adjudication form for complications requiring surgery or repeat intervention)") %>%
  select(
    Subject,
    FADAT_NEW,
    FAORRES_MJCARSCM,
    FAORRES_MJCARSCM_SRGRQ
  ) %>%
  mutate(
    PARAM   = "Major Cardiac Structural Complications Requiring Surgery to Repair",
    PARAMCD = "CARDIAC",
    AVALC    = ""
  ) %>%
  rename(ADT = FADAT_NEW) %>%
  select(-starts_with("FAORRES_"))
# stage 2 or 3 AKI (including new dialysis) ----
aki <- fa_cec %>%
  filter(Subject %in% enrolled$Subject) %>%
  filter(FAORRES_AKISTAGE_CEC == "Yes" & 
           FAORRES_AKISTAGE_CEC_SPEC %in% c("Stage 2", "Stage 3", "New dialysis")) %>%
  select(
    Subject,
    FADAT_NEW,
    FAORRES_AKISTAGE_CEC,
    FAORRES_AKISTAGE_CEC_SPEC
  ) %>%
  mutate(
    PARAM   = "Stage 2 or 3 Acute Kidney Injury (Including New Dialysis)",
    PARAMCD = "AKI",
    AVALC    = FAORRES_AKISTAGE_CEC_SPEC
  ) %>%
  rename(ADT = FADAT_NEW) %>%
  select(-starts_with("FAORRES_"))

# myocardial infarction ----
mi <- fa_cec %>%
  filter(Subject %in% enrolled$Subject) %>%
  filter(FAORRES_MI == "Yes") %>%
  select(
    Subject,
    FADAT_NEW,
    FAORRES_MI, 
    FAORRES_CISREINT
  ) %>%
  mutate(
    PARAM   = "Myocardial Infarction or Coronary Ischemia Requiring PCI or CABG",
    PARAMCD = "MI",
    AVALC    = ""
  ) %>%
  rename(ADT = FADAT_NEW) %>%
  select(-starts_with("FAORRES_"))

# unexpected cardiogenic shock ----
card_shock <- fa_cec %>%
  filter(Subject %in% enrolled$Subject) %>%
  filter(FAORRES_CRDGSHK == "Yes") %>%
  select(
    Subject,
    FADAT_NEW,
    FAORRES_CRDGSHK
  ) %>%
  mutate(
    PARAM   = "Unexpected Cardiogenic Shock Requiring ICU Admission and Treatment",
    PARAMCD = "SHOCK",
    AVALC    = ""
  ) %>%
  rename(ADT = FADAT_NEW) %>%
  select(-starts_with("FAORRES_"))
# any Valve-Related Dysfunction, Migration, Thrombosis, or Other Complications Requiring Surgery or Repeat Interventions ----
any <- fa_cec %>%
  filter(Subject %in% enrolled$Subject) %>%
  filter(FAORRES_CMRQSRGI == "Yes") %>%
  select(
    Subject,
    FADAT_NEW,
    FAORRES_CMRQSRGI
  ) %>%
  mutate(
    PARAM   = "Any Valve-Related Dysfunction, Migration, Thrombosis, or Other Complications Requiring Surgery or Repeat Interventions",
    PARAMCD = "VALVE",
    AVALC    = ""
  ) %>%
  rename(ADT = FADAT_NEW) %>%
  select(-starts_with("FAORRES_"))



  
# put all params together ----
adcec <- rbind(deaths, strokes, ach, bleed, vasc, cardiac_comp, aki, card_shock, any)


# create composite param
#comp_rows <- adcec %>%
#  group_by(Subject) %>%                         # group by participant
#  summarise(ADT = min(ADT, na.rm = TRUE),       # get earliest event date
#            .groups = "drop") %>%               # remove grouping
#  mutate(PARAM = "Composite event", 
#          PARAMCD = "COMP",
#            AVALC = "")                      # assign composite PARAMCD
#adcec <- rbind(adcec, comp_rows)

# create ANL01FL = "Y" if event happened in first 30 days; 
# ANL02FL = "Y" if event happened before 6 months;
# ANL02FL = "Y" if event happened before 1 yr;
# ANL02FL = "Y" if event happened before 2 yr;
# ANL02FL = "Y" if event happened before 3 yr;
proc_dt <- adsl[c("Subject", "PRSTDAT")]
adcec <- left_join(adcec, proc_dt, by="Subject")

adcec <- adcec %>%
  mutate(
    days_diff = as.numeric(difftime(ADT, PRSTDAT, units = "days")),  # Calculate difference in days
    
    ANL01FL = if_else(days_diff <= 30, "Y", NA_character_),
    ANL02FL = if_else(days_diff <= 30.5*6, "Y", NA_character_),
    ANL03FL = if_else(days_diff <= 365, "Y", NA_character_),
    ANL04FL = if_else(days_diff <= 720, "Y", NA_character_),
    ANL05FL = if_else(days_diff <= 1095, "Y", NA_character_)
    
  ) %>%
  select(-PRSTDAT)






