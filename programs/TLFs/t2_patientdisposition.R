####################################################################################################
#  PROGRAM: 		      
#
#  PURPOSE:         create ADSV dataset
#
#  INPUT:           pro.pr2
#                   pro.cv_sb1
#                   pro.cv_sb2
#
#  OUTPUT:          
#
#  MACROS USED:	   none
#  AUTHOR:          Luke Hall
#  CREATION DATE:   06/17/2025
#
#  NOTES:           none
#  MODIFICATIONS:   none
####################################################################################################
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
                   "1 Month Follow-Up" = "30 Days",
                   "1 Year Follow-Up" = "1 Year",
                   "2 Year Follow-Up" = "2 Years",
                   "3 Month Follow-Up" = "3 Months",
                   "6 Month Follow-Up" = "6 Months",
                   "Discharge" = "Discharge",
                   "Screening/Baseline" = "Screening/Baseline"
    ),
    VISITNUM = recode(FolderName,
                      "1 Month Follow-Up" = 3,
                      "1 Year Follow-Up" = 7,
                      "2 Year Follow-Up" = 6,
                      "3 Month Follow-Up" = 4,
                      "6 Month Follow-Up" = 5,
                      "Discharge" = 2,
                      "Screening/Baseline" = 1
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
# Step 1: Create subject-by-visit grid
subjects <- unique(adsv$Subject)
visits <- unique(adsv$VISIT)

phantom_grid <- expand.grid(Subject = subjects, VISIT = visits)

# Step 2: Merge and assign DTYPE
adsv <- phantom_grid %>%
  left_join(adsv, by = c("Subject", "VISIT")) %>%
  mutate(
    DTYPE = if_else(is.na(SVDAT), "PHANTOM", NA_character_),
    
    VISITNUM = case_when(
      VISIT == "Screening/Baseline" ~ 1,
      VISIT == "Discharge"          ~ 2,
      VISIT == "30 Days"            ~ 3,
      VISIT == "3 Months"            ~ 4,
      VISIT == "6 Months"            ~ 5,
      VISIT == "2 Years"             ~ 6,
      VISIT == "1 Year"             ~ 7,
      TRUE ~ NA_real_  # Handle any unexpected or future visits
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



# create VISITDY = days from procedure_dt
adsv <- adsv %>%
  mutate(VISITDY = as.numeric(difftime(SVDAT, procedure_dt, units = "days"))) %>%
  arrange(Subject, VISITNUM)


# create variable showing days from procedure to today
adsv <- adsv %>%
  mutate(days_since_procedure = as.numeric(difftime(Sys.Date(), procedure_dt, units = "days")))

# create eligible for visit flag 
# **** ADD IN DEATHS / EXPLANTS / WITHDRAWALS LATER
adsv <- adsv %>%
  mutate(ELIGIBLE_FL = case_when(
    VISIT == "30 Days" & !is.na(days_since_procedure) &
      days_since_procedure >= (30.5 * 1 - 7) ~ "Y",
    
    VISIT == "6 Months" & !is.na(days_since_procedure) &
      days_since_procedure >= (30.5 * 6 - 14) ~ "Y",
    
    VISIT == "1 Year" & !is.na(days_since_procedure) &
      days_since_procedure >= (30.5 * 12 - 30) ~ "Y",
    
    VISIT == "2 Years" & !is.na(days_since_procedure) &
      days_since_procedure >= (30.5 * 24 - 45) ~ "Y",
    
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
    
    TRUE ~ NA_character_
  ))

# create VISIT_NOT_PERFORMED flag
adsv <- adsv %>%
  mutate(VISIT_NOT_PERFORMED = case_when(
    VISIT %in% c("30 Days", "6 Months", "1 Year", "2 Years") &
      is.na(SVDAT) ~ "Y",
    VISIT %in% c("30 Days", "6 Months", "1 Year", "2 Years") ~ "N",
    TRUE ~ NA_character_
  ))


test <- adsv %>%
  filter(VISIT %in% c("30 Days", "6 Months", "1 Year", "2 Years"))




# table 2 in fall DSMB report ----
library(dplyr)
library(knitr)


# Step 1: Get denominator
total_patients <- dplyr::n_distinct(adsv$Subject[adsv$VISIT == "Screening/Baseline"])

# Transpose and clean row names
visit_summary <- adsv %>%
  filter(VISIT %in% c("30 Days", "6 Months", "1 Year", "2 Years")) %>%
  group_by(VISIT) %>%
  summarise(
    Eligible     = paste0(sum(ELIGIBLE_FL == "Y", na.rm = TRUE), "/", total_patients, " (",
                          round(100 * sum(ELIGIBLE_FL == "Y", na.rm = TRUE) / total_patients, 1), "%)"),
    WithinWindow = paste0(sum(CRIT01FL == "Y", na.rm = TRUE), "/", total_patients, " (",
                          round(100 * sum(CRIT01FL == "Y", na.rm = TRUE) / total_patients, 1), "%)"),
    OutsideWindow= paste0(sum(CRIT02FL == "Y", na.rm = TRUE), "/", total_patients, " (",
                          round(100 * sum(CRIT02FL == "Y", na.rm = TRUE) / total_patients, 1), "%)"),
    NotPerformed = paste0(sum(VISIT_NOT_PERFORMED == "Y", na.rm = TRUE), "/", total_patients, " (",
                          round(100 * sum(VISIT_NOT_PERFORMED == "Y", na.rm = TRUE) / total_patients, 1), "%)")
  ) %>%
  column_to_rownames("VISIT") %>%
  t() %>%
  as.data.frame()

# Reorder columns and drop row names
visit_summary <- visit_summary[, c("30 Days", "6 Months", "1 Year", "2 Years")]
rownames(visit_summary) <- NULL

# Add final row labels
visit_df <- visit_summary %>%
  mutate(`Patient Status at Follow-Up` = c(
    "Eligible for Visit",
    "Visit Completed Within Window",
    "Visit Completed Outside Window",
    "Visit Not Performed"
  )) %>%
  select(`Patient Status at Follow-Up`, everything())

#extract_date = "09JUL2025"
#program_name = basename(rstudioapi::getSourceEditorContext()$path)


library(flextable)

#ft_table2 <- flextable(visit_df) %>%
#  set_caption(caption = NULL) %>%
#  autofit() %>%
#  font(fontname = "Calibri", part = "all") %>%
#  fontsize(size = 11, part = "all") %>%
#  align(align = "left", part = "all") %>%
#  bold(i = 1, part = "header") %>%
#  bg(i = 1, bg = "#D3D3D3", part = "header") %>%
#  padding(i = 2:4, j = 1:5, padding.left = 40) %>%
#  border(part = "all", border = fp_border(color = "grey70", width = 1)) %>%
#  width(j = 1, width = 2.5) %>%    # Widen "Patient Status at Follow-Up"
#  width(j = 2:5, width = 2.5)      # Widen each follow-up column

extract_date <- "09JUL2025"
program_name <- "t1_enrollmentbysite.R"
program_info <- paste0("Source: ", program_name, " Extract Date: ", extract_date, 
                       " Run Date (Time): ", format(Sys.time(), "%d%b%Y (%H:%M)"))

t2_patientdisposition <- flextable(visit_df) %>%
  set_caption(caption = NULL) %>%
  autofit() %>%
  font(fontname = "Calibri", part = "all") %>%
  fontsize(size = 11, part = "all") %>%
  align(align = "left", part = "all") %>%
  bold(i = 1, part = "header") %>%
  bg(i = 1, bg = "#D3D3D3", part = "header") %>%
  padding(i = 2:4, j = 1:5, padding.left = 40) %>%
  border(part = "all", border = officer::fp_border(color = "grey70", width = 1)) %>%
  width(j = 1, width = 2.25) %>%
  width(j = 2:5, width = 1.75) %>%
  add_footer_lines(c(
    "[1] Patients are eligible if they complete the visit or their visit window is open and prior to FU visit window , they (a) are alive , (b) are not explanted,  (c) did not withdraw from study,  (d) are not lost to FU.",
    "[2] Specify the visit window: 30 Days FU window (23-37 days), 6 FU window (166-194 days), 1 year FU window (335-390 days).",
    "Categorical measures: %",
    program_info
  )) %>%
  font(fontname = "Calibri", part = "footer") %>%
  fontsize(size = 11, part = "footer") %>%
  border_outer(part = "footer") %>%
  fix_border_issues()

 
t2_patientdisposition





