# Table 8 - CEC Adjudicated Events (early and late)
{library(flextable)
  library(officer)
  library(dplyr)
  library(knitr)
  library(dplyr)
  library(tibble)
  library(stringr)
  library(tidyr)
}

extract_date <- "2025AUG06"
program_name <- "t8_CEC_adj.R"
program_info <- paste0("Source: ", program_name, " Extract Date: ", extract_date, 
                       " Run Date (Time): ", format(Sys.time(), "%d%b%Y (%H:%M)"))

source("C:/Users/luke_hall/OneDrive - Edwards Lifesciences/lhall/TWIST_EFS_DSMB_2025NOV/programs/makedata/mk_adsl.R")
source("C:/Users/luke_hall/OneDrive - Edwards Lifesciences/lhall/TWIST_EFS_DSMB_2025NOV/programs/makedata/mk_adcec.R")
source("C:/Users/luke_hall/OneDrive - Edwards Lifesciences/lhall/TWIST_EFS_DSMB_2025NOV/programs/makedata/mk_adststat.R")
source("C:/Users/luke_hall/OneDrive - Edwards Lifesciences/lhall/TWIST_EFS_DSMB_2025NOV/programs/makedata/mk_adsv.R")


# Define denominators
denom_all <- nrow(adststat)
denom_30d <- nrow(subset(adsv, adsv$VISIT == "30 Days" & !is.na(adsv$SVDAT)))
denom_6m <- nrow(subset(adsv, adsv$VISIT == "6 Months" & !is.na(adsv$SVDAT)))
denom_1y <- nrow(subset(adsv, adsv$VISIT == "1 Year" & !is.na(adsv$SVDAT)))
denom_2y <- nrow(subset(adsv, adsv$VISIT == "2 Years" & !is.na(adsv$SVDAT)))

# Count unique patients per PARAM and flag
count_patients_by_flag <- function(data, flag) {
  data %>%
    filter(.data[[flag]] == "Y") %>%
    distinct(PARAM, Subject) %>%
    count(PARAM, name = "n_patients")
}

# Build event summary
# Build event summary
event_summary <- adcec %>%
  group_by(PARAM) %>%
  summarise(
    `30_days` = sum(ANL01FL == "Y", na.rm = TRUE),
    `6_mo`    = sum(ANL02FL == "Y", na.rm = TRUE),
    `1_yr`    = sum(ANL03FL == "Y", na.rm = TRUE),
    `2_yr`    = sum(ANL04FL == "Y", na.rm = TRUE),
    #`3_yr`    = sum(ANL05FL == "Y", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(count_patients_by_flag(adcec, "ANL01FL") %>% rename(`30_days_raw` = n_patients), by = "PARAM") %>%
  left_join(count_patients_by_flag(adcec, "ANL02FL") %>% rename(`6_mo_raw` = n_patients), by = "PARAM") %>%
  left_join(count_patients_by_flag(adcec, "ANL03FL") %>% rename(`1_yr_raw` = n_patients), by = "PARAM") %>%
  left_join(count_patients_by_flag(adcec, "ANL04FL") %>% rename(`2_yr_raw` = n_patients), by = "PARAM") %>%
  #left_join(count_patients_by_flag(adcec, "ANL05FL") %>% rename(`3_yr_raw` = n_patients), by = "PARAM") %>%
  mutate(
    across(ends_with("_raw"), ~replace_na(., 0)),
    `30_days_Patients` = paste0(`30_days_raw`, "/", denom_30d, " (", sprintf("%.1f", `30_days_raw` / denom_30d * 100), "%)"),
    `6_mo_Patients`    = paste0(`6_mo_raw`, "/", denom_6m, " (", sprintf("%.1f", `6_mo_raw` / denom_6m * 100), "%)"),
    `1_yr_Patients`    = paste0(`1_yr_raw`, "/", denom_1y, " (", sprintf("%.1f", `1_yr_raw` / denom_1y * 100), "%)"),
    `2_yr_Patients`    = paste0(`2_yr_raw`, "/", denom_2y, " (", sprintf("%.1f", `2_yr_raw` / denom_2y * 100), "%)"),
    #`3_yr_Patients`    = paste0(`3_yr_raw`, "/", denom_2y, " (", sprintf("%.1f", `3_yr_raw` / denom_2y * 100), "%)")
  ) %>%
  select(PARAM,
         `30_days`, `30_days_Patients`,
         `6_mo`,    `6_mo_Patients`,
         `1_yr`,    `1_yr_Patients`,
         `2_yr`,    `2_yr_Patients`)
         #`3_yr`,    `3_yr_Patients`)


# Create a zero-filled row aligned with updated column names
manual_row <- tibble(
  PARAM             = "Myocardial Infarction or Coronary Ischemia Requiring PCI or CABG",
  `30_days`         = 0,
  `30_days_Patients`= paste0("0/", denom_30d, " (0.0%)"),
  `6_mo`            = 0,
  `6_mo_Patients`   = paste0("0/", denom_6m, " (0.0%)"),
  `1_yr`            = 0,
  `1_yr_Patients`   = paste0("0/", denom_1y, " (0.0%)"),
  `2_yr`            = 0,
  `2_yr_Patients`   = paste0("0/", denom_2y, " (0.0%)")
  #`3_yr`            = 0,
  #`3_yr_Patients`   = paste0("0/", denom_all, " (0.0%)")
)


# Bind with your summary table
event_summary <- bind_rows(event_summary, manual_row)

bleeding_denoms <- adcec %>%
  filter(PARAM == "Bleeding") %>%
  summarise(
    denom_30d_bleed = n_distinct(Subject[ANL01FL == "Y"]),
    denom_6m_bleed  = n_distinct(Subject[ANL02FL == "Y"]),
    denom_1y_bleed  = n_distinct(Subject[ANL03FL == "Y"]),
    denom_2y_bleed  = n_distinct(Subject[ANL04FL == "Y"])
  )


bleeding_subsummary <- adcec %>%
  filter(PARAM == "Bleeding") %>%
  group_by(AVALC, Subject) %>%
  summarise(
    ANL01FL = any(ANL01FL == "Y"),
    ANL02FL = any(ANL02FL == "Y"),
    ANL03FL = any(ANL03FL == "Y"),
    ANL04FL = any(ANL04FL == "Y"),
    .groups = "drop"
  ) %>%
  group_by(AVALC) %>%
  summarise(
    `30_days_raw` = sum(ANL01FL, na.rm = TRUE),
    `6_mo_raw`    = sum(ANL02FL, na.rm = TRUE),
    `1_yr_raw`    = sum(ANL03FL, na.rm = TRUE),
    `2_yr_raw`    = sum(ANL04FL, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    PARAM = paste0(AVALC),
    `30_days` = `30_days_raw`,
    `6_mo`    = `6_mo_raw`,
    `1_yr`    = `1_yr_raw`,
    `2_yr`    = `2_yr_raw`,
    `30_days_Patients` = paste0(`30_days_raw`, "/", bleeding_denoms$denom_30d_bleed, " (", sprintf("%.1f", `30_days_raw` / bleeding_denoms$denom_30d_bleed * 100), "%)"),
    `6_mo_Patients`    = paste0(`6_mo_raw`, "/", bleeding_denoms$denom_6m_bleed,  " (", sprintf("%.1f", `6_mo_raw` / bleeding_denoms$denom_6m_bleed  * 100), "%)"),
    `1_yr_Patients`    = paste0(`1_yr_raw`, "/", bleeding_denoms$denom_1y_bleed,  " (", sprintf("%.1f", `1_yr_raw` / bleeding_denoms$denom_1y_bleed  * 100), "%)"),
    `2_yr_Patients`    = paste0(`2_yr_raw`, "/", bleeding_denoms$denom_2y_bleed,  " (", sprintf("%.1f", `2_yr_raw` / bleeding_denoms$denom_2y_bleed  * 100), "%)")
  ) %>%
  select(PARAM,
         `30_days`, `30_days_Patients`,
         `6_mo`,    `6_mo_Patients`,
         `1_yr`,    `1_yr_Patients`,
         `2_yr`,    `2_yr_Patients`)



bleeding_index <- which(event_summary$PARAM == "Bleeding")
event_summary <- bind_rows(
  event_summary[1:bleeding_index, ],
  bleeding_subsummary,
  event_summary[(bleeding_index + 1):nrow(event_summary), ]
)

event_summary <- event_summary %>%
  mutate(PARAM = case_when(
    PARAM == "" ~ "Missing",
    TRUE        ~ PARAM
  ))


#event_summary <- event_summary %>%
#  mutate(PARAM = ifelse(grepl("Bleeding", PARAM), gsub("Bleeding", "Bleeding\u00B9", PARAM), PARAM))

# Reorder rows based on updated PARAM list
param_order <- c(
  "Death",
  "Stroke",
  "Bleeding",
  "Major bleed",
  "Extensive bleed",
  "Fatal bleed",
  "Missing",
  "Major Vascular Complications Requiring Surgery to Repair",
  "Major Cardiac Structural Complications Requiring Surgery to Repair",
  "Stage 2 or 3 Acute Kidney Injury (Including New Dialysis)",
  "Myocardial Infarction or Coronary Ischemia Requiring PCI or CABG",  # 👈 Inserted here
  "Unexpected Cardiogenic Shock",
  "Any Valve-Related Dysfunction, Migration, Thrombosis, or Other Complications Requiring Surgery or Repeat Interventions"
)


# Apply custom row order again
event_summary <- event_summary %>%
  mutate(PARAM = factor(PARAM, levels = param_order)) %>%
  arrange(PARAM)



# composite row
# Total number of events per time window
# Composite event counts
composite_events <- adcec %>%
  summarise(
    `30_days` = sum(ANL01FL == "Y", na.rm = TRUE),
    `6_mo`    = sum(ANL02FL == "Y", na.rm = TRUE),
    `1_yr`    = sum(ANL03FL == "Y", na.rm = TRUE),
    `2_yr`    = sum(ANL04FL == "Y", na.rm = TRUE)
    #`3_yr`    = sum(ANL05FL == "Y", na.rm = TRUE)
  )

# Composite patient counts
composite_patients <- adcec %>%
  summarise(
    `30_days_raw` = n_distinct(Subject[ANL01FL == "Y"]),
    `6_mo_raw`    = n_distinct(Subject[ANL02FL == "Y"]),
    `1_yr_raw`    = n_distinct(Subject[ANL03FL == "Y"]),
    `2_yr_raw`    = n_distinct(Subject[ANL04FL == "Y"]),
    #`3_yr_raw`    = n_distinct(Subject[ANL05FL == "Y"])
  )

# Combine and format composite row
composite_row <- bind_cols(composite_events, composite_patients) %>%
  mutate(
    PARAM = "Composite MAE",
    `30_days_Patients` = paste0(`30_days_raw`, "/", denom_30d, " (", sprintf("%.1f", `30_days_raw` / denom_30d * 100), "%)"),
    `6_mo_Patients`    = paste0(`6_mo_raw`, "/", denom_6m, " (", sprintf("%.1f", `6_mo_raw` / denom_6m * 100), "%)"),
    `1_yr_Patients`    = paste0(`1_yr_raw`, "/", denom_1y, " (", sprintf("%.1f", `1_yr_raw` / denom_1y * 100), "%)"),
    `2_yr_Patients`    = paste0(`2_yr_raw`, "/", denom_2y, " (", sprintf("%.1f", `2_yr_raw` / denom_2y * 100), "%)")
    #`3_yr_Patients`    = paste0(`3_yr_raw`, "/", denom_2y, " (", sprintf("%.1f", `3_yr_raw` / denom_2y * 100), "%)")
  ) %>%
  select(PARAM,
         `30_days`, `30_days_Patients`,
         `6_mo`,    `6_mo_Patients`,
         `1_yr`,    `1_yr_Patients`,
         `2_yr`,    `2_yr_Patients`)
         #`3_yr`,    `3_yr_Patients`)

# Add composite row to event summary
event_summary <- bind_rows(composite_row, event_summary)




t8_CEC_adj <- flextable(event_summary) %>%
  set_caption(caption = NULL) %>%
  set_header_labels(
    PARAM             = "Event",
    `30_days`         = "No. Events",
    `30_days_Patients`= "Patients",
    `6_mo`            = "No. Events",
    `6_mo_Patients`   = "Patients",
    `1_yr`            = "No. Events",
    `1_yr_Patients`   = "Patients",
    `2_yr`            = "No. Events",
    `2_yr_Patients`   = "Patients"
    #`3_yr`            = "No. Events",
    #`3_yr_Patients`   = "Patients"
  ) %>%
  add_header_row(
    top = TRUE,
    values = c("Event", 
               "30 Days", 
               "6 Months", 
               "1 Year", 
               "2 Years"),
               #"3 Years"),
    colwidths = c(1, 2, 2, 2, 2)
  ) %>%
  
  autofit() %>%
  bold(i = 1, j = 1, part = "body") %>%
  font(fontname = "Calibri", part = "all") %>%
  fontsize(size = 11, part = "all") %>%
  align(align = "center", part = "header") %>%
  align(j = 1, align = "left", part = "body") %>%
  align(j = 2:9, align = "center", part = "body") %>%
  bold(i = 1:2, part = "header") %>%
  bg(i = 1:2, bg = "#D3D3D3", part = "header") %>%
  fix_border_issues() %>%
  
  # Remove horizontal lines between rows 3–7
  border(i = 3:7, j = 1:9, border.bottom = officer::fp_border(color = "transparent", width = 0), part = "body") %>%
  
  # Outer border for entire table
  border_outer(border = officer::fp_border(color = "grey70", width = 1)) %>%
  
  # Header grid lines
  border_inner_h(part = "header", border = officer::fp_border(color = "grey70", width = 1)) %>%
  border_inner_v(part = "header", border = officer::fp_border(color = "grey70", width = 1)) %>%
  
  # Vertical grid lines in body and header
  border(j = c(1, 3, 5, 7, 9), border.right = officer::fp_border(color = "grey70", width = 1), part = "body") %>%
  border(j = c(1, 3, 5, 7, 9), border.right = officer::fp_border(color = "grey70", width = 1), part = "header") %>%
  
  # Transparent borders for spacing
  border(i = 2, j = c(2, 4, 6, 8), border.right = officer::fp_border(color = "transparent", width = 1), part = "body") %>%
  
  padding(i = 5:8, j = 1:ncol(event_summary), padding.left = 40) %>%
  
  # Column widths
  width(j = 1, width = 3.5) %>%
  width(j = 2, width = 0.7) %>%
  width(j = 3, width = 1.1) %>%
  width(j = 4, width = 0.7) %>%
  width(j = 5, width = 1.1) %>%
  width(j = 6, width = 0.7) %>%
  width(j = 7, width = 1.1) %>%
  width(j = 8, width = 0.7) %>%
  width(j = 9, width = 1.1) %>%
  
  # Footer
  add_footer_lines(paste0(
    "Categorical measures: n/Total N (%)\n",
    program_info
  )) %>%
  font(fontname = "Calibri", part = "footer") %>%
  fontsize(size = 11, part = "footer") %>%
  padding(part = "footer", padding.top = 1, padding.bottom = 1) %>%
  border_outer(part = "footer", border = officer::fp_border(color = "grey70", width = 1)) %>%
  fix_border_issues()

# Merge the "Event" header cell across two rows
t8_CEC_adj <- merge_at(t8_CEC_adj, i = 1:2, j = 1, part = "header")

# Add vertical grid line between "Event" and first time window
t8_CEC_adj <- border(t8_CEC_adj, i = 1:2, j = 1, border.right = officer::fp_border(color = "grey70", width = 1), part = "header")

# Remove vertical lines between paired columns in second header row
t8_CEC_adj <- border(t8_CEC_adj, i = 2, j = c(2, 4, 6, 8), border.right = officer::fp_border(color = "transparent", width = 1), part = "header")

t8_CEC_adj
