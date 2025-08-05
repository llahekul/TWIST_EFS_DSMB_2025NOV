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

extract_date <- "2025JUL28"
program_name <- "t8_CEC_MAEs.R"
program_info <- paste0("Source: ", program_name, " Extract Date: ", extract_date, 
                       " Run Date (Time): ", format(Sys.time(), "%d%b%Y (%H:%M)"))

source("C:/Users/luke_hall/OneDrive - Edwards Lifesciences/lhall/TWIST_EFS_DSMB_2025NOV/programs/makedata/mk_adsl.R")
source("C:/Users/luke_hall/OneDrive - Edwards Lifesciences/lhall/TWIST_EFS_DSMB_2025NOV/programs/makedata/mk_adcec.R")
source("C:/Users/luke_hall/OneDrive - Edwards Lifesciences/lhall/TWIST_EFS_DSMB_2025NOV/programs/makedata/mk_adststat.R")


# Define denominators
denom_all <- nrow(adststat)
denom_30plus <- nrow(adststat[adststat$AVAL >= 30, ])

# Count unique patients per PARAM and flag
count_patients_by_flag <- function(data, flag) {
  data %>%
    filter(.data[[flag]] == "Y") %>%
    distinct(PARAM, Subject) %>%
    count(PARAM, name = "n_patients")
}

# Build event summary
event_summary <- adcec %>%
  group_by(PARAM) %>%
  summarise(
    `<=30_NoEvents`   = sum(ANL01FL == "Y", na.rm = TRUE),
    `>30_NoEvents`    = sum(ANL02FL == "Y", na.rm = TRUE),
    `Total_NoEvents`  = sum(ANL03FL == "Y", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(count_patients_by_flag(adcec, "ANL01FL") %>% rename(`<=30_Patients_raw` = n_patients), by = "PARAM") %>%
  left_join(count_patients_by_flag(adcec, "ANL02FL") %>% rename(`>30_Patients_raw` = n_patients), by = "PARAM") %>%
  left_join(count_patients_by_flag(adcec, "ANL03FL") %>% rename(`Total_Patients_raw` = n_patients), by = "PARAM") %>%
  mutate(
    across(ends_with("_raw"), ~replace_na(., 0)),
    `<=30_Patients`  = paste0(`<=30_Patients_raw`, "/", denom_all, " (", sprintf("%.1f", `<=30_Patients_raw` / denom_all * 100), "%)"),
    `>30_Patients`   = paste0(`>30_Patients_raw`, "/", denom_30plus, " (", sprintf("%.1f", `>30_Patients_raw` / denom_30plus * 100), "%)"),
    `Total_Patients` = paste0(`Total_Patients_raw`, "/", denom_all, " (", sprintf("%.1f", `Total_Patients_raw` / denom_all * 100), "%)")
  ) %>%
  select(PARAM,
         `<=30_NoEvents`, `<=30_Patients`,
         `>30_NoEvents`,  `>30_Patients`,
         `Total_NoEvents`, `Total_Patients`)

# Create a zero-filled row
manual_row <- tibble(
  PARAM             = "Myocardial Infarction or Coronary Ischemia Requiring PCI or CABG",
  `<=30_NoEvents`   = 0,
  `<=30_Patients`   = paste0("0/", denom_all, " (0.0%)"),
  `>30_NoEvents`    = 0,
  `>30_Patients`    = paste0("0/", denom_30plus, " (0.0%)"),
  `Total_NoEvents`  = 0,
  `Total_Patients`  = paste0("0/", denom_30plus, " (0.0%)")
)

# Bind with your summary table
event_summary <- bind_rows(event_summary, manual_row)

event_summary <- event_summary %>%
  mutate(PARAM = ifelse(grepl("Bleeding", PARAM), gsub("Bleeding", "Bleeding\u00B9", PARAM), PARAM))

# Reorder rows based on updated PARAM list
param_order <- c(
  "Death",
  "Stroke",
  "Bleeding\u00B9",
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





# Format flextable
t8 <- flextable(event_summary) %>%
  set_caption(caption = NULL) %>%
  set_header_labels(
    PARAM             = "Event",
    `<=30_NoEvents`   = "No. Events",
    `<=30_Patients`   = "Patients",
    `>30_NoEvents`    = "No. Events",
    `>30_Patients`    = "Patients",
    `Total_NoEvents`  = "No. Events",
    `Total_Patients`  = "Patients"
  ) %>%
  add_header_row(
    values = c(" ", "Early Events \n (\u2264 30 Days)", "Late Events \n (> 30 days to 1 Year)", "Total Events"),
    colwidths = c(1, 2, 2, 2)
  ) %>%
  autofit() %>%
  font(fontname = "Calibri", part = "all") %>%
  fontsize(size = 11, part = "all") %>%
  align(align = "center", part = "header") %>%
  align(j = 1, align = "left", part = "body") %>%
  align(j = 2:7, align = "center", part = "body") %>%
  bold(i = 1:2, part = "header") %>%
  bg(i = 1:2, bg = "#D3D3D3", part = "header") %>%
  border_remove() %>%
  border_outer(part = "all", border = fp_border(color = "black", width = 1)) %>%
  border(i = NULL, j = 1, border.right = fp_border(color = "black", width = 1)) %>%
  border(i = NULL, j = 3, border.right = fp_border(color = "black", width = 1)) %>%
  border(i = NULL, j = 5, border.right = fp_border(color = "black", width = 1)) %>%
  # ✅ Extend vertical lines into second header row
  border(i = 1:2, j = 1, part = "header", border.right = fp_border(color = "black", width = 1)) %>%
  border(i = 1:2, j = 3, part = "header", border.right = fp_border(color = "black", width = 1)) %>%
  border(i = 1:2, j = 5, part = "header", border.right = fp_border(color = "black", width = 1)) %>%
  border(i = 2,   j = 2:7, part = "header", border.top = fp_border(color = "black", width = 1)) %>%

  width(j = 1, width = 4.5) %>%
  width(j = 2, width = 0.7) %>%
  width(j = 3, width = 1.1) %>%
  width(j = 4, width = 0.7) %>%
  width(j = 5, width = 1.1) %>%
  width(j = 6, width = 0.7) %>%
  width(j = 7, width = 1.1) %>%
  add_footer_lines(paste0(
    "[1] Includes fatal, life-threatening, extensive or major bleeding as defined by MVARC \n",
    "Categorical measures: n/N (%).\n",
    program_info
  )) %>%
  font(fontname = "Calibri", part = "footer") %>%
  fontsize(size = 11, part = "footer") %>%
  padding(part = "footer", padding.top = 1, padding.bottom = 1) %>%
  border_outer(part = "footer") %>%
  fix_border_issues()

t8
