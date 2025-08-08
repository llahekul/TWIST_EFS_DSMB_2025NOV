library(knitr)
library(dplyr)
library(tibble)
library(flextable)
library(tinytex)
library(officer)
library(stringr)
library(lubridate)
library(ggplot2)
library(lubridate)
library(stringr)
library(showtext)
library(patchwork)

extract_date <- "2025JUL28"
program_name <- "t2_patientdisposition.R"
program_info <- paste0("Source: ", program_name, " Extract Date: ", extract_date, 
                       " Run Date (Time): ", format(Sys.time(), "%d%b%Y (%H:%M)"))


source("C:/Users/luke_hall/OneDrive - Edwards Lifesciences/lhall/TWIST_EFS_DSMB_2025NOV/programs/makedata/mk_adsv.R")

total_patients <- dplyr::n_distinct(adsv$Subject[adsv$VISIT == "Screening/Baseline"])
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
                          round(100 * sum(VISIT_NOT_PERFORMED == "Y", na.rm = TRUE) / total_patients, 1), "%)"),
    Ineligible   = paste0(sum(ELIGIBLE_FL == "N", na.rm = TRUE), "/", total_patients, " (",
                          round(100 * sum(ELIGIBLE_FL == "N", na.rm = TRUE) / total_patients, 1), "%)"),
    VisitNotDue  = paste0(sum(VISIT_NOT_DUE == "Y", na.rm = TRUE), "/", total_patients, " (",
                          round(100 * sum(VISIT_NOT_DUE == "Y", na.rm = TRUE) / total_patients, 1), "%)"),
    DiedBeforeWindow = paste0(sum(DIED_BEFORE_WINDOW == "Y", na.rm = TRUE), "/", total_patients, " (",
                              round(100 * sum(DIED_BEFORE_WINDOW == "Y", na.rm = TRUE) / total_patients, 1), "%)"),
    PendingVisitWithinWindow = paste0(sum(PENDING_VISIT_WITHIN_WINDOW == "Y", na.rm = TRUE), "/", total_patients, " (",
                                      round(100 * sum(PENDING_VISIT_WITHIN_WINDOW == "Y", na.rm = TRUE) / total_patients, 1), "%)"),
    WithdrewBeforeWindow = paste0(sum(WITHDREW_BEFORE_WINDOW == "Y", na.rm = TRUE), "/", total_patients, " (",
                                  round(100 * sum(WITHDREW_BEFORE_WINDOW == "Y", na.rm = TRUE) / total_patients, 1), "%)")
  ) %>%
  column_to_rownames("VISIT") %>%
  t() %>%
  as.data.frame()



visit_summary <- visit_summary[, c("30 Days", "6 Months", "1 Year", "2 Years")]

visit_summary <- visit_summary[c(
  "Eligible",
  "WithinWindow",
  "OutsideWindow",
  "NotPerformed",
  "PendingVisitWithinWindow",
  "Ineligible",
  "VisitNotDue",
  "DiedBeforeWindow",
  "WithdrewBeforeWindow"
), ]


visit_df <- visit_summary %>%
  mutate(`Patient Status at Follow-Up` = c(
    "Eligible for Visit\u00B9",
    "Visit Completed Within Window",
    "Visit Completed Outside Window",
    "Visit Not Performed (Reasons Below)",
    "Pending Visit Within Window",
    "Ineligible for Visit",
    "Visit Not Due",
    "Died Before Window",
    "Withdrew Before Visit Window"
  )) %>%
  select(`Patient Status at Follow-Up`, everything())




t2_patientdisposition <- flextable(visit_df) %>%
  set_caption(caption = NULL) %>%
  autofit() %>%
  font(fontname = "Calibri", part = "all") %>%
  fontsize(size = 11, part = "all") %>%
  align(align = "left", part = "body") %>%
  align(align = "center", part = "header") %>%
  bold(i = 1, part = "header") %>%
  bg(i = 1, bg = "#D3D3D3", part = "header") %>%
  
  padding(i = 2:4, j = 1:ncol(visit_df), padding.left = 40) %>%
  padding(i = 5, j = 1:ncol(visit_df), padding.left = 65) %>%
  padding(i = 7:9, j = 1:ncol(visit_df), padding.left = 40) %>%
  
  border(part = "all", border = fp_border(color = "grey70", width = 1)) %>%
  width(j = 1, width = 2.25) %>%
  width(j = 2:5, width = 1.75) %>%
  add_footer_lines(c(
    "[1] Patients are eligible if they complete the visit or their visit window is open and prior to FU visit window , they (a) are alive , (b) are not explanted,  (c) did not withdraw from study,  (d) are not lost to FU.",
    "Visit Windows: 30 Days FU window (23-37 days), 6 FU window (166-194 days), 1 year FU window (335-390 days), 2 year FU window (680-765 days)",
    "Categorical measures: n/Total N (%)",
    program_info
  )) %>%
  font(fontname = "Calibri", part = "footer") %>% 
  fontsize(size = 11, part = "footer") %>%
  padding(part = "footer", padding.top = 1, padding.bottom = 1) %>% 
  border_outer(part = "footer", border = fp_border(color = "grey70", width = 1)) %>%
  fix_border_issues()


t2_patientdisposition


