library(dplyr)
library(tidyr)
library(flextable)
library(officer)



source("C:/Users/luke_hall/OneDrive - Edwards Lifesciences/lhall/TWIST_EFS_DSMB_2025NOV/programs/makedata/mk_adsl.R")
source("C:/Users/luke_hall/OneDrive - Edwards Lifesciences/lhall/TWIST_EFS_DSMB_2025NOV/programs/makedata/mk_adecho.R")
source("C:/Users/luke_hall/OneDrive - Edwards Lifesciences/lhall/TWIST_EFS_DSMB_2025NOV/programs/makedata/mk_adsv.R")

implanted <- subset(adsl, adsl$ImplantedFl == "Y")

# get denoms
denom <- adsv %>%
  filter(Subject %in% implanted$Subject,
         !is.na(SVDAT))

denom_baseline <- denom %>%
  filter(VISIT == "Screening/Baseline") %>%
  nrow()

denom_discharge <- denom %>%
  filter(VISIT == "Discharge") %>%
  nrow()

denom_30D <- denom %>%
  filter(VISIT == "30 Days") %>%
  nrow()

denom_6M <- denom %>%
  filter(VISIT == "6 Months") %>%
  nrow()

denom_1Y <- denom %>%
  filter(VISIT == "1 Year") %>%
  nrow()



mr <- adecho %>%
  filter(Subject %in% implanted$Subject,
         ANL01FL == "Y",
         AVISIT %in% c("Baseline", "Discharge", "30 Days", "6 Months", "1 Year"),
         AVALC %in% c("None/Trace", "Mild", "Mild-Moderate", "Moderate-Severe", "Severe"))




# Get all combinations of AVISIT and AVALC
summary_df <- mr %>%
  count(AVISIT, AVALC) %>%
  complete(AVISIT, AVALC, fill = list(n = 0)) %>%
  group_by(AVISIT) %>%
  mutate(
    total = sum(n),
    pct = ifelse(n == 0, 0, round(100 * n / total, 1)),
    cell = paste0(n, "/", total, " (", pct, "%)")
  ) %>%
  ungroup() %>%
  select(AVALC, AVISIT, cell) %>%
  pivot_wider(names_from = AVISIT, values_from = cell)


summary_df <- summary_df %>%
  select(AVALC, Baseline, Discharge, `30 Days`, `6 Months`, `1 Year`)

summary_df <- summary_df %>%
  mutate(AVALC = factor(AVALC, levels = c(
    "None/Trace",
    "Mild",
    "Mild-Moderate",
    "Moderate-Severe",
    "Severe"
  ))) %>%
  arrange(AVALC)



t14_MRgrade <- flextable(summary_df) %>%
  set_caption(caption = NULL) %>%
  set_header_labels(
    AVALC = "",  # First column label
    Discharge = paste0("Discharge \n (N=", denom_discharge, ")"),
    Baseline = paste0("Baseline \n (N=", denom_baseline, ")"),
    `30 Days` = paste0("30 Days \n (N=", denom_30D, ")"),
    `6 Months` = paste0("6 Months \n (N=", denom_6M, ")"),
    `1 Year` = paste0("1 Year \n (N=", denom_1Y, ")")
  ) %>%
  autofit() %>%
  font(fontname = "Calibri", part = "all") %>%
  fontsize(size = 11, part = "all") %>%
  align(align = "center", part = "body") %>%
  align(j = 1, align = "left", part = "body") %>%
  align(j = 2:6, align = "center", part = "header") %>%
  bold(i = 1, part = "header") %>%
  bg(i = 1, bg = "#D3D3D3", part = "header") %>%
  border(part = "all", border = fp_border(color = "black", width = 0.5)) %>%
  add_footer_lines(c(
    "Baseline column reports cumulative MR grade, while all other columns report transvalvular MR grade",
    "Categorical measures (%)",
    program_info
  )) %>%
  font(fontname = "Calibri", part = "footer") %>%
  fontsize(size = 11, part = "footer") %>%
  padding(part = "footer", padding.top = 1, padding.bottom = 1) %>%
  border_outer(part = "footer") %>%
  fix_border_issues()

t14_MRgrade
