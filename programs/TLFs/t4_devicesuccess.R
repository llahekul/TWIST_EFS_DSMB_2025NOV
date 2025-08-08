library(tibble)
library(flextable)
library(officer)

source("C:/Users/luke_hall/OneDrive - Edwards Lifesciences/lhall/TWIST_EFS_DSMB_2025NOV/programs/makedata/mk_adsl.R")

extract_date <- "08AUG2025"
program_name <- "t4_devicesuccess.R"
program_info <- paste0("Source: ", program_name, " Extract Date: ", extract_date, 
                       " Run Date (Time): ", format(Sys.time(), "%d%b%Y (%H:%M)"))

enrolled <- subset(adsl, adsl$EnrolledFl == "Y")
implanted <- subset(adsl, adsl$ImplantedFl == "Y")

n_attempted <- nrow(enrolled)   # Procedure attempted
n_successful <- nrow(implanted) # Procedure successful


summary_table <- tibble(
  Variable = c(
    "Device success\u00B9",
    "Device deployed as intended",
    "Device system successfully retrieved as intended"
  ),
  Statistic = rep(
    paste0(n_successful, "/", n_attempted, " (", sprintf("%.1f", n_successful / n_attempted * 100), "%)"),
    3
  )
)


t4_devicesuccess <- flextable(summary_table) %>%
  set_header_labels(
    Variable = "Device Outcome",
    Statistic = "Total"
  ) %>%
  width(j = 1, width = 4) %>%
  width(j = 2, width = 2) %>%
  font(fontname = "Calibri", part = "all") %>%
  fontsize(size = 11, part = "all") %>%
  align(align = "left", part = "body") %>%
  align(align = "center", part = "header") %>%
  bold(i = 1, part = "header") %>%
  bg(i = 1, bg = "#D3D3D3", part = "header") %>%
  border(part = "all", border = fp_border(color = "grey70", width = 1)) %>%
  padding(i = 2:3, j = 1, padding.left = 40) %>%  # Indent rows 2 and 3 in the first column
  add_footer_lines(c(
    "[1] Device Success: Device is deployed as intended and the delivery system is successfully retrieved as intended at the time of the patient’s exit from the cardiac catheterization laboratory.",
    "Categorical measures: n/Total N (%)",
    program_info
  )) %>%
  font(fontname = "Calibri", part = "footer") %>% 
  fontsize(size = 11, part = "footer") %>%
  padding(part = "footer", padding.top = 1, padding.bottom = 1) %>% 
  border_outer(part = "footer", border = fp_border(color = "grey70", width = 1)) %>%
  fix_border_issues()

t4_devicesuccess