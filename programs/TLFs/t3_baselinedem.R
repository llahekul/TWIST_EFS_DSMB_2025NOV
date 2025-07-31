# TABLE 3: Demographics and Baseline Characteristics
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

# enrolled pop
enrolled <- pr2[!is.na(pr2$PRSTDAT),]

enrolled <- enrolled[c("Subject")]


# age, sex
age_sex <- dm[c("Subject", "AGE", "SEX")]
age_sex <- left_join(enrolled, age_sex, by = "Subject")

# bmi, NYHA
clin <- sv_sb[c("Subject", "VSORRES_BMI", "RSORRES_NYHACLS")]
clin <- left_join(enrolled, clin, by = "Subject")

# LVEF (categorical)
lvef <- cv_sb1[c("Subject", "CVTESTCD", "CVDAT", "CVORRES_LVEF_EC")]
lvef <- subset(lvef, lvef$CVTESTCD == "TTE")
lvef <- left_join(enrolled, lvef, by = "Subject")
lvef <- lvef %>%
  group_by(Subject) %>%
  filter(CVDAT == max(CVDAT)) %>%
  ungroup()
lvef <- lvef[!duplicated(lvef$Subject), ]
lvef <- left_join(enrolled, lvef, by = "Subject")


# medical history
history <- mh[c("Subject", "MHOCCUR_STROKE", "MHOCCUR_AFIB", "MHOCCUR_PCI", "MHOCCUR_CABG")]
history <- left_join(enrolled, history, by="Subject")

# MERGE ALL DFs TOGETHER
final_df <- age_sex %>%
  full_join(clin, by = "Subject") %>%
  full_join(lvef, by = "Subject") %>%
  full_join(history, by = "Subject")


# make table
library(dplyr)
library(tidyr)
library(purrr)

# Identify variable types
#categorical_vars <- names(final_df)[sapply(final_df, is.factor) | sapply(final_df, is.character)]
continuous_vars <- c("AGE", "VSORRES_BMI")
# List of binary Y/N variables to filter by "Yes"
categorical_vars <- c("SEX", "RSORRES_NYHACLS", "CVORRES_LVEF_EC",
                      "MHOCCUR_STROKE", "MHOCCUR_AFIB", "MHOCCUR_PCI", "MHOCCUR_CABG")

yes_only_vars <- c("MHOCCUR_STROKE", "MHOCCUR_AFIB", "MHOCCUR_PCI", "MHOCCUR_CABG")

label_lookup <- c(
  AGE = "Age (Years)",
  VSORRES_BMI = "BMI (kg/m²)",
  SEX = "Sex at Birth",
  RSORRES_NYHACLS = "NYHA Class",
  CVORRES_LVEF_EC = "LVEF (TTE)",
  MHOCCUR_STROKE = "Stroke",
  MHOCCUR_AFIB = "Atrial Fibrillation",
  MHOCCUR_PCI = "PCI",
  MHOCCUR_CABG = "CABG"
)

lookup_label <- function(var) label_lookup[[var]] %||% var



# Summary for continuous variables in compact format
cont_summary <- map_df(continuous_vars, function(var) {
  label <- lookup_label(var)
  
  mean_sd <- paste0(
    round(mean(final_df[[var]], na.rm = TRUE), 1), " ± ",
    round(sd(final_df[[var]], na.rm = TRUE), 1), " (",
    sum(!is.na(final_df[[var]])), ")"
  )
  
  med_min_max <- paste0(
    round(median(final_df[[var]], na.rm = TRUE), 1), " (",
    round(min(final_df[[var]], na.rm = TRUE), 1), ", ",
    round(max(final_df[[var]], na.rm = TRUE), 1), ")"
  )
  
  tibble(
    Variable = label,
    Statistic = paste(mean_sd, med_min_max, sep = "\n")
  )
})






cat_summary <- map_df(categorical_vars, function(var) {
  df <- final_df %>%
    filter(!is.na(.data[[var]]), .data[[var]] != "") %>%  # ← Exclude both NA and ""
    
    count(!!sym(var))
  
  total_n <- sum(df$n)
  if (total_n == 0) return(NULL)
  
  label <- lookup_label(var)
  
  if (var %in% yes_only_vars) {
    df <- df %>%
      filter(.data[[var]] == "Yes")
    
    tibble(
      Variable = label,
      Statistic = paste0(df$n, " / ", total_n, " (", round(100 * df$n / total_n, 1), "%)")
    )
  } else {
    tibble(
      Variable = paste(
        c(label, paste0(paste(rep("\u00A0", 8), collapse = ""), as.character(df[[var]]))),
        collapse = "\n"
      ),
      Statistic = paste(
        c("",
          paste0(df$n, " / ", total_n, " (", round(100 * df$n / total_n, 1), "%)")),
        collapse = "\n"
      )
    )
  }
})



summary_table <- bind_rows(cont_summary, cat_summary)

extract_date <- "28JUL2025"
program_name <- "t3_baselinedem.R"
program_info <- paste0("Source: ", program_name, " Extract Date: ", extract_date, 
                       " Run Date (Time): ", format(Sys.time(), "%d%b%Y (%H:%M)"))

t_baseline <- flextable(summary_table) %>%
  set_header_labels(
    Variable = "Baseline Characteristic",
    Statistic = "Total"
  ) %>%
  width(j = 1, width = 7.5) %>%
  width(j = 2, width = 7.5) %>%
  font(fontname = "Calibri", part = "all") %>%
  fontsize(size = 11, part = "all") %>%
  align(align = "left", part = "all") %>%
  bold(i = 1, part = "header") %>%
  bg(i = 1, bg = "#D3D3D3", part = "header") %>%
  border(part = "all", border = officer::fp_border(color = "grey70", width = 1)) %>%
  add_footer_lines(c(
    "Continuous variables: Mean ± SD (n); Median (min, max)",
    "Categorical measures: n/Total N (%)",
    program_info
  )) %>%
  font(fontname = "Calibri", part = "footer") %>%
  fontsize(size = 11, part = "footer") %>%
  border_outer(part = "footer") %>%
  fix_border_issues()



t_baseline






