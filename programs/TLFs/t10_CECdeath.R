library(flextable)
library(officer)
library(dplyr)
library(knitr)
library(dplyr)
library(tibble)
library(stringr)


source("C:/Users/luke_hall/OneDrive - Edwards Lifesciences/lhall/TWIST_EFS_DSMB_2025NOV/programs/makedata/mk_adsl.R")


extract_date <- "09JUL2025"
program_name <- "t10_CECdeath.R"
#program_name <- basename(rstudioapi::getSourceEditorContext()$path)
program_info <- paste0("Source: ", program_name, " Extract Date: ", extract_date, 
                       " Run Date (Time): ", format(Sys.time(), "%d%b%Y (%H:%M)"))

totaln_enrolled <- dplyr::n_distinct(adsl$USUBJID[adsl$EnrolledFl == "Y" & adsl$USAFl=="Y"])
totaln_implanted <- dplyr::n_distinct(adsl$USUBJID[adsl$ImplantedFl == "Y" & adsl$USAFl=="Y"])

t_data <- left_join(adsl, fa_cec, by=join_by(USUBJID==USUBJID))

t_data %>%
  filter(EnrolledFl=="Y" & USAFl=="Y" & FAORRES_DTHTYP_STD!="")%>%
  select(USUBJID, FADAT_NEW_INT, FAORRES_DTHTYP_STD, EnrolledFl, USAFl, PRSTDAT)%>%
  mutate(datediff = FADAT_NEW_INT- PRSTDAT)%>%
  mutate(Early = case_when(datediff < 30 ~"Y", TRUE~"N"))%>%
  mutate(Late = case_when(datediff >= 30 & datediff <= 365 ~"Y", TRUE~"N"))





t_enrolled <- adsl %>% 
  filter(EnrolledFl=="Y", USAFl=="Y") %>%
  count(SiteNumber, SiteName, sort=T) %>%
  mutate(freq_enrolled=n/sum(n)) %>%
  group_by(SiteNumber, SiteName) %>%
  rename(n_enrolled=n)%>%
  mutate(print_enrolled=paste0(n_enrolled, "/", totaln_enrolled, " (", format(round(100*freq_enrolled, 1), nsmall=1), "%)"))

t_enrolled

t_implanted <- adsl %>%
  filter(ImplantedFl=="Y", USAFl=="Y") %>%
  count(SiteNumber, SiteName, sort=T) %>%
  mutate(freq_implanted=n/sum(n)) %>%
  group_by(SiteNumber, SiteName) %>%
  rename(n_implanted=n)%>%
  mutate(print_implanted=paste0(n_implanted, "/", totaln_implanted, " (", format(round(100*freq_implanted, 1), nsmall=1), "%)"))

t_implanted

t1 <- left_join(t_enrolled, t_implanted, by=join_by(SiteNumber == SiteNumber, SiteName == SiteName))
t1 <- t1 %>%
  select(SiteNumber, SiteName, print_enrolled, print_implanted)

t1_enrollmentbysite <- flextable(t1) %>%
  set_caption(caption = NULL) %>%
  set_header_labels(
    SiteNumber = "Site Number",
    SiteName = "Site Name",
    print_enrolled = paste0("Enrolled\u00B9 (N=", totaln_enrolled, ")"),
    print_implanted = paste0("Implanted\u00B2 (N=", totaln_implanted, ")")
  ) %>%
  autofit() %>%
  font(fontname = "Calibri", part = "all") %>%
  fontsize(size = 11, part = "all") %>%
  align(align = "left", part = "all") %>%
  bold(i = 1, part = "header") %>%
  bg(i = 1, bg = "#D3D3D3", part = "header") %>%
  border(part = "all", border = officer::fp_border(color = "grey70", width = 1)) %>%
  width(j = 1, width = 1.25) %>%    
  width(j = 2, width = 2.75) %>% 
  width(j = 3:4, width = 2) %>%
  add_footer_lines(c(
    "[1] Enrolled: A patient is considered enrolled if they have signed informed consent and have the study procedure attempted (defined as introduction of the investigational delivery system into the patient).",
    "[2] Implanted: A patient is considered implanted if they have undergone the study procedure and leave the operating room with the study valve in place.",
    "Categorical measures: %",
    program_info
  )) %>%
  font(fontname = "Calibri", part = "footer") %>% 
  fontsize(size = 11, part = "footer") %>%
  padding(part = "footer", padding.top = 1, padding.bottom = 1) %>% 
  border_outer(part = "footer") %>% 
  fix_border_issues()



t1_enrollmentbysite