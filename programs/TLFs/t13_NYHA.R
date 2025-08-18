{library(flextable)
  library(officer)
  library(dplyr)
  library(knitr)
  library(dplyr)
  library(tibble)
  library(stringr)}

extract_date <- "2025AUG06"
program_name <- "t13_NYHA.R"
program_info <- paste0("Source: ", program_name, " Extract Date: ", extract_date, 
                       " Run Date (Time): ", format(Sys.time(), "%d%b%Y (%H:%M)"))

## NEED TO READ ADSL INTO ENVIRONMENT FIRST 
#load("C:/Users/luke_hall/OneDrive - Edwards Lifesciences/lhall/TWIST_EFS/data_archive/data_2025JUL16.RData")
#source("C:/Users/luke_hall/OneDrive - Edwards Lifesciences/lhall/TWIST_EFS_2025_11_DSMB/programs/makedata/mk_adsl.R")

#load(paste0("C:/Users/isabelle_weir/Edwards Lifesciences/Luke Hall - TWIST_EFS_2025_11_DSMB/data/data_", extract_date, ".RData"))
#source("C:/Users/isabelle_weir/Edwards Lifesciences/Luke Hall - TWIST_EFS_2025_11_DSMB/programs/makedata/mk_adsl.R")
#source("C:/Users/isabelle_weir/Edwards Lifesciences/Luke Hall - TWIST_EFS_2025_11_DSMB/programs/makedata/mk_adsv.R")
#source("C:/Users/isabelle_weir/Edwards Lifesciences/Luke Hall - TWIST_EFS_2025_11_DSMB/programs/makedata/mk_adststat.R")
source("C:/Users/luke_hall/OneDrive - Edwards Lifesciences/lhall/TWIST_EFS_DSMB_2025NOV/programs/makedata/mk_adsl.R")
source("C:/Users/luke_hall/OneDrive - Edwards Lifesciences/lhall/TWIST_EFS_DSMB_2025NOV/programs/makedata/mk_adsv.R")
source("C:/Users/luke_hall/OneDrive - Edwards Lifesciences/lhall/TWIST_EFS_DSMB_2025NOV/programs/makedata/mk_adststat.R")

totaln_enrolled <- dplyr::n_distinct(adsl$USUBJID[adsl$EnrolledFl == "Y"])
totaln_implanted <- dplyr::n_distinct(adsl$USUBJID[adsl$ImplantedFl == "Y"])

sl_ststat <- left_join(adsl, adststat, by=join_by(USUBJID==USUBJID, Subject==Subject))
baseline_data <- left_join(sl_ststat, sv_sb, by=join_by(USUBJID==USUBJID, Subject==Subject, SiteNumber==SiteNumber))
visit_data <- left_join(sl_ststat, sv, by=join_by(USUBJID==USUBJID, Subject==Subject, SiteNumber==SiteNumber))

# determine denominators:
ad_sl_sv <- left_join(adsl, adsv, by=join_by(USUBJID==USUBJID, Subject==Subject))

denominators <- ad_sl_sv %>%
  filter(ImplantedFl=="Y" & !is.na(SVDAT) & VISIT %in% c("Screening/Baseline", "30 Days", "6 Months", "1 Year", "2 Years")) %>%
  mutate(VISITnew = recode(VISIT,
                          "2 Years" = "FU2Y",
                          "1 Year" = "FU1Y",
                          "6 Months" = "FU6M",
                          "Screening/Baseline" = "FU0M",
                          "30 Days"="FU30D"))%>%
  group_by(VISITnew)%>%
  summarise(denom=n())

denom30 <- denominators %>%
  filter(VISITnew=="FU30D")%>%
  select(denom)

denom6M <- denominators %>%
  filter(VISITnew=="FU6M")%>%
  select(denom)

denom1Y <- denominators %>%
  filter(VISITnew=="FU1Y")%>%
  select(denom)

denom2Y <- denominators %>%
  filter(VISITnew=="FU2Y")%>%
  select(denom)

# table(baseline_data$RSORRES_NYHACLS_STD)
# table(baseline_data$RSORRES_NYHACLS)

# baseline measurement
baseline_res <- baseline_data %>%
  filter(ImplantedFl=="Y")%>%
  select(USUBJID, SVDAT, RSORRES_NYHACLS_STD, RSORRES_NYHACLS, EnrolledFl, USAFl, PRSTDAT)%>%
  mutate(datediff = difftime(SVDAT, PRSTDAT, units="days"))%>%
  #summarise(unique_categories = n_distinct(USUBJID))%>%
  group_by(RSORRES_NYHACLS)%>%
  summarise(n=paste0(n(), "/", totaln_implanted, " (", 
                   format(round(100*n()/totaln_implanted, 1), nsmall=1), "%)"))

# other visits
visit_res <- visit_data %>%
  filter(ImplantedFl=="Y" & Folder %in% c("FU30D", "FU6M", "FU1Y", "FU2Y"))%>%
  select(USUBJID, SVDAT, RSORRES_NYHACLS_STD, RSORRES_NYHACLS, EnrolledFl, USAFl, PRSTDAT, Folder)%>%
  mutate(datediff = difftime(SVDAT, PRSTDAT, units="days"))%>%
  #summarise(unique_categories = n_distinct(USUBJID))%>%
  group_by(Folder, RSORRES_NYHACLS)%>%
  summarise(n=paste0(n()))

visit_res
visit_res_denom <- inner_join(denominators, visit_res, by=join_by(VISITnew==Folder))

visit_results <- visit_res_denom %>%
  mutate(fraction = as.numeric(n) / denom*100)%>%
   mutate(res=paste0(n, "/", denom, " (", format(round(fraction, 1), nsmall=1), "%)"))


allresults <- tibble(
    NYHAClass = c("Class I", "Class II", "Class III", "Class IV"),
    Baseline = c("-", baseline_res$n[1], baseline_res$n[2], "-"),
    res30D = c(visit_results$res[9:12]),
    res6M = c(visit_results$res[13:16]),
    res1Y = c(visit_results$res[2:4], "-"),
    res2Y = c(visit_results$res[6:8], "-")
  )

allresults

# make flextable
t13_NYHA <- 
  flextable(allresults) %>%
  set_caption(caption = NULL) %>%
  set_header_labels(
    Category = "",
    NYHAClass="NYHA Class", 
    Baseline=paste0("Baseline \n (N=", totaln_implanted, ")"),
    res30D=paste0("30 Days \n (N=", denom30, ")"), 
    res6M=paste0("6 Months \n (N=", denom6M, ")"),
    res1Y=paste0("1 Year \n (N=", denom1Y, ")"),
    res2Y=paste0("2 Years \n (N=", denom2Y, ")")) %>%
  autofit() %>%
  font(fontname = "Calibri", part = "all") %>%
  fontsize(size = 11, part = "all") %>%
  align(align = "center", part = "body") %>%
  align(align = "center", part = "header") %>%
  align(i=1:4, j=1, align = "left", part = "body") %>%
  align(i=1, j=2:5, align = "center", part = "header") %>%
  bold(i = 1, part = "header") %>%
  bg(i = 1, bg = "#D3D3D3", part = "header") %>%
  border(part = "all", border = officer::fp_border(color = "grey70", width = 0.5)) %>%
  # width(j = 1, width = 2) %>%    
  # width(j = 2:7, width = 1) %>% 
  #width(j = 3:4, width = 2) %>%
  add_footer_lines(c(
    "Categorical measures: n/Total N (%)",
    program_info
  )) %>%
    font(fontname = "Calibri", part = "footer") %>% 
    fontsize(size = 11, part = "footer") %>%
    padding(part = "footer", padding.top = 1, padding.bottom = 1) %>% 
    border_outer(part = "footer", border = fp_border(color = "grey70", width = 1)) %>%
    fix_border_issues()


t13_NYHA








