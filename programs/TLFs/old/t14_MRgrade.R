{library(flextable)
  library(officer)
  library(dplyr)
  library(knitr)
  library(dplyr)
  library(tibble)
  library(stringr)}

extract_date <- "2025JUL28"
program_name <- "t4_MRgrade.R"
program_info <- paste0("Source: ", program_name, " Extract Date: ", extract_date, 
                       " Run Date (Time): ", format(Sys.time(), "%d%b%Y (%H:%M)"))



source("C:/Users/luke_hall/OneDrive - Edwards Lifesciences/lhall/TWIST_EFS_DSMB_2025NOV/programs/makedata/mk_adsl.R")
source("C:/Users/luke_hall/OneDrive - Edwards Lifesciences/lhall/TWIST_EFS_DSMB_2025NOV/programs/makedata/mk_adsv.R")
source("C:/Users/luke_hall/OneDrive - Edwards Lifesciences/lhall/TWIST_EFS_DSMB_2025NOV/programs/makedata/mk_adststat.R")

totaln_enrolled <- dplyr::n_distinct(adsl$USUBJID[adsl$EnrolledFl == "Y"])
totaln_implanted <- dplyr::n_distinct(adsl$USUBJID[adsl$ImplantedFl == "Y"])

sl_ststat <- left_join(adsl, adststat, by=join_by(USUBJID==USUBJID, Subject==Subject))
baseline_data <- left_join(sl_ststat, cv_sb1, by=join_by(USUBJID==USUBJID, Subject==Subject, SiteNumber==SiteNumber))
visit_data <- left_join(sl_ststat, cv_fu2, by=join_by(USUBJID==USUBJID, Subject==Subject, SiteNumber==SiteNumber))

# determine denominators:
ad_sl_sv <- left_join(adsl, adsv, by=join_by(USUBJID==USUBJID, Subject==Subject))

denominators <- ad_sl_sv %>%
  filter(ImplantedFl=="Y" & ANL01FL=="Y" & VISIT %in% c("Screening/Baseline", "Discharge", "30 Days", "6 Months", "1 Year")) %>%
  mutate(VISITnew = recode(VISIT,
                           "1 Year" = "EO1Y",
                           "6 Months" = "EO6M",
                           "Screening/Baseline" = "EO0M",
                           "30 Days"="EO30D",
                           "Discharge"="EODS"))%>%
  group_by(VISITnew)%>%
  summarise(denom=n())

denomdischarge <- denominators %>%
  filter(VISITnew=="EODS")%>%
  select(denom)

denom30 <- denominators %>%
  filter(VISITnew=="EO30D")%>%
  select(denom)

denom6M <- denominators %>%
  filter(VISITnew=="EO6M")%>%
  select(denom)

denom1Y <- denominators %>%
  filter(VISITnew=="EO1Y")%>%
  select(denom)

# baseline measurement
baseline_res <- baseline_data %>%
  filter(ImplantedFl=="Y" & USAFl=="Y")%>%
  select(USUBJID, CVDAT, CVORRES_MVLREGTS, CVORRES_MVLREGTS_STD, ImplantedFl, USAFl, PRSTDAT)%>%
  mutate(datediff = difftime(CVDAT, PRSTDAT, units="days"))%>%
  #summarise(unique_categories = n_distinct(USUBJID))%>%
  group_by(CVORRES_MVLREGTS_STD)%>%
  summarise(n=paste0(n(), "/", totaln_implanted, " (", 
                     format(round(100*n()/totaln_implanted, 1), nsmall=1), "%)"))

# other visits
visit_res <- visit_data %>%
  filter(ImplantedFl=="Y" & Folder %in% c("EODS", "EO30D", "EO6M", "EO1Y"))%>%
  select(USUBJID, CVDAT, CVORRES_TRNSVMR, CVORRES_TRNSVMR_STD, ImplantedFl, USAFl, PRSTDAT, Folder)%>%
  mutate(datediff = difftime(CVDAT, PRSTDAT, units="days"))%>%
  #summarise(unique_categories = n_distinct(USUBJID))%>%
  group_by(Folder, CVORRES_TRNSVMR)%>%
  summarise(n=paste0(n()))

visit_res
visit_res_denom <- inner_join(denominators, visit_res, by=join_by(VISITnew==Folder))

visit_results <- visit_res_denom %>%
  mutate(fraction = as.numeric(n) / denom*100)%>%
  mutate(res=paste0(n, "/", denom, " (", format(round(fraction, 1), nsmall=1), "%)"))


allresults <- tibble(
  MitralRegug = c("None / Trace", "Mild", "Mild-Moderate", "Moderate-Severe", "Severe", "Not-evaluable"),
  Baseline = c("-", baseline_res$n[1], baseline_res$n[2], "-"),
  resDC = c(visit_results),
  res30D = c(visit_results$res[5:8]),
  res6M = c(visit_results$res[9:12]),
  res1Y = c(visit_results$res[2:4], "-")
)

allresults


