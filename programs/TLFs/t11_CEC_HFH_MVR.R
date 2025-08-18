# Table 11 - CEC Adjudicated Heart Failure Hospitalizations and Non-elective Mitral Valve Reinterventions 
{library(flextable)
  library(officer)
  library(dplyr)
  library(knitr)
  library(dplyr)
  library(tibble)
  library(stringr)}

extract_date <- "2025AUG06"
program_name <- "t11_CEC_HFH_MVR.R"
program_info <- paste0("Source: ", program_name, " Extract Date: ", extract_date, 
                       " Run Date (Time): ", format(Sys.time(), "%d%b%Y (%H:%M)"))

## NEED TO READ ADSL INTO ENVIRONMENT FIRST 
#load("C:/Users/luke_hall/OneDrive - Edwards Lifesciences/lhall/TWIST_EFS/data_archive/data_2025JUL16.RData")
#source("C:/Users/luke_hall/OneDrive - Edwards Lifesciences/lhall/TWIST_EFS_2025_11_DSMB/programs/makedata/mk_adsl.R")

#load(paste0("C:/Users/isabelle_weir/Edwards Lifesciences/Luke Hall - TWIST_EFS_2025_11_DSMB/data/data_", extract_date, ".RData"))
#source("C:/Users/isabelle_weir/Edwards Lifesciences/Luke Hall - TWIST_EFS_2025_11_DSMB/programs/makedata/mk_adsl.R")
#source("C:/Users/isabelle_weir/Edwards Lifesciences/Luke Hall - TWIST_EFS_2025_11_DSMB/programs/makedata/mk_adsv.R")
#source("C:/Users/isabelle_weir/Edwards Lifesciences/Luke Hall - TWIST_EFS_2025_11_DSMB/programs/makedata/mk_adststat.R")
source("C:/Users/luke_hall/OneDrive - Edwards Lifesciences/lhall/TWIST_EFS_DSMB_2025NOV/programs/makedata/mk_adststat.R")

totaln_enrolled <- dplyr::n_distinct(adsl$USUBJID[adsl$EnrolledFl == "Y"])

sl_ststat <- left_join(adsl, adststat, by=join_by(USUBJID==USUBJID, Subject==Subject))

t_data <- left_join(sl_ststat, fa_cec, by=join_by(USUBJID==USUBJID, Subject==Subject, SiteNumber==SiteNumber))

onstudy0 <- sl_ststat %>%
  filter(EnrolledFl=="Y") %>%
  summarise(N_onstudy0=n())

onstudy30 <- sl_ststat %>%
  filter(EnrolledFl=="Y" & AVAL>= 30) %>%
  summarise(N_onstudy30=n())


allHFH <- t_data %>%
  filter(EnrolledFl=="Y" & FAORRES_HOSP_STD=="Y" & HOINDC=="Heart failure")%>%
  select(USUBJID, FADAT_NEW_INT, FAORRES_HOSP_STD, EnrolledFl, USAFl, PRSTDAT)%>%
  mutate(datediff = FADAT_NEW_INT- PRSTDAT)%>%
  mutate(Early = case_when(datediff <= 30 ~"Y", TRUE~"N"))%>%
  mutate(Late = case_when(datediff > 30 & datediff <= 365 ~"Y", TRUE~"N"))%>%
  mutate(TimeWindow = case_when(datediff <= 30 ~"Early",
                                datediff > 30 & datediff <= 365 ~"Late",
                                datediff > 365 ~ "Outside Window"
                         ))%>%
  mutate(TotalEvents = case_when(TimeWindow %in% c("Early", "Late") ~ "Total"))

# calculate the "No. Event" first
Etotal <- allHFH %>%   
  group_by(TimeWindow)%>%
  filter(TimeWindow %in% c("Early"))%>%
  summarise(n=paste0(n()))
Etotal

Ltotal <- allHFH %>%   
  group_by(TimeWindow)%>%
  filter(TimeWindow %in% c("Late"))%>%
  summarise(n=paste0(n()))
Ltotal

Ttotal <- allHFH %>%  
  group_by(TotalEvents)%>%
  filter(TotalEvents %in% c("Total"))%>%
  summarise(n=paste0(n()))
  
Ttotal

# now calculate the number of unique patients with at least 1 event

firstHFH <- allHFH %>% 
  group_by(USUBJID, TimeWindow) %>%
  slice(1) %>% ungroup()

Epts <- allHFH %>%
  group_by(USUBJID, TimeWindow) %>%
  slice(1) %>% ungroup() %>%
  group_by(TimeWindow)%>%
  filter(TimeWindow %in% c("Early"))%>%
  summarise(n=paste0(n(), "/", onstudy0, " (", 
                        format(round(100*n()/onstudy0, 1), nsmall=1), "%)"))

Epts

Lpts <- allHFH %>%
  group_by(USUBJID, TimeWindow) %>%
  slice(1) %>% ungroup() %>%
  group_by(TimeWindow)%>%
  filter(TimeWindow %in% c("Late"))%>%
  summarise(n=paste0(n(), "/", onstudy30, " (", 
                        format(round(100*n()/onstudy30, 1), nsmall=1), "%)"))

Lpts

Tpts <- allHFH %>%
  group_by(USUBJID, TotalEvents) %>%
  slice(1) %>% ungroup() %>%
  group_by(TotalEvents)%>%
  filter(TotalEvents %in% c("Total"))%>%
  summarise(n=paste0(n(), "/", onstudy0, " (", 
                        format(round(100*n()/onstudy0, 1), nsmall=1), "%)"))

Tpts

Etotal
Ltotal
Ttotal
Epts
Lpts
Tpts

# repeat for Reintervention 

allMVR <- t_data %>%
  filter(EnrolledFl=="Y" & FAORRES_CMRQSRGI_STD=="Y")%>%
  select(USUBJID, FADAT_NEW_INT, FAORRES_CMRQSRGI, EnrolledFl, USAFl, PRSTDAT)%>%
  mutate(datediff = difftime(FADAT_NEW_INT, PRSTDAT, units="days"))%>%
  mutate(Early = case_when(datediff <= 30 ~"Y", TRUE~"N"))%>%
  mutate(Late = case_when(datediff > 30 & datediff <= 365 ~"Y", TRUE~"N"))%>%
  mutate(TimeWindow = case_when(datediff <= 30 ~"Early",
                                datediff > 30 & datediff <= 365 ~"Late",
                                datediff > 365 ~ "Outside Window"
  ))%>%
  mutate(TotalEvents = case_when(TimeWindow %in% c("Early", "Late") ~ "Total"))

# calculate the "No. Event" first
EtotalMVR <- allMVR %>%   
  group_by(TimeWindow)%>%
  filter(TimeWindow %in% c("Early"))%>%
  summarise(n=paste0(n()))


LtotalMVR <- allMVR %>%   
  group_by(TimeWindow)%>%
  filter(TimeWindow %in% c("Late"))%>%
  summarise(n=paste0(n()))


TtotalMVR <- allMVR %>%  
  group_by(TotalEvents)%>%
  filter(TotalEvents %in% c("Total"))%>%
  summarise(n=paste0(n()))


# now calculate the number of unique patients with at least 1 event

firstMVR <- allMVR %>% 
  group_by(USUBJID, TimeWindow) %>%
  slice(1) %>% ungroup()

EptsMVR <- allMVR %>%
  group_by(USUBJID, TimeWindow) %>%
  slice(1) %>% ungroup() %>%
  group_by(TimeWindow)%>%
  filter(TimeWindow %in% c("Early"))%>%
  summarise(n=paste0(n(), "/", onstudy0, " (", 
                        format(round(100*n()/onstudy0, 1), nsmall=1), "%)"))


LptsMVR <- allMVR %>%
  group_by(USUBJID, TimeWindow) %>%
  slice(1) %>% ungroup() %>%
  group_by(TimeWindow)%>%
  filter(TimeWindow %in% c("Late"))%>%
  summarise(n=paste0(n(), "/", onstudy30, " (", 
                        format(round(100*n()/onstudy30, 1), nsmall=1), "%)"))

LptsMVR

TptsMVR <- allMVR %>%
  group_by(USUBJID, TotalEvents) %>%
  slice(1) %>% ungroup() %>%
  group_by(TotalEvents)%>%
  filter(TotalEvents %in% c("Total"))%>%
  summarise(n=paste0(n(), "/", onstudy0, " (", 
                        format(round(100*n()/onstudy0, 1), nsmall=1), "%)"))


# make raw table: 
resHFH <- data.frame("Name"=c("Heart Failure Hospitalization"),
                  "R1"=c(Etotal[1,2]),
                  "R2"=c(Epts[1,2]),
                  "R3"=c(Ltotal[1,2]),
                  "R4"=c(Lpts[1,2]),
                  "R5"=c(Ttotal[1,2]),
                  "R6"=c(Tpts[1,2]))
resHFH

resMVR <- data.frame("Name"=c( "Non-elective mitral valve re-intervention, percutaneous or surgical"),
                  "R1"=c(EtotalMVR[1,2]),
                  "R2"=c(EptsMVR[1,2]),
                  "R3"=c(LtotalMVR[1,2]),
                  "R4"=c(LptsMVR[1,2]),
                  "R5"=c(TtotalMVR[1,2]),
                  "R6"=c(TptsMVR[1,2]))
resMVR

allresults <- rbind(resHFH, resMVR)

colnames(allresults) <- c("Category",
                   "E_NEvents",
                   "E_Patients",
                   "L_NEvents",
                   "L_Patients",
                   "T_NEvents",
                   "T_Patients")

allresults

t11_CEC_HFH_MVR <- flextable(allresults) %>%
  set_caption(caption = NULL) %>%
  set_header_labels(
    Category    = "",
    E_NEvents   = "No. Events", 
    E_Patients  = "Patients",
    L_NEvents   = "No. Events", 
    L_Patients  = "Patients",
    T_NEvents   = "No. Events", 
    T_Patients  = "Patients"
  ) %>%
  add_header_row(
    top = TRUE,
    values = c("Category", 
               "Early Events \n (\u2264 30 Days)", 
               "Late Events \n (\u003E 30 Days to 1 Year)", 
               "Total Events"),
    colwidths = c(1, 2, 2, 2)
  ) %>%
  autofit() %>%
  font(fontname = "Calibri", part = "all") %>%
  fontsize(size = 11, part = "all") %>%
  align(align = "center", part = "body") %>%
  align(i = 1:2, j = 1, align = "left", part = "body") %>%
  align(i = 1:2, align = "center", part = "header") %>%
  bold(i = 1:2, part = "header") %>%
  bg(i = 1:2, bg = "#D3D3D3", part = "header") %>%
  
  # Header grid lines
  border_inner_h(part = "header", border = officer::fp_border(color = "grey70", width = 1)) %>%
  border_inner_v(part = "header", border = officer::fp_border(color = "grey70", width = 1)) %>%
  
  # Vertical lines ONLY between No. Events and Patients in second header row
  border(i = 2, j = c(2, 4, 6), border.right = officer::fp_border(color = "grey70", width = 1), part = "header") %>%
  border(i = 1:2, j = c(1, 3, 5), border.right = officer::fp_border(color = "grey70", width = 1), part = "body") %>%
  
  # Transparent borders between paired columns in second header row
  #border(i = 2, j = c(3, 5, 7), border.right = officer::fp_border(color = "transparent", width = 1), part = "header") %>%
  
  # Outer border for entire table
  border_outer(border = officer::fp_border(color = "grey70", width = 1)) %>%
  
  # Column widths
  width(j = 1, width = 2) %>%
  width(j = 2:7, width = 1) %>%
  
  # Footer
  add_footer_lines(c(
    "Categorical measures: n/Total N (%)",
    program_info
  )) %>%
  font(fontname = "Calibri", part = "footer") %>%
  fontsize(size = 11, part = "footer") %>%
  padding(part = "footer", padding.top = 1, padding.bottom = 1) %>%
  border_outer(part = "footer", border = officer::fp_border(color = "grey70", width = 1)) %>%
  
  fix_border_issues()

# Merge the "Category" header cell across two rows
t11_CEC_HFH_MVR <- merge_at(t11_CEC_HFH_MVR, i = 1:2, j = 1, part = "header")

# Add vertical grid line between "Category" and first time window
t11_CEC_HFH_MVR <- border(t11_CEC_HFH_MVR, i = 1:2, j = 1, border.right = officer::fp_border(color = "grey70", width = 1), part = "header")

t11_CEC_HFH_MVR






