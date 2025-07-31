# Make ADSL 
f1 <- ds_ic[,c("USUBJID", "Subject", "DSYN_IC_STD", "Site", "SiteNumber")]

# to determine "enrolled"
f2 <- pr2[,c("USUBJID", "PRSUPP_DSYSFMV", "PRSTDAT", "PRSTDAT_yyyy", "PRSTDAT_MMM")]
f2$PRSTDAT_f <- as.Date(format(as.Date(f2$PRSTDAT), "%Y/%m/%d"))

# to determine deaths during procedure 
#f3 <- ae[,c("USUBJID", "AEOUT", "AESUPP_REPVISIT_STD")]
f3 <- ae[ae$AEOUT=="Death" & ae$AESUPP_REPVISIT_STD=="PR",c("USUBJID", "AEOUT", "AESUPP_REPVISIT_STD")]
f3

adsl <- left_join(f1, f2, by = join_by(USUBJID == USUBJID))
adsl <- left_join(adsl, f3, by=join_by(USUBJID == USUBJID))

# remove string from Site to limit only to the Site Name
adsl <- adsl %>% 
  mutate(SiteName = str_remove(Site, " -.+"))

# USA Flag 
adsl <- adsl %>%
  mutate(USAFl = case_when(
    SiteName!="Istituto Clinico Humanitas"~"Y",
    TRUE ~ "N")
  )
adsl
table(adsl$USAFl)

# Enrolled Flag
adsl <- adsl %>% 
  mutate(EnrolledFl = case_when(
    DSYN_IC_STD=="Y" & PRSUPP_DSYSFMV=="Performed" ~ "Y", 
    TRUE~ "N")
  )
adsl

# Implanted Flag
adsl <- adsl %>% 
  mutate(ImplantedFl = case_when(
    (EnrolledFl=="Y" & AEOUT=="Death" & AESUPP_REPVISIT_STD=="PR" ~ "N"),
    (EnrolledFl=="Y" ~ "Y"),
    TRUE~ "N")
  )%>%
  select(USUBJID, Subject, SiteNumber, SiteName, PRSTDAT, PRSTDAT_f, EnrolledFl, ImplantedFl, USAFl)

table(adsl$ImplantedFl)
adsl

# add something to save this adsl dataset? 