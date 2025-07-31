# create ADSTSTAT

cut_dt <- "2025-07-28"  # Example date-time string

enrolled <- pr2[!is.na(pr2$PRSTDAT),]
enrolled <- enrolled[c("Subject", "PRSTDAT")]

#rename procedure date
enrolled <- enrolled %>%
  rename(procedure_dt = PRSTDAT)

# get deaths
# CEC-adjudicated deaths
deaths_cec <- subset(fa_cec, fa_cec$FAORRES_DTHTYP %in% c("Cardiovascular", "Non-cardiovascular"))

# subset variables
deaths_cec <- deaths_cec[c("Subject", "FADAT_NEW")]

#rename death date
#deaths_cec <- deaths_cec %>%
#  rename(death_dt = FADAT_NEW)

# deaths in AE log
deaths_ae <- subset(ae, ae$AESDTH == 1)
deaths_ae <- deaths_ae[c("Subject", "DDDAT")]

deaths <- full_join(deaths_ae, deaths_cec, by="Subject")

# get rid of duplicate death
deaths <- deaths %>%
  distinct(Subject, .keep_all = TRUE)

# pick death in AE log over CEC-death
# per Rafeek - most probably was a data entry error from Innovalve team when transfer data for RedCap to RAVE
deaths$death_dt <- as.POSIXct(coalesce(deaths$DDDAT, deaths$FADAT_NEW))
deaths <- deaths[c("Subject", "death_dt")]

# row-bind them together
adststat <- full_join(enrolled, deaths, by="Subject")

adststat$cut_dt <- as.POSIXct(cut_dt, format = "%Y-%m-%d", tz = "America/New_York")

adststat$PARAM <- "Time on study"
adststat$PARAMCD <- "TIMEONST"

adststat$AVAL <- ifelse(
  !is.na(adststat$death_dt),
  as.numeric(difftime(adststat$death_dt, adststat$procedure_dt, units = "days")),
  as.numeric(difftime(adststat$cut_dt, adststat$procedure_dt, units = "days"))
)

# add in USUBJID too
USUBJID <- pr2[c("Subject", "USUBJID")]
adststat <- left_join(adststat, USUBJID, by="Subject")

adststat

