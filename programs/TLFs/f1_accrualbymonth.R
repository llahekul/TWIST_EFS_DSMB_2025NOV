# FIGURE 1: Linear Graph of Cumulative Enrollment by Month
library(lubridate)
library(ggplot2)
library(dplyr)
library(stringr)
library(showtext)
library(patchwork)

extract_date <- "09JUL2025"
program_name <- "f1_accrualbymont.R"
program_info <- paste0("Source: ", program_name, " Extract Date: ", extract_date, 
                       " Run Date (Time): ", format(Sys.time(), "%d%b%Y (%H:%M)"))

# AUTOMATE LATER
#load("C:/Users/luke_hall/OneDrive - Edwards Lifesciences/lhall/TWIST_EFS/data_archive/data_2025JUL16.RData")

# load in adsl
source("C:/Users/luke_hall/OneDrive - Edwards Lifesciences/lhall/TWIST_EFS_2025_11_DSMB/programs/makedata/mk_adsl.R")

accrual <- adsl %>% 
  filter(EnrolledFl=="Y", USAFl=="Y") %>%
  mutate(new_date=floor_date(PRSTDAT_f, "month"))%>%
  count(EnrolledFl, new_date, sort=T) %>%
  #spread(EnrolledFl, n, fill=0) %>% 
  arrange(new_date)%>%
  mutate(csum=cumsum(n))

totaln_enrolled <- dplyr::n_distinct(adsl$USUBJID[adsl$EnrolledFl == "Y" & adsl$USAFl=="Y"])

# add 0,0 
dayzero <- data.frame(EnrolledFl="Y", new_date=as.Date("2022-08-01"), n=0, csum=0)
accrual <- rbind(dayzero, accrual)



f1_accrualbymonth <- ggplot(data = accrual, aes(x = new_date)) +
  geom_line(aes(y = csum)) +
  geom_point(aes(y = csum)) +
  geom_text(aes(y = csum, label = csum), hjust = 0.5, vjust = -2, size = 3, color = "black") +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y", expand = c(0.01, 0)) +
  scale_y_continuous(expand = c(0.01, 0), limits = c(0, 40), breaks = seq(0, 40, 5)) +
  ylab("Number of Patients Enrolled") +
  xlab("") +
  ggtitle(paste0("Enrolled Population (N=", totaln_enrolled, ")")) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Load system fonts and activate
font_add("calibri", regular = "C:/Windows/Fonts/calibri.ttf")
showtext_auto()

footer <- ggplot() +
  geom_text(aes(x = 0, y = 0, label = program_info), hjust = 0, size = 3.5, family = "calibri") +
  xlim(0, 1) +  # stretch x range so text aligns far left
  theme_void() +
  theme(plot.margin = margin(t = -10, r = 0, b = 5, l = 5, unit = "pt"))

# Combine plot and footnote
final_plot <- f1_accrualbymonth / footer + plot_layout(heights = c(6, 1))

final_plot
