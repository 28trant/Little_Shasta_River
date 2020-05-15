# SCRIPT to organize and analyze dissolved oxygen (DO) data collected from Peacemaker Ranch (PMK)
# AKL 2020

# ADD Libraries
install.packages("dplyr") 
library(dplyr)
install.packages("datetime") 
library(datetime)
install.packages("lubridate") 
library(lubridate)
install.packages("ggplot2") 
library(ggplot2)
install.packages("measurements") 
library(measurements)
install.packages("frost")
library(frost)

# ADD Data
PMK_do1 <- read.delim("C:/GitHub_projects/Little_Shasta_River/data/PMK_raw/DO/PMK_DO_20191115.csv", header=FALSE, comment.char="#", stringsAsFactors=FALSE) # (9/27/2019-11/15/2019)

PMK_do2 <- read.csv("C:/GitHub_projects/Little_Shasta_River/data/PMK_raw/DO/PMK_DO_20191213.csv", header=FALSE, comment.char="#", stringsAsFactors=FALSE) # (11/15/2019-12/13/2019)

PMK_do3 <- read.delim("C:/GitHub_projects/Little_Shasta_River/data/PMK_raw/DO/PMK_DO_20200123.csv", header=FALSE, comment.char="#", stringsAsFactors=FALSE) # (12/13/2019-1/23/2020)

PMK_do4 <- read.delim("C:/GitHub_projects/Little_Shasta_River/data/PMK_raw/DO/PMK_DO_20200221.csv", header=FALSE, comment.char="#", stringsAsFactors=FALSE) # (1/23/2020-2/21/2020)

PMK_do5 <- read.csv("C:/GitHub_projects/Little_Shasta_River/data/PMK_raw/DO/PMK_DO_20200312.csv", stringsAsFactors=FALSE) # MISSING DATA, dates filled in (2/21/2020-3/12/2020)

PMK_do6 <- read.delim("C:/GitHub_projects/Little_Shasta_River/data/PMK_raw/DO/PMK_DO_20200415_RE.csv", header=FALSE, comment.char="#", stringsAsFactors=FALSE) # (3/12/2020-4/15/2020)


# ORGANIZE 
colnames(PMK_do1) [1] <- "datapoint" # rename
  colnames(PMK_do1) [2] <- "DateTime" # rename 
  colnames(PMK_do1) [3] <- "DO" # rename
  colnames(PMK_do1) [4] <- "Temp.C" # rename
    PMK_do1 <- subset(PMK_do1, select=-c(5)) # remove battery column 
    PMK_do1 <- PMK_do1[-c(1:8,2362,2363), ] # remove high temp points and empty bottom rows
colnames(PMK_do2) [1] <- "datapoint" # rename
  colnames(PMK_do2) [2] <- "Date" # rename 
  colnames(PMK_do2) [3] <- "Time" # rename
  colnames(PMK_do2) [4] <- "DO" # rename
  colnames(PMK_do2) [5] <- "Temp.C"
    PMK_do2$DateTime <- paste(PMK_do2$Date, PMK_do2$Time)
    PMK_do2 <- subset(PMK_do2, select=-c(2,3))
colnames(PMK_do3) [1] <- "datapoint" # rename
  colnames(PMK_do3) [2] <- "DateTime" # rename 
  colnames(PMK_do3) [3] <- "DO" # rename
  colnames(PMK_do3) [4] <- "Temp.C" # rename
    PMK_do3 <- subset(PMK_do3, select=-c(5)) # remove battery column
    PMK_do3 <- PMK_do3[-c(1975,1976), ] # remove empty bottom rows
colnames(PMK_do4) [1] <- "datapoint" # rename
  colnames(PMK_do4) [2] <- "DateTime" # rename 
  colnames(PMK_do4) [3] <- "DO" # rename
  colnames(PMK_do4) [4] <- "Temp.F" # rename
    PMK_do4 <- subset(PMK_do4, select=-c(5:9)) # remove battery column
    PMK_do4 <- PMK_do4[-c(1), ] # remove bad top row
    PMK_do4$Temp.C <- convert.temperature(from="F",to="C",PMK_do4$Temp.F)
      PMK_do4 <- subset(PMK_do4, select=-c(4))
colnames(PMK_do5) [1] <- "datapoint" # rename
  colnames(PMK_do5) [2] <- "DateTime" # rename 
colnames(PMK_do6) [1] <- "datapoint" # rename
  colnames(PMK_do6) [2] <- "DateTime" # rename 
  colnames(PMK_do6) [3] <- "DO" # rename
  colnames(PMK_do6) [4] <- "Temp.F" # rename
    PMK_do6 <- PMK_do6[-c(1,1634,1635), ] # remove bad initial point, empty bottom rows
  PMK_do6$Temp.C <- convert.temperature(from="F",to="C",PMK_do6$Temp.F)
  PMK_do6 <- subset(PMK_do6, select=-c(4))
    PMK_do6 <- subset(PMK_do6, select=-c(4:7)) # remove battery column

PMK_do <- bind_rows(PMK_do1,PMK_do2,PMK_do3,PMK_do4,PMK_do5, PMK_do6) # merge all datasets together
PMK_do$Visit <- ifelse(PMK_do$datapoint==9, PMK_do$DO, NA) #add in field trips days for analysis
PMK_do$Order <- seq.int(nrow(PMK_do)) # add index column for ease in plotting
  
# SAVE data as csv file
write.csv(PMK_do, file="C:/GitHub_projects/Little_Shasta_River/data/PMK_processed/PMK_DO.csv")



write.csv(PMK_do, file="C:/GitHub_projects/Little_Shasta_River/data/PMK_processed/PMK_DO.csv")

### PLOT time series of DO
ggplot()+
  geom_line(data=PMK_do, aes(x=Order, y=DO, color="DO"), group=1) +
  geom_point(data=PMK_do, aes(x=Order, y=Visit, color="scrub"), size=1.25) +
  geom_hline(yintercept=5, color="red", size=0.5, linetype="dashed") +
  labs(title = "PMK Dissolved Oxygen Concentration",
       x = "2020 Water Year", 
       y = "DO Concentration (mg/L)") +
  scale_color_manual(values=c("DO"="slateblue1", "scrub"="black", "threshold"="red")) +
  scale_y_continuous(limits=c(0, 15)) +
  scale_x_continuous(breaks=c(0,2364,3708,5683,7069,8029,8701),labels=c(" "="9/27","2364"="11/15","3708"="12/13","5683"="1/23","7069"="2/21","8029"="3/12","8701"="4/15"))+        #run w/o to find breaks
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5,size = 22), 
        plot.subtitle = element_text(hjust = 0.5, size = 17),
        axis.title.y = element_text(size=14),
        axis.title.x = element_text(size=14),
        legend.title = element_blank(),
        #legend.position = c(0.8, 0.9),
        axis.text.x = element_text(size=11.5),
        axis.text.y = element_text(size=11.5),
        legend.text = element_text(size=14)) 

### PLOT Time Series of DO vs Temperature
ggplot()+
  geom_line(data=PMK_do, aes(x=Order, y=DO, color="DO"), group=1) +
  geom_point(data=PMK_do, aes(x=Order, y=Visit, color="scrubbed"), size=1.25) +
  geom_hline(yintercept=5, color="red", size=0.5, linetype="dashed") +
  geom_line(data=PMK_do, aes(x=Order, y=Temp.C/1.5, color="Temp"), group=1) +
  labs(title = "PMK Dissolved Oxygen vs. Water Temperature",
       x = "2020 Water Year", 
       y = "DO Concentration (mg/L)") +
  scale_color_manual(values=c("DO"="slateblue3", "scrubbed"="black", "threshold"="red", "Temp" = "deepskyblue")) +
  scale_y_continuous(limits=c(0, 15)) +
  scale_x_continuous(breaks=c(0,2364,3708,5683,7069,8029,9651),labels=c(" "="9/27","2364"="11/15","3708"="12/13","5683"="1/23","7069"="2/21","8029"="3/12","9651"="4/15"))+        #run w/o to find breaks
  scale_y_continuous(sec.axis = sec_axis(~.*1.5, name = "Water Temperature (C)")) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5,size = 19), 
        plot.subtitle = element_text(hjust = 0.5, size = 17),
        axis.title.y = element_text(size=14),
        axis.title.x = element_text(size=14),
        legend.title = element_blank(),
        #legend.position = c(0.8, 0.9),
        axis.text.x = element_text(size=11.5),
        axis.text.y = element_text(size=11.5),
        legend.text = element_text(size=14)) 

### PLOT time series for winter diversion period (Nov 1-Mar 31)
ggplot()+
  geom_line(data=PMK_do, aes(x=Order, y=DO, color="DO"), group=1) +
  geom_point(data=PMK_do, aes(x=Order, y=Visit, color="scrubbed"), size=1.25) +
  geom_hline(yintercept=5, color="red", size=0.5, linetype="dashed") +
  geom_line(data=PMK_do, aes(x=Order, y=Temp.C/1.5, color="Temp"), group=1) +
  labs(title = "PMK Dissolved Oxygen vs. Water Temperature",
       x = "2020 Water Year", 
       y = "DO Concentration (mg/L)") +
  scale_color_manual(values=c("DO"="slateblue3", "scrubbed"="black", "threshold"="red", "Temp" = "deepskyblue")) +
  scale_y_continuous(limits=c(0, 15)) +
  scale_x_continuous(limits=c(1652,8947), breaks=c(1652,2364,3708,5683,7069,8029,8900),labels=c("1652"="11/1","2364"="11/15","3708"="12/13","5683"="1/23","7069"="2/21","8029"="3/12","8900"="3/31"))+        #run w/o to find breaks
  scale_y_continuous(sec.axis = sec_axis(~.*1.5, name = "Water Temperature (C)")) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5,size = 19), 
        plot.subtitle = element_text(hjust = 0.5, size = 17),
        axis.title.y = element_text(size=14),
        axis.title.x = element_text(size=14),
        legend.title = element_blank(),
        #legend.position = c(0.8, 0.9),
        axis.text.x = element_text(size=11.5),
        axis.text.y = element_text(size=11.5),
        legend.text = element_text(size=14)) 




###Working
install.packages("weathermetrics") 
library(weathermetrics)
install.packages("data.table") 
library(data.table)
install.packages("rlang") 
library(rlang)
install.packages("tidyverse") 
library(tidyverse)
install.packages("caTools") 
library(caTools)
install.packages("pracma") 
library(pracma)
library(lubridate)

# PMK_do2$number <- as.character(PMK_do2$number)
#PMK_do2$DO <- as.character(PMK_do2$DO)
#PMK_do2$Temp.C <- as.character(PMK_do2$Temp.C)
# ASSESS flows
BVR_s %>% top_n(20, CalcFlow.cfs) # highest flows (>25cfs) 12/12-12/13
BVR_s %>% top_n(10, Temp.C) # search for bad data point (logger out of water, etc.)
>>>>>>> fd0acc4b7c6fd97e86596fadc9bcc131641a51f2
