# GITHUB PRACTICE
# SCRIPT to plot dissolved oxygen (DO) data collected from Peacemaker Ranch (PMK)
# AKL 2020

# ADD Libraries
install.packages("dplyr") 
library(dplyr)
install.packages("ggplot2") 
library(ggplot2)

# ADD Data
PMK_do <- read.csv("C:/GitHub_projects/Little_Shasta_River/data/PMK_processed/PMK_DO.csv")

### PLOT full time series of DO (9/27/2019-4/15/2020)
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

ggsave("PMK_DO_full.png", 
       plot=last_plot(), device="png", path="/GitHub_projects/Little_Shasta_River/output/", scale=1, width=6, height=3.5, units="in", dpi=300, limitsize=TRUE)

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

ggsave("PMK_DOvT_full.png", 
       plot=last_plot(), device="png", path="/GitHub_projects/Little_Shasta_River/output/", scale=1, width=6, height=3.5, units="in", dpi=300, limitsize=TRUE)

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

ggsave("PMK_DOvT_2020diversionperiod.png", 
       plot=last_plot(), device="png", path="/GitHub_projects/Little_Shasta_River/output/", scale=1, width=6, height=3.5, units="in", dpi=300, limitsize=TRUE)


