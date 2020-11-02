###############################################################################################
#  
# Author: Karly Harker
# Date: 2020-10-28
# Project: Columbia WQOs
# Purpose: Code follows the process I used to set WQOs. It mirrors
#          the WQO guidance document.

##### Note: Script for numeric guidelines

###############################################################################################

require(rems)
require(wqbc)
require(ggplot2)
require(tidyr)
require(dplyr)
require(magrittr)
require(NADA)
require(lubridate)
require(cowplot)
require(forcats)

rm(list=ls(all=TRUE))

setwd("C:\\Columbia_WQO")


db <- read.csv(file="2020-08-14_Columbia-WQ-DB_All-Data_FINAL.csv", header=TRUE, 
               fileEncoding="UTF-8-BOM")

db$date <- as_date(db$date)

working_data <- db 


#this is a fix for my db 
working_data <- dplyr::filter(working_data, !is.na(Parameter))
working_data <- dplyr::filter(working_data, !is.na(Sample.Type))
working_data <- dplyr::filter(working_data, !is.na(Site.Name))
working_data <- dplyr::filter(working_data, Sample.Type != "Effluent")
working_data<- dplyr::filter(working_data, Fraction != "EXTRACTABLE")
working_data<- dplyr::filter(working_data, Measurement != -0.999999)

###############################################################################
###############################################################################

#Set your POC once here 

x <- "ALUMINUM"
###### filter by parameter here: 

POC <- filter(working_data, Parameter.Sep == x)

####### calculate moving guideline here

# exp(1.6 - 3.327 * EMS_0004 + 0.402 * EMS_0004^2)

#Al %<>% mutate(AqLT = ifelse(Parameter == "pH", exp(1.6 - 3.327*Measurement + Measurement^2),NA))


###############################################################################
###############################################################################

###### add in 95% percentile info to filtered parameter

p95 <- read.csv(file="tables\\2020-10-26_Summary-Stats-Censored-Ref-DS.csv", header=TRUE, 
               fileEncoding="UTF-8-BOM")

p95 <- filter(p95, Parameter.Sep == x & Sample.Type != "Within IDZ") #filter for parameter of interest

POC %<>% left_join(p95, by = c("Parameter", "Parameter.Sep", "Fraction", 'Sample.Type'))

## print out difference between u/s and d/s 95% percentile ############### 

p95 <- p95 %>% 
  group_by(Parameter.Sep, Fraction, Sample.Type) %>%
  mutate(diff = r_95-lag(r_95))

##### add in guideline (not calculated)
## percent of data over guideline here 



# what % of current assimilative capacity is used by parameter - 95 p / guideline * 100




####################################################################################################
###PLOT: CONCENTRATION OVER TIME - basic, no guidelines/percentiles

ggplot(Al, aes(x = date, y = Measurement)) + 
  #expand_limits(y = c(0, 0.5)) +
  geom_point(aes(shape=Below.RDL, color=Fraction)) +
  scale_shape_manual(labels=c("Above RDL","Below RDL"), values=c(16,1))+
  scale_color_manual(labels=c("Dissolved","Total"), values=c("cadetblue3","coral"))+
  facet_grid(cols=vars(Fraction))+
  xlab("Date") +
  ylab("Ammonia Concentration (mg/L)")+
  theme_bw()+ 
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank())
  
ggsave("POC\\NO3\\2020-10-20_conc-vs-time_NO3-facet.tiff", units="in", 
       width=9, height=6, dpi=200, compression = 'lzw')

####################################################################################################
###CONCENTRATION OVER TIME w/ guideline & percentiles

Al %>% 
  group_by(Sample.Type, Parameter) %>% 
  summarize(q95 = quantile(Measurement, 0.95))

Al_d <- filter(Al, Fraction == "DISSOLVED")

Al_t <- filter(Al, Fraction == "TOTAL")

d <- ggplot(Al_d, aes(x = date, y = as.numeric(Measurement))) + 
  #expand_limits(y = c(0, 0.5)) +
  geom_point(aes(shape=Below.RDL, color=Fraction)) +
  scale_shape_manual(labels=c("Above RDL","Below RDL"), values=c(16,1))+
  scale_color_manual(values="cadetblue")+
  guides(color=FALSE)+
  geom_hline(yintercept=0.0392, color="black", size=1.25, linetype=2)+
  #geom_label(aes(x=as.Date("2020-04-01"),y=0.05,label = "AqLT-D"))+
  geom_hline(yintercept=0.0568, color="darkgrey", size=1.25, linetype="dotted")+
  #geom_label(aes(x=as.Date("2020-04-01"),y=0.0149,label = "R95-D"))+
  #geom_hline(yintercept=0.0526, color="coral3", size=1, linetype="dotted")+
  #geom_label(aes(x=as.Date("2020-04-01"),y=0.0526,label = "R95-T"))+
  #facet_grid(cols=vars(Fraction))+
  #ylim(0,0.41)+
  xlab("Date") +
  ylab("Dissolved Ammonia Concentration (mg/L)")+
  theme_bw()+ 
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank())
  

t<- ggplot(Al_t, aes(x = date, y = Measurement)) + 
  #expand_limits(y = c(0, 0.5)) +
  geom_point(aes(shape=Below.RDL, color=Fraction)) +
  scale_shape_manual(labels=c("Above RDL","Below RDL"), values=c(16,1))+
  scale_color_manual(values="coral")+
  #scale_color_manual(labels=c("Dissolved","Total"), values=c("cadetblue3","coral"))+
  #geom_hline(yintercept=0.05, color="cadetblue3", size=1, linetype=2)+
  #geom_label(aes(x=as.Date("2020-04-01"),y=0.05,label = "AqLT-D"))+
  #geom_hline(yintercept=0.0149, color="deepskyblue4", size=1, linetype="dotted")+
  #geom_label(aes(x=as.Date("2020-04-01"),y=0.0149,label = "R95-D"))+
  #geom_hline(yintercept=0.0526, color="darkgrey", size=1.25, linetype="dotted")+
  #geom_label(aes(x=as.Date("2020-04-01"),y=0.0526,label = "R95-T"))+
  #facet_grid(cols=vars(Fraction))+
  #ylim(0,0.41)+
  xlab("Date") +
  ylab("Total Ammonia Concentration (mg/L)")+
  guides(color=FALSE)+
  theme_bw()+ 
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank())
  

plot_grid(d, t, labels = c("Dissolved", "Total"), label_size = 10)

#ggsave("POC\\NO3\\2020-10-20_conc-vs-time_NO3.tiff", units="in", 
       # width=9, height=6, dpi=200, compression = 'lzw')

####################################################################################################    #####################
#MONTHLY VARIATION - BOXPLOT

ggplot(Al, aes(as.factor(month), Measurement, group = interaction(month, Fraction), 
       fill = Fraction)) + 
  # ggtitle("Monthly Variation - Total Arsenic")+
  xlab("Month of Year") +
  ylab("Aluminum Concentration (mg/L)") +
  #geom_jitter(alpha = 0.3, color = c("darkslategray4", "coral")) +
  geom_boxplot() +
  scale_fill_manual(labels=c("Dissolved","Total"), values=c("cadetblue3","coral"))+
  geom_hline(yintercept=0.0149, color="cadetblue", size=1.25, linetype="dotted")+
  geom_hline(yintercept=0.0526, color="coral3", size=1.25, linetype="dotted")+
  theme_bw()+ 
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank())
  #geom_point(aes(colour = factor(Fraction)))+
  #geom_violin(aes(group=month),alpha = 0) +
  #geom_hline(yintercept=5, color="black", size=1, linetype=2)+
  #geom_label(aes(x="2",y=5,label = "LCR WQO & ST AqL"))

#ggsave("monthly\\2020-10-26_Al-monthly-95p.tiff", units="in", 
      # width=9, height=6, dpi=200, compression = 'lzw')

####################################################################################################
#YEAR TO YEAR VARIATION - BOXPLOT

ggplot(Al, aes(as.factor(year), Measurement, group = interaction(year, Fraction), 
                  fill = Fraction)) + 
 # geom_boxplot(aes(group=month)) + 
  #ggtitle("Year to Year Variation - Total Arsenic")+
  xlab("Year of Sampling") +
  ylab("Aluminum Concentration in mg/L") +
  geom_boxplot() +
  scale_fill_manual(labels=c("Dissolved","Total"), values=c("cadetblue3","coral"))+
  geom_hline(yintercept=0.0149, color="cadetblue", size=1.25, linetype="dotted")+
  geom_hline(yintercept=0.0526, color="coral3", size=1.25, linetype="dotted")+
  theme_bw()+ 
  #geom_hline(yintercept=5, color="black", size=1, linetype=2) +
  #geom_label(aes(x="2006",y=5,label = "LCR WQO")))
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank())

#ggsave("POC\\Al\\2020-10-26_Al-yearly-boxplots-w-95p.tiff", units="in", width=9, height=6, dpi=300, compression = 'lzw')


####################################################################################################
#Concentration over distance - Scatter


Al %>%
  mutate(Site.Name = fct_reorder(Site.Name, desc(Site.Order))) %>%
  ggplot(aes(Site.Name, Measurement, fill=Fraction, group = interaction(Site.Name, Fraction))) +
  geom_boxplot() +
  scale_fill_manual(labels=c("Dissolved","Total"), values=c("cadetblue3","coral"))+
  xlab("Sampling Sites - Upstream to Downstream") +
  ylab("Aluminum Concentration in mg/L") +
  theme_bw() +
  theme(axis.text.x=element_text(angle = 45, vjust = 1, hjust=1))+
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank()) +
  geom_hline(yintercept=0.0149, color="cadetblue", size=1.25, linetype="dotted")+
  geom_hline(yintercept=0.0526, color="coral3", size=1.25, linetype="dotted")


#ggsave("POC\\Al\\2020-10-26_Al-us-ds-boxplots-w-95p.tiff", units="in", width=9, height=6, dpi=300, compression = 'lzw')


##########################################################################################################


