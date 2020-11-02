#Copyright 2017 Province of British Columbia
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

############################################################################### # Module 3. INITIAL DATA VISUALIZATION
############################################################################### 
## ORGANIZE DATASET BY UNITS TO PLOT
## 
## Set a vector of the parameters we are interested in
## Do this manually by looking at the `parameters` and `all_data_limits` dataframes
## 
## PLOT DATA 1 (in a loop, make one plot per parameter)
## First set working directory to save plots to. This section only plots with clean data. Water quality guidelines not taken into account.

#setwd('XXX/FolderX')

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

setwd("C:\\Columbia_WQO\\")


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

###### filter by parameter here: 

NO3 <- filter(working_data, Parameter.Sep == "AMMONIA")

####### calculate moving guideline here

# exp(1.6 - 3.327 * EMS_0004 + 0.402 * EMS_0004^2)

#Al %<>% mutate(AqLT = ifelse(Parameter == "pH", exp(1.6 - 3.327*Measurement + Measurement^2),NA))

####################################################################################################
###CONCENTRATION OVER TIME

ggplot(NO3, aes(x = date, y = Measurement)) + 
  #expand_limits(y = c(0, 0.5)) +
  geom_point(aes(shape=Below.RDL, color=Fraction)) +
  scale_shape_manual(labels=c("Above RDL","Below RDL"), values=c(16,1))+
  scale_color_manual(labels=c("Dissolved","Total"), values=c("cadetblue3","coral"))+
  #geom_point(colour = 'blue') +
  ##geom_hline(data = mgL_plots, aes(yintercept = UpperLimit), colour = "red", linetype = "dashed", show.legend = TRUE) +
  #geom_hline(yintercept=5, color="black", size=1, linetype=2)+
  #geom_label(aes(x=as.Date("2007-04-25"),y=5,label = "LCR WQO & ST AqL"))+
  #ggtitle("Total Arsenic Concentration in ug/L") +
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

NO3 %>% 
  group_by(Sample.Type, Parameter) %>% 
  summarize(q95 = quantile(Measurement, 0.95))

NO3_d <- filter(NO3, Fraction == "DISSOLVED")

NO3_t <- filter(NO3, Fraction == "TOTAL")

d <- ggplot(NO3_d, aes(x = date, y = as.numeric(Measurement))) + 
  #expand_limits(y = c(0, 0.5)) +
  geom_point(aes(shape=Below.RDL, color=Fraction)) +
  scale_shape_manual(labels=c("Above RDL","Below RDL"), values=c(16,1))+
  scale_color_manual(values="cadetblue")+
  guides(color=FALSE)+
  #geom_hline(yintercept=0.05, color="black", size=1.25, linetype=2)+
  #geom_label(aes(x=as.Date("2020-04-01"),y=0.05,label = "AqLT-D"))+
  #geom_hline(yintercept=0.0149, color="darkgrey", size=1.25, linetype="dotted")+
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
  

t<- ggplot(NO3_t, aes(x = date, y = Measurement)) + 
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

ggsave("POC\\NO3\\2020-10-20_conc-vs-time_NO3.tiff", units="in", 
        width=9, height=6, dpi=200, compression = 'lzw')

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

ggsave("monthly\\2020-10-26_Al-monthly-95p.tiff", units="in", 
       width=9, height=6, dpi=200, compression = 'lzw')

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

ggsave("POC\\Al\\2020-10-26_Al-yearly-boxplots-w-95p.tiff", units="in", width=9, height=6, dpi=300, compression = 'lzw')


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


ggsave("POC\\Al\\2020-10-26_Al-us-ds-boxplots-w-95p.tiff", units="in", width=9, height=6, dpi=300, compression = 'lzw')


##########################################################################################################


cad <- filter(full, Parameter == "E COLI" | Parameter == "ENTEROCOCCUS" | Parameter == "FECAL COLIFORM") 
              #& Sample.Type != "Within IDZ")

cad <- filter(cad, Mod.Measurement > 0 & Mod.Measurement < 100) 
#cad <- filter(cad, Mod.Measurement > 0) 
#based on IQR method of outlier detection

sum <- group_by(cad, Parameter) %>% summarise(mean=mean(Mod.Measurement), median=median(Mod.Measurement), 
                           sd=sd(Mod.Measurement),
                           min=min(Mod.Measurement), max=max(Mod.Measurement),
                           quantile95=quantile(Mod.Measurement,probs=0.95, na.rm=TRUE), 
                           quantile90=quantile(Mod.Measurement,probs=0.90, na.rm=TRUE),
                           quantile25=quantile(Mod.Measurement,probs=0.25, na.rm=TRUE),
                           quantile75=quantile(Mod.Measurement,probs=0.75, na.rm=TRUE),
                           IQR=IQR(Mod.Measurement, na.rm=TRUE), unit=first(Guideline.Unit), n=n())

####################################################################################################
###CONCENTRATION OVER TIME

plot <- ggplot(cad, aes(x = as.Date(date), y = Mod.Measurement)) + 
  #expand_limits(y = c(0, 0.5)) +
  geom_point(aes(shape=Below.RDL., color=as.factor(Parameter))) +
  scale_shape_manual(labels=c("Above RDL","Below RDL"), values=c(16,1))+
  #geom_point(colour = 'blue') +
  ##geom_hline(data = mgL_plots, aes(yintercept = UpperLimit), colour = "red", linetype = "dashed", show.legend = TRUE) +
  geom_hline(yintercept=100, color="black", size=1, linetype=2)+
  geom_hline(yintercept=25, color="chartreuse4", size=1, linetype=3)+
  #geom_hline(yintercept=0.156, color="black", size=1, linetype=4)+
  geom_label(aes(x=as.Date("2007-04-03"),y=100,label = "LCR WQO - E. Coli & Fecal Coliforms"))+
  geom_label(aes(x=as.Date("2006-04-03"),y=25,label = "LCR WQO - Enterococci"))+
  #geom_label(aes(x=as.Date("2006-04-25"),y=0.156,label = "BC AQ - C, D"))+
  ggtitle("Microbial Parameters") +
  xlab("Date") +
  ylab("Concentration in cfu/100 mL")

plot(plot)


plot + theme(
  legend.title=element_blank()
)

ggsave("H:\\ColumbiaWQO_DB\\Parameters\\MicrobialScatter.tiff", units="in", width=9, height=6, dpi=300, compression = 'lzw')

####################################################################################################
#MONTHLY VARIATION - BOXPLOT

plot<-ggplot(cad, aes(as.factor(month), Mod.Measurement, color=as.factor(Parameter))) + 
  # geom_boxplot(aes(group=month)) + 
  ggtitle("Monthly Variation - Microbial Parameters")+
  xlab("Month of Year") +
  ylab("Concentration in cfu/100 mL") +
  geom_jitter(alpha = 0.3) +
  geom_violin(aes(group=month),alpha = 0) +
  geom_hline(yintercept=100, color="black", size=1, linetype=2)+
  geom_hline(yintercept=25, color="chartreuse4", size=1, linetype=3)+
  #geom_hline(yintercept=0.156, color="black", size=1, linetype=4)+
  geom_label(aes(x="5",y=100,label = "LCR WQO - E. Coli & Fecal Coliforms"))+
  geom_label(aes(x="5",y=25,label = "LCR WQO - Enterococci"))
  #geom_hline(yintercept=7.5, color="black", size=1, linetype=2)+
  #geom_hline(yintercept=33, color="black", size=1, linetype=3)+
  #geom_hline(yintercept=0.383, color="black", size=1, linetype=3)+
  #geom_hline(yintercept=0.156, color="black", size=1, linetype=4)+
  #geom_label(aes(x="2",y=7.5,label = "BC AQ - C, T"))
  #geom_label(aes(x="2",y=33,label = "BC AQ - A, T"))
  #geom_label(aes(x="1",y=0.15,label = "LCR WQO"))
plot + theme(
  legend.title=element_blank()
)


ggsave("H:\\ColumbiaWQO_DB\\Parameters\\NH3MonthBoxP.tiff", units="in", width=9, height=6, dpi=150, compression = 'lzw')

####################################################################################################
#YEAR TO YEAR VARIATION - BOXPLOT

plot<-ggplot(cad, aes(as.factor(year), Mod.Measurement, color=as.factor(Parameter))) + 
  # geom_boxplot(aes(group=month)) + 
  ggtitle("Year to Year Variation - Microbial Parameters")+
  xlab("Year of Sampling") +
  ylab("Concentration in cfu/100 mL") +
  geom_jitter(alpha = 0.3) +
  geom_violin(aes(group=year),alpha = 0)+
  geom_hline(yintercept=100, color="black", size=1, linetype=2)+
  geom_hline(yintercept=25, color="chartreuse4", size=1, linetype=3)+
  #geom_hline(yintercept=0.156, color="black", size=1, linetype=4)+
  geom_label(aes(x="2006",y=100,label = "LCR WQO - E. Coli & Fecal Coliforms"))+
  geom_label(aes(x="2006",y=25,label = "LCR WQO - Enterococci"))#+
  #geom_hline(yintercept=7.5, color="black", size=1, linetype=2)+
  #geom_hline(yintercept=33, color="black", size=1, linetype=3)+
  #geom_hline(yintercept=0.383, color="black", size=1, linetype=3)+
  #geom_hline(yintercept=0.156, color="black", size=1, linetype=4)
  #geom_label(aes(x=as.Date("2006-04-25"),y=0.03,label = "LCR WQO - C, T"))+
  #geom_label(aes(x=as.Date("2006-04-25"),y=0.383,label = "BC AQ - A, D"))+
  #geom_label(aes(x=as.Date("2006-04-25"),y=0.156,label = "BC AQ - C, D"))
  #geom_label(aes(x="2006",y=7.5,label = "BC AQ - C, T"))
  #geom_label(aes(x="2006",y=33,label = "BC AQ - A, T"))

plot + theme(
  legend.title=element_blank()
)


ggsave("H:\\ColumbiaWQO_DB\\Parameters\\MicrobYearBoxP.tiff", units="in", width=9, height=6, dpi=300, compression = 'lzw')

#cadDC <- filter(cad, Parameter == "ZINC TOTAL" & Mod.Measurement >= 7.5)
#cadDA <- filter(cad, Parameter == "ZINC TOTAL" & Mod.Measurement >= 33)
#cadTC <- filter(cad, Parameter == "LEAD TOTAL" & Mod.Measurement >= 4.972)
#cadTA <- filter(cad, Parameter == "LEAD TOTAL" & Mod.Measurement >= 42.61)


####################################################################################################
#Concentration over distance - Scatter

plot <- ggplot(cad, aes(x = Distance, y = Mod.Measurement)) + 
  #expand_limits(y = c(0, 0.5)) +
  geom_point(aes(shape=Below.RDL., color=as.factor(Parameter)), data=cad, position = position_jitter(w = 0.1, h = 0)) +
  scale_shape_manual(labels=c("Above RDL","Below RDL"), values=c(16,1))+
  geom_hline(yintercept=100, color="black", size=1, linetype=2)+
  geom_hline(yintercept=25, color="chartreuse4", size=1, linetype=3)+
  #geom_hline(yintercept=0.156, color="black", size=1, linetype=4)+
  geom_label(aes(x=17.5,y=100,label = "LCR WQO - E. Coli & Fecal Coliforms"))+
  geom_label(aes(x=15,y=25,label = "LCR WQO - Enterococci"))+
  #geom_point(colour = 'blue') +
  ##geom_hline(data = mgL_plots, aes(yintercept = UpperLimit), colour = "red", linetype = "dashed", show.legend = TRUE) +
  #geom_hline(yintercept=7.5, color="black", size=1, linetype=2)+
  #geom_hline(yintercept=33, color="black", size=1, linetype=3)+
  #geom_hline(yintercept=0.383, color="black", size=1, linetype=3)+
  #geom_hline(yintercept=0.156, color="black", size=1, linetype=4)+
  #geom_label(aes(x=5, y=1,label = "LCR WQO - C, T"))+
  #geom_label(aes(x=5,y=33,label = "BC AQ - C, T"))+
  #geom_label(aes(x=5,y=7.5,label = "BC AQ - C, T"))+
  ggtitle("Microbial Parameters over LCR Reach") +
  xlab("Distance from Hugh Keenleyside Dam") +
  ylab("Concentration in cfu/100 mL")+
  xlim(10,35)

plot(plot)


plot + theme(
  legend.title=element_blank()
)


ggsave("H:\\ColumbiaWQO_DB\\Parameters\\MicrobDistNoIDZ.tiff", units="in", width=9, height=6, dpi=300, compression = 'lzw')


write.csv(cad,"H:\\ColumbiaWQO_DB\\Parameters\\NH3_T_D.csv")
write.csv(sum,"H:\\ColumbiaWQO_DB\\Parameters\\NH3_sum.csv")

cad_ <- filter(cad, Mod.Measurement > 0.03)

############################################################################################################################



wqp <- ggplot(data=wqtf, aes(x=month, y=Imidacloprid, group=Stream : Position)) +
  geom_point(aes(shape=Position:I_Detect, color=Exceed_), position=position_dodge(width=0.5), size=2) +
  geom_line() +
  scale_shape_manual(labels=c("Downstream - Detected","Downstream - Not detected",
                              "Upstream - Detected", "Upstream - Not detected"), values=c(16,1,17,2))+
  scale_color_manual(labels=c("Below Guidelines", "Exceeds CCME Guideline", "Exceeds PMRA Endpoint"), 
                     values=c("black", "blue", "red"))+ #exceedances show up as red
  xlab("Sample Date") +
  ylab("Concentration of Imidacloprid (ug/L)")+
  geom_hline(yintercept=0.005, color="#252525", linetype=2)+ #Detection Limit
  geom_hline(yintercept=0.41, color="red", size=1, linetype=2)+ #PMRA guideline
  geom_hline(yintercept=0.23, color="blue", size=1, linetype=2)+
  #geom_hline(yintercept=0.01, color="#8856a7", linetype=2)+ #EPA chronic guideline
  #geom_hline(yintercept=0.385, color="#66c2a4")+ #EPA acute guideline 
  facet_grid(Stream~Year)

wqp + theme(
  legend.title=element_blank(),
  panel.spacing.x = unit(5, "mm"),
  panel.background = element_rect(fill="white", size=0.5, linetype = "solid", colour = "grey50")
)






###########################################################################################################
###########################################################################################################
###########################################################################################################
g <- g %>% rename(Samples = n_distinct(ALS.Sample.ID)) 

g <- ggplot(samples, aes(samples)))
# Number of cars in each class:
g + geom_bar()


## mg/L plots
site_mgL <- filter(all_data_clean, Variable %in% mgL)

for (v in mgL) {
  #change_units <- ifelse(E303845_ugL$Units == "ug/L", E303845_ugL$Value/1000, E303845_ugL$Value). 
  ## This is making the values to the power of 100. Only have to change if want unit other than what guideline is expressed is.
  mgL_plots <- filter(site_mgL, Variable == v)
  plot <- ggplot(mgL_plots, aes(x = Date, y = Value)) + 
    expand_limits(y = c(0, 0.5)) +
    geom_point(colour = 'blue') +
    ##geom_hline(data = mgL_plots, aes(yintercept = UpperLimit), colour = "red", linetype = "dashed", show.legend = TRUE) +
    ggtitle(v, "mg/L") + scale_x_date(labels = date_format("%b %Y")) +
    xlab("Date") +
    ylab("Value")
  plot(plot)
  ggsave(filename = paste0(v,".png"), plot = plot, units= "in")
}

## ug/L parameters plots
site_ugL <- filter(all_data_clean, Variable %in% ugL)

for (v in ugL) {
  ugL_plots <- filter(site_ugL, Variable == v)
  plot <- ggplot(ugL_plots, aes(x = Date, y = Value)) + 
    expand_limits(y = c(0, 10)) +
    geom_point(colour = 'blue') +
    ##geom_hline(data = ugL_plots, aes(yintercept = UpperLimit), colour = "red", linetype = "dashed", show.legend = TRUE) +
    ggtitle(v, "ug/L") + scale_x_date(labels = date_format("%b %Y")) +
    xlab("Date") +
    ylab("Value")
  plot(plot)
  ggsave(filename = paste0(v,".png"), plot = plot, units= "in")
}

## SINGLE PLOTS for single unit parameters
## pH
site_pH <-filter(all_data_clean, Variable=="pH") 
pHplot <- ggplot(site_pH, aes(x=Date, y=Value)) + 
  geom_point(colour="blue") + 
  expand_limits(y = c(0, 15)) + 
  scale_x_date(labels = date_format("%b %Y")) +
  xlab("Date") + 
  ylab("pH")
plot(pHplot)
ggsave(filename = "pHplot.png", plot = pHplot, units= "in") 

## Temperature
E257094_temp <-filter(all_data_clean, Variable=="Temperature") 
tempplot <- ggplot(E206585_temp, aes(x=Date, y=Value)) + geom_point(colour="blue") + 
  scale_x_date(labels = date_format("%b %Y"))+
  xlab("Date") + ylab("Temperature (Celcius)")
plot(tempplot)
ggsave(filename = "temp_plot.png", plot = tempplot, units= "in") 

## Conductivity
E257094_cond <-filter(all_data_clean, Variable=="Conductance")
condplot <- ggplot(E206585_cond, aes(x=Date, y=Value)) + geom_point(colour="blue") + 
  xlab("Date") + ylab("Conductivity (us/cm)")
plot(condplot)
ggsave(filename = "cond_plot.png", plot = pHplot, units= "in", dpi = 120) 

## E coli
E257094_Ecol <-filter(all_data_clean, Variable=="E. coli")
Ecolplot <- ggplot(E206585_Ecol, aes(x=Date, y=Value)) + geom_point(colour="blue") + 
  scale_x_date(labels = date_format("%b %Y"))+
  ylab("(E. coli per 100 mL)")
plot(Ecolplot)
ggsave(filename = "Ecoli_plot.png", plot = Ecolplot, units= "in") 

## Turbidity 
E257094_turb <-filter(all_data_clean, Variable=="Turbidity")
turbplot <- ggplot(E206585_turb, aes(x=Date, y=Value)) + geom_point(colour="blue") + 
  scale_x_date(labels = date_format("%b %Y")) + ylab("Turbidity (NTU)")
plot(turbplot) 
ggsave(filename = "turbs_plot.png", plot = turbplot, units= "in") 

## Hardness Dissolved
hardness <- filter(all_data_clean, Variable=="Hardness Dissolved")
hard_plot <- ggplot(hardness, aes(x=Date, y=Value))+ geom_point(colour="blue") + 
  scale_x_date(labels = date_format("%b %Y")) + 
  ylab("Dissolved Hardness (mg/L)")
plot(hard_plot)

## PLOT DATA 2 - WITH WQGS
## Run plots again to add WQG line onto plot for those parameters that have WQGs calculated. 
## Have to make sure you've run 'all_data_limits' first (in 02_clean_calc_WQO)

#setwd('XXX/FolderX')

## mg/L plots
site_mgL <- filter(all_data_limits, Variable %in% mgL_withWQG)

for (v in mgL_withWQG) {
  #change_units <- ifelse(E303845_ugL$Units == "ug/L", E303845_ugL$Value/1000, E303845_ugL$Value). 
  ##This is making the values to the power of 100. Only have to change if want unit other than what 
  ##limit is expressed is.
  mgL_plots_WQGs <- filter(site_mgL, Variable == v)
  plot <- ggplot(mgL_plots_WQGs, aes(x = Date, y = Value)) + 
    expand_limits(y = c(0, 0.5)) +
    geom_point(colour = 'blue') +
    geom_hline(data = mgL_plots_WQGs, aes(yintercept = UpperLimit), colour = "red", 
               linetype = "dashed", show.legend = TRUE) +
    ggtitle(v, "mg/L") + scale_x_date(labels = date_format("%b %Y")) +
    xlab("Date") +
    ylab("Value")
  plot(plot)
  ggsave(filename = paste0(v,".png"), plot = plot, units= "in")
}

## ug/L parameters plots
site_ugL <- filter(all_data_limits, Variable %in% ugL_withWQG)
for (v in ugL_withWQG) {
  ugL_plots_WQGs <- filter(site_ugL, Variable == v)
  plot <- ggplot(ugL_plots_WQGs, aes(x = Date, y = Value)) + 
    expand_limits(y = c(0, 10)) +
    geom_point(colour = 'blue') +
    geom_hline(data = ugL_plots_WQGs, aes(yintercept = UpperLimit), colour = "red", 
               linetype = "dashed", show.legend = TRUE) +
    ggtitle(v, "ug/L") + scale_x_date(labels = date_format("%b %Y")) +
    xlab("Date") +
    ylab("Value")
  plot(plot)
  ggsave(filename = paste0(v,".png"), plot = plot, units= "in")
  
}
  
ggplot(locations, aes(x=as.factor(year), y=n)) + 
    geom_bar(stat = "identity", fill="darkslategray3")+
    ggtitle("Sampling Effort by Year in the LCR") +
    xlab("Year") +
    ylab("Sampling Effort (n observations)")
