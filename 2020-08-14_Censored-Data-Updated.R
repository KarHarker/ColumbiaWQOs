# Combine EMS and CABIN data, identify outliers, and summarize data by region

require(rems)
require(wqbc)
require(ggplot2)
require(tidyr)
require(dplyr)
require(magrittr)
require(NADA)
require(lubridate)

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

#working_data <- dplyr::filter(working_data, Sample.Type != "Within IDZ")
#write.csv(working_data, file="2020-08-21_Columbia-WQ-DB_All-Data_TTCopy.csv")

# Calculate averages of replicates
working_data %<>%
  group_by(date, year, month, day, Site.Name, longitude, latitude, Sampled.By, Sample.Type, 
           Parameter, Parameter.Sep, Fraction, Compound.Type, Guideline.Unit, Detection.Limit, Below.RDL, Site.Order) %>%
  summarise(Result = max(Measurement, na.rm = TRUE),
            N = length(Measurement),
            Censored = ifelse(all(Below.RDL == TRUE), TRUE, FALSE)) %>%
  ungroup()



## MODIFY BASED UPON NEW WORKING DATASET
# Summary Stats for table 1 - based upon uncensored data 

table.1 <- working_data %>% group_by(Parameter, Parameter.Sep, Fraction, Site.Name, Site.Order) %>%
  summarise(samples = length(date),
            start.date = min(date),
            end.date = max(date), 
            mean=mean(Result), 
            median=median(Result), 
            sd=sd(Result),
            min=min(Result), 
            max=max(Result),
            quantile95=quantile(Result,probs=0.95, na.rm=TRUE), 
            quantile90=quantile(Result,probs=0.90, na.rm=TRUE),
            quantile25=quantile(Result,probs=0.25, na.rm=TRUE),
            quantile75=quantile(Result,probs=0.75, na.rm=TRUE),
            IQR=IQR(Result, na.rm=TRUE), 
            unit=first(Guideline.Unit), n=n()) %>%
    ungroup()

#################################################################

cens_data <- filter(working_data, Below.RDL == TRUE)

table.2 <- cens_data %>% group_by(Parameter, Parameter.Sep, Fraction, Site.Name, Site.Order) %>%
  summarise(N.ND = length(Result),
            ND.MIN = min(Result),
            ND.MAX = max(Result))

table.1 %<>% left_join(table.2, by = c("Parameter", "Parameter.Sep", "Fraction", 'Site.Name', "Site.Order"))

table.1 %<>% arrange(Parameter.Sep, Fraction, Site.Order, .by_group = TRUE)

write.csv(table.1, file="tables\\2020-08-14_Table-1_Summary-Stats_No-Censor.csv")

# Determine non-detects by station to select method.
# Count samples that are D vs ND
method_selection <- working_data %>%
  group_by(Parameter, Parameter.Sep, Fraction, Site.Name, Site.Order) %>%
  summarise(N = length(Result),
            N.ND = length(Result[Below.RDL == TRUE])) %>%
  ungroup() %>%
  mutate(PROP.ND = N.ND / N,
         METHOD = ifelse(PROP.ND == 1, "0.5 MDL",
                         ifelse(PROP.ND > 0 & N - N.ND < 3, "0.5 MDL - MEAN",
                                ifelse(PROP.ND > 0 & N - N.ND > 3, "ROS", "MEAN"))))


# Join to data set
working_data %<>% left_join(method_selection, by = c("Parameter", "Parameter.Sep", "Fraction", 'Site.Name', "Site.Order"))

method_selection%<>% arrange(Parameter.Sep, Fraction, Site.Order, .by_group = TRUE)

write.csv(method_selection, file="tables\\2020-08-14_Method-Selection.csv")


# Create Table 6.1 (Breakdown by method)
method_summary <- method_selection %>%
  group_by(METHOD, Parameter, Parameter.Sep, Fraction) %>%
  summarise(STN.COUNT = length(unique(Site.Name)),
            SMP.COUNT = sum(N))

write.csv(method_summary, file="tables\\2020-08-14_Method-Summary.csv")



# Calculate Station Means
# GROUP 1 - STATIONS WHERE ALL DATA IS CENSORED, PROPORTION OF ND=1; mean is min MDL=0.00005
g1_data <- working_data %>% filter(METHOD == "0.5 MDL") %>%
  group_by(Parameter, Parameter.Sep, Fraction, Site.Name, Site.Order) %>%
  summarise(r_mean = signif(min(Result / 2), 3), #takes 1/2 of minimum MDL
            r_med = signif(median(Result, na.rm = TRUE), 3),
            r_95 = signif(quantile(Result, prob=0.95, na.rm = TRUE), 3),
            r_90 = signif(quantile(Result, prob=0.90, na.rm = TRUE), 3),
            r_10 = signif(quantile(Result, prob=0.10, na.rm = TRUE), 3),
            r_25 = signif(quantile(Result, prob=0.25, na.rm = TRUE), 3),
            r_75 = signif(quantile(Result, prob=0.75, na.rm = TRUE), 3),
            r_min = signif(min(Result, na.rm = TRUE), 3), #indicates this value is half lowest MDL
            r_max = signif(max(Result, na.rm = TRUE), 3),
            r_n = length(Result)) %>%
  ungroup() %>%
  mutate(CENSOR.METHOD = "0.5 MDL")

# GROUP 2 - Stations where ND < 3, substitute detection limit,  and calculate
# arithmetic mean
g2_data <- working_data %>% filter(METHOD == "0.5 MDL - MEAN") %>%
  mutate(CENS.VAL = ifelse(Below.RDL == TRUE, signif(Result / 2, 3),
                           Result)) %>%
  group_by(Parameter, Parameter.Sep, Fraction, Site.Name, Site.Order) %>%
  summarise(r_mean = signif(mean(CENS.VAL, 3)),
            r_med = signif(median(CENS.VAL, na.rm = TRUE), 3),
            r_95 = signif(quantile(CENS.VAL, prob=0.95, na.rm = TRUE), 3),
            r_90 = signif(quantile(CENS.VAL, prob=0.90, na.rm = TRUE), 3),
            r_10 = signif(quantile(CENS.VAL, prob=0.10, na.rm = TRUE), 3),
            r_25 = signif(quantile(CENS.VAL, prob=0.25, na.rm = TRUE), 3),
            r_75 = signif(quantile(CENS.VAL, prob=0.75, na.rm = TRUE), 3),
            r_min = signif(min(CENS.VAL, na.rm = TRUE), 3), #indicates this value is half lowest MDL
            r_max = signif(max(CENS.VAL, na.rm = TRUE), 3),
            r_n = length(CENS.VAL))%>%
  ungroup() %>%
  mutate(CENSOR.METHOD = "0.5 MDL - MEAN")

# GROUP 3 - STATIONS WHERE %ND is >0% and <100% AND D >= 3, calculate station
# means using ros in package NADA
g3_data <- working_data %>% filter(METHOD == "ROS")

# Create function for running ROS and returning the calculated mean in a data
# frame (do() requires that a data frame is returned)

calc_ros_mean <- function(result, censor) {

  x_ros <- ros(result, censor)
  r_mean <- signif(mean(x_ros, na.rm = TRUE), 3)
  r_med <- signif(median(x_ros, na.rm = TRUE), 3)
  r_95 <- signif(quantile(x_ros, prob=0.95, na.rm = TRUE), 3)
  r_90 <- signif(quantile(x_ros, prob=0.90, na.rm = TRUE), 3)
  r_10 <- signif(quantile(x_ros, prob=0.10, na.rm = TRUE), 3)
  r_25 <- signif(quantile(x_ros, prob=0.25, na.rm = TRUE), 3)
  r_75 <- signif(quantile(x_ros, prob=0.75, na.rm = TRUE), 3)
  r_min <- signif(min(x_ros$modeled, na.rm = TRUE), 3)
  r_max <- signif(max(x_ros$modeled, na.rm = TRUE), 3)

  return(data.frame(r_mean, r_med, r_95, r_90, r_10, r_25, r_75, r_min, r_max))

}

#######################################################
# Run analysis


g3_data %<>%
  group_by(Parameter, Parameter.Sep, Fraction, Site.Name, Site.Order) %>%
  do(calc_ros_mean(.$Result, .$Below.RDL)) %>%
  ungroup() %>%
  mutate(CENSOR.METHOD = "ROS")

#the n() was not working within the calc_ros_mean function, had to add it afterwards 
g3_data %<>% left_join(method_selection, by = c("Parameter", "Parameter.Sep", 
                                                "Fraction", 'Site.Name', "Site.Order"))

names(g3_data)[names(g3_data)=="N"]<- "r_n" #renaming column to match g1,g2,g4_data

g3_data %<>% select(-c(N.ND, PROP.ND, METHOD))

# GROUP 4 - Stations where ND = 0%
g4_data <- working_data %>% filter(METHOD == "MEAN") %>%
  group_by(Parameter, Parameter.Sep, Fraction, Site.Name, Site.Order) %>%
  summarise(r_mean = signif(mean(Result, 3)),
            r_med = signif(median(Result, na.rm = TRUE), 3),
            r_95 = signif(quantile(Result, prob=0.95, na.rm = TRUE), 3),
            r_90 = signif(quantile(Result, prob=0.90, na.rm = TRUE), 3),
            r_10 = signif(quantile(Result, prob=0.10, na.rm = TRUE), 3),
            r_25 = signif(quantile(Result, prob=0.25, na.rm = TRUE), 3),
            r_75 = signif(quantile(Result, prob=0.75, na.rm = TRUE), 3),
            r_min = signif(min(Result, na.rm = TRUE), 3), #indicates this value is half lowest MDL
            r_max = signif(max(Result, na.rm = TRUE), 3),
            r_n = length(Result)) %>%
  ungroup() %>%
  mutate(CENSOR.METHOD = "MEAN")

# COMBINE MEANS INTO TABLE,ADD GEOGRAPHIC INFO & CALCULATE SUMMARY STATS
summary_stats <- bind_rows(g1_data, g2_data, g3_data, g4_data)
rm(g1_data, g2_data, g3_data, g4_data)

#Modify table if you want to add # of ND and units back in 
summary_stats %<>% left_join(select(table.1, Parameter, Section,
                             N.ND, unit)) %>%
                    mutate(PROP.N = signif(r_n / N.ND), 3)


summary_stats %<>% arrange(Parameter.Sep, Fraction, Site.Order, .by_group = TRUE)

write.csv(summary_stats, file="tables\\2020-08-14_Summary-Stats-Censored.csv")




                          