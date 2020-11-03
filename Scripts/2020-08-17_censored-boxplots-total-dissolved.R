#censored boxplots


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

sumstats <- read.csv(file="tables\\2020-08-14_Summary-Stats-Censored.csv", header=TRUE, 
                        fileEncoding="UTF-8-BOM") 

sumstats<- dplyr::filter(sumstats, Fraction != "EXTRACTABLE")

write.csv(sumstats, "tables\\2020-08-21_Summary-Stats-Censored_TTcopy.csv")
#####################################################################################################
#####################################################################################################
#p <- sumstats %>% filter(Parameter.Sep == "ANTIMONY")

parameters <- as.character(unique(sumstats$Parameter.Sep))

#upload cva file
for (i in 1:length(parameters)){
  
  x <- parameters[i]
  p <- sumstats %>% filter(Parameter.Sep == x)
  
  p %>%
    mutate(Site.Name = fct_reorder(Site.Name, desc(Site.Order))) %>%
    ggplot(aes(Site.Name, fill=Fraction, group = interaction(Site.Name, Fraction))) +
    geom_boxplot(aes(
      lower = r_10, 
      upper = r_95, 
      middle = r_mean, 
      ymin = r_min,# - r_min*0.5, 
      ymax = r_max),# + r_max*0.5),
      stat = "identity") +
    labs(fill = "Fraction") +
    xlab("SITE") +
    ylab(paste0(x, " CONCENTRATION")) +
    theme_bw() +
    theme(axis.text.x=element_text(angle = 45, vjust = 1, hjust=1))
  
  
  ggsave(filename=paste0("C:\\Columbia_WQO\\Figures\\2020-08-17_Boxplot_",x,".tiff"), units="in", width=9, height=6, dpi=300, compression = 'lzw')
  
  print(paste0("Done figure ", x))
}


praise::praise()





p %>%
mutate(Site.Name = fct_reorder(Site.Name, desc(Site.Order))) %>%
  ggplot(aes(x=Site.Name, fill = Fraction)) +
  geom_boxplot(aes(
    lower = r_10, 
    upper = r_95, 
    middle = r_mean, 
    ymin = r_min,# - r_min*0.5, 
    ymax = r_max),# + r_max*0.5),
    stat = "identity") +
  xlab("SITE") +
  ylab(paste0(x, "CONCENTRATION")) +
  theme_bw() +
  theme(axis.text.x=element_text(angle = 45, vjust = 1, hjust=1))


