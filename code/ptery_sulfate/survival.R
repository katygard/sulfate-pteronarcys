#quick script to visualize survival data of sulfxptery experiment

library(ggplot2)
library(viridis)
library(viridisLite)
library(dplyr)
library(plotly)
library(ggpubr)
library(tidyr)
library(DBI)

# connect to project database
ptery_sulf_db <- dbConnect(RSQLite::SQLite(), "../../raw_data/ptery_sulf.db")

# get data from ptery_sulf_db database
surv <- dbGetQuery(ptery_sulf_db, "SELECT * FROM survival;")  

# create sampling_date (date class) from date (character class)----
surv <- surv %>%
  mutate(sampling_date = lubridate::ymd(date)) 

#create days column
surv <- surv %>%
  mutate(days = sampling_date - as.Date("2020-07-28"))

surv <- surv[,c(3:6, 8)]

#total by treatment, regardless of temperature
surv_trtment <- surv[,c(3:5)]
surv_trtment <- aggregate(. ~sulfate_trtment+days, data = surv_trtment, sum)
surv_trtment$survival <- (((surv_trtment$survival)/60)*100)

surv_trtment$sulfate_trtment <- as.factor(surv_trtment$sulfate_trtment)

#survival plot by sulfate trtment over time
ggplot(surv_trtment, aes(x=days, y=survival, color=sulfate_trtment)) +
  geom_line(lwd = 1.3) +
  theme_bw() +
  xlab("Sampling Date") +
  ylab("% Survival") +
  theme(legend.position = c(0.1, 0.3),
        legend.key.size = unit(1, "cm"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  scale_y_continuous(limits = c(0,100)) +
  scale_color_viridis_d() 

#total by temperature, regardless of treatment
surv_temp <- surv[,c(2,4:5)]
surv_temp <- aggregate(. ~set_temp+days, data = surv_temp, sum)
surv_temp$survival <- (((surv_temp$survival)/60)*100)

surv_temp$set_temp <- as.factor(surv_temp$set_temp)

#survival plot by temperature over time
ggplot(surv_temp, aes(x=days, y=survival, color=set_temp)) +
  geom_line(lwd = 1.3) +
  theme_bw() +
  xlab("Sampling Date") +
  ylab("% Survival") +
  theme(legend.position = c(0.1, 0.3),
        legend.key.size = unit(1, "cm"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  scale_y_continuous(limits = c(0,100)) +
  scale_color_viridis_d() 

####barplots of final survival
#sulfate
sulf_final <- subset(surv_trtment, days == 153)
ggplot(sulf_final, aes(x=sulfate_trtment, y=survival, color=sulfate_trtment, fill=sulfate_trtment)) +
  geom_bar(position="dodge", stat="identity") +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  xlab("Sulfate Treatment mg/L") +
  ylab("% Survival at End of Experiment") +
  scale_y_continuous(limits = c(0,100))

#temp
temp_final <- subset(surv_temp, days == 153)
ggplot(temp_final, aes(x=set_temp, y=survival, color=set_temp, fill=set_temp)) +
  geom_bar(position="dodge", stat="identity") +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  xlab("Temperature (C)") +
  ylab("% Survival at End of Experiment") +
  scale_y_continuous(limits = c(0,100))
