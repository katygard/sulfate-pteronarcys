## in this script, I will calculate a fitness index (growth of surviving individuals x percent survival in trtment)

library(ggplot2)
library(viridis)
library(dplyr)
library(plotly)
library(ggpubr)
library(tidyr)
library(DBI)

# connect to project database
ptery_sulf_db <- dbConnect(RSQLite::SQLite(), "../../raw_data/ptery_sulf.db")

# get data from ptery_sulf_db database
# survival and head_width entire tables not necessary for analysis, load them in if you want to view all data
#survival <- dbGetQuery(ptery_sulf_db, "SELECT * FROM survival;")  
#head_width <-dbGetQuery(ptery_sulf_db, "SELECT * FROM head_width")


# only select head width information of individuals that were alive on 2nd measurement day and last
# NOTE: using 2nd rather than first to try and eliminate some of the measurement error in first date
 hw_survivors <- dbGetQuery(ptery_sulf_db, "SELECT *
                                            FROM (SELECT *          
                                                  FROM head_width
                                                  WHERE bug_id IN(
                                                      SELECT bug_id
                                                      FROM head_width
                                                      GROUP BY bug_id
                                                      HAVING COUNT(DISTINCT meas_id) = 12))
                                            WHERE (week = 2) OR (week = 22);") 
#nested inquiry isolates individuals alive on all dates
#WHERE statement selects only the data from last first and last measurement dates
####NOTE: could probably have used filter() to complete above in tidyverse. try at later time###

#double check that only 240 individuals were alive on last measurement date
  end_survival <- dbGetQuery(ptery_sulf_db, "SELECT *
                                             FROM survival
                                             WHERE date = '2020-12-28';")
sum(end_survival$survival)  

#calculate growth rate for each individual, using specific growth rate (G). can change equation if needed
hw_survivors <- hw_survivors[,c(2,5:7)]
hw_survivors <- spread(hw_survivors, week, head_width)

hw_survivors <- hw_survivors %>% 
  dplyr:: rename(init = 3, final = 4) %>% 
  mutate(ln = log(final/init), G = (ln/140)*100 ) #140 days between 08/10 and 12/28

#average growth rate for each tank
tank_fit <- aggregate(hw_survivors[,6], list(hw_survivors$tank), mean)
tank_fit <- tank_fit %>%
  dplyr::rename(tank = Group.1, avg_G = x)

#change survival numbers to proportions
end_survival <- end_survival %>% 
  mutate(surv_prop = survival/10)

#create fitness index 
tank_fit <- tank_fit %>% 
  mutate(surv_prop = end_survival$surv_prop,
         fit_index = (surv_prop*avg_G),
         temp = end_survival$set_temp,
         trtment = end_survival$sulfate_trtment)
###NOTE: first, third, fourth line of mutate code only worked well bc tanks were in order in both df's
###would need to recode to include some "by group" argument to get to work otherwise



####data vis on fitness index####

#surface plot------------------------------------------------------------------------
#data reorg: survival.wide format good for 3D scatter, but for surface z must be a matrix
prep <- tank_fit[order(tank_fit$trtment),] #reordering survival.wide by sulfate level
prep1 <- prep[order(prep$temp),] #now reordering by temperature

#you want the reordering steps so that you can predictably label the z matrix w/ temp and sulfate 
#column/row names. the original survival.wide had treatments in no particular order
temp <- seq(12, 22, 2) #setting up matrix colnames
sulf <- seq(0, 1000, 200) #rownames

z <- matrix(prep1$fit_index, 6, 6) #reformatting 36 survival data points into matrix, by treatment

rownames(z) <- sulf
colnames(z) <- temp

surf_fit <- plot_ly(type = "surface", z=z, x=temp, y=sulf)
surf_fit <- surf_fit %>% layout(scene = list(xaxis = list(title = 'Temperature'),
                                             yaxis = list(title = 'Sulfate mg/L'),
                                             zaxis = list(title = 'Fitness Index')))

surf_fit
#save as html so interactive functionality is preserved
htmlwidgets::saveWidget(as_widget(surf_fit), "fitness_surface_plot.html")

#grouped bar and ggplots----------------------------------------------------------------------
tank_fit$temp <- as.factor(tank_fit$temp)
tank_fit$trtment <- as.factor(tank_fit$trtment)

#grouped by temp
ggplot(tank_fit, aes(fill=trtment, y=fit_index, x=temp)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_viridis_d()+
  xlab("Temperature") +
  ylab("Fitness Index") +
  theme_bw()

avg <- tank_fit %>%
  group_by(temp) %>%
  summarise(fit_index = mean(fit_index))

ggplot(tank_fit, aes(x=temp, y=fit_index, color = temp, fill = temp)) +
  geom_point() +
  geom_bar(data=avg, stat = "identity", alpha = 0.7) +
  ggrepel::geom_text_repel(aes(label = tank), color = "black", size = 2.5, segment.color = "grey") +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  xlab("Temperature") +
  ylab("Fitness Index") +
  theme_bw()

#grouped by sulfate treatment
ggplot(tank_fit, aes(fill=temp, y=fit_index, x=trtment)) + 
  geom_bar(position="dodge", stat="identity")

avg <- tank_fit %>%
  group_by(trtment) %>%
  summarise(fit_index = mean(fit_index))

ggplot(tank_fit, aes(x=trtment, y=fit_index, color = trtment, fill = trtment)) +
  geom_point() +
  geom_bar(data=avg, stat = "identity", alpha = 0.7) +
  ggrepel::geom_text_repel(aes(label = tank), color = "black", size = 2.5, segment.color = "grey") +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  xlab("Sulfate mg/L") +
  ylab("Fitness Index") +
  theme_bw()

#------
just growth averages
avg <- tank_fit %>%
  group_by(temp) %>%
  summarise(avg_G = mean(avg_G))

ggplot(avg, aes(x=temp, y=avg_G, color = temp, fill = temp)) +
  geom_bar(data=avg, stat = "identity") +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  xlab("Temperature") +
  ylab("Mean Growth Rate (G)") +
  theme_bw() +
  scale_y_continuous(limits = c(0,0.23))

avg <- tank_fit %>%
  group_by(trtment) %>%
  summarise(avg_G = mean(avg_G))

ggplot(avg, aes(x=trtment, y=avg_G, color = trtment, fill = trtment)) +
  geom_bar(data=avg, stat = "identity") +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  xlab("Sulfate mg/L") +
  ylab("Mean Growth Rate (G)") +
  theme_bw() +
  scale_y_continuous(limits = c(0,0.23))
