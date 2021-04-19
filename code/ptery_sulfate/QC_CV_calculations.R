library(DBI)
library(tidyverse)

# read in QC data
QC <- read.csv("../../raw_data/tech_QC/QC.csv")
names(QC)[names(QC) == "ï..date"] <- "date"

QC %>%
  summarise(count = n_distinct(bug_ID)) #should be 25 unique bug_IDs measured

#calculate mean and sd for each bug_ID, use these to generate within-subject CV (expressed as %)
total_QC <- QC %>%
              group_by(bug_ID) %>%
              summarize(mean = mean(hw_mm),
                        sd = sd(hw_mm),
                        cv_percent = (sd/mean)*100)
obs_QC <- QC %>%
              group_by(bug_ID, observer) %>%
              summarize(mean = mean(hw_mm),
                        sd = sd(hw_mm),
                        cv_percent = (sd/mean)*100) %>%
              na.omit()
write.csv(obs_QC, "../../../../../../../../OneDrive/Desktop/QC_CV.csv")

#averaging within_subject CV's by observer to see average error by observer
CV_by_obs <- obs_QC %>%
              group_by(observer) %>%
              na.omit() %>%
              summarize(avg_CV = mean(cv_percent))

#our within_subject CV's expressed together
total_CV <- total_QC %>%
              na.omit() %>%
              summarize(avg_CV = mean(cv_percent))

#difference in error between alyssa and I
diff <- obs_QC %>%
  group_by(bug_ID)%>%
  na.omit()%>%
  mutate(difference = cv_percent - lag(cv_percent, default=first(cv_percent)))

mean(diff$difference)
# on average, my CV % is 0.45% lower than Alyssa's (took difference of each CV, average differences)
# my average CV: 0.88%
# Alyssa's average CV: 1.79%
# within-subject CV *not* partitioned by observer is 1.66% 
# (^ calculated w/in subject for each bug, without considering observer, then averaged)


# --- need to calculate variance and not CV, use in hierarchical mixed model to determine