# load in packages
library(DBI)
library(RSQLite)
library(tidyverse)

# establish database connection
ptery_sulf_db <- dbConnect(RSQLite::SQLite(), "../../raw_data/ptery_sulf.db")

# load in head width table for surviving individuals ONLY, but select all data points (except first 2 weeks) for those individuals
hw_data <- dbGetQuery(ptery_sulf_db, "SELECT *
                                      FROM (SELECT *          
                                            FROM head_width
                                            WHERE bug_id IN(
                                              SELECT bug_id
                                              FROM head_width
                                              GROUP BY bug_id
                                              HAVING COUNT(DISTINCT meas_id) = 12))
                                      WHERE (week != 0) OR (week != 2);") 

# need code that uses linear regression to determine growth rate for each individual 


# Break up by bug_ID, then fit the specified model to each piece and return a list
library(plyr)
models <- dlply(hw_data, "bug_ID", function(df) 
  lm(head_width ~ week, data = df))

# Apply coef to each model and return a data frame
ldply(models, coef)
# in output, "week" coefficient estimate is growth rate/week?

# Print the summary of each model (PRINTING THIS IS HUGE, MORE APPROPRIATE FOR RMARKDOWN)
# l_ply(models, summary, .print = TRUE)

# example print?
print(models$`01C`)
