rm(list=ls()) # removes all variables stored previously 
library(Hmisc) # need this for 'describe' function


covid19_dataset <- read.csv("C:/Users/maullonv/Downloads/datasets_494724_994000_COVID19_line_list_data (2).csv")

#####################################
### Short summary of the data set ###
#####################################

describe(covid19_dataset) # from Hmisc packg
# Shows some informative things regarding the dataset
  # i.e. 27 vars and total of 1085 observations, missing and distinct values, 
  # max and min etc.
# Notice the data set is messy
  # Example: The 'death' variable shows that there are 14 distinct values 
  # opposed to 2
    # The reason for this is b/c the entries either shows a '1', '0', or 
    # date of the death
      # '0' if one didn't die; '1' if one died
# So now, will proceed with cleaning up data as this is inconsistent and difficult to
# work with


#####################
## Clean the Data ###
#####################

# Cleaning the data ('death' column)
covid19_dataset$death_dummy <- as.integer(covid19_dataset$death != 0)
# Will create a new column called 'death_dummy'
  # IOW if the data/ entry within the death column is '0', then the person died
  # if it's not i.e. '1' then they died
  # using 'unique(...)', it shows the values are only '0', '1'
  # now the death col is clean 

# Now, can CALCULATE the DEATH RATE
sum(covid19_dataset$death_dummy) / nrow(covid19_dataset)
# data shows a death rate of about 5.8%


###########
### AGE ###
###########

# According to research, it is more likely that
# the person that dies from covid-19 is older than 
# a person that surivives covid-19
# Can we prove that this claim is correct using our data?
# CLAIM: people who tested positive for covid and died, are older
# than those who tested positive and survived 
dead = subset(covid19_dataset, death_dummy == 1)
alive = subset(covid19_dataset, death_dummy == 0)
# So we have: 63 covid deaths and 1022 covid survivors

# Now will calculate the mean age of both groups (those dead and alive)
mean(dead$age, na.rm = TRUE)
mean(alive$age, na.rm = TRUE)
# The initial output is 'NA', indicating missing values
  # Looking at the data, there are some entries that appear as 'NA'
  # The reason behind Rs output as 'NA', indicates R does not 
  # know how to interpret such missing values
  # So, will include 'na.rm = TRUE' into the code
    # This will just ignore all those values that appear as 'NA'
# So now, the output shows:
  # The average age of one with covid and DIED is 68-69 years old
  # The average age of one with covid and SURVIVED is 48 years old
# There is a 20 year gap between the mean ages

  # But is this statistically significant?
  # Will now use the t-test to check
t.test(alive$age, dead$age, 
       aleternative="two.sided", conf.level = 0.95)
# Shows there is a 95% chance that the difference between a person
# who is alive and dead in age is from 24 yrs to 16-17 (this is the conf int)
# So an average, a person who is alive is much much younger
# Try with 99%
t.test(alive$age, dead$age, 
       aleternative="two.sided", conf.level = 0.99)
# Got around the same values
# Now, look at the p-value: 2.2e-16
  # This is the probability that from the sample we randomly
  # got such an extreme result, so this is basically 0
  # So there is a 0% chance that the ages of the 2 populations
  # (dead and alive) are the same (reject null)
  # RECALL: normaly, if p-value is < 0.05, we reject null
  # hypothesis

# CONCLUDE:our result IS STATISTICALLY SIGNIFICANT!
# So, the people who die from Covid-19 are indeed
# older than the people who survive covid-19


##############
### Gender ###
##############

# Question: Are women more likely to die due to Covid compared
# to men? Or is it vice-versa?
# Claim/ Hypothesis: Gender has no effect

# Similar to testing the claim regarding age and Covid
# Will make two subsets
women = subset(covid19_dataset, gender == "female")
men = subset(covid19_dataset, gender == "male")
# Shows there are 520 males and 382 females

mean(women$death_dummy, na.rm = TRUE)
# Shows women have a covid death rate of 3.7%
mean(men$death_dummy, na.rm = TRUE)
# Shows men have a covid death rate of 8.5%
# This is a pretty LARGE discrepancy

# Is this STATISTICALLY SIGNIFICANT?
t.test(women$death_dummy, men$death_dummy, 
       aleternative="two.sided", conf.level = 0.99)
# See that the means are the same
# and that with 99% confidence, men have from .78% to 8.8%
# higher fatality rates than woman
# With p-value: 0.002105 < 0.05 so reject null
# And thus IS SIGNIFICANT
# Therefore mens higher death rate represents the population
# and thus, gender does have an effect






