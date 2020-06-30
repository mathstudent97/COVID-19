library(ggplot2)


### Physical Distancing and Flattening the Curve ###

### Focus: Exponential Growth ###

# I want to explore the theoretical effect that physical distancing 
# can have on reducing the spread of the virus.  
# The theory, as well as the trends being observed, support the idea 
# that physical distancing and case tracing is reducing the number of cases.

# Will start with 100 cases and will assume 24% daily increase in the
# number of cases
days.1to15 <- 0:15 # will create day 0 to 15
# Will start with seeing how the number of cases  will grow 
# within the first 15 days
# Day 0 being the 100 cases we start with
cases.1to15 <- round(100*(1.24^days.1to15))
# Then will work out how many cases there would be on each of those
# 15 days
rbind(days.1to15, cases.1to15)
# Heres the days and cases per day
# I'm binding them together so we can view what we'd see on day 0
# which is 100 cases, day 1 with 124 cases,...

# Plots

ggplot()+
  geom_point(aes(x=days.1to15, y=cases.1to15)) +
  geom_line(aes(x=days.1to15, y=cases.1to15)) +
  xlab("Day") + 
  ylab("Total Cases")
# Notice: Exponential growth relation

# Will also look at the number of cases on the log scale
ggplot()+
  geom_point(aes(x=days.1to15, y=cases.1to15)) +
  geom_line(aes(x=days.1to15, y=cases.1to15)) +
  scale_y_log10(labels = scales::comma) +
  xlab("Day") + 
  ylab("Total Cases")
# On the scale of log number of cases, there is linear growth


# Want to look at the number of cases AFTER 15 days
# Say days 16-30
# Again asssuming there is a 24% increase per day
# Want to see how the trend continues for another 15 days
days.16to30 <- 16:30
days.1to30 <-c(days.1to15, days.16to30)
# Below are the cases per day
cases.16to30 <- round(100*(1.24^days.16to30), digits = 0)
cases.1to30 <- c(cases.1to15, cases.16to30)
rbind(days.1to30, cases.1to30)
# Binding those together by day
# On day 15 there were 2520 cases,...

#Plots
ggplot()+
  geom_point(aes(x=days.1to30, y=cases.1to30)) +
  geom_line(aes(x=days.1to30, y=cases.1to30)) +
  xlab("Day") + 
  ylab("Total Cases")
# Notice: Exponential growth relation

# Will also look at the number of cases on the log scale
ggplot()+
  geom_point(aes(x=days.1to30, y=cases.1to30)) +
  geom_line(aes(x=days.1to30, y=cases.1to30)) +
  scale_y_log10(labels = scales::comma) +
  xlab("Day") + 
  ylab("Total Cases")
# On the scale of log number of cases, there is linear growth


# What can physical distancing do?
# Suppose after day 15, physical distancing was implemented
# and followed reasonably well
# So, assume the rate at which are increasing by day is a 12%
# increase per day rather than a 24% increase
# Want to see how the number of cases would grow in this case
days.distancing.16to30 <- 16:30
cases.distancing.16to30 <- round(cases.1to15[16]*
                                   (1.12^(1:15)), digits = 0)
# Will look at the number of cases by day (with rate of increase down to 
# 12 % starting on day 16)
# Bind those together by day:
rbind(days.distancing.16to30, cases.distancing.16to30)
# Notice: On day 16, have 2822 cases 
# (which is less than 3124 (data from 24% growth rate));
# day 30: 3793 (less than 63482 (" "))

# Will take a look at the same plot again (similar)
# First will show the number of cases expected without distancing
# Then will show the number of cases that will be expected WITH distancing
# and assuming that the number of cases increasing at a rate of 24 percent
# per day to 12 percent per day

ggplot()+
  geom_line(aes(x=days.1to30, y=cases.1to30), alpha=0.1) +
  geom_line(aes(x=days.1to15, y=cases.1to15)) + # cases w/out physical distancing
  geom_line(aes(x=days.distancing.16to30, 
                y=cases.distancing.16to30)) + # with distancing
  geom_point(aes(x=days.1to30, y=cases.1to30), alpha=0.1) +
  geom_point(aes(x=days.1to15, y=cases.1to15)) +
  geom_point(aes(x=days.distancing.16to30,
                 y=cases.distancing.16to30), col="red") +  
  xlab("Day") + 
  ylab("Total Cases")
# Black points show the trajectory from day 0 to day 15
# Gray points show the trajectory that would have continued on, without
# distancing; continuing to see an increase in cases of 24% per day
# Red points show the trajectory that would have changed to WITH implementing
# physical distancing and assuming a 12% increase rate in cases per day (fall)
# Obv. a dramatic impact


# Same plot on the logscale
ggplot()+
  geom_line(aes(x=days.1to30, y=cases.1to30), alpha=0.1) +
  geom_line(aes(x=days.1to15, y=cases.1to15)) + # cases w/out physical distancing
  geom_line(aes(x=days.distancing.16to30, 
                y=cases.distancing.16to30)) + # with distancing
  geom_point(aes(x=days.1to30, y=cases.1to30), alpha=0.1) +
  geom_point(aes(x=days.1to15, y=cases.1to15)) +
  geom_point(aes(x=days.distancing.16to30,
                 y=cases.distancing.16to30), col="red") +  
  scale_y_log10(labels = scales::comma) +  
  xlab("Day") + 
  ylab("Total Cases")
# Similarly: Gray: represents 24% increase per day without distancing
# red: 12% increase per day with distancing

# Conclusion/ Purpose:
# This helped theorize the effect of physical distancing
# and what it is meant by "flattening the curve"
# Graph regarding BC data also proves the effect distancing has on the
# rate of Covid-19 cases












