library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)

setwd("..")

#If importing environment, set imported to true, otherwise set false to generate vars
imported = true

if(imported)
{
  load("data/RDataFrames.RData")
} else {
  data.raw = read.csv('data/Housing_Maintenance_Code_Violations.csv')
  
  data.raw$InspectionDate = mdy(data.raw$InspectionDate, truncated=1)
  data.raw$ApprovedDate = mdy(data.raw$ApprovedDate, truncated=1)
  data.raw$OriginalCertifyByDate = mdy(data.raw$OriginalCertifyByDate, truncated=1)
  data.raw$OriginalCorrectByDate = mdy(data.raw$OriginalCorrectByDate, truncated=1)
  data.raw$NewCertifyByDate = mdy(data.raw$NewCertifyByDate, truncated=1)
  data.raw$NewCorrectByDate = mdy(data.raw$NewCorrectByDate, truncated=1)
  data.raw$CertifiedDate = mdy(data.raw$CertifiedDate, truncated=1)
  data.raw$NOVIssuedDate = mdy(data.raw$NOVIssuedDate, truncated=1)
  data.raw$CurrentStatusDate = mdy(data.raw$CurrentStatusDate, truncated=1)
  
  #1963 is when the data in sontinuous til, so filter after then
  data.raw = data.raw %>%
    mutate(iniYear = year(InspectionDate)) %>%
    filter(iniYear > 1962) 
  # data.2018up = data.raw %>% 
  #   filter(year(InspectionDate) > 2017)
  data.2001p = data.raw %>%
    filter(year(InspectionDate) > 2000)


  # #All years are filtered by the initial inspection date
  # #2018 violations
  # data.2018 = data.raw %>% 
  #   filter(iniYear == 2018)
  # #2019 violations
  # data.2019 = data.raw %>% 
  #   filter(iniYear == 2019)
  # #2020 violations
  # data.2020 = data.raw %>% 
  #   filter(iniYear == 2020)
  # #2021 violations
  # data.2021 = data.raw %>% 
  #   filter(iniYear == 2021)
  
  
  # Average violations/year
    data.annualsummary = data.raw %>%
      group_by(iniYear) %>%
      summarise(violations = n()) %>%
      mutate(logviolations = log(violations, base=10))
    rm(data.raw)
}
  #Log10 plot of violations
  ggplot(data = data.annualsummary, aes(x=iniYear, y=logviolations)) +
    #scale_y_continuous(trans='log10') +
    geom_point(size=3) + 
    labs(title = "Violations issued per year since 1963",
         y = "Log of Violation",
         x = "Year") +
    geom_smooth(method='lm')
  
  #Last 20 yrs of data
  data.annualsummary2001p = data.annualsummary %>% 
    filter(iniYear > 2000)
  ggplot(data = data.annualsummary2001p, aes(x=iniYear, y=logviolations)) +
    #scale_y_continuous(trans='log10') +
    geom_point(size=3) + 
    labs(title = "Violations issued per year since 2001",
         y = "Log of Violation",
         x = "Year") +
    geom_smooth(method='lm')
  
  #Expected violations for 2020 and 2021:
  fit.overall = lm(logviolations ~ iniYear, data=data.annualsummary)
  summary(fit.overall)   
  fit.2001p = lm(logviolations ~ iniYear, data=data.annualsummary2001p)
  summary(fit.2001p)   
  
  #last 20yrs has much better adjR2 (0.867 vs 0.565)
  #last 20yrs has much better adjR2 (0.868  vs 0.933 ) on log scale too
  #focus on last 20 yrs of data to compare to/estimate from
  
  #violations typically increase by 
  
# Total Violations this year and last vs expected
last2violations = data.frame(actual = c(
  data.annualsummary[data.annualsummary$iniYear == 2020,2][[1]],
  data.annualsummary[data.annualsummary$iniYear == 2021,2][[1]]))
last2violations = cbind(last2violations, data.frame(estimated = c(
  10 ^ predict(fit.2001p, data.frame(iniYear=c(2020, 2021))))))
last2violations = last2violations %>%
  mutate(difference = actual - estimated) %>%
  mutate(percDiff = difference/estimated)

if(!imported)
{
  # %Resolved violations per year comparison
    #Find conflicting 
    data.excludedstatus = data.2001p %>%
      group_by(CurrentStatus, ViolationStatus) %>%
      summarise(count = n()) %>%
      filter(count < 200) %>%
      select(!count)
    
    #delete violations that fall in the excluded combination categories
    data.cleaned2001p = data.2001p %>%
      anti_join(y = data.excludedstatus,
                by = c("CurrentStatus", "ViolationStatus"))
    
    #Number of violations removed: 483
    nrow(data.2001p) - nrow(data.cleaned2001p)
    rm(data.2001p)
}
  #Resolved(Closed) violations by year
  data.closed2001p = data.cleaned2001p %>%
    group_by(iniYear, ViolationStatus) %>%
    summarise(count = n()) %>%
    pivot_wider(names_from = ViolationStatus,
                values_from = count) %>%
    mutate(Total = Close+Open) %>%
    mutate(percClosed = Close/Total)
  
  #Determine how long the average violation was open by year
  data.timeOpen2001p = data.cleaned2001p %>%
    filter(ViolationStatus == "Close") %>%
    mutate(weeksOpen = as.numeric(difftime(CurrentStatusDate,
                                            InspectionDate,
                                            units = "weeks"))) %>%
    group_by(iniYear) %>%
    summarise(avgMonthsOpen = mean(weeksOpen*12/52))
  
  #Analyze violation classes
  data.cleaned2001p %>%
    group_by(Class, RentImpairing) %>%
    summarize(count = n())
  
  #Repeat above steps, but for each class of violation
  data.classbreakdown2001p = data.cleaned2001p %>%
    group_by(iniYear, Class, ViolationStatus) %>%
    summarize(count = n()) %>%
    pivot_wider(names_from = ViolationStatus,
                values_from = count) %>%
    mutate(Total = Close+Open) %>%
    mutate(percClosed = Close/Total) %>%
    pivot_longer(cols = c("Close", "Open", "Total", "percClosed"),
                 names_to = "Stats",
                 values_to = "count") %>%
    pivot_wider(names_from = c(Class, Stats),
                values_from = count,
                names_sep = "_")
  
  #Bind the three results into one dataframe for ease of visualization
  data.summarized2001p = inner_join(x = data.closed2001p,
                                    y = inner_join(
                                      x = data.timeOpen2001p, 
                                      y = data.classbreakdown2001p, 
                                      by = "iniYear"),
                                    by = "iniYear")

  data.impairment2001p = data.cleaned2001p %>%
    group_by(iniYear, RentImpairing) %>%
    summarize(count = n())