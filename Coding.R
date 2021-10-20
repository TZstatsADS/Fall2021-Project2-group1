# Install required packages if needed
install.packages("devtools")
library(devtools)
# install.packages("devtools")
library(devtools)
install_github('arilamstein/choroplethrZip@v1.5.0')

if (!require("dplyr")) install.packages("dplyr")
if (!require("tidyr")) install.packages("tidyr")
if (!require("pacman")) install.packages("pacman")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("zipcode")) install.packages("zipcode")
if (!require("data.table")) install.packages("data.table")
if (!require("devtools")) install.packages("devtools")
if (!require("choroplethrZip")) install.packages("choroplethrZip")
if (!require("choroplethr")) install.packages("choroplethr")
if (!require("choroplethrAdmin1")) install.packages("choroplethrAdmin1")
if (!require("arilamstein/choroplethrZip@v1.5.0")) install_github("arilamstein/choroplethrZip@v1.5.0")


# Load contributed packages with pacman
pacman::p_load(pacman, party, psych, rio, tidyverse)


# LOAD AND PREPARE DATA ####################################

# Import CSV files with readr::read_csv() from tidyverse
df_vl <- data.frame(read_csv("data/Housing_Maintenance_Code_Violations.csv"))
df_cm <- data.frame(read_csv("data/Housing_Maintenance_Code_Complaints.csv"))

# Changing type of values (dates)
df_vl$InspectionDate <- as.Date(df_vl$InspectionDate, format = '%m/%d/%Y')
df_cm$ReceivedDate <- as.Date(df_cm$ReceivedDate, format = '%m/%d/%Y')

# Generating key Year-Month
Y_m_vl <- paste(strftime(df_vl$InspectionDate, "%Y"))
Y_m_cm <- paste(strftime(df_cm$ReceivedDate, "%Y"))

# Combining data with key
df_vl <- cbind(df_vl,Y_m_vl)
df_cm <- cbind(df_cm,Y_m_cm)


# Building counting tables & changing column names
counting.df_vl <- df_vl %>% 
  count(Postcode, Y_m_vl) %>% 
  group_by(Postcode)

colnames(counting.df_vl)<-c("region","key","value")

counting.df_cm <- df_cm %>% 
  count(Zip, Y_m_cm) %>% 
  group_by(Zip)

colnames(counting.df_cm)<-c("region","key","value")

# Filtering actual zipcodes
counting.df_vl <- filter(counting.df_vl, region > 10000 & region < 12000)
counting.df_cm <- filter(counting.df_cm, region > 10000 & region < 12000)

counting.df_vl <- filter(counting.df_vl, key > 2017)
counting.df_cm <- filter(counting.df_cm, key > 2017)

counting.df_vl$region <- as.character(counting.df_vl$region)
counting.df_cm$region <- as.character(counting.df_cm$region)



# Exporting tables to CSV files
write.csv(counting.df_vl, "counting.df_vl.csv")
write.csv(counting.df_cm, "counting.df_cm.csv")


counting.df_vl2018 <- filter(counting.df_vl, key == 2018)
counting.df_cm2018 <- filter(counting.df_cm, key == 2018)

counting.df_vl2019 <- filter(counting.df_vl, key == 2019)
counting.df_cm2019 <- filter(counting.df_cm, key == 2019)

counting.df_vl2020 <- filter(counting.df_vl, key == 2020)
counting.df_cm2020 <- filter(counting.df_cm, key == 2020)

counting.df_vl2021 <- filter(counting.df_vl, key == 2021)
counting.df_cm2021 <- filter(counting.df_cm, key == 2021)


# Plotting Heat maps
zip_choropleth(counting.df_vl2021,
               title       = "2021 Manhattan Housing Maintenance Code Violations",
               legend      = "Number of Violations",
               county_zoom = 36061) + coord_map() 




