#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#-------------------------------------------------App Server----------------------------------

if (!require("shiny")) {
    install.packages("shiny")
    library(shiny)
}
if (!require("leaflet")) {
    install.packages("leaflet")
    library(leaflet)
}
if (!require("leaflet.extras")) {
    install.packages("leaflet.extras")
    library(leaflet.extras)
}
if (!require("magrittr")) {
    install.packages("magrittr")
    library(magrittr)
}
if (!require("leafsync")) {
    install.packages("leafsync")
    library(leafsync)
}
if (!require("lubridate")) {
    install.packages("lubridate")
    library(lubridate)
}
if (!require("tidyverse")) {
    install.packages("tidyverse")
    library(tidyverse)
}
if (!require("pacman")) {
    install.packages("pacman")
    library(pacman)
}
if (!require("zipcodeR")) {
    install.packages("zipcodeR")
    library(zipcodeR)
}
if (!require("data.table")) {
    install.packages("data.table")
    library(data.table)
}
if (!require("devtools")) {
    install.packages("devtools")
    library(devtools)
}
if (!require("tigris")) {
    install.packages("tigris")
    library(tigris)
}
if (!require("htmltools")) {
    install.packages("htmltools")
    library(htmltools)
}

# LOAD AND PREPARE DATA ####################################

# Import CSV files with readr::read_csv() from tidyverse
df_vl <- data.frame(read_csv("../data/Housing_Maintenance_Code_Violations.csv"))
df_cm <- data.frame(read_csv("../data/Housing_Maintenance_Code_Complaints.csv"))

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

colnames(counting.df_vl) <- c("region","key","value")

counting.df_cm <- df_cm %>%
    count(Zip, Y_m_cm) %>%
    group_by(Zip)

colnames(counting.df_cm) <- c("region","key","value")

# Filtering actual zipcodes
counting.df_vl <- filter(counting.df_vl, region > 10000 & region < 12000)
counting.df_cm <- filter(counting.df_cm, region > 10000 & region < 12000)

counting.df_vl <- filter(counting.df_vl, key > 2017)
counting.df_cm <- filter(counting.df_cm, key > 2017)

counting.df_vl$region <- as.character(counting.df_vl$region)
counting.df_cm$region <- as.character(counting.df_cm$region)

# cache zip boundaries that are downloaded via tigris package
options(tigris_use_cache = TRUE)

zcta <- zctas(starts_with = c(10, 11), year = 2010, state = "New York")


# static plots
df <- load("../data/2018up.Rda")
df <- data.2018up

df$date <- paste(year(df$CurrentStatusDate),month(df$CurrentStatusDate),sep="-")

# covid data
covid_df = read.csv("../data/COVID-19_Daily_Counts_of_Cases__Hospitalizations__and_Deaths.csv")
covid_df$DATE <- as.Date(covid_df$DATE_OF_INTEREST, format = "%m/%d/%Y")

shinyServer(function(input, output) {
    issue <- reactive({
        if (input$issue == "violations") {
            return(filter(counting.df_vl, key == input$iyear))
        }
        if (input$issue == "complaints") {
            return(filter(counting.df_cm, key == input$iyear))
        }
    })
    
    output$heatmap <- renderLeaflet({
        zcta <- geo_join(zcta, issue(), by_sp = "ZCTA5CE10", by_df = "region", how = "left")
        zcta <- zcta %>% drop_na()
        
        if (input$issue == "violations") {
            pal <- colorNumeric(
                palette = "Greens",
                domain = zcta$value)
        }
        
        if (input$issue == "complaints") {
            pal <- colorNumeric(
                palette = "Blues",
                domain = zcta$value)
        }
        
        labels <- 
            paste0(
                "zip code: ",
                zcta$ZCTA5CE10, "<br/>", input$issue,
                ": ",
                zcta$value) %>%
            lapply(htmltools::HTML)
        
        zcta %>% 
            leaflet %>% 
            # add base map
            addProviderTiles("CartoDB") %>% 
            # add zip codes
            addPolygons(fillColor = ~pal(value),
                        weight = 2,
                        opacity = 1,
                        color = "white",
                        dashArray = "3",
                        fillOpacity = 0.7,
                        highlight = highlightOptions(weight = 2,
                                                     color = "#666",
                                                     dashArray = "",
                                                     fillOpacity = 0.7,
                                                     bringToFront = TRUE),
                        label = labels) %>%
            addLegend(pal = pal,
                      values = ~value,
                      opacity = 0.7, 
                      title = htmltools::HTML(paste0(input$issue, "<br> 
                                    by zip code")),
                      position = "bottomright")
    })
    
    output$static <- renderPlot({
        ggplot(data=df) +
            geom_bar(aes(x=factor(.data[[input$db]]),fill=factor(ViolationStatus)))+
            labs(title = "Housing Violations",x=input$db,y="Violation Count",fill="Violation Status")+
            theme(plot.title = element_text(hjust = 0.5),
                  axis.text.x = element_text(hjust = 0.5, angle = 45),
                  legend.position = c(0.88, 0.75))
    })
    
    output$covid <- renderPlot({
        ggplot(data = covid_df)+
            geom_point(mapping = aes(x=DATE, y=CASE_COUNT))+
            geom_line(mapping = aes(x=DATE, y=CASE_COUNT))+
            labs(x="Date",y="Cases", title ="Covid-19 Case Count")+
            scale_x_date(date_breaks = "1 month")+
            theme(plot.title = element_text(hjust = 0.5),
                  axis.text.x = element_text(hjust = 0.5, angle = 45))
    })
})