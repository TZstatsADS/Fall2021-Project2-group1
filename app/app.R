#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
###############################Install Related Packages #######################
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
if (!require("dplyr")) {
    install.packages("dplyr")
    library(dplyr)
}
if (!require("magrittr")) {
    install.packages("magrittr")
    library(magrittr)
}
if (!require("mapview")) {
    install.packages("mapview")
    library(mapview)
}
if (!require("leafsync")) {
    install.packages("leafsync")
    library(leafsync)
}
if (!require("ggplot2")) {
    install.packages("ggplot2")
    library(ggplot2)
}
if (!require("ggvis")) {
    install.packages("ggvis")
    library(ggvis)
}
if (!require("shinydashboard")) {
    install.packages("shinydashboard")
    library(shinydashboard)
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
if (!require("rsconnect")) {
    install.packages("rsconnect")
    library(rsconnect)
}

#===============================================Shiny UI=========================================================
ui <- navbarPage(
        "Housing Violations",
        #################### tab 1 ####################
        tabPanel(
            "Introduction",
            tags$img(
                src = "https://nypost.com/wp-content/uploads/sites/2/2021/02/nyc-housing-forecast-prices-market.jpg?quality=80&strip=all",
                width = "100%",
                style = "opacity: 0.90"
            ),
            fluidRow(
                absolutePanel(
                    style = "background-color: white",
                    top = "40%",
                    left = "25%",
                    right = "25%",
                    height = 170,
                    tags$p(
                        style = "padding: 5%; background-color: white; font-family: alegreya; font-size: 120%",
                        "The Covid-19 pandemic has had a profound impact on many aspects of daily life in New York City. Due to rising cases and several lockdowns, business and governmental work slowed. Here we look at how housing maintenance code violations were handled by the Department of Housing Preservation and Development (HPD) during this time."
                    )
                )
            )
        ),
        #################### tab 2 ####################
        tabPanel(
            "Static Plots",
            sidebarLayout(
                sidebarPanel(
                    selectInput("db", "By", c("date", "Borough"))
                ),
                mainPanel(
                    plotOutput("static"),
                    plotOutput("covid")
                )
            )
        ),
        #################### tab 3 ####################
        tabPanel(
            "Interactive Plot",
            sidebarLayout(
                sidebarPanel(
                    selectInput("issue", "Issue", c("violations", "complaints")),
                    selectInput("iyear", "Year", c(2018, 2019, 2020, 2021))
                ),
                mainPanel(
                    leafletOutput("heatmap")
                )
            )
        ),
        #################### tab 4 ####################
        tabPanel(
            "Filtered Plot",
            fluidPage(
                sidebarLayout(position = "left",
                      sidebarPanel(
                          h3("Select a filter to see trends over time:", align = "left", style="color:#045a8d"),
                          radioButtons("StackedBCFilter", label = h3("Filter"),
                                       choices = list("Violation Class" = 1, "Rent Impairing" = 2, "Overall Open/Closed" = 3), 
                                       selected = 1),
                      ),
                      mainPanel(
                          plotOutput("box")
                      )
                )
            )
        ),
        #################### tab 5 ####################
        tabPanel(
            "References",
            tags$h2(
                "Data Sources"
            ),
            tags$a(
                href = "https://data.cityofnewyork.us/Housing-Development/Housing-Maintenance-Code-Violations/wvxf-dwi5",
                "data.cityofnewyork.us/Housing-Development/Housing-Maintenance-Code-Violations", 
            ),br(),
            tags$a(
                href = "https://data.cityofnewyork.us/Housing-Development/Housing-Maintenance-Code-Complaints/uwyv-629c",
                "data.cityofnewyork.us/Housing-Development/Housing-Maintenance-Code-Complaints"
            ),br(),
            tags$a(
                href = "https://data.cityofnewyork.us/Health/COVID-19-Daily-Counts-of-Cases-Hospitalizations-an/rc75-m7u3",
                "data.cityofnewyork.us/Health/COVID-19-Daily-Counts-of-Cases-Hospitalizations-an"
            ),
            tags$h2(
                "Contributors"
            ),
            tags$p(
                "Arya Ayati-Ghaffari"
            ),
            tags$p(
                "Dipesh Patel"
            ),
            tags$p(
                "Rodrigo Zarate"
            ),
            tags$p(
                "Yang Zhao"
            ),
            tags$h2(
                "GitHub Repository"
            ),
            tags$a(
                href = "https://github.com/TZstatsADS/Fall2021-Project2-group1",
                "https://github.com/TZstatsADS/Fall2021-Project2-group1"
            )
    ) # end of navbar
) # end of ui
    


#===============================================Shiny SERVER=====================================================
# LOAD AND PREPARE DATA ####################################

shinyServer <- function(input, output, session) {
    #Import data produced from analysis.r script
    counting.df_cm <- readRDS("countingdf_cm.Rda")
    counting.df_vl <- readRDS("countingdf_vl.Rda")
    data.summarizedlong2001p <- readRDS("sumlong.Rda")
    df <- readRDS("2018up.Rda")
    covid_df = read.csv("COVID-19_Daily_Counts_of_Cases__Hospitalizations__and_Deaths.csv")
    
    # covid data
    covid_df$DATE <- as.Date(covid_df$DATE_OF_INTEREST, format = "%m/%d/%Y")
    
    # cache zip boundaries that are downloaded via tigris package
    options(tigris_use_cache = TRUE)
    
    zcta <- zctas(starts_with = c(10, 11), year = 2010, state = "New York")
    
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
    
    # static plots
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

    # ========================== stacked bar chart ===========================
    rv <- reactiveValues(update = 0)
    observeEvent(input$StackedBCFilter, {
        rv$update <- input$StackedBCFilter
    })
    
    output$box <- renderPlot({
        library(tidyverse)
        rv$update
        
        if(rv$update == 3) { #Overall
            ggplot(data.summarizedlong2001p %>%
                       filter(Class %in% c("O")), 
                   aes(iniYear, Count, fill = Status), environment=environment()) +
                geom_bar(position="stack", stat="identity") +
                ylab("Total") +
                labs(title="Overall Open/Closed Violations")
        } else if (rv$update == 2){ #Rent Impair
            ggplot(data.summarizedlong2001p %>%
                       filter(Class %in% c("N", "Y")) %>%
                       pivot_wider(names_from = "Status",
                                   values_from = "Count") %>%
                       mutate(Total = Open + Close), 
                   aes(iniYear, Total, fill = Class), environment=environment()) +
                geom_bar(position="stack", stat="identity") +
                ylab("Total") +
                labs(title="Rent Impairing Open/Closed Violations")
        } else if (rv$update == 1){ #Classes
            ggplot(data.summarizedlong2001p %>%
                       filter(Class %in% c("A", "B", "C")) %>%
                       pivot_wider(names_from = "Status",
                                   values_from = "Count") %>%
                       mutate(Total = Open + Close), 
                   aes(iniYear, Total, fill = Class), environment=environment()) +
                geom_bar(position="stack", stat="identity") +
                ylab("Total") +
                labs(title="Open/Closed Violations by Class ")
        }
    })
}

shiny::shinyApp(ui, shinyServer)
