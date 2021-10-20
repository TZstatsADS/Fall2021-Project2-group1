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
    install.packages("leafsync")
    library(leafsync)
}
if (!require("ggvis")) {
    install.packages("leafsync")
    library(leafsync)
}
if (!require("shinydashboard")) {
    install.packages("shinydashboard")
    library(leafsync)
}

# Prepare variables
# Load datasets
# setwd("..")
# readRDS("data/impair.Rda")
data.summarizedlong2001p <- readRDS("data/summarylong.Rda")

#===============================================Shiny UI=========================================================
ui <- fluidPage(
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

#===============================================Shiny SERVER=====================================================
shinyServer <- function(input, output, session) {

    #=========================
    #======== tab 1 ==========
    #=========================
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
