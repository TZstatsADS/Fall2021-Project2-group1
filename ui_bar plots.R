#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

if (!require("shiny")) {
    install.packages("shiny")
    library(shiny)
}
if (!require("shinydashboard")) {
    install.packages("shinydashboard")
    library(shinydashboard)
}
if (!require("ggplot2")) {
    install.packages("ggplot2")
    library(ggplot2)
}

# Define UI for application that draws a histogram
body <- dashboardBody(tabItems(
    tabItem(tabName = "Date",fluidPage(

    # Application title
    titlePanel("Violation Status"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            dateRangeInput("dates", label = h3("Date range")),
            hr(),
            fluidRow(column(4, verbatimTextOutput("value")))
        ),

        # Show a plot of the generated distribution
        tabsetPanel(tabPanel("Case Number", plotOutput("plot1")), 
                    tabPanel("Open Violation", plotOutput("plot2")),
                    tabPanel("Closed Violation", plotOutput("plot3")),
                    tabPanel("Open & Closed Violation", plotOutput("plot4"))
        )
    )
    )),

    ###################Violation Status by Borough###############
    tabItem(tabName = "Borough",fluidPage(
    
        # Application title
        titlePanel("Violation Status in Different Boroughs"),
    
        
        checkboxGroupInput("checkGroup", label = h3("Checkbox group"), 
                           choices = list("Stacked Bar Chart" = 1, "Bar Chart" = 2),
                           selected = 1),

        hr(),
        fluidRow(column(3, verbatimTextOutput("value"))),
        
            # Show a plot of the generated distribution
            mainPanel(
                plotOutput("value")
            )
        )
    ))
)


ui <- dashboardPage(
    title="Impact of COVID-19 on NYC Housing Violation",
    skin = "blue", 
    dashboardHeader(title=span("NYC Housing Violation",style="font-size: 16px")),
    
    dashboardSidebar(sidebarMenu(
        menuItem("Date", tabName = "Borough"),
        menuItem("Borough", tabName = "Borough")
    )),
    body 
)
