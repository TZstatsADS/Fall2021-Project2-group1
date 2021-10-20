#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
## new
library(ggplot2)
library(lubridate)

#Data Processing
df <- load("2018up.Rda")
df <- data.2018up
covid_df = read.csv('COVID-19_Daily_Counts_of_Cases__Hospitalizations__and_Deaths.csv')
covid_df$DATE <- as.Date(covid_df$DATE_OF_INTEREST, format = "%m/%d/%Y")
df_open <- df[df$ViolationStatus=="Open",]
df_close <- df[df$ViolationStatus=="Close",]
df$date <- paste(year(df$CurrentStatusDate),month(df$CurrentStatusDate),sep="-")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$plot1 <- renderPlot({

        ggplot(data = covid_df)+
            geom_point(mapping = aes(x=DATE, y=CASE_COUNT))+
            geom_line(mapping = aes(x=DATE, y=CASE_COUNT))+
            labs(x="Date",y="COVID-19 Case Number")+
            scale_x_date(date_breaks = "1 month", limits=as.date(dates))+
            theme(axis.text.x = element_text(hjust = 0.5, angle = 45))

    })
    
    # open violation v.s date
    output$plot2 <- renderPlot({
        
        ggplot(data=df_open) +
            geom_bar(aes(x=factor(date)),col="blue",fill="white")+
            labs(x="Date",y="Violation Number",fill="Violation Status")+
            scale_x_date(limits=as.date(dates))+
            theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(hjust = 0.5, angle = 45))
        
    })
    
    # close violation v.s date
    output$plot3 <- renderPlot({
        
        ggplot(data=df_close) +
            geom_bar(aes(x=factor(date)),col="blue",fill="white")+
            labs(x="Date",y="Violation Number",fill="Violation Status")+
            scale_x_date(limits=as.date(dates))+
            theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(hjust = 0.5, angle = 45))
        
    })
    
    # open & close violation v.s date
    output$plot4 <- renderPlot({
        
        ggplot(data=df) +
            geom_bar(aes(x=factor(date),fill=factor(ViolationStatus)))+
            labs(x="Date",y="Violation Number",fill="Violation Status")+
            scale_x_date(limits=as.date(dates))+
            theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(hjust = 0.5, angle = 45))
        
    })
    
    output$value <- renderPrint({ 
        if (input$checkGroup = 1){
            ggplot(data=df) +
                geom_bar(aes(x=factor(Borough),fill=factor(ViolationStatus)))+
                labs(x="Borough",y="Violation Number",fill="Violation Status")+
                theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(hjust = 0.5, angle = 45))
        } 
        if (input$checkGroup = 1){
            ggplot(data=df) +
                geom_bar(aes(x=factor(ViolationStatus),fill=factor(ViolationStatus)))+
                facet_grid(~Borough)+
                labs(x="Borough",y="Violation Number",fill="Violation Status")+
                theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(hjust = 0.5, angle = 45))
        }
    })

})
