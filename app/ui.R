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

shinyUI(
  navbarPage(
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
      "References",
      tags$h2(
        "Data Sources"
      ),
      tags$a(
        href = "https://data.cityofnewyork.us/Housing-Development/Housing-Maintenance-Code-Violations/wvxf-dwi5",
        "https://data.cityofnewyork.us/Housing-Development/Housing-Maintenance-Code-Violations/wvxf-dwi5"
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
    )
  ) # end of navbar
) # end of ui