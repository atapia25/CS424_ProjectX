library(shiny)
library(leaflet)
library(leaflet.extras)

globalData <- read.csv("global_power_plant_databasev2.csv", header = TRUE, sep = ",")

northAmerica <- c("Canada", "Costa Rica", "Cuba", "Dominican Republic", 
                  "El Salvador", "Guatemala", "Honduras", "Jamaica", "Mexico", 
                  "Nicaragua", "Panama", "Trinidad and Tobago", 
                  "United States of America")

southAmerica <- c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia", 
                  "Ecuador", "French Guiana", "Guyana", "Paraguay",
                  "Peru", "Uruguay", "Venezuela")

europe <- c("Albania", "Armenia", "Austria", "Azerbaijan", "Belarus", "Belgium",
            "Bosnia and Herzegovina", "Bulgaria", "Croatia", "Cyprus",
            "Czech Republic", "Denmark", "Estonia", "Finland", "France", "Georgia", 
            "Germany", "Greece", "Hungary", "Iceland", "Ireland", "Italy",
            "Kazakhstan", "Kosovo", "Latvia", "Lithuania","Luxembourg", "Moldova", 
            "Montenegro", "Netherlands", "Norway", "Poland", "Portugal",
            "Romania", "Russia", "Serbia", "Slovakia", "Slovenia", "Spain",
            "Sweden", "Switzerland", "Turkey", "Ukraine", "United Kingdom")

africa <- c("Algeria", "Angola", "Benin", "Botswana", "Burkina Faso",
            "Burundi", "Cape Verde", "Cameroon", "Central African Republic",
            "Congo", "Cote DIvoire", "Democratic Republic of the Congo",
            "Egypt", "Equatorial Guinea", "Eritrea", "Ethiopia", "Gabon",
            "Gambia", "Ghana", "Guinea", "Guinea-Bissau", "Kenya", "Lesotho",
            "Liberia", "Libya", "Madagascar", "Malawi", "Mali", "Mauritania",
            "Mauritius", "Morocco", "Mozambique", "Namibia", "Niger", "Nigeria",
            "Rwanda", "Senegal", "Sierra Leone", "South Africa", "Sudan",
            "Swaziland", "Tanzania", "Togo", "Tunisia", "Uganda", "Western Sahara",
            "Zambia", "Zimbabwe")

asia <- c("Afghanistan", "Armenia", "Azerbaijan", "Bahrain", "Bangladesh",
          "Bhutan", "Brunei Darussalam", "Cambodia", "China", "Cyprus", 
          "Georgia", "India",
          "Indonesia", "Iran", "Iraq", "Israel", "Japan", "Jordan", "Kazakhstan",
          "Kuwait", "Kyrgyzstan", "Laos", "Lebanon", "Malaysia", "Mongolia", 
          "Myanmar", "Nepal", "North Korea", "Oman", "Pakistan", "Philippines",
          "Qatar", "Russia", "Saudi Arabia", "Singapore", "South Korea", 
          "Sri Lanka", "Syrian Arab Republic", "Taiwan", "Tajikistan",
          "Thailand", "Turkey", "Turkmenistan", "United Arab Emirates",
          "Uzbekistan", "Vietnam", "Yemen")

oceania <- c("Australia", "Fiji", "Indonesia", "New Zealand", "Papua New Guinea")

antartica <- c("Antarctica")

colorFactors <- colorFactor(c("#007806", "#000000", "red", "#7200AB",
                            "brown", "#1FF0FF", "#FA89F1", "#4A270A",
                            "#8F8F8F", "#3B3B3B", "#FFFB00", "orange",
                            "#80FF00", "#0066FF", "#A1A1A1"), domain = globalData$Fuel)


ui <- navbarPage("CS 424 Project X",
        tabPanel("Power to the People",
          fluidRow(
            column(2,
              fluidRow(
                selectInput("continent", "Select which continent to view on 
                            the map:", c("North America", "South America", 
                                         "Europe", "Africa", "Asia",
                                         "Oceania", "Antarctica"),
                            selected = "North America"),
                checkboxGroupInput("fuel", "Select which fuel sources you would
                                   like to view", 
                                   sort(c(unique(globalData$Fuel), "All")),
                                   selected = "All"),
                checkboxInput("sliderAll", "All"),
                sliderInput("slider1", "Adjust to view plants based on their maximum
                            capacity", min = 1, max = 22500, value = 1),
                sliderInput("slider2", "Adjust to view plants based on their minimum
                            capacity", min = 1, max = 22500, value = 22500)
                
        )
      ),
      column(10,
        fluidRow(
          leafletOutput("globalMap2019", height = 800)
        )
      )
    )
  ),
  tabPanel("About",
           h1("Information about data"),
           p("The data consists of electrical production throughout 2019. While
             previous projects have covered electrical production and usage before,
             this dataset is on a global scale as it has plants from all over the
             globe on all seven continents. Different countries will produce one
             type of energy more than another, or one part of the world may have
             mastered the production of electricity from renewable energy sources."),
           br(),
           p("The dataset can be found at this ",
             a("link.",
               href = "https://datasets.wri.org/dataset/globalpowerplantdatabase")),
           br(),
           h2("Author of code"),
           p("The code for this Shiny App was written by Andres Tapia. At the time of this release,
              I am currently in my second semester of my third year at the University of Illinois
              at Chicago. This project was something fun that my professor proposed and I wanted to
             do it during the end of the semester.")
  )
)

server <- function(input, output, session) {
  
  globalReactive <- reactive({
    if(input$continent == "North America")
    {
      globalData[globalData$Country %in% northAmerica & 
                   globalData$Fuel %in% input$fuel &
                   globalData$Capacity >= input$slider1 &
                   globalData$Capacity < input$slider2,]
    }
    else if(input$continent == "South America")
    {
      globalData[globalData$Country %in% southAmerica & 
                   globalData$Fuel %in% input$fuel,]
    }
    else if (input$continent == "Europe")
    {
      globalData[globalData$Country %in% europe & 
                   globalData$Fuel %in% input$fuel &
                   globalData$Capacity >= input$slider1 &
                   globalData$Capacity < input$slider2,]
    }
    else if (input$continent == "Africa")
    {
      globalData[globalData$Country %in% africa & 
                   globalData$Fuel %in% input$fuel &
                   globalData$Capacity >= input$slider1 &
                   globalData$Capacity < input$slider2,]
    }
    else if (input$continent == "Asia")
    {
      globalData[globalData$Country %in% asia & 
                   globalData$Fuel %in% input$fuel &
                   globalData$Capacity >= input$slider1 &
                   globalData$Capacity < input$slider2,]
    }
    else if (input$continent == "Oceania")
    {
      globalData[globalData$Country %in% oceania & 
                   globalData$Fuel %in% input$fuel &
                   globalData$Capacity >= input$slider1 &
                   globalData$Capacity < input$slider2,]
    }
    else if (input$continent == "Antarctica")
    {
      globalData[globalData$Country %in% antartica & 
                   globalData$Fuel %in% input$fuel &
                   globalData$Capacity >= input$slider1 &
                   globalData$Capacity < input$slider2,]
    }
  })
  
  output$globalMap2019 <- renderLeaflet({
    globalMap <- globalReactive()
    if(input$fuel == "All")
    {
      updateCheckboxGroupInput(session, "fuel",
                               selected = sort(c(unique(globalData$Fuel), "All")))
    }
    map <- leaflet(globalMap)
    map <- addTiles(map)
    map <- addCircles(map,
                      lng = globalMap$Longitude,
                      lat = globalMap$Latititude,
                      radius = (globalMap$Capacity * 40),
                      color = colorFactors(globalMap$Fuel),
                      popup = paste("Country:", globalMap$Country, "<br>",
                                    "Plant:", globalMap$Plant, "<br>",
                                    "Capacity:", globalMap$Capacity, "<br>",
                                    "Fuel Type:", globalMap$Fuel))
    map <- addResetMapButton(map)
    map <- addLegend(map, "topright", colorFactors, values = globalMap$Fuel)
    map
  })
}

shinyApp(ui = ui, server = server)

#globalData[globalData$Country %in% northAmerica,]