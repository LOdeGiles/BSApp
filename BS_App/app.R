#
# App drafted by Lauren Ode-Giles, 5/12/2021
# ESCI 599 - Wk 08 Updates

library(shiny)
library(ggplot2)
library(vroom)
library(tidyverse)


#data wrangling
bsdat <- vroom::vroom("BSdf")
bsdat$length_miles <- bsdat$Shape_Length/5280
bsdat <- bsdat %>%
  rename(Armor = Armored)

AOI <- c("All Counties", unique(bsdat$CountyName)) #extract county name options
HUC <- c("All HUCs", unique(bsdat$HUC12Name)) #extract HUC name options
features <- colnames(bsdat)
features <- features[c(3, 6:7)]

stypeSel <- sort(unique(bsdat$Shoretype))
stypePal <- c("#f8f22c", "#0f7fd4", "#12b0b5", 
              "#6c2cf8", "#393939", "#989898", 
              "#a9b64f", "#bdfbfd", "#f6bdfd", 
              "#8a3d93", "#2dd40f")

dctypeSel <- sort(unique(bsdat$DCType))
dctypePal <- c("#247610", "#8e8e8e", "#8c070b")

armorSel <- sort(unique(bsdat$Armor))
armorPal <- c("#9fdf75", "#50504f")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Beach Strategies Data Exploration"),
    p("Select a region to explore, either by county or by hyrdrologic unit code (HUC) region name"),
    # Select regional attribute of interest
    fluidRow(
      column(4,
             selectInput(inputId = "region", 
                   label = "Select Region", 
                   choices = AOI)),
      column(4,
             selectInput(inputId = "huc",
                         label = "Select HUC name",
                         choices = HUC))),
    fluidRow(
      column(4,
             radioButtons("attribute", "Select attribute to display", features))
    ),
    hr(),
    
    p("This will be a dynamic summary plot that updates in response to user selections from the dropdown menu above, and that does not have a horrifying color palette. I would like to get this formatted so that the user can toggle between showing the cumulative length of the feature of interest (ex: there are 75 miles of transport zone shoretype in Pierce County) and showing the percent of the total shoreline in eace county made up by each feature (ex: Accretion shoreforms account for 10% of Whatcom County shores)."),
    #generate summary plot
    #this is a placeholder with an example plot; not yet dynamically updated
    plotOutput("summaryPlot"),
    #generate summary table
    dataTableOutput("summaryTable"),
    
    p("This will be a dynamic summary table that shows numerical values associated with the values shown in the 
      above plot"))

#define server-side functions
server <- function(input, output, session) {
    
    #make summary plot
    output$summaryPlot <- renderPlot({
        ggplot(bsdat, aes(fill = Shoretype, y = CountyName, x = length_miles))+
            scale_fill_manual(values = stypePal)+
            geom_bar(position = "stack", stat = "identity")+
            labs(x = "Cumulative feature length in County (miles)",
                 y = "County")+
            theme_bw()
        }, res = 96)
    #make summary table
    output$summaryTable <- renderDataTable(bsdat[,2:25], options = list(pageLength = 5))

}

# Run the application 
shinyApp(ui = ui, server = server)

