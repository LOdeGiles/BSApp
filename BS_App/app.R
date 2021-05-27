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

#select shoretype; set up color palette
stypeSel <- sort(unique(bsdat$Shoretype))
stypePal <- c("#f8f22c", "#0f7fd4", "#12b0b5", 
              "#6c2cf8", "#393939", "#989898", 
              "#a9b64f", "#bdfbfd", "#f6bdfd", 
              "#8a3d93", "#2dd40f")

#select drift cell type; set up color palette
dctypeSel <- sort(unique(bsdat$DCType))
dctypePal <- c("#247610", "#8e8e8e", "#8c070b")

#change armor from numeric to character factor
bsdat$Armor <- as.character(bsdat$Armor)
bsdat$Armor <- plyr::revalue(bsdat$Armor, c("0"="Unarmored", "1" = "Armored"))
#select armor; set up color palette
armorSel <- sort(unique(bsdat$Armor))
armorPal <- c("#50504f", "#9fdf75")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Beach Strategies Data Exploration"),
    p("Explore Puget Sound shores! These plots and summary tables describe shoreline attributes in the Puget 
      Sound region, including mapped shoretype, the direction and length of littoral drift cells, and the presence
      of armor structures (bulkheads, revetments, etc.) in the nearshore area. These values can be explored by 
      County or by Hydrologic Unit Code (HUC)."),
    p("This data is sourced from the Beach Strategies for Nearshore Restoration and Protection in Puget Sound project. 
      To learn more about the project and explore the project story map, check out the Salish Sea Wiki page:"),
    a(href = "https://salishsearestoration.org/wiki/Beach_Strategies_for_Nearshore_Restoration_and_Protection_in_Puget_Sound", "Salish Sea Wiki - Beach Strategies"),
    hr(),
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
             radioButtons("attribute", "Select attribute to display", features))),
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
  
  #make a reactive to select a county
  selCounty <- reactive(bsdat %>% filter(CountyName == input$region))
  #make a reactive to select a huc
  selHUC <- reactive(bsdat %>% filter(bsdat$HUC12Name == input$huc))
    
  #make a reactive summary plot
  output$summaryPlot <- renderPlot({
    if(input$attribute == "DCType") {
      ggplot(bsdat, aes(fill = DCType, y = CountyName, x = length_miles))+
        scale_fill_manual(values = dctypePal)+
        geom_bar(position = "stack", stat = "identity")+
        labs(x = "Cumulative drift cell length in County by direction (miles)",
             y = "County",
             title = "Drift cell type")+
        theme_bw()
    } else if(input$attribute == "Shoretype") {
      ggplot(bsdat, aes(fill = Shoretype, y = CountyName, x = length_miles))+
        scale_fill_manual(values = stypePal)+
        geom_bar(position = "stack", stat = "identity")+
        labs(x = "Cumulative shoretype length in County (miles)",
             y = "County",
             title = "Shoretype")+
        theme_bw()
    } else {
      ggplot(bsdat, aes(fill = Armor, y = CountyName, x = length_miles))+
        scale_fill_manual(values = armorPal)+
        geom_bar(position = "stack", stat = "identity")+
        labs(x = "Cumulative shore length (armored and unarmored) in County (miles)",
             y = "County",
             title = "Shore armor presence/absence")+
        theme_bw()
    }
      }, res = 96)
  #make summary table
  output$summaryTable <- renderDataTable(bsdat[,2:25], options = list(pageLength = 5))

}

# Run the application 
shinyApp(ui = ui, server = server)

