#
# App drafted by Lauren Ode-Giles, 5/12/2021
# ESCI 599 - Wk 07

library(shiny)
library(ggplot2)
library(vroom)

bsdat <- vroom::vroom("BSdf")
bsdat$length_miles <- bsdat$Shape_Length/5280
#there's more data wrangling I'll need to do in order to make this totally usable for plotting
#not currently that useful


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Beach Strategies Data Exploration"),
    p("This will be a selector tab that allows the user to specify which attributes they would like to investigate. This is currently not operational, but it will be once I figure out how I want to wrangle my data!"),
    # Select Attribute of interest
    varSelectInput("BSattribute", "Select Attribute", bsdat),
    hr(),
    
    p("This will be a dynamic summary plot that updates in response to user selections from the dropdown menu above, and that does not have a horrifying color palette. I would like to get this formatted so that the user can toggle between showing the cumulative length of the feature of interest (ex: there are 75 miles of transport zone shoretype in Pierce County) and showing the percent of the total shoreline in eace county made up by each feature (ex: Accretion shoreforms account for 10% of Whatcom County shores)."),
    #generate summary plot
    #this is a placeholder with an example plot; not yet dynamically updated
    plotOutput("summaryPlot"),
    
    p("This will be a dynamic summary table that shows numerical values associated with the values shown in the 
      above plot"))

#define server-side functions
server <- function(input, output, session) {
    
    #make summary plot
    output$summaryPlot <- renderPlot({
        ggplot(bsdat, aes(fill = Shoretype, y = CountyName, x = length_miles))+
            geom_bar(position = "stack", stat = "identity")+
            labs(x = "Cumulative feature length in County (miles)",
                 y = "County")+
            theme_bw()
        }, 
        res = 96)


}

# Run the application 
shinyApp(ui = ui, server = server)


    
