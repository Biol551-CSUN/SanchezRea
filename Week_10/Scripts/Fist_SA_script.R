library(shiny)
library(tidyverse)
library(here)
library(lubridate)

BB <- read.csv(here("Week_10","Data","HatchBabyExport.csv"))

Bubble_Buns <- BB %>% 
  mutate(DateTime = mdy_hm(Start.Time)) %>% 
  mutate(DateTime - round_date(DateTime,"hour")) %>% 
  select(Baby.Name, DateTime, Activity, Amount) %>% 
  filter(Activity %in% c("Feeding","Diaper")) %>% 
  group_by(Baby.Name, DateTime) %>% 
  count(Activity)



# Define UI for application that draws a histogram
ui <- fluidPage("sup")
  
  # Application title
  titlePanel("Baby Buns"),
  
  # Sidebar with a slider input for number of bins 
  sidebarPanel(
    selectInput(
      inputId = "Baby.Name",
      label = "Baby",
      choices = c("Blakely","Micah")),
      selected = "Blakely" # default selection
    ), 
  ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  rBB <- reactive({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
}

# Run the application 
shinyApp(ui = ui, server = server)