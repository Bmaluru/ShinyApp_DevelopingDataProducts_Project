library(shiny)

# Define UI for dataset viewer application
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Compare Selection"),
  
  # Sidebar with controls to provide a caption, select a dataset,
  # and specify the number of observations to view. Note that
  # changes made to the caption in the textInput control are
  # updated in the output area immediately as you type
  sidebarLayout(
    sidebarPanel(
                  
      selectInput("area1", "Select 1st County to compare:", 
                  choices = c("Adams", "Asotin", "Benton", "Chelan", "Clallam", "Clark", "Columbia",
                              "Cowlitz", "Douglas", "Ferry", "Franklin", "Garfield" , "Grant", "Grays Harbor", 
                              "Island", "Jefferson", "King", "Kitsap", "Kittitas", "Klickitat", "Lewis",
                              "Lincoln", "Mason", "Okanogan", "Pacific", "Pend Oreille", "Pierce", "San Juan", 
                              "Skagit", "Skamania", "Snohomish", "Spokane", "Stevens", "Thurston", "Wahkiakum", 
                              "Walla Walla", "Whatcom", "Whitman", "Yakima", "State"), selected = "King"),
      
      selectInput("area2", "Select 2nd County to compare:", 
                  choices = c("Adams", "Asotin", "Benton", "Chelan", "Clallam", "Clark", "Columbia",
                              "Cowlitz", "Douglas", "Ferry", "Franklin", "Garfield" , "Grant", "Grays Harbor", 
                              "Island", "Jefferson", "King", "Kitsap", "Kittitas", "Klickitat", "Lewis",
                              "Lincoln", "Mason", "Okanogan", "Pacific", "Pend Oreille", "Pierce", "San Juan", 
                              "Skagit", "Skamania", "Snohomish", "Spokane", "Stevens", "Thurston", "Wahkiakum", 
                              "Walla Walla", "Whatcom", "Whitman", "Yakima", "State"), selected = "Snohomish"),
      sliderInput("displayYear",
                  "Select Year to view # of households for the year:",
                  min = 1990,  max = 2014, value = 2014),
      
      helpText("README: To compare households trends by counties:"),
      helpText("SELECTION: select two counties from above dropdowns.",
               "You can also slide the Year bar to view # of households of the counties in the year selected."),
      helpText("DISPLAY: The graph shows comparative rate of change of households by year.",
               "The title of graph displays the # of households for the selected year of the selected counties."),
      helpText("Disclaimer: This a student project and is not meant for consumption.",
               "The above listed counties are from Washington State, USA.",
               "Data for this project is sourced from ", 
               "http://catalog.data.gov/dataset/waofm-april-1-housing-by-state-county-and-city-1990-to-present-78d5e")
      
    ),
    
    
    # Show the caption, a summary of the dataset and an HTML 
    # table with the requested number of observations
    mainPanel(
      
      
      h3("Compare households trends by counties (in WA state)"),
      h4(" "),
      
      h4("Graph: Rate of change in households per year"),
      plotOutput("compareGraph"),
      
      h4("# of households (in thousands), by year, for the selected counties:"),
      tableOutput("countiesData"),
      
      width = 8
    )
  )
))