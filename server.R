library(shiny)
library(datasets)
require(ggplot2)
require(dplyr)

HH <- read.csv("WAOFM_-_April_1_-_Housing_by_State__County_and_City__1990_to_Present.csv", 
               header=TRUE, as.is=TRUE, numerals="no.loss", stringsAsFactors=FALSE)
      for(j in 5:16) { 
        for(i in 1:nrow(HH)) { 
          HH[i,j] <- ifelse(HH[i,j] == "." |HH[i,j] == "NA", NA, as.numeric(HH[i,j]))
        }
      }
      for(j in 5:16){ HH[,j] <- type.convert(HH[,j])}
      
      Counties <- unique(HH$COUNTY)

# Define server logic required to summarize and view the selected
# dataset
shinyServer(function(input, output) {
  
  # By declaring datasetInput as a reactive expression we ensure 
  # that:
  #
  #  1) It is only called when the inputs it depends on changes
  #  2) The computation and result are shared by all the callers 
  #    (it only executes a single time)
  #
  
  CountiesLoaded <- renderTable({ 
    Counties
  })
  
  datasetInput <- reactive({
    switch(input$dataset,
           "rock" = rock,
           "pressure" = pressure,
           "cars" = cars)
  })
  
  area1Input <- reactive({
    switch(input$area1,
           "Adams" = Adams,
           "Asotin" = Asotin,
           "Benton" = Benton,
           "Chelan" = Chelan,
           "Clallam" = Clallam,
           "Clark" = Clark,
           "Columbia" = Columbia,
           "Cowlitz" = Cowlitz,
           "Douglas" = Douglas,
           "Ferry" = Ferry,
           "Franklin" = Franklin,
           "Garfield" = Garfield,
           "Grant" = Grant,
           "Grays Harbor" = GraysHarbor,
           "Island" = Island,
           "Jefferson" = Jefferson,
           "King" = King,
           "Kitsap" = Kitsap,
           "Kittitas" = Kittitas,
           "Klickitat" = Klickitat,
           "Lewis" = Lewis,
           "Lincoln" = Lincoln,
           "Mason" = Mason,
           "Okanogan" = Okanogan,
           "Pacific" = Pacific,
           "Pend Oreille" = PendOreille,
           "Pierce" = Pierce,
           "San Juan" = SanJuan,
           "Skagit" =  Skagit,
           "Skamania" = Skamania,
           "Snohomish" = Snohomish,
           "Spokane" = Spokane,
           "Stevens" = Stevens,
           "Thurston" = Thurston,
           "Wahkiakum" = Wahkiakum,
           "Walla Walla" = WallaWalla,
           "Whatcom" = Whatcom,
           "Whitman" = Whitman,
           "Yakima" = Yakima,
           "State" = State
    )
  })
  
  area2Input <- reactive({
    switch(input$area2,
           "Adams" = Adams,
           "Asotin" = Asotin,
           "Benton" = Benton,
           "Chelan" = Chelan,
           "Clallam" = Clallam,
           "Clark" = Clark,
           "Columbia" = Columbia,
           "Cowlitz" = Cowlitz,
           "Douglas" = Douglas,
           "Ferry" = Ferry,
           "Franklin" = Franklin,
           "Garfield" = Garfield,
           "Grant" = Grant,
           "Grays Harbor" = GraysHarbor,
           "Island" = Island,
           "Jefferson" = Jefferson,
           "King" = King,
           "Kitsap" = Kitsap,
           "Kittitas" = Kittitas,
           "Klickitat" = Klickitat,
           "Lewis" = Lewis,
           "Lincoln" = Lincoln,
           "Mason" = Mason,
           "Okanogan" = Okanogan,
           "Pacific" = Pacific,
           "Pend Oreille" = PendOreille,
           "Pierce" = Pierce,
           "San Juan" = SanJuan,
           "Skagit" =  Skagit,
           "Skamania" = Skamania,
           "Snohomish" = Snohomish,
           "Spokane" = Spokane,
           "Stevens" = Stevens,
           "Thurston" = Thurston,
           "Wahkiakum" = Wahkiakum,
           "Walla Walla" = WallaWalla,
           "Whatcom" = Whatcom,
           "Whitman" = Whitman,
           "Yakima" = Yakima,
           "State" = State
    )
    })
  
  # The output$caption is computed based on a reactive expression
  # that returns input$caption. When the user changes the
  # "caption" field:
  #
  #  1) This function is automatically called to recompute the 
  #     output 
  #  2) The new caption is pushed back to the browser for 
  #     re-display()
  # ()
  # Note that because the data-oriented reactive expressions
  # below don't depend on input$caption, those expressions are
  # NOT called when input$caption changes.
  output$caption <- renderText({
    input$caption
  })
  
  output$selectedCounties <- renderText({
    paste("Selected Counties: ", input$area1, ",", input$area2, "Display Year = ", input$displayYear)
  })

  
  output$compareGraph <- renderPlot ({
    county1 <- input$area1
    county2 <- input$area2
    
    slideYear <- input$displayYear
    
    area1 <- colSums(HH[HH$COUNTY==county1,5:29], na.rm=TRUE)
    area2 <- colSums(HH[HH$COUNTY==county2,5:29], na.rm=TRUE)
    area1Years <- as.numeric(gsub("HU_", "", names(area1)))
    area2Years <- as.numeric(gsub("HU_", "", names(area2)))
    ds1 <- data.frame(county1, area1Years, area1, row.names=NULL)
    ds2 <- data.frame(county2, area2Years, area2, row.names=NULL)
    names(ds2) <- c("Area", "Year", "Households")
    names(ds1) <- c("Area", "Year", "Households")
    names(area1) <- area1Years
    names(area2) <- area2Years
    area1 <- c(county1, area1)
    area2 <- c(county2, area2)
    
    for ( i in 1:nrow(ds1)) 
        {ds1$HHRatio[i] <- 
            ifelse(i == 1, 0, (ds1$Households[i] - ds1$Households[i-1])/ds1$Households[i-1])}
    for ( i in 1:nrow(ds1)) 
        {ds2$HHRatio[i] <- 
            ifelse(i == 1, 0, (ds2$Households[i] - ds2$Households[i-1])/ds2$Households[i-1])}
    ds <- rbind(ds1, ds2)
    
    displayText0 <- paste("# of households for ", slideYear)
    displayText1 <- paste(county1, " = ", ds1$Households[which(ds1$Year == slideYear)])
    displayText2 <- paste(county2, " = ", ds2$Households[which(ds2$Year == slideYear)])
    textPositionX <- ifelse(slideYear > mean(mean(area1Years), mean(area2Years)), 1998, 2010)
    textPositionY <- 0.05
    displayTitle <- paste("# of house holds in ", slideYear, ": ", 
                          county1, " = ", ds1$Households[which(ds1$Year == slideYear)], " & ",
                          county2, " = ", ds2$Households[which(ds2$Year == slideYear)], " \n")
    
    comparePlot <- ggplot(ds, aes(x = Year, y = HHRatio)) + 
                     theme(panel.background = element_rect(fill = "lightblue")) + 
                     geom_line(data = ds, aes(x=Year, y = HHRatio, color = Area), lwd=1.5) +
                     labs(title = displayTitle) +
                     labs(y = "Rate of change in households") +
                     geom_vline(xintercept = slideYear, col="darkblue", lwd=1) 
                     
      
      # + geom_text(x=textPositionX, y=textPositionY, label=displayText0, col="darkblue", size = 5) +
      # geom_text(x=textPositionX, y=textPositionY - 0.0025, label=displayText1, col="red", size = 5) +
      # geom_text(x=textPositionX, y=textPositionY - 0.005, label=displayText2, col="red", size = 5)
    
    comparePlot <- comparePlot + abline(v=2000, col = "red", lwd = 2)
    
    
#      ggplot(ds, aes(y=Households, x=Year)) + 
#      theme(panel.background = element_rect(fill = "lightblue")) + 
#      facet_grid(. ~ Area) + 
#      stat_smooth(method=lm, fill = "pink", fullrange = TRUE) + 
#      geom_point(color="RED") + 
#      theme(legend.background = element_rect(fill = "blue"))

# Pie olot
# pie(x, lables = names(x), col = c("#fec44f", "#addd8e", "#756bb1"))


    comparePlot
    
  })
  
  # The output$view depends on both the databaseInput reactive
  # expression and input$obs, so will be re-executed whenever
  # input$dataset or input$obs is changed. 

  output$countiesData <- renderTable({
    county1 <- input$area1
    county2 <- input$area2
    area1 <- colSums(HH[HH$COUNTY==county1,5:29], na.rm=TRUE)
    area2 <- colSums(HH[HH$COUNTY==county2,5:29], na.rm=TRUE)
    area1Years <- as.numeric(gsub("HU_", "", names(area1)))
    area2Years <- as.numeric(gsub("HU_", "", names(area2)))
    ds1 <- data.frame(county1, area1Years, area1, row.names=NULL)
    ds2 <- data.frame(county2, area2Years, area2, row.names=NULL)
    names(ds2) <- c("Area", "Year", "Households")
    names(ds1) <- c("Area", "Year", "Households")
    names(area1) <- area1Years
    names(area2) <- area2Years
    area1 <- c(county1, round(area1/1000, digits = 0))
    area2 <- c(county2, round(area2/1000, digits = 0))
    countyData <- rbind(area1, area2)
    countyData
  })
})