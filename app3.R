library(shiny)
library(ggplot2)
library(ggthemes)
library(plotly)
library(readr)
library(tidyverse)
library(maptools)
library(maps)
library(ggmap)
library(Hmisc)
library(readxl)
library(shinydashboard)

#Main File
countypop <- read_csv("../countypop1900_2016.csv")
df <- read_excel("../countypop1900_2016.xlsx")
mn_counties <-map_data("county", "minnesota")
mn_State <- map_data("state", "minnesota")
mn_counties_extended <- merge(mn_counties, df, by.x = "subregion", by.y = "Name")
col <- c("#E7F5D9", "#C7EF99", "#90E033", "#6AC400", "#5CA81F", "#076324")
col <- rev(col)

population <- read_csv("../countypop1900_2016andy.csv")
years1 <- names(population)[1:1]
aitkin1 <- names(population)[2:2]
len1 <- length(colnames(population))
namesData1 <- names(population)[2:len1]
len1 <- length(colnames(population))

mhi <- read_csv("../mhi1980_2016.csv")
mhi_ruca <- merge(mhi, countypop[, c("countyfp", "Dem_RUCA", "Dem_Desc")], by = "countyfp", all.x=TRUE)
mhi <- t(mhi)
colnames(mhi) <- mhi[1, ]
mhi <- data.frame(mhi)
mhi <- mhi[-1, ]
mhi <- mhi[-1, ]
colnames(mhi)[1] <- "Year"
mhi$Year <- c("1980", "1990", "2000", "2012", "2016")

pci <- read_csv("../bea - per capita income county.csv")
pci_ruca <- merge(pci, countypop[, c("countyfp", "Dem_RUCA", "Dem_Desc")], by = "countyfp", all.x=TRUE)
pci <- t(pci)
colnames(pci) <- pci[2, ]
pci <- data.frame(pci)
pci <- pci[-1, ]
pci <- pci[-1, ]
year <- c(1969:2016)
pci$Year <- as.character(year)

years <- names(pci)[89:89]
aitkin <- names(pci)[2:2]
len <- length(colnames(pci))
len <- len - 1
namesData <- names(pci)[2:len]
len <- length(colnames(pci))



mhi_ruca <- mhi_ruca[-1, ]
mhi_ruca <- aggregate(mhi_ruca[,3:7], list(mhi_ruca$Dem_Desc), mean)
colnames(mhi_ruca) <- c("Group", "1980", "1990", "2000", "2012", "2016")
mhi_ruca <- t(mhi_ruca)
colnames(mhi_ruca) <- mhi_ruca[1,]
mhi_ruca <- data.frame(mhi_ruca)
mhi_ruca <- mhi_ruca[-1, ]
mhi_ruca$Year <- rownames(mhi_ruca)
colnames(mhi_ruca)[5] <- "Year"
years_last <- "Year"
"Entirely rural" <- names(mhi_ruca)[2:2]
len_last <- length(colnames(mhi_ruca))
len_last <- len_last - 1
namesData_last <- names(mhi_ruca)[1:len_last]
len_last <- length(colnames(mhi_ruca))
rownames(mhi_ruca) <- c(1:length(rownames(mhi_ruca)))

medianage <- read_csv("../medianage1980-2010.csv")
medianage$Name <- tolower(medianage$Name)
colnames(medianage) <- c("Name", "countyfp", "pop1980", "pop1990", "pop2000", "pop2010", "pop2016")
mn_counties_extended2 <- merge(mn_counties, medianage, by.x = "subregion", by.y = "Name")

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
                    @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
                    
                    header{
                    text-align: center;
                    }
                    
                    h1 {
                    font-family: 'Lobster', cursive;
                    font-weight: 500;
                    line-height: 1.1;
                    color: #48ca3b;
                    }
                    
                    .Titleimg {
                    zoom: 50%;
                    }
                    
                    body {
                    background-image: url('Lake2.jpeg');
                    background-size: 100% auto;
                    background-repeat: no-repeat;
                    align: center;
                    }
                    
                    .plot2 {
                    width: 100%;
                    height: 600px;
                    }
                    
                    .boxWhite {
                    margin: 0 auto;
                    
                    background-color: #ffff;
                    }
                    
                    "))
  ),
  headerPanel(div(img(class="Titleimg", src="AtlasLogo.png"))),
  titlePanel("Atlas Project"),
  sidebarPanel(
    conditionalPanel(condition="input.conditionedPanels==1",
                     helpText("Population Maps"),
                     selectInput("topic", "Choose a state:",
                                 list(`Rural-Urban Commuting Area (RUCA)` = c("Dem_Desc", "Dem_RUCA"),
                                      `Census Years` = c("Population 2010", "Population 2000", "Population 1990", "Population 1980",
                                                         "Population 1970", "Population 1960", "Population 1950", "Population 1940",
                                                         "Population 1930", "Population 1920", "Population 1910", "Population 1900"),
                                      `Population 2016` = "Pop2010"))
    ),
    conditionalPanel(condition="input.conditionedPanels==2",
                     helpText("Population Linecharts")
    ),
    conditionalPanel(condition="input.conditionedPanels==3",
                     helpText("MHI Linecharts")
    ),
    conditionalPanel(condition="input.conditionedPanels==4",
                     helpText("PCI Linecharts")
    ),
    conditionalPanel(condition="input.conditionedPanels==5",
                     helpText("By RUCA Linecharts")
    )
    # conditionalPanel(condition="input.conditionedPanels==6",
    #                  helpText("By Median Age Maps"),
    #                  selectInput("topic", "Choose a state:",
    #                              list(`Census Years` = c("Population 2010", "Population 2000", "Population 1990", "Population 1980",
    #                                                      "Population 1970", "Population 1960", "Population 1950", "Population 1940",
    #                                                      "Population 1930", "Population 1920", "Population 1910", "Population 1900"))
    #                  )
    # )
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Population Maps", value=1, div(class="graph", plotlyOutput("plot2"))), 
      tabPanel("Population Linecharts", value=2, plotlyOutput("plot")),
      tabPanel("MHI Linecharts", value=3, plotlyOutput("plot3")), 
      tabPanel("PCI Linecharts", value=4, plotlyOutput("plot4")),
      tabPanel("By RUCA Linecharts", value=5, plotlyOutput("plot5")) 
      #tabPanel("By Median Age Maps", value=6, plotlyOutput("plot2"))
      , id = "conditionedPanels"
    )
  )
)

server <- function(input, output) {
  
  output$plot2 <- renderPlotly({
    text <- input$topic
    text <- match(text, names(mn_counties_extended));
    print(
      ggplotly(
        ggplot(mn_counties_extended, aes(long, lat)) +
          geom_polygon(aes(group = group, fill = mn_counties_extended[[text]], 
                           text=paste("County:", capitalize(subregion), '<br>', "Population 2016: ", prettyNum(pop2016,big.mark=",", preserve.width="none") )), 
                       colour = alpha("black", 1/2)) + theme_void() + scale_fill_manual(values = col)))
    
  })
  
  
  output$plot <- renderPlotly({
    p <- plot_ly(population, x = population[[years1]], y = population[[aitkin1]], name = 'Aitkin', mode = 'lines', visible = "legendonly")
    for(i in namesData1){
      p <- add_trace(p, x = population[[years1]], y = population[[i]], name = toString(i), mode="lines", type = "scatter", evaluate = TRUE)
    }
    options(warn = -1)
    p %>% layout(xaxis = list(range = c(1900, 2016)))
  }) 
  
  output$plot3 <- renderPlotly({
    p <- plot_ly(mhi, x = mhi[[years]], y = mhi[[aitkin]], name = 'Aitkin', mode = 'lines', visible = "legendonly")
    for(i in namesData){
      p <- add_trace(p, x = mhi[[years]], y = mhi[[i]], name = toString(i), mode="lines", type = "scatter", evaluate = TRUE)
    }
    options(warn = -1)
    p %>% layout(xaxis = list(range = c(1980, 2016)))
  }) 
  
  output$plot4 <- renderPlotly({
    p <- plot_ly(pci, x = pci[[years]], y = pci[[aitkin]], name = 'Aitkin', mode = 'lines', visible = "legendonly")
    for(i in namesData){
      p <- add_trace(p, x = pci[[years]], y = pci[[i]], name = toString(i), mode="lines", type = "scatter", evaluate = TRUE)
    }
    options(warn = -1)
    p %>% layout(xaxis = list(range = c(1969, 2016)))
  }) 
  
  output$plot5 <- renderPlotly({
    p <- plot_ly(mhi_ruca, x = mhi_ruca[[years_last]], y = mhi_ruca[["Entirely.rural"]], name = 'Entirely.rural', mode = 'lines', visible = "legendonly")
    for(i in namesData_last){
      p <- add_trace(p, x = mhi_ruca[[years_last]], y = mhi_ruca[[i]], name = toString(i), mode="lines", type = "scatter", evaluate = TRUE)
    }
    options(warn = -1)
    p %>% layout(xaxis = list(range = c(1980, 2016)))
  })
  
  output$plot6 <- renderPlotly({
    text <- input$year
    text <- match(text, names(mn_counties_extended2));
    print(
      ggplotly(
        ggplot(mn_counties_extended2, aes(long, lat)) +
          geom_polygon(aes(group = group, fill = mn_counties_extended2[[text]], 
                           text=paste("County:", capitalize(subregion), '<br>', "Population 2016: ", prettyNum(pop2016,big.mark=",", preserve.width="none") )), 
                       colour = alpha("black", 1/2)) + theme_void() + scale_fill_manual(values = col)))
    
  })
  
}

shinyApp(ui, server)
