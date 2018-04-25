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
countypop <- read_csv("C:/Users/Rocherno/Documents/countypop1900_2016.csv")
df <- read_excel("C:/Users/Rocherno/Documents/countypop1900_2016.xlsx")
mn_counties <-map_data("county", "minnesota")
mn_State <- map_data("state", "minnesota")
mn_counties_extended <- merge(mn_counties, df, by.x = "subregion", by.y = "Name")
col <- c("#E7F5D9", "#C7EF99", "#90E033", "#6AC400", "#5CA81F", "#076324")
col <- rev(col)

population <- read_csv("C:/Users/Rocherno/Documents/countypop1900_2016andy.csv")
years_population <- names(population)[1:1]
aitkin_population <- names(population)[2:2]
len_population <- length(colnames(population))
namesData_population <- names(population)[2:len_population]
len_population <- length(colnames(population))

mhi <- read_csv("C:/Users/Rocherno/Documents/mhi1980_2016.csv")
mhi_ruca <- merge(mhi, countypop[, c("countyfp", "Dem_RUCA", "Dem_Desc")], by = "countyfp", all.x=TRUE)
mhi <- t(mhi)
colnames(mhi) <- mhi[1, ]
mhi <- data.frame(mhi)
mhi <- mhi[-1, ]
mhi <- mhi[-1, ]
colnames(mhi)[1] <- "Year"
mhi$Year <- c(1980, 1990, 2000, 2012, 2016)
rownames(mhi) <- c(1:5)
years_mhi <- names(mhi)[1:1]
aitkin_mhi <- names(mhi)[2:2]
len_mhi <- length(colnames(mhi))
namesData_mhi <- names(mhi)[2:len_mhi]
colnames(mhi)[88] <- "Yellow Medicine"
mhi[] <- lapply(mhi, function(x) as.numeric(as.character(x)))

pci <- read_csv("C:/Users/Rocherno/Documents/bea - per capita income county.csv")
pci_ruca <- merge(pci, countypop[, c("countyfp", "Dem_RUCA", "Dem_Desc")], by = "countyfp", all.x=TRUE)
pci <- t(pci)
colnames(pci) <- pci[2, ]
pci <- data.frame(pci)
pci <- pci[-1, ]
pci <- pci[-1, ]
year <- c(1969:2016)
pci$Year <- as.character(year)
rownames(pci) <- c(1:length(rownames(pci)))

years_pci <- names(pci)[89:89]
aitkin_pci <- names(pci)[2:2]
len_pci <- length(colnames(pci))
len_pci <- len_pci - 1
namesData_pci <- names(pci)[2:len_pci]
len_pci <- length(colnames(pci))
colnames(pci)[88] <- "Yellow Medicine"
pci[] <- lapply(pci, function(x) as.numeric(as.character(x)))

mhi_ruca <- mhi_ruca[-1, ]
mhi_ruca <- aggregate(mhi_ruca[,3:7], list(mhi_ruca$Dem_Desc), mean)
colnames(mhi_ruca) <- c("Group", "1980", "1990", "2000", "2012", "2016")
mhi_ruca <- t(mhi_ruca)
colnames(mhi_ruca) <- mhi_ruca[1,]
mhi_ruca <- data.frame(mhi_ruca)
mhi_ruca <- mhi_ruca[-1, ]
mhi_ruca$Year <- rownames(mhi_ruca)
colnames(mhi_ruca) <- c("Entirely Rural", "Entirely Urban", "Town Rural Mix", "Urban Town Rural Mix", "Year")
years_mhi_ruca <- "Year"
"Entirely rural" <- names(mhi_ruca)[2:2]
len_mhi_ruca <- length(colnames(mhi_ruca))
len_mhi_ruca <- len_mhi_ruca - 1
namesData_mhi_ruca <- names(mhi_ruca)[1:len_mhi_ruca]
len_mhi_ruca <- length(colnames(mhi_ruca))
rownames(mhi_ruca) <- c(1:length(rownames(mhi_ruca)))
mhi_ruca[] <- lapply(mhi_ruca, function(x) as.numeric(as.character(x)))

medianage <- read_csv("C:/Users/Rocherno/Documents/medianage1980-2010.csv")
medianage$Name <- tolower(medianage$Name)
colnames(medianage) <- c("Name", "countyfp", "pop1980", "pop1990", "pop2000", "pop2010", "pop2016", "Population 1980", "Population 1990", "Population 2000", "Population 2010", "Population 2016")
mn_counties <-map_data("county", "minnesota")
medianage$Name <- df$Name
mn_counties_extended2 <- merge(mn_counties, medianage, by.x = "subregion", by.y = "Name")



ui <- dashboardPage(
  dashboardHeader(title = "Atlas Project"),
  dashboardSidebar(sidebarMenu(
    menuItem("Population Maps", tabName = "population-maps", icon = icon("dashboard")),
    menuItem("Population Linecharts", tabName = "population-linecharts", icon = icon("th")),
    menuItem("MHI Linecharts", tabName = "mhi-linecharts", icon = icon("dashboard")),
    menuItem("PCI Linecharts", tabName = "pci-linecharts", icon = icon("th")),
    menuItem("By RUCA Linecharts", tabName = "by-ruca-linecharts", icon = icon("th")),
    menuItem("By Median Age Maps", tabName = "median-population-maps", icon = icon("th"))
  )),
  dashboardBody(
    tabItems(
      tabItem(tabName="population-maps", fluid = TRUE,
              sidebarLayout(
                selectInput("topic", "Choose a state:",
                            list(`Rural-Urban Commuting Area (RUCA)` = c("Dem_Desc", "Dem_RUCA"),
                                 `Census Years` = c("Population 2010", "Population 2000", "Population 1990", "Population 1980",
                                                    "Population 1970", "Population 1960", "Population 1950", "Population 1940",
                                                    "Population 1930", "Population 1920", "Population 1910", "Population 1900"),
                                 `Population 2016` = "Pop2010")),
                mainPanel(
                  plotlyOutput("plot2")
                )
              )
      ),
      tabItem(tabName="population-linecharts", fluid = TRUE,
              mainPanel(
                plotlyOutput("plot")
              )
      ),
      tabItem(tabName="mhi-linecharts", fluid = TRUE,
              mainPanel(
                plotlyOutput("plot3")
              )
      ),
      tabItem(tabName="pci-linecharts", fluid = TRUE,
              mainPanel(
                plotlyOutput("plot4")
              )
      ),
      tabItem(tabName = "by-ruca-linecharts",fluid = TRUE,
              mainPanel(
                plotlyOutput("plot5")
              )
      ),
      tabItem(tabName="median-population-maps", fluid = TRUE,
              sidebarLayout(
                selectInput("topic_median", "Choose a state:",
                            list(`Years` = c("Population 2016", "Population 2010", "Population 2000", "Population 1990", "Population 1980")
                                 )),
                mainPanel(
                  plotlyOutput("plot6")
                )
              )
      )
      
              )
  )
)

server <- function(input, output) {
  
  
  output$plot <- renderPlotly({
    p <- plot_ly(population, x = population[[years_population]], y = population[[aitkin_population]], name = 'Aitkin', mode = 'lines', visible = "legendonly")
    for(i in namesData_population){
      p <- add_trace(p, x = population[[years_population]], y = population[[i]], name = toString(i), mode="lines", type = "scatter", evaluate = TRUE)
    }
    options(warn = -1)
    p %>% layout(xaxis = list(range = c(1900, 2016)))
  }) 
  
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
  
  output$plot3 <- renderPlotly({
    p <- plot_ly(mhi, x = mhi[[years_mhi]], y = mhi[[aitkin_mhi]], name = 'Aitkin', mode = 'lines', visible = "legendonly")
    for(i in namesData_mhi){
      p <- add_trace(p, x = mhi[[years_mhi]], y = mhi[[i]], name = toString(i), mode="lines", type = "scatter", evaluate = TRUE)
    }
    options(warn = -1)
    p %>% layout(xaxis = list(range = c(1980, 2016)))
  }) 
  
  output$plot4 <- renderPlotly({
    p <- plot_ly(pci, x = pci[[years_pci]], y = pci[[aitkin_pci]], name = 'Aitkin', mode = 'lines', visible = "legendonly")
    for(i in namesData_pci){
      p <- add_trace(p, x = pci[[years_pci]], y = pci[[i]], name = toString(i), mode="lines", type = "scatter", evaluate = TRUE)
    }
    options(warn = -1)
    p %>% layout(xaxis = list(range = c(1969, 2016)))
  }) 
  
  output$plot5 <- renderPlotly({
    p <- plot_ly(mhi_ruca, x = mhi_ruca[[years_mhi_ruca]], y = mhi_ruca[["Entirely Rural"]], name = 'Entirely Rural', mode = 'lines', visible = "legendonly")
    for(i in namesData_mhi_ruca){
      p <- add_trace(p, x = mhi_ruca[[years_mhi_ruca]], y = mhi_ruca[[i]], name = toString(i), mode="lines", type = "scatter", evaluate = TRUE)
    }
    options(warn = -1)
    p %>% layout(xaxis = list(range = c(1980, 2016)))
  })
  
  output$plot6 <- renderPlotly({
    text <- input$topic_median
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