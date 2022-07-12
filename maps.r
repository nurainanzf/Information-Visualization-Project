library(readr)
library(janitor)
library(leaflet)
library(dplyr)
library(sp)
library(plotly)
library(shiny)

#importing globalcrops.csv
climate <- read.csv("globalcrops.csv")
View(climate)

#importing worldcoord.csv
coordinates <- read.csv("worldcoord.csv")
View(coordinates)

#importing codes.csv
codes <- read.csv("codes.csv")
View(codes)

#cleaning the data
climate <- climate %>%
  janitor::clean_names()#clean names
  
#selecting specific columns 
#removing missing values with na.omit
df <- climate[, c("i_country", "act_ch_wha1f2020", "act_ch_mza1f2020", "act_ch_ria1f2020")] %>%
      na.omit(df)
df

#combine tables together
#new data frame df2
df2 = merge(x=df, y=coordinates, by="i_country")
df2


#selecting specific columns 
#removing missing values with na.omit
df3 <- climate[, c("i_country", "act_ch_wha1f2050", "act_ch_mza1f2050", "act_ch_ria1f2050")] %>%
  na.omit(df)
df3

#combine tables together
#new data frame df3
df4 = merge(x=df3, y=coordinates, by="i_country")
df4


#selecting specific columns 
#removing missing values with na.omit
df5 <- climate[, c("i_country", "act_ch_wha1f2080", "act_ch_mza1f2080", "act_ch_ria1f2080")] %>%
  na.omit(df)
df5

#combine tables together
#new data frame df6
df6 = merge(x=df5, y=codes, by="i_country")
df6

#changing data of latitude and longitude into numerical value 
df2$latitude <- as.numeric(df2$latitude)
df2$longitude <- as.numeric(df2$longitude)

#creating a new spatial point data frame 
#to echo spatial point data frame  there
#for mapping
# - 
#getting spatial data from columns 5 and 6
#the
df2.SP <- SpatialPointsDataFrame(df2[,c(5,6)], df2[,-c(5,6)])


#changing data of latitude and longitude into numerical value 
df4$latitude <- as.numeric(df4$latitude)
df4$longitude <- as.numeric(df4$longitude)

#creating a new spatial point data frame 
#to echo spatial point data frame  there
#for mapping
# - 
#getting spatial data from columns 5 and 6
#the
df4.SP <- SpatialPointsDataFrame(df4[,c(5,6)], df4[,-c(5,6)])

#adding font style
fontStyle <- list(
  family = "Arial",
  size = 15,
  color = "black"
)

#editing the label
label <- list(
  bgcolor = "#EEEEEE",
  bordercolor = "transparent",
  font = fontStyle
)

#adding hover
df6 <- df6 %>%
  mutate(hover = paste0("Country: ", i_country, "\n", "Wheat Total Production Change: ", act_ch_wha1f2080))

#changing colors
countCol <- colorFactor(palette = 'RdYlGn', df4$i_country)

#plotting maps using leaflet
print (
  m <- leaflet() %>%
  addProviderTiles("Esri.WorldImagery") %>%
  addCircleMarkers(data = df2, lng = ~longitude, lat = ~latitude, 
             popup = paste("Country:", df2$i_country, "<br>",
                           "Wheat Total Production Changes:", df2$act_ch_wha1f2020, "<br>",
                           "Maize Total Production Changes:", df2$act_ch_mza1f2020, "<br>",
                           "Rice Total Production Changes:", df2$act_ch_ria1f2020),
             color = "red"
             )
)

print (
  m2 <- leaflet() %>%
    addTiles()%>%
    #addProviderTiles("Esri.WorldGrayCanvas") %>%
    addCircles(data = df4, lng = ~longitude, lat = ~latitude, 
               popup = paste("Country:", df4$i_country, "<br>",
                             "Wheat Total Production Changes:", df4$act_ch_wha1f2050, "<br>",
                             "Maize Total Production Changes:", df4$act_ch_mza1f2050, "<br>",
                             "Rice Total Production Changes:", df4$act_ch_ria1f2050),
               color = ~countCol(i_country)
    )
)


#plotting maps using plotly
print(
  m3 <- plot_geo(df6,
                 locationmode = 'ISO-3')%>%
    add_trace(locations = ~code,
              z = ~act_ch_wha1f2080,
              zmin = 0,
              colorscale = 'Electric',
              zmax = max(df6$act_ch_wha1f2080),
              color = ~act_ch_wha1f2080,
              text = ~hover,
              hoverinfo = 'text')%>%
    colorbar(title= list(text ="<b>Wheat Total Production Change (ActChWHA1F02080)</b>")) %>%
    layout(font = list(family = "Arial"),
           title = list(text ="<b>Wheat Total Production Changes in 2080 Applying the SRES A1F2080 Scenario Yield Change to the 1990 Production</b>" ))
)

print(
  m4 <- plot_geo(df6,
                 locationmode = 'ISO-3')%>%
    add_trace(locations = ~code,
              z = ~act_ch_mza1f2080,
              zmin = 0,
              colorscale = 'Blues',
              zmax = max(df6$act_ch_wha1f2080),
              color = ~act_ch_mza1f2080,
              text = ~hover,
              hoverinfo = 'text')%>%
    colorbar(title= list(text ="<b>Maize Total Production Change (ActChMZA1F02080)</b>")) %>%
    layout(font = list(family = "Arial"),
           title = list(text ="<b>Maize Total Production Changes in 2080 Applying the SRES A1F2080 Scenario Yield Change to the 1990 Production</b>" ))
)

print(
  m5 <- plot_geo(df6,
                 locationmode = 'ISO-3')%>%
    add_trace(locations = ~code,
              z = ~act_ch_ria1f2080,
              zmin = 0,
              colorscale = 'Viridis',
              zmax = max(df6$act_ch_ria1f2080),
              color = ~act_ch_ria1f2080,
              text = ~hover,
              hoverinfo = 'text')%>%
    colorbar(title= list(text ="<b>Rice Total Production Change (ActChRIA1F02080)</b>")) %>%
    layout(font = list(family = "Arial"),
           title = list(text ="<b>Rice Total Production Changes in 2080 Applying the SRES A1F2080 Scenario Yield Change to the 1990 Production</b>" ))
)



#displaying all those charts in shiny

ui <- fluidPage(
  
  #creating the navigation bar for changing between charts
  (navbarPage(title="Climate Change Impact Charts",
                      tabPanel("SRES A1F2020 Scenario", 
                               leafletOutput("myplot"),
                               h4("Figure 1", align = "center"),
                               h5("Wheat, Maize and Rice Total Production Changes in 2020 Applying the SRES A1F2020 Scenario Yield Change to 1990 Production",
                                  align = "center")
                      ),
              tabPanel("SRES A1F2050 Scenario",
                       leafletOutput("myplot2"),
                       h4("Figure 2", align = "center"),
                       h5("Wheat, Maize and Rice Total Production Changes in 2050 Applying the SRES A1F2050 Scenario Yield Change to 1990 Production")
                       ),
              tabPanel("SRES A1F2080 Scenario",
                       plotlyOutput("myplot3"),
                       radioButtons(inputId = "plot_type" , 
                       label = "Select any plot:", 
                       choices = c("Rice", "Wheat", "Maize" ),
                       inline = TRUE
                      )
                  )
              )
   
   )
)
  


server <- function(input, output, session) {
  
  #leaflet map 1
  output$myplot <- renderLeaflet({
    
    m <- leaflet() %>%
      addProviderTiles("Esri.WorldImagery") %>%
      addCircleMarkers(data = df2, lng = ~longitude, lat = ~latitude, 
                       popup = paste("Country:", df2$i_country, "<br>",
                                     "Wheat Total Production Changes:", df2$act_ch_wha1f2020, "<br>",
                                     "Maize Total Production Changes:", df2$act_ch_mza1f2020, "<br>",
                                     "Rice Total Production Changes:", df2$act_ch_ria1f2020),
                       color = "red"
      )
      
  })
  
  #leaflet map 2
  output$myplot2 <- renderLeaflet({
    
    m2 <- leaflet() %>%
      addTiles()%>%
      #addProviderTiles("Esri.WorldGrayCanvas") %>%
      addCircles(data = df4, lng = ~longitude, lat = ~latitude, 
                 popup = paste("Country:", df4$i_country, "<br>",
                               "Wheat Total Production Changes:", df4$act_ch_wha1f2050, "<br>",
                               "Maize Total Production Changes:", df4$act_ch_mza1f2050, "<br>",
                               "Rice Total Production Changes:", df4$act_ch_ria1f2050),
                 color = ~countCol(i_country)
      )
    
    
  })
  
  #plotly map 3 (consists of 3 maps)
  output$myplot3 <- renderPlotly({
    
    #If user chooses to rice
    #then the following code will display the rice chart
    if (input$plot_type == "Rice") {
      m5 <- plot_geo(df6,
                     locationmode = 'ISO-3')%>%
        add_trace(locations = ~code,
                  z = ~act_ch_ria1f2080,
                  zmin = 0,
                  colorscale = 'Viridis',
                  zmax = max(df6$act_ch_ria1f2080),
                  color = ~act_ch_ria1f2080,
                  text = ~hover,
                  hoverinfo = 'text')%>%
        colorbar(title= list(text ="<b>Rice Total Production Change (ActChRIA1F02080)</b>")) %>%
        layout(font = list(family = "Arial"),
               title = list(text ="<b>Rice Total Production Changes in 2080 Applying the SRES A1F2080 Scenario Yield Change to the 1990 Production</b>" ))
    } 
    
    #If user chooses wheat
    #then the following code will display the wheat chart
    else if (input$plot_type == "Wheat") {
      
      m3 <- plot_geo(df6,
                     locationmode = 'ISO-3')%>%
        add_trace(locations = ~code,
                  z = ~act_ch_wha1f2080,
                  zmin = 0,
                  colorscale = 'Electric',
                  zmax = max(df6$act_ch_wha1f2080),
                  color = ~act_ch_wha1f2080,
                  text = ~hover,
                  hoverinfo = 'text')%>%
        colorbar(title= list(text ="<b>Wheat Total Production Change (ActChWHA1F02080)</b>")) %>%
        layout(font = list(family = "Arial"),
               title = list(text ="<b>Wheat Total Production Changes in 2080 Applying the SRES A1F2080 Scenario Yield Change to the 1990 Production</b>" ))
    }
    
    #if user chooses maize
    #then the following code will display the chart
    else if (input$plot_type == "Maize") {
      
      m4 <- plot_geo(df6,
                     locationmode = 'ISO-3')%>%
        add_trace(locations = ~code,
                  z = ~act_ch_mza1f2080,
                  zmin = 0,
                  colorscale = 'Blues',
                  zmax = max(df6$act_ch_wha1f2080),
                  color = ~act_ch_mza1f2080,
                  text = ~hover,
                  hoverinfo = 'text')%>%
        colorbar(title= list(text ="<b>Maize Total Production Change (ActChMZA1F02080)</b>")) %>%
        layout(font = list(family = "Arial"),
               title = list(text ="<b>Maize Total Production Changes in 2080 Applying the SRES A1F2080 Scenario Yield Change to the 1990 Production</b>" ))
    }
  })
  
  
  
}

shinyApp(ui, server)



