###############################################################################
# Project: COVID-19 data visualisation dashboard                              #
# Author: William Lee                                                         #  
# Date: 10/3/2020                                                             #
# About: This code create a online dashboard to visualise data related to the #
# COVID-19 geographically. Shiny and Shinydashboard is used                   #
#                                                                             # 
###############################################################################


# Packages used:
# Shiny, Shinydashboard: for creation of dashboard
# rnaturalearth, sf: for mapping the data geographically
# ggplot2: For plotting time series and map
# ggrepel: for labelling data on map
# tidyverse, stringr: generate data and string operations
# RColorBrewer: For color scale



library(shiny)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos) #This is required for shinyapp.io
library(sf)
library(ggplot2)
library(stringr)
library(tidyverse)
library(RColorBrewer)
library(shinydashboard)
library(ggrepel)


#Specify the URL for retrieving the data for confirmed cases, recovered cases and deaths

url <- "https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/"
file_header <- "time_series_covid19_"
confirmed_url <- paste(url, file_header, "confirmed_global.csv", sep = '')
deaths_url <- paste(url, file_header, "deaths_global.csv", sep = '')
recovered_url <- paste(url, file_header, "recovered_global.csv", sep = '')


#Load Data, set NA to 0 as NA means 0 cases was discovered in that country

df_confirm <- read.csv(confirmed_url)
df_confirm[is.na(df_confirm)] <- 0
df_recover <- read.csv(recovered_url)
df_recover[is.na(df_recover)] <- 0
df_deaths <- read.csv(deaths_url)
df_deaths[is.na(df_deaths)] <- 0



#Pad up recover dataframe rows since it JHS has stopped updating it
#First, add rows to df_confirm and df_deaths in df_recover but not df_confirm 
df_lone <- anti_join(df_recover[,c(1:2)], df_confirm[,c(1:2)])
df_confirm <- bind_rows(df_confirm, df_lone)
df_deaths <- bind_rows(df_deaths, df_lone)
df_confirm[is.na(df_confirm)] <- 0
df_deaths[is.na(df_deaths)] <- 0

#Then, add rows that are missing in df_recover
df_missing <- anti_join(df_confirm[,c(1:2)], df_recover[,c(1:2)])
df_recover <- bind_rows(df_recover, df_missing)
df_recover[is.na(df_recover)] <- 0

#Sort all three dataframes
df_confirm <- df_confirm[order(df_confirm$Country.Region),]
df_deaths <- df_deaths[order(df_deaths$Country.Region),]
df_recover <- df_recover[order(df_recover$Country.Region),]


#Create dataframe for the current number of cases, considering df_recover would have less columns since JHS stopped updating
df_current <- df_confirm

for (i in 5:length(df_recover)){
    df_current[,c(i)] <- df_confirm[,c(i)] - df_deaths[,c(i)] - df_recover[,c(i)] 
}
if (length(df_recover) < length(df_current)){
  for (i in length(df_recover)+1:length(df_current)){
    df_current[,c(i)] <- df_confirm[,c(i)] - df_deaths[,c(i)] - df_recover[,c(length(df_recover))] 
  }
}


#current case = cconfirmed - deaths - recovered. Only the data rows are evaluated 
for (i in 5:length(df_confirm)){
  df_current[,c(i)] <- df_confirm[,c(i)] - df_deaths[,c(i)] - df_recover[,c(i)] 
  
}


#We will only consider cases by Country, so need to sum up all data for each country by aggregate()
df_death_world <- aggregate(df_deaths[,c(5:(length(df_deaths)))], by=list(Category=df_deaths$Country.Region), FUN=sum)
df_recover_world <- aggregate(df_recover[,c(5:(length(df_recover)))], by=list(Category=df_recover$Country.Region), FUN=sum)
df_confirm_world <- aggregate(df_confirm[,c(5:(length(df_confirm)))], by=list(Category=df_confirm$Country.Region), FUN=sum)
df_current_world <- aggregate(df_current[,c(5:(length(df_current)))], by=list(Category=df_current$Country.Region), FUN=sum)

#Create a df for the locations to obtain a latitude and longitude for each country for labelling. Median value taken if more than one Lat and Long
df_locations <- aggregate(df_confirm[, c(3,4)], by=list(Category=df_confirm$Country.Region), FUN=median)


#Rename country in the data to match that on WorldMap in rnaturalworld
df_death_world$Category <- recode(df_death_world$Category, 'US' = 'United States' , 'Korea, South' = 'Korea', "Taiwan*" = 'Taiwan', "Czechia" =  "Czech Rep.", "Congo (Kinshasa)" =  "Dem. Rep. Congo", "Dominican Republic" = "Dominican Rep.")
df_recover_world$Category <- recode(df_recover_world$Category, 'US' = 'United States' , 'Korea, South' = 'Korea', "Taiwan*" = 'Taiwan', "Czechia" =  "Czech Rep.", "Congo (Kinshasa)" =  "Dem. Rep. Congo", "Dominican Republic" = "Dominican Rep.")
df_confirm_world$Category <- recode(df_confirm_world$Category, 'US' = 'United States' , 'US' = 'United States' , 'Korea, South' = 'Korea', "Taiwan*" = 'Taiwan', "Czechia" =  "Czech Rep.", "Congo (Kinshasa)" =  "Dem. Rep. Congo", "Dominican Republic" = "Dominican Rep.")
df_current_world$Category <- recode(df_current_world$Category, 'US' = 'United States' , 'Korea, South' = 'Korea', "Taiwan*" = 'Taiwan', "Czechia" =  "Czech Rep.", "Congo (Kinshasa)" =  "Dem. Rep. Congo", "Dominican Republic" = "Dominican Rep.")
df_locations$Category <- recode(df_locations$Category, 'US' = 'United States' , 'Korea, South' = 'Korea', "Taiwan*" = 'Taiwan', "Czechia" =  "Czech Rep.", "Congo (Kinshasa)" =  "Dem. Rep. Congo", "Dominican Republic" = "Dominican Rep.")


#Calculate the total confirmed, recovered, deaths and current in the world to be displayed on dashboard
total_confirmed <- as.numeric(colSums(df_confirm_world[,c(2:length(df_confirm_world))], na.rm = TRUE)[length(df_confirm_world)-1])
total_recovered <- as.numeric(colSums(df_recover_world[,c(2:length(df_recover_world))], na.rm = TRUE)[length(df_recover_world)-1])
total_deaths <- as.numeric(colSums(df_death_world[,c(2:length(df_death_world))], na.rm = TRUE)[length(df_death_world)-1])
total_current <- as.numeric(colSums(df_current_world[,c(2:length(df_current_world))], na.rm = TRUE)[length(df_current_world)-1])


#The following creates the UI for the dashboard

ui <- dashboardPage(
  #Create header. Title need extra space
  dashboardHeader(title = "COVID-19 Data Visualisation Dashboard v1", titleWidth = 450),
  #Create sidebar for selections that is universal for all graphs. Here, selecting the type of case to be viewed
  dashboardSidebar(
    mainPanel(
      selectInput(inputId = "Cases", label = "Select A Category to View", choices = c("Confirmed", "Deaths", "Recovered", "Current")),
    width = 12)
  ),
  #Create dashboard main panel
  dashboardBody(
    
    #First row: Big boxes displaying the total number of cases for each category as of the latest data
    fluidRow(
      valueBoxOutput(outputId = "Total_Confirmed", width = 3),
      valueBoxOutput(outputId = "Total_Deaths", width = 3),
      valueBoxOutput(outputId = "Total_Recovered", width = 3),
      valueBoxOutput(outputId = "Total_Current", width = 3)

    ),
    #Second row: Plotting of the Map, and inputs for map display. The Date and region can be selected for the map view
    fluidRow(
     column(10, plotOutput(outputId = "Map")),
     column(2,  
            dateInput(inputId = "Date", label = "Select A Date to View", value = Sys.Date(), min = "2020-01-22", max = Sys.Date()),
            selectInput(inputId = "Location", label = "Select A Region to View", choices = c("The World", "East Asia", "North America", "South America", "Europe", "Middle East", "Australisia", "Africa"))),
   
    ),
    #Empty rows to seperate out the two graphs
    br(),
    br(),
    br(),
    #Third row: Plotting of time series plot, and inputs for the time series. Time series for each country can be selected and viewed
    fluidRow(
      column(10, plotOutput(outputId = "TimeSeries")),
      column(2, selectInput(inputId = "Countries", label = "Select A Country/Region", choices = c("All", as.character(df_confirm_world$Category))))
    )
  )
)


#The following code is the server which performs all the analysis
server <- function(input, output){
  #Function to decode column name into datestring. Column name in X.M.D.YYYY format
  decode_date <- function(x){
    last_date <- str_split(x, "\\.")
    year_string = paste("20", last_date[[1]][3], sep = '')
    month_string = sub("X", '', last_date[[1]][1])
    day_string = last_date[[1]][2]
    paste(year_string, month_string, day_string, sep = '-')
  }
  
  #Function to encode a datestring into the corresponding column name
  encode_date <- function(x){
    datestring <- str_split(x, '-')
    month_s <- as.character(as.numeric(datestring[[1]][2]))
    day_s <- as.character(as.numeric(datestring[[1]][3]))
    year_s <- as.character(as.numeric(datestring[[1]][1]) - 2000)
    col_sel <- paste("X", month_s, sep = '')
    paste(col_sel, day_s, year_s, sep = '.')
  }
  
  #Display the total cases in the corresponding value boxes
  output$Total_Confirmed <- renderValueBox({valueBox(total_confirmed, "Confirmed Cases")})
  
  output$Total_Deaths <- renderValueBox({valueBox(total_deaths, "Deaths")})
  
  output$Total_Recovered <- renderValueBox({valueBox(total_recovered, "Recovered (not updated since 27/3)")})

  output$Total_Current <- renderValueBox({valueBox(total_current, "Still in Care")})

  #Rendering the map
  output$Map <- renderPlot({
    
    #selected the correct dataframe based on the Cases option. Also change the display string and label threshold accordingly
    if (input$Cases == "Confirmed"){
      df_data <- df_confirm_world
      display_case <- "Confirmed Cases"
      threshold_world <- 5000
      threshold_region <- 500
    }
    if (input$Cases == "Recovered"){
      df_data <- df_recover_world
      display_case <- "Recovered Cases"
      threshold_world <- 500
      threshold_region <- 100
    }
    if (input$Cases == "Deaths"){
      df_data <- df_death_world
      display_case <- "Casualties"
      threshold_world <- 100
      threshold_region <- 5
    }
    if (input$Cases == "Current"){
      df_data <- df_current_world
      display_case <- "Current Cases"
      threshold_world <- 5000
      threshold_region <- 300
    }
    

    #Load in world data which contains the polygons required for mapping
    world <- ne_countries(scale = "medium", returnclass = "sf")
    
    #Combine the COVID-19 data with the world map data based on the country name
    combined_data <- left_join(world, df_data, by = c('name' = 'Category'))
    
    #Find out the last date in which the data was recorded
    last_date <- decode_date(colnames(combined_data)[length(combined_data) - 1])
    
    #if the date selected exceed the last date that data recorded, display the last set of data
    #else, display the data for the chosen date. This is done by comparing the encoded date with
    #the column names of the table. Set the name of that Column to Column_Plot to enable easy 
    #access
    
    
    if (as.Date(input$Date) > as.Date(last_date)){
      display_date <- last_date
      names(combined_data)[names(combined_data) == colnames(combined_data)[length(combined_data) - 1]] <- "Column_Plot"
    }
  
    else {
      display_date <- input$Date
      col_sel <- encode_date(input$Date)
      names(combined_data)[names(combined_data) == col_sel] <- "Column_Plot"
     
    }
    
    #Add in label data (which have the median Long and Lat for each country) 
    combined_data <- left_join(combined_data, df_locations, by = c('name' = 'Category'))
    
    
    #Bin data into appropiate bins to allow colour scale display
    combined_data$Column_Plot[is.na(combined_data$Column_Plot)] <- 0
    breaks <- c(-Inf, 0, 50, 100, 500, 1000, 5000, 10000, 50000, +Inf)
    names <- c("None Reported", "1 - 50", "50 - 100", "100 - 500", "500 - 1000", "1000 - 5000", "5000 - 10000", "10000 - 50000", "50000+")
    combined_data$data_bin <- cut(combined_data$Column_Plot, breaks = breaks, labels = names)
    
    #Limits the Long and Lat of the map displayed, and limit the labels being displayed to within the displayed region
    if (input$Location == "The World"){
      latitude = c(-80, 80)
      longitude = c(-175, 175)
      
      label_data <- subset(combined_data, 
                             Column_Plot > threshold_world)
    }
    else if (input$Location == "East Asia"){
      latitude = c(-5, 45)
      longitude = c(90, 150)
      label_data <- subset(combined_data, 
                             (as.character(continent) == "Asia" & 
                                (as.character(region_wb) == "East Asia & Pacific")))
        
    }
    else if (input$Location == "Middle East"){
      latitude = c(0, 45)
      longitude = c(30, 90)
      label_data <- subset(combined_data, 
                        (as.character(continent) == "Asia" & 
                          (as.character(region_wb) == "Middle East & North Africa" | 
                             as.character(region_wb) == "South Asia")))
    }
    else if (input$Location == "Europe"){
      latitude = c(30, 70)
      longitude = c(-25, 45)
      label_data <- subset(combined_data, 
                             (as.character(continent) == "Europe"))
    }
    else if (input$Location == "North America"){
      latitude = c(15, 75)
      longitude = c(-170, -45)
      label_data <- subset(combined_data, 
                             (as.character(continent) == "North America"))
    }
    else if (input$Location == "South America"){
      latitude = c(-60, 10)
      longitude = c(-105, -30)
      label_data <- subset(combined_data, 
                             (as.character(continent) == "South America"))
    }
    else if (input$Location == "Australisia"){
      latitude = c(-50, -5)
      longitude = c(105, 180)
      label_data <- subset(combined_data, 
                           (as.character(continent) == "Oceania"))
    }
    else if (input$Location == "Africa"){
        latitude = c(-35, 37.5)
        longitude = c(-25, 53)
        label_data <- subset(combined_data, 
                             (as.character(continent) == "Africa"))
    }
    
    
    #Generate the plot. 
   
    
    ggplot(data = world) + geom_sf() + 
        #Color in countries according to the number of cases. A Purple to Red Palette is used. 
        geom_sf(data = combined_data, aes(fill = data_bin)) + 
        scale_fill_brewer(palette = "PuRd", drop = FALSE) + 
        #Add labels for countries above threshold cases within the displayed map, use geom_label_repel to make sure they do not overlap
        geom_label_repel(data= label_data[((label_data$Column_Plot) > threshold_region),],aes(x=Long, y=Lat, label= paste(name, Column_Plot, sep = ":"))) +
        #Aesthetics - make background transparent, bold title, remove legend title, make it transparent 
        theme(plot.background = element_blank(), plot.title = element_text(face = "bold"), legend.title = element_blank(), legend.background = element_blank()) + 
        #Limit coordinates of the map according to region selected
        coord_sf(xlim = longitude, ylim = latitude, expand = TRUE) +
        #Add title according to case type, date, and location
        ggtitle(paste("Number of ", display_case, " of COVID-19 in ", input$Location,  " as of ", display_date, sep = ""))
   
  
     
  }, bg = "transparent") #make sure renderPlot background is transparent
  
  

  
  #Display time series plot
  output$TimeSeries <- renderPlot({
    
    #Again, select the appropiate dataframe, as well as giving it a color
    if (input$Cases == "Confirmed"){
      df_data2 <- df_confirm_world
      display_case <- "Confirmed Cases"
      color = "#0072B2"
    }
    if (input$Cases == "Recovered"){
      df_data2 <- df_recover_world
      display_case <- "Recovered Cases"
      color = "#CC79A7"
    }
    if (input$Cases == "Deaths"){
      df_data2 <- df_death_world
      display_case <- "Casualties"
      color = "#D55E00"
    }
    if (input$Cases == "Current"){
      df_data2 <- df_current_world
      display_case <- "Current Cases"
      color = "#009E73"
    }
    
    #Depending on which country is selected to select only those rows of that country
    #special case when all is selected - data on each date is summed to get the world total
    #A dataframe is create to put the date and data into columns
    if (input$Countries == "All"){
      display_country <- "the World"
      df_time<- data.frame("Date" = colnames(df_data2[,c(2:length(df_data2))]), 
                           "Data" = colSums(df_data2[,c(2:length(df_data2))], na.rm = TRUE))
    }
    else{
      display_country <- input$Countries
      print(input$Countries)
      df_time<- data.frame("Date" = colnames(df_data2[,c(2:length(df_data2))]), 
                           "Data" = as.numeric(df_data2[as.character(df_data2$Category) == as.character(input$Countries), c(2:length(df_data2))]))
      
    }

 

    #turn Dates into actual dates
    df_time$Date <- as.Date(sapply(as.character(df_time$Date), decode_date))
    
    #generate plot
    #Plot both line and points, appropiate title, and split the date scale appropiately. Also adjust plot margin for it to match the width of the map above better
    ggplot(df_time, aes(x=Date, y=Data)) + geom_point(color = color, size = 3) + geom_line(color = "black", size = 1) +
           ggtitle(paste(display_case, " in ",  display_country,  " by Date", sep = "")) +
           scale_x_date(date_breaks = "7 days", date_minor_breaks = "1 day")  + labs(x = "Date", y= "Cases") + 
           theme(plot.background = element_blank(), plot.title = element_text(face = "bold"), plot.margin = margin(t=0, r=5.9, b=0, l=0.68, unit = "cm"))
    
      
    
  }, bg="transparent", execOnResize = FALSE)
  
}

#create the Shiny App
shinyApp(ui = ui, server = server)