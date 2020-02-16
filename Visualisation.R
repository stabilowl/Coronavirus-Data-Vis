#included required libraries

require(ggplot2)
require(zoo)
require(reshape2)
require(dplyr)
require(rio)
require(lubridate)
require(stringr)

#Set working directory
setwd("C:/Users/William Lee/Documents/Projects")

###########################################################################################################
# Part 1: Data from NHC (National Department of Health of China)
# This is official data updated daily in a written report form. Data extracted into a csv manually
# This data is for within Mainland China only, and is used for the more detailed breakdown of suspected cases
# and severe case within the confirmed cases
############################################################################################################

#Load in Data

filename = "C:/Users/William Lee/Documents/2019_ncov_data_csv.csv"
df <- read.csv(filename)

#Check Data via viewing
head(df)

#Put Date column in date format
df$Date <-as.Date(df$Date,"%d/%m/%Y")

#Create new cases column by subtracting successive rows from the Confirmed column. 
new_confirm = df$Confirmed
new_confirm[1] = 0
new_confirm[2:length(new_confirm)] = tail(df,-1)$Confirmed - head(df,-1)$Confirmed

#Do the same for the Suspected Column to get the new suspected cases. 
new_suspect = df$suspected
new_suspect[1] = 0
new_suspect[2:length(new_suspect)] = tail(df,-1)$suspect - head(df,-1)$suspect

df$new_confirm = new_confirm
df$new_suspect = new_suspect

#Check Data  
print(df)

#--------------------------------------------------------------------------------
#Plot 1: Confirmed and suspected cases vs time
#--------------------------------------------------------------------------------

# Step 1: Select column and move them into individual data tables. This is required for ggplot to work properly
df_confirm <- df[,c("Date", "Confirmed")]
df_suspected <- df[,c("Date", "suspected")]
colnames(df_confirm)<- c("Date","Cases")
colnames(df_suspected)<- c("Date","Cases")

#Step 2: Add label to the data
df_confirm$Label <- 'Confirmed'
df_suspected$Label <- 'Suspected'

#Step 3: Combined the data
df_cases <- rbind.data.frame(df_confirm, df_suspected)
print(df_cases)

#Step 4: Plot data
case_plot <- ggplot(df_cases, aes(x = Date, y = Cases)) + geom_point(aes(color = Label), size = 3) + geom_line(aes(color = Label), size = 1.5)
case_plot <- case_plot + ggtitle("Total number of cases of 2019-nCov reported in China over time")
case_plot + scale_x_date(date_breaks = "7 days", date_minor_breaks = "1 day") 

#----------------------------------------------------------------------------------
# Plot 2: New Cases vs time
#----------------------------------------------------------------------------------

#Repeat the same as above but use new_confirm and new_suspect column instead

df_new_confirm <- df[,c("Date","new_confirm")]
df_new_suspect <- df[,c("Date", "new_suspect")]
colnames(df_new_confirm) <- c("Date", "Cases")
colnames(df_new_suspect) <- c("Date", "Cases")
df_new_confirm$Label <- "Confirmed"
df_new_suspect$Label <- "Suspected"
df_new_cases <- rbind.data.frame(df_new_confirm, df_new_suspect)


# Plot the data
new_case_plot <- ggplot(df_new_cases, aes(x=Date, y=Cases)) + geom_point(aes(color = Label), size = 3) + geom_line(aes(color = Label), size = 1.5)
new_case_plot <- new_case_plot + ggtitle("Number of new cases of 2019-nCov reported by Date")
new_case_plot + scale_x_date(date_breaks = "7 days", date_minor_breaks = "1 day")

#Note:Graph looks different to published by media. That's because media does not take into account some suspected cases 
#can be determined to be not the virus. Using just the "new suspected cases" number does not take into account of that

#-----------------------------------------------------------------------------------
# Plot 3: Portion of Deaths, Serious, and discharged case over time
#-----------------------------------------------------------------------------------
#Step 1: fill in NA values from previous row using zoo::na.locf()
df <- na.locf(df)
#Step 2: Calculate mild (not severe) cases
df$Mild = df$Confirmed - df$deaths - df$Severe - df$discharged
#Step 3: Seperate out each column to form new data frame. A function from reshape2, called melt, can do this easily
df_Severity <- df[,c("Date","discharged","Mild","Severe","deaths")]
df_reshape <-melt(df_Severity,id.var="Date")

#Step 3: Plot bar graph

#create bar graphs - default position = "stack"
new_plot <- ggplot(df_reshape, aes(x = Date, y = value, fill = variable)) + geom_bar(stat = "identity")
# add title
new_plot <- new_plot + ggtitle("Number of Cases of 2019-nCov in China by day, catergorized by severity")
# changes scale in date axis
new_plot <- new_plot +  scale_x_date(date_breaks = "7 days", date_minor_breaks = "1 day")
# Change y-axis label to "Cases", remove legend title, and change legend label
new_plot + labs(y= "Cases") + theme(legend.title = element_blank()) + scale_fill_discrete(labels = c("Discharged", "Normal", "Severe", "Death"))

#-----------------------------------------------------------------------------------
# Plot 4: Stacked percentage bar graph of plot 3
#-----------------------------------------------------------------------------------

#Repeat the plotting in above, but use a "fill" position in geom_bar to get a stacked percentage graph
new_plot <- ggplot(df_reshape, aes(x = Date, y = value, fill = variable)) + geom_bar(position = "fill", stat = "identity")
new_plot <- new_plot + ggtitle("Percentage of severity of 2019-nCov cases in China by Date")
new_plot <- new_plot +  scale_x_date(date_breaks = "7 days", date_minor_breaks = "1 day")
new_plot <- new_plot + labs(y= "Percentage of Cases") + theme(legend.title = element_blank()) + scale_fill_discrete(labels = c("Discharged", "Normal", "Severe", "Death"))
#Need to convert y-axis into percentage values instead of between 0 and 1
new_plot + scale_y_continuous(labels = function(x) paste0(x*100, "%"))

#-----------------------------------------------------------------------------------------------
# Plot 5: Animated Pie Chart. The standard R pie plot is used as the labelling is cleaner
#-----------------------------------------------------------------------------------------------
#Generate a PNG for each date
for (i in df_Severity$Date){
  
  #select all data from that date, and relabel
  df_temp <- df_Severity[df_Severity$Date == as.Date(i),]
  colnames(df_temp) <- c("Date","Discharge","Normal", "Severe", "Deaths")
  #reshapre the data table for plotting, and relabel
  df_pie <-melt(df_temp,id.vars="Date")
  colnames(df_pie) <- c("Date","Severity","Cases")
  #Calculate percentage and create labels
  pct <- round(df_pie$Cases/sum(df_pie$Cases)*100, digits = 1)
  lbls <- paste(pct, "%", sep="")
  title = paste("Percentage of Severity of 2019-nCov as of", as.Date(i), sep = ":")
  pie(df_pie$Cases, labels = paste(df_pie$Severity, lbls, sep= ":"), main = title, col=rainbow(length(df_pie$Severity)))

}

############################################################################################################
#Part 2: The John Hopkins University (JHU) Dataset
# This dataset is compiled by the The Center for Systems Science and Engineering (CSSE) at JHU 
# They compiled the data from a variety of sources. They have used this dataset to create a online
# Dashboard which shows geographically the data for confirmed cases and deaths
# The dashboard is at:
# https://gisanddata.maps.arcgis.com/apps/opsdashboard/index.html#/bda7594740fd40299423467b48e9ecf6
# The link to the data is available at this link as well. The data has been downloaded and saved as
# seperate csv files.
############################################################################################################

#---------------------------------------------------------------------------------------------------------
#Note: The following session was obselete after John Hopskin put forward a much more well formated dataset.
#      The initial data set can still be found in the Github Page
#
#Load data (Source: John Hopskin). All excel sheets are loaded into the list
#data_list <- import_list("CoronaVirusData.xlsx", rbind = "True")

#Viewing the list shows that the data is poorly organised - some columns have different names even though they give the same stats
#Write a function to swap names to ensure same entity have same column name
#name_swap <- function(x){
#  if("Demised" %in% colnames(x)){
#    names(x)[names(x) == "Demised"] <- "Deaths" 
#  }
#  if ("Date last updated" %in% colnames(x)){
#    names(x)[names(x) == "Date last updated"] <- "Last Update"
#  }
#  if ("Country/Region" %in% colnames(x)){
#    names(x)[names(x) == "Country/Region"] <- "Country"
#  }
#  x
#
#}

#apply function to the whole list
#new_list <- lapply(data_list, name_swap)
#with the column names the same, we can now combine the data into a single dataframe
#new_data <- bind_rows(new_list)
#rename Last Update to Date - easier to call
#names(new_data)[names(new_data) == "Last Update"] <- "Date"
#Only keep the date. Since on somedays the data are recorded twice, we also need to remove all duplicates 
#new_data$Date <- as.Date(new_data$Date)
#new_data <- distinct(new_data)

#Now the data is ready to be visualised. We will start with Mainland China data again
#df_China <- new_data[new_data$Country == "Mainland China",c("Date", "Province/State", "Confirmed", "Deaths", "Recovered")]
#df_China$Deaths[is.na(df_China$Deaths)] <- 0
#df_China$Recovered[is.na(df_China$Recovered)] <- 0
#df_China$Confirmed[is.na(df_China$Confirmed)] <- 0
#names(df_China)[names(df_China) == "Province/State"] <- "Province"
#for (i in unique(df_China$Date))
#{
# df_bar <- df_China[df_China$Date == as.Date(i),c("Province", "Confirmed", "Deaths", "Recovered")]
#  df_bar <- melt(df_bar, id.vars = "Province")
#  p <- ggplot(df_bar, aes(x = Province, y = value, fill = variable)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
#  p <- p + ggtitle(paste("Cases of 2019-nCov in China by Province as of", as.Date(i), sep = ":")) + ylim(0, 20000)
#  print(p)
#}
  
#-------------------------------------------------------------------------------------------------------------

#Load in data. The data is seperated into three files - Confirmed.csv, Recovered.csv and Deaths.csv, which shows
#the confirmed, recovered and death cases respectively

df_confirm_geo <- read.csv("time_series_2019-ncov-Confirmed.csv")
#The following is obselete after even newer dataset is available
#Rename column to make it consistent with the other two files
#names(df_confirm_geo)[names(df_confirm_geo)=="First.confirmed.date.in.country..Est.."] <- "First.confirmed.date.in.country"

#Set all na to 0, as NA indicates no case has been detected
df_confirm_geo[is.na(df_confirm_geo)] <- 0

#Read in recovered cases
df_recover_geo <- read.csv("time_series_2019-ncov-Recovered.csv")
df_recover_geo[is.na(df_recover_geo)] <- 0

#The following is obselete in newer versions of data table where this is fixed
#upon viewing the data it is discovered that a column is missing in the Recovered.csv. This is added to allow data to
#be processed more smoothly
#df_recover_geo$X1.31.2020.7.00.PM <- df_recover_geo$X1.31.2020.2.00.PM

#Read in Death cases
df_deaths_geo <- read.csv("time_series_2019-ncov-Deaths.csv")
df_deaths_geo[is.na(df_deaths_geo)] <- 0

#Similar to before, substract deaths and recovery from confirmed cases to get cases that is still in care 
df_mild_geo <- df_confirm_geo

#Only the data rows are calculated
for (i in 5:length(df_confirm_geo)){
  df_mild_geo[,c(i)] <- df_confirm_geo[,c(i)] - df_deaths_geo[,c(i)] - df_recover_geo[,c(i)] 
  
}


#similarly for new cases. This time, it is substrated column-wise. This left a column for the initial date. 

df_newcase_geo <- df_confirm_geo

for (i in 6:length(df_newcase_geo))
{
  df_newcase_geo[,c(i)] <- df_confirm_geo[,c(i)] - df_confirm_geo[,c(i-1)]
}

#The first day column is removed since all new cases are calculated based on this
df_newcase_geo[,c(5)] <- NULL


#Create data frame to store the death rate
df_deathrate_geo <- df_confirm_geo

for (i in 5:length(df_deathrate_geo))
{
  df_deathrate_geo[,c(i)] <- (df_deaths_geo[,c(i)]/df_confirm_geo[,c(i-1)])*100
}


#Label cases to allow them to be categorised once combined into a single dataframe
df_mild_geo$Case <- "Admitted"
df_recover_geo$Case <- "Recovered"
df_deaths_geo$Case <- "Deaths"


#Combine the three datatables. Note that rbind only works if the dataframes have the same column
df_all <- rbind.data.frame(df_mild_geo, df_recover_geo, df_deaths_geo)

#Change column name to make it easier to access
names(df_all)[names(df_all) == "Province.State"] <- "Province"
names(df_all)[names(df_all) == "Country.Region"] <- "Country"

#For the first part of the analysis, only Mainland China data is used
df_China <- df_all[df_all$Country == "Mainland China",]



#-----------------------------------------------------------------------------------------------------
#First need to create a function that converts to column names into strings that resembles a datetime
#-----------------------------------------------------------------------------------------------------
#Note that the Datetime are all in format "XM.DD.YYYY.HH.MM.GG" where GG represents AM or PM. Some columns
#Don't have AM and PM and is presented in the 24hr format instead
decode_date <- function(x){
  #split the string by "."
  stringlist <- str_split(x, "\\.")[[1]]
  #Take out the month, day and year
  mon = gsub('X', '',  stringlist[1])
  day = stringlist[2]
  year = stringlist[3]
  #Convert hours to 24 hours format. Special case to seperate 12am vs 12pm
  hr = stringlist[4]
  
  if (length(stringlist) == 6){
    if ((stringlist[6] == "AM" && !stringlist[4] == 12)  || (stringlist[4] == "12" && stringlist[6] == "PM")){
      hr = stringlist[4]  
    }
    else if (stringlist[6] == "AM" && stringlist[4] == "12"){
      hr = "0"
    }else{
            hr = toString(as.numeric(stringlist[4]) + 12)
    }
    
  }
  #Take out the minute 
  min = stringlist[5]
  #Combine into a datestring that can be easily read visually and easily decoded by standard Datetime functions in R
  Datestring <- paste(day,"-",mon, "-", year, " ", hr, ":", min, sep = '')
  Datestring
}
#########################################################################################


#---------------------------------------------------------------------------------------
#Plot 1: Stack bargraphs for cases
#---------------------------------------------------------------------------------------

#The dataframe is already in a form that can be plotted with a bargraph. For each column which represents 1 day, 
#Decode the actual date and plot a bargraph. The resulting graphs can be saved to form a animation
for (i in 5:length(df_China)-1)
{
  df_bar <- df_China[,c("Province", "Case", colnames(df_China)[i])]
  names(df_bar)[names(df_bar) == colnames(df_China)[i]] <- "Value"
  df_bar$Value[is.na(df_bar$Value)] <- 0
  
  #seperate out column name into proper datetime string

  Datestring <- decode_date(colnames(df_China)[i])
  filename <- colnames(df_China)[i]
  
  #Adjust the axis labels to verticle so that the text don't overlap
  p <- ggplot(df_bar, aes(x = Province, y = Value, fill = Case)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.2))
  #Display the total number of cases for each province on top of the bar
  p <- p + stat_summary(aes(label = ..y.., group = Province), fun.y = sum, geom = "text", angle = 90, hjust = -0.2) 
  #Fix axis so that the increase in cases by day is clearer
  p <- p + ggtitle(paste("Cases of 2019-nCov in China by Province as of ", Datestring, sep = ":")) + ylim(0, 55000) + labs(y= "Cases") + labs(x= "Provinces") + theme(legend.title = element_blank()) 
  plot(p)
  ggsave(paste(filename, ".jpeg", sep = ""), plot = p, device = "jpeg")
}


#---------------------------------------------------------------------------------------
#Plot 2: Same as above but with Hubei removed
#---------------------------------------------------------------------------------------

#The graphs above is overly dominated by data from Hubei due to the large number of cases
#Here, we do the same plot but with Hubei removed to get an idea of what's happening in
#the other provinces
for (i in 6:length(df_China)-1)
{
  df_bar <- df_China[,c("Province", "Case", colnames(df_China)[i])]
  names(df_bar)[names(df_bar) == colnames(df_China)[i]] <- "Value"
  df_bar$Value[is.na(df_bar$Value)] <- 0
  df_bar <- df_bar[!df_bar$Province == "Hubei",]
  
  #See above for explanation
 
  Datestring <- decode_date(colnames(df_China)[i])
  filename <- paste(colnames(df_China)[i], "no_hubei", sep = "_")
  p <- ggplot(df_bar, aes(x = Province, y = Value, fill = Case)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.2))
  p <- p + stat_summary(aes(label = ..y.., group = Province), fun.y = sum, geom = "text", angle = 90, hjust = -0.2)
  p <- p + ggtitle(paste("Cases of 2019-nCov in China by Province (Except Hubei) as of ", Datestring, sep = ":")) + ylim(0, 2000) + labs(y= "Cases", x = "Provinces") + theme(legend.title = element_blank())
  plot(p)
  ggsave(paste(filename, ".jpeg", sep = ""), plot = p, device = "jpeg")
}

#---------------------------------------------------------------------------------------
#Plot 3: New cases over time for top 5 provinces vs others
#---------------------------------------------------------------------------------------
#From the plots, it is clear that the most infected provinces are:
# Hubei, Guangdong, Zhejiang, Henan, Hunan, Anhui, Jiangxi
# We limit ourselves with only these provinces to get a better visualisation

#Select from new cases dataframe only those from mainland China
df_newcase_China <- df_newcase_geo[df_newcase_geo$Country.Region == "Mainland China",]
names(df_newcase_China)[names(df_newcase_China) == "Province.State"] <- "Province"

#In order to plot it as line graph, we need the table in a form with a datetime column and a data column
#One way is to transpose the whole dataframe

#Store province names - these will be the new column names
Provinces <- df_newcase_China$Province
#transpose data
df2 <- as.data.frame(t(df_newcase_China[,-1]))
colnames(df2) <- Provinces
#Now the dates are names of the row. To access it more easily, turn it into a column in the dataframe
df2$recordDate <- factor(row.names(df2))
#Remove Country, first detected, and Lat and Long information as these are not needed here
df2 <- df2[-c(1:4),]
#Decode record date
df2$recordDate <- lapply(df2$recordDate, decode_date)
#Turn datestring into a datetime column. Note the need of as.character, as right now, recordDate are factors, not characters
df2$datetime <- as.POSIXct(strptime(x = as.character(df2$recordDate), format = "%d-%m-%Y %H:%M"))

#Only select the provinces that we want, and the decoded datetime
df_plot <- df2[,c("datetime","Guangdong","Zhejiang","Henan", "Hunan", "Hubei", "Anhui", "Jiangxi")]

#Similar to recordDate, the transpose function turns everything into a factor, so needs to be converted first to 
#characters, then to numbers in order for it to be plotted
df_plot$Guangdong <- as.numeric(as.character(df_plot$Guangdong))
df_plot$Zhejiang <- as.numeric(as.character(df_plot$Zhejiang))
df_plot$Henan <- as.numeric(as.character(df_plot$Henan))
df_plot$Hunan <- as.numeric(as.character(df_plot$Hunan))
df_plot$Hubei <- as.numeric(as.character(df_plot$Hubei))
df_plot$Anhui <- as.numeric(as.character(df_plot$Anhui))
df_plot$Jiangxi <- as.numeric(as.character(df_plot$Jiangxi))


#Replace 0 with NA. This is because when new case is 0, at this stage it is because the data has not been updated yet
#Putting it to NA allows it to be then filled from previous value
df_plot$Guangdong[df_plot$Guangdong == 0] <- NA
df_plot$Zhejiang[df_plot$Zhejiang == 0] <- NA
df_plot$Henan[df_plot$Henan == 0] <- NA
df_plot$Hunan[df_plot$Hunan == 0] <- NA
df_plot$Hubei[df_plot$Hubei == 0] <- NA
df_plot$Anhui[df_plot$Hubei == 0] <- 
df_plot$Jiangxi[df_plot$Hubei == 0] <- NA
#Fill from previous value
df_plot <- na.locf(df_plot)

#Because of the irregular updates, the actual data for new cases is quite noisy. Here I try to smooth
#it by using a moving average of three columns. 

#Create dataframe to store moving averages. Also, create two dataframes, one for top 5 including Hubei, 
#one for top 5 excluding Hubei
df_plot2 <- df_plot[1:(nrow(df_plot)-2),c("datetime","Guangdong","Zhejiang","Henan", "Hunan", "Anhui")]
df_plot3 <- df_plot[1:(nrow(df_plot)-2),c("datetime", "Guangdong","Zhejiang","Henan", "Hunan", "Hubei")]

df_plot2$Guangdong <- rollmean(df_plot$Guangdong, k = 3)
df_plot2$Zhejiang <- rollmean(df_plot$Zhejiang, k = 3)
df_plot2$Henan <- rollmean(df_plot$Henan, k = 3)
df_plot2$Hunan <- rollmean(df_plot$Hunan, k = 3)
df_plot2$Anhui <- rollmean(df_plot$Anhui, k = 3)


df_plot3$Guangdong <- rollmean(df_plot$Guangdong, k = 3)
df_plot3$Zhejiang <- rollmean(df_plot$Zhejiang, k = 3)
df_plot3$Henan <- rollmean(df_plot$Henan, k = 3)
df_plot3$Hunan <- rollmean(df_plot$Hunan, k = 3)
df_plot3$Hubei <- rollmean(df_plot$Hubei, k = 3)

#Melt to allow it to be plotted with ggplot2
df_reshape <-melt(df_plot2,id.var="datetime")
df_reshape$value <- as.numeric(df_reshape$value)

df_reshape2 <-melt(df_plot3,id.var="datetime")
df_reshape2$value <- as.numeric(df_reshape2$value)


#Create plots - Exclude Hubei
new_case_plot <- ggplot(df_reshape, aes(x=datetime, y=value)) + geom_point(aes(color = variable)) + geom_line(aes(color = variable))
new_case_plot <- new_case_plot + ggtitle("Number of new cases of 2019-nCov reported by Most Infected Provinces (excluding Hubei)")
new_case_plot + scale_x_datetime(breaks = "3 days", minor_breaks = "1 day") + labs(x = "Date", y= "Cases") + theme(legend.title = element_blank())

#Create plots - Include Hubei
new_case_plot2 <- ggplot(df_reshape2, aes(x=datetime, y=value)) + geom_point(aes(color = variable)) + geom_line(aes(color = variable))
new_case_plot2 <- new_case_plot2 + ggtitle("Number of new cases of 2019-nCov reported by Most Infected Provinces")
new_case_plot2 + scale_x_datetime(breaks = "3 days", minor_breaks = "1 day") + labs(x = "Date", y= "Cases") + theme(legend.title = element_blank())


#---------------------------------------------------------------------------------------------
# Plot 4: Death rate for the top 5 province vs national average
#---------------------------------------------------------------------------------------------
# A similar approach as above for the death rate, except that we will only make one plot
# and the national average will also be displayed

#See above for explanations
df_deathrate_China <- df_deathrate_geo[df_deathrate_geo$Country.Region == "Mainland China",]
names(df_deathrate_China)[names(df_deathrate_China) == "Province.State"] <- "Province"
Provinces <- df_deathrate_China$Province
df3 <- as.data.frame(t(df_deathrate_China[,-1]))
colnames(df3) <- Provinces
df3$recordDate <- factor(row.names(df3))
df3 <- df3[-c(1:4),]

df3$recordDate <- lapply(df3$recordDate, decode_date)
df3$datetime <- as.POSIXct(strptime(x = as.character(df3$recordDate), format = "%d-%m-%Y %H:%M"))

for (i in 1:(length(df3)-2)){
  df3[,c(i)] <- as.numeric(as.character(df3[,c(i)]))
}

#Here, we calculate the national average death rate for each day by taking the mean of each row
df3$Average <- rowMeans(df3[,c(1:(length(df3)-2))], na.rm = TRUE)

#Take the columns we want
df_dr <- df3[,c("datetime","Guangdong","Zhejiang","Henan", "Hunan", "Hubei", "Average")]

#Create moving average
df_plot <- df_dr[1:(nrow(df_dr)-2),]


df_plot$Guangdong <- rollmean(df_dr$Guangdong, k = 3)
df_plot$Zhejiang <- rollmean(df_dr$Zhejiang, k = 3)
df_plot$Henan <- rollmean(df_dr$Henan, k = 3)
df_plot$Hunan <- rollmean(df_dr$Hunan, k = 3)
df_plot$Hubei <- rollmean(df_dr$Hubei, k = 3)
df_plot$Average <- rollmean(df_dr$Average, k = 3)

#Melt and plot
df_reshape <-melt(df_plot,id.var="datetime")
df_reshape$value <- as.numeric(df_reshape$value)
new_case_plot <- ggplot(df_reshape, aes(x=datetime, y=value)) + geom_point(aes(color = variable)) + geom_line(aes(color = variable))
new_case_plot <- new_case_plot + ggtitle("Death rate of 2019-nCov reported for the five most infected provinces")
new_case_plot + scale_x_datetime(breaks = "3 days", minor_breaks = "1 day") + labs(x = "Date", y= "Death Rate (%)") + theme(legend.title = element_blank())


#---------------------------------------------------------------------------------------------
# Plot 4: Analysis of Worldwide data
#---------------------------------------------------------------------------------------------

#Similar exercises as above, but with world data

#We will only consider cases by Country, so need to sum up all data for each country by aggregate()
df_death_world <- aggregate(df_deaths_geo[,c(5:(length(df_deaths_geo) - 1))], by=list(Category=df_deaths_geo$Country.Region), FUN=sum)
df_death_world$Case <- "Deaths"
df_recover_world <- aggregate(df_recover_geo[,c(5:(length(df_recover_geo) - 1))], by=list(Category=df_recover_geo$Country.Region), FUN=sum)
df_recover_world$Case <- "Recovered"
df_mild_world <- aggregate(df_mild_geo[,c(5:(length(df_mild_geo) - 1))], by=list(Category=df_mild_geo$Country.Region), FUN=sum)
df_mild_world$Case <- "Admitted"
df_confirm_world <- aggregate(df_confirm_geo[,c(5:(length(df_confirm_geo)))], by=list(Category=df_confirm_geo$Country.Region), FUN=sum)

#Combine the data. Note that since we have only aggregate the data columns, the only things left in the dataframe
#are the countries, and data columns, with each column corresponding to a datetime
df_world <- rbind(df_death_world, df_recover_world, df_mild_world)


for (i in 2:(length(df_world)-1))
{
  df_bar <- df_world[!df_world$Category == "Mainland China",c("Category", "Case", colnames(df_world)[i])]
  names(df_bar)[names(df_bar) == colnames(df_world)[i]] <- "Value"
  df_bar$Value[is.na(df_bar$Value)] <- 0
  
  #seperate out column name into proper datetime string
  
  Datestring <- decode_date(colnames(df_world)[i])
  filename <- paste(colnames(df_world)[i], "world", sep = "_")
  p <- ggplot(df_bar, aes(x = Category, y = Value, fill = Case)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.2))
  p <- p + stat_summary(aes(label = ..y.., group = "Category"), fun.y = sum, geom = "text", angle = 90, hjust = -0.2)
  p <- p + ggtitle(paste("Cases of 2019-nCov Outside Mainland China as of ", Datestring, sep = ":")) + ylim(0, 200) + labs(y = "Cases", x = "Countries") + theme(legend.title = element_blank())
  print(p)
  ggsave(paste(filename, ".jpeg", sep = ""), plot = p, device = "jpeg")
}

#Do some statistics to compare cases inside and outside mainland China
total_number_cases = sum(df_confirm_world[,c(length(df_confirm_world))])
total_outside_China = sum(df_confirm_world[!df_confirm_world$Category == "Mainland China",c(length(df_confirm_world))])
Outside_China_percent = total_outside_China/total_number_cases*100
deaths_outside_china = sum(df_death_world[!df_death_world$Category == "Mainland China",c(length(df_death_world)-1)])
death_rate_world = deaths_outside_china/total_outside_China*100

#Look at overall new cases outside of China. This is because the country seperated data is sparse
#Note that the colSums() create a "dataframe" with 1 row that makes it difficult to process. Therefore, it is converted into a vector

cases_world <- colSums(df_confirm_world[!df_confirm_world$Category == "Mainland China",c(2:length(df_confirm_world))], na.rm = TRUE)
newcases_world <- as.vector(cases_world)

#Calculate new cases
for (i in 2:length(newcases_world))
{
  newcases_world[i] <- cases_world[i] - cases_world[i-1]
}

#Set the first number to 0 - 0 new cases on first day
newcases_world[1] <- 0

#Convert Dates
recordDate <- sapply(colnames(df_confirm_world)[2:length(df_confirm_world)], decode_date)
#Create Dataframe using the two vectors
data <- data.frame(recordDate, newcases_world)
data$datetime <- as.POSIXct(strptime(x = as.character(data$recordDate), format = "%d-%m-%Y %H:%M"))

#Create moving average
data_ma <- data[1:(nrow(data)-2),]
data_ma$newcases_world <- rollmean(data$newcases_world, k = 3)

#Plot
world_plot <- ggplot(data_ma, aes(x=datetime, y=newcases_world)) + geom_point() + geom_line()
world_plot <- world_plot + ggtitle("New cases outside Mainland China by Date")
world_plot + scale_x_datetime(breaks = "3 days", minor_breaks = "1 day") + labs(x = "Date", y= "Cases") + theme(legend.title = element_blank())


