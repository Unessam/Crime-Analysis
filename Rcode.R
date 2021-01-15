options(warn = -1)
library(naniar)
library(anytime) #converting date-time
library(lubridate) #converting date-time
library(plyr) #importing multiple csv files and merging them, manipulating dataframe
library(e1071)
library(dbscan)
library(MASS)
library(corrplot) #correlation plot
library(LiblineaR)
library(ggplot2) #polotting graphs
library(ggmap) #plotting maps
library(dplyr) #data manipulation
library(gridExtra)
library(caret)
library(ggExtra)
library(readr)
library(janitor) #cleaning data
setwd("C:Users/uness/Desktop/project")
getwd()
# reading all .csv files in the working directory, merging row wise and importing
# to the environment
temp = list.files(pattern="*.csv")
dataset<- ldply(temp, read.csv, header=TRUE)
# dataset includes 96 separate dataframe from 2011-01 to 2018-12
paste0("Dataframe dimension :", dim(dataset)) #intial dataset has 2,815,198 rows and 12 columns
head(dataset)
print(names(dataset))
# preparing a dataset with appropriate features,
# removing "Crime.ID", "Reported.by", "Falls.within",
# "Last.outcome.category", and "Context"
drops<- c("Crime.ID", "Reported.by", "Falls.within", #"Last.outcome.category","Context")
dataset<- dataset[, !(names(dataset) %in% drops)]
 names(dataset)
# based on the scale of this assignment I have chosen only specific types of crimes for prediction, hence I'll subset the dataset based on rows containing those specific types of crimes. 7 crimes have been chosen from the total of 16,
 crimes <- c('Anti-social behaviour', 'Violence and sexual offences', 
             'Burglary', 'Criminal damage and arson', 'Shoplifting', 
             'Public order', 'Violent crime')
dataset <- subset(dataset, Crime.type %in% crimes)
dim(dataset) # new dataset has 2,130,169 rows and 7 columns
# Given the Dataset as population set, for the purpose of this analysis, 10% of data
# has been sampled randomly
set.seed(42)
 rows<- sample(nrow(dataset), as.integer((nrow(dataset))*0.1))
sample_df<- dataset[rows, ]
# shuffling the dataset, and spliting the dataset into test(60%) and test(40%)
set.seed(47)
rows<- sample(nrow(sample_df), as.integer((nrow(sample_df))*0.4))
train<- sample_df[-rows, ]
test<- sample_df[rows, ]
# saving generated datasets to working directory
write.csv(sample_df, "new datasets/sample_df.csv", row.names = T)
write.csv(train, "new datasets/train.csv", row.names = T)
write.csv(test, "new datasets/test.csv", row.names = T)
write.csv(dataset, "new datasets/dataset.csv", row.names = T)
# random sampling 30% of the train dataset to reduce computational load for visualizations
df<- train[sample(nrow(train), as.integer((nrow(train))*0.3)),]
rm(train, test, dataset, sample_df)
# saving generated sample to working directory
write.csv(df, "new datasets/df.csv", row.names = T)
rm(df)
df<- read.csv("new datasets/df.csv", 
              header = T, stringsAsFactors = F, na.strings = c("", "NA", "#N/A"))
_____________________________________________________________________________
_____________________________________________________________________________

## Defining functions for analysis

_____________________________________________________________________________
_____________________________________________________________________________
# functions to make variables from date info.
feat_crt<- function(dataframe){
  col_names<- names(dataframe)
  dataframe <- data.frame(matrix(unlist(dataframe),nrow=nrow(dataframe)),stringsAsFactors=FALSE)
  names(dataframe) <- col_names
  names(dataframe)[names(dataframe)== "X"]<- "Row_Number"
  names(dataframe)[names(dataframe)== "Month"]<- "Date"
  dataframe$Years <- year(anytime(dataframe$Date))
  dataframe$Months <- month(anytime(dataframe$Date))
  dataframe$half <- mapvalues(dataframe$Months, from= c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), to=c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2))
  dataframe$season <- mapvalues(dataframe$Months, from= c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), to=c('winter', 'winter', 'spring', 'spring', 'spring', 'summer', 'summer', 'summer', 'autumn', 'autumn', 'autumn', 'winter'))
  dataframe<-mutate(dataframe, Date_New=anydate(Date)) 
  dataframe$District<- gsub("On or near ", "", as.character(dataframe$LSOA.name))
  dataframe$District<- lapply(strsplit(dataframe$District," ",fixed=TRUE), "[", 1)
  dataframe$Location<- gsub("On or near ", "", as.character(dataframe$Location))
  row_dataframe= as.data.frame(dataframe$Row_Number)
  col_drops<- c("Row_Number", "LSOA.name", "Date")
  dataframe<- dataframe[, !(names(dataframe) %in% col_drops)]
  dataframe<-mutate(dataframe, District = sapply(District, toString))
  dataframe$Longitude<- as.numeric(dataframe$Longitude)
  dataframe$Latitude<- as.numeric(dataframe$Latitude)
  dataframe<- dataframe[!(dataframe$District=='NA'),]
  return(dataframe)
}
# functions to draw map of crime.
map_crime <- function(dataframe, crime) {
  filtered <- filter(dataframe, Crime.type %in% crime)
  plot <- ggmap(map, extent='device') + 
    geom_point(data=filtered, 
               aes(x=Longitude, y=Latitude, color=Category), alpha=0.1)
  return(plot)
}
# functions to make contour maps
map_contours <- function(data, alp) {
  p1 = ggmap(map, extent='device') + 
    geom_point(data=data, aes(x=Longitude, y=Latitude), alpha= alp) + 
    stat_density2d(aes(x = Longitude, y = Latitude,
                       fill = ..level.., alpha = ..level..),
                   size = 0.1, data = data, n=100,
                   geom = "polygon") +
    theme(legend.position="none")
  return(p1)
}
plot_marginals <- function(data) {
  p2 = ggplot(data=data, aes(x=Longitude, y=Latitude), alpha=0.1)+
    geom_point()
  p2 = ggMarginal(p2 , type = "histogram")
  return(p2)
}
_____________________________________________________________________________
_____________________________________________________________________________

## Inspecting Dataframe and its variables

_____________________________________________________________________________
_____________________________________________________________________________
dim(df) # this dataframe includes 38343 rows and 8 columns
names(df)
head(df)
str(df)
table(as.character(sapply(df, class))) # consisting 5 character, 1 integer, and 2 numeric features
summary(df)
vis_miss(df) # less than 0.1% of values in LSOA.code and LSOA.name are missing
sum(is.na(df)) #there is 10 missing value in the dataframe
sapply(df, function(x)length(unique(x))) #inspecting number of unique values
# Observation: Crime ID due to having unique value for each row is not a valuable feature,
# Month data type should be converted to date format using anytime package, its name also should be changed to Date, then it's possible to extract and create  more features of it such as Month, Year, season and half-year,
# Longitude and Latitude remain intact since they are coordination properties (spatial columns),they will be treated as one variable due to the nature of coordination through the analysis.
# It's possible to extract more meaningful data from Location and LSOA.name features to inform us of the more precise location and district of the crimes, 
# LOSA.code remains intact since it can provides meaninful statistical information during the analysis, I'll also try to visualize the data with the help of this feature
# Crime.type remains intact as the target variable of this analysis. 
_____________________________________________________________________________
_____________________________________________________________________________

## Creating new features, changing data types, dropping junk columns, and renaming number of variables variables 

_____________________________________________________________________________
_____________________________________________________________________________

df<- feat_crt(df)
table(as.character(sapply(df, class)))
head(df)

_____________________________________________________________________________
_____________________________________________________________________________

## Downloading required maps for the analysis

_____________________________________________________________________________
_____________________________________________________________________________
# requesting maps through Google cloud platform
pass = xxxxxxxxxxxxx
ggmap::register_google(key = pass)
#set.seed(20)
#map<-get_map("greater manchester, uk", zoom = 10)
#ggmap(map)
## defining coordination of Geater Manchester for the map
paste0("Longitude: max ", max(df$Longitude), " & min ", min(df$Longitude)) 
paste0("Longitude: max ", max(df$Latitude), " & min ", min(df$Latitude)) 
loc<- c(left=-2.780, right=-1.960, top=53.67, bottom=53.33)
map<-get_map(loc, color="bw")
ggmap(map)
_____________________________________________________________________________
_____________________________________________________________________________

## Univariate Analysis

_____________________________________________________________________________
_____________________________________________________________________________
### Variable:(Longitude, Latitude)
### Data type: Numeric, Continues
# Overal Distribution of crime across the Greater Manchester
dist_map <- ggmap(map, extent='device') + 
  geom_point(data=df, aes(x=Longitude, y=Latitude),
             alpha = 1/40,color = "red")+
  scale_colour_brewer(type="qual")+
  ggtitle('Overal Distribution of Crime')
ggsave(dist_map, dpi=300, filename = "distplot.png", path='C:/Users/uness/Desktop/project/plots', device= 'png')
# Observation: the distribution of crime for the sample dataset shows that the crime occurrence is distributed across the province, however, higher number of crimes in populated cities and towns are obvious
mg_plot<- plot_marginals(df)
ggsave(dpi=300, filename = "mgplot.png", path='C:/Users/uness/Desktop/project/plots', device= 'png', limitsize = T)
# Observation: distribution of crimes is concentrated in center due to the presence of metropolitan city of Manchester, 
# distribution along y-axis (Latitude) is approximately normal, and the distribution along the x-axis is sightly skewed to the left due to the geometry properties of the province and existence of major cities and town to the right side of it. 
ct_plot <- map_contours(df,.03)+
  ggtitle('Contour plot of Crime')
ggsave(dpi=300, filename = "ct_plot.png", path='C:/Users/uness/Desktop/project/plots', device= 'png')
# Observation: detailed contour map indicates the concentration of crimes in metropolitan city of Manchester followed by Bolton and Wigan
#_____________________________________________________________________________
### Variable: (District)
### Data type: Character
#df<- df[complete.cases(df),] 
df<- df[!(df$District=='NA'),] #removing missing values
cnt_gp_dst = df %>%
  group_by(District) %>%
  dplyr::summarize(n = dplyr::n())
cnt_gp_dst$District<-as.character(cnt_gp_dst$District)
cnt_gp_dst<- as.data.frame(cnt_gp_dst) %>% 
  transform(District= reorder(District,n))
district_count<- ggplot(cnt_gp_dst) +
  geom_bar(aes(x=District, y=n, 
               color = District, fill = District),
           stat="identity")+
  theme(legend.position="None")+
  ggtitle('Number of Crimes in each District')+
  coord_flip()
ggsave(dpi=300, filename = "district_count.png", path='C:/Users/uness/Desktop/project/plots', device= 'png')
per_gp_dst <- df %>% 
  group_by(District) %>% 
  summarise(count = n()) %>% 
  mutate(perc = count/sum(count)) 
per_gp_dst$District <- as.character(per_gp_dst$District)
per_gp_dst<- as.data.frame(per_gp_dst) %>% 
  transform(District= reorder(District,perc))
district_norm_count<- ggplot(per_gp_dst) +
  geom_bar(aes(x=District, y=perc, 
               color = District, fill = District),
           stat="identity")+
  theme(legend.position="None")+
  ggtitle('Normal count of Crimes in each District')+
  coord_flip()
ggsave(dpi=300, filename = "district_norm_count.png", path='C:/Users/uness/Desktop/project/plots', device= 'png')
# Observation: City of Manchester Accounts for roughly 25% of crimes in the province followed by Bolton (~10%) and Wigan (10%), These top three districts account for nearly half of the crime occurrence across the Great Manchester area, also number of records for some districts such as Blackburn, and Warrington are very few, hence they can not influence the analysis and removing them from the analysis might be good approach,
top_districts <- per_gp_dst[per_gp_dst$count>10, ]
top_dst_name <- as.character(unique(top_districts$District))
df <- subset(df, District %in% top_dst_name)
unique(df$District)
#_____________________________________________________________________________

### Variable: (Crime.type)
### Data type: Character
per_gp_crm <- df %>% 
  group_by(Crime.type) %>% 
  summarise(count = n()) %>% 
  mutate(perc = count/sum(count)) 
per_gp_crm$Crime.type <- as.character(per_gp_crm$Crime.type)
per_gp_crm<- as.data.frame(per_gp_crm) %>% 
  transform(Crime.type= reorder(Crime.type, perc))
crime_count<- ggplot(per_gp_crm) +
  geom_bar(aes(x=Crime.type, y=perc, 
               color = Crime.type, fill = Crime.type),
           stat="identity")+
  ggtitle('Number of occurrence for each Type of Crime')+
  theme(legend.position="None")+
  coord_flip()
ggsave(dpi=300, filename = "crime_count.png", path='C:/Users/uness/Desktop/project/plots', device= 'png')
# Observation: The highest rate of crime belongs to Anti-social behavior (~43%) followed by Violence and sexual offences at ~ 19% , and criminal damage and arson at ~ 13%.Also Violent crime (~4%), Shoplifting (~6%), and Public Order (~7%) are located at the bottom of the chart. Top 3 Crimes account for more than 75% of the crimes in this area. 
#_____________________________________________________________________________
### Variable: (Location)
### Data type: Character
per_gp_loc <- df %>% 
  group_by(Location) %>% 
  summarise(count = n()) %>% 
  mutate(perc = count/sum(count)) 
per_gp_loc$Location <- as.character(per_gp_loc$Location)
per_gp_loc<- as.data.frame(per_gp_loc) %>% 
  transform(Location= reorder(Location,perc))

per_gp_loc[(per_gp_loc$count)>100,]
location_count<- ggplot(per_gp_loc[(per_gp_loc$count)>100,]) +
  geom_bar(aes(x=Location, y=perc, 
               color = Location, fill = Location),
           stat="identity")+
  ggtitle('Number of Crimes in each Location')+
  theme(legend.position="None")+
  coord_flip()
ggsave(dpi=300, filename = "location_count.png", path='C:/Users/uness/Desktop/project/plots', device= 'png')
paste0("Top 8 Locations account for roughly ",
       round((sum(per_gp_loc[(per_gp_loc$count)>100, "count"])
              /sum(per_gp_loc$count))*100, 2),"% of whole crimes" )
# Observation: Parking areas, supermarkets, and shopping areas are among the most popular places for crime commitment, petrol stations, sport/recreation area, and nightclubs areas follows the top 3 popular places respectively. In total, 8 top locations account for approximately ~10% of whole crimes across the area. 
#_____________________________________________________________________________

### Variable: (Years)
### Data type:Numeric, Discrete (interval) 
cnt_gp_yr = df %>%
  group_by(Years) %>%
  dplyr::summarize(n = dplyr::n())
cnt_gp_yr$Years<-as.character(cnt_gp_yr$Years)
#cnt_gp_yr<- as.data.frame(cnt_gp_yr) %>% 
  #transform(Years= reorder(Years,n))
year_count<- ggplot(cnt_gp_yr) +
  geom_bar(aes(x=Years, y=n, 
               color = Years, fill = Years),
           stat="identity")+
  ggtitle('Normal count of Crime in each Year')+
  theme(legend.position="None")
ggsave(dpi=300, filename = "year_count.png", path='C:/Users/uness/Desktop/project/plots', device= 'png')
year_line<- ggplot(cnt_gp_yr) +
  geom_line(aes(x=Years, y=n, group=1), color='red3')+
  ggtitle('Time series of Crime over 8 Years')
ggsave(dpi=300, filename = "year_line.png", path='C:/Users/uness/Desktop/project/plots', device= 'png')
# Observation: the line graph shows number of crime occurrence across the time from Jan 2011 to Dec 2018, it indicates a  insignificant decrease in numbers from 2011 to 2012, however the figures increase steeply for five years from 2012 to 2017, where it demonstrates a surge from 4050 in numbers to approximately 5750 in 2017 which is the highest point in this 8-year period, then there is a fall in numbers afterwards from 2017 to 2018.  
#_____________________________________________________________________________

### Variable: (Months)
### Data type:Numeric, Discrete (interval) 
per_gp_mth <- df %>% 
  group_by(Months) %>% 
  summarise(count = n()) %>% 
  mutate(perc = count/sum(count)) 
per_gp_mth$Months <- as.character(per_gp_mth$Months)
#per_gp_mth<- as.data.frame(per_gp_mth) %>% 
  #transform(Months= reorder(Months,perc))
per_gp_mth$Months <- factor(per_gp_mth$Months, levels=c(seq(12)))
month_norm_count<- ggplot(per_gp_mth) +
  geom_bar(aes(x=Months, y=perc, 
               color = Months, fill = Months),
           stat="identity")+
  ggtitle('Normal count of Crime in each month')+
  theme(legend.position="None")
ggsave(dpi=300, filename = "month_norm_count.png", path='C:/Users/uness/Desktop/project/plots', device= 'png')
#ggplot(per_gp_mth) +
  #geom_line(aes(x=Months, y=count, group=1), color='red3')  
# Observation: February accounts for the least number of crimes in duration of 8 years from Jan 2011 to Dec 2018, followed by December and January. The highest number of crimes belongs to July, october and August respectively. 
#_____________________________________________________________________________
### Variable: (Season)
### Data type:Numeric, Discrete (interval)
per_gp_seas <- df %>% 
  group_by(season) %>% 
  summarise(count = n()) %>% 
  mutate(perc = count/sum(count)) 
per_gp_seas$season <- as.character(per_gp_seas$season)
#per_gp_seas$season <- factor(per_gp_seas$season, levels=c(seq(4)))
seas_norm_count<- ggplot(per_gp_seas) +
  geom_bar(aes(x=season, y=perc, 
               color = season, fill = season),
           stat="identity")+
  ggtitle('Normal count of Crime for each Season')+
  theme(legend.position="None")
ggsave(dpi=300, filename = "seas_norm_count.png", path='C:/Users/uness/Desktop/project/plots', device= 'png')
per_gp_seas
# Observation: Summer accounts for the highest number of crimes while numbers are the least for Winter
```

#_____________________________________________________________________________
### Variable: (half)
### Data type:Numeric, Discrete (interval)
per_gp_half <- df %>% 
  group_by(half) %>% 
  summarise(count = n()) %>% 
  mutate(perc = count/sum(count)) 
per_gp_half$half <- as.character(per_gp_half$half)
#per_gp_seas$season <- factor(per_gp_seas$season, levels=c(seq(4)))
half_norm_count<- ggplot(per_gp_half) +
  geom_bar(aes(x=half, y=perc, 
               color = half, fill = half),
           stat="identity")+
  ggtitle('Normal count of Crime for first & Sencond half of the Year')+
  theme(legend.position="None")
ggsave(dpi=300, filename = "half_norm_count.png", path='C:/Users/uness/Desktop/project/plots', device= 'png')
# Observation: Number of crimes are higher in second half of the year for the duration of 8 years.
#_____________________________________________________________________________
### Variable: (Date)
### Data type: Numeric, date-time
per_gp_dt <- df %>% 
  group_by(Date_New) %>% 
  summarise(count = n()) %>% 
  mutate(perc = count/sum(count))
per_gp_dt$Date_New <- as.character(per_gp_dt$Date_New)
per_gp_dt<- as.data.frame(per_gp_dt) 
date_line<- ggplot(per_gp_dt) +
  geom_line(aes(y=count, x=Date_New, group=1), 
               color = 'red3',stat="identity")+
  ggtitle('Timeseries of Crime of the period of eight years')
  theme(legend.position="None", axis.text.x = element_text(angle = 90, size = 5))
ggsave(dpi=300, filename = "date_line_graph.png", path='C:/Users/uness/Desktop/project/plots', device= 'png')
norm_gp_dt <- df %>% 
  group_by(Date_New) %>% 
  summarise(count = n()) %>%
  mutate(norm_count = (count-mean(count))/sd(count))

date_norm_line<- ggplot(norm_gp_dt) +
  geom_line(aes(y=norm_count, x=Date_New, group=1), 
               color = 'red3',stat="identity")+
  ggtitle('Timeseries of Crime of the period of eight years- Normal')
  theme(legend.position="None", axis.text.x = element_text(angle = 90, size = 5))
ggsave(dpi=300, filename = "date_norm_line_graph.png", path='C:/Users/uness/Desktop/project/plots', device= 'png')
# Observation: Overall incresing trend for this period is discernible, however no clear seasonality pattern is present in this timeseries.
_____________________________________________________________________________
_____________________________________________________________________________
## Bivariate Analysis
_____________________________________________________________________________
_____________________________________________________________________________
#_____________________________________________________________________________
### Variables: Years vs Months
yrs_mths <- df %>%
  group_by(Years, Months) %>%
  summarize(count= n())
yrs_mths_box<- ggplot(data= yrs_mths, aes(x= Years, y=count, color=Years, group= Years))+
  geom_boxplot()+
  ylab("Crimes Count")+
  ggtitle('Years vs Months')+
  theme(legend.position="None")
ggsave(dpi=300, filename = "yrs_mnths_box.png", path='C:/Users/uness/Desktop/project/plots', device= 'png')
# Observation: This graph indicates the distribution of crime within each year, the average numbers decreases from 2011 to 2012, however, it soars since 2012, riches the maximum point during this period in 2017, and then there is a slight fall from that date to 2018. The distribution of crimes across each year does not vary significantly.
mths_yrs <- df %>%
  group_by(Months, Years) %>%
  summarize(count= n())
mths_yrs_box<- ggplot(data= mths_yrs, aes(x= Months, y=count, color=Months, group= Months))+
  geom_boxplot()+
  ylab("Crimes Count")+
  ggtitle('Months vs Years')+
  theme(legend.position="None")
ggsave(dpi=300, filename = "mnths_yrs_box.png", path='C:/Users/uness/Desktop/project/plots', device= 'png')
# Observations: this graph indicates the distribution of crime for each month through the period of 8 years, February posses the least average value and June shows either uniform or normal distribution over 8 years, and the highest median value belongs to July. 

tab <- as.data.frame.matrix(table(df$Months, df$Years))
summary(tab)
sapply(tab, sd)
#_____________________________________________________________________________
### Variables: Years vs Crime.type
yrs_crm_bar<- ggplot(df, aes(Years, fill= Crime.type))+
  geom_bar(position = "fill", width= 0.5)+
  ggtitle('Years vs Crimes')
ggsave(dpi=300, filename = "yrs_crm_bar.png", path='C:/Users/uness/Desktop/project/plots', device= 'png')
# Observation: A significant decrease in Anti-social behavior over a period of 8-year is distinguishable, also violent crimes decreases from 2011 and there is no record of that after 2014, on the contrary, violence and sexual offences increases noticeablly since 2013 and riches 4 times greater in numbers in 2018, Public order is the other type with a significant increase since 2013,  
norm_yr_crm<- df %>%
  group_by(Years, Crime.type) %>%
  summarise(count = n()) %>%
  mutate(norm_count= (count - mean(count))/sd(count))
yrs_crm_line<- ggplot(norm_yr_crm, aes(x= as.numeric(Years), y=count, color= Crime.type))+
  geom_point()+
  geom_line()+
  ggtitle('Timeseries of total Crimes across Years')
ggsave(dpi=300, filename = "yrs_crm_line.png", path='C:/Users/uness/Desktop/project/plots', device= 'png')
yrs_crm_norm_line<- ggplot(norm_yr_crm, aes(x= as.numeric(Years), y=norm_count, color= Crime.type))+
  geom_point()+
  geom_line()+
  ggtitle('Normalized Timeseries of total Crimes across Years')
ggsave(dpi=300, filename = "yrs_crm_nrm_line.png", path='C:/Users/uness/Desktop/project/plots', device= 'png')
# Observations: Anti-social behavior demonstrates an increase in numbers before decreasing in 2013 for 5 consequitive years, violence and sexual offence surging to the top and surpasses anti-social behavior in mid 2016, while public order shows a gradual increase comparatively. Shoplifting and Criminal damage and arson show gradual inceases over this period, while Violent crime decreases significantly from 2011-2013, The rate of crime for burglary does not vary through this period significantly.
# there is no record of data available for violent crime after 2013, also there is no record available for violence and sexual offences as well as public order prior 2013, that might be due to the changes in definition of crimes during this period.
#_____________________________________________________________________________
### Variables: Months vs Crime.type
mths_crm_bar<- ggplot(df, aes(Months, fill= Crime.type))+
  geom_bar(position = "fill", width= 0.5)+
  ggtitle('Months vs Crimes')
ggsave(dpi=300, filename = "mths_crm_bar.png", path='C:/Users/uness/Desktop/project/plots', device= 'png')
# Observation: the proportion of crime for each month through the period of 8 years does not vary significantly,
norm_mnth_crm<- df %>%
  group_by(Years, Months, Crime.type) %>%
  summarise(count = n()) %>%
  mutate(norm_count= (count - mean(count))/sd(count))
yrs_crm_line_sep<- ggplot()+
  geom_line(norm_mnth_crm, mapping= aes(x= as.numeric(Months), y= count, 
                                      color=Crime.type))+
  facet_wrap(Years~.)+
  ggtitle('Crimes for each Year')+
  theme(legend.key.size = unit(0.2, "cm"), legend.position="top")
ggsave(dpi=300, filename = "yrs_crm_line_sep.png", path='C:/Users/uness/Desktop/project/plots', device= 'png')
# Observation: this graph does not convey any meaningful insight
#_____________________________________________________________________________
### Variables: Date vs Crime.type
norm_dt_crm<- df %>%
  group_by(Date_New, Crime.type)%>%
  summarise(count= n())%>%
  mutate(norm_count= (count - mean(count))/sd(count))
dt_crm_line<- ggplot()+
  geom_line(norm_dt_crm, mapping= aes(x= Date_New, y= count, 
                                      color=Crime.type))+
  ggtitle('Crimes over eight years')+
  theme(legend.key.size = unit(0.2, "cm"), legend.position="top")
ggsave(dpi=300, filename = "dt_crm_line.png", path='C:/Users/uness/Desktop/project/plots', device= 'png')
# Observation: since there is not clear seasonality pattern in data this graph does not bring added value to us, as timeseries of Crime.type over 8 years already demonstrated the available trend in data. 
#_____________________________________________________________________________
### Variables: Season vs Crime.type
seas_crm_bar<- ggplot(df, aes(season, fill= Crime.type))+
  geom_bar(position = "fill", width= 0.5)+
  ggtitle('Seasons vs Crimes')
ggsave(dpi=300, filename = "seas_crm_bar.png", path='C:/Users/uness/Desktop/project/plots', device= 'png')
# Observation: the demonstration of type of crimes in different seasons does not indicate any significant variation,
#_____________________________________________________________________________
### Variables: half vs Crime.type
half_crm_bar<- ggplot(df, aes(half, fill= Crime.type))+
  geom_bar(position = "fill", width= 0.5)+
  ggtitle('1st and 2nd Half of the Year vs Crimes')
ggsave(dpi=300, filename = "half_crm_bar.png", path='C:/Users/uness/Desktop/project/plots', device= 'png')
# Observation: Type of Crime does not vary across first and second half of the year.
#_____________________________________________________________________________
### Variables: Years vs District
yrs_dis_bar<- ggplot(df, aes(Years, fill= District))+
  geom_bar(position = "fill", width= 0.5)+
  ggtitle('Years vs District')
ggsave(dpi=300, filename = "yrs_dis_bar.png", path='C:/Users/uness/Desktop/project/plots', device= 'png')
norm_yr_crm<- df %>%
  group_by(Years, District) %>%
  summarise(count = n()) %>%
  mutate(norm_count= (count - mean(count))/sd(count))
yrs_dis_line <- ggplot(norm_yr_crm, aes(x= as.numeric(Years), y=count, color= District))+
  geom_point()+
  geom_line()+
  theme(legend.position = 'top')+
  ggtitle('Crime trend in various Districts across 8 Years')
ggsave(dpi=300, filename = "yrs_dis_line.png", path='C:/Users/uness/Desktop/project/plots', device= 'png')
# Observation: proportion of Crime does not vary significantly across various districts during 8 years,
# Observation: Overall, most of the districts follows an increasing trend in number of crimes until 2017, and the there is a sight depression from 2017 to 2018.
#_____________________________________________________________________________
### Variables: Months vs District
mths_dis_bar <- ggplot(df, aes(Months, fill= District))+
  geom_bar(position = "fill", width= 0.5)+
  ggtitle('Months vs District')
ggsave(dpi=300, filename = "mths_dis_bar.png", path='C:/Users/uness/Desktop/project/plots', device= 'png')
# Observation: the propotion of crime does not vary significantly across various districts in each month, 
#_____________________________________________________________________________
### Variables: District vs Crime
dis_crm_bar <- ggplot(df, aes(District, fill= Crime.type))+
  geom_bar(position = "fill", width= 0.5)+
  theme(axis.text.x = element_text(angle = 90), legend.key.size = unit(0.5, "cm"))+
  ggtitle('District vs Crime')
ggsave(dpi=300, filename = "dis_crm_bar.png", path='C:/Users/uness/Desktop/project/plots', device= 'png')
# Observation: The proportion of crimes is more or less similar in each district, however, Anti-Social behaviour occurs more in Stockport and Tameside compare to other districts,
#_____________________________________________________________________________
### Variables: District vs Location
top_8_loc<- c(as.character(per_gp_loc[(per_gp_loc$count)>100, "Location"]))
dis_loc_bar <- ggplot(subset(df, Location %in% top_8_loc), aes(District, fill= Location ))+
  geom_bar(position = "fill", width= 0.5)+
  theme(axis.text.x = element_text(angle = 90), legend.key.size = unit(0.5, "cm"))+
  ggtitle('District vs Location')
ggsave(dpi=300, filename = "District vs Location.png", path='C:/Users/uness/Desktop/project/plots', device= 'png')
# Observations: location of crimes varies for each district, for instance crimes in Supermarkets account for a significant proportion of total crimes in Tameside, while a significant proportion of crime is happening in parking area in Rochdale and the highest quota belongs to Nightclubs, shopping area, parking area  in Manchester.
#_____________________________________________________________________________
### Variables: Location vs Crime.type
loc_crm_bar <- ggplot(subset(df, Location %in% top_8_loc), aes(Location, fill= Crime.type ))+
  geom_bar(position = "fill", width= 0.5)+
  theme(axis.text.x = element_text(angle = 45, size = 7, hjust = 0.8), legend.key.size = unit(0.4, "cm"))+
  scale_colour_brewer(palette = "Set2")+
  ggtitle('Location vs Crime')
ggsave(dpi=300, filename = "loc_crm_bar.png", path='C:/Users/uness/Desktop/project/plots', device= 'png')
# Observation: Type of crimes vary based on properties of each location. For instance, Anti-social behavior accounts for the highest proportion of crimes in pedestrian Subways and Nightclubs, while shoplifting as expected holds the highest quota in supermarkets and shopping area
#_____________________________________________________________________________
### Variables: Spread vs Crime.type
type_of_crimes <- as.character(unique(df$Crime.type))
length(type_of_crimes)
lst = list()
i = 1
for (crime in type_of_crimes) {
       lst[[crime]] = map_contours(subset(df,
                                             Crime.type == crime),
                                         alp=.01) + 
               ggtitle(crime)
        i = i+1
} 
coord_crm_scat <- grid.arrange(lst[[1]],lst[[2]],lst[[3]],
        lst[[4]],lst[[5]],lst[[6]],
        lst[[7]],ncol=2)
ggsave(plot= coord_crm_scat, dpi=300, filename = "Distribution of various Crimes across GB.png", path='C:/Users/uness/Desktop/project/plots', device= 'png')
# Observation: While the city of Manchester almost accounts for the highest concentration of crimes across the area , obviously pattern of distribution for some type of crimes vary significantly on GB map.
#_____________________________________________________________________________
### Variables: Spread vs District
# creating specific coordination for top district
maps= list()
i=1
for (district in top_dst_name){
  right <- as.numeric(max(df[df$District==district, "Longitude"])+0.0005)
  left <- as.numeric(min(df[df$District==district, "Longitude"])-0.0005)
  top <- as.numeric(max(df[df$District==district, "Latitude"])+0.0005)
  bottom <- as.numeric(min(df[df$District==district, "Latitude"])-0.0005)
  loc<- c(left=left, right=right, top=top, bottom=bottom)
  maps[[district]] = get_map(loc, color="bw") 
}
dst_lst= list()
i=1
for (district in top_dst_name) {
  dst_lst[[district]] = ggmap(maps[[district]], extent='device') + 
  geom_point(subset(df, District == district ), mapping = aes(x=Longitude, y=Latitude),alpha = 1/20,color = "red", size=0.5)+
  scale_colour_brewer(type="qual")+
    ggtitle(district)+ 
         theme(axis.ticks= element_blank(), 
               axis.text = element_blank(),
               axis.title = element_blank(), 
               plot.title = element_text(size = 5, face = "bold"))
        i = i+1
} 
dst_crm_dis <- grid.arrange(dst_lst[[1]],dst_lst[[2]],
        dst_lst[[4]],dst_lst[[5]],dst_lst[[6]],dst_lst[[3]],
        dst_lst[[7]], dst_lst[[8]], dst_lst[[9]], dst_lst[[10]],ncol=4)

ggsave(plot= dst_crm_dis, dpi=300, filename = "Distribution of Crime in each District.png", path='C:/Users/uness/Desktop/project/plots', device= 'png')
# Observation: It is Obvious that number of crimes is positively correlated to the population size of that sector. rate of incident is significantly higher in downtown of cities and towns where more businesses are located and reduces as it moves toward rural places and low populated areas.
#_____________________________________________________________________________
### Variables: Spread vs Years
Eight_Years <-c(as.character(sort(unique(df$Years))))
yr_lst= list()
i = 1
for (year in Eight_Years) {
       yr_lst[[year]] = map_contours(subset(df,
                                             Years == year),
                                         alp=.01) + 
               ggtitle(year)
        i = i+1
} 
coord_yrs_dis <- grid.arrange(grobs= yr_lst ,ncol=3)
ggsave(plot= coord_yrs_dis, dpi=300, filename = "Distribution of Crime over 8 Years.png", path='C:/Users/uness/Desktop/project/plots', device= 'png')
# Observation: By investigating contour map of crime distribution through eight years, a shrinking trend in distribution of crime across the area is perceptible, where the crime occurrence had distributed over the greater Manchester area in 2011 and gradually has been diminishing and  eventually restricted to cities and towns within the area in 2018.
#_____________________________________________________________________________
### Variables: Spread vs Months
Twelve_Months <-c(as.character(sort(unique(df$Months))))
mth_lst= list()
i = 1
for (month in Twelve_Months) {
       mth_lst[[month]] = map_contours(subset(df,
                                             Months == month),
                                         alp=.01) + ggtitle(month)
        i = i+1
} 
coord_mths_dis <- grid.arrange(grobs= mth_lst ,ncol=3)
ggsave(plot= coord_mths_dis, dpi=300, filename = "Distribution over 12 Months.png", path='C:/Users/uness/Desktop/project/plots', device= 'png')
# Observation: While the distribution and accumulation of crimes remains almost steady for almost 9 months, it outspreads in January, and February shrinks in March , expands again in April and contacts afterwards. _____________________________________________________________________________
_____________________________________________________________________________
## Multivariate Analysis
_____________________________________________________________________________
_____________________________________________________________________________
#_____________________________________________________________________________
### Variables: Spread vs amount of Crime.type
pivot<- df %>%
  select(District, Crime.type, Longitude, Latitude) %>%
  group_by(District, Crime.type) %>%
  summarise(mean_lon= mean(Longitude), mean_lat= mean(Latitude), count= n())
bubble_lst= list()
i=1
crimes <- as.character(unique(df$Crime.type))
for (crime in crimes ){
  bubble_lst[[crime]] = ggmap(map)+
  geom_point(subset(pivot, Crime.type== crime), mapping = aes(x=mean_lon, y=mean_lat, color= "red3", size= count), alpha= 0.6)+
    ggtitle(crime)+
  theme(legend.position = "none",
        axis.ticks= element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(), 
        plot.title = element_text(size = 5, face = "bold"))+
  scale_size_continuous(range = c(1, 5))
  i=i+1
}
bubble_plot <- grid.arrange(grobs= bubble_lst, ncol= 2)
ggsave(plot= bubble_plot, dpi=300, filename = "bubble_plot.png", path='C:/Users/uness/Desktop/project/plots', device= 'png')
#Observation: it is obvious that the city of Manchester has the highest number crimes across the area followed by Bolton and Wigan
#_____________________________________________________________________________
### Variables: spread vs Months vs Crime.type
top_3_crimes <- c("Anti-social behaviour", "Violence and sexual offences", "Burglary")
month_lst<- list()
i=1
for (crime in top_3_crimes) {  
  new_df = subset(df, Crime.type== crime)  
  for (mnth in Twelve_Months){
    new_df2<- subset(new_df, Months== mnth)
    month_lst[[i]] = ggmap(map, extent = "device")+
    geom_point(new_df2, mapping= aes(x= Longitude, y= Latitude), alpha= 0.3, size=0.25) + 
    stat_density2d(aes(x = Longitude, y = Latitude,
                       fill = ..level.., alpha = ..level..),
                   size = 0.1, data = new_df2, n=300,
                   geom = "polygon") +
    theme(legend.position = "none",
        axis.ticks= element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(), 
        plot.title = element_text(size=4, face= 'bold'))+
    ggtitle(paste0(mnth,crime))
    i=i+1  
  }
}
months_plot <- grid.arrange(grobs= month_lst, ncol= 6)
ggsave(plot= months_plot , dpi=300, filename = "crime_months_plot.png", path='C:/Users/uness/Desktop/project/plots', device= 'png')
# Observation: Distribution of top 3 crimes over the months neither indicates a clear pattern nor any meaningful relationship between these variables.  
_____________________________________________________________________________
_____________________________________________________________________________
## LSOA

_____________________________________________________________________________
_____________________________________________________________________________
require(rgdal) # importing shapefile
bound <- readOGR(dsn=path.expand("C:/Users/uness/Desktop/project/boundaries/LSOA_2011_EW_BFC_shp/LSOA_2011_EW_BFC.shp"), layer = 'LSOA_2011_EW_BFC',  verbose = F)
summary(bound)
#change column name of LSOA code to match the LSOA code column in the LSOA polygon data
colnames(df)[which(names(df) == "LSOA.code")] <- "LSOA11CD"
#left outer join LSOA polys and df 
bound@data <- merge(x= df, y= bound, by = "LSOA11CD", all.x = TRUE)
class(bound)
summary(bound@data)
writeOGR(bound, 'C:/Users/uness/Desktop/project/boundaries/LSOA_2011_EW_BFC_shp/Bound.shp', driver="ESRI Shapefile", layer = 'Bound.shp', overwrite_layer=T)
_____________________________________________________________________________
_____________________________________________________________________________
## Clustering
_____________________________________________________________________________
_____________________________________________________________________________
require(clustMixType) # K-prototypes for clustering
require(factoextra) # computing clustering tendency based on Hopkins statistic
num_df <- (df[,c(1, 2)])
norm_minmax <- function(x){
                           (x- min(x)) /(max(x)-min(x))
}
num_df<- as.data.frame(lapply(num_df[, c(1, 2)], norm_minmax))
set.seed(42)
rows<- sample(nrow(num_df), as.integer((nrow(num_df))*0.1))
num_df<- num_df[rows, ]
dim(num_df)
clust <- get_clust_tendency(num_df, n=nrow(num_df)-1, graph= F)
clust$hopkins_stat # H-value: 0.89 demonstrates a high clusterable tendency for numerical data
# creating a new daraframe with "Longitude", "Latitude", "Location", "Years", "District" variables to cluster data
var <- c("Longitude", "Latitude", "Location", "Years", "District")
cl_df <- df[, names(df) %in% var]
per_gp_loc <- per_gp_loc[order(-per_gp_loc$count), ]
others_loc <- c(as.character(per_gp_loc[7:nrow(per_gp_loc), 'Location']))
maplist <- rep('others',length(others_loc))
require(plyr) # mapvalues function
cl_df$Location <- mapvalues(cl_df$Location, from= others_loc, to=maplist)
num_df <- (cl_df[,c(1, 2)])
norm_minmax <- function(x){
                           (x- min(x)) /(max(x)-min(x))
}
num_df<- as.data.frame(lapply(num_df[, c(1, 2)], norm_minmax))
cl_df <- (cl_df[, -c(1, 2)])
cl_df <- cbind(num_df, cl_df)
# below code to remove empty levels 
cl_df$Years <- as.factor(cl_df$Years)
column_class <- lapply(cl_df,class)
column_class <- column_class[column_class != "factor"]
factor_levels <- lapply(cl_df, nlevels)
factor_levels <- factor_levels[factor_levels > 1]
cl_df <- cl_df[,names(cl_df) %in% c(names(factor_levels), names(column_class))]
cl_df <- as.data.frame(unclass(cl_df))
head(cl_df)
# Elbow method:
data <- cl_df
# Elbow Method for finding the optimal number of clusters
set.seed(123)
# Compute and plot wss for k = 2 to k = 15.
k.max <- 15
data <- na.omit(data) # to remove the rows with NA's
wss <- sapply(1:k.max, 
              function(k){kproto(data, k)$tot.withinss})
wss_df <- data.frame(k=1:15, wss=wss)
ggplot(wss_df, aes(x = k, y = wss)) +
  geom_line() + geom_point()+
  scale_x_continuous(breaks = 1:15)
elbow_plot <-  plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
ggsave(dpi=300, filename = "elbow_plot.png", path='C:/Users/uness/Desktop/project/plots', device= 'png')
# Observation: Optimum number of clusters are between 4-6
kpred <- kproto(x=cl_df, k=6)
kpred
summary(kpred)
clprofiles(kpred, cl_df)
ggsave(dpi=300, filename = "clust_plot1.png", path='C:/Users/uness/Desktop/project/plots', device= 'png')
cl_df2 <- cbind(value=cl_df, clust=kpred$cluster, Crime=df$Crime.type)
cl_df2$clust <- as.factor(cl_df2$clust)
head(cl_df2)
bar_clust_crime <- ggplot(cl_df2, aes(clust, fill= Crime ))+
  geom_bar(position = "fill", width= 0.5)+
  theme(axis.text.x = element_text(angle = 90), legend.key.size = unit(0.5, "cm"))+
  ggtitle('Cluster vs Crime')
ggsave(dpi=300, filename = "Clust_Crime.png", path='C:/Users/uness/Desktop/project/plots', device= 'png')
# The variation for different Crime types does not vary significantly in clusters,
_____________________________________________________________________________
_____________________________________________________________________________
## Assumptions
_____________________________________________________________________________
_____________________________________________________________________________
#### Above analysis of data demonstrates that the proportion of various crimes does not vary significantly in various districts, however, a few types of crimes are limited to paticular districts such as bicycle theft and the proportion of a few categories such as Anti-social behavior is higher in some districts."district" varibale might not be the strongest feature possessing predictive characteristics, however, including this variable for modeling might increase the accuracy of the model to a reasonable extent. In addition, creating number of features representing the amount (count) of each crime with respect to its district can increase the model accuracy.

#### Analyzing the distribution of different crimes across greater Manchester shows several distinct patterns which means the probability of an incident might be higher in some places compare to others., hence I assume it is safe to consider coordination variables(Longtitude, Latitude) as strong predictors to determine types of crime. However, further investigation pinpoints the strong effect of city of Manchester, hence to avoid building a bais model it's necessary to reduce the weight of city of Manchester through regularization technique.

#### from 8160 mentioned locations in Location variable, 6 top Location account for ~10% of crime occurrence, and further investigation unfolds that the highest proportion of crimes vary based on properties of each location. For instance, Anti-social behavior accounts for the highest proportion of crimes in sport/recreation areas, while shoplifting as expected holds the highest quota in supermarkets and shopping area, hence I consider the "Location" variable as a predictor with acceptable predictive attributes, to increase the quality of this predictor, and avoid overfitting while improving generalization capability of model, removing values with low occurence is crucial, though. 

#### It seems "Months" variable does not illustrate a strong correlation with type of crime since no clear pattern such as seasonality or trend has been observed, however, the distribution for top 3 crimes varies in each months. Overall, "Months" variable might not be a strong preditor of our target variable, so I exclude it from the list of independent variables/ predictors.

#### Further investigation of crime.type with respect to years, displays a trend in number of crimes over 8 years from Jan 2011- Dec 2018, Also distribution of crime indicates a clear shrinking trend over 8 years across the area.I accept "Years" variable as an acceptible variable and include it in the list of predictors.

#### Date variable shows trend and seasonality for each type of crime to a fairly good extent, I'll include this variable in the prediction.

#### Other correlated features:It seems Distribution (Longitude, Latitude) and Year have a correlation, since as Years pass the distribution shrinks more and more. In addition, It is Obvious that number of crimes is positively correlated to the population size of that sector. rate of incident is significantly higher in downtown of cities and towns where more businesses are located and reduces as it moves toward rural places and low populated areas, also number of incidents is extremely high in number of locations such as parking area and shopping centers, hence the existance of a correlation between districts, Location and distribution of crime is inevitable. 

### In next step, I'll impute missing values, create number of new features for train and test datasets, drop weak predictors, and then I would prepare features for feeding to algorithms by transforming and normalizing them.  
train<- read.csv("train.csv", 
              header = T, stringsAsFactors = F, na.strings = c("", "NA", "#N/A"))
test<- read.csv("test.csv", 
              header = T, stringsAsFactors = F, na.strings = c("", "NA", "#N/A"))
copy<- train
cte<- test
feat_crt<- function(dataframe){
  col_names<- names(dataframe)
  dataframe <- data.frame(matrix(unlist(dataframe),nrow=nrow(dataframe)),stringsAsFactors=FALSE)
  names(dataframe) <- col_names
  names(dataframe)[names(dataframe)== "X"]<- "Row_Number"
  names(dataframe)[names(dataframe)== "Month"]<- "Date"
  dataframe$Years <- year(anytime(dataframe$Date))
  dataframe$Months <- month(anytime(dataframe$Date))
  dataframe$half <- mapvalues(dataframe$Months, from= c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), to=c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2))
  dataframe$season <- mapvalues(dataframe$Months, from= c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), to=c('winter', 'winter', 'spring', 'spring', 'spring', 'summer', 'summer', 'summer', 'autumn', 'autumn', 'autumn', 'winter'))
  dataframe<-mutate(dataframe, Date_New=anydate(Date)) 
  dataframe$District<- gsub("On or near ", "", as.character(dataframe$LSOA.name))
  dataframe$District<- lapply(strsplit(dataframe$District," ",fixed=TRUE), "[", 1)
  dataframe$Location<- gsub("On or near ", "", as.character(dataframe$Location))
  row_dataframe= as.data.frame(dataframe$Row_Number)
  col_drops<- c("Row_Number", "LSOA.name", "Date")
  dataframe<- dataframe[, !(names(dataframe) %in% col_drops)]
  dataframe<-mutate(dataframe, District = sapply(District, toString))
  dataframe$Longitude<- as.numeric(dataframe$Longitude)
  dataframe$Latitude<- as.numeric(dataframe$Latitude)
  dataframe<- dataframe[!(dataframe$District=='NA'),]
  return(dataframe)
}
# Creating test and train files for SAS
test <- feat_crt(test)
train <- feat_crt(train)
per_gp_loc <- train %>% 
  group_by(Location) %>% 
  summarise(count = n())
per_gp_loc<- as.data.frame(per_gp_loc)
per_gp_loc <- per_gp_loc[order(-per_gp_loc$count), ]
#per_gp_loc$Location <- as.character(per_gp_loc$Location)
sum(per_gp_loc[1:7,"count"])/sum(per_gp_loc$count)*100
others_loc <- c(as.character(per_gp_loc[8:nrow(per_gp_loc), 'Location']))
maplist <- rep('others',length(others_loc))
train$Location <- mapvalues(train$Location, from= others_loc, to=maplist)
per_gp_loc <- test %>% 
  group_by(Location) %>% 
  summarise(count = n())
per_gp_loc<- as.data.frame(per_gp_loc)
per_gp_loc <- per_gp_loc[order(-per_gp_loc$count), ]
#per_gp_loc$Location <- as.character(per_gp_loc$Location)
sum(per_gp_loc[1:7,"count"])/sum(per_gp_loc$count)*100
others_loc <- c(as.character(per_gp_loc[8:nrow(per_gp_loc), 'Location']))
maplist <- rep('others',length(others_loc))
test$Location <- mapvalues(test$Location, from= others_loc, to=maplist)
tot_dataset <- rbind(train, test)
write.csv(test, "new datasets/test_sas.csv", row.names = T)
write.csv(train, "new datasets/train_sas.csv", row.names = T)
write.csv(tot_dataset, "new datasets/data_sas.csv", row.names = T)
copy <- feat_crt(copy)
head(copy)
# mapping low frequency values in Location to 'others'
per_gp_loc <- copy %>% 
  group_by(Location) %>% 
  summarise(count = n())
per_gp_loc<- as.data.frame(per_gp_loc)
per_gp_loc <- per_gp_loc[order(-per_gp_loc$count), ]
#per_gp_loc$Location <- as.character(per_gp_loc$Location)
sum(per_gp_loc[1:7,"count"])/sum(per_gp_loc$count)*100
others_loc <- c(as.character(per_gp_loc[8:nrow(per_gp_loc), 'Location']))
maplist <- rep('others',length(others_loc))
copy$Location <- mapvalues(copy$Location, from= others_loc, to=maplist)
head(copy)
# Data transformation: I'll create two types of dataframe, one without transforming variables(labeling, normalization and outlier treatment) and I'll use it to predict target variable via decision tree and random forest algorithms, and another dataframe with transformation and I'll utilize it to predict target variable through linear regression, lasso, ridge, elastic, decision tree, random forest, knn, naive bayes
# without transformation:
drops <- c('Date_New', 'LSOA.code')
no_tr<- copy
no_tr <- no_tr[, !(names(no_tr) %in% drops)]
head(no_tr)
# with transformation:
library(fastDummies) # creating dummy variables
dummy_lst <- c("Location","Months", "season", "District")
bin_dummy_lst <- c("half")
tr <- copy
tr <- fastDummies::dummy_cols(tr, select_columns= dummy_lst)
tr <- fastDummies::dummy_cols(tr, select_columns= bin_dummy_lst, remove_first_dummy= TRUE)
tr<- tr[, !(names(tr) %in% dummy_lst)]
tr <- tr[, !(names(tr) %in% bin_dummy_lst)]
tr <- tr[, !(names(tr) %in% drops)]
tr$Years <- mapvalues(tr$Years, from=c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018), to=c(1, 2, 3, 4, 5, 6, 7, 8))
norm_minmax <- function(x){
                           (x- min(x)) /(max(x)-min(x))
                          }
tr$Years <- as.numeric(tr$Years)
norm_df<- as.data.frame(lapply(tr[, c(1, 2, 4)], norm_minmax))
drops<- c(names(tr[, c(1, 2, 4)]))
tr <- tr[, !(names(tr) %in% drops)]
tr <- cbind(norm_df, tr)
head(tr)
# Random Forest with the dataset without transformation:
# shuffeling the data and deviding that to train and validation, 
set.seed(47)
val_size <- floor(0.3*nrow(no_tr))
samp <- sample(nrow(no_tr), val_size, replace=FALSE)
y_val <- no_tr[samp, 4]
X_val <- no_tr[samp, -c(4)]
y_train <- no_tr[-samp, 4]
X_train <- no_tr[-samp, -c(4)]
# converting target variable to categorical 
y_train <- factor(y_train)
y_val <- factor(y_val)
# Creating training and vlidation set
train <- cbind(y_train, X_train)
validation <- cbind(y_val, X_val)
colnames(train)[1] <- 'Label'
colnames(validation)[1] <- 'Label'
fac <- data.frame(lapply(train[, c(4, 8, 9)], as.factor))
train <- train[, -c(4, 8, 9)]
train <- cbind(train, fac)
fac_val <- data.frame(lapply(validation[, c(4, 8, 9)], as.factor))
validation <- validation[, -c(4, 8, 9)]
validation <- cbind(validation, fac_val)
validation <- rbind(train[1, ] , validation)
validation <- validation[-1,]
X_val <- validation[, -c(1)]
require(randomForest)
# Fitting model
mtry = 8 # since there are only 8 independent variables I will set Number of randomly selected variables for each split 70% of all predictors.
model_1 <-  randomForest::randomForest(Label~., data = train, importance = TRUE)
print(model_1)
# prediction on validation set and model evaluation
pred_1 = predict(model_1, X_val)
accuracy_m1 <-  mean(y_val == pred_1)
print(paste0('accuracy of model_1 is: ', round(accuracy_m1, 4)*100, '%' )) # 47.09% Accuracy
f1_score <- function(predicted, expected, positive.class="1") {
    predicted <- factor(as.character(predicted), levels=unique(as.character(expected)))
    expected  <- as.factor(expected)
    cm = as.matrix(table(expected, predicted))
    precision <- diag(cm) / colSums(cm)
    recall <- diag(cm) / rowSums(cm)
    f1 <-  ifelse(precision + recall == 0, 0, 2 * precision * recall / (precision + recall))
    #Assuming that F1 is zero when it's not possible compute it
    f1[is.na(f1)] <- 0
    #Binary F1 or Multi-class macro-averaged F1
    ifelse(nlevels(expected) == 2, f1[positive.class], mean(f1))
}
paste0('F1-score: ', f1_score(pred_1, y_val, positive.class="1"))
#install.packages('pROC')
library(pROC)
pred_1 <- factor(pred_1, ordered=T)
multiclass.roc(y_val, pred_1)
# Random Forest with the preprocessed dataset:
# shuffeling the data and deviding that to train and validation, 
set.seed(47)
val_size <- floor(0.3*nrow(tr))
samp <- sample(nrow(tr), val_size, replace=FALSE)
train <- tr[-samp, ]
validation <- tr[samp, ]
y_val <- validation[, 4]
X_val <- validation[, -c(4)]
y_train <- train[, 4]
X_train <- train[, -c(4)]
# converting target variable to factor 
y_train <- factor(y_train)
y_val <- factor(y_val)
# converting dummy variables to categorical
fac <- data.frame(lapply(X_val[, c(4:42)], as.factor))
X_val <- X_val[, -c(4:42)]
X_val <- cbind(X_val, fac)
fac <- data.frame(lapply(X_train[, c(4:42)], as.factor))
X_train <- X_train[, -c(4:42)]
X_train <- cbind(X_train, fac)
# Creating training and vlidation set
train <- cbind(y_train, X_train)
validation <- cbind(y_val, X_val)
colnames(train)[1] <- 'Label'
colnames(validation)[1] <- 'Label'
head(train)
# install.packages('randomForest')
# Fitting model
mtry = sqrt(45) # since there are only 8 independent variables I will set Number of randomly selected variables for each split 70% of all predictors.
model_2 <-  randomForest::randomForest(Label~., data = train, importance = TRUE)
print(model_2)
# prediction on validation set and model evaluation
validation <- rbind(train[1, ] , validation)
validation <- validation[-1,]
X_val <- validation[, -c(1)]
pred_2 = predict(model_2, X_val)
accuracy_m2 <-  mean(y_val == pred_2)
print(paste0('accuracy of model_2 is: ', round(accuracy_m2, 4)*100, '%' )) #"accuracy of model_2 is: 47.0%"
f1_score(pred_2, y_val, positive.class="1")
pred_2 <- factor(pred_2, ordered=T)
multiclass.roc(y_val, pred_2)
importance <-  randomForest::importance(model_1)
varImportance <-  data.frame(Variables = row.names(importance),
                           Importance =round(importance[, 'MeanDecreaseAccuracy'],2))
rankImportance <-  varImportance%>%
  mutate(Rank=paste('#',dense_rank(desc(Importance))))
feat_imp_ml1 <- ggplot(rankImportance,aes(x=reorder(Variables,Importance),
                          y=Importance,fill=Importance, group= Importance))+ 
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'white') +
  labs(x = 'Variables') +
  coord_flip() + 
  theme_classic()+
  ggtitle('RF Feature Importance for 1st Model' )
ggsave(feat_imp_ml1, filename = 'feat_imp_ml1.png', dpi = 300,path='C:/Users/uness/Desktop/project/plots', device= 'png' )
importance <-  randomForest::importance(model_2)
varImportance <-  data.frame(Variables = row.names(importance),
                           Importance =round(importance[, 'MeanDecreaseAccuracy'],2))
rankImportance <-  varImportance%>%
  mutate(Rank=paste('#',dense_rank(desc(Importance))))
feat_imp_ml2 <- ggplot(rankImportance,aes(x=reorder(Variables,Importance),
                          y=Importance,fill=Importance, group= Importance))+ 
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'white') +
  labs(x = 'Variables') +
  coord_flip() + 
  theme_classic()+
  ggtitle('RF Feature Importance for 2nd Model' )
ggsave(feat_imp_ml2, filename = 'feat_imp_ml2.png', dpi = 300,path='C:/Users/uness/Desktop/project/plots', device= 'png' )
# Feature selection:
ordered_imp <- varImportance[order(-varImportance$Importance), ]
portion <- sum(ordered_imp$Importance[1:20])/sum(ordered_imp$Importance)*100
print(paste0(round(portion, 2), '% variation of the response variable is explicable with 20 features'))
imp_feat <- c(as.character(ordered_imp[1:20, "Variables"]))
# Logistic Regression
 X_train <- X_train[, imp_feat]
 X_val <- X_val[, imp_feat]
 train <- cbind(y_train, X_train)
 validation <- cbind(y_val, X_val)
# setting one of the levels of the dependent variable as a baseline
 colnames(train)[1] <- 'Label'
 colnames(validation)[1] <- 'Label'
train$Label <- relevel(train$Label, ref='Burglary')
require(nnet) # utilizing multi-nominal model
lin_model <- multinom(Label~., data=train, maxit= 500)
lin_model$deviance
lin_model$AIC
# iter 340 value 25475.667290
# Final negative log value: 25475.667290
# our final negative log likelihood is a cost function that is used as loss for machine learning models, telling us how bad it's performing, the lower the better
# Residual deviance: Residual Deviance (in Regression Only) is short for Mean Residual Deviance and measures the goodness of the models' fit
# Null deviance: Deviance is a measure of goodness of fit of a model. Higher numbers always indicates bad fit. The null deviance shows how well the response variable is predicted by a model that includes only the intercept (grand mean) where as residual with inclusion of independent variables
# Residual Value: 50951.33
# AIC : 51551.33
# The Akaike Information Criterion (AIC) is 51551.33 and it provides a method for assessing the quality of your model through comparison of related models.
# calculating Z score and p-Value for the variables in the model
z <- summary(lin_model)$coefficients/summary(lin_model)$standard.errors
p <- (1-pnorm(abs(z),0,1))*2
eval_tab_Anti_social <- rbind(summary(lin_model)$coefficients[2, ],summary(lin_model)$standard.errors[2, ],z[2, ],p[2, ])
rownames(eval_tab_Anti_social) <- c("Coefficient","Std. Errors","z stat","p value")
knitr::kable(eval_tab_Anti_social)
# Table of Probabilities
p_tab <- as.data.frame(p)
knitr::kable(p_tab)
# Table of Coefficiencies
coe_tab <- as.data.frame(summary(lin_model)$coefficients)
knitr::kable(coe_tab)
# Prediction
pred_lin <- predict(lin_model, X_val)
# Creating a confusion Matrix
pred_lin <- factor(pred_lin, levels = c(levels(y_val)))
cm <- caret::confusionMatrix(as.factor(pred_lin),as.factor(y_val))
# Overal statistics of Confusion Matrix
acc <- cm$overall[[1]]
# The accuracy of linear model on validation set is ¬ 32% wich is less than the RF model accuracy
miss_class <- mean(as.character(pred_lin) != as.character(y_val))
# also miss classification rate is ¬ 67%, and that means the model has predicted wrong labels 67 out of 100 times
print(paste0('The Accuracy for Multi-Nomial Regression Model is : ', round(acc*100, 2), '% and miss classification rate is: ', round(miss_class*100, 2), '%'))
paste0('F1-score for logistic model: ',f1_score(pred_lin, y_val, positive.class="1"))
pred_lin <- factor(pred_lin, ordered=T)
multiclass.roc(y_val, pred_lin)
mostImportantVariables <- varImp(lin_model)
mostImportantVariables$Variables <- row.names(mostImportantVariables)
feat_imp_lin_ml <- ggplot(mostImportantVariables,aes(x=reorder(Variables, Overall),
                          y=Overall,fill=Overall, group= Overall))+ 
  geom_bar(stat='identity') +
  labs(x = 'Variables') +
  coord_flip() + 
  theme_classic()+
  ggtitle('Feature Importance for Multi nomial linear Model' )
ggsave(feat_imp_lin_ml, filename = 'feat_imp_lin_ml.png', dpi = 300,path='C:/Users/uness/Desktop/project/plots', device= 'png' )
set.seed(47)
val_size <- floor(0.3*nrow(tr))
samp <- sample(nrow(tr), val_size, replace=FALSE)
train <- tr[-samp, ]
validation <- tr[samp, ]
y_val <- validation[, 4]
X_val <- validation[, -c(4)]
y_train <- train[, 4]
X_train <- train[, -c(4)]
# converting target variable to factor 
y_train <- factor(y_train)
y_val <- factor(y_val)
# converting dummy variables to categorical
fac <- data.frame(lapply(X_val[, c(4:43)], as.factor))
X_val <- X_val[, -c(4:43)]
X_val <- cbind(X_val, fac)
fac <- data.frame(lapply(X_train[, c(4:43)], as.factor))
X_train <- X_train[, -c(4:43)]
X_train <- cbind(X_train, fac)
train <- cbind(y_train, X_train)
validation <- cbind(y_val, X_val)
colnames(train)[1] <- 'Label'
colnames(validation)[1] <- 'Label'

# below code to remove level error in predict function of svm
column_class <- lapply(train,class)
column_class <- column_class[column_class != "factor"]
factor_levels <- lapply(train, nlevels)
factor_levels <- factor_levels[factor_levels > 1]
train <- train[,names(train) %in% c(names(factor_levels), names(column_class))]
train <- as.data.frame(unclass(train))
column_class <- lapply(validation,class)
column_class <- column_class[column_class != "factor"]
factor_levels <- lapply(validation, nlevels)
factor_levels <- factor_levels[factor_levels > 1]
validation <- validation[,names(validation) %in% c(names(factor_levels), names(column_class))]
validation <- as.data.frame(unclass(validation))
# importing svm algorithm using e1071 package
model_svm <- svm(Label~., data=train) # training without Dist_St variable
summary(model_svm) # cost function can be changed to avoid overfitting
pred_svm <- predict(model_svm, validation[, -c(1)])
pred_svm <-  factor(pred_svm, c(levels(y_val)))
cm_svm <- caret::confusionMatrix(as.factor(pred_svm),as.factor(y_val))
svm_acc <- cm_svm$overall[1] # accuracy of model is 46.97%
svm_missclass <- mean(as.character(pred_svm)!= as.character(y_val)) # missclassification rate is ¬ 53.03%
print(paste0('The Accuracy for SVM Model is : ', round(svm_acc*100, 2), '% and miss classification rate is: ', round(svm_missclass*100, 2), '%'))
svm_class <- cbind(prediction= summary(pred_svm), Actual=summary(y_val)) 
svm_class
paste0('F1-score for SVM model: ',f1_score(pred_svm, y_val, positive.class="1"))
pred_svm <- factor(pred_svm, ordered=T)
multiclass.roc(y_val, pred_svm)
