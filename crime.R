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
require(clustMixType) # K-prototypes for clustering
require(factoextra) # computing clustering tendency based on Hopkins statistic

# setting working directory

#setwd("C:Users/uness/Desktop/project/paper/code/Crime-Analysis/")
getwd()

# reading all .csv files in the working directory, merging row wise and importing
# to the environment
temp = list.files(pattern="*.csv")
dataset<- ldply(temp, read.csv, header=TRUE)
# dataset includes 96 separate dataframe from 2011-01 to 2018-12
paste0("Dataframe dimension :", dim(dataset)) #intial dataset has 2,815,198 rows and 12 columns
head(dataset)
print(names(dataset))

#preparing a dataset with appropriate features,
# removing "Crime.ID", "Reported.by", "Falls.within",
# "Last.outcome.category", and "Context"
drops<- c("Crime.ID", "Reported.by", "Falls.within", "Last.outcome.category","Context")
dataset<- dataset[, !(names(dataset) %in% drops)]
names(dataset)

#based on the scale of this paper I have chosen only specific types of crimes for prediction, hence I'll subset the dataset based on rows containing those specific types of crimes. 7 crimes have been chosen from the total of 16,
crimes <- c('Violence and sexual offences', 
            'Burglary', 'Criminal damage and arson', 'Shoplifting', 
            'Public order', 'Violent crime')

dataset <- subset(dataset, Crime.type %in% crimes)

dim(dataset) # new dataset has 1,705,717 rows and 7 columns

# shuffling and sampling
set.seed(47)
rows<- sample(nrow(dataset), as.integer((nrow(dataset))*0.1))
sample_df<- dataset[rows, ]
dim(sample_df)

# Stratified sampling
set.seed(42)
#iris_subset <- iris[c(1:50, 51:80, 101:120), ]
stratified_sample <- sample_df %>%
  group_by(Crime.type) %>%
  mutate(num_rows=n()) %>%
  sample_frac(0.4, weight=num_rows) %>%
  ungroup

# These results should be equal
#table(iris_subset$Species) / nrow(iris_subset)
table(stratified_sample$Crime.type)*100 / nrow(stratified_sample)

dim(stratified_sample)
names(stratified_sample)
unique(stratified_sample$Crime.type)
head(stratified_sample)

# shuffling
 set.seed(123)
 rows<- sample(nrow(stratified_sample))
 df<- stratified_sample[rows,]
 head(df)
 dim(df)

write.csv(df, "df.csv", row.names = T)
rm(df, dataset, sample_df, stratified_sample)
df<- read.csv("df.csv", 
              header = T, stringsAsFactors = F, na.strings = c("", "NA", "#N/A"))



feat_crt<- function(dataframe){
  
  col_names<- names(dataframe)
  dataframe <- data.frame(matrix(unlist(dataframe),nrow=nrow(dataframe)),stringsAsFactors=FALSE)
  names(dataframe) <- col_names
  
  names(dataframe)[names(dataframe)== "X"]<- "Row_Number"
  names(dataframe)[names(dataframe)== "Month"]<- "Date"
  dataframe$Years <- year(anytime(dataframe$Date))
  #dataframe$Months <- month(anytime(dataframe$Date))
  #dataframe$half <- mapvalues(dataframe$Months, from= c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), to=c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2))
  #dataframe$season <- mapvalues(dataframe$Months, from= c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), to=c('winter', 'winter', 'spring', 'spring', 'spring', 'summer', 'summer', 'summer', 'autumn', 'autumn', 'autumn', 'winter'))
  #dataframe<-mutate(dataframe, Date_New=anydate(Date)) 
  dataframe$District<- gsub("On or near ", "", as.character(dataframe$LSOA.name))
  dataframe$District<- lapply(strsplit(dataframe$District," ",fixed=TRUE), "[", 1)
  dataframe$Location<- gsub("On or near ", "", as.character(dataframe$Location))
  row_dataframe= as.data.frame(dataframe$Row_Number)
  col_drops<- c("Row_Number", "LSOA.name", "Date", "X.1", "LSOA.code", 'num_rows', "X.2")
  dataframe<- dataframe[, !(names(dataframe) %in% col_drops)]
  dataframe<-mutate(dataframe, District = sapply(District, toString))
  dataframe$Longitude<- as.numeric(dataframe$Longitude)
  dataframe$Latitude<- as.numeric(dataframe$Latitude)
  
  dataframe<- dataframe[!(dataframe$District=='NA'),]
  
  return(dataframe)
}

new_df<-feat_crt(df)

head(new_df)
names(new_df)

write.csv(new_df, "new_df.csv", row.names = T)



# text mining the location and reducing the variance, then merging the new column
# to this daraftame

loc <- read.csv("myDf.csv", 
                header = T, stringsAsFactors = F, na.strings = c("", "NA", "#N/A"))

cl_df <- cbind(new_df, loc)
head(cl_df)
crime_col <- cl_df$Crime.type
head(crime_col)
drop_col <- c("Location", "Crime.type")
cl_df <- cl_df[, !names(cl_df) %in% drop_col]
head(cl_df, 10)

#require(plyr) # mapvalues function

num_df <- (cl_df[,c(1, 2)])
norm_minmax <- function(x){
  (x- min(x)) /(max(x)-min(x))
}
num_df<- as.data.frame(lapply(num_df[, c(1, 2)], norm_minmax))

cl_df <- (cl_df[, -c(1, 2)])
cl_df <- cbind(num_df, cl_df)
head(cl_df)

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
head(data)
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

# Observation: Optimum number of clusters are between 6-8

kpred <- kproto(x=cl_df, k=8)
kpred
summary(kpred)

clprofiles(kpred, cl_df)

cl_df2 <- cbind(cl_df, crime=crime_col, clust=kpred$cluster)
cl_df2$clust <- as.factor(cl_df2$clust)
head(cl_df2)


ggplot(cl_df2, aes(clust, fill= crime ))+
  geom_bar(position = "fill", width= 0.5)+
  theme(axis.text.x = element_text(angle = 90), legend.key.size = unit(0.5, "cm"))+
  ggtitle('Cluster vs Crime')

unique(cl_df2$Years)
train <- subset(cl_df2, Years %in% c("2017", "2016", "2015", "2014", "2013","2012", "2011"))
nrow(train)
head(train)
val <- subset(cl_df2, Years %in% c("2018"))
nrow(val)
head(val)

write.csv(train, "train.csv", row.names = T)
write.csv(val, "val.csv", row.names = T)

train<- read.csv("train.csv", 
                 header = T, stringsAsFactors = F, na.strings = c("", "NA", "#N/A"))

val<- read.csv("val.csv", 
                header = T, stringsAsFactors = F, na.strings = c("", "NA", "#N/A"))
