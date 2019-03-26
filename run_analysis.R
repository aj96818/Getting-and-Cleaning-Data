
# 'Run_analysis.R' script for peer-graded assignment on getting and cleaning data.

# tidy data as per the ReadMe that can be read into R with read.table(header=TRUE)

# Import Data

library(readr)

setwd('D:/coursera/Data Science/cleaning data/final')

X_test <- read_table2("D:/coursera/Data Science/cleaning data/final/test/X_test.txt", 
                      col_names = FALSE)

X_train <- read_table2("D:/coursera/Data Science/cleaning data/final/train/X_train.txt", 
                       col_names = FALSE)

features <- read_csv("D:/coursera/Data Science/cleaning data/final/features.txt", 
                     col_names = FALSE)

# Descriptive values for all rows to be added as additional column to data
y_test <- read_csv("D:/coursera/Data Science/cleaning data/final/test/y_test.txt", 
                     col_names = FALSE)

subject_test <- read_csv("D:/coursera/Data Science/cleaning data/final/test/subject_test.txt", 
                     col_names = FALSE)

y_train <- read_csv("D:/coursera/Data Science/cleaning data/final/train/y_train.txt", 
                     col_names = FALSE)

activity_labels <- read_csv("D:/coursera/Data Science/cleaning data/final/activity_labels.txt", 
                    col_names = FALSE)

head(features$X1)
head(subject_test$X1)
head(X_test$X1)
head(X_train$X1)
head(y_test$X1)
head(y_train$X1)

# label 'test' and 'train' datasets using 'features' vector as column names:

colnames(X_test)<-c(features$X1)
colnames(X_train)<-c(features$X1)

# replace integer code with activity labels (e.g., "WALKING", "WALKING_UPSTAIRS", etc.)

y_test$X1<-ifelse(y_test$X1 == 1, "WALKING",
           ifelse(y_test$X1 == 2, "WALKING_UPSTAIRS",
           ifelse(y_test$X1 == 3, "WALKING_DOWNSTAIRS",
           ifelse(y_test$X1 == 4, "SITTING",
           ifelse(y_test$X1 == 5, "STANDING",
           ifelse(y_test$X1 == 6, "LAYING", "NA"))))))

y_train$X1<-ifelse(y_train$X1 == 1, "WALKING",
            ifelse(y_train$X1 == 2, "WALKING_UPSTAIRS",
            ifelse(y_train$X1 == 3, "WALKING_DOWNSTAIRS",
            ifelse(y_train$X1 == 4, "SITTING",
            ifelse(y_train$X1 == 5, "STANDING",
            ifelse(y_train$X1 == 6, "LAYING", "NA"))))))

# rename column
colnames(y_test)<-c('activity_label')
colnames(y_train)<-c('activity_label')

# Add 'activity labels' column to X_test and X_train datasets:
x_test<-cbind(X_test, y_test)
x_train<-cbind(X_train, y_train)

# Combine x_test & x_train datasets using rbind:
x_both<-rbind(x_test, x_train)

# Create subset of data with only mean and std deviation measurements:

grep1<-grepl('.*[Ss]td.*', features$X1)
grep2<-grepl('.*[Mm]ean.*', features$X1)

col_interest1<-x_both[grep1]
col_interest2<-x_both[grep2]

x_all_vars<-cbind(col_interest1, col_interest2)

# Compute average across all observations for each variable grouped by activity.

# split activity into list

df_list<-split(x_all_vars, x_all_vars$activity_label)

average_func<-function(df){
  x<-subset(df, select = -c(activity_label))
  x<-apply(x, 2, mean)
  return(x)
}

df_list_output<-lapply(df_list, average_func)

# need to fix output

laying<-as.data.frame(df_list_output[1])
sitting<-as.data.frame(df_list_output[2])
standing<-as.data.frame(df_list_output[3])
walking<-as.data.frame(df_list_output[4])
walking_downstairs<-as.data.frame(df_list_output[5])
walking_upstairs<-as.data.frame(df_list_output[6])

tidy_activity<-cbind(laying, sitting, standing, walking, walking_downstairs, walking_upstairs)

write.table(tidy_activity, 'tidy_dataset.txt',row.names = F)

#write_csv(test, 'test_walking.csv')


  
  
  