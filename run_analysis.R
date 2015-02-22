## run_analysis.R

## Script Objective
    ## 1. Merge "training" and "test" data sets into one data set.
    ## 2. Extracts mean and standard deviation for each measurement.
    ## 3. Rename activity names to be more descriptive.
    ## 4. Remane variable names to be more descriptive.
    ## 5. Create independent tidy data set with 
         ## the average of each variable for each activity and each subject.

## Load necessary R-Code packages
library(plyr)
library(dplyr)
library(tidyr)
library(reshape2)

## Rememeber Home
home_dir<- getpw()

## File locations
root_dir<-"~/Desktop/Data Science Notes/Data Cleaning"
project_dir<-paste0(root_dir,"/","Data-Cleaning-Project")
dataset_dir<-paste0(root_dir,"/","UCI HAR Dataset")
test_dir<-paste0(dataset_dir,"/","test")
## test_IS_dir<-paste0(test_dir,"/","Inertial Signals")
train_dir<-paste0(dataset_dir,"/","train")
## train_IS_dir<-paste0(train_dir,"/","Inertial Signals")

## Set Working Directory
setwd(project_dir)

## PART 1 - Merge "test" and "train" data sets

## Merge "test" data sets
test<-read.table(paste0(test_dir,"/","subject_test.txt")) %>% ##Subject
    rename("Subject" = V1) %>%
    cbind(read.table(paste0(test_dir,"/","y_test.txt"))) %>% ## y data
    rename("Activity" = V1) %>%
    cbind(read.table(paste0(test_dir,"/","X_test.txt"))) ## X data


## Merge "train" data set
train<-read.table(paste0(train_dir,"/","subject_train.txt")) %>% ##Subject
    rename("Subject" = V1) %>%
    cbind(read.table(paste0(train_dir,"/","y_train.txt"))) %>% ## y data
    rename("Activity" = V1) %>%
    cbind(read.table(paste0(train_dir,"/","X_train.txt"))) ## X data

## Merge "Test" amd "train" datasets
UCI_HAR_all<-rbind(test,train)
    rm(test) ## Remove "test" data-frame
    rm(train) ## Remove "train" data-frame

## PART 2 - Extract out only "mean" and "std" observations  

## Subset feature list for "mean" or "std" 
features_ms<-read.table(paste0(dataset_dir,"/","features.txt")) %>% 
    separate(V2,into=c("Measurement","Stat_type"),sep="-",extra="merge") %>%
    separate(Stat_type,into=c("Stat_type","Direction"),sep="-",extra="merge") %>%
    separate(Stat_type,into="Stat_type",extra="drop") %>%
    separate(Measurement,into=c("tf","type"),sep=1) %>%
    filter(Stat_type=="mean" | Stat_type=="std") %>%
    mutate(Index=V1+2)   %>% ## Creates index for subsetting combined dataset
    arrange(Stat_type, type, Direction,tf)

features_ms$type<-lapply(features_ms$type, function (x) 
    {gsub('BodyBody','Body',x)})


## Subset combined "test" and "train" dataset for "mean" or "std"
UCI_HAR_all<-select(UCI_HAR_all,1,2,features_ms$Index)

## PART 3 - Convert activities index to descripive labels

## Read in Activity Labels
act_labels<-read.table(paste0(dataset_dir,"/","activity_labels.txt"))

## Rename activity labels per legend
for (i in 1:6) {
    UCI_HAR_all$Activity <- gsub(i,act_labels$V2[i],UCI_HAR_all$Activity)
}

## PART 4 - Convert activities index to descripive labels

## Create new label with "_" between attributes f/t, type and direction
f_labels<-features_ms %>%
    unite(label,tf,type,Direction,sep="_")

## Create a column for "Variable" & parse our "Mean" measurement
UCI_HAR_all_mean<-UCI_HAR_all %>%
    select(1:35) %>% 
    mutate(Variable="Mean") %>%
    select (1,2,36,3:35)

## Create a column for "Variable" & parse our "std" measurement
UCI_HAR_all_std<-UCI_HAR_all%>%
    select(1,2,36:68) %>% 
    mutate(Variable="Std Dev") %>%
    select (1,2,36,3:35)

## Rename columns with new labels
colnames(UCI_HAR_all_mean)[4:36] <- f_labels$label[1:33]
colnames(UCI_HAR_all_std)[4:36] <- f_labels$label[1:33]

## Re-combine datasets
UCI_HAR_all<-rbind(UCI_HAR_all_mean,UCI_HAR_all_std)

## PART 5 - Group by Activity and Subject and average each measurement

final_tidy<-UCI_HAR_all %>%
    arrange(Activity,Subject,Variable) %>%
    filter( Variable == "Mean") %>%
    select(1,2,4:36) %>%
    ddply(.(Activity,Subject),summarise,
        ##  BodyAcc - Time
        Avg_t_BodyAcc_X= mean(t_BodyAcc_X),   
        Avg_t_BodyAcc_Y= mean(t_BodyAcc_Y),
        Avg_t_BodyAcc_Z= mean(t_BodyAcc_Z),
        ##  GravityAcc - Time
        Avg_t_GravityAcc_X= mean(t_GravityAcc_X),   
        Avg_t_GravityAcc_Y= mean(t_GravityAcc_Y),
        Avg_t_GravityAcc_Z= mean(t_GravityAcc_Z),       
        ##  BodyAccJerk - Time
        Avg_t_BodyAccJerk_X= mean(t_BodyAccJerk_X),   
        Avg_t_BodyAccJerk_Y= mean(t_BodyAccJerk_Y),
        Avg_t_BodyAccJerk_Z= mean(t_BodyAccJerk_Z),
        ##  BodyGyro - Time
        Avg_t_BodyGyro_X= mean(t_BodyGyro_X),   
        Avg_t_BodyGyro_Y= mean(t_BodyGyro_Y),
        Avg_t_BodyGyro_Z= mean(t_BodyGyro_Z),
        ##  BodyGyroJerk - Time
        Avg_t_BodyGyroJerk_X= mean(t_BodyGyroJerk_X),   
        Avg_t_BodyGyroJerk_Y= mean(t_BodyGyroJerk_Y),
        Avg_t_BodyGyroJerk_Z= mean(t_BodyGyroJerk_Z),      
        ##  BodyAccMag - Time
        Avg_t_BodyAccMag= mean(t_BodyAccMag_NA),   
        ##  GravityAccMag - Time
        Avg_t_GravityAccMag= mean(t_GravityAccMag_NA),   
        ##  BodyAccJerkMag - Time
        Avg_t_BodyAccJerkMag= mean(t_BodyAccJerkMag_NA),        
        ##  BodyGyroMag - Time
        Avg_t_BodyGyroMag= mean(t_BodyGyroMag_NA),        
        ##  BodyGyroJerkMag - Time
        Avg_t_BodyGyroJerkMag= mean(t_BodyGyroJerkMag_NA),   
         ##  BodyAcc - Frequency
        Avg_f_BodyAcc_X= mean(f_BodyAcc_X), 
        Avg_f_BodyAcc_Y= mean(f_BodyAcc_Y),
        Avg_f_BodyAcc_Z= mean(f_BodyAcc_Z),
        ##  BodyAccJerk - Frequency
        Avg_f_BodyAccJerk_X= mean(f_BodyAccJerk_X), 
        Avg_f_BodyAccJerk_Y= mean(f_BodyAccJerk_Y),
        Avg_f_BodyAccJerk_Z= mean(f_BodyAccJerk_Z),        
        ##  BodyGyro- Frequency
        Avg_f_BodyGyro_X= mean(f_BodyGyro_X), 
        Avg_f_BodyGyro_Y= mean(f_BodyGyro_Y),
        Avg_f_BodyGyro_Z= mean(f_BodyGyro_Z),        
        ##  BodyAccMag - Frequency
        Avg_f_BodyAccMag= mean(f_BodyAccMag_NA),   
        ##  BodyAccJerkMag - Frequency
        Avg_f_BodyAccJerkMag= mean(f_BodyAccJerkMag_NA),        
        ##  BodyGyroMag - Frequency
        Avg_f_BodyGyroMag= mean(f_BodyGyroMag_NA),        
        ##  BodyGyroJerkMag - Frequency
        Avg_f_BodyGyroJerkMag= mean(f_BodyGyroJerkMag_NA))

## Final Output

write.table(final_tidy,paste0(project_dir,"/final_tidy.txt"),row.name=FALSE)
