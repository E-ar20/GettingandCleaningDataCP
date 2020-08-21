library(dplyr)
library(tidyr)
library(readr)
library(stringr)

# Activity Labels
#
activity_labels <- read_delim("C:/Users/USER/Documents/GettingandCleaningDataCP/UCI_HAR_Dataset/activity_labels.txt", 
                              col_names = FALSE,
                              delim=" ")
activity_labels <- rename(activity_labels, activity.id = X1, activity.name = X2)

#
features.all <- read_delim("C:/Users/USER/Documents/GettingandCleaningDataCP/UCI_HAR_Dataset/features.txt", 
                           col_names = FALSE, 
                           delim=" ")
features.all <- rename(features.all, feature.id = X1, feature.desc = X2)

# 
features.all.vect <- as.vector(unlist(features.all[,2])) 

#
features.all.vect <- str_replace_all(features.all.vect, "\\(", "")
features.all.vect <- str_replace_all(features.all.vect, "\\)", "")
features.all.vect <- str_replace_all(features.all.vect, "-", ".")

#
mean_std <- grep(".*(mean|std)\\(\\).*", features.all$feature.desc)
#
features.mean_std <- features.all[mean_std,]


# Training Data 
#
subject_train <- read_delim("C:/Users/USER/Documents/GettingandCleaningDataCP/UCI_HAR_Dataset/train/subject_train.txt", 
                            col_names = FALSE,
                            delim=" ")
subject_train <- rename(subject_train, subject.id = X1)

y_train <- read_table("C:/Users/USER/Documents/GettingandCleaningDataCP/UCI_HAR_Dataset/train/y_train.txt", 
                      col_names = FALSE)
y_train <- rename(y_train, activity.id = X1)

x_train <- read_table("C:/Users/USER/Documents/GettingandCleaningDataCP/UCI_HAR_Dataset/train/X_train.txt", 
                      col_names = FALSE)
colnames(x_train) <- features.all.vect

#
x_train_mean_std <- x_train[,mean_std]

#
y_train_merge <- merge(y_train, activity_labels)
y_train_desc <- y_train_merge[,2]

#
train_data <- cbind(subject_train, x_train_mean_std, y_train_desc)
train_data <- rename(train_data, activity = y_train_desc)


# Test Data 
# 
subject_test <- read_delim("C:/Users/USER/Documents/GettingandCleaningDataCP/UCI_HAR_Dataset/test/subject_test.txt", 
                           col_names = FALSE,
                           delim=" ")
subject_test <- rename(subject_test, subject.id = X1)

y_test <- read_table("C:/Users/USER/Documents/GettingandCleaningDataCP/UCI_HAR_Dataset/test/y_test.txt", 
                     col_names = FALSE)
y_test <- rename(y_test, activity.id = X1)

x_test <- read_table("C:/Users/USER/Documents/GettingandCleaningDataCP/UCI_HAR_Dataset/test/X_test.txt", 
                     col_names = FALSE)
colnames(x_test) <- features.all.vect

#
x_test_mean_std <- x_test[,mean_std]

#
y_test_merge <- merge(y_test, activity_labels)
y_test_desc <- y_test_merge[,2]

#
test_data <- cbind(subject_test, x_test_mean_std, y_test_desc)
test_data <- rename(test_data, activity = y_test_desc)


# Merge test and training datasets

all_data <- rbind(train_data, test_data)

# Return tidy data set with the average of each variable summarized by each activity and each subject


tidy_data <- all_data %>%
  group_by(activity, subject.id) %>%
  summarize_each(funs(mean)) %>%
  arrange(activity, subject.id)

write.table(tidy_data, "tidyDatat.txt", row.names = FALSE)
