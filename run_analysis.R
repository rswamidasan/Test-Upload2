
######################################################
##     Getting & Cleaning Data - Course Project     ##
######################################################

##
##  This script creates a tidy data set from the
##  Samsung Galaxy S Smartphone data sets provided.
##
##  The data provided are in two data sets - Test 
##  and Train.  Each data set has its own directory 
##  which contains 3 text (.txt) files:
##
##  (1) subject_ : Identifies the individual
##                 on whom the observation was made.
##  (2) y_ : Codes to identify the activity performed
##           by the individual during the observation.
##  (3) X_ : The measured variables of the observation.
##
##  This script also uses data from the features.txt 
##  and activity_labels.txt files which are in the parent 
##  directory of the test and training data sets.
##  
##  - The features file has a list of the measured variables,
##    which are used by this script because the X_ files do not
##    have column names to identify the measured variables.
##
##  - The activity_labels file gives descriptive activity
##    names to the codes present in the y_ files.
##
##  This script will create a tidy data set by doing the
##  following:
##
##  1) Merge the training and the test sets to create one
##     data set.
##  2) Extract only the measurements on the mean & standard 
##     deviation for each measurement. 
##  3) Use descriptive activity names to name the activities
##     in the data set.
##  4) Appropriately label the data set with descriptive
##     variable names. 
##  5) Create a second, independent tidy data set with the 
##     average of each variable for each activity and each
##     subject, from the data set in step 4.
##
##  ----------------------------------------------------------

##  Loading the dplyr package for the convenience of 
##  mutate, arrange, grouping and chaining operations.

library(dplyr)

## Step 1: Merge the training and test data sets
##
##         - Read the test and train file into data frames (DF)
##         - Merge, by Row, the corresponding subject_, y_, and 
##           X_ DFs.
##         - Merge, by column, the subject_, y_, and X_ DFs.
##
##           "T" after "df" indicates Train data read in.
##           No "T" = Test data read in; Train data merged in later.

## Reading Subject data ... and merging test & train
df_Sub  <- read.table("test/subject_test.txt") 
dfT_Sub <- read.table("train/subject_train.txt") 
df_Sub  <- rbind(df_Sub, dfT_Sub)

## df_Sub has the merged subject data.

rm(dfT_Sub)

## Reading Activity data ... and merging test & train
df_y  <- read.table("test/y_test.txt")
dfT_y <- read.table("train/y_train.txt")
df_y  <-rbind(df_y, dfT_y)

## df_y has the merged activity data.
rm(dfT_y)

## Merging activity and subject data
dfM <- cbind(df_y, df_Sub)
rm(df_y, df_Sub)

## Reading measured variables ... and merging test & train
df_X <- read.table("test/X_test.txt") 
dfT_X <- read.table("train/X_train.txt")
df_X <-rbind(df_X, dfT_X)

## df_X has the merged measured variables.

rm(dfT_X)

## Merging measured variables df with activity and subject dfs
dfM <- cbind(dfM, df_X)

##  dfM has the complete merged data set.

rm(df_X)

## ***** End of Step 1 *****

## ***** The merged data in dfM is 10,299 rows x 563 cols. ***** 

## Step 2: Extract only the mean & standard deviation measurements 
##
##         - The required variables have "mean", "Mean"
##           or "std" present in their name.  
##
##         - Read the features file. Variable names are in Col 2.
##         - as.is = T, to prevent the var names being treated as
##           a factor (to allow string manipulation later on).

fdf <- read.table("features.txt", as.is = TRUE)
 
##  Form a logical vector with TRUE only in the indices of the 
##  needed variables.

reqVars <- (grepl("mean", fdf$V2) | grepl("std", fdf$V2) | 
            grepl("Mean", fdf$V2)) 

##  The first two columns of dfM (Activity & Subject) are
##  also needed -- prepend 2 x TRUEs to the logical vector.

reqCols <- c(TRUE, TRUE, reqVars)

##  Select a subset of dfM with only the (86) needed columns.
sub_dfM <- dfM[, reqCols]

##  The large, merged DF (10,299 x 563) is not needed further.
rm(dfM)

## ***** End of Step 2 *****

##  The needed data is in sub_dfM - 10,299 rows x 86 cols

## Step 3: Use descriptive Activity names.
##
##         - Replace Activity codes (1 thru' 6) od sub_dfM with
##           corresponding labels in the activity_labels file

dfAL <- read.table("activity_labels.txt")
sub_dfM$V1 <- dfAL$V2[sub_dfM$V1]

##  The Activity variable is a factor, by default, and has
##  levels assigned in alphabetical order.  Reset them
##  to be the same as in the activity_labels file.

levels(sub_dfM$V1) <- dfAL$V2

rm(dfAL)

## ***** End of Step 3 *****

## Step 4: Label the df columns with descriptive variable names. have been read into

##         - Mask the rows of fdf with the logical vector of required variables.
##         - Place those needed in the Required Variable Names df (rVNames).
##         - Mutate Col 2 of rVNames via chained operations to get a tidy, 
##           syntactically correct set of measured variable names.
    
rVNames <- fdf[(reqVars), ] %>%
           mutate(V2 = sub("\\()", "", V2)) %>%              
           mutate(V2 = gsub(")", "", V2))   %>%
           mutate(V2 = sub("\\(", "_", V2)) %>%
           mutate(V2 = sub(",", "_", V2))   %>%
           mutate(V2 = gsub("-", "_", V2))  %>%
           mutate(V2 = gsub("BodyBody", "Body", V2))

##  Assign the tidied up measured variable names to the cols of sub_dfM,
##  prepended with "Activity" & "Subject" as names for cols 1 & 2.
 
colnames(sub_dfM) <- c("Activity", "Subject", rVNames$V2)
                  
## ***** End of Step 4 *****

## Step 5: Create a final Tidy Data Set with the Average of 
##         each Variable for each Activity and each Subject.     

##         - Group the df by Activity & Subject.
##         - Summarize each of the measured variable cols with mean().      

sub_dfM <- group_by(sub_dfM, Activity, Subject)
tidy_df <- summarise_each(sub_dfM, funs(mean))

##  The tidy Data Set, tidy_df, has 180 rows x 86 cols, with:
##      - One summarized observation per row, grouped by Activity & Subject.
##      - one variable per column -- 2 x Fixed and 84 x Measured variables.

## ***** End of Step 5 (Part A) ***** 


##  Step 5 (Part B)

##  The tidy_df can be made tidier to conform to Tidy Data norms:
##
##         1) Split each row into 2 x rows, 1 for Acc and 1 for Gyro
##         2) Introduce a new Fixed variable named "Device" to identify the above.
##         3) Group related variables together, e.g.
##            a) The "Mag" variable(s) should follow the corresponding XYZ variables
##            b) "fBody" variable set should follow the "tBody" variable set
##            c) "angle_...GravityMean" should follow the "tGravity..." variable set.
##         4) Set values in Gyro row variables to NA if data not present.
##         5) Fix an incorrectly named variable.
##
##         (See the CodeBook and README for a justification.)
##

colVN <- colnames(tidy_df)             ## store all tidy_data col names

##  Split tidy_df, by column, into the following DFs:
##
##        1) ASdf   : with Activity & Subject cols
##        2) Accdf  : with Acc cols (Accelerometer Body variables)
##        3) Gyrodf : with Gyro cols (Gyroscope Body variables)
##        4) Gravdf : with Gravity cols (Gravity acceleration variables)
##        5) angdf  : with angle variables
##

ASdf <- tidy_df[,1:2]                  ## make a new DF of Activity & Subject cols

                                       ## make a Truth vector of "BodyAcc" cols
AccCols <- grepl("BodyAcc", colVN)  & !grepl("angle", colVN)

Accdf <- tidy_df[, AccCols]            ## make a new DF of BodyAcc cols
AccVN <- colnames(Accdf)               ## extract "...BodyAcc..." variable names

                                       ## make a Truth vector of "BodyGyro" cols
GyroCols <- grepl("BodyGyro", colVN) & !grepl("angle", colVN) 
Gyrodf   <- tidy_df[, GyroCols]        ## make a new DF of BodyGyro cols

GravCols <- grepl("tGravity", colVN)   ## make a Truth vector of "tGravity" cols
Gravdf   <- tidy_df[,GravCols]         ## make a new DF of tGravity cols

angCols <- grepl("angle", colVN)       ## make a Truth vector of "angle" cols
angdf   <- tidy_df[, angCols]          ## make a new DF of angle cols 

                                       ## fixing a typo in one angle col name.

colnames(angdf) <- sub("AccMean_gravity", "AccMean_gravityMean", colnames(angdf))

##  Constructing a new Accdf from Accdf (+ ASdf, Gravdf, angdf), with :
##               * Activity & Subject prepended
##               * Col 3 named "Device" with values = "Acc"
##               * The "Mag" variable(s) moved to immediately after the corresponding XYZ variables.
##               * Gravity cols appended after the "t" and "f' variables.
##               * Angle cols without "Gyro" in col name appended.

new_Accdf <- cbind(ASdf, Device = "Acc", 
                   Accdf[,1:3], Accdf[,13], Accdf[,4:6], Accdf[,14],
                   Accdf[,7:9], Accdf[,15], Accdf[,10:12], Accdf[,16], 
                   Accdf[,17:19], Accdf[,35], Accdf[,20:22], Accdf[,36],
                   Accdf[,23:25], Accdf[,37], Accdf[,26:28], Accdf[,38],
                   Accdf[,29:31], Accdf[,39], Accdf[,32:34], Accdf[,40], 
                   Gravdf[,1:3], Gravdf[,7], Gravdf[,4:6], Gravdf[,8],
                   angdf[,1:2], angdf[,5:7])

##  Reset the col names affected by coercion to the original names.

colnames(new_Accdf)[7]  <- colnames(Accdf)[13]
colnames(new_Accdf)[11] <- colnames(Accdf)[14]
colnames(new_Accdf)[15] <- colnames(Accdf)[15]
colnames(new_Accdf)[19] <- colnames(Accdf)[16]
colnames(new_Accdf)[23] <- colnames(Accdf)[35]
colnames(new_Accdf)[27] <- colnames(Accdf)[36]
colnames(new_Accdf)[31] <- colnames(Accdf)[37]
colnames(new_Accdf)[35] <- colnames(Accdf)[38]
colnames(new_Accdf)[39] <- colnames(Accdf)[39]
colnames(new_Accdf)[43] <- colnames(Accdf)[40]

colnames(new_Accdf)[47] <- colnames(Gravdf)[7]
colnames(new_Accdf)[51] <- colnames(Gravdf)[8]

##  Revove "Acc" from new_Accdf col names
colnames(new_Accdf) <- sub("Acc", "", colnames(new_Accdf))

##  new_Accdf : 180 rows x 56 cols


## Constructing a new Gyrodf from Gyrodf (+ ASdf, Accdf, Gravdf, angdf), with :
##               * Activity & Subject prepended
##               * Col 3 named "Device" with values = "Gyro"
##               * The "Mag" variable(s) moved to immediately after the corresponding XYZ variables.
##               * "fBodyAccJerK" XYZ cols from Accdf inserted with NA values 
##               * "fBodyGyroJerKMag" col(s) from Gyrodf inserted after the corresponding XYZ variables.
##               * Gravity cols appended after the "t" and "f" variables with values set to NA.
##               * Angle cols with "Gyro" or XYZ in col name appended, with NA values in XYZ cols.
##
##               + The cols with NAs are required to give the new Gyrodf the
##                 same structure as the new Accdf.
##               + Further, some of the missing values may represent observations
##                 that should be recorded; specifically -- the "fBody...JerK" XYZ variables

Gravdf[,] <- as.numeric(NA)          ## This data must be set to NA for insertion into the new Gyro df.

fBAJerkCols  <- grepl("fBodyAccJerk_", AccVN)   ## Get the "fBodyAccJerk_" cols from (the old) Accdf.
fBAJerkdf    <- Accdf[, fBAJerkCols]  
fBAJerkdf[,] <- as.numeric(NA)       ## This data must be set to NA for insertion into the new Gyro df. 

angdf[,5:7] <- as.numeric(NA)        ##  XYZ angle data is not valid for Gyro; must be set to NA.

new_Gyrodf <- cbind(ASdf, Device = "Gyro", 
                    Gyrodf[,1:3], Gyrodf[,13], Gyrodf[,4:6], Gyrodf[,14], 
                    Gyrodf[,7:9], Gyrodf[,15], Gyrodf[,10:12], Gyrodf[,16],
                    Gyrodf[,17:19], Gyrodf[,26], Gyrodf[,20:22], Gyrodf[,27],
                    Gyrodf[,23:25], Gyrodf[,28], fBAJerkdf[,1:3], Gyrodf[,29],
                    fBAJerkdf[,4:6], Gyrodf[,30], fBAJerkdf[,7:9], Gyrodf[,31], 
                    Gravdf[,1:3], Gravdf[,7], Gravdf[,4:6], Gravdf[,8],
                    angdf[,3:7])

##  new_Gyrodf : 180 rows x 56 cols

##  The columns of new_Gyrodf and new_Accdf match exactly.
##  
##      - Set the new_Gyrodf colnames to that of new_Accdf.

colnames(new_Gyrodf) <- colnames(new_Accdf)

##  Remove not needed DFs.
rm(Gravdf, angdf, Gyrodf, Accdf, ASdf, fBAJerkdf, tidy_df)

##  Stack new_Accdf and new_Gyrodf. 

tidier_df <- rbind(new_Accdf, new_Gyrodf)

##  tidier_df : 360 rows x 56 cols, comprising of 
##            - 180 rows of Accelerometer measured variables, followed by
##            - 180 rows of Gyroscope measured variables
##              - Each row has :
##                   - 3 x Fixed varaiables: Activity, Subject, Device, followed by
##                   - 53 x Measured variables (with 20 x vars = NA in "Gyro" rows)

##  Sort tidier_df by Activity, Subject & Device cols

tidier_df <- arrange(tidier_df, Activity, Subject, Device)

## tidier_df now has 180 "sets" of observations :
##           - Grouped by the Fixed variables Activity, Subject & Device.
##           - Each set has, for the same Activity & Subject :
##                  (a) 1 x row of Accelerometer measurements (Device = "Acc")
##                  (b) 1 x row of Gyroscope measurements (Device = "Gyro")

write.table(tidier_df, "tidy_data.txt", row.names = FALSE)

##  *****  End of Step 5 (Part B)

##  *****  END OF SCRIPT  *****

#############################################################################

