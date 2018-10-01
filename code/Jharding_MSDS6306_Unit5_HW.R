#############################################################################################################
###   a.  First, import the .txt file into R so you can process it. Keep in mind this is not a CSV file. 
##        You might have to open the file to see what you’re dealing with. Assign the resulting data frame to an 
##        object, df, that consists of three columns with humanreadable column names for each.
#############################################################################################################

setwd("./repos/SMU/SMU-MSDS-6306/Unit_5")
df <- read.delim("./data/yob2016.txt", sep = ";", header = FALSE)

#############################################################################################################
##    b.  Display the summary and structure of df
#############################################################################################################
summary.data.frame(df)
colnames(df) <- c("FName","Sex","y2016")

str(df)

#############################################################################################################
##    c.  Your client tells you that there is a problem with the raw file. One name was entered 
##        twice and misspelled. The client cannot remember which name it is; there are thousands he saw! 
##        But he did mention he accidentally put three y’s at the end of the name. Write an R command to 
##        figure out which name it is and display it.
#############################################################################################################

library(dplyr)



#############################################################################################################
##    d.  Upon finding the misspelled name, please remove this particular observation, as the 
##        client says it’s redundant. Save the remaining dataset as an object: y2016
#############################################################################################################



df[grepl("yyy$", df$FName),]
df[grepl("^Fiona", df$FName),]


y2016 <- df[!grepl("yyy$", df$FName),]
y2016[grepl("^Fiona", y2016$FName),]


#############################################################################################################
##   #2. Data Merging (30 points): 
##    Utilize yob2015.txt for this question. This file is similar to yob2016, but contains names, 
##    gender, and total children given that name for the year 2015.
#############################################################################################################



#############################################################################################################
##    a. Like 1a, please import the .txt file into R. Look at the file before you do. 
##    You might have to change some options to import it properly. Again, please give the 
##    dataframe human-readable column names. Assign the dataframe to y2015.
#############################################################################################################

y2015 <- read.csv("./data/yob2015.txt", header = FALSE)
colnames(y2015) <- c("FName","Sex","y2015")

#############################################################################################################
##    b. Display the last ten rows in the dataframe. Describe something you find interesting about these 10 rows.
#############################################################################################################

tail(y2015, 10)

#############################################################################################################
##    c. Merge y2016 and y2015 by your Name column; assign it to final. The client only cares about names 
##    that have data for both 2016 and 2015; there should be no NA values in either of your amount of children 
##    rows after merging.
#############################################################################################################

## Outer join 

## Check for is.NA values
sapply(y2015, function(x) sum(is.na(x)))
sapply(y2016, function(x) sum(is.na(x)))

## Merge y2015 & y2016 into final dataframe
final <- merge(y2015, y2016, all = TRUE)
sapply(final, function(x) sum(is.na(x)))

## Find and replace NA
final[is.na(final)] <- 0
sapply(final, function(x) sum(is.na(x)))

#############################################################################################################
##    3. Data Summary (30 points): 
##    Utilize your data frame object final for this part.
#############################################################################################################

#############################################################################################################
##    a. Create a new column called “Total” in final that adds the amount of children in 2015 and 2016 together. 
##    In those two years combined, how many people were given popular names?
#############################################################################################################  

final <- final %>%
  mutate(total = y2015 + y2016)

str(final)

#############################################################################################################
##    b. Sort the data by Total. What are the top 10 most popular names?
#############################################################################################################
  
top10 <- head(final[order(final$total, decreasing = TRUE),], 10)
cat("    Top 10 Names   ","\n","--------------------")
print(top10, row.names = FALSE)
  
#############################################################################################################
##    c. The client is expecting a girl! Omit boys and give the top 10 most popular girl’s names.
#############################################################################################################

filter(final, Sex == "F") 

top10F <- head(final %>%
  filter(Sex =='F') %>%
  arrange(desc(total)),10)

print(top10F, row.names = FALSE)

#############################################################################################################
##    d. Write these top 10 girl names and their Totals to a CSV file. Leave out the other columns entirely.
#############################################################################################################

write.csv(top10F[,c(1,5)], file="./data/top10femaleNames.csv", row.names = FALSE)

