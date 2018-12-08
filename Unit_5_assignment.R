# Samuel Kadyebo Unit # 5 Home Work

library(dplyr)
library(plyr)
library(tidyr)
library(dataMaid)

#1 Data Munging
#a and b importing file add names to columns and display summary and structure of df
yob2016_df <- read.csv("C:/Users/Kadyebo/Desktop/SMU/6306/HW5/yob2016.txt", header=TRUE, sep=";") 
colnames(yob2016_df)<-c("1stName","Sex","NumberWithName")
summary(yob2016_df)
#C finding misspelled name[s] with 3 "yyy"
grep("yyy$", yob2016_df$`1stName`, value = TRUE)
#d removing misspelled name[s] observation from dataset
y2016 <- yob2016_df[!yob2016_df$`1stName` == "Fionayyy",]

#2 Data Merging
#a Importing and cleaning y2015 so as merge with y2016
y2015 <- read.csv("C:/Users/Kadyebo/Desktop/SMU/6306/HW5/yob2015.txt", header=TRUE)
colnames(y2015)<-c("1stName","Sex","NumberWithName")
View(y2015)
tail(y2015, 10)
#b
message("Last 10 rows in this dataframe all have NumberwithName set to 5")
#c Merging y2016 and y2015 and no NA values
final <- merge(y2016,y2015, by="1stName") %>% drop_na()

#3 Data Summary
#a and b
SumNumberWithName = final$NumberWithName.y+final$NumberWithName.x #to get individual totals
final$Total = SumNumberWithName
final
sum(final$Total) #to get the total number

#3b:
  head(final[order(-final$Total),], n=10) #to sort by highest total for the top 10 most popular names
  
#c
Top10GirlsName <- select(filter(final[order(-final$Total),], 
                    final[order(-final$Total),]$Sex.y == "F"),c("1stName"))
head(Top10GirlsName, 10)

#d Writing top 10 girls names and their Totals to a CSV file
GirlsNameandTotals <- select(filter(final[order(-final$Total),], 
                                final[order(-final$Total),]$Sex.y == "F"),c("1stName","Total"))
Top10GirlsNameandTotals <- GirlsNameandTotals[1:10,]
View(Top10GirlsNameandTotals)
write.csv(Top10GirlsNameandTotals,'Top10GirlsNameandTotals2.csv')

#4 Upload to GitHub
#makeCodebook(final)
message("The URL to assignment 5 Github repo: https://github.com/SamuelKadyebo/MSDS-6306")






