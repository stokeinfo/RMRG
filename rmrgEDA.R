###RMRG EDA###

df <- read.csv("./scripts/toupload/mergedmaster.csv", sep=",", header=TRUE, stringsAsFactors=FALSE)

####Data Formatting####
#convert Total.Due to numbers
Total.Due <- df$Total.Due

library(stringr)

Total.Due <- str_extract(df$Total.Due, "\\d(.*)\\d")

#numbers over 999 have commas, need to strip out to properly turn to numeric
Total.Due <- gsub(",","", Total.Due)

df$Total.Due <- as.numeric(Total.Due)


####Now the fun part####

library(ggplot2)
library(scales)

#Scatterplot of PriorityScore by Total.Due with smoother line to show relationship
qplot(PriorityScore,Total.Due, data=df, geom = c("point", "smooth"))

#Why is expected dollar score greater than Total.Due?
df2 <- df[which(!is.na(df$PriorityScore)), c("Total.Due", "PriorityScore")]
length(which(df2$PriorityScore > df2$Total.Due))


#Histogram of Total.Due
qplot(Total.Due, data=df)


#Summarizing by state
class(df$State)
df$State<- as.factor(df$State)
library(plyr)
dfState <- ddply(df, .(State), summarise, Total.Due=sum(Total.Due))
dfState$Total.Due <- round(dfState$Total.Due, 0)


#Plot of Total.Due by State
a <- ggplot(dfState, aes(State, Total.Due, fill=State))
a + geom_bar(stat="identity") + scale_y_continuous(limits=c(0, 500000), labels=comma) + theme(legend.position="none")


#box and whisker plot by state
b <- ggplot(df, aes(State, Total.Due, fill=State))
b + geom_boxplot() + theme(legend.position="none")
