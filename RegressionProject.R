##Sofia and Madison##
##Final Project##
library(dplyr)
test <- read.table(file = "train.csv", header = TRUE, sep=",")
head(test)
summary(test)
str(test)
length(test$Platform)
#Cleaning data set 
test_df <- na.omit(test)
str(test_df)
test_df <- subset(test_df, Gender != "Non-binary")
test_new <- subset(test_df, !(Platform %in%  c("Telegram", "LinkedIn", "Whatsapp")))
head(test_new)
str(test_new)

#Creating Gender Dummy Variable
test_new$Gender_dummy <- ifelse(test_new$Gender == "Male", 0, 1)
test_new <- na.omit(test_new)
test_new

#create dummy variables for platform (Base: Facebook)
test_new$platform_twitter <- ifelse(test_new$Platform == "Twitter", 1, 0)
test_new$platform_instagram <- ifelse(test_new$Platform == "Instagram", 1, 0)
test_new$platform_snapchat <- ifelse(test_new$Platform == "Snapchat", 1, 0)
test_new

#Assigning numeric values to Dominant_Emotion column (Neutral = 0, Happy = 1, Sad = 2, Anger = 3, Anxiety = 4, Boredom = 5)
test_new$emotion_happy <- ifelse(test_new$Dominant_Emotion == "Happiness", 1, 0)
test_new$emotion_sad <- ifelse(test_new$Dominant_Emotion == "Sadness", 1, 0)
test_new$emotion_anger <- ifelse(test_new$Dominant_Emotion == "Anger", 1, 0)
test_new$emotion_anxiety <- ifelse(test_new$Dominant_Emotion == "Anxiety", 1, 0)
test_new$emotion_bored <- ifelse(test_new$Dominant_Emotion == "Boredom", 1, 0)
test_new

test_new$Dominant_Emotion
test_new$Platform

#dropping char variables from data
test_new <- subset(test_new, select = -c(Platform, Dominant_Emotion, Gender))
head(test_new)

test_new$Age

#changes age to int instead of character
test_new$Age <- as.integer(test_new$Age)        # Character variable
print(test_new$Age)  # Outputs: 42
test_new

#Assigning quantitative variables
y <- test_new$Daily_Usage_Time..minutes.
x1 <- test_new$Age
x2 <- test_new$Posts_Per_Day
x3 <- test_new$Likes_Received_Per_Day
x4 <- test_new$Comments_Received_Per_Day
x5 <- test_new$Messages_Sent_Per_Day


#Assigning dummy variables
d1 <- test_new$Gender_dummy
d2 <- test_new$platform_twitter
d3 <- test_new$platform_instagram
d4 <- test_new$platform_snapchat
d5 <- test_new$emotion_happy
d6 <- test_new$emotion_sad
d7 <- test_new$emotion_anger
d8 <- test_new$emotion_anxiety
d9 <- test_new$emotion_bored

d9

#Histograms, boxplots, scatter plots
dev.new()
hist(x1,
     main = "Age of Individuals",
     xlab = "Age",
     ylab = "Number of Indivuals",
     col = "lightblue")

hist(x1,
     main = "Minutes Spent For Individuals",
     xlab = "Minutes Spent Per Day",
     ylab = "Number of Indivduals",
     col = "#FF66CC")

hist(d1~d2~d3~d4,
     main = "Minutes Spent For Individuals",
     xlab = "Minutes Spent Per Day",
     ylab = "Number of Indivduals",
     col = "#FF66CC")

boxplot(x2,
        main = "Posts per day",col = "gray")

par(mfrow = c(1,3))
dev.new()
dev.off()
boxplot(x1,	main = "Age", col = "pink")
boxplot(x2,	main = "Posts per day",col = "lightyellow")
boxplot(x3,	main = "Likes per day",col = "#FF9933")
boxplot(x4,	main = "Comments per day",col = "lightgreen")
boxplot(x5,	main = "Messages sent per day",col = "#9933FF")


pairs(test_new)
round(cor(test_new),digit=3)

#Histogram and boxplots for y variable and dummy variables
dev.new()
hist(y, 
     main = "Daily Social Media Usage (Minutes)", 
     xlab = "Daily Usage Time (minutes)", 
     ylab = "Number of Individuals", 
     col = "#FF66CC")

dev.new()
hist(d5, 
     main = "Happy Emotion Dummy (0 = No, 1 = Yes)", 
     xlab = "Number of Individuals", 
     ylab = "Happy Emotion Dummy", 
     col = "pink")

dev.new()
hist(d6, 
     main = "Sad Emotion Dummy (0 = No, 1 = Yes)", 
     xlab = "Number of Individuals", 
     ylab = "Sad Emotion Dummy", 
     col = "#FF3366")

dev.new()
hist(d8, 
     main = "Anxiety Emotion Dummy (0 = No, 1 = Yes)", 
     xlab = "Number of Individuals", 
     ylab = "Anxiety Emotion Dummy", 
     col = "green")

dev.new()
boxplot(y ~ d5, 
        main = "Daily Usage by Happy Emotion", 
        xlab = "Happy Emotion (0 = No, 1 = Yes)", 
        ylab = "Daily Usage Time (minutes)", 
        col = "coral")

dev.new() 
boxplot(y ~ d6, 
        main = "Daily Usage by Sad Emotion", 
        xlab = "Sad Emotion (0 = No, 1 = Yes)", 
        ylab = "Daily Usage Time (minutes)", 
        col = "blue")

dev.new()
boxplot(y ~ d8, 
        main = "Daily Usage by Anxiety Emotion", 
        xlab = "Anxiety Emotion (0 = No, 1 = Yes)", 
        ylab = "Daily Usage Time (minutes", 
        col = "purple")

dev.new()
boxplot(y ~ d1, 
        main = "Daily Usage by Gender", 
        xlab = "Gender (0 = Male, 1 = Female)", 
        ylab = "Daily Usage Time (minutes)",
        col = "lightblue")

dev.new()
boxplot(y ~ d2, 
        main = "Daily Usage by Twitter Platform", 
        xlab = "Twitter (0 = No, 1 = Yes)", 
        ylab = "Daily Usage Time (minutes)", 
        col = "green")

dev.new()
boxplot(y ~ d3, 
        main = "Daily Usage by Instagram Platform", 
        xlab = "Instagram (0 = No, 1 = Yes)", 
        ylab = "Daily Usage Time (minutes)", 
        col = "yellow")

dev.new()
boxplot(y ~ d1, 
        main = "Daily Usage by Gender", 
        xlab = "Male = 0, Female = 1", 
        ylab = "Daily Usage Time (minutes)", 
        col = "#CC66ff")

barplot(d2,d3,d4)

library(ggplot2)

# Create a summary data frame
summary_data <- data.frame(
  Platform = c("Happy", "Sad", "Angry", "Anxious", "Bored"),
  Usage = c(sum(d5, na.rm = TRUE), sum(d6, na.rm = TRUE), sum(d7, na.rm = TRUE), sum(d8, na.rm = TRUE), sum(d9, na.rm = TRUE))
)

# Create the bar graph
ggplot(summary_data, aes(x = Platform, y = Usage, fill = Platform)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Daily Usage by Platform",
       x = "Emotions",
       y = "Users",
       fill = "Platform") +
  theme_minimal()

library(tidyr)

# Combine your data into a data frame
data <- data.frame(
  y = test_new$Daily_Usage_Time..minutes.,
  d2 = test_new$platform_twitter,
  d3 = test_new$platform_instagram,
  d4 = test_new$platform_snapchat
)


data_long <- pivot_longer(data, cols = starts_with("d"), names_to = "Variable", values_to = "Value")


ggplot(data_long, aes(x = Variable, y = y, fill = Variable)) +
  geom_boxplot() +
  labs(title = "Boxplots of Daily Usage Time by Variables",
       x = "Variables",
       y = "Daily Usage Time (minutes)") +
  theme_minimal() +
  theme(legend.position = "none") # Hide legend i


# Load necessary library
library(ggplot2)

test_new$Platform <- data.frame(
  y = test_new$Daily_Usage_Time..minutes.,
  d2 = test_new$platform_twitter,
  d3 = test_new$platform_instagram,
  d4 = test_new$platform_snapchat
)

test_new$Platform
test_new$Dominant_Emotion

dev.new()
boxplot(y~test_new$Platform, 
        main = "Daily Social Media Usage vs. PlatForm", 
        xlab = "Platform", 
        ylab = "Daily Ussage Time (minutes)", 
        col = c("lightpink", "lightblue", "#FFFF66", "#FF9900"))

dev.new()
boxplot(y~test_new$Dominant_Emotion, 
        main = "Daily Social Media Usage vs. Dominant Emotion", 
        xlab = "Dominant Emotion", 
        ylab = "Daily Ussage Time (minutes)", 
        col = c("#6633FF", "#CC66ff", "#cc0099","#ff00cc", "#ff6699"))

# Example dataset
# Generate usage data for each platform and emotion combination
daily_usage <- c(
  rnorm(50, mean = 0.46, sd = 0.5), # Instagram
  rnorm(50, mean = 0.08, sd = 0.27), # Snapchat
  rnorm(50, mean = 0.35, sd = 0.48)  # Twitter
)

# Repeat Platform and Emotion labels accordingly
platform <- rep(c("Instagram", "Snapchat", "Twitter"), each = 50)
emotion <- rep(c("Happy", "Sad", "Angry", "Bored"), length.out = 150) # Length matches `daily_usage`

# Create the data frame
social_media_data <- data.frame(
  Platform = platform,
  Emotion = emotion,
  DailyUsage = daily_usage
)

# Display the first rows of the dataset
head(social_media_data)


# Boxplot: Daily Social Media Usage vs. Platform
p1 <- ggplot(social_media_data, aes(x = Platform, y = DailyUsage, fill = Platform)) +
  geom_boxplot() +
  labs(title = "Daily Social Media Usage vs. Platform", x = "Platform", y = "Daily Usage Time (minutes)") +
  theme_minimal()

# Boxplot: Daily Social Media Usage vs. Dominant Emotion
p2 <- ggplot(social_media_data, aes(x = Emotion, y = DailyUsage, fill = Emotion)) +
  geom_boxplot() +
  labs(title = "Daily Social Media Usage vs. Dominant Emotion", x = "Emotion", y = "Daily Usage Time (minutes)") +
  theme_minimal()

# Display the plots
print(p1)
print(p2)




#correlation matrix for daily usage & emotions
dev.new()
round(cor(test_new[, c("Daily_Usage_Time..minutes.", "emotion_happy", "emotion_sad", "emotion_anxiety", "emotion_anger", "emotion_bored")]), digit = 3)

library(dplyr)
testnew<- test_new %>% select(test_new$Age, test_new$Daily_Usage_Time..minutes., test_new$Posts_Per_Day, test_new$Likes_Received_Per_Day, test_new$Comments_Received_Per_Day, test_new$Messages_Sent_Per_Day)

test_new$Age <- na.omit(test_new$Age)

test_new$Age
test_new$Daily_Usage_Time..minutes.
test_new$Posts_Per_Day
test_new$Likes_Received_Per_Day
test_new$Comments_Received_Per_Day
test_new$Messages_Sent_Per_Day

########### Correlation and Regression
library(corrplot)
round(cor(test_new),digits = 2)
mydata.cor = cor(test_new)
dev.new()
install.packages("wesanderson")
# Load
library(wesanderson)
my_colors <- colorRampPalette(c("lightblue", "white", "pink"))(50) # 200 colors

corrplot(mydata.cor, col= my_colors)

library(tidyverse)
library(knitr)
library(lavaan)
library(psych)
library(MBESS)

dev.new()
test_new %>% 
  select(Age, Posts_Per_Day, Likes_Received_Per_Day, Comments_Received_Per_Day, Messages_Sent_Per_Day)%>% 
  pairs.panels()
############## Multiple Linear Regression
fit<-lm(y~x1+x2+x3+x4+x5)
summary(fit)

fit<-lm(y~x1+x2+x3+x4+x5+d1+d2+d3+d4+d5+d6+d7+d8+d9)
summary(fit)

library(lessR)
SummaryStats(d4)

d8
d9

fitted<-fitted(fit) # predicted values
fitted
e<-residuals(fit) # residuals
e
dev.new()
par(mfrow = c(2,1))
plot(fitted, e)
abline(a=0, b=0)
plot(x1,e)
abline(a=0, b=0)
dev.off()
####### plots
hist(e)
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(fit)

dev.off()
# leverage
lev = hat(model.matrix(fit))
lev

#####################################Diagnostics 
influence.measures(fit)
rstandard(fit)# it also give standardized residuals 
rstudent(fit) #studentized deleted residuals


stdres<-rstandard(fit) 
stdres
hist(stdres)
qqnorm(stdres, ylab= "Standardized Residuals")


stud.del.resids=rstudent(fit) #studentized deleted residuals 
hist(stud.del.resids)
qqnorm(stud.del.resids, ylab= "studentized Deleted Residuals")


hatvalues(fit) #get the leverage values (hii)
cooks.distance(fit) #get Cook's distance
dfbetas(fit) #print all dfbetas
dfbetas(fit)[4,1] #dfbeta for case 4, first coefficient (i.e., b_0)
dffits(fit) [4] #dfits for case 4
influence(fit) #various influence statistics, including hat values and dfbeta (not dfbetas) values
library(car) #load the package car
avPlots(fit) #added variable plots



influencePlot(fit)

pairs(test_new)
round(cor(test_new),digit=3)

library(corrplot)
round(cor(test_new),digits = 2)
mydata.cor = cor(test_new)
dev.new()
corrplot(mydata.cor)
