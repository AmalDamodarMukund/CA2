#Reading whole raw data from CSV file
happiness_data_raw <- read.csv("World-happiness.csv")

#Dataframe
happiness_data_raw

#Structure of Dataframe
str(happiness_data_raw)

#Data cleaning

#Selecting required columns
happiness_data <- happiness_data_raw[, 3:11]
str(happiness_data)

#Summarizing the rows with NULL values
colSums(is.na(happiness_data))

#Removing NULL value rows
happiness_data <- na.omit(happiness_data)

#Checking and confirming there are no rows with NA rows after replacement with 0
colSums(is.na(happiness_data))

# Renaming the columns to meaningful one

names(happiness_data)[1] <- "life_ladder_score"
names(happiness_data)[2] <- "GDP"
names(happiness_data)[3] <- "social_support_score"
names(happiness_data)[4] <- "life_expectancy"
names(happiness_data)[5] <- "freedom_score"
names(happiness_data)[6] <- "generosity"
names(happiness_data)[7] <- "corruption_perception_score"
names(happiness_data)[8] <- "positive_score"
names(happiness_data)[9] <- "negative_score"
str(happiness_data)

#Rounding values to 2 decimal points
 
happiness_data$life_ladder_score <- round(happiness_data$life_ladder_score, 2)
happiness_data$GDP <- round(happiness_data$GDP, 2)
happiness_data$social_support_score <- round(happiness_data$social_support_score, 2)
happiness_data$life_expectancy <- round(happiness_data$life_expectancy, 2)
happiness_data$freedom_score <- round(happiness_data$freedom_score, 2)
happiness_data$generosity <- round(happiness_data$generosity, 2)
happiness_data$corruption_perception_score <- round(happiness_data$corruption_perception_score, 2)
happiness_data$positive_score <- round(happiness_data$positive_score, 2)
happiness_data$negative_score <- round(happiness_data$negative_score, 2)

str(happiness_data)

# Examine initial linearity between variables in the dataset
library(psych)
pairs.panels(happiness_data,
             smooth = FALSE,      # If TRUE, draws less smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = FALSE,    # If TRUE, draws ellipses
             method = "spearman",# Correlation method (also "pearson" or "kendall")
             pch = 21,           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE)          # If TRUE, adds confidence intervals


#Finding outliers

opar <- par(no.readonly = TRUE)
par(mfrow = c(4, 2))  # Adjust the margin values as needed
attach(happiness_data)
boxplot(life_ladder_score,
        main = "life_ladder_score",
        sub = paste("Outlier rows: ",
                    boxplot.stats(life_ladder_score)$out)) # box plot for 'life_ladder_score'
boxplot(GDP,
        main = "GDP",
        sub = paste("Outlier rows: ",
                    boxplot.stats(GDP)$out)) # box plot for 'GDP'
boxplot(social_support_score,
        main = "social_support_score",
        sub = paste("Outlier rows: ",
                    boxplot.stats(social_support_score)$out)) # box plot for 'social_support_score'

boxplot(life_expectancy,
        main = "life_expectancy",
        sub = paste("Outlier rows: ",
                    boxplot.stats(life_expectancy)$out)) # box plot for 'life_expectancy'

boxplot(freedom_score,
        main = "freedom_score",
        sub = paste("Outlier rows: ",
                    boxplot.stats(freedom_score)$out)) # box plot for 'freedom_score'
boxplot(generosity,
        main = "generosity",
        sub = paste("Outlier rows: ",
                    boxplot.stats(generosity)$out)) # box plot for 'generosity'
boxplot(corruption_perception_score,
        main = "corruption_perception_score",
        sub = paste("Outlier rows: ",
                    boxplot.stats(corruption_perception_score)$out)) # box plot for 'corruption_perception_score'
boxplot(positive_score,
        main = "positive_score",
        sub = paste("Outlier rows: ",
                    boxplot.stats(positive_score)$out)) # box plot for 'positive_score'
boxplot(negative_score,
        main = "negative_score",
        sub = paste("Outlier rows: ",
                    boxplot.stats(negative_score)$out)) # box plot for 'negative_score'
detach(happiness_data)
par(opar)


# Both the population and Income variables contain outliers.
# Use boxplot.stats() function to generate relevant outliers
outlier_values <- boxplot.stats(happiness_data$social_support_score)$out # outlier values.
paste("social_support_score outliers: ", paste(outlier_values, collapse=", "))

outlier_values <- boxplot.stats(happiness_data$life_expectancy)$out # outlier values.
paste("life_expectancy outliers: ", paste(outlier_values, collapse=", "))

outlier_values <- boxplot.stats(happiness_data$freedom_score)$out # outlier values.
paste("freedom_score outliers: ", paste(outlier_values, collapse=", "))

outlier_values <- boxplot.stats(happiness_data$generosity)$out # outlier values.
paste("generosity outliers: ", paste(outlier_values, collapse=", "))

outlier_values <- boxplot.stats(happiness_data$corruption_perception_score)$out # outlier values.
paste("corruption_perception_score outliers: ", paste(outlier_values, collapse=", "))

outlier_values <- boxplot.stats(happiness_data$positive_score)$out # outlier values.
paste("positive_score outliers: ", paste(outlier_values, collapse=", "))

outlier_values <- boxplot.stats(happiness_data$negative_score)$out # outlier values.
paste("negative_score outliers: ", paste(outlier_values, collapse=", "))

#Deleting Outliers for various variables

#Removing outliers in social_support_score
#Selecting outliers
values_to_delete_social_support_score <- c ("0.45","0.48","0.42","0.47","0.44","0.38","0.48",
                                            "0.43","0.44","0.44","0.29","0.33","0.42","0.48",
                                            "0.48","0.39","0.29","0.32","0.48","0.48","0.37",
                                            "0.46","0.44","0.29","0.3","0.44","0.48","0.51",
                                            "0.51","0.51","0.51","0.51","0.51","0.51")

# Delete outliers with selected values
happiness_data <- happiness_data[!(happiness_data$social_support_score %in% values_to_delete_social_support_score), ]

#After deleting, tested outliers again in social_support_score variable 
#Found below outliers which are also deleted 

values_to_delete_social_support_score <- c ("0.33", "0.33", "0.49","0.49","0.5","0.5","0.49",
                                            "0.49","0.49")

# Delete outliers with selected values
happiness_data <- happiness_data[!(happiness_data$social_support_score %in% values_to_delete_social_support_score), ]

#Removing outliers in life_expectancy
#Selecting outliers
values_to_delete_life_expectancy <- c ("40.9","40.38","32.3","36.86","41.42","40.3","41.2",
                                       "42.1","40.81","41.58","42.86","43.9")

# Delete outliers with selected values
happiness_data <- happiness_data[!(happiness_data$life_expectancy %in% values_to_delete_life_expectancy), ]


#Removing outliers in freedom_score
#Selecting outliers
values_to_delete_freedom_score <- c ("0.26","0.26","0.31","0.3","0.3","0.32","0.29","0.37","0.34","0.36",
                                     "0.33","0.37","0.37","0.33","0.36","0.37","0.36","0.36","0.37","0.34")

# Delete outliers with selected values
happiness_data <- happiness_data[!(happiness_data$freedom_score %in% values_to_delete_freedom_score), ]

#Removing outliers in generosity
#Selecting outliers
values_to_delete_generosity <- c ("0.42","0.44","0.41","0.4","0.42","0.45","0.44","0.41",
                                  "0.47","0.5","0.49","0.51","0.56","0.44","0.48","0.42",
                                  "0.46","0.4","0.64","0.69","0.69","0.68","0.65","0.49",
                                  "0.56","0.47","0.42","0.52","0.54","0.4","0.46","0.55",
                                  "0.4","0.39","0.35","0.35","0.38","0.35","0.35","0.38",
                                  "0.35","0.38","0.33","0.36","0.35","0.34","0.33","0.38",
                                  "0.36","0.34","0.35")

# Delete outliers with selected values
happiness_data <- happiness_data[!(happiness_data$generosity %in% values_to_delete_generosity), ]


#Removing outliers in corruption_perception_score
#Selecting outliers
values_to_delete_corruption_perception_score <- c ("0.37","0.38","0.37","0.36","0.4","0.41","0.41","0.37",
                                  "0.41","0.41","0.41","0.38","0.36","0.37","0.21","0.25",
                                  "0.21","0.17","0.22","0.19","0.17","0.24","0.19","0.21",
                                  "0.18","0.15","0.17","0.21","0.4","0.13","0.22","0.41",
                                  "0.32","0.36","0.31","0.26","0.22","0.25","0.19","0.2",
                                  "0.2","0.16","0.35","0.32","0.35","0.42","0.41","0.41",
                                  "0.42","0.41","0.41","0.4","0.34","0.36","0.37","0.36",
                                  "0.33","0.42","0.39","0.4","0.3","0.37","0.38","0.36",
                                  "0.33","0.38","0.39","0.42","0.4","0.36","0.41","0.36",
                                  "0.37","0.36","0.28","0.22","0.3","0.33","0.32","0.27",
                                  "0.29","0.31","0.27","0.19","0.28","0.22","0.21","0.23",
                                  "0.28","0.4","0.37","0.41","0.3","0.41","0.25","0.27",
                                  "0.27","0.27","0.18","0.29","0.41","0.16","0.08","0.12",
                                  "0.08","0.1","0.16","0.21","0.16","0.17","0.06","0.07",
                                  "0.04","0.06","0.1","0.24","0.13","0.1","0.05","0.16",
                                  "0.1","0.07","0.29","0.31","0.29","0.25","0.27","0.25",
                                  "0.32","0.25","0.23","0.25","0.24","0.26","0.25","0.2",
                                  "0.41","0.34","0.32","0.28","0.21","0.3","0.32","0.3",
                                  "0.29","0.28","0.2","0.34","0.36","0.42","0.42","0.4")

# Delete outliers with selected values
happiness_data <- happiness_data[!(happiness_data$corruption_perception_score %in% values_to_delete_corruption_perception_score), ]

#After deleting, tested outliers again in corruption_perception_score variable 
#Found below outliers which are also deleted 
values_to_delete_corruption_perception_score <- c ("0.51","0.43","0.43","0.44","0.43","0.49",
                                                   "0.49","0.52","0.52","0.52","0.46","0.46",
                                                   "0.46","0.51","0.51","0.47","0.5","0.5",
                                                   "0.43","0.47","0.44","0.43","0.44","0.43",
                                                   "0.52","0.5","0.46","0.5","0.47","0.45",
                                                   "0.5","0.46","0.47","0.49","0.49","0.43",
                                                   "0.44","0.43","0.5","0.46","0.43","0.48",
                                                   "0.5","0.51","0.44","0.52","0.5","0.44",
                                                   "0.48","0.46","0.46","0.48","0.49","0.48",
                                                   "0.47","0.49","0.52","0.46","0.43","0.47",
                                                   "0.47","0.52","0.51","0.55","0.55","0.54",
                                                   "0.53","0.54","0.53","0.55","0.55","0.54",
                                                   "0.53","0.54","0.58","0.57","0.56","0.56",
                                                   "0.57","0.58","0.58","0.57","0.58","0.57",
                                                   "0.58","0.57","0.56","0.57","0.56","0.58",
                                                   "0.57","0.58","0.57","0.56","0.56","0.57",
                                                   "0.56")

# Delete outliers with selected values
happiness_data <- happiness_data[!(happiness_data$corruption_perception_score %in% values_to_delete_corruption_perception_score), ]


#Removing outliers in positive_score
#Selecting outliers
values_to_delete_positive_score <- c ("0.32")

# Delete outliers with selected values
happiness_data <- happiness_data[!(happiness_data$positive_score %in% values_to_delete_positive_score), ]


#Removing outliers in negative_score
#Selecting outliers
values_to_delete_negative_score <- c ("0.51","0.54","0.54","0.55","0.51",
                                      "0.52","0.53","0.56","0.56","0.58",
                                      "0.57","0.59","0.53","0.51","0.5","0.5",
                                      "0.5","0.7","0.62")

# Delete outliers with selected values
happiness_data <- happiness_data[!(happiness_data$negative_score %in% values_to_delete_negative_score), ]


str(happiness_data)
# Check for normality
# Skewness function to examine normality
# install.packages("e1071")
library(e1071)
opar <- par(no.readonly = TRUE)
par(mfrow = c(4,2)) # divide graph area into 1 row x 2 cols

# skewness of < -1 or > 1 = highly skewed
# -1 to -0.5 and 0.5 to 1 = moderately skewed
# Skewness of -0.5 to 0.5 = approx symetrical
plot(density(happiness_data$life_ladder_score),
     main = "Density plot : life_ladder_score",
     ylab = "Frequency", xlab = "life_ladder_score",
     sub = paste("Skewness : ", round(e1071::skewness(happiness_data$life_ladder_score), 2)))
# fill the area under the plot
polygon(density(happiness_data$life_ladder_score), col = "red")

plot(density(happiness_data$GDP),
     main = "Density plot : GDP",
     ylab = "Frequency", xlab = "GDP",
     sub = paste("Skewness : ", round(e1071::skewness(happiness_data$GDP), 2)))
polygon(density(happiness_data$GDP), col = "red")

plot(density(happiness_data$social_support_score),
     main = "Density plot : social_support_score",
     ylab = "Frequency", xlab = "social_support_score",
     sub = paste("Skewness : ", round(e1071::skewness(happiness_data$social_support_score), 2)))
# fill the area under the plot
polygon(density(happiness_data$social_support_score), col = "red")

plot(density(happiness_data$life_expectancy),
     main = "Density plot : life_expectancy",
     ylab = "Frequency", xlab = "life_expectancy",
     sub = paste("Skewness : ", round(e1071::skewness(happiness_data$life_expectancy), 2)))
polygon(density(happiness_data$life_expectancy), col = "red")

plot(density(happiness_data$freedom_score),
     main = "Density plot : freedom_score",
     ylab = "Frequency", xlab = "freedom_score",
     sub = paste("Skewness : ", round(e1071::skewness(happiness_data$freedom_score), 2)))
# fill the area under the plot
polygon(density(happiness_data$freedom_score), col = "red")

plot(density(happiness_data$generosity),
     main = "Density plot : generosity",
     ylab = "Frequency", xlab = "generosity",
     sub = paste("Skewness : ", round(e1071::skewness(happiness_data$generosity), 2)))
# fill the area under the plot
polygon(density(happiness_data$generosity), col = "red")

plot(density(happiness_data$corruption_perception_score),
     main = "Density plot : corruption_perception_score",
     ylab = "Frequency", xlab = "corruption_perception_score",
     sub = paste("Skewness : ", round(e1071::skewness(happiness_data$corruption_perception_score), 2)))
# fill the area under the plot
polygon(density(happiness_data$corruption_perception_score), col = "red")

plot(density(happiness_data$positive_score),
     main = "Density plot : positive_score",
     ylab = "Frequency", xlab = "positive_score",
     sub = paste("Skewness : ", round(e1071::skewness(happiness_data$positive_score), 2)))
# fill the area under the plot
polygon(density(happiness_data$positive_score), col = "red")

plot(density(happiness_data$negative_score),
     main = "Density plot : negative_score",
     ylab = "Frequency", xlab = "negative_score",
     sub = paste("Skewness : ", round(e1071::skewness(happiness_data$negative_score), 2)))
# fill the area under the plot
polygon(density(happiness_data$negative_score), col = "red")

par(opar)


# Minimal skewness = -0.11 - slightly skewed to the left. 
# NB a skewness value <-1 or >1 = highly skewed. 
# Skewness -1 to -05 and 0.5 to 1 = moderately skewed. 
# And skewness -0.5 to 0-5 = approx symmetric.
paste("Skewness for life_ladder_score : ", round(e1071::skewness(happiness_data$life_ladder_score), 2))
paste("Skewness for GDP : ", round(e1071::skewness(happiness_data$GDP), 2))
paste("Skewness for social_support_score : ", round(e1071::skewness(happiness_data$social_support_score), 2))
paste("Skewness for life_expectancy : ", round(e1071::skewness(happiness_data$life_expectancy), 2))
paste("Skewness for freedom_score : ", round(e1071::skewness(happiness_data$freedom_score), 2))
paste("Skewness for generosity : ", round(e1071::skewness(happiness_data$generosity), 2))
paste("Skewness for corruption_perception_score : ", round(e1071::skewness(happiness_data$corruption_perception_score), 2))
paste("Skewness for positive_score : ", round(e1071::skewness(happiness_data$positive_score), 2))
paste("Skewness for negative_score : ", round(e1071::skewness(happiness_data$negative_score), 2))


# Check normality of variables
shapiro.test(happiness_data$life_ladder_score)
shapiro.test(happiness_data$GDP)
shapiro.test(happiness_data$social_support_score)
shapiro.test(happiness_data$life_expectancy)
shapiro.test(happiness_data$freedom_score)
shapiro.test(happiness_data$generosity)
shapiro.test(happiness_data$corruption_perception_score)
shapiro.test(happiness_data$positive_score)
shapiro.test(happiness_data$negative_score)

#All variables are not normally distributed


library(MASS)

# find optimal lambda for box-cox transform
# Plotting lambda from -6 to 6 in steps of 0.1
Box = boxcox(happiness_data$life_ladder_score ~ 1, lambda = seq(-6,6,0.1))
# Create a data frame with the results
Cox = data.frame(Box$x, Box$y)   
# Order the new data frame by decreasing y
cox_smallest_y = Cox[with(Cox, order(-Cox$Box.y)),] 
# Display the lambda with the greatest
# log likelihood
cox_smallest_y[1,]                                  
# Extract that lambda
lambda = cox_smallest_y[1, "Box.x"]                 
lambda
transformed_life_ladder_score = (happiness_data$life_ladder_score ^ lambda - 1)/lambda
hist(transformed_life_ladder_score)
shapiro.test(transformed_life_ladder_score)


#Model building
attach(happiness_data)

# Split the data into training and testing
set.seed(1)
no_rows_data <- nrow(happiness_data)
sample <- sample(1:no_rows_data, size = round(0.7 * no_rows_data), replace = FALSE)

training_data <- happiness_data[sample, ]
testing_data <- happiness_data[-sample, ]

model <- lm(life_ladder_score ~ . , data=training_data)
summary(model)

#Removing variables with low correlation - freedom_score and negative_score
updated_model <- lm(life_ladder_score ~ GDP + social_support_score + 
                      life_expectancy  + generosity + corruption_perception_score + 
                      positive_score, data=training_data)
summary(updated_model)

#QQ plot
library(car)
qqPlot(updated_model, 
       labels=row.names(training_data$name), 
       id.method="identify", 
       simulate=TRUE, 
       main="Q-Q Plot for updated model")


# A rough rule is that standardized residuals 
# that are larger than 2 or less than –2 are worth attention
studentized_fit <- rstudent(updated_model)
hist(studentized_fit, 
     breaks=10, 
     freq=FALSE, 
     xlab="Studentized Residual", 
     main="Distribution of Errors")

# Rug plot is used to show the distribution of the data
# Here it is showing the distribution
# of the x-axis data. The term "rug plot" originates
# from the effect that perpendicular markers look like tassles
# on the edge of a rug
rug(jitter(studentized_fit), col="brown")
curve(dnorm(x, mean=mean(studentized_fit), sd=sd(studentized_fit)), add=TRUE, col="blue", lwd=2)
lines(density(studentized_fit)$x, density(studentized_fit)$y, col="red", lwd=2, lty=2)
legend("topright", legend = c( "Normal Curve", "Kernel Density Curve"), lty=1:2, col=c("blue","red"), cex=.7)


#global validation of the model
install.packages("gvlma")
library(gvlma)
gvmodel <- gvlma(updated_model)
summary(gvmodel)

#prediction with testing data
predicted_life_ladder_score <- predict(updated_model, testing_data)
actual_predictions <- data.frame(cbind(actuals = testing_data$life_ladder_score, 
                                       predicted = predicted_life_ladder_score))
actual_predictions

head(actual_predictions)

cor_accuracy <- cor(actual_predictions)
cor_accuracy

#Model forcasting
#Q1
#What is the life_ladder_score
#where the GDP = 11.5 (high)
#social_support_score = 1.0 (high)
#life_expectancy = 40.1 (low)
#generosity = 0.9 (high)
#corruption_perception_score = 0.9 (high)
#positive_score = 0.7 (high)
question <- data.frame(GDP = c(11.5,),
                       social_support_score = c(1.0),
                       life_expectancy = c(40.1),
                       generosity = c(0.9),
                       corruption_perception_score = c(0.9),
                       positive_score = c(0.7))

predicted_life_ladder_score <- predict(updated_model, question)
predicted_life_ladder_score

#6.024617

#Q2
#What is the life_ladder_score
#where the GDP = 6.2 (low)
#social_support_score = 1.0 (high)
#life_expectancy = 74.5 (high)
#generosity = 0.9 (high)
#corruption_perception_score = 0.92 (high)
#positive_score = 0.7 (high)
question <- data.frame(GDP = c(6.2),
                       social_support_score = c(1.0),
                       life_expectancy = c(74.5),
                       generosity = c(0.9),
                       corruption_perception_score = c(0.92),
                       positive_score = c(0.7))

predicted_life_ladder_score <- predict(updated_model, question)
predicted_life_ladder_score
#5.55738

#Model forcasting using correlation method
#Inserting forcasting dataset
question <- data.frame(GDP = c(8.5, 9.25, 9.5, 10.75, 11.15),
                       social_support_score = c(0.5, 0.6, 0.67, 0.72, 0.73),
                       life_expectancy = c(65.3, 67.5, 69.4, 74.5, 73.5),
                       generosity = c(0.09, 0.07, 0.06, 0.03, 0.01),
                       corruption_perception_score = c(0.12, 0.32, 0.46, 0.54, 0.72),
                       positive_score = c(0.3, 0.43, 0.51, 0.63, 0.66))


#Prediction using model
predicted_life_ladder_score <- predict(updated_model, question)
predicted_life_ladder_score

#Adding the prediction variable to forcasting dataframe
question$predicted_life_ladder_score <- predicted_life_ladder_score

str(question)

# Q1) Correlation of life_ladder_score Vs GDP in original dataset and forcasting dataset
cor(happiness_data$life_ladder_score, happiness_data$GDP)
cor(question$predicted_life_ladder_score, question$GDP)
#0.7129475 and 0.9844913

# Q2) Correlation of life_ladder_score Vs social_support_score in original dataset and forcasting dataset
cor(happiness_data$life_ladder_score, happiness_data$social_support_score)
cor(question$predicted_life_ladder_score, question$social_support_score)
#0.6322641 and 0.9680127











