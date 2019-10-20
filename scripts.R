# The R language script for Homework 1.1, created using RStudio.

   # Submitted by:

#| Name                       | Student ID       |
#|----------------------------|------------------|
#| ANGULO MONTES LUIS EDUARDO | AM628320         |
#| KLIMAS MATEUSZ             | X3179625J        |
#| RUIZ-TAGLE ORIOL JUAN LUIS | 53620839L        |
#| TAHIRI ALAOUI OUSSAMA      | U183316          |
#|-----------------------------------------------|

# Installing and loading needed libraries
install.packages("dplyr")
install.packages("GGally")
install.packages("rstudioapi")
install.packages("tidyverse")

library(dplyr)
library(GGally)
library(rstudioapi)
library(tidyverse)

## Dataset preperation ###############################################
# Set working directory and load the diamonds data using a relative path
# (this works only after setting the working directory,
# most easily with the RStudio UI)

# the following line is for getting the path of your current open file
current_path <- getActiveDocumentContext()$path 
# The next line set the working directory to the relevant one:
setwd(dirname(current_path ))
# you can make sure you are in the right directory
print( getwd() )

diamonds <- read.csv("diamonds.csv", stringsAsFactors = FALSE)

# Checking for missing data and its number
sum(is.na(diamonds))

# Basic statistical summary of the data set
summary(diamonds)

# Boxplot to visualize the outliers
boxplot(diamonds$x, diamonds$y, diamonds$z, names=c("x","y","z"),xlab="coordinates", ylab="size",main="Outliers of x, y and z")

# Now we clean the dataframe eliminating the rows that have x, y or z as a 0 value
diamonds <- subset(diamonds, diamonds$x !=0 & diamonds$y !=0 & diamonds$z !=0)

# Now lets have a quick overview about the data that we have in the x, y and z coordinates
# checking for null values and outliers

summary(diamonds[c("x","y","z")])

# Now we order the dataframe first by y and then by z to check if the outliers are actually wrong data or not,
# and then we eliminate the considered ones

diamonds <- diamonds[order(-diamonds$y, decreasing = F),]
# Deleting the first and second outlier
diamonds <- diamonds[-c(1,2),]

diamonds <- diamonds[order(-diamonds$z, decreasing = F),]
# Deleting the first outlier
diamonds <- diamonds[-c(1),]

##  2.3 Question ###############################################
# Which is the relation between each characteristic and the dimensions?

# Classifying
diamonds <- arrange(diamonds, x)
diamonds <- arrange(diamonds,desc(z))
diamonds <- arrange(diamonds,desc(z))

# Removing the null values
diamonds <- diamonds %>%
    filter (y < 30 & y >0) %>%
    filter (z < 30 & z >0) %>%
    filter (x > 0)

# Strings frequency
table(diamonds$cut)
table(diamonds$color)
table(diamonds$clarity)

# Correlation matrix
cor_mat_carat <- diamonds %>%
  select(x, y, z, carat) %>%
  cor(use = "pairwise.complete.obs")

#Reorder the variables in a given list

diamonds$clarity <- factor (diamonds$clarity, levels = c ("I1" , "SI2", "SI1", "VS2", "VS1", "VVS2", "VVS1", "IF"))
diamonds$color <- factor (diamonds$color, levels = c ("J" , "I", "H", "G", "F", "E", "D"))

## Plotting

# Diamonds Clarity

# You could say that there is a negative relationship between the dimensions
# and the clarity of the diamond, the smaller the diamond is, the clearer
ggplot(data = diamonds) +
  aes(x = clarity, y = y) +
  geom_boxplot(color = "darkgray", fill = "blue", alpha = 0.7, outlier.color = "darksalmon") +
  labs (title = "Relation between clarity and the width of the diamond", y = "Width of the diamond (y)", x = "Clarity - measurement of how clear the diamond is (I1 (worst), ... , IF (best))") 

ggplot(data = diamonds) +
  aes(x = clarity, y = x) +
  geom_boxplot(color = "darkgray", fill = "gold1", alpha = 0.7, outlier.color = "darksalmon") +
  labs (title = "Relation between clarity and the length of the diamond", y = "Length of the diamond (x)", x = "Clarity - measurement of how clear the diamond is (I1 (worst), ... , IF (best))") 

ggplot(data = diamonds) +
  aes(x = clarity, y = z) +
  geom_boxplot(color = "darkgray", fill = "darkseagreen", alpha = 0.7, outlier.color = "darksalmon") +
  labs (title = "Relation between clarity and the depth of the diamond", y = "Depth of the diamond (z)", x = "Clarity - measurement of how clear the diamond is (I1 (worst), ... , IF (best))") 

# Diamonds Cut

# There is no apparent relationship, the boxes almost at the same level, overlap
ggplot(data = diamonds) +
  aes(x = cut, y = y) +
  geom_boxplot(color = "darkgray", fill = "gray28", alpha = 0.7, outlier.color = "darksalmon") +
  labs (title = "Relation between cut and the width of the diamond", y = "Width of the diamond (y)", x = "Cut - quality of the cut") 

ggplot(data = diamonds) +
  aes(x = cut, y = x) +
  geom_boxplot(color = "darkgray", fill = "dodgerblue4", alpha = 0.7, outlier.color = "darksalmon") +
  labs (title = "Relation between cut and the length of the diamond", y = "Length of the diamond (x)", x = "Cut - quality of the cut") 

ggplot(data = diamonds) +
  aes(x = cut, y = z) +
  geom_boxplot(color = "darkgray", fill = "darkslateblue", alpha = 0.7, outlier.color = "darksalmon") +
  labs (title = "Relation between cut and depth of the diamond", y = "Depth of the diamond (z)", x = "Cut - quality of the cut") 

# Diamonds Color

# Possible relationship, but it is not so clear 
ggplot(data = diamonds) +
  aes(x = color, y = y) +
  geom_boxplot(color = "darkgray", fill = "lightgoldenrod4", alpha = 0.7, outlier.color = "darksalmon") +
  labs (title = "Relation between color and the width of the diamond", y = "Width of the diamond (y)", x = "Color - diamond colour, from J (worst) to D (best)") 

ggplot(data = diamonds) +
  aes(x = color, y = x) +
  geom_boxplot(color = "darkgray", fill = "paleturquoise1", alpha = 0.7, outlier.color = "darksalmon") +
  labs (title = "Relation between color and the length of the diamond", y = "Length of the diamond (x)", x = "Color - diamond colour, from J (worst) to D (best)") 

ggplot(data = diamonds) +
  aes(x = color, y = z) +
  geom_boxplot(color = "darkgray", fill = "darkslateblue", alpha = 0.7, outlier.color = "darksalmon") +
  labs (title = "Relation between color and depth of the diamond", y = "Depth of the diamond (z)", x = "Color - diamond colour, from J (worst) to D (best)") 

# Diamonds Carat

# Relationship that seems to follow a logarithmic function
summary (diamonds$carat)

ggplot(data = diamonds) +
  aes(x = carat, y = y) + 
  geom_point(color = "darkblue", fill="black", alpha = 0.2)+
  labs (title = "Relation between carat and the width of the diamond", y = "Width of the diamond (y)", x = "Weight of the diamond") 

ggplot(data = diamonds) +
  aes(x = carat, y = x) + 
  geom_point(color = "burlywood4", fill="black", alpha = 0.2)+
  labs (title = "Relation between carat and the length of the diamond", y = "Length of the diamond (x)", x = "Weight of the diamond") 

ggplot(data = diamonds) +
  aes(x = carat, y = z) + 
  geom_point(color = "yellow", fill="black", alpha = 0.6)+
  labs (title = "Relation between carat and the depth of the diamond", y = "Depth of the diamond (z)", x = "Weight of the diamond") 

# Diamonds Depth

# Graphically no relationship is observed
# Everything indicates why it is because - depth total depth percentage = z / mean (x, y)
# also that the scales of measurements are different
ggplot(data = diamonds) +
  aes(x = depth, y = x) + 
  geom_point(color = "gray", fill="black", alpha = 0.6)

ggplot(data = diamonds) +
  aes(x = depth, y = y) + 
  geom_point(color = "gray", fill="black", alpha = 0.6)

ggplot(data = diamonds) +
  aes(x = depth, y = z) + 
  geom_point(color = "gray", fill="black", alpha = 0.6)

##  2.4 Question ###############################################
# Is it sufficient to know the dimensions (x,y,z) of the diamond to determine its price?
# With which error rate?

clean <- subset(diamonds, diamonds$x != 0 & diamonds$y != 0 & diamonds$z != 0)
clean <- subset(clean, clean$y <= 11.0 & clean$z <= 11.0)

head(clean[order(clean$z, decreasing = TRUE),],10)

clean <- clean[order(clean$price),]

# Generating the model
model <- lm(formula = log1p(price) ~ x + y + z,data = clean)
summary(model)
predictions <- predict(model,select(clean,x,y,z))

# Plotting predictions
plot(exp(predictions),type = "p",col="red",
        # log="y",
        main="Diamond price prediction",
        xlab="Diamonds in the dataset",
        ylab="Price in USD")

lines(clean$price, col = "green")
legend(x = "topleft",legend=c("Predictions", "Real prices"),
       col=c("red", "green"), lty=1:2, box.lty=0)

error <- abs((exp(predictions) - clean$price)/clean$price)

mean(error)
var(error)

# Plotting the error
plot(100*error[0:length(error)],col = "orange",
        main="Mean absolute percentage error",
        xlab="Diamonds in the dataset",
        ylab="% error")

## 2.5 Question ###############################################
# What is the distribution of the diamonds depending on the color and clarity?

diamondsdf <- as.data.frame(diamonds)
diamondsdf$X <- NULL

# Now we clean the dataframe eliminating the rows that have x, y or z as a 0 value
diamondsdf <- subset(diamondsdf, diamondsdf$x !=0 & diamondsdf$y !=0 & diamondsdf$z !=0)

# Outliers are removed
diamondsdf <- diamondsdf[order(-diamondsdf$y, decreasing = F),]
diamondsdf <- diamondsdf[-c(1,2),]

diamondsdf <- diamondsdf[order(-diamondsdf$z, decreasing = F),]
diamondsdf <- diamondsdf[-c(1),]

diamondsData <- data.frame(
  # We put the clarities in this order to have the qualities in the right order
  clarity = rep(c('I1', 'SI2', 'SI1', 'VS2', 'VS1', 'VVS2', 'VVS1', 'IF'), length(unique(diamondsdf$color))),
  color = rep(sort(unique(diamondsdf$color), decreasing = TRUE), each=length(unique(diamondsdf$clarity)))
)

for(i in 1:nrow(diamondsData)){
  row <- diamondsData[i,]
  coincidences <- sum(diamondsdf$color == row$color & diamondsdf$clarity == row$clarity)
  diamondsData[i,"numberDiamonds"] <- coincidences
}

# Set a number of 'empty bar' to add at the end of each group
empty_bar <- 3
to_add <- data.frame( matrix(NA, empty_bar*nlevels(diamondsData$color), ncol(diamondsData)) )
colnames(to_add) <- colnames(diamondsData)
to_add$color <- rep(levels(diamondsData$color), each=empty_bar)
diamondsData <- rbind(diamondsData, to_add)
diamondsData <- diamondsData %>% arrange(color)
diamondsData$id <- seq(1, nrow(diamondsData))

# Get the name and the y position of each label
label_data <- diamondsData
number_of_bar <- nrow(label_data)

# We substract 0.5 here because the letter must have the angle of the center of the bars.
# Not extreme right(1) or extreme left(0)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

# Preparing a data frame for base lines
base_data <- diamondsData %>% 
  group_by(color) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))

# Preparing a data frame for grid (scales)
grid_data <- base_data
grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1,]

# Creating the plots
ggplot(diamondsData, aes(x=as.factor(id), y=numberDiamonds, fill=color)) +     
  
  geom_bar(aes(x=as.factor(id), y=numberDiamonds, fill=color), stat="identity", alpha=0.5) +
  
  geom_segment(data=grid_data, aes(x = end, y = 2500, xend = start, yend = 2500), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 2000, xend = start, yend = 2000), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 1500, xend = start, yend = 1500), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 1000, xend = start, yend = 1000), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 500, xend = start, yend = 500), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  
  annotate("text", x = rep(max(diamondsData$id),5), y = c(500, 1000, 1500, 2000, 2500), label = c("500", "1000", "1500", "2000", "2500") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
  
  geom_bar(aes(x=as.factor(id), y=numberDiamonds, fill=color), stat="identity", alpha=0.5) +
  ylim(-100,2500) +
  theme_minimal() +
  theme(
    legend.position = "left",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar() + 
  geom_text(data=label_data, aes(x=id, y=2500, label=clarity, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2, angle= label_data$angle, inherit.aes = FALSE ) +
  
  geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )
