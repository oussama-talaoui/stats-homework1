# LUIS EDUARDO Script

#Back up DB

diamondsBU <- diamonds

#checking for missing data and its number
sum(is.na(diamonds))

# Basic statistical summary of the data set
summary(diamonds)

#ordenar
diamonds <- arrange(diamonds, x)
diamonds <- arrange(diamonds,desc(z))
diamonds <- arrange(diamonds,desc(z))

#Borrar los ceros

diamonds <- diamonds %>%
    filter (y < 30 & y >0) %>%
    filter (z < 30 & z >0) %>%
    filter (x > 0)

#Frecuencia strings
table(diamonds$cut)
table(diamonds$color)
table(diamonds$clarity)

#Matriz de correlación
cor_mat_carat <- diamonds %>%
  select(x, y, z, carat) %>%
  cor(use = "pairwise.complete.obs")

#Reorder a variable with a given list

diamonds$clarity <- factor (diamonds$clarity, levels = c ("I1" , "SI2", "SI1", "VS2", "VS1", "VVS2", "VVS1", "IF"))
diamonds$color <- factor (diamonds$color, levels = c ("J" , "I", "H", "G", "F", "E", "D"))

#Graph

#Clarity

#Se podría decir que existe una relación negativa entre las dimensiones
#y la claridad del diamante, entre más pequño es el diamante, más claro 
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

#Cut

#No hay relación aparante, los cajas casi que se ubican en el mismo lugar, se traslapan
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

#Color

#Posible relación, no es tan clara 
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

#Carat

#relación que parece seguir una función logarítmica

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


#Depth

#Gráficamente no se observa relación
#Todo indica a que se debe a que - depth total depth percentage = z / mean(x, y) 
#además que las escalas de medidas son diferentes

ggplot(data = diamonds) +
  aes(x = depth, y = x) + 
  geom_point(color = "gray", fill="black", alpha = 0.6)

ggplot(data = diamonds) +
  aes(x = depth, y = y) + 
  geom_point(color = "gray", fill="black", alpha = 0.6)

ggplot(data = diamonds) +
  aes(x = depth, y = z) + 
  geom_point(color = "gray", fill="black", alpha = 0.6)

#Matriz de correlación
cor_mat_depth <- diamonds %>%
  select(x, y, z, depth) %>%
  cor(use = "pairwise.complete.obs")

#---------------------------------
#MATEUSZ Script

diamonds <- read.csv(file="/Users/Mati/Downloads/diamonds.csv", header=TRUE, sep=",")
diamondsdf <- as.data.frame(diamonds)
diamondsdf$X <- NULL

#Now we clean the dataframe eliminating the rows that have x, y or z as a 0 value
diamondsdf <- subset(diamondsdf, diamondsdf$x !=0 & diamondsdf$y !=0 & diamondsdf$z !=0)

#Outliers are removed
diamondsdf <- diamondsdf[order(-diamondsdf$y, decreasing = F),]
diamondsdf <- diamondsdf[-c(1,2),]

diamondsdf <- diamondsdf[order(-diamondsdf$z, decreasing = F),]
diamondsdf <- diamondsdf[-c(1),]


diamondsData <- data.frame(
  #We put the clarities in this order to have the qualities in the right order
  clarity = rep(c('I1', 'SI2', 'SI1', 'VS2', 'VS1', 'VVS2', 'VVS1', 'IF'), length(unique(diamondsdf$color))),
  color = rep(sort(unique(diamondsdf$color), decreasing = TRUE), each=length(unique(diamondsdf$clarity)))
)

for(i in 1:nrow(diamondsData)){
  row <- diamondsData[i,]
  coincidences <- sum(diamondsdf$color == row$color & diamondsdf$clarity == row$clarity)
  diamondsData[i,"numberDiamonds"] <- coincidences
}

library(tidyverse)

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
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

# prepare a data frame for base lines
base_data <- diamondsData %>% 
  group_by(color) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))

# prepare a data frame for grid (scales)
grid_data <- base_data
grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1,]

# Make the plot
p <- ggplot(diamondsData, aes(x=as.factor(id), y=numberDiamonds, fill=color)) +     
  
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
  #geom_text(data=base_data, aes(x = title, y = -20, label=color), hjust=c(1,1,1,1,0,0,0), colour = "black", alpha=0.8, size=7, fontface="bold", inherit.aes = FALSE)

p