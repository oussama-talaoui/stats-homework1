#Diamonds

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

#Matriz de correlaci�n
cor_mat_carat <- diamonds %>%
  select(x, y, z, carat) %>%
  cor(use = "pairwise.complete.obs")

#Reorder a variable with a given list

diamonds$clarity <- factor (diamonds$clarity, levels = c ("I1" , "SI2", "SI1", "VS2", "VS1", "VVS2", "VVS1", "IF"))
diamonds$color <- factor (diamonds$color, levels = c ("J" , "I", "H", "G", "F", "E", "D"))

#Graph

#Clarity

#Se podr�a decir que existe una relaci�n negativa entre las dimensiones
#y la claridad del diamante, entre m�s pequ�o es el diamante, m�s claro 
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

#No hay relaci�n aparante, los cajas casi que se ubican en el mismo lugar, se traslapan
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

#Posible relaci�n, no es tan clara 
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

#relaci�n que parece seguir una funci�n logar�tmica

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

#Gr�ficamente no se observa relaci�n
#Todo indica a que se debe a que - depth total depth percentage = z / mean(x, y) 
#adem�s que las escalas de medidas son diferentes

ggplot(data = diamonds) +
  aes(x = depth, y = x) + 
  geom_point(color = "gray", fill="black", alpha = 0.6)

ggplot(data = diamonds) +
  aes(x = depth, y = y) + 
  geom_point(color = "gray", fill="black", alpha = 0.6)

ggplot(data = diamonds) +
  aes(x = depth, y = z) + 
  geom_point(color = "gray", fill="black", alpha = 0.6)

#Matriz de correlaci�n
cor_mat_depth <- diamonds %>%
  select(x, y, z, depth) %>%
  cor(use = "pairwise.complete.obs")

