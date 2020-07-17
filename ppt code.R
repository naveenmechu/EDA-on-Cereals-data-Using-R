
#Import of Cereals.csv file

library(readr)
Cereals<- read_csv("C:/Users/Administrator/Desktop/Cereals.csv")



# R Codes for knowing the data
dim(Cereals)

#Summary of Cereals
# R Codes for knowing the data
summary(Cereals)

# remove missing values from potass, carbo, sugars
Cereals_new=kNN(Cereals,variable= c( "sugars", "carbo",
                                     "potass"))
Cereals_new= Cereals_new[1:16]



library(ggplot2)
library(VIM)


#boxplot of two variable calories and rating
boxplot(rating~calories, data= Cereals_new, col= heat.colors(4))


# ggplot
ggplot(Cereals_new, aes(x = calories, y = rating)) +
  labs(title = 'rating and calories',
       y = 'rating',
       x = 'calories') + geom_point(aes(color= mfr)) +
  geom_smooth()


#Histogram
hist(Cereals$calories, col=heat.colors(4))

#heatmap
hmap= select(Cereals_new ,calories,rating, protein,	fat,	sodium,	fiber,	carbo,	sugars,	potass,	vitamins)
m=as.matrix(hmap)
heatmap(m, col=topo.colors(100))



#pairs pnel
library(psych)
aa= Cereals_new[, c(4,5,6)]
pairs.panels(aa)


# manufecturer wise avg of parameters
mfrwise_avg=Cereals_new %>% group_by(mfr) %>%
  summarise(avg_calories= mean(calories),	
            avg_protein= mean(protein), 
            avg_fat= mean(fat),	avg_sodium= mean(sodium),	
            avg_fiber= mean(fiber), avg_carbo= mean(carbo),
            avg_sugars= mean(sugars),	avg_potass= mean(potass),
            avg_vitamins= mean(vitamins),	avg_weight= mean(weight),
            avg_shelf= mode(shelf),	avg_cups= mode(cups),
            avg_rating= mean(rating))

boxplot(avg_rating~mfr, mfrwise_avg, col= heat.colors(4))
boxplot(avg_calories~mfr, mfrwise_avg, col= heat.colors(4))

#Heatmap manufacturer wise
st=mfrwise_avg
rownames(st)= st$mfr
rt= select (st, avg_calories, avg_protein, avg_fat,	avg_sodium, avg_vitamins,	avg_weight)
str(rt)
we=as.matrix(rt)
heatmap(we, col=topo.colors(100))


#scatterplot
library(car)
scatterplot(rating~calories|mfr, data= Cereals_new,
            xlab="Calories", ylab="Ratings", 
            main="manufacturer wise Rating V/S calories" )


# mfrwise_avg clustring 
library(cluster)
library(factoextra)
m=mfrwise_avg
rownames(m)=m$mfr
dn=select(m ,avg_calories,avg_rating, avg_protein,	avg_fat,	avg_sodium,	avg_fiber,	avg_carbo,	avg_sugars,	avg_potass)
set.seed(60)
dn2=scale(dn)


yt= dist(dn2, method = 'euclidean')
round(as.matrix(yt)[1:3, 1:3], 1)
fviz_dist(yt)

#plot cluster avg
z=mfrwise_avg
rownames(z)= z$mfr
t1= select(z, avg_calories,	
           avg_protein, 
           avg_fat,	avg_sodium,	
           avg_fiber, avg_carbo,
           avg_sugars,	avg_potass,
           avg_vitamins,	avg_weight,
           avg_rating)
t2 <- kmeans(t1, 3 , nstart = 1)
fviz_cluster(t2, data = t1,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
             ellipse.type = "euclid", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal()
             
)

# heat map manufacturer wise avg
st=mfrwise_avg
rownames(st)= st$mfr
rt= select (st, avg_calories,	
            avg_protein, 
            avg_fat,	avg_sodium,
            avg_vitamins,	avg_weight)
str(rt)
we=as.matrix(rt)
heatmap(we, col=topo.colors(100))

# cluster plot cereals name wise  
yy=Cereals_new
rownames(yy)= yy$name
s1= select(yy, calories, sugars,		 rating)
s2 <- kmeans(s1, 4 , nstart = 1)
fviz_cluster(s2, data = s1,
             palette = c("red", "green", "#2E9FDF", "brown", "black"),
             ellipse.type = "euclid",
             
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal()
             
)

View(word_freq1)


# Text mining triangle
library(tm)
library(RColorBrewer)
library(wordcloud2)setwd("C:/Users/Administrator/Desktop/TM190519")
readLines("Cereal.txt")
chunk_2=readLines("Cereal.txt")
chunk_2_pasted= paste(chunk_2, collapse =" ")
clean_data1=tolower(chunk_2_pasted)
clean_data2= gsub(pattern = "\\W", replace= " ", clean_data1)
clean_data1 = tolower(chunk_2_pasted)
clean_data3= gsub(pattern= "\\d", replace =" ", clean_data2)
clean_data4= removeWords(clean_data3, stopwords())
clean_data5= gsub(pattern = "\\b[A-z]\\b{1}", replace =" ", clean_data4)
clean_data6= stripWhitespace(clean_data5)
clean_data7= strsplit(clean_data6, " ")
word_freq1= table(clean_data7)
wordcloud2(word_freq1,color="random-dark",background="white",size=0.5,shape="triangle")

# clourful histogram
View(Cereals)
library(dplyr)
library("ggplot2")
Cereals %>%
  ggplot(aes(x = rating, fill = mfr)) +
  geom_histogram() +
  scale_fill_brewer(palette = "Set1") +
  scale_x_continuous(name = "rating in %", expand = c(0,0)) +
  scale_y_continuous(name = "Count", expand = c(0,0), limits = c(0, 10)) +
  labs(fill = "Manufacturer", title = "Manufacturer Vs Rating") +
  theme_minimal()
hist(Property_Price_Train$Sale_Price, col = heat.colors(4))
