#Roshni Suhanda : 188009850 : MITA
#Multivariate Analysis
#Part 2 of Project : Breast Cancer Analysis

#Loading and Reading the data here
wisc_bc_df <- read.csv("C:/Users/roshn/Desktop/RBS/mva/wisc_bc_data.csv")
wisc_bc_df

#Here we use View func to get the excel style view of the data
#It is convenient to read the data
View(wisc_bc_df)

#Here we are renaming the data set for our ease
cancer<-wisc_bc_df

#After installing the following 3 packages, we use library() to execute it
#We use ggplot2 for the graphs. It lets us add details to the graphs such as aes value and color etc
#corrplot is used for correlatiional matrix
#reshape is for change how the data looks. eg : you can change the matrix columns of your matrix
library("ggplot2")
library("corrplot")
library("reshape")

#str() tells us the data type of data in each column
str(cancer)

#displays the summary of the dataset
summary(cancer)

#Gives you frequency table
#creating a data frame for categorizing benign & malignant
diagnosis.table <- table(cancer$diagnosis)
diagnosis.table


#Bar Plot
#Plotting the frequency of the 2 categories in a bar plot
ggplot(data=cancer, aes(x=diagnosis)) + geom_bar(stat = "count") 

#Pie chart represented in frequency in terms of %
diagnosis.prop.table <- prop.table(diagnosis.table)*100
diagnosis.prop.df <- as.data.frame(diagnosis.prop.table)

#Here we labeling the 2 categories of the pie chart
pielabels <- sprintf("%s - %3.1f%s", diagnosis.prop.df[,1], diagnosis.prop.table, "%")

#using colors to show distinction
colors <- terrain.colors(2)
pie(diagnosis.prop.table,
    labels=pielabels,  
    clockwise=TRUE,
    col=colors,
    border="gainsboro",
    radius=0.8,
    cex=0.8, 
    main="frequency of cancer diagnosis")
legend(1, .4, legend=diagnosis.prop.df[,1], cex = 0.7, fill = colors)

#Plot histograms of "mean" variables group by diagnosis
data_mean <- cancer[ ,c("diagnosis", "radius_mean", "texture_mean","perimeter_mean", "area_mean", "smoothness_mean", "compactness_mean", "concavity_mean", "symmetry_mean" )]

#Plot histograms for comparison
ggplot(data = melt(data_mean, id.var = "diagnosis"), mapping = aes(x = value)) + 
  geom_histogram(bins = 10, aes(fill=diagnosis), alpha=0.5) + facet_wrap(~variable, scales ='free_x')

#Scatter plot of two varaible (concavity against radius)
a <-cancer[,c('concavity_worst','radius_worst')]
plot(x = cancer$concavity_worst,y = cancer$radius_worst,
     xlab = "concavity_worst",
     ylab = "radius_worst",
     main = "Concavity_worst vs radius_worst",
     pch=15,col=c("blue","yellow"))
rug(cancer$concavity_worst, side = 1)

rug(cancer$radius_worst, side = 2)

#Correlation Matrix of columns
corMatMy <- cor(cancer[,3:32])
corrplot(corMatMy, order = "hclust", tl.cex = 0.7)

#Scatterplot Matrix
pairs(~radius_mean+perimeter_mean+area_mean+compactness_mean+concavity_mean,data = cancer,main = "Scatterplot Matrix")

