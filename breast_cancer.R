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
#shows the frequency of number of ids diagnosed with B or M
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

names(cancer)
#Multivariate analysis
#T TEST
with(data=cancer,t.test(radius_mean[diagnosis=="B"],radius_mean[diagnosis=="M"],var.equal=TRUE))
with(data=cancer,t.test(texture_mean[diagnosis=="B"],texture_mean[diagnosis=="M"],var.equal=TRUE))
with(data=cancer,t.test(perimeter_mean[diagnosis=="B"],perimeter_mean[diagnosis=="M"],var.equal=TRUE))
with(data=cancer,t.test(area_mean[diagnosis=="B"],area_mean[diagnosis=="M"],var.equal=TRUE))
with(data=cancer,t.test(smoothness_mean[diagnosis=="B"],smoothness_mean[diagnosis=="M"],var.equal=TRUE))
with(data=cancer,t.test(compactness_mean[diagnosis=="B"],compactness_mean[diagnosis=="M"],var.equal=TRUE))
with(data=cancer,t.test(concavity_mean[diagnosis=="B"],concavity_mean[diagnosis=="M"],var.equal=TRUE))
with(data=cancer,t.test(points_mean[diagnosis=="B"],points_mean[diagnosis=="M"],var.equal=TRUE))
with(data=cancer,t.test(symmetry_mean[diagnosis=="B"],symmetry_mean[diagnosis=="M"],var.equal=TRUE))
with(data=cancer,t.test(dimension_mean[diagnosis=="B"],dimension_mean[diagnosis=="M"],var.equal=TRUE))

#Hotelling T2 test
#install.packages("Hotelling")
library(Hotelling)
t2testcan <- hotelling.test(radius_mean + texture_mean + perimeter_mean + area_mean + smoothness_mean + compactness_mean + concavity_mean + points_mean + symmetry_mean + dimension_mean ~ diagnosis, data=cancer)
# Output of the function hotelling.test is given
cat("T2 statistic =",t2testcan$stat[[1]],"\n")
#print(t2testcan)
#  T2 statistic is located in the first element of the list "stat"
#View(t2testcan)



## Levene's tests based on absolute differences around means using t-tests. Standarizing the sparrows data set with scale()
matstand <- scale(cancer[,3:10])
matstand
matsurv <- matstand[cancer$diagnosis =="B",]
matsurv
matnosurv <- matstand[cancer$diagnosis == "M",]
vecmediansurv <- apply(matsurv, 2, median)
# in the above 2 represents column. Hence, we are asking for column median
vecmediansurv

vecmediannosurv <- apply(matnosurv, 2, median)
matabsdevsurv <- abs(matsurv - matrix(rep(vecmediansurv,nrow(matsurv)),nrow=nrow(matsurv), byrow=TRUE))

matabsdevnosurv <- abs(matnosurv - matrix(rep(vecmediannosurv,nrow(matnosurv)),nrow=nrow(matnosurv), byrow=TRUE))

matabsdevnosurv

matabsdev.all <- rbind(matabsdevsurv,matabsdevnosurv)
matabsdev.all <- data.frame(cancer$diagnosis, matabsdev.all)

t.test(matabsdev.all$radius_mean[cancer$diagnosis == "B"],matabsdev.all$radius_mean[cancer$diagnosis == "M"], alternative="less",var.equal = TRUE)
t.test(matabsdev.all$texture_mean[cancer$diagnosis == "B"],matabsdev.all$texture_mean[cancer$diagnosis == "M"], alternative="less",var.equal = TRUE)
t.test(matabsdev.all$perimeter_mean[cancer$diagnosis == "B"],matabsdev.all$perimeter_mean[cancer$diagnosis == "M"], alternative="less",var.equal = TRUE)
t.test(matabsdev.all$area_mean[cancer$diagnosis == "B"],matabsdev.all$area_mean[cancer$diagnosis == "M"], alternative="less",var.equal = TRUE)
t.test(matabsdev.all$smoothness_mean[cancer$diagnosis == "B"],matabsdev.all$smoothness_mean[cancer$diagnosis == "M"], alternative="less",var.equal = TRUE)
t.test(matabsdev.all$compactness_mean[cancer$diagnosis == "B"],matabsdev.all$compactness_mean[cancer$diagnosis == "M"], alternative="less",var.equal = TRUE)
t.test(matabsdev.all$concavity_mean[cancer$diagnosis == "B"],matabsdev.all$concavity_mean[cancer$diagnosis == "M"], alternative="less",var.equal = TRUE)
t.test(matabsdev.all$points_mean[cancer$diagnosis == "B"],matabsdev.all$points_mean[cancer$diagnosis == "M"], alternative="less",var.equal = TRUE)


matstand
matstand.all <- data.frame(cancer$diagnosis, matstand)
matstand.all
colnames(matstand.all) <- colnames(cancer[2:10])
t2testcan <- hotelling.test(radius_mean + texture_mean + perimeter_mean + area_mean + smoothness_mean + compactness_mean + concavity_mean + points_mean + symmetry_mean + dimension_mean ~ diagnosis, data=cancer)
cat("T2 statistic =",t2testcan$stat[[1]],"\n")
print(t2testcan)

# In the above we standardized using scale function
matabsdev.all

#install.packages("car")
library(car)
#leveneTest() produces a two-sided test
# Leverne test is used to verify Homoscedasticity. It tests if the variance of two samples are # #equal. Levene's test is an inferential statistic used to assess the equality of variances for a #variable calculated for two or more groups.[1] Some common statistical procedures assume that #variances of the populations from which different samples are drawn are equal. Levene's test #assesses this assumption.
leveneTest(radius_mean ~ diagnosis, data=cancer)
leveneTest(texture_mean ~ diagnosis, data=cancer)
leveneTest(perimeter_mean ~ diagnosis, data=cancer)
leveneTest(area_mean ~ diagnosis, data=cancer)
leveneTest(smoothness_mean ~ diagnosis, data=cancer)
leveneTest(compactness_mean~ diagnosis, data=cancer)
leveneTest(concavity_mean~ diagnosis, data=cancer)
leveneTest(points_mean ~ diagnosis, data=cancer)
leveneTest(symmetry_mean ~ diagnosis, data=cancer)
leveneTest(dimension_mean ~ diagnosis, data=cancer)
