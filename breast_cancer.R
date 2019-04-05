#Roshni Suhanda : 188009850 : MITA
#Multivariate Analysis
#Part 2 of Project : Breast Cancer Analysis

#Loading and Reading the data here
wisc_bc_df <- read.csv("C:/Users/roshn/Desktop/RBS/mva/wisc_bc_data.csv")
wisc_bc_df

#Here we use View func to get the excel style view of the data
#It is convenient to read the data
#View(wisc_bc_df)

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

#PCA

dim(cancer)
attach(cancer)
head(cancer)
#Get the Correlations between the measurements
cor(cancer[-2])
c <- (cor(cancer[-2]))
plot(c)
# Using prcomp to compute the principal components (eigenvalues and eigenvectors). With scale=TRUE, variable means are set to zero, and variances set to one
cancer_pca <- prcomp(cancer[,-2],scale=TRUE)
cancer_pca
plot(cancer_pca)
summary(cancer_pca)
#View(cancer_pca)
head(cancer_pca$x)
# sample scores stored in cancer_pca$x
# singular values (square roots of eigenvalues) stored in cancer_pca$sdev
# loadings (eigenvectors) are stored in cancer_pca$rotation
# variable means stored in cancer_pca$center
# variable standard deviations stored in sparrows_pca$scale
# A table containing eigenvalues and %'s accounted, follows
# Eigenvalues are sdev^2
(eigen_cancer <- cancer_pca$sdev^2) ## brackets for print
names(eigen_cancer) <- paste("PC",1:31,sep="")
eigen_cancer
sumlambdas <- sum(eigen_cancer)
sumlambdas
propvar <- eigen_cancer/sumlambdas
propvar
summary(eigen_cancer)
summary(cancer_pca)
cumvar_cancer <- cumsum(propvar)
cumvar_cancer
matlambdas <- rbind(eigen_cancer,propvar,cumvar_cancer)
rownames(matlambdas) <- c("Eigenvalues","Prop. variance","Cum. prop. variance")
round(matlambdas,4)
summary(cancer_pca)
cancer_pca$rotation
print(cancer_pca)
# Sample scores stored in cancer_pca$x
head(cancer_pca$x)
# Identifying the scores by their diagnosis
diag_pca <- cbind(data.frame(diagnosis),cancer_pca$x)
head(diag_pca)
# Means of scores for all the PC's classified by diagnosis status
tabmeansPC <- aggregate(diag_pca[,2:31],by=list(diagnosis=cancer$diagnosis),mean)
tabmeansPC
tabmeansPC <- tabmeansPC[rev(order(tabmeansPC$diagnosis)),]
tabmeansPC
tabfmeans <- t(tabmeansPC[,-1])
tabfmeans
colnames(tabfmeans) <- t(as.vector(tabmeansPC[1]))
tabfmeans
# Standard deviations of scores for all the PC's classified by diagnosis status
tabsdsPC <- aggregate(diag_pca[,2:31],by=list(cancer$diagnosis),sd)
tabfsds <- t(tabsdsPC[,-1])
colnames(tabfsds) <- t(as.vector(tabsdsPC[1]))
tabfsds
t.test(PC1~cancer$diagnosis,data=diag_pca)
t.test(PC2~cancer$diagnosis,data=diag_pca)
t.test(PC3~cancer$diagnosis,data=diag_pca)
t.test(PC4~cancer$diagnosis,data=diag_pca)
t.test(PC5~cancer$diagnosis,data=diag_pca)
t.test(PC6~cancer$diagnosis,data=diag_pca)
t.test(PC7~cancer$diagnosis,data=diag_pca)
t.test(PC8~cancer$diagnosis,data=diag_pca)
t.test(PC9~cancer$diagnosis,data=diag_pca)
t.test(PC10~cancer$diagnosis,data=diag_pca)
t.test(PC11~cancer$diagnosis,data=diag_pca)
t.test(PC12~cancer$diagnosis,data=diag_pca)
t.test(PC13~cancer$diagnosis,data=diag_pca)
t.test(PC14~cancer$diagnosis,data=diag_pca)
t.test(PC15~cancer$diagnosis,data=diag_pca)
t.test(PC16~cancer$diagnosis,data=diag_pca)
t.test(PC17~cancer$diagnosis,data=diag_pca)
t.test(PC18~cancer$diagnosis,data=diag_pca)
t.test(PC19~cancer$diagnosis,data=diag_pca)
t.test(PC20~cancer$diagnosis,data=diag_pca)
t.test(PC21~cancer$diagnosis,data=diag_pca)
t.test(PC22~cancer$diagnosis,data=diag_pca)
t.test(PC23~cancer$diagnosis,data=diag_pca)
t.test(PC24~cancer$diagnosis,data=diag_pca)
t.test(PC25~cancer$diagnosis,data=diag_pca)
t.test(PC26~cancer$diagnosis,data=diag_pca)
t.test(PC27~cancer$diagnosis,data=diag_pca)
t.test(PC28~cancer$diagnosis,data=diag_pca)
t.test(PC29~cancer$diagnosis,data=diag_pca)
t.test(PC30~cancer$diagnosis,data=diag_pca)
t.test(PC31~cancer$diagnosis,data=diag_pca)

# F ratio tests
var.test(PC1~cancer$diagnosis,data=diag_pca)
var.test(PC2~cancer$diagnosis,data=diag_pca)
var.test(PC3~cancer$diagnosis,data=diag_pca)
var.test(PC4~cancer$diagnosis,data=diag_pca)
var.test(PC5~cancer$diagnosis,data=diag_pca)
var.test(PC6~cancer$diagnosis,data=diag_pca)
var.test(PC7~cancer$diagnosis,data=diag_pca)
var.test(PC8~cancer$diagnosis,data=diag_pca)
var.test(PC9~cancer$diagnosis,data=diag_pca)
var.test(PC10~cancer$diagnosis,data=diag_pca)
var.test(PC11~cancer$diagnosis,data=diag_pca)
var.test(PC12~cancer$diagnosis,data=diag_pca)
var.test(PC13~cancer$diagnosis,data=diag_pca)
var.test(PC14~cancer$diagnosis,data=diag_pca)
var.test(PC15~cancer$diagnosis,data=diag_pca)
var.test(PC16~cancer$diagnosis,data=diag_pca)
var.test(PC17~cancer$diagnosis,data=diag_pca)
var.test(PC18~cancer$diagnosis,data=diag_pca)
var.test(PC19~cancer$diagnosis,data=diag_pca)
var.test(PC20~cancer$diagnosis,data=diag_pca)
var.test(PC21~cancer$diagnosis,data=diag_pca)
var.test(PC22~cancer$diagnosis,data=diag_pca)
var.test(PC23~cancer$diagnosis,data=diag_pca)
var.test(PC24~cancer$diagnosis,data=diag_pca)
var.test(PC25~cancer$diagnosis,data=diag_pca)
var.test(PC26~cancer$diagnosis,data=diag_pca)
var.test(PC27~cancer$diagnosis,data=diag_pca)
var.test(PC28~cancer$diagnosis,data=diag_pca)
var.test(PC29~cancer$diagnosis,data=diag_pca)
var.test(PC30~cancer$diagnosis,data=diag_pca)
var.test(PC31~cancer$diagnosis,data=diag_pca)
# Levene's tests (one-sided)
library(car)
(LTPC1 <- leveneTest(PC1~cancer$diagnosis,data=diag_pca))
(p_PC1_1sided <- LTPC1[[3]][1]/2)
(LTPC2 <- leveneTest(PC2~cancer$diagnosis,data=diag_pca))
(p_PC2_1sided=LTPC2[[3]][1]/2)
(LTPC3 <- leveneTest(PC3~cancer$diagnosis,data=diag_pca))
(p_PC3_1sided <- LTPC3[[3]][1]/2)
(LTPC4 <- leveneTest(PC4~cancer$diagnosis,data=diag_pca))
(p_PC4_1sided <- LTPC4[[3]][1]/2)
(LTPC5 <- leveneTest(PC5~cancer$diagnosis,data=diag_pca))
(p_PC5_1sided <- LTPC5[[3]][1]/2)
# Plotting the scores for the first and second components
plot(diag_pca$PC1, diag_pca$PC2,pch=ifelse(diag_pca$diagnosis == "S",1,16),xlab="PC1", ylab="PC2", main="569 entries against values for PC1 & PC2")
abline(h=0)
abline(v=0)
legend("bottomleft", legend=c("Benign","Malignant"), pch=c(1,16))
plot(eigen_cancer, xlab = "Component number", ylab = "Component variance", type = "l", main = "Scree diagram")
plot(log(eigen_cancer), xlab = "Component number",ylab = "log(Component variance)", type="l",main = "Log(eigenvalue) diagram")
print(summary(cancer_pca))
#View(cancer_pca)
diag(cov(cancer_pca$x))
xlim <- range(cancer_pca$x[,1])
head(cancer_pca$x[,1])
head(cancer_pca$x)
plot(cancer_pca$x,xlim=xlim,ylim=xlim)

#Factor Analysis
library(psych)
#install.packages("psych", lib="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
library(psych)
fit.pc <- principal(cancer[-2], nfactors=4, rotate="varimax")
fit.pc
round(fit.pc$values, 3)
fit.pc$loadings
# Loadings with more digits
for (i in c(1,3,2,4)) { print(fit.pc$loadings[[1,i]])}
# Communalities
fit.pc$communality
# Rotated factor scores, Notice the columns ordering: RC1, RC3, RC2 and RC4
head(fit.pc$scores)
# Play with FA utilities

fa.parallel(cancer[-2]) # See factor recommendation
fa.plot(fit.pc) # See Correlations within Factors
fa.diagram(fit.pc) # Visualize the relationship

