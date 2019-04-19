# breast_cancer

1.R code
2.Html file
3.Questions in Read.md (which is as below)
4.Data Dictionary

Part A] We have performed 3 tests on our data set and have uploaded the updated R code i.e breast_cancer.R
Part B] Questions:
1.How are the various measurements related?
2.Are there statistically significant differences between Benign and Malignant patients for the mean values of the variables?
3. Do the Benign and Malignant show similar amounts of variation for the variables?


As a part of Assignment H.W 2:
I have uploaded 3 files:
1. updated R script
2. pdf document of analysis
3. html file


Using the following code to read the data set.


wisc_bc_df <- read.csv("C:/Users/roshn/Desktop/RBS/mav/wisc_bc_df.csv")
wisc_bc_df

Hello,
Our data set about breast cancer.
Our Problem Statement is as follows :


Breast cancer starts when cells in the breast begin to grow out of control. These cells usually form a tumor that can often be seen on an x-ray or felt as a lump. The tumor is malignant (cancer) if the cells can grow into surrounding tissues or spread to distant areas of the body.

The main aim of our project is to analyse the breast cancer data so as to provide preventive measures that can be taken by patients before the condition becomes too serious.
We also wish to analyse at which stage is the cancer by analysing the causing factors.

For the analysis we will use KNN (K nearest neighbour) method.
We use this method, as it is most preferred for classification for analysis for breast cancer and gives maximum accuracy.

Questions that can be asked :


1. Which is more common ? benign or malignant ?
2. Classify that for a given case (tumor) whether Benign or Malignant. (This we will achieve using KNN Model)
