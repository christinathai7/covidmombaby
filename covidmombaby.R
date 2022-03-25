# install & load packages
install.packages("tidyverse")
library("tidyverse")

install.packages("VIM")
library("VIM")

install.packages("lmtest")
library("lmtest")

install.packages("ggplot2")
library("ggplot2")

install.packages("mice")
library("mice")


# load data set
mydataset <- read.csv('covidmombaby.csv',stringsAsFactors = TRUE,
                      na.strings=c("",NA))

# in this study, I only care about the CBC so
# I'll make a CBC subset from the data
mydataset_cbc <- mydataset[c(27:36)]


# how many NA's do we have?
dim(mydataset_cbc)
mydataset_cbc %>% summarise_all(funs(sum(is.na(.))))


# there's no hard cut off from the literature about what's an appropriate 
# proportion of missing data

# Schafer (1999) - analysis likely biased if >5% data missing
# Bennett (2001) - analysis likely biased if >10% data missing
# Tabachnick & Fidell (2012) - missing data mechanisms & patterns matter 
# more than the proportion of missing data

# source: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3701793/

# there are 4 missing rows in the CBC
# out of total 73 rows
(4/73) * 100 # which means approximately 5% of the CBC data is missing


# representation of missing data using VIM package
# Code from:
# https://datascienceplus.com/imputing-missing-data-with-r-mice-package/
aggr_plot <- aggr(mydataset_cbc, col=c('navyblue','red'),
                  numbers=TRUE,
                  sortVars=TRUE,
                  labels=names(mydataset_cbc),
                  cex.axis=.7,
                  gap=3,
                  ylab=c("Histogram of Missing data","Pattern"))


# checking out all the analysis methods I can run
methods(mice)

# given that H&H go together (when hgb is low, hct is also usually low)...
# and the scatter plot of H&H shows normal distribution, linearity, & 
# homoscedasticity...
attach(mydataset_cbc)
plot(hbg, hct, 
     main="Hemoglobin vs. Hematocrit", 
     xlab="Hemoglobin", ylab="Hematocrit", pch=19) 
# a linear regression model would be appropriate to use.


# same goes for WBC & plts and MCV & MCH:
# WBC & plts go together (when you have an infection, the body's acute 
# response is to make more WBCs & plts, making these values both HIGH)...

# MCH tends to mirror MCV (bigger RBCs generally contain more hgb while smaller 
# RBCs tend to have less.

# scatter plots for WBC vs. plt and MCH vs. MCV shows normal distribution, 
# linearity, & homoscedasticity... 
attach(mydataset_cbc)
plot(wbc, platelet, 
     main="White Blood Cells vs. Platelets", 
     xlab="White Blood Cells", ylab="Platelets", pch=19) 

attach(mydataset_cbc)
plot(mcv, mch, 
     main="MCV vs. MCH", 
     xlab="MCV", ylab="MCH",
     xlim = c(100,135),
     ylim = c(30,45))

# therefore, a linear regression model would be appropriate to use for these
# as well



# running the imputation
# "m" represents the # of cycles to run
imp_handh <- mice(mydataset[,c(29:30)], 
                  m = 10, 
                  maxit = 20, 
                  method = "norm.predict",
                  seed = 127493)

imp_wbcandplt <- mice(mydataset[,c(27,31)], 
                  m = 10, 
                  maxit = 20, 
                  method = "norm.predict",
                  seed = 123333)

imp_mcvandmch <- mice(mydataset[,c(32:33)], 
                      m = 10, 
                      maxit = 20, 
                      method = "norm.predict",
                      seed = 123346)


# filling in the NA's with imputed data
imp_handh <- mice::complete(imp_handh, 1)
imp_wbcandplt  <- mice::complete(imp_wbcandplt , 1)
imp_mcvandmch  <- mice::complete(imp_mcvandmch , 1)
data_fixed <- as.data.frame(cbind(imp_handh, imp_wbcandplt, imp_mcvandmch))

# Check for NA's
anyNA(data_fixed)


# filling in the rest of the NA's for the CBC using predictive mean matching
imp_restofcbc <- mice(mydataset[,c(28,34:36)], 
                  m = 10, 
                  maxit = 20, 
                  method = "pmm",
                  seed = 123456)

imp_restofcbc <- mice::complete(imp_restofcbc, 1)

data_fixed <- as.data.frame(cbind(imp_handh, imp_wbcandplt, imp_mcvandmch, 
                                  imp_restofcbc))

# Check for NA's
anyNA(data_fixed)

