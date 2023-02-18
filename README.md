rm(list=ls())    # Removes all items in Environment!
options(repr.plot.width = 6, repr.plot.height = 6,repr.plot.res = 150)

# LOAD PACKAGES:
packages <- c("plm","systemfit","modelsummary")
# Loop though vector and load respective packages:
zzz <- lapply(packages, function(xxx) suppressMessages(require(xxx, 
                                                               character.only = TRUE, quietly=TRUE, warn.conflicts = FALSE)))  # quiet no warnings
#zzz <- lapply(packages, require, character.only = TRUE) # verbose with warnings

#read data from file
my.raw.data <- read.csv("~/Desktop/Grunfeld.csv")
my.data <- pdata.frame(my.raw.data, c("FIRM", "YEAR"))  #configure the data as a panel

#Using OLS estumation
myOLS.sysfit <- systemfit(I ~ F + C, method = "OLS", data = my.data)
# summary(myOLS.sysfit)$coeff
get_estimates(myOLS.sysfit)


#using SUR estimation
mySUR.sysfit <- systemfit(I ~ F + C, method = "SUR", data = my.data)
# summary(mySUR.sysfit)$coeff
get_estimates(mySUR.sysfit)

#Calculate the covariance and correlation matrix of the OLS residuals
summary(myOLS.sysfit)$residCov
summary(myOLS.sysfit)$residCor

myOLS.pvcm <- pvcm(I ~ F + C, data = my.data, model = "within")
myOLS.pvcm

# Conduct the cross-sectional dependence/contemporaneous correlation using Breusch-Pagan LM test of Independence
pcdtest(myOLS.pvcm, test = c("lm"))

#Estimate the pooled model 
my.Pooled.model <- plm(I ~ F + C, data = my.data, model = "pooling")
get_estimates(my.Pooled.model)

#test the coefficients for homogeneity across equations
pooltest(my.Pooled.model, myOLS.pvcm)


