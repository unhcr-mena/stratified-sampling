## This script allow to generate stratified samples usung proportional allocation



#########################################################
## STEP 1: Install & load Required Library
#########################################################

### uncomment the next tow lines at first utilisation
#install.packages("simFrame")
#install.packages("sampling")

library(simFrame)
library(sampling)


#########################################################
## STEP 2: Generate the variables and the dataset
#########################################################
# PS: note that you can skip that step if you have already your dataset

#Confirm the the population size called here N
N <- 3000000

## generate randome variables for the test dataset
size <- sample(x=c(1,2,3,4,5), size=N, replace=TRUE, prob=c(.3,.4,.2,.07,.03))
return <- sample(x=c(0,1), size=N, replace=TRUE, prob=c(.7,.3))
sex <- sample(x=c(0,1), size=N, replace=TRUE, prob=c(.4,.6))
region <- sample(x=c("Egypt","Iraq","Jordan","Lebanon"), size=N, replace=TRUE, prob=c(.2,.3,.1,.4))
needs <- sample(x=c(0,1), size=N, replace=TRUE, prob=c(.45,.55))

## Bind all variable to get our test dataset
data <- data.frame(size, return, sex, region, needs)
rm(size, return, sex, region, needs)

#########################################################
## STEP 3: Insert your configuration for the sample
#########################################################



#  Decide  the confidence level
#  It represents the probability of the same result if you re-sampled, all other things equal. 
## Common values are 90%,  95% or 99%
z <- 1.95

# Decide on the margin of error - Precision is the variability of the estimate. 
## A common value is: ± 5%)
e <- 0.05

#fill the proportion of the attribute
## Estimate of the prevalence or mean & STDev of the key indicator (e.g. 30% retunr intention). 

#### Prevalence is the total number of cases for a variable of interest that is typically binary 
#### within a population divided by its total population (for instance intention to return). 

#### Mean is the expected value of a variable of interest that is typically continuous 
#### within a prescribed range for a given population (for instance expenditure per case)
p <- 0.3
q <- 1-p

#########################################################
## STEP 4: Calculate the sample size
#########################################################
#

#compute the sample size for a large population
n0 <- (z^2)*p*q/(e^2)
n0 <- round(n0, digits = 0)

#compute the sample size for a finite population
N <- nrow(data)

n <- n0/(1+((n0-1)/N))
n <- round(n, digits = 0)


#########################################################
## STEP 5: stratify the dataset
#########################################################

# We buidl the 'Strata' object
st <- stratify(data, c("size", "needs"))
#summary(st)
str(st)
max(st@nr)

#compute the sample sizes of the strata using proportional allocation: nh = Nh/N*n for each strata h
n_size <- numeric(max(st@nr))
for (h in 1:max(st@nr)){
    n_size[h] <- st@size[h]/N*n
    n_size[h] <- round(n_size[h], digits = 0)
  }
print(n_size)

#use a simple random or systematic sample to select your sample
data[order(data$size,data$needs),]
stratified_sample <- strata(data, c("size", "needs"), c(n_size), method=("srswor"), pik,description=FALSE)
summary(stratified_sample)
data_sampled <- getdata(data, stratified_sample)
#print(data_sampled)

#check if the the sample is good by checkin if the proportion of the attribute in the sample
# is close to its population's counterpart
summary(data_sampled$return)









