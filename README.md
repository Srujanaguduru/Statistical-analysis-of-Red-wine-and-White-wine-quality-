# Statistical-analysis-of-Red-wine-and-White-wine-quality-
Statistical analysis of Red wine and White wine quality 

Refer the attached pdf for the complete statistical analysis and below is the R code of the same.

library(car)\
allredwine <- read.csv("winequality-red.csv",header=TRUE,sep=";")\
allwhitewine <- read.csv("winequality-white.csv",header=TRUE,sep=";")\
cor(allwhitewine)\
redwine<-allredwine[,c('alcohol','quality')] \
whitewine<-allwhitewine[,c('alcohol','quality')]

## ECDF of red wine along with 95% confidence interval 

red_quality.ecdf <-ecdf(redwine$quality)\
plot(red_quality.ecdf,main='ECDF - Red wine quality')\
Alpha=0.05\
n1=length(redwine$quality)\
Eps=sqrt(log(2/Alpha)/(2*n1))\
grid<-seq(0,9, length.out = 1000)\
lines(grid, pmin(red_quality.ecdf(grid)+Eps,1))\
lines(grid, pmax(red_quality.ecdf(grid)-Eps,0))\
##ECDF of white wine along with 95% confidence interval\ 
white_quality.ecdf <-ecdf(whitewine$quality)\
plot(white_quality.ecdf,main='ECDF - White wine quality')\
Alpha=0.05\
n2=length(whitewine$quality)\
Eps=sqrt(log(2/Alpha)/(2*n2))\
grid<-seq(0,10, length.out = 1000)\
lines(grid, pmin(white_quality.ecdf(grid)+Eps,1))\
lines(grid, pmax(white_quality.ecdf(grid)-Eps,0))

## Probability of poor quality 
red_quality.ecdf(5)\
white_quality.ecdf(5)

## Bootstrap estimation for difference of means:
x1_bar <- mean(redwine$quality)\
x2_bar <- mean(whitewine$quality)\
mean_diff <- x1_bar-x2_bar 
## Non-parametric bootstrap
library(bootstrap)\
cat("bootstrapping the difference of means of redwine and white wine:\n")\
cat("bootstrapping is done independently for the two groups\n")\
red.boot <- bootstrap(redwine$quality, 10000, mean)\
white.boot <- bootstrap(whitewine$quality, 10000, mean)\
boot.diff <- red.boot$thetastar - white.boot$thetastar\
abline(v=0, col="red2")\
se.boot<- sd(boot.diff)

## Confidence intervals
normal.ci<-c(mean_diff-2*se.boot, mean_diff+2*se.boot)*i \
quantile.ci<-quantile(boot.diff,c(0.025, 0.975))\
pivatol.ci<-c(2*mean_diff-quantile(boot.diff,0.975), 2*mean_diff-quantile(boot.diff,0.025))
##Hypothesis testing:
#Test statistic:
sigma.hat<-sqrt((sd(redwine$quality)^2/n1)+(sd(whitewine$quality)^2/n2))\
z<- mean_diff/(sigma.hat)\
p.value=2*(1-pnorm(abs(z)))\
x<-(1-pnorm(10.14936))

## Bootstrap on Regression model
library(simpleboot)\
install.packages('simpleboot')\
linearmodel <- lm(redwine$quality~redwine$alcohol)\
summary(linearmodel)\
R=1000\
modelboot <- lm.boot(linearmodel, R, rows = TRUE, new.xpts = NULL, ngrid = 100,
        weights = NULL)\
summary(modelboot)\
modelboot11 <- lm.boot(linearmodel, R, rows = FALSE, new.xpts = NULL, ngrid = 100,
                     weights = NULL)\
summary(modelboot11)\
plot(redwine$alcohol,redwine$alcohol)
##Permutation test to compare the means:
label<-'red'\
redwine_m <- cbind(redwine,label)\
label<-'white'\
whitewine_m <- cbind(whitewine,label)\
wine_m <- rbind(redwine_m,whitewine_m)

## diff test
diff.test <- function(group, quality) {
        resampled.group <- sample(group)
        mean(quality[resampled.group == "red"]) - 
                mean(quality[resampled.group == "white"])}\
perm.means <- replicate(1000, diff.test(wine_m$label, wine_m$quality))\
hist(perm.means)\
pvalue <- mean(abs(perm.means) > abs(diff.means))

## Bayesian analysis:
#posterior of mu1
Ib.1=1\
Ix.1 = n1/var(redwine$quality)\
post1.mean=((mean(redwine$quality))*Ix.1)/(Ib.1+Ix.1)\
post1.var = 1/(Ib.1+Ix.1)\
posterior.1 = rnorm(100,post1.mean,sqrt(post1.var))
#posterior of mu2
Ib.2=1\
Ix.2 = n2/var(whitewine$quality)\
post2.mean=((mean(whitewine$quality))*Ix.2)/(Ib.2+Ix.2)\
post2.var = 1/(Ib.2+Ix.2)\
posterior.2 = rnorm(100,post2.mean,sqrt(post2.var))\
#posterior of mu1-mu2\
post3.mean = post1.mean-post2.mean\
post3.var = post1.var+post2.var\
posterior.1.2 = rnorm(100,post3.mean,sqrt(post3.var))\
hist(posterior.1.2,main="Posterior distribution of mu1-mu2")\
mean(posterior.1.2)\

First, we estimated the distribution of quality by using empirical cumulative distribution function. Then, from the bootstrap method and permutation method, we found that there is a difference in the means of both the distributions. Based on the hypothesis testing we rejected the hypothesis that both the distributions are equal. We also validated the result using Bayesian approach which also gave the same result. 
By the various methods, we concluded that the red wine and white wine have different distributions.
Finally, we have built a linear regression model of red wine quality based on alcohol content using bootstrap.


