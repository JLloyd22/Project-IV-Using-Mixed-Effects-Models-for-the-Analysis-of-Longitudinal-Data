### This R Code can be used to obtain all results included in my Project IV poster 
### 'Using Mixed Effects Models for the Analysis of Longitudinal Data'
## Jack Lloyd, supervised by Reza Drikvandi
## Department of Mathematics, Durham University
## February 2024. 


## Loading the required packages
library(lme4)
library(ggplot2)
require(sjPlot)

### Loading and Cleaning the Dataset
data("ChickWeight")
ChickWeight
ChickWeight$Chick = as.factor(ChickWeight$Chick)
ChickWeight$Diet = as.factor(ChickWeight$Diet)

## Plotting the ChickWeight Dataset

ggplot(data=ChickWeight, aes(x=Time, y=weight, col=Chick))+
  geom_line(size=1, alpha=0.8)+
  theme(legend.position = 'none')

### Fitting a Linear Regression Model and plotting it with the data
ggplot(data=ChickWeight, aes(x=Time, y=weight, col=Chick))+
  geom_point(size=1.2, alpha=0.8)+
  geom_line(aes(y=predict(lm(weight~Time, data=ChickWeight)), x=Time),col='red', size=1, alpha=0.8)+
  theme(legend.position='theme')

## Fitting a Linear Mixed Effects Model with a random slope and intercept
fitcw = lmer(weight ~ Time + Diet + (Time|Chick), data=ChickWeight)

## Getting the coefficients and other statistics of the model
summary(fitcw)

## Plotting the Random Effects (can get exact values in the summary of the model)
plot_model(fitcw, type='re', transform=NULL)

## Observing how missing data affects the RSS of the LME

## Making a sequence of values which will be the percentages of data that we take out of ChickWeight
x = seq(0,1,0.05)

## After trying the data we saw that it was sufficient just to go up to taking maximum 60% of data out of ChickWeight
## STEP 1 - Make an 1x13 empty matrix to put our RSS values in 
rss = matrix(NA, nrow=1, ncol=13)

## STEP 2 - We randomly choose a percentage of the response values to set equal to NA
## We then fit a Linear Mixed Effects Random Slope model to the data using lmer. 
## We then obtained the RSS of each model and input that value into our empty 'rss' matrix
for (i in 1:13){
  mis = round(runif((578*x[i]), min=0, max=578))
  ChickWeightNA = ChickWeight
  ChickWeightNA[mis,1] = NA
  chick.lmm = lmer(weight ~ Time +(1|Chick), data=ChickWeightNA)
  chick.lmm.pred = predict(chick.lmm, ChickWeight)
  rss[i] = sum((chick.lmm.pred - ChickWeight[,1])^2)
}

##Then checked our RSS
rss

## Note - Step 1 and Step 2 may need to be repeated multiple times before a full set of RSS values is obtained. This is due to a few chicks having very 
## few response values before dropping out. As we take more data, the chance of all response values of a chick being set to NA increases. R then cannot 
## fit a model to the dataset. 

## Once a full set of RSS values is obtained, save this as rssperf
rssperf = rss

## Multiplying x by 100 to give us percentage values
xperc = 100*x

## Only taking up to 60% of data so only need x values 1:13
xperc = xperc[1:13]

## Making xperc a matrix so we can created a combined dataframe which ggplot can understand
xperc = as.matrix(xperc, nrow=1)

## Creating a combined dataframe of percentages and rss values
plotdf = as.data.frame(cbind(xperc, t(rssperf)))

## Changing the names of the columns so we can refer to them in ggplot
names(plotdf) = c('Percentage', 'RSS')

## Plotting Percentage vs RSS value. See an overall positive trend. 
ggplot(plotdf, aes(x=Percentage, y=RSS, xlab='Percentage of Missing Data'), ylab='Residual Sum of Squares')+
  geom_point(col='red',size=2.5)+ 
  geom_smooth(method='lm', se=FALSE)+
  labs(x='Percentage of Missing Data', y='Residual Sum of Squares')

## Alternatively, we could just use plot() function and avoid the hastle of creating matrices and a dataframe once the values are obtained. 
## However this plot does not look as nice

## THANKS FOR READING!!
## My report will also be uploaded to my GitHub once completed so please give that a read if interested. The R code that is used for results in my report will also be uploaded. 


