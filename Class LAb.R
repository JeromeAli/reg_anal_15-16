********#CHAPTER 2**********
************* Example 3 ********
comp <- read.table("CH2Q3.txt",header=T)
modCH2Q3 <- lm(y~ x, data = comp)
summary(modCH2Q3)
# Check the fit graphically 
plot(comp$x,comp$y)
abline(modCH2Q3$coef)
# We have an R-squared of 99% and an apparently good fit to the data. We now fit a model that reserves a
# parameter for each group of data with the same value of x. This is accomplished by declaring the predictor
# to be a factor.
modCH2Q3a <- lm(y~ factor(x), data = comp)
points(comp$x,modCH2Q3a$fit,pch=18)

# H0 :The functional form of the simple linear regression model is correct
# H1 :The functional form of the simple linear regression model is not correct
anova(modCH2Q3,modCH2Q3a)
#___________________________________________________________________________________
#Chapter 3
 #Example 1 
modCH2Q3.res <- resid(modCH2Q3)
plot(comp$x,modCH2Q3.res, ylab = "Residuals", xlab = "x" )
modCH2Q3.stdres = rstandard(modCH2Q3)
  abline(h =0)
plot(fitted(modCH2Q3), modCH2Q3.res, ylab = "Residuals", xlab = "Fitted Values" )
plot(comp$i, modCH2Q3.stdres, ylab = "Standardized Residuals", xlab = "Observation Order") # i in this case is the timing of the observations
# The above used to test Costant varince assumption.

# If no patterns (ie Negative or positive autocorrelation) is seen (line 26) then we can say the residuals are independent
#3.3
x=c(-4,-3,-2,-1,0,10)
ybad=c(2.48,0.73,-0.04,-1.44,-1.32,0.00)
ygood=c(2.48,0.73,-0.04,-1.44,-1.32,-11.40)
m1=lm(ybad~x)
m2=lm(ygood~x)
summary(m1)
summary(m2)

#___________________________________________________________________________________
# Chapter 4
eg <- read.table("eg461.txt",header=T)
# names Function
# eg
#Note assign columns with name DO NOT USE  attach function

y=eg$y
x1=eg$x1
x2=eg$x2

par(mfrow=c(2,1)) # Split page into 2 rows and a col
plot(x1,y,main="Fig1.weekly fuel consumption vs average hourly temperature",cex.main=0.8) # ce.main - Font sizes
plot(x2,y,main="Fig2.weekly fuel consumption vs cchill index",cex.main=0.8)
plot(eg) # Plot matrix of graphs - Note  how to comprehend these


mod=lm(y~x1+x2) # Creates a linear model
ls(mod) # List all variables or objects in Mod
summary(mod) # Gives Summary statistics
anova(mod) # analysis of variance Note regressionSS on both x

mat=model.matrix(mod) # Gives design matrix     IMPORTANT
mat # Note that YOu can use this to perfoem the desired matrix algebra


#####  RESIDUAL ANALYIS #######
yfit=fitted.values(mod) # Fitted Values
resd=resid(mod) # Residuals
sresd=rstandard(mod) # Standardized Residual

par(mfrow=c(2,2)) # Again
plot(yfit,sresd, xlab="yhat",ylab="sresid",main= "Fitted values vs Standardised residuals",cex.main=0.75 )
plot(x2,sresd,ylab="sresid",main="Chill Index vs Standardised residuals",cex.main=0.75)
plot(x1,sresd,ylab="sresid",main="Average hourly temperature vs Standardised residuals",cex.main=0.65)
qqnorm(sresd,cex.main=0.75)
qqline(sresd)
     
     
#     ***************************************Partial F test********************************************************************
eg472=read.table("eg472.txt",header=T)
     
mod1=lm(y~x1+x2+x1sq+x2sq+x1x2+x1sqx2+x1x2sq,data=eg472)
summary(mod1)
anova(mod1)
     
mod2=lm(y~x1+x2+x1sq+x1x2+x1sqx2,data=eg472)
summary(mod2)
anova(mod2)
     
mod3=lm(y~x1+x2+x1sq+x1x2,data=eg472)
summary(mod3) # NOTE THE pvalues for the variables
anova(mod3)
     
     
mod4=lm(y~x2+x2sq,data=eg472)
summary(mod4)
anova(mod4)
     
mod5=lm(y~x1+x1sq,data=eg472)
summary(mod5)
anova(mod5)
     
mod6=lm(y~x1+x2,data=eg472)
summary(mod6) # note the pvalues and signoficance of the predictor varible of the  reduced model
anova(mod6)

# Comparison     
anova(mod3,mod6) # Note the order of the mod is important. Produces neg df
anova(mod6,mod3) # Correct order  Note the size of F values 
anova(mod3,mod1)
     
 #    ***********************Interaction Terms*************************
par(mfrow=c(2,1))
int=read.table("eg48.txt",header=T)
     
interaction.plot(int$x1,int$x2,int$y) # Note last argument is response and Middle variable is held constant
interaction.plot(int$x2,int$x1,int$y) # Changing the order keeping x1 constant
# Noted that par function is still in effect gence two columns (Closing the window would stop the function)
     
# Below is similar to analyis above
interact.mod=lm(y~x1+x2+x1:x2,data=int) # Note the : tells r includes an interaction
summary(interact.mod)
anova(interact.mod)
     
yhat=fitted.values(interact.mod)
resd=resid(interact.mod)
sresd=rstandard(interact.mod)
     
par(mfrow=c(2,2))
plot(yhat,sresd,xlab="yhat",ylab="std resid",main="Fitted values vs Standardised residuals",cex.main=0.75)
plot(x1,sresd,ylab="std resid",main="Tv-radio Expenses vs Standardised residuals",cex.main=0.75)
plot(x2,sresd,ylab="std resid",main="Print Expenses vs Standardised residuals",cex.main=0.65)
qqnorm(sresd,cex.main=0.75)
qqline(sresd)
     
  #*******************************Dummy Model********************************* To do in class
eg49=read.table("eg49.txt",header=T)
eg49[1:2, 1:3] # First 2 rows and first 3 columns
x1=eg49$exp
y=eg49$salary
     
x2=rep(1:0,each=6) # Repeat 1 and 0 six times - Note there other programs to do this.
x2=as.factor(x2) # Change numeric values of x2 and turn them into factors of x2
     
interaction.plot(x1,x2,y)
     
tech.mod=lm(y~exp+sex+exp:sex,data=eg49)
summary(tech.mod) # Note the we can produce the equation using the values
# y = 18593.00 + 969.00 x1 + 866.71x2 + 260.13x1x2
# Finding the salary for males or females only let the other variable = 0
anova(tech.mod)
     
     # Getting a Matrix for 
      m = model.matrix(tech.mod)
      m
eg491=read.table("eg491.txt",header=T)
     
dum.mod=lm(y~x+as.factor(loc),eg491)
summary(dum.mod)
anova(dum.mod)
yhat=fitted.values(dum.mod)
resd=resid(dum.mod)
sresd=rstandard(dum.mod)
     
     
     