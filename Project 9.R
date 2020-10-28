getwd()
setwd("C:/Users/HP/Documents/R Dataset")
rm(list = ls())

library(DataExplorer)
library(ggplot2)
library(readxl)
library(Hmisc)
library(naniar)
library(nFactors)
library(psych)

train <- read_excel("GL-raw-data.xlsx")

summary(train)
str(train)
names(train)
describe(train)
dim(train)


#reform variable names
names(train) <- gsub(" ", ".", names(train))
names(train) <- gsub("/", "by", names(train))
names(train) <- gsub("%", "p", names(train))
names(train) <- gsub("&", "n", names(train))
names(train) <- gsub("-", ".", names(train))
names(train)


#Add the Default variable
rm(Default)
train$Default <- ifelse(train$Networth.Next.Year >0, 0, 1)
prop.table(table(train$Default))


#Convert "NA" to 0 before converting to the correct data type
train$Creditors.turnover <- gsub("NA", "0", train$Creditors.turnover)
train$Debtors.turnover <- gsub("NA", "0", train$Debtors.turnover)
train$PE.on.BSE <- gsub("NA", "0", train$PE.on.BSE)
train$Total.income <- gsub("NA", "0", train$Total.income)

#Convert to the correct data type
str(train)
sum(is.na(PE.on.BSE))
train$Creditors.turnover <- as.numeric(train$Creditors.turnover)
train$Debtors.turnover <- as.numeric(train$Debtors.turnover)
train$PE.on.BSE <- as.numeric(train$PE.on.BSE)
train$Default <- as.factor(train$Default)
train$Total.income <- as.numeric(train$Total.income)

#MISSING VALUE TREATMENT
library(VIM)
plot_missing(train)
gg_miss_var(train)
train$Total.income[is.na(train$Total.income)] <- train$Sales+train$Income.from.financial.services+train$Other.income
(3541-3256)/3541

#select variables
train <- train[,c(1:5,12:16,29:31,36,39,41:43,49,52,53)]
names(train)

train <- na.omit(train)


#OUTLIER TREATMENT
boxplot(train)
mystats <- function(x)
{
  nmiss<-sum(is.na(x))   #to calculate the missing values
  a <- x[!is.na(x)]      
  m <- mean(a)           #to calculate the mean
  n <- length(a)         #the length
  s <- sd(a)             #the standard devistion
  min <- min(a)          #the minimum value
  p1<-quantile(a,0.01)
  p99<-quantile(a,0.99)
  max <- max(a)          #the max value
  UC <- m+3*s            #the upper limit
  LC <- m-3*s            #the lower limit
  outlier_flag<- max>UC | min<LC #mark the variable/data with outlierflag, if it is above Upper cut-off/ lower than the Lower cut-off
  return(c(n=n, nmiss=nmiss, outlier_flag=outlier_flag, mean=m, stdev=s,min = min, p1=p1, p99=p99,max=max, UC=UC, LC=LC ))
}
#select the variables from the dataset, on which the calculations are to be performed.
diag_stats<-t(data.frame(apply(train[,c(1:20)], 2, mystats)))
#Missing Values & outliers
View(diag_stats)

#Outlier Capping
train$Networth.Next.Year[train$Networth.Next.Year>quantile(train$Networth.Next.Year, 0.99)]<- quantile(train$Networth.Next.Year, 0.99)
train$Networth.Next.Year[train$Networth.Next.Year<quantile(train$Networth.Next.Year, 0.01)]<- quantile(train$Networth.Next.Year, 0.01)
train$Total.assets[train$Total.assets>quantile(train$Total.assets, 0.99)]<- quantile(train$Total.assets, 0.99)
train$Total.assets[train$Total.assets<quantile(train$Total.assets, 0.01)]<- quantile(train$Total.assets, 0.01)
train$Net.worth[train$Net.worth>quantile(train$Net.worth, 0.99)]<- quantile(train$Net.worth, 0.99)
train$Net.worth[train$Net.worth<quantile(train$Net.worth, 0.01)]<- quantile(train$Net.worth, 0.01)
train$Total.income[train$Total.income>quantile(train$Total.income, 0.99)]<- quantile(train$Total.income, 0.99)
train$Total.income[train$Total.income<quantile(train$Total.income, 0.01)]<- quantile(train$Total.income, 0.01)
train$PBDITA.as.p.of.total.income[train$PBDITA.as.p.of.total.income>quantile(train$PBDITA.as.p.of.total.income, 0.99)]<- quantile(train$PBDITA.as.p.of.total.income, 0.99)
train$PBDITA.as.p.of.total.income[train$PBDITA.as.p.of.total.income<quantile(train$PBDITA.as.p.of.total.income, 0.01)]<- quantile(train$PBDITA.as.p.of.total.income, 0.01)
train$PBT.as.p.of.total.income[train$PBT.as.p.of.total.income>quantile(train$PBT.as.p.of.total.income, 0.99)]<- quantile(train$PBT.as.p.of.total.income, 0.99)
train$PBT.as.p.of.total.income[train$PBT.as.p.of.total.income<quantile(train$PBT.as.p.of.total.income, 0.01)]<- quantile(train$PBT.as.p.of.total.income, 0.01)
train$PAT.as.p.of.total.income[train$PAT.as.p.of.total.income>quantile(train$PAT.as.p.of.total.income, 0.99)]<- quantile(train$PAT.as.p.of.total.income, 0.99)
train$PAT.as.p.of.total.income[train$PAT.as.p.of.total.income<quantile(train$PAT.as.p.of.total.income, 0.01)]<- quantile(train$PAT.as.p.of.total.income, 0.01)
train$Cash.profit.as.p.of.total.income[train$Cash.profit.as.p.of.total.income>quantile(train$Cash.profit.as.p.of.total.income, 0.99)]<- quantile(train$Cash.profit.as.p.of.total.income, 0.99)
train$Cash.profit.as.p.of.total.income[train$Cash.profit.as.p.of.total.income<quantile(train$Cash.profit.as.p.of.total.income, 0.01)]<- quantile(train$Cash.profit.as.p.of.total.income, 0.01)
train$PAT.as.p.of.net.worth[train$PAT.as.p.of.net.worth>quantile(train$PAT.as.p.of.net.worth, 0.99)]<- quantile(train$PAT.as.p.of.net.worth, 0.99)
train$PAT.as.p.of.net.worth[train$PAT.as.p.of.net.worth<quantile(train$PAT.as.p.of.net.worth, 0.01)]<- quantile(train$PAT.as.p.of.net.worth, 0.01)
train$TOLbyTNW[train$TOLbyTNW>quantile(train$TOLbyTNW, 0.99)]<- quantile(train$TOLbyTNW, 0.99)
train$TOLbyTNW[train$TOLbyTNW<quantile(train$TOLbyTNW, 0.01)]<- quantile(train$TOLbyTNW, 0.01)
train$Total.term.liabilities.by.tangible.net.worth[train$Total.term.liabilities.by.tangible.net.worth>quantile(train$Total.term.liabilities.by.tangible.net.worth, 0.99)]<- quantile(train$Total.term.liabilities.by.tangible.net.worth, 0.99)
train$Total.term.liabilities.by.tangible.net.worth[train$Total.term.liabilities.by.tangible.net.worth<quantile(train$Total.term.liabilities.by.tangible.net.worth, 0.01)]<- quantile(train$Total.term.liabilities.by.tangible.net.worth, 0.01)
train$`Contingent.liabilities.by.Net.worth-p`[train$`Contingent.liabilities.by.Net.worth-p`>quantile(train$`Contingent.liabilities.by.Net.worth-p`, 0.99)]<- quantile(train$`Contingent.liabilities.by.Net.worth-p`, 0.99)
train$`Contingent.liabilities.by.Net.worth-p`[train$`Contingent.liabilities.by.Net.worth-p`<quantile(train$`Contingent.liabilities.by.Net.worth-p`, 0.01)]<- quantile(train$`Contingent.liabilities.by.Net.worth-p`, 0.01)
train$Net.working.capital[train$Net.working.capital>quantile(train$Net.working.capital, 0.99)]<- quantile(train$Net.working.capital, 0.99)
train$Net.working.capital[train$Net.working.capital<quantile(train$Net.working.capital, 0.01)]<- quantile(train$Net.working.capital, 0.01)
#train$Quick.ratio.times[train$Quick.ratio.times>quantile(train$Quick.ratio.times, 0.99)]<- quantile(train$Quick.ratio.times, 0.99)
#train$Quick.ratio.times[train$Quick.ratio.times<quantile(train$Quick.ratio.times, 0.01)]<- quantile(train$Quick.ratio.times, 0.01)
#train$Current.ratio.times[train$Current.ratio.times>quantile(train$Current.ratio.times, 0.99)]<- quantile(train$Current.ratio.times, 0.99)
#train$Current.ratio.times[train$Current.ratio.times<quantile(train$Current.ratio.times, 0.01)]<- quantile(train$Current.ratio.times, 0.01)
train$Debt.to.equity.ratio.times[train$Debt.to.equity.ratio.times>quantile(train$Debt.to.equity.ratio.times, 0.99)]<- quantile(train$Debt.to.equity.ratio.times, 0.99)
train$Debt.to.equity.ratio.times[train$Debt.to.equity.ratio.times<quantile(train$Debt.to.equity.ratio.times, 0.01)]<- quantile(train$Debt.to.equity.ratio.times, 0.01)
#train$Cash.to.current.liabilities.times[train$Current.ratio.times>quantile(train$Cash.to.current.liabilities.times, 0.99)]<- quantile(train$Cash.to.current.liabilities.times, 0.99)
#train$Cash.to.current.liabilities.times[train$Current.ratio.times<quantile(train$Cash.to.current.liabilities.times, 0.01)]<- quantile(train$Cash.to.current.liabilities.times, 0.01)
train$Cash.to.average.cost.of.sales.per.day[train$Cash.to.average.cost.of.sales.per.day>quantile(train$Cash.to.average.cost.of.sales.per.day, 0.99)]<- quantile(train$Cash.to.average.cost.of.sales.per.day, 0.99)
train$Cash.to.average.cost.of.sales.per.day[train$Cash.to.average.cost.of.sales.per.day<quantile(train$Cash.to.average.cost.of.sales.per.day, 0.01)]<- quantile(train$Cash.to.average.cost.of.sales.per.day, 0.01)
train$Creditors.turnover[train$Creditors.turnover>quantile(train$Creditors.turnover, 0.99)]<- quantile(train$Creditors.turnover, 0.99)
train$Creditors.turnover[train$Creditors.turnover<quantile(train$Creditors.turnover, 0.01)]<- quantile(train$Creditors.turnover, 0.01)
train$Debtors.turnover[train$Debtors.turnover>quantile(train$Debtors.turnover, 0.99)]<- quantile(train$Debtors.turnover, 0.99)
train$Debtors.turnover[train$Debtors.turnover<quantile(train$Debtors.turnover, 0.01)]<- quantile(train$Debtors.turnover, 0.01)
train$EPS[train$EPS>quantile(train$EPS, 0.99)]<- quantile(train$EPS, 0.99)
train$EPS[train$EPS<quantile(train$EPS, 0.01)]<- quantile(train$EPS, 0.01)
train$PE.on.BSE[train$PE.on.BSE>quantile(train$PE.on.BSE, 0.99)]<- quantile(train$PE.on.BSE, 0.99)
train$PE.on.BSE[train$PE.on.BSE<quantile(train$PE.on.BSE, 0.01)]<- quantile(train$PE.on.BSE, 0.01)


#Correlation Check
library(corrplot)
plot_correlation(train)
cormat <- round(cor(train[,c(1:20)]),2)
cor.plot(cormat)
corrplot(cor(train[,c(-21)]), method = "circle", type = "upper")


#UNIVARIATE ANALYSIS
#CONTINUOUS VARIABLES
attach(train)
library(viridis)
boxplot(Total.assets~Default, col = viridis(3))
boxplot(Net.worth~Default, col = viridis(3))
boxplot(Total.income~Default, col = viridis(3))
boxplot(PBDITA.as.p.of.total.income~Default, col = viridis(3))
boxplot(PBT.as.p.of.total.income~Default, col = viridis(3))
boxplot(PAT.as.p.of.total.income~Default, col = viridis(3))
boxplot(Cash.profit.as.p.of.total.income~Default, col = viridis(3))
boxplot(PAT.as.p.of.net.worth~Default, col = viridis(3))
boxplot(TOLbyTNW~Default, col = viridis(3))
boxplot(Total.term.liabilities.by.tangible.net.worth~Default, col = viridis(3))
boxplot(Contingent.liabilities.by.Net.worth.p~Default, col = viridis(3))
boxplot(Net.working.capital~Default, col = viridis(3))
boxplot(Debt.to.equity.ratio.times~Default, col = viridis(3))
boxplot(Cash.to.average.cost.of.sales.per.day~Default, col = viridis(3))
boxplot(Creditors.turnover~Default, col = viridis(3))
boxplot(Debtors.turnover~Default, col = viridis(3))
boxplot(EPS~Default, col = viridis(3))
boxplot(PE.on.BSE~Default, col = viridis(3))



#BIVARIATE ANALYSIS

#PLOT - Independent Variables vs Dependent Variable
##1. SCATTER PLOT
p1 <- qplot(Net.working.capital, Cash.to.average.cost.of.sales.per.day, colour = Default, data = train)
p2 <- qplot(Total.assets, Debtors.turnover, colour = Default, data = train)
gridExtra::grid.arrange(p1, p2, ncol = 2)

p3 <- qplot(Net.worth, PAT.as.p.of.net.worth, colour = Default, data = train)
p4 <- qplot(Total.income, Cash.profit.as.p.of.total.income, colour = Default, data = train)
gridExtra::grid.arrange(p3, p4, ncol = 2)

p5 <- qplot(Net.worth, TOLbyTNW, colour = Default, data = train)
p6 <- qplot(Total.assets, Contingent.liabilities.by.Net.worth.p, colour = Default, data = train)
gridExtra::grid.arrange(p5, p6, ncol = 2)

p7 <- qplot(Cash.profit.as.p.of.total.income, Cash.to.average.cost.of.sales.per.day, colour = Default, data = train)
p8 <- qplot(PAT.as.p.of.net.worth, Debtors.turnover, colour = Default, data = train)
gridExtra::grid.arrange(p7, p8, ncol = 2)

p9 <- qplot(Cash.to.average.cost.of.sales.per.day, TOLbyTNW, colour = Default, data = train)
p10 <- qplot(Debtors.turnover, Debt.to.equity.ratio.times, colour = Default, data = train)
gridExtra::grid.arrange(p9, p10, ncol = 2)


..............................................................................
#                   PCA
..............................................................................

#Dataset Creation
PCA.train <- train[,c(-1,-2,-21)]

#Eigen value comoutation
ev <- eigen(cor(PCA.train))
eigenvalues <- ev$values
eigenvectors <- ev$vectors

plot(eigenvalues, type = "lines", xlab = "Principal Component", ylab = "Eigen Values")

factors <- c(1:18)
scree <- data.frame(factors, eigenvalues)
plot(scree, main = "Scree Plot", col = "Blue", ylim = c(0,4), xlab = "Principal Component", 
     ylab = "Eigen Values", xlim = c(0,10))
lines(scree, col = "Red")
eigenvalues

library(psych)
unrotate <- principal(PCA.train, nfactors = 4, rotate = "none")
unrotate
rotate <- principal(PCA.train, nfactors = 4, rotate = "varimax") #orthogonal rotation will make the factors independent
rotate
rotatedprofile <- plot(rotate, row.names(rotate$loadings), cex = 1.0)

plot(rotate)

#Translate PCA into regression
ndata <- as.data.frame(rotate$scores)
PCA.train <- cbind(train$Default, ndata)
names(PCA.train) <- c("Default", "Profit", "Size", "Leverage", "Liquidity")
names(PCA.train)
summary(PCA.train)
plot_correlation(PCA.train)

..............................................................................
#                   SMOTE Dataset & Logistic Regression
..............................................................................

library(DMwR)
set.seed(1234)

#SMOTE
SMOTE.train <- SMOTE(Default~., PCA.train, perc.over = 200, k = 5, perc.under =100)

prop.table(table(SMOTE.train$Default))

library(car)
#LOGISTIC REGRESSION
model1 <- glm(SMOTE.train$Default~., family = "binomial", data = SMOTE.train)
model2 <- glm(train$Default~., data = train, family = "binomial")
summary(model2)
model1$fitted.values
vif(model1)

#Prediction
LR.prob <- predict(model1, data = SMOTE.train)
LR.pred <- ifelse(LR.prob>0.1, "1","0")

table(SMOTE.train$Default, LR.pred)

#Variable Importance
library(caret)
varImp(model1)
imp <- as.data.frame(varImp(model2))
imp <- data.frame(names = rownames(imp),overall = imp$Overall)
imp[order(imp$overall, decreasing = T),]

..........................................................................
#             MODEL VALIDATION
..........................................................................

test <- read_excel("GL-validation_data.xlsx")

#reform variable names
names(test) <- gsub(" ", ".", names(test))
names(test) <- gsub("/", "by", names(test))
names(test) <- gsub("%", "p", names(test))
names(test) <- gsub("&", "n", names(test))
names(test) <- gsub("-", ".", names(test))
names(test)

#Convert "NA" to 0 before converting to the correct data type
test$Creditors.turnover <- gsub("NA", "0", test$Creditors.turnover)
test$Debtors.turnover <- gsub("NA", "0", test$Debtors.turnover)
test$PE.on.BSE <- gsub("NA", "0", test$PE.on.BSE)
test$Total.income <- gsub("NA", "0", test$Total.income)

#Convert to the correct data type
str(test)
test$Total.income[is.na(test$Total.income)] <- test$Sales+test$Income.from.financial.services+test$Other.income
test$Creditors.turnover <- as.numeric(test$Creditors.turnover)
test$Debtors.turnover <- as.numeric(test$Debtors.turnover)
test$Total.income <- as.numeric(test$Total.income)
test$PE.on.BSE <- as.numeric(test$PE.on.BSE)
test$Default...1 <- as.factor(test$Default...1)
names(test)[2] <- "Default"

#VARIABLE SELECTION
test <- test[,c(1:5,12:16,29:31,36,39,41:43,49,52)]

#MISSING VALUE TREATMENT
test <- na.omit(test)

#OUTLIER CAPPING
test$Networth.Next.Year[test$Networth.Next.Year>quantile(test$Networth.Next.Year, 0.99)]<- quantile(test$Networth.Next.Year, 0.99)
test$Networth.Next.Year[test$Networth.Next.Year<quantile(test$Networth.Next.Year, 0.01)]<- quantile(test$Networth.Next.Year, 0.01)
test$Total.assets[test$Total.assets>quantile(test$Total.assets, 0.99)]<- quantile(test$Total.assets, 0.99)
test$Total.assets[test$Total.assets<quantile(test$Total.assets, 0.01)]<- quantile(test$Total.assets, 0.01)
test$Net.worth[test$Net.worth>quantile(test$Net.worth, 0.99)]<- quantile(test$Net.worth, 0.99)
test$Net.worth[test$Net.worth<quantile(test$Net.worth, 0.01)]<- quantile(test$Net.worth, 0.01)
test$Total.income[test$Total.income>quantile(test$Total.income, 0.99)]<- quantile(test$Total.income, 0.99)
test$Total.income[test$Total.income<quantile(test$Total.income, 0.01)]<- quantile(test$Total.income, 0.01)
test$PBDITA.as.p.of.total.income[test$PBDITA.as.p.of.total.income>quantile(test$PBDITA.as.p.of.total.income, 0.99)]<- quantile(test$PBDITA.as.p.of.total.income, 0.99)
test$PBDITA.as.p.of.total.income[test$PBDITA.as.p.of.total.income<quantile(test$PBDITA.as.p.of.total.income, 0.01)]<- quantile(test$PBDITA.as.p.of.total.income, 0.01)
test$PBT.as.p.of.total.income[test$PBT.as.p.of.total.income>quantile(test$PBT.as.p.of.total.income, 0.99)]<- quantile(test$PBT.as.p.of.total.income, 0.99)
test$PBT.as.p.of.total.income[test$PBT.as.p.of.total.income<quantile(test$PBT.as.p.of.total.income, 0.01)]<- quantile(test$PBT.as.p.of.total.income, 0.01)
test$PAT.as.p.of.total.income[test$PAT.as.p.of.total.income>quantile(test$PAT.as.p.of.total.income, 0.99)]<- quantile(test$PAT.as.p.of.total.income, 0.99)
test$PAT.as.p.of.total.income[test$PAT.as.p.of.total.income<quantile(test$PAT.as.p.of.total.income, 0.01)]<- quantile(test$PAT.as.p.of.total.income, 0.01)
test$Cash.profit.as.p.of.total.income[test$Cash.profit.as.p.of.total.income>quantile(test$Cash.profit.as.p.of.total.income, 0.99)]<- quantile(test$Cash.profit.as.p.of.total.income, 0.99)
test$Cash.profit.as.p.of.total.income[test$Cash.profit.as.p.of.total.income<quantile(test$Cash.profit.as.p.of.total.income, 0.01)]<- quantile(test$Cash.profit.as.p.of.total.income, 0.01)
test$PAT.as.p.of.net.worth[test$PAT.as.p.of.net.worth>quantile(test$PAT.as.p.of.net.worth, 0.99)]<- quantile(test$PAT.as.p.of.net.worth, 0.99)
test$PAT.as.p.of.net.worth[test$PAT.as.p.of.net.worth<quantile(test$PAT.as.p.of.net.worth, 0.01)]<- quantile(test$PAT.as.p.of.net.worth, 0.01)
test$TOLbyTNW[test$TOLbyTNW>quantile(test$TOLbyTNW, 0.99)]<- quantile(test$TOLbyTNW, 0.99)
test$TOLbyTNW[test$TOLbyTNW<quantile(test$TOLbyTNW, 0.01)]<- quantile(test$TOLbyTNW, 0.01)
test$Total.term.liabilities.by.tangible.net.worth[test$Total.term.liabilities.by.tangible.net.worth>quantile(test$Total.term.liabilities.by.tangible.net.worth, 0.99)]<- quantile(test$Total.term.liabilities.by.tangible.net.worth, 0.99)
test$Total.term.liabilities.by.tangible.net.worth[test$Total.term.liabilities.by.tangible.net.worth<quantile(test$Total.term.liabilities.by.tangible.net.worth, 0.01)]<- quantile(test$Total.term.liabilities.by.tangible.net.worth, 0.01)
test$`Contingent.liabilities.by.Net.worth-p`[test$`Contingent.liabilities.by.Net.worth-p`>quantile(test$`Contingent.liabilities.by.Net.worth-p`, 0.99)]<- quantile(test$`Contingent.liabilities.by.Net.worth-p`, 0.99)
test$`Contingent.liabilities.by.Net.worth-p`[test$`Contingent.liabilities.by.Net.worth-p`<quantile(test$`Contingent.liabilities.by.Net.worth-p`, 0.01)]<- quantile(test$`Contingent.liabilities.by.Net.worth-p`, 0.01)
test$Net.working.capital[test$Net.working.capital>quantile(test$Net.working.capital, 0.99)]<- quantile(test$Net.working.capital, 0.99)
test$Net.working.capital[test$Net.working.capital<quantile(test$Net.working.capital, 0.01)]<- quantile(test$Net.working.capital, 0.01)
test$Debt.to.equity.ratio.times[test$Debt.to.equity.ratio.times>quantile(test$Debt.to.equity.ratio.times, 0.99)]<- quantile(test$Debt.to.equity.ratio.times, 0.99)
test$Debt.to.equity.ratio.times[test$Debt.to.equity.ratio.times<quantile(test$Debt.to.equity.ratio.times, 0.01)]<- quantile(test$Debt.to.equity.ratio.times, 0.01)
test$Cash.to.average.cost.of.sales.per.day[test$Cash.to.average.cost.of.sales.per.day>quantile(test$Cash.to.average.cost.of.sales.per.day, 0.99)]<- quantile(test$Cash.to.average.cost.of.sales.per.day, 0.99)
test$Cash.to.average.cost.of.sales.per.day[test$Cash.to.average.cost.of.sales.per.day<quantile(test$Cash.to.average.cost.of.sales.per.day, 0.01)]<- quantile(test$Cash.to.average.cost.of.sales.per.day, 0.01)
test$Creditors.turnover[test$Creditors.turnover>quantile(test$Creditors.turnover, 0.99)]<- quantile(test$Creditors.turnover, 0.99)
test$Creditors.turnover[test$Creditors.turnover<quantile(test$Creditors.turnover, 0.01)]<- quantile(test$Creditors.turnover, 0.01)
test$Debtors.turnover[test$Debtors.turnover>quantile(test$Debtors.turnover, 0.99)]<- quantile(test$Debtors.turnover, 0.99)
test$Debtors.turnover[test$Debtors.turnover<quantile(test$Debtors.turnover, 0.01)]<- quantile(test$Debtors.turnover, 0.01)
test$EPS[test$EPS>quantile(test$EPS, 0.99)]<- quantile(test$EPS, 0.99)
test$EPS[test$EPS<quantile(test$EPS, 0.01)]<- quantile(test$EPS, 0.01)
test$PE.on.BSE[test$PE.on.BSE>quantile(test$PE.on.BSE, 0.99)]<- quantile(test$PE.on.BSE, 0.99)
test$PE.on.BSE[test$PE.on.BSE<quantile(test$PE.on.BSE, 0.01)]<- quantile(test$PE.on.BSE, 0.01)

#PCA
PCA.test <- test[,c(-1,-2)]

rotate.test <- principal(PCA.test, nfactors = 4, rotate = "varimax")
rotate.test
newdf.test <- rotate.test$scores
ndata.test <- as.data.frame(newdf.test)
PCA.test <- cbind(test$Default, ndata.test)
names(PCA.test) <- c("Default", "Profit", "Size", "Leverage", "Liquidity")
names(regPCA)

#PREDICTION/VALIDATING the MODEL
LR.prob.test <- predict(model1, newdata = PCA.test, type = "response")
LR.pred.test <- ifelse(LR.prob.txest>0.6, 1,0)

#MODEL VALIDATION MEASURES
##CONFUSION MATRIX
LR_CM.test = table(PCA.test$Default, LR.pred.test>0.01)
LR_CM.test

##ERROR RATE
(LR_CM.test[1,2]+LR_CM.test[2,1])/nrow(PCA.test)

##ACCURACY
(LR_CM.test[1,1]+LR_CM.test[2,2])/nrow(PCA.test)

#SENSITIVITY
LR_CM.test[1,1]/sum(LR_CM.test[1,1], LR_CM.test[1,2])

#SPECIFICITY
LR_CM.test[2,2]/sum(LR_CM.test[2,1], LR_CM.test[2,2])

##ROC CURVE
test.ROC = prediction(LR.prob.test, PCA.test$Default)
test.ROC.plot = performance(test.ROC,"tpr","fpr")
plot(performance(test.ROC,"tpr","fpr"), col = "red", main = "ROC Curve for Test Data")
abline(0,1,lty = 8, col = "blue")

##KS
max(test.ROC.plot@y.values[[1]]-test.ROC.plot@x.values[[1]])

##AUC
test.AUC=performance(test.ROC,"auc")
slot(test.AUC, "y.values")

##GINI COEFFICIENT
ineq(LR.pred.test, "gini")

...........................................................................
                     #MODEL PERFORMANCE MEASURES
...........................................................................


#ROC is one of the measures if the AUC should be >70% - then very good model, KS > 40
##Similarly from the confusion matrix you can calculate sensitivity and specificity
##Probablity related parameters like KS,ROC,AUC,Concordance,discordance and gini
#regression-can use root mean sq. error to determine - sqrt(predected-actual value) - minimum! is good
#KS - how well you can classify b/w the +ve and the -ve or is it randomly distributed
#Lift - max - best
#gini - (area of A/area of A+B OR 2AUC-1)how well the model is bifurcating
#AUC - higher, the better

library(ROCR)
library(ineq)
library(InformationValue)

##CONFUSION MATRIX
LR_CM = table(SMOTE.train$Default, LR.pred>0.01)
LR_CM

##ERROR RATE
(LR_CM[1,2]+LR_CM[2,1])/nrow(SMOTE.train) #1,2; 2,1 refers to the placements

##ACCURACY
(LR_CM[1,1]+LR_CM[2,2])/nrow(SMOTE.train)

#SENSITIVITY
LR_CM[1,1]/sum(LR_CM[1,1], LR_CM[1,2])

#SPECIFICITY
LR_CM[2,2]/sum(LR_CM[2,1], LR_CM[2,2])

##ROC CURVE
LR.ROC = prediction(LR.prob, SMOTE.train$Default)
LR.ROC.plot = performance(LR.ROC,"tpr","fpr")
plot(performance(LR.ROC,"tpr","fpr"), col = "red", main = "ROC Curve for Train Data")
abline(0,1,lty = 8, col = "blue")

##KS
max(LR.ROC.plot@y.values[[1]]-LR.ROC.plot@x.values[[1]])

##AUC
LR.AUC=performance(LR.ROC,"auc")
slot(LR.AUC, "y.values")
#as.numeric(LR.AUC@y.values)

##GINI COEFFICIENT
ineq(LR.pred, "gini")

##Concordance
Concordance(actuals=SMOTE.train$Default, predictedScores = LR.pred)

library(pscl)
pR2(model1)
#McFadden = 0.4424367, indicates that the model is not an excellent, but is a good fit for the given dataset.

#DECILING
#Dividing the dataset into deciles
library(dplyr)
#SMOTE.train <- SMOTE.train[,c(-6,-7,-8)]
final <- data.frame(SMOTE.train, LR.prob)
final$LR.prob <- round(final$LR.prob, 2)
head(final)

L.F <- arrange(final, desc(LR.prob))
L.F$decile <- with(L.F, cut_number(LR.prob, 10, labels = 10:1))

head(L.F)
train.score <- L.F %>% group_by(decile)


Fold1 <- subset(train.score, decile==1)
Fold2 <- subset(train.score, decile==2)
Fold3 <- subset(train.score, decile==3)
Fold4 <- subset(train.score, decile==4)
Fold5 <- subset(train.score, decile==5)
Fold6 <- subset(train.score, decile==6)
Fold7 <- subset(train.score, decile==7)
Fold8 <- subset(train.score, decile==8)
Fold9 <- subset(train.score, decile==9)
Fold10 <- subset(train.score, decile==10)

train.score1 <- train.score %>% 
  summarise_each(funs(sum), Default) %>% 
  arrange(desc(decile))

train.score2 <- L.F %>% 
  group_by(decile) %>% 
  summarise(Default = n()) %>% 
  arrange(desc(decile))

train.table <- left_join(train.score1, train.score2, by = "decile")
