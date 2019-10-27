#NBA DraftKings Analysis

# Set working directory and upload data set
setwd("C:/Users/gelmashni/Dropbox/Sportsmanac/NBA")

require(xlsx)
data <- read.xlsx(file="NBADK1617.xlsx",sheetName = "Sheet1")

# Inspect full data set
str(data)
head(data)
names(data)
summary(data)

# Call up libraries
library(corrplot)
library(MASS)
library(car)
library(modelr)
library(broom)

# EDA
par(mfrow=c(2,2))
hist(data$DKSalary, col = "navy", xlab = "DK Salary", main = "Histogram of DraftKings Salary", ylim = c(0,4000))
boxplot(data$DKSalary, col = "navy", main = "Boxplot of DraftKings Salary")
hist(data$DKPoints, col = "navy", xlab = "DK Points", main = "Histogram of DraftKings Points", ylim = c(0,4000))
boxplot(data$DKPoints, col = "navy", main = "Boxplot of DraftKings Points")

# Transform DK Salary
data$logDKSalary <- log(data$DKSalary)
data$sqrtDKSalary <- sqrt(data$DKSalary)

# More EDA
par(mfrow=c(2,2))
hist(data$logDKSalary, col = "navy", xlab = "Log DK Salary", main = "Histogram of Log DraftKings Salary", ylim = c(0,4000))
boxplot(data$logDKSalary, col = "navy", main = "Boxplot of Log DraftKings Salary")
hist(data$sqrtDKSalary, col = "navy", xlab = "Sqrt DK Points", main = "Histogram of Sqrt DraftKings Points", ylim = c(0,4000))
boxplot(data$sqrtDKSalary, col = "navy", main = "Boxplot of Sqrt DraftKings Points")

# Create subsets
nbasub1 <- subset(data, select = c(DKPoints, logDKSalary, Venue, Minutes, Usage, Score, OppScore,
                                      PtDiff, Total, Pace, OEFF, DEFF, Rest, HeavySchedule, Spread, OU,
                                      ImpScore, ImpOppScore), na.rm = TRUE)

nbanumeric1 <- subset(data, select = c(DKPoints, logDKSalary, Minutes, Usage, Score, OppScore,
                                   PtDiff, Total, Pace, OEFF, DEFF, Rest, Spread, OU,
                                   ImpScore, ImpOppScore), na.rm = TRUE)

# Correlation plot
c1 <- cor(nbanumeric1)
par(mfrow = c(1,1))
corrplot(c1, method = "square")

#####################################################
################### DraftKings Model ###############
#####################################################
# Define the upper model as the FULL model
upper.nba <- lm(DKPoints ~ .,data=nbasub1);
summary(upper.nba)

# Define the lower model as the Intercept model
lower.nba <- lm(DKPoints ~ 1,data=nbasub1);
summary(lower.nba)

# Backward DK Model
backward.nba.lm <- stepAIC(object=upper.nba,direction=c('backward'));
summary(backward.nba.lm)

vif(backward.nba.lm)
sqrt(vif(backward.nba.lm)) > 2

# Judge models
AIC(backward.nba.lm)

# Final DK Model
best.nba.stepwise <- backward.nba.lm

data.frame(
  R2 = rsquare(best.nba.stepwise, data = nbasub1),
  RMSE = rmse(best.nba.stepwise, data = nbasub1),
  MAE = mae(best.nba.stepwise, data = nbasub1)
)

glance(best.nba.stepwise)

anova(best.nba.stepwise)
summary(best.nba.stepwise)
par(mfrow=c(2,2))  # visualize four graphs at once
plot(best.nba.stepwise)

# Influential Points
par(mfrow=c(1,1))
influencePlot(best.nba.stepwise,	id.method="identify", main="DK Influence Plot", 
              sub="Circle size is proportial to Cook's Distance")

cook <- cooks.distance(best.nba.stepwise)
nbasub1 <- nbasub1[-c(22303, 22986), ]
nbasub1$cook <- round(cook, 6)

#Influential points removed
cooks_cutoff_nba <- 1/10000
nbasub1 <- nbasub1[which(nbasub1$cook < cooks_cutoff_nba),]

########## Run model again with new subset - Trial 2 ##########
nbasub2 <- subset(nbasub1, select = c(DKPoints, logDKSalary, Venue, Minutes, Usage, Score, OppScore,
                                   PtDiff, Total, Pace, OEFF, DEFF, Rest, HeavySchedule, Spread, OU,
                                   ImpScore, ImpOppScore), na.rm = TRUE)

nbanumeric2 <- subset(nbasub1, select = c(DKPoints, logDKSalary, Minutes, Usage, Score, OppScore,
                                       PtDiff, Total, Pace, OEFF, DEFF, Rest, Spread, OU,
                                       ImpScore, ImpOppScore), na.rm = TRUE)

c2 <- cor(nbanumeric2)
par(mfrow = c(1,1))
corrplot(c2, method = "square")

# Define the upper model as the FULL model
upper.nba <- lm(DKPoints ~ .,data=nbasub2);
summary(upper.nba)

# Define the lower model as the Intercept model
lower.nba <- lm(DKPoints ~ 1,data=nbasub2);
summary(lower.nba)

# Backward DK Model
backward.nba.lm <- stepAIC(object=upper.nba,direction=c('backward'));
summary(backward.nba.lm)

vif(backward.nba.lm)
sqrt(vif(backward.nba.lm)) > 2

# Judge models
AIC(backward.nba.lm)

# Final DK Model
best.nba.stepwise <- backward.nba.lm

data.frame(
  R2 = rsquare(best.nba.stepwise, data = nbasub2),
  RMSE = rmse(best.nba.stepwise, data = nbasub2),
  MAE = mae(best.nba.stepwise, data = nbasub2)
)

glance(best.nba.stepwise)

anova(best.nba.stepwise)
summary(best.nba.stepwise)
par(mfrow=c(2,2))  # visualize four graphs at once
plot(best.nba.stepwise)

# Influential Points
par(mfrow=c(1,1))
influencePlot(best.nba.stepwise,	id.method="identify", main="DK Influence Plot", 
              sub="Circle size is proportial to Cook's Distance")

cook <- cooks.distance(best.nba.stepwise)
nbasub2 <- nbasub2[-c(23087, 23636), ]
nbasub2$cook <- round(cook, 6)

# Influential points removed
cooks_cutoff_nba <- 1/10000
nbasub2 <- nbasub2[which(nbasub2$cook < cooks_cutoff_nba),]

########## Run model again with new subset - Trial 3 ##########
nbasub3 <- subset(nbasub2, select = c(DKPoints, logDKSalary, Venue, Minutes, Usage, Score, OppScore,
                                      PtDiff, Total, Pace, OEFF, DEFF, Rest, HeavySchedule), na.rm = TRUE)

nbanumeric3 <- subset(nbasub3, select = c(DKPoints, logDKSalary, Minutes, Usage, Score, OppScore,
                                          PtDiff, Total, Pace, OEFF, DEFF, Rest), na.rm = TRUE)

c3 <- cor(nbanumeric3)
par(mfrow = c(1,1))
corrplot(c3, method = "square")

# Define the upper model as the FULL model
upper.nba <- lm(DKPoints ~ .,data=nbasub3);
summary(upper.nba)

# Define the lower model as the Intercept model
lower.nba <- lm(DKPoints ~ 1,data=nbasub3);
summary(lower.nba)

# Backward DK Model
backward.nba.lm <- stepAIC(object=upper.nba,direction=c('backward'));
summary(backward.nba.lm)

vif(backward.nba.lm)
sqrt(vif(backward.nba.lm)) > 2

# Judge models
AIC(backward.nba.lm)

# Final DK Model
best.nba.stepwise <- backward.nba.lm

data.frame(
  R2 = rsquare(best.nba.stepwise, data = nbasub3),
  RMSE = rmse(best.nba.stepwise, data = nbasub3),
  MAE = mae(best.nba.stepwise, data = nbasub3)
)

glance(best.nba.stepwise)

anova(best.nba.stepwise)
summary(best.nba.stepwise)
par(mfrow=c(2,2))  # visualize four graphs at once
plot(best.nba.stepwise)

# Influential Points
par(mfrow=c(1,1))
influencePlot(best.nba.stepwise,	id.method="identify", main="DK Influence Plot", 
              sub="Circle size is proportial to Cook's Distance")

cook <- cooks.distance(best.nba.stepwise)
nbasub3 <- nbasub3[-c(20000), ]
nbasub3$cook <- round(cook, 6)

# Influential points removed
cooks_cutoff_nba <- 1/20000
nbasub3 <- nbasub3[which(nbasub3$cook < cooks_cutoff_nba),]

########## Run model again with new subset - Trial 4 ##########
nbasub4 <- subset(nbasub3, select = c(DKPoints, logDKSalary, Venue, Minutes, Usage, Score, OppScore,
                                      PtDiff, Total, Pace, DEFF, Rest, HeavySchedule), na.rm = TRUE)

nbanumeric4 <- subset(nbasub4, select = c(DKPoints, logDKSalary, Minutes, Usage, Score, OppScore,
                                          PtDiff, Total, Pace, DEFF, Rest), na.rm = TRUE)

c4 <- cor(nbanumeric4)
par(mfrow = c(1,1))
corrplot(c4, method = "square")

# Define the upper model as the FULL model
upper.nba <- lm(DKPoints ~ .,data=nbasub4);
summary(upper.nba)

# Define the lower model as the Intercept model
lower.nba <- lm(DKPoints ~ 1,data=nbasub4);
summary(lower.nba)

# Backward DK Model
backward.nba.lm <- stepAIC(object=upper.nba,direction=c('backward'));
summary(backward.nba.lm)

vif(backward.nba.lm)
sqrt(vif(backward.nba.lm)) > 2

# Judge models
AIC(backward.nba.lm)

# Final DK Model
best.nba.stepwise <- backward.nba.lm

data.frame(
  R2 = rsquare(best.nba.stepwise, data = nbasub4),
  RMSE = rmse(best.nba.stepwise, data = nbasub4),
  MAE = mae(best.nba.stepwise, data = nbasub4)
)

glance(best.nba.stepwise)

anova(best.nba.stepwise)
summary(best.nba.stepwise)
par(mfrow=c(2,2))  # visualize four graphs at once
plot(best.nba.stepwise)

# Influential Points
par(mfrow=c(1,1))
influencePlot(best.nba.stepwise,	id.method="identify", main="DK Influence Plot", 
              sub="Circle size is proportial to Cook's Distance")

cook <- cooks.distance(best.nba.stepwise)
nbasub3 <- nbasub3[-c(20000), ]
nbasub3$cook <- round(cook, 6)

# Influential points removed
cooks_cutoff_nba <- 1/20000
nbasub3 <- nbasub3[which(nbasub3$cook < cooks_cutoff_nba),]

