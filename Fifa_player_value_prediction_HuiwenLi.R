library(dplyr)
library(ggplot2)


#Load data
full_data <- read.csv("C:/Users/wendy li/Desktop/NEU_MS Analytics/2_Spring Quarter 2019/ALY6015 Intermediate Analytics/Project/fifa_player_data.csv", stringsAsFactors = FALSE)
Encoding(full_data$Value) <- "UTF-8"
Encoding(full_data$Wage) <- "UTF-8"
Encoding(full_data$Release.Clause) <- "UTF-8"
head(full_data)
str(full_data)

col <- c("Name","Age","Nationality","Overall","Club","Value","Wage","International.Reputation","Weak.Foot",
         "Skill.Moves","Work.Rate","Position","Height","Weight","Crossing","Finishing","HeadingAccuracy",
         "ShortPassing","Volleys","Dribbling","Curve","FKAccuracy","LongPassing","BallControl","Acceleration",
         "SprintSpeed","Agility","Reactions","Balance","ShotPower","Jumping","Stamina","Strength","LongShots",
         "Aggression","Interceptions","Positioning","Vision","Penalties","Composure","Marking","StandingTackle",
         "SlidingTackle","GKDiving","GKHandling","GKKicking","GKPositioning","GKReflexes","Release.Clause")
players <- full_data[col]

dim(players)
str(players)

#check any NA and remove rows with NA
colSums(is.na(players))
players <- na.omit(players)


###change all money column to numerical in thonsand dollars
#for "Value" column
players$Value <- gsub("â¬","",players$Value)
players$Value

for (i in 1:length(players$Value)){
        if (grepl("M",players$Value[i])){
             players$Value[i] <- (as.numeric(gsub("M","",players$Value[i])))*1000000
        } else if (grepl("K",players$Value[i])){
                players$Value[i] <- as.numeric(gsub("K","",players$Value[i]))*1000  
        } 
}

class(players$Value)

players$Value <- as.numeric(players$Value)

#for "Wage" column
summary(players$Wage)

players$Wage <- gsub("Â","",players$Wage) 
players$Wage <- as.numeric(gsub("K","",players$Wage))*1000

summary(players$Wage)

#for "Release.Clause" column
players$Release.Clause <- gsub("Â","",players$Release.Clause)

for (i in 1:length(players$Release.Clause)){
        if (grepl("M", players$Release.Clause[i])){
                players$Release.Clause[i] <- (as.numeric(gsub("M","", players$Release.Clause[i])))*1000000
        }else if (grepl("K", players$Release.Clause[i])){
                players$Release.Clause[i] <- as.numeric(gsub("K","",players$Release.Clause[i]))*1000
        }
}

players$Release.Clause <- as.numeric(players$Release.Clause)
summary(players$Release.Clause)

sum(is.na(players$Release.Clause))

#for "Height" column, change to cm
install.packages("stringr")
library(stringr)

for (i in 1:length(players$Height)){
        players$Height[[i]] <- as.numeric(str_sub(players$Height[[i]],1,1))*30.48 + 
                as.numeric(str_sub(players$Height[[i]],3,4))*2.54
}

head(players$Height)
players$Height <- as.numeric(players$Height)
head(players$Height)


#for "Weight" column, remove lbs
players$Weight <- as.numeric(gsub("lbs","",players$Weight))

str(players)

#change 0 to NA and remove NAs again
players$Value[which(players$Value == 0)] <- NA
sum(is.na(players$Value))

players$Wage[which(players$Wage == 0)] <- NA
sum(is.na(players$Wage))

sum(is.na(players$Release.Clause))

colSums(is.na(players))
players <- na.omit(players)

colSums(is.na(players))
str(players)

###Create a simplified position varaible to account for all player positions
for (i in 1:length(players$Position)){
        if (players$Position[[i]] == "RB"| players$Position[[i]] == "LB"| players$Position[[i]] == "CB"|
            players$Position[[i]] == "LCB"| players$Position[[i]] == "RCB"| players$Position[[i]] == "RWB"|
            players$Position[[i]] == "LWB"){
                players$Position[[i]] <- "DF"
                
        }else if (players$Position[[i]] == "LDM"| players$Position[[i]] == "CDM"| players$Position[[i]] == "RDM"){
                players$Position[[i]] <- "DM"
                
        }else if(players$Position[[i]] == "LM"| players$Position[[i]] == "LCM"| players$Position[[i]] == "CM"|
                 players$Position[[i]] == "RCM"| players$Position[[i]] == "RM"){
                players$Position[[i]] <- "MF"
                
        }else if(players$Position[[i]] == "LAM"| players$Position[[i]] == "CAM"| players$Position[[i]] == "RAM"|
                 players$Position[[i]] == "LW"| players$Position[[i]] == "RW"){
                players$Position[[i]] <- "AM"
                
        }else if(players$Position[[i]] == "RS"| players$Position[[i]] == "ST"| players$Position[[i]] == "LS"|
                 players$Position[[i]] == "CF"| players$Position[[i]] == "LF"| players$Position[[i]] == "RF"){
                players$Position[[i]] <- "ST"
        }
}

str(players$Position)
sum(is.na(players$Position))

###Create a simplified work.rate varaible to account for all player positions

for (i in 1:length(players$Work.Rate)){
        if (players$Work.Rate[[i]] == "High/ High"| players$Work.Rate[[i]] == "High/ Medium"|
                players$Work.Rate[[i]] == "Medium/ High"){
                players$Work.Rate[[i]] <- "High"
        }else if (players$Work.Rate[[i]] == "High/ Low"| players$Work.Rate[[i]] == "Low/ High"|
                  players$Work.Rate[[i]] == "Medium/ Medium"){
                players$Work.Rate[[i]] <- "Medium"
        }else if (players$Work.Rate[[i]] == "Low/ Low"| players$Work.Rate[[i]] == "Low/ Medium"|
                  players$Work.Rate[[i]] == "Medium/ Low"){
                players$Work.Rate[[i]] <- "Low"
        }
}

str(players$Work.Rate)

#Distribution of Value
#total value by nationality
library(ggplot2)
val_Nat <- aggregate(Value ~ Nationality, players, sum)
val_Nat <- val_Nat[order(val_Nat$Value, decreasing=TRUE),]
rownames(val_Nat) <- NULL


ggplot(val_Nat[1:10,], aes(x=reorder(Nationality, Value), y=round(Value/1000000,1), label=round(Value/1000000,1))) + 
        geom_bar(stat="identity", fill="salmon") + 
        geom_text()+
        coord_flip()+
        xlab("Nationality")+
        ylab("Total Value in million")

#total value by club
val_Club <- aggregate(Value ~ Club, players, sum)
val_Club <- val_Club[order(val_Club$Value, decreasing=TRUE),]
rownames(val_Club) <- NULL

dev.off()

ggplot(val_Club[1:10,], aes(x=reorder(Club, Value), y=round(Value/1000000,1), label=round(Value/1000000,1))) + 
        geom_bar(stat="identity", fill="turquoise3") + 
        geom_text()+
        coord_flip()+
        xlab("Club")+
        ylab("Total Value in million")

#top wage by club
wage_Club <- aggregate(Wage ~ Club, players, sum)
wage_Club <- wage_Club[order(wage_Club$Wage, decreasing=TRUE),]
rownames(wage_Club) <- NULL

ggplot(wage_Club[1:10,], aes(x=reorder(Club, Wage), y=round(Wage/1000,1), label=round(Wage/1000,1))) + 
        geom_bar(stat="identity", fill="lightskyblue") + 
        geom_text()+
        coord_flip()+
        xlab("Club")+
        ylab("Total Wage in thousands")

#Top individual wage
top_wage <- players %>% select(c("Name","Wage")) %>% arrange(desc(Wage)) %>% slice(1:20)
head(top_wage,20)
ggplot(top_wage, aes(x=reorder(Name,Wage), y=round(Wage/1000,1), label=round(Wage/1000,1), fill=Wage)) + 
        geom_bar(stat="identity") + 
        coord_flip() +
        geom_text(color="red") +
        xlab("Names") +
        ylab("Wage in thousands")

#Top individual value
top_value <- players %>% select(c("Name","Value")) %>% arrange(desc(Value)) %>% slice(1:20)
head(top_value,20)

ggplot(top_value, aes(x=reorder(Name,Value), y=round(Value/1000000,1), label=round(Value/1000000,1), fill=Value)) + 
        geom_bar(stat="identity") + 
        coord_flip() +
        geom_text(color="red") +
        xlab("Names") +
        ylab("Value in Millions")


###Select columns for modelling
col_model <- c("Age","Overall","Value","Wage","International.Reputation","Weak.Foot",
               "Skill.Moves","Work.Rate","Position","Height","Weight","Crossing","Finishing","HeadingAccuracy",
               "ShortPassing","Volleys","Dribbling","Curve","FKAccuracy","LongPassing","BallControl","Acceleration",
               "SprintSpeed","Agility","Reactions","Balance","ShotPower","Jumping","Stamina","Strength","LongShots",
               "Aggression","Interceptions","Positioning","Vision","Penalties","Composure","Marking","StandingTackle",
               "SlidingTackle","GKDiving","GKHandling","GKKicking","GKPositioning","GKReflexes","Release.Clause")

fifa <- players[col_model]
fifa$Work.Rate <- factor(fifa$Work.Rate)
fifa$Position <- factor(fifa$Position)
fifa$Height <- as.numeric(fifa$Height)

str(fifa)

###histgram to check distribution
par(mfrow=c(1,2))

hist(fifa$Value,prob=TRUE,
     main = "Histogram of player's Value",
     col = "darkseagreen2")

lines(density(fifa$Value),type="l", col="red", lwd=2)

hist(log(fifa$Value),prob=TRUE,       #really necessay?
     main = "Histogram of log(Value)",
     col = "darkseagreen2")
lines(density(log(fifa$Value)),type="l", col="red", lwd=2)

par(mfrow=c(1,1))

hist(fifa$Wage,prob=TRUE,
     main = "Histogram of player's Value",
     col = "darkseagreen2")

hist(log(fifa$Release.Clause),prob=TRUE,
     main = "Histogram of player's Value",
     col = "darkseagreen2")


par(mfrow=c(3,4))

for(i in 1:12) {
        hist(fifa[[i]], main=names(fifa)[i])
}

for(i in 13:24) {
        hist(fifa[[i]], main=names(fifa)[i])
}

for(i in 25:36) {
        hist(fifa[[i]], main=names(fifa)[i])
}

for(i in 37:46) {
        hist(fifa[[i]], main=names(fifa)[i])
}


###scatter plot of all 
par(mfrow=c(3,4))

for(i in 1:12) {
        plot(fifa$Value, fifa[[i]], main=names(fifa)[i])
}

for(i in 13:24) {
        plot(fifa$Value, fifa[[i]], main=names(fifa)[i])
}

for(i in 25:36) {
        plot(fifa$Value, fifa[[i]], main=names(fifa)[i])
}

for(i in 37:46) {
        plot(fifa$Value, fifa[[i]], main=names(fifa)[i])
}

par(mfrow=c(1,1))

###Correlation Plot of all variables
install.packages("corrplot")
library(corrplot)

cordata <- fifa %>% select(-c("Work.Rate","Position"))
correlations <- cor(cordata)

png(filename = "corrplot.png", width = 1000, height = 1000, pointsize = 12)
corrplot(correlations, method="circle")
dev.off()

###randomly split data into train and test
# 80% of the sample size
samp_size <- floor(0.8 * nrow(fifa))
# set the seed to make your partition reproducible
set.seed(123)
train_row <- sample(seq_len(nrow(fifa)), size = samp_size)
train <- fifa[train_row, ]
test <- fifa[-train_row, ]

###fitting model
#iterarively fit a linear model
lm_fit1 <- lm(Value ~ ., data=train)
summary(lm_fit1)
par(mfrow=c(2,2))
plot(lm_fit1)

#lm_fit1 result not good, try to add log to Value, Wage, Release.Clause
lm_fit2 <- lm(log(Value) ~ Overall + log(Wage) + International.Reputation + Position + 
                      Crossing + Finishing + Volleys + FKAccuracy + LongPassing +
                      Stamina + Marking + StandingTackle + GKHandling + log(Release.Clause), 
                      data=train)
summary(lm_fit2)
plot(lm_fit2)

#check multicollinearity
install.packages("car")
library(car)
vif(lm_fit2)

#remove predictors with high multicollinearity > 10
lm_fit3 <- lm(log(Value) ~ Overall + log(Wage) + International.Reputation + 
                      Crossing + Finishing + Volleys + FKAccuracy + LongPassing +
                      Stamina + Marking + StandingTackle + log(Release.Clause),
                      data=train)
summary(lm_fit3)
plot(lm_fit3)

#Use step regression to fit a linear model
library(MASS)

fit_step <- lm(log(Value) ~ Age + Overall + log(Wage) + International.Reputation + 
                       Weak.Foot + Skill.Moves + Work.Rate + Position + Height + 
                       Weight + Crossing + Finishing + HeadingAccuracy + ShortPassing + 
                       Volleys + Dribbling + Curve + FKAccuracy + LongPassing + 
                       BallControl + Acceleration + SprintSpeed + Agility + Reactions + 
                       Balance + ShotPower + Jumping + Stamina + Strength + LongShots + 
                       Aggression + Interceptions + Positioning + Vision + Penalties + 
                       Composure + Marking + StandingTackle + SlidingTackle + GKDiving + 
                       GKHandling + GKKicking + GKPositioning + GKReflexes + log(Release.Clause), 
               data=train)
step <- stepAIC(fit_step, direction="both")
step$anova # display results
step


#final model
step_fit <- lm(log(Value) ~ Age + Overall + log(Wage) + International.Reputation + 
                       Weak.Foot + Skill.Moves + Position + Weight + Crossing + 
                       HeadingAccuracy + Volleys + Dribbling + FKAccuracy + LongPassing + 
                       Acceleration + Agility + Balance + Jumping + Stamina + Strength + 
                       Aggression + Interceptions + Positioning + Vision + Penalties + 
                       Composure + Marking + SlidingTackle + GKDiving + GKHandling + 
                       GKPositioning + log(Release.Clause), data = train)
summary(step_fit)
plot(step_fit)


### Fit a LASSO model
library(glmnet)

train_noy <- train[,-3]

y <- as.matrix(train$Value)
x <- model.matrix(~ Age + Overall + log(Wage) + International.Reputation + 
                          Weak.Foot + Skill.Moves + Work.Rate + Position + Height + 
                          Weight + Crossing + Finishing + HeadingAccuracy + ShortPassing + 
                          Volleys + Dribbling + Curve + FKAccuracy + LongPassing + 
                          BallControl + Acceleration + SprintSpeed + Agility + Reactions + 
                          Balance + ShotPower + Jumping + Stamina + Strength + LongShots + 
                          Aggression + Interceptions + Positioning + Vision + Penalties + 
                          Composure + Marking + StandingTackle + SlidingTackle + GKDiving + 
                          GKHandling + GKKicking + GKPositioning + GKReflexes + log(Release.Clause),data=train_noy)

model_lasso <- glmnet(x,log(y))

# use glmnet function to plot the path of each of predictor variable coefficients 
# against the L1 norm of the beta vector. 
plot.glmnet(model_lasso, xvar = "norm", label = TRUE) 
plot.glmnet(model_lasso, xvar = "lambda", label = TRUE) 

# CV Fit 
cv_fit <- cv.glmnet(x=x, y=log(y), alpha = 1, nlambda = 1000)
plot.cv.glmnet(cv_fit)

# cv.glmnet function to get the cross validation curve and the value of lambda 
# that minimizes the mean cross validation error.
cv_fit$lambda.min

# Using the minimum value of lambda from the previous exercise, get the estimated beta matrix. 
# Note that some coefficients have been shrunk to zero. 
# This indicates which predictors are important in explaining the variation in y.
fit_lasso <- glmnet(x=x, y=log(y), alpha = 1, lambda=cv_fit$lambda.min)
fit_lasso$beta
coef(fit_lasso)

#Final model:
lasso_fit <- lm(log(Value) ~ Overall + Skill.Moves + Finishing + Stamina + Positioning +
                         log(Release.Clause), data = train)
summary(lasso_fit)
plot(lasso_fit)

#set margin back to default
par(mfrow=c(1,1))

###Cross-validation
library(DAAG)

#check on linear regression
cv.lm(data = train, lm_fit3, m=5)  # mse0.0177

#check on step regression
cv.lm(data = train, step_fit, m = 5)  #0.0159

#check on lasso regression
cv.lm(data = train, lasso_fit, m = 5 ) #0.0185

library(stats)
AIC(lm_fit3)
BIC(lm_fit3)
AIC(step_fit)
BIC(step_fit)
AIC(lasso_fit)
BIC(lasso_fit)

### calculate predicted R^2 and mean square error
##linear regression model
y <- log(test$Value)
par(mfrow = c(2,2))

y_hat_lm <- predict(lm_fit3, test)
mse_lm <- mean((y_hat_lm - y)^2)
pre_R2_lm <- 1- (sum((y_hat_lm - y)^2)/sum((y - mean(y))^2))
mse_lm
pre_R2_lm

#plot predicted y against actual y
plot(y, y_hat_lm,main="OLS Model", xlab="Actual y values", ylab="Fitted y Values")
trendline_lm = lm(y_hat_lm ~ y)
abline(trendline_lm, col='red', lwd=2)
summary(trendline_lm)

##step regression model
y_hat_step <- predict(step_fit, test)
mse_step <- mean((y_hat_step - y)^2)
pre_R2_step <- 1- (sum((y_hat_step - y)^2)/sum((y - mean(y))^2))
mse_lm
pre_R2_step

#plot predicted y against actual y
plot(y, y_hat_step,main="Step Model", xlab="Actual y values", ylab="Fitted y values")
trendline_step = lm(y_hat_step ~ y)
abline(trendline_step, col='red', lwd=2)
summary(trendline_step)

##lasso regression model
y_hat_lasso <- predict(lasso_fit, test)
mse_lasso <- mean((y_hat_lasso - y)^2)
pre_R2_lasso <- 1- (sum((y_hat_lasso - y)^2)/sum((y - mean(y))^2))
mse_lasso
pre_R2_lasso

#plot predicted y against actual y
plot(y, y_hat_lasso,main="LASSO Model", xlab="Actual y values", ylab="Fitted y values")
trendline_lasso = lm(y_hat_lasso ~ y)
abline(trendline_lasso, col='red', lwd=2)
summary(trendline_lasso)

