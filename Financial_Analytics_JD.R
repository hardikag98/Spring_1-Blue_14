# needed libraries
library(gmodels)
library(vcd)
library(smbinning)
library(dplyr)
library(stringr)
library(shades)
library(latticeExtra)
library(plotly)

# read in data
accepts <- read.csv("C:\\Users\\Justin\\OneDrive - North Carolina State University\\Documents\\NC State\\IAA R\\Data\\accepted_customers.csv")

rejects <- read.csv("C:\\Users\\Justin\\OneDrive - North Carolina State University\\Documents\\NC State\\IAA R\\Data\\rejected_customers.csv")

# create score card #######################################################################################################

table(accepts$GB) # 50/50 split

# Setting Categorical Variables as Factors #
accepts$BUREAU <- as.factor(accepts$BUREAU)
accepts$CAR <- as.factor(accepts$CAR)
accepts$CARDS <- as.factor(accepts$CARDS)
accepts$NAT <- as.factor(accepts$NAT)
accepts$PRODUCT <- as.factor(accepts$PRODUCT)
accepts$PROF <- as.factor(accepts$PROF)
accepts$REG <- as.factor(accepts$REG)
accepts$TEL <- as.factor(accepts$TEL)

# binary variables into factors
accepts$DIV <- as.factor(accepts$DIV)
accepts$EC_CARD <- as.factor(accepts$EC_CARD)
accepts$FINLOAN <- as.factor(accepts$FINLOAN)
accepts$LOCATION <- as.factor(accepts$LOCATION)
accepts$RESID <- as.factor(accepts$RESID)


# Create Training and Validation #
set.seed(12345)
train_id <- sample(seq_len(nrow(accepts)), size = floor(0.7*nrow(accepts)))

train <- accepts[train_id, ]
test <- accepts[-train_id, ]

# Information Value for Each Variable #
iv_summary <- smbinning.sumiv(df = train, y = "GB")

smbinning.sumiv.plot(iv_summary)
iv_summary # Only Continuous Variables >= 0.1 IV #

# Binning of Continuous Variables - IV >= 0.1 #
num_names <-  names(train[c("AGE", "INCOME", "TMJOB1", "PERS_H")]) # Gathering the names of numeric variables in data #

result_all_sig <- list() # Creating empty list to store all results #

for(i in 1:length(num_names)){
  check_res <- smbinning(df = train, y = "GB", x = num_names[i])
  
 if(check_res$iv < 0.1 | is.na(check_res$iv)) {
    next
  }
  else {
    result_all_sig[[num_names[i]]] <- check_res
  }
}

# Need to add CARDS

# Generating Variables of Bins and WOE Values #

for(i in 1:length(result_all_sig)) {
  train <- smbinning.gen(df = train, ivout = result_all_sig[[i]], chrname = paste(result_all_sig[[i]]$x, "_bin", sep = ""))
}

for (j in 1:length(result_all_sig)) {
  for (i in 1:nrow(train)) {
    bin_name <- paste(result_all_sig[[j]]$x, "_bin", sep = "")
    bin <- substr(train[[bin_name]][i], 2, 2)
    
    woe_name <- paste(result_all_sig[[j]]$x, "_WOE", sep = "")
    
    if(bin == 0) {
      bin <- dim(result_all_sig[[j]]$ivtable)[1] - 1
      train[[woe_name]][i] <- result_all_sig[[j]]$ivtable[bin, "WoE"]
    } else {
      train[[woe_name]][i] <- result_all_sig[[j]]$ivtable[bin, "WoE"]
    }
  }
}


# Build Initial Logistic Regression #
initial_score <- glm(data = train, GB ~ PERS_H_WOE + 
                       AGE_WOE + 
                       TMJOB1_WOE + INCOME_WOE, 
                     weights = train$X_freq_, family = "binomial")

summary(initial_score)

# Evaluate the Initial Model - Training Data #
train$pred <- initial_score$fitted.values

smbinning.metrics(dataset = train, prediction = "pred", actualclass = "GB", report = 1)
smbinning.metrics(dataset = train, prediction = "pred", actualclass = "GB", report = 0, plot = "ks")
smbinning.metrics(dataset = train, prediction = "pred", actualclass = "GB", report = 0, plot = "auc")

#Evaluate the Initial Model - Testing Data #
for(i in 1:length(result_all_sig)) {
  test <- smbinning.gen(df = test, ivout = result_all_sig[[i]], chrname = paste(result_all_sig[[i]]$x, "_bin", sep = ""))
}

for (j in 1:length(result_all_sig)) {
  for (i in 1:nrow(test)) {
    bin_name <- paste(result_all_sig[[j]]$x, "_bin", sep = "")
    bin <- substr(test[[bin_name]][i], 2, 2)
    
    woe_name <- paste(result_all_sig[[j]]$x, "_WOE", sep = "")
    
    if(bin == 0) {
      bin <- dim(result_all_sig[[j]]$ivtable)[1] - 1
      test[[woe_name]][i] <- result_all_sig[[j]]$ivtable[bin, "WoE"]
    } else {
      test[[woe_name]][i] <- result_all_sig[[j]]$ivtable[bin, "WoE"]
    }
  }
}

test$pred <- predict(initial_score, newdata=test, type='response')

smbinning.metrics(dataset = test, prediction = "pred", actualclass = "GB", report = 1)
smbinning.metrics(dataset = test, prediction = "pred", actualclass = "GB", report = 0, plot = "ks")
smbinning.metrics(dataset = test, prediction = "pred", actualclass = "GB", report = 0, plot = "auc")

# Add Scores to Initial Model #
pdo <- 50
score <- 500
odds <- 20
fact <- pdo/log(2)
os <- score - fact*log(odds)
var_names <- names(initial_score$coefficients[-1])

for(i in var_names) {
  beta <- initial_score$coefficients[i]
  beta0 <- initial_score$coefficients["(Intercept)"]
  nvar <- length(var_names)
  WOE_var <- train[[i]]
  points_name <- paste(str_sub(i, end = -4), "points", sep="")
  
  train[[points_name]] <- -(WOE_var*(beta) + (beta0/nvar))*fact + os/nvar
}

colini <- (ncol(train)-nvar + 1)
colend <- ncol(train)
train$Score <- rowSums(train[, colini:colend])

hist(train$Score, breaks = 50, xlim = c(475,725), main = "Distribution of Train Scores", xlab = "Score")

for(i in var_names) {
  beta <- initial_score$coefficients[i]
  beta0 <- initial_score$coefficients["(Intercept)"]
  nvar <- length(var_names)
  WOE_var <- test[[i]]
  points_name <- paste(str_sub(i, end = -4), "points", sep="")
  
  test[[points_name]] <- -(WOE_var*(beta) + (beta0/nvar))*fact + os/nvar
}

colini <- (ncol(test)-nvar + 1)
colend <- ncol(test)
test$Score <- rowSums(test[, colini:colend])

hist(test$Score, breaks = 50, xlim = c(475,725), main = "Distribution of Test Scores", xlab = "Score")

#accepts_scored <- rbind(train, test)
#hist(accepts_scored$Score, breaks = 50, xlim = c(475,725), main = "Distribution of Scores", xlab = "Score")

# Plotting Default by Score in Train Data #
cutpoints <- quantile(train$Score, probs = seq(0,1,0.10))
train$Score.QBin <- cut(train$Score, breaks=cutpoints, include.lowest=TRUE)
Default.QBin.train <- round(table(train$Score.QBin, train$GB)[,2]/rowSums(table(train$Score.QBin, train$GB))*100,2)

print(Default.QBin.train)

Default.QBin.train.pop <- round(table(train$Score.QBin, train$GB)[,2]/(table(train$Score.QBin, train$GB)[,2] + table(train$Score.QBin, train$GB)[,1]*4.75)*100,2)

print(Default.QBin.train.pop)

# Plotting Default by Score in Test Data #
cutpoints <- quantile(test$Score, probs = seq(0,1,0.10))
test$Score.QBin <- cut(test$Score, breaks=cutpoints, include.lowest=TRUE)
Default.QBin.test <- round(table(test$Score.QBin, test$GB)[,2]/rowSums(table(test$Score.QBin, test$GB))*100,2)

print(Default.QBin.test)

barplot(Default.QBin.test, 
        main = "Default Decile Plot", 
        xlab = "Deciles of Scorecard",
        ylab = "Default Rate (%)", ylim = c(0,100),
        col = saturation(heat.colors, scalefac(0.8))(10))
abline(h = 20.5, lwd = 2, lty = "dashed")
text(11.5, 23, "Current = 20.5%")

# Reject Inference - Clean & Prepare Reject Data #
for(i in names(result_all_sig)) {
  result_all_sig[[i]]$bands[1] <- min(c(accepts[[i]], rejects[[i]]), na.rm = TRUE)
  result_all_sig[[i]]$bands[length(result_all_sig[[i]]$bands)] <- max(c(accepts[[i]], rejects[[i]]), na.rm = TRUE)
}


rejects_scored <- rejects
for(i in 1:length(result_all_sig)) {
  rejects_scored <- smbinning.gen(df = rejects_scored, ivout = result_all_sig[[i]], chrname = paste(result_all_sig[[i]]$x, "_bin", sep = ""))
}

for (j in 1:length(result_all_sig)) {
  for (i in 1:nrow(rejects_scored)) {
    bin_name <- paste(result_all_sig[[j]]$x, "_bin", sep = "")
    bin <- substr(rejects_scored[[bin_name]][i], 2, 2)
    
    woe_name <- paste(result_all_sig[[j]]$x, "_WOE", sep = "")
    
    if(bin == 0) {
      bin <- dim(result_all_sig[[j]]$ivtable)[1] - 1
      rejects_scored[[woe_name]][i] <- result_all_sig[[j]]$ivtable[bin, "WoE"]
    } else {
      rejects_scored[[woe_name]][i] <- result_all_sig[[j]]$ivtable[bin, "WoE"]
    }
  }
}

pdo <- 50
score <- 500
odds <- 20
fact <- pdo/log(2)
os <- score - fact*log(odds)
var_names <- names(initial_score$coefficients[-1])

for(i in var_names) {
  beta <- initial_score$coefficients[i]
  beta0 <- initial_score$coefficients["(Intercept)"]
  nvar <- length(var_names)
  WOE_var <- rejects_scored[[i]]
  points_name <- paste(str_sub(i, end = -4), "points", sep="")
  
  rejects_scored[[points_name]] <- -(WOE_var*(beta) + (beta0/nvar))*fact + os/nvar
}

colini <- (ncol(rejects_scored)-nvar + 1)
colend <- ncol(rejects_scored)
rejects_scored$Score <- rowSums(rejects_scored[, colini:colend])

# Reject Inference - Fuzzy Augmentation #
rejects_scored$pred <- predict(initial_score, newdata=rejects_scored, type='response')

rejects_g <- rejects
rejects_b <- rejects

rejects_g$bad <- 0
rejects_g$weight_ar <- (1-rejects_scored$pred)*weight_rg
rejects_g$good <- 1

rejects_b$bad <- 1
rejects_b$weight_ar <- (rejects_scored$pred)*weight_rb
rejects_b$good <- 0

accepts$weight_ar <- ifelse(accepts$GB == 1, weight_ab, weight_ag)
comb_fuzz <- rbind(accepts[, !(names(accepts) == 'weight')], rejects_g, rejects_b)
