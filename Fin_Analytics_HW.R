# read in libraries
library(gmodels)
library(vcd)
library(smbinning)
library(dplyr)
library(stringr)
library(shades)
library(latticeExtra)
library(plotly)

# read in data
accept <- read.csv("C:\\Users\\Justin\\OneDrive - North Carolina State University\\Documents\\NC State\\IAA R\\Data\\accepted_customers.csv")

reject <- read.csv("C:\\Users\\Justin\\OneDrive - North Carolina State University\\Documents\\NC State\\IAA R\\Data\\rejected_customers.csv")

# create score card #######################################################################################################

# turn all categorical variables into factor
accept$BUREAU <- as.factor(accept$BUREAU)
accept$CAR <- as.factor(accept$CAR)
accept$CARDS <- as.factor(accept$CARDS)
accept$NAT <- as.factor(accept$NAT)
accept$PRODUCT <- as.factor(accept$PRODUCT)
accept$PROF <- as.factor(accept$PROF)
accept$REG <- as.factor(accept$REG)
accept$TEL <- as.factor(accept$TEL)

# binary variables into factors
accept$TEL <- as.factor(accept$DIV)
accept$TEL <- as.factor(accept$EC_CARD)
accept$TEL <- as.factor(accept$FINLOAN)
accept$TEL <- as.factor(accept$LOCATION)
accept$TEL <- as.factor(accept$RESID)
accept$TEL <- as.factor(accept$FINLOAN)

# split into train and test
set.seed(12345)

train_id <- sample(seq_len(nrow(accept)), size = floor(0.7*nrow(accept)))

train <- accept[train_id, ]
test <- accept[-train_id, ]


# need to bin all continuous variables

result_age <- smbinning(df = train, y = "GB", x = "AGE")
result_cash <- smbinning(df = train, y = "GB", x = "CASH")
result_children <- smbinning(df = train, y = "GB", x = "CHILDREN")
result_income <- smbinning(df = train, y = "GB", x = "INCOME")
result_loans <- smbinning(df = train, y = "GB", x = "LOANS")
result_nmbloan <- smbinning(df = train, y = "GB", x = "NMBLOAN")
result_pers_h <- smbinning(df = train, y = "GB", x = "PERS_H")
result_tmadd <- smbinning(df = train, y = "GB", x = "TMADD")
result_tmjob1 <- smbinning(df = train, y = "GB", x = "TMJOB1")

result_age$ivtable
#result_cash$ivtable # no significant splits
result_children$ivtable
result_income$ivtable
#result_loans$ivtable # no significant splits
#result_nmbloan$ivtable # Uniques values < 5
result_pers_h$ivtable
result_tmadd$ivtable
result_tmjob1$ivtable

# finding iv values
iv_summary <- smbinning.sumiv(df = train, y = "GB")
iv_summary
smbinning.sumiv.plot(iv_summary)

# subset vars that are significant
result_all_sig_bins <- train[c("GB","AGE", "INCOME", "TMJOB1", "CARDS", "PERS_H", "X_freq_")]

# create binned factor variables for the subset
#hold <- smbinning(df = train, y = "GB", x = "AGE")
# get output of smbinning into a list
result_all_sig <- list()
result_all_sig$age_smbin <- smbinning(df = train, y = "GB", x = "AGE")
result_all_sig$income_smbin <- smbinning(df = train, y = "GB", x = "INCOME")
result_all_sig$tmjob1_smbin <- smbinning(df = train, y = "GB", x = "TMJOB1")
result_all_sig$cards_smbin <- smbinning.factor(df = train, y = "GB", x = "CARDS")
result_all_sig$pers_h_smbin <- smbinning(df = train, y = "GB", x = "PERS_H")

# make all bins
result_all_sig_bins$age_bin <- smbinning.gen(df = train, ivout = smbinning(df = train, y = "GB", x = "AGE"), chrname = "age_bin")$age_bin
result_all_sig_bins$income_bin <- smbinning.gen(df = train, ivout = smbinning(df = train, y = "GB", x = "INCOME"), chrname = "income_bin")$income_bin
result_all_sig_bins$tmjob1_bin <- smbinning.gen(df = train, ivout = smbinning(df = train, y = "GB", x = "TMJOB1"), chrname = "tmjob1_bin")$tmjob1_bin
#result_all_sig_bins$cards_bin <- smbinning.gen(df = train, ivout = smbinning(df = train, y = "GB", x = "CARDS"), chrname = "cards_bin")$cards_bin
result_all_sig_bins$pers_h_bin <- smbinning.gen(df = train, ivout = smbinning(df = train, y = "GB", x = "PERS_H"), chrname = "pers_h_bin")$pers_h_bin

# find WOE for all variables

# age

for (i in 1:nrow(result_all_sig_bins)) {
  bin_name <- "age_bin"
  bin <- substr(result_all_sig_bins[[bin_name]][i], 2, 2)
  
  woe_name <- "age_WOE"
  
  if(bin == 0) {
    bin <- dim(result_all_sig$age_smbin$ivtable)[1] - 1
    result_all_sig_bins[[woe_name]][i] <- result_all_sig$age_smbin$ivtable[bin, "WoE"]
  } else {
    result_all_sig_bins[[woe_name]][i] <- result_all_sig$age_smbin$ivtable[bin, "WoE"]
  }
}

# income
for (i in 1:nrow(result_all_sig_bins)) {
  bin_name <- "income_bin"
  bin <- substr(result_all_sig_bins[[bin_name]][i], 2, 2)
  
  woe_name <- "income_WOE"
  
  if(bin == 0) {
    bin <- dim(result_all_sig$income_smbin$ivtable)[1] - 1
    result_all_sig_bins[[woe_name]][i] <- result_all_sig$income_smbin$ivtable[bin, "WoE"]
  } else {
    result_all_sig_bins[[woe_name]][i] <- result_all_sig$income_smbin$ivtable[bin, "WoE"]
  }
}


# tmjob1
for (i in 1:nrow(result_all_sig_bins)) {
  bin_name <- "tmjob1_bin"
  bin <- substr(result_all_sig_bins[[bin_name]][i], 2, 2)
  
  woe_name <- "tmjob1_WOE"
  
  if(bin == 0) {
    bin <- dim(result_all_sig$tmjob1_smbin$ivtable)[1] - 1
    result_all_sig_bins[[woe_name]][i] <- result_all_sig$tmjob1_smbin$ivtable[bin, "WoE"]
  } else {
    result_all_sig_bins[[woe_name]][i] <- result_all_sig$tmjob1_smbin$ivtable[bin, "WoE"]
  }
}

# cards
for (i in 1:nrow(result_all_sig_bins)) {
  bin_name <- "CARDS"
  bin <- substr(result_all_sig_bins[[bin_name]][i], 2, 2)
  
  woe_name <- "cards_WOE"
  
  if(bin == 0) {
    bin <- dim(result_all_sig$cards_smbin$ivtable)[1] - 1
    result_all_sig_bins[[woe_name]][i] <- result_all_sig$cards_smbin$ivtable[bin, "WoE"]
  } else {
    result_all_sig_bins[[woe_name]][i] <- result_all_sig$cards_smbin$ivtable[bin, "WoE"]
  }
}


# pers_h
for (i in 1:nrow(result_all_sig_bins)) {
  bin_name <- "pers_h_bin"
  bin <- substr(result_all_sig_bins[[bin_name]][i], 2, 2)
  
  woe_name <- "pers_h_WOE"
  
  if(bin == 0) {
    bin <- dim(result_all_sig$pers_h__smbin$ivtable)[1] - 1
    result_all_sig_bins[[woe_name]][i] <- result_all_sig$pers_h_smbin$ivtable[bin, "WoE"]
  } else {
    result_all_sig_bins[[woe_name]][i] <- result_all_sig$pers_h_smbin$ivtable[bin, "WoE"]
  }
}


# build model off WOE
initial_score <- glm(data = result_all_sig_bins, GB ~ age_WOE + 
                       income_WOE + 
                       tmjob1_WOE + 
                       #cards_WOE + temp take out cards
                       pers_h_WOE, 
                     weights = result_all_sig_bins$weight, family = "binomial")

#metrics

summary(initial_score)


train$pred <- initial_score$fitted.values

smbinning.metrics(dataset = train, prediction = "pred", actualclass = "GB", report = 1)

# score buckets with default rate

# create scoring
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
  WOE_var <- result_all_sig_bins[[i]]
  points_name <- paste(str_sub(i, end = -4), "points", sep = "")
  result_all_sig_bins[[points_name]] <- -(WOE_var*(beta) + (beta0/nvar))*fact + os/nvar
}

colini <- (ncol(result_all_sig_bins)-nvar + 1)
colend <- ncol(result_all_sig_bins)
result_all_sig_bins$Score <- rowSums(result_all_sig_bins[, colini:colend])

# plot default rate

cutpoints <- quantile(result_all_sig_bins$Score, probs = seq(0,1,0.10))
result_all_sig_bins$Score.QBin <- cut(result_all_sig_bins$Score, breaks=cutpoints, include.lowest=TRUE)
Default.QBin.test <- round(table(result_all_sig_bins$Score.QBin, result_all_sig_bins$GB)[,2]/rowSums(table(result_all_sig_bins$Score.QBin, result_all_sig_bins$GB))*100,2)

print(Default.QBin.test)

barplot(Default.QBin.test, 
        main = "Default Decile Plot", 
        xlab = "Deciles of Scorecard",
        ylab = "Default Rate (%)", ylim = c(0,80),
        col = saturation(heat.colors, scalefac(0.8))(10))
abline(h = 49.9, lwd = 2, lty = "dashed")
text(11.5, 52, "Current = 49.9%")


# reject inference - clean all reject data #################################################################################################################

for(i in names(result_all_sig)) {
  result_all_sig[[i]]$bands[1] <- min(c(accept[[i]], reject[[i]]), na.rm = TRUE)
  result_all_sig[[i]]$bands[length(result_all_sig[[i]]$bands)] <- max(c(accepts[[i]], rejects[[i]]), na.rm = TRUE)
}

for(i in 1:length(rejects[["ltv"]])){
  rejects[["ltv"]][is.na(rejects[["ltv"]])] <- floor(mean(rejects[["ltv"]], na.rm = TRUE))
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

# hard augmentation

rejects_scored$pred <- predict(initial_score, newdata = reject,
                               type = 'response')

rejects$bad <- as.numeric(rejects_scored$pred > 0.0545)
rejects$weight <- ifelse(rejects$bad == 1, 2.80, 0.59)
rejects$good <- abs(rejects$bad - 1)
comb_hard <- rbind(accepts, rejects)

