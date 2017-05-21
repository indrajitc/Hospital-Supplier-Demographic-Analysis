# Hospital Supplier Analysis
# Reading in Data
library(plyr)
path <- "C:/Users/Owner/Documents/Grad School/MS Statistics/Spring 2017/Interpretation of Data II/Final Project/Final Project Resources/proj/proj/CLPS_RD.csv"
CLPS = read.csv(path)
# Method of Getting Date (in Months 1-36) in vector poMonth 
poMonth <- vector(mode="numeric",length=nrow(CLPS))
for(i in 1:nrow(CLPS)){
  indexMonth <- as.numeric(ldply(strsplit(as.character(ldply(strsplit(as.character(CLPS[i,8]), split = " "))[[1]]), split = "/"))[[1]]) 
  indexYear <- as.numeric(ldply(strsplit(as.character(ldply(strsplit(as.character(CLPS[i,8]), split = " "))[[1]]), split = "/"))[[3]]) 
  poMonth[i] = (indexMonth+12*indexYear)-(1+12*2010)+1 
}
CLPS1 <- cbind(CLPS,poMonth)

# Credo Investigation (Creation of New Covariates)
# Overall Credo Percentages for all Months
credo_table <- table(CLPS1$poMonth,CLPS1$Credo.Spend.Ind)
overall_credo_average <- sum(credo_table[,2])/sum(credo_table[,1],credo_table[,2])
month_credo_splits <- vector(mode="numeric",length=nrow(credo_table))
credo_above_average <- vector(mode="numeric",length=nrow(credo_table))
for(i in 1:nrow(credo_table)){
  month_credo_splits[i] <- credo_table[i,2]/sum(credo_table[i,1:2]) 
  if(month_credo_splits[i]>overall_credo_average){
    credo_above_average[i] <- 1
  }
}
credo_table <- cbind(credo_table,month_credo_splits,credo_above_average)
print(credo_table)
plot(credo_table[,3],main="Credo Percentage by Month",xlab="Month (1-36)",
     ylab="Total Credo Percentage",type="b")
abline(a=overall_credo_average,b=0,col="blue")

#Overall Credo Percentage split by Company:
credo_table_company <- table(CLPS1$Company,CLPS1$Credo.Spend.Ind)
credo_company_splits <- vector(mode="numeric",length=nrow(credo_table_company))
credo_company_diff <- vector(mode="numeric",length=nrow(credo_table_company))
credo_company_abs_diff <- vector(mode="numeric",length=nrow(credo_table_company))
for(i in 1:nrow(credo_table_company)){
  credo_company_splits[i] <- credo_table_company[i,2]/sum(credo_table_company[i,1:2]) 
  credo_company_diff[i] <- credo_company_splits[i] - overall_credo_average
  credo_company_abs_diff[i] <- abs(credo_company_diff[i])
}
credo_company_splits <- cbind(credo_table_company,credo_company_splits,credo_company_diff,credo_company_abs_diff)
sorted_credo_company_splits <- credo_company_splits[order(credo_company_splits[,5],decreasing=TRUE),]
print(sorted_credo_company_splits)

# Category Names Credo
categ_credo_table <- table(CLPS1$Category.Name,CLPS1$Credo.Spend.Ind)
categ_credo_splits <- vector(mode="numeric",length=nrow(categ_credo_table))
categ_credo_diff <- vector(mode="numeric",length=nrow(categ_credo_table))
for(i in 1:nrow(categ_credo_table)){
  categ_credo_splits[i] <- categ_credo_table[i,2]/sum(categ_credo_table[i,1:2]) 
  categ_credo_diff[i] <- categ_credo_splits[i] - overall_credo_average
}
categ_credo_table <- cbind(categ_credo_table,categ_credo_splits,categ_credo_diff)
print(categ_credo_table)

# Subcategory Names Credo
subcateg_credo_table <- table(CLPS1$Subcategory.Name,CLPS1$Credo.Spend.Ind)
subcateg_credo_splits <- vector(mode="numeric",length=nrow(subcateg_credo_table))
subcateg_credo_diff <- vector(mode="numeric",length=nrow(subcateg_credo_table))
subcateg_credo_absdiff <- vector(mode="numeric",length=nrow(subcateg_credo_table))
for(i in 1:nrow(subcateg_credo_table)){
  subcateg_credo_splits[i] <- subcateg_credo_table[i,2]/sum(subcateg_credo_table[i,1:2]) 
  subcateg_credo_diff[i] <- subcateg_credo_splits[i] - overall_credo_average
  subcateg_credo_absdiff[i] <- abs(subcateg_credo_diff[i])
}
subcateg_credo_table <- cbind(subcateg_credo_table,subcateg_credo_splits,subcateg_credo_diff,subcateg_credo_absdiff)
sorted_subcateg_credo_table <- subcateg_credo_table[order(subcateg_credo_table[,5],decreasing=TRUE),]
print(sorted_subcateg_credo_table)

# Business Size Credo 
bus_credo_table <- table(CLPS1$Business.Size.Code,CLPS1$Credo.Spend.Ind)
bus_credo_splits <- vector(mode="numeric",length=nrow(bus_credo_table))
bus_credo_diff <- vector(mode="numeric",length=nrow(bus_credo_table))
bus_credo_absdiff <- vector(mode="numeric",length=nrow(bus_credo_table))
for(i in 1:nrow(bus_credo_table)){
  bus_credo_splits[i] <- bus_credo_table[i,2]/sum(bus_credo_table[i,1:2]) 
  bus_credo_diff[i] <- bus_credo_splits[i] - overall_credo_average
  bus_credo_absdiff[i] <- abs(bus_credo_diff[i])
}
bus_credo_table <- cbind(bus_credo_table,bus_credo_splits,bus_credo_diff,bus_credo_absdiff)
sorted_bus_credo_table <- bus_credo_table[order(bus_credo_table[,5],decreasing=TRUE),]
print(sorted_bus_credo_table)

# JNJ Site code Credo
site_credo_table <- table(CLPS1$JNJ.Site.Code,CLPS1$Credo.Spend.Ind)
site_credo_splits <- vector(mode="numeric",length=nrow(site_credo_table))
site_credo_diff <- vector(mode="numeric",length=nrow(site_credo_table))
site_credo_absdiff <- vector(mode="numeric",length=nrow(site_credo_table))
site_credo_totals <- vector(mode="numeric",length=nrow(site_credo_table))
large_group <- vector(mode="numeric",length=nrow(site_credo_table)) 
for(i in 1:nrow(site_credo_table)){
  site_credo_splits[i] <- site_credo_table[i,2]/sum(site_credo_table[i,1:2]) 
  site_credo_diff[i] <- site_credo_splits[i] - overall_credo_average
  site_credo_absdiff[i] <- abs(site_credo_diff[i])
  site_credo_totals[i] <- sum(site_credo_table[i,1:2])
  if(site_credo_totals[i] > 999){
    large_group[i] <- 1
  }
}
site_credo_table <- cbind(site_credo_table,site_credo_splits,site_credo_diff,site_credo_absdiff,large_group)
sorted_site_credo_table <- site_credo_table[order(site_credo_table[,5],decreasing=TRUE),]
sorted_site_credo_table <- sorted_site_credo_table[sorted_site_credo_table[,6]>0,] 
head(sorted_site_credo_table)

# SB Investigation (Creation of New Covariates)

# Overall Small Business Percentages for all Months
sb_table <- table(CLPS1$poMonth,CLPS1$Business.Size.Code)
overall_sb_average <- sum(sb_table[,4])/sum(sb_table[,1:4])
month_sb_splits <- vector(mode="numeric",length=nrow(sb_table))
sb_above_average <- vector(mode="numeric",length=nrow(sb_table))
for(i in 1:nrow(sb_table)){
  month_sb_splits[i] <- sb_table[i,4]/sum(sb_table[i,1:4])
  if(month_sb_splits[i]>overall_sb_average){
    sb_above_average[i] <- 1
  }
}
sb_table <- cbind(sb_table,month_sb_splits,sb_above_average)
print(sb_table)
plot(sb_table[,5],main="Small Business Percentage by Month",xlab="Month (1-36)",
     ylab="Total Small Business Percentage",type="b")
abline(a=overall_sb_average,b=0,col="blue")

#Overall Small Business Percentage split by Company:
sb_company_table <- table(CLPS1$Company,CLPS1$Business.Size.Code)
sb_company_splits <- vector(mode="numeric",length=nrow(sb_company_table))
sb_company_diff <- vector(mode="numeric",length=nrow(sb_company_table))
sb_company_abs_diff <- vector(mode="numeric",length=nrow(sb_company_table))
for(i in 1:nrow(sb_company_table)){
  sb_company_splits[i] <- sb_company_table[i,4]/sum(sb_company_table[i,1:4])
  sb_company_diff[i] <- sb_company_splits[i] - overall_sb_average
  sb_company_abs_diff[i] <- abs(sb_company_diff[i])
}
sb_company_table <- cbind(sb_company_table,sb_company_splits,sb_company_diff,sb_company_abs_diff) 
sorted_sb_company_table <- sb_company_table[order(sb_company_table[,7],decreasing=TRUE),]
print(sorted_sb_company_table)

# Category Names SB
categ_sb_table <- table(CLPS1$Category.Name,CLPS1$Business.Size.Code)
categ_sb_splits <- vector(mode="numeric",length=nrow(categ_sb_table))
categ_sb_diff <- vector(mode="numeric",length=nrow(categ_sb_table))
categ_sb_absdiff <- vector(mode="numeric",length=nrow(categ_sb_table)) 
for(i in 1:nrow(categ_sb_table)){
  categ_sb_splits[i] <- categ_sb_table[i,4]/sum(categ_sb_table[i,1:4])
  categ_sb_diff[i] <- categ_sb_splits[i] - overall_sb_average
  categ_sb_absdiff[i] <- abs(categ_sb_diff[i])
}
categ_sb_table <- cbind(categ_sb_table,categ_sb_splits,categ_sb_diff,categ_sb_absdiff)
sorted_categ_sb_table <- categ_sb_table[order(categ_sb_table[,7],decreasing=TRUE),]
print(sorted_categ_sb_table)

# Subcategory Names SB
subcateg_sb_table <- table(CLPS1$Subcategory.Name,CLPS1$Business.Size.Code)
subcateg_sb_splits <- vector(mode="numeric",length=nrow(subcateg_sb_table))
subcateg_sb_diff <- vector(mode="numeric",length=nrow(subcateg_sb_table))
subcateg_sb_absdiff <- vector(mode="numeric",length=nrow(subcateg_sb_table))
subcateg_sb_totals <- vector(mode="numeric",length=nrow(subcateg_sb_table))
large_group <- vector(mode="numeric",length=nrow(subcateg_sb_table)) 
for(i in 1:nrow(subcateg_sb_table)){
  subcateg_sb_splits[i] <- subcateg_sb_table[i,4]/sum(subcateg_sb_table[i,1:4]) 
  subcateg_sb_diff[i] <- subcateg_sb_splits[i] - overall_sb_average
  subcateg_sb_absdiff[i] <- abs(subcateg_sb_diff[i])
  subcateg_sb_totals[i] <- sum(subcateg_sb_table[i,1:4])
  if(subcateg_sb_totals[i]>999){
    large_group[i] <- 1
  }
}
subcateg_sb_table <- cbind(subcateg_sb_table,subcateg_sb_splits,subcateg_sb_diff,subcateg_sb_absdiff,large_group)
sorted_subcateg_sb_table <- subcateg_sb_table[order(subcateg_sb_table[,7],decreasing=TRUE),]
sorted_subcateg_sb_table <- sorted_subcateg_sb_table[sorted_subcateg_sb_table[,8]>0,] 
print(sorted_subcateg_sb_table)

# Credo and SB
credo_sb_table <- table(CLPS1$Credo.Spend.Ind,CLPS1$Business.Size.Code)
credo_sb_splits <- vector(mode="numeric",length=nrow(credo_sb_table))
credo_sb_diff <- vector(mode="numeric",length=nrow(credo_sb_table))
credo_sb_absdiff <- vector(mode="numeric",length=nrow(credo_sb_table))
for(i in 1:nrow(credo_sb_table)){
  credo_sb_splits[i] <- credo_sb_table[i,4]/sum(credo_sb_table[i,1:4]) 
  credo_sb_diff[i] <- credo_sb_splits[i] - overall_sb_average
  credo_sb_absdiff[i] <- abs(credo_sb_diff[i])
}
credo_sb_table <- cbind(credo_sb_table,credo_sb_splits,credo_sb_diff,credo_sb_absdiff)
sorted_credo_sb_table <- credo_sb_table[order(credo_sb_table[,7],decreasing=TRUE),]
print(sorted_credo_sb_table)

# JNJ Site Code  SB
site_sb_table <- table(CLPS1$JNJ.Site.Code,CLPS1$Business.Size.Code)
site_sb_splits <- vector(mode="numeric",length=nrow(site_sb_table))
site_sb_diff <- vector(mode="numeric",length=nrow(site_sb_table))
site_sb_absdiff <- vector(mode="numeric",length=nrow(site_sb_table))
site_sb_totals <- vector(mode="numeric",length=nrow(site_sb_table))
large_group <- vector(mode="numeric",length=nrow(site_sb_table)) 
for(i in 1:nrow(site_sb_table)){
  site_sb_splits[i] <- site_sb_table[i,4]/sum(site_sb_table[i,1:4]) 
  site_sb_diff[i] <- site_sb_splits[i] - overall_sb_average
  site_sb_absdiff[i] <- abs(site_sb_diff[i])
  site_sb_totals[i] <- sum(site_sb_table[i,1:4])
  if(site_sb_totals[i]>999){
    large_group[i] <- 1
  }
}
site_sb_table <- cbind(site_sb_table,site_sb_splits,site_sb_diff,site_sb_absdiff,large_group)
sorted_site_sb_table <- site_sb_table[order(site_sb_table[,7],decreasing=TRUE),]
sorted_site_sb_table <- sorted_site_sb_table[sorted_site_sb_table[,8]>0,] 
head(sorted_site_sb_table)

# GLM Analysis with Credo Covariates
library(glmnet)

# Creation of Matrix for Credo Covariates
credoMatrix <- matrix(data=0,nrow=nrow(CLPS1),ncol=15)
rows <- nrow(CLPS1)
for(i in 1:rows){
  credoMatrix[i,1] <- CLPS1$poMonth[i]
  if(CLPS1$Credo.Spend.Ind[i]=="Y"){
    credoMatrix[i,2] <- 1
  }
  if(CLPS1$Company[i]=="HCS"){
    credoMatrix[i,3] <- 1
  }
  if(CLPS1$Company[i]=="Nutritionals"){
    credoMatrix[i,4] <- 1
  }
  if(CLPS1$Company[i]=="PR"){
    credoMatrix[i,5] <- 1
  }
  if(CLPS1$Category.Name[i]=="Consulting- Labor and Professional Services"){
    credoMatrix[i,6] <- 1
  }
  if(CLPS1$Subcategory.Name[i]=="Managed Service Provider"){
    credoMatrix[i,7] <- 1
  }
  if(CLPS1$Subcategory.Name[i]=="Professional Services"){
    credoMatrix[i,8] <- 1
  }
  if(CLPS1$Subcategory.Name[i]=="Temporary Staffing"){
    credoMatrix[i,9] <- 1
  }
  if(CLPS1$Business.Size.Code[i]=="D"){
    credoMatrix[i,10] <- 1
  }
  if(CLPS1$Business.Size.Code[i]=="S"){
    credoMatrix[i,11] <- 1
  }
  if(CLPS1$JNJ.Site.Code[i] == "171013"){
    credoMatrix[i,12] <- 1
  }
  if(CLPS1$JNJ.Site.Code[i] == "141018"){
    credoMatrix[i,13] <- 1
  }
  if(CLPS1$JNJ.Site.Code[i] == "198001"){
    credoMatrix[i,14] <- 1
  }
  if(CLPS1$JNJ.Site.Code[i] == "165999"){
    credoMatrix[i,15] <- 1
  }
}
credoMatrix1 <- credoMatrix

#Creation of Matrix that Aggregates the above results over 36 months
#Month given, 13 covariates given as proportion of occurence in previous month
aggMonths <- matrix(data=0,nrow=36,ncol=18)
for (i in 4:nrow(aggMonths)){
  aggMonths[i,1] <- i
  currentMonth <- credoMatrix1[credoMatrix1[,1]==i,] # Matrix for Current Month
  priorOneMonth <- credoMatrix1[credoMatrix1[,1]==i-1,] # Matrix for 1 Month Prior
  priorTwoMonth <- credoMatrix1[credoMatrix1[,1]==i-2,] # Matrix for 2 Months Prior
  priorThreeMonth <- credoMatrix1[credoMatrix1[,1]==i-3,] # Matrix for 3 Months Prior
  for(j in 3:15){
    aggMonths[i,j] <- sum(priorOneMonth[,j])/nrow(priorOneMonth) 
  }
  aggMonths[i,2] <- sum(currentMonth[,2])/nrow(currentMonth) 
  aggMonths[i,16] <- sum(priorOneMonth[,2])/nrow(priorOneMonth)
  aggMonths[i,17] <- sum(priorTwoMonth[,2])/nrow(priorTwoMonth)
  aggMonths[i,18] <- sum(priorThreeMonth[,2])/nrow(priorThreeMonth)
}

#Matrix discarding first 3 observations
credoMonthMatrix <- aggMonths[4:36,]
#Creation of full covariate matrix for month 37 
#For predictions use subset of month37 vector as x value 
month37 <- vector(mode="numeric",length=ncol(credoMonthMatrix))
month37[1] <- 37
priorOneMonth <- credoMatrix1[credoMatrix1[,1]==36,] 
priorTwoMonth <- credoMatrix1[credoMatrix1[,1]==35,] 
priorThreeMonth <- credoMatrix1[credoMatrix1[,1]==34,]
for(j in 3:15){
  month37[j] <- sum(priorOneMonth[,j])/nrow(priorOneMonth) 
}
month37[16] <- sum(priorOneMonth[,2])/nrow(priorOneMonth)
month37[17] <- sum(priorTwoMonth[,2])/nrow(priorTwoMonth)
month37[18] <- sum(priorThreeMonth[,2])/nrow(priorThreeMonth)

# Covariate Matrix including covariates for 37th Month
credoGlmMatrix <- rbind(credoMonthMatrix,month37)

# GLM Model with all Covariates
y <- credoGlmMatrix[,2]
x <- credoGlmMatrix[,3:18]
fit <- glm(y~x,family=gaussian())
sumfit <- summary(fit)
predictfit <- predict(fit, type="response")
predictedval <- as.numeric(predictfit[34])
print(predictedval)
actualfit <- y[1:33]
plot(y=predictfit,x=4:37,type="b",col="red",xlim=c(0,40), ylim=c(0.02, 0.16),
     main="Credo Percentage by Month (Full Model)",xlab="Month (1-37)",
     ylab="Total Credo Percentage")
lines(y=actualfit,x=4:36,type = "b",col="green")
points(x=37,y=predictedval, type="p", col="blue")
legend(c("topright"),c("Predicted","Actual","Predicted 37th"),lty=c(1,1),lwd=c(2.5,2.5),
       col=c("red","green", "blue"),cex=0.75) 

# GLM Model with Only Time Series Components 
y <- credoGlmMatrix[,2]
x <- credoGlmMatrix[,16:18]
fit <- glm(y~x,family=gaussian())
sumfit <- summary(fit)
predictfit <- predict(fit, type="response")
predictedval <- as.numeric(predictfit[34])
print(predictedval)
actualfit <- y[1:33]
plot(y=predictfit,x=4:37,type="b",col="red",xlim=c(0,40), ylim=c(0.04, 0.16),
     main="Credo Percentage by Month (Time Series Model)",xlab="Month (1-37)",
     ylab="Total Credo Percentage")
lines(y=actualfit,x=4:36,type = "b",col="green")
points(x=37,y=predictedval, type="p", col="blue")
legend(c("topright"),c("Predicted","Actual","Predicted 37th"),lty=c(1,1),lwd=c(2.5,2.5),
       col=c("red","green", "blue"),cex=0.75) 

# GLM Model Chosen by GLM Net 
y <- credoGlmMatrix[,2]
x <- credoGlmMatrix[,3:18]
cvfit <- cv.glmnet(x=x, y=y, family = "gaussian")
sigcoef <- as.matrix(coef(cvfit, s = "lambda.min"))
chosencoef <- which(sigcoef!=0)
for (i in 1:length(chosencoef)){
  chosencoef[i] <- chosencoef[i] - 1
}
if(chosencoef[1]==0){
  chosencoef <- chosencoef[2:length(chosencoef)]
}
print(paste0("The Significant Variables Chosen by GLMNet are ", chosencoef))
newx <- matrix(data=1,nrow=nrow(x),ncol=1)
for(i in 1:length(chosencoef)){
  if(chosencoef[i]<14){
    newx <- cbind(newx,x[,chosencoef[i]])
  }
}
newx <- cbind(newx,x[,14:16]) # Ensures that all times series terms are in
newx <- newx[,2:ncol(newx)]
fit <- glm(y~newx,family=gaussian())
sumfit <- summary(fit)
predictfit <- predict(fit, type="response")
predictedval <- as.numeric(predictfit[34])
print(predictedval)
actualfit <- y[1:33]
plot(y=predictfit,x=4:37,type="b",col="red",xlim=c(0,40), ylim=c(0.04, 0.16),
     main="Credo Percentage by Month (GLM Net Model)",xlab="Month (1-37)",
     ylab="Total Credo Percentage")
lines(y=actualfit,x=4:36,type = "b",col="green")
points(x=37,y=predictedval, type="p", col="blue")
legend(c("topright"),c("Predicted","Actual","Predicted 37th"),lty=c(1,1),lwd=c(2.5,2.5),
       col=c("red","green", "blue"),cex=0.75)

# GLM Analysis with SB Covariates (with original 12 Covariates)
# Creation of Matrix for SB Covariates

sbMatrix <- matrix(data=0,nrow=nrow(CLPS1),ncol=14)
rows <- nrow(CLPS1)
for(i in 1:rows){
  sbMatrix[i,1] <- CLPS1$poMonth[i]
  if(CLPS1$Business.Size.Code[i]=="S"){
    sbMatrix[i,2] <- 1
  }
  if(CLPS1$Company[i]=="PR"){
    sbMatrix[i,3] <- 1
  }
  if(CLPS1$Company[i]=="J&J Medical"){
    sbMatrix[i,4] <- 1
  }
  if(CLPS1$Company[i]=="Global Ortropaedics"){
    sbMatrix[i,5] <- 1
  }
  if(CLPS1$Subcategory.Name[i]=="Clinical - Data Management Technology"){
    sbMatrix[i,6] <- 1
  }
  if(CLPS1$Subcategory.Name[i]=="Clinical - R&D Medical Testing (non-lab)"){
    sbMatrix[i,7] <- 1
  }
  if(CLPS1$Subcategory.Name[i]=="Product Development - Engineering & Testing"){
    sbMatrix[i,8] <- 1
  }
  if(CLPS1$Credo.Spend.Ind[i]=="Y"){
    sbMatrix[i,9] <- 1
  }
  if(CLPS1$JNJ.Site.Code[i]=="165999"){
    sbMatrix[i,10] <- 1
  }
  if(CLPS1$JNJ.Site.Code[i]=="129001"){
    sbMatrix[i,11] <- 1
  }
  if(CLPS1$JNJ.Site.Code[i] == "620301"){
    sbMatrix[i,12] <- 1
  }
  if(CLPS1$JNJ.Site.Code[i] == "445501"){
    sbMatrix[i,13] <- 1
  }
  if(CLPS1$JNJ.Site.Code[i] == "151001"){
    sbMatrix[i,14] <- 1
  }
}
sbMatrix1 <- sbMatrix

#Creation of Matrix that Aggregates the above results over 36 months
#Month given, 12 covariates given as proportion of occurence in previous month
aggMonths <- matrix(data=0,nrow=36,ncol=17)
for (i in 4:nrow(aggMonths)){
  aggMonths[i,1] <- i
  currentMonth <- sbMatrix1[sbMatrix1[,1]==i,] # Matrix for Current Month
  priorOneMonth <- sbMatrix1[sbMatrix1[,1]==i-1,] # Matrix for 1 Month Prior
  priorTwoMonth <- sbMatrix1[sbMatrix1[,1]==i-2,] # Matrix for 2 Months Prior
  priorThreeMonth <- sbMatrix1[sbMatrix1[,1]==i-3,] # Matrix for 3 Months Prior
  for(j in 3:14){
    aggMonths[i,j] <- sum(priorOneMonth[,j])/nrow(priorOneMonth) 
  }
  aggMonths[i,2] <- sum(currentMonth[,2])/nrow(currentMonth) 
  aggMonths[i,15] <- sum(priorOneMonth[,2])/nrow(priorOneMonth)
  aggMonths[i,16] <- sum(priorTwoMonth[,2])/nrow(priorTwoMonth)
  aggMonths[i,17] <- sum(priorThreeMonth[,2])/nrow(priorThreeMonth)
}

#Matrix discarding first 3 observations
sbMonthMatrix <- aggMonths[4:36,]

#Creation of full covariate matrix for month 37 
#For predictions use subset of month37 vector as x value 
month37 <- vector(mode="numeric",length=ncol(sbMonthMatrix))
month37[1] <- 37
priorOneMonth <- sbMatrix1[sbMatrix1[,1]==36,] 
priorTwoMonth <- sbMatrix1[sbMatrix1[,1]==35,] 
priorThreeMonth <- sbMatrix1[sbMatrix1[,1]==34,]
for(j in 3:14){
  month37[j] <- sum(priorOneMonth[,j])/nrow(priorOneMonth) 
}
month37[15] <- sum(priorOneMonth[,2])/nrow(priorOneMonth)
month37[16] <- sum(priorTwoMonth[,2])/nrow(priorTwoMonth)
month37[17] <- sum(priorThreeMonth[,2])/nrow(priorThreeMonth)

# Covariate Matrix including covariates for 37th Month
sbGlmMatrix <- rbind(sbMonthMatrix,month37)

# GLM Model with all Covariates
y <- sbGlmMatrix[,2]
x <- sbGlmMatrix[,3:17]
fit <- glm(y~x,family=gaussian())
sumfit <- summary(fit)
predictfit <- predict(fit, type="response")
predictedval <- as.numeric(predictfit[34])
actualfit <- y[1:33]
plot(predictfit,type="b",col="red",xlim=c(0,35), ylim=c(0.13, 0.25))
lines(actualfit,type = "b",col="green")
points(x=34,y=predictedval, type="p", col="blue")
legend(c("topright"),c("Predicted","Actual","Predicted 37th"),lty=c(1,1),lwd=c(2.5,2.5),
       col=c("red","green", "blue"),cex=0.75) 

# GLM Model with Only Time Series Components 
y <- sbGlmMatrix[,2]
x <- sbGlmMatrix[,15:17]
fit <- glm(y~x,family=gaussian())
sumfit <- summary(fit)
predictfit <- predict(fit, type="response")
predictedval <- as.numeric(predictfit[34])
print(predictedval)
actualfit <- y[1:33]
plot(predictfit,type="b",col="red",xlim=c(0,35), ylim=c(0.14, 0.22))
lines(actualfit,type = "b",col="green")
points(x=34,y=predictedval, type="p", col="blue")

# GLM Model Chosen by GLM Net 
y <- sbGlmMatrix[,2]
x <- sbGlmMatrix[,3:17]
cvfit <- cv.glmnet(x=x, y=y, family = "gaussian")
sigcoef <- as.matrix(coef(cvfit, s = "lambda.min"))
chosencoef <- which(sigcoef!=0)
for (i in 1:length(chosencoef)){
  chosencoef[i] <- chosencoef[i] - 1
}
if(chosencoef[1]==0){
  chosencoef <- chosencoef[2:length(chosencoef)]
}
print(paste0("The Significant Variables Chosen by GLMNet are ", chosencoef))
newx <- matrix(data=1,nrow=nrow(x),ncol=1)
for(i in 1:length(chosencoef)){
  if(chosencoef[i]<13){
    newx <- cbind(newx,x[,chosencoef[i]])
  }
}
newx <- cbind(newx,x[,13:15]) # Ensures that all times series terms are in
newx <- newx[,2:ncol(newx)]
fit <- glm(y~newx,family=gaussian())
sumfit <- summary(fit)
predictfit <- predict(fit, type="response")
predictedval <- as.numeric(predictfit[34])
print(predictedval)
actualfit <- y[1:33]
plot(predictfit,type="b",col="red",xlim=c(0,35), ylim=c(0.14, 0.22))
lines(actualfit,type = "b",col="green")
points(x=34,y=predictedval, type="p", col="blue")

# GLM Analysis with SB Covariates (with 19 Covariates)
# Creation of Matrix for SB Covariates

sbMatrix1 <- matrix(data=0,nrow=nrow(CLPS1),ncol=21)
rows <- nrow(CLPS1)
for(i in 1:rows){
  sbMatrix1[i,1] <- CLPS1$poMonth[i]
  if(CLPS1$Business.Size.Code[i]=="S"){
    sbMatrix1[i,2] <- 1
  }
  if(CLPS1$Company[i]=="PR"){
    sbMatrix1[i,3] <- 1
  }
  if(CLPS1$Company[i]=="J&J Medical"){
    sbMatrix1[i,4] <- 1
  }
  if(CLPS1$Company[i]=="Global Ortropaedics"){
    sbMatrix1[i,5] <- 1
  }
  if(CLPS1$Subcategory.Name[i]=="Clinical - Data Management Technology"){
    sbMatrix1[i,6] <- 1
  }
  if(CLPS1$Subcategory.Name[i]=="Clinical - R&D Medical Testing (non-lab)"){
    sbMatrix1[i,7] <- 1
  }
  if(CLPS1$Subcategory.Name[i]=="Product Development - Engineering & Testing"){
    sbMatrix1[i,8] <- 1
  }
  if(CLPS1$Subcategory.Name[i]=="Professional Services"){
    sbMatrix1[i,9] <- 1
  }
  if(CLPS1$Subcategory.Name[i]=="Training and Development"){
    sbMatrix1[i,10] <- 1
  }
  if(CLPS1$Subcategory.Name[i]=="R&D Lab Supplies - Equipment and Instrumentation"){
    sbMatrix1[i,11] <- 1
  }
  if(CLPS1$Subcategory.Name[i]=="Product Development - Product Design & Prototyping"){
    sbMatrix1[i,12] <- 1
  }
  if(CLPS1$Subcategory.Name[i]=="Consulting"){
    sbMatrix1[i,13] <- 1
  }
  if(CLPS1$Subcategory.Name[i]=="Clinical - Clinical Lab Services"){
    sbMatrix1[i,14] <- 1
  }
  if(CLPS1$Subcategory.Name[i]=="Memberships & Subscriptions"){
    sbMatrix1[i,15] <- 1
  }
  if(CLPS1$Credo.Spend.Ind[i]=="Y"){
    sbMatrix1[i,16] <- 1
  }
  if(CLPS1$JNJ.Site.Code[i]=="165999"){
    sbMatrix1[i,17] <- 1
  }
  if(CLPS1$JNJ.Site.Code[i]=="129001"){
    sbMatrix1[i,18] <- 1
  }
  if(CLPS1$JNJ.Site.Code[i] == "620301"){
    sbMatrix1[i,19] <- 1
  }
  if(CLPS1$JNJ.Site.Code[i] == "445501"){
    sbMatrix1[i,20] <- 1
  }
  if(CLPS1$JNJ.Site.Code[i] == "151001"){
    sbMatrix1[i,21] <- 1
  }
}
sbMatrix2 <- sbMatrix1
#Creation of Matrix that Aggregates the above results over 36 months
#Month given, 19 covariates given as proportion of occurence in previous month
aggMonths <- matrix(data=0,nrow=36,ncol=24)
for (i in 4:nrow(aggMonths)){
  aggMonths[i,1] <- i
  currentMonth <- sbMatrix1[sbMatrix1[,1]==i,] # Matrix for Current Month
  priorOneMonth <- sbMatrix1[sbMatrix1[,1]==i-1,] # Matrix for 1 Month Prior
  priorTwoMonth <- sbMatrix1[sbMatrix1[,1]==i-2,] # Matrix for 2 Months Prior
  priorThreeMonth <- sbMatrix1[sbMatrix1[,1]==i-3,] # Matrix for 3 Months Prior
  for(j in 3:21){
    aggMonths[i,j] <- sum(priorOneMonth[,j])/nrow(priorOneMonth) 
  }
  aggMonths[i,2] <- sum(currentMonth[,2])/nrow(currentMonth) 
  aggMonths[i,22] <- sum(priorOneMonth[,2])/nrow(priorOneMonth)
  aggMonths[i,23] <- sum(priorTwoMonth[,2])/nrow(priorTwoMonth)
  aggMonths[i,24] <- sum(priorThreeMonth[,2])/nrow(priorThreeMonth)
}

#Matrix discarding first 3 observations
sbMonthMatrix <- aggMonths[4:36,]

#Creation of full covariate matrix for month 37 
#For predictions use subset of month37 vector as x value 
month37 <- vector(mode="numeric",length=ncol(sbMonthMatrix))
month37[1] <- 37
priorOneMonth <- sbMatrix1[sbMatrix1[,1]==36,] 
priorTwoMonth <- sbMatrix1[sbMatrix1[,1]==35,] 
priorThreeMonth <- sbMatrix1[sbMatrix1[,1]==34,]
for(j in 3:21){
  month37[j] <- sum(priorOneMonth[,j])/nrow(priorOneMonth) 
}
month37[22] <- sum(priorOneMonth[,2])/nrow(priorOneMonth)
month37[23] <- sum(priorTwoMonth[,2])/nrow(priorTwoMonth)
month37[24] <- sum(priorThreeMonth[,2])/nrow(priorThreeMonth)

# Covariate Matrix including covariates for 37th Month
sbGlmMatrix <- rbind(sbMonthMatrix,month37)

# GLM Model with all Covariates
y <- sbGlmMatrix[,2]
x <- sbGlmMatrix[,3:24]
fit <- glm(y~x,family=gaussian())
sumfit <- summary(fit)
predictfit <- predict(fit, type="response")
predictedval <- as.numeric(predictfit[34])
print(predictedval)
actualfit <- y[1:33]
plot(y=predictfit,x=4:37,type="b",col="red",xlim=c(0,40), ylim=c(0.09, 0.25),
     main="Small Business Percentage by Month (Full Model)",xlab="Month (1-37)",
     ylab="Total Small Business Percentage")
lines(y=actualfit,x=4:36,type = "b",col="green")
points(x=37,y=predictedval, type="p", col="blue")
legend(c("bottomleft"),c("Predicted","Actual","Predicted 37th"),lty=c(1,1),lwd=c(2.5,2.5),
       col=c("red","green", "blue"),cex=0.75) 

# GLM Model with Only Time Series Components 
y <- sbGlmMatrix[,2]
x <- sbGlmMatrix[,22:24]
fit <- glm(y~x,family=gaussian())
sumfit <- summary(fit)
predictfit <- predict(fit, type="response")
predictedval <- as.numeric(predictfit[34])
print(predictedval)
actualfit <- y[1:33]
plot(y=predictfit,x=4:37,type="b",col="red",xlim=c(0,40), ylim=c(0.09, 0.25),
     main="Small Business Percentage by Month (Time Series Model)",xlab="Month (1-37)",
     ylab="Total Small Business Percentage")
lines(y=actualfit,x=4:36,type = "b",col="green")
points(x=37,y=predictedval, type="p", col="blue")
legend(c("bottomleft"),c("Predicted","Actual","Predicted 37th"),lty=c(1,1),lwd=c(2.5,2.5),
       col=c("red","green", "blue"),cex=0.75) 

# GLM Model Chosen by GLM Net 
y <- sbGlmMatrix[,2]
x <- sbGlmMatrix[,3:24]
cvfit <- cv.glmnet(x=x, y=y, family = "gaussian")
sigcoef <- as.matrix(coef(cvfit, s = "lambda.min"))
chosencoef <- which(sigcoef!=0)
for (i in 1:length(chosencoef)){
  chosencoef[i] <- chosencoef[i] - 1
}
if(chosencoef[1]==0){
  chosencoef <- chosencoef[2:length(chosencoef)]
}
print(paste0("The Significant Variables Chosen by GLMNet are ", chosencoef))
newx <- matrix(data=1,nrow=nrow(x),ncol=1)
for(i in 1:length(chosencoef)){
  if(chosencoef[i]<13){
    newx <- cbind(newx,x[,chosencoef[i]])
  }
}
newx <- cbind(newx,x[,13:15]) # Ensures that all times series terms are in
newx <- newx[,2:ncol(newx)]
fit <- glm(y~newx,family=gaussian())
sumfit <- summary(fit)
predictfit <- predict(fit, type="response")
predictedval <- as.numeric(predictfit[34])
print(predictedval)
actualfit <- y[1:33]
plot(y=predictfit,x=4:37,type="b",col="red",xlim=c(0,40), ylim=c(0.09, 0.25),
     main="Small Business Percentage by Month (GLM Net Model)",xlab="Month (1-37)",
     ylab="Total Small Business Percentage")
lines(y=actualfit,x=4:36,type = "b",col="green")
points(x=37,y=predictedval, type="p", col="blue")
legend(c("bottomleft"),c("Predicted","Actual","Predicted 37th"),lty=c(1,1),lwd=c(2.5,2.5),
       col=c("red","green", "blue"),cex=0.75) 