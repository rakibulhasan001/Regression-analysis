df1 <- read.csv("RelatedMultipleRegData.csv", header = TRUE, sep = ",")  #reading the csv file from the working directory. It is set through Ctrl+Shift+H
df1

#log transformation of qi and bound with original data file
df1 <- cbind(df1, ln_q1 <- log(df1[,2]),                   
                  ln_q2 <- log(df1[,3]), 
                  ln_q3 <- log(df1[,4]),
                  ln_q4 <- log(df1[,5]))

#renaming the columns with short names
colnames(df1) = c("year", "q1", "q2", "q3", "q4", "w1", "w2", "w3", "w4", "ln_q1","ln_q2", "ln_q3", "ln_q4")   
df1

#creating Stone Quantity Index, and then binding with the data file
df1 <- cbind(df1, ln_Q <- df1$w1*df1$ln_q1 + df1$w2*df1$ln_q2 + df1$w3*df1$ln_q3 + df1$w4*df1$ln_q4)        
df1                                                                                                               

#renaming the columns with short names by using colname() and c() function
colnames(df1) = c("year", "q1", "q2", "q3", "q4", "w1", "w2", "w3", "w4", "ln_q1","ln_q2", "ln_q3", "ln_q4", "ln_Q")    
df1

#Using for loop to fit the data for all the linear models
install.packages('olsrr') #install the package that runs residual plots and check assumptions
library(olsrr)    #Load the package

for (i in 1:4) {                                                      
   model <- lm(df1[,5+i] ~ ln_q1 + ln_q2 + ln_q3 + ln_q4 + ln_Q, data = df1)           #df1[,5+i] iterates through wi
   # Print the summary of the model
   print(paste("Model for w",i))
   print(summary(model))                             #prints all the model result sequentially
   print(ols_coll_diag(model))                       # Produces both VIF and Condition Index to check multicolinearity
   print(plot(model,which=1))
   print(shapiro.test(model$residuals))              #runs normality test
}


