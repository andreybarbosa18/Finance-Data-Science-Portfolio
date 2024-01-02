## This analysis focus on the evaluation of VaR and ES according to different assumptions
## for a USD 100 million portfolio of equities 100% invested in the WILL5000PR index

# Import library quantmod
library(quantmod)

# Download of data
data <- getSymbols("WILL5000PR", src = "FRED", auto.assign = FALSE)

# Pre-processing of data (omitting na values)
data <- na.omit(data)
data <- data["1979-12-31/2022-12-31"]
logret <- diff(log(data))
logret <- logret[-1]

#Calculation of statistical parameters
mu <- mean(logret)
sd <- sd(logret)
ret <- exp(logret)-1


#We will obtain the VaR (Value at risk) and ES (Expected Shortfall) according to 
#several assumptions)

# Define confidence level and parameters for sample
RNGkind(sample.kind = "Rounding")
set.seed(123789)
alpha <- 0.01
n <- 10^5
# From formula which assumes normality
# VaR and ES 
VaR_form <- qnorm(alpha,mu,sd)
ES_form <- mu - sd*dnorm(qnorm(alpha,0,1),0,1)/alpha

# Obtain values for a USD 100 millions portfolio
HF_VaR_form <- (exp(VaR_form)-1)*10^3
HF_ES_form <- (exp(ES_form)-1)*10^3



# From a daily logreturn vector from a normal distribution with same mean and standard dev.
rvector_norm <- rnorm(n, mu, sd)
#VaR and ES
VaR_norm <- quantile(rvector_norm,alpha)
ES_norm <- mean(rvector_norm[rvector_norm < VaR_norm])

# Obtain values for a USD 100 millions portfolio
HF_VaR_norm <- (exp(VaR_norm)-1)*10^3
HF_ES_norm <- (exp(ES_norm)-1)*10^3



#From a vector of real empirical data
rvector_emp <- sample(as.vector(logret),n,replace = TRUE)
#VaR and ES
VaR_emp <- quantile(rvector_emp,alpha)
ES_emp <- mean(rvector_emp[rvector_emp < VaR_emp])

# Obtain values for a USD 100 millions portfolio
HF_VaR_emp <- (exp(VaR_emp)-1)*10^3
HF_ES_emp <- (exp(ES_emp)-1)*10^3


#Print values
cat("HF_VaR_form", HF_VaR_form)
cat("HF_VaR_norm: ",HF_VaR_norm)
cat("HF_VaR_emp: ",HF_VaR_emp)

cat("HF_ES_form: ",HF_ES_form)
cat("HF_ES_norm: ",HF_ES_norm)
cat("HF_ES_emp: ",HF_ES_emp)

#Plot the normal distribution curve
plot(density(rvector_emp), col = "blue",main = "Daily Empirical log returns of WILL5000PR index (95% confidence)", ylab = "cdf", xlab = "Returns",xlim = c(-0.075, +0.075))

#Add a vertical line at VaR
abline(v = VaR_emp, col = "red", lty = 2, lwd = 2)

#Add a legend
legend("topright", legend = c("Distribution Curve", "VaR"), col = c("blue", "red"), lty = c(1, 2))

# Display VaR value
text(VaR_emp, VaR_emp, labels = paste("            ",round(VaR_emp,4) ), col = "red")
