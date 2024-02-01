## Analysis focus on the evaluation of VaR and ES according to different assumptions
#     #1) Normal Distribution
      #2) Empirical observed distribution
      #3) Scaled t distribution for a GARCH(1,1) model

## for a USD 100 million portfolio of equities invested in the WILL5000PR index

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

# Define confidence level and parameters for sample
RNGkind(sample.kind = "Rounding")
set.seed(123789)
alpha <- 0.01
n <- 10^5

#1) Normal Distribution
rvector_norm <- rnorm(n, mu, sd)
VaR_norm <- quantile(rvector_norm,alpha)
ES_norm <- mean(rvector_norm[rvector_norm < VaR_norm])

# Obtain values for a USD100 million portfolio
HF_VaR_norm <- (exp(VaR_norm)-1)*10^8
HF_ES_norm <- (exp(ES_norm)-1)*10^8


#2) Empirical observed distribution
rvector_emp <- sample(as.vector(logret),n,replace = TRUE)
VaR_emp <- quantile(rvector_emp,alpha)
ES_emp <- mean(rvector_emp[rvector_emp < VaR_emp])

# Obtain values for a USD 100 millions portfolio
HF_VaR_emp <- (exp(VaR_emp)-1)*10^8
HF_ES_emp <- (exp(ES_emp)-1)*10^8

#3) Scaled t distribution for a GARCH(1,1)-t model using "rugarch" library
#This model addresses the presence of volatility clusters (heteroskedaticity)
#Thicker tails are addressed by t-scaled student distribution

library(rugarch)
#Sample size for bootstrapping techniques

#Model specification and fit
uspec <- ugarchspec( variance.model = list(model = "sGARCH",garchOrder = c(1,1)), 
                     mean.model = list(armaOrder = c(0,0), include.mean = TRUE), 
                     distribution.model = "std")
fit.garch <- ugarchfit(uspec,data = as.vector(logret))


#Define parameter for bootstrap method
boot.garch <- ugarchboot(fit.garch, data = as.vector(logret), method = 'Partial', sampling = "raw", n.ahead = 1, n.bootpred = n, solver = "solnp")
rvec_garch <- boot.garch@fseries

VaR_garch <- quantile(rvec_garch,alpha)
ES_garch <- round(mean(rvec_garch[rvec_garch < VaR_garch]),6)
VaR_garch <- round(VaR_garch,6)

# Obtain values for a USD 100 millions portfolio
HF_VaR_garch <- (exp(VaR_garch)-1)*10^8
HF_ES_garch <- (exp(ES_garch)-1)*10^8


#Print values
cat("HF_VaR_norm: ",HF_VaR_norm)
cat("HF_VaR_emp: ",HF_VaR_emp)
cat("HF_VaR_garch: ",HF_VaR_garch)

cat("HF_ES_norm: ",HF_ES_norm)
cat("HF_ES_emp: ",HF_ES_emp)
cat("HF_ES_garch: ",HF_ES_garch)

#Plot the normal distribution curve
plot(density(rvec_garch), col = "blue",main = "1-Day VaR of WILL5000PR index (95% confidence)", ylab = "cdf", xlab = "Returns",xlim = c(-0.075, +0.075),ylim = c(0, +60))
lines(density(rvector_emp), col = "black")

#Add a vertical line at VaR
abline(v = VaR_garch, col = "red", lty = 2, lwd = 2)

#Add a legend
legend("topright", legend = c("GARCH(1,1)", "Real Distribution", "VaR"), col = c("blue","black", "red"), lty = c(1,1,2), cex = 0.75)

# Display VaR value
text(VaR_garch, VaR_garch, labels = paste("            ",round(VaR_garch,4) ), col = "red")
