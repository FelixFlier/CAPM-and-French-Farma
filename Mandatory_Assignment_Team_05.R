#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Mandatory assignment- Team 5 ----
# Mastercard & Agilent Technologies
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Introduction ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 
# Install and load necessary packages
install.packages(c("readxl", "dplyr", "lubridate", "tidyverse", "lmtest", "sandwich", "ggplot2","moments", "patchwork"))
library(readxl)
library(dplyr)
library(lubridate)
library(tidyverse)
library(lmtest)
library(sandwich)
library(ggplot2)
library(moments)
library(patchwork)


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Step 1: Load data sets ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

SP500 <- read_excel("C:\\Users\\felix\\OneDrive\\Dokumente\\IntroR.Rproj\\SP500.xlsx")
FFfactors <- read.csv("C:\\Users\\felix\\OneDrive\\Dokumente\\IntroR.Rproj\\FFFactors.csv", skip = 4)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Step 2: Select necessary columns and format dates ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

SP500 <- SP500 %>% select(Name, `MASTERCARD`, `AGILENT TECHS.`)
SP500$Name <- as.Date(SP500$Name, format = "%Y-%m-%d")
colnames(SP500)[1] <- "Date"

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Step 3: Calculate daily net and log returns ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

returns <- SP500 %>%
  mutate(
    Return_MASTERCARD = (`MASTERCARD` / lag(`MASTERCARD`) - 1),
    Return_AGILENT = (`AGILENT TECHS.` / lag(`AGILENT TECHS.`) - 1)
  ) %>%
  select(Date, Return_MASTERCARD, Return_AGILENT) %>%
  na.omit()

log_returns <- SP500 %>%
  mutate(
    log_Return_MASTERCARD = log(`MASTERCARD` / lag(`MASTERCARD`)),
    log_Return_AGILENT = log(`AGILENT TECHS.` / lag(`AGILENT TECHS.`))
  ) %>%
  select(Date, log_Return_MASTERCARD, log_Return_AGILENT) %>%
  na.omit()


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Step 4: Add portfolio net and log returns and multiply with 100 ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

returns <- returns %>%
  mutate(PF = 0.5 * (Return_MASTERCARD + Return_AGILENT))
log_returns <- log_returns %>%
  mutate(log_PF = 0.5 * (log_Return_MASTERCARD + log_Return_AGILENT))


returns$Return_AGILENT <- returns$Return_AGILENT*100
returns$Return_MASTERCARD <- returns$Return_MASTERCARD*100
returns$PF <- returns$PF*100

log_returns$log_Return_AGILENT <- log_returns$log_Return_AGILENT*100
log_returns$log_Return_MASTERCARD <- log_returns$log_Return_MASTERCARD*100
log_returns$log_PF <- log_returns$log_PF*100

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Step 5: Format the Date column ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

FFfactors$Date <- as.Date(as.character(FFfactors$Date), format = "%Y%m%d")

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Step 6: merge of the data sets ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

CAPMdat <- merge(returns, FFfactors, by = "Date")

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Step 7: Statistics and Plots ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#Skewness
skewness_Mastercard <- skewness(log_returns$log_Return_MASTERCARD, na.rm=TRUE)
skewness_Agilent <- skewness(log_returns$log_Return_AGILENT, na.rm=TRUE)
skewness_PF <- skewness(log_returns$log_PF, na.rm=TRUE)

#Kurtosis
kurtosis_Mastercard <- kurtosis(log_returns$log_Return_MASTERCARD, na.rm=TRUE)
kurtosis_Agilent <- kurtosis(log_returns$log_Return_AGILENT, na.rm=TRUE)
kurtosis_PF <- kurtosis(log_returns$log_PF, na.rm=TRUE)

kurtosis_Mastercard
kurtosis_Agilent
kurtosis_PF

#Excess Kurtosis
excess.kurt.ret_Mastercard <- kurtosis_Mastercard - 3
excess.kurt.ret_Mastercard

excess.kurt.ret_Agilent <- kurtosis_Agilent -3
excess.kurt.ret_Agilent

excess.kurt.ret_PF <- kurtosis_PF -3
excess.kurt.ret_PF

#expected returns (average of daily returns)
expected_return_MASTERCARD <- mean(CAPMdat$Return_MASTERCARD)
expected_return_AGILENT <- mean(CAPMdat$Return_AGILENT)
expected_return_PF <- mean(CAPMdat$PF)

#standard deviations of daily returns
std_dev_MASTERCARD <- sd(CAPMdat$Return_MASTERCARD)
std_dev_AGILENT <- sd(CAPMdat$Return_AGILENT)
std_dev_PF <- sd(CAPMdat$PF)

# Density of Log-Returns
dens_MASTERCARD <- density(log_returns$log_Return_MASTERCARD/100, na.rm = TRUE)
dens_AGILENT <- density(log_returns$log_Return_AGILENT/100, na.rm = TRUE)


#Plot 1: Density Plot of Log-Returns
# Plot
par(mar = c(5, 4, 2, 2))
hist(log_returns$log_Return_MASTERCARD/100, breaks = 100, freq = FALSE, xlab = "Log-Returns", ylab = "Density", xlim = c(-0.14, 0.14), main = "Density Plot of Log-Returns", col = rgb(1, 0, 0, 0.5))
lines(dens_MASTERCARD, col = "red", lwd = 2)
lines(dens_AGILENT, col = "blue", lwd = 2)
legend("topright", legend = c("Mastercard", "Agilent"), col = c("red", "blue"), lty = 1, lwd = 2)
box()

# Plot 2: Mastercard, Agilent and Equally Weighted Portfolio daily returns
par(mfcol = c(1, 3), oma = c(0, 0, 0, 0), mar = c(5, 4, 4, 2) + 0.1)

#Mastercard returns
plot(CAPMdat$Date, CAPMdat$Return_MASTERCARD, type = "l", col = "blue", lwd = 2,
     xlab = "Date", ylab = "Return", main = "Daily Returns of Mastercard", ylim = c(-15,15))
grid()
# Customize x-axis with yearly ticks
years <- unique(format(CAPMdat$Date, "%Y"))
year_positions <- as.Date(paste0(years, "-01-01"))
legend("topright", legend = "Mastercard", col = "blue", lwd = 2, xpd = TRUE, inset = c(-0.15, 0))

#AGILENT TECHS. returns
plot(CAPMdat$Date, CAPMdat$Return_AGILENT, type = "l", col = "blue", lwd = 2,
     xlab = "Date", ylab = "Return", main = "Daily Returns of Agilent",ylim = c(-15,15))
grid()
# Customize x-axis with yearly ticks
years <- unique(format(CAPMdat$Date, "%Y"))
year_positions <- as.Date(paste0(years, "-01-01"))
legend("topright", legend = "Agilent", col = "blue", lwd = 2)


#Portfolio returns
plot(CAPMdat$Date, CAPMdat$PF, type = "l", col = "blue", lwd = 2,
     xlab = "Date", ylab = "Return", main = "Daily Returns of Portfolio",ylim = c(-15,15))
grid()
# Customize x-axis with yearly ticks
years <- unique(format(CAPMdat$Date, "%Y"))
year_positions <- as.Date(paste0(years, "-01-01"))
legend("topright", legend = "Portfolio", col = "blue", lwd = 2)

par(mfrow = c(1, 1))



#Plot 3: time series plot for stock prices
p1 <- ggplot(SP500, aes(x = Date)) +
  geom_line(aes(y = `MASTERCARD`, color = "Mastercard")) +
  geom_line(aes(y = `AGILENT TECHS.`, color = "Agilent")) +
  geom_line(aes(y = `PF`, color = "Portfolio")) +
  labs(y = "Price", 
       x = "Year",
       color = "Legend") +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1))

print(p1)

# Plot 4: Risk-return combination
#expected returns and standard deviations
expected_returns <- c(expected_return_PF, expected_return_MASTERCARD, expected_return_AGILENT)
std_devs <- c(std_dev_PF, std_dev_MASTERCARD, std_dev_AGILENT)
labels <- c("Portfolio", "MASTERCARD", "AGILENT")
# Plot
plot(std_devs, expected_returns,
     xlab="Standard deviation", ylab="Expected return",
     pch=19, col="blue", cex=1.5,
     xlim=c(min(std_devs) - 0.2, max(std_devs) + 0.2),
     ylim=c(min(expected_returns) - 0.01, max(expected_returns) + 0.01))
text(std_devs, expected_returns, labels=labels, pos=4)
lines(c(std_dev_PF, std_dev_MASTERCARD), c(expected_return_PF, expected_return_MASTERCARD), col="black")
lines(c(std_dev_PF, std_dev_AGILENT), c(expected_return_PF, expected_return_AGILENT), col="black")


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Estimation of the CAPM ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Step 8: Change the data set in numeric values ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

CAPMdat <- CAPMdat %>%
  mutate(
    Return_MASTERCARD = as.numeric(Return_MASTERCARD),
    Return_AGILENT = as.numeric(Return_AGILENT),
    PF = as.numeric(PF),
    Mkt_RF = as.numeric(Mkt.RF),
    SMB = as.numeric(SMB),
    HML = as.numeric(HML),
    RF = as.numeric(RF))

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Step 9: Compute excess returns for each stock and the portfolio ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

RF <- CAPMdat %>% select(RF)
exret <- CAPMdat %>%
  mutate(
    Re_MASTERCARD = Return_MASTERCARD - RF,
    Re_AGILENT = Return_AGILENT - RF,
    Re_PF = PF - RF
  ) %>%
  select(Date, Re_MASTERCARD, Re_AGILENT, Re_PF, Mkt_RF, SMB, HML, RF)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Step 10: Estimate CAPM models ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# CAPM-Modell for MASTERCARD
CAPM_MASTERCARD <- lm(Re_MASTERCARD ~ Mkt_RF, data = exret)
summary_MASTERCARD <- coeftest(CAPM_MASTERCARD, vcov = NeweyWest(CAPM_MASTERCARD, lag = 1))

#CAPM-Modell for AGILENT
CAPM_AGILENT <- lm(Re_AGILENT ~ Mkt_RF, data = exret)
summary_AGILENT <- coeftest(CAPM_AGILENT, vcov = NeweyWest(CAPM_AGILENT, lag = 1))

# CAPM-Modell for the Portfolio
CAPM_PF <- lm(Re_PF ~ Mkt_RF, data = exret)
summary_PF <- coeftest(CAPM_PF, vcov = NeweyWest(CAPM_PF, lag = 1))

# Print CAPM results
print("CAPM Results for MASTERCARD:")
print(summary_MASTERCARD)

print("CAPM Results for AGILENT:")
print(summary_AGILENT)

print("CAPM Results for the Portfolio:")
print(summary_PF)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Step 11: Combine the summaries into a data frame for CAPM ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

CAPM_summaries <- data.frame(
  Model = c("MASTERCARD", "AGILENT", "Portfolio"),
  Alpha = c(summary_MASTERCARD[1, "Estimate"], summary_AGILENT[1, "Estimate"], summary_PF[1, "Estimate"]),
  Alpha_SE = c(summary_MASTERCARD[1, "Std. Error"], summary_AGILENT[1, "Std. Error"], summary_PF[1, "Std. Error"]),
  Beta = c(summary_MASTERCARD[2, "Estimate"], summary_AGILENT[2, "Estimate"], summary_PF[2, "Estimate"]),
  Beta_SE = c(summary_MASTERCARD[2, "Std. Error"], summary_AGILENT[2, "Std. Error"], summary_PF[2, "Std. Error"])
)


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Estimation of the Fama-French Model ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Step 12: Estimate Fama-French models ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Fama-French Model for MASTERCARD
FF_MASTERCARD <- lm(Re_MASTERCARD ~ Mkt_RF + SMB + HML, data = exret)
summary_FF_MASTERCARD <- coeftest(FF_MASTERCARD, vcov = NeweyWest(FF_MASTERCARD, lag = 1))

# Fama-French Model for AGILENT
FF_AGILENT <- lm(Re_AGILENT ~ Mkt_RF + SMB + HML, data = exret)
summary_FF_AGILENT <- coeftest(FF_AGILENT, vcov = NeweyWest(FF_AGILENT, lag = 1))

# Fama-French Model for the Portfolio
FF_PF <- lm(Re_PF ~ Mkt_RF + SMB + HML, data = exret)
summary_FF_PF <- coeftest(FF_PF, vcov = NeweyWest(FF_PF, lag = 1))

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Step 13: Combine the Fama-French model summaries into a data frame ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

FF_summaries <- data.frame(
  Model = c("MASTERCARD", "AGILENT", "Portfolio"),
  Alpha = c(summary_FF_MASTERCARD[1, "Estimate"], summary_FF_AGILENT[1, "Estimate"], summary_FF_PF[1, "Estimate"]),
  Alpha_SE = c(summary_FF_MASTERCARD[1, "Std. Error"], summary_FF_AGILENT[1, "Std. Error"], summary_FF_PF[1, "Std. Error"]),
  Beta_Mkt_RF = c(summary_FF_MASTERCARD[2, "Estimate"], summary_FF_AGILENT[2, "Estimate"], summary_FF_PF[2, "Estimate"]),
  Beta_Mkt_RF_SE = c(summary_FF_MASTERCARD[2, "Std. Error"], summary_FF_AGILENT[2, "Std. Error"], summary_FF_PF[2, "Std. Error"]),
  Beta_SMB = c(summary_FF_MASTERCARD[3, "Estimate"], summary_FF_AGILENT[3, "Estimate"], summary_FF_PF[3, "Estimate"]),
  Beta_SMB_SE = c(summary_FF_MASTERCARD[3, "Std. Error"], summary_FF_AGILENT[3, "Std. Error"], summary_FF_PF[3, "Std. Error"]),
  Beta_HML = c(summary_FF_MASTERCARD[4, "Estimate"], summary_FF_AGILENT[4, "Estimate"], summary_FF_PF[4, "Estimate"]),
  Beta_HML_SE = c(summary_FF_MASTERCARD[4, "Std. Error"], summary_FF_AGILENT[4, "Std. Error"], summary_FF_PF[4, "Std. Error"])
)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Step 14: additional Plot ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#Plot 5: scatter plots for the excess returns
#scatter plot Mastercard
plot_Mastercard <- ggplot(exret, aes(x = Mkt_RF, y = Re_MASTERCARD)) +
  geom_point(color = "purple", alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Mastercard Excess Returns ",
       x = "Market Excess Return",
       y = "Mastercard Excess Return") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 24),    
    axis.title.x = element_text(size = 24),  
    axis.title.y = element_text(size = 24),  
    panel.border = element_rect(color = "black", fill = NA, size = 1)  
  )
print(plot_Mastercard)

#scatter plot Agilent
plot_Agilent <- ggplot(exret, aes(x = Mkt_RF, y = Re_AGILENT)) +
  geom_point(color = "grey", alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Agilent Excess Returns ",
       x = "Market Excess Return",
       y = "Agilent Excess Return") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 24),    
    axis.title.x = element_text(size = 24),  
    axis.title.y = element_text(size = 24),  
    panel.border = element_rect(color = "black", fill = NA, size = 1)  
  )
print(plot_Agilent)

#scatter plot Portfolio
plot_PF <- ggplot(exret, aes(x = Mkt_RF, y = Re_PF)) +
  geom_point(color = "blue", alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Portfolio Excess Returns ",
       x = "Market Excess Return",
       y = "Portfolio Excess Return") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 24),    
    axis.title.x = element_text(size = 24),  
    axis.title.y = element_text(size = 24),  
    panel.border = element_rect(color = "black", fill = NA, size = 1)  
  )
print(plot_PF)

# Combine the three plots 
combined_plot <- plot_Mastercard + plot_Agilent + plot_PF + plot_layout(ncol = 3)
ggsave("plot1.png", plot = combined_plot, width = 18, height = 6)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Final Step ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Step 15: Store all results in a list ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#Expected returns
expected_returns <- list(
  MASTERCARD = expected_return_MASTERCARD,
  AGILENT = expected_return_AGILENT,
  Portfolio = expected_return_PF
)

#standard deviation
standard_deviation <- list(
  MASTERCARD = std_dev_MASTERCARD,
  AGILENT = std_dev_AGILENT,
  Portfolio = std_dev_PF
)

#Skewness
skewness <- list(
  MASTERCARD = skewness_Mastercard,
  AGILENT = skewness_Agilent,
  Portfolio = skewness_PF
)

#Kurtosis
kurtosis <- list(
  MASTERCARD = kurtosis_Mastercard,
  AGILENT = kurtosis_Agilent,
  Portfolio = kurtosis_PF
)

#Excess Kurtosis
excess_kurtosis <- list(
  MASTERCARD = excess.kurt.ret_Mastercard,
  AGILENT = excess.kurt.ret_Agilent,
  Portfolio = excess.kurt.ret_PF
)

#CAPM 
CAPM_summaries <- list(
  MASTERCARD = list(
    Alpha = summary_MASTERCARD[1, "Estimate"],
    Alpha_SE = summary_MASTERCARD[1, "Std. Error"],
    Beta = summary_MASTERCARD[2, "Estimate"],
    Beta_SE = summary_MASTERCARD[2, "Std. Error"]
  ),
  AGILENT = list(
    Alpha = summary_AGILENT[1, "Estimate"],
    Alpha_SE = summary_AGILENT[1, "Std. Error"],
    Beta = summary_AGILENT[2, "Estimate"],
    Beta_SE = summary_AGILENT[2, "Std. Error"]
  ),
  Portfolio = list(
    Alpha = summary_PF[1, "Estimate"],
    Alpha_SE = summary_PF[1, "Std. Error"],
    Beta = summary_PF[2, "Estimate"],
    Beta_SE = summary_PF[2, "Std. Error"]
  )
)

#Fama-French
FF_summaries <- list(
  MASTERCARD = list(
    Alpha = summary_FF_MASTERCARD[1, "Estimate"],
    Alpha_SE = summary_FF_MASTERCARD[1, "Std. Error"],
    Beta_Mkt_RF = summary_FF_MASTERCARD[2, "Estimate"],
    Beta_Mkt_RF_SE = summary_FF_MASTERCARD[2, "Std. Error"],
    Beta_SMB = summary_FF_MASTERCARD[3, "Estimate"],
    Beta_SMB_SE = summary_FF_MASTERCARD[3, "Std. Error"],
    Beta_HML = summary_FF_MASTERCARD[4, "Estimate"],
    Beta_HML_SE = summary_FF_MASTERCARD[4, "Std. Error"]
  ),
  AGILENT = list(
    Alpha = summary_FF_AGILENT[1, "Estimate"],
    Alpha_SE = summary_FF_AGILENT[1, "Std. Error"],
    Beta_Mkt_RF = summary_FF_AGILENT[2, "Estimate"],
    Beta_Mkt_RF_SE = summary_FF_AGILENT[2, "Std. Error"],
    Beta_SMB = summary_FF_AGILENT[3, "Estimate"],
    Beta_SMB_SE = summary_FF_AGILENT[3, "Std. Error"],
    Beta_HML = summary_FF_AGILENT[4, "Estimate"],
    Beta_HML_SE = summary_FF_AGILENT[4, "Std. Error"]
  ),
  Portfolio = list(
    Alpha = summary_FF_PF[1, "Estimate"],
    Alpha_SE = summary_FF_PF[1, "Std. Error"],
    Beta_Mkt_RF = summary_FF_PF[2, "Estimate"],
    Beta_Mkt_RF_SE = summary_FF_PF[2, "Std. Error"],
    Beta_SMB = summary_FF_PF[3, "Estimate"],
    Beta_SMB_SE = summary_FF_PF[3, "Std. Error"],
    Beta_HML = summary_FF_PF[4, "Estimate"],
    Beta_HML_SE = summary_FF_PF[4, "Std. Error"]
  )
)

#Combine all results into the results list
results <- list(
  expected_returns = expected_returns,
  standard_deviation = standard_deviation,
  skewness = skewness,
  kurtosis = kurtosis,
  excess_kurtosis = excess_kurtosis,
  CAPM_summaries = CAPM_summaries,
  FF_summaries = FF_summaries
)

