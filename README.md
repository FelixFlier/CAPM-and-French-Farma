# Stock Analysis: Mastercard & Agilent Technologies

This R script performs a comprehensive financial analysis of Mastercard and Agilent Technologies stocks, including portfolio analysis, CAPM modeling, and Fama-French three-factor model implementation.

## Overview

The analysis includes:
- Calculation of daily returns and log returns
- Portfolio construction with equal weights
- Statistical analysis (expected returns, standard deviation, skewness, kurtosis)
- CAPM (Capital Asset Pricing Model) estimation
- Fama-French three-factor model implementation
- Visualization of results

## Prerequisites

The following R packages are required:
```R
- readxl
- dplyr
- lubridate
- tidyverse
- lmtest
- sandwich
- ggplot2
- moments
- patchwork
```

## Data Requirements

The script requires two input files:
1. `SP500.xlsx`: Contains stock price data for Mastercard and Agilent Technologies
2. `FFFactors.csv`: Contains Fama-French factors data

## Analysis Steps

1. **Data Loading and Preparation**
   - Load stock price and Fama-French factor data
   - Format dates and select necessary columns

2. **Return Calculations**
   - Calculate daily net returns
   - Calculate logarithmic returns
   - Create equally-weighted portfolio

3. **Statistical Analysis**
   - Calculate descriptive statistics (mean, standard deviation)
   - Compute skewness and kurtosis
   - Generate density plots and time series visualizations

4. **CAPM Analysis**
   - Calculate excess returns
   - Estimate CAPM models for individual stocks and portfolio
   - Generate summary statistics with Newey-West standard errors

5. **Fama-French Model**
   - Implement three-factor model
   - Generate model summaries and statistics
   - Create visualization of results

## Visualizations

The script generates several plots:
1. Density plot of log-returns
2. Daily returns time series for both stocks and portfolio
3. Stock price time series
4. Risk-return combination plot
5. Scatter plots of excess returns

## Results

The analysis provides comprehensive results including:
- Expected returns and risk metrics
- CAPM parameters (alpha, beta)
- Fama-French factor loadings
- Various risk measures (standard deviation, skewness, kurtosis)

## Usage

1. Install required packages:
```R
install.packages(c("readxl", "dplyr", "lubridate", "tidyverse", "lmtest", 
                  "sandwich", "ggplot2", "moments", "patchwork"))
```

2. Update file paths in the script:
```R
SP500 <- read_excel("path/to/SP500.xlsx")
FFfactors <- read.csv("path/to/FFFactors.csv", skip = 4)
```

3. Run the script to generate analysis and plots

## Output

The script stores all results in a structured list containing:
- Expected returns
- Standard deviations
- Skewness metrics
- Kurtosis values
- CAPM summary statistics
- Fama-French model results
