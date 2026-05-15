#####################################
# ============================================================
# Practical 3: IPW and Multiple Imputation
# 2450 - Causal Inference and Missing Data
# ============================================================
# -----------------------------
# 0. Load packages
# -----------------------------
install.packages("mice")
library(mice)
# -----------------------------
# 1. Load data
# -----------------------------
setwd("C:/Users/danit/Downloads")
load("nhanesMort.Rdata")

# Check data
ls()
head(nhanesMort)
str(nhanesMort)

# ============================================================
# 1. COMPLETE CASE ANALYSIS
# ============================================================

# Estimate proportion with ALQ100 = Yes using complete cases
ccaALQYes <- mean(nhanesMort$ALQ100 == "Yes", na.rm = TRUE)

# Number of complete cases for ALQ100
nCCA <- sum(!is.na(nhanesMort$ALQ100))

# Standard error for complete case proportion
ccaSE <- sqrt(ccaALQYes * (1 - ccaALQYes) / nCCA)

# Print results
ccaALQYes
ccaSE


# ============================================================
# 2. INVERSE PROBABILITY WEIGHTING
# ============================================================

# Create observation indicator:
# 1 = ALQ100 observed
# 0 = ALQ100 missing
nhanesMort$alq100Obs <- 1 * (!is.na(nhanesMort$ALQ100))

# Logistic regression model for probability ALQ100 is observed
obsModel <- glm(
  alq100Obs ~ age + ethnicity + gender,
  family = binomial,
  data = nhanesMort
)

summary(obsModel)

# Predicted probability of ALQ100 being observed
pihat <- obsModel$fitted.values

# Inverse probability weights
wgt <- 1 / pihat

# Create binary outcome:
# 1 = ALQ100 Yes
# 0 = ALQ100 No
# NA = missing
nhanesMort$alq100Yes <- 1 * (nhanesMort$ALQ100 == "Yes")
nhanesMort$alq100Yes[is.na(nhanesMort$ALQ100)] <- NA

# Weighted mean/proportion

ipwMod <- lm(
  alq100Yes ~ 1,
  weights = wgt,
  data = nhanesMort
)

# View model summary
summary(ipwMod)


# Save IPW estimate and SE
ipwEstimate <- coef(summary(ipwMod))[1, 1]
ipwSE <- coef(summary(ipwMod))[1, 2]

ipwEstimate
ipwSE

# Histogram of weights among complete cases
hist(
  wgt[nhanesMort$alq100Obs == 1],
  main = "Distribution of IPW weights among complete cases",
  xlab = "Inverse probability weights"
)


# ============================================================
# 3. MULTIPLE IMPUTATION
# ============================================================

# Create subset for imputation
nhanes_subset <- subset(
  nhanesMort,
  select = c("age", "ethnicity", "gender", "ALQ100")
)

# Check missingness pattern
md.pattern(nhanes_subset)

# Set seed for reproducibility
set.seed(7341)

# Run multiple imputation
imps <- mice(
  nhanes_subset,
  m = 10,
  maxit = 1
)

# Summarise imputation object
summary(imps)

# Check imputation methods
imps$method


# ============================================================
# 4. ANALYSIS OF IMPUTED DATASETS
# ============================================================

# Fit logistic regression in each imputed dataset
fit <- with(
  imps,
  exp = glm(ALQ100 ~ 1, family = "binomial")
)

# Pool estimates using Rubin's rules
pooled_fit <- pool(fit)

summary(pooled_fit, conf.int = TRUE)

# Extract pooled log odds estimate and SE
mi_summary <- summary(pooled_fit)

log_odds_MI <- mi_summary$estimate[1]
log_odds_SE <- mi_summary$std.error[1]

log_odds_MI
log_odds_SE

# Transform log odds to probability
miProp <- exp(log_odds_MI) / (1 + exp(log_odds_MI))

miProp

# Delta method SE for probability
miSE <- miProp * (1 - miProp) * log_odds_SE

miSE


# ============================================================
# 5. COMPARE CCA, IPW, AND MI
# ============================================================

results <- data.frame(
  Method = c("Complete Case Analysis", "IPW", "Multiple Imputation"),
  Estimate = c(ccaALQYes, ipwEstimate, miProp),
  SE = c(ccaSE, ipwSE, miSE)
)

results


# ============================================================
# 6. EXTREME ASSUMPTIONS FOR MISSING ALQ100
# ============================================================

# Assume all missing ALQ100 are Yes
alq_all_yes <- nhanesMort$ALQ100
alq_all_yes[is.na(alq_all_yes)] <- "Yes"

prop_all_missing_yes <- mean(alq_all_yes == "Yes")

# Assume all missing ALQ100 are No
alq_all_no <- nhanesMort$ALQ100
alq_all_no[is.na(alq_all_no)] <- "No"

prop_all_missing_no <- mean(alq_all_no == "Yes")

prop_all_missing_yes
prop_all_missing_no


# ============================================================
# 7. FINAL SUMMARY TABLE INCLUDING EXTREME SCENARIOS
# ============================================================

final_results <- data.frame(
  Method = c(
    "Complete Case Analysis",
    "IPW",
    "Multiple Imputation",
    "All missing assumed Yes",
    "All missing assumed No"
  ),
  Estimate = c(
    ccaALQYes,
    ipwEstimate,
    miProp,
    prop_all_missing_yes,
    prop_all_missing_no
  )
)

final_results