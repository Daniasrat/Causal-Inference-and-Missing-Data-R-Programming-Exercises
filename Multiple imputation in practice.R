#####################################
# ========== Exercise 4: Multiple Imputation
# 2450 - Causal Inference and Missing Data
# ============================================================
# -----------------------------
# 1. Load packages
# -----------------------------
install.packages("mice")
library(mice)
# -----------------------------
# 2. Load data
# -----------------------------
setwd("C:/Users/danit/Downloads")
load("nhanesMort.Rdata")

# Check data
ls()
head(nhanesMort)
str(nhanesMort)
# =========================================================
# 3. CREATE 10-YEAR MORTALITY OUTCOME
# =========================================================

nhanesMort$dead10 <- 1 * (
  (nhanesMort$dead == 1) &
    (nhanesMort$tMonths < 120)
)

table(nhanesMort$dead10)

# =========================================================
# 4. COMPLETE CASE ANALYSIS (CCA)
# =========================================================

cca10year <- glm(
  dead10 ~ gender + age + ethnicity + sbp +
    waist_circum + weight + total_chol +
    hdl + ALQ100,
  family = "binomial",
  data = nhanesMort
)

summary(cca10year)

# =========================================================
# 5. NUMBER OF COMPLETE CASES USED
# =========================================================

n_complete <- nobs(cca10year)

n_total <- nrow(nhanesMort)

n_dropped <- n_total - n_complete

n_total
n_complete
n_dropped

# =========================================================
# 6. MISSING DATA PATTERN
# =========================================================

md.pattern(nhanesMort)

# =========================================================
# 7. INITIAL MICE SETUP (NO IMPUTATION YET)
# =========================================================

imps <- mice(
  nhanesMort,
  m = 10,
  maxit = 0
)

imps

# View predictor matrix
imps$predictorMatrix

# View default methods
imps$method

# =========================================================
# 8. CREATE CUSTOM PREDICTOR MATRIX
# =========================================================

# Get default predictor matrix
myPredictorMatrix <- make.predictorMatrix(nhanesMort)

# Remove dead and tMonths as predictors
myPredictorMatrix[, c("dead", "tMonths")] <- 0

# View predictor matrix
myPredictorMatrix

# =========================================================
# 9. DEFINE IMPUTATION METHODS
# =========================================================

myDefaultMethod <- c(
  "norm",     # numeric
  "logreg",   # binary
  "polyreg",  # unordered categorical
  "polr"      # ordered categorical
)

myDefaultMethod

# =========================================================
# 10. RUN MULTIPLE IMPUTATION
# =========================================================

set.seed(52267)

imps <- mice(
  nhanesMort,
  m = 10,
  defaultMethod = myDefaultMethod,
  predictorMatrix = myPredictorMatrix,
  printFlag = FALSE
)

# Summary of imputations
summary(imps)

# =========================================================
# 11. FIT SUBSTANTIVE MODEL TO IMPUTED DATA
# =========================================================

fit <- with(
  data = imps,
  exp = glm(
    dead10 ~ gender + age + ethnicity +
      sbp + waist_circum + weight +
      total_chol + hdl + ALQ100,
    family = "binomial"
  )
)

# =========================================================
# 12. POOL RESULTS
# =========================================================

pooled <- pool(fit)

summary(pooled, conf.int = TRUE)

# =========================================================
# 13. COMPARE COEFFICIENTS
# =========================================================

cbind(
  CCA = coef(cca10year),
  MI = summary(pooled)[, 2]
)

# =========================================================
# 14. COMPARE STANDARD ERRORS
# =========================================================

cbind(
  CCA_SE = sqrt(diag(vcov(cca10year))),
  MI_SE = summary(pooled)[, 3]
)

# =========================================================
# 15. CONVERGENCE CHECKING
# =========================================================

set.seed(52267)

convImps <- mice(
  nhanesMort,
  m = 10,
  defaultMethod = myDefaultMethod,
  predictorMatrix = myPredictorMatrix,
  maxit = 50,
  printFlag = FALS)

# Convergence plots
plot(convImps)
  
# =========================================================
# 16. DENSITY PLOTS
# =========================================================

# Create copy
nhanesCopy <- nhanesMort

# Convert integer variables to numeric
for (i in c(6, 9, 10)) {
  nhanesCopy[, i] <- as.numeric(nhanesCopy[, i])
}

# Re-impute
set.seed(52267)

impsCopy <- mice(
  nhanesCopy,
  m = 10,
  defaultMethod = myDefaultMethod,
  predictorMatrix = myPredictorMatrix,
  printFlag = FALSE
)

# Density plots
densityplot(impsCopy)

# =========================================================
# 17. PERCENTAGE OF INCOMPLETE CASES
# =========================================================

# Complete cases
complete_cases <- complete.cases(nhanesMort)

# Percentage incomplete
percent_incomplete <- mean(!complete_cases) * 100

percent_incomplete

# =========================================================
# 18. INCREASE NUMBER OF IMPUTATIONS
# =========================================================

# Recommended number of imputations
m_new <- ceiling(percent_incomplete)

m_new

# Re-run MI with larger m
set.seed(52267)

imps_large <- mice(
  nhanesMort,
  m = m_new,
  defaultMethod = myDefaultMethod,
  predictorMatrix = myPredictorMatrix,
  printFlag = FALSE
)

# Fit model
fit_large <- with(
  data = imps_large,
  exp = glm(
    dead10 ~ gender + age + ethnicity +
      sbp + waist_circum + weight +
      total_chol + hdl + ALQ100,
    family = "binomial"
  )
)

# Pool results
pooled_large <- pool(fit_large)

summary(pooled_large, conf.int = TRUE)

# =========================================================
# 19. OMIT OUTCOME dead10 FROM IMPUTATION MODELS
# =========================================================

# Create new predictor matrix
pm_no_outcome <- myPredictorMatrix

# Remove dead10 as predictor
pm_no_outcome[, "dead10"] <- 0

# Run MI
set.seed(52267)

imps_no_outcome <- mice(
  nhanesMort,
  m = 10,
  defaultMethod = myDefaultMethod,
  predictorMatrix = pm_no_outcome,
  printFlag = FALSE
)

# Fit model
fit_no_outcome <- with(
  data = imps_no_outcome,
  exp = glm(
    dead10 ~ gender + age + ethnicity +
      sbp + waist_circum + weight +
      total_chol + hdl + ALQ100,
    family = "binomial"
  )
)

# Pool results
pooled_no_outcome <- pool(fit_no_outcome)

summary(pooled_no_outcome, conf.int = TRUE)

# =========================================================
# 20. COMPARE ALL RESULTS
# =========================================================

summary(pooled, conf.int = TRUE)

summary(pooled_large, conf.int = TRUE)

summary(pooled_no_outcome, conf.int = TRUE)