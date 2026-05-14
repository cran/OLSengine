## ----message=FALSE------------------------------------------------------------
library(OLSengine)

# 1. Simulate data with non-constant variance
set.seed(123)
n <- 200
x <- rnorm(n, 50, 10)
y <- 10 + 0.5 * x + rnorm(n, 0, x * 0.2) # Heteroskedasticity
df <- data.frame(y, x)

# 2. Run the engine
model <- paper_engine(y ~ x, data = df, model = "ols")

## -----------------------------------------------------------------------------
model_robust <- paper_engine(y ~ x, data = df, model = "ols", robust = TRUE)
model_robust$tables$Table2_OLS_Estimation

## -----------------------------------------------------------------------------
# Simulating 3 groups with non-normal distribution
set.seed(789)
group_data <- data.frame(
  score = c(rgamma(30, 2, 0.5), rgamma(30, 5, 0.5), rgamma(30, 3, 0.5)),
  group = rep(c("Control", "Treatment A", "Treatment B"), each = 30)
)

# Run ANOVA engine with "auto" non-parametric detection
model_anova <- paper_engine(score ~ group, data = group_data, model = "anova", non_parametric = "auto")

# View the result (it will automatically use Kruskal-Wallis if normality fails)
model_anova$tables$Table1_ANOVA_Results

## -----------------------------------------------------------------------------
# Simulating binary data
set.seed(101)
n_logit <- 100
age <- rnorm(n_logit, 40, 10)
passed <- rbinom(n_logit, 1, plogis(-5 + 0.12 * age))
logit_df <- data.frame(passed, age)

# Run Logit engine
model_logit <- paper_engine(passed ~ age, data = logit_df, model = "logit")

# View Odds Ratios and Accuracy
model_logit$tables$Table3_Logit_Estimation

## -----------------------------------------------------------------------------
# Forest plot for our robust OLS model
plot_engine(model_robust)

