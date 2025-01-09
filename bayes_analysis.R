library(brms)
library(rstan)
library(bayesplot)
library(dplyr)

data <- read.csv("/Users/anirudhparameswaran/Desktop/worldvaluessurvey-main/cleaned_dataset.csv", header=TRUE, sep = ",")

data <- filter(data, voter >= 0)

# data$income_level <- factor(data$income_level, ordered = TRUE)  # Ordinal categorical
# data$education <- factor(data$education, ordered = TRUE)  # Ordinal categorical
# data$freedom <- factor(data$freedom, ordered = TRUE)  # Ordinal categorical
# data$satisfaction <- factor(data$satisfaction, ordered = TRUE)  # Ordinal categorical
# data$financial_wellbeing <- factor(data$financial_wellbeing, ordered = TRUE)  # Ordinal categorical

data$sex <- factor(data$sex)  # Categorical
data$immigrant <- factor(data$immigrant)  # Categorical
data$praying_frequency <- factor(data$praying_frequency)  # Categorical
# data$god_importance <- factor(data$god_importance)  # Categorical 

head(data)

test <- voter ~ freedom + satisfaction + financial_wellbeing + sex + age + immigrant + children + income_level + education + god_importance + praying_frequency + ethics_score

fit <- brm(
  formula = test,
  data = data,
  family = bernoulli(),
  prior = c(
    set_prior("normal(0, 5)", class = "b"),
    set_prior("normal(0, 10)", class = "Intercept")
  ),
  chains = 4,
  iter = 2000,
  warmup = 1000,
  cores = 4,
  seed = 42
)

summary(fit)
plot(fit)
pp_check(fit, ndraws = 100, type = 'ecdf_overlay')
