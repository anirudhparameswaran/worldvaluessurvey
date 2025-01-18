library(brms) # for the analysis
library(tidyverse) # needed for data manipulation.
library(RColorBrewer)
library(ggmcmc)
library(ggthemes)
library(ggridges)

sampled_data <- read.csv("/Users/anirudhparameswaran/Desktop/worldvaluessurvey-main/resample_data.csv", header=TRUE, sep = ",")

head(sampled_data, n=3)

sampled_data$sex <- factor(sampled_data$sex)
sampled_data$immigrant <- factor(sampled_data$immigrant)
sampled_data$voter_2 <- factor(sampled_data$voter_2)
sampled_data$voter_4 <- factor(sampled_data$voter_4, ordered = TRUE)
sampled_data$country <- factor(sampled_data$country)

formula1 <- voter_2 ~ age + sex + (1 | country)

prior1 <- c(
  # 1% higher chance of voting per year of age
  prior(normal(0.04, 0.01), class = "b", coef = "age"),
  
  # more than 1% chance of voting if respondent is male
  prior(normal(0.05, 0.1), class = "b", coef = "sexMale"),
  
  # Variability in effects across countries (USA vs. India)
  prior(normal(0.15, 0.05), class = "sd", coef = "Intercept", group = "country")
)

model1 <- brm(formula1,
              family = bernoulli(link = "logit"),
              data = sampled_data,
              iter = 1500,
              chains = 2,
              prior = prior1,
              control = list(max_treedepth = 15, adapt_delta = 0.999),
              cores = 4,
              seed = 42)

summary(model1)

formula2 <- voter_2 ~ age + sex + education + income_level + (1 | country)

prior2 <- c(
  # educated people have a higher chance of voting based on this https://pmc.ncbi.nlm.nih.gov/articles/PMC10225039/
  prior(normal(0.2, 0.1), class = "b", coef = "education"),
  # people with higher income are more likely to vote based on this https://econofact.org/voting-and-income
  prior(normal(0.1, 0.1), class = "b", coef = "income_level"),
  # 1% higher chance of voting per year of age
  prior(normal(0.04, 0.01), class = "b", coef = "age"),
  # more than 1% chance of voting if respondent is male
  prior(normal(0.05, 0.1), class = "b", coef = "sexMale"),
  # Variability in effects across countries (USA vs. India)
  prior(normal(0.15, 0.05), class = "sd", coef = "Intercept", group = "country")
)

model2 <- brm(formula2,
              family = bernoulli(link = "logit"),
              data = sampled_data,
              iter = 1500,
              chains = 2,
              prior = prior2,
              control = list(max_treedepth = 15, adapt_delta = 0.999),
              cores = 4,
              seed = 42)
summary(model2)

loo(model1, model2)
