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
sampled_data$country <- factor(sampled_data$country)

formula1 <- voter_2 ~ age + sex + immigrant + income_level + education + god_importance + praying_frequency + ethics_score + satisfaction + (1 | country)

get_prior(formula1, data = sampled_data)

model1 <- brm(formula1,
              family = bernoulli(link = "logit"),
              data = sampled_data,
              iter = 1500,
              chains = 2,
              control = list(max_treedepth = 15, adapt_delta = 0.999),
              cores = 4,
              seed = 42)

summary(model1)

saveRDS(model1, file = "/Users/anirudhparameswaran/Desktop/worldvaluessurvey-main/model1.rds")

formula2 <- voter_2 ~ age + sex + immigrant + income_level + education + god_importance + praying_frequency + ethics_score + satisfaction + (1 + sex + age + education + income_level | country)

prior1 <- c(
  # Older people tend to vote more than younger people
  prior(normal(5, 10), class = "b", coef = "age"),
  
  # Proportion of men voting is higher than women
  prior(normal(5, 10), class = "b", coef = "sexMale"),
  
  # Poor people tend to vote more often than rich people
  prior(normal(-5, 10), class = "b", coef = "income_level"),
  
  # Educated people tend to vote more often
  prior(normal(5, 10), class = "b", coef = "education"),
  
  # there is high variance in data between USA and IND in terms of the relationship of sex, income_level and education
  prior(cauchy(5, 10), class = "sd", group = "country", coef = "sexMale"),
  prior(cauchy(5, 10), class = "sd", group = "country", coef = "education"),
  prior(cauchy(5, 10), class = "sd", group = "country", coef = "income_level")
)

model2 <- brm(formula2,
              family = bernoulli(link = "logit"),
              data = sampled_data,
              iter = 1500,
              chains = 2,
              prior = prior1,
              control = list(max_treedepth = 20, adapt_delta = 0.999),
              cores = 4,
              seed = 42)

summary(model2)

saveRDS(model2, file = "/Users/anirudhparameswaran/Desktop/worldvaluessurvey-main/model2.rds")

prior2 <- c(
  # 1% higher chance of voting per year of age
  prior(normal(0.04, 0.01), class = "b", coef = "age"),
  
  # more than 1% chance of voting if respondent is male
  prior(normal(0.05, 0.1), class = "b", coef = "sexMale"),
  
  # Variability in effects across countries (USA vs. India)
  prior(normal(0.15, 0.05), class = "sd", coef = "Intercept", group = "country")
)

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
