library(brms) # for the analysis
library(tidyverse) # needed for dataset manipulation.
library(RColorBrewer)
library(ggmcmc)
library(ggthemes)
library(ggridges)
library(cmdstanr)
library(dplyr)

dataset <- read.csv("full_datasetset.csv", header=TRUE, sep = ",")

dataset$sex <- factor(dataset$sex)
dataset$immigrant <- factor(dataset$immigrant)
dataset$voter_2 <- factor(dataset$voter_2)
dataset$country <- factor(dataset$country)

formula2 <- voter_2 ~ age + sex + immigrant + income_level + education + god_importance + praying_frequency + ethics_score + satisfaction + (1 + sex + age + education + income_level | country)

prior1 <- c(
  prior(normal(1, 3), class = "b", coef = "age"),
  prior(normal(1, 3), class = "b", coef = "sexMale"),
  prior(normal(-1, 3), class = "b", coef = "income_level"),
  prior(normal(1, 3), class = "b", coef = "education"),
  
  prior(student_t(3, 2, 3), class = "sd", group = "country", coef = "sexMale"),
  prior(student_t(3, 2, 3), class = "sd", group = "country", coef = "education"),
  prior(student_t(3, 2, 3), class = "sd", group = "country", coef = "income_level")
)

model2 <- brm(formula2,
              family = bernoulli(link = "logit"),
              dataset = dataset,
              iter = 1500,
              chains = 4,
              prior = prior1,
              control = list(max_treedepth = 17, adapt_delta = 0.999),
              cores = 4,
              seed = 42)

summary(model2)

library(bayesplot)
mcmc_trace(model2)

plot(model2)

saveRDS(model2, file = "model2.rds")

prior2 <- c(
  prior(normal(0.006, 0.05), class = "b", coef = "age"),
  prior(normal(-0.088, 0.05), class = "b", coef = "sexMale"),
  prior(normal(0, 0.5), class = "b", coef = "income_level"),
  prior(normal(1, 0.5), class = "b", coef = "education"),
  prior(normal(1, 0.05), class = "b", coef = "immigrantNotimmigrant"),
  
  prior(student_t(3, 0.05, 3), class = "sd", group = "country", coef = "sexMale"),
  prior(student_t(3, 0.05, 3), class = "sd", group = "country", coef = "education"),
  prior(student_t(3, 0.05, 3), class = "sd", group = "country", coef = "income_level")
)

formula2 <- voter_2 ~ age + sex + education + income_level + immigrant + praying_frequency + (1 + age + sex + education + income_level | country)

model3 <- brm(formula2,
              family = bernoulli(link = "logit"),
              dataset = dataset,
              iter = 2000,
              chains = 4,
              prior = prior1,
              control = list(max_treedepth = 15, adapt_delta = 0.999),
              cores = 26,
              seed = 42)
summary(model3)

saveRDS(model3, file = "model3.rds")

formula6 <- voter_2 ~ country+age+sex+education+country:education
formula7 <- voter_2 ~ country+age+sex+education+country:(education + age)

prior6 <- c(
  prior(normal(1, 3), class = "b", coef = "age"),
  prior(normal(1, 3), class = "b", coef = "sexMale"),
  prior(normal(1, 3), class = "b", coef = "education"),
  prior(normal(1, 3), class = "b", coef = "countryUSA"),
  prior(normal(1, 3), class = "b", coef = "countryUSA:education")
)

model6 <- brm(formula6,
              family = bernoulli(link = "logit"),
              dataset = filtered_dataset,
              iter = 1000,
              chains = 2,
              control = list(max_treedepth = 17),
              seed = 42,
              cores = 8,
              prior = prior5)

prior7 <- c(
  prior(normal(1, 3), class = "b", coef = "age"),
  prior(normal(1, 3), class = "b", coef = "sexMale"),
  prior(normal(1, 3), class = "b", coef = "education"),
  prior(normal(1, 3), class = "b", coef = "countryUSA"),
  prior(normal(1, 3), class = "b", coef = "countryUSA:education"),
  prior(normal(1, 3), class = "b", coef = "countryUSA:age")
)

model7 <- brm(formula7,
              family = bernoulli(link = "logit"),
              dataset = dataset,
              iter = 1000,
              chains = 2,
              control = list(max_treedepth = 17),
              seed = 42,
              cores = 8,
              prior = prior6)
summary(model7)

bayesplot::mcmc_rhat(rhat(model7))
pp_check(model7, type = "ecdf_overlay", ndraws = 100)
