library(brms) # for the analysis
library(tidyverse) # needed for data manipulation.
library(RColorBrewer) # needed for some extra colours in one of the graphs
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

model1 <- voter_2 ~ 1 + age + sex + income_level + education + god_importance + satisfaction + immigrant + children + ethics_score + praying_frequency + (1 | country)

model1test <- brm(model1,
                  family = bernoulli(link = "logit"),
                  data = sampled_data,
                  iter = 1500,
                  chains = 2,
                  control = list(max_treedepth = 15, adapt_delta = 0.999),
                  cores = 4,
                  seed = 42)

summary(model1test)

get_prior(model1, data = sampled_data)

priors <- c(prior(normal(0, 1), class = Intercept),
            prior(normal(0.1, 0.1), class = b, coef = "age"))

model1test <- brm(model1,
                  family = bernoulli(link = "logit"),
                  data = sampled_data,
                  iter = 1500,
                  chains = 2,
                  prior = priors,
                  control = list(max_treedepth = 15, adapt_delta = 0.999),
                  cores = 4,
                  seed = 42)

summary(model1test)
