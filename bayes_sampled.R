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
              data = sampled_data,
              iter = 1500,
              chains = 4,
              prior = prior1,
              control = list(max_treedepth = 17, adapt_delta = 0.999),
              cores = 4,
              seed = 42)

summary(model2)

conditional_effects(model2)

transformed <- ggs(model2)
ggplot(filter(transformed, Parameter %in% colnames(as_draws_df(model2))),
       aes(x   = Iteration,
           y   = value, 
           col = as.factor(Chain)))+
  geom_line() +
  geom_vline(xintercept = 1000)+
  facet_grid(Parameter ~ . ,
             scale  = 'free_y',
             switch = 'y')+
  labs(title = "Caterpillar Plots", 
       col   = "Chains")

library(bayesplot)
mcmc_trace(model2)

plot(model2)

ggplot(filter(transformed,
              Parameter == "b_education", 
              Iteration > 999),
       aes(x = value)) +
  geom_density(fill = "yellow", alpha = 0.5) +
  geom_vline(xintercept = 0, col = "red", linewidth = 1) + 
  scale_x_continuous(name = "Value", limits = c(-1, 3)) +
  geom_vline(xintercept = summary(model1test)$fixed["education", "l-95% CI"],
             col = "blue", linetype = 2) +
  geom_vline(xintercept = summary(model1test)$fixed["education", "u-95% CI"],
             col = "blue", linetype = 2) +
  theme_light() +
  labs(title = "Posterior Density of Education")

pp_check(model2, type = 'hist', bins=3, binwidth = 0.5, ndraws = 5) + theme(aspect.ratio = 1)
pp_check(model2, type = "stat", stat = "mean")
pp_check(model2, type = "stat", stat = "sd")

saveRDS(model2, file = "/Users/anirudhparameswaran/Desktop/worldvaluessurvey-main/model2.rds")

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
              data = sampled_data,
              iter = 2000,
              chains = 4,
              prior = prior1,
              control = list(max_treedepth = 15, adapt_delta = 0.999),
              cores = 26,
              seed = 42)
summary(model3)

loo(model1, model2, model3)

saveRDS(model3, file = "/Users/anirudhparameswaran/Desktop/worldvaluessurvey-main/model3.rds")
