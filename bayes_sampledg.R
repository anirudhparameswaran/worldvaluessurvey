library(brms) # for the analysis
library(tidyverse) # needed for data manipulation.
library(RColorBrewer)
library(ggmcmc)
library(ggthemes)
library(ggridges)
library(cmdstanr)

sampled_data <- read.csv("full_dataset.csv", header=TRUE, sep = ",")

model1 <- readRDS("model1g.rds")
model2 <- readRDS("model2g.rds")
model3 <- readRDS("model3g.rds")

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

saveRDS(model1, file = "model1.rds")

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
              data = sampled_data,
              iter = 2000,
              chains = 4,
              prior = prior1,
              control = list(max_treedepth = 15, adapt_delta = 0.999),
              cores = 26,
              seed = 42)
summary(model3)

loo(model2, model3)

saveRDS(model3, file = "model3.rds")

library(dplyr)

filtered_data_ind <- sampled_data %>% 
  filter(country == "IND") %>%
  slice_max(order_by = age, n = 1000)

filtered_data_usa <- sampled_data %>% 
  filter(country == "USA") %>%
  tail(1000)

filtered_data <- rbind(filtered_data_ind, filtered_data_usa)

filtered_data <- filtered_data[, c("voter_2", "age", "sex", "education", "country")]

print(filtered_data)

formula4 <- voter_2 ~ age * sex + education
formula5 <- voter_2 ~ age:sex + education

model4 <- brm(formula4,
              family = bernoulli(link = "logit"),
              data = filtered_data,
              iter = 1000,
              chains = 2,
              control = list(max_treedepth = 17),
              seed = 42,
              options(brms.backend = "cmdstanr"),
              threads = parallel::detectCores())
summary(model4)

formula6 <- voter_2 ~ country+age+sex+education+country:education
formula7 <- voter_2 ~ country+age+sex+education+country:(education + age)

prior5 <- c(
  prior(normal(1, 3), class = "b", coef = "age"),
  prior(normal(1, 3), class = "b", coef = "sexMale"),
  prior(normal(1, 3), class = "b", coef = "education"),
  prior(normal(1, 3), class = "b", coef = "countryUSA"),
  prior(normal(1, 3), class = "b", coef = "countryUSA:education")
)

model5 <- brm(formula6,
              family = bernoulli(link = "logit"),
              data = filtered_data,
              iter = 1000,
              chains = 2,
              control = list(max_treedepth = 17),
              seed = 42,
              cores = 8,
              prior = prior5)

prior6 <- c(
  prior(normal(1, 3), class = "b", coef = "age"),
  prior(normal(1, 3), class = "b", coef = "sexMale"),
  prior(normal(1, 3), class = "b", coef = "education"),
  prior(normal(1, 3), class = "b", coef = "countryUSA"),
  prior(normal(1, 3), class = "b", coef = "countryUSA:education"),
  prior(normal(1, 3), class = "b", coef = "countryUSA:age")
)

model6 <- brm(formula7,
              family = bernoulli(link = "logit"),
              data = sampled_data,
              iter = 1000,
              chains = 2,
              control = list(max_treedepth = 17),
              seed = 42,
              cores = 8,
              prior = prior6)
summary(model6)
