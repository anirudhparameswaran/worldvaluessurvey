library(brms) # for the analysis
library(tidyverse) # needed for data manipulation.
library(RColorBrewer) # needed for some extra colours in one of the graphs
library(ggmcmc)
library(ggthemes)
library(ggridges)

data <- read.csv("/Users/anirudhparameswaran/Desktop/worldvaluessurvey-main/cleaned_dataset.csv", header=TRUE, sep = ",")

head(data, n=3)

data$sex <- factor(data$sex)
data$immigrant <- factor(data$immigrant)
# data$voter <- factor(data$voter)
data$country <- factor(data$country)

ggplot(data  = data,
       aes(x = education,
           y = voter))+
  geom_point(size = 1.2,
             alpha = .8,
             position = "jitter")+# to add some random noise for plotting purposes
  theme_minimal()+
  labs(title = "Education vs Voting Pattern")

InterceptOnlyModel <- voter ~ 1 + (1 | country)

interceptonlymodeltest <- brm(InterceptOnlyModel, 
    data   = data, 
    warmup = 3000, 
    iter   = 8000, 
    chains = 5, 
    cores  = 2,
    seed   = 42)

summary(interceptonlymodeltest)

model1 <- voter ~ 1 + age + sex + income_level + education + god_importance + (1 | country)

model1test <- brm(model1,
                  family = bernoulli(),
                  data = data,
                  #warmup = 2000,
                  iter = 5000,
                  chains = 4,
                  control = list(max_treedepth = 15, adapt_delta = 0.99),
                  cores = 10,
                  seed = 42)

summary(model1test)

model1test2 <- brm(model1,
                  family = bernoulli(),
                  data = data,
                  iter = 2000,
                  chains = 2,
                  control = list(max_treedepth = 15, adapt_delta = 0.99),
                  cores = 2,
                  seed = 42)

summary(model1test2)

plot(fit)
pp_check(fit, ndraws = 100, type = 'ecdf_overlay')
