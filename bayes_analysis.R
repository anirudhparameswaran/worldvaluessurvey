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
data$voter <- factor(data$voter, ordered = TRUE)
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
    family = cumulative(),
    data   = data, 
    iter   = 500, 
    chains = 2, 
    control = list(max_treedepth = 15, adapt_delta = 0.99),
    cores  = 4,
    seed   = 42)

summary(interceptonlymodeltest)

model1 <- voter ~ 1 + age + sex + income_level + education + god_importance + (1 | country)

model1test <- brm(model1,
                  family = cumulative(),
                  data = data,
                  iter = 1000,
                  chains = 2,
                  control = list(max_treedepth = 15, adapt_delta = 0.99),
                  cores = 2,
                  seed = 42)

summary(model1test)

model1transformed <- ggs(model1test)
ggplot(filter(model1transformed, Parameter %in% colnames(as_draws_df(model1test))),
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

ggplot(filter(model1transformed,
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

