library(rjags)
library(runjags)
library(tidyverse)



cbb_data <- read_csv('/Users/viren1/Documents/College/Python/Datasets/cbb.csv')


cbb_data 



glimpse(cbb_data)


likelihood_data <- cbb_data %>% filter(YEAR == 2019)

prior_data <- cbb_data %>% filter(!(YEAR == 2019))


likelihood_data$YEAR

prior_data$YEAR


likelihood_data

prior_data



glimpse(likelihood_data)

glimpse(prior_data)


prior_data %>% ggplot(aes(W, fill='red')) + geom_density(bw=2) + labs(x = 'Wins in prior data before 2019')

likelihood_data %>% ggplot(aes(W)) + geom_density(bw=2,  fill='lightblue') + labs(x = 'Wins in likelihood data during 2019')



mean(prior_data$W)

sd(prior_data$W)


# Y ~ Normal(mu, s^2)

# mu ~ Normal(16, 6^2)

# s ~ Uniform(0, 10)



cbb_model <- "model{

for (i in 1:length(Y)) {

Y[i] ~ dnorm(m, s^(-2))

}

m ~ dnorm(16, 6^(-2))

s ~ dunif(0, 10)


}"

mu <- rnorm(10000, 16, 6)

sd <- runif(10000, 0, 10)

hist(m)
plot(density(m))

priors_parameters <- data.frame(mu, sd)

cbb_connection <- jags.model(textConnection(cbb_model), data = list(Y = likelihood_data$W), inits = list(.RNG.name="base::Wichmann-Hill", .RNG.seed = 1989), n.chains = 4)

cbb_samples <- coda.samples(cbb_connection, variable.names = c("m", "s"), n.iter=1000)

cbb_advanced_samples <- coda.samples(cbb_connection, variable.names = c("m", "s"), n.iter = 10000)


summary(cbb_samples)
summary(cbb_advanced_samples)

plot(cbb_samples)
plot(cbb_advanced_samples)



head(cbb_advanced_samples)


mymodel_mat <- as.matrix(cbb_advanced_samples)

mymodel_df <- as.data.frame(mymodel_mat)


mymodel_df %>% ggplot(aes(m)) + geom_histogram(fill='lightblue')

mymodel_df %>% ggplot(aes(m)) + geom_density(fill='lightblue')


combined_post_prior <- cbind(mymodel_df, priors_parameters)

ggplot(combined_post_prior) + geom_density(aes(mu), color='red') + geom_density(aes(m), color='darkblue') + geom_vline(xintercept = mean(combined_post_prior$m))

ggplot(combined_post_prior) + geom_density(aes(sd), color='red') + geom_density(aes(s), color='darkblue') geom_vline(xintercept = mean(combined_post_prior$s))

# Regression Model


prior_data %>% ggplot(aes(`2P_O`, W)) + geom_jitter()

regression_data <- cbb_data %>% filter(YEAR == 2019) %>% select(W, `2P_O`)

regression_data

max(prior_data$`2P_O`)

cbb_lm <- lm(W ~ `2P_O`, prior_data)

summary(cbb_lm)

broom::augment(cbb_lm)

coef(cbb_lm)

# Yi ~ Normal(mi, s^2)

# mi <- a + b  * Xi

# a ~ Normal(0, 70^2)

# b ~ Normal(1, 0.5^2)

#s ~ Unif(0, 10)

cbb_regression_model <- "model {

for (i in 1:length(Y)) {

Y[i] ~ dnorm(m[i], s^(-2))

m[i] <- a + b * X[i]

}

#priors

a ~ dnorm(0, 70^(-2))

b ~ dnorm(1, 0.5^(-2))

s ~ dunif(0, 10)


}"

cbb_reg_mod <- jags.model(textConnection(cbb_regression_model), data = list(X = regression_data$`2P_O`, Y = regression_data$W), inits =list(.RNG.name="base::Wichmann-Hill", .RNG.seed = 1989), n.chains = 4)


cbb_chains <- coda.samples(cbb_reg_mod, variable.names = c("a", "b", "s"), n.iter=1000)

cbb_more_chains <- coda.samples(cbb_reg_mod, variable.names = c("a", "b", "s"), n.iter = 10000)

cbb_even_more_chains <- coda.samples(cbb_reg_mod, variable.names = c("a", "b", "s"), n.iter = 100000)

summary(cbb_chains)
summary(cbb_more_chains)
summary(cbb_even_more_chains)

plot(cbb_chains)
plot(cbb_more_chains)
plot(cbb_even_more_chains)

my_reg_mod_mat <- as.matrix(cbb_even_more_chains)

my_reg_mod_df <- as.matrix(my_reg_mod_mat)





prior_a <- rnorm(100000, 0, 70)

prior_b <- rnorm(100000, 1, 0.5)

prior_s <- runif(100000, 0, 10)

reg_priors_df <- data.frame(prior_a, prior_b, prior_s)



combined_reg_df <- cbind(my_reg_mod_df, reg_priors_df)

head(combined_reg_df)


#Posterior v. Prior : a

ggplot(combined_reg_df) + geom_density(aes(prior_a), color='red') + geom_density(aes(a), color='darkblue') + geom_vline(xintercept = mean(combined_reg_df$a, color='darkred'))

#Posterior v. Prior: b
ggplot(combined_reg_df) + geom_density(aes(prior_b), color='red') + geom_density(aes(b), color='darkblue') + geom_vline(xintercept = mean(combined_reg_df$b, color='darkred'))



#Posterior v, Prior: s
ggplot(combined_reg_df) + geom_density(aes(prior_s), color='red') + geom_density(aes(s), color='darkblue') + geom_vline(xintercept = mean(combined_reg_df$s, color='darkred'))
