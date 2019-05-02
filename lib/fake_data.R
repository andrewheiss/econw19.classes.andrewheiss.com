library(tidyverse)

# set.seed(1234)
# fake_data <- MASS::mvrnorm(500, mu = c(20, 40), 
#                            Sigma = matrix(c(2, -0.1, -0.1, 2), ncol = 2),
#                            empirical = TRUE) %>% 
#   as_tibble() 
# 
# lm(V2 ~ V1, data = fake_data)
# 
# # ggplot(fake_data, aes(x = x, y = y)) +
# ggplot(fake_data, aes(x = V1, y = V2)) +
#   geom_point() + 
#   geom_smooth(method = "lm") #+
# coord_cartesian(xlim = c(0, 200), ylim = c(0, 20))
# 
# library(tidyverse)
# library(brms)
# library(tidybayes)
# 
# set.seed(1234)
# 
# random_data <- tibble(x = rnorm(100), y = rnorm(100))
# 
# model <- brm(brmsformula(y ~ x),
#              data = random_data, family = gaussian(),
#              prior = c(prior("normal(18, 0.1)", class = "b", coef = "x"),
#                        prior("normal(55, 0.1)", class = "Intercept")),
#              sample_prior = "only", chains = 1)
# summary(model)
# 
# posterior_samples(model) %>% head()
# 
# asdf <- posterior_predict(model)
# 
# dim(asdf)
# 


library(fabricatr)
library(truncnorm)
set.seed(12345)

wyoming <- fabricate(N = 312, 
                     x = rtruncnorm(N, a = 0, mean = 100, sd = 35), 
                     y = rtruncnorm(N, a = 0, mean = 20 + -0.05 * x, sd = 1.5)) %>% 
  mutate(state = "Wyoming")

newyork <- fabricate(N = 1367, 
                     x = rtruncnorm(N, a = 0, mean = 25, sd = 10), 
                     y = rtruncnorm(N, a = 0, mean = 70 + -1.4 * x, sd = 7)) %>% 
  mutate(state = "New York")

national <- fabricate(N = 2245, 
                      x = rtruncnorm(N, a = 0, mean = 25, sd = 15), 
                      y = rtruncnorm(N, a = 0, mean = 50 + -0.8 * x, sd = 4)) %>% 
  mutate(state = "National")

lm(y ~ x, data = national)

ggplot(national, aes(x = x, y = y)) +
  geom_point() + 
  geom_smooth(method = "lm")


both <- bind_rows(wyoming, newyork)

lm(y ~ x, data = wyoming)
lm(y ~ x, data = newyork)

ggplot(both, aes(x = x, y = y, color = state)) +
  geom_point() + 
  geom_smooth(method = "lm") +
  facet_wrap(~ state, scales = "free")



asdf <- random_data %>% 
  add_predicted_draws(model)

gather_draws(model)

data_grid()

predicted_draws(model = model, newdata = NULL)

fake_data <- posterior_predict(thing, draws = 10)

get_prior(brmsformula(y ~ x),
          data = bloop)
