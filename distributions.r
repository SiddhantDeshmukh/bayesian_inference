library(tidyverse)
library(patchwork)
# Binomial: in n events, get r successes with probability p for each success
# P(r|p, n) = nCr p^r (1-p)^r
# E[r] = np, Var(r) = np(1 - p)
# P vs r for fixed n, range of p
n <- 10 # num trials
r <- 0:n # num successes
pseq <- c(0.1, 0.2, 0.5, 0.8, 0.9) # chance of success
binom_plot1 <- expand_grid(r = r, p = pseq) %>%
  mutate(probability = dbinom(x = r, size = n, prob = p)) %>%
  # Above here we created the tibble, below here is the pipe into ggplot
  ggplot(aes(x = r, y = probability, color = factor(p))) +
  geom_point() +
  geom_line(lty = 2) +
  labs(x = "r (Number of successes)", y = "P(r | p,n)") +
  scale_color_discrete(name = "p") +
  theme_minimal()

# P vs p for range of r
p <- seq(0., 1., 0.001) # chance of success
rseq <- c(0, 1, 3, 5) # number of successes
binom_plot2 <- expand_grid(r = rseq, p = p) %>%
  mutate(probability = dbinom(x = r, size = n, prob = p)) %>%
  ggplot(aes(x = p, y = probability, color = factor(r))) +
  geom_point() +
  geom_line(lty = 2) +
  labs(x = "p", y = "P(r | p,n)") +
  scale_color_discrete(name = "r") +
  theme_minimal()

# Plot both
combined_binom_plot <- (binom_plot1 + binom_plot2) +
  plot_layout(ncol = 2) +
  ggtitle("Binomial Distribution")
print(combined_binom_plot)

# Poisson: probability of getting r events if mean expected number is lambda
# P(r | lambda) = (lambda^r * e^-lambda) / r!; lambda, r > 0
# E[r] = lambda, Var(r) = lambda
# Plot P(r|lambda) as func(r)
r <- 0:20
lambda <- c(1, 2, 3.5, 10)
poisson_plot <- expand_grid(r = r, lambda = lambda) %>%
  mutate(probability = dpois(x = r, lambda = lambda)) %>%
  ggplot(aes(x = r, y = probability, color = factor(lambda))) +
  geom_point(shape = 1) +
  geom_line(linetype = "dashed") +
  labs(title = "Poisson Distribution", x = "r", y = "P(r | lambda)") +
  scale_color_discrete(name = "lambda") +
  theme_minimal()
print(poisson_plot)
