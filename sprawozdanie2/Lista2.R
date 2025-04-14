# zad 1
library(dplyr)
library(binom)
res <- c(14, 17, 40, 100, 29)
alpha <- 0.05

binom.confint(res, 200, 1 - alpha/5, method='exact')
binom.confint(res, 200, 1 - alpha/5, method='wilson')
binom.confint(res, 200, 1 - alpha/5, method='asymptotic')

# Wnioski

# zad 2
chi_sq_test <- function(x, p0, method='pearson') {
  if (method == 'pearson') {
    n <- sum(x)
    Chi2 <- sum((x - n * p0)^2 / (n * p0))
    p_val <- 1 - pchisq(Chi2, length(x) - 1)
    print(paste("p-value:", p_val))
  }
  if (method == 'IW') {
    n <- sum(x)
    G2 <- 2 * sum(x * log(x / (n * p0)))
    p_val <- 1 - pchisq(G2, length(x) - 1)
    print(paste("p-value:", p_val))
  }
}

chi_sq_test(res, c(0.1, 0.2, 0.4, 0.2, 0.1), method='pearson')
chi_sq_test(res, c(0.1, 0.2, 0.4, 0.2, 0.1), method='IW')

# zad 3
ankieta <- read.csv('/Users/tomasz/Politechnika/semestr_6/ankiety/ankieta.csv', header = TRUE, sep = ";", fileEncoding = "Latin2")
vec <- subset(ankieta, DZIAŁ == 'PD') %>% group_by(PYT_1) %>%
  summarise(n = n()) %>% select(n)
vec <- as.numeric(vec$n)

chi_sq_test(vec, rep(1/5, 5), method='pearson')  
chi_sq_test(vec, rep(1/5, 5), method='IW')
