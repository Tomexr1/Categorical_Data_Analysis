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

# zad 4
# fisher.test

# zad 5
contingency_table <- table(ankieta$PŁEĆ, ankieta$CZY_KIER)
fisher.test(contingency_table, conf.level = 0.95)
# p-value > 0.05 => cechy są niezależne, prawdopodobieństwa są równe

# zad 6
ankieta['WIEK_KAT'] <- ifelse(ankieta$WIEK < 36, 'młody', ifelse(ankieta$WIEK < 46, 'średni', ifelse(ankieta$WIEK < 56, 'starszy', 'emerytura')))
ankieta['CZY_ZADOW'] <- ifelse(ankieta$PYT_2 %in% c(1, 2), 'zadowolony', 'niezadowolony')
# a)
t <- table(ankieta$CZY_KIER, ankieta$WIEK_KAT)
fisher.test(t, conf.level = 0.95) 
# p-value > 0.05 => zajmowanie stanowiska kierowniczego nie zależy do wieku
# b)
t <- table(ankieta$CZY_KIER, ankieta$STAŻ)
fisher.test(t, conf.level = 0.95)
# p-value < 0.05 => zajmowanie stanowiska kierowniczego zależy od stażu pracy
# c)
t <- table(ankieta$PYT_2, ankieta$CZY_KIER)
fisher.test(t, conf.level = 0.95)
# p-value < 0.05 => zadowolenie ze szkoleń zależy od zajmowanego stanowiska
# d)
t <- table(ankieta$PYT_2, ankieta$STAŻ)
fisher.test(t, conf.level = 0.95)
# p-value < 0.05 => zadowolenie ze szkoleń zależy od stażu pracy
# e)
t <- table(ankieta$PYT_2, ankieta$PŁEĆ)
fisher.test(t, conf.level = 0.95)
# p-value > 0.05 => zadowolenie ze szkoleń nie zależy od płci
# f)
t <- table(ankieta$PYT_2, ankieta$WIEK_KAT)
fisher.test(t, conf.level = 0.95, workspace = 2e7)
# p-value > 0.05 => zadowolenie ze szkoleń nie zależy od wieku
# c)'
t <- table(ankieta$CZY_ZADOW, ankieta$CZY_KIER)
fisher.test(t, conf.level = 0.95)
# p-value > 0.05 => ogólniejsze zadowolenie ze szkoleń nie zależy od zajmowanego stanowiska
# d)'
t <- table(ankieta$CZY_ZADOW, ankieta$STAŻ)
fisher.test(t, conf.level = 0.95)
# p-value > 0.05 => ogólniejsze zadowolenie ze szkoleń nie zależy od stażu pracy
# e)'
t <- table(ankieta$CZY_ZADOW, ankieta$PŁEĆ)
fisher.test(t, conf.level = 0.95)
# p-value > 0.05 => ogólniejsze zadowolenie ze szkoleń nie zależy od płci
# f)'
t <- table(ankieta$CZY_ZADOW, ankieta$WIEK_KAT)
fisher.test(t, conf.level = 0.95)
# p-value > 0.05 => ogólniejsze zadowolenie ze szkoleń nie zależy od wieku

# zad 7
# chi2 independence test chisq.test
# install.packages("vcd")
library(grid)
library(vcd)

# zad 8
t <- table(ankieta$PYT_2, ankieta$CZY_KIER)

summary(t)

res <- chisq.test(t, correct = TRUE)
res$p.value

assocstats(t)
# p-value < 0.05 => zadowolenie ze szkoleń zależy od zajmowanego stanowiska
library(ggstatsplot)
library(ggplot2)

# plot
ggbarstats(
  data = ankieta,
  x = PYT_2,
  y = CZY_KIER,
) +
  labs(caption = NULL) # remove caption

mosaic(~ PYT_2 + CZY_KIER,
       direction = c("v", "h"),
       data = ankieta,
       shade = TRUE
)
# zad 9
data <- rmultinom(1, size = 50, prob = c(1/40, 19/40, 3/40, 17/40))
data <- matrix(data, nrow = 2)
data

set.seed(123)  # dla powtarzalności

# Parametry
prob <- c(1, 3, 19, 17) / 40
alpha <- 0.05
n_iter <- 10000
n_values <- c(50, 100, 1000)

# Funkcja do przeprowadzenia symulacji
simulate_power <- function(n, iter, prob, alpha) {
  fisher_rejections <- 0
  chisq_rejections <- 0
  
  for (i in 1:iter) {
    sample <- rmultinom(1, size = n, prob = prob)
    table <- matrix(sample, nrow = 2, byrow = TRUE)
    
    fisher_p <- fisher.test(table)$p.value
    chisq_p <- suppressWarnings(chisq.test(table, correct = FALSE, simulate.p.value = TRUE)$p.value)
    
    iter_chisq <- iter
    
    if (fisher_p < alpha) {
      fisher_rejections <- fisher_rejections + 1
    } 
    if (!is.na(chisq_p) && chisq_p < alpha) {
      chisq_rejections <- chisq_rejections + 1
    }
    if (is.na(chisq_p)) {
      iter_chisq <- iter_chisq - 1
    }
  }
  fisher_power <- fisher_rejections / iter
  chisq_power <- chisq_rejections / iter_chisq
  
  return(c(Fisher = fisher_power, ChiSq = chisq_power))
}

# Wykonanie symulacji
results <- sapply(n_values, function(n) simulate_power(n, n_iter, prob, alpha))
colnames(results) <- paste("n =", n_values)
print(round(results, 4))

# zad 10
# Napisz funkcje ̨, która dla danych z tablicy dwudzielczej oblicza wartos ́c ́ poziomu krytycznego w tes ́cie niezalez ̇nosci opartym na ilorazie wiarogodnosci.
lr_test_pvalue <- function(table) {
  # Obserwacje
  observed <- table
  total <- sum(observed)
  
  # Oczekiwane liczności pod H0 (niezależność)
  row_totals <- rowSums(observed)
  col_totals <- colSums(observed)
  expected <- outer(row_totals, col_totals) / total
  
  # Wykluczenie zer (dla bezpieczeństwa w log)
  valid <- observed > 0
  
  # Obliczenie statystyki G^2
  G2 <- 2 * sum(observed[valid] * log(observed[valid] / expected[valid]))
  
  # Stopnie swobody: (r-1)(c-1)
  df <- (nrow(observed) - 1) * (ncol(observed) - 1)
  
  # Obliczenie p-value
  p_value <- 1 - pchisq(G2, df = df)
  
  return(p_value)
}


t <- table(ankieta$PYT_2, ankieta$CZY_KIER)
lr_test_pvalue(t)

table <- matrix(c(20, 30, 25, 25), nrow = 2)
lr_test_pvalue(table)

# zad 11
t <- matrix(c(0.00140, 0.00010, 0.00669, 0.00413), nrow = 2)
rownames(t) <- c("Palenie", "Nie palenie")
colnames(t) <- c("Rak płuc", "Choroba niedokrwienna serca")
RP_p <- t[1, 1] - t[2, 1]
RP_p # chyba male, brak zaleznosci
RP_s <- t[1, 2] - t[2, 2]
RP_s # chyba male, brak zaleznosci
RR_p <- t[1, 1] / t[2, 1]
RR_s <- t[1, 2] / t[2, 2]
RR_p # duze ryzyko, moze jednak byc zaleznosc
RR_s # blisko jedynki, brak zaleznosci
OR_p <- t[1, 1] / (1 - t[1, 1]) / (t[2, 1] / (1 - t[2, 1]))
OR_s <- t[1, 2] / (1 - t[1, 2]) / (t[2, 2] / (1 - t[2, 2]))
OR_p # duze ryzyko, moze jednak byc zaleznosc
OR_s # blisko jedynki, brak zaleznosci

# zad 12
t <- matrix(c(1085, 703, 55623, 441239), nrow = 2)
rownames(t) <- c('bez pasów', 'z pasami')
colnames(t) <- c('śmiertelny', 'nieśmiertelny')
t
# 12.1
pi1 <- t[1, 1] / sum(t[1, ])
pi2 <- t[2, 1] / sum(t[2, ])
pi1
pi2
# 12.2
pi1 <- t[1, 1] / sum(t[, 1])
pi2 <- t[2, 1] / sum(t[, 2])
pi1
pi2




# zad 13
library(DescTools)
# 13.1
t <- table(ankieta$CZY_ZADOW, ankieta$CZY_KIER)
GoodmanKruskalTau(t)
# z freemana-haltona wyszło, że są zależne, tau jest bliskie 0, czyli słaba zależność
# 13.2
t <- table(ankieta$CZY_ZADOW, ankieta$STAŻ)
GoodmanKruskalTau(t)
# z freemana-haltona wyszło, że są niezależne, tau jest bliskie 0, potwierdza to
t <- table(ankieta$PYT_2, ankieta$STAŻ)
GoodmanKruskalGamma(t)
# z freemana-haltona wyszło, że są zależne, gamma wskazuje na lekką dodatnią zależność 
# 13.3
t <- table(ankieta$DZIAŁ, ankieta$STAŻ)
fisher.test(t, conf.level = 0.95, workspace = 2e7)
GoodmanKruskalTau(t)
# tau wykazuje zależność silniejszą niz w przypadku poprzednich zmiennych; z freemana-haltona wyszło, że są zależne z bardzo małym pvalue

# zad 14
library(ggplot2)
library(expm)

correspondence_analysis <- function(t, variables = c("row", "col")) {
  P <- prop.table(t)
  r <- rowSums(P)
  c <- colSums(P)
  
  D_r <- diag(r)
  R <- solve(D_r) %*% P
  D_c <- diag(c)
  C <- P %*% solve(D_c)
  
  A <- solve(sqrtm(D_r)) %*% (P - r %*% t(c)) %*% solve(sqrtm(D_c))

  SVD <- svd(A)
  U <- SVD$u
  V <- SVD$v
  D <- diag(SVD$d)
  
  F <- solve(sqrtm(D_r)) %*% U %*% D
  G <- solve(sqrtm(D_c)) %*% V %*% D
  print(F)
  print(G)
  
  row_std <- F[, 1:2]
  col_std <- G[, 1:2]
  
  ggplot(data.frame(x = c(row_std[, 1], col_std[, 1]),
                    y = c(row_std[, 2], col_std[, 2]),
                    label = c(rownames(t), colnames(t)),
                    variable = c(rep(variables[1], nrow(t)), rep(variables[2], ncol(t)))), 
         aes(x = x, y = y, color = variable)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
    geom_point(aes(x = x, y = y)) +
    geom_text(aes(x = x, y = y, label = label), vjust = 1.5) +
    labs(x = "Dim 1", y = "Dim 2") +
    theme_minimal()
}
t <- table(ankieta$PYT_2, ankieta$STAŻ)
correspondence_analysis(t, variables = c("PYT_2", "STAŻ"))


# zad dodatkowe 1
dCor_pvalue <- function(x, y) {
  
  dCor <- function(x, y) {
    observed <- table(x, y)
    total <- sum(observed)
    
    probs <- prop.table(observed)
    row_probs <- rowSums(probs)
    col_probs <- colSums(probs)
    
    numerator <- 0
    
    for (i in 1:nrow(observed)) {
      for (j in 1:ncol(observed)) {
        numerator <- numerator + (probs[i, j] - row_probs[i] * col_probs[j])^2
      }
    }
    numerator <- sqrt(numerator)
    
    denominator <- (sum(row_probs^2 * (sum(row_probs^2) + 1)) - 2*sum(row_probs^3))^0.25*
      (sum(col_probs^2 * (sum(col_probs^2) + 1)) - 2*sum(col_probs^3))^0.25
    return(numerator / denominator)
  }
  
  statistic <- dCor(x, y)
  
  for (i in 1:1000) {
    x_perm <- sample(x)
    y_perm <- sample(y)
    
    statistic_perm <- dCor(x_perm, y_perm)
    
    if (statistic_perm >= statistic) {
      p_value <- (i + 1) / 1000
      break
    }
  }
  return(p_value)
}

x <- ankieta$PYT_2
y <- ankieta$CZY_KIER
dCor_pvalue(ankieta$PYT_2, ankieta$CZY_KIER)
