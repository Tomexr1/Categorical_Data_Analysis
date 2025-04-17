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