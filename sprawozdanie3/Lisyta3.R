# zad 1

symmetry_test <- function(t) {
  if (sum(t) < 50) {
  n12 <- t[1, 2]
  n21 <- t[2, 1]
  
  if (n12 < (n12+n21)/2) {
    s <- 0
    for (i in 0:n12) {
      s <- s + choose(n12+n21, i) * 0.5^i * 0.5^(n12+n21-i)
    }
    p_value <- 2 * s
  }
  if (n12 > (n12+n21)/2) {
    s <- 0
    for (i in n12:n12+n21) {
      s <- s + choose(n12+n21, i) * 0.5^i * 0.5^(n12+n21-i)
    }
    p_value <- 2 * s
  }
  else {
    p_value <- 1
  }
  print(paste("p-value: ", p_value))
  }
  else {
    nL <- min(t[1, 2], t[2, 1])
    nP <- max(t[1, 2], t[2, 1])
    p_value <- 0
    for (i in 0:nL) {
      p_value <- p_value + dbinom(i, nL+nP, 0.5)
    }
    for (i in nP:nP+nL) {
      p_value <- p_value + dbinom(i, nL+nP, 0.5)
    }
    print(paste("p-value: ", p_value))
  } 
}

# zad 2

t <- as.table(matrix(c(1, 2, 5, 4), 
                     nrow = 2, 
                     byrow = FALSE,
                     dimnames = list("Reakcja na lek A" = c("Negatywna", "Pozytywna"),
                                     "Reakcja na lek B" = c("Negatywna", "Pozytywna"))))

# mcnemar, przyjmuje H0
mcnemar.test(t)
# warunkowy, odrzuca H0
symmetry_test(t)

# zad 3

z_test <- function(t) {
  n <- sum(t)
  n12 <- t[1, 2]
  n21 <- t[2, 1]
  D <- (n12 - n21) / n
  
  p <- prop.table(t)
  p1_plus <- rowSums(p)[1]
  p_plus1 <- colSums(p)[1]
  
  s <- (p1_plus * (1 - p1_plus) + p_plus1 * (1 - p_plus1) - 2 * (p[1, 1] * p[2, 2] + p[1, 2] * p[2, 1])) / n
  
  z <- D / sqrt(s)
  
  p_value <- 2 * (1 - pnorm(abs(z)))
  # print(paste("p-value: ", p_value))
  p_value |> as.numeric()
} 

z0_test <- function(t) {
  z0 <- (t[1, 2] - t[2, 1]) / sqrt(t[1, 2] + t[2, 1])
  p_value <- 2 * (1 - pnorm(abs(z0)))
  # print(paste("p-value: ", p_value))
  p_value
}

# moc testów
p1 <- 0.5
n <- 1000
x <- rbinom(n, 1, p1)
X <- c(x, 1-x)
for (p2 in seq(0, 1, 0.01)) {
  y <- rbinom(n, 1, p2)
  Y <- c(y, 1-y)
  t <- table(X, Y)
  print(paste("p2: ", p2))
  print(t)
  z_test(t)
}

power <- function(n, MC = 1000) {
  
  p1 <- 0.5
  x <- sum(rbinom(n, 1, p1))
  X <- c(x, n-x)
  
  plot_data <- data.frame()
  for (p2 in seq(0, 1, 0.01)) {
    MC_z <- 0
    MC_z0 <- 0
    for (i in 1:MC) {
      y <- sum(rbinom(n, 1, p2))
      Y <- c(y, n-y)
      
      t <- rbind(X,Y)
      MC_z <- MC_z + (z_test(t) < 0.05)
      MC_z0 <- MC_z0 + (z0_test(t) < 0.05)
    }
    MC_z <- MC_z / MC
    MC_z0 <- MC_z0 / MC
    
    plot_data <- rbind(plot_data, cbind(data.frame(p2=p2), data.frame(method = c('Z_test', 'Z0_test'),
               pow = c(MC_z, MC_z0))))
  }
  
  split_dfs <- split(plot_data, plot_data$method)
  z_power <- split_dfs[["Z_test"]][, "pow"]
  z0_power <- split_dfs[["Z0_test"]][, "pow"]
  ggplot(plot_data, aes(x=p2, y=pow, color=method, linetype=method)) +
    geom_line() +
    geom_hline(yintercept = 0.05, linetype="dashed") +
    scale_color_brewer(palette = "Set1", name = "Metoda") +
    scale_linetype_discrete(name = "Metoda") +
    labs(title = "Moc testu dla różnych wartości p2", 
         x = "p2", 
         y = "Moc testu") +
    theme_minimal()
}

power(40, 1000)
power(400, 1000) # z test częściej będzie popełniał błąd I rodzaju, za to rzadziej błąd II rodzaju, z0 na odwrot


# zad 4
ankieta <- read.csv('/Users/tomasz/Politechnika/semestr_6/ankiety/ankieta.csv', header = TRUE, sep = ";", fileEncoding = "Latin2")
ankieta['CZY_ZADOW'] <- ifelse(ankieta$PYT_2 %in% c(1, 2), 'zadowolony', 'niezadowolony')
ankieta['CZY_ZADOW_2'] <- ifelse(ankieta$PYT_3 %in% c(1, 2), 'zadowolony', 'niezadowolony')
# table of CZY_ZADOW and CZY_ZADOW_2 counts for mcnemar test
t <- table(ankieta$CZY_ZADOW, ankieta$CZY_ZADOW_2)
mcnemar.test(t)

# zad 5

t <- as.table(matrix(c(10, 2, 1, 1, 0, 0, 15, 1, 1, 0, 1, 1, 32, 6, 0, 0, 0, 1, 96, 3, 1, 1, 0, 1 ,26), 
                     nrow = 5, 
                     byrow = TRUE,
                     dimnames = list("Pytanie 1" = -2:2,
                                     "Pytanie 2" = -2:2)))  
mcnemar.test(t)
IW_test <- function(t) {
  G2 <- 0
  n <- sum(t)
  for (i in 1:nrow(t)) {
    for (j in 1:ncol(t)) {
      if (t[i, j] > 0) {
        G2 <- G2 + t[i, j] * log(t[i, j] / (t[i, j] + t[j, i]) * 2)
      }
    }
  }
  p_value <- 1 - pchisq(2 * G2, df = nrow(t) * (ncol(t) - 1) / 2)
  print(paste("p-value: ", p_value))
}
IW_test(t). # przyjmuje H0, czyli jest symetria

# zad 6
t3 <- as.table(matrix(c(117, 104, 177, 44), 
                     nrow = 2, 
                     byrow = TRUE,
                     dimnames = list("Metoda" = c("Leczenie A", "Leczenie B"),
                                     "Wynik leczenia" = c("Poprawa", "Brak"))))
t4 <- as.table(matrix(c(17, 101, 2, 36), 
                      nrow = 2, 
                      byrow = TRUE,
                      dimnames = list("Metoda" = c("Leczenie A", "Leczenie B"),
                                      "Reakcja" = c("Poprawa", "Brak"))))
t5 <- as.table(matrix(c(100, 3, 175, 8), 
                      nrow = 2, 
                      byrow = TRUE,
                      dimnames = list("Metoda" = c("Leczenie A", "Leczenie B"),
                                      "Reakcja" = c("Poprawa", "Brak"))))

# P(poprawa|leczenie A) = 117/(117+104) = 0.5294118
# P(poprawa|leczenie B) = 177/(177+44) = 0.8015873
# P(poprawa|leczenie A, choroby współ) = 17/(17+101) = 0.1445783
# P(poprawa|leczenie B, choroby współ) = 2/(2+36) = 0.05263158
# P(poprawa|leczenie A, brak choroby współ) = 100/(100+3) = 0.9701493
# P(poprawa|leczenie B, brak choroby współ) = 175/(175+8) = 0.9565217

chisq.test(t3)  # odrzuca H0, czyli jest zależność
chisq.test(t4)  # przyjmuje H0, czyli jest niezależność
chisq.test(t5)  # przyjmuje H0, czyli jest niezależność

# zad 8
library(dplyr)

# Model [123]

tab1 <- ankieta |> as_tibble()  %>%
  group_by(CZY_KIER, PYT_2, STAŻ) %>%
  summarise(count = n(), .groups = 'drop')

model <- glm(count ~ CZY_KIER * PYT_2 * STAŻ, 
    data = tab1, 
    family = poisson)

tab1$fitted <- fitted(model)

# proporcje odpowiedzi na PYT_2 wśród osób na stanowisku kierowniczym (faktyczne + dopasowane przez model)
subset(tab1, CZY_KIER == 'Tak') |> group_by(PYT_2) %>% 
  summarise(p_dane = sum(count) / sum(subset(tab1, CZY_KIER == 'Tak')$count), 
            p_model = sum(fitted) / sum(subset(tab1, CZY_KIER == 'Tak')$fitted), .groups = 'drop')

# proporcje osób pracujących na stanowisku kierownicznym wśród osób pracujących krócej niż rok (faktyczne + dopasowane przez model)
subset(tab1, STAŻ == 1) |> group_by(CZY_KIER) %>% 
  summarise(p_dane = sum(count) / sum(subset(tab1, STAŻ == 1)$count), 
            p_model = sum(fitted) / sum(subset(tab1, STAŻ == 1)$fitted), .groups = 'drop')

# proporcje osób pracujących na stanowisku kierownicznym wśród osób pracujących dłużej niż 3 lata (faktyczne + dopasowane przez model)
subset(tab1, STAŻ == 3) |> group_by(CZY_KIER) %>% 
  summarise(p_dane = sum(count) / sum(subset(tab1, STAŻ == 3)$count), 
            p_model = sum(fitted) / sum(subset(tab1, STAŻ == 3)$fitted), .groups = 'drop')

# Model [12 23]

model <- glm(count ~ CZY_KIER*PYT_2 + PYT_2*STAŻ, 
    data = tab1, 
    family = poisson)

tab1$fitted2 <- fitted(model)

# proporcje odpowiedzi na PYT_2 wśród osób na stanowisku kierowniczym (faktyczne + dopasowane przez model)
subset(tab1, CZY_KIER == 'Tak') |> group_by(PYT_2) %>% 
  summarise(p_dane = sum(count) / sum(subset(tab1, CZY_KIER == 'Tak')$count), 
            p_model = sum(fitted2) / sum(subset(tab1, CZY_KIER == 'Tak')$fitted2), .groups = 'drop')

# proporcje osób pracujących na stanowisku kierownicznym wśród osób pracujących krócej niż rok (faktyczne + dopasowane przez model)
subset(tab1, STAŻ == 1) |> group_by(CZY_KIER) %>% 
  summarise(p_dane = sum(count) / sum(subset(tab1, STAŻ == 1)$count), 
            p_model = sum(fitted2) / sum(subset(tab1, STAŻ == 1)$fitted2), .groups = 'drop')

# proporcje osób pracujących na stanowisku kierownicznym wśród osób pracujących dłużej niż 3 lata (faktyczne + dopasowane przez model)
subset(tab1, STAŻ == 3) |> group_by(CZY_KIER) %>% 
  summarise(p_dane = sum(count) / sum(subset(tab1, STAŻ == 3)$count), 
            p_model = sum(fitted2) / sum(subset(tab1, STAŻ == 3)$fitted2), .groups = 'drop')

# zad 9

# a) zmienne CZY_KIER, PYT_2, STAŻ są wzajemnie niezależne
# H0: [1 2 3]
M0 <- glm(count ~ CZY_KIER + PYT_2 + STAŻ, 
    data = tab1, 
    family = poisson)

# H1: [12 23]
M1 <- glm(count ~ CZY_KIER * PYT_2 + PYT_2 * STAŻ, 
    data = tab1, 
    family = poisson)

an <- anova(M0, M1)
deviance <- an$Deviance[2]
df <- an$Df[2]
p_value <- 1 - pchisq(deviance, df)
print(paste("p-value: ", p_value))  # nie odrzucamy H0, model bez zależności jest okej, nie ma zależności pomiędzy zmiennymi?
# H1: [13 23]
M1 <- glm(count ~ CZY_KIER * STAŻ + PYT_2 * STAŻ, 
    data = tab1, 
    family = poisson)

an <- anova(M0, M1)
deviance <- an$Deviance[2]
df <- an$Df[2]
p_value <- 1 - pchisq(deviance, df)
print(paste("p-value: ", p_value))  # też nie odrzucamy H0, model bez zależności jest okej, nie ma zależności pomiędzy zmiennymi?
# H1: [12 13]
M1 <- glm(count ~ CZY_KIER * PYT_2 + CZY_KIER * STAŻ, 
    data = tab1, 
    family = poisson)

an <- anova(M0, M1)
deviance <- an$Deviance[2]
df <- an$Df[2]
p_value <- 1 - pchisq(deviance, df)
print(paste("p-value: ", p_value))  # też nie odrzucamy H0, model bez zależności jest okej, nie ma zależności pomiędzy zmiennymi?

# b) zmienna PYT_2 niezależna od CZY_KIER i STAŻ
# H0: [13 2]
M0 <- glm(count ~ CZY_KIER * STAŻ + PYT_2, 
    data = tab1, 
    family = poisson)

# H1: [12 13]
M1 <- glm(count ~ CZY_KIER * PYT_2 + CZY_KIER * STAŻ, 
    data = tab1, 
    family = poisson)

an <- anova(M0, M1)
deviance <- an$Deviance[2]
df <- an$Df[2]
p_value <- 1 - pchisq(deviance, df)
print(paste("p-value: ", p_value)) # nie odrzucamy H0, model bez zależności jest okej, nie ma zależności pomiędzy zmiennymi?

# H1: [12 13 23]
M1 <- glm(count ~ CZY_KIER * PYT_2 + CZY_KIER * STAŻ + PYT_2 * STAŻ,
    data = tab1, 
    family = poisson)

an <- anova(M0, M1)
deviance <- an$Deviance[2]
df <- an$Df[2]
p_value <- 1 - pchisq(deviance, df)
print(paste("p-value: ", p_value))  # nie odrzucamy H0, model bez zależności jest okej, nie ma zależności pomiędzy zmiennymi?

# H1: [123]
M1 <- glm(count ~ CZY_KIER * PYT_2 * STAŻ, 
    data = tab1, 
    family = poisson)

an <- anova(M0, M1)
deviance <- an$Deviance[2]
df <- an$Df[2]
p_value <- 1 - pchisq(deviance, df)
print(paste("p-value: ", p_value)) # nadal nie odrzucamy H0, model bez zależności jest okej, nie ma zależności pomiędzy zmiennymi?

# c) zmienna PYT_2 jest warunkowo niezależna od CZY_KIER przy ustalonej wartości STAŻ
# H0: [13 23]
M0 <- glm(count ~ CZY_KIER * STAŻ + PYT_2 * STAŻ, 
    data = tab1, 
    family = poisson)

# H1: [12 13 23]
M1 <- glm(count ~ CZY_KIER * PYT_2 + CZY_KIER * STAŻ + PYT_2 * STAŻ, 
    data = tab1, 
    family = poisson)

an <- anova(M0, M1)
deviance <- an$Deviance[2]
df <- an$Df[2]
p_value <- 1 - pchisq(deviance, df)
print(paste("p-value: ", p_value)) # nie odrzucamy H0, model bez zależności jest okej, nie ma zależności pomiędzy zmiennymi?

# H1: [123]
M1 <- glm(count ~ CZY_KIER * PYT_2 * STAŻ, 
    data = tab1, 
    family = poisson)

an <- anova(M0, M1)
deviance <- an$Deviance[2]
df <- an$Df[2]
p_value <- 1 - pchisq(deviance, df)
print(paste("p-value: ", p_value)) # nadal nie odrzucamy H0, model bez zależności jest okej, nie ma zależności pomiędzy zmiennymi?

