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
           