library(tidyverse)
library(patchwork)
library(ggmosaic)
library(ggplot2)
library(DescTools)

ankieta <- read.csv('/Users/tomasz/Politechnika/semestr_6/ankiety/ankieta.csv', header = TRUE, sep = ";", check.names = F)
ankieta |> View()

colnames(ankieta)[1] <- "DZIAŁ"
colnames(ankieta)[2] <- "STAŻ"
colnames(ankieta)[7] <- "PŁEĆ"

missing_values <- sum(is.na(ankieta))
missing_values

sapply(ankieta, class)

ankieta['WIEK_KAT'] <- ifelse(ankieta$WIEK < 36, 'młody', ifelse(ankieta$WIEK < 46, 'średni', ifelse(ankieta$WIEK < 56, 'starszy', 'emerytura')))

ankieta |> group_by(DZIAŁ) |> summarise(n = n())
ankieta |> group_by(STAŻ) |> summarise(n = n())
ankieta |> group_by(CZY_KIER) |> summarise(n = n())
ankieta |> group_by(PŁEĆ) |> summarise(n = n())
ankieta |> group_by(WIEK_KAT) |> summarise(n = n())

p1 <- ggplot(ankieta, aes(x='', fill=factor(PYT_1))) + geom_bar(color='white') +
  coord_polar('y', start=pi/2) + theme_void() + labs(title='Pytanie 1', fill='Odpowiedzi') 
p2 <- ggplot(ankieta, aes(x='', fill=factor(PYT_2))) + geom_bar(color='white') +
  coord_polar('y', start=pi/2) + theme_void() + labs(title='Pytanie 2', fill='Odpowiedzi')
p3 <- ggplot(ankieta, aes(x='', fill=factor(PYT_3))) + geom_bar(color='white') + 
  coord_polar('y', start=pi/2) + theme_void() + labs(title='Pytanie 3', fill='Odpowiedzi')

p1 + p2 + p3 + plot_layout(guides = 'collect', axis_titles = 'collect')

ankieta |> group_by(DZIAŁ) |> summarise(PYT_1 = mean(PYT_1, na.rm = TRUE))
ankieta |> group_by(STAŻ) |> summarise(PYT_1 = mean(PYT_1, na.rm = TRUE))
ankieta |> group_by(CZY_KIER) |> summarise(PYT_1 = mean(PYT_1, na.rm = TRUE))
ankieta |> group_by(PŁEĆ) |> summarise(PYT_1 = mean(PYT_1, na.rm = TRUE))
ankieta |> group_by(WIEK_KAT) |> summarise(PYT_1 = mean(PYT_1, na.rm = TRUE))

ankieta |> group_by(PYT_2, PYT_3) |> summarise(n = n(), .groups='keep') |> pivot_wider(names_from = PYT_3, values_from = n, values_fill = 0)

ankieta['CZY_ZADOW'] <- ifelse(ankieta$PYT_2 %in% c(1, 2), 'zadowolony', 'niezadowolony')

ggplot() + geom_mosaic(data = ankieta, aes(weight = 1, x = product(DZIAŁ, CZY_ZADOW), fill = CZY_ZADOW)) + labs(title='Zadowolenie w zależności od działu') + theme(legend.position = 'bottom')
ggplot() + geom_mosaic(data = ankieta, aes(weight = 1, x = product(STAŻ, CZY_ZADOW), fill = CZY_ZADOW)) + labs(title='Zadowolenie w zależności od działu') + theme(legend.position = 'bottom')
ggplot() + geom_mosaic(data = ankieta, aes(weight = 1, x = product(CZY_KIER, CZY_ZADOW), fill = CZY_ZADOW)) + labs(title='Zadowolenie w zależności od działu') + theme(legend.position = 'bottom')
ggplot() + geom_mosaic(data = ankieta, aes(weight = 1, x = product(PŁEĆ, CZY_ZADOW), fill = CZY_ZADOW)) + labs(title='Zadowolenie w zależności od działu') + theme(legend.position = 'bottom') 
ggplot() + geom_mosaic(data = ankieta, aes(weight = 1, x = product(WIEK_KAT, CZY_ZADOW), fill = CZY_ZADOW)) + labs(title='Zadowolenie w zależności od działu') + theme(legend.position = 'bottom')

# install.packages('likert')
library(likert)
summary(ankieta)
ankieta |> group_by(PYT_1) |> summarise('%' = n() / nrow(ankieta))
ankieta |> group_by(PYT_1) |> summarise(n=n()) |> ggplot(aes(x=PYT_1, y=n, fill=PYT_1)) + geom_bar(stat='identity') + labs(title='Pytanie 1', x='Odpowiedź', y='Liczba odpowiedzi') + theme(legend.position = 'none')
ankieta |> group_by(PYT_1, CZY_KIER) |> summarise(n = n(), .groups='keep') |> ggplot(aes(x = PYT_1, y = n, fill = CZY_KIER)) + geom_bar(stat = 'identity', position = 'dodge') + labs(title='Pytanie 1 w zależności od stanowiska', x='Odpowiedź', y='Liczba odpowiedzi') + theme(legend.position = 'bottom')

ankieta[sample(nrow(ankieta), nrow(ankieta)*0.1, replace=FALSE), ]
ankieta[sample(nrow(ankieta), nrow(ankieta)*0.1, replace=TRUE), ]

bin_rvs <- function(n, p) {
  sum(sample(c(0, 1), n, replace=TRUE, prob=c(1-p, p)))
}

bin_rvs(10, 0.5)  

p <- 0.5
n <- 10

xs <- tibble(Value=replicate(100*n, bin_rvs(n, 0.5)))
ggplot(xs, aes(x=Value)) +
  geom_histogram(aes(y=after_stat(density)), bins=n+1, color="black") +
  stat_function(fun=dbinom, color='red', xlim=c(min(xs$Value), max(xs$Value)),
                args=list(size=n, prob=p)) +
  ggtitle("Przykładowy histogram 1000 obserwacji z rozkładu Bin(10, 0.5)") +
  ylab("Gęstość prawdopodobieńśtwa") +
  xlab("Wartość wylosowana z rozkładu normalnego")

ggplot(xs, aes(x=Value)) +
  stat_ecdf(geom="step", color="blue") +
  stat_function(fun=pbinom, color='red', args=list(size=n, prob=p)) +
  ggtitle("Empirical CDF vs Theoretical CDF") +
  ylab("P(X <= x)") +
  xlab("x") 


install.packages("qqplotr")
library(qqplotr)

ggplot(xs, aes(sample=Value)) +
  stat_qq_point(distribution="binom", dparams=list(size=n, prob=p)) +
  stat_qq_line(distribution="binom", dparams=list(size=n, prob=p)) +
  stat_qq_band(distribution="binom", dparams=list(size=n, prob=p), bandType="ell") +
  ylab("Kwantyle z próby") +
  xlab("Kwantyle teoretyczne")


multinomial_rv <- function(n, p) {
  X <- rep(0, length(p))
  for (i in 1:n) {
    temp <- sample(1:length(p), 1, prob=p)
    X[temp] <- X[temp] + 1
  }
  X
}

multinomial_rvs <- function(size, n, p) {
  matrix(replicate(size, multinomial_rv(n, p)), nrow=length(p))
}
 
p <- c(0.1, 0.2, 0.3, 0.4)
n <- 10
size <- 100

x <- multinomial_rvs(size, n, p)
rowSums(x) / size / n  # empiryczne prawdopodbieństwa
p  # teoretyczne prawdopodobieństwa


logit_CI <- function(x, n, alpha=0.05) {
  p <- x / n
  se <- sqrt(1 / (p * (1 - p) * n))
  z <- qnorm(alpha / 2)
  L <- Logit(p) - z * se
  U <- Logit(p) + z * se
  data.frame(est=p, lwr.ci=exp(U) / (1 + exp(U)), upr.ci=exp(L) / (1 + exp(L)))
}

logit_CI(3,10)
BinomCI(3, 10, method="logit")

logit_CI(30,100)
BinomCI(30, 100, method="logit")

# zad 6
CP_CI <- function(conf.level, x, n=NA) {
  if (is.na(n)) {
    n <- x[2]
    x <- x[1]
  }
  alpha <- 1 - conf.level
  L <- qbeta(alpha / 2, x, n - x + 1)
  U <- qbeta(1 - alpha / 2, x + 1, n - x)
  if (x == 0) {
    L <- 0
  }
  if (x == n) {
    U <- 1
  }
  data.frame(est=x/n, lwr.ci=L, upr.ci=U)
}

# zad 7
ankieta['CZY_ZADOW_2'] <- ifelse(ankieta$PYT_3 %in% c(1, 2), 'zadowolony', 'niezadowolony')
x_zadw <- ankieta |> filter(CZY_ZADOW == 'zadowolony') |> nrow()
x_zadw2 <- ankieta |> filter(CZY_ZADOW_2 == 'zadowolony') |> nrow()
n <- nrow(ankieta)
CP_CI(0.95, c(x_zadw, n))
BinomCI(x_zadw, n, method="clopper-pearson", conf.level=0.95)
CP_CI(0.95, x_zadw2, n)
BinomCI(x_zadw2, n, method="clopper-pearson", conf.level=0.95)

# zad 9
sprawdzaj_CI_exact <- function(n, p, method, alpha=0.05){
  X <- dbinom(0:n, n, p) # prawdopodobieństwa
  wyniki <- BinomCI(0:n, n, method=method, conf.level=1-alpha)
  dlugosc <- sum((wyniki[,'upr.ci'] - wyniki[,'lwr.ci']) * X)
  Y <- sum(1*((wyniki[, 2] <= p) & (p <= wyniki[,3])) * X) # p w przedziale średnio
  list(valid=Y>1-alpha, length=dlugosc, coverage=Y)
}

best_of_methods <- function(n, p, methods, ...){
  ramka <- data.frame(valid=c(), length=c(), coverage=c())
  for (method in methods) {
    ramka <- tryCatch({
      rbind(ramka, as.data.frame(sprawdzaj_CI_exact(n, p, method, ...)))
    }, error=function(...){
      rbind(ramka, data.frame(valid=FALSE, length=NA, coverage=NA))
    })
  }
  ramka$method <- methods
  valid_methods <- ramka[ramka$valid, ]
  valid_methods[which.min(valid_methods$length), ]
  which(valid_methods[which.min(valid_methods$length),]$method == methods)
}

methods <- c("clopper-pearson", "wald", "jeffreys")

ns <- c(30, 100, 1000)
ps <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
wyniki <- matrix(NA, nrow=length(ns), ncol=length(ps))
for (i in 1:length(ns)){
  for (j in 1:length(ps)){
    wyniki[i, j] <- best_of_methods(ns[i], ps[j], methods)
  }
}
rownames(wyniki) <- ns
colnames(wyniki) <- ps
data <- expand.grid(Var1=rownames(wyniki), Var2=colnames(wyniki))
data$value <- as.vector(wyniki)
data$method <- methods[data$value]
colors = hcl.colors(length(methods), 'ylorRd', rev=TRUE)
ggplot(data, aes(x=Var1, y=Var2, fill=factor(value))) +
  geom_tile() +
  geom_tile(color = "#00000022") +
  scale_fill_manual(values = colors, labels=methods)+
  coord_equal() +
  theme_bw() +
  labs(title="Best method for different n and p",
       x="n", y="p", fill="method") 

