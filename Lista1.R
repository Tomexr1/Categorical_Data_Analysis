library(tidyverse)
library(patchwork)
library(ggmosaic)
library(ggplot2)

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





  
  