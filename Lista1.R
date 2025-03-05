library(tidyverse)
library(patchwork)
library(ggmosaic)
library(ggplot2)

ankieta <- read.csv('/Users/tomasz/Politechnika/semestr_6/ankiety/ankieta.csv', header = TRUE, sep = ";", check.names = F)
ankieta |> View()
# rename columns 1st 2nd and 7th column
colnames(ankieta)[1] <- "DZIAŁ"
colnames(ankieta)[2] <- "STAŻ"
colnames(ankieta)[7] <- "PŁEĆ"

# find missing values
missing_values <- sum(is.na(ankieta))
missing_values

# get type of columns
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
