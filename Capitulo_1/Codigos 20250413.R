library(readr)
library(dplyr)
library(modeest)
library(sjstats)
library(cleaner)
library(DescTools)
library(moments)
library(reflimR)
library(janitor) # función tabyl
library(summarytools)
library(ggplot2)
library(waffle)
library(treemapify)
library(skimr)
library(explore)

datos <- read_csv('Salud.csv')
datos |>
  mutate(Sedentario = ifelse(Minutos_ejercicio<30,"Sí","No")) -> datos

datos |>
  pull(Sedentario) |> 
  tabyl() |> 
  adorn_totals("row") |> 
  mutate(percent = percent*100)


datos |>
  filter(Edad>=60) |>
  summarytools::freq(Edad,
                     report.nas = F)


datos |>
  #filter(Edad>=60) |>
  summarytools::freq(Edad,
                     report.nas = F)


datos |>
  pull(Edad) |>
  DescTools::Freq()

?Freq


datos |>
  pull(Edad) |>
  DescTools::Freq(breaks = seq(20,70,10))

datos |>
  pull(Edad) |>
  DescTools::Freq(breaks = c(30,35,45,60))

datos |>
  count(Sedentario)


datos |>
  count(Sedentario) |>
  ggplot(aes(x = Sedentario, y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = n, y = n / 2), color = "white") +
  labs(title = "Distribución de pacientes según su actividad física",
       x = "Actividad física",
       y = "Cantidad de pacientes") +
  theme_replace()

datos |>
  ggplot(aes(x = Sedentario)) +
  geom_bar(fill = "darkblue") +
  labs(title = "Distribución de pacientes según su actividad física",
       x = "Actividad física",
       y = "Cantidad de pacientes") +
  theme_minimal()

datos |>
  count(Sedentario)
datos |>
  count(Sedentario) |>
  mutate(porcentaje = round(n / sum(n) * 100, 1),
         etiqueta = paste0(porcentaje, "%\n n=",n)) |>
  ggplot(aes(x = "", y = n, fill = Sedentario)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = etiqueta), position = position_stack(vjust = 0.5)) +
  labs(title = "Distribución de pacientes según su actividad física") +
  theme_void()


str(datos)

datos |>
  count(Sedentario) |>
  mutate(n = round(n / sum(n) * 100)) |>
  waffle(rows = 10,size = 1,
         title = "Distribución de pacientes")

         
datos |>
  count(Sedentario) |>
  mutate(n = round(n / sum(n) * 100)) |>
  waffle(rows = 10, title = "Distribución de pacientes
         según nivel de actividad física")

rnorm(2000) -> muestra_simulada
muestra_simulada |> hist()
muestra_simulada |> skewness()


datos |> pull(Edad) |> median()

datos |> pull(Edad) |> shapiro.test()


# mediana IMC = 30
# Simetría
# 50% encima de 30 puntos

datos |> skim()
datos |> group_by(Sedentario) |> skim()

datos |> explore::explore()


datos |> 
  slice_sample(n = 17)
set.seed(1111)
datos |> 
  sample_n(10)



