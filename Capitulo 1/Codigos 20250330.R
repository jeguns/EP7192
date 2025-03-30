library(readr)
library(dplyr)
library(modeest)
datos <- read_csv('Salud.csv')
datos |> head(5)
head(datos,5)
datos |> tail(3)
mean(datos$Edad)
datos |> summarize(Media = mean(Edad))
datos |> filter(Edad > 50)
datos |> filter(Edad > 50) |> 
  summarize(Media = mean(Presion_sistolica))
datos |> summarize(Mediana = median(IMC))
datos |>
  mutate(Sedentario = ifelse(Minutos_ejercicio<30,"Sí","No")) -> datos
datos |>
  group_by(Sedentario) |>
  summarize(Medianas = median(Presion_sistolica))

library(modeest)
datos |>
  summarize(Moda = mfv(Presion_sistolica))
datos |>
  reframe(Moda = mfv(Edad))
datos |>
  filter(Sedentario == "Sí") |>
  reframe(Moda = mfv(Minutos_ejercicio))

datos |> summarize(P15 = quantile(Minutos_ejercicio, 0.15, type = 4))
datos |> summarize(P15 = quantile(Minutos_ejercicio, 0.15, type = 5))
datos |> summarize(P15 = quantile(Minutos_ejercicio, 0.15))

datos |>
  summarize(P41 = quantile(Edad, 0.41))

datos |>
  summarize(P50 = quantile(Edad, 0.50))

datos |>
  summarize(P21 = quantile(Edad, 0.021))

datos |>
  filter(Sedentario == "No") |>
  reframe(Percentiles = quantile(Minutos_ejercicio, c(0.12,0.74)))

datos |>
  filter(Sedentario == "No") |>
  reframe(Percentiles = quantile(Minutos_ejercicio, 
                                 seq(0.01, 0.99, 0.01))) |> 
  mutate(Percentil = 1:99) |> 
  print(n = 99)

datos |> reframe(Perc = quantile(Minutos_ejercicio,
                                 c(0.25,0.50,0.75))) -> cuartiles
cuartiles[1,1] |> as.numeric() -> cuartil1
cuartiles[2,1] |> as.numeric() -> cuartil2
datos |> 
  filter(Minutos_ejercicio > cuartil1 & Minutos_ejercicio < cuartil2) |> 
  summarize(Promedio = mean(Minutos_ejercicio))
