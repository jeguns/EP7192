
# Carga de paquetes -------------------------------------------------------

library(readr)
library(dplyr)
library(modeest)
library(sjstats)
library(cleaner)
library(DescTools)
library(moments)
library(reflimR)
library(janitor)
library(summarytools)
library(ggplot2)
library(waffle)
library(treemapify)
library(skimr)
library(explore)


# Lectura de datos --------------------------------------------------------

datos <- read_csv('Salud.csv')


# Medidas de tendencia central --------------------------------------------

## Media -------------------------------------------------------------------

datos |> summarize(Media = mean(Edad))
summarize(datos, Media = mean(Edad))

datos |> 
  filter(Edad > 50) |> 
  summarize(Media = mean(Presion_sistolica))


## Mediana -----------------------------------------------------------------

datos |>
  mutate(Sedentario = ifelse(Minutos_ejercicio<30,"Sí","No")) |> 
  group_by(Sedentario) |>
  summarize(Medianas = median(Presion_sistolica))

## Moda --------------------------------------------------------------------

datos |>
  summarize(Moda = modeest::mfv(Presion_sistolica))

datos |>
  reframe(Moda = mfv(Edad))


# Medidas de posición -----------------------------------------------------

datos |>
  mutate(Sedentario = ifelse(Minutos_ejercicio<30,"Sí","No")) -> datos

datos |>
  filter(Sedentario == "No") |>
  reframe(Percentiles = quantile(Minutos_ejercicio, c(0.12,0.74)))


# Medidas de dispersión ---------------------------------------------------

datos |> summarize(cv_imc = cv(IMC)*100,
                   cv_pres = cv(Presion_sistolica)*100)


# Medidas de asimetría ----------------------------------------------------

library(moments)

datos |> 
  summarise(asimetria = skewness(Edad))

datos |> 
  summarise(asim_edad = skewness(Edad),
            asim_ejer = skewness(Minutos_ejercicio),
            asim_imc  = skewness(IMC),
            asim_pres = skewness(Presion_sistolica))

datos |>
  summarise(across(where(is.numeric), ~ skewness(.x)))

datos |>
  summarise(across(where(is.numeric), ~ bowley(.x)))


# Tablas de frecuencia ----------------------------------------------------

datos |> pull(Sedentario)
datos |> select(Sedentario)

datos |> pull(Edad)
datos |> select(Edad)

datos |>
  pull(Sedentario) |>
  tabyl()|> 
  adorn_totals("row") |> 
  rename(Sedentario = 1,
         Cantidad    = 2,
         Proporcion    = 3)

datos |>
  select(Sedentario) |>
  tabyl() 

# ejemplo
numautos = c(0,1,2,0,0,2,1,2,3,2,0,1,1,0,1,1)
numautos |> 
  tabyl() |>
  adorn_totals("row")

datos |>
  filter(Edad>=60) |>
  summarytools::freq(Edad)

datos |>
  summarytools::freq(Edad)

datos |>
  pull(Edad) |>
  DescTools::Freq()

datos |>
  pull(Edad) |>
  DescTools::Freq(breaks = c(20,30,40,50,60,70)) |> 
  data.frame() |> 
  rename(Edad  = 1,
         Cantidad = 2,
         Porcentaje = 3,
         Cant_Acum = 4,
         Porc_Acum = 5)


# Gráficas ----------------------------------------------------------------

datos |> 
  pull(Sedentario) |> 
  table() |> 
  barplot()

datos |>
  count(Sedentario) |> 
  ggplot(aes(x = Sedentario, y = n)) + 
  geom_bar(stat = "identity", fill = "lightgreen") +
  geom_text(aes(label = n, y = n / 2), color = "black")+
  labs(title = "Distribución de pacientes según su sedentarismo",
       x = "Sedentarismo",
       y = "Cantidad de pacientes") +
  theme_minimal()



datos |>
  count(Sedentario) |> 
  ggplot(aes(x = Sedentario, y = n)) + 
  geom_bar(stat = "identity", fill = "lightgreen") +
  geom_text(aes(label = n, y = n / 2), color = "black")+
  labs(title = "Distribución de pacientes según su sedentarismo",
       x = "Sedentarismo",
       y = "Cantidad de pacientes") +
  coord_flip() +
  theme_minimal()

datos |>
  ggplot(aes(x = Sedentario)) +
  geom_bar(fill = "darkblue") +
  labs(title = "Distribución de pacientes según su sedentarismo",
       x = "Sedentarismo",
       y = "Cantidad de pacientes") +
  theme_minimal()



datos |> 
  pull(Sedentario) |> 
  table() |> 
  pie()


datos |>
  count(Sedentario) |>
  mutate(porcentaje = round(n / sum(n) * 100, 1),
         etiqueta = paste0(Sedentario, ": ", porcentaje, "%"))


datos |>
  count(Sedentario) |>
  mutate(porcentaje = round(n / sum(n) * 100, 1),
         etiqueta = paste0(Sedentario, ": ", porcentaje, "%")) |>
  ggplot(aes(x = "", y = n, fill = Sedentario)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = etiqueta), position = position_stack(vjust = 0.5)) +
  labs(title = "Distribución de pacientes según su actividad física") +
  theme_void()

datos |>
  count(Sedentario) |>
  mutate(n = round(n / sum(n) * 100)) |> 
  waffle(rows = 10, title = "Distribución de pacientes",
         colors = c("skyblue1","gold"))+
  geom_text(aes(label = "No, 88%", x = 2, y = 1))+
  geom_text(aes(label = "Sí, 12%", x = 9.25, y = 10))

datos |>
  count(Sedentario) |>
  ggplot(aes(area = n, fill = Sedentario, label = Sedentario)) +
  geom_treemap()+
  geom_treemap_text(colour = "white", place = "centre", grow = TRUE)+
  labs(title = "Distribución de pacientes") 

datos |>
  filter(Edad > 60) |>
  count(Edad) |>
  ggplot(aes(x = factor(Edad), y = n)) +
  geom_bar(stat = "identity", fill = "darkgreen", width = 0.05)

datos |>
  filter(Edad > 60) |>
  count(Edad) |>
  ggplot(aes(x = factor(Edad), y = n)) +
  geom_bar(stat = "identity", fill = "darkgreen", width = 0.05) +
  geom_text(aes(label = n), vjust = -0.5) +
  scale_y_continuous(limits = c(0,4.5)) +
  labs(title = "Distribución de las edades de los pacientes adultos mayores",
       x = "Valor",
       y = "Frecuencia") +
  theme_minimal()
