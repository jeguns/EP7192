g1 = 12:18
g2 = c(10,16,7,16,18,19,19)
g3 = c(7,7,7,16,19,19,19)
g1 |> mean()
g2 |> mean()
g3 |> mean()
g1 |> sd()
c(15-2.16, 15+2.16)
g2 |> sd()
c(15-4.69, 15+4.69)
g3 |> sd()
c(13.4 - 6.1, 13.4 + 6.1)
g4 = c(7,7,7,16,19,19,59)
g4 |> sd()

estatura = c(170,158,160,165,176)
peso = c(72,55,68,60,70)
estatura |> sd()
peso |> sd()
estatura |> mean()
peso |> mean()
7.362065/165.8*100
7.211103/65*100

peso1 = c(12,15,18,12,15,16)
peso1 |> sd()
peso2 = c(1500,1250,1600,985,1540,1320)
peso2 |> sd()
peso1 |> mean()
peso2 |> mean()
2.33809/14.66667*100
229.661/1365.833*100


n = 400
1/n
n/((n-1)*(n-2))

library(readr)
library(dplyr)
library(modeest)
library(sjstats) # cv
library(cleaner)
library(DescTools) # Range
library(moments) # skewness
library(reflimR) # bowley
library(janitor)
library(summarytools)
library(ggplot2)
library(waffle)
library(treemapify)
library(skimr)
library(explore)

datos <- read_csv('Salud.csv')
datos |>
  mutate(Sedentario = ifelse(Minutos_ejercicio<30,"Sí","No")) -> datos


datos |> group_by(Sedentario) |> summarize(r = Range(IMC))
datos |> group_by(Sedentario) |> reframe(r = Range(IMC))



datos |> summarize(media = mean(Minutos_ejercicio),
                   s     = sd(Minutos_ejercicio))

datos$Minutos_ejercicio |> summary()

datos |> summarize(s_imc = sd(IMC),
                   s_pres = sd(Presion_sistolica),
                   m_imc = mean(IMC),
                   m_pres = mean(Presion_sistolica),
                   cv_imc = cv(IMC)*100,
                   cv_pres = cv(Presion_sistolica)*100)

datos |> mutate(Grupo_Edad = case_when(Edad < 30 ~ "Joven",
                                       Edad >= 30 & Edad < 60 ~ "Adulto",
                                       Edad >= 60 ~ "Adulto mayor"))


datos |> summarize(as_edad = skewness(Edad),
                   as_ejer = skewness(Minutos_ejercicio),
                   as_IMC  = skewness(IMC),
                   as_pres = skewness(Presion_sistolica))

datos |>
  summarise(across(where(is.numeric), ~ skewness(.x)))

datos |>
  summarise(across(where(is.numeric), ~ moments::skewness(.x)))

datos |>
  summarise(across(where(is.numeric), ~ bowley(.x)))
