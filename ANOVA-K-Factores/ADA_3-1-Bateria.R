#-----ANOVA PARA DOS FACTORES------

library(readxl)
library(dplyr)
library(ggplot2)
library(car)
library(agricolae)

ruta_archivo <- "ruta"  
datos <- read_excel(ruta_archivo, sheet = "Técnica_Dominio")

# Factores
datos$material <- factor(datos$Material)
datos$temperatura <- factor(datos$Temperatura)

# Análisis Descriptivo
descriptivo <- datos %>%
  group_by(Material, Temperatura) %>%
  summarise(
    n = n(),
    media = mean(Vida),
    error_estandar = sd(Vida) / sqrt(n),
    limite_inferior = qt(0.025, df = n - 1, lower.tail = TRUE) * (sd(Vida) / sqrt(n)) + mean(Vida),
    limite_superior = qt(0.975, df = n - 1, lower.tail = TRUE) * (sd(Vida) / sqrt(n)) + mean(Vida)
  )

print("Estadísticas descriptivas por Material y Temperatura")
print(as.data.frame(descriptivo))

descriptivo_material <- datos %>%
  group_by(Material) %>%
  summarise(
    n = n(),
    media = mean(Vida),
    error_estandar = sd(Vida) / sqrt(n),
    limite_inferior = qt(0.025, df = n - 1, lower.tail = TRUE) * (sd(Vida) / sqrt(n)) + mean(Vida),
    limite_superior = qt(0.975, df = n - 1, lower.tail = TRUE) * (sd(Vida) / sqrt(n)) + mean(Vida)
  )
print("Estadísticas descriptivas por Material")
print(as.data.frame(descriptivo_material))

descriptivo_temperatura <- datos %>%
  group_by(Temperatura) %>%
  summarise(
    n = n(),
    media = mean(Vida),
    error_estandar = sd(Vida) / sqrt(n),
    limite_inferior = qt(0.025, df = n - 1, lower.tail = TRUE) * (sd(Vida) / sqrt(n)) + mean(Vida),
    limite_superior = qt(0.975, df = n - 1, lower.tail = TRUE) * (sd(Vida) / sqrt(n)) + mean(Vida)
  )
print("Estadísticas descriptivas por Temperatura")
print(as.data.frame(descriptivo_temperatura))

# Gráfico de interacciones
grafico_interacciones <- ggplot(datos, aes(x = Temperatura, y = Vida, color = Material, group = Material)) +
  stat_summary(fun = mean, geom = "line", linewidth = 1.2) + # Línea de medias
  stat_summary(fun = mean, geom = "point", size = 3) +       # Puntos de medias
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) + # Barras de error
  labs(
    title = "Gráfico de Interacciones entre Material y Temperatura",
    x = "Temperatura",
    y = "Vida útil promedio (horas)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right"
  )

# Mostrar el gráfico
print(grafico_interacciones)

# Gráfico de medias para Material
grafico_material <- ggplot(descriptivo_material, aes(x = Material, y = media)) +
  geom_errorbar(aes(ymin = media - error_estandar, ymax = media + error_estandar), width = 0.2) +
  labs(title = "Gráfico de Medias por Material", x = "Material", y = "Vida útil promedio (horas)") +
  theme_minimal()

# Gráfico de medias para Temperatura
grafico_temperatura <- ggplot(descriptivo_temperatura, aes(x = Temperatura, y = media))  +
  geom_errorbar(aes(ymin = media - error_estandar, ymax = media + error_estandar), width = 0.2) +
  labs(title = "Gráfico de Medias por Temperatura", x = "Temperatura", y = "Vida útil promedio (horas)") +
  theme_minimal()

# Gráficos
print(grafico_material)
print(grafico_temperatura)


#Planteamos el modelo de datos

modelo <- lm(datos$Vida~(datos$material+datos$temperatura+(datos$material*datos$temperatura)))

# ------------- Realizamos ANOVA ------------- 

anova <- aov(modelo)
print(anova)
summary(anova)

# Prueba LSD para el material
pruebaLSD_material <- LSD.test(anova, c("datos$material"), p.adj = "none")
print(pruebaLSD_material)
plot(pruebaLSD_material)

# Prueba LSD para la temperatura
pruebaLSD_temperatura <- LSD.test(anova, c("datos$temperatura"), p.adj = "none")
print(pruebaLSD_temperatura)
plot(pruebaLSD_temperatura)

pruebaLSD <- LSD.test(anova, c("datos$material","datos$temperatura"), group=TRUE, console=TRUE)
plot(pruebaLSD)


#Normalidad 
shapiro.test(modelo$residuals)
qqPlot (anova, main= "Gráfico Cuantil - Cuantil", id=FALSE)

# ----- Graficamos valores predichos vs Residuales -----

predichos <- anova$fitted.values
residuales <- anova$residuals
plot(predichos,residuales, main="Predichos VS Residuales", xlab="Valores predichos", ylab="Residuales", ylim = c(-60,45), xlim=c(40,160))

#Homocedasticidad
# Obtener los residuos del modelo
residuos <- residuals(anova)

# Realizar la prueba de Levene sobre los residuos para verificar homocedasticidad
cat("\nPrueba de Levene para homocedasticidad de los residuos\n")
prueba_levene_residuos <- leveneTest(residuos ~ material * temperatura, data = datos)
print(prueba_levene_residuos)
