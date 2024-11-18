library(readxl)
library(dplyr)
library(ggplot2)
library(car)
library(agricolae)

# Leer datos desde el archivo Excel
ruta_archivo <- "ruta"  
datos <- read_excel(ruta_archivo)

# Factores
datos$Metodología <- factor(datos$Metodología)
datos$Madurez <- factor(datos$Madurez)

# Análisis Descriptivo
descriptivo <- datos %>%
  group_by(Metodología, Madurez) %>%
  summarise(
    n = n(),
    media = mean(Productividad),
    error_estandar = sd(Productividad) / sqrt(n),
    limite_inferior = qt(0.025, df = n - 1) * (sd(Productividad) / sqrt(n)) + mean(Productividad),
    limite_superior = qt(0.975, df = n - 1) * (sd(Productividad) / sqrt(n)) + mean(Productividad)
  )

print("Estadísticas descriptivas por Metodología y Madurez")
print(as.data.frame(descriptivo))

# Estadísticas descriptivas por Metodología
descriptivo_metodologia <- datos %>%
  group_by(Metodología) %>%
  summarise(
    n = n(),
    media = mean(Productividad),
    error_estandar = sd(Productividad) / sqrt(n)
  )

print("Estadísticas descriptivas por Metodología")
print(as.data.frame(descriptivo_metodologia))

# Estadísticas descriptivas por Madurez
descriptivo_madurez <- datos %>%
  group_by(Madurez) %>%
  summarise(
    n = n(),
    media = mean(Productividad),
    error_estandar = sd(Productividad) / sqrt(n)
  )

print("Estadísticas descriptivas por Madurez")
print(as.data.frame(descriptivo_madurez))

# Gráfico de interacciones
grafico_interacciones <- ggplot(datos, aes(x = Madurez, y = Productividad, color = Metodología, group = Metodología)) +
  stat_summary(fun = mean, geom = "line", linewidth = 1.2) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  labs(
    title = "Gráfico de Interacciones entre Metodología y Madurez",
    x = "Madurez",
    y = "Productividad promedio"
  ) +
  theme_minimal()

# Mostrar gráfico de interacciones
print(grafico_interacciones)

# Gráfico de medias para Metodología
grafico_metodologia <- ggplot(descriptivo_metodologia, aes(x = Metodología, y = media)) +
  geom_errorbar(aes(ymin = media - error_estandar, ymax = media + error_estandar), width = 0.2) +
  labs(title = "Gráfico de Medias por Metodología", x = "Metodología", y = "Productividad promedio") +
  theme_minimal()

# Gráfico de medias para Madurez
grafico_madurez <- ggplot(descriptivo_madurez, aes(x = Madurez, y = media)) +
  geom_errorbar(aes(ymin = media - error_estandar, ymax = media + error_estandar), width = 0.2) +
  labs(title = "Gráfico de Medias por Madurez", x = "Madurez", y = "Productividad promedio") +
  theme_minimal()

# Mostrar gráficos
print(grafico_metodologia)
print(grafico_madurez)

# Modelo de ANOVA
modelo <- lm(Productividad ~ Metodología * Madurez, data = datos)

# Realizar ANOVA
anova <- aov(modelo)
print("Resultados del ANOVA")
summary(anova)

# Prueba LSD para Metodología
pruebaLSD_metodologia <- LSD.test(anova, "Metodología", p.adj = "none")
print(pruebaLSD_metodologia)
plot(pruebaLSD_metodologia)

# Prueba LSD para Madurez
pruebaLSD_madurez <- LSD.test(anova, "Madurez", p.adj = "none")
print(pruebaLSD_madurez)
plot(pruebaLSD_madurez)

# Prueba LSD para la interacción
pruebaLSD_interaccion <- LSD.test(anova, c("Metodología", "Madurez"), group = TRUE, console = TRUE)
plot(pruebaLSD_interaccion)

# Prueba de normalidad
shapiro.test(modelo$residuals)

# Gráfico QQ para normalidad
qqPlot(modelo$residuals, main = "Gráfico Cuantil - Cuantil de los Residuos", id = FALSE)

# Valores predichos vs residuales
predichos <- modelo$fitted.values
residuales <- modelo$residuals
plot(predichos, residuales, main = "Valores Predichos vs Residuales",
     xlab = "Valores Predichos", ylab = "Residuales")

# Prueba de homocedasticidad
cat("\nPrueba de Levene para homocedasticidad de los residuos\n")
prueba_levene <- leveneTest(residuales ~ Metodología * Madurez, data = datos)
print(prueba_levene)
