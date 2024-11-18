#-----ANOVA DOS FACTORES------
library(readxl)
library(dplyr)
library(ggplot2)
library(car)
library(agricolae)
library(flextable)

# Leer datos desde el archivo Excel
ruta_archivo <- "/Users/adjanyarmenta/Downloads/02_Metodología-Madurez_Productividad.xlsx"
datos <- read_excel(ruta_archivo, sheet = "Técnica_Dominio")  

# Convertir Madurez y Metodología a factores
datos$Madurez <- factor(datos$Madurez)
datos$Metodología <- factor(datos$Metodología)

# Análisis Descriptivo con datos transformados
descriptivo_log <- datos %>%
  group_by(Metodología, Madurez) %>%
  summarise(
    n = n(),
    media = mean(Logaritmica),
    error_estandar = sd(Logaritmica) / sqrt(n),
    limite_inferior = qt(0.025, df = n - 1) * (sd(Logaritmica) / sqrt(n)) + mean(Logaritmica),
    limite_superior = qt(0.975, df = n - 1) * (sd(Logaritmica) / sqrt(n)) + mean(Logaritmica)
  )

print("Estadísticas descriptivas por Madurez y Metodología (Transformación Logarítmica)")
flextable(descriptivo_log)

# Estadísticas descriptivas por Metodología
descriptivo_metodologia <- datos %>%
  group_by(Metodología) %>%
  summarise(
    n = n(),
    media = mean(Logaritmica),
    error_estandar = sd(Logaritmica) / sqrt(n)
  )

print("Estadísticas descriptivas por Metodología")
flextable(descriptivo_metodologia)

# Estadísticas descriptivas por Madurez
descriptivo_madurez <- datos %>%
  group_by(Madurez) %>%
  summarise(
    n = n(),
    media = mean(Logaritmica),
    error_estandar = sd(Logaritmica) / sqrt(n)
  )

print("Estadísticas descriptivas por Madurez")
flextable(descriptivo_madurez)


# Gráfico de interacciones
grafico_interacciones <- ggplot(datos, aes(x = Madurez, y = Logaritmica, color = Metodología, group = Metodología)) +
  stat_summary(fun = mean, geom = "line", linewidth = 1.2) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  labs(
    title = "Gráfico de Interacciones entre Metodología y Madurez",
    x = "Madurez",
    y = "Productividad transformada"
  ) +
  theme_minimal()

# Mostrar gráfico de interacciones
print(grafico_interacciones)

# Modelo de ANOVA de dos factores con datos transformados
modelo_log <- lm(Logaritmica ~ Madurez * Metodología, data = datos)

# Realizar ANOVA de dos factores
anova_log <- aov(modelo_log)
summary(anova_log)

# Prueba de Shapiro-Wilk para normalidad de residuos
shapiro_test_log <- shapiro.test(residuals(modelo_log))
print(shapiro_test_log)

# Gráfico Q-Q para ver si los residuos son normales
qqPlot(anova_log, main = "Gráfico Cuantil-Cuantil (Transformación Logarítmica)", id = FALSE)

# Prueba de Levene para homocedasticidad
prueba_levene_log <- leveneTest(residuals(modelo_log) ~ Madurez * Metodología, data = datos)
print(prueba_levene_log)

# Valores predichos vs residuales
predichos <- modelo_log$fitted.values
residuales <- modelo_log$residuals
plot(predichos, residuales, main = "Valores Predichos vs Residuales",
     xlab = "Valores Predichos", ylab = "Residuales")

# Análisis Post Hoc: Prueba LSD para ambos factores
pruebaLSD_log_madurez <- LSD.test(anova_log, "Madurez", p.adj = "none")
pruebaLSD_log_metodologia <- LSD.test(anova_log, "Metodología", p.adj = "none")

print(pruebaLSD_log_madurez)
plot(pruebaLSD_log_madurez)

print(pruebaLSD_log_metodologia)
plot(pruebaLSD_log_metodologia)

# Prueba LSD para la interacción
pruebaLSD_interaccion <- LSD.test(anova_log, c("Metodología", "Madurez"), group = TRUE, console = TRUE)
plot(pruebaLSD_interaccion)

      
