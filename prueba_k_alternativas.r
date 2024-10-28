# PRUEBA ANOVA PARA K ALTERNATIVAS
# Ejercicio de determinar el mejor algoritmo con base su rapidez
library(openxlsx)
library(readxl)
library(agricolae)
library(ggplot2)
library(dplyr)
library(car)
library(lmtest)

datos <- read.xlsx("ruta_archivo")
str(datos)

# Resumen descriptivo por algoritmo
resumen <- datos %>%
  group_by(algoritmo) %>%
  summarise(
    Media = mean(rapidez),
    Mediana = median(rapidez),
    Desviacion_Estandar = sd(rapidez),
    Minimo = min(rapidez),
    Maximo = max(rapidez)
  )
print(resumen)

# Diagrama de Caja (Boxplot) para rapidez por algoritmo
ggplot(datos, aes(x = factor(algoritmo), y = rapidez)) +
  geom_boxplot() +
  labs(title = "Diagrama de Caja de la Rapidez por Algoritmo", x = "Algoritmo", y = "Rapidez (tiempo)") +
  theme_minimal()

# Prueba ANOVA
anova_resultado <- aov(rapidez ~ factor(algoritmo), data = datos)
summary(anova_resultado)

# Prueba de múltples rangos (LSD) utilizando el resultado de ANOVA
lsd_test <- LSD.test(anova_resultado, "factor(algoritmo)", p.adj = "none")
print(lsd_test)

# Graficar los resultados de LSD
plot(lsd_test)

# Validación de supuestos

# 1. Normalidad (Gráfico QQ-Plot)

# Prueba de Shapiro-Wilk para normalidad
shapiro_test <- shapiro.test(anova_resultado$residuals)
print(shapiro_test)

qqnorm(residuals(anova_resultado))
qqline(residuals(anova_resultado), col = "red", lwd = 2)

# 2. Homocedasticidad (Gráfico de residuos vs predichos)

# Prueba de Levene para homocedasticidad

levene_test <- leveneTest(rapidez ~ factor(algoritmo), data = datos) 
print(levene_test)
plot(anova_resultado, 1)

# 3. Independencia (Gráfico de Residuos vs Orden de Observaciones)

# Prueba de Durbin-Watson para la independencia de los residuos

dw_test <- dwtest(anova_resultado)
print(dw_test)
plot(residuals(anova_resultado), main = "Residuos vs Orden", ylab = "Residuos", xlab = "Orden de Observaciones")