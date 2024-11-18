# MUESTRAS PARAMÉTRICAS INDEPENDIENTES
# EJERCICIO DE DOS SEDES COMPARANDO LÍNEAS DE CÓDIGO
library(readxl)
library(ggplot2)
library(car)
library(lmtest)

# Leer el archivo Excel
data <- read_excel("/Users/adjanyarmenta/Downloads/BD_Sede-LC (2).xlsx")

# Filtrar los datos para cada sede
x_data <- data$`#LC`[data$Sede == "x"]
y_data <- data$`#LC`[data$Sede == "y"]

print(x_data)
print(y_data)

# Dataframe para análisis descriptivo
summary_stats <- data.frame(
  Sede = c("Mérida", "Valladolid"),
  Media = c(mean(x_data), mean(y_data)),
  Mediana = c(median(x_data), median(y_data)),
  Desviacion_Estandar = c(sd(x_data), sd(y_data)),
  Minimo = c(min(x_data), min(y_data)),
  Maximo = c(max(x_data), max(y_data))
)

print(summary_stats)

# Diagrama de caja y bigotes
ggplot(data, aes(x = Sede, y = `#LC`, fill = Sede)) +
  geom_boxplot() +
  labs(title = "Diagrama de Caja y Bigotes por Sede", y = "# de Líneas de Código", x = "Sede") +
  theme_minimal()

# Prueba t para diferencias de medias de las sedes
t_test_result <- t.test(x_data, y_data)
print(t_test_result)

# Prueba de normalidad de Shapiro-Wilk
shapiro_test_x <- shapiro.test(x_data)
shapiro_test_y <- shapiro.test(y_data)

print(shapiro_test_x)
print(shapiro_test_y)

# Gráfico de Mérida
qqnorm(x_data, main="Q-Q Plot de Mérida")
qqline(x_data, col="red")

#Gráfico de Valladolid
qqnorm(y_data, main="Q-Q Plot de Valladolid")
qqline(y_data, col="red")


# Prueba de homocedasticidad (prueba de Levene)
levene_test_result <- leveneTest(`#LC` ~ Sede, data = data)
print(levene_test_result)


# Dataframe con los datos
data <- data.frame(
  LC = c(x_data, y_data),
  Sede = factor(rep(c("Mérida", "Valladolid"), each = length(x_data)))
)

# Ajustar un modelo de regresión
modelo <- lm(LC ~ Sede, data = data)

# Prueba de Breusch-Pagan para comparar varianzas 
bp_test <- bptest(modelo)
print(bp_test)