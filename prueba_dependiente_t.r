# MUESTRAS DEPENDIENTES PARAMÉTRICAS
# EJERCICIO DEL PINTADO DE UN AUTOMÓVIL CON EL COFRE LEVANTANDO O BAJADO
library(ggplot2)
library(qqplotr)
library(car)
bajado <- c(3.4, 3.7,2.9,2.5,1.6,2.8,3.7,5.9,4.8,4.3)
levantado <- c(2.7,3.2,1.8,1.9,1.1,2.2,2.8,4.8,4.3,3.4)


# Dataframe para análisis descriptivo
summary_stats <- data.frame(
  Posicion = c("Bajado", "Levantado"),
  Media = c(mean(bajado), mean(levantado)),
  Mediana = c(median(bajado), median(levantado)),
  Desviacion_Estandar = c(sd(bajado), sd(levantado)),
  Minimo = c(min(bajado), min(levantado)),
  Maximo = c(max(bajado), max(levantado))
)

print(summary_stats)

# Datos combinados en un dataframe para el diagrama de caja y bigotes
datos <- data.frame(
  Valor = c(bajado, levantado),
  Posicion = rep(c("Bajado", "Levantado"), each = 10)
)

# Crear el boxplot
boxplot(Valor ~ Posicion, data = datos, 
        main = "Diagrama de Caja y Bigotes",
        xlab = "Posición",
        ylab = "Valores",
        col = c("lightblue", "lightgreen"))

# Prueba t para muestras pareadas
resultado_ttest <- t.test(bajado, levantado, paired = TRUE)

print(resultado_ttest)

# Prueba de normalidad de Shapiro-Wilk
shapiro_test_bajado <- shapiro.test(bajado)
shapiro_test_levantado <- shapiro.test(levantado)

print(shapiro_test_bajado)
print(shapiro_test_levantado)

# Crear un dataframe para usar en ggplot
df_diferencias <- data.frame(diferencias = diferencias)

# Gráfico Q-Q con bandas de confianza para la diferencias
ggplot(df_diferencias, aes(sample = diferencias)) +
  stat_qq_band() +  
  stat_qq_line() +  
  stat_qq_point() +  
  labs(title = "Q-Q Plot con Bandas de Confianza",
       x = "Cuantiles Teóricos",
       y = "Cuantiles de las Diferencias") +
  theme_minimal()

# Gráfico del coche levantado
qqnorm(levantado, main="Q-Q Plot del cofre levantado")
qqline(levantado, col="red")

#Gráfico del coche bajado
qqnorm(bajado, main="Q-Q Plot del cofre bajado")
qqline(bajado, col="red")

# Prueba de Levene
resultado_levene <- leveneTest(Valor ~ Posicion, data = datos)
print(resultado_levene)

