# MUESTRA NO PARAMÉTRICA INDEPENDIENTE
# PRUEBA DE Wilcoxon Mann Whitney (Ejemplo como si fuera no parmétrica)
# Usamos mediana

femenino <- c(9,11,15)
masculino <- c(6,8,10,13)
#Dataframe de los datos
datos <- data.frame(
  Faltas = c(femenino, masculino),
  Genero = rep(c("Femenino", "Masculino"), times = c(length(femenino), length(masculino)))
)

print(datos)

# Estadísticas descriptivas por género
library(dplyr)
resumen <- datos %>%
  group_by(Genero) %>%
  summarise(
    Media = mean(Faltas),
    Mediana = median(Faltas),
    Min = min(Faltas),
    Max = max(Faltas),
    Rango_Intercuartil = IQR(Faltas)
  )

print(resumen)

library(ggplot2)
ggplot(datos, aes(x = Genero, y = Faltas, fill = Genero)) +
  geom_boxplot() +
  labs(title = "Diagrama de Caja y Bigotes de Faltas por Género",
       x = "Género", y = "Número de Faltas")

# Prueba de Wilcoxon Mann-Whitney
wilcox.test(Faltas ~ Genero, data = datos, exact = FALSE)