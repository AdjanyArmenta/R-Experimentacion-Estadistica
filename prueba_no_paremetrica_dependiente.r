# MUESTRAS DEPENDIENTES NO PARAMÉTRICAS
# Prueba de los rangos con signo (Wilcoxon), ejercicio de un proyecto utilizando dos técnicas (a,b)
# Usamos la mediana no la media
proyecto <- 1:9
TA <- c(0.47, 1.02, 0.33, 0.70, 0.94, 0.85, 0.39, 0.52, 0.47)
TB <- c(0.41, 1.00, 0.46, 0.61, 0.84, 0.87, 0.36, 0.52, 0.51)

# Dataframe con los datos para el análisis descriptivo
datos_summary <- data.frame(Proyecto = proyecto, TA = TA, TB = TB)
summary(datos_summary)

# Dataframe
datos <- data.frame(
  Proyecto = rep(proyecto, 2),
  Errores = c(TA, TB),
  Técnica = rep(c("TA", "TB"), each = length(proyecto))
)

# Diagrama de caja y bigotes
ggplot(datos, aes(x = Técnica, y = Errores, fill = Técnica)) +
  geom_boxplot() +
  labs(title = "Diagrama de Caja y Bigotes de TA vs TB",
       x = "Técnica",
       y = "Errores detectados por unidad de tiempo")

# Prueba de Wilcoxon para muestras pareadas
wilcox.test(TA, TB, paired = TRUE)