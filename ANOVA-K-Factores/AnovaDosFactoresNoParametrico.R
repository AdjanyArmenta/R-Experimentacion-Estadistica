# -----ANOVA PARA DOS FACTORES QUE NO PASA SUPUESTOS-----
library(readxl)
library(ggplot2)
library(dplyr)
library(car) 
library(FSA)  
library(stats) 

# 1. Leer datos desde Excel
ruta_archivo <- "/Users/adjanyarmenta/Downloads/02_Metodología-Madurez_Productividad.xlsx"
datos <- read_excel(ruta_archivo, sheet = "Técnica_Dominio")

# Verificar los primeros registros
head(datos)

# Asegurar que las variables sean factores
datos$metodologia <- factor(datos$Metodología)
datos$madurez <- factor(datos$Madurez)

# 2. ANOVA sin transformación (sólo para demostrar que no pasa supuestos)

# ANOVA
anova_result <- aov(Productividad ~ metodologia * madurez, data = datos)
summary(anova_result)

# Prueba de normalidad (Shapiro-Wilk) en los residuos del ANOVA
shapiro_test <- shapiro.test(residuals(anova_result))

# Mostrar resultados
print("Resultado de la prueba de normalidad:")
print(shapiro_test)

# Si no se cumplen los supuestos, pasar a pruebas no paramétricas

# 3. Pruebas No Paramétricas

# Prueba de Kruskal-Wallis para 'metodologia'
kruskal_metodologia <- kruskal.test(Productividad ~ metodologia, data = datos)

# Prueba de Kruskal-Wallis para 'madurez'
kruskal_madurez <- kruskal.test(Productividad ~ madurez, data = datos)

# Mostrar resultados de Kruskal-Wallis
print("Resultado de Kruskal-Wallis para metodologia:")
print(kruskal_metodologia)

print("Resultado de Kruskal-Wallis para madurez:")
print(kruskal_madurez)

# 4. Prueba de Friedman para evaluar interacción

library(reshape2) # Para reorganizar datos

datos_long <- dcast(datos, madurez ~ metodologia, value.var = "Productividad", fun.aggregate = mean)
data_matrix <- as.matrix(datos_long[, -1])
print(data_matrix)

# Test de Friedman (si la madurez interactua con la metodologia) 
friedman_result <- friedman.test(data_matrix)

print(friedman_result)

# 5. Pruebas Post-Hoc con Dunn Test si hay significancia

# Prueba de Dunn para 'metodologia' (Es para ver los grupos)
dunn_test_metodologia <- dunnTest(Productividad ~ metodologia, data = datos, method = "bonferroni")

# Prueba de Dunn para 'madurez' (Es para ver los grupos)
dunn_test_madurez <- dunnTest(Productividad ~ madurez, data = datos, method = "bonferroni")

# Mostrar resultados de las pruebas de Dunn
print("Resultados del Test de Dunn para metodologia:")
print(dunn_test_metodologia)

print("Resultados del Test de Dunn para madurez:")
print(dunn_test_madurez)
