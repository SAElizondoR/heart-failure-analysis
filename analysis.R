# Bibliotecas necesarias.
packages <- c("dplyr","tidyverse", "janitor", 
              "ggplot2", "conflicted","FactoMineR",#analisis factorial
              "factoextra")# Visualización de análisis factorial.
pacman::p_load( packages , character.only = TRUE )
rm(packages)

# Configuración de preferencias para conflictos.
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")

# Carga de datos.
data <- read.csv("dataset_edited.csv")

# Identificar las columnas categóricas y convertirlas a factor.
categorical_vars <- c("sex", "smoking", "diabetes", "high_blood_pressure",
                      "anaemia", "death_event")
data[categorical_vars] <- lapply(data[categorical_vars], as.factor)

# Inspección inicial.
glimpse(data)
summary(data)
head(data)

# Tabla de contingencia entre sexo y evento de muerte.
contingency_table <- table(data$sex, data$death_event)
print(contingency_table)

# Porcentaje de hombres y mujeres fallecidos.
prop_table <- prop.table(contingency_table, margin = 1)
print(prop_table)

# Correlación de Spearman entre variables numéricas.
correlations <- cor(select(data, where(is.numeric)), method = "spearman")
print(correlations)

# Correlación de Spearman entre creatinina y sodio sérico.
cor_test <- cor.test(data$creatinine, data$sodium, method = "spearman")
print(cor_test)
# Ver distribución de edad por evento de muerte.
ggplot(data, aes(x = age, fill = death_event)) +
  geom_histogram(bins = 30, position = "dodge") +
  labs(title = "Distribución de eventos de muerte por edad",
       fill = "Evento de muerte") +
  theme_minimal()


# Comparación de edad entre fallecidos y sobrevivientes (Mann-Whitney).
mann_whitney <- wilcox.test(age ~ death_event, data = data)
print(mann_whitney)

# Comparación de fracción de eyección entre sexos (Kruskal-Wallis).
kruskal_test <- kruskal.test(ejection_fraction ~ sex, data = data)
print(kruskal_test)

# Normalización de datos numéricos.
scaled_data <- scale(select(data, creatinine, sodium, age, ejection_fraction))

# Agrupamiento jerárquico (dendrograma).
dist_matrix <- dist(scaled_data, method = "euclidean")
hclust_res <- hclust(dist_matrix, method = "ward.D2")
plot(hclust_res, labels = as.character(data$death_event),
     main = "Dendrograma de agrupamiento", cex = 0.5)

# Corte del dendrograma.
num_clusters <- 6
clusters <- cutree(hclust_res, k = num_clusters)

# Agregar los grupos a la tabla original.
data$cluster <- as.factor(clusters)

# Ver cómo se distribuyen los eventos de muerte en los grupos.
table(data$death_event, data$cluster)

# Calcular el porcentaje de fallecidos por cada grupo.
prop_fallecidos <- prop.table(table(data$death_event, data$cluster), margin = 2)
print(prop_fallecidos)

# Ver la distribución de mortalidad por grupo.
ggplot(data, aes(x = cluster, fill = factor(death_event))) +  # Mapeamos como factor
  geom_bar(position = "fill") +
  labs(title = "Distribución de Mortalidad por Grupo", y = "Proporción", fill = "Estado de Mortalidad") +  # Cambiar el título de la leyenda
  scale_fill_manual(
    values = c("0" = "lightblue", "1" = "blue"),  # Colores para 0 y 1
    labels = c("0" = "No Muerto", "1" = "Muerto")  # Etiquetas personalizadas
  ) +
  theme_minimal()


# Prueba de ji al cuadrdo para ver si la mortalidad está asociada con los
# grupos.
chi_square_test <- chisq.test(table(data$death_event, data$cluster))
print(chi_square_test)

# Variables numéricas por grupo (ejemplo con creatinina).
ggplot(data, aes(x = cluster, y = creatinine, fill = cluster)) +
  geom_violin() +
  labs(title = "Distribución de Creatinina por Grupo", y = "Creatinina")

# Tabla de contingencia y prueba de ji al cuadrado: Tabaquismo y evento de
# muerte.
contingency_table <- table(data$smoking, data$death_event)
chi_square_test <- chisq.test(contingency_table)
print(contingency_table)
print(chi_square_test)

# Análisis factorial de datos mixtos (FAMD).
famd_res = FAMD(data, graph = TRUE)
fviz_famd_var(famd_res, repel = TRUE, ggtheme = theme_minimal())

# Resumen de resultados.
summary_table <- tibble(
  Prueba = c("Mann-Whitney", "Kruskal-Wallis", "Correlación de Spearman"),
  `Valor p` = c(mann_whitney$p.value,
                kruskal_test$p.value,
                cor_test$p.value) %>%
    format(scientific = TRUE)
)
print(summary_table)
