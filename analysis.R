# Bibliotecas necesarias.
library(conflicted)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(FactoMineR)
library(factoextra)

# Configuración de conflictos.
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")

# Carga de datos.
data <- read.csv("dataset_edited.csv")

# Identificar las columnas categóricas y convertirlas a factor.
categorical_vars <- c("sex", "smoking", "diabetes", "high_blood_pressure",
                      "anaemia", "death_event")

data <- data %>%
  mutate(across(all_of(categorical_vars), as.factor))

# Inspección inicial.
glimpse(data)
summary(data)
head(data)

# Correlación de Spearman de variables numéricas.
correlations <- data %>%
  select(where(is.numeric)) %>%
  cor(method = "spearman")
print(correlations)

# Correlación de Spearman entre creatinina y sodio sérico.
cor_test <- cor.test(data$creatinine, data$sodium, method = "spearman")
print(cor_test)

# Ver distribución de edad por evento de muerte.
table(data$sex, data$death_event)
ggplot(data, aes(x = age, fill = death_event)) +
  geom_histogram(bins = 30, position = "dodge") +
  labs(title = "Distribución de eventos de muerte por edad", fill = "Evento de muerte") +
  theme_minimal()

# Comparación de edad entre fallecidos y sobrevivientes (Mann-Whitney).
mann_whitney <- wilcox.test(age ~ death_event, data = data)
print(mann_whitney)

# Comparación de fracción de eyección entre sexos (Kruskal-Wallis).
kruskal_test <- kruskal.test(ejection_fraction ~ sex, data = data)
print(kruskal_test)

# Normalización de datos numéricos.
scaled_data <- data %>%
  select(creatinine, sodium, age, ejection_fraction) %>%
  scale()

# Agrupamiento jerárquico (dendrograma).
hclust_res <- hclust(dist(scaled_data, method = "euclidean"),
                     method = "ward.D2")
plot(hclust_res, labels = as.character(data$death_event),
     main = "Dendrograma de agrupamiento")

# Tabla de contingencia y prueba de ji al cuadrado: Fumar y evento de muerte.
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
