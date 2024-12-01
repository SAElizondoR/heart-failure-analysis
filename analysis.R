# Bibliotecas necesarias.
packages <- c("dplyr","tidyverse", "janitor","reshape2","RColorBrewer","rpart.plot",
              "ggplot2", "conflicted","FactoMineR",#analisis factorial
              "factoextra")# Visualización de análisis factorial.
pacman::p_load( packages , character.only = TRUE )
rm(packages)

# Configuración de preferencias para conflictos.
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
conflicts_prefer(janitor::chisq.test)

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
correlations <- cor(select((data%>%select(-time)), where(is.numeric)), method = "spearman")
print(correlations)

correlations_melted <- melt(round(correlations,3))
ggplot(correlations_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() + 
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, 
                       limits = c(-1, 1), name = "Correlación") + 
  geom_text(aes(label = value), color = "black", size = 4) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        axis.text.y = element_text(angle = 0, hjust = 1),   
        plot.title = element_text(hjust = 0.5)) + 
  labs(title = "Correlaciones de Spearman") +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) 

# Correlación de Spearman entre creatinina y sodio sérico.
cor_test <- cor.test(data$creatinine, data$sodium, method = "spearman")
print(cor_test)
# Ver distribución de edad por evento de muerte.
ggplot(data, aes(x = age, fill = death_event)) +
  geom_histogram(bins = 30, position = "dodge") +
  labs(title = "Distribución de eventos de muerte por edad",
       fill = "Evento de muerte") +
  scale_fill_manual(values = c("0" = "green4", "1" = "blue")) +
  theme_minimal()


# Comparación de edad entre fallecidos y sobrevivientes (Mann-Whitney).

#H0: No hay diferencia entre las edades de fallecidos y sobrevivientes
#Ha: Hay diferencia entre las edades de fallecidos y sobrevivientes
mann_whitney <- wilcox.test(age ~ death_event, data = data)
print(mann_whitney)
#p-valor= 0.00017
#Se rechaza la hipotesis nula

#La edad sí es relevante para determinar la fatalidad de los casos.

#Distribucion de Edad por sexos en nuestros pacientes.
ggplot(data, aes(x = sex, y = age, fill = as.factor(sex))) +
  geom_boxplot() +
  labs(title = "Distribución de Edad por Sexo", x = "Sexo", y = "Edad", fill = "") +
  scale_x_discrete(labels = c("0" = "Mujer", "1" = "Hombre")) +  
  scale_fill_manual(values = c("0" = "salmon", "1" = "lightblue3"), 
                    labels = c("0" = "Mujer", "1" = "Hombre")) +  
  theme_minimal()

#H0: No hay diferencia entre las edades de hombres y mujeres pacientes
#Ha: Hay diferencia entre las edades de hombres y mujeres pacientes
mann_whitney_edad_por_Sex <- wilcox.test(age ~ sex, data = data)
mann_whitney_edad_por_Sex
#p-valor= 0.3188
#No se rechaza la hipotesis nula
#No hay diferencia entre las edades de hombres y mujeres pacientes


# Comparación de fracción de eyección entre sexos (Kruskal-Wallis).

#H0: La fracción de eyección no varía entre sexos
#Ha: La fracción de eyección varía entre sexos
kruskal_test <- kruskal.test(ejection_fraction ~ sex, data = data)
print(kruskal_test)

#p-valor= 0.024
#Se rechaza la hipotesis nula

#La fracción de eyección sí varía entre sexos.

ggplot(data, aes(x = as.factor(sex), y = ejection_fraction, fill = as.factor(sex))) +
  geom_boxplot() +
  labs(title = "Distribución de la Fracción de Eyección por Sexo",
       x = "Sexo",
       y = "Fracción de Eyección") +
  scale_x_discrete(labels = c("0" = "Mujer", "1" = "Hombre")) +
  scale_fill_manual(values = c("0" = "coral", "1" = "lightblue4")) +
  theme_minimal()

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

# Calcular el porcentaje de fallecidos por cada grupo.
prop_fallecidos <- prop.table(table(data$death_event, data$cluster), margin = 2)
print(prop_fallecidos)

# Ver la distribución de mortalidad por grupo.
ggplot(data, aes(x = cluster, fill = factor(death_event))) +
  geom_bar(position = "fill") +
  labs(title = "Distribución de Mortalidad por Grupo", y = "Proporción", fill = "Estado de Mortalidad") + 
  scale_fill_manual(
    values = c("0" = "lightblue", "1" = "blue"),  
    labels = c("0" = "No Muerto", "1" = "Muerto")
  ) +
  theme_minimal()


# Prueba de ji al cuadrdo para ver si la mortalidad está asociada con los
# grupos.

#H0: La frecuencia de muertes no esta relacionada con el cluster asignado
#Ha: La frecuencia de muertes se relaciona con el cluster asignado
chi_square_test <- chisq.test(table(data$death_event, data$cluster))
print(chi_square_test)
#p-valor= 1.92 x 10^-6
#Se rechaza la hipotesis nula
#La frecuencia de muertes varía entre grupos

# Variables numéricas por grupo
ggplot(data, aes(x = cluster, y = creatinine, fill = cluster)) +
  geom_violin() +
  labs(title = "Distribución de Creatinina por Grupo", y = "Creatinina")
ggplot(data, aes(x = cluster, y = sodium, fill = cluster)) +
  geom_violin() +
  labs(title = "Distribución de Sodio por Grupo", y = "Sodio")

# Tabla de contingencia y prueba de ji al cuadrado: Tabaquismo y evento de
# muerte.
contingency_table <- table(data$smoking, data$death_event)

#H0: La frecuencia de muertes no esta relacionada con fumar
#Ha: La frecuencia de muertes se relaciona con fumar
chi_square_test <- chisq.test(contingency_table)
print(contingency_table)
print(chi_square_test)
#p-valor= 0.93
#No se rechaza la hipotesis nula
#La frecuencia de muertes NO se relaciona con ser o no fumador

#H0: La frecuencia de muertes no esta relacionada con diabetes
#Ha: La frecuencia de muertes se relaciona con diabetes
contingency_table <- table(data$diabetes, data$death_event)
chi_square_test <- chisq.test(contingency_table)
print(contingency_table)
print(chi_square_test)
#p-valor= 1
#Noe rechaza la hipotesis nula
#La frecuencia de muertes NO se relaciona con ser diabético

#H0: La frecuencia de muertes no esta relacionada con la anemia
#Ha: La frecuencia de muertes se relaciona con la anemia
contingency_table <- table(data$anaemia, data$death_event)
chi_square_test <- chisq.test(contingency_table)
print(contingency_table)
print(chi_square_test)
#p-valor= 0.3
#No se rechaza la hipotesis nula
#La frecuencia de muertes NO se relaciona con ser anémico

#H0: La frecuencia de muertes no esta relacionada al sexo
#Ha: La frecuencia de muertes se relaciona con el sexo
contingency_table <- table(data$sex, data$death_event)
chi_square_test <- chisq.test(contingency_table)
print(contingency_table)
print(chi_square_test)
#p-valor= 1
#No se rechaza la hipotesis nula
#La frecuencia de muertes NO se relaciona con ser hombre o mujer


#Diferencias en Indices Sanguineos
indices <- c("sodium", "creatinine", "CPK", "platelets")
results <- list()
for (var in indices) {
  mann_whitney_indice <- wilcox.test(data[[var]] ~ data$death_event)
  results[[var]] <- list(
    p_value = mann_whitney_indice$p.value,
    interpretation = ifelse(mann_whitney_indice$p.value < 0.05, 
                            "Hay una diferencia significativa.",
                            "No hay diferencia significativa."))}
for (var in indices) {
  cat("\nVariable:", var, "\n")
  cat("Valor p:", results[[var]]$p_value, "\n")
  cat("Interpretación:", results[[var]]$interpretation, "\n")
}
muertos <- data %>% filter(death_event == 1)
vivos <- data %>% filter(death_event == 0)
mann_whitney_sodium <- wilcox.test(muertos$sodium,vivos$sodium, paired=FALSE, alternative = "less")
print(mann_whitney_sodium)
#El sodio es menor en pacientes fallecidos
mann_whitney_creatinine <- wilcox.test(muertos$creatinine,vivos$creatinine, paired=FALSE, alternative = "greater")
print(mann_whitney_creatinine)
#La creatinina es mayor en pacientes fallecidos


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



#Árbol de decisión
tree_model <- rpart(death_event ~ age + sex + smoking + creatinine + ejection_fraction+diabetes+high_blood_pressure+anaemia+sodium+platelets+CPK,
                    data = data, method = "class")
rpart.plot(tree_model)

# Matriz de Confusión
pred_tree <- predict(tree_model, type = "class")
table(pred_tree, data$death_event)
#Accuracy 82.5%

#Random Forest

library(randomForest)

rf_model <- randomForest(death_event ~ age + sex + smoking + creatinine + ejection_fraction+diabetes+high_blood_pressure+anaemia+sodium+platelets+CPK,
                         data = data, ntree = 100)
print(rf_model) #74.6% de rendimiento
importance(rf_model)


