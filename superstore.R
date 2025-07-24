#....cargar librerias


#Cargar el conjunto de datos
> superstore_data <- read.csv("Superstore.csv")
> View(superstore_data)
> head(superstore_data)


#Fechas a formato Date
superstore_data$Order.Date <- as.Date(superstore_data$Order.Date, format="%m/%d/%Y")
superstore_data$Ship.Date <- as.Date(superstore_data$Ship.Date, format="%m/%d/%Y")


str(superstore_data) #Tipos de dato

superstore_data <- na.omit(superstore_data) #Eliminar valores NA

##ANALISIS ESTADISTICO

#Resumen estadístico de variables numéricas
summary(superstore_data)

##GRÁFICOS 

#Gráfico de ventas por categoría
ggplot(superstore_data, aes(x=Category, y=Sales)) +
  geom_boxplot(fill="red", color="lightblue", outlier.color = "blue", outlier.size = 2) +
  labs(title="Distribución de Ventas por Categoría", x="Categoría", y="Ventas") +
  theme_minimal(base_size = 15) +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12)) +
  scale_y_continuous(labels = scales::dollar_format()) +
  coord_cartesian(ylim = c(0, max(superstore_data$Sales, na.rm = TRUE) * 1.1))

# Gráfico de ganancias por región
ggplot(superstore_data, aes(x=Region, y=Profit, fill=Region)) +
  geom_bar(stat="identity", show.legend = FALSE) +
  labs(title="Ganancias por Región", x="Región", y="Ganancias") +
  theme_minimal(base_size = 15) +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14)) +
  scale_fill_manual(values = c("lightgreen", "lightblue", "purple", "pink")) +
  scale_y_continuous(labels = scales::dollar_format())

#ANALISIS
#Análisis de la relación entre descuentos y ganancias
ggplot(superstore_data, aes(x=Discount, y=Profit)) +
  geom_point(alpha=0.5, color="orange") +
  labs(title="Relación entre Descuentos y Ganancias", x="Descuento", y="Ganancias") +
  theme_minimal(base_size = 15) +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14)) +
  scale_y_continuous(labels = scales::dollar_format())

#_______________________#
###MODELO DE REGRESION###
#Preparacion de datos
model_data <- superstore_data %>%
  select(Sales, Quantity, Discount, Profit)

#Dividir los datos en conjunto de entrenamiento y prueba
set.seed(123)
train_index <- createDataPartition(model_data$Sales, p=0.8, list=FALSE)
train_data <- model_data[train_index, ]
test_data <- model_data[-train_index, ]

#Ajustar Modelo
sales_model <- lm(Sales ~ Quantity + Discount, data=train_data)

#Resumen del modelo
summary(sales_model)

#Predecir en el conjunto de prueba
predictions <- predict(sales_model, newdata=test_data)

#Calcular el error cuadrático medio (RMSE)
rmse <- sqrt(mean((predictions - test_data$Sales)^2))
print(paste("RMSE:", rmse))
