#Reporte_2
getwd()
setwd("/Users/magy_rivera/Documents/Maestria_FCFM/1er_Tetra/Estadistica/Tareas/Reportes_MEB/R1MEB_MRV")
mi_data <- read.csv("Suicide_Data.csv", header = TRUE, sep = ",")
head(mi_data)
#Ver características de cada variable
#################################################suicides.100k.pop
#Medidas descriptivas: 

Valor_Minimo <- min(mi_data$suicides.100k.pop, na.rm = TRUE) #Mínimo
Valor_Max <- max(mi_data$suicides.100k.pop, na.rm = TRUE) #Máximo
Rango <- Valor_Max-Valor_Minimo #Rango
Media <- mean(mi_data$suicides.100k.pop, na.rm = TRUE) #Media
Varianza <- var(mi_data$suicides.100k.pop, na.rm = TRUE) #Varianza
DesvEstandar <-sd(mi_data$suicides.100k.pop, na.rm = TRUE) #Desviación Estándar
CoefVar<-(DesvEstandar/Media)*100 #Coeficiente de Variacion

#Resultados
Valor_Minimo
Valor_Max
Rango
Media
Varianza
DesvEstandar
CoefVar

# Crear el histograma
hist(
     mi_data$suicides.100k.pop, 
     main = "Distribución de la Tasa de Suicidios por 100,000 Habitantes", 
     xlab = "Tasa de Suicidios por cada 100k habitantes", 
     ylab = "Frecuencia", 
     col = "lightblue", 
     border = "black",
     ylim = c(0, 27000),
     xlim = c(0, 150),
     cex.axis = 0.7
     )

#Creamos BoxPlot
boxplot(mi_data$suicides.100k.pop, 
        main = "Distribución de Suicidios por 100,000 Personas", 
        horizontal = TRUE, 
        col = "lightgreen", 
        xlab = "suicides/100k pop")

#Realizamos la tabla de frecuencia
# Definir los intervalos para agrupar la variable
breaks <- seq(0, max(mi_data$suicides.100k.pop, na.rm = TRUE), by = 5)
mi_data$suicides_group <- cut(mi_data$suicides.100k.pop, breaks = breaks, include.lowest = TRUE)
tabla_frecuencia <- table(mi_data$suicides_group)

# Mostrar la tabla de frecuencia
print(tabla_frecuencia)

# Convertimos la tabla a un data frame
df_frecuencia <- as.data.frame(tabla_frecuencia)
colnames(df_frecuencia) <- c("Intervalo de Suicidios/100k pop", "Frecuencia")

# Se hace un objeto flexible
ft <- flextable(df_frecuencia)
ft <- theme_box(ft)  # Aplicar un tema básico
ft <- autofit(ft)    # Ajustar el ancho de las columnas

# Se crea un documento en word nuevo
doc <- read_docx()
doc <- body_add_flextable(doc, value = ft)
doc <- body_add_par(doc, "Tabla de Frecuencia de Suicides/100k Pop", style = "heading 1")

# Guardar el documento de Word
print(doc, target = "tabla_frecuencia_suicides.docx")
