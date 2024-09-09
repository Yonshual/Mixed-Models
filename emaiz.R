#######################################################################
#     Modelos mixtos                                                  #
#     Uso de varIdent para modelar la heterocedasticidad              #
#     Análisis del rendimiento de maíz en múltiples ambientes         #
#     Yonshual Nehemías Xinico Ajú                                    #
#######################################################################

# Instalar paquetes necesarios
install.packages("broom.mixed") # para convertir resultados del mm en formato de tablas ordenadas
install.packages("cluster") # para análisis de conglomerados y agrupamiento jerárquico
install.packages("dendextend") # para las funcionalidades de dendogramas
install.packages("stats") # para regresiones, andeva, pruebas de hipótesis, análisis multivariado...
install.packages("nlme") # para modelos mixtos y modelos no generalizados

# Cargar bibliotecas necesarias

library(car) # regresión y diagnóstico de modelos
library(ggplot2) # para la construcción de gráficos
library(tidyr) # facilita la manipulación y limpieza de datos
library(ggdendro) # para crear dendogramas a partir de objetos de agrupamiento jerárquico
library(broom.mixed) # facilita conversión de resultado de mm
library(cluster) # herramientas para análisis de conglomerados
library(dendextend) # extiende funcionalidades para dendogramas
library(stats) # para regresión, andeva, pruebas de hipótesis

# Cargar la base de datos
if(!require(readxl)){install.packages("readxl")} # Lectura de archivos de Excel

datos<-read_excel("EnsayosMaiz.xlsx")
print(datos)
attach(datos)

############################
#   MODELO LINEAL MIXTO    #
############################

#Estimación del modelo
modelo_lmm <- lmer(Rend ~ Hibrido + (1|Ensayos), data = datos)
modelo_lmm

summary(modelo_lmm)

#GRÁFICOS PARA LA EXPLORACIÓN DEL MODELO

# Residuos vs. valores ajustados (Fitted)
plot(modelo_lmm)

# QQ plot de los efectos aleatorios
qqnorm(ranef(modelo_lmm)$Ensayos[,1])
qqline(ranef(modelo_lmm)$Ensayos[,1])

#Graficos de varianzas de las variables independientes

# Extraer los residuos
residuos <- resid(modelo_lmm)

#Residuos vs Ensayos
boxplot(split(residuos,Ensayos), xlab = "Ensayos", ylab = "Residuos")

#Residuos vs Hibridos
boxplot(split(residuos,Hibrido), xlab = "Hibrido", ylab = "Residuos")

###################################
#   ANÁLISIS POR CONGLOMERADOS    #
###################################

# Calcular la matriz de distancias euclideas
distancias <- dist(Ensayos, method = "euclidean")

# Realizar el análisis de conglomerados jerárquico con el método de Ward
cluster_jerarquico <- hclust(distancias, method = "ward.D2")

# Visualizar el dendrograma
plot(cluster_jerarquico, labels = FALSE, hang = -1, main = "Dendrograma - Método de Ward")

# Cortar el árbol para obtener un número específico de grupos
clusters <- cutree(cluster_jerarquico, k = 10)

# Añadir los conglomerados a los datos originales
datos$Cluster <- clusters

# Visualizar los resultados en un gráfico de dispersión
plot(Ensayos, Rend, col = datos$Cluster, pch = 19, 
     main = "Clusters obtenidos", xlab = "Ensayos", ylab = "Rendimiento")
legend("topright", legend = levels(datos$grupo), col = 1:4, pch = 19)

# Imprimir un resumen de los grupos
print(table(clusters))

##############################################################
#    CORRECCIÓN DE LA HETEROCEDASTICIDAD USANDO VARIDENT     #
##############################################################

#MODELACIÓN DE VARIANZAS

# Instalar y cargar el paquete 'nlme'
install.packages("nlme")
library(nlme)

# Ajuste de un modelo GLS considerando heterocedasticidad en 'Cluster'
model_gls <- gls(Rend ~ Hibrido, data = datos, 
                 weights = varIdent(form = ~1 | Cluster)) #Especifica que la varianza de los residuos puede diferir entre los niveles de cluster, modelando así la heterocedasticidad

# Resumen del modelo
summary(model_gls)

#####################################################
#   GRÁFICOS PARA LA EXPLORACIÓN DEL NUEVO MODELO   #
#####################################################

# Residuos vs. valores ajustados (Fitted)
plot(model_gls)

#Extraer los residuos del modelo creado
resid_est <- residuals(model_gls, type = "normalized")

# QQ plot de los efectos aleatorios
qqnorm(resid_est, 
       main = "Q-Q Plot de residuos estandarizados")

qqline(resid_est, 
       col = "red", 
       lwd = 2)

#Residuos vs Ensayos
boxplot(split(resid_est,datos$Ensayos), xlab = "Ensayos", ylab = "Residuos")

#Residuos vs Hibridos
boxplot(split(resid_est,datos$Hibrido), xlab = "Hibrido", ylab = "Residuos")

#Una vez los supuestos se cumplieron, se procede al ANOVA
anova(model_gls)

#################################################
#    PRUEBA DE COMPARACIÓN MÚLTIPLE DE MEDIAS   #
#################################################

# Instalar y cargar los paquetes necesarios
install.packages("agricolae")
library(agricolae)

model4 <- anova(model_gls)

# Realizar el ANOVA
anova_model <- aov(Rend ~ factor(Hibrido), data = datos)

# Resumen del ANOVA
summary(anova_model)

# Realizar el test LSD de Fisher
lsd_test <- LSD.test(anova_model, "factor(Hibrido)", p.adj = "none")

# Mostrar los resultados del test LSD
print(lsd_test)

# Graficar los resultados del test LSD
plot(lsd_test)




