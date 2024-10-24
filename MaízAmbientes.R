#Análisis del rendimiento de maíz en múltiples ambientes-desbalance#
rm(list = ls()) #Limpiar memoria#
# Instalar paquetes necesarios
if(!require(broom.mixed)){install.packages("broom.mixed")}
if(!require(cluster)){install.packages("cluster")}
if(!require(dendextend)){install.packages("dendextend")}
if(!require(stats)){install.packages("stats")}
if(!require(nlme)){install.packages("nlme")}
if(!require(fBasics)){install.packages("fBasics")}
# Cargar bibliotecas necesarias

library(car)
library(ggplot2)
library(tidyr)
library(ggdendro)
library(broom.mixed)
library(cluster)
library(dendextend)
library(stats)
library(fBasics)
# Cargar la base de datos
if(!require(readxl)){install.packages("readxl")} # Lectura de archivos de Excel

datos<-read_excel("EnsayosMaiz.xlsx")
print(datos)
attach(datos)
colnames(datos)
str(datos)
#Transformamos los datos de ensayos e hibrido a factor
datos$Ensayos <-as.factor(datos$Ensayos)
datos$Hibrido <-as.factor(datos$Hibrido)
str(datos)
#hacemos un resumen de estadísticas descriptivas importantes
basicStats(datos$Rend)
#Graficamos el rendimiento en función de las localidades por un boxplot
ggplot(datos,aes(x=Ensayos, y=Rend))+
  geom_boxplot() +
  labs(title="Boxplot del Rendimiento por Ensayo", 
       x= "Ensayos",
       y= "Rendimiento") +
  theme(axis.text.x = element_text(angle= 45, hjust= 1))
#Efectuamos la misma gráfica pero para el hibrido en función del rendimiento
ggplot(datos, aes(x=Hibrido, y=Rend))+
  geom_boxplot() +
  labs(title = "Boxplot del Rendimiento por Híbrido",
       x="Híbrido", 
       y="Rendimiento")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1 ))

                
############################
#   MODELO LINEAL MIXTO    #
############################

#Estimación del modelo
modelo_lmm <- lme(fixed= Rend ~ Hibrido, random = ~ 1|Ensayos, data = datos)
modelo_lmm

summary(modelo_lmm)
anova(modelo_lmm)

#GRÁFICOS PARA LA EXPLORACIÓN DEL MODELO

# Residuos vs. valores ajustados (Fitted)
residuos<-residuals(modelo_lmm, type = "normalized")
qqnorm(residuos)
qqline(residuos, col="red")
#Gráfico de residuos vs Predichos
valores_predichos<-fitted(modelo_lmm)
plot(valores_predichos,residuos,
     xlab="Valores Predichos", 
     ylab = "Residuos Normalizados",
     main="Residuos vs. Valores Predichos")
abline(h=0, col="red")
#Cuál es el valor que me está causado ruido
#Otra forma más práctica de hacerlo
plot(modelo_lmm)
#Gráfico de boxplot de los residuos en función del híbrido
datos$residuos<- residuos #Agregamos los residuos a nuestro data.frame
ggplot(datos, aes(x=Hibrido, y=residuos))+
  geom_boxplot()+
  labs(title="Boxplot de Residuos por Híbrido",
       x="Híbrido",
       y= "Residuos normalizados")+
  theme_minimal()
#Gráfico de boxplot para los residuos en función de los ensayos
ggplot(datos, aes(x=Ensayos, y=residuos))+
  geom_boxplot()+
  labs(title="Boxplot de Residuos por Ensayos",
       x="Ensayos",
       y="Residuos normalizados")+
  theme_minimal()

#Trabajamos con los residuos estandarizados de Pearson
residuos_estandarizados<-residuals(modelo_lmm, type="pearson")
qqnorm(residuos_estandarizados)
qqline(residuos_estandarizados, col="red")
datos$residuos_estandarizados<- residuos_estandarizados #Agregamos los residuos a nuestro data.frame
ggplot(datos, aes(x=Hibrido, y=residuos))+
  geom_boxplot()+
  labs(title="Boxplot de Residuos por Híbrido",
       x="Híbrido",
       y= "Residuos_estandarizados")+
  theme_minimal()
 
ggplot(datos, aes(x=Ensayos, y=residuos))+
  geom_boxplot()+
  labs(title="Boxplot de Residuos por Ensayos(loc)",
       x="Ensayos",
       y="Residuos_estandarizados")+
  theme_minimal()

#Otra forma de hacerlo más simple
#Residuos vs Ensayos
boxplot(split(residuos,Ensayos), xlab = "Ensayos", ylab = "Residuos")

#Residuos vs Hibridos
boxplot(split(residuos,Hibrido), xlab = "Hibrido", ylab = "Residuos")


####Modelo 2 ####
#Al ser muchos datos y estar desbalanceados, las estimaciones para modelar la heterocedasticidad pueden ser erróneas y ser costosas, por lo que hay que agrupar los valores en conglomerados
###################################
#   ANÁLISIS POR CONGLOMERADOS    #
###################################

#Primero calculamos la varianza del rendimiento por ambiente
library(dplyr)
varianza_por_ambiente <-datos %>%
  group_by(Ensayos) %>%
  summarise(Varianza=var(Rend, na.rm = TRUE))
print(varianza_por_ambiente)

# Calcular las distancias
distancias <- dist(varianza_por_ambiente$Varianza, method = "euclidean")

# Aplicar el método de clustering jerárquico
cluster_jerarquico <- hclust(distancias, method = "ward.D2")

# Cortar el dendrograma para obtener 10 clusters
varianza_por_ambiente$Cluster <- cutree(cluster_jerarquico, k = 10)

# Generar el dendrograma y añadir bordes a los clusters
plot(cluster_jerarquico)
rect.hclust(cluster_jerarquico, k = 10, border = "red")

# Revisar los grupos asignados
print(varianza_por_ambiente)

# Fusionar los data frames por la columna común 'Ensayos'
datos <- merge(datos, varianza_por_ambiente[, c("Ensayos", "Cluster")], by = "Ensayos")

# Revisar el data frame actualizado
head(datos)

##############################################################
#    CORRECCIÓN DE LA HETEROCEDASTICIDAD USANDO VARIDENT     #
##############################################################


# Cargar la librería 'nlme'
library(nlme)
#modelo_varident<-lme(fixed = Rend ~ Hibrido, # Efecto fijo: hibrido
#                     random = ~ 1|Ensayos, # Efecto aleatorio: ensayo
#                    weights = varIdent(form = ~1|Cluster), # Modelar la varianza en función de los clústeres
#                     control=lmeControl(opt="optim", # Parámetros de control para la optimización # Utiliza el algoritmo 'optim' para la optimización
#                                        msMaxIter=150, # Número máximo de iteraciones para la minimización de la suma de cuadrados
#                                        maxIter = 200),data=datos)  # Conjunto de datos utilizado para ajustar el modelo


modelo_varident<-lme(fixed = Rend ~ Hibrido, random = ~ 1|Ensayos, 
                     weights = varIdent(form = ~1|Cluster),
                     control=lmeControl(opt="optim",msMaxIter=150, maxIter = 200),data=datos)
           
# Resumen del modelo
summary(modelo_varident)

#####################################################
#   GRÁFICOS PARA LA EXPLORACIÓN DEL NUEVO MODELO   #
#####################################################

# Residuos vs. valores ajustados (Fitted)
plot(modelo_varident)

#Extraer los residuos del modelo creado
resid_est <- residuals(modelo_varident, type = "pearson")

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

anova_mVarIdent<-anova(modelo_varident)
anova_mVarIdent

#################################################
#    PRUEBA DE COMPARACIÓN MÚLTIPLE DE MEDIAS   #
#################################################

library(emmeans)

emmean_res<- emmeans(modelo_varident,"Hibrido")
emmean_df<- as.data.frame(emmean_res)

# DGC
MSerror <- summary(modelo_varident)$sigma^2

DFerror <- modelo_varident$fixDF$X[2]


# Paso 2: Aplicar el test de DGC
dgc_result <- duncan.test(emmean_df$emmean, trt=emmean_df$Hibrido,
                          MSerror = MSerror, DFerror = DFerror, group = TRUE, console = TRUE)

print(dgc_result$groups)

#################################################
#       COMPARACIÓN GRÁFICA DE LOS MODELOS      #
#################################################

par(mfrow=c(2,2))

qqnorm(residuos_estandarizados)
qqline(residuos_estandarizados, col="red")
qqnorm(resid_est, 
       main = "Q-Q Plot de residuos estandarizados")

qqline(resid_est, 
       col = "red", 
       lwd = 2)

boxplot(split(residuos,Ensayos), xlab = "Ensayos", ylab = "Residuos")
boxplot(split(resid_est,datos$Ensayos), xlab = "Ensayos", ylab = "Residuos")



