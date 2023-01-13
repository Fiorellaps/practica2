
# Cargar las librerías

{r, message=FALSE, echo=FALSE}
library(stringr) # Transformar strings
library(lubridate) # Tratar fechas
library(TH.data) # Cargar datos
library(dplyr)
library(MASS) # Regresión logística



# Exploración de los datos



####### EXPLORACIÓN DE LOS DATOS ######

#data("GBSG2", package = "TH.data")
GBSG2 <- read.csv("GBSG2_original.csv")
# https://www.rdocumentation.org/packages/TH.data/versions/1.1-1/topics/GBSG2

# Resumen del dataframe
summary(GBSG2)

# Imprimir los primeros registros del conjunto
head(GBSG2)

# Inspeccionar el tipo de los campos
sapply(GBSG2, function(x) class(x))


# Limpieza



####### LIMPIEZA DE LAS COLUMAS Y REGISSTROS DUPLICADOS ######

colnames(GBSG2)

# Renombrar las columnas por un nombre más relevante
colnames(GBSG2) <- c("hormonal_therapy", "age", "menopausal_status", "tumor_size", "tumor_grade", "positive_nodes", "progesterone_receptor", "estrogen_receptor", "survival_time", "censoring")

colnames(GBSG2)

# Eliminar filas duplicadas
dim(GBSG2)
GBSG2 <- unique(GBSG2)
dim(unique(GBSG2))


# Análisis univariante

## Age


####### ANÁLISIS UNIVARIANTE ######

### Age

sort(unique(GBSG2$age))

hist(GBSG2$age,
     main="Histograma de la edad",
     xlab="Edad",
     #xlim=c(50,100),
     #col=c("blue", "red", "gray", "green", "darkmagenta")
     col="darkmagenta"
)

boxplot(GBSG2$age)



# Imprimir los outliers
boxplot.stats(GBSG2$age)$out


## Hormonal therapy



### hormonal_therapy

# Eliminar filas con valores vacíos o NA
GBSG2 <- GBSG2[GBSG2$hormonal_therapy != "" | ! is.na(GBSG2$hormonal_therapy),]

unique(GBSG2$hormonal_therapy)

# Convertir a factor
GBSG2$hormonal_therapy <- factor(GBSG2$hormonal_therapy, levels = c("no", "yes"), labels = c("NO", "YES"))

par(mfrow=c(1, 2))

barplot(prop.table(table(GBSG2$hormonal_therapy)),
        main="Histograma",
        xlab="Terapia hormonal",
        col=c("blue", "red"))

pie(prop.table(table(GBSG2$hormonal_therapy)),
        main="Diagrama circular",
        xlab="Terapia hormonal",
        col=c("blue", "red"))


## Menopausal status



### menopausal_status

unique(GBSG2$menopausal_status)

# Convertir a factor
GBSG2$menopausal_status <- factor(GBSG2$menopausal_status, levels = c("Post", "Pre"), labels = c("POST", "PRE"))

par(mfrow=c(1, 2))

barplot(prop.table(table(GBSG2$menopausal_status)),
        main="Histograma",
        xlab="Estado de menopausia",
        col=c("blue", "red"))

pie(prop.table(table(GBSG2$menopausal_status)),
        main="Diagrama circular",
        xlab="Estado de menopausia",
        col=c("blue", "red"))



## Tumor size



### tumor_size

# Resumen de la variable numérica
summary(GBSG2$tumor_size)

par(mfrow=c(1, 3))

plot(GBSG2$tumor_size, 
     main="Gráfico del tamaño del tumor",
     xlab="Muetra",
     col="blue")

boxplot(GBSG2$tumor_size,
        main="Boxplot del tamaño del tumor",
        xlab="Muestra",
        col="magenta")

hist(GBSG2$tumor_size, 
     main="Histograma del tamaño del tumor",
     xlab="Tamaño del tumor",
     col="yellow")




boxplot(GBSG2$tumor_size,
        main="Boxplot del tamaño del tumor",
        xlab="Muestra",
        col="green")



# imprimir los outliers
sort(boxplot.stats(GBSG2$tumor_size)$out)


## Tumor grade



### tumor_grade 

# Eliminar filas con valores vacíos o NA
GBSG2 <- GBSG2[GBSG2$tumor_grade != "" | ! is.na(GBSG2$tumor_grade),]

unique(GBSG2$tumor_grade)

par(mfrow=c(1, 2))

barplot(prop.table(table(GBSG2$tumor_grade)),
        main="Histograma del grado del tumor",
        xlab="Grado del tumor",
        col=c("blue", "lightblue", "darkblue"))

pie(prop.table(table(GBSG2$tumor_grade)),
        main="Histograma del grado del tumor",
        xlab="Grado del tumor",
        col=c("blue", "lightblue", "darkblue"))


## Tumor size



### tumor_size 
summary(GBSG2$positive_nodes)

par(mfrow=c(1, 3))

plot(GBSG2$positive_nodes, 
     main="Gráfica del número de nodos positivos",
     xlab="Muestra",
     col="blue")

boxplot(GBSG2$positive_nodes,
        main="Boxplot del número de nodos positivos",
        xlab="Muestra",
        col="green")

hist(GBSG2$positive_nodes, 
     main="Histograma del número de nodos positivos",
     xlab="Nodos positivos",
     col="yellow")



boxplot(GBSG2$positive_nodes,
        main="Boxplot del número de nodos positivos",
        xlab="Muestra",
        col="green")



# imprimir los outliers
sort(boxplot.stats(GBSG2$positive_nodes)$out)


## Progesterone receptor



### progesterone_receptor

summary(GBSG2$progesterone_receptor)

par(mfrow=c(1, 3))

plot(GBSG2$progesterone_receptor, 
     main="Gráfica del número de receptores de progesterona",
     xlab="Muestra",
     col="blue")

boxplot(GBSG2$progesterone_receptor,
        main="Boxplot del número de receptores de progesterona",
        xlab="Muestra",
        col="green")

hist(GBSG2$progesterone_receptor, 
     main="Histogram del número de receptores de progesterona",
     xlab="Receptores de progesterona",
     col="yellow")





boxplot(GBSG2$progesterone_receptor,
        main="Boxplot del número de receptores de progesterona",
        xlab="Muestra",
        col="green")



# imprimir los outliers
sort(boxplot.stats(GBSG2$progesterone_receptor)$out)


## Estrogen receptor


### estrogen_receptor  

summary(GBSG2$estrogen_receptor )

par(mfrow=c(2, 2))

plot(GBSG2$estrogen_receptor , 
     main="Gráfica del número de receptores de estrógenos",
     xlab="Muestra",
     col="blue")

boxplot(GBSG2$estrogen_receptor ,
        main="Boxplot del número de receptores de estrógenos",
        xlab="Muestra",
        col="green")

hist(GBSG2$estrogen_receptor , 
     main="Histograma del número de receptores de estrógenos",
     xlab="Receptores de estrógenos",
     col="yellow")

barplot(GBSG2$estrogen_receptor , 
     main="Diagrama de barras del número de receptores de estrógenos",
     xlab="Receptores de estrógenos",
     col="red")




boxplot(GBSG2$estrogen_receptor ,
        main="Boxplot del número de receptores de estrógenos",
        xlab="Muestra",
        col="green")



# imprimir los outliers
sort(boxplot.stats(GBSG2$estrogen_receptor)$out)


## Censoting



### censoring

# Eliminar filas con valores vacíos o NA
GBSG2 <- GBSG2[GBSG2$censoring != "" | ! is.na(GBSG2$censoring),]

unique(GBSG2$censoring)

# Convertir a factor
GBSG2$censoring <- factor(GBSG2$censoring, levels = c(0, 1), labels = c("ALIVE", "DEAD"))

par(mfrow=c(1, 2))

barplot(prop.table(table(GBSG2$censoring)),
        main="Histograma de la variable 'censoring'",
        xlab="Censoring",
        col=c("blue", "red"))


percentages <- round(table(GBSG2$censoring) / length(GBSG2$censoring) * 100)	# calcular porcentajes
labels <- paste(names(table(GBSG2$censoring)), percentages, "%")	# Añadir los porcentajes a las etiquetas
pie(table(GBSG2$censoring),
    col=c("steelblue", "red"),
    labels=labels,
    density = 20,
    angle = 30*1:2)


## Survival time



## survival_time

hist(GBSG2$survival_time,
     main="Histogram del tiempo de supervivencia en días",
     xlab="Días de supervivencia",
     ylab="Frecuencia",
     col=heat.colors(15))

abline(v = median(GBSG2$survival_time), 
       col = "blue", lwd = 2)



# Calcular la mediana y media de días de supervivencia
paste("Mediana de superviviencia:", median(GBSG2$survival_time))
paste("Media de superviviencia:", mean(GBSG2$survival_time))


# Generar dataset transformado


write.csv(GBSG2,"GBSG2_transformed.csv",row.names = F)



# Generación de grupos


## Agrupación por edad

GBSG2_menor40 <- GBSG2 %>% dplyr::filter(age <= 40)

GBSG2_mayor40_menor65 <- GBSG2 %>% dplyr::filter(age > 40 & age < 65)

GBSG2_mayor65 <- GBSG2 %>% dplyr::filter(age >= 65)




## Agrupación por grado de tumor

GBSG2_gradoI <- GBSG2[GBSG2$tumor_grade == "I",]

GBSG2_gradoII <- GBSG2[GBSG2$tumor_grade == "II",]

GBSG2_gradoIII <- GBSG2[GBSG2$tumor_grade == "III",]




## Agrupación por grado de tumor

GBSG2_menopausal_post <- GBSG2 %>% dplyr::filter(menopausal_status == "POST")

GBSG2_menopausal_pre <- GBSG2 %>% dplyr::filter(menopausal_status == "PRE")




## Agrupación por tamaño de tumor

GBSG2_tumor_size_menor10 <- GBSG2[GBSG2$tumor_size <= 10,]

GBSG2_tumor_size_mayor10_menor40 <- GBSG2[GBSG2$tumor_size > 10 & GBSG2$tumor_size <= 40,]

GBSG2_tumor_size_mayor40 <- GBSG2[GBSG2$tumor_size > 40,]



# Análsis bivariante


## Wilcox test


# Comprobar dependencia, normalidad y homocedasticidad de las variables: age y censoring

# Comprobar si la variable age sigue una distribución normal
shapiro.test(GBSG2$age)

if(shapiro.test(GBSG2$age)$p.value > 0.05){
  print("La variable age sigue una distribución normal según el test de Sahpiro.")
} else {
  print("La variable age no sigue una distribución normal según el test de Sahpiro.")
}

ks.test(GBSG2$age, pnorm, mean(GBSG2$age), sd(GBSG2$age))


if(ks.test(GBSG2$age, pnorm, mean(GBSG2$age), sd(GBSG2$age))$p.value > 0.05){
  print("La variable age sigue una distribución normal según el test de Kolmogorov-Smirnov.")
} else {
  print("La variable age no sigue una distribución normal según el test de Kolmogorov-Smirnov.")
}

# Dado que la prueba de Shapiro-Wilk se considera más robusta, una posición más conservadora concluiría que los datos no siguen una distribución normal

# Comprobar homocedasticidad con test de Fligner-Killeen, alternativa no paramétrica

fligner.test(age ~ censoring, data = GBSG2)

if(fligner.test(age ~ censoring, data = GBSG2)$p.value > 0.05){
  print("Las variables cumplen el criterio de homocedasticidad según el test de Fligner-Killeen.")
} else {
  print("Las variables no cumplen el criterio de homocedasticidad según el test de Fligner-Killeen.")
}

# Al ser variable numérica y categórica, y no se cumple la normalidad, hay que aplicar la prueba no paramétrica como Wilconxon

wilcox.test(age ~ censoring, data = GBSG2)

if(wilcox.test(age ~ censoring, data = GBSG2)$p.value < 0.05){
  print("Existe correlación entre las variables según el test Wilcoxon.")
} else {
   print("No existe correlación entre las variables según el test Wilcoxon.")
}

boxplot(age ~ censoring, data = GBSG2)




# Comprobar dependencia, normalidad y homocedasticidad de las variables: tumor_size y censoring

# Comprobar si las variables siguen una distribución normal
shapiro.test(GBSG2$tumor_size)

if(shapiro.test(GBSG2$tumor_size)$p.value > 0.05){
  print("La variable age sigue una distribución normal según el test de Sahpiro.")
} else {
  print("La variable age no sigue una distribución normal según el test de Sahpiro.")
}

ks.test(GBSG2$tumor_size, pnorm, mean(GBSG2$tumor_size), sd(GBSG2$tumor_size))


if(ks.test(GBSG2$tumor_size, pnorm, mean(GBSG2$tumor_size), sd(GBSG2$tumor_size))$p.value > 0.05){
  print("La variable age sigue una distribución normal según el test de Kolmogorov-Smirnov.")
} else {
  print("La variable age no sigue una distribución normal según el test de Kolmogorov-Smirnov.")
}

# Dado que la prueba de Shapiro-Wilk se considera más robusta, una posición más conservadora concluiría que los datos no siguen una distribución normal

# Comprobar homocedasticidad con test de Fligner-Killeen, alternativa no paramétrica

fligner.test(tumor_size ~ censoring, data = GBSG2)

if(fligner.test(tumor_size ~ censoring, data = GBSG2)$p.value > 0.05){
  print("Las variables cumplen el criterio de homocedasticidad según el test de Fligner-Killeen.")
} else {
  print("Las variables no cumplen el criterio de homocedasticidad según el test de Fligner-Killeen.")
}

# Al ser variable numérica y categórica, y no se cumple la normalidad, hay que aplicar la prueba no paramétrica como Wilconxon

wilcox.test(tumor_size ~ censoring, data = GBSG2)

if(wilcox.test(tumor_size ~ censoring, data = GBSG2)$p.value < 0.05){
  print("Existe correlación entre las variables según el test Wilcoxon.")
} else {
   print("No existe correlación entre las variables según el test Wilcoxon.")
}

boxplot(tumor_size ~ censoring, data = GBSG2)



## Chi squared test



# Test hormonal_therapy y patient_status

# Al ser las dos variables categóricas donde el estado del tumor tiene más de dos grupos,  usamos el test de chi cuadrado

tabla_relacion <- xtabs(~ censoring + hormonal_therapy , data = GBSG2)

chisq.test(tabla_relacion)

if(chisq.test(tabla_relacion)$p.value < 0.05){
  print("Existe correlación entre las variables según el test Chi sqared.")
} else {
   print("No existe correlación entre las variables según el test Chi sqared.")
}

barplot(tabla_relacion, legend = TRUE)


# Existe correlación entre variables categóricas






# Comprobar dependencia de las variables categóricas: tumor_stage y censoring

# Al ser las dos variables categóricas donde el estado del tumor tiene más de dos grupos,  usamos el test de chi cuadrado

tabla_relacion <- xtabs(~ censoring + tumor_grade , data = GBSG2)

chisq.test(tabla_relacion)

if(chisq.test(tabla_relacion)$p.value < 0.05){
  print("Existe correlación entre las variables según el test Chi sqared.")
} else {
   print("No existe correlación entre las variables según el test Chi sqared.")
}

barplot(tabla_relacion, legend = TRUE)



## Correlación de Spearman



# Comprobar dependencia, normalidad y homocedasticidad de las variables numéricas: progesterone_receptor y estrogen_receptor

# Comprobar si las variables numéricas siguen una distribución normal
shapiro.test(GBSG2$progesterone_receptor)

if(shapiro.test(GBSG2$progesterone_receptor)$p.value > 0.05){
  print("La variable progesterone_receptor sigue una distribución normal según el test de Sahpiro.")
} else {
  print("La variable progesterone_receptor no sigue una distribución normal según el test de Sahpiro.")
}

shapiro.test(GBSG2$estrogen_receptor)

if(shapiro.test(GBSG2$estrogen_receptor)$p.value > 0.05){
  print("La variable estrogen_receptor sigue una distribución normal según el test de Sahpiro.")
} else {
  print("La variable estrogen_receptor no sigue una distribución normal según el test de Sahpiro.")
}

# Dado que la prueba de Shapiro-Wilk se considera más robusta, una posición más conservadora concluiría que los datos no siguen una distribución normal

# Comprobar homocedasticidad con test de Fligner-Killeen, alternativa no paramétrica

fligner.test(estrogen_receptor ~ progesterone_receptor, data = GBSG2)

if(fligner.test(estrogen_receptor ~ progesterone_receptor, data = GBSG2)$p.value > 0.05){
  print("Las variables cumplen el criterio de homocedasticidad según el test de Fligner-Killeen.")
} else {
  print("Las variables no cumplen el criterio de homocedasticidad según el test de Fligner-Killeen.")
}

cor.test(GBSG2$estrogen_receptor, GBSG2$progesterone_receptor, method="spearman")

if(cor.test(GBSG2$estrogen_receptor, GBSG2$progesterone_receptor, method="spearman")$p.value < 0.05){
  print("Existe correlación entre las variables según el test Chi sqared.")
} else {
   print("No existe correlación entre las variables según el test Chi sqared.")
}

plot(GBSG2$estrogen_receptor, GBSG2$progesterone_receptor, col = c(2, 3))

legend(x = "topright",          
      legend = c("estrogen_receptor", "progesterone_receptor"),  
      lty = c(1, 2),           
      col = c(2, 3),        
      lwd = 2)  


## Regresión logística


# Regresión logística

modelo_glm <- glm(as.numeric(GBSG2$censoring == "ALIVE") ~ tumor_grade, data = GBSG2, family = binomial("logit"))

calidad <- stepAIC(modelo_glm, direction = "backward")

summary(modelo_glm)



## Regresión lineal



# Regresión lineal

modelo_lm <- lm(as.numeric(GBSG2$censoring == "ALIVE") ~ tumor_size + tumor_grade + hormonal_therapy, data = GBSG2)


summary(modelo_lm)


