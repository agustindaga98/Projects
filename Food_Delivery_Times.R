setwd("/Users/agustindaga/Desktop")
datos <- read.csv("Food_Delivery_Times.csv")

attach(datos)
datos

# ALUMNOS: AGUSTIN DAGA y DANA HANSMAN

# Ejercicio a) 

# Eliminamos todos los casos para los que las variables Weather, Time_of_day y Traffic_Level tienen datos perdidos.

datos_filtrados <- datos[datos$Weather != "" & datos$Time_of_Day != "" & datos$Traffic_Level != "", ]

print(datos_filtrados)

# Dado que se obsevan N/A en la columna Courier_Experience_yrs, se eliminan para comparar todos los modelos con el mismo n

datos_filtrados <- datos_filtrados[!is.na(datos_filtrados$Courier_Experience_yrs), ]
datos_filtrados

datos_especificos <- datos_filtrados[, c("Distance_km", "Preparation_Time_min", "Courier_Experience_yrs", "Delivery_Time_min")]

# Creamos tabla con los estadísticos descriptivos
resumen <- data.frame(
  Mínimo = sapply(datos_especificos, min, na.rm = TRUE),
  Media = round(sapply(datos_especificos, mean, na.rm = TRUE),2),
  Mediana = round(sapply(datos_especificos, median, na.rm = TRUE),2),
  Máximo = sapply(datos_especificos, max, na.rm = TRUE),
  Desvío_Estándar = round(sapply(datos_especificos, sd, na.rm = TRUE),2),
  N = sapply(datos_especificos, function(x) sum(!is.na(x)))
)

# Ver la tabla
print(resumen)

# Ejercicio b) 

datos_filtrados$Clear<- ifelse(datos_filtrados$Weather=='Clear', 1, 0)
datos_filtrados$Foggy<- ifelse(datos_filtrados$Weather=='Foggy', 1, 0)
datos_filtrados$Rainy<- ifelse(datos_filtrados$Weather=='Rainy', 1, 0)
datos_filtrados$Snowy<- ifelse(datos_filtrados$Weather=='Snowy', 1, 0)
datos_filtrados$Windy<- ifelse(datos_filtrados$Weather=='Windy', 1, 0)

datos_filtrados$High<- ifelse(datos_filtrados$Traffic_Level=='High', 1, 0)
datos_filtrados$Low<- ifelse(datos_filtrados$Traffic_Level=='Low', 1, 0)
datos_filtrados$Medium<- ifelse(datos_filtrados$Traffic_Level=='Medium', 1, 0)

datos_filtrados$Afternoon<- ifelse(datos_filtrados$Time_of_Day=='Afternoon', 1, 0)
datos_filtrados$Evening<- ifelse(datos_filtrados$Time_of_Day=='Evening', 1, 0)
datos_filtrados$Morning<- ifelse(datos_filtrados$Time_of_Day=='Morning', 1, 0)
datos_filtrados$Night<- ifelse(datos_filtrados$Time_of_Day=='Night', 1, 0)

datos_filtrados$Bike<- ifelse(datos_filtrados$Vehicle_Type=='Bike', 1, 0)
datos_filtrados$Car<- ifelse(datos_filtrados$Vehicle_Type=='Car', 1, 0)
datos_filtrados$Scooter<- ifelse(datos_filtrados$Vehicle_Type=='Scooter', 1, 0)

datos_filtrados

# Ejercicio c) 

# Si incluyeramos todas las dummies, R omite automáticamente "Windy" por ser la última

reg <- lm(Delivery_Time_min~Clear+Foggy+Rainy+Snowy+Windy+Distance_km, data = datos_filtrados)
summary(reg)

# Omitimos "Clear" para realizar comparaciones climáticas vs condiciones normales

reg1 <- lm(Delivery_Time_min~Foggy+Rainy+Snowy+Windy+Distance_km, data = datos_filtrados)
summary(reg1)

# Ejercicio d) 

reg2 <- lm(Delivery_Time_min~Foggy+Rainy+Snowy+Windy+Distance_km+Preparation_Time_min, data = datos_filtrados)
summary(reg2)

# Ejercicio e)

# Se justifica en el informe.

# Ejercicio f) 

# Omitimos la dummy de "Low" para después validar la hipótesis de tiempo de tráfico alto vs medio

reg3 <- lm(Delivery_Time_min~Foggy+Rainy+Snowy+Windy+Distance_km+Preparation_Time_min+High+Medium, data = datos_filtrados)
summary(reg3)

library(car)
linearHypothesis(reg3, c("High=2*Medium"))


# Ejercicio g) 

datos_filtrados$Novato<- ifelse(datos_filtrados$Courier_Experience_yrs=='0', 1, 0)

# Convertimos a factor para graficar con color
datos_filtrados$Experiencia <- factor(datos_filtrados$Novato, labels = c("No Novato", "Novato"))

# Gráfico de dispersión con regresión diferenciada por novato
library(ggplot2)
ggplot(datos_filtrados, aes(x = Distance_km, y = Delivery_Time_min, color = Experiencia)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Tiempo de Entrega vs Distancia según Experiencia",
       x = "Distancia (km)",
       y = "Tiempo de entrega (min)",
       color = "Experiencia") +
  theme_minimal()

reg4 <- lm(Delivery_Time_min~Novato+Distance_km+Novato*Distance_km, data = datos_filtrados)
summary(reg4)

# Ejercicio h)

reg2.1 <- lm(log(Delivery_Time_min)~Foggy+Rainy+Snowy+Windy+Distance_km+Preparation_Time_min, data = datos_filtrados)
summary(reg2.1)

# Ejercicio i)

reg2.2 <- lm(log(Delivery_Time_min)~Foggy+Rainy+Snowy+Windy+Distance_km+I(Distance_km^2)+Preparation_Time_min, data = datos_filtrados)
summary(reg2.2)

# Ejercicio j)

# install.packages("stargazer")

library(stargazer)

# Creamos tabla con todos los modelos
stargazer(reg, reg1, reg2, reg3, reg4, reg2.1, reg2.2,
          type = "text",      
          title = "Resultados de regresiones lineales",
          column.labels = c("Reg", "Reg1", "Reg2", "Reg3", "Reg4", "Reg2.1", "Reg2.2"),
          dep.var.labels = c("Delivery Time", "log(Delivery Time)",
                             model.numbers = FALSE))

# Ejercicio k)

# El modelo con mejor ajuste es Reg3 (R2 ajustado = 0.779) y el de peor ajuste es Reg4 (R2 ajustado = 0.614)

# Predicción
pred_mejor <- predict(reg3)
pred_peor <- predict(reg4)

# Errores de predicción
error_mejor <- datos_filtrados$Delivery_Time_min - pred_mejor
error_peor <- datos_filtrados$Delivery_Time_min - pred_peor

summary(error_mejor)
summary(error_peor)

rmse_mejor <- sqrt(mean(error_mejor^2))
rmse_peor <- sqrt(mean(error_peor^2))

mape_mejor <- mean(abs(error_mejor / datos_filtrados$Delivery_Time_min)) * 100
mape_peor <- mean(abs(error_peor / datos_filtrados$Delivery_Time_min)) * 100

# Ejercicio L 

# Modelo de mejor ajuste:
reg3 <- lm(Delivery_Time_min ~ Foggy + Rainy + Snowy + Windy + Distance_km +
             Preparation_Time_min + High + Medium, data = datos_filtrados)

# Guardamos el coeficiente de la distancia del modelo de mejor ajuste
coef_mejor <- coef(reg3)["Distance_km"]

coef_simulados <- c()

# Repetimos el proceso 10000 veces
set.seed(123)  
for (i in 1:10000) {
  datos_simul <- datos_filtrados
  
  datos_simul$Distance_km <- sample(datos_filtrados$Distance_km)
  
  modelo_simulado <- lm(Delivery_Time_min ~ Foggy + Rainy + Snowy + Windy + Distance_km +
                          Preparation_Time_min + High + Medium, data = datos_simul)
  
  coef_simulados[i] <- coef(modelo_simulado)["Distance_km"]
}

# Calculamos el p-valor empírico: cuántos coeficientes simulados son tan extremos como el real

p_empirico <- mean(abs(coef_simulados) >= abs(coef_mejor))
print(p_empirico)


