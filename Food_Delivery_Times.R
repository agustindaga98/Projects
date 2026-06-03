setwd("/Users/__/Desktop")
datos <- read.csv("Food_Delivery_Times.csv")

attach(datos)
datos

# Exercise a)

# Remove all cases where Weather, Time_of_day and Traffic_Level have missing values
datos_filtrados <- datos[datos$Weather != "" & datos$Time_of_Day != "" & datos$Traffic_Level != "", ]

print(datos_filtrados)

# N/A values found in Courier_Experience_yrs are removed to compare all models with the same n
datos_filtrados <- datos_filtrados[!is.na(datos_filtrados$Courier_Experience_yrs), ]
datos_filtrados

datos_especificos <- datos_filtrados[, c("Distance_km", "Preparation_Time_min", "Courier_Experience_yrs", "Delivery_Time_min")]

# Descriptive statistics table
resumen <- data.frame(
  Minimum = sapply(datos_especificos, min, na.rm = TRUE),
  Mean = round(sapply(datos_especificos, mean, na.rm = TRUE),2),
  Median = round(sapply(datos_especificos, median, na.rm = TRUE),2),
  Maximum = sapply(datos_especificos, max, na.rm = TRUE),
  Std_Deviation = round(sapply(datos_especificos, sd, na.rm = TRUE),2),
  N = sapply(datos_especificos, function(x) sum(!is.na(x)))
)

print(resumen)

# Exercise b)

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

# Exercise c)

# Including all dummies, R automatically omits "Windy" as it is the last one
reg <- lm(Delivery_Time_min~Clear+Foggy+Rainy+Snowy+Windy+Distance_km, data = datos_filtrados)
summary(reg)

# Omitting "Clear" to compare weather conditions against normal conditions
reg1 <- lm(Delivery_Time_min~Foggy+Rainy+Snowy+Windy+Distance_km, data = datos_filtrados)
summary(reg1)

# Exercise d)

reg2 <- lm(Delivery_Time_min~Foggy+Rainy+Snowy+Windy+Distance_km+Preparation_Time_min, data = datos_filtrados)
summary(reg2)

# Exercise e)

# Justification provided in the report.

# Exercise f)

# Omitting "Low" dummy to test the hypothesis of high vs medium traffic time
reg3 <- lm(Delivery_Time_min~Foggy+Rainy+Snowy+Windy+Distance_km+Preparation_Time_min+High+Medium, data = datos_filtrados)
summary(reg3)

library(car)
linearHypothesis(reg3, c("High=2*Medium"))

# Exercise g)

datos_filtrados$Novice <- ifelse(datos_filtrados$Courier_Experience_yrs=='0', 1, 0)

# Convert to factor for color plotting
datos_filtrados$Experience <- factor(datos_filtrados$Novice, labels = c("Experienced", "Novice"))

# Scatter plot with differentiated regression lines by experience
library(ggplot2)
ggplot(datos_filtrados, aes(x = Distance_km, y = Delivery_Time_min, color = Experience)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Delivery Time vs Distance by Courier Experience",
       x = "Distance (km)",
       y = "Delivery Time (min)",
       color = "Experience") +
  theme_minimal()

reg4 <- lm(Delivery_Time_min~Novice+Distance_km+Novice*Distance_km, data = datos_filtrados)
summary(reg4)

# Exercise h)

reg2.1 <- lm(log(Delivery_Time_min)~Foggy+Rainy+Snowy+Windy+Distance_km+Preparation_Time_min, data = datos_filtrados)
summary(reg2.1)

# Exercise i)

reg2.2 <- lm(log(Delivery_Time_min)~Foggy+Rainy+Snowy+Windy+Distance_km+I(Distance_km^2)+Preparation_Time_min, data = datos_filtrados)
summary(reg2.2)

# Exercise j)

# install.packages("stargazer")

library(stargazer)

# Table with all models
stargazer(reg, reg1, reg2, reg3, reg4, reg2.1, reg2.2,
          type = "text",      
          title = "Linear Regression Results",
          column.labels = c("Reg", "Reg1", "Reg2", "Reg3", "Reg4", "Reg2.1", "Reg2.2"),
          dep.var.labels = c("Delivery Time", "log(Delivery Time)",
                             model.numbers = FALSE))

# Exercise k)

# Best fitting model: Reg3 (Adjusted R2 = 0.779), worst fitting model: Reg4 (Adjusted R2 = 0.614)

# Predictions
pred_mejor <- predict(reg3)
pred_peor <- predict(reg4)

# Prediction errors
error_mejor <- datos_filtrados$Delivery_Time_min - pred_mejor
error_peor <- datos_filtrados$Delivery_Time_min - pred_peor

summary(error_mejor)
summary(error_peor)

rmse_mejor <- sqrt(mean(error_mejor^2))
rmse_peor <- sqrt(mean(error_peor^2))

mape_mejor <- mean(abs(error_mejor / datos_filtrados$Delivery_Time_min)) * 100
mape_peor <- mean(abs(error_peor / datos_filtrados$Delivery_Time_min)) * 100

# Exercise l)

# Best fitting model
reg3 <- lm(Delivery_Time_min ~ Foggy + Rainy + Snowy + Windy + Distance_km +
             Preparation_Time_min + High + Medium, data = datos_filtrados)

# Store distance coefficient from best fitting model
coef_mejor <- coef(reg3)["Distance_km"]

coef_simulados <- c()

# Repeat process 10000 times
set.seed(123)  
for (i in 1:10000) {
  datos_simul <- datos_filtrados
  
  datos_simul$Distance_km <- sample(datos_filtrados$Distance_km)
  
  modelo_simulado <- lm(Delivery_Time_min ~ Foggy + Rainy + Snowy + Windy + Distance_km +
                          Preparation_Time_min + High + Medium, data = datos_simul)
  
  coef_simulados[i] <- coef(modelo_simulado)["Distance_km"]
}

# Empirical p-value: proportion of simulated coefficients as extreme as the observed one
p_empirico <- mean(abs(coef_simulados) >= abs(coef_mejor))
print(p_empirico)
