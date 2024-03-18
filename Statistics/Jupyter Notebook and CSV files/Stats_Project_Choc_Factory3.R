library(plm)
library(lmtest)

# Carregando o novo dataset
chocolate_data <- read.csv('C:/Users/Pedro/OneDrive/Área de Trabalho/NOVA IMS/First semester/Statistics/Project/Proj. Data Factory/Proj. Data Factory 3/test_tres_Chocolate_Factory_Panel_Data.csv')
chocolate_data

# Convertendo 'Year' para formato de data (se necessário) e ajustando os tipos de variáveis
chocolate_data$Year <- as.Date(as.character(chocolate_data$Year), format = "%Y")
chocolate_data$EnergyConsumption <- as.numeric(chocolate_data$EnergyConsumption)
chocolate_data$OompaLoompaSatisfaction <- as.numeric(chocolate_data$OompaLoompaSatisfaction)

# Modelo de Efeitos Aleatórios
re_chocolate <- plm(ChocolateProductionInKg ~ NumberOfOompaLoompas + RawMaterialConsumptionInKg + OperationalCostInDollars + EnergyConsumption + OompaLoompaSatisfaction, 
                    model = 'random', 
                    index = c('RoomID', 'Year'), 
                    data = chocolate_data)

# Resumo do Modelo de Efeitos Aleatórios
summary(re_chocolate)

# Modelo de Efeitos Fixos
fe_chocolate <- plm(ChocolateProductionInKg ~ NumberOfOompaLoompas + RawMaterialConsumptionInKg + OperationalCostInDollars + EnergyConsumption + OompaLoompaSatisfaction,
                    data = chocolate_data,
                    model = "within", 
                    index = c("RoomID", "Year"))

# Resumo do Modelo de Efeitos Fixos
summary(fe_chocolate)

# Teste Hausman
phtest(fe_chocolate, re_chocolate)



# Pooled OLS Model
pooled_chocolate <- plm(ChocolateProductionInKg ~ NumberOfOompaLoompas + RawMaterialConsumptionInKg + OperationalCostInDollars + EnergyConsumption + OompaLoompaSatisfaction, 
                        data = chocolate_data, 
                        model = "pooling")

# Summary of Pooled OLS Model
summary(pooled_chocolate)

# Run the Breusch-Pagan test for heteroskedasticity on the fixed effects model
bp_test_fe <- bptest(fe_chocolate)
bp_test_fe

# Run the Breusch-Pagan test for heteroskedasticity on the random effects model
bp_test_re <- bptest(re_chocolate)
bp_test_re

# Run the Breusch-Pagan test for heteroskedasticity on the Pooled OLS model
bp_test_re <- bptest(pooled_chocolate)
bp_test_re


#WHITE TEST!!!!
# White test on the Random Effects model
white_test_re <- bptest(re_chocolate, ~ fitted(re_chocolate) + I(fitted(re_chocolate)^2), data = chocolate_data)
print("White test for Random Effects Model:")
print(white_test_re)

# White test on the Fixed Effects model
white_test_fe <- bptest(fe_chocolate, ~ fitted(fe_chocolate) + I(fitted(fe_chocolate)^2), data = chocolate_data)
print("White test for Fixed Effects Model:")
print(white_test_fe)

# White test on the Pooled OLS model
white_test_pooled <- bptest(pooled_chocolate, ~ fitted(pooled_chocolate) + I(fitted(pooled_chocolate)^2), data = chocolate_data)
print("White test for Pooled OLS Model:")
print(white_test_pooled)



