#install.packages("dplyr")
#install.packages("Hmisc")
#install.packages("lubridate")

library(dplyr)
library(Hmisc)
library(lubridate)

# Carregando o dataset
load("~/mobility.RData")





# Criando um novo dataset somente com as colunas desejadas
dados <- data %>% select(DATE, VELOCITY, DISTANCE, HEIGHT, VSPMode, CO_2, CO, HC, NO_x)
dados <- na.omit(dados) # Removendo valores nulos 




# Discretização de VELOCITY
dados <- dados[dados$VELOCITY != 0, ] # Removendo casos de VELOCITY == 0 (quando o ônibus está parado)

# Tratamento de Outliers
Q1 <- quantile(dados$VELOCITY, 0.25)
Q3 <- quantile(dados$VELOCITY, 0.75)
IQR_value <- Q3 - Q1
limite_inferior <- Q1 - 1.5 * IQR_value
limite_superior <- Q3 + 1.5 * IQR_value
outliers_indices <- which(dados$VELOCITY < limite_inferior | dados$VELOCITY > limite_superior)
dados <- dados[-outliers_indices, ]

dados <- dados %>% # Normalizando VELOCITY utilizando a Normalização Min Max
  mutate(VELOCITY = scale(VELOCITY, center = min(VELOCITY), scale = max(VELOCITY) - min(VELOCITY))) %>% 
  mutate(VELOCITY = round(VELOCITY, 1)) # Arredondando para uma casa decimal




# Removendo casos de DISTANCE == 0
dados <- dados[dados$DISTANCE != 0, ]

# Tratamento de Outliers para DISTANCE
Q1_DISTANCE <- quantile(dados$DISTANCE, 0.25)
Q3_DISTANCE <- quantile(dados$DISTANCE, 0.75)
IQR_VALUE_DISTANCE <- Q3_DISTANCE - Q1_DISTANCE
limite_inferior_DISTANCE <- Q1_DISTANCE - 1.5 * IQR_VALUE_DISTANCE
limite_superior_DISTANCE <- Q3_DISTANCE + 1.5 * IQR_VALUE_DISTANCE
outliers_indices_DISTANCE <- which(dados$DISTANCE < limite_inferior_DISTANCE | dados$DISTANCE > limite_superior_DISTANCE)
dados <- dados[-outliers_indices_DISTANCE, ]

# Normalizando DISTANCE utilizando a Normalização Min Max
dados <- dados %>% 
  mutate(DISTANCE = scale(DISTANCE, center = min(DISTANCE), scale = max(DISTANCE) - min(DISTANCE))) %>% 
  mutate(DISTANCE = round(DISTANCE, 1)) # Arredondando para uma casa decimal




# Tratamento de Outliers para HEIGHT
Q1_HEIGHT <- quantile(dados$HEIGHT, 0.25)
Q3_HEIGHT <- quantile(dados$HEIGHT, 0.75)
IQR_VALUE_HEIGHT <- Q3_HEIGHT - Q1_HEIGHT
limite_inferior_HEIGHT <- Q1_HEIGHT - 1.5 * IQR_VALUE_HEIGHT
limite_superior_HEIGHT <- Q3_HEIGHT + 1.5 * IQR_VALUE_HEIGHT
outliers_indices_HEIGHT <- which(dados$HEIGHT < limite_inferior_HEIGHT | dados$HEIGHT > limite_superior_HEIGHT)
dados <- dados[-outliers_indices_HEIGHT, ]

# Normalizando HEIGHT utilizando a Normalização Min Max
dados <- dados %>% 
  mutate(HEIGHT = scale(HEIGHT, center = min(HEIGHT), scale = max(HEIGHT) - min(HEIGHT))) %>% 
  mutate(HEIGHT = round(HEIGHT, 1)) # Arredondando para uma casa decimal





# Normalizando CO_2 utilizando a Normalização Min Max
dados <- dados %>% 
  mutate(CO_2 = scale(CO_2, center = min(CO_2), scale = max(CO_2) - min(CO_2))) %>% 
  mutate(CO_2 = round(CO_2, 1)) # Arredondando para uma casa decimal





# Normalizando CO utilizando a Normalização Min Max
dados <- dados %>% 
  mutate(CO = scale(CO, center = min(CO), scale = max(CO) - min(CO))) %>% 
  mutate(CO = round(CO, 1)) # Arredondando para uma casa decimal





# Normalizando NO_x utilizando a Normalização Min Max
dados <- dados %>% 
  mutate(NO_x = scale(NO_x, center = min(NO_x), scale = max(NO_x) - min(NO_x))) %>% 
  mutate(NO_x = round(NO_x, 1)) # Arredondando para uma casa decimal





# Normalizando HC utilizando a Normalização Min Max
dados <- dados %>% 
  mutate(HC = scale(HC, center = min(HC), scale = max(HC) - min(HC))) %>% 
  mutate(HC = round(HC, 1)) # Arredondando para uma casa decimal





# Adicionando a coluna WEEKDAY 
dados <- dados %>% 
  mutate(WEEKDAY = weekdays(as.Date(DATE))) %>% 
  select(WEEKDAY, everything())

# Adicionando a coluna HOUR 
dados <- dados %>%
  mutate(HOUR = hour(DATE)) %>%
  select(WEEKDAY, HOUR, everything())

# Removendo variáveis de dados
dados <- subset(dados, select = -DATE)





# Copiando o dataset
dadosdiscretos <- dados





# Discretizando os valores com o método de binning de largura igual para VELOCITY
num_intervalos <- 3
dadosdiscretos$VELOCITY <- cut(dadosdiscretos$VELOCITY, 
                               breaks = seq(min(dadosdiscretos$VELOCITY), max(dadosdiscretos$VELOCITY), length.out = num_intervalos + 1), 
                               labels = c("Baixa", "Média", "Alta"))
dadosdiscretos$VELOCITY[is.na(dadosdiscretos$VELOCITY)] <- "Baixa"

# Discretizando os valores com o método de binning de largura igual para DISTANCE
dadosdiscretos$DISTANCE <- cut(dadosdiscretos$DISTANCE, 
                               breaks = seq(min(dadosdiscretos$DISTANCE), max(dadosdiscretos$DISTANCE), length.out = num_intervalos + 1), 
                               labels = c("Baixa", "Média", "Alta"))
dadosdiscretos$DISTANCE[is.na(dadosdiscretos$DISTANCE)] <- "Baixa"

# Discretizando os valores com o método de binning de largura igual para HEIGHT
dadosdiscretos$HEIGHT <- cut(dadosdiscretos$HEIGHT, 
                             breaks = seq(min(dadosdiscretos$HEIGHT), max(dadosdiscretos$HEIGHT), length.out = num_intervalos + 1), 
                             labels = c("Baixa", "Média", "Alta"))
dadosdiscretos$HEIGHT[is.na(dadosdiscretos$HEIGHT)] <- "Baixa"

# Calcule o z-score para CO_2
dadosdiscretos$Z_SCORE_CO2 <- scale(dadosdiscretos$CO_2)

# Crie categorias com base no z-score para CO_2
dadosdiscretos$CO_2 <- cut(dadosdiscretos$Z_SCORE_CO2, breaks = c(-Inf, -1, 1, Inf), labels = c("Baixa", "Média", "Alta"))

# Calcule o z-score para CO
dadosdiscretos$Z_SCORE_CO <- scale(dadosdiscretos$CO)

# Crie categorias com base no z-score para CO
dadosdiscretos$CO <- cut(dadosdiscretos$Z_SCORE_CO, breaks = c(-Inf, -1, 1, Inf), labels = c("Baixa", "Média", "Alta"))

# Calcule o z-score para NO_x
dadosdiscretos$Z_SCORE_NOx <- scale(dadosdiscretos$NO_x)

# Crie categorias com base no z-score para NO_x
dadosdiscretos$NO_x <- cut(dadosdiscretos$Z_SCORE_NOx, breaks = c(-Inf, -1, 1, Inf), labels = c("Baixa", "Média", "Alta"))

# Calcule o z-score para HC
dadosdiscretos$Z_SCORE_HC <- scale(dadosdiscretos$HC)

# Crie categorias com base no z-score para HC
dadosdiscretos$HC <- cut(dadosdiscretos$Z_SCORE_HC, breaks = c(-Inf, -1, 1, Inf), labels = c("Baixa", "Média", "Alta"))






# Removendo variáveis de dadosdiscretos
dadosdiscretos <- subset(dadosdiscretos, select = -c(Z_SCORE_CO2, Z_SCORE_CO, Z_SCORE_NOx, Z_SCORE_HC))
