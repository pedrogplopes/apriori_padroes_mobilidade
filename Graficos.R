#install.packages(ggplot2)
#install.packages(dplyr)
#install.packages("reshape2")

library(reshape2)
library(ggplot2)
library(dplyr)

dados$VELOCITY <- as.numeric(as.character(dados$VELOCITY))
dados$HEIGHT <- as.numeric(as.character(dados$HEIGHT))
dados$DISTANCE <- as.numeric(as.character(dados$DISTANCE))
dados$CO_2 <- as.numeric(as.character(dados$CO_2))
dados$CO <- as.numeric(as.character(dados$CO))
dados$HC <- as.numeric(as.character(dados$HC))
dados$NO_x <- as.numeric(as.character(dados$NO_x))


# BAR PLOT - Frequência de VELOCITY em dados
ggplot(dados, aes(x = as.factor(VELOCITY))) +
  geom_bar(fill = "skyblue", color = "black", position = "dodge") +
  labs(title = "Frequência de VELOCITY", x = "VELOCITY", y = "Count") +
  scale_y_continuous(labels = scales::comma) +  # Isso impede a notação científica no eixo y
  theme_minimal()

# BAR PLOT - Velocidade média por dia da semana

dados$WEEKDAY <- factor(dados$WEEKDAY, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"), ordered = TRUE)

# Calculando a velocidade média para cada dia da semana
velocidades_por_dia <- dados %>%
  group_by(WEEKDAY) %>%
  summarise(media_velocity = mean(VELOCITY))

# Criando o gráfico de barras
ggplot(velocidades_por_dia, aes(x = WEEKDAY, y = media_velocity)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Velocidade Média por Dia da Semana", x = "Dia da Semana", y = "Velocidade Média") +
  theme_minimal()

# BAR PLOT - Velocidade média por hora do dia

# Calculando a velocidade média para cada hora do dia
velocidades_por_hora <- dados %>%
  group_by(HOUR) %>%
  summarise(media_velocity = mean(VELOCITY))

# Criando o gráfico de barras
ggplot(velocidades_por_hora, aes(x = factor(HOUR), y = media_velocity)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Velocidade Média por Hora do Dia", x = "Hora do Dia", y = "Velocidade Média") +
  theme_minimal()

# BAR PLOT - Velocidade Média por Altura
velocidade_por_altura <- dados %>%
  group_by(HEIGHT) %>%
  summarise(
    media_VELOCITY = mean(VELOCITY)
  )

# Criando o gráfico de barras
ggplot(velocidade_por_altura, aes(x = as.factor(HEIGHT), y = media_VELOCITY)) +
  geom_bar(stat = "identity", position = "dodge", fill = "skyblue") +
  labs(title = "Velocidade Média por Altura",
       x = "Altura",
       y = "Velocidade Média") +
  theme_minimal()

# BAR PLOT - Velocidade Média por VSPMode
velocidade_por_vspmode <- dados %>%
  group_by(VSPMode) %>%
  summarise(
    media_VELOCITY = mean(VELOCITY)
  )

# Criando o gráfico de barras
ggplot(velocidade_por_vspmode, aes(x = as.factor(VSPMode), y = media_VELOCITY)) +
  geom_bar(stat = "identity", position = "dodge", fill = "skyblue") +
  labs(title = "Velocidade Média por Modo Específico do Veículo",
       x = "Modo Específico do Veículo",
       y = "Velocidade Média") +
  theme_minimal()


# BAR PLOT - CO_2, CO, NOX E HC por hora do dia

# Calculando as médias de poluentes para cada hora do dia
poluentes_por_hora <- dados %>%
  group_by(HOUR) %>%
  summarise(
    media_CO2 = mean(CO_2),
    media_NOx = mean(NO_x),
    media_HC = mean(HC),
    media_CO = mean(CO)
  )

# Melt do dataframe para facilitar a plotagem
melted_data <- melt(poluentes_por_hora, id.vars = "HOUR", variable.name = "Poluente", value.name = "Média")

# Criando o gráfico de barras
ggplot(melted_data, aes(x = factor(HOUR), y = Média, fill = Poluente)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Emissão Média por Hora do Dia",
       x = "Hora do Dia",
       y = "Emissão Média (log scale)") +
  theme_minimal()

# BAR PLOT - CO_2, CO, NOX E HC por velocidade
poluentes_por_velocidade <- dados %>%
  group_by(VELOCITY) %>%
  summarise(
    media_CO2 = mean(CO_2),
    media_NOx = mean(NO_x),
    media_HC = mean(HC),
    media_CO = mean(CO)
  )

# Melt do dataframe para facilitar a plotagem
melted_data <- melt(poluentes_por_velocidade, id.vars = "VELOCITY", variable.name = "Poluente", value.name = "Média")

# Criando o gráfico de barras
ggplot(melted_data, aes(x = as.factor(VELOCITY), y = Média, fill = Poluente)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Emissão Média por Velocidade",
       x = "Velocidade",
       y = "Emissão Média") +
  theme_minimal()

# BAR PLOT - CO_2, CO, NO_x e HC por distância
poluentes_por_distancia <- dados %>%
  group_by(DISTANCE) %>%
  summarise(
    media_CO2 = mean(CO_2),
    media_NOx = mean(NO_x),
    media_HC = mean(HC),
    media_CO = mean(CO)
  )

# Melt do dataframe para facilitar a plotagem
melted_data_distancia <- melt(poluentes_por_distancia, id.vars = "DISTANCE", variable.name = "Poluente", value.name = "Média")

# Criando o gráfico de barras
ggplot(melted_data_distancia, aes(x = as.factor(DISTANCE), y = Média, fill = Poluente)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Emissão Média por Distância",
       x = "Distância",
       y = "Emissão Média") +
  theme_minimal()

# BAR PLOT - CO_2, CO, NO_x e HC por VSPMode
poluentes_por_vspmode <- dados %>%
  group_by(VSPMode) %>%
  summarise(
    media_CO2 = mean(CO_2),
    media_NOx = mean(NO_x),
    media_HC = mean(HC),
    media_CO = mean(CO)
  )

# Melt do dataframe para facilitar a plotagem
melted_data_vspmode <- melt(poluentes_por_vspmode, id.vars = "VSPMode", variable.name = "Poluente", value.name = "Média")

# Criando o gráfico de barras
ggplot(melted_data_vspmode, aes(x = as.factor(VSPMode), y = Média, fill = Poluente)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Emissão Média por Modo Específico do Veículo",
       x = "Modo Específico do Veículo",
       y = "Emissão Média") +
  theme_minimal()
