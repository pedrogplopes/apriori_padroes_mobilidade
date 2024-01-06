#install.packages("arules")
#install.packages("arulesViz")


library(arulesViz)
library(arules)

# Transformar variáveis para fatores
dadosdiscretos$HOUR <- as.factor(dadosdiscretos$HOUR)
dadosdiscretos$WEEKDAY <- as.factor(dadosdiscretos$WEEKDAY)
dadosdiscretos$VELOCITY <- as.factor(dadosdiscretos$VELOCITY)
dadosdiscretos$HEIGHT <- as.factor(dadosdiscretos$HEIGHT)
dadosdiscretos$DISTANCE <- as.factor(dadosdiscretos$DISTANCE)
dadosdiscretos$CO_2 <- as.factor(dadosdiscretos$CO_2)
dadosdiscretos$CO <- as.factor(dadosdiscretos$CO)
dadosdiscretos$HC <- as.factor(dadosdiscretos$HC)
dadosdiscretos$NO_x <- as.factor(dadosdiscretos$NO_x)

# Selecione quais variáveis vão entrar nas transações
variaveis_selecionadas <- c("VELOCITY", "CO_2")

# Criar transações apenas para as colunas selecionadas
transacoes <- as(dadosdiscretos[, variaveis_selecionadas], "transactions")

# Executar o algoritmo Apriori
regras <- apriori(transacoes, parameter = list(support = 0.0001, confidence = 0.001))
regras <- sort(regras, by = "lift", decreasing = TRUE)

# Exibir as regras
inspectDT(regras)

# Plotar as regras de associação
plot(regras, method = "graph", control = list(type = "items"))
