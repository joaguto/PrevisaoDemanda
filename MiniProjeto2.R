# Projeto com Feedback da Formação Cientista de Dados da Data Science Academy

# Objetivo: Neste projeto de aprendizado de máquina, você deve desenvolver um 
# modelo para prever com precisão a demanda de estoque com base nos dados 
# históricos de vendas. Isso fará com que os consumidores dos mais de 100 
# produtos de panificação não fiquem olhando para as prateleiras vazias, além de 
# reduzir o valor gasto com reembolsos para os proprietários de lojas com produtos 
# excedentes impróprios para venda.

# Sobre a empresa:O Grupo Bimbo, se esforça para atender a 
# demanda diária dos consumidores por produtos frescos de panificação nas 
# prateleiras de mais de 1 milhão de lojas ao longo das suas 45.000 lojas em todo o 
# México.

# Objetivo especifico: Construir um modelo preditivo com uma precisão de 92%
################################################################################
# Pré processamento de dados

# Definindo o diretório de Trabalho
setwd("C:/Users/JoaoA/OneDrive - SENAC - SP/CoisasdoJoao/Curso/Data Science/Cursos/01_Big_Data_Analytics_Com_R/Mini-Projetos/Projeto2")
getwd()

# Carregando os Dados
library(data.table)
dir('C:/Users/JoaoA/OneDrive - SENAC - SP/CoisasdoJoao/Curso/Data Science/Cursos/01_Big_Data_Analytics_Com_R/Mini-Projetos/Projeto2/Dataset')

# Dados complementares
cliente <- fread('Dataset/cliente_tabla.csv')
produto <- fread('Dataset/producto_tabla.csv')
cidade <- fread('Dataset/town_state.csv', encoding = 'UTF-8')

# Dados de Teste
teste <- fread('Dataset/test.csv')

# Carregando dados de treino
library(RSQLite)

system("gruop_bimbo.db")

# Criando driver e conexão ao banco de dados
drv = dbDriver("SQLite")

# Criando o banco de dados
con = dbConnect(drv, dbname = "grupo_bimbo.db")

# Carregando os dados
dbWriteTable(con, "teste", "Dataset/test.csv", sep = ",", header = T)
dbWriteTable(con, "treino", "Dataset/train.csv", sep = ",", header = T)
dbWriteTable(con, "produto", "Dataset/producto_tabla.csv", sep = ",", header = T)
dbWriteTable(con, "cliente", "Dataset/cliente_tabla.csv", sep = ",", header = T)
dbWriteTable(con, "cidade", "Dataset/town_state.csv", sep = ",", header = T)

# Listando as tabelas
dbListTables(con)

# Coletando 10000 Dados na base de treino
# Tive problemas com o processamento de dados e por isso optei por essa alternativa
qry = 'SELECT * FROM treino ORDER BY RANDOM() LIMIT 100000'
rs = dbSendQuery(con, qry)
dados = fetch(rs)
treino = dados
dados = NULL
dbDisconnect(con)

# Salvando amostra dos arquivos de treino
write.csv(treino, 'Dataset/train_amostra.csv')
###############################################################
# Processamento dos dados
library(dplyr)

# Vizualizando os Dados de Teste
View(teste)
dim(teste)
str(teste)

# Vizualizando os dados de treino
View(treino)
dim(treino)
str(treino)

# Vizualizando dados complementares
View(cidade)
View(cliente)
View(produto)
names(treino)

# Agrupando as bases
treino = left_join(treino, produto, by = 'Producto_ID')
treino = left_join(treino, cidade, by = 'Agencia_ID')
View(treino)

# Renomando as variavais
names(treino) = c('Semana', 'Agencia_ID', 'Canal_ID', 'Rota_ID', 'Cliente_ID',
                  'Produto_ID', 'Vendas_Unitarias_Semana', 'Vendas_Pesos_Semana',
                  'Unidades_Proxima_Semana', 'Peso_Proxima_Semana', 'Demanda_Ajustada',
                  'Cidade', 'Estado', 'NomeProduto')
View(treino)  

# Salvando base nova
write.csv(treino, 'Dataset/train_amostra.csv')

# Realizando o upload dos dados
df = fread('Dataset/train_amostra.csv')
df$V1 = NULL
str(df)

df$Semana = as.factor(df$Semana)
df$Agencia_ID = as.character(df$Agencia_ID)
df$Canal_ID = as.character(df$Canal_ID)
df$Rota_ID =  as.character(df$Rota_ID)
df$Cliente_ID = as.character(df$Cliente_ID)
df$Produto_ID = as.character(df$Produto_ID)

##########################################################################################
# Analise Exploratoria dos Dados
library(ggplot2)

# Verificando se existe dados ausentes
any(is.na(df))

# Vizualizando o numero de Semanas
table(df$Semana)

# Explorando Variavel Target
boxplot(df$Demanda_Ajustada)
hist(df$Demanda_Ajustada)
summary(df$Demanda_Ajustada)

# Foi identificado muito Outliers nos dados
# Se eu optar por apenas excluir os dados utilizando o 3 quartil,
# estarei perdendo ~22% dos meu dados
nrow(filter(df, Demanda_Ajustada > 10))/nrow(df)

# No entando, se eu aumentar um pouco a minha margem, fico apenas com ~1% de perda
# Estarei optando por essa opção:
nrow(filter(df, Demanda_Ajustada > 10))/nrow(df)
df = filter(df, Demanda_Ajustada < 10)

# Explorando Variavel limpa
boxplot(df$Demanda_Ajustada)
hist(df$Demanda_Ajustada, breaks = 5)
summary(df$Demanda_Ajustada)
library(moments)

# Verificando o Coeficiente de Assimetria
skewness(df$Demanda_Ajustada)
# Um coeficiente alto, indica que os dados tendem estar acima da média
# Estão concentrados no lado direto do Histograma

# Verificando qual semana possui um numero de demanda maior
semana = df %>%
  group_by(Semana) %>%
  summarise(Demanda_Media = mean(Demanda_Ajustada),
            Demanda_Soma = sum(Demanda_Ajustada))

g1 = ggplot(semana, aes(x = Semana, y = Demanda_Media, fill = as.factor(Semana))) +
  geom_bar(stat = 'identity') +
  ggtitle('Média de Demanda Ajustada por Semana') +
  xlab('N° Semana') +
  ylab('Demanda') +
  theme_light() +
  geom_label( aes(label = round(Demanda_Media, 2)))

g2 = ggplot(semana, aes(x = Semana, y = Demanda_Soma, fill = as.factor(Semana))) +
  geom_bar(stat = 'identity') +
  ggtitle('Soma de Demanda Ajustada por Semana') +
  xlab('N° Semana') +
  ylab('Demanda') +
  theme_light() +
  geom_label( aes(label = round(Demanda_Soma, 2)))

# Colocando em um unico painek
library(patchwork)
(g1/g2)

# Aqui podemos ver um comportamento interesante, onde que as semanas que possuem
# Uma média de demanda maior não necessariamente possui uma soma maior.
# Fato que pode ser explicado através de medidas de desvio

# Calculando o Desvio Padrão
semana = df %>%
  group_by(Semana) %>%
  summarise(Demanda_Media = mean(Demanda_Ajustada),
            Demanda_Soma = sum(Demanda_Ajustada),
            Desv_Padrao = sd(Demanda_Ajustada))

g3 = ggplot(semana, aes(x = Semana, y = Desv_Padrao, fill = as.factor(Semana))) +
  geom_bar(stat = 'identity') +
  ggtitle('Desv Padrao de Demanda Ajustada por Semana') +
  xlab('N° Semana') +
  ylab('Desvio') +
  theme_light() +
  geom_label( aes(label = round(Desv_Padrao, 2)))

# Fato comprovado
(g1/g2 | g3)

# Verificando distribuição dos estados
table(df$Estado)
table(df$NomeProduto)

names(df)

# Verificando correlações de forma macro
library(corrplot)
corrplot(cor(df[, 7:11]))

# Correlação com Vendas da semana
ggplot(df, aes(x = Demanda_Ajustada, y =Vendas_Unitarias_Semana )) +
  geom_point()

# Correlação com Peso de Vendas da semana
ggplot(df, aes(x = Demanda_Ajustada, y =Vendas_Pesos_Semana )) +
  geom_point()

# Verificando as demandas ao longo do tempo
ggplot(semana, aes(x = Semana, y =Demanda_Soma )) +
  geom_line() +
  geom_smooth(method = 'lm', se = TRUE) +
  ggtitle('Demanda Ajusta ao longo das Semanas') +
  theme_light()
# Ou seja, estamos em tendencia de queda


# Verificando a distribuição das variaveis numericas
hist(df$Vendas_Unitarias_Semana)
boxplot(df$Vendas_Unitarias_Semana)

hist(df$Unidades_Proxima_Semana)
boxplot(df$Unidades_Proxima_Semana)

# Removendo alguns Outliers
df = filter(df, Unidades_Proxima_Semana < 150, Vendas_Unitarias_Semana < 60)

# Verificando as melhores agencias
agencia = df %>%
  group_by(Agencia_ID) %>%
  summarise(mediaVendas = mean(Vendas_Unitarias_Semana),
            somaVendas = sum(Vendas_Unitarias_Semana),
            desv = sd(Vendas_Unitarias_Semana),
            assimetriaa = skewness(Vendas_Unitarias_Semana)) %>%
  arrange(desc(mediaVendas))
View(agencia)

# Gráfico com as top 5 agencias

# Verificando os melhores produtos
 # As melhores agencias ficam no México, sendo elas:
# 1250
# 1255
# 1593

#######################################################################
# Feature Selection
library(randomForest)
library(caTools)

# Excluindo os dados que repetidos
df$Cidade = NULL
df$Estado = NULL
df$NomeProduto = NULL

# Criando modelo de teste
teste1 = randomForest(Demanda_Ajustada~., data = df, importance = TRUE)
varImpPlot(teste1)

# Normalizando os dados
df[,7] = scale(df[,7])
df[, 8] = scale(df[,8])
df[, 9]= scale(df[,9])
df[, 10]= scale(df[,10])

# Realizando novamente o teste
teste2 = randomForest(Demanda_Ajustada~., data = df, importance = TRUE)
varImpPlot(teste2)

# Deletando Variaveis não necessarias:
df$Cliente_ID = NULL
df$Canal_ID = NULL

# Separando os dados
apoio = sample.split(df$Semana, 0.7)

treino = df[apoio, ]
teste = df[apoio== FALSE,0:8]
real = df[apoio== FALSE,9]



####################################################################
# Criando Modelos Preditivos

# Modelo 1 - Random Forest
model1 = randomForest(Demanda_Ajustada~., data = treino)
previsao1 = predict(model1, teste)
final = data.frame(real, previsao1)

final = final %>%
  mutate(diferenca = Demanda_Ajustada - previsao1)

summary(model1)

# Verificando o residuo do modelo
hist(final$diferenca)

# Modelo 2 - Neuralnet
# Pequeno ajuste como o algoritmo não aceita valores não numericos
library(neuralnet)
neural = read.csv('teste.csv')
write.csv(teste, 'teste.csv')
str(neural)

model2 = neuralnet(Demanda_Ajustada~., neural)
previsao2 = predict(model2, neural)

final2 = data.frame(real, previsao2)

final2 = final2 %>%
  mutate(diferenca = Demanda_Ajustada - previsao2)

hist(final2$diferenca)

# Modelo3 - bayesian-linear-regression
model3<- stan_glm(Demanda_Ajustada~., data = treino)
previsao3 <- predict(model3, teste)
length(previsao3)

final3 = data.frame(real, previsao3[0:24706])
final3 = final3 %>%
  mutate(diferenca = Demanda_Ajustada - previsao3.0.24706.)
hist(final3$diferenca, breaks = 50)

##########################################################
# Ao olhar o histograma, os melhores modelos foram o 1 e 3
# Agora vamos realizar uma analise mais profunda para achar o melhor modelo

# Modelo1
summary(final$diferenca)
hist(final$diferenca)
boxplot(final$diferenca)

qqnorm(final$diferenca)
qqline(final$diferenca)
# O modelo está está tanto com a média, como a mediana e os quartil bem próximo de 0
# O que mostra que o modelo errou bem pouco.

# Modelo2
summary(final3$diferenca)
hist(final3$diferenca)
boxplot(final3$diferenca)

qqnorm(final3$diferenca)
qqline(final3$diferenca)

# O modelo 2 teve muitos Outliers, e esses erros não podemos cometer em nosso problema
# Por esse motivo estaremos utilizando o modelo 1

# Verificando a quantidade de erro por Agencia ID
erro = data.frame(final, teste)
erro_id = erro %>%
  group_by(Agencia_ID) %>%
  summarise(mediaErro = mean(diferenca),
            desvPadrao = sd(diferenca)) %>%
  arrange(desc(mediaErro))
View(erro_id)

# O modelo 1 está bom!

#################################################################
# Otimização do Modelo

model1.1 = randomForest(Demanda_Ajustada~ .-Rota_ID, data = treino )
previsao1.1 = predict(model1.1, teste)
final1.1 = data.frame(real, previsao1.1)

final1.1 = final1.1 %>%
  mutate(diferenca = Demanda_Ajustada - previsao1.1)

summary(final1.1$diferenca)
hist(final1.1$diferenca)

# Mesmo o modelo tendo uma variação um pouco maior do que sua primeira versão,
# Nessa versão sua média e mediana ficaram mais próxima de 0 do que o primeiro

# Tentativa 2
model1.2 = randomForest(Demanda_Ajustada~ .-Rota_ID, data = treino, ntree = 25 )
previsao1.2 = predict(model1.2, teste)
final1.2 = data.frame(real, previsao1.2)

final1.2 = final1.2 %>%
  mutate(diferenca = Demanda_Ajustada - previsao1.2)

summary(final1.2$diferenca)
hist(final1.2$diferenca)

# Houve uma piora no modelo

##########################################################################

# Eu entregaria o modelo 1.1 aos tomadores de decisão,
# Pois é o modelo que errou muito pouco.
# O modelo está preciso e operante!
