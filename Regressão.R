setwd("C:/RTrabalhos")

#Exercicio 1
#install.packages("ggpubr")
library(ggplot2)
library(ggpubr)
library(moments)
library(dplyr)

df <- data.frame(x = c(5.2,5.1,4.9,4.6,4.7,4.8,4.6,4.9),
           y = c(13,15,16,20,19,17,21,16))
df

dev.off()

p <- ggplot(data = df, aes(x = x, y = y)) +
  geom_point(color = "red", fill="blue") + xlab("x") +
  ylab("y") + theme_pubr(legend = "right") +
  labs( caption = "Fonte: Elaborado pelo autor") + labs_pubr() +
  theme(plot.caption = element_text(hjust = 0))
 
p

#O modelo por meio do R-squared de 0.9396 
#explica aproximado 94% da variabilidade da nossa variável resposta
#Observamos que o nosso p-value das nossas duas variaveis está abaixo no nivel
#de significancia de 0,05, logo rejeitamos a hipotese nula
#que o resultado poderia ser obtido de maneira aleatoria, ou seja os dois parametros 
#Não são por acaso, logo existe um relacionamento sobre os parametros que foram estimados pelo modelo.
#Em ralação o grafico de residos e valores ajustados percebemos que não existe uma variancia constante,
#Não temos uma homocedasticidade no nosso conjunto de dados
#Logo a nossa variavel y tambem não tem uma distribuição normal
#Segundo o grafico Q-Q os pontos estão distante da linha tracejada.
dev.off()

fit <- lm(y ~ x, data=df)
summary(fit)
par(mfrow=c(2,2))

plot(fit)


dev.off()
plot(y~x,data = df) 
abline(lm(y ~ x, data=df))


#Exercicio 2

library(data.table)


dados <- read.csv2("pib_gapminder.csv", header=TRUE, sep = ",", encoding = "UTF-8" )


str(dados)
#variavel Ano  quantitativa continua
#Variavel Pais qualitativa nominal
#Variavel continente qualitativa nominal 
#Variavel ExpVida quantitativa continua 
#Variavel Pipercap quantitativa continua
#Variavel pop quantitativa discreta
summary(dados)

dados$pop <- as.numeric(as.character(dados$pop))
dados$expVida <- as.numeric(as.character(dados$expVida))
dados$pibPercap <- as.numeric(as.character(dados$pibPercap))

summary(dados)

group_by(dados,continente)%>%summarise(count=n())

table(dados$continente)

x = c(624,300,396,360,24)
prop.table(x)*100

Frequencia = data.frame(continente = c("Africa","Americas","Asia","Europe","Oceania"),
                        Frequencia_Absolut=x,Frequencia_Relativa =prop.table(x)*100)

Frequencia

barplot(Frequencia$Frequencia_Absolut, names=Frequencia$continente, col="#69b3a2")

q <- ggplot(data = dados, aes(x = expVida , y = pibPercap)) +
  geom_point(aes(col=continente)) + xlab("x") +
  ylab("y") + theme_pubr(legend = "right") +
  labs( caption = "Fonte: Elaborado pelo autor") + labs_pubr() +
  theme(plot.caption = element_text(hjust = 0))

q

dados.pct = copy(dados)

dados.pct$lpibPercap = log(dados.pct$pibPercap)
dados.pct$lexpVida = log(dados.pct$expVida)

j <- ggplot(data = dados.pct, aes(x = lexpVida , y = lpibPercap)) +
  geom_point(aes(col=continente)) + xlab("lexpVida") +
  ylab("lpibPercap") + theme_pubr(legend = "right") +
  labs( caption = "Fonte: Elaborado pelo autor") + labs_pubr() +
  theme(plot.caption = element_text(hjust = 0))

j


#O modelo por meio do R-squared de 0.613 
#explica aproximado 61% da variabilidade da nossa variável resposta
#Observamos que o nosso p-value das nossas duas variaveis está abaixo no nivel
#de significancia de 0,05, logo rejeitamos a hipotese nula
#que o resultado poderia ser obtido de maneira aleatoria, ou seja os dois parametros 
#Não são por acaso, logo existe um relacionamento sobre os parametros que foram estimados pelo modelo.
#Em ralação o grafico de residos e valores ajustados percebemos que não existe uma variancia constante,
#Não temos uma homocedasticidade no nosso conjunto de dados
#Logo a nossa variavel y tambem não tem uma distribuição normal
#Segundo o grafico Q-Q os pontos estão distante da linha tracejada.


fit <- lm(lexpVida ~lpibPercap, data=dados.pct)
summary(fit)
par(mfrow=c(2,2))

plot(fit)



#Exercicio 3



library(mice)
library(tidyr)

library(corrplot)

#CARREGAR DASET
dadosAuto <- read.csv2("autos.csv", header=TRUE,  encoding = "UTF-8" )


#SUMARIO ESTATISTICO
summary(dadosAuto)
#ESTRUTURA
str(dadosAuto)

#VETOR VARIAVEIS CATEGORICAS

cat = c("make","fuel.type","aspiration","body.style","drive.wheels","engine.location","fuel.system","engine.type","num.cylinders")

#DATASET VARIAVEIS NUMERICAS
df.num <- dadosAuto[sapply(dadosAuto, is.numeric)]
#DATASET VARIAVEIS CATEGORICAS
df <- subset(dadosAuto, select =cat)

df

#VISUALIZAÇÕES VARIAVEIS NUMERICAS

df.num %>%  gather(-price, key = "var", value = "Specifications") %>%
  ggplot(aes(x = price, y = Specifications)) +
  geom_point() +
  facet_wrap(~ var, scales = "free") +
  ggtitle('Relationship between Price & Specifications')

#MATRIX DE CORRELAÇÃO
library(corrplot)
df.num %>%  as.matrix() %>% cor() %>% corrplot(method = "number")

ggplot(dadosAuto, aes(x=factor(num.cylinders), fill=factor(num.cylinders)))+geom_bar()+
  labs(title = 'Motor Cars Cyl & Count', subtitle='Bar Plot in R')

ggplot(dadosAuto, aes(num.doors, body.style)) + geom_jitter(aes(color=make), size = 3) + labs(title = "All vehicle makes")


ggplot(dadosAuto, aes(num.doors, body.style)) + geom_jitter(aes(color=make), size = 3) + labs(title = "All vehicle makes") # all data
ggplot(filter(dadosAuto,make=="mazda" | make=="dodge"), aes(num.doors, body.style)) + geom_jitter(aes(color=make), size = 3) + labs(title = "Mazda, Dodge")


#VISUALIZAÇÃO VARIAVEIS CATEGORICAS
df %>% group_by(make) %>% summarise(count = n()) %>%  
  ggplot(aes(x= reorder(make,count),y=count)) + geom_bar(stat = "identity") + coord_flip() +
  xlab("Car Make") + ylab("Count")

# Boxplot
dadosAuto %>% select(make,price) %>%  ggplot(aes(x= make,y=price)) + geom_boxplot() + coord_flip() +  xlab("Car Make") + ylab("Price")



# Fuel type
df %>% group_by(fuel.type) %>% summarise(count = n()) %>%  
  ggplot(aes(x= reorder(fuel.type,count),y=count)) + geom_bar(stat = "identity") +
  xlab("Fuel type") + ylab("Count")

# Boxplot
dadosAuto %>% select(fuel.type,price) %>%  ggplot(aes(x= fuel.type,y=price)) + geom_boxplot() +  xlab("Fuel Type") + ylab("Price")


# Aspiration
df %>% group_by(aspiration) %>% summarise(count = n()) %>%  
  ggplot(aes(x= reorder(aspiration,count),y=count)) + geom_bar(stat = "identity") +
  xlab("Aspiration") + ylab("Count")

# Boxplot
dadosAuto %>% select(aspiration,price) %>%  ggplot(aes(x= aspiration,y=price)) + geom_boxplot() +  xlab("Aspiration") + ylab("Price")


# Engine Location
df %>% group_by(engine.location) %>% summarise(count = n()) %>%  
  ggplot(aes(x= reorder(engine.location,count),y=count)) + geom_bar(stat = "identity") +
  xlab("Engine Location") + ylab("Count")

# Boxplot
dadosAuto %>% select(engine.location,price) %>%  ggplot(aes(x= engine.location,y=price)) + geom_boxplot() +  xlab("Engine Location") + ylab("Price")


# Engine SIZE
df %>% group_by(engine.type) %>% summarise(count = n()) %>%  
  ggplot(aes(x= reorder(engine.type,count),y=count)) + geom_bar(stat = "identity") +
  xlab("Engine Type") + ylab("Count")

# Boxplot
dadosAuto %>% select(engine.type,price) %>%  ggplot(aes(x= engine.type,y=price)) + geom_boxplot() +  xlab("Engine Type") + ylab("Price")


# Fuel System
df %>% group_by(fuel.system) %>% summarise(count = n()) %>%  
  ggplot(aes(x= reorder(fuel.system,count),y=count)) + geom_bar(stat = "identity") +
  xlab("Fuel System") + ylab("Count")

# Boxplot
dadosAuto %>% select(fuel.system,price) %>%  ggplot(aes(x= fuel.system,y=price)) + geom_boxplot() +  xlab("Fuel System") + ylab("Price")

# Drive wheels
df %>% group_by(drive.wheels) %>% summarise(count = n()) %>%  
  ggplot(aes(x= reorder(drive.wheels,count),y=count)) + geom_bar(stat = "identity") +
  xlab("Drive Wheels") + ylab("Count")

# Boxplot
dadosAuto %>% select(drive.wheels,price) %>%  ggplot(aes(x= drive.wheels,y=price)) + geom_boxplot() +  xlab("Drive Wheels") + ylab("Price")


# Num of cylinders
df %>% group_by(num.cylinders) %>% summarise(count = n()) %>%  
  ggplot(aes(x= reorder(num.cylinders,count),y=count)) + geom_bar(stat = "identity") +
  xlab("Num of cylinders") + ylab("Count")

# Boxplot
dadosAuto %>% select(num.cylinders,price) %>%  ggplot(aes(x= num.cylinders,y=price)) + geom_boxplot() +  xlab("Num of cylinders") + ylab("Price")



#carregando dados



dadossub <-subset(dadosAuto, select=c("price","horsepower"))
dadossub




#existe uma forte correlação postiva entre as variaveis preço e potencia
M= cor(dadossub)
corrplot(M, method = 'number')

  ggplot(data = dadossub, aes(x = price, y = horsepower)) +
  geom_point(color = "red", fill="blue") + xlab("x") +
  ylab("y") + theme_pubr(legend = "right") +
  labs( caption = "Fonte: Elaborado pelo autor") + labs_pubr() +
  theme(plot.caption = element_text(hjust = 0)) 

  
  
#R-squared:  0.6583 , explicando 65% da variabiliade da variavel resposta
plot(price~horsepower,data = dadossub) 
abline(lm(price ~horsepower ,  data=dadossub))



sapply(dadosAuto, function(x) sum(is.na(x)))
#os dados não seguem uma distribuição normal e percebesse uma grande discrepancia nos residuos
fit <- lm(price ~horsepower, data=dadosAuto)
summary(fit)
par(mfrow=c(2,2))
plot(fit)

#Eixo y é cortado no ponto -4630, não faz sentido devido tratar de preço a variavel resposta não existindo preço ou potencia negativa
coef(fit)

#removendo o intercept do modelo agora possui um R-squared:  0.8974 sendo melhorado

plot(price~horsepower,data = dadossub) 
abline(lm(price ~horsepower -1 ,  data=dadossub))

fit <- lm(price ~horsepower -1, data=dadosAuto)
summary(fit)
par(mfrow=c(2,2))
plot(fit)


#apenas a potencia não pe suficiente para explicar a variavael resposta
#poderia haver inclusão de mais variaveis para criação de um modelo, mesmo tendo 89% R-square.
#pois não segue uma distribuição normal nem tem homosticidade nos dados
#são os principais fatores para um bom modelo, ou posso até mesmo usar transformação na variavel resposta 
#buscando a normalização dos dados