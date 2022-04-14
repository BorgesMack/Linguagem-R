library(MASS)
library(ggplot2)
library(dplyr)
library(data.table)
library(tidyr)
library(GGally)

data(diamonds)
diamonds= data.table(diamonds)
#diamonds
#A saída da função summary() está de acordo com a descrição mostrada anteriormente?
#Na variavel Clarity esta diferente da descrição da descrição mostrada anteriomente faltando dois status
#I1 e IF
summary(diamonds)

#Estrutura dos dados
str(diamonds)

#Primeiras linhas
head(diamonds, n =10)
#Ultimas linhas
tail(diamonds, n = 10)

#Analise do preço
diamonds %>%
  ggplot(aes(x=cut,y=price, color=cut)) +
  geom_boxplot()

boxplot(diamonds$price)

diamonds %>%
  ggplot(aes(x=(price))) +
  geom_histogram(stat="bin",binwidth= 500) +
  facet_wrap(~cut, scales = "free")


par(mfrow=c(1,2))
hist(diamonds$price,main="Price")
hist(log(diamonds$price),main="log Price")

par(mfrow = c(1,3))
boxplot(diamonds$carat, main ="Carat")
boxplot(diamonds$depth, main = "Depth")
boxplot(diamonds$table, main = "Table")

par(mfrow = c(1,3))
boxplot(diamonds$x, main ="x")
boxplot(diamonds$y, main ="y")
boxplot(diamonds$z, main ="z")

par(mfrow=c(1,3))
ggplot(diamonds, aes(x = price)) +
  geom_histogram(fill = 'blue', bins = 30 )
ggplot(diamonds, aes(x = carat)) +
  geom_histogram(fill = 'blue', bins = 30 )
ggplot(diamonds, aes(x = depth)) +
  geom_histogram(fill = 'blue', bins = 30 )

ggplot(diamonds, aes(x = table)) +
  geom_histogram(fill = 'blue', bins = 30 )

ggplot(diamonds, aes(x = x)) +
  geom_histogram(fill = 'blue', bins = 30 )

ggplot(diamonds, aes(x = y)) +
  geom_histogram(fill = 'blue', bins = 30 )
ggplot(diamonds, aes(x = z)) +
  geom_histogram(fill = 'blue', bins = 30 )

#correlação
cor((diamonds)[,.(carat,x,y,z)])

cor(diamonds[,.(price, carat)])

cor(diamonds[,.(log(price), log(carat))])

#Ao plotar as variáveis categóricas como cor, podemos ver que essas três variáveis ​​mostram uma relação com Price. A partir daqui, poderíamos supor que essas três variáveis ​​seriam importantes na previsão de preço, portanto, incluí-las
#como recursos em nosso modelo de previsão a seguir.

par(mfrow=c(1,3))
diamonds %>%
  ggplot(aes(log(price),log(carat), col= clarity))+
  geom_point()

diamonds %>%
  ggplot(aes(log(price),log(carat), col= cut))+
  geom_point()

diamonds %>%
  ggplot(aes(log(price),log(carat), col= color))+
  geom_point()


#Relacão entre duas variaveis categoricas, utilizando a frequencia relativa 
new= 
  diamonds %>%
  group_by(color, cut) %>%
  summarise(n=n()) %>%
  group_by(cut) %>%
  mutate(sum.n= sum(n)) %>%
  ungroup() %>%
  mutate(n2= n/sum.n) %>%
  select(color, cut, n2)


#Azul mais escuro indica contagem mais alta e branco indica contagem baixa
new %>% spread(cut,n2)
new %>%
  ggplot(aes(color, cut)) +
  geom_tile(aes(fill=n2*100), colour = "white") +
  scale_fill_gradient(low="white",high="blue") +
  labs(fill = "Density")

