setwd("C:/RTrabalhos")
library(ggplot2)
library(ggpubr)
library(moments)
library(GGally)
library(dplyr)
library(lmtest)
library(olsrr)
library(gvlma)

#CARREGANDO DADOS
dadosAuto <- read.csv2("autos.csv", header=TRUE,  encoding = "UTF-8" )

#CRIANDO UM RESUMO ESTATISTICO DOS DADOS
summary(dadosAuto)

#CRIANDO UM SUBSET 
dados= select(dadosAuto,horsepower,length,engine.size,city.mpg,price)
newdados = select(dadosAuto,horsepower,length,engine.size,city.mpg,price)
#PRIMEIRAS LINHAS DO SUBSET
head(dados)

#ESTRUTURA DOS DADOS
str(dados)
#VERIFICANDO VALORES NULOS
sum(is.na(dados))
#VISUALIZANDO AS VARIAVEIS EM BOXPLOT,
#VARIAVEIS HORSERPOWER, ENGINE.SIZE E PRICE POSSUI ALGUNS OULTILIERS 
par(mfrow = c(1,3))
boxplot(dados$horsepower, main ="horsepower")
boxplot(dados$length, main = "length")
boxplot(dados$engine.size, main = "engine.size")

par(mfrow = c(1,2))
boxplot(dados$city.mpg, main ="city.mpg")
boxplot(dados$price, main = "price")

#SUMARIO ESTATISTICO DO SUBSET
summary(dados)


#ANALISANDO CORRELACAO ENTRE AS VARIAVEIS,
#VARIAVEL HORSEPOWER POSSUI UM FORTE RELACIONAMENTO COM AS VARIAVEIS PRICE,
#LENGTH E ENGINE.SIZE APRESENTANDO INDICIOS DE MULTICOLINEARIDADE  
ggcorr(dados, palette = "RdYlGn", name = bquote(rho),
      label = TRUE, label_color = "black") +
      labs( caption = "Fonte: Elaborado pelo autor") +
      theme(plot.caption = element_text(hjust = 0,size = 8))

 ggpairs(dados, columns = 1:ncol(dados), title = "", axisLabels = "show") +
      theme(axis.text.x = element_text(angle=90, vjust=0.5, size=10)) +
      theme_pubr() + labs_pubr() +
      labs( caption = "Fonte: Elaborado pelo autor") + labs_pubr() +
      theme(plot.caption = element_text(hjust = 0, size = 8))
#ANALISANDO RELACIONAMENTO ENTRE A VARIAVEL RESPOSTA E AS DEMAIS 
 
par(mfrow = c(1,2)) 
plot(price~city.mpg,data = dados)
plot(price~engine.size,data = dados)

par(mfrow = c(1,2))
plot(price~horsepower,data = dados)
plot(price~length,data = dados)

dev.off()
#CRIANDO MODELO 
##VARIAVEL CITY.MPG NAO POSSUI SIGNIFICANCIA ESTATISTICA 
#MODELO EXPLICA 81,29% DA VARIABILIDADE EM RELACAO A VARIAVEL RESPOSTA 
fit <- lm(price ~ horsepower + length + engine.size + city.mpg, data=dados)
summary(fit)
par(mfrow=c(2,2))
plot(fit)

#ATUALIZANDO O MODELO REMOVENDO VARIAVEL CITY.MPG
#MODELO EXPLICA 81,33% DA VARIABILIDADE EM RELACAO A VARIAVEL RESPOSTA 
fit2 <- update(fit, . ~ . - city.mpg)
summary(fit)
par(mfrow=c(2,2))
plot(fit)

#ATUALIZANDO O MODELO REMOVENDO VARIAVEL CITY.MPG
#MODELO EXPLICA 80,17% DA VARIABILIDADE EM RELACAO A VARIAVEL RESPOSTA 
fit3 <- update(fit2, . ~ . - horsepower)
summary(fit2)
par(mfrow=c(2,2))
plot(fit2)

#TESTE DE HOMOSTICIDADE, RESULTADO DO TESTE REJEITASSE 
#H0 DE QUE O MODELO POSSUI VARIANCIA CONSTANTE, POIS P -VALUE <0,05
bptest(fit)


#TESTE DOS RESIDUOS
#neste modelo, o valor de p para todos os testes  é menor que 0,05,
#então podemos rejeitar o nulo de que os erros não são normalmente distribuídos.
ols_test_normality(fit)

#neste modelo, o valor de p para todos os testes  é menor que 0,05,
#então podemos rejeitar o nulo de que os erros não são normalmente distribuídos.
ols_test_normality(fit2)

#neste modelo, o valor de p para todos os testes (exceto para o Kolmogorov-Smirnov, que está bem na fronteira) é menor que 0,05,
#então podemos rejeitar o nulo de que os erros não são normalmente distribuídos.
ols_test_normality(fit3)

#Como vemos no teste nosso modelo passa no teste de Heterocedasticidade e da Função Link (nosso variável resposta é contínua),
#mas falha na normalidade dos resíduos e na combinação linear das preditoras.
summary(gvlma(fit3))


#TRANSFORMACAO BOX-COX###########
library(nortest)
library(lawstat)
library(MASS)


#NAO EXISTE NORMALIDADE NOS DADOS
lillie.test(dados$price)

ggplot(dados, aes(x=price))+
  geom_histogram(aes(y=..density..),bins = 10, colour ="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")
#ANALISANDO HOMOSTICIDADE DAS VARIAVEIS 
levene.test(dados$price,dados$length)
levene.test(dados$price,dados$engine.size)
levene.test(dados$price,dados$horsepower)



boxcox(price~length + engine.size, data = dados, lam = seq(-1,1,1/10))

priceT =(dados$price^(-0.25)-1)/-0.25
priceT

lillie.test(priceT)


library(ggstatsplot)

#LISTA DE OUTLIER 

boxplot(dados$price) $out

x = c(30760, 41315, 36880, 32250, 35550, 36000, 31600, 34184, 35056,
            40960, 45400 ,32528, 34028, 37028)

library(dplyr)
library(moments)
library(ggpubr)


#REMOVENDO OS OUTILIER DA VARIAVEL RESPOSTA
n = length(x)
n

newdados

for(i in 1:n)
{ newdados= newdados[!grepl(x[i],newdados$price),]
print(x[i])
}
#TRANSFORMANDO VARIAVEIS COM BOXCOX  
boxplot(newdados$price) $out

boxcox(price~length + engine.size + horsepower + city.mpg, data = newdados, lam = seq(0.2, 0.4, by=0.01))
priceNew =(newdados$price^(0.24)-1)/0.24
priceNew

lillie.test(priceNew)

newdados$price2 <- priceNew
newdados

ggplot(newdados, aes(x=price2))+
  geom_histogram(aes(y=..density..),bins = 10, colour ="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")

#CRIANDO NOVOS MODELOS
fit_1 <- lm(price2 ~ horsepower + length + engine.size + city.mpg, data=newdados)
summary(fit_1)
par(mfrow=c(2,2))

fit_2 <- update(fit_1, . ~ . - city.mpg)
summary(fit_2)
par(mfrow=c(2,2))
plot(fit_2)


fit_3 <- update(fit_2, . ~ . -  horsepower)
summary(fit_3)
par(mfrow=c(2,2))
plot(fit_3)

#Como vemos no teste nosso modelo passa em todos os teste de Heterocedasticidade e da Função Link (nosso variável resposta é contínua),
#de normalidade dos resíduos e na combinação linear das preditoras.
#MODELO EXPLICA #75% DA VARIABILIDADE EM RELACAO A VARIAVEL RESPOSTA
#ACREDITO QUE PARA TER UM MELHOR NIVEL DE ACURACCY É NECESSÁRIO MAIS VARIAVEIS E UMA MAIOR COLETA DE DADOS
summary(gvlma(fit_3))

  