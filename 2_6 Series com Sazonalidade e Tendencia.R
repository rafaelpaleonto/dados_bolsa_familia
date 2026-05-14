#=============================================================
# CARREGAMENTO DE PACOTES
#=============================================================
library(tidyverse)   # Manipulação e visualização de dados
library(readxl)      # Leitura de arquivos Excel
library(forecast)    # Funções para análise de séries temporais
library(TSA)         # Métodos adicionais para séries temporais
library(trend)       # Testar de Tendencia

#-------------------------------------------------------------
# DEFINIÇÃO DO DIRETÓRIO DE TRABALHO
# Menu: Session -> Set Working Directory -> Choose Directory
#-------------------------------------------------------------

#-------------------------------------------------------------
# Carregar funções auxiliares, não implementadas em pacotes
#-------------------------------------------------------------
source("funcoes.R") 

#=============================================================
# GERAÇÃO DE ELETRICIDADE NOS EUA
#=============================================================

# Importação dos dados
dados1=read_excel("eletricidade.xlsx", sheet = 1)

#Anexar base de dados ao ambiente, permite acessar as variáveis diretamente
attach(dados1)

#Visualização nomes das variáveis
names(dados1)

# Conversão para série temporal
y1 <- ts(eletri, start = 1973, frequency = 12)

# Visualização da série
plot(y1, xlab = "Tempo",ylab = "Eletricidade gerada (milhões de kWh)")

#-------------------------------------------------------------
# ANÁLISE DA ESTABILIDADE DA VARIÂNCIA (BOX-COX)
#-------------------------------------------------------------
lambda <- BoxCox.ar(y1, lambda = seq(-2, 2, 0.01), method = "yw")
lambda$mle
lambda$ci

# Aplicação da transformação de Box-Cox
y2 <- BoxCox(y1, lambda$mle)
y2 <- as.ts(y2)

# Visualização da série transformada
plot(y2, main = "Série transformada (Box-Cox)")

#-------------------------------------------------------------
# TESTE DE TENDENCIA
#-------------------------------------------------------------

cs.test(y2)

#-------------------------------------------------------------
# REMOÇÃO DE TENDENCIA
#-------------------------------------------------------------

# Aplicar a diferença na série
y3=diff(y2)

#Testar se a série diferenciada ainda apresenta tendencia
cs.test(y3) 

#-------------------------------------------------------------
# TESTE DE SAZONALIDADE
#-------------------------------------------------------------

#obter periodograma
P=periodograma(y3)
plot(P,type="l")

#Teste de fisher
Fisher.test(P)

#-------------------------------------------------------------
# REMOÇÃO DE SAZONALIDADE
#-------------------------------------------------------------

# Aplicar a diferença na série no periodo indicado
y4=diff(y3,lag=6,diff=1)


#Testar se a série diferenciada ainda sazonalidade
P=periodograma(y4)
plot(P,type="l")
Fisher.test(P)

par(mfrow = c(1,2))
plot(y1, xlab = "Tempo",ylab = "Eletricidade gerada (milhões de kWh)")
plot(y4,xlab = "Tempo", ylab = "série diferenciada")

#-------------------------------------------------------------
# ANÁLISE DE DEPENDÊNCIA TEMPORAL
#-------------------------------------------------------------

#Série original
Acf(y1) # Função de autocorrelação
Pacf(y1) # Função de autocorrelação parcial

#Série sem tendencia e sazonalidade
Acf(y4) # Função de autocorrelação
Pacf(y4) # Função de autocorrelação parcial

