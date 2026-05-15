# Instalação e carregamento do pacote "zoo" se necessário
if(!require("zoo", character.only = TRUE)){
  install.packages("zoo", dependencies = TRUE)
  library(zoo, character.only = TRUE)
}  

periodograma <- function(z, fr = "default") {
  n <- length(z)  # Tamanho da série
  
  if (identical(fr, "default")) {
    # Calculando o periodograma com a FFT
    ans <- cbind((1:floor(n / 2)) / n, (Mod(fft(z))^2 / n)[2:(n %/% 2 + 1)])
  } else {
    stopifnot(is.numeric(fr))  # Verifica se fr é numérico
    
    if (length(fr) == 1) {
      madj <- fr - n
      x <- c(z, rep(0, madj))  # Zero-padding se fr for maior que n
      ans <- cbind((1:floor(fr / 2)) / fr, (Mod(fft(x))^2 / n)[2:(fr %/% 2 + 1)])
    } else {
      # Caso fr seja um vetor de frequências
      ans <- cbind(fr, Re(sapply(fr, function(f0) {
        y <- sum(z * exp((0 + 1i) * 2 * pi * f0 * seq(0, n - 1)))
        y * Conj(y)
      }) / length(z)))
    }
  }
  periodo=1/ans[,1]
  ans[,1]=periodo
  colnames(ans) <- c("periodo", "intensidade")  # Nomeia as colunas do resultado
  ans=as.data.frame(ans)
  ans <- ans[order(ans$periodo), ] 
  return(ans)  # Retorna o resultado como um data.frame
}


Fisher.test <- function(P) {
  spec <- P[,2]
  p <- which.max(P[,2] == max(spec, na.rm = TRUE))
  g <- max(spec,na.rm = TRUE) / sum(spec, na.rm = TRUE)
  n <- nrow(P)
  periodo <- round(P[p,1],2)
  x <- floor(1/g)
  
  i <- 1:x
  valorp <- sum(choose(n, i) * (-1)^(i-1) * (1 - i * g)^(n-1))
  
  if (is.na(valorp) || valorp > 1) valorp <- 1
  if (periodo <3) valorp <- 1
  # Formatar o valor-p corretamente
  if (valorp < 0.0001) {
    valorp_formatted <- "<0.0001"
  } else {
    valorp_formatted <- sprintf("%.4f", valorp)
  }
  
  
  # Organizar os resultados
  return(list(g=round(g, 5),"valor-p"=valorp_formatted,periodo=periodo))
}

# Função para realizar o Teste das Somas Acumuladas
cusum_test <- function(Z1, Z2) {
  # Função interna para calcular o periodograma
  calc_periodogram <- function(series) {
    n <- length(series)
    periodogram <- abs(fft(series) / sqrt(n))^2
    freq <- (0:(n-1)) / n
    list(periodogram = periodogram[1:(n/2)], freq = freq[1:(n/2)])
  }
  # Passo 1: Calcular os periodogramas I1 e I2
  periodogram_1 <- calc_periodogram(Z1)
  periodogram_2 <- calc_periodogram(Z2)
  # Passo 2: Calcular a razão dos periodogramas
  J <- periodogram_1$periodogram / periodogram_2$periodogram
  # Passo 3: Calcular zi = ln(1 + 1/J)
  z <- log(1 + 1/J)
  # Passo 4: Calcular as somas acumuladas cj
  c <- cumsum(z)
  # Passo 5: Obter as estatísticas oj = cj / cm
  o <- c / max(c)
  # Passo 6: Comparar a distribuição dos oj com a distribuição U(0, 1) usando o teste de Kolmogorov-Smirnov
  ks_test <- ks.test(o, "punif")
  
  # Retornar o p-valor do teste
  return(ks_test)
}

# Função para plotar o Teste de Box-Pierce
box_pierce_plot <- function(residuals) {
  # Vetor para armazenar os p-valores, com um valor inicial para lag 0
  n <- length(residuals)
  
  max_lag <- floor(10 * log10(n))
  p_values <- numeric(max_lag)
  p_values[1] <- NA  # Define o p-valor para lag 0 como NA
  
  # Calculando o p-valor para cada lag de 1 até max_lag
  for (lag in 1:max_lag) {
    test <- Box.test(residuals, lag = lag, type = "Box-Pierce")
    p_values[lag + 1] <- test$p.value
  }
  
  # Plotando os p-valores
  plot(0:max_lag, p_values,  pch = 19, col = "blue",
       xlab = "Lag", ylab = "valor-p", main = "Teste de Box-Pierce",
       ylim = c(0, 1))  # Ajusta o limite do eixo y
  
  # Linha de significância de 0,05
  abline(h = 0.05, col = "red", lty = 2)
  text(max_lag / 2, 0.06, "Significance level 0.05", col = "red", pos = 3)
  
  # Adicionando grid
  grid()
}


# Função para plotar as previsões
plot_pred <- function(model, h = 10, nivel_conf = 0.95) {
  # Obter as previsões e os intervalos de confiança
  pred1 <- forecast(model, h = h, level = nivel_conf * 100)
  
  # Valores ajustados na série existente
  mu <- as.numeric(model$fitted)
  LI <- rep(NA, length(mu))  # Limite inferior (NA para os valores ajustados)
  LS <- rep(NA, length(mu))  # Limite superior (NA para os valores ajustados)
  
  # Criar data frame para os valores ajustados
  data.pred1 <- data.frame(mu, LI, LS)
  colnames(data.pred1) <- c("mu", "LI", "LS")
  
  # Adicionar previsões ao data frame
  zz <- as.data.frame(pred1)
  colnames(zz) <- c("mu", "LI", "LS")
  data.pred1 <- rbind(data.pred1, zz)
  
  # Criar sequência de tempo baseada na frequência da série
  freq <- frequency(model$x)
  tempo <- time(model$x)
  tempo <- as.Date(as.yearmon(tempo))  # Converter para formato de data
  
  # Ajuste para a frequência correta (mensal, trimestral, semanal, anual)
  if (freq == 12) {
    # Mensal
    data.pred1$T <- seq(tempo[1], by = "month", length.out = nrow(data.pred1))
  } else if (freq == 4) {
    # Trimestral
    data.pred1$T <- seq(tempo[1], by = "quarter", length.out = nrow(data.pred1))
  } else if (freq == 52) {
    # Semanal
    data.pred1$T <- seq(tempo[1], by = "week", length.out = nrow(data.pred1))
  } else if (freq == 1) {
    # Anual
    data.pred1$T <- seq(tempo[1], by = "year", length.out = nrow(data.pred1))
  } else if(freq == 365) {
    # Outra frequência (como diária)
    data.pred1$T <- seq(tempo[1], by = "day", length.out = nrow(data.pred1))
  }
  
  # Adicionar a série original e previsões ao data frame
  Y1 <- as.numeric(model$x)
  np <- length(pred1$mean)
  X1 <- rep(NA, np)  # Preencher os valores futuros com NA
  data.pred1$Y <- c(Y1, X1)
  
  # Criar o gráfico
  p <- ggplot(data = data.pred1, aes(x = T)) +
    geom_ribbon(aes(ymin = LI, ymax = LS), fill = "grey70") +  # Intervalo de confiança
    geom_point(aes(y = Y), color = "blue") +  # Pontos dos valores reais em azul
    geom_line(aes(y = Y), color = "blue") +  # Linha dos valores reais em azul
    geom_line(aes(y = mu), color = "red") +  # Linha das previsões em vermelho
    theme_bw() +
    labs(y = "Valores", x = "Tempo") +
    ggtitle("Valores Reais e Previsões com Intervalo de Confiança") +
    scale_x_date(date_labels = "%m-%Y")
  
  # Garantir que o gráfico seja impresso
  print(p)
  
  return( pred1)
}
