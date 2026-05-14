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