
install.packages("tidyverse")
install.packages("readxl")
install.packages("forecast")
install.packages("TSA")
install.packages("trend")
install.packages("readr")
install.packages("httr")

library(tidyverse)  
library(readxl)      
library(forecast)    
library(TSA)         
library(trend) 
library(readr)
library(httr)
library(dplyr)
library(tidyr)
source("funcoes.R")

url2004 <- "https://aplicacoes.mds.gov.br/sagi/servicos/misocial?fq=anomes_s:2004*&fq=tipo_s:mes_mu&wt=csv&q=*&fl=ibge:codigo_ibge,anomes:anomes_s,qtd_familias_beneficiarias_bolsa_familia,valor_repassado_bolsa_familia&rows=10000000&sort=anomes_s%20asc,%20codigo_ibge%20asc"
r2004 <- httr::GET(url2004)

dados_pbf2004 <- readr::read_csv(
  file = I(httr::content(r2004, "text", encoding = "UTF-8"))
)
colnames(dados_pbf2004)
str(dados_pbf2004)
dados_pbf2004 <- dados_pbf2004 |>
  dplyr::filter(
    ibge == 510340
  )

dados_pbf2004 <- dados_pbf2004 |>
  dplyr::rename(
    cod_ibge = ibge
  )

url2005 <- "https://aplicacoes.mds.gov.br/sagi/servicos/misocial?fq=anomes_s:2005*&fq=tipo_s:mes_mu&wt=csv&q=*&fl=ibge:codigo_ibge,anomes:anomes_s,qtd_familias_beneficiarias_bolsa_familia,valor_repassado_bolsa_familia&rows=10000000&sort=anomes_s%20asc,%20codigo_ibge%20asc"
r2005 <- httr::GET(url2005)

dados_pbf2005 <- readr::read_csv(
  file = I(httr::content(r2005, "text", encoding = "UTF-8"))
)
colnames(dados_pbf2005)

dados_pbf2005 <- dados_pbf2005 |>
  dplyr::filter(
    ibge == 510340
  )
dados_pbf2005 <- dados_pbf2005 |>
  dplyr::rename(
    cod_ibge = ibge
  )

url2006 <- "https://aplicacoes.mds.gov.br/sagi/servicos/misocial?fq=anomes_s:2006*&fq=tipo_s:mes_mu&wt=csv&q=*&fl=ibge:codigo_ibge,anomes:anomes_s,qtd_familias_beneficiarias_bolsa_familia,valor_repassado_bolsa_familia&rows=10000000&sort=anomes_s%20asc,%20codigo_ibge%20asc"
r2006 <- httr::GET(url2006)
dados_pbf2006 <- readr::read_csv(
  file = I(content(r2006, "text", encoding = "UTF-8"))
)

dados_pbf2006 <- dados_pbf2006 |>
  dplyr::filter(
    ibge == 510340
  )
dados_pbf2006 <- dados_pbf2006 |>
  dplyr::rename(
    cod_ibge = ibge
  )

url2007 <- "https://aplicacoes.mds.gov.br/sagi/servicos/misocial?fq=anomes_s:2007*&fq=tipo_s:mes_mu&wt=csv&q=*&fl=ibge:codigo_ibge,anomes:anomes_s,qtd_familias_beneficiarias_bolsa_familia,valor_repassado_bolsa_familia&rows=10000000&sort=anomes_s%20asc,%20codigo_ibge%20asc"
r2007 <- httr::GET(url2007)

dados_pbf2007 <- readr::read_csv(
  file = I(content(r2007, "text", encoding = "UTF-8"))
)

dados_pbf2007 <- dados_pbf2007 |>
  dplyr::filter(
    ibge == 510340
  )
dados_pbf2007 <- dados_pbf2007 |>
  dplyr::rename(
    cod_ibge = ibge
  )

url2008 <- "https://aplicacoes.mds.gov.br/sagi/servicos/misocial?fq=anomes_s:2008*&fq=tipo_s:mes_mu&wt=csv&q=*&fl=ibge:codigo_ibge,anomes:anomes_s,qtd_familias_beneficiarias_bolsa_familia,valor_repassado_bolsa_familia&rows=10000000&sort=anomes_s%20asc,%20codigo_ibge%20asc"
r2008 <- httr::GET(url2008)
dados_pbf2008 <- readr::read_csv(
  file = I(content(r2008,"text",encoding = "UTF-8"))
)

dados_pbf2008 <- dados_pbf2008 |>
  dplyr::filter(
    ibge == 510340
  )

dados_pbf2008 <- dados_pbf2008 |>
  dplyr::rename(
    cod_ibge = ibge
  )


url2009 <- "https://aplicacoes.mds.gov.br/sagi/servicos/misocial?fq=anomes_s:2009*&fq=tipo_s:mes_mu&wt=csv&q=*&fl=ibge:codigo_ibge,anomes:anomes_s,qtd_familias_beneficiarias_bolsa_familia,valor_repassado_bolsa_familia&rows=10000000&sort=anomes_s%20asc,%20codigo_ibge%20asc"
r2009 <- httr::GET(url2009)
dados_pbf2009 <- readr::read_csv(
  file = I(content(r2009,"text",encoding = "UTF-8"))
)
dados_pbf2009

dados_pbf2009 <- dados_pbf2009 |>
  dplyr::filter(
    ibge == 510340
  )
dados_pbf2009 <- dados_pbf2009 |>
  dplyr::rename(
    cod_ibge = ibge
  )

url2010 <- "https://aplicacoes.mds.gov.br/sagi/servicos/misocial?fq=anomes_s:2010*&fq=tipo_s:mes_mu&wt=csv&q=*&fl=ibge:codigo_ibge,anomes:anomes_s,qtd_familias_beneficiarias_bolsa_familia,valor_repassado_bolsa_familia&rows=10000000&sort=anomes_s%20asc,%20codigo_ibge%20asc"
r2010 <- httr::GET(url2010) 
dados_pbf2010 <- readr::read_csv(
  file = I(content(r2010,"text",encoding = "UTF-8"))
)
dados_pbf2010 <- dados_pbf2010 |>
  dplyr::filter(
    ibge == 510340
  )
dados_pbf2010 <- dados_pbf2010 |>
  dplyr::rename(
    cod_ibge = ibge
  )

url2011 <- "https://aplicacoes.mds.gov.br/sagi/servicos/misocial?fq=anomes_s:2011*&fq=tipo_s:mes_mu&wt=csv&q=*&fl=ibge:codigo_ibge,anomes:anomes_s,qtd_familias_beneficiarias_bolsa_familia,valor_repassado_bolsa_familia&rows=10000000&sort=anomes_s%20asc,%20codigo_ibge%20asc"
r2011 <- httr::GET(url2011)
dados_pbf2011 <- readr::read_csv(
  file = I(content(r2011,"text",encoding = "UTF-8"))
)

dados_pbf2011 <- dados_pbf2011 |>
  dplyr::filter(
    ibge == 510340
  )
dados_pbf2011 <- dados_pbf2011 |>
  dplyr::rename(
    cod_ibge = ibge
  )
str(dados_pbf2011)
dados_pbf2011 <- dados_pbf2011 |>
  dplyr::mutate(
    cod_ibge = as.double(cod_ibge)
  ) |>
  dplyr::mutate(
    anomes = as.double(anomes)
  ) |>
  dplyr::mutate(
    qtd_familias_beneficiarias_bolsa_familia = as.double(qtd_familias_beneficiarias_bolsa_familia)
  ) |>
  dplyr::mutate(
    valor_repassado_bolsa_familia = as.double(valor_repassado_bolsa_familia)
  )


url2012 <- "https://aplicacoes.mds.gov.br/sagi/servicos/misocial?fq=anomes_s:2012*&fq=tipo_s:mes_mu&wt=csv&q=*&fl=ibge:codigo_ibge,anomes:anomes_s,qtd_familias_beneficiarias_bolsa_familia,valor_repassado_bolsa_familia&rows=10000000&sort=anomes_s%20asc,%20codigo_ibge%20asc"
r2012 <- httr::GET(url2012)

dados_pbf2012 <- readr::read_csv(
  file = I(content(r2012,"text",encoding = "UTF-8"))
)

colnames(dados_pbf2012)
str(dados_pbf2012)

dados_pbf2012 <- dados_pbf2012 |>
  dplyr::filter(
    ibge == 510340
  )

dados_pbf2012 <- dados_pbf2012 |>
  dplyr::rename(
    cod_ibge = ibge
  )

url2013 <- "https://aplicacoes.mds.gov.br/sagi/servicos/misocial?fq=anomes_s:2013*&fq=tipo_s:mes_mu&wt=csv&q=*&fl=ibge:codigo_ibge,anomes:anomes_s,qtd_familias_beneficiarias_bolsa_familia,valor_repassado_bolsa_familia&rows=10000000&sort=anomes_s%20asc,%20codigo_ibge%20asc"
r2013 <- httr::GET(url2013)

dados_pbf2013 <- readr::read_csv(
  file = I(content(r2013,"text",encoding = "UTF-8"))
)
dados_pbf2013 <- dados_pbf2013 |>
  dplyr::filter(
    ibge == 510340
  )

dados_pbf2013 <- dados_pbf2013 |>
  dplyr::rename(
    cod_ibge = ibge
  )

str(dados_pbf2013)

url2014 <- "https://aplicacoes.mds.gov.br/sagi/servicos/misocial?fq=anomes_s:2014*&fq=tipo_s:mes_mu&wt=csv&q=*&fl=ibge:codigo_ibge,anomes:anomes_s,qtd_familias_beneficiarias_bolsa_familia,valor_repassado_bolsa_familia&rows=10000000&sort=anomes_s%20asc,%20codigo_ibge%20asc"
r2014 <- httr::GET(url2014)

dados_pbf2014 <- readr::read_csv(
  file = I(content(r2014,"text",encoding = "UTF-8"))
)

dados_pbf2014 <- dados_pbf2014 |>
  dplyr::filter(
    ibge == 510340
  )

dados_pbf2014 <- dados_pbf2014 |>
  dplyr::rename(
    cod_ibge = ibge
  )

str(dados_pbf2014)

url2015 <- "https://aplicacoes.mds.gov.br/sagi/servicos/misocial?fq=anomes_s:2015*&fq=tipo_s:mes_mu&wt=csv&q=*&fl=ibge:codigo_ibge,anomes:anomes_s,qtd_familias_beneficiarias_bolsa_familia,valor_repassado_bolsa_familia&rows=10000000&sort=anomes_s%20asc,%20codigo_ibge%20asc"
r2015 <- httr::GET(url2015)

dados_pbf2015 <- readr::read_csv(
  file = I(content(r2015,"text",encoding = "UTF-8"))
)
dados_pbf2015 <- dados_pbf2015 |>
  dplyr::filter(
    ibge == 510340
  )

dados_pbf2015 <- dados_pbf2015 |>
  dplyr::rename(
    cod_ibge = ibge
  )

colnames(dados_pbf2015) == colnames(dados_pbf_serie)
str(dados_pbf2015)

url2016 <- "https://aplicacoes.mds.gov.br/sagi/servicos/misocial?fq=anomes_s:2016*&fq=tipo_s:mes_mu&wt=csv&q=*&fl=ibge:codigo_ibge,anomes:anomes_s,qtd_familias_beneficiarias_bolsa_familia,valor_repassado_bolsa_familia&rows=10000000&sort=anomes_s%20asc,%20codigo_ibge%20asc" 
r2016 <- httr::GET(url2016)

dados_pbf2016 <- readr::read_csv(
  file = I(content(r2016,"text",encoding = "UTF-8"))
)

dados_pbf2016 <- dados_pbf2016 |>
  dplyr::filter(
    ibge == 510340
  )

dados_pbf2016 <- dados_pbf2016 |>
  dplyr::rename(
    cod_ibge = ibge
  )

url2017 <- "https://aplicacoes.mds.gov.br/sagi/servicos/misocial?fq=anomes_s:2017*&fq=tipo_s:mes_mu&wt=csv&q=*&fl=ibge:codigo_ibge,anomes:anomes_s,qtd_familias_beneficiarias_bolsa_familia,valor_repassado_bolsa_familia&rows=10000000&sort=anomes_s%20asc,%20codigo_ibge%20asc"
r2017 <- httr::GET(url2017)

dados_pbf2017 <- readr::read_csv(
  file = I(content(r2017,"text",encoding = "UTF-8"))
)
str(dados_pbf2017)

dados_pbf2017 <- dados_pbf2017 |>
  dplyr::filter(
    ibge == 510340
  )

dados_pbf2017 <- dados_pbf2017 |>
  dplyr::rename(
    cod_ibge = ibge
  )

url2018 <- "https://aplicacoes.mds.gov.br/sagi/servicos/misocial?fq=anomes_s:2018*&fq=tipo_s:mes_mu&wt=csv&q=*&fl=ibge:codigo_ibge,anomes:anomes_s,qtd_familias_beneficiarias_bolsa_familia,valor_repassado_bolsa_familia&rows=10000000&sort=anomes_s%20asc,%20codigo_ibge%20asc"
r2018 <- httr::GET(url2018)

dados_pbf2018 <- readr::read_csv(
  file = I(content(r2018,"text",encoding = "UTF-8"))
)

dados_pbf2018 <- dados_pbf2018 |>
  dplyr::filter(
    ibge == 510340
  )

dados_pbf2018 <- dados_pbf2018 |>
  dplyr::rename(
    cod_ibge = ibge
  )

url2019 <- "https://aplicacoes.mds.gov.br/sagi/servicos/misocial?fq=anomes_s:2019*&fq=tipo_s:mes_mu&wt=csv&q=*&fl=ibge:codigo_ibge,anomes:anomes_s,qtd_familias_beneficiarias_bolsa_familia,valor_repassado_bolsa_familia&rows=10000000&sort=anomes_s%20asc,%20codigo_ibge%20asc"
r2019 <- httr::GET(url2019)

dados_pbf2019 <- readr::read_csv(
  file = I(content(r2019,"text",encoding = "UTF-8"))
)

dados_pbf2019 <- dados_pbf2019 |>
  dplyr::filter(
    ibge == 510340
  )

dados_pbf2019 <- dados_pbf2019 |>
  dplyr::rename(
    cod_ibge = ibge
  )
colnames(dados_pbf_serie) == colnames(dados_pbf2019)
str(dados_pbf2018)

dados_pbf_serie <- dplyr::bind_rows(dados_pbf2004, dados_pbf2005)
dados_pbf_serie <- dplyr::bind_rows(dados_pbf_serie, dados_pbf2006)
dados_pbf_serie <- dplyr::bind_rows(dados_pbf_serie, dados_pbf2007)
dados_pbf_serie <- dplyr::bind_rows(dados_pbf_serie, dados_pbf2008)
dados_pbf_serie <- dplyr::bind_rows(dados_pbf_serie, dados_pbf2009)
dados_pbf_serie <- dplyr::bind_rows(dados_pbf_serie, dados_pbf2010)
dados_pbf_serie <- dplyr::bind_rows(dados_pbf_serie, dados_pbf2011)
dados_pbf_serie <- dplyr::bind_rows(dados_pbf_serie, dados_pbf2012)
dados_pbf_serie <- dplyr::bind_rows(dados_pbf_serie, dados_pbf2013)
dados_pbf_serie <- dplyr::bind_rows(dados_pbf_serie, dados_pbf2014)
dados_pbf_serie <- dplyr::bind_rows(dados_pbf_serie, dados_pbf2015)
dados_pbf_serie <- dplyr::bind_rows(dados_pbf_serie, dados_pbf2016)
dados_pbf_serie <- dplyr::bind_rows(dados_pbf_serie, dados_pbf2017)
dados_pbf_serie <- dplyr::bind_rows(dados_pbf_serie, dados_pbf2018)
dados_pbf_serie <- dplyr::bind_rows(dados_pbf_serie, dados_pbf2019)
view(dados_pbf_serie)


dados_pbf_serie <- dados_pbf_serie |>
  dplyr::mutate(
    anomes = as.character(anomes)
  )

dados_pbf_serie <- dados_pbf_serie |>
  tidyr::separate(anomes, into = c("ano","mes"),sep = 4)

dados_pbf_serie <- dados_pbf_serie |>
  dplyr::mutate(
    ano = as.integer(ano)
  ) |>
  dplyr::mutate(
    mes = as.integer(mes)
  )

dados_pbf_serie
dados_bf <- dados_pbf_serie
attach(dados_bf)
names(dados_bf)

serie1 <- ts(qtd_familias_beneficiarias_bolsa_familia, start = c(2004,1), frequency = 12)
plot(serie1, xlab = "Período Mensal", ylab = "Quantidade de família no PBF")

plot(serie1, 
     xlab = "Período (Ano-Mês)", 
     ylab = "Quantidade de famílias no PBF",
     main = "Série Temporal do Bolsa Família",
     col = "steelblue",
     lwd = 2,
     xaxt = "n")

grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted", lwd = 1)

tempo <- time(serie1)
anos_meses <- format(as.Date(paste(rep(start(serie1)[1]:end(serie1)[1], each = 12), 
                                   1:12, "01", sep = "-")), "%Y-%m")
anos_meses <- anos_meses[1:length(serie1)]

# Define pontos para os eixos (a cada 6 meses para não ficar poluído)
idx_ticks <- seq(1, length(serie1), by = 6)
axis(side = 1, 
     at = tempo[idx_ticks], 
     labels = anos_meses[idx_ticks], 
     las = 2,
     cex.axis = 0.7)


names(dados_bf)

serie2 <- ts(valor_repassado_bolsa_familia, start = c(2004,1), frequency = 12)

plot(serie2, 
     xlab = "Período (Ano-Mês)", 
     ylab = "Valor repassado ao PBF (R$)",
     main = "Série Temporal do Valor Repassado do Bolsa Família",
     col = "darkgreen",
     lwd = 2,
     xaxt = "n")

grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted", lwd = 1)

tempo2 <- time(serie2)
anos_meses2 <- format(as.Date(paste(rep(start(serie2)[1]:end(serie2)[1], each = 12), 
                                    1:12, "01", sep = "-")), "%Y-%m")
anos_meses2 <- anos_meses2[1:length(serie2)]

idx_ticks2 <- seq(1, length(serie2), by = 6)
axis(side = 1, 
     at = tempo2[idx_ticks2], 
     labels = anos_meses2[idx_ticks2], 
     las = 2,
     cex.axis = 0.7)
