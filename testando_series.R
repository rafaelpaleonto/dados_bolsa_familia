getwd()
setwd("/home/rafaelsimoes/Documentos/Séries temporais")
save.image("/home/rafaelsimoes/Documentos/Séries temporais/dados_pbf.RData")
load(file = "/home/rafaelsimoes/Documentos/Séries temporais/dados_pbf.RData")
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
  file = I(content(r2010,"text",enconding = "UTF-8"))
)
dados_pbf2010 <- dados_pbf2010 |>
  dplyr::filter(
    ibge == 510340
  )
dados_pbf2010 <- dados_pbf2010 |>
  dplyr::rename(
    cod_ibge = ibge
  )
 
url2011 <- "https://aplicacoes.mds.gov.br/sagi/servicos/misocial?fq=anomes_s:2011*&fq=tipo_s:mes_mu&wt=csv&q=*&fl=ibge:codigo_ibge,anomes:anomes_s,qtd_familias_beneficiarias_bolsa_familia,valor_repassado_bolsa_familia&rows=10000000&sort=anomes_s%20asc,%20codigo_ibge%20asc0"
r2011 <- httr::GET(url2011)
dados_pbf2011 <- readr::read_csv(
  file = I(content(r2011,"text",enconding = "UTF-8"))
)

dados_pbf2011 <- dados_pbf2011 |>
  dplyr::filter(
    ibge == 510340
  )
dados_pbf2011 <- dados_pbf2011 |>
  dplyr::rename(
    cod_ibge = cod_rename
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

colnames(dados_pbf2011) == colnames(dados_pbd_serie)
colnames(dados_pbf2010) == colnames(dados_pbd_serie)
colnames(dados_pbf2009) == colnames(dados_pbd_serie)
colnames(dados_pbf2008) == colnames(dados_pbd_serie)
colnames(dados_pbf2007) == colnames(dados_pbd_serie)
colnames(dados_pbf2006) == colnames(dados_pbd_serie)
str(dados_pbd_serie)
dados_pbd_serie <- dplyr::bind_rows(dados_pbf2004,   dados_pbf2005)
dados_pbd_serie <- dplyr::bind_rows(dados_pbd_serie, dados_pbf2006)
dados_pbd_serie <- dplyr::bind_rows(dados_pbd_serie, dados_pbf2007)
dados_pbd_serie <- dplyr::bind_rows(dados_pbd_serie, dados_pbf2008)
dados_pbd_serie <- dplyr::bind_rows(dados_pbd_serie, dados_pbf2009)
dados_pbd_serie <- dplyr::bind_rows(dados_pbd_serie, dados_pbf2010)
dados_pbd_serie <- dplyr::bind_rows(dados_pbd_serie, dados_pbf2011)
