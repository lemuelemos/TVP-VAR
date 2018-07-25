setwd("~/Documentos/TVP-VAR")
library(BETS)
library(timeSeries)
library(zoo)
library(mFilter)
library(strucchange)
library(bvarsv)
library(seasonal)
library(readxl)
library(dplyr)
library(rbcb)
library(lubridate)
library(tidyr)
library(timetk)

#Baixando os Dados: Gap, Câmbio e Selic
CDS_Brazil <- read_excel("CDS_Brazil.xlsx", 
                         col_types = c("date", "numeric", "numeric")) ### Import CDS
CDS_Brazil <- na.omit(CDS_Brazil) ### Excluding NA's
colnames(CDS_Brazil)[1] <- "index" ### Setting the first column to Dates name
CDS_Brazil %>% 
  group_by(year(index),month(index)) %>% 
  summarise(CDS5Y = mean(`BRAZIL CDS USD SR 5Y D14 Corp`)) %>% 
  ungroup() %>% 
  mutate(index = as.yearmon(paste0(`year(index)`,"-", `month(index)`))) %>%
  select(index,CDS5Y)-> CDS_Brazil

EMBI <- read_excel("EMBI.xlsx", col_types = c("date", "numeric", "numeric"))  ### Import EMBI
EMBI <- na.omit(EMBI) ## Excluding NA's
colnames(EMBI)[1] <- "index" ### Setting the first column to Dates name
EMBI %>% 
  group_by(year(index),month(index)) %>%
  summarise(EMBI = mean(`JPEIGLSP Index`)) %>%
  ungroup() %>% 
  mutate(index = as.yearmon(paste0(`year(index)`,"-", `month(index)`))) %>%
  select(index,EMBI)-> EMBI


volatilidade_historica <- read_excel("volatilidade historica.xlsx", 
                                     col_types = c("date", "numeric", "blank")) ### Import Hist vol
volatilidade_historica <- na.omit(volatilidade_historica) ## Excluding NA's
colnames(volatilidade_historica)[1] <- "index" ### Setting the first column to Dates name
volatilidade_historica %>% 
  group_by(year(index),month(index)) %>%
  summarise(VOL30 = mean(`Volatility 30 Day`)) %>%
  ungroup() %>% 
  mutate(index = as.yearmon(paste0(`year(index)`,"-", `month(index)`))) %>%
  select(index,VOL30)-> volatilidade_historica



volatilidade_implicita <- read_excel("Volatilidade Implicita.xlsx", 
                                     col_types = c("date", "numeric", "numeric")) ### Import impli vol
volatilidade_implicita <- na.omit(volatilidade_implicita) ## Excluding NA's
colnames(volatilidade_implicita)[1] <- "index"
volatilidade_implicita %>% 
  group_by(year(index),month(index)) %>%
  summarise(VOLIMPL1Y = mean(`USDBRLV1Y Curncy`)) %>%
  ungroup() %>% 
  mutate(index = as.yearmon(paste0(`year(index)`,"-", `month(index)`))) %>%
  select(index,VOLIMPL1Y)-> volatilidade_implicita



### Downloadign data
cdgdata <- read.csv2("Códigos BETS.csv", sep = "\t")
codes <- as.character(cdgdata$codigo)
nomes <- as.character(cdgdata$Nome)
tempdata <- 0
data_bets <- NULL
for(i in 1:length(codes)){
  tempdata <- BETSget(codes[i])
  data_bets <-  cbind(data_bets,tempdata)
}
colnames(data_bets) <- nomes
data_bets <- window(data_bets, start=c(2002,1), end= c(2017,12))
data_bets <- tk_tbl(data_bets)

#Definindo as variáveis
data_bets %>% select(index,`SELIC Acumulada Anualizada`) -> selic
data_bets %>% select(index,`Taxa de câmbio - Livre - Dólar americano (venda)`) -> cambio
vcambio <- diff(tk_xts(cambio,silent = T))
vcambio2 <- diff(log(tk_xts(cambio,silent = T)))
vcambio <- na.omit(vcambio)
vcambio2 <- na.omit(vcambio2)
vcambio2 <- tk_tbl(vcambio2)
cambio %>% 
  mutate(taxa_de_cambio = log(`Taxa de câmbio - Livre - Dólar americano (venda)`)) %>% 
  select(index,taxa_de_cambio) -> cambiol
#calculando o hiato do produto
prodind <- ts(data_bets$`Indicador de Produção Geral`, start = 2002, frequency = 12)
saz <- seas(x = prodind)
prodinddsaz <- saz$series$s11
prodindhp <- hpfilter(prodinddsaz,freq=14400,type=c("lambda"),drift=FALSE)
prodindtrend <- prodindhp$trend
hiato <- (log(prodinddsaz)-log(prodindtrend))*100
hiato <- tk_tbl(hiato)
####### Dados de Inflação
indic <- c("IPCA","IGP-DI")
end_date <- "2018-06-31"
start_date <- "2002-01-01"
infla <- get_annual_market_expectations(indic, 
                                        end_date = end_date, 
                                        start_date = start_date) ## baixando os dados

ipca <- infla %>% select(indic,date,reference_year,mean) %>%
  filter(indic == "IPCA") %>%
  group_by(year(date),month(date),reference_year) %>%
  summarise(media = mean(mean)) %>%
  ungroup() %>%
  mutate(date = as.yearmon(paste0(`year(date)`,"-", `month(date)`))) %>%
  select(date,reference_year,media) %>%
  dplyr::rename(Data = date,Ano_referencia=reference_year,Media = media)%>%
  group_by(Data) %>%
  filter(Ano_referencia %in% c(year(Data),year(Data)+1))

igpmdi <- infla %>% select(indic,date,reference_year,mean) %>%
  filter(indic == "IGP-DI") %>%
  group_by(year(date),month(date),reference_year) %>%
  summarise(media = mean(mean)) %>%
  ungroup() %>%
  mutate(date = as.yearmon(paste0(`year(date)`,"-", `month(date)`))) %>%
  select(date,reference_year,media) %>%
  dplyr::rename(Data = date,Ano_referencia=reference_year,Media = media)%>%
  group_by(Data) %>%
  filter(Ano_referencia %in% c(year(Data),year(Data)+1))

metas <- read.csv("Metas.csv")
metas <- metas %>% select(-X)
metas <- gather(metas)
datas <- seq(as.Date("2002-01-01"),as.Date("2017-12-01"), by = "month")
datas <- as.yearmon(datas)
metas$key <- datas
colnames(metas) <- c("Data","Meta")
metas <- tk_tbl(metas)

#calculo do desvio da meta v
ipca <- inner_join(ipca,metas, by="Data")
igpmdi <- inner_join(igpmdi,metas, by="Data")

ipca %>%
  mutate(Desvio = Media - Meta) -> ipca

igpmdi %>%
  mutate(Desvio = Media - Meta) -> igpmdi

Dt <- as.data.frame(0)
for(j in 2002:2017){
  ipca %>% 
    filter(year(Data) == j) -> data 
  for(i in 1:12){
    data %>% filter(Ano_referencia == j) -> aux1
    data %>% filter(Ano_referencia == j+1) -> aux2
    aux3 <- as.data.frame(0)
    aux3[,1] <- (aux1$Desvio[i]*((12-i)/12))+(aux2$Desvio[i]*(i/12))
    Dt <- rbind(Dt,aux3)
  }
}
Dt <- Dt[-which(Dt == 0),]
Dt <- xts::as.xts(Dt, order.by = unique(ipca$Data))
colnames(Dt) <- "Desvio_das_Expectativas"
Dt <- tk_tbl(Dt)


variaveis1 <- inner_join(selic,hiato)
variaveis1 <- inner_join(variaveis1,cambiol)
variaveis1 <- inner_join(variaveis1,Dt)
variaveis1 <- ts(variaveis1[,2:ncol(variaveis1)], start = c(2002,1), frequency = 12)
variaveis1d <- diff(variaveis1)

variaveis2 <- inner_join(selic,hiato)
variaveis2 <- inner_join(variaveis2,Dt)
variaveis2 <- inner_join(variaveis2,cambiol)
variaveis2 <- inner_join(variaveis2,volatilidade_implicita)
variaveis2 <- ts(variaveis2[,2:ncol(variaveis2)], start = c(2003,10), frequency = 12)
variaveis2d <- diff(variaveis2)

variaveis3 <- inner_join(selic,hiato)
variaveis3 <- inner_join(variaveis3,Dt)
variaveis3 <- inner_join(variaveis3,cambiol)
variaveis3 <- inner_join(variaveis3,CDS_Brazil)
variaveis3 <- inner_join(variaveis3,volatilidade_historica)
variaveis3 <- ts(variaveis3[,2:ncol(variaveis3)], start = c(2002,1), frequency = 12)
variaveis3d <- diff(variaveis3)

#### estimando o modelo
set.seed(300)
bv1 <- bvar.sv.tvp(variaveis1, p = 2, nf = 2, tau = 48)
bv1d <- bvar.sv.tvp(variaveis1d, p = 2, nf = 2, tau = 48)
bv2 <- bvar.sv.tvp(variaveis2, p = 2, nf = 1, tau = 36)
bv2d <- bvar.sv.tvp(variaveis2d, p = 2, nf = 1, tau = 36)
bv3 <- bvar.sv.tvp(variaveis3, p = 2, nf = 2, tau = 48)
bv3d <- bvar.sv.tvp(variaveis3d, p = 2, nf = 2, tau = 48)
#bv5 <- bvar.sv.tvp(variaveis3, p = 2, nf = 2, tau = 40)
################################################
################Plotando Gráficos######
matplot2 <- function(x, y, ylim, ...){
  matplot(x = x, y = y, ylim = ylim, type = "l", xlab = "", ylab = "",
          lty = 1, lwd = 2, bty = "n", ...)
}
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2",
               "#D55E00", "#CC79A7","#000000","#828282")
cols1 <- cbPalette[c(9, 10)]
cols2 <- cbPalette[c(7, 5, 4, 9, 3, 8)]
abline2 <- function(...){
  abline(..., lty = 4, lwd = 0.3)
}

tml <- paste0(year(floor(time(as.xts(metas$Meta,order.by = metas$Data)))), 
              "M", 
              (1 + round(12*(time(as.xts(metas$Meta,order.by = metas$Data)) - floor(time(as.xts(metas$Meta,order.by = metas$Data)))))))
# Reação considerando os Presidentes do BACEN
all_dts <- c("2011M1","2016M7")
for (rr in 1:4){
  tmp <- list()
  for (dd in all_dts){
    t <- which(tml == dd) -40
    t_ind <- which(all_dts == dd)
    # Compute impulse responses
    aux <- impulse.responses(bv2, impulse.variable = rr,
                             response.variable = 1, t = t,
                             scenario = 3,
                             draw.plot = FALSE, nhor = 40)$irf
    tmp[[t_ind]] <- aux
  }
  # Make data for graph in top left panel of Figure 10, 11
  gdat <- rbind(0, sapply(tmp, function(z) apply(as.matrix(z), 2, median)))
  # Configure and print plot
  if (rr == 1){
    yb <- c(-0.01, 0.4)
  } else if (rr == 2){
    yb <- c(-0.01, 0.55)
  } else if (rr == 3){
    yb <- c(-0.05, 0.15)
  } else{
    yb <- c(-0.01, 0.25)
  }
  plot_title <- if(rr == 2){
  "Reação do BACEN a Variação do Câmbio"
  } else if(rr == 3){
  "Reação do BACEN a Variação do Hiato do Produto"
  } else{
  "Reação do BACEN a Variação do Desvio da Expextativas Inflacionárias"
  }
  
  
  matplot2(x = 0:40, y = gdat, ylim = yb, col = cols1)
  abline2(v = seq(5, 60, 5), h = seq(yb[1], yb[2], 0.05))
  if (rr == 2){
    legend_loc <- "bottomright"
  } else if (rr == 3){
    legend_loc <- "topright"
  } else{
    legend_loc <- "topright"
  }
  legend(legend_loc, legend = all_dts, col = cols1, lty = 1, lwd = 2, bty = "n") 
}
# Reação dos Desvios das Expectativas Presidentes do BACEN ##################
all_dts <- c("2007M1","2016M7")
for (rr in 1:4){
  tmp <- list()
  for (dd in all_dts){
    t <- which(tml == dd) -42
    t_ind <- which(all_dts == dd)
    # Compute impulse responses
    aux <- impulse.responses(bv2, impulse.variable = rr,
                             response.variable = 4, t = t,
                             scenario = 3,
                             draw.plot = FALSE, nhor = 40)$irf
    tmp[[t_ind]] <- aux
  }
  # Make data for graph in top left panel of Figure 10, 11
  gdat <- rbind(0, sapply(tmp, function(z) apply(as.matrix(z), 2, median)))
  # Configure and print plot
  if (rr == 1){
    yb <- c(-0.08, 0.1)
  } else if (rr == 2){
    yb <- c(-0.01, 0.1)
  } else if(rr == 3){
    yb <- c(-0.15, 0.04)
  } else{
    yb <- c(-0.02,0.2)
  }
  plot_title <- if(rr == 2){
    "Reação do BACEN a Variação do Câmbio"
  } else if(rr == 3){
    "Reação do BACEN a Variação do Hiato do Produto"
  } else{
    "Reação do BACEN a Variação do Desvio da Expextativas Inflacionárias"
  }
  
  
  matplot2(x = 0:40, y = gdat, ylim = yb, col = cols1)
  abline2(v = seq(5, 40, 5), h = seq(yb[1], yb[2], 0.02))
  if (rr == 1){
    legend_loc <- "top"
  } else if (rr == 2){
    legend_loc <- "topright"
  } else if (rr == 3){
    legend_loc <- "bottomright"
  } else {
    legend_loc <- "top"
  }
  legend(legend_loc, legend = all_dts, col = cols1, lty = 1, lwd = 2, bty = "n") 
}
###################Reação do hiato
all_dts <- c("2005M7","2011M1")
for (rr in 1:4){
  tmp <- list()
  for (dd in all_dts){
    t <- which(tml == dd) -42
    t_ind <- which(all_dts == dd)
    # Compute impulse responses
    aux <- impulse.responses(bv1, impulse.variable = rr,
                             response.variable = 3, t = t,
                             scenario = 3,
                             draw.plot = FALSE, nhor = 30)$irf
    tmp[[t_ind]] <- aux
  }
  # Make data for graph in top left panel of Figure 10, 11
  gdat <- rbind(0, sapply(tmp, function(z) apply(as.matrix(z), 2, median)))
  # Configure and print plot
  if (rr == 1){
    yb <- c(-0.13, 0.5)
  } else if (rr == 2){
    yb <- c(-0.5, 0.04)
  } else if (rr == 3){
    yb <- c(-0.03, 3.5)
  } else{
    yb <- c(-0.25, 0.05)
  }
  plot_title <- if(rr == 2){
    "Reação do BACEN a Variação do Câmbio"
  } else if(rr == 3){
    "Reação do BACEN a Variação do Hiato do Produto"
  } else{
    "Reação do BACEN a Variação do Desvio da Expextativas Inflacionárias"
  }
  
  
  matplot2(x = 0:30, y = gdat, ylim = yb, col = cols1)
  abline2(v = seq(5, 50, 5), h = seq(yb[1], yb[2], 0.1))
  if (rr == 1){
    legend_loc <- "top"
  } else if (rr == 2){
    legend_loc <- "bottom"
  } else {
    legend_loc <- "bottomright"
  }
  legend(legend_loc, legend = all_dts, col = cols1, lty = 1, lwd = 2, bty = "n") 
}
###################Reação do cambio
all_dts <- c("2005M7","2011M1")
for (rr in 1:4){
  tmp <- list()
  for (dd in all_dts){
    t <- which(tml == dd) -42
    t_ind <- which(all_dts == dd)
    # Compute impulse responses
    aux <- impulse.responses(bv1, impulse.variable = rr,
                             response.variable = 2, t = t,
                             scenario = 3,
                             draw.plot = FALSE, nhor = 30)$irf
    tmp[[t_ind]] <- aux
  }
  # Make data for graph in top left panel of Figure 10, 11
  gdat <- rbind(0, sapply(tmp, function(z) apply(as.matrix(z), 2, median)))
  # Configure and print plot
  if (rr == 1){
    yb <- c(-0.01, 0.01)
  } else if (rr == 2){
    yb <- c(-0.01, 0.04)
  } else if (rr == 3){
    yb <- c(-0.01, 0.02)
  } else{
    yb <- c(-0.01, 0.02)
  }
  plot_title <- if(rr == 2){
    "Reação do BACEN a Variação do Câmbio"
  } else if(rr == 3){
    "Reação do BACEN a Variação do Hiato do Produto"
  } else{
    "Reação do BACEN a Variação do Desvio da Expextativas Inflacionárias"
  }
  
  
  matplot2(x = 0:30, y = gdat, ylim = yb, col = cols1)
  abline2(v = seq(5, 50, 5), h = seq(yb[1], yb[2], 0.1))
  if (rr == 1){
    legend_loc <- "top"
  } else if (rr == 2){
    legend_loc <- "top"
  } else {
    legend_loc <- "bottomright"
  }
  legend(legend_loc, legend = all_dts, col = cols1, lty = 1, lwd = 2, bty = "n") 
}
# Reação considerando a Crise
all_dts <- c("2005M7","2010M1")
for (rr in 1:4){
  tmp <- list()
  for (dd in all_dts){
    t <- which(tml == dd) -42
    t_ind <- which(all_dts == dd)
    # Compute impulse responses
    aux <- impulse.responses(bv1, impulse.variable = rr,
                             response.variable = 1, t = t,
                             scenario = 3,
                             draw.plot = FALSE, nhor = 40)$irf
    tmp[[t_ind]] <- aux
  }
  # Make data for graph in top left panel of Figure 10, 11
  gdat <- rbind(0, sapply(tmp, function(z) apply(as.matrix(z), 2, median)))
  # Configure and print plot
  if (rr == 1){
    yb <- c(-0.02, 0.5)
  } else if (rr == 2){
    yb <- c(-0.08, 0.01)
  } else if (rr == 3){
    yb <- c(-0.01, 0.25)
  } else{
    yb <- c(-0.01, 0.25)
  }
  plot_title <- if(rr == 2){
    "Reação do BACEN a Variação do Câmbio"
  } else if(rr == 3){
    "Reação do BACEN a Variação do Hiato do Produto"
  } else{
    "Reação do BACEN a Variação do Desvio da Expextativas Inflacionárias"
  }
  
  
  matplot2(x = 0:40, y = gdat, ylim = yb, col = cols1)
  abline2(v = seq(5, 60, 5), h = seq(yb[1], yb[2], 0.05))
  if (rr == 2){
    legend_loc <- "bottomright"
  } else if (rr == 3){
    legend_loc <- "topright"
  } else{
    legend_loc <- "topright"
  }
  legend(legend_loc, legend = all_dts, col = cols1, lty = 1, lwd = 2, bty = "n") 
}
