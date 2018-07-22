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

#Baixando os Dados: Gap, Câmbio e Selic
CDS_Brazil <- read_excel("CDS_Brazil.xlsx", 
                         col_types = c("date", "numeric", "numeric")) ### Import CDS
CDS_Brazil <- na.omit(CDS_Brazil) ### Excluding NA's
colnames(CDS_Brazil)[1] <- "Dates" ### Setting the first column to Dates name
EMBI <- read_excel("EMBI.xlsx", col_types = c("date", "numeric", "numeric"))  ### Import EMBI
EMBI <- na.omit(EMBI) ## Excluding NA's
colnames(EMBI)[1] <- "Dates" ### Setting the first column to Dates name
dados <- inner_join(CDS_Brazil,EMBI)
volatilidade_historica <- read_excel("volatilidade historica.xlsx", 
                                     col_types = c("date", "numeric", "blank")) ### Import Hist vol
volatilidade_historica <- na.omit(volatilidade_historica) ## Excluding NA's
colnames(volatilidade_historica)[1] <- "Dates" ### Setting the first column to Dates name
dados <- inner_join(dados,volatilidade_historica)
Volatilidade_Implicita <- read_excel("Volatilidade Implicita.xlsx", 
                                     col_types = c("date", "numeric", "numeric")) ### Import impli vol
Volatilidade_Implicita <- na.omit(Volatilidade_Implicita) ## Excluding NA's
dados <- inner_join(dados,Volatilidade_Implicita)

### Downloadign data
cdgdata <- read.csv2("Códigos BETS.csv", sep = "\t")
codes <- as.character(cdgdata$codigo)
nomes <- as.character(cdgdata$Nome)
tempdata <- 0
data <- NULL
for(i in 1:length(codes)){
  tempdata <- BETSget(codes[i])
  data <-  cbind(data,tempdata)
}
colnames(data) <- nomes[1:ncol(data)]

data <- window(data, start=c(2002,1), end= c(2017,12))

#Definindo as variáveis
selic <- data[,1]
cambio <- data[,3]
vcambio <- diff(cambio)
vcambio2 <- diff(log(cambio))
#calculando o hiato do produto
prodind <- ts(data[,2], start = 2002, frequency = 12)
saz <- seas(x = prodind)
prodinddsaz <- saz$series$s11
prodindhp <- hpfilter(prodinddsaz,freq=14400,type=c("lambda"),drift=FALSE)
prodindtrend <- prodindhp$trend
hiato <- ((prodind - prodindtrend)/prodindtrend)*100
hiato <- log(prodind)-log(prodindtrend)

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
  dplyr::rename(Datas = date,Ano_referencia=reference_year,Media = media)%>%
  group_by(Datas) %>%
  filter(Ano_referencia %in% c(year(Datas),year(Datas)+1))

igpmdi <- infla %>% select(indic,date,reference_year,mean) %>%
  filter(indic == "IGP-DI") %>%
  group_by(year(date),month(date),reference_year) %>%
  summarise(media = mean(mean)) %>%
  ungroup() %>%
  mutate(date = as.yearmon(paste0(`year(date)`,"-", `month(date)`))) %>%
  select(date,reference_year,media) %>%
  dplyr::rename(Datas = date,Ano_referencia=reference_year,Media = media)%>%
  group_by(Datas) %>%
  filter(Ano_referencia %in% c(year(Datas),year(Datas)+1))

metas <- read.csv("Metas.csv")
metas <- metas %>% select(-X)
metas <- gather(metas)
datas <- seq(as.Date("2002-01-01"),as.Date("2017-12-01"), by = "month")
datas <- as.yearmon(datas)
metas$key <- datas
colnames(metas) <- c("Data","Meta")

#calculo do desvio da meta
desviott <- data.frame(1:12,0)
metas <- read.csv("Metas.csv", header = T, stringsAsFactors = F)[,-1]
for(i in 2002:2016){
  desviott[,1] <- eval(as.symbol(paste0("mediam",i)))[,1] - metas[,paste0("X",i)]
  desviott[,2] <- eval(as.symbol(paste0("mediam",i)))[,2] - metas[,paste0("X",i+1)]
  assign(paste0("desviom",i),desviott)
}

#variavel DT
Dt <- as.data.frame(0)
for(i in 2002:2016){
  for(j in 1:12){
    Dt[j,1] <- eval(as.symbol(paste0("desviom",i)))[j,1]*((12-j)/12) + eval(as.symbol(paste0("desviom",i)))[j,2]*(j/12)
    assign(paste0("Dt",i),Dt)
  }
}

Dt <- rbind.data.frame(Dt2002,Dt2003,Dt2004,Dt2005,Dt2006,Dt2007,Dt2008,Dt2009,Dt2010,Dt2011,
                       Dt2012,Dt2013,Dt2014,Dt2015,Dt2016)
Dt <- ts(Dt, start = 2002, frequency = 12)

rm(list=c("Desvio2002","Desvio2003","Desvio2004","Desvio2005","Desvio2006", "Desvio2007",  "Desvio2008", 
          "Desvio2009", "Desvio2010","Desvio2011","Desvio2012", "Desvio2013","Desvio2014","Desvio2015",
          "Desvio2016", "mediam2002","mediam2003","mediam2004", 
          "mediam2005", "mediam2006",  "mediam2007", "mediam2008",  "mediam2009",  
          "mediam2010","mediam2011","mediam2012","mediam2013","mediam2014","mediam2015",
          "mediam2016","mdtt","tempdata","desviott","desviom2002","desviom2003","desviom2004",
          "desviom2005","desviom2006","desviom2007", "desviom2008", "desviom2009", "desviom2010",
          "desviom2011","desviom2012", "desviom2013", "desviom2014", "desviom2015", "desviom2016",
          "Dt2002","Dt2003","Dt2004","Dt2005","Dt2006","Dt2007","Dt2008","Dt2009","Dt2010","Dt2011",
          "Dt2012","Dt2013","Dt2014","Dt2015","Dt2016"))

####unindo variáveis####
variaveis1 <- cbind(selic,vcambio,hiato,Dt)
variaveis1 <- window(variaveis1, start = c(2002,2), end = c(2016,12))
variaveis2 <- cbind(diff(selic), vcambio, diff(hiato), diff(Dt))
variaveis3 <- cbind(selic,cambio,hiato,Dt)
############# Variaveis Variação percentual
dpselic <- (selic/lag(selic,-1) - 1)*100 
dpcambio <- (cambio/lag(cambio,-1) - 1)*100
dphiato <- (hiato/lag(hiato,-1) - 1)*100
dplDt <- (Dt/lag(Dt,-1) - 1)*100
variaveis4 <- cbind(dpselic,dpcambio,dphiato,dplDt)
#### estimando o modelo
set.seed(300)
bv1 <- bvar.sv.tvp(variaveis1, p = 2, nf = 2, tau = 40)
bv3 <- bvar.sv.tvp(variaveis2, p = 2, nf = 2, tau = 40)
bv4 <- bvar.sv.tvp(variaveis1, p = 2, nf = 2, tau = 36)
bv5 <- bvar.sv.tvp(variaveis3, p = 2, nf = 2, tau = 40)
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

tml <- paste0(floor(time(variaveis3)), "M", (1 + round(12*(time(variaveis3) - floor(time(variaveis3))))))
# Reação considerando os Presidentes do BACEN
all_dts <- c("2006M1","2011M1")
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
# Reação dos Desvios das Expectativas Presidentes do BACEN ##################
all_dts <- c("2006M1","2011M1")
for (rr in 1:4){
  tmp <- list()
  for (dd in all_dts){
    t <- which(tml == dd) -42
    t_ind <- which(all_dts == dd)
    # Compute impulse responses
    aux <- impulse.responses(bv1, impulse.variable = rr,
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
