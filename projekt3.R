library(lubridate)
library(dplyr)
library(ggplot2)
#tu sobie mozna zmienic sciezke
sciezka <- getwd()
data_gold <- read.csv(file.path(sciezka,"xauusd_d.csv"))
data_USDPLN <- read.csv(file.path(sciezka,"usdpln_d.csv"))
data_gold$Data <- as.Date(data_gold$Data)
data_USDPLN$Data <- as.Date(data_USDPLN$Data)
#razem
data_goldPLN <- na.omit(data_gold %>% left_join(data_USDPLN, by="Data"))
data_goldPLN <- data_goldPLN %>% mutate(Zamkniecie=Zamkniecie.x*Zamkniecie.y) %>% select(Data, Zamkniecie)
data_goldPLN_train <- data_goldPLN %>% filter(Data < "2019-06-30")
data_goldPLN_test <- data_goldPLN %>% filter(Data >= "2019-06-30" & Data <= "2020-06-30")
data_goldPLN_train$zwroty <- c(NA,(data_goldPLN_train$Zamkniecie[2:nrow(data_goldPLN_train)]-data_goldPLN_train$Zamkniecie[1:(nrow(data_goldPLN_train)-1)])/data_goldPLN_train$Zamkniecie[1:(nrow(data_goldPLN_train)-1)])
rm(data_goldPLN)
#oddzielnie
data_gold_train <- data_gold %>% filter(Data < "2019-06-30") %>% select(Data, Zamkniecie)
data_gold_test <- data_gold %>% filter(Data >= "2019-06-30" & Data <= "2020-06-30") %>% select(Data, Zamkniecie)
data_USDPLN_train <- data_USDPLN %>% filter(Data < "2019-06-30") %>% select(Data, Zamkniecie)
data_USDPLN_test <- data_USDPLN %>% filter(Data >= "2019-06-30" & Data <= "2020-06-30") %>% select(Data, Zamkniecie)
data_gold_train$zwroty <- c(NA,(data_gold_train$Zamkniecie[2:nrow(data_gold_train)]-data_gold_train$Zamkniecie[1:(nrow(data_gold_train)-1)])/data_gold_train$Zamkniecie[1:(nrow(data_gold_train)-1)])
data_USDPLN_train$zwroty <- c(NA,(data_USDPLN_train$Zamkniecie[2:nrow(data_USDPLN_train)]-data_USDPLN_train$Zamkniecie[1:(nrow(data_USDPLN_train)-1)])/data_USDPLN_train$Zamkniecie[1:(nrow(data_USDPLN_train)-1)])
rm(data_gold, data_USDPLN)

dt <- 1/nrow(data_gold_test)

GBM <- function(S0, drift, volatility, stocks_names, correlation = -100, dt = 1/252, t0 = 0, Time = 1){
  result <- list()
  if((length(drift) != length(volatility)) | (length(drift) != length(S0)) | (length(drift) != length(stocks_names)))
  {
    stop("Dlugosci dryfu, zmiennosci, S0 sie nie zgadzaja")
  }
  if(sum(correlation != -100)>0)
  {
    if(length(drift) != (length(correlation) + 1))
      stop("Dlugosci dryfu i korelacji sie nie zgadzaja")
  }

  t <- seq(t0, Time, by = dt)
  X_main <- c(0, rnorm(length(t) - 1, 0, 1))
  result[[ stocks_names[1] ]] <- S0[1]*exp( (drift[1] - 0.5*volatility[1]^2)*t + volatility[1]*cumsum(X_main*sqrt(dt)))
  if( sum(correlation == -100) ){
    return(result)
  }
  else{
    for (j in 1:length(correlation)) {
      Y <- correlation[j]*X_main + sqrt(1 - correlation[j]^2)*c(0, rnorm(length(t) - 1, 0, 1))
      result[[ stocks_names[j+1] ]] <- S0[j+1]*exp( (drift[j+1] - 0.5*volatility[j+1]^2)*t + volatility[j+1]*cumsum(Y*sqrt(dt)))
    }
  }
  return(result) #zwracana jest lista
}


generate_trajectories <- function(simulations_number, stocks_names, method, args){
  trajectories <- list()
  replicator <- replicate(simulations_number, eval(parse(text = paste0(method,"(",args,")"))))
  df <- data.table(matrix(unlist(replicator), ncol = length(replicator), byrow = F))

  for (k in 1:length(stocks_names)) {
    indexes <- seq(k, length(replicator), length(stocks_names))
    trajectories[[ stocks_names[k] ]] <- df[, indexes, with = FALSE]
  }
  return(trajectories)
}

quantile_plot <- function(lista, dane_historyczne = NULL, quantiles)
{

  library(ggplot2)
  library(ggthemes)
  kwantyle <- apply(lista, 1, function(y) {
    quantile(y, probs = quantiles)
  })
  kwantyle_vec <- as.vector(t(kwantyle))
  n <- length(lista[[1]])
  napis <- paste0("kwantyl ", quantiles*100, "%")
  n1 <- length(napis)
  cena <- kwantyle_vec
  dane_historyczne <- c(lista$V1[1], dane_historyczne)
  if(length(dane_historyczne) != 0)
  {
    n1 <- n1 + 1
    cena <- c(dane_historyczne, cena)
    napis <- c("dane historyczne", napis)
  }
  to_plot <- data.frame(dzien = rep(1:n, n1), cena = cena, grupa = rep(napis, times = rep(n, n1)))
  plot <- ggplot(data=to_plot) + geom_line(aes(x=dzien, y=cena, colour = grupa)) + theme_wsj() + ggtitle("Kwantyle GBM")
  return(plot)
}
# 
# 
# 
# estymuj_dryf_zmiennosc_przedzialy <- function(dane,ile_okresow)
# {
#   dane <- dane %>% mutate(data_cut=cut(year(Data), ile_okresow))
#   tmp_dryf <- c()
#   tmp_zmiennosc <- c()
#   for(okres in levels(dane$data_cut))
#   {
#     tmp_dryf <- c(tmp_dryf,dane %>% filter(data_cut==okres) %>% select(zwroty) %>% pull() %>% mean(na.rm=T))
#     tmp_zmiennosc <- c(tmp_zmiennosc,dane %>% filter(data_cut==okres) %>% select(zwroty) %>% pull() %>% sd(na.rm=T))
#   }
#   return(data.frame(dryf=tmp_dryf,zmiennosc_roczna=tmp_zmiennosc,okres=levels(dane$data_cut)))
# }
# 
# 
# estymuj_dryf_zmiennosc_do_teraz <- function(dane)
# {
#   dane <- dane %>% mutate(rok=year(Data))
#   tmp_dryf <- c()
#   tmp_zmiennosc <- c()
#   for(okres in unique(dane$rok))
#   {
#     tmp_dryf <- c(tmp_dryf,dane %>% filter(rok>=okres) %>% select(zwroty) %>% pull() %>% mean(na.rm=T))
#     tmp_zmiennosc <- c(tmp_zmiennosc,dane %>% filter(rok>=okres) %>% select(zwroty) %>% pull() %>% sd(na.rm=T))
#   }
#   return(data.frame(dryf=tmp_dryf,zmiennosc_roczna=tmp_zmiennosc,okres=paste0("[",unique(dane$rok)," ; 2019]")))
# }
# 
# estymuj_korelacje_przedzialy <- function(dane1, dane2, ile_okresow)
# {
#   dane1 <- dane1 %>% mutate(data_cut = cut(year(Data), ile_okresow))
#   dane2 <- dane2 %>% mutate(data_cut = cut(year(Data), ile_okresow))
#   tmp_cor <- c()
#   for(okres in levels(dane1$data_cut))
#   {
#     tmp_cor <- c(tmp_cor,cor(dane1 %>% filter(data_cut==okres) %>% select(zwroty) %>% pull(), dane2 %>% filter(data_cut==okres) %>% select(zwroty) %>% pull(), method="spearman"))
#   }
#   return(data.frame(cor=tmp_cor,okres=levels(dane1$data_cut)))
# }
# 
# estymuj_korelacje_do_teraz <- function(dane1, dane2)
# {
#   dane1 <- dane1 %>% mutate(rok=year(Data))
#   dane2 <- dane2 %>% mutate(rok=year(Data))
# 
#   tmp_cor <- c()
#   for(okres in unique(dane1$rok))
#   {
#     tmp_cor <- c(tmp_cor,cor(dane1 %>% filter(rok>=okres) %>% select(zwroty) %>% pull(), dane2 %>% filter(rok>=okres) %>% select(zwroty) %>% pull(), method = 'spearman' ))
#   }
#   return(data.frame(cor=tmp_cor,okres=paste0("[",unique(dane1$rok)," ; 2019]")))
# }
# 
# 
# ################################
# #########DOBOR OKRESU###########
# ################################
# 
# 
# dryf_zmiennosc_przedzialy <- estymuj_dryf_zmiennosc_przedzialy(data_goldPLN_train, 18)
# ggplot(data=dryf_zmiennosc_przedzialy) + geom_point(aes(x=okres,y=dryf,colour=okres)) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# ggplot(data=dryf_zmiennosc_przedzialy) + geom_point(aes(x=okres,y=zmiennosc_roczna,colour=okres)) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# 
# dryf_zmiennosc_do_teraz <- estymuj_dryf_zmiennosc_do_teraz(data_goldPLN_train)
# ggplot(data=dryf_zmiennosc_do_teraz) + geom_point(aes(x=okres,y=dryf,colour=okres)) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# ggplot(data=dryf_zmiennosc_do_teraz) + geom_point(aes(x=okres,y=zmiennosc_roczna,colour=okres)) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# 
# #dla GOLDPLN 2018 - dryf roczny i 2017 - zmiennosc roczna
# 
# 
# dryf_zmiennosc_przedzialy <- estymuj_dryf_zmiennosc_przedzialy(data_gold_train, 20)
# ggplot(data=dryf_zmiennosc_przedzialy) + geom_point(aes(x=okres,y=dryf,colour=okres)) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# ggplot(data=dryf_zmiennosc_przedzialy) + geom_point(aes(x=okres,y=zmiennosc_roczna,colour=okres)) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# 
# dryf_zmiennosc_do_teraz <- estymuj_dryf_zmiennosc_do_teraz(data_gold_train)
# ggplot(data=dryf_zmiennosc_do_teraz) + geom_point(aes(x=okres,y=dryf,colour=okres)) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# ggplot(data=dryf_zmiennosc_do_teraz) + geom_point(aes(x=okres,y=zmiennosc_roczna,colour=okres)) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# 
# #dla GOLD  2018 - dryf roczny 2017 - zmiennosc roczna
# 
# dryf_zmiennosc_przedzialy <- estymuj_dryf_zmiennosc_przedzialy(data_USDPLN_train, 20)
# ggplot(data=dryf_zmiennosc_przedzialy) + geom_point(aes(x=okres,y=dryf,colour=okres)) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# ggplot(data=dryf_zmiennosc_przedzialy) + geom_point(aes(x=okres,y=zmiennosc_roczna,colour=okres)) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# 
# dryf_zmiennosc_do_teraz <- estymuj_dryf_zmiennosc_do_teraz(data_USDPLN_train)
# ggplot(data=dryf_zmiennosc_do_teraz) + geom_point(aes(x=okres,y=dryf,colour=okres)) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# ggplot(data=dryf_zmiennosc_do_teraz) + geom_point(aes(x=okres,y=zmiennosc_roczna,colour=okres)) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# 
# #zeby zachowac spojnosc tak samo: 2018 - dryf rocczny i 2017 - zmiennosc roczna
# tmp_train <- na.omit(data_gold_train %>% left_join(data_USDPLN_train, by="Data"))
# data_gold_train <- tmp_train %>% select(Data, Zamkniecie.x, zwroty.x) %>% rename(Zamkniecie=Zamkniecie.x, zwroty=zwroty.x)
# data_USDPLN_train <- tmp_train %>% select(Data, Zamkniecie.y, zwroty.y) %>% rename(Zamkniecie=Zamkniecie.y, zwroty=zwroty.y)
# rm(tmp_train)
# 
# korelacja_przedzialy <- estymuj_korelacje_przedzialy(data_gold_train[-1, ], data_USDPLN_train[-1, ], 20)
# ggplot(data=korelacja_przedzialy) + geom_point(aes(x=okres,y=cor,colour=okres)) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# 
# cor_zmiennosc_do_teraz <- estymuj_korelacje_do_teraz(data_gold_train[-1,],data_USDPLN_train[-1,])
# ggplot(data=cor_zmiennosc_do_teraz) + geom_point(aes(x=okres,y=cor,colour=okres)) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# 
# #korelacja - 2017
# 
# 
# ################################
# #ESTYMACJA PARAMETROW###########
# ################################
# 
# v1 <- numeric(3)
# v2 <- numeric(3)
# names(v1) <-  names(v2) <- c("goldPLN", "gold", "USDPLN")
# parametry <- list(korelacja = 0, dryf_roczny = v1, zmiennosc_roczna = v2)
# parametry$korelacja <- cor(data_gold_train %>% filter(year(Data) >= 2017) %>% select(zwroty) %>% pull(), data_USDPLN_train %>% filter(year(Data) >= 2017) %>% select(zwroty) %>% pull(), method = 'spearman')
# 
# parametry$dryf_roczny[1] <- mean(data_goldPLN_train %>% filter(year(Data) >= 2018) %>% select(zwroty) %>% pull())/dt
# parametry$dryf_roczny[2] <- mean(data_gold_train %>% filter(year(Data) >= 2018) %>% select(zwroty) %>% pull())/dt
# parametry$dryf_roczny[3] <- mean(data_USDPLN_train %>% filter(year(Data) >= 2018) %>% select(zwroty) %>% pull())/dt
# 
# parametry$zmiennosc_roczna[1] <- sd(data_goldPLN_train %>% filter(year(Data) >= 2017) %>% select(zwroty) %>% pull())/sqrt(dt)
# parametry$zmiennosc_roczna[2] <- sd(data_gold_train %>% filter(year(Data) >= 2017) %>% select(zwroty) %>% pull())/sqrt(dt)
# parametry$zmiennosc_roczna[3] <- sd(data_USDPLN_train %>% filter(year(Data) >= 2017) %>% select(zwroty) %>% pull())/sqrt(dt)
# 
# saveRDS(parametry, file=file.path(sciezka,"parametry.rds"))

parametry <- readRDS(file.path(sciezka, "parametry.rds"))
#przyklad generowania 1 trajektorii
#GBM(stocks_names = 'GOLDPLN', drift = parametry$dryf_roczny[1], volatility = parametry$zmiennosc_roczna[1], S0 = tail(data_goldPLN_train$Zamkniecie,1), dt = dt)
library(data.table)
#przyklad generowanie wielu trajektorii
trajectories_GOLDPLN <- generate_trajectories(simulations_number = 1000, stocks_names = "GOLDPLN", method = "GBM", args = "stocks_names = 'GOLDPLN', drift = parametry$dryf_roczny[1], volatility = parametry$zmiennosc_roczna[1], S0 = tail(data_goldPLN_train$Zamkniecie,1), dt = dt")
quantile_plot(lista = trajectories_GOLDPLN$GOLDPLN, dane_historyczne = data_goldPLN_test$Zamkniecie, quantiles = c(0.1, 0.25, 0.5, 0.75, 0.9))

trajectories_GOLD <- generate_trajectories(simulations_number = 1000, stocks_names = "GOLD", method = "GBM", args = "stocks_names = 'GOLD', drift = parametry$dryf_roczny[2], volatility = parametry$zmiennosc_roczna[2], S0 = tail(data_gold_train$Zamkniecie,1), dt = dt")
quantile_plot(lista = trajectories_GOLD$GOLD, dane_historyczne = data_gold_test$Zamkniecie, quantiles = c(0.1, 0.25, 0.5, 0.75, 0.9))

trajectories_USDPLN <- generate_trajectories(simulations_number = 1000, stocks_names = "USDPLN", method = "GBM", args = "stocks_names = 'USDPLN', drift = parametry$dryf_roczny[3], volatility = parametry$zmiennosc_roczna[3], S0 = tail(data_USDPLN_train$Zamkniecie,1), dt = dt")
quantile_plot(lista = trajectories_USDPLN$USDPLN, dane_historyczne = data_USDPLN_test$Zamkniecie, quantiles = c(0.1, 0.25, 0.5, 0.75, 0.9))
