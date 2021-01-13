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
# trajectories_GOLDPLN <- generate_trajectories(simulations_number = 1000, stocks_names = "GOLDPLN", method = "GBM", args = "stocks_names = 'GOLDPLN', drift = parametry$dryf_roczny[1], volatility = parametry$zmiennosc_roczna[1], S0 = tail(data_goldPLN_train$Zamkniecie,1), dt = dt")
# quantile_plot(lista = trajectories_GOLDPLN$GOLDPLN, dane_historyczne = data_goldPLN_test$Zamkniecie, quantiles = c(0.1, 0.25, 0.5, 0.75, 0.9))
# 
# trajectories_GOLD <- generate_trajectories(simulations_number = 1000, stocks_names = "GOLD", method = "GBM", args = "stocks_names = 'GOLD', drift = parametry$dryf_roczny[2], volatility = parametry$zmiennosc_roczna[2], S0 = tail(data_gold_train$Zamkniecie,1), dt = dt")
# quantile_plot(lista = trajectories_GOLD$GOLD, dane_historyczne = data_gold_test$Zamkniecie, quantiles = c(0.1, 0.25, 0.5, 0.75, 0.9))
# 
# trajectories_USDPLN <- generate_trajectories(simulations_number = 1000, stocks_names = "USDPLN", method = "GBM", args = "stocks_names = 'USDPLN', drift = parametry$dryf_roczny[3], volatility = parametry$zmiennosc_roczna[3], S0 = tail(data_USDPLN_train$Zamkniecie,1), dt = dt")
# quantile_plot(lista = trajectories_USDPLN$USDPLN, dane_historyczne = data_USDPLN_test$Zamkniecie, quantiles = c(0.1, 0.25, 0.5, 0.75, 0.9))


d1_f = function(S, t, K, r, sigma, T, D)
{
  d1 = (log(S / K) + (r - D + 0.5 * (sigma ^ 2)) * (T - t)) / (sigma * sqrt(T - t))
  d1
}

d2_f = function(S, t, K, r, sigma, T, D)
{
  d2 = d1_f(S, t, K, r, sigma, T, D) - sigma * sqrt(T - t)
  d2
}

cena_call <- function(S, t, K, r, sigma, T, czy_put = F, D)
{
  d1 = d1_f(S, t, K, r, sigma, T, D)
  d2 = d2_f(S, t, K, r, sigma, T, D)
  V = S * pnorm(d1) * exp(-D*(T-t)) - K * exp(-r * (T - t)) * pnorm(d2)
  V - czy_put * (S - K * exp(-r * (T - t)))
}

delta <- function(S, t, K, r, sigma, T, D)
{
  d1 = d1_f(S, t, K, r, sigma, T, D)
  exp(-D*(T-t))*pnorm(d1)
}

# okres - liczba dni do rehedgingow
bilans <- function(trajektoria,
                   okres,
                   zmiennosci,
                   korelacja,
                   r,
                   K = 1,
                   zabezpieczenie = 0,
                   czy_put = F, 
                   plot_bilans = F) {
  delta = 0
  zmiennosc_zlota <- zmiennosci[1]
  zmiennosc_kursu <- zmiennosci[2]
  n = length(trajektoria)
  dt = 1 / n
  bilans = c(0)
  K <- K * trajektoria[1]
  dywidenda <- r + korelacja * zmiennosc_kursu * zmiennosc_zlota
  kapital = 100/trajektoria[1] * cena_call(S = trajektoria[1],  t = dt, K = K, r = r, sigma = zmiennosc_zlota, T = 1, czy_put = czy_put, D = dywidenda) * (1 + zabezpieczenie) # zabezpieczenie czyli mar¿a od wartosci akcji
  dni = seq(1, n - 1, okres)
  tabelka = data.frame(bilans=0, cena_indeksu=0, delta=0, kapital=0)
  for (i in dni) {
    S = trajektoria[i] #aktualna cena indeksu
    delta_old = delta
    delta <- 100/trajektoria[1] * delta(S = S, t = i/n, K = K, r = r, sigma = zmiennosc_zlota, T = 1, D = dywidenda)
    #skok o okres
    if(i+okres>n)
      okres = n-i
    S_T = trajektoria[i + okres]
    kapital = (kapital - (delta - delta_old) * S) * exp(r * okres / n) + max(delta - delta_old, 0) * S * dywidenda * dt * okres
    #kapital = (kapital - (delta - delta_old) * S) * exp(r * okres / n) - min(delta - delta_old, 0) * S * dywidenda * dt * okres
    bilans = c(bilans, -100/trajektoria[1] * cena_call(S = S_T, t = (i + okres) * dt, K = K, r = r, sigma = zmiennosc_zlota, T = n * dt, czy_put = czy_put, D = dywidenda) + delta * S_T + kapital) # wartosc naszego portfela
    #tabelka <- tabelka %>% add_row(bilans=tail(bilans,1), cena_indeksu=S_T, delta=delta, kapital=kapital)
  }
  if(plot_bilans==F)
    bilans
    #tabelka
  else{
    if (tail(dni, 1) != 250) dni = c(dni, 250)
    to_plot = data.frame(bilans = bilans, dni = dni)
    ggplot(data=to_plot, aes(x=dni, y=bilans)) +
      geom_line()+
      geom_point()
  }
}

portfel <-function(okres, dryf, zmiennosci, r, S0, K = 1, zabezpieczenie = 0, czy_put, korelacja, nazwa_aktywa){
  p = c()
  len = ceiling(249 / okres)
  set.seed(56)
  for (i in 1:1000) {
    p = c(p, bilans(trajektoria = GBM(stocks_names = nazwa_aktywa, drift = dryf, volatility = zmiennosci[1], S0 = tail(data_goldPLN_train$Zamkniecie,1), dt = dt)[[nazwa_aktywa]], okres = okres, zmiennosci = zmiennosci, korelacja = korelacja, r = r, K = K, zabezpieczenie = zabezpieczenie, czy_put = czy_put)[len])
  }
  p
  #bilans(GBM(stocks_names = nazwa_aktywa, drift = dryf, volatility = zmiennosci[1], S0 = S0, dt = dt)[[nazwa_aktywa]], okres,  zmiennosci, korelacja, r, K, zabezpieczenie, czy_put)
}

plot_hist <- function(dane, xmin, xmax, ymin, ymax, ...){
  dane_plot = data.frame(skok = dane)
  ggplot(dane_plot, aes(x=skok)) +
    geom_histogram(color="black", fill="grey", ...) +
    labs(x="zysk", y = "count") + xlim(xmin, xmax) + ylim(ymin, ymax)
}

library(ggplot2)
library(gridExtra)
set.seed(56)

S_0 <- tail(data_gold_train$Zamkniecie,1)

K = 1
xmin = -7
xmax = 2.6
ymin = 0
ymax = 40
binwidth = 0.02

okres <- 1
dane = portfel(okres = okres,  dryf = parametry$dryf_roczny[2], zmiennosci = parametry$zmiennosc_roczna[-1], r = 0.01, S0 = S_0, zabezpieczenie =  0, czy_put = F, K = K, korelacja = parametry$korelacja, nazwa_aktywa = "GOLD")
p1 <- plot_hist(dane, binwidth=binwidth, xmin=xmin, xmax = xmax, ymin=ymin, ymax=ymax) + theme(axis.title.x = element_blank()) + ggtitle(paste0("okres = ",okres," K = ",K))
summary(dane)
sd(dane)
sum(dane< xmin | dane > xmax)


okres <- 5
dane = portfel(okres = okres,  dryf = parametry$dryf_roczny[2], zmiennosci = parametry$zmiennosc_roczna[-1], r = 0.01, S0 = S_0, zabezpieczenie =  0, czy_put = F, K = K, korelacja = parametry$korelacja, nazwa_aktywa = "GOLD")
p2 <- plot_hist(dane, binwidth=binwidth, xmin=xmin, xmax = xmax, ymin=ymin, ymax=ymax) + theme(axis.title.x = element_blank()) + ggtitle(paste0("okres = ",okres," K = ",K))
summary(dane)
sd(dane)
sum(dane< xmin | dane > xmax)


okres <- 10
dane = portfel(okres = okres,  dryf = parametry$dryf_roczny[2], zmiennosci = parametry$zmiennosc_roczna[-1], r = 0.01, S0 = S_0, zabezpieczenie =  0, czy_put = F, K = K, korelacja = parametry$korelacja, nazwa_aktywa = "GOLD")
p3 <- plot_hist(dane, binwidth=binwidth, xmin=xmin, xmax = xmax, ymin=ymin, ymax=ymax) + theme(axis.title.x = element_blank()) + ggtitle(paste0("okres = ",okres," K = ",K))
summary(dane)
sd(dane)
sum(dane< xmin | dane > xmax)


okres <- 30
dane = portfel(okres = okres,  dryf = parametry$dryf_roczny[2], zmiennosci = parametry$zmiennosc_roczna[-1], r = 0.01, S0 = S_0, zabezpieczenie =  0, czy_put = F, K = K, korelacja = parametry$korelacja, nazwa_aktywa = "GOLD")
p4 <- plot_hist(dane, binwidth=binwidth, xmin=xmin, xmax = xmax, ymin=ymin, ymax=ymax) + theme(axis.title.x = element_blank()) + ggtitle(paste0("okres = ",okres," K = ",K))
summary(dane)
sd(dane)
sum(dane< xmin | dane > xmax)


okres <- 60
dane = portfel(okres = okres,  dryf = parametry$dryf_roczny[2], zmiennosci = parametry$zmiennosc_roczna[-1], r = 0.01, S0 = S_0, zabezpieczenie =  0, czy_put = F, K = K, korelacja = parametry$korelacja, nazwa_aktywa = "GOLD")
p5 <- plot_hist(dane, binwidth=binwidth, xmin=xmin, xmax = xmax, ymin=ymin, ymax=ymax) + theme(axis.title.x = element_blank()) + ggtitle(paste0("okres = ",okres," K = ",K))
summary(dane)
sd(dane)
sum(dane< xmin | dane > xmax)


okres <- 120
dane = portfel(okres = okres,  dryf = parametry$dryf_roczny[2], zmiennosci = parametry$zmiennosc_roczna[-1], r = 0.01, S0 = S_0, zabezpieczenie =  0, czy_put = F, K = K, korelacja = parametry$korelacja, nazwa_aktywa = "GOLD")
p6 <- plot_hist(dane, binwidth=binwidth, xmin=xmin, xmax = xmax, ymin=ymin, ymax=ymax) + theme(axis.title.x = element_blank()) + ggtitle(paste0("okres = ",okres," K = ",K))
summary(dane)
sd(dane)
sum(dane< xmin | dane > xmax)


grid.arrange(p1, p2, p3, p4, p5, p6, ncol=2, nrow=3)


#K=0
K = 0
xmin = -0.63
xmax = -0.14
ymin = 0
ymax = 160
binwidth = 0.01


okres <- 1
dane = portfel(okres = okres,  dryf = parametry$dryf_roczny[2], zmiennosci = parametry$zmiennosc_roczna[-1], r = 0.01, S0 = S_0, zabezpieczenie =  0, czy_put = F, K = K, korelacja = parametry$korelacja, nazwa_aktywa = "GOLD")
p1 <- plot_hist(dane, binwidth=binwidth, xmin=xmin, xmax = xmax, ymin=ymin, ymax=ymax) + theme(axis.title.x = element_blank()) + ggtitle(paste0("okres = ",okres," K = ",K))
summary(dane)
sd(dane)
sum(dane< xmin | dane > xmax)


okres <- 5
dane = portfel(okres = okres,  dryf = parametry$dryf_roczny[2], zmiennosci = parametry$zmiennosc_roczna[-1], r = 0.01, S0 = S_0, zabezpieczenie =  0, czy_put = F, K = K, korelacja = parametry$korelacja, nazwa_aktywa = "GOLD")
p2 <- plot_hist(dane, binwidth=binwidth, xmin=xmin, xmax = xmax, ymin=ymin, ymax=ymax) + theme(axis.title.x = element_blank()) + ggtitle(paste0("okres = ",okres," K = ",K))
summary(dane)
sd(dane)
sum(dane< xmin | dane > xmax)


okres <- 10
dane = portfel(okres = okres,  dryf = parametry$dryf_roczny[2], zmiennosci = parametry$zmiennosc_roczna[-1], r = 0.01, S0 = S_0, zabezpieczenie =  0, czy_put = F, K = K, korelacja = parametry$korelacja, nazwa_aktywa = "GOLD")
p3 <- plot_hist(dane, binwidth=binwidth, xmin=xmin, xmax = xmax, ymin=ymin, ymax=ymax) + theme(axis.title.x = element_blank()) + ggtitle(paste0("okres = ",okres," K = ",K))
summary(dane)
sd(dane)
sum(dane< xmin | dane > xmax)


okres <- 30
dane = portfel(okres = okres,  dryf = parametry$dryf_roczny[2], zmiennosci = parametry$zmiennosc_roczna[-1], r = 0.01, S0 = S_0, zabezpieczenie =  0, czy_put = F, K = K, korelacja = parametry$korelacja, nazwa_aktywa = "GOLD")
p4 <- plot_hist(dane, binwidth=binwidth, xmin=xmin, xmax = xmax, ymin=ymin, ymax=ymax) + theme(axis.title.x = element_blank()) + ggtitle(paste0("okres = ",okres," K = ",K))
summary(dane)
sd(dane)
sum(dane< xmin | dane > xmax)


okres <- 60
dane = portfel(okres = okres,  dryf = parametry$dryf_roczny[2], zmiennosci = parametry$zmiennosc_roczna[-1], r = 0.01, S0 = S_0, zabezpieczenie =  0, czy_put = F, K = K, korelacja = parametry$korelacja, nazwa_aktywa = "GOLD")
p5 <- plot_hist(dane, binwidth=binwidth, xmin=xmin, xmax = xmax, ymin=ymin, ymax=ymax) + theme(axis.title.x = element_blank()) + ggtitle(paste0("okres = ",okres," K = ",K))
summary(dane)
sd(dane)
sum(dane< xmin | dane > xmax)


okres <- 120
dane = portfel(okres = okres,  dryf = parametry$dryf_roczny[2], zmiennosci = parametry$zmiennosc_roczna[-1], r = 0.01, S0 = S_0, zabezpieczenie =  0, czy_put = F, K = K, korelacja = parametry$korelacja, nazwa_aktywa = "GOLD")
p6 <- plot_hist(dane, binwidth=binwidth, xmin=xmin, xmax = xmax, ymin=ymin, ymax=ymax) + theme(axis.title.x = element_blank()) + ggtitle(paste0("okres = ",okres," K = ",K))
summary(dane)
sd(dane)
sum(dane< xmin | dane > xmax)


grid.arrange(p1, p2, p3, p4, p5, p6, ncol=2, nrow=3)



#K=0.5
K = 0.5
xmin = -0.63
xmax = -0.14
ymin = 0
ymax = 160
binwidth = 0.01


okres <- 1
dane = portfel(okres = okres,  dryf = parametry$dryf_roczny[2], zmiennosci = parametry$zmiennosc_roczna[-1], r = 0.01, S0 = S_0, zabezpieczenie =  0, czy_put = F, K = K, korelacja = parametry$korelacja, nazwa_aktywa = "GOLD")
p1 <- plot_hist(dane, binwidth=binwidth, xmin=xmin, xmax = xmax, ymin=ymin, ymax=ymax) + theme(axis.title.x = element_blank()) + ggtitle(paste0("okres = ",okres," K = ",K))
summary(dane)
sd(dane)
sum(dane< xmin | dane > xmax)


okres <- 5
dane = portfel(okres = okres,  dryf = parametry$dryf_roczny[2], zmiennosci = parametry$zmiennosc_roczna[-1], r = 0.01, S0 = S_0, zabezpieczenie =  0, czy_put = F, K = K, korelacja = parametry$korelacja, nazwa_aktywa = "GOLD")
p2 <- plot_hist(dane, binwidth=binwidth, xmin=xmin, xmax = xmax, ymin=ymin, ymax=ymax) + theme(axis.title.x = element_blank()) + ggtitle(paste0("okres = ",okres," K = ",K))
summary(dane)
sd(dane)
sum(dane< xmin | dane > xmax)


okres <- 10
dane = portfel(okres = okres,  dryf = parametry$dryf_roczny[2], zmiennosci = parametry$zmiennosc_roczna[-1], r = 0.01, S0 = S_0, zabezpieczenie =  0, czy_put = F, K = K, korelacja = parametry$korelacja, nazwa_aktywa = "GOLD")
p3 <- plot_hist(dane, binwidth=binwidth, xmin=xmin, xmax = xmax, ymin=ymin, ymax=ymax) + theme(axis.title.x = element_blank()) + ggtitle(paste0("okres = ",okres," K = ",K))
summary(dane)
sd(dane)
sum(dane< xmin | dane > xmax)


okres <- 30
dane = portfel(okres = okres,  dryf = parametry$dryf_roczny[2], zmiennosci = parametry$zmiennosc_roczna[-1], r = 0.01, S0 = S_0, zabezpieczenie =  0, czy_put = F, K = K, korelacja = parametry$korelacja, nazwa_aktywa = "GOLD")
p4 <- plot_hist(dane, binwidth=binwidth, xmin=xmin, xmax = xmax, ymin=ymin, ymax=ymax) + theme(axis.title.x = element_blank()) + ggtitle(paste0("okres = ",okres," K = ",K))
summary(dane)
sd(dane)
sum(dane< xmin | dane > xmax)


okres <- 60
dane = portfel(okres = okres,  dryf = parametry$dryf_roczny[2], zmiennosci = parametry$zmiennosc_roczna[-1], r = 0.01, S0 = S_0, zabezpieczenie =  0, czy_put = F, K = K, korelacja = parametry$korelacja, nazwa_aktywa = "GOLD")
p5 <- plot_hist(dane, binwidth=binwidth, xmin=xmin, xmax = xmax, ymin=ymin, ymax=ymax) + theme(axis.title.x = element_blank()) + ggtitle(paste0("okres = ",okres," K = ",K))
summary(dane)
sd(dane)
sum(dane< xmin | dane > xmax)


okres <- 120
dane = portfel(okres = okres,  dryf = parametry$dryf_roczny[2], zmiennosci = parametry$zmiennosc_roczna[-1], r = 0.01, S0 = S_0, zabezpieczenie =  0, czy_put = F, K = K, korelacja = parametry$korelacja, nazwa_aktywa = "GOLD")
p6 <- plot_hist(dane, binwidth=binwidth, xmin=xmin, xmax = xmax, ymin=ymin, ymax=ymax) + theme(axis.title.x = element_blank()) + ggtitle(paste0("okres = ",okres," K = ",K))
summary(dane)
sd(dane)
sum(dane< xmin | dane > xmax)


grid.arrange(p1, p2, p3, p4, p5, p6, ncol=2, nrow=3)



#K=1.5
K = 1.5
xmin = -0.015
xmax = 0.012
ymin = 0
ymax = 300
binwidth = 0.0001


okres <- 1
dane = portfel(okres = okres,  dryf = parametry$dryf_roczny[2], zmiennosci = parametry$zmiennosc_roczna[-1], r = 0.01, S0 = S_0, zabezpieczenie =  0, czy_put = F, K = K, korelacja = parametry$korelacja, nazwa_aktywa = "GOLD")
p1 <- plot_hist(dane, binwidth=binwidth, xmin=xmin, xmax = xmax, ymin=ymin, ymax=ymax) + theme(axis.title.x = element_blank()) + ggtitle(paste0("okres = ",okres," K = ",K))
summary(dane)
sd(dane)
sum(dane< xmin | dane > xmax)


okres <- 5
dane = portfel(okres = okres,  dryf = parametry$dryf_roczny[2], zmiennosci = parametry$zmiennosc_roczna[-1], r = 0.01, S0 = S_0, zabezpieczenie =  0, czy_put = F, K = K, korelacja = parametry$korelacja, nazwa_aktywa = "GOLD")
p2 <- plot_hist(dane, binwidth=binwidth, xmin=xmin, xmax = xmax, ymin=ymin, ymax=ymax) + theme(axis.title.x = element_blank()) + ggtitle(paste0("okres = ",okres," K = ",K))
summary(dane)
sd(dane)
sum(dane< xmin | dane > xmax)


okres <- 10
dane = portfel(okres = okres,  dryf = parametry$dryf_roczny[2], zmiennosci = parametry$zmiennosc_roczna[-1], r = 0.01, S0 = S_0, zabezpieczenie =  0, czy_put = F, K = K, korelacja = parametry$korelacja, nazwa_aktywa = "GOLD")
p3 <- plot_hist(dane, binwidth=binwidth, xmin=xmin, xmax = xmax, ymin=ymin, ymax=ymax) + theme(axis.title.x = element_blank()) + ggtitle(paste0("okres = ",okres," K = ",K))
summary(dane)
sd(dane)
sum(dane< xmin | dane > xmax)


okres <- 30
dane = portfel(okres = okres,  dryf = parametry$dryf_roczny[2], zmiennosci = parametry$zmiennosc_roczna[-1], r = 0.01, S0 = S_0, zabezpieczenie =  0, czy_put = F, K = K, korelacja = parametry$korelacja, nazwa_aktywa = "GOLD")
p4 <- plot_hist(dane, binwidth=binwidth, xmin=xmin, xmax = xmax, ymin=ymin, ymax=ymax) + theme(axis.title.x = element_blank()) + ggtitle(paste0("okres = ",okres," K = ",K))
summary(dane)
sd(dane)
sum(dane< xmin | dane > xmax)


okres <- 60
dane = portfel(okres = okres,  dryf = parametry$dryf_roczny[2], zmiennosci = parametry$zmiennosc_roczna[-1], r = 0.01, S0 = S_0, zabezpieczenie =  0, czy_put = F, K = K, korelacja = parametry$korelacja, nazwa_aktywa = "GOLD")
p5 <- plot_hist(dane, binwidth=binwidth, xmin=xmin, xmax = xmax, ymin=ymin, ymax=ymax) + theme(axis.title.x = element_blank()) + ggtitle(paste0("okres = ",okres," K = ",K))
summary(dane)
sd(dane)
sum(dane< xmin | dane > xmax)


okres <- 120
dane = portfel(okres = okres,  dryf = parametry$dryf_roczny[2], zmiennosci = parametry$zmiennosc_roczna[-1], r = 0.01, S0 = S_0, zabezpieczenie =  0, czy_put = F, K = K, korelacja = parametry$korelacja, nazwa_aktywa = "GOLD")
p6 <- plot_hist(dane, binwidth=binwidth, xmin=xmin, xmax = xmax, ymin=ymin, ymax=ymax) + theme(axis.title.x = element_blank()) + ggtitle(paste0("okres = ",okres," K = ",K))
summary(dane)
sd(dane)
sum(dane< xmin | dane > xmax)


grid.arrange(p1, p2, p3, p4, p5, p6, ncol=2, nrow=3)



K = 2
xmin = -1.2e-10
xmax = 1.2e-10
ymin = 0
ymax = 200
binwidth = 1e-12


okres <- 1
dane = portfel(okres = okres,  dryf = parametry$dryf_roczny[2], zmiennosci = parametry$zmiennosc_roczna[-1], r = 0.01, S0 = S_0, zabezpieczenie =  0, czy_put = F, K = K, korelacja = parametry$korelacja, nazwa_aktywa = "GOLD")
p1 <- plot_hist(dane, binwidth=binwidth, xmin=xmin, xmax = xmax, ymin=ymin, ymax=ymax) + theme(axis.title.x = element_blank()) + ggtitle(paste0("okres = ",okres," K = ",K))
summary(dane)
sd(dane)
sum(dane< xmin | dane > xmax)


okres <- 5
dane = portfel(okres = okres,  dryf = parametry$dryf_roczny[2], zmiennosci = parametry$zmiennosc_roczna[-1], r = 0.01, S0 = S_0, zabezpieczenie =  0, czy_put = F, K = K, korelacja = parametry$korelacja, nazwa_aktywa = "GOLD")
p2 <- plot_hist(dane, binwidth=binwidth, xmin=xmin, xmax = xmax, ymin=ymin, ymax=ymax) + theme(axis.title.x = element_blank()) + ggtitle(paste0("okres = ",okres," K = ",K))
summary(dane)
sd(dane)
sum(dane< xmin | dane > xmax)


okres <- 10
dane = portfel(okres = okres,  dryf = parametry$dryf_roczny[2], zmiennosci = parametry$zmiennosc_roczna[-1], r = 0.01, S0 = S_0, zabezpieczenie =  0, czy_put = F, K = K, korelacja = parametry$korelacja, nazwa_aktywa = "GOLD")
p3 <- plot_hist(dane, binwidth=binwidth, xmin=xmin, xmax = xmax, ymin=ymin, ymax=ymax) + theme(axis.title.x = element_blank()) + ggtitle(paste0("okres = ",okres," K = ",K))
summary(dane)
sd(dane)
sum(dane< xmin | dane > xmax)


okres <- 30
dane = portfel(okres = okres,  dryf = parametry$dryf_roczny[2], zmiennosci = parametry$zmiennosc_roczna[-1], r = 0.01, S0 = S_0, zabezpieczenie =  0, czy_put = F, K = K, korelacja = parametry$korelacja, nazwa_aktywa = "GOLD")
p4 <- plot_hist(dane, binwidth=binwidth, xmin=xmin, xmax = xmax, ymin=ymin, ymax=ymax) + theme(axis.title.x = element_blank()) + ggtitle(paste0("okres = ",okres," K = ",K))
summary(dane)
sd(dane)
sum(dane< xmin | dane > xmax)


okres <- 60
dane = portfel(okres = okres,  dryf = parametry$dryf_roczny[2], zmiennosci = parametry$zmiennosc_roczna[-1], r = 0.01, S0 = S_0, zabezpieczenie =  0, czy_put = F, K = K, korelacja = parametry$korelacja, nazwa_aktywa = "GOLD")
p5 <- plot_hist(dane, binwidth=binwidth, xmin=xmin, xmax = xmax, ymin=ymin, ymax=ymax) + theme(axis.title.x = element_blank()) + ggtitle(paste0("okres = ",okres," K = ",K))
summary(dane)
sd(dane)
sum(dane< xmin | dane > xmax)


okres <- 120
dane = portfel(okres = okres,  dryf = parametry$dryf_roczny[2], zmiennosci = parametry$zmiennosc_roczna[-1], r = 0.01, S0 = S_0, zabezpieczenie =  0, czy_put = F, K = K, korelacja = parametry$korelacja, nazwa_aktywa = "GOLD")
p6 <- plot_hist(dane, binwidth=binwidth, xmin=xmin, xmax = xmax, ymin=ymin, ymax=ymax) + theme(axis.title.x = element_blank()) + ggtitle(paste0("okres = ",okres," K = ",K))
summary(dane)
sd(dane)
sum(dane< xmin | dane > xmax)


grid.arrange(p1, p2, p3, p4, p5, p6, ncol=2, nrow=3)
