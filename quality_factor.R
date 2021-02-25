########### notatki ##############
# do zrobienia:
# 1. wprowadz kryterium ¿e spolka wylosowana z danego sektora musi miec minimalna historie kursu.
# chyba ze to moze wprowadzic jakis bias do proby. np. Starsze spolki etc?
# 2. wyeksportuj plik csv ze stopami zwrotu oraz pobierz jeszze raz analogiczne dane o market capie

# przemyslenia:
# 1. chyba sa spore dysproporcje w sektorach wobec historii kursów ich spolek. Np ochrona zdrowia i gaming


# setup #########
library(tidyverse)
library(TTR)
library(magrittr)
library(purrr)
library(lubridate)
library(ggplot2)
library(vroom)

ret <- function(x) (x - lag(x))/ lag(x)
cum_prod <-  function(x) cumprod(replace(x, is.na(x), 0) + 1)
  
data_folder <- "data"

# importing data
every_df <- vroom(paste(data_folder,"/", "every_df.csv", sep = ""))[,-1]
ticker_list <- vroom(paste(data_folder,"/", "ticker_list.csv", sep = ""))[,-1]
df <- vroom(paste(data_folder, "/", "sample_df_rets.csv", sep = ""))[,-1]
df_mc <- vroom(paste(data_folder, "/", "df_mc.csv", sep = ""))[,-1]
benchmark <- vroom(paste(data_folder, "/", "benchmark.csv", sep = ""))[,-1] %>%
  mutate(Data = as.Date(Data))
swig <- vroom(paste(data_folder, "/", "swig.csv", sep = ""))[,-1] %>%
  mutate(Data = as.Date(Data))

# making more narrow setor names
agg_sectors <- list(chem_sur = c("Chemia", "Drewno i papier", "Górnictwo", "Tworzywa i guma", "Hutnictwo", "Recykling"),
                    dobr_kons = c("Inne dobra konsumpcyjne", "Produkcja ¿ywno¶ci", "Motoryzacja", "Odzie¿ i kosmetyki", "Wyposa¿enie domu"),
                    finanse = c("Banki", "Finanse pozosta³e", "Rynek kapita³owy", "Ubezpieczenia"),
                    handel_uslugi = c("Handel hurtowy", "Handel internetowy", "Media", "Pozosta³y handel i us³ugi", "Rekreacja i wypoczynek", "Sieci handlowe"),
                    ochrona_zdr = c("Ochrona zdrowia", "Biotechnologia"),
                    energia = c("Paliwa", "Energia", "Dystrybucja ciep³a i wody"),
                    prod_przem = c("Elektromaszynowy", "Transport", "Us³ugi dla przedsiêbiorstw", "Zaopatrzenie przedsiêbiorstw"),
                    prod_bud = c("Budownictwo", "Nieruchomo¶ci"),
                    tech = c("Gry video", "Informatyka", "Telekomunikacja", "Nowe technologie"))%>%
  unlist()%>%
  as.data.frame() %>%
  data.frame(sector = gsub('[[:digit:]]+', '',row.names(.)))

# what is the minimal number of stocks in a single group?
n_sample <- ticker_list %>%
  filter(stock_name %in% colnames(every_df)) %>%
  select(agg_sector) %>%
  table() %>%
  as.data.frame() %>%
  arrange(desc(".")) %>%
  .$Freq %>%
  min()

invest_tickers <- group_by(ticker_list, agg_sector) %>%
  arrange(desc(`.`)) %>%
  mutate(nth_ticker = 1:n()) %>%
  ungroup() %>%
  drop_na() %>%
  filter(nth_ticker <= n_sample) %>%
  select(ticker, "sector" = agg_sector) %>%
  mutate(ticker = tolower(ticker))

# data wrangling for backtest ##################

backtest_t0 <- "2007-01-01"

# sector daily returns
sector_rets <- filter(df, Data >= backtest_t0) %>%
                mutate_at(vars(-Data), ret) %>%
                pivot_longer(cols = -Data) %>%
                merge(.,rename(invest_tickers, "name" = ticker), by = "name") %>%
                group_by(Data, sector) %>%
                summarise(avg_ret = mean(value, na.rm = TRUE)) %>%
                ungroup() %>%
                pivot_wider(names_from = sector, values_from = avg_ret) 



mutate_at(sector_rets, vars(-Data), cum_prod) %>%
pivot_longer(cols = -Data) %>%
mutate(Data = ymd(Data)) %>%
ggplot(aes(x = Data)) +
  geom_line(aes(y = value, color = name))

# stressful times in every year per sector
stressful_time <- mutate(sector_rets) %>%
                    mutate(Data = floor_date(as_date(Data), "month")) %>%
                    pivot_longer(cols = -c(Data)) %>%
                    group_by(Data,  name) %>%
                    summarise(month_ret = prod(value + 1, na.rm = TRUE)) %>%
                    ungroup() %>%
                    mutate(year_t = year(Data)) %>%
                    group_by(name, year_t) %>%
                    summarise(worst_month = min(month_ret, na.rm = TRUE),
                              which_month = Data[which(month_ret == min(month_ret, na.rm = TRUE))]) %>%
                    ungroup() %>%
                    mutate(worst_month = month(which_month))


# return of a given stock during stressful time
stress_stock_rets <- filter(df, Data >= backtest_t0) %>%
                      mutate_at(vars(-Data), ret) %>%
                      pivot_longer(cols = -Data) %>%
                      mutate(Data = floor_date(as_date(Data), "month")) %>%
                      group_by(name, Data) %>%
                      summarise(stock_month_ret = prod(value + 1, na.rm = TRUE)) %>%
                      merge(rename(invest_tickers, "name" = ticker), by = "name", all = TRUE) %>%
                      mutate(year_t = year(Data)) %>%
                      merge(select(stressful_time,"sector" = name, worst_month, year_t), 
                            by = c("sector", "year_t"), all.y = TRUE) %>%
                      group_by(name, year_t) %>%
                      summarise(stress_ret = stock_month_ret[which(month(Data) == worst_month)]) %>%
                      ungroup() %>%
                      merge(rename(invest_tickers, "name" = ticker), by = "name", all = TRUE) %>%
                      merge(select(stressful_time,"sector" = name, worst_month, year_t), 
                            by = c("sector", "year_t"), all.y = TRUE)


# size data frame
mean_mc <- pivot_longer(df_mc, cols = -Data) %>%
  filter(ymd(Data) < tail(df$Data, n = 1)) %>%
  mutate(y = year(ymd(Data)),
         name = str_split_fixed(name, "_", n = 3)[,1]) %>%
  group_by(y, name) %>%
  summarise(mean_cap = mean(value, na.rm = TRUE)) %>%
  ungroup() %>%
  rename("ticker" = name) %>%
  merge(invest_tickers, by = "ticker") %>%
  group_by(y, sector) %>%
  summarise(median_sector_cap = median(mean_cap, na.rm = TRUE), ticker, mean_cap) %>%
  ungroup() %>%
  mutate(size = ifelse(mean_cap > median_sector_cap, "big", "small"))


## 4 portfolios - SS big/small and SV big/small ###############

stress_ranks <- rename(mean_mc, "year_t" = y, "name" = ticker) %>%
                  merge(stress_stock_rets, by  = c("name", "year_t", "sector")) %>%
                  group_by(year_t, sector, size) %>%
                  summarise(rank = rank(stress_ret, na.last = NA),
                              n_stocks = sum(!is.na(size)),name, stress_ret) %>%
                  ungroup() %>%
                  mutate(rel_rank = rank/n_stocks,
                         stress_stable = ifelse(rel_rank > 0.5, 1,0),
                         year_t = year_t - 1) 
  

portfolio_SSbigsmall <- filter(df, Data >= backtest_t0) %>%
  mutate_at(vars(-Data), ret) %>%
  pivot_longer(cols = -Data) %>%
  mutate(year_t = year(ymd(Data))) %>%
  merge(select(stress_ranks, year_t, stress_stable, name, size, rel_rank),
        by  = c("year_t", "name")) %>%
  mutate(Data = ymd(Data),
         stress_stable = recode(stress_stable, '0' = "SV", '1' = "SS"),
         rel_rank = ifelse(stress_stable == 1, rel_rank, rel_rank *(-1))) %>%
  group_by(Data, size, stress_stable) %>%
  summarise(portfolio_ret = weighted.mean(value, rel_rank, na.rm = TRUE)) %>%
  mutate(class = paste(size, "_", stress_stable, sep = "")) %>%
  ungroup()

drop_na(portfolio_SSbigsmall) %>%
  group_by(class) %>%
  summarise(cum_ret = cumprod(portfolio_ret + 1), Data) %>%
  ungroup() %>%
  pivot_wider(names_from = class, values_from = cum_ret) %>%
  merge(select(benchmark, Data, "wig" =  Otwarcie), by = "Data") %>%
  mutate(wig = cumprod(1 + ifelse(is.na(ret(wig)), 0, ret(wig)))) %>%
  merge(select(swig, Data, "swig" = Otwarcie), by  = "Data") %>%
  mutate(swig = cumprod(1 + ifelse(is.na(ret(swig)), 0, ret(swig)))) %>%
  pivot_longer(cols = -Data) %>%
ggplot() +
  geom_line(aes(x = Data, y = value, color = name), size = 1) 

drop_na(portfolio_SSbigsmall) %>%
  select(Data, class, portfolio_ret) %>%
  pivot_wider(names_from = class, values_from = portfolio_ret) %>%
  merge(select(benchmark, Data, "wig" =  Otwarcie), by = "Data") %>%
  merge(select(swig, Data, "swig" = Otwarcie), by  = "Data") %>%
  mutate_at(vars(wig, swig), ret) %>%
  drop_na() %>%
  pivot_longer(cols = -Data) %>%
  group_by(name) %>%
  summarise(sharpe = mean(value, na.rm = TRUE)/sd(value, na.rm = TRUE),
            ret = prod(value + 1, na.rm = TRUE),
            vol = sqrt(252) * sd(value, na.rm = TRUE),
            maxdd = tseries::maxdrawdown(cumprod(value + 1))$maxdrawdown) %>%
  arrange(desc(sharpe))

# No size control ##############

stress_rank_nc <- group_by(stress_stock_rets, sector, year_t) %>%
  summarise(rank = rank(stress_ret, na.last = NA),
            n_stocks = n(), name, stress_ret) %>%
  ungroup() %>%
  mutate(rel_rank = rank/n_stocks,
         stress_stable = ifelse(rel_rank > 0.5, 1, 0),
         year_t = year_t - 1) 

portfolio_nc <- filter(df, Data >= backtest_t0) %>%
  mutate_at(vars(-Data), function(x){ROC(x, type = "discrete")}) %>%
  pivot_longer(cols = -Data) %>%
  mutate(year_t = year(ymd(Data))) %>%
  merge(select(stress_rank_nc, year_t, stress_stable, name), by  = c("year_t", "name")) %>%
  mutate(Data = ymd(Data),
         stress_stable = recode(stress_stable, '0' = "SV", '1' = "SS")) %>%
  group_by(Data, stress_stable) %>%
  summarise(portfolio_ret = mean(value, na.rm = TRUE)) %>%
  ungroup()

drop_na(portfolio_nc) %>%
  group_by(stress_stable) %>%
  summarise(cum_ret = cumprod(portfolio_ret + 1), Data) %>%
  ungroup() %>%
  pivot_wider(names_from = stress_stable, values_from = cum_ret) %>%
  merge(select(benchmark, Data, Otwarcie), by = "Data") %>%
  mutate(Otwarcie = cumprod(1 + ifelse(is.na(ROC(Otwarcie, type = "discrete")), 0, ROC(Otwarcie, type = "discrete")))) %>%
  pivot_longer(cols = -Data) %>%
  ggplot() +
  geom_line(aes(x = Data, y = value, color = name))

drop_na(portfolio_nc) %>%
  pivot_wider(names_from = stress_stable, values_from = portfolio_ret) %>%
  merge(select(benchmark, Data, "wig" =  Otwarcie), by = "Data") %>%
  merge(select(swig, Data, "swig" = Otwarcie), by  = "Data") %>%
  mutate_at(vars(wig, swig), function(x){ROC(x, type = "discrete")}) %>%
  drop_na() %>%
  pivot_longer(cols = -Data) %>%
  group_by(name) %>%
  summarise(sharpe = mean(value, na.rm = TRUE)/sd(value, na.rm = TRUE),
            ret = prod(value + 1, na.rm = TRUE),
            vol = sqrt(252) * sd(value, na.rm = TRUE)) %>%
  arrange(desc(sharpe))


# weight by rank, no size control #############

portfolio_weightd <- filter(df, Data >= backtest_t0) %>%
  mutate_at(vars(-Data), function(x){ROC(x, type = "discrete")}) %>%
  pivot_longer(cols = -Data) %>%
  mutate(year_t = year(ymd(Data))) %>%
  merge(select(stress_rank_nc, year_t, stress_stable, name, rel_rank), by  = c("year_t", "name")) %>%
  mutate(Data = ymd(Data),
         stress_stable = recode(stress_stable, '0' = "SV", '1' = "SS")) %>%
  group_by(Data, stress_stable) %>%
  summarise(portfolio_ret = weighted.mean(value, rel_rank, na.rm = TRUE)) %>%
  ungroup()

drop_na(portfolio_weightd) %>%
  group_by(stress_stable) %>%
  summarise(cum_ret = cumprod(portfolio_ret + 1), Data) %>%
  ungroup() %>%
  pivot_wider(names_from = stress_stable, values_from = cum_ret) %>%
  merge(select(benchmark, Data, Otwarcie), by = "Data") %>%
  mutate(Otwarcie = cumprod(1 + ifelse(is.na(ROC(Otwarcie, type = "discrete")), 0, ROC(Otwarcie, type = "discrete")))) %>%
  merge(select(swig, Data, "swig" = Otwarcie), by  = "Data") %>%
  mutate(swig = cumprod(1 + ifelse(is.na(ROC(swig, type = "discrete")), 0, ROC(swig, type = "discrete")))) %>%
  pivot_longer(cols = -Data) %>%
  ggplot() +
  geom_line(aes(x = Data, y = value, color = name))


# long/short portfolio 

stress_ranks_other <- rename(mean_mc, "year_t" = y, "name" = ticker) %>%
  merge(stress_stock_rets, by  = c("name", "year_t", "sector")) %>%
  group_by(year_t, sector, size) %>%
  summarise(rank = rank(stress_ret, na.last = NA),
            n_stocks = sum(!is.na(size)),name, stress_ret) %>%
  ungroup() %>%
  mutate(rel_rank = rank/n_stocks,
         stress_stable = ifelse(rel_rank > 0.75, 1, ifelse(rel_rank < 0.25, 0, 2)),
         year_t = year_t - 1) 

portfolio_SSbigsmall <- filter(df, Data >= backtest_t0) %>%
  mutate_at(vars(-Data), function(x){ROC(x, type = "discrete")}) %>%
  pivot_longer(cols = -Data) %>%
  mutate(year_t = year(ymd(Data))) %>%
  merge(select(stress_ranks_other, year_t, stress_stable, name, size, rel_rank),
        by  = c("year_t", "name")) %>%
  mutate(Data = ymd(Data),
         stress_stable = recode(stress_stable, '0' = "SV", '1' = "SS", '2' = "other"),
         rel_rank = ifelse(stress_stable == 1, rel_rank, rel_rank *(-1)),
         value = ifelse(stress_stable == "SV", -value, value)) %>%
  group_by(Data, size, stress_stable) %>%
  summarise(portfolio_ret = weighted.mean(value, rel_rank, na.rm = TRUE)) %>%
  mutate(class = paste(size, "_", stress_stable, sep = "")) %>%
  ungroup()


drop_na(portfolio_SSbigsmall) %>%
  group_by(class) %>%
  summarise(cum_ret = cumprod(portfolio_ret + 1), Data) %>%
  ungroup() %>%
  pivot_wider(names_from = class, values_from = cum_ret) %>%
  merge(select(benchmark, Data, "wig" =  Otwarcie), by = "Data") %>%
  mutate(wig = cumprod(1 + ifelse(is.na(ROC(wig, type = "discrete")), 0, ROC(wig, type = "discrete")))) %>%
  merge(select(swig, Data, "swig" = Otwarcie), by  = "Data") %>%
  mutate(swig = cumprod(1 + ifelse(is.na(ROC(swig, type = "discrete")), 0, ROC(swig, type = "discrete"))),
         portfolio = (big_SS + big_SV + small_SS + small_SV)/4) %>%
  pivot_longer(cols = -Data) %>%
  filter(name %in% c("portfolio", "swig", "wig")) %>%
  ggplot() +
  geom_line(aes(x = Data, y = value, color = name), size = 1) 


####### with variables #############

backtest_t0 <- "2009-01-01"
until_date <- Sys.Date()
quintile <- 0.5
size_control <- FALSE
weighted <- TRUE
short_SV <- FALSE
size_per_sector <- TRUE
size_est <- "median" # median or mean


# sector daily returns
sector_rets <- filter(df, Data >= backtest_t0) %>%
  mutate_at(vars(-Data), function(x){ROC(x, type = "discrete")}) %>%
  pivot_longer(cols = -Data) %>%
  merge(.,rename(invest_tickers, "name" = ticker), by = "name") %>%
  group_by(Data, sector) %>%
  summarise(avg_ret = mean(value, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(names_from = sector, values_from = avg_ret) 

cum_prod <-  function(x){ cumprod(replace(x, is.na(x), 0) + 1)}

mutate_at(sector_rets, vars(-Data), cum_prod) %>%
  pivot_longer(cols = -Data) %>%
  mutate(Data = ymd(Data)) %>%
  ggplot(aes(x = Data)) +
  geom_line(aes(y = value, color = name))


# stressful times in every year per sector
stressful_time <- mutate(sector_rets) %>%
  mutate(Data = floor_date(as_date(Data), "month")) %>%
  pivot_longer(cols = -c(Data)) %>%
  group_by(Data,  name) %>%
  summarise(month_ret = prod(value + 1, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(year_t = year(Data)) %>%
  group_by(name, year_t) %>%
  summarise(worst_month = min(month_ret, na.rm = TRUE),
            which_month = Data[which(month_ret == min(month_ret, na.rm = TRUE))]) %>%
  ungroup() %>%
  mutate(worst_month = month(which_month))


# return of a given stock during stressful time
stress_stock_rets <- filter(df, Data >= backtest_t0) %>%
  mutate_at(vars(-Data), function(x){ROC(x, type = "discrete")}) %>%
  pivot_longer(cols = -Data) %>%
  mutate(Data = floor_date(as_date(Data), "month")) %>%
  group_by(name, Data) %>%
  summarise(stock_month_ret = prod(value + 1, na.rm = TRUE)) %>%
  merge(rename(invest_tickers, "name" = ticker), by = "name", all = TRUE) %>%
  mutate(year_t = year(Data)) %>%
  merge(select(stressful_time,"sector" = name, worst_month, year_t), 
        by = c("sector", "year_t"), all.y = TRUE) %>%
  group_by(name, year_t) %>%
  summarise(stress_ret = stock_month_ret[which(month(Data) == worst_month)]) %>%
  ungroup() %>%
  merge(rename(invest_tickers, "name" = ticker), by = "name", all = TRUE) %>%
  merge(select(stressful_time,"sector" = name, worst_month, year_t), 
        by = c("sector", "year_t"), all.y = TRUE)

mean_mc <- pivot_longer(df_mc, cols = -Data) %>%
  filter(ymd(Data) < tail(df$Data, n = 1)) %>%
  mutate(y = year(ymd(Data)),
         name = str_split_fixed(name, "_", n = 3)[,1]) %>%
  group_by(y, name) %>%
  summarise(mean_cap = mean(value, na.rm = TRUE)) %>%
  ungroup() %>%
  rename("ticker" = name) %>%
  merge(invest_tickers, by = "ticker") %>%
  group_by(y, sector) %>%
  summarise(central_sector_cap = ifelse(size_est == "median",
                                        median(mean_cap, na.rm = TRUE),
                                        mean(mean_cap, na.rm = TRUE)), ticker, mean_cap) %>%
  ungroup() %>%
  mutate(size = ifelse(mean_cap > central_sector_cap, "big", "small"))


stress_ranks <- rename(mean_mc, "year_t" = y, "name" = ticker) %>%
  merge(stress_stock_rets, by  = c("name", "year_t", "sector")) %>%
  group_by(year_t, sector, if(size_control){size}) %>%
  summarise(rank = rank(stress_ret, na.last = NA),
            n_stocks = ifelse(size_control, sum(!is.na(size)), n()),name, stress_ret) %>%
  ungroup() %>%
  mutate(rel_rank = rank/n_stocks,
         stress_stable = ifelse(rel_rank > quintile, 1, 
                                ifelse(rel_rank < (1 - quintile), 0, "rest")),
         year_t = year_t - 1) 

portfolio_QMJ <- filter(df, Data >= backtest_t0) %>%
  mutate_at(vars(-Data), function(x){ROC(x, type = "discrete")}) %>%
  pivot_longer(cols = -Data) %>%
  mutate(year_t = year(ymd(Data))) %>%
  merge(select(stress_ranks, year_t, stress_stable, name, if(size_control){size}, rel_rank),
        by  = c("year_t", "name")) %>%
  mutate(Data = ymd(Data),
         stress_stable = recode(stress_stable, '0' = "SV", '1' = "SS"),
         rel_rank = ifelse(stress_stable == 1, rel_rank, rel_rank *(-1)),
         value = ifelse(stress_stable == "SV", ifelse(short_SV, -value, value), value)) %>%
  group_by(Data, if(size_control){size}, stress_stable) %>%
  summarise(portfolio_ret = weighted.mean(value, if(weighted){rel_rank}, na.rm = TRUE))

portfolio_QMJ <- if(size_control){ mutate(portfolio_QMJ, class = paste(size, "_", stress_stable, sep = ""))} else {portfolio_QMJ}

drop_na(portfolio_QMJ) %>%
  group_by(ifelse(size_control, class, stress_stable)) %>%
  summarise(cum_ret = cumprod(portfolio_ret + 1), Data) %>%
  ungroup() %>%
  pivot_wider(names_from = ifelse(size_control, class, stress_stable), 
              values_from = cum_ret) %>%
  merge(select(benchmark, Data, "wig" =  Otwarcie), by = "Data") %>%
  mutate(wig = cumprod(1 + ifelse(is.na(ROC(wig, type = "discrete")), 0, ROC(wig, type = "discrete")))) %>%
  merge(select(swig, Data, "swig" = Otwarcie), by  = "Data") %>%
  mutate(swig = cumprod(1 + ifelse(is.na(ROC(swig, type = "discrete")), 0, ROC(swig, type = "discrete")))) %>%
  pivot_longer(cols = -Data) %>%
  ggplot() +
  geom_line(aes(x = Data, y = value, color = name), size = 1) 
