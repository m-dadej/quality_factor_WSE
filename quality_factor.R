########### notatki ##############
# do zrobienia:
# 1. wprowadz kryterium ?e spolka wylosowana z danego sektora musi miec minimalna historie kursu.
# chyba ze to moze wprowadzic jakis bias do proby. np. Starsze spolki etc?
# 2. wyeksportuj plik csv ze stopami zwrotu oraz pobierz jeszze raz analogiczne dane o market capie

# przemyslenia:
# 1. chyba sa spore dysproporcje w sektorach wobec historii kurs?w ich spolek. Np ochrona zdrowia i gaming


# setup #########
library(tidyverse)
library(TTR)
library(magrittr)
library(purrr)
library(lubridate)
library(ggplot2)
library(vroom)
library(hrbrthemes)

# return function
ret <- function(x) (x - lag(x))/ lag(x)
# cumulative return function
cum_prod <-  function(x) cumprod(replace(x, is.na(x), 0) + 1)
# max drawdown function
max_dd <- function(r){ max(1 - cumprod(1 + r) / cummax(cumprod(1 + r))) }

  
data_folder <- "data"

# importing data
invest_tickers <- vroom(paste(data_folder,"/", "ticker_sectors.csv", sep = ""))[,-1]
df <- vroom(paste(data_folder, "/", "sample_df_rets.csv", sep = ""))[,-1] %>%
        arrange(Data) %>%
        filter(Data <= "2021-01-05")
df_mc <- vroom(paste(data_folder, "/", "df_mc.csv", sep = ""))[,-1] %>%
          arrange(Data) %>%
          filter(Data <= "2021-01-05")
benchmark <- vroom(paste(data_folder, "/", "benchmark.csv", sep = ""))[,-1] %>%
  mutate(Data = as.Date(Data))
swig <- vroom(paste(data_folder, "/", "swig.csv", sep = ""))[,-1] %>%
  mutate(Data = as.Date(Data))


invest_tickers <- select(invest_tickers, "name" = ticker, "sector" = agg_sector) %>%
                    mutate(name = tolower(name))

# data wrangling for backtest ##################

backtest_t0 <- "2007-01-01"

# sector daily returns
sector_rets <- filter(df, Data >= backtest_t0, Data <= "2021-01-05") %>%
                mutate_at(vars(-Data), ret) %>%
                mutate_at(vars(-Data), function(x){replace(x, is.na(x), 0)}) %>%
                pivot_longer(cols = -Data) %>%
                merge(., invest_tickers, by = "name") %>%
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
                      ungroup() %>%
                      mutate(stock_month_ret = ifelse(stock_month_ret == 1, NA, stock_month_ret)) %>%
                      merge(invest_tickers, by = "name", all = TRUE) %>%
                      mutate(year_t = year(Data)) %>%
                      merge(select(stressful_time,"sector" = name, worst_month, year_t), 
                            by = c("sector", "year_t"), all.y = TRUE) %>%
                      group_by(name, year_t) %>%
                      summarise(stress_ret = stock_month_ret[which(month(Data) == worst_month)]) %>%
                      ungroup() %>%
                      merge(invest_tickers, by = "name", all = TRUE) %>%
                      merge(select(stressful_time,"sector" = name, worst_month, year_t), 
                            by = c("sector", "year_t"), all.y = TRUE)

sector_heatmap <- filter(sector_rets[-1,], year(Data) != 2021) %>%
pivot_longer(cols = -Data) %>%
  mutate(month_t = month(Data),
         year_t = year(Data)) %>%
  group_by(month_t, year_t, name) %>%
  summarise(ret = prod(value + 1)) %>%
  ungroup() %>%
  mutate(ret = ifelse(is.na(ret), 1, ret)) %>%
  left_join(rename(stressful_time, "month_t" = worst_month),
            by = c("month_t", "year_t", "name")) %>%
  mutate(which_month = month(which_month),
         name = recode(name,
                       chem_materials = "Chemicals & Basic materials",
                       construction = "Construction",
                       consumer_goods = "Consumer goods",
                       energy = "Energy",
                       finance = "Finance",
                       healthcare = "Healthcare",
                       industrials = "Manufacturing",
                       tech = "Technology",
                       trade_services = "Trade & Services"),
         ret = ret - 1) %>%
  rename("Return" = ret) %>%
  ggplot(aes(x = month_t, y = year_t, fill = Return)) +
  geom_tile() +
  geom_rect(aes(xmin = which_month - 0.5, 
                xmax = which_month + 0.5, 
                ymin = year_t - 0.5, 
                ymax = year_t + 0.5), size=1, fill=NA, colour="black") +
  facet_wrap(~name) +
  scale_x_continuous(breaks = seq(2,12,2), labels = function(x){month.abb[x]}) +
  scale_y_continuous(breaks = seq(2008, 2020, 2)) +
  labs(title = "Monthly returns and stressful times of each sector",
       x = "month", y = "year") +
  theme_minimal() +
  theme(strip.background = element_rect(colour="white")) +
  scale_fill_viridis_c(option = "B",labels = scales::percent)

mc_sd_plot <- pivot_longer(df_mc, cols = -Data) %>%
  rename("ret" = value) %>%
  mutate(name = str_split_fixed(name, "_", n = 3)[,1]) %>%
  full_join(pivot_longer(df, cols = -Data), by  = c("name", "Data")) %>%
  filter(year(Data) >= 2010) %>%
  group_by(name) %>%
  mutate(ret = ROC(ret, type = "discrete")) %>%
  summarise(median_mc = median(value, na.rm = TRUE),
            std_dev = sd(ret, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(std_dev < 0.4, 
         median_mc < 4000) %>%
  ggplot(aes(x = median_mc, y = std_dev)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() + 
  geom_smooth(method = "lm")

market_cap_sd_model <- pivot_longer(df_mc, cols = -Data) %>%
  rename("ret" = value) %>%
  mutate(name = str_split_fixed(name, "_", n = 3)[,1]) %>%
  full_join(pivot_longer(df, cols = -Data), by  = c("name", "Data")) %>%
  filter(year(Data) >= 2010) %>%
  group_by(name) %>%
  mutate(ret = ROC(ret, type = "discrete")) %>%
  summarise(median_mc = median(value, na.rm = TRUE),
            std_dev = sd(ret, na.rm = TRUE)) %>%
  ungroup() %>%
  lm(log10(std_dev) ~ log10(median_mc), data = .) %>%
  summary()

# size data frame
mean_mc <- pivot_longer(df_mc, cols = -Data) %>%
  filter(ymd(Data) < tail(df$Data, n = 1)) %>%
  mutate(y = year(ymd(Data)),
         name = str_split_fixed(name, "_", n = 3)[,1]) %>%
  group_by(y, name) %>%
  summarise(mean_cap = mean(value, na.rm = TRUE)) %>%
  ungroup() %>%
  rename("ticker" = name) %>%
  merge(rename(invest_tickers, "ticker" = name), by = "ticker") %>%
  group_by(y, sector) %>%
  summarise(median_sector_cap = median(mean_cap, na.rm = TRUE), ticker, mean_cap) %>%
  ungroup() %>%
  mutate(size = ifelse(mean_cap > median_sector_cap, "big", "small"))


## 4 portfolios - SS big/small and SV big/small ###############

stress_ranks <- rename(mean_mc, "year_t" = y, "name" = ticker) %>%
                  merge(stress_stock_rets, by  = c("name", "year_t", "sector")) %>%
                  group_by(year_t, sector, size) %>%
                  summarise(rank = rank(stress_ret, na.last = "keep"),
                              n_stocks = sum(!is.na(size)),name, stress_ret) %>%
                  ungroup() %>%
                  mutate(rel_rank = rank/n_stocks,
                         stress_stable = case_when(rel_rank >   0.5 ~ 1,
                                                   rel_rank <= 0.5 ~ 0),
                         year_t = year_t - 1) 
  

portfolio_SSbigsmall <- filter(df, Data >= backtest_t0) %>%
  mutate_at(vars(-Data), ret) %>%
  pivot_longer(cols = -Data) %>%
  mutate(year_t = year(ymd(Data))) %>%
  merge(select(stress_ranks, year_t, stress_stable, name, size, rel_rank),
        by  = c("year_t", "name")) %>%
  mutate(Data = ymd(Data),
         stress_stable = recode(stress_stable, '0' = "SV", '1' = "SS"),
         rel_rank = ifelse(stress_stable == "SS", rel_rank, rel_rank *(-1))) %>%
  group_by(Data, size, stress_stable) %>%
  summarise(portfolio_ret = weighted.mean(value, rel_rank, na.rm = TRUE)) %>%
  mutate(class = paste(size, "_", stress_stable, sep = "")) %>%
  ungroup()


avrg_sample_rets <- filter(df, Data >= backtest_t0) %>%
  mutate_at(vars(-Data), ret) %>%
  pivot_longer(cols = -Data) %>%
  group_by(Data) %>%
  summarise(sample_ret = mean(value, na.rm = TRUE)) %>%
  ungroup()

drop_na(portfolio_SSbigsmall) %>%
  group_by(class) %>%
  summarise(cum_ret = cumprod(portfolio_ret + 1), Data) %>%
  ungroup() %>%
  pivot_wider(names_from = class, values_from = cum_ret) %>%
  merge(mutate(avrg_sample_rets, sample_ret = cum_prod(sample_ret)), by  = "Data") %>%
  merge(select(benchmark, Data, "wig" =  Otwarcie), by = "Data") %>%
  mutate(wig = cumprod(1 + ifelse(is.na(ret(wig)), 0, ret(wig)))) %>%
  merge(select(swig, Data, "swig" = Otwarcie), by  = "Data") %>%
  mutate(swig = cumprod(1 + ifelse(is.na(ret(swig)), 0, ret(swig)))) %>%
  rename("Big SS" = big_SS,
         "Big SV" = big_SV,
         "Small SS" = small_SS,
         "Small SV" = small_SV,
         "Sample" = sample_ret,
         "WIG" = wig,
         "sWIG80" = swig) %>%
  pivot_longer(cols = -Data) %>%
  mutate(name = fct_reorder(name, value, function(x){-tail(x, n = 1)})) %>%
ggplot() +
  geom_line(aes(x = Data, y = value, color = name)) +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y") +
  scale_y_continuous(labels = scales::percent, n.breaks = 6) +
  labs(title = "Return based quality factor on Warsaw Stock Exchange",
       subtitle = "Stress stable (SS) and Stress vulnerable (SV) portfolios with benchmarks",
       x = "", y = "Cumulative return") +
  theme_ipsum() +
  theme(legend.title = element_blank())



drop_na(portfolio_SSbigsmall) %>%
  select(Data, class, portfolio_ret) %>%
  pivot_wider(names_from = class, values_from = portfolio_ret) %>%
  merge(select(benchmark, Data, "wig" =  Otwarcie), by = "Data") %>%
  merge(select(swig, Data, "swig" = Otwarcie), by  = "Data") %>%
  merge(avrg_sample_rets, by  = "Data") %>%
  mutate_at(vars(wig, swig), ret) %>%
  drop_na() %>%
  pivot_longer(cols = -Data) %>%
  group_by(name) %>%
  summarise('Sharpe ratio' = sqrt(252) *( mean(value, na.rm = TRUE)/sd(value, na.rm = TRUE)),
            'Cumulative return' = prod(value + 1, na.rm = TRUE),
            'Annualized return' = prod(value + 1, na.rm = TRUE) ^ (1/8),
            'Annualized volatility' = sqrt(252) * sd(value, na.rm = TRUE),
            max_dd = max_dd(value)) %>%
  arrange(desc(`Sharpe ratio`))

## performance viz ###########

filter(portfolio_SSbigsmall, class == "big_SS") %>%
  left_join(avrg_sample_rets, by = "Data") %>%
  mutate(portfolio_ret = portfolio_ret - sample_ret) %>% # tutaj zmien jesli nie chcesz excess
  mutate(day_of_year = yday(Data),
         year_t = year(Data),
         portfolio_ret = replace(portfolio_ret, is.na(portfolio_ret), 0)) %>%
  filter(day_of_year >= yday(Sys.Date())) %>%  # tutaj zmien dzien od ktorego startowac
  group_by(year_t) %>%
  mutate(cum_ret = cumprod(portfolio_ret + 1),
         positive_ret = tail(cum_ret, n = 1) > 1) %>%
  ungroup() %>%
  ggplot(aes(y = cum_ret - 1, x = day_of_year, color = as.factor(year_t))) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = 2) +
  scale_y_percent(n.breaks = 10)

filter(portfolio_SSbigsmall, class == "big_SS") %>%
  mutate(day_of_year = yday(Data),
         year_t = year(Data),
         portfolio_ret = replace(portfolio_ret, is.na(portfolio_ret), 0)) %>%
  filter(day_of_year >= yday(Sys.Date())) %>%
  group_by(year_t) %>%
  mutate(cum_ret = cumprod(portfolio_ret + 1),
         positive_ret = tail(cum_ret, n = 1) > 1) %>%
  ungroup() %>%
  ggplot(aes(y = cum_ret - 1, x = day_of_year)) +
  geom_line() +
  facet_wrap(~ year_t) +
  geom_hline(yintercept = 0, linetype = 2)


filter(portfolio_SSbigsmall, class == "big_SS") %>%
  mutate(day_of_year = yday(Data),
         year_t = year(Data),
         portfolio_ret = replace(portfolio_ret, is.na(portfolio_ret), 0)) %>%
  filter(day_of_year >= yday(Sys.Date())) %>%
  group_by(year_t) %>%
  mutate(cum_ret = cumprod(portfolio_ret + 1),
         positive_ret = tail(cum_ret, n = 1) > 1) %>%
  ungroup() %>%
  group_by(day_of_year) %>%
  summarise(ret = mean(portfolio_ret, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(cum_ret = cumprod(ret + 1)) %>%
  ggplot(aes(x = day_of_year, y = cum_ret)) +
  geom_line()

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
