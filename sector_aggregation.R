ticker_list <- read.csv(paste(data_folder,"/", "ticker_list.csv", sep = ""))[,-1]


agg_sectors <- list(chem_materials = c("Chemia", "Drewno i papier", "Górnictwo", "Tworzywa i guma", "Hutnictwo", "Recykling"),
                    consumer_goods = c("Inne dobra konsumpcyjne", "Produkcja żywności", "Motoryzacja", "Odzież i kosmetyki", "Wyposażenie domu"),
                    finance = c("Banki", "Finanse pozostałe", "Rynek kapitałowy", "Ubezpieczenia"),
                    trade_services = c("Handel hurtowy", "Handel internetowy", "Media", "Pozostały handel i usługi", "Rekreacja i wypoczynek", "Sieci handlowe"),
                    healthcare = c("Ochrona zdrowia", "Biotechnologia"),
                    energy = c("Paliwa", "Energia", "Dystrybucja ciepła i wody"),
                    industrials = c("Elektromaszynowy", "Transport", "Usługi dla przedsiębiorstw", "Zaopatrzenie przedsiębiorstw"),
                    construction = c("Budownictwo", "Nieruchomości"),
                    tech = c("Gry video", "Informatyka", "Telekomunikacja", "Nowe technologie"))%>%
  unlist()%>%
  as.data.frame() %>%
  data.frame(sector = gsub('[[:digit:]]+', '',row.names(.)))


df <- select(agg_sectors, "sector" = `.`, "agg_sector" = sector) %>%
  as_tibble() %>%
  left_join(rename(ticker_list, "agg_sector2" = "agg_sector"), by  = "sector") %>%
  select('.', stock_name, ticker, sector, agg_sector)

write.csv(df, file = "data/ticker_sectors.csv")

# . ; stock_name ; ticker ; sector ; agg_sector


df1 <- read.csv(paste(data_folder,"/", "ticker_list.csv", sep = ""))[,-1] %>% drop_na()
df2 <- read.csv(paste(data_folder,"/", "ticker_sectors.csv", sep = ""))[,-1] %>% drop_na()

colnames(df2)
