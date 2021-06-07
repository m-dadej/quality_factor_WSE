ticker_list <- read.csv(paste(data_folder,"/", "ticker_list.csv", sep = ""))[,-1]


agg_sectors <- list(chem_materials = c("Chemia", "Drewno i papier", "G�rnictwo", "Tworzywa i guma", "Hutnictwo", "Recykling"),
                    consumer_goods = c("Inne dobra konsumpcyjne", "Produkcja �ywno�ci", "Motoryzacja", "Odzie� i kosmetyki", "Wyposa�enie domu"),
                    finance = c("Banki", "Finanse pozosta�e", "Rynek kapita�owy", "Ubezpieczenia"),
                    trade_services = c("Handel hurtowy", "Handel internetowy", "Media", "Pozosta�y handel i us�ugi", "Rekreacja i wypoczynek", "Sieci handlowe"),
                    healthcare = c("Ochrona zdrowia", "Biotechnologia"),
                    energy = c("Paliwa", "Energia", "Dystrybucja ciep�a i wody"),
                    industrials = c("Elektromaszynowy", "Transport", "Us�ugi dla przedsi�biorstw", "Zaopatrzenie przedsi�biorstw"),
                    construction = c("Budownictwo", "Nieruchomo�ci"),
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