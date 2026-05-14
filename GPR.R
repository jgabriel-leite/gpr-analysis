# =========================
# Pacotes ----
# =========================

#Código para baixar os dados do Geopolitical Risk Indes e criar um indicador de 
#exposição geopolítica para cada país com base na proximidade geográfica de cada
#parceiro bilateral e outro com base nos peso dos parceiros comerciais. 
#Também conta com códigos para plotar alguns gráficos explorando a correlação 
#entre esse índice para os países da América Latina e do BRICS+ com dados disponíveis nessa base.

#Baixa e lê os pacotes necessários

if (!require(readxl)) install.packages("readxl")
if (!require(dplyr)) install.packages("dplyr")
if (!require(tidyr)) install.packages("tidyr")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(reshape2)) install.packages("reshape2")
if (!require(cepiigeodist)) install.packages("cepiigeodist")
if (!require(lubridate)) install.packages("lubridate")
if (!require(stringr)) install.packages("stringr")
if (!require(imfapi)) install.packages("imfapi")

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)
library(cepiigeodist)
library(lubridate)
library(stringr)
library(imfapi)

# =========================
# Download ----
# =========================

#Cria local, salva e importa a base de dados

dir.create("data-raw", showWarnings = FALSE)

download.file(
  "https://www.matteoiacoviello.com/gpr_files/data_gpr_export.xls",
  destfile = "data-raw/data_gpr_export.xls",
  mode = "wb"
)

data_gpr_export <- read_excel("data-raw/data_gpr_export.xls", 
                              col_types = c("date", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "text", "text"))


#Ficar apenas com dados da média anual do GPR para os países da base e retira
#observações antes de 1985, ano em que começa o índice não histórico do GPR

data_gpr <- data_gpr_export %>%
  mutate(year = year(month)) %>%
  group_by(year) %>%
  summarise(
    across(
      where(is.numeric),
      ~ mean(.x, na.rm = TRUE)
    )
  )

data_gpr <- data_gpr %>%
  filter(year >= 1985)

# ======================================================
# Ajuste na base de dados para incluir peso geográfico -
# ======================================================

#Ajusta a base do GPR para contar apenas com os dados anuais e cria um indicador
#específico para a exposição de cada país aos risco geopolítico dos países mais
#próximos. Faz isso com base nos dados de distância geográfica do CEPII, dando 
#peso maior ao risco geopolítico de países mais próximos.

#Baixa os dados da CEPII. O pacote CEPII tem apenas duas funções dist_cepii 
#e geo_cepii para puxar os dados. geo_cepii tem dados do tipo dummies culturais,
#políticos e históricos para os países da base.
dist <- dist_cepii

#Código para listar apenas os países da base de dados do GPR index
GPR_countries <- c(
  # North America
  "CAN", "MEX", "USA",
  
  # South America
  "ARG", "BRA", "CHL", "COL", "PER", "VEN",
  
  # Europe (North and East)
  "DNK", "FIN", "HUN", "NOR", "POL",
  "RUS", "SWE", "UKR", "GBR", "SUN", #SUN é necessário para ter os dados da Rússia na época da URSS
  
  # Europe (South and West)
  "BEL", "FRA", "DEU", "ITA",
  "NLD", "PRT", "ESP", "CHE",
  
  # Middle East and Africa
  "EGY", "ISR", "SAU", "ZAF",
  "TUN", "TUR",
  
  # Asia and Oceania
  "AUS", "CHN", "HKG", "JPN",
  "KOR", "PHL", "TWN", "IDN",
  "IND", "MYS", "THA", "VNM"
)

#Ficar apenas com os países da base do GPR na base do CEPII como origem
#e retira a distância do país consigo mesmo

dist <- dist %>%
  filter(
    iso_o %in% GPR_countries,
    #iso_d %in% GPR_countries, usar esse filtro caso queira deixar também apenas como destino, o que aumenta muito o peso de países mais perto
    iso_o != iso_d
    )

#Cria pesos para cada parceiro bilateral. Países mais perto têm peso maior

dist <- dist %>%
  group_by(iso_o) %>%
  mutate(
    weight_dist = 1 / dist,
    weight_dist = weight_dist / sum(weight_dist, na.rm = TRUE)
  ) %>%
  ungroup()

#Transformar GPRC em formato longo
gpr_long <- data_gpr %>%
    pivot_longer(
    cols = starts_with("GPRC_"),
    names_to = "iso_d",
    names_prefix = "GPRC_",
    values_to = "GPRC"
  )

#Juntar com pesos de distância
gpr_geo_weighted <- gpr_long %>%
  left_join(
    dist %>%
      select(iso_o, iso_d, weight_dist),
    by = "iso_d"
  )

#Calcular contribuição ponderada
gpr_geo_weighted <- gpr_geo_weighted %>%
  mutate(
    geo_weighted_GPRC = GPRC * weight_dist
  )

#Somar para cada país de origem e período
geo_exposure <- gpr_geo_weighted %>%
  group_by(year, iso_o) %>%
  summarise(
    geo_exposure = sum(geo_weighted_GPRC, na.rm = TRUE),
    .groups = "drop"
  )

#Voltar para wide
geo_exposure_wide <- geo_exposure %>%
  pivot_wider(
    names_from = iso_o,
    values_from = geo_exposure,
    names_prefix = "GEOEXP_"
  )

#Juntar de volta na base específica para GPR com peso geográfico
data_gpr_geo <- data_gpr %>%
  left_join(
    geo_exposure_wide,
    by = "year"
  )

#geo_exposure é a base de dados que será usada para a regressão com o GPR ponderado
#por proximidade geográfica. O data_gpr_geo serve para ter os dados do indicador
#ponderado na mesma base para comparação ou manipulação conjunta, caso necessário.
#O arquivo a ser usado na regressão será salvo em data-raw.

geo_exposure <- geo_exposure %>%
  rename(
    country = iso_o
  )

write.csv(geo_exposure, "data-raw/geo_exposure.csv", row.names = FALSE)

#Comparação visual de como estão variando o indicador global (GPR), o indicador
#do país (GPRC_XXX) e o indicador da exposição aos demais países por proximidade
#geográfica (GEOEXP_XXX)

plot_data <- data_gpr_geo %>%
  select(year, GPR, GPRC_CHN, GEOEXP_CHN) %>%
  mutate(across(-year, scale)) %>%
  pivot_longer(
    cols = -year,
    names_to = "serie",
    values_to = "valor"
  )

ggplot(plot_data, aes(year, valor, color = serie)) +
  geom_line(size = 1.2) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Séries padronizadas (z-score)",
    x = "Ano",
    y = "Desvio-padrão",
    color = "Série"
  )

# ======================================================
# Ajuste na base de dados para incluir peso comercial  -
# ======================================================

#Pegando os dados de comércio da base de dados do FMI com o pacto imfapi. Como
#trava ao tentar baixar todos os dados de uma vez só, vamos baixar apenas os 
#dados dos países da base de dados do GPR (lista criada acima dos GPR_countries)
#primeiro as importações, depois as exportações e juntar tudo a partir do ano
#em que há dados do indicador GPR (não estamos usando o indicador GPR histórico). 

#Dados de importação
data_trade_M <- imf_get(
  dataflow_id = "IMTS",
  dimensions = list(
    COUNTRY = GPR_countries,
    INDICATOR = "MG_CIF_USD",
    FREQUENCY = "A"),
  max_tries = 10000L
)

#Dados de exportação
data_trade_X <- imf_get(
  dataflow_id = "IMTS",
  dimensions = list(
    COUNTRY = GPR_countries,
    INDICATOR = "XG_FOB_USD",
    FREQUENCY = "A"),
  max_tries = 10000L
)

#Juntando os dois para obter a corrente de comércio (X+M)
data_trade <- bind_rows(data_trade_M, data_trade_X) %>%
  filter(as.numeric(TIME_PERIOD) >= 1985) %>%
  group_by(COUNTRY, COUNTERPART_COUNTRY, TIME_PERIOD) %>%
  summarise(
    total_trade = sum(OBS_VALUE, na.rm = TRUE),
    .groups = "drop"
  )

#Nos dados do FMI, há em COUNTERPART_COUNTRIES vários agregados e lista histórica
#de países. Os agregados serão retirados e, por simplicidade, será considerado o
#comércio da URSS (SUN) - 1985-92 - como Rússia (RUS) - 1993 em diante. Nenhum 
#outro país/território histórico e seu sucessor constam da base individual do GPR, 
#de modo que não terá diferença na hora de calcular os GPRC ponderado para comércio.

#Retirar agregados

aggregates <- c(
  "G001","G080","G092","G110","G163","G200","G205",
  "G400","G505","G603","G903","G998",
  "GX170","GX405","GX440","GX605","GX901",
  "TX126","TX399","TX489","TX598","TX799",
  "TX884","TX898","TX899","TX910"
)

data_trade <- data_trade %>%
  filter(!COUNTERPART_COUNTRY %in% aggregates)

#Substituindo União Soviética (SUN) por Rússia (RUS)

data_trade <- data_trade %>%
  mutate(
    COUNTRY = if_else(COUNTRY == "SUN", "RUS", COUNTRY),
    COUNTERPART_COUNTRY = if_else(COUNTERPART_COUNTRY == "SUN", "RUS", COUNTERPART_COUNTRY)
  )

#Criando pesos para o comércio bilateral
data_trade <- data_trade %>%
  group_by(COUNTRY, TIME_PERIOD) %>%
  mutate(
    trade_share = total_trade / sum(total_trade, na.rm = TRUE)
  ) %>%
  ungroup()

#Taiwan não está incluído nos dados de comércio do FMI como país reportante, apenas
#como país reportado. Para isso, se for para incluir Taiwan entre os países com
#GPRC de exposição comercial calculado é preciso obter os dados para Taiwan de outras 
#fontes e acrescentar à base de dados de pesos bilaterais. TENTEI COM WDI E LÁ
#NÃO TEM TAIWAN TAMBÉM. OMC TEM TAIPEI NO PORTAL DE ESTATÍSTICAS, MAS NÃO ENCONTREI
#COMO BAIXAR OS DADOS DE COMÉRCIO BIALTERAL PARA TAIWAN LÁ, APENAS VISUALIZAR. POR ORA, PRIMEIRO TESTE COM OS DADOS SEM TAIWAN
#BÉLGICA TEM DADOS APENAS A PARTIR DE 1997 NA BASE DE COMÉRCIO. POR ORA, PRIMEIRO TESTE COM DADOS SEM AJUSTE DESSA PARTE

#Aplicando os pesos do comércio bilateral para calcular o GPR ponderado pela
#exposição ao comércio

#Transformar GPRC em formato longo
gpr_long <- data_gpr %>%
    pivot_longer(
    cols = starts_with("GPRC_"),
    names_to = "iso_d",
    names_prefix = "GPRC_",
    values_to = "GPRC"
  )

#Preparar a base de dados dos pesos de comércio para juntar com a outra
trade_weights <- data_trade %>%
  transmute(
    iso_o = COUNTRY,
    iso_d = COUNTERPART_COUNTRY,
    year = as.numeric(TIME_PERIOD),
    trade_share
  )

#Incluir coluna referente ao país de interesse em relação aos demais parceiros
countries <- unique(trade_weights$iso_o)
gpr_long <- gpr_long %>%
  tidyr::crossing(iso_o = countries) %>%
  filter(iso_o != iso_d)

#Juntar as duas bases de dados
gpr_trade_weighted <- gpr_long %>%
  left_join(
    trade_weights,
    by = c("iso_o", "iso_d", "year")
  )

#Calcular contribuição ponderada
gpr_trade_weighted <- gpr_trade_weighted %>%
    mutate(
    trade_weighted_GPRC = GPRC * trade_share
  )

#Somar para cada país de origem e período
trade_exposure <- gpr_trade_weighted %>%
  group_by(year, iso_o) %>%
  summarise(
    trade_exposure = sum(trade_weighted_GPRC, na.rm = TRUE),
    .groups = "drop"
  )

#TESTES E CHECAGENS COM AS BASES. PODE SER NECESSÁRIO VOLTAR AQUI DEPOIS E FAZER
#TESTES DE ROBUSTEZ PARA VERIFICAR SE OS DADOS NORMALIZADOS SERIAM NECESSÁRIOS,
#UMA VEZ QUE O INDICADOR COM PESO PODE ESTAR DANDO MUITO MAIS PESO PARA PAÍSES
#COM DADOS MELHORES DE COMÉRCIO AOS QUE, POR ACASO, TEM DADOS INDIVIDUAIS NO GPRC
#I.E. UM VIÉS PARA PAÍSES QUE TROCAM MAIS COM PAÍSES COM MELHORES DADOS OU MAIS 
#IMPORTANTES (O QUE JÁ SE OBSERVARIA NA PRÁTICA DE QQ FORMA) E HÁ MUITO RUÍDO E 
#DADOS FALATANDO ANTES DE 1997 PARA PAÍSES COMO BÉLGICA, ÁFRICA DO SUL E UCRÂNIA
#ALÉM DA QUESTÃO DE TAIWAN FALTANDO COMO REPORTING

#Voltar para wide
trade_exposure_wide <-  trade_exposure %>%
  pivot_wider(
    names_from = iso_o,
    values_from = trade_exposure,
    names_prefix = "GPRC_TRADE_"
  )

#Juntar com a base mais geral
data_gpr_trade <- data_gpr %>%
  left_join(
    trade_exposure_wide,
    by = "year"
  )

#trade_exposure é a base de dados que será usada para a regressão com o GPR ponderado
#por relação comercial. O data_gpr_trade serve para ter os dados do indicador
#ponderado na mesma base para comparação ou manipulação conjunta, caso necessário.
#O arquivo a ser usado na regressão será salvo em data-raw.

trade_exposure <- trade_exposure %>%
  rename(
    country = iso_o
  )

write.csv(trade_exposure, "data-raw/trade_exposure.csv")

#Comparação visual de como estão variando o indicador global (GPR), o indicador
#do país (GPRC_XXX) e o indicador da exposição aos demais países por proximidade
#geográfica (TRADE_XXX)

plot_data <- data_gpr_trade %>%
  filter(year < 2026) %>%
  select(year, GPR, GPRC_TRADE_BRA, GPRC_TRADE_ARG, GPRC_TRADE_COL) %>%
  mutate(across(-year, scale)) %>%
  pivot_longer(
    cols = -year,
    names_to = "serie",
    values_to = "valor"
  )

ggplot(plot_data, aes(year, valor, color = serie)) +
  geom_line(size = 1.2) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Séries padronizadas (z-score)",
    x = "Ano",
    y = "Desvio-padrão",
    color = "Série"
  )

# ===========================================
# Série temporal América Latina e BRICS+ ----
# ===========================================

# Criar pasta "plots" se não existir
dir.create("plots", showWarnings = FALSE)

#Extrai apenas os países da América Latina e cria observações "data-país" e retira
#as linhas vazias (dados disponíveis apenas a partir de 1990)

df_latam <- data_gpr_export %>%
  select(month,
         `GPRC_ARG`,
         `GPRC_BRA`,
         `GPRC_CHL`,
         `GPRC_COL`,
         `GPRC_MEX`,
         `GPRC_PER`,
         `GPRC_VEN`) %>%
  pivot_longer(-month, names_to = "country", values_to = "value")

df_latam <- df_latam %>%
  filter(!is.na(value))

#Altera o nome da variável para melhorar a visualização no gráfico

df_latam <- df_latam %>%
  mutate(country = recode(country,
                          "GPRC_BRA" = "Brazil",
                          "GPRC_ARG" = "Argentina",
                          "GPRC_CHL" = "Chile",
                          "GPRC_COL" = "Colombia",
                          "GPRC_MEX" = "Mexico",
                          "GPRC_PER" = "Peru",
                          "GPRC_VEN" = "Venezuela"
  ))

#Cria gráfico mostrando a variação do índice para países da América Latina

gpr_latam <- ggplot(df_latam, aes(x = month, y = value, color = country)) +
             geom_line(size = 1) +
             labs(title = "GPR - Latin America",
                  x = "Month",
                  y = "Percent of articles") +
             theme_minimal()

# Salvar gráfico
ggsave(
  filename = "plots/gpr_latam_plot.png",
  plot = gpr_latam,              # nome do objeto do gráfico
  width = 10,
  height = 6,
  dpi = 300
)


#Extrai apenas os países do BRICS+ e cria observações "data-país" e retira
#as linhas vazias (dados disponíveis apenas a partir de 1990)

df_brics <- data_gpr_export %>%
  select(month,
         `GPRC_BRA`,
         `GPRC_CHN`,
         `GPRC_EGY`,
         `GPRC_IDN`,
         `GPRC_IND`,
         `GPRC_RUS`,
         `GPRC_ZAF`) %>%
  pivot_longer(-month, names_to = "country", values_to = "value")

df_brics <- df_brics %>%
  filter(!is.na(value))

#Altera o nome da variável para melhorar a visualização no gráfico

df_brics <- df_brics %>%
  mutate(country = recode(country,
                          "GPRC_BRA" = "Brazil",
                          "GPRC_CHN" = "China",
                          "GPRC_EGY" = "Egypt",
                          "GPRC_IDN" = "Indonesia",
                          "GPRC_IND" = "India",
                          "GPRC_RUS" = "Russia",
                          "GPRC_ZAF" = "South Africa"
  ))

#Cria gráfico mostrando a variação do índice para países do BRICS+

gpr_brics <- ggplot(df_brics, aes(x = month, y = value, color = country)) +
             geom_line(size = 1) +
             labs(title = "GPR - BRICS+",
                  x = "Month",
                  y = "Percent of articles") +
             theme_minimal()

# Salvar gráfico
ggsave(
  filename = "plots/gpr_brics_plot.png",
  plot = gpr_brics,              # nome do objeto do gráfico
  width = 10,
  height = 6,
  dpi = 300
)

# ===========================================
# Covariância América Latina e BRIC+ ----
# ===========================================

#Cria matriz de covariância das observações para os países da América Latina

df_wide_latam <- data_gpr_export %>%
  select(month,
         GPRC_ARG, GPRC_BRA, GPRC_CHL, 
         GPRC_COL, GPRC_MEX, GPRC_PER, GPRC_VEN) %>%
  arrange(month)

cor_mat_latam <- cor(df_wide_latam %>% select(-month), use = "pairwise.complete.obs")

cor_df_latam <- melt(cor_mat_latam)

#Cria gráfico no estilo heatmap para mostrar entre quais países há maior covariância

cov_latam <- ggplot(cor_df_latam, aes(Var1, Var2, fill = value)) +
             geom_tile() +
             scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
             theme_minimal() +
             labs(title = "Correlation of GPR Across Countries",
                  x = "", y = "", fill = "Correlation") +
             theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Salvar gráfico
ggsave(
  filename = "plots/cov_latam_plot.png",
  plot = cov_latam,              # nome do objeto do gráfico
  width = 10,
  height = 6,
  dpi = 300
)

#Cria matriz de covariância das observações para os países do BRICS+

df_wide <- data_gpr_export %>%
  select(month,
         GPRC_BRA, GPRC_CHN, GPRC_EGY,
         GPRC_IDN, GPRC_IND, GPRC_RUS, GPRC_ZAF) %>%
  arrange(month)

cor_mat <- cor(df_wide %>% select(-month), use = "pairwise.complete.obs")

cor_df <- melt(cor_mat)

#Cria gráfico no estilo heatmap para mostrar entre quais países há maior covariância

cov_brics <- ggplot(cor_df, aes(Var1, Var2, fill = value)) +
             geom_tile() +
             scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
             theme_minimal() +
             labs(title = "Correlation of GPR Across Countries",
                  x = "", y = "", fill = "Correlation") +
             theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Salvar gráfico
ggsave(
  filename = "plots/cov_brics_plot.png",
  plot = cov_brics,              # nome do objeto do gráfico
  width = 10,
  height = 6,
  dpi = 300
)

# ======================================================
# Normalizando os dados para América Latina e BRIC+ ----
# ======================================================

#Cria base de dados com os z-scores para América Latina

df_latam_z <- df_latam %>%
              group_by(country) %>%
              mutate(z_score = as.numeric(scale(value))) %>%
              ungroup()

#Gera o gráfico para América Latina

ggplot(df_latam_z, aes(x = month, y = z_score, color = country)) +
  geom_line(alpha = 0.6) +
  geom_hline(yintercept = c(-2, 2), linetype = "dashed") +
  theme_minimal() +
  labs(title = "Normalized shocks (z-scores) by country",
       y = "Z-score", x = "Month")

ggplot(df_latam_z, aes(x = month, y = country, fill = z_score)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red") +
  theme_minimal() +
  labs(title = "Synchronized shocks across countries",
       fill = "Z-score")

df_latam_sync <- df_latam_z %>%
  group_by(month) %>%
  summarise(n_shocks = sum(abs(z_score) > 2, na.rm = TRUE))

ggplot(df_latam_sync, aes(x = month, y = n_shocks)) +
  geom_col() +
  theme_minimal() +
  labs(title = "Number of countries in extreme shock",
       y = "Count")

#Cria base de dados com os z-scores para BRICS+

df_brics_z <- df_brics %>%
              group_by(country) %>%
              mutate(z_score = as.numeric(scale(value))) %>%
              ungroup()
  
#Gera o gráfico para BRICS+  

ggplot(df_brics_z, aes(x = month, y = z_score, color = country)) +
  geom_line(alpha = 0.6) +
  geom_hline(yintercept = c(-2, 2), linetype = "dashed") +
  theme_minimal() +
  labs(title = "Normalized shocks (z-scores) by country",
       y = "Z-score", x = "Month")

ggplot(df_brics_z, aes(x = month, y = country, fill = z_score)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red") +
  theme_minimal() +
  labs(title = "Synchronized shocks across countries",
       fill = "Z-score")

df_brics_sync <- df_brics_z %>%
  group_by(month) %>%
  summarise(n_shocks = sum(abs(z_score) > 2, na.rm = TRUE))

ggplot(df_brics_sync, aes(x = month, y = n_shocks)) +
  geom_col() +
  theme_minimal() +
  labs(title = "Number of countries in extreme shock",
       y = "Count")
       