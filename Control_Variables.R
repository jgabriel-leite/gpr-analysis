# =========================
# Pacotes ----
# =========================

#Código para baixar e colocar no formato para ser usado na regressão as variáveis
#controle. As variáveis são as seguintes:
#STOCK PRICE VOLATILITY INDICATOR - Fonte: Banco Mundial ou Bloomberg?
# Taxa de inflação - Fonte: FMI com o pacote imfapi
# Taxa de câmbio real efetiva - Fonte: FMI com o pacote imfapi
# Taxa de juros - Fonte: FMI com o pacote imfapi
#         Taxa usada: MFS166_RT_PT_A_PT - Monetary policy-related, Rate, Percent per annum
#Output gap - Fonte: FMI com o pacote imfapi
#         Indicador usado: NGAP_NPGDP - Output gap, Percent of potential GDP
#Rating de crédito "quantificado" (AINDA SEM DECIDIR QUAL/COMO USAR)
#Relação dívida-PIB para governo geral - Fonte: FMI com o pacote imfapi
#         Indicador usado: GGXWDG_NGDP - Gross debt, General government, Percent of GDP

#Baixa e lê os pacotes necessários
if (!require(dplyr)) install.packages("dplyr")
if (!require(tidyr)) install.packages("tidyr")
if (!require(tidyverse)) install.packages("tidyverse")
if (!require(imfapi)) install.packages("imfapi")
if (!require(WDI)) install.packages("WDI")


library(dplyr)
library(tidyr)
library(tidyverse)
library(imfapi)
library(WDI)

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

# =========================
# Taxa de inflação ----
# =========================

#Puxar os dados para os países da base
inflation <- imf_get(
  dataflow_id = "CPI",
  dimensions = list(
    COUNTRY = GPR_countries,
    INDEX_TYPE = "CPI",
    COICOP_1999 = "_T",
    TYPE_OF_TRANSFORMATION = "POP_PCH_PA_PT",
    FREQUENCY = "A"),
  max_tries = 10000L
) 

#Ficar apenas com os dados para 1985 em diante
inflation <- inflation %>%
  filter(TIME_PERIOD >= 1985)

#Ficar apenas com as variáveis de interesse e no mesmo formato da base para regressão

inflation <- inflation %>%
  select(COUNTRY, TIME_PERIOD, OBS_VALUE) %>%
  rename(
    country = COUNTRY,
    year = TIME_PERIOD,
    inflation = OBS_VALUE
  )

#Checar os países que não estão na base
setdiff(GPR_countries, unique(inflation$country))

#NESSE BASE DE DADOS NÃO HÁ REGISTROS PARA TAIWAN E NEM PARA UNIÃO SOVIÉTICA

##O arquivo a ser usado na regressão será salvo em data-raw.
write.csv(inflation, "data-raw/inflation.csv")

# ================================
# Taxa de câmbio real efetiva ----
# ================================

#Puxar os dados para os países da base
reer <- imf_get(
  dataflow_id = "EER",
  dimensions = list(
    COUNTRY = GPR_countries,
    INDICATOR = "REER_IX_RY2010_ACW_RCPI",
    FREQUENCY = "A"),
  max_tries = 10000L
) 

#Ficar apenas com os dados para 1985 em diante
reer <- reer %>%
  filter(TIME_PERIOD >= 1985)

#Ficar apenas com as variáveis de interesse e no mesmo formato da base para regressão

reer <- reer %>%
  select(COUNTRY, TIME_PERIOD, OBS_VALUE) %>%
  rename(
    country = COUNTRY,
    year = TIME_PERIOD,
    reer = OBS_VALUE
  )

#Checar os países que não estão na base
setdiff(GPR_countries, unique(reer$country))

#Países ausentes da lista com dados de REER disponíveis no FMI:
#  Argentina 
#  Australia 
#  Egypt 
#  Hong Kong 
#  Indonesia 
#  India 
#  South Korea 
#  Peru 
#  Soviet Union (historical series) 
#  Thailand 
#  Turkey 
#  Taiwan 
#  Vietnam

##O arquivo a ser usado na regressão será salvo em data-raw.
write.csv(reer, "data-raw/reer.csv")

# ===================
# Taxa de juros ----
# ===================

#Puxar os dados para os países da base
interest <- imf_get(
  dataflow_id = "MFS_IR",
  dimensions = list(
    COUNTRY = GPR_countries,
    INDICATOR = "MFS166_RT_PT_A_PT",
    FREQUENCY = "A"),
  max_tries = 10000L
) 

#Ficar apenas com os dados para 1985 em diante
interest <- interest %>%
  filter(TIME_PERIOD >= 1985)

#Ficar apenas com as variáveis de interesse e no mesmo formato da base para regressão
interest <- interest %>%
  select(COUNTRY, TIME_PERIOD, OBS_VALUE) %>%
  rename(
    country = COUNTRY,
    year = TIME_PERIOD,
    interest = OBS_VALUE
  )

#Checar os países que não estão na base
setdiff(GPR_countries, unique(interest$country))

#Países ausentes dessa base de dados:
# Venezuela
# Finland
# Ukraine
# United Kingdom
# Soviet Union (historical series)
# Belgium
# France
# Germany
# Italy
# Netherlands
# Portugal
# Spain
# Switzerland
# Saudi Arabia
# Tunisia
# China
# Taiwan
# India
# Vietnam

#O arquivo a ser usado na regressão será salvo em data-raw.
write.csv(interest, "data-raw/interest.csv")

# ================================
# Hiato do produto ----
# ================================

#Puxar os dados para os países da base
output_gap <- imf_get(
  dataflow_id = "WEO",
  dimensions = list(
    COUNTRY = GPR_countries,
    INDICATOR = "NGAP_NPGDP",
    FREQUENCY = "A"),
  max_tries = 10000L
) 

#Ficar apenas com os dados para 1985 em diante
output_gap <- output_gap %>%
  filter(TIME_PERIOD >= 1985)

#Ficar apenas com as variáveis de interesse e no mesmo formato da base para regressão
output_gap <- output_gap %>%
  select(COUNTRY, TIME_PERIOD, OBS_VALUE) %>%
  rename(
    country = COUNTRY,
    year = TIME_PERIOD,
    output_gap = OBS_VALUE
  )

#Checar os países que não estão na base
setdiff(GPR_countries, unique(output_gap$country))

#Países ausentes dessa base de dados:
#Mexico
#Argentina
#Brazil
#Chile
#Colombia
#Peru
#Venezuela
#Hungary
#Poland
#Russia
#Ukraine
#SovietUnion
#Switzerland
#Egypt
#Israel
#SaudiArabia
#SouthAfrica
#Tunisia
#Turkey
#China
#HongKong
#Philippines
#Taiwan
#Indonesia
#India
#Malaysia
#Thailand
#Vietnam

#O arquivo a ser usado na regressão será salvo em data-raw.
write.csv(output_gap, "data-raw/output_gap.csv")


# ================================
# Hiato do produto ----
# ================================

#Puxar os dados para os países da base
debt <- imf_get(
  dataflow_id = "WEO",
  dimensions = list(
    COUNTRY = GPR_countries,
    INDICATOR = "GGXWDG_NGDP",
    FREQUENCY = "A"),
  max_tries = 10000L
) 

#Ficar apenas com os dados para 1985 em diante
debt <- debt %>%
  filter(TIME_PERIOD >= 1985)

#Ficar apenas com as variáveis de interesse e no mesmo formato da base para regressão
debt <- debt %>%
  select(COUNTRY, TIME_PERIOD, OBS_VALUE) %>%
  rename(
    country = COUNTRY,
    year = TIME_PERIOD,
    debt = OBS_VALUE
  )

#Checar os países que não estão na base
setdiff(GPR_countries, unique(debt$country))

#Único país não presente na base de dados é a União Soviética (dados históricos)

#O arquivo a ser usado na regressão será salvo em data-raw.
write.csv(debt, "data-raw/debt.csv")


# ================================
# Juntando tudo ----
# ================================

#Junta todas as bases de dados salvas em data-raw e cria arquivo único

controls <- debt %>%
  full_join(inflation, by = c("country", "year")) %>%
  full_join(interest, by = c("country", "year")) %>%
  full_join(output_gap, by = c("country", "year")) %>%
  full_join(reer, by = c("country", "year"))


################################################################################
################################################################################
################################################################################
################################################################################

#Olha as dimensões que há em uma base
imf_get_datastructure(dataflow_id = "WEO")

#Em cada dimensão, ver os códgos que precisam ser informados para extrair os dados
imf <- imf_get_codelists(
  dimension_id = "INDICATOR",
  dataflow_id = "WEO")

#Ver os códigos dos dataflows
imf <- imf_get_dataflows()