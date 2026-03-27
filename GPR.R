# =========================
# Pacotes ----
# =========================

#Código para baixar os dados do Geopolitical Risk Indes e plotar alguns gráficos
#explorando a correlação entre esse índice para os países da América Latina e
#do BRICS+ com dados disponíveis nessa base.

#Baixa e lê os pacotes necessários

if (!require(readxl)) install.packages("readxl")
if (!require(dplyr)) install.packages("dplyr")
if (!require(tidyr)) install.packages("tidyr")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(reshape2)) install.packages("reshape2")

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)

# =========================
# Download ----
# =========================

#Cria local, salva e importa a base de dados e local apra salvar gráficos

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



# Criar pasta "plots" se não existir
dir.create("plots", showWarnings = FALSE)

# ===========================================
# Série temporal América Latina e BRIC+ ----
# ===========================================

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