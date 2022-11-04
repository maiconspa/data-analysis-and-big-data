# Data preparation
# setwd(paste(getwd(), "/output/oct11/", sep = ""))

dataset <- read.table(
  paste(getwd(), "/data/corn_harvest.csv", sep = ""),
  sep = ",",
  header = TRUE,
  quote = "\"'",
  dec = ","
)

years <- unique(dataset$AnoSafra)


getmode <- function(column) {
  uniqv <- unique(column)
  uniqv[which.max(tabulate(match(column, uniqv)))]
}

# a) Para cada ano, calcular : Média, Mediana, Percentil, Moda.
#    Gravar arquivo chamado item-I-a.csv com os valores obtidos,
#    separados por vírgula, com cabeçalho e com ponto decimal.
df_final_a <- data.frame()

for (i in 1:length(years)) {
  df_temp <- dataset[dataset$AnoSafra == years[i], ]

  mean <- round(mean(df_temp$PrecosMilho), 2)
  median <- round(median(df_temp$PrecosMilho), 2)
  percent <- round(
    data.frame(quantile(df_temp$PrecosMilho, probs=c(.25,.75))),
    2
  )
  mode <- getmode(df_temp$PrecosMilho)

  harvest_analysis <- c(
    years[i],
    mean,
    median,
    percent[1, 1],
    percent[2, 1],
    mode
  )

  df_final_a <- rbind(df_final_a, data.frame(harvest_analysis))
}

View(df_final_a)

colnames(df_final_a) <- c("Years", "Mean", "Median", "25%", "75%", "Mode")
write.table(df_final_a, "item-I-a.csv", sep = ",", dec = ".", row.names = FALSE)


# b) Para preço entre 25 e 27 reais, calcular : Frequência, Média, Mediana,
#    Percentil, Moda. Gravar arquivo chamado item-I-b.csv com os valores
#    obtidos, separados por vírgula, com cabeçalho e com ponto decimal.
df_final_b <- data.frame()


# c) Ordenar em ordem crescente por data. Gravar arquivo chamado
#    item-I-c.csv com os valores ordenados, separados por vírgula,
#    com cabeçalho e com ponto decimal.
df_final_c <- data.frame()


# d) Ordenar em ordem crescente por preço do milho. Gravar arquivo
#    chamado item-I-d.csv com os valores ordenados, separados por vírgula,
#    com cabeçalho e com ponto decimal.
df_final_d <- data.frame()


# Utils
