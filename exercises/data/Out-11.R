archive = read.table("./corn_harvest.csv", sep = ",", header=T, quote = "\"'", dec = ",")
#View(archive)

#a) Para cada ano, calcular : Média, Mediana, Percentil, Moda. 
#Gravar arquivo chamado item-I-a.csv com os valores obtidos, separados por vírgula, 
#com cabeçalho e com ponto decimal.
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

years= unique(archive$AnoSafra)

df_ex = data.frame()
df_ex

for (i in 1:length(years)){
  dataFrame= archive[archive$AnoSafra == years[i],]
  mean= round(mean(dataFrame$PrecosMilho), 2)
  median= round(median(dataFrame$PrecosMilho), 2)
  percentil= round(data.frame(quantile(dataFrame$PrecosMilho, probs=c(.25,.75))), 2)
  mode= getmode(dataFrame$PrecosMilho)
  df_ex = rbind(df_ex, data.frame(years[i],mean, median, percentil[1,1], percentil[2,1],mode))
}
  
View(df_ex)

colnames(df_ex)<-c("Years","Mean","Median","25%","75%","Mode")
write.table(df_ex, "item-I-a.csv", sep=",", dec=".", row.names = F)


#b) Para preço entre 25 e 27 reais, calcular : Frequência, Média, Mediana,
#Percentil, Moda. Gravar arquivo chamado item-I-b.csv com os valores obtidos, 
#separados por vírgula, com cabeçalho e com ponto decimal.

#c) Ordenar em ordem crescente por data. Gravar arquivo chamado item-I-c.csv 
#com os valores ordenados, separados por vírgula, com cabeçalho e com ponto 
#decimal.

#d) Ordenar em ordem crescente por preço do milho. Gravar arquivo chamado 
#item-I-d.csv com os valores ordenados, separados por vírgula, com cabeçalho e 
#com ponto decimal.