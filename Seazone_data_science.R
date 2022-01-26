#Desafio Seazone

#instalar o pacote caso nao tenha instalado
if(!require(dplyr)) install.packages("dplyr")

#pacote
library(dplyr)

# Definindo o local do arquivo
setwd("C:/Users/lucas/Desktop/Desafio Seazone 3")
getwd()

# Importando os dados
detalhes <- read.csv("desafio_details.csv", sep = ",")
preco <- read.csv("desafio_priceav.csv", sep = ",")


# Corrigindo a acentuaÃ§Ã£o dos Bairros
Encoding(detalhes$suburb) <- "UTF-8"

# Agrupando as tabelas
dados <- merge(preco,detalhes,by="airbnb_listing_id")

View(detalhes)
View(preco)
View(dados)


#-------------------------------------------------------------------------------

# 1

# Quantidade de anuncios por bairro 
num_listing <- tapply(dados$suburb, dados$suburb , FUN = length)

# Ordenando em ordem crescente
( num_listing <- sort(num_listing) )

#Grafico do numero de anuncios por bairro
g_num_listing <- barplot(num_listing, 
        main = "Número de listings por bairro",
        ylab = "Quantidade de anúncios",
        ylim = c(0,200000),
        las = 1,
        col.lab = "gray47",
        col = c("royalblue1","gold1","orangered4","orange1","darkblue"),
        cex.lab = 0.7, cex.axis = 0.7, cex = 0.7,
        border = NA
)

text(x=c(g_num_listing), y=num_listing, labels=num_listing, 
     pos=3, cex = 0.8, col = "gray20")



#-------------------------------------------------------------------------------

# 2

# Filtrando os dados dos listings ocupados
ocup <- dados[(dados$occupied == 0), ]


# Calculando o faturamento dos Id por bairro
fat_por_bairro = tapply(ocup$price_string,list(ocup$airbnb_listing_id,ocup$suburb),sum)

#verificando as 5 primeiras linhas do faturamento
head(fat_por_bairro, 5)

# Calculando o faturamento medio para cada Bairro removendo os NA
( med_canasvieiras     = mean(na.omit( fat_por_bairro[,1])) )
( med_centro           = mean(na.omit( fat_por_bairro[,2])) )
( med_ingleses         = mean(na.omit( fat_por_bairro[,3])) )
( med_jerere           = mean(na.omit( fat_por_bairro[,4])) )
( med_lagoadaconceicao = mean(na.omit( fat_por_bairro[,5])) )

# Agrupando as medias dos bairros
( fat_med = c(med_canasvieiras,med_centro,med_ingleses,med_jerere,med_lagoadaconceicao) )

# casas deciamis = 2
( fat_med = sort( round(fat_med, 2)) )

#Grafico da media de faturamento por bairro
g_fat_med <- barplot(fat_med, 
                     main = "Faturamento médio dos listings por bairro",
                     ylab = "Faturamento médio (R$)",
                     ylim = c(0,30000),
                     las = 1,
                     names = c("Centro", "Lagoa da Conceição", "Canasvieiras", "Ingleses", "Jurerê"),
                     col.lab = "gray47",
                     col = c("royalblue1","gold1","orange1","darkblue","orangered4"),
                     cex.lab = 0.7, cex.axis = 0.7, cex = 0.7,
                     border = NA
)

text(x=c(g_fat_med), y=fat_med, labels=fat_med, 
     pos=3, cex = 0.8, col = "gray20")



#-------------------------------------------------------------------------------
# 3

# Removendo os NA dos listings ocupados
ocup <- na.omit(ocup)

# Criando matriz do faturamento e seu respectivo id
M_fat <- cbind(tapply(ocup$price_string,sort(ocup$airbnb_listing_id),sum),sort(unique(ocup$airbnb_listing_id)))

# Nomeando as colunas
colnames(M_fat) <- c("Faturamento", "airbnb_listing_id")

#agrupando a matriz do faturamento com os dados gerais ocupados
faturamento <- merge(M_fat,ocup, by="airbnb_listing_id")

# para calculo da correlação vamos unificar os id
fat_unico <- distinct(faturamento, 
         faturamento$airbnb_listing_id,
         faturamento$Faturamento,
         faturamento$number_of_bedrooms,
         faturamento$number_of_bathrooms,
         faturamento$star_rating,
         faturamento$number_of_reviews)


# Verificando se existe algum NA, caso seja 0 nao ha NA
sum(is.na(ocup))


#correlacao entre o faturamento e o numero de quartos
round(corfatnumquar <- cor(fat_unico$`faturamento$Faturamento`, fat_unico$`faturamento$number_of_bedrooms`), 2)


#correlacao entre o faturamento e o numero de banheiros
round(corfatnumban <- cor(fat_unico$`faturamento$Faturamento`, fat_unico$`faturamento$number_of_bathrooms`), 2)



#correlacao entre o faturamento e a avaliacao(numero de estrelas)
round(corfatav <- cor(fat_unico$`faturamento$Faturamento`, fat_unico$`faturamento$star_rating`), 2)



#correlacao entre o faturamento e a quantidade de comentarios
round( corfatnumcom <- cor(fat_unico$`faturamento$Faturamento`, fat_unico$`faturamento$number_of_reviews`), 2)



#-------------------------------------------------------------------------------

# 4

# Removendo os dados sem reservas
reserv <- dados[(dados$booked_on != "blank"), ]

# Transformando os dados no formato Data
reserv$booked_on <- as.Date(reserv$booked_on)
reserv$date <- as.Date(reserv$date)

# Adicionando uma coluna com as antecedencias da reserva em numero de dias
reserv$antec <- with(reserv, (reserv$date) - (reserv$booked_on))

# Verificando a tabela
View(reserv)


round( antec_media <- mean(reserv$antec), 2)

# Calculo da media por bairro
tapply(reserv$antec, reserv$suburb, FUN = mean)

# a ----

# Adicionando uma coluna com os dias da semana
reserv$dia_da_semana <- with(reserv, weekdays.Date(reserv$date, abbreviate = FALSE))


fim_semana <- tapply(reserv$antec, reserv$dia_da_semana == "domingo" | reserv$dia_da_semana == "sábado", mean)

( fim_semana <- round(fim_semana,2) )

#GrÃ¡fico da antecedÃªncia mÃ©dia nos finais de semana
g_fim_semana = barplot(fim_semana, 
        main = "Antecedência média das reservas entre \n os dias da semana e finais de semana",
        ylab = "Média de dias",
        ylim = c(0,40),
        las = 1,
        names = c("Dias da semana","Finais de semana"),
        col = c("tomato3","purple4"),
        cex.lab = 0.9, cex.axis = 0.8, cex = 1,
        border = NA
)

text(x=c(g_fim_semana), y=fim_semana, labels=fim_semana, pos=3)


# Por curiosidade vamos ver se ha diferença na antecedencia media por dia da semana separadamente

# Antecedencia media por dia da semana
antec_media_por_dia = tapply(reserv$antec, reserv$dia_da_semana , FUN = mean)

# Ordenando em ordem crescente pelo dia em que mais tem antecedência na reserva
( antec_media_por_dia <- round(sort(antec_media_por_dia), 2))

#Grafico da antecedencia média por dia da semana
g_antec_media_por_dia <- barplot(antec_media_por_dia, 
        main = "Antecedência média das reservas por dia da semana",
        #xlab = "Dia da semana",
        ylab = "Média de dias",
        ylim = c(0,40),
        las = 1,
        col.lab = "gray40",
        col = c("tomato3","tomato3","purple4","purple4","tomato3","tomato3", "tomato3"),
        cex.lab = 0.7, cex.axis = 0.7, cex = 0.7,
        border = NA
)

text(x=c(g_antec_media_por_dia), y=antec_media_por_dia, labels=antec_media_por_dia, 
     pos=3, cex = 0.8, col = "gray20")











