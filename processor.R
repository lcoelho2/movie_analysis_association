# arquivo com o processamento dos dados

# Separando os gêneros dos filmes classificados com mais de um.
moviess <- movies %>% mutate(genres = strsplit(genres, "\u007C",fixed=TRUE)) %>% unnest(genres)

# Alterando o formato da data
ratingsBefore <- ratings
ratings$timestamp <- as.POSIXct(as.numeric(ratings$timestamp),origin="1970-01-01",tz="GMT")
ratings$timestamp <- format(ratings$timestamp, "%Y")

# Juntando o aquivo com os ratings e o arquivo com os gêneros
total <- ratings %>% inner_join(moviess,by="movieId")

# Analisando quem são os outliers. Foi excluído o ano de 1995, pois existem apenas 4 filmes nesse ano.  
#y1995 <- total %>% filter(timestamp == 1995)  
#movies %>% filter(movieId %in% y1995$movieId)
# Retirando outliers
tout <- total %>% filter(timestamp != 1995)  

# Agrupando a média dos ratings por gênero e tempo
tagg <- tout %>% group_by(genres,timestamp) %>% summarise(rating = mean(rating))

# Resposta da questão 1
# Analisando as maiores médias
mm <- tagg %>% group_by(genres) %>% summarise(rating = mean(rating)) %>% arrange(desc(rating)) %>%
  top_n(n=5,wt=rating)


# Questão 2
cresc <- tout %>% filter(timestamp %in% c(2005:2013) & genres != "(no genres listed)")
queda <- tout %>% filter(timestamp %in% c(1999:2005) & genres != "(no genres listed)")

#Abaixo temos os gêneros que tiveram os maiores crescimentos na média.

crescLm <- plyr::ddply(cresc %>% select(genres,timestamp,rating), "genres",  
                       function(x) coef(lm(rating ~ timestamp,data = x))[2])
names(crescLm)[2] <- "slope"
head(crescLm %>% arrange(desc(slope)))

#Abaixo temos os gêneros que tiveram as maiores quedas na média.
quedaLm <- plyr::ddply(queda %>% select(genres,timestamp,rating), "genres", 
                       slope = function(x) coef(lm(rating ~ timestamp,data = x))[2])
names(quedaLm)[2] <- "slope"
head(quedaLm %>% arrange(desc(slope)))

#E a segunda seria calcular a diferença entre o maior e o menor rating nesses períodos, por gênero. 
#Para isso usei o pacote `sqldf`. Não gerou o mesmo resultado, a ordem dos maiores não foi a mesma.
#Crescimento na média.
cresc <- sqldf::sqldf("select genres, (max(rating) - min(rating)) diff from tagg
               where timestamp between 2005 and 2013 and genres != '(no genres listed)' 
                      group by genres order by diff desc")
hcresc <- head(cresc) ##alguma coisa no processamento do slidify não estava carregando a tabela cresc direito

#Queda na média.
decr <- sqldf::sqldf("select genres, (max(rating) - min(rating)) diff from tagg 
              where timestamp between 1999 and 2005 and genres != '(no genres listed)' 
                     group by genres order by diff desc")


# Questão 3

usrRatMax <- tout %>% filter(genres != "(no genres listed)") %>% group_by(userId,genres,rating) %>%
  summarise(count = n())
usrRatxCt <- usrRatMax %>% mutate(cxr = rating * count) %>% group_by(userId,genres) %>% 
  summarise(avg = mean(cxr))

#Agora, vamos padronizar a coluna de rating, por usuário (alguns usuários que veem mais filmes que outros).
usrgensc <- plyr::ddply(usrRatxCt, "userId", function(x){ x$scavg = scale(x$avg)})
usrgensc <- mutate(usrgensc, genres = usrRatxCt$genres) #adiciona genres ao data.frame de novo
colnames(usrgensc) <- c("userId", "avgsc","genres")

#Passamos agora os gênero para colunas para fazermos a correlação.
usrgenres <- reshape::cast(usrgensc, userId ~ genres ,value= "avgsc")

# função para adicionar a significância e P values ao gráfico da correlação
cor.mtest <- function(mat, conf.level = 0.95){
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat <- lowCI.mat <- uppCI.mat <- matrix(NA, n, n)
  diag(p.mat) <- 0
  diag(lowCI.mat) <- diag(uppCI.mat) <- 1
  for(i in 1:(n-1)){
    for(j in (i+1):n){
      tmp <- cor.test(mat[,i], mat[,j], conf.level = conf.level)
      p.mat[i,j] <- p.mat[j,i] <- tmp$p.value
      lowCI.mat[i,j] <- lowCI.mat[j,i] <- tmp$conf.int[1]
      uppCI.mat[i,j] <- uppCI.mat[j,i] <- tmp$conf.int[2]
    }
  }
  return(list(p.mat, lowCI.mat, uppCI.mat))
}

#Abaixo vemos o processamento para a geração do gráfico
genrcor <- cor(usrgenres[,2:20],use ="complete.obs")
dimnames(genrcor) <- list(colnames(usrgenres[2:20]),colnames(usrgenres[2:20]))
res1 <- cor.mtest(usrgenres[,2:20],0.95)   #retira a coluna do userId
m <- genrcor
p <- cor.mtest(usrgenres[,2:20])


############## Questao 3 Associacao e dendrogram

igGenr2 <- sqldf::sqldf("
                        select userId, avgsc, genres, 
                        case when avgsc < -0.7
                        then ('No' ||'_'|| genres)
                        when -0.7 <= avgsc and avgsc < 0.5
                        then ('Neutro' ||'_'|| genres)
                        else ('Yes' ||'_'|| genres)
                        end 'like'
                        from usrgensc
                        ")
semNeutro <- igGenr2[!(grepl("Neutro_.",igGenr2$like)),]

ttg <- as(split(semNeutro$like, semNeutro$userId), "transactions")

#format(1/nrow(ttg), scientific=FALSE);  ## heuristica para o valor do support


ln <- c(semNeutro[grepl("No_.",semNeutro$like),"like"],semNeutro[grepl("Yes_.",semNeutro$like),"like"])
lun <- unique(ln)
t <-  data.frame()

##### primeiro rodei com maxlen=2, lhs= c(lun[v]), default="rhs" -- resultado igual para maxlen=3
##### depois com maxlen=2 e 3, rhs= c(lun[v]), default="lhs" 
##### tudo agrupado no data frame t
for(v in 1:length(lun)){
  rules <- apriori(ttg, 
                   parameter = list(minlen=1,maxlen=2, supp=0.000001, conf=0.7),
                   appearance = list( lhs= c(lun[v]), 
                                      default="rhs" ),
                   control = list(verbose=F))
  rules1 <- as(rules, "data.frame");
  if(nrow(rules1) > 1){
    orules <- head(rules1 %>% filter(lift>1) %>% arrange(desc(lift,confidence)),n=3)
  } 
  else {
    orules <- head(rules1 %>% filter(lift>1),n=3)
  }
  t <- rbind(t,orules)
}

for(i in 2:3){
  for(v in 1:length(lun)){
    rules <- apriori(ttg, 
                     parameter = list(minlen=1,maxlen=i, supp=0.000001, conf=0.7),
                     appearance = list( rhs= c(lun[v]), 
                                        default="lhs" ),
                     control = list(verbose=F))
    rules1 <- as(rules, "data.frame");
    if(nrow(rules1) > 1){
      orules <- head(rules1 %>% filter(lift>1) %>% arrange(desc(lift,confidence)),n=3)
    } 
    else {
      orules <- head(rules1 %>% filter(lift>1),n=3)
    }
    t <- rbind(t,orules)
  }
}

ruless <- t %>% separate(rules, c("lhs","rhs"), "=>",remove = TRUE, fixed=TRUE)

ruless$lhs <- str_trim(ruless$lhs)
ruless$rhs <- str_trim(ruless$rhs)

rf <- ruless %>% unique() %>% group_by(rhs) %>%
  mutate(rank = rank(lift), 
         support = round(support,3), 
         confidence = round(confidence,3),
         lift = round(lift,3)) %>%
  filter(rank ==1) %>% select(-rank)   %>% arrange(desc(lift))


###### Visualizando as regras

rules_plot <- apriori(ttg, 
                      parameter = list(minlen=1,maxlen=3, supp=0.000001, conf=0.7),
                      control = list(verbose=F))

subset.matrix <- is.subset(rules_plot, rules_plot)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
rulesr <- rules_plot[!redundant]


######################### Dendrogram     #################
library(ggdendro)

## Criando sparse matrix pra plotar o dendrogram
usrgenres <- reshape::cast(semNeutro, like ~ userId ,value= "avgsc")

row.names(usrgenres) <- usrgenres[,"like"]
usrgenres$like <- NULL

usrt2 <- usrgenres

#timestamp()
usrt2[is.na(usrt2)] <- 0
usrt2[usrt2!=0] <-1
#timestamp() # -- 1h40min

hc <- as.dendrogram(hclust(dist(usrt2), method="ave"))




