
setwd("~/Desktop/testededados/Litteris/arquivos4 Association")

library(readr)
library(dplyr)
library(tidyr)
library(sqldf)


library(arules)
library(stringr)
library(arulesViz)



ratings <- read_csv("./data/ml-20m/ratings.csv")
movies <- read_csv("./data/ml-20m/movies.csv")
moviess <- movies %>% mutate(genres = strsplit(genres, "\u007C",fixed=TRUE)) %>% unnest(genres)
ratingsBefore <- ratings
ratings$timestamp <- as.POSIXct(as.numeric(ratings$timestamp),origin="1970-01-01",tz="GMT")
ratings$timestamp <- format(ratings$timestamp, "%Y")

total <- ratings %>% inner_join(moviess,by="movieId")
tout <- total %>% filter(timestamp != 1995) 
total2 <- tout[1:10000,c(1,3,6)]
total2 <- tout
#total2 <- sample_n(tout[,c(1,3,6)],size = 10000)

usrRatMax <- total2 %>% filter(genres != "(no genres listed)") %>% 
  group_by(userId,genres,rating) %>% summarise(count = n())

usrRatxCt <- usrRatMax %>% mutate(cxr = rating * count) %>% 
  group_by(userId,genres) %>% summarise(avg = mean(cxr))

#Agora, vamos padronizar a coluna de rating, por usuário (alguns usuários que veem mais filmes que outros).
usrgensc <- plyr::ddply(usrRatxCt, "userId", function(x){ x$scavg = scale(x$avg)})
usrgensc <- mutate(usrgensc, genres = usrRatxCt$genres) #adiciona genres ao data.frame de novo
colnames(usrgensc) <- c("userId", "avgsc","genres")

#head(usrgensc)

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
  filter(rank ==1) %>% arrange(desc(lift))

#head(rf,n=20)


###### Visualizando as regras

rules_plot <- apriori(ttg, 
                 parameter = list(minlen=1,maxlen=3, supp=0.000001, conf=0.7),
                 control = list(verbose=F))

subset.matrix <- is.subset(rules_plot, rules_plot)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
rules.pruned <- rules_plot[!redundant]
rulesr<-rules.pruned


plot(rulesr[quality(rulesr)$lift > 1.0], 
     method="grouped",
     measure="confidence", 
     shading="lift",
     interactive=TRUE)


################# EDA

#head(ruless %>% filter(lift>1) %>% arrange(desc(lift)),n=10)
#head(rules1rs %>% filter(lift>1,rhs == grepl("Yes_Action.",rules1rs$rhs)) 
#     %>% select(-rules) %>% arrange(-lift,lhs),n=1000)


#inspect( sort(subset( rules, subset = (rhs %pin% "No_Drama" )), by="lift",decreasing=TRUE))
#inspect( sort(subset( rules, lhs == "Yes_Drama" ), by="lift",decreasing=TRUE)[1:10])
#inspect(subset(rules[quality(rules)$lift > 1.0], lhs %pin% ".No_Drama,No_War."  ))

#inspect( sort(rules_plot[quality(rules_plot)$lift > 1.0], by="lift",decreasing=TRUE)[1:20])

#ruless$rhs<-gsub("[{}]","",ruless$rhs)

#sort(unique(union(ruless$rhs, ruless$lhs)))
#intersect(lun,unique(union(ruless$rhs, ruless$lhs)))
#setdiff(lun,unique(union(ruless$rhs, ruless$lhs)))


######################### Dendrogram     #################
library(ggdendro)

## Criando sparse matrix pra plotar o dendrogram
usrgenres <- reshape::cast(semNeutro, like ~ userId ,value= "avgsc")

#usrt[,990:1001]
#nc <- ncol(usrgenres)

row.names(usrgenres) <- usrgenres[,"like"]
usrgenres$like <- NULL

usrt2 <- usrgenres

timestamp()
usrt2[is.na(usrt2)] <- 0
usrt2[usrt2!=0] <-1
timestamp() # -- 1h40min

#ur <- usrgenres[,30001:40000]

#plot do dendrogram
hc <- as.dendrogram(hclust(dist(usrt2), method="ave"))
ggdendrogram(hc, rotate = FALSE, size = 2)

#install.packages("dendextend")
#install.packages('dendextendRcpp')
#library(dendextend)
#library(dendextendRcpp)

