#kwd_nm split 및 Ngram
##library---------------------------------------------------------------------------------------------
library(tidyverse)

##KWD_NM split (단어 별 나누기)-----------------------------------------------------------------------
dtotal <- read_csv("C:\\Users\\user\\Desktop\\lpoint\\dtotal.csv")
dtotal <- dtotal %>% select(PD_BRA_NM, KWD_NM)
i2 <- read_csv("C:\\Users\\user\\Desktop\\lpoint\\i2.csv", col_names = F) #검색 후 구매한 고객 index 
i2$X1 <- i2$X1 + 1 #python index와 다르기 때문에 index 조정 
dtotal <- dtotal[i2$X1,]

kwd_nm <- data.frame(KWD_NM = dtotal$KWD_NM)
kwd_nm_split <- str_split(kwd_nm$KWD_NM, pattern = " ")

len <- 0
for (i in 1:length(kwd_nm_split)) {
  if (len < length(kwd_nm_split[[i]])) {
    len <- length(kwd_nm_split[[i]])
  }
}

n = length(kwd_nm_split)
j = 3
split <- matrix(0, nrow = n, ncol = 1)
for (i in 1:n) {
  if (is.na(kwd_nm_split[[i]][j])) {
    split[i,1] <- NA
  }
  else {
    split[i,1] <- kwd_nm_split[[i]][j]
  }
}

split <- as.data.frame(split)
colnames(split) <- "X3"
write.csv(split, "kwd_split_3.csv", row.names = F)

##Ngram--------------------------------------------------------------------------------------------------------
dpdnm <- data.frame(PD_NM = dtotal$PD_NM) #상품명 테이블 
dbrandnm <- data.frame(PD_BRA_NM = dtotal$PD_BRA_NM) #브랜드명 테이블 

###ngram ftn -------------------------------------------------------------------------------------------------
ngram <- function(s, num) {
  res <- c()
  slen <- nchar(s) - num + 1
  for (i in c(1:slen)) {
    res[i] <- substr(s, i, (i + num - 1))
  }
  return(res)
}

diff_ngram <- function(sa, sb) {
  
  if (is.na(sa) | is.na(sb)) {
    return(0)
  }
  sa <- as.character(sa)
  sb <- as.character(sb)
  
  if (nchar(sa) < 3 | nchar(sb) < 3) {
    num <- min(nchar(sa), nchar(sb))
  }
  
  else {
    num <- 3
  }
  
  na <- ngram(sa, num)
  nb <- ngram(sb, num)
  
  sa_len <- nchar(sa) - num + 1
  
  both <- c()
  
  for(i in na) {
    for (j in nb) {
      if (i == j) {
        both <- both %>% c(i)
      }
    }
  }
  return(length(both)/sa_len)
}

###Ngram 실행--------------------------------------------------------------------------------------------------
dpdnm <- dpdnm %>% select(PD_NM = "0")

kwd1 <- read.csv("C:\\Users\\user\\Desktop\\lpoint\\kwd_split_1.csv", stringsAsFactors = F)
kwd2 <- read.csv("C:\\Users\\user\\Desktop\\lpoint\\kwd_split_2.csv", stringsAsFactors = F)
kwd3 <- read.csv("C:\\Users\\user\\Desktop\\lpoint\\kwd_split_3.csv", stringsAsFactors = F)
kwd4 <- read.csv("C:\\Users\\user\\Desktop\\lpoint\\kwd_split_4.csv", stringsAsFactors = F)
kwd5 <- read.csv("C:\\Users\\user\\Desktop\\lpoint\\kwd_split_5.csv", stringsAsFactors = F)
kwd6 <- read.csv("C:\\Users\\user\\Desktop\\lpoint\\kwd_split_6.csv", stringsAsFactors = F)

##brand_nm ngram-----------------------------------------------------------------------------------------------
###kwd1--------------------------------------------------------------------------------------------
result_again1 <- c()
for (i in c(1:36000000)) {
  result_again1[i] <- diff_ngram(dbrandnm$PD_BRA_NM[i], split$X1[i])
}

result_again2 <- c()
for (i in c(36000001:nrow(dbrandnm))) {
  result_again2[i-36000000] <- diff_ngram(dbrandnm$PD_BRA_NM[i], split$X1[i])
}

df1 <- data.frame(pd_index = result_again1)
df2 <- data.frame(pd_index = result_again2)

df <- rbind(df1, df2)
unique(df$brand_index)
rm(df1, df2)
write.csv(df, "brand_index_1.csv")

###kwd2--------------------------------------------------------------------------------------------
result_again1 <- c()
for (i in c(1:36000000)) {
  result_again1[i] <- diff_ngram(dbrandnm$PD_BRA_NM[i], split$X2[i])
}

result_again2 <- c()
for (i in c(36000001:nrow(dbrandnm))) {
  result_again2[i-36000000] <- diff_ngram(dbrandnm$PD_BRA_NM[i], split$X2[i])
}

df1 <- data.frame(pd_index = result_again1)
df2 <- data.frame(pd_index = result_again2)

df <- rbind(df1, df2)
unique(df$brand_index)
rm(df1, df2)
write.csv(df, "brand_index_2.csv")

###kwd3--------------------------------------------------------------------------------------------
result_again1 <- c()
for (i in c(1:36000000)) {
  result_again1[i] <- diff_ngram(dbrandnm$PD_BRA_NM[i], split$X3[i])
}

result_again2 <- c()
for (i in c(36000001:nrow(dbrandnm))) {
  result_again2[i-36000000] <- diff_ngram(dbrandnm$PD_BRA_NM[i], split$X3[i])
}

df1 <- data.frame(pd_index = result_again1)
df2 <- data.frame(pd_index = result_again2)

df <- rbind(df1, df2)
unique(df$brand_index)
rm(df1, df2)
write.csv(df, "brand_index_3.csv")


###kwd4--------------------------------------------------------------------------------------------
result_again1 <- c()
for (i in c(1:36000000)) {
  result_again1[i] <- diff_ngram(dbrandnm$PD_BRA_NM[i], split$X4[i])
}

result_again2 <- c()
for (i in c(36000001:nrow(dbrandnm))) {
  result_again2[i-36000000] <- diff_ngram(dbrandnm$PD_BRA_NM[i], split$X4[i])
}

df1 <- data.frame(pd_index = result_again1)
df2 <- data.frame(pd_index = result_again2)

df <- rbind(df1, df2)
unique(df$brand_index)
rm(df1, df2)
write.csv(df, "brand_index_4.csv")

###kwd5--------------------------------------------------------------------------------------------
result_again1 <- c()
for (i in c(1:36000000)) {
  result_again1[i] <- diff_ngram(dbrandnm$PD_BRA_NM[i], split$X5[i])
}

result_again2 <- c()
for (i in c(36000001:nrow(dbrandnm))) {
  result_again2[i-36000000] <- diff_ngram(dbrandnm$PD_BRA_NM[i], split$X5[i])
}

df1 <- data.frame(pd_index = result_again1)
df2 <- data.frame(pd_index = result_again2)

df <- rbind(df1, df2)
unique(df$brand_index)
rm(df1, df2)
write.csv(df, "brand_index_5.csv")

###kwd6--------------------------------------------------------------------------------------------
result_again1 <- c()
for (i in c(1:36000000)) {
  result_again1[i] <- diff_ngram(dbrandnm$PD_BRA_NM[i], split$X6[i])
}

result_again2 <- c()
for (i in c(36000001:nrow(dbrandnm))) {
  result_again2[i-36000000] <- diff_ngram(dbrandnm$PD_BRA_NM[i], split$X6[i])
}

df1 <- data.frame(pd_index = result_again1)
df2 <- data.frame(pd_index = result_again2)

df <- rbind(df1, df2)
unique(df$brand_index)
rm(df1, df2)
write.csv(df, "brand_index_6.csv")

##pd_nm도 같은 방식으로 진행 

#INDEX--------------------------------------------------------------------------------------------------------------------
##브랜드지수------------------------------------------------------------------------------------------------------------------------
dindex_b <- index1 %>% select(i1 = brand_index) %>%
  mutate(i2 = index2$brand_index, i3 = index3$brand_index, i4 = index4$brand_index,
         i5 = index5$brand_index, i6 = index6$brand_index) %>%
  mutate(INDEX_B = i1 + i2 + i3 + i4 + i5 + i6) %>% select(INDEX_B) 

write.csv(dindex_b, "dindex_b.csv", row.names = F)

##상품명 지수 ----------------------------------------------------------------------------------------------------------
pd_index <- index1 %>% select(i1 = pd_index) %>%
  mutate(i2 = index2$pd_index, i3 = index3$pd_index, i4 = index4$pd_index,
         i5 = index5$pd_index, i6 = index6$pd_index) %>%
  mutate(pd_index = i1 + i2 + i3 + i4 + i5 + i6) %>% select(pd_index) 

write.csv(pd_index, "dindex_p.csv", row.names = F)
