library(tidyverse)
library(PerformanceAnalytics)
library(quantmod)
library(Quandl)
library(xts)
library(zoo)
library(corrplot)
library(readxl)
library(writexl)
library(data.table)

xl_data <- "~/2021.1/Artigo Financas Internacionais/db.xlsx"

excel_sheets(path = xl_data)

tab_names <- excel_sheets(path = xl_data)

list_all <- lapply(tab_names, function(x) read_excel(path = xl_data, 
                                                     sheet = x,
                                                     col_names = c("Data","Valor"),
                                                     col_types = c("date","numeric")))

str(list_all)

ldb <- list()
i <- 0
for (x in list_all) {
  i <- i+1
  id <- x
  id$Data <- as.Date(id$Data, format("%y-%m-%d"), tz="UTC")
  id <- id[order(id$Data),]
  ldb[[i]] <- as.xts(id[,2], order.by = id$Data)
}

str(ldb)
lapply(ldb, dim)
names(ldb) <- c("IBOV","ISE","S&P","DJ","A1SGI","SBBMGLU","W1SGI")

Quandl.api_key("")
treasury <- Quandl("FED/SVENPY",type = 'xts',
                   start_date = "2008-06-30",
                   end_date = "2021-05-28")

#treasury <- as.data.frame(treasury, keep.rownames = TRUE)
#setDT(treasury, keep.rownames = TRUE)[]
#write_xlsx(treasury,"~/2021.1/Artigo Financas Internacionais/t10.xlsx")

treasury <- treasury[,10]
head(treasury)
tail(treasury)
dim(treasury)
rf_e <- (1+ (treasury/100))^(1/252)-1
names(rf_e) <- c("Treasury10")
head(rf_e) #Retorno diário da Treasury de 10 anos (taxa livre de risco)
tail(rf_e)

selic <- Quandl("BCB/1178",type = 'xts',
                start_date = "2008-06-30",
                end_date = "2021-05-28")
head(selic)
tail(selic)
rf_b <- (1+ (selic/100))^(1/252)-1
names(rf_b) <- c("Selic")
head(rf_b) #Retorno diário da Selic (taxa livre de risco Brasil)
tail(rf_b)

db_b <- cbind.xts(ldb[[1]],ldb[[2]])
db_e <- cbind.xts(ldb[[3]],ldb[[4]],ldb[[5]])
db_m <- cbind.xts(ldb[[6]],ldb[[7]])
head(db_b)
tail(db_e)
dim(db_m)

returns_b <- Return.calculate(db_b)
names(returns_b) <- c("IBOV","ISE")
colSums(is.na(returns_b))
returns_b <- cbind.xts(returns_b,rf_b)
returns_b <- returns_b[-1,]
colSums(is.na(returns_b))
head(returns_b)
plot.zoo(returns_b, main = "Retorno Índices Brasil")
table.AnnualizedReturns(returns_b[, 1:2], Rf = returns_b[,3])
charts.PerformanceSummary(returns_b[, 1:2], main = "Retornos Índices Brasil")
returns_b2 <- na.approx(returns_b)
colSums(is.na(returns_b2))
returns_b_m <- to.monthly.contributions(returns_b2)
head(returns_b_m)
plot.zoo(returns_b_m)
charts.RollingPerformance(returns_b_m[,1:2],
                          Rf = returns_b_m[,3],
                          main="Retornos Mensais Brasil",
                          legend.loc="topleft")

returns_e <- Return.calculate(db_e)
names(returns_e) <- c("S&P","DJ","A1SGI")
colSums(is.na(returns_e))
returns_e <- cbind.xts(returns_e,rf_e)
returns_e <- returns_e[-1,]
colSums(is.na(returns_e))
head(returns_e)
plot.zoo(returns_e, main = "Retorno Índices Estados Unidos")
returns_e$Treasury10 <- na.approx(returns_e$Treasury10)
table.AnnualizedReturns(returns_e[, 1:3], Rf = returns_e[,4])
charts.PerformanceSummary(returns_e[, 1:3], main = "Retornos Índices Estados Unidos")
returns_e2 <- na.approx(returns_e)
colSums(is.na(returns_e2))
returns_e_m <- to.monthly.contributions(returns_e2)
head(returns_e_m)
plot.zoo(returns_e_m)
charts.RollingPerformance(returns_e_m[,1:3],
                          Rf = returns_e_m[,4],
                          main="Retornos Mensais Estados Unidos",
                          legend.loc="topleft")

returns_m <- Return.calculate(db_m)
names(returns_m) <- c("SBBMGLU","W1SGI")
colSums(is.na(returns_m))
returns_m <- cbind.xts(returns_m,rf_e)
returns_m <- returns_m[-1,]
colSums(is.na(returns_m))
head(returns_m)
plot.zoo(returns_m, main = "Retorno Índices Mundo")
returns_m$Treasury10 <- na.approx(returns_m$Treasury10)
table.AnnualizedReturns(returns_m[, 1:2], Rf = returns_m[,3])
charts.PerformanceSummary(returns_m[, 1:2], main = "Retornos Índices Mundo")
returns_m2 <- na.approx(returns_m)
colSums(is.na(returns_m2))
returns_m_m <- to.monthly.contributions(returns_m2)
head(returns_m_m)
plot.zoo(returns_m_m)
charts.RollingPerformance(returns_m_m[,1:2],
                          Rf = returns_m_m[,3],
                          main="Retornos Mensais Mundo",
                          legend.loc="topleft")

db <- cbind.xts(ldb[[1]],ldb[[2]],ldb[[3]],ldb[[4]],ldb[[5]],ldb[[6]],ldb[[7]],rf_b,rf_e)
names(db) <- c("IBOV","ISE","S&P","DJ","A1SGI","SBBMGLU","W1SGI","Selic","Treasury10")
head(db)

plot.zoo(db, main = "Índices e Taxas Livres de Risco no período")

colSums(is.na(db))
db <- na.approx(db)
db[,1:7] <- Return.calculate(db[,1:7])
db <- db[-1,]
colSums(is.na(db))
head(db)

res1 <- cor.mtest(db, conf.level = .95)

M <- cor(db)

corrplot(M,
         order = "hclust",
         method = "number",
         number.cex = 1,
         addrect = 2)
  