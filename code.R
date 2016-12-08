setwd("~/Documents/MIT/Dropbox/6.867 Final Project")

library(dplyr)
library(ggplot2)
library(caTools)
library(zoo)
library(tidyr)

# data <- read.csv("train_ver2.csv", as.is = T)
# clientes <- unique(data$ncodpers)
# 
# cte_full <- data %>%
#     group_by(ncodpers) %>%
#     summarise(tot = n()) %>%
#     filter(tot == 17) 
# 
# ctes <- cte_full$ncodpers
# 
# data_full <- data %>%
#     filter(ncodpers %in% ctes) %>%
#     mutate(antiguedad = as.numeric(antiguedad)) %>%
#     select(c(fecha_dato, ncodpers, sexo, age,fecha_alta,
#              antiguedad, indresi, indext, cod_prov,
#              nomprov, ind_actividad_cliente, renta, segmento,
#              ind_cco_fin_ult1, ind_cno_fin_ult1, ind_ctop_fin_ult1,
#               ind_ctpp_fin_ult1, ind_dela_fin_ult1, ind_ecue_fin_ult1,
#               ind_reca_fin_ult1, ind_tjcr_fin_ult1, ind_nomina_ult1,
#               ind_nom_pens_ult1, ind_recibo_ult1))
# 
# write.csv(data_full,'data.csv')

#

data <- read.csv("data.csv")

data <- 

cco <- data %>% select(ncodpers,fecha_dato,ind_cco_fin_ult1) %>%
    group_by(ncodpers) %>% spread(fecha_dato, ind_cco_fin_ult1)
cno <- data %>% select(ncodpers,fecha_dato,ind_cno_fin_ult1) %>%
    group_by(ncodpers) %>% spread(fecha_dato, ind_cno_fin_ult1)
ctop <- data %>% select(ncodpers,fecha_dato,ind_ctop_fin_ult1) %>%
    group_by(ncodpers) %>% spread(fecha_dato, ind_ctop_fin_ult1)
dela <- data %>% select(ncodpers,fecha_dato,ind_dela_fin_ult1) %>%
    group_by(ncodpers) %>% spread(fecha_dato, ind_dela_fin_ult1)
ecue <- data %>% select(ncodpers,fecha_dato,ind_ecue_fin_ult1) %>%
    group_by(ncodpers) %>% spread(fecha_dato, ind_ecue_fin_ult1)
reca <- data %>% select(ncodpers,fecha_dato,ind_reca_fin_ult1) %>%
    group_by(ncodpers) %>% spread(fecha_dato, ind_reca_fin_ult1)
tjcr <- data %>% select(ncodpers,fecha_dato,ind_tjcr_fin_ult1) %>%
    group_by(ncodpers) %>% spread(fecha_dato, ind_tjcr_fin_ult1)
nomina <- data %>% select(ncodpers,fecha_dato,ind_nomina_ult1) %>%
    group_by(ncodpers) %>% spread(fecha_dato, ind_nomina_ult1)
pens <- data %>% select(ncodpers,fecha_dato,ind_nom_pens_ult1) %>%
    group_by(ncodpers) %>% spread(fecha_dato, ind_nom_pens_ult1)
recibo <- data %>% select(ncodpers,fecha_dato,ind_recibo_ult1) %>%
    group_by(ncodpers) %>% spread(fecha_dato, ind_recibo_ult1)

full <- cco %>% inner_join(cno, by = c("ncodpers"="ncodpers"), suffix = c(".cco",".cno"))
full <- full %>% inner_join(ctop, by = c("ncodpers"="ncodpers"))
full <- full %>% inner_join(dela, by = c("ncodpers"="ncodpers"),suffix = c(".ctop",".dela"))
full <- full %>% inner_join(ecue, by = c("ncodpers"="ncodpers"))
full <- full %>% inner_join(reca, by = c("ncodpers"="ncodpers"),suffix = c(".ecue",".reca"))
full <- full %>% inner_join(tjcr, by = c("ncodpers"="ncodpers"))
full <- full %>% inner_join(nomina, by = c("ncodpers"="ncodpers"),suffix = c(".tjcr",".nomina"))
full <- full %>% inner_join(pens, by = c("ncodpers"="ncodpers"))
full <- full %>% inner_join(recibo, by = c("ncodpers"="ncodpers"),suffix = c(".pens",".recibo"))

names(full) <- gsub("-28",'',names(full))

write.csv(full,"horizontal.csv")

seed <- 1234

split1 = sample.split(clientes, SplitRatio = 0.1)
split2 = sample.split(clientes, SplitRatio = 0.01)

clientes_1 <- clientes[split1]
clientes_2 <- clientes[split2]
clientes_3 <- clientes[1:100]


subset1 <- data %>%
    filter(ncodpers %in% clientes_1)

subset2 <- data %>%
    filter(ncodpers %in% clientes_2)

subset3 <- data %>%
    filter(ncodpers %in% clientes_3)

write.csv(subset1, file = 'subset1.csv')
write.csv(subset2, file = 'subset2.csv')
write.csv(subset3, file = 'subset3.csv')

subset3$fec <- as.yearmon(subset3$fecha_dato)
subset3$fec2 <- subset3$fec+(1/12)

products <- names(subset3[25:48])

for j=1:length(products){
    for i=2:dim(subset3)[1]{
        if subset3[]
    }
}

#Análisis de productos


prod_sum1 <- data[,c(2,25:48)] %>%
    group_by(ncodpers) %>%
    summarise_each(funs(sum))

prod_sum2 <- data[,c(2,25:48)] %>%
    group_by(ncodpers) %>%
    summarise_each(funs(mean)) 
    
##

subset4 <- subset3 %>%
    select(ncodpers, fecha_dato, ind_cco_fin_ult1, ind_recibo_ult1) %>%
    arrange(ncodpers, fecha_dato)

prueba <- subset4 %>% mutate(var = paste(fecha_dato,'ind_cco')) %>%
    select(-fecha_dato,-ind_recibo_ult1) %>%
    group_by(ncodpers) %>% spread(var, ind_cco_fin_ult1)

matriz <- function(var){

    return(m)
}

matriz(ind_cco_fin_ult1)

prueba <- prueba %>% group_by(ncodpers) %>% spread(fecha_dato, ind_recibo_ult1)
