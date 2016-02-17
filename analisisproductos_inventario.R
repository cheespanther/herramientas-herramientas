setwd("C:/R/productos")
  
library(plyr)

lista_referencia <- read.csv("productostotales_inventario.csv")

headers <- c("id", "codigo", "producto", "costo", "precio", "proveedor", "codigo_default", "usuario", "impuestos", "stock", "stock_virtual")
colnames(lista_referencia) <- headers
  
lista_referencia$stock[is.na(lista_referencia$stock)] <- 0
lista_referencia$costo[is.na(lista_referencia$costo)] <- 0
lista_referencia$stock_virtual[is.na(lista_referencia$stock_virtual)] <- 0  
  
producto_nombre <- lista_referencia$name

coniva <- subset(lista_referencia, lista_referencia$impuestos == "IVA(16%) VENTAS")
siniva <- subset(lista_referencia, lista_referencia$impuestos == "IVA(0%) VENTAS")

price_coniva <- coniva$precio
cost_coniva <- siniva$costo

price_siniva <- siniva$precio
cost_siniva <- siniva$costo


gain_coniva <- (100*((price_coniva/1.16) - cost_coniva)/cost_coniva)
gain_siniva <- (100*(price_siniva - cost_siniva)/cost_siniva)

coniva <- rbind(coniva, gain_coniva)
  
productos_stockproveedor <<- ddply (lista_referencia, "proveedor", summarise, stock_proveedor = length(stock))
costo <<- lista_referencia$stock * lista_referencia$costo
lista_referencia$stock[is.na(stock)] <- 0

