setwd("D:/KEEP_ISKAR/R/productos")

  library(plyr)

productos <- read.csv("productostotales_021115.csv")

headers <- c("id","codigo", "producto", "costo", "precio", "proveedor", "proveedor2","unidad", "impuestos", "gain sin iva", "gain con iva")
colnames(productos) <- headers

producto_nombre <- productos$name

coniva <- subset(productos, productos$impuestos == "IVA(16%) VENTAS")
siniva <- subset(productos, productos$impuestos == "IVA(0%) VENTAS")

price_coniva <- coniva$precio
cost_coniva <- siniva$costo

price_siniva <- siniva$precio
cost_siniva <- siniva$costo

gain_coniva <- 100*((price_coniva - (cost_coniva*1.16)/cost_coniva)
gain_siniva <- 100*((price_siniva - cost_siniva))/cost_siniva)

coniva <- rbind(coniva, gain_coniva)
siniva <- rbind(siniva, gain_siniva)
  
productos_catpapa <<- ddply (productos, "categoria papa", summarise, suma_proveedor = length(categoriapapa))
costo_proveedor <<- ddply ()

productos_totales <- rbind(coniva, siniva)

