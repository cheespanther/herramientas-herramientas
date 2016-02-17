compartivo_inventarios <- function(reporte, fecha)
  
{
  
  library(plyr)
  library(dplyr)
  setwd("D:/KEEP_ISKAR/R/datos/herramientas/reporte_comparativo/datos") 
  
  lista_busqueda <<- read.csv(paste("busqueda_", fecha, ".csv", sep=""))
  lista_referencia <<- read.csv(paste("referencia_", fecha, ".csv", sep=""))
  
  headers_referencia <- c("id", "codigo", "producto", "stock", "stock_virtual", "costo", "precio", "proveedor", 
                          "categoria", "categoria_b")
  colnames(lista_referencia) <- headers_referencia
  
  header_busqueda <<- c("codigo", "producto")
  colnames(lista_busqueda) <- header_busqueda
  
  lista_referencia$stock[is.na(lista_referencia$stock)] <- 0
  lista_referencia$costo[is.na(lista_referencia$costo)] <- 0
  lista_referencia$stock_virtual[is.na(lista_referencia$stock_virtual)] <- 0  
  
  producto_nombre <- lista_referencia$producto

  #coniva <- subset(lista_referencia, lista_referencia$impuestos == "IVA(16%) VENTAS")
  #siniva <- subset(lista_referencia, lista_referencia$impuestos == "IVA(0%) VENTAS")

  #price_coniva <- coniva$precio
  #cost_coniva <- siniva$costo

  #price_siniva <- siniva$precio
  #cost_siniva <- siniva$costo


  #gain_coniva <- (100*((price_coniva/1.16) - cost_coniva)/cost_coniva)
  #gain_siniva <- (100*(price_siniva - cost_siniva)/cost_siniva)

  #coniva <- rbind(coniva, gain_coniva)
  
  productos_stockproducto <<- ddply (lista_referencia, "producto", summarise, stock_proveedor_cantidad = sum(stock),
                                     stock_proveedor_valor = sum(stock * costo))
  
  # ref_proveedor <<- unique(ddply (lista_referencia, "producto", summarise, proveedor = print(proveedor)))
  
  productos_stockcategoria <<- ddply (lista_referencia, "categoria", summarise, stock_categoria_cantidad = sum(stock),
                                      stock_categoria_valor = sum(stock * costo))

  # tabla_producto_proveedor <<- cbind(productos_stockproducto, ref_proveedor$proveedor)
  
  busqueda_match <<- na.omit(lista_busqueda[ match(lista_busqueda$codigo, lista_referencia$codigo),])
  referencia_match <<- lista_referencia[ match(busqueda_match$codigo, lista_referencia$codigo),]
  
  setwd("D:/KEEP_ISKAR/R/datos/herramientas/reporte_comparativo/reportes") 
  
  write.csv(productos_stockproducto, file = paste("valor_stock_producto", reporte, ".csv", sep=""))
  write.csv(productos_stockcategoria, file = paste("valor_stock_categoria", reporte, ".csv", sep=""))
            
}