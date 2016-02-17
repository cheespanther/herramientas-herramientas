pricefinder <<- function(tienda, mes, year)
  
{
  
  library(plyr)
  library(dplyr)
   
  setwd("D:/KEEP_ISKAR/R/datos/pricefinder/datos")
  producto_supermas <<- read.csv("supermas.csv")
  x <<- read.csv(paste("ventas_categoria_", tienda, mes, year,".csv", sep = ""))
  
  #duplicados <<- duplicated(producto_walmart)
  #producto_walmart <<- producto_walmart[!duplicated(producto_walmart), ]
  
  supermas_UPC_match <<- producto_supermas[match(x$EAN13.PRODUCTO, producto_supermas$ean13),]
  x_UPC_match <<- na.omit(x[match(producto_supermas$ean13, x$EAN13.PRODUCTO),])
  
  supermas_match <<- arrange(supermas_UPC_match, desc(ean13), ean13)
  x_match <<- arrange(x_UPC_match, desc(EAN13.PRODUCTO), EAN13.PRODUCTO)
  x_order <<- arrange(x, desc(EAN13.PRODUCTO), EAN13.PRODUCTO)
  
  x_add <<- cbind(x_order, supermas_match$standard_price)
  
  setwd("D:/KEEP_ISKAR/R/datos/pricefinder/datos")
  
  write.csv(x_add, file = "xadd.csv")
  
  comparativa
  no_en_supermas
  porcentaje
  
}