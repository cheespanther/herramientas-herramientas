valoracion_inventario <<- function(tienda, fecha)
  
{
  
  library(plyr)
  library(dplyr)
  
  setwd("E:/KEEP_ISKAR/R/datos/herramientas/walmart_supermas/datos")    
  producto_supermas <<- read.csv("supermas.csv")
  
  setwd("E:/KEEP_ISKAR/R/datos/herramientas/valoracioninventarios/datos")
  valoracion <<- read.csv(paste("ValoracionInventario", tienda, "_", fecha,".csv", sep = ""))
  id_prod <<- gsub("__export__.product_product_", "", producto_supermas$id)
  producto_supermas <<- cbind(id_prod, producto_supermas[-1])
  
  supermas_productmatch <<- producto_supermas[match(valoracion$product_id, producto_supermas$id),]
  valoracion_productmatch <<- valoracion[match(producto_supermas$name, valoracion$name),]
  reporte <<- cbind(producto_supermas$ean13, producto_supermas$name, id_prod, producto_supermas$standard_price,
                    producto_supermas$list_price, valoracion_productmatch)
  
  colnames(reporte) <<- c("EAN13", "PRODUCTO", "ID", "COSTO", "PRECIO", "IDVALORACION", "PRODUCTO2", "INVENTARIO", "COSTO2", "VALORACION")
  
  inventario <<- reporte[c(1,2,3,8)] 
  inventario[is.na(inventario)] <- 0
  inventario_sort <- inventario[order(inventario$EAN13),]
  
  setwd("E:/KEEP_ISKAR/R/datos/herramientas/valoracioninventarios/reportes")
  
  write.csv(reporte, file = paste("reporte", tienda, fecha, ".csv", sep=""))
  write.csv(inventario_sort, file = paste("inventario", tienda, fecha, ".csv", sep=""))
  
}