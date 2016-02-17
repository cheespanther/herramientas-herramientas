revision_mov_desperdicio <- function (inventario_nombre, tiendas, fechas, inventario = "In Store Movement")
  ## fechas = fecha del inventario
  ## inventario = nombre del inventario en Open ERP que va a filtrar los datos
  ## Nombre de archivo movimientos_DDMMAA.csv
  
{
#
    library(plyr)
    xy <<- paste(inventario_nombre, inventario)
    x <<- inventario_nombre
    csv_nombre <<- paste(inventario_nombre,"_", tiendas, "_", fechas, sep = "")
    setwd("E:/KEEP_ISKAR/R/datos/herramientas/movimientos/datos")
    datos <<- read.csv(paste("movimientos_", csv_nombre,  ".csv", sep=""))
    inventario_r <<- subset(datos, datos$name == inventario)
    unicos_inventario <<- unique(inventario_r["product_id.name"])
    nombres <<- unique(datos$name)
    locations <<- unique(datos$location_dest_id.name)
    duplicados <<- duplicated(datos["product_id.name"])
    
#
    desperdicioSM1 <<- subset(inventario_r, 
                   inventario_r["location_dest_id.name"] == "SM1 Desperdicio"|
                   inventario_r["location_dest_id.name"] == "SM1 Caducado"|
                   inventario_r["location_dest_id.name"] == "SM1 Merma")
  
    desperdicioSM1_producto <<- ddply (desperdicioSM1, "product_id.ean13", summarise,
                                          subtotal = sum(product_id.standard_price*product_qty),
                                          cantidad = sum(product_qty))
#   
    desperdicioSM3 <<- subset(inventario_r, 
                              inventario_r["location_dest_id.name"] == "SM3 Desperdicio"|
                              inventario_r["location_dest_id.name"] == "SM3 Caducado"|
                              inventario_r["location_dest_id.name"] == "SM3 Merma")
    
    desperdicioSM3_producto <<- ddply (desperdicioSM3, "product_id.ean13", summarise,
                                       subtotal = sum(product_id.standard_price*product_qty),
                                       cantidad = sum(product_qty))
#
    
    desperdicioSM5 <<- subset(inventario_r, 
                              inventario_r["location_dest_id.name"] == "SM5 Desperdicio"|
                              inventario_r["location_dest_id.name"] == "SM5 Caducado"|
                              inventario_r["location_dest_id.name"] == "SM5 Merma")
    
    desperdicioSM5_producto <<- ddply (desperdicioSM5, "product_id.ean13", summarise,
                                       subtotal = sum(product_id.standard_price*product_qty),
                                       cantidad = sum(product_qty))
    
    
    
    setwd("E:/KEEP_ISKAR/R/datos/herramientas/movimientos/reportes")
    
    write.csv(desperdicioSM1_producto, file = paste("SM1_", inventario_nombre, tienda, fechas, ".csv"))
    write.csv(desperdicioSM3_producto, file = paste("SM3_", inventario_nombre, tienda, fechas, ".csv"))
    write.csv(desperdicioSM5_producto, file = paste("SM5_", inventario_nombre, tienda, fechas, ".csv"))
    
    
    
}