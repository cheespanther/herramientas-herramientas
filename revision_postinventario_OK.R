revision_postinventario <- function (fechas, inventario_nombre, tienda, inventario)
  ## fechas = fecha del inventario
  ## inventario = nombre del inventario en Open ERP que va a filtrar los datos
  ## Nombre de archivo movimientos_DDMMAA.csv
  
{
    library(plyr)
    xy <<- paste(inventario_nombre, inventario)
    x <<- inventario_nombre
    csv_nombre <<- paste(tienda,inventario_nombre, sep = "")
    setwd("E:/KEEP_ISKAR/R/datos/herramientas/movimientos/datos")
    datos <<- read.csv(paste("movimientos_", inventario_nombre, "_", tienda, "_", fechas,  ".csv", sep=""))
    inventario_r <<- subset(datos, datos$name == inventario)
    nombres <<- unique(datos$name)
    duplicados <<- duplicated(datos["product_id.name"])
    recuperacion <<- subset(inventario_r, inventario_r["location_dest_id.name"] != "Inventory loss")
    perdidas <<- subset(inventario_r, inventario_r["location_dest_id.name"] == "Inventory loss")
    total <<- rbind(recuperacion, perdidas)
    
    inventarioperdido_producto <<- ddply (perdidas, "product_id.name", summarise,
                                          subtotal = sum(product_id.standard_price*product_qty),
                                          cantidad = sum(product_qty)*-1)
    
    inventariorecuperado_producto <<- ddply (recuperacion, "product_id.name", summarise,
                                             subtotal = sum(product_id.standard_price*product_qty),
                                             cantidad = sum(product_qty))
    
    reporte <<- rbind(inventarioperdido_producto, inventariorecuperado_producto)
    
    setwd("E:/KEEP_ISKAR/R/datos/herramientas/movimientos/reportes")
    write.csv(inventarioperdido_producto, file = paste("perdidas_", tienda, fechas, inventario_nombre, ".csv", sep=""))
    write.csv(inventariorecuperado_producto, file = paste("recuperacion_", tienda, fechas, inventario_nombre, ".csv", sep="")) 
    write.csv(total, file = paste("movimientostotales", tienda, fechas, inventario_nombre, ".csv", sep=""))
    write.csv(reporte, file = paste("reportepostinventario_", tienda, fechas, inventario_nombre, ".csv", sep="" ))
    
}