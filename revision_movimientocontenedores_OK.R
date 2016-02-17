revision_mov_contenedores <- function (inventario_nombre, tiendas, fechas, inventario)
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
    movimientosSM3 <<- subset(inventario_r, 
                   inventario_r["location_dest_id.name"] == "SM3 Contenedor Lunes"|
                   inventario_r["location_dest_id.name"] == "SM3 Contenedor Martes"|
                   inventario_r["location_dest_id.name"] == "SM3 Contenedor Miércoles"|
                   inventario_r["location_dest_id.name"] == "SM3 Contenedor Jueves"|
                   inventario_r["location_dest_id.name"] == "SM3 Contenedor Viernes"|
                   inventario_r["location_dest_id.name"] == "SM3 Contenedor Sábado")
  
    movimientosSM3_producto <<- ddply (movimientosSM3, "product_id.ean13", summarise,
                                          subtotal = sum(product_id.standard_price*product_qty),
                                          cantidad = sum(product_qty))
#   
    movimientosSM5 <<- subset(inventario_r, 
                   inventario_r["location_dest_id.name"] == "SM5 Contenedor Lunes"|
                     inventario_r["location_dest_id.name"] == "SM5 Contenedor Martes"|
                     inventario_r["location_dest_id.name"] == "SM5 Contenedor Miércoles"|
                     inventario_r["location_dest_id.name"] == "SM5 Contenedor Jueves"|
                     inventario_r["location_dest_id.name"] == "SM5 Contenedor Viernes"|
                     inventario_r["location_dest_id.name"] == "SM5 Contenedor Sábado")
    
    movimientosSM5_producto <<- ddply (movimientosSM5, "product_id.ean13", summarise,
                            subtotal = sum(product_id.standard_price*product_qty),
                            cantidad = sum(product_qty))
#
    
    movimientosSM1 <<- subset(inventario_r, 
                   inventario_r["location_dest_id.name"] == "SM1 Contenedor Lunes"|
                     inventario_r["location_dest_id.name"] == "SM1 Contenedor Martes"|
                     inventario_r["location_dest_id.name"] == "SM1 Contenedor Miércoles"|
                     inventario_r["location_dest_id.name"] == "SM1 Contenedor Jueves"|
                     inventario_r["location_dest_id.name"] == "SM1 Contenedor Viernes"|
                     inventario_r["location_dest_id.name"] == "SM1 Contenedor Sábado")
    
    movimientosSM1_producto <<- ddply (movimientosSM1, "product_id.ean13", summarise,
                            subtotal = sum(product_id.standard_price*product_qty),
                            cantidad = sum(product_qty))
    
    setwd("E:/KEEP_ISKAR/R/datos/herramientas/movimientos/reportes")
    
}