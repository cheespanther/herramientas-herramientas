lineasventas <- function (fecha, tienda)
    
{
    library(plyr)
    library(caret)
    library(zoo)
    library(lubridate)
  
    # LECTURA DE DATOS
    setwd("E:/KEEP_ISKAR/R/datos/herramientas/lineasdeventas/datos")
    lineas <- read.csv(paste("lineas_",tienda,"_",fecha,".csv", sep=""))
    # ASIGNAR TITULOS A COLUMNAS
    titulos <- c("id", "cuenta", "fecha_hora", "producto", "precio", "cantidad", 
                 "subtotal", "subtotalB", "total", "impuestos", "impuestosID" )
    colnames(lineas) <- titulos
    
    # ELIMINAR DATOS NULOS Y ELIMINAR DATOS INECESARIOS DEL ID
    lineas[is.na(lineas)] <- 0
    id <- gsub("__export__.pos_order_", "", lineas$id)
    
    # SUSTITUIR ID CON NUEVO ID Y LLENAR CELDAS VACÍAS CON DATO ANTERIOR
    lineas <- cbind(id, lineas[-1])
    lineas$id <- sub("^$", NA, lineas$id)  
    lineas$id <- na.locf(lineas$id)
    
    lineas$fecha_hora <- sub("^$", NA, lineas$fecha_hora)  
    lineas$fecha_hora <- na.locf(lineas$fecha_hora)
    
    ## CICLO NO PROBADO PARA REEMPLAZAR NA
    #lineas$id <- function(lineas$id) { 
     # y <- c(NA, head(z, -1))
     # z <- ifelse(is.na(z), y, z)
     # if (any(is.na(z))) Recall(z) else z }
    
    # SEPARAR TIMESTAMP EN FECHA Y HORA
    date <- as.POSIXct(lineas$fecha_hora, format = "%m/%d/%Y", tz = "")

    hour <- as.POSIXct(lineas$fecha_hora, format = "%m/%d/%Y %H:%M")    
    hora <- hour(hour)
    
    lineas <- cbind(lineas, hora)
    
    # VENTAS POR SESIÓN
    
    ventas_sesion <<- ddply (lineas, "id", summarise, subtotales = sum(subtotal))
    ventas_hora <<- ddply (lineas, "hora", summarise, subtotales = sum(subtotal))
    
}

