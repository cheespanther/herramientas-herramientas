comparativa_walmart_supermas <<- function(producto, fecha)
    
{
    
    library(plyr)
    library(dplyr)
    setwd("E:/KEEP_ISKAR/R/datos/herramientas/walmart_supermas/datos")    
    
    producto_supermas <<- read.csv("supermas.csv")
    producto_walmart <<- read.csv("walmart.csv")
    
    x <<- producto_supermas[1]
    
    z <<- as.numeric(substr(x[1:nrow(producto_supermas),1], 1, nchar(x[1:nrow(producto_supermas),1])-1))
    
    producto_supermas <<- cbind(z, producto_supermas)
    
    colnames(producto_supermas) <<- c("UPC", "UPCajustado", "Producto", "Precio")
    colnames(producto_walmart) <<- c("UPC", "Producto", "Precio")
    
    duplicados <<- duplicated(producto_walmart)
    producto_walmart <<- producto_walmart[!duplicated(producto_walmart), ]
    
    supermas_UPC_match <<- na.omit(producto_supermas[match(producto_walmart$UPC, producto_supermas$UPC),])
    walmart_UPC_match <<- na.omit(producto_walmart[match(producto_supermas$UPC, producto_walmart$UPC),])
    
    supermas_match <<- arrange(supermas_UPC_match, desc(UPC), UPC)
    walmart_match <<- arrange(walmart_UPC_match, desc(UPC), UPC)
    
    matching_upc <<- supermas_match$UPC
    
    comparativa <<- cbind(supermas_match, walmart_match$Precio)
    colnames(comparativa) <- c("UPCajustado", "UPC", "Producto", "PrecioSM", "PrecioWM")
    WM_SM <<- as.numeric(comparativa$PrecioWM - comparativa$PrecioSM)
    comparativa2 <<- cbind(comparativa, WM_SM)
    ## colnames(comparativa) <- c("UPC", "Producto", "PrecioWM", "PrecioSM", "WM-SM")
    
    
    no_en_supermas <<- producto_walmart[!producto_walmart$UPC%in%producto_supermas$UPC,]
    no_en_supermas2 <<- anti_join(producto_walmart, producto_supermas, by = "UPC")
    
    porcentaje <<- nrow(supermas_UPC_match)/nrow(producto_walmart)
    
    setwd("E:/KEEP_ISKAR/R/datos/herramientas/walmart_supermas/reportes")
    
    write.csv(comparativa2, file = paste(producto, fecha, "walmart_vs_supermas.csv"))
    write.csv(no_en_supermas, file = paste(producto, fecha, "no_en_supermas.csv"))
    
    comparativa
    no_en_supermas
    porcentaje
    
}
    
