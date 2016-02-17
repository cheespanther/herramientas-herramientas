files <- list.files (pattern=".csv")
data <- lapply(files, read.csv, header = TRUE)
combine <- do.call(rbind, data)



precios <- read.csv("precios.csv")
preciosclean <- na.omit(precios)
diferencia <- (preciosclean["PRECIO.SUPERMAS"]-preciosclean["PRECIO.WALMART"])
lista <- cbind(preciosclean, diferencia)

compl <- function {
  cases <- complete.cases(dif)
  

}

