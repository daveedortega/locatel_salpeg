# Función para iterar sobre una lista de data frames x e intersectarlos a una geometría z, para pegar el resultado en df y
# dimensión de x puede ser la que sea, dimensión de salida será n = filas de z por k = length  de la lista x
iterated_intersection <- function(x,y,z){
  for (i in 1:length(x)){
    df <- x[[i]] # ith data frame
    fetch_date <- df[1,1] #get date, always in first object in df
    fetch_what <- paste(df[1,2],df[1,3]) # get what it is
    isects <- lengths(st_intersects(z,df$geometry)) # vector of intersections
    y[,ncol(y)+1] <- isects # después lo puedo colapsar como columna de atributos con pivot wider
    colnames(y)[ncol(y)] <- paste(fetch_what,fetch_date) # rename w date
    print(paste(i, "intersected data frames out of:", length(x)))
  }
  return(y)
}
