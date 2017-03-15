coins <- rep(c(T,F,T,T,T,F),1000)
coins_int <- rep(c(1,0,1,1,1,0),1000)
object.size(coins)
object.size(coins_int)


colores <- c("azul","amarillo","rojo","verde")
colores_char <- sample(colores,10000,T)
colores_factor <- factor(colores_char)
object.size(colores_char)
object.size(colores_factor)

nivel <- c("malo","medio","bueno")
nivel_char <- sample(nivel,10000,T)
nivel_cat <- factor(nivel_char)
nivel_ord <- factor(nivel_char,ordered = T,levels = nivel)
unclass(nivel_cat)
unclass(nivel_ord)

unif <- runif(1000,0,10)
ints <- sample(1:10,1000,T)
unif_d<- as.double(unif)
unif_s<- as.single(unif)
str(unif_d)
object.size(unif_d)/1000
object.size(ints)/1000
object.size(unif_s)/1000
??type
