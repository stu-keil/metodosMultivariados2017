install.packages("dummies")
library(dummies)
install.packages("FactoMineR")
library(FactoMineR)
install.packages("ggtern")
library(ggtern)

data <- datasets::HairEyeColor
summary(data)
HairEye <- apply(data,c(1,2),sum)
HairEye

# Convert from data frame of counts to data frame of cases.
# `countcol` is the name of the column containing the counts
countsToCases <- function(x, countcol = "Freq") {
  # Get the row indices to pull from x
  idx <- rep.int(seq_len(nrow(x)), x[[countcol]])

  # Drop count column
  x[[countcol]] <- NULL

  # Get the rows from x
  x[idx, ]
}

df <- countsToCases(as.data.frame(as.table(HairEye)))
df.dummies <- dummy.data.frame(df, sep = ".")
cor(df.dummies)
res <- FactoMineR::PCA(df.dummies)
par(mfrow=c(1,1))

chisq.test(HairEye)
curve(dchisq(x,df=9),from=0,to=20)


prop.table(HairEye)-(prop.table(margin.table(HairEye,1)) %*% t(prop.table(margin.table(HairEye,2))))


x <- as.matrix(df.dummies[,1:4])
y <- as.matrix(df.dummies[,5:8])

### Obtener tabla de contingencia
P <- t(x) %*% y

### Probabilidad marginal
t(x) %*% x
t(y) %*% y

### Probabilidad condicional

P1 <- P / sum(as.numeric(P))

apply(P1,c(1,2),scales::percent)

#### Perfiles por renglon, probabilidad de tener color de pelo si tengo algun color de ojos
#### P(ojos|pelo)
Dr <- t(x) %*% x /sum(as.numeric(P))
apply(Dr,c(1,2),scales::percent)

Drinv <- diag(1/diag(Dr))
dimnames(Drinv) <- dimnames(Dr)
Pr <- Drinv %*% P1
apply(Pr,c(1,2),scales::percent)

#Prob(pelo|ojos)
Dc <- t(y) %*% y /sum(as.numeric(P))
apply(Dc,c(1,2),scales::percent)

Dcinv <- diag(1/diag(Dc))
dimnames(Dcinv) <- dimnames(Dc)
Pc <- P1 %*% Dcinv
apply(Pc,c(1,2),scales::percent)

#--------------------------------------------------------

data <- datasets::HairEyeColor
summary(data)
HairEye <- apply(data,c(1,2),sum)
HairEye
t(x)%*%x
t(y)%*%y

#Matriz de frecuencias relativas
t(x)%*%y/sum(as.numeric(t(x)%*%y))


#Analisis por filas
#(X'X)^(-1)X'Y
Pr <- solve(t(x)%*%x,t(x)%*%y)
Pr

plot_dat <- data.frame(Pr,check.names = FALSE)
ggtern(plot_dat, aes(x=Eye.Brown, y = Eye.Blue,z=Eye.Hazel,color=Eye.Blue))+
  geom_point() +
  geom_text(label = row.names(plot_dat))

#para poder medir distancias adecuadamente entre cada perfil de fila
#necesitamos medir la diferencia en distribución pesada inversamente
#proporcional al peso o masa de cada columna. Esto se conoce como distancia chi-cuadrada
tbl <- HairEye
chi_dist <- function(row1, row2, tbl){
  col_masses <- apply(tbl,2,sum)/sum(as.numeric(tbl))
  Pr <- solve(t(x)%*%x,t(x)%*%y)
  dif <- as.numeric(Pr[row1,]) - as.numeric(Pr[row2,])
  sqrt(sum((1/col_masses)*dif^2))#masas se pesan por diferencias al cuadrado
}
#Distancia entre brown hair y black hair
chi_dist(1,2,tbl)

chi_dist(1,4,tbl)

#Matriz de distancias entre perfiles de fila
hair_types <- dimnames(tbl)[[1]]
n_types <- length(hair_types)
row_dist_mat <- matrix(0,ncol = n_types, nrow = n_types,dimnames = list(hair_types,hair_types))
for(i in 1:(n_types-1)){
  for(j in i:n_types){
    row_dist_mat[i,j] <- chi_dist(i,j,tbl)
    row_dist_mat[j,i] <- row_dist_mat[i,j]
  }
}
row_dist_mat

# scores o factores de filas
colmas <- apply(tbl,2,sum)/sum(as.numeric(tbl))
#restar a cada columna su masa al porcentaje

ones_col <- matrix(rep(1,dim(tbl)[1]),ncol = 1)
#Vamos a calcular las diferencias vs el promedio del mundo
Pr_tilde <- Pr - ones_col %*% t(colmas)

res <- svd(Pr_tilde)

?geom_text
