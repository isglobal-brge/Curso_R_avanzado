dd <- data.frame(id=c(1,1,1,2,2,3,3,3,4), 
                 dise=as.factor(c("hta", "diab", "epoc", "hta", "inf",
                                  "diab","hta","epoc", "inf")))
dd

dise <- unique(dd$dise)
dise

indv <- unique(dd$id)
datos <- NULL
for(i in indv){
 datos.i <- c(i, dise%in%dd$dise[dd$id==i])
 datos <- rbind(datos, datos.i)
}
datos <- data.frame(datos)
rownames(datos) <- 1:nrow(datos)
colnames(datos) <- c("id", as.character(dise))
datos


# Fast version
datos.wide <- reshape(dd, idvar = "id", timevar = "dise", direction = "wide")
nn <- colnames(datos.wide)
nn<- gsub("var.", "", nn)
colnames(datos.wide) <- nn
datos.wide

