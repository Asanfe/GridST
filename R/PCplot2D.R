PCplot2D <- function(x, plane ="xy"){
  if(class(x)[1] == "repgrid"){
    x <- adapt(x)
  }
 names <- rownames(PCAcoord(x))
 if(plane == "xy"){
   plot(PCAcoord(x), xlab="Primer Componente", ylab="Segundo Componente")
   text(PCAcoord(x), labels=rownames(x))
 }
 if(plane == "xz"){
    plot(PCAcoord(x)[,1],PCAcoord(x)[,3],xlab="Primer Componente", ylab="Tercer Componente" )
    text(PCAcoord(x)[,1],PCAcoord(x)[,3], labels=rownames(x))
 }
 if(plane == "yz"){
    plot(PCAcoord(x)[,2],PCAcoord(x)[,3],xlab="Primer Componente", ylab="Tercer Componente")
    text(PCAcoord(x)[,2],PCAcoord(x)[,3], labels=rownames(x))
  }
}
