PCplot3D <- function(x, arrows = FALSE, colarrows = "darkred"){
                           #Require para comprobar la instalación y carga de los paquetes necesarios.
  require(plot3D)
  require(plot3Drgl)
                           #Codigo para crear los puntos en el espacio 3D en base a las coordenadas principales.
  if(!arrows){
    text3D(PCAcoord(x)[,1],PCAcoord(x)[,2],PCAcoord(x)[,3], labels = rownames(PCAcoord(x)), ticktype= "detailed")
    plotrgl()
  }
                           #Codido para crear los vectores en el espacio 3D en base a las coordenadas principales.
  else{
    d <- dim(PCAcoord(x))[1]
    x0 <- c(rep(0,d))
    y0 <- c(rep(0,d))
    z0 <- c(rep(0,d))
    arrows3D(x0,y0,z0,PCAcoord(x)[,1],PCAcoord(x)[,2],PCAcoord(x)[,3], col = colarrows,ticktype= "detailed")
    text3D(PCAcoord(x)[,1],PCAcoord(x)[,2],PCAcoord(x)[,3], labels = rownames(PCAcoord(x)), ticktype= "detailed", add = TRUE)
    plotrgl()
  }
}
