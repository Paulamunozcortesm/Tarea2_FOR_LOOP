library(dplyr)
library(stringr)
library(ggplot2)

tamanios <- list('grandes','medianas','pequena','micro')
paises <- list('chile','peru','colombia')
bases <- data.frame(fecha=character(),
                    pais=character(), 
                    ingresos=character(), 
                    costos=character(),
                    procentaje_muejeres=character(),
                    exportaciones=character(),
                    importaciones=character(),
                    endeudamiento=character(),
                    morosidad=character(),
                    reservas=character(),
                    spread=character(),
                    tasa_interes=character(),
                    tamanio=character()) 


for(tamanio in tamanios)
{
  for(pais in paises)
  {
    archivo <- paste(tamanio, "_", pais, ".csv", sep = "")
    #punto 1 cargar las bases de datos
    aux <- read.csv(file=archivo, header=TRUE, sep=';')
    #punto 2 juntar las bases y crear la columna tamaño
    aux['tamanio'] = tamanio
    bases <- bind_rows(bases, aux)
  }
}
#Fix de porcentaje mujeres (3 nombres distintos en los CSV)
ab = coalesce(bases['porcentaje_mujeres'], bases['procentaje_mujeres'], bases['procentaje_muejeres'])
l = do.call(coalesce, ab)
bases['procentaje_muejeres'] = data.frame(matrix(unlist(l), nrow=length(l), byrow=T))
bases = subset(bases, select = -c(porcentaje_mujeres,procentaje_mujeres) )

sapply(bases, class)

#Punto 3
peru = chile = 0
for(row in 1:nrow(bases))
{
  pais <- bases[row, "pais"]
  if(pais == "chile")
  {
    chile = chile + 1
  }
  else if(pais == "peru")
  {
    peru = peru + 1
  }
}
print(paste("Peru tiene",peru,"observaciones. Mientras que Chile tiene",chile,"observaciones."))

#punto 4
i_chile = i_peru = i_colombia = 0.0
for(row in 1:nrow(bases))
{
  pais <- bases[row, "pais"]
  ingreso <- as.double(str_replace(bases[row, "ingresos"],',','.'))
  if(pais == "chile")
  {
    i_chile = i_chile + ingreso
  }
  else if(pais == "peru")
  {
    i_peru = i_peru + ingreso
  }
  else if(pais == "colombia")
  {
    i_colombia = i_colombia + ingreso
  }
}
max_p = i_chile
max_pa = "chile"
if(i_peru > max_p)
{
  max_p = i_peru
  max_pa = "peru"
}
if(i_colombia > max_p)
{
  max_p = i_colombia
  max_pa = "colombia"
}
print(paste("El pais con el máximo de ingresos es",max_pa,"con un total de",max_p))

#punto 5
bases["columna"] = 0
for(row in 1:nrow(bases))
{
  pais <- bases[row, "pais"]
  tasa_interes <- as.double(str_replace(bases[row, "tasa_interes"],',','.'))
  if(pais == "chile")
  {
    bases[row, "columna"] = tasa_interes * 0.1
  }
  else if(pais == "peru")
  {
    bases[row, "columna"] = tasa_interes + 0.3
  }
  else if(pais == "colombia")
  {
    bases[row, "columna"] = tasa_interes / 10
  }
}
bases

#punto 6
for(row in 1:nrow(bases))
{
  exportaciones <- round(as.double(str_replace(bases[row, "exportaciones"],',','.')), 1)
  if(exportaciones > 2.1)
  {
    bases[row, "exportaciones"] = 1
  }
  else if(exportaciones < 2.1)
  {
    bases[row, "exportaciones"] = 2
  }
  else if(exportaciones == 2.1)
  {
    bases[row, "exportaciones"] = 3
  }
}
bases

#punto 7 ranking de tasa de interes por pais.

p <- ggplot(data=bases, aes(x=pais, y=columna)) + geom_bar(stat="identity")
p