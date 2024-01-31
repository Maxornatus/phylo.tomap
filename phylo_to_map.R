# Establecer el directorio de trabajo
setwd("C:/Users/usuar/OneDrive/Documents/GitHub/phylo.tomap")

# Cargar bibliotecas
library(phytools)
library(viridis)
library(viridisLite)
library(ape)
library(maps)



# lectura de datos 
data <- read.csv("coo_vari.csv", header = TRUE, row.names = 1)
tree <- read.tree("B_vari.tre")
tree <- force.ultrametric(tree)


# Definir rangos de filas y grupos correspondientes
rangos_color <- list(
  c(1, 12),   # Primer rango
  c(13, nrow(data))  # Segundo rango
)

# crear el objeto phylo.to.map
obj <- phylo.to.map(tree, data, plot=FALSE, region="america")



# Definir colores para cada grupo
colores_personalizados <- c("blue", "green")

# Obtener los colores para cada especie dentro de los grupos
cols <- rep(NA, length(obj[["tree"]][["tip.label"]]))

# Asigna colores basándote en los rangos
for (i in seq_along(rangos_color)) {
  rango <- rangos_color[[i]]
  colores_rango <- rep(colores_personalizados[i], diff(rango) + 1)
  cols[rango[1]:rango[2]] <- colores_rango
}

# Asegura que cualquier posición no asignada obtenga un color predeterminado (puedes ajustar según necesites)
cols[is.na(cols)] <- "gray"

# Crea un vector de colores asignados a las puntas del árbol
cols <- setNames(cols, tree$tip.label)



# Grafica
plot(obj, direction = "rightwards",colors = cols, ftype = "off", fsize=0.8,cex.points=c(0.8,1.2),
     pts = T, lwd = 1.5, xlim = c(-90, -35), ylim = c(-55, 12),)
legend("topleft", legend = c("Población uno", "Población dos"), fill = colores_personalizados,
       cex = 1.5, bty = "n")

# Grafica con nombre
plot(obj, direction = "rightwards",colors = cols, ftype = "i", fsize=0.8,cex.points=c(0.8,1.2),
     pts = T, lwd = 1.5, xlim = c(-90, -35), ylim = c(-55, 12),)
legend("topright", legend = c("Población uno", "Población dos"), fill = colores_personalizados,
       cex = 1.5, bty = "n")

# Grafica con clado dentro del mapa1 
plot(obj, direction = "rightwards", type = "direct", colors = cols, ftype = "off", cex.points = 3,
     pts = T, lwd = 1, delimit_map=TRUE)
legend("topright", legend = c("Población uno", "Población dos"), fill = colores_personalizados, title = "Poblaciones",
       cex = 1.5, bty = "n")


# Grafica con clado dentro del mapa 2
plot(obj, type = "direct", colors = cols, pts=FALSE,fsize=0.8,  xlim = c(-90, -35), ylim = c(-55, 12),
     map.bg="lightgreen", map.fill="lightblue", ftype = "off", cex.points = 3, delimit_map=TRUE)

legend("topright", legend = c("Población uno", "Población dos"), fill = colores_personalizados,
       cex = 1.5, bty = "n")


# Colorear y mostrar el mapa (opcion arcoiris)
reinbowc <- setNames(sample(rainbow(n = Ntip(tree))), tree$tip.label)

plot(obj, direction="rightwards", colors = reinbowc, ftype = "off", cex.points = c(0, 1.5),
     pts = TRUE, lwd = c(1, 0.5))



