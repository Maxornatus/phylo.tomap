
## phylo.to.map para análisis biogeográficos
## author: Maximiliano Ortiz Varela

<div style="text-align: justify">
En los campos de la biología evolutiva y la biogeografía, investigar la distribución de las especies y su relación con la evolución es esencial para desentrañar los complejos patrones que han dado forma a la diversidad de la vida a lo largo del tiempo y en diferentes lugares. La distribución geográfica de las especies es el resultado de una interacción dinámica entre una multitud de factores. Entre estos factores se incluyen eventos históricos, como las glaciaciones que han modificado los paisajes y los hábitats; cambios climáticos a lo largo de escalas temporales diversas, que han influido en la disponibilidad de recursos y en la adaptabilidad de las especies; barreras geográficas, como cordilleras, ríos y océanos, que han actuado como obstáculos para la dispersión; y procesos evolutivos, como la evolución de características morfológicas y fisiológicas que han permitido a las especies adaptarse a entornos específicos y expandirse a nuevas áreas.

En este tutorial, exploraremos cómo utilizar la función "phylo.to.map" para representar gráficamente datos filogenéticos y de distribución geográfica de especies. Dicha función nos permite combinar técnicas de análisis filogenético y mapeo geográfico para visualizar la relación entre la historia evolutiva de las especies o individuos mde una misma especie y su distribución geográfica. A lo largo de este tutorial, aprenderemos paso a paso cómo utilizar esta función para crear visualizaciones impactantes que nos ayuden a comprender mejor la biogeografía y la evolución de las especies. 
</div>

Establecer el directorio de trabajo

```{r setwd, echo=TRUE, message = FALSE}

setwd("D:/archivos portatil/archivos y programas/trabajos en r/PHYLO TO MAP")

```

Cargar bibliotecas

```{r librerias, echo=TRUE, message = FALSE}

library(phytools)
library(viridis)
library(viridisLite)
library(ape)
library(maps)
```

lectura de datos 

```{r datos, echo=TRUE, message = FALSE, results='asis'}

data <- read.csv("coo_vari.csv", header = TRUE, row.names = 1)
tree <- read.tree("B_vari.tre")
```

Definir rangos de filas y grupos correspondientes

```{r color listas, echo=TRUE}

rangos_color <- list(
  c(1, 12),   # Primer rango
  c(13, nrow(data))  # Segundo rango
)

```

Crear el objeto phylo.to.map

```{r obj, echo=TRUE, message = FALSE}

obj <- phylo.to.map(tree, data, plot=FALSE, direction="rightwards",
                    region="america")
```

Definir colores para cada grupo

```{r colores, echo=TRUE}

colores_personalizados <- c("blue", "green")
```

Obtener los colores para cada especie dentro de los grupos

```{r colores asignacion, echo=TRUE}

cols <- rep(NA, length(obj[["tree"]][["tip.label"]]))

```

Asigna colores basándote en los rangos

```{r for, echo=TRUE}

for (i in seq_along(rangos_color)) {
  rango <- rangos_color[[i]]
  colores_rango <- rep(colores_personalizados[i], diff(rango) + 1)
  cols[rango[1]:rango[2]] <- colores_rango
}
```

Asegura que cualquier posición no asignada obtenga un color predeterminado (puedes ajustar según necesites)

```{r color opcion, echo=TRUE}

cols[is.na(cols)] <- "gray"

```

Crea un vector de colores asignados a las puntas del árbol

```{r vector color, echo=TRUE}

cols <- setNames(cols, tree$tip.label)
```

Grafica

```{r grafica, echo=TRUE}

plot(obj, direction = "rightwards", colors = cols, ftype = "off", cex.points = c(0, 1.2),
     pts = FALSE, lwd = c(1, 0.5))
```

Colorear y mostrar el mapa (opcion arcoiris)

```{r grafica alternativa, echo=TRUE}

reinbowc <- setNames(sample(rainbow(n = Ntip(tree))), tree$tip.label)

plot(obj, direction="rightwards", colors = reinbowc, ftype = "off", cex.points = c(0, 1.5),
     pts = TRUE, lwd = c(1, 0.5))
```

