# ATRIBUTOS # /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////// #

library(ggpubr)
library(cowplot)
library(corrplot)

# - code: Numero del codigo de la muestra
# - clumpThickness: Grosor del grupo (1 - 10)
# - unifCellSize: TamaÃ±o de celula uniforme (1 - 10)
# - unifCellShape: Forma de celula uniforme (1 - 10)
# - marginalAdhesion: Adhesion marginal (1 - 10)
# - epithCellSize: TamaÃ±o de celula epitelial (1 - 10)
# - bareNuclei: Nucleos desnudos (1 - 10)
# - blandChromatin: Cromatina suave (1 - 10)
# - normalNucleoli: Nucleolos normales (1 - 10) 
# - mitoses: Mitosis (1 - 10)
# - class: Clase (2 para BENIGNO, 4 para MALIGNO)

columns = c("code",
            "clumpThickness",
            "unifCellSize",
            "unifCellShape",
            "marginalAdhesion",
            "epithCellSize",
            "bareNuclei",
            "blandChromatin",
            "normalNucleoli",
            "mitoses",
            "class"
)
            
url = "https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/breast-cancer-wisconsin.data"
df = read.csv(url, header = F, sep=",", col.names = columns)

# Eliminacion de Missing Values "?"
df <- df[!(df$bareNuclei == "?"),]

# CORRELACION # /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////// #

df$code <- NULL
df$bareNuclei <- as.numeric(df$bareNuclei)
#Se verifica si las variables siguen una distribución normal
distribucion <- apply(df,2,shapiro.test)
apply(df,2,hist)
#Ninguna sigue una distribuición normal, se opta por un test no paramétrico
df.cor = cor(df, method = 'spearman')

corrplot(df.cor, method = 'ellipse')
title("Matriz de Correlacion")

# GRAFICOS # //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////// #

# Transformar los 2 y 4 a "B" y "M", respetivamente.
df$class <- as.character(df$class)
df$class[df$class == "2"] <- "Benign"
df$class[df$class == "4"] <- "Malignant"

# Comparacion Grosor del Grupo # 
boxplot(clumpThickness ~ class, data = df, xlab = "class", ylab = "clumpThickness", border = c("green", "red"), col = "lightgray")
title("Comparacion Clump Thickness")

# ----------------------------------------------------------------------------------------------------------------------------------- #
# Comparacion TamaÃ±o Celula Uniforme # 
boxplot(unifCellSize ~ class, data = df, xlab = "class", ylab = "unifCellSize", border = c("green", "red"), col = "lightgray")
title("Comparacion Uniformity of Cell Size")

# ----------------------------------------------------------------------------------------------------------------------------------- #
# Comparacion Forma Celula Uniforme #
boxplot(unifCellShape ~ class, data = df, xlab = "class", ylab = "unifCellShape", border = c("green", "red"), col = "lightgray")
title("Comparacion Uniformity of Cell Shape")

# ----------------------------------------------------------------------------------------------------------------------------------- #
# Comparacion Adhesion marginal #
boxplot(marginalAdhesion ~ class, data = df, xlab = "class", ylab = "marginalAdhesion", border = c("green", "red"), col = "lightgray")
title("Comparacion Marginal Adhesion")

# ----------------------------------------------------------------------------------------------------------------------------------- #
# Comparacion TamaÃ±o Celula Epitelial #
boxplot(epithCellSize ~ class, data = df, xlab = "class", ylab = "epithCellSize", border = c("green", "red"), col = "lightgray")
title("Comparacion Single Epithelial Cell Size")

# ----------------------------------------------------------------------------------------------------------------------------------- #
# Comparacion Nucleo Desnudo #
boxplot(bareNuclei ~ class, data = df, xlab = "class", ylab = "bareNuclei", border = c("green", "red"), col = "lightgray")
title("Comparacion Bare Nuclei")

# ----------------------------------------------------------------------------------------------------------------------------------- #
# Comparacion Cromatina Suave #
boxplot(blandChromatin ~ class, data = df, xlab = "class", ylab = "blandChromatin", border = c("green", "red"), col = "lightgray")
title("Comparacion Bland Chromatin")

# ----------------------------------------------------------------------------------------------------------------------------------- #
# Comparacion Nucleolo Normal #
boxplot(normalNucleoli ~ class, data = df, xlab = "class", ylab = "normalNucleoli", border = c("green", "red"), col = "lightgray")
title("Comparacion Normal Nucleoli")

# ----------------------------------------------------------------------------------------------------------------------------------- #
# Comparacion Mitosis #
boxplot(mitoses ~ class, data = df, xlab = "class", ylab = "mitoses", border = c("green", "red"), col = "lightgray")
title("Comparacion Mitoses")

# /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////// #
# Grafico de Pie

diagnosis.table <- table(df$class)
colors <- terrain.colors(2) 
# Create a pie chart 
diagnosis.prop.table <- prop.table(diagnosis.table)*100
diagnosis.prop.df <- as.data.frame(diagnosis.prop.table)
pielabels <- sprintf("%s - %3.1f%s", diagnosis.prop.df[,1], diagnosis.prop.table, "%")

pie(diagnosis.prop.table,
    labels=pielabels,  
    clockwise=TRUE,
    col=colors,
    border="gainsboro",
    radius=0.8,
    cex=0.8, 
    main="Distribucion de la Clase")
legend(1, .4, legend=diagnosis.prop.df[,1], cex = 0.7, fill = colors)

class(df$bareNuclei)
class(df$clumpThickness)

# GRAFICOS2 # /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////// #

# Comparación Clump Thickness #
boxplot.clumpThickness =  ggboxplot(data = df, x = "class", y = "clumpThickness", color = "class") + border()

ydens = axis_canvas(boxplot.clumpThickness, axis = "y", coord_flip = TRUE) + geom_density(data = df, aes(x = clumpThickness, fill = class), alpha = 0.7, size = 0.2) + coord_flip()

boxplot.clumpThickness = insert_yaxis_grob(boxplot.clumpThickness, ydens, grid::unit(.2, "null"), position = "right")
ggdraw(boxplot.clumpThickness)

# ----------------------------------------------------------------------------------------------------------------------------------- #
# Comparacion Uniformity of Cell Size # 
boxplot.unifCellSize =  ggboxplot(data = df, x = "class", y = "unifCellSize", color = "class") + border()

ydens = axis_canvas(boxplot.unifCellSize, axis = "y", coord_flip = TRUE) + geom_density(data = df, aes(x = unifCellSize, fill = class), alpha = 0.7, size = 0.2) + coord_flip()

boxplot.unifCellSize = insert_yaxis_grob(boxplot.unifCellSize, ydens, grid::unit(.2, "null"), position = "right")
ggdraw(boxplot.unifCellSize)

# ----------------------------------------------------------------------------------------------------------------------------------- #
# Comparacion Uniformity of Cell Shape #
boxplot.unifCellShape =  ggboxplot(data = df, x = "class", y = "unifCellShape", color = "class") + border()

ydens = axis_canvas(boxplot.unifCellShape, axis = "y", coord_flip = TRUE) + geom_density(data = df, aes(x = unifCellShape, fill = class), alpha = 0.7, size = 0.2) + coord_flip()

boxplot.unifCellShape = insert_yaxis_grob(boxplot.unifCellShape, ydens, grid::unit(.2, "null"), position = "right")
ggdraw(boxplot.unifCellShape)

# ----------------------------------------------------------------------------------------------------------------------------------- #
# Comparacion Marginal Adhesion #
boxplot.marginalAdhesion =  ggboxplot(data = df, x = "class", y = "marginalAdhesion", color = "class") + border()

ydens = axis_canvas(boxplot.marginalAdhesion, axis = "y", coord_flip = TRUE) + geom_density(data = df, aes(x = marginalAdhesion, fill = class), alpha = 0.7, size = 0.2) + coord_flip()

boxplot.marginalAdhesion = insert_yaxis_grob(boxplot.marginalAdhesion, ydens, grid::unit(.2, "null"), position = "right")
ggdraw(boxplot.marginalAdhesion)

# ----------------------------------------------------------------------------------------------------------------------------------- #
# Comparacion Single Epithelial Cell Size #
boxplot.epithCellSize =  ggboxplot(data = df, x = "class", y = "epithCellSize", color = "class") + border()

ydens = axis_canvas(boxplot.epithCellSize, axis = "y", coord_flip = TRUE) + geom_density(data = df, aes(x = epithCellSize, fill = class), alpha = 0.7, size = 0.2) + coord_flip()

boxplot.epithCellSize = insert_yaxis_grob(boxplot.epithCellSize, ydens, grid::unit(.2, "null"), position = "right")
ggdraw(boxplot.epithCellSize)

# ----------------------------------------------------------------------------------------------------------------------------------- #
# Comparacion Bare Nuclei #
boxplot.bareNuclei =  ggboxplot(data = df, x = "class", y = "bareNuclei", color = "class") + border()

ydens = axis_canvas(boxplot.bareNuclei, axis = "y", coord_flip = TRUE) + geom_density(data = df, aes(x = bareNuclei, fill = class), alpha = 0.7, size = 0.2) + coord_flip()

boxplot.bareNuclei = insert_yaxis_grob(boxplot.bareNuclei, ydens, grid::unit(.2, "null"), position = "right")
ggdraw(boxplot.bareNuclei)

# ----------------------------------------------------------------------------------------------------------------------------------- #
# Comparacion Bland Chromatin #
boxplot.blandChromatin =  ggboxplot(data = df, x = "class", y = "blandChromatin", color = "class") + border()

ydens = axis_canvas(boxplot.blandChromatin, axis = "y", coord_flip = TRUE) + geom_density(data = df, aes(x = blandChromatin, fill = class), alpha = 0.7, size = 0.2) + coord_flip()

boxplot.blandChromatin = insert_yaxis_grob(boxplot.blandChromatin, ydens, grid::unit(.2, "null"), position = "right")
ggdraw(boxplot.blandChromatin)

# ----------------------------------------------------------------------------------------------------------------------------------- #
# Comparacion Normal Nucleoli #
boxplot.normalNucleoli =  ggboxplot(data = df, x = "class", y = "normalNucleoli", color = "class") + border()

ydens = axis_canvas(boxplot.normalNucleoli, axis = "y", coord_flip = TRUE) + geom_density(data = df, aes(x = normalNucleoli, fill = class), alpha = 0.7, size = 0.2) + coord_flip()

boxplot.normalNucleoli = insert_yaxis_grob(boxplot.normalNucleoli, ydens, grid::unit(.2, "null"), position = "right")
ggdraw(boxplot.normalNucleoli)

# ----------------------------------------------------------------------------------------------------------------------------------- #
# Comparacion Mitosis #
boxplot.mitoses =  ggboxplot(data = df, x = "class", y = "mitoses", color = "class") + border()

ydens = axis_canvas(boxplot.mitoses, axis = "y", coord_flip = TRUE) + geom_density(data = df, aes(x = mitoses, fill = class), alpha = 0.7, size = 0.2) + coord_flip()

boxplot.mitoses = insert_yaxis_grob(boxplot.mitoses, ydens, grid::unit(.2, "null"), position = "right")
ggdraw(boxplot.mitoses)

