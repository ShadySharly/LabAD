# ATRIBUTOS # /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////// #

# - code: Numero del codigo de la muestra
# - clumpThickness: Grosor del grupo (1 - 10)
# - unifCellSize: Tamaño de celula uniforme (1 - 10)
# - unifCellShape: Forma de celula uniforme (1 - 10)
# - marginalAdhesion: Adhesion marginal (1 - 10)
# - epithCellSize: Tamaño de celula epitelial (1 - 10)
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

# Transformar los 2 y 4 a "B" y "M", respetivamente.
df$class <- as.character(df$class)
df$class[df$class == "2"] <- "B"
df$class[df$class == "4"] <- "M"

# Transformar la columna "Bare Nuclei" de factor a numerico.
df$bareNuclei <- as.numeric(df$bareNuclei)
df.cor = cor(df)

# GRAFICOS # //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////// #

# Comparacion Grosor del Grupo # 
boxplot(clumpThickness ~ class, data = df, xlab = "class", ylab = "clumpThickness", border = c("green", "red"), col = "lightgray")
title("Comparacion Clump Thickness")

# ----------------------------------------------------------------------------------------------------------------------------------- #
# Comparacion Tamaño Celula Uniforme # 
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
# Comparacion Tamaño Celula Epitelial #
boxplot(epithCellSize ~ class, data = df, xlab = "class", ylab = "epithCellSize�o", border = c("green", "red"), col = "lightgray")
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

# /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////// #
  
