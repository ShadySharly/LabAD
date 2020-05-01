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
            
url = "Escritorio/BCW/breast-cancer-wisconsin.data"
df = read.csv(url, header = F, sep=",", col.names = columns)

# GRAFICOS # //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////// #

# Comparacion Grosor del Grupo # 
boxplot(clumpThickness ~ class, data = df, xlab = "Clase", ylab = "Grosor", border = c("green", "red"), col = "lightgray")
title("Comparacion Grosor del Grupo")

# ----------------------------------------------------------------------------------------------------------------------------------- #
# Comparacion Tamaño Celula Uniforme # 
boxplot(unifCellSize ~ class, data = df, xlab = "Clase", ylab = "Tamaño", border = c("green", "red"), col = "lightgray")
title("Comparacion Tamaño Celula Uniforme")

# ----------------------------------------------------------------------------------------------------------------------------------- #
# Comparacion Forma Celula Uniforme #
boxplot(unifCellShape ~ class, data = df, xlab = "Clase", ylab = "Forma", border = c("green", "red"), col = "lightgray")
title("Comparacion Forma Celula Uniforme")

# ----------------------------------------------------------------------------------------------------------------------------------- #
# Comparacion Adhesion marginal #
boxplot(marginalAdhesion ~ class, data = df, xlab = "Clase", ylab = "Adhesion Marginal", border = c("green", "red"), col = "lightgray")
title("Comparacion Adhesion marginal")

# ----------------------------------------------------------------------------------------------------------------------------------- #
# Comparacion Tamaño Celula Epitelial #
boxplot(epithCellSize ~ class, data = df, xlab = "Clase", ylab = "Tamaño", border = c("green", "red"), col = "lightgray")
title("Comparacion Tamaño Celula Epitelial")

# ----------------------------------------------------------------------------------------------------------------------------------- #
# Comparacion Nucleo Desnudo # **
#boxplot(bareNuclei ~ class, data = df, xlab = "Clase", ylab = "Nucleo Desnudo", border = c("green", "red"), col = "lightgray")
#title("Comparacion Nucleo Desnudo")

# ----------------------------------------------------------------------------------------------------------------------------------- #
# Comparacion Cromatina Suave #
boxplot(blandChromatin ~ class, data = df, xlab = "Clase", ylab = "Cromatina Suave", border = c("green", "red"), col = "lightgray")
title("Comparacion Cromatina Suave")

# ----------------------------------------------------------------------------------------------------------------------------------- #
# Comparacion Nucleolo Normal #
boxplot(normalNucleoli ~ class, data = df, xlab = "Clase", ylab = "Nucleolo Normal", border = c("green", "red"), col = "lightgray")
title("Comparacion Nucleolo Normal")

# ----------------------------------------------------------------------------------------------------------------------------------- #
# Comparacion Mitosis #
boxplot(mitoses ~ class, data = df, xlab = "Clase", ylab = "Mitosis", border = c("green", "red"), col = "lightgray")
title("Comparacion Mitosis")

# /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////// #






  
