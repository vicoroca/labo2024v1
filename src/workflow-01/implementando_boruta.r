
# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require("rpart")
require("rpart.plot")

install.packages("Boruta")
require("Boruta")

setwd("~/buckets/b1/") # establezco la carpeta donde voy a trabajar

# cargo el dataset
dataset <- fread( "./datasets/dataset_pequeno.csv")


dir.create("./exp/", showWarnings = FALSE)
dir.create("./exp/BO-01/", showWarnings = FALSE)
# Establezco el Working Directory DEL EXPERIMENTO
setwd("./exp/BO-01/")

#uso esta semilla 
set.seed(102191)

boruta <- Boruta(clase_ternaria~., data = dataset, doTrace = 2)
print(boruta)
plot(boruta)