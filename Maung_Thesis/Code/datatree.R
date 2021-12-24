library(data.tree)
#Creating tree progomatically 
acme <- Node$new("Acme Inc.",v=1)
  accounting <- acme$AddChild("Accounting",v=2)
    software <- accounting$AddChild("New Software")
    standards <- accounting$AddChild("New Accounting Standards")
  research <- acme$AddChild("Research")
    newProductLine <- research$AddChild("New Product Line")
    newLabs <- research$AddChild("New Labs")
  it <- acme$AddChild("IT")
    outsource <- it$AddChild("Outsource")
    agile <- it$AddChild("Go agile")
    goToR <- it$AddChild("Switch to R")

print(acme,"v")
accounting$Get("v")

#Creating tree from data frame
library(treemap)
data(GNI2014)
head(GNI2014)
GNI2014$pathString <- paste("world", 
                            GNI2014$continent, 
                            GNI2014$country, 
                            sep = "/")
population <- as.Node(GNI2014)
print(population, "iso3", "population", "GNI", limit = 20)
print(population)