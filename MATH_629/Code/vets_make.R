library(tidyverse)
setwd("C:/GitStuff/Survival_Analysis/MATH_629/Data")
vets<-read.fwf("vets.dat", widths = c(-1,1,1,1,1,1,-1,3,-1,2,-1,2,-1,2,-1,2,-1,1),header=FALSE)
vets <-
  rename(vets,
    Treatment = V1,
    Cell.Type.1 = V2,
    Cell.Type.2 = V3,
    Cell.Type.3 = V4,
    Cell.Type.4 = V5,
    Survival.time = V6,
    Performance.status=V7,
    Disease.duration=V8,
    Age=V9,
    Prior.Therapy=V10,
    Status=V11
    )
