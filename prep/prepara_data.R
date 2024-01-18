# librerías
library(tidyverse)
library(styleBreton)


# adquisición de datos
dt_00 <- 
  read_csv("raw/PDB_LV_21122023155425889.csv")



# limpieza de datos
dt_01 <- 
  dt_00 |> 
  select(LOCATION,
         Country,
         code = SUBJECT,
         description = Subject,
         Unit,
         Measure, 
         Time,
         PowerCode,
         reference_period = `Reference Period`,
         Value,
         Flags) |> 
  janitor::clean_names()
  


# códigos contenidos
codigos <- 
  dt_01 |> 
  distinct(code, description)



# medidas
dt_01 |> 
  distinct(measure)



# paises
dt_01 |> 
  distinct(location, country) |> 
  print(n = 50)


# data solo países
dt_02 <- 
  dt_01 |> 
  filter(!code %in% c("BRIICS", "OECD", "G-7", "EA19", "EU27_2020"))



# guardar dataframe
write_csv(dt_02, "prep/productivity_00.csv")







