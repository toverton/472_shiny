restr <- read_csv("./gun_violence_restrictions.csv")

states@data[["name"]] |> as_vector() |> View()

restr$state |> View()

restr$state == states@data[["name"]]

restr |> 
  mutate_if(is.numeric,as.factor) -> restr #As factor the columns.

states$background_checks_private_sales <- restr$background_checks_private_sales
