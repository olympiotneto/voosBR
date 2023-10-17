library(tidyverse)


dados <- readRDS("data-raw/brflights.rds")

dados <- dados |>
  mutate(
    across(
      ends_with("date"),
      ~.x |> lubridate::dmy_hm()
    ),
    across(
  ends_with("date"),
  ~.x |> lubridate::year(),
  .names = "{.col}_year"
    )
)



#manipulação. o saveRDS compacta por natureza

saveRDS(dados,"data/brflights.rds")


#manipulação. o write_rds do readr não compacta por padrão

# readr::write_rds(dados,"data/brflights.rds")
