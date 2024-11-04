library(DBI)
library(config)
library(tidyverse)
library(lubridate)
library(janitor)
library(feather)
library(metill)
library(arrow)

init_db()


mtl_tiundamork() |> 
    collect() |> 
    write_feather("data/tiundamork.feather")


mtl_skyribreytur_skuldahlutfoll() |> 
    filter(str_detect(skyribreyta, "[0-9]"), hlutf_tegund != "Samtals", name != "Samtals") |> 
    collect() |> 
    write_feather("data/skuldahlutfall.feather")

mtl_fjolskyldugerd_aldur_buseta() |> 
    filter(str_detect(skyribreyta, "[0-9]+")) |> 
    collect() |> 
    write_feather("data/aldurshopar.feather")


read_feather("data/tiundamork.feather") |> 
    filter(
        tiundarbreyta == "Heildartekjur",
        name %in% c("Skattar alls",
                    "Tekjur alls",
                    "Fjöldi í hóp")
        ) |> 
    pivot_wider() |> 
    janitor::clean_names() |> 
    mutate(skattbyrdi = skattar_alls / tekjur_alls) |> 
    write_feather("data/skattbyrdi.feather")




