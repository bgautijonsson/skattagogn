library(shiny)
library(plotly)
library(cowplot)
library(tidyverse)
library(scales)
library(ggthemes)
library(kableExtra)
library(gganimate)
library(lubridate)
library(geomtextpath)
library(ggtext)
library(here)
library(readxl)
library(janitor)
library(DT)
library(bslib)
library(thematic)
library(shinycssloaders)
library(DBI)
library(feather)
library(visitalaneysluverds)
library(metill)
library(shinyjs)

shinyOptions(plot.autocolor = TRUE)

#### DATA ####

tiundamork <- read_feather("data/tiundamork_1997_2021.feather") |> 
    filter(tiundarhluti != "Alls") |> 
    inner_join(
        vnv() |> 
            mutate(cpi = cpi / cpi[date == max(date)]) |> 
            group_by(ar = year(date)) |> 
            summarise(cpi = mean(cpi)),
        by = "ar"
    )

aldurshopar <- read_feather("data/aldurshopar.feather") |> 
    inner_join(
        vnv() |> 
            mutate(cpi = cpi / cpi[date == max(date)]) |> 
            group_by(ar = year(date)) |> 
            summarise(cpi = mean(cpi)),
        by = "ar"
    )

skuldahlutfall <- read_feather("data/skuldahlutfall.feather") |> 
    group_by(ar, hlutf_tegund) |> 
    mutate(hlutf = value / sum(value)) |> 
    ungroup() |> 
    group_by(ar, skyribreyta, hlutf_tegund) |> 
    mutate(hlutf_aldur = value / sum(value)) |> 
    group_by(ar, name, hlutf_tegund) |> 
    mutate(hlutf_skuld = value / sum(value)) |> 
    ungroup()

skattbyrdi <- read_feather("data/skattbyrdi.feather")

isk <- function(x, scale = 1, suffix = " m.kr") number(x / scale, suffix = suffix, big.mark = ".", decimal.mark = ",", accuracy = 1)
hlutf <- label_percent(accuracy = 0.1, big.mark = ".", decimal.mark = ",")

##### Sidebar Info and Plot Captions #####
# This is pasted into the sidebar on each page
sidebar_info <- paste0(
    br(" "),
    h5("Höfundur:"),
    p("Brynjólfur Gauti Guðrúnar Jónsson"),
    HTML("<a href='https://github.com/bgautijonsson/skattagogn' target='_top'> Kóði og gögn </a>")
)
# This is the caption for plots
global_caption <- ""


##### THEMES #####
# Making a light and dark theme in case I want to offer the option later
theme_set(theme_metill())



bs_global_theme(
    bootswatch = "flatly"
)

bs_global_add_variables(
    primary = "#484D6D",
    secondary = "#969696",
    success = "#969696",
    # danger = "#FF8CC6",
    # info = "#FF8CC6",
    light = "#faf9f9",
    dark = "#484D6D",
    bg = "#faf9f9",
    fg = "#737373",
    "body-bg" = "#faf9f9",
    base_font = "Lato",
    heading_font = "Segoe UI",
    "navbar-brand-font-family" = "Playfair Display",
    code_font = "SFMono-Regular"
)


