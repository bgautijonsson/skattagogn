tiundamork_dreifing_fig <- function(data, input, ...) {
    data |> 
        mutate(p = ifelse(value < 0, 0, p)) |> 
        group_by(ar, name) |> 
        mutate(p = p / sum(p)) |> 
        ungroup() |> 
        ggplot(aes(ar, p, text = text)) +
        geom_col(aes(fill = tiundarhluti, group = tiundarhluti, col = tiundarhluti), position = "fill") +
        scale_x_continuous() +
        scale_y_continuous(labels = label_percent(big.mark = ".", decimal.mark = ",")) +
        scale_colour_brewer(type = "div", palette = "RdYlBu") +
        scale_fill_brewer(type = "div", palette = "RdYlBu") +
        coord_cartesian(expand = FALSE) +
        theme(legend.position = "none") +
        labs(x = NULL,
             y = NULL,
             fill = NULL,
             title = tiundamork_make_plot_name(data$name, input),
             caption = global_caption)
}

tiundamork_magn_fig <- function(data, input, ...) {
    data |> 
        mutate(plot_value = ifelse(plot_value < 0, 0, plot_value)) |> 
        ggplot(aes(ar, plot_value, text = text)) +
        geom_col(aes(fill = tiundarhluti, col = tiundarhluti, group = tiundarhluti), position = "stack") +
        scale_x_continuous() +
        scale_y_continuous(labels = label_number(suffix = " mkr", big.mark = ".", decimal.mark = ",")) +
        scale_colour_brewer(type = "div", palette = "RdYlBu") +
        scale_fill_brewer(type = "div", palette = "RdYlBu") +
        coord_cartesian(expand = FALSE) +
        theme(legend.position = "none") +
        labs(x = NULL,
             y = NULL,
             fill = NULL,
             title = tiundamork_make_plot_name(data$name, input),
             caption = global_caption)
}

tiundamork_make_plotly <- function(plot, ...) {
    ggplotly(
        plot, 
        tooltip = "text"
    ) |> 
        layout(hoverlabel = list(align = "left")) |> 
        config(
            displayModeBar = FALSE
        )
}

tiundamork_tiundabreytur_eignafall <- c(
    "Eignir" = "eigna",
    "Skuldir" = "skulda",
    "Eigið fé" = "eigin fjár",
    "Heildartekjur" = "heildartekna",
    "Ráðstöfunartekjur" = "ráðstöfunartekna"
)

tiundamork_make_plot_name <- function(name, input, ...) {
    nm <- unique(name)
    
    return(str_c(nm, " eftir tíundum ", tiundamork_tiundabreytur_eignafall[input$tiundarbreyta]))
}

tiundamork_verdlag <- function(input, ...) {
    outs <- list(
        "Núvirt" = "value_adj",
        "Verðlag hvers árs" = "value"
    )
    
    outs[[input$verdlag]]
}


tiundamork_input_names <- c(
    "Eignir alls",
    "Fasteignir",
    "Verðbréf",
    "Skuldir alls",
    "Íbúðalán",
    "Vaxtagjöld v/íbúðalána",
    "Eigið fé alls (Eignir - Skuldir)",
    "Eigið fé í fasteign",
    "Tekjur alls",
    "Atvinnutekjur",
    "Fjármagnstekjur",
    "Skattar alls",
    "Ráðstöfunartekjur (Tekjur - Skattar)"
)

tiundamork_tiundabreytur <- c(
    "Eignir",
    "Skuldir",
    "Eigið fé",
    "Heildartekjur",
    "Ráðstöfunartekjur"
)


tiundir_text <- str_c("Tíundir virka þannig að einstaklingum er raðað í hækkandi röð eftir einhverri breytu",
                      " og skipt í 10 jafnstóra hópa þ.a. hópur 1 inniheldur einstaklingana sem skora lægst á breytunni og",
                      " hópur 10 þá sem skorar hæst.")