throun_make_df <- function(input) {
    tiundamork |> 
        filter(
            tiundarbreyta == input$tiundarbreyta,
            name %in% c(input$teljari, input$nefnari, "Fjöldi í hóp")
        ) |> 
        pivot_wider() |> 
        rename(teljari = input$teljari, nefnari = input$nefnari, fjoldi = "Fjöldi í hóp") |>
        mutate(
            teljari = adjust_cpi(teljari, cpi, input),
            nefnari = adjust_cpi(nefnari, cpi, input),
            hlutf = teljari / nefnari,
            text = str_c(
                "Tíundarhluti: ", tiundarhluti, "\n",
                "Ár: ", ar, "\n",
                "Fjöldi í hóp: ", fjoldi, "\n",
                input$teljari, ": ", isk(teljari), "\n",
                input$teljari, " (á mann): ", isk(teljari/fjoldi, scale = 1e-6, suffix = " kr"), "\n",
                input$nefnari, ": ", isk(nefnari), "\n",
                input$nefnari, " (á mann): ", isk(nefnari/fjoldi, scale = 1e-6, suffix = " kr"), "\n",
                "Hlutfall: ", hlutf(hlutf)
            )
        )
    
}

throun_make_ggplot <- function(df, input) {
    df |> 
        ggplot(aes(ar, hlutf, text = text)) +
        geom_line(aes(group = tiundarhluti, col = tiundarhluti), size = 1) +
        geom_text(
            data = df |> 
                filter(ar == max(ar)),
            aes(
                label = str_c(tiundarhluti, ". tíund"), 
                col = tiundarhluti,
                group = tiundarhluti
            ),
            nudge_x = diff(range(df$ar))/16,
            size = 5
        ) +
        scale_x_continuous(
            breaks = pretty_breaks(pmin(length(unique(df$ar)), 6)),
            limits = c(min(df$ar), max(df$ar + diff(range(df$ar))/8)),
            expand = expansion()
        ) +
        scale_y_continuous(
            labels = function(x) hlutf(x),
            limits = c(
                pmin(min(df$hlutf), 0),
                max(df$hlutf) * 1.01
            ),
            expand = expansion()
        ) +
        scale_colour_distiller(type = "div", palette = "RdYlBu", direction = 1) +
        coord_cartesian() +
        theme(
            legend.position = "none",
            plot.title = element_text(size = 16)
        ) +
        labs(
            x = NULL,
            y = NULL,
            colour = NULL,
            title = throun_plot_title(input),
            caption = global_caption
        )
    
}

throun_make_plotly <- function(plot, ...) {
    ggplotly(
        plot, 
        tooltip = "text"
    ) |> 
        layout(hoverlabel = list(align = "left")) |> 
        config(
            displayModeBar = FALSE
        )
}


adjust_cpi <- function(value, cpi, input) {
    if (input$verdlag == "Núvirt") {
        return(value / cpi)
    } else {
        value
    }
}

throun_plot_title <- function(input) {
    str_c(
        input$teljari, 
        " sem hlutfall af ",
        str_to_lower(names(nefnari_choices)[nefnari_choices == input$nefnari]),
        " eftir tíundum ",
        tiundamork_tiundabreytur_eignafall[input$tiundarbreyta]
    )
}

nefnari_choices <- c(
    "Eignum alls" = "Eignir alls",
    "Skuldum alls" = "Skuldir alls",
    "Eigin fé alls (Eignum - Skuldum)" = "Eigið fé alls (Eignir - Skuldir)",
    "Tekjum alls" = "Tekjur alls",
    "Ráðstöfunartekjum (Tekjum - Sköttum)" = "Ráðstöfunartekjur (Tekjur - Skattar)"
)


teljari_choices <- c(
    "Eignir alls",
    "Fasteignir",
    "Ökutæki",
    "Innlán",
    "Verðbréf",
    "Aðrar eignir",
    "Skuldir alls",
    "Íbúðalán",
    "Aðrar skuldir",
    "Eigið fé í fasteign",
    "Eigið fé annað",
    "Atvinnutekjur",
    "Fjármagnstekjur",
    "Aðrar tekjur",
    "Skattar alls",
    "Vaxtagjöld v/íbúðalána",
    "Ráðstöfunartekjur (Tekjur - Skattar)"
)
