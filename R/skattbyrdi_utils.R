skattbyrdi_make_df <- function(input) {
    skattbyrdi |> 
        filter(ar >= input$ar_fra) |> 
        group_by(tiundarhluti) |> 
        mutate(
            visitala = make_index(skattbyrdi, ar, input)
        ) |> 
        ungroup() |> 
        mutate(
            text = str_c(
                "Tíundarhluti: ", tiundarhluti, "\n",
                "Ár: ", ar, "\n",
                "Skattbyrði: ", percent(skattbyrdi, accuracy = 0.01), "\n",
                "Breyting (síðan ", input$ar_fra, "):", skattbyrdi_y_labels(input)(visitala)
            )
        )
}

skattbyrdi_make_ggplot <- function(df, input) {
    df |> 
        ggplot(aes(ar, visitala, text = text)) +
        geom_line(
            data = tibble(),
            aes(x = range(df$ar),
                y = c(0, 0)), 
            lty = 2, 
            alpha = 0.4,
            inherit.aes = FALSE
        ) +
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
            labels = skattbyrdi_y_labels(input)
        ) +
        scale_colour_distiller(type = "div", palette = "RdYlBu", direction = 1) +
        coord_cartesian() +
        theme(legend.position = "none") +
        labs(
            x = NULL,
            y = NULL,
            colour = NULL,
            title = skattbyrdi_plot_title(input),
            caption = global_caption
        )
}

skattbyrdi_make_plotly <- function(plot, ...) {
    ggplotly(
        plot, 
        tooltip = "text"
    ) |> 
        layout(hoverlabel = list(align = "left")) |> 
        config(
            displayModeBar = FALSE
        )
}


skattbyrdi_make_table <- function(df, input) {
    df |> 
        select(ar, tiundarhluti, skattbyrdi, visitala) |> 
        filter(ar %in% range(ar)) |> 
        pivot_longer(c(skattbyrdi, visitala)) |> 
        unite(name, ar, col = "name") |> 
        pivot_wider() |> 
        select(-3) |> 
        mutate(visitala_2021 = skattbyrdi_y_labels(input)(visitala_2021)) |> 
        mutate_at(vars(starts_with("skattbyrdi")), label_percent(accuracy = 0.1)) |> 
        set_names(
            "Tíundarhluti", 
            str_c("Skattbyrði (", input$ar_fra, ")"),
            "Skattbyrði (2021)",
            "Hrein breyting"
        )
}


make_index <- function(skattbyrdi, ar, input) {
    out <- skattbyrdi - skattbyrdi[ar == min(ar)]
}


skattbyrdi_index_type_choices <- c(
    "Hrein breyting",
    "Hlutfallsleg breyting"
)

skattbyrdi_yintercepts <- list(
    "Hrein breyting" = 0,
    "Hlutfallsleg breyting" = 1
)

skattbyrdi_y_labels <- function(input) {
    out <- label_percent(accuracy = 0.1, suffix = "%-stig")
    out
}

skattbyrdi_plot_title <- function(input) {
    
    str_c(
        "Hrein breyting", " á skattbyrði eftir tekjutíund síðan ", input$ar_fra
    )
}

skattbyrdi_table_caption <- function(input) {
    str_c(
        "Samanburður á skattbyrði eftir tekjutíundunum árin ",
        input$ar_fra,
        " og ",
        max(skattbyrdi$ar)
    )
}
