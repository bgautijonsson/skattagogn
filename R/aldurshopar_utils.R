aldurshopar_dreifing_fig <- function(data, ...) {
    data |> 
        mutate(p = ifelse(value < 0, 0, p)) |> 
        ggplot(aes(ar, p, text = text)) +
        geom_area(aes(fill = skyribreyta, group = skyribreyta, col = skyribreyta), position = "fill") +
        scale_x_continuous() +
        scale_y_continuous(labels = label_percent(big.mark = ".", decimal.mark = ",")) +
        scale_colour_brewer(type = "div", palette = "RdYlBu") +
        scale_fill_brewer(type = "div", palette = "RdYlBu") +
        coord_cartesian(expand = FALSE) +
        theme(legend.position = "none") +
        labs(x = NULL,
             y = NULL,
             fill = NULL,
             title = aldurshopar_make_plot_name(data$name),
             caption = global_caption)
}

aldurshopar_magn_fig <- function(data, ...) {
    data |> 
        mutate(plot_value= ifelse(plot_value < 0, 0, plot_value)) |> 
        ggplot(aes(ar, plot_value, text = text)) +
        geom_area(aes(fill = skyribreyta, col = skyribreyta, group = skyribreyta), position = "stack") +
        scale_x_continuous() +
        scale_y_continuous(labels = label_number(suffix = " mkr", big.mark = ".", decimal.mark = ",")) +
        scale_colour_brewer(type = "div", palette = "RdYlBu") +
        scale_fill_brewer(type = "div", palette = "RdYlBu") +
        coord_cartesian(expand = FALSE) +
        theme(legend.position = "none") +
        labs(x = NULL,
             y = NULL,
             fill = NULL,
             title = aldurshopar_make_plot_name(data$name),
             caption = global_caption)
}

aldurshopar_make_plotly <- function(plot, ...) {
    ggplotly(
        plot, 
        tooltip = "text"
    ) |> 
        layout(hoverlabel = list(align = "left"),
               margin = list(
                   t = 60,
                   r = 0,
                   b = 120,
                   l = 0
               ),
               annotations = list(
                   list(x = 0.8, xanchor = "right", xref = "paper",
                        y = -0.15, yanchor = "bottom", yref = "paper",
                        showarrow = FALSE,
                        text = global_caption)
               )) |> 
        config(
            displayModeBar = FALSE
        )
}


aldurshopar_make_plot_name <- function(name, ...) {
    nm <- unique(name)
    
    return(str_c(nm, " eftir aldurshópum"))
}

aldurshopar_verdlag <- function(input, ...) {
    outs <- list(
        "Núvirt" = "value_adj",
        "Verðlag hvers árs" = "value"
    )
    
    outs[[input$verdlag]]
}


aldurshopar_input_names <- c(
    "Eignir alls",
    "Fasteignir",
    "Verðbréf",
    "Innlán",
    "Ökutæki",
    "Skuldir alls",
    "Íbúðalán",
    "Eigið fé alls (Eignir - Skuldir)",
    "Eigið fé í fasteign",
    "Tekjur alls",
    "Atvinnutekjur",
    "Fjármagnstekjur",
    "Skattar alls",
    "Vaxtagjöld v/íbúðalána",
    "Ráðstöfunartekjur (Tekjur - Skattar)"
)