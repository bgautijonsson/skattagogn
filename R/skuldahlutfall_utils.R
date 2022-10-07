skuldahlutfall_age_levels <- c(
    "< 24 ára",
    "25-29 ára",
    "30-34 ára",
    "35-39 ára",
    "40-44 ára",
    "45-49 ára",
    "50-54 ára",
    "55-59 ára",
    "60-66 ára",
    "67+ ára",
    "Alls"
)

skuldahlutfall_hlutf_levels <- c(
    "< 300%",
    "300-500%",
    ">500%"
)

skuldahlutfall_eignir_levels <- c(
    "< 75%",
    "75-100%",
    "100-150%",
    "> 150%"
)

text_names <- c("Aldri" = "Aldur",
                "Skuldahlutfalli" = "Hlutfall skulda af ráðstöfunartekjum",
                "Eignir" = "eignum",
                "Ráðstöfunartekjur" = "ráðstöfunartekjum")






make_skuldahlutfall_data <- function(input) {
    
    
    plot_dat <- skuldahlutfall |> 
        filter(hlutf_tegund == input$hlutf_tegund)
    
    if (input$group_var == "Aldri") {
        plot_dat$filter_var <- plot_dat$name
        plot_dat$fill_var <- plot_dat$skyribreyta
        filter_var_level <- input$filter_skuldahlutfall
    } else {
        plot_dat$filter_var <- plot_dat$skyribreyta
        plot_dat$fill_var <- plot_dat$name |> as_factor() |> fct_relevel("< 300%", "300-500%")
        filter_var_level <- input$filter_aldur
        if (input$filter_aldur == "Alls") {
            plot_dat <- plot_dat |> 
                mutate(filter_var = "Alls") |> 
                count(ar, filter_var, fill_var, wt = value, name = "value") |> 
                group_by(ar) |> 
                mutate(hlutf = value / sum(value)) |> 
                ungroup() |> 
                group_by(ar, filter_var) |> 
                mutate(hlutf_aldur = value / sum(value)) |> 
                group_by(ar, fill_var) |> 
                mutate(hlutf_skuld = value / sum(value)) |> 
                ungroup()
        }
    }
    
    
    plot_dat <- plot_dat |> 
        select(ar, filter_var, fill_var, value, hlutf, hlutf_aldur, hlutf_skuld) |> 
        filter(filter_var == filter_var_level) |> 
        group_by(ar) |> 
        mutate(p = value / sum(value)) |> 
        ungroup()
    
    
    
    
    plot_dat 
}


skuldahlutfall_dreifing_fig <- function(data, input) {
    
    if (input$group_var == "Aldri") {
        scale_colour <- scale_colour_brewer(type = "div", palette = "RdYlBu")
        scale_fill <- scale_fill_brewer(type = "div", palette = "RdYlBu")
        title <- str_c("Skipting í aldurshópa meðal fólks með skuldir ", input$filter_skuldahlutfall, " af ", text_names[input$hlutf_tegund])
        
        data <- data |> 
            mutate(text = str_c("Ár: ", ar, "\n",
                                text_names[input$group_var], ": ", fill_var, "\n",
                                "Hlutfall (af skuldahlutfallsflokki): ", percent(p, accuracy = 0.1, big.mark = ".", decimal.mark = ","), "\n",
                                "Fjöldi: ", number(value, big.mark = ".", decimal.mark = ",", suffix = " manns")))
        
        alpha <- 1
        
    } else {
        scale_colour <- scale_colour_brewer(type = "qual", palette = "Set1", direction = -1)
        scale_fill <- scale_fill_brewer(type = "qual", palette = "Set1", direction = -1)
        title <- str_c("Skipting ", str_to_lower(input$filter_aldur), " fólks í hópa eftir hlutfalli skulda af ", text_names[input$hlutf_tegund])
        
        data <- data |> 
            mutate(text = str_c("Ár: ", ar, "\n",
                                text_names[input$group_var], ": ", fill_var, "\n",
                                "Hlutfall (af aldurshóp): ", percent(p, accuracy = 0.1, big.mark = ".", decimal.mark = ","), "\n",
                                "Fjöldi: ", number(value, big.mark = ".", decimal.mark = ",", suffix = " manns")))
        
        alpha <- 0.6
    }
    
    
    
    data |> 
        ggplot(aes(ar, p, text = text)) +
        geom_area(aes(fill = fill_var, group = fill_var, col = fill_var), position = "fill", alpha = alpha) +
        scale_x_continuous() +
        scale_y_continuous(labels = label_percent(big.mark = ".", decimal.mark = ",")) +
        scale_colour +
        scale_fill +
        coord_cartesian(expand = FALSE) +
        theme(legend.position = "none") +
        labs(x = NULL,
             y = NULL,
             fill = NULL,
             caption = global_caption,
             title = title)
    
    
}



skuldahlutfall_hlutf_fig <- function(data, input) {
    
    if (input$group_var == "Aldri") {
        scale_colour <- scale_colour_brewer(type = "div", palette = "RdYlBu")
        scale_fill <- scale_fill_brewer(type = "div", palette = "RdYlBu")
        title <- str_c("Hlutfall Íslendinga þar sem skuldir eru ", input$filter_skuldahlutfall, " af ", text_names[input$hlutf_tegund])
        
        data <- data |> 
            group_by(ar) |> 
            mutate(hlutf_heild = sum(hlutf)) |> 
            ungroup() |> 
            mutate(text = str_c("Ár: ", ar, "\n",
                                text_names[input$group_var], ": ", fill_var, "\n",
                                "Hlutfall (af aldurshóp): ", percent(hlutf_aldur, accuracy = 0.1, big.mark = ".", decimal.mark = ","), "\n",
                                "Hlutfall (af öllum einstaklingum): ", percent(hlutf_heild, accuracy = 0.1, big.mark = ".", decimal.mark = ","), "\n",
                                "Fjöldi: ", number(value, big.mark = ".", decimal.mark = ",", suffix = " manns")))
        
        alpha <- 1
    } else {
        scale_colour <- scale_colour_brewer(type = "qual", palette = "Set1", direction = -1)
        scale_fill <- scale_fill_brewer(type = "qual", palette = "Set1", direction = -1)
        title <- str_c("Skuldahlutfall ", input$filter_aldur, " fólks sem hlutfall af ", text_names[input$hlutf_tegund])
        
        data <- data |> 
            mutate(text = str_c("Ár: ", ar, "\n",
                                text_names[input$group_var], ": ", fill_var, "\n",
                                "Hlutfall (af skuldahlutfallsflokki): ", percent(hlutf_skuld, accuracy = 0.1, big.mark = ".", decimal.mark = ","), "\n",
                                "Fjöldi: ", number(value, big.mark = ".", decimal.mark = ",", suffix = " manns")))
        
        alpha <- 0.6
    }
    
    
    
    data |> 
        ggplot(aes(ar, hlutf, text = text)) +
        geom_area(aes(fill = fill_var, group = fill_var, col = fill_var), position = "stack", alpha = alpha) +
        scale_x_continuous() +
        scale_y_continuous(labels = label_percent(big.mark = ".", decimal.mark = ",")) +
        scale_colour +
        scale_fill + 
        coord_cartesian(expand = FALSE) +
        theme(legend.position = "none") +
        labs(x = NULL,
             y = NULL,
             fill = NULL,
             title = title,
             caption = global_caption)
    
    
}

skuldahlutfall_magn_fig <- function(data, input) {
    
    if (input$group_var == "Aldri") {
        scale_colour <- scale_colour_brewer(type = "div", palette = "RdYlBu")
        scale_fill <- scale_fill_brewer(type = "div", palette = "RdYlBu")
        title <- str_c("Fjöldi fólks með skuldir ", input$filter_skuldahlutfall, " af ",  text_names[input$hlutf_tegund], " eftir aldri")
        alpha <- 1
    } else {
        scale_colour <- scale_colour_brewer(type = "qual", palette = "Set1", direction = -1)
        scale_fill <- scale_fill_brewer(type = "qual", palette = "Set1", direction = -1)
        title <- str_c("Fjöldi ", str_to_lower(input$filter_aldur), " einstaklinga eftir skuldum sem hlutfall af ", text_names[input$hlutf_tegund])
        alpha <- 0.6
    }
    
    data <- data |> 
        mutate(text = str_c("Ár: ", ar, "\n",
                            text_names[input$group_var], ": ", fill_var, "\n",
                            "Hlutfall (af skuldahlutfallsflokki): ", percent(p, accuracy = 0.1, big.mark = ".", decimal.mark = ","), "\n",
                            "Fjöldi: ", number(value, big.mark = ".", decimal.mark = ",", suffix = " manns")))
    
    data |> 
        ggplot(aes(ar, value, text = text)) +
        geom_area(aes(fill = fill_var, group = fill_var, col = fill_var), position = "stack", alpha = alpha) +
        scale_x_continuous() +
        scale_y_continuous(labels = label_number(suffix = " manns", big.mark = ".", decimal.mark = ",")) +
        scale_colour +
        scale_fill + 
        coord_cartesian(expand = FALSE) +
        theme(legend.position = "none") +
        labs(x = NULL,
             y = NULL,
             fill = NULL,
             caption = global_caption,
             title = title)
    
    
}

skuldahlutfall_make_plotly <- function(my_plot, input) {
    ggplotly(
        my_plot,
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