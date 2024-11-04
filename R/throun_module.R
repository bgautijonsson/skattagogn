#### UI ####

throun_ui <- function(id) {
    sidebarLayout(
        sidebarPanel(
            width = 3,
            selectInput(
                inputId = NS(id, "tiundarbreyta"),
                label = "Breyta til að skipta í tíundamörk",
                choices = tiundamork_tiundabreytur,
                selected = c("Skuldir"),
                multiple = FALSE,
                selectize = FALSE
            ),
            fluidRow(
                column(
                    6,
                    
                    selectInput(
                        inputId = NS(id, "teljari"),
                        label = "Sýna",
                        choices = teljari_choices,
                        selected = c("Skuldir alls"),
                        multiple = FALSE,
                        selectize = FALSE
                    )
                ),
                column(
                    6,
                    selectInput(
                        inputId = NS(id, "nefnari"),
                        label = "Sem hlutfall af",
                        choices = nefnari_choices,
                        selected = c("Eignum alls"),
                        multiple = FALSE,
                        selectize = FALSE
                    )
                )
            ),
            selectInput(
                inputId = NS(id, "verdlag"),
                label = "Verðlag",
                choices = c("Verðlag hvers árs", "Núvirt"),
                selected = "Núvirt"
            ),
            div(
                actionButton(
                    inputId = NS(id, "goButton"),
                    label = "Sækja gögn",
                    width = "120px"
                ),
                class = "center", align = "middle"
            ),
            HTML(sidebar_info)
        ),
        
        mainPanel(
            plotlyOutput(
                NS(id, "throun_plot"),
                height = 700,
                width = "100%"
            ) |> 
                withSpinner()
            # downloadButton(
            #     outputId = NS(id, "download_plot"),
            #     label = "Sækja mynd"
            # )
        )
    )
}



#### SERVER ####

throun_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        
        throun_df <- reactive({
            throun_make_df(input)
        })
        
        throun_plot <- reactive({
            throun_df() |> 
                throun_make_ggplot(input)
        })
        
        output$throun_plot <- renderPlotly({
            throun_plot() |> 
                throun_make_plotly()
        }) |> 
            bindEvent(
                input$goButton, ignoreNULL = FALSE
            )
        
        output$download_plot <- downloadHandler(
            filename = function() {
                "myndrit.png"
            },
            content = function(file) {
                ggsave(
                    plot = throun_plot(),
                    filename = file,
                    width = 8, height = 0.5 * 8, scale = 1.3
                )
            }
        )
    })
    
}