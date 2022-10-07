#### UI ####

skattbyrdi_ui <- function(id) {
    sidebarLayout(
        sidebarPanel(
            width = 3,
            sliderInput(
                inputId = NS(id, "ar_fra"),
                label = "Upphafsár samanburðar",
                step = 1,
                sep = "",
                value = min(skattbyrdi$ar),
                min = min(skattbyrdi$ar),
                max = max(skattbyrdi$ar)
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
            tabsetPanel(
                tabPanel(
                    "Mynd",
                    br(),
                    plotlyOutput(NS(id, "skattbyrdi_plot"), height = 700, width = "100%") |> withSpinner()  
                ),
                tabPanel(
                    "Tafla",
                    br(),
                    dataTableOutput(NS(id, "skattbyrdi_table"))
                )
            )
        )
    )
}



#### SERVER ####

skattbyrdi_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        
        skattbyrdi_df <- reactive({
            skattbyrdi_make_df(input)
            
        })
        
        skattbyrdi_plot <- reactive({
            skattbyrdi_df() |> 
                skattbyrdi_make_ggplot(input)
        })
        
        output$skattbyrdi_plot <- renderPlotly({
            skattbyrdi_plot() |> 
                skattbyrdi_make_plotly()
        }) |> 
            bindCache(
                input$ar_fra, input$index_type
            ) |> 
            bindEvent(
                input$goButton, ignoreNULL = FALSE
            )
        
        skattbyrdi_table <- reactive({
            skattbyrdi_make_df(input) |> 
                skattbyrdi_make_table(input)
        }) 
        
        output$skattbyrdi_table <- renderDataTable({
            
            caption = skattbyrdi_table_caption(input)
            
            datatable(
                skattbyrdi_table(),
                extensions = "Buttons",
                rownames = FALSE,
                caption = htmltools::tags$caption(
                    style = "caption-side: top",
                    h4(caption)
                ),
                options = list(
                    dom = "fBrtip",
                    buttons = c("csv", "excel", "pdf"),
                    pageLength = 68,
                    lengthChange = FALSE,
                    searching = TRUE,
                    autoWidth = FALSE,
                    captionSide = "top",
                    ordering = FALSE,
                    language = list(
                        decimal = ",",
                        thousands = ".",
                        url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Icelandic.json'
                    )
                )
            )
        }) |> 
            bindEvent(
                input$goButton, ignoreNULL = FALSE
            )
    })
}