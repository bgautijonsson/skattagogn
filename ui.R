ui <- shinyUI(
    tagList(
        tags$head(
            tags$style(
                type = "text/css",
                "
                .navbar-brand {
                    display: none;
                }
                .navbar {
                        font-family: Optima;
                        font-weight: 300;
                        font-size: 20px;
                        padding-top: 5px;
                        padding-bottom: 5px;
                }
                "
            )
        ),
        navbarPage(
            title = "Skattagögn Hagstofunnar",
            theme = bs_global_get(),
            
            
            #### TÍUNDAMÖRK ####
            tabPanel(
                title = "Tíundir",
                tiundamork_ui("tiundamork")
            ),
            
            #### ALDURSHÓPAR ####
            tabPanel(
                title = "Aldurshópar",
                aldurshopar_ui("aldurshopar")
            ),
            
            #### SKULTAHLUTFALL ####
            tabPanel(
                title = "Skuldahlutfall",
                skuldahlutfall_ui("skuldahlutfall")
            ),
            
            #### SKATTBYRÐI ####
            tabPanel(
                title = "Skattbyrði",
                skattbyrdi_ui("skattbyrdi")
            ),
            
            #### ÞRÓUN ####
            tabPanel(
                title = "Hlutföll",
                throun_ui("throun")
            )
            
        )
    )
)
