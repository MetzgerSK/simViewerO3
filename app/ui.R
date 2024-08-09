## >>>>>> UI CODE <<<<<< --------
ui <- fluidPage(

    # CSS
    tags$head(
        tags$style(HTML("
        /* Much nicer way of coding the div-in-div trick  */
        .inline-block-center {
        	text-align: center;
        }
        .inline-block-center div {
        	display: inline-block;
        	text-align: center;
        }
        
        /* Padding to top of tab body */
        .tab-pane{
            margin-top:10px;
        }
    "))),
    
    # Enable
    withMathJax(),
    useShinyjs(),
    
    # Title
    titlePanel("Simulation Graphs"),

    # Second row (selection info - scenario #) ====
    fluidRow(
        id = "row2_scNum",
        column(1),
        column(4, 
            fixedRow(
                selectInput(
                    "scNumPicker",
                    label = HTML("Set of Scenarios?"),
                    choices = NULL,
                    selected = NULL
                )
            ),
            fixedRow(
                selectInput(
                    "nObs",
                    label="\\(n\\)?",
                    choices = c(100, 2000),
                    selected = 2000
                )
            )
        ),
        column(3, style="text-align:center;", class="inline-block-center",
            sliderInput(
                inputId = "subjDist",
                label = "Distrib. of subjects across transitions?",
                min=10,
                max=90,
                step=20,
                value=50
            ),
            br(),"(% of subjs. in tr1's starting riskset)"
        ),
        column(4,
            # Which graphs to view?
            radioGroupButtons(
                inputId = "effectSel",
                label = "Graph of Interest?",
                choices = c("All"="all", "Main"="main", "TVEs"="TVC"),
                selected = "all",
                checkIcon = list(yes = icon("ok",
                                            lib = "glyphicon")),
                justified = TRUE,
                status = "primary"
            )
        )
    ),
    
    fluidRow(style="margin-top:10px;"), # To put some spacing between sel widget row and start of tabs
    
    # Body (graphs et al.) ==== 
    tabsetPanel(
        tabPanel("Simulation Results",
            # (also add pop-up w/reminder as to what each ordering corresponds to)
            div(class="inline-block-center",
                div(actionButton("modalOrd_btn", "Ordering Key", icon=icon("info-circle"), class="btn-primary")),
				HTML("<p style='margin-top:15px;'>Note: the images can sometimes take 10&ndash;20 seconds to appear.</p>"),
                br(),
                uiOutput("imgs")       
            )
        ),
        tabPanel("True DGPs", 
            fluidRow(
                column(2,
                    selectInput(
                        "scGphPicker",
                        label = HTML("Specific Scenario?"),
                        choices = NULL,
                        selected = NULL
                    )
                ),
                column(2,
                    numericInput(
                        "gph_x1Val",
                        label = "\\(x_1\\)'s value?",
                        0,
                        step=0.1
                    ),
                    numericInput(
                        "gph_x2Val",
                        label = "\\(x_2\\)'s value?",
                        0,
                        step=0.1
                    )
                ),
                column(2,
                    uiOutput("hazEqs")
                ),
                
                column(6)   # for some empty space on the right
            ),
            plotOutput("hazGph")
        )
    ),
    

    bsModal(id="modalOrd", trigger="modalOrd_btn",
        h3("Ordering Key"),
        HTML("<ul>",
            "<li>Ordering A: \\(h_0(t)\\), \\(\\hat{\\beta}\\), PH</li>",
            "<li>Ordering B: \\(h_0(t)\\), PH, \\(\\hat{\\beta}\\)</li>",
            "<li>Ordering C: \\(\\hat{\\beta}\\), \\(h_0(t)\\), PH</li>",
            "<li>Ordering D: \\(\\hat{\\beta}\\), PH, \\(h_0(t)\\)</li>",
            "<li>Ordering E: PH, \\(h_0(t)\\), \\(\\hat{\\beta}\\)</li>",
            "<li>Ordering F: PH, \\(\\hat{\\beta}\\), \\(h_0(t)\\)</li>",
            "</ul>"
        )
    )

)