# For saving me a bunch of time, a huge H/T: https://github.com/rstudio/shiny/issues/1058

    
## >>>>>> SERVER CODE <<<<<< --------
server <- function(input, output, session) {
    
    withMathJax()

    # HELPER: Get relevant file names, given selections ====
    fNames <- reactive({
        # Get relv regex for start
        regEx <- glue("cr - pc, {input$scNumPicker}")

        # Get relv regex for distro
        regEx_distro <- glue("\\[{input$subjDist},{100-input$subjDist}\\]")
        
        # Get relv regex for graph type
        sfx <- if_else(input$effectSel=="all", "\\]\\.png", glue("_{input$effectSel}\\.png"))

        # Pull imgs from file with this syntax
        files <- map(imgList, ~.x[grep(regEx, .x)] %>%
                                 .[grep(regEx_distro, .)] %>%
                                 .[grep(sfx, .)] %>%
                                 .[grep(glue("n={input$nObs}"), .)]) %>% # Sample size no longer hardcoded
                   unlist

        # Return
        files
    })

    
    # HELPER: form up the renderImage calls ====
    observe({
        lapply(seq_along(fNames()), function(i) {
          req(input$scNumPicker)
            
            selSc <- input$scNumPicker %>% as.numeric
            fontNm <- "sans"
            
          # Save temporary image with redone scenario labels
          tmpImg <- {
              # This is a super hacky way to do it, but not in the mood to argue
              # when it's only 1009-1012 that are the exception.
              if(selSc!=9 & selSc!=1009){
                  image_read(fNames()[i]) %>% 
                    # First scenario
                    ## Ensure previous number is completely hidden
                    image_annotate(scNumKey[scNumKey==selSc] %>% paste0(., " "),
                                   size=80, font = fontNm,
                                   color = "white", boxcolor = "white",
                                   location="+450+170"      
                    ) %>%
                    ## Add the new number
                    image_annotate(scNumKey[scNumKey==selSc] %>% names,
                                   size=55, font = fontNm,
                                   color = "black", boxcolor = "white",
                                   location="+480+170"         
                    ) %>%
    
                    # Second scenario
                    ## Ensure previous number is completely hidden
                    image_annotate(scNumKey[scNumKey==(selSc+1)] %>% paste0(., " "),
                                   size=80, font = fontNm,
                                   color = "white", boxcolor = "white",
                                   location="+450+470"     
                    ) %>%
                    ## Add the new number
                    image_annotate(scNumKey[scNumKey==(selSc+1)] %>% names, 
                                   size=55, font = fontNm, 
                                   color = "black", boxcolor = "white",
                                   location="+480+470"      
                    ) %>%
                  
                    # Third scenario 
                    ## Ensure previous number is completely hidden
                    image_annotate(scNumKey[scNumKey==(selSc+2)] %>% paste0(., " "),
                                   size=80, font = fontNm,
                                   color = "white", boxcolor = "white",
                                   location="+450+760"      
                    ) %>%
                    ## Add the new number
                    image_annotate(scNumKey[scNumKey==(selSc+2)] %>% names,
                                   size=55, font = fontNm, 
                                   color = "black", boxcolor = "white",
                                   location="+480+760"         
                    ) %>%
                    
                    # Fourth scenario
					## Ensure previous number is completely hidden
                    image_annotate(scNumKey[scNumKey==(selSc+3)] %>% paste0(., " "),
                                   size=80, font = fontNm,
                                   color = "white", boxcolor = "white",
                                   location="+450+1055"   
                    ) %>%
                    ## Add the new number
                    image_annotate(scNumKey[scNumKey==(selSc+3)] %>% names,
                                   size=55, font = fontNm, 
                                   color = "black", boxcolor = "white",
                                   location="+480+1055"         
                    ) %>%
                  # Save
                  image_write(tempfile(fileext='png'), format = 'png')
              } else {
                fNames()[i]  
              }
           }
              
          # Output everything
          output[[paste0("images", i)]] <- renderImage({
            return(list(
              src = tmpImg,
              width="900" 
            ))
          }, deleteFile = FALSE)
        })
    })


    # OUTPUT: create the image grid ====
    output$imgs <- renderUI({
        # Render everything
        lapply(seq_along(fNames()), function(i) {
            imageOutput(paste0("images", i), height="auto")
        })
    })

    # HELPER: build TeX expression for hazard ====
    hazEqs <- reactive({
        # Select scenario
        scElem <- scenList[[glue("scen{input$scGphPicker}")]]
        
        # Build string
        hazList <- 
            map(1:2, \(tr){
                # Get everything in easier-to-access names
                for(i in c("gammas", "lambdas", "cov", "tde")){
                    assign(i,
                           pluck(scElem, i, glue("tr{tr}")) 
                    )
                }
                
                # Linear combos
                for(i in c("cov", "tde")){
                    # Relv obj to extract from
                    coeffs <- i %>% get
                    
                    # Form up MJ expression
                    assign(glue("mj_{i}"), {
                        # Toss any elements equal to zero
                        coeffs %<>% .[.!=0]
                        
                        # Form up the pieces of the expression
                        pcs <- map2(coeffs, names(coeffs), ~glue("{.x}{.y}") %>% gsub("x", "x_", .))
                        
                        # Aggregate them all together
                        all <- paste(pcs, collapse=" + ") %>%
                                 gsub("\\+\\s*-", "-", .)
                        
                        # Return 
                        all
                    })
                }

                # Build this tr's h0
                h <- glue("h_{tr}(t) &= \\frac{{ {lambdas}*{gammas}*t^{{\\left({gammas} - 1 \\right)}} \\exp \\left(-{lambdas}*t^{{ {gammas} }} \\right) }}",
                           "{{ \\left(-{lambdas}*t^{{ {gammas} }} \\right) }}",
                            # Add the covariates
                          "&* \\exp \\left( {mj_cov} HIPPOS \\right)")
                h <- {
                    if(mj_tde=='')  gsub("HIPPOS", "", h)
                    else            gsub("HIPPOS", glue("+ \\\\ln(t) *\\\\left[ {mj_tde} \\\\right]"), h)
                }
                
                
                # Return
                h
            })
        
        # Put into full equation form
        finOut <- 
            glue("\\( 
            \\begin{{array}}{{lll}}
                 {hazList[[1]]} \\\\\\\\\\
                 {hazList[[2]]}
                 \\end{{array}}
            \\)")
        
        # Return
        finOut
    })
        
    # OUTPUT: hazard equations ====
    output$hazEqs <- renderUI({
        withMathJax(hazEqs())
    })

    # OUTPUT: display hazard for a particular covariate profile ====
    output$hazGph <- renderPlot({
        # Select scenario
        scElem <- scenList[[glue("scen{input$scGphPicker}")]]

        # Paper's numbering?
        scPprNum <- names(scNumKey)[scNumKey==as.numeric(input$scGphPicker)]
        
        # Start doing graph
        for(eq in c("main", "tde")){
            innerElem <- if_else(eq=="main", "cov", "tde")
            for(tr in 1:2){
                assign(glue("{eq}.tr{tr}"),
                       (pluck(scElem, innerElem, glue("tr{tr}")) %>% unlist %>% as.matrix %>% t) %*%
                        c(input$gph_x1Val,input$gph_x2Val) %>% as.matrix
                )
            }
        }
        theHaz <- \(tVec, shape, scale, tde, main){
            # assumes weibull
            h0 <- scale*shape*tVec^(shape-1) / exp(-scale*tVec^shape)   # p. 675 of Crowther and Lambert 2012 (SJ)
            h <- h0 * exp(as.numeric(main) + as.numeric(tde)*log(tVec)) # ASSUMES g(t) = ln  (with the as.numeric()s to get rid of R's "recycling array of length 1" warning)
            h
        }
       
        tVec <- seq(0.1, 10, by=0.1)
        data.frame(t = tVec) %>%
            mutate(haz1 = theHaz(t, 
                                shape = pluck(scElem, "gammas" , "tr1") %>% as.numeric,
                                scale = pluck(scElem, "lambdas", "tr1") %>% as.numeric,
                                main  = .env$main.tr1,
                                tde   = .env$tde.tr1),
                   haz2 = theHaz(t, 
                                shape = pluck(scElem, "gammas" , "tr2") %>% as.numeric,
                                scale = pluck(scElem, "lambdas", "tr2") %>% as.numeric,
                                main  = .env$main.tr2,
                                tde   = .env$tde.tr2)) %>%
            pivot_longer(   # to make graph easier to format
                starts_with("haz"),
                names_prefix = "haz",
                names_to = "Transition",
                values_to = "haz"
            ) %>%
            
            # THE GRAPH
            ggplot(aes(x=t, y=haz, linetype=Transition, color=Transition)) +
              geom_line(linewidth=0.75) +
              
              xlab("<em>t</em>") +
              ylab("<em>h</em>(<em>t</em>)") +
              
              labs(title    = glue("Scenario {scPprNum}"),
                   caption  = glue("<em>x</em><sub>1</sub> = {input$gph_x1Val}, <em>x</em><sub>2</sub> = {input$gph_x2Val}")# ,
              ) +
        
            theme_bw() +
        
            theme(
                # Legend 
                legend.justification = "top",

                # Center main gph title (+ render HTML correctly)
                plot.title = element_markdown(hjust = 0.5, face="bold"),

                # Render HTML in other labels correctly
                plot.caption = element_markdown(hjust=0, 
                                                margin=margin(t=-8, l=-19)),    # shrink gap btwn x-axis and caption
                axis.title.x = element_markdown(),
                axis.title.y = element_markdown()
            )
    })
    
    # MISC: populate the scenario picker dropdown ====
    scRangeList <- reactive({
        subset <- scNumKey[scNumKey %% 4==1]
        subset <- subset[subset<1000 | subset %in% c(1009:1012)]
        
        # Form up nicer labels
        labs <- names(subset)
        newNms <- map_chr(labs, ~glue("Scenarios {.x}-{as.numeric(.x)+3}"))
        
        # Return
        names(subset) <- newNms
        subset
    })
    
    observe({
        updateSelectInput(session, "scNumPicker",
                          choices = scRangeList(),  
                          selected = scRangeList()[1]
                         )
    })
    
    # MISC: update the specific scenario picker dropdown on gph page
    observe({
        req(input$scNumPicker)
                
        # Form up the new obj
        val <- input$scNumPicker %>% as.numeric
        
        # Generate the relevant choices
        scChoices <- scNumKey[scNumKey %in% c(val:(val+3))]
        labs <- names(scChoices)
        names(scChoices) <- map_chr(labs, ~glue("Scen. {.x}"))
        
        # Extract paper names, return
        updateSelectInput(session, "scGphPicker",
                          choices = scChoices,  
                          selected = scChoices[1]
                         )
        
    })
}
