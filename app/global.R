library(shiny)
library(shinyjs)
library(magrittr)
library(shinyWidgets)
library(stringr)
library(purrr)
library(glue)
library(dplyr)
library(shinyBS)
library(magick)

# for haz gphs
library(ggplot2)
library(ggtext)
library(tidyr)

#***********************************************************
# Source in any functions
source("functs.R", local=TRUE)

# Read in scenario list + parse
scenDef.loc <- "common functions - scenario defs.do" # path for scenario definition file, relative to app 
scenDoFile <- readLines(scenDef.loc)
    srt <- grep("#delimit ;", scenDoFile)
    end <- grep("#delimit cr", scenDoFile)
    
    # Get elements isolated
    extr <- map2(srt,end, ~scenDoFile[(.x+1):(.y-1)]) %>%
                # +1: start reading in the line after #delimit ;
                # -1: stop reading at the (end of the) line before #delimit cr
              map(., ~str_replace(.x, "// NOTE: hardcoded tr2 shape!", ""))
    
    # Process into list
    ## HARDCODED FOR TWO TRANSITIONS
    scenList_core <- parseScens(extr)
    
    # Expand for sc 1000s
    ## Scs. < 1000  => all shapes = 1.25.
    scenListTmp1 <- 
        map(scenList_core, 
            function(x){
                gam <- pluck(x, "gammas") 
                gam[["tr1"]] <- {
                    if(is.character(gam[["tr1"]]))  "1.25"
                    else                            gam[["tr1"]]
                }
                gam[["tr2"]] <- {
                    if(is.character(gam[["tr2"]]))  "1.25"
                    else                            gam[["tr2"]]
                }
                list_modify(x, !!!list(gammas=gam))
            }    
        ) 
    
    ## Scs. > 1000 => first shape = 0.9, second = 1.25
    gammaRepl <- list("gammas" = list("tr1"= 0.9, "tr2"=1.25))
    scenListTmp2 <-
        map(scenList_core, 
            function(x){
                gam <- pluck(x, "gammas") 
                gam[["tr1"]] <- {
                    if(is.character(gam[["tr1"]]))  "0.9"
                    else                            gam[["tr1"]]
                }
                gam[["tr2"]] <- {
                    if(is.character(gam[["tr2"]]))  "1.25"
                    else                            gam[["tr2"]]
                }
                list_modify(x, !!!list(gammas=gam))
            }    
        )
    names(scenListTmp2) <- map_chr(1:length(scenList_core),
                                   ~glue("scen{.x+1000}"))
    
    # Combine
    scenList <- c(scenListTmp1, scenListTmp2)
    rm(srt, end, scenListTmp1, scenListTmp2, gammaRepl)
    

# Store list of scenario numbers in paper -> actual scenario numbers
## Do quick in d.fr., then store as vector
scCorresp <-
    tibble(sc = c(9:12, 17:20, 25:32)) %>%
      rbind(., mutate(., sc = sc + 1000)) %>% 
      mutate(baseSc = if_else(sc>=1000, sc-1000, sc),
             scPpr = case_when(
                baseSc %in% c(9:12)     ~ baseSc,
                baseSc %in% c(17:20)    ~ baseSc - 16,
                baseSc %in% c(21:24)    ~ baseSc - 20,
                baseSc %in% c(25:28)    ~ baseSc - 20,
                baseSc %in% c(29:32)    ~ baseSc - 16,
                TRUE                    ~ NA),
             scPpr = if_else(sc>=1000, scPpr+1000, scPpr)
      ) %>%
      arrange(scPpr)
                    
scNumKey <- pull(scCorresp, sc)
names(scNumKey) <- pull(scCorresp, scPpr) %>% as.character
    
# Dir where graphs are located, relative to app folder + getting list of imgs
## Ordering A/B
dir_AB <- "../graphs/A-B"
imgList_AB <-  list.files(dir_AB, pattern="cr - pc",
                       full.names=TRUE) %>%
                .[grep("png$",.)]             # PNGs only

dir_CD <- "../graphs/C-D"
imgList_CD <-  list.files(dir_CD, pattern="cr - pc",
                       full.names=TRUE) %>%
                .[grep("png$",.)]             # PNGs only

dir_EF <- "../graphs/E-F"
imgList_EF <-  list.files(dir_EF, pattern="cr - pc",
                       full.names=TRUE) %>%
                .[grep("png$",.)]             # PNGs only

imgList <- list("AB" = c(imgList_AB),
                "CD" = c(imgList_CD),
                "EF" = c(imgList_EF))