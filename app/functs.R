parseScens <- function(inpt){
# INPUT: huge block of text (i.e., the do file's contents)
# OUTPUT: nested list with depth 3
#   TOP LEVEL: the scenario         [list]
#   2nd LEVEL: survsim parameters   [list]
#   3rd LEVEL: parameter values     [either list or vector]
 

    outpt <-
        map(inpt, function(x){
            # Flatten and squish
            x.fs <- unlist(x) %>% str_flatten %>% str_squish
            
            # Parse on simsurv arguments
                ## find where equals sign is, split only after that
                eq.loc <- str_locate_all(x.fs, "=")[[1]][1,1]
                rhs <- str_sub(x.fs, eq.loc+1) %>%
                         ## TOSS QUOTES + ;
                         str_remove_all('[";]') %>%
                         ## TRIM
                         str_trim
            argms <- str_split(rhs,
                               "\\) ")[[1]] %>%
                       map_chr(., ~glue("{.x})")) %>%
                       map(., ~gsub("\\)\\)", "\\)",.x))
            
            # Get names for second level names
            nms.lv2 <- str_match_all(rhs, "([a-z0-9]+)\\(")[[1]][,2]
        
            # Get arguments for third level
            argmVals <- str_extract_all(argms, "(?<=\\().*(?=\\))") %>%
                          set_names(nms.lv2)
            
            # If there's no x2 in the tde, add it w/0 0s
            if(!grepl("x2", argmVals$tde)){
                argmVals$tde <- glue("{argmVals$tde} x2 0 0")
            }

            # For cov and tde, need to drill down AGAIN.
            for(i in c("cov","tde")){
                obj <- NULL
                # If this option exists, split it
                if(pluck(argmVals,i) %>% is.null %>% not){                    
                    temp <- pluck(argmVals, i) %>% 
                      strsplit(" ") %>% 
                      .[[1]] %>% 
                      matrix(., nrow=2, ncol=3, byrow=TRUE)
                    
                    # Automate manually, for sanity
                    obj[["tr1"]] <-
                        temp[,2] %>% as.numeric %>% as.list %>% set_names(temp[,1])
                    
                    obj[["tr2"]] <-
                        temp[,3] %>% as.numeric %>% as.list %>% set_names(temp[,1])
                    
                    # Store back in obj of interest
                    argmVals[[i]] <- obj
                }
            }

            # For anything else that isn't cov/tde, just apply nice labels for tr1, tr2
            TEMP <- map(argmVals[(!grepl("cov|tde", nms.lv2))], 
                        ~strsplit(.x, " ") %>% 
                           .[[1]] %>% 
                           as.list %>% 
                           set_names(c("tr1","tr2"))
                    )
            
            argmVals <- list_modify(argmVals, !!!TEMP) # will swap in the new contents of lambdas/gammas without modifying the rest
        
        }) %>%
          # Store all numbers as numeric
          type.convert(as.is=TRUE)
    
    # Get all lv. 1 names (easier to do here than inside the map())
    nms.lv1 <- map_chr(inpt, ~str_match_all(.x, "global (scen[0-9]+)")[[1]][,2])
    names(outpt) <- nms.lv1
    
    # Return
    outpt
}