# Modified Shinyswipr modules & JSS


# Ettore Settanni 
# (ettore.settanni@gmail.com)
# 18-28 09 2021

# Motivation
# I noticed that Shiny apps built with the original shinyswipr package (https://github.com/nstrayer/shinyswipr) won't
# work on devices that enable both touch and mouse input simultanously (e.g. touchscreen laptop)

# Contribution
# A modified version of shinyswipr's "quote swipr" example described in https://livefreeordichotomize.com/2017/03/12/introducing-shinyswipr-swipe-your-way-to-a-great-shiny-ui/
# The modified app displays paired-up randomly generated quotes and images on a card deck
# However, I change the inner workings of shinyswipr as follows
# --- Hammer.js (replace touchSwipe.js)
# --- Own .js and .css governing the card swipe animation (replace shinySwiper.js and swiprStyle.css) 

# Caveat
# In findings my way around shinyswipr's ineer workings I've realised that the following major updates have occurred
# --- updated approach to writing modules server functions in shiny, see Ch. 19 in https://mastering-shiny.org/
# --- communicating from JavaSript back to SHiny: using Shiny.setInputValue instead of Shiny.onInputChange https://shiny.rstudio.com/articles/communicating-with-js.html
# However, after fiddling a bit with these updates - without much success - I've decided to stick with the "old" approach to calling a module's server; similarly I didn't


#### Part 0: Load packages ####
library(shiny)
library("googlesheets4")

# for graphs
rm(list = ls())
gc()
library(igraph)
library(networkD3)
library(plotly)
library(shiny)
library(dplyr)

# source("prepare_d3.R")

#### Global parameters for saving results into local csv and google sheet ####
## Set to FALSE to disable any of them
SAVE_LOCAL_CSV <- TRUE
SAVE_GOOGLESHEET <- TRUE
url_sheet = "https://docs.google.com/spreadsheets/d/1n7NQtTZmQUFKOFH5XeU_QalZmk6Nh8fUAPk91I3IRUU/edit#gid=0" # Xiang's
#url_sheet ="https://docs.google.com/spreadsheets/d/1bDBD1h8UhP-71maeklR4bMoRwLW_zASp8CnW8DxZqqI/edit#gid=0" # Ettore's

## Set local csv results to be saved into

if (SAVE_LOCAL_CSV==TRUE){
  local_results_csv = paste("data/results_",format(Sys.time(), "%Y%m%d%H%M"),".csv",sep="")
}
## Handles access to Google Sheets for deployment 
## Checks if token is available in ".secrets/" folder
## If it exists, then directly access sheet without prompting Developer
## Otherwise, prompt Developer for first login (needs to be done once by Developer when deploying)


if (SAVE_GOOGLESHEET==TRUE){
  has_secret_token <- if (identical(list.files(".secrets/"), character(0))) FALSE else TRUE
  
  if (has_secret_token == TRUE){
    print("SECRET TOKEN FOUND...LOGIN WITHOUT PROMPT")
    options(
      gargle_oauth_cache = ".secrets",
      gargle_oauth_email = TRUE
    )
  } else if (has_secret_token == FALSE){
    print("SECRET TOKEN NOT FOUND...PROMPTING FOR LOGIN")
    gs4_auth(cache = ".secrets", email = "bxy20@cam.ac.uk")
  }
  
}

#### Part 1: Module UI for swipe card deck ####
Module_swipeCard_UI <- function(id, pass_img1, pass_img2, pass_h3, pass_p1, pass_p2){
  ns <- NS(id)
  tagList(                                      # a special type of layout function that allows you to bundle together multiple components without actually implying how they will be laid. Ch.19 https://mastering-shiny.org/scaling-modules.html
    tags$head(                                  # load our www/myjavascrip.js and www/mycss.css files, which are saved under a www subfolder.
      tags$script(src = "hammer-min.js"),       # Call a third-party script that deals with mouse and touch events
      tags$script(src = "test-V5.js"),          # My customised script that governs the swipe detection and card animation on swipe
      ## TO DO: css with buttons to replace swipes if necessary (Ettore has a work in progress)
      tags$link(rel = "stylesheet", type = "text/css", href = "test-V5-2images.css")  # My customised card style (without buttons)
    ),
    tags$body(
      # create dependency: deck, card, card elements
      tags$div(id = id, class = "card-deck",
               tags$div(class = "card",
                        ## Variable card contents: text and images
                        tags$div(class = "row",
                                 tags$h3("pair no.:"),
                                 tags$h3(pass_h3),                               
                        ),
                        ## The variable text and images are better placed in divs
                        ## place images from URL next to each other - see also CSS
                        tags$div(class = "row",
                                 tags$div(class = "randomImage", pass_img1),
                                 tags$div(class = "randomImage", pass_img2),
                                 
                        ),
                        ## place labels (image descriptions) next to each other
                        tags$div(class = "textwrapper",
                                 tags$div(class = "randomText1", pass_p1),
                                 tags$div(class = "randomText2", pass_p2),
                        ),
                        ## fixed card content: describe directions of swipe to the user
                        tags$div(class="row",
                                 tags$h3(),
                                 p(icon("arrow-up")),
                                 p("mutual influence"),
                                 p(icon("arrow-left"),
                                   HTML("does NOT influence &#160 &#160 &#9679 &#160 &#160 &#160 &#160 &#160 INFLUENCES"),
                                   icon("arrow-right"),
                                 ),
                                 p("no influence either way"),
                                 p(icon("arrow-down")),
                        ),
               )
      )
    )
  ) #end tag list.
}

#### Part 2: Module server for swipe card deck  ####

# There is a more up-to-date approach to modules in Shiny (see Ch. 19 in https://mastering-shiny.org/) but I stick to the original
Module_swipeCard_serverOLD <- function(input, output, session){
  card_id <- gsub("-", "", session$ns("")) 
  # Communication from Shiny to JS - Tutorial: https://shiny.rstudio.com/articles/communicating-with-js.html
  # step 1: send messages through a method on the session object. 
  # Notice that 
  # -- "observe" continually monitors any changes in all reactive values within its environment
  # -- the type is a string that helps identify the correct JavaScript code to invoke
  observe({ 
    session$sendCustomMessage(type = "initializeCard", message = card_id)                                      # in JavaScript lookup for: Shiny.addCustomMessageHandler("initializeCard", ...). Here the "message" provides the argument for a function that triggers the Tinder-like carousel swipe
  })
  # step 2: JS to Shiny
  # In Javascript we caused the module server's input "cardSwiped" to be set to whatever direction a card has been swiped
  # Notice that: 
  # -- the (module) server input$cardSwiped CARRIES the final swipe decision (e.g. left, right...)
  # -- Yet Java Script will return a combination of swipe direction and a "nonce" (in my revised JavaScript code: a random number).
  # -- Without a nonce, "observe" won't perceive any change in the environment - see https://shiny.rstudio.com/articles/js-send-message.html
  # In the below, the original shinysipr code uses the input value returned by the card swipe JavaScript as part of a reactive expression.
  # But first it strips the input of the nonce
  swipe_result <- reactive({
    if(is.null(input$cardSwiped)){
      input$cardSwiped
    } else {
      strsplit(input$cardSwiped, "-")[[1]][2]
    }
  })
  return(swipe_result)
}




#### Part 3: APP ui and server####
ui <- fluidPage(
  # Below I introduce a multipage layout so that the swipe log is shown in a different tab, rather than in the same page as the card
  tabsetPanel(id = "tabSwitch",
    tabPanel("Deck",
             Module_swipeCard_UI("my_tinderLike_swiper",
                                 # These are the FIXED contents passed on to the card div
                                 htmlOutput("url_1"),                                                            # This trick helps accommodate randomly generated images retrieved from the web
                                 htmlOutput("url_2"),
                                 # show the progressive edge number as card ID,
                                 textOutput("arc_ID"),
                                 textOutput("quote_1"),                                                           # the random text will be accommodated in a separate div
                                 textOutput("quote_2")
                                 
             ),
    ),
    tabPanel("Swipes log",
             tableOutput("resultsTable"),
             style = "height:600px;overflow-y: scroll;"
    ),
    tabPanel("skip list",
             textOutput("check_ID")
    ),
    tabPanel("Graph",
             fluidRow(
               checkboxGroupInput(inputId = "checkGroup", 
                                  label = h3("Select category"),
                                  # choices =  unique(nodes_D3$group),
                                  # selected = unique(nodes_D3$group)
                                  # choices =  c("Strategic.Design"),
                                  # selected = c("Strategic.Design")
                                  choices =  c("Flexible and automated production"),
                                  selected = c("Flexible and automated production")
               )
             ),
             fluidRow(
               checkboxGroupInput(inputId = "SubOption",
                                  label = h3("Choose type"),
                                  # choices =  unique(nodes_D3$type),
                                  # selected = unique(nodes_D3$type)
                                  # choices =  c("Flexible and automated production"),
                                  # selected = c("Flexible and automated production")
                                  choices =  c("Strategic.Design"),
                                  selected = c("Strategic.Design")
               )
             ),
             fluidRow(
               column(width = 12, 
                      h3('Interdependencies graph'),  
                      forceNetworkOutput(outputId = "force", height = "800px")
               )
             )
    )
  )
)

server <- function(input, output, session){
  # This approach to invoking a module's server is obsolete but I stick to it
  # for an update see see Ch. 19 in https://mastering-shiny.org/
  card_swipe <- callModule(Module_swipeCard_serverOLD, "my_tinderLike_swiper")
  
  
  ## New: Read node list and genereate edge list (all possible edges between nodes except self loops)
  nodes <- read.csv("data/test_data_20220321.csv")
  ## Option 1 - Xiang's suggestion - really cool but works better for generating the edge list of an undirected graphs 
  # edges <- as.data.frame(t(combn(nodes$node_ID,2)))                                       
  # colnames(edges) <- c("source", "target")
  ## Option 2 expand.grid
  edges1 <- expand.grid(nodes$node_ID, nodes$node_ID)        # work out all the possible edges
  edges1 <- edges1[,c(2,1)]                                  # swap column order
  edges2 <- apply(edges1,1,function(x){
    ## remove self loops
    if(x[1] != x[2]){
      ## retrieve card contents (description, image url) from the nodes involved in each edge (Card)
      lookup_vector <- nodes[,"node_ID"]
      idx_source <- match(x[1], lookup_vector)
      idx_target <- match(x[2], lookup_vector)
      c(x,nodes[idx_source ,"node_label"],nodes[idx_target ,"node_label"],nodes[idx_source ,"node_url"],nodes[idx_target ,"node_url"])
    }
  })           
  edges3 <- edges2[!unlist(lapply(edges2, is.null))]          # pick non-null values from list. Thread: https://stackoverflow.com/questions/4227223/convert-a-list-to-a-data-frame
  edges <- do.call(rbind.data.frame, edges3)                  # list to dataframe. Thread: https://stackoverflow.com/questions/4227223/convert-a-list-to-a-data-frame
  colnames(edges) <- c("Source", "Target","Source_label","Target_label","Source_URL","Target_URL")
  ## Add arc ID column
  cards_content <- cbind(1:nrow(edges),edges)
  colnames(cards_content) <- c("arc_id",colnames(edges))
  
  ## empty list of cards to skip
  skip_card_list <- reactiveValues()
  list_idx <- reactiveValues(counter = 0L)
  
  ## For debug: read just few entries
  # cards_content <- cards_content[1:4,]
  
  ## count the total n. of edges  
  n_edges_all <- nrow(cards_content)
  cards_ID_list <- as.vector(cards_content[,1])
  ## initiate sequential edge counter (card pair) 
  current_card_ID <- 1
  
  ## Internal counter (reactive): thread https://stackoverflow.com/questions/33671915/r-shiny-server-how-to-keep-variable-value-in-observeevent-function
  v <- reactiveValues(counter = 1L)
  output$n_swipes <- renderPrint({
    print(v$counter)
  }) 
  
  current_card_content <- cards_content[current_card_ID,]         # select current card contents
  our_arc_ID <- as.character(current_card_content$arc_id)
  our_quote_1 <- as.character(current_card_content$Source_label)
  our_quote_2 <- as.character(current_card_content$Target_label)
  our_img_1 <- as.character(paste0("<img src='", current_card_content$Source_URL, "'>"))
  our_img_2 <- as.character(paste0("<img src='", current_card_content$Target_URL, "'>"))
  
  # Render each "pair"'s features 
  output$arc_ID <- renderText({our_arc_ID}) 
  output$quote_1 <- renderText({ strtrim(our_quote_1, 150) })
  output$quote_2 <- renderText({ strtrim(our_quote_2, 150) })
  output$url_1 <- renderText({our_img_1})
  output$url_2 <- renderText({our_img_2})
  output$resultsTable <- renderDataTable({appVals$swipes})
  
  appVals <- reactiveValues(
    current_card_content = current_card_content,
    swipes = data.frame(pair_ID = character(),
                        source_var = character(),
                        target_var = character(),
                        swipe = character(),
                        score = numeric()
    )
  )
  
  
  observeEvent( card_swipe(), {
    if (current_card_ID <= n_edges_all & v$counter <= n_edges_all){
      #Record last swipe result
      new_swipe_result = data.frame(pair_ID = appVals$current_card_content$arc_id, 
                                    source_var = appVals$current_card_content$Source_label,
                                    target_var = appVals$current_card_content$Target_label,
                                    swipe = card_swipe())
      ## For swipe log only add binary value
      if ((new_swipe_result$swipe == "right")  | (new_swipe_result$swipe == "up")){
        new_swipe_result$score = 1
      } else {
        if ((new_swipe_result$swipe == "left")  | (new_swipe_result$swipe == "down")){
          new_swipe_result$score = 0
        }
      }
      ## special cases swipe up and swipe down: two records
      if ((new_swipe_result$swipe == "up") | (new_swipe_result$swipe == "down")){
        new_swipe_result2 <- new_swipe_result
        new_swipe_result2$source_var <- new_swipe_result$target_var
        new_swipe_result2$target_var <- new_swipe_result$source_var
        
        ## fetch card id
        double_card_idx <- which(cards_content[,"Source_label"] == new_swipe_result2$source_var & cards_content[,"Target_label"] == new_swipe_result2$target_var )
        new_swipe_result2$pair_ID <- cards_content[double_card_idx,"arc_id"]
        
        
        ## TO DO: check if the card to skip has already been swiped
        if (!(new_swipe_result2$pair_ID %in% appVals$swipes$pair_ID)){
          ## Update list of cards to skip (equivalent to removing card from deck)
          ## TO DO:  allow extra skips using e.g. transitivity (if A influences B, and B influences C, there is no need to evaluate whether A influences C)
          list_idx$counter <- list_idx$counter + 1 
          skip_card_list$dList <- c(isolate(skip_card_list$dList), double_card_idx)       # thread: https://stackoverflow.com/questions/23874674/add-to-a-list-in-shiny
          #skip_card_list[[list_idx$counter]] <- double_card_idx
          output$check_ID <- renderPrint({
            print(isolate(skip_card_list$dList))
            #print(list_idx$counter)
          }) 
          
          
          ## generate two logs with one swipe
          new_swipe_result <- rbind(new_swipe_result2, new_swipe_result)
        }
      }
      appVals$swipes <- rbind(
        new_swipe_result, 
        appVals$swipes
      )
      
      
      ## save data into Google sheet
      if (SAVE_GOOGLESHEET==TRUE){
        sheet_append(url_sheet, data = new_swipe_result, sheet = 1)
      }
      
      ## save data into local csv
      if (SAVE_LOCAL_CSV==TRUE){
        write.table(new_swipe_result,  
                    file=local_results_csv, 
                    append = T, 
                    sep=',', 
                    row.names=F, 
                    col.names=F)
      }
      
      ## send results to the output.
      output$resultsTable <- renderTable({appVals$swipes})
      
      ## update the pair on the card (determine next edge)
      current_card_ID <- appVals$current_card_content$arc_id + 1
      v$counter <- v$counter + 1
      
      ## check card is not on skip list: PLEASE TEST THIS LOOP I HOPE IT DOESN'T MESS UP
      aux_count <- 1
      while((current_card_ID %in% isolate(skip_card_list$dList)) & aux_count <= n_edges_all ){
        current_card_ID <- current_card_ID + 1
        v$counter <- v$counter + 1
      }
      appVals$current_card_content <- cards_content[current_card_ID,]         # select
      
      ## Internal counter thread: https://stackoverflow.com/questions/33671915/r-shiny-server-how-to-keep-variable-value-in-observeevent-function
      output$n_swipes <- renderPrint({
        print(v$counter)
      }) 
      
      ## Handle card swipe updates
      ## Updates to the next card
      if (v$counter <= n_edges_all){
        ## send update to the ui.
        our_arc_ID <- as.character(appVals$current_card_content$arc_id)
        our_quote_1 <- as.character(appVals$current_card_content$Source_label)
        our_quote_2 <- as.character(appVals$current_card_content$Target_label)
        our_img_1 <- as.character(paste0("<img src='", appVals$current_card_content$Source_URL, "'>"))
        our_img_2 <- as.character(paste0("<img src='", appVals$current_card_content$Target_URL, "'>"))
        
        ## Render the "pair"'s features 
        output$arc_ID <- renderText({our_arc_ID}) 
        output$quote_1 <- renderText({ strtrim(our_quote_1, 150) })
        output$quote_2 <- renderText({ strtrim(our_quote_2, 150) })
        output$url_1 <- renderText({our_img_1})
        output$url_2 <- renderText({our_img_2})
        
        ## Handling the final swipe
      }else{
        print("FINISH SWIPING ALL...")
        ## Renders the card empty
        output$arc_ID <- renderText("")
        output$quote_1 <- renderText("")
        output$quote_2 <- renderText("")
        output$url_1 <- renderText("")
        output$url_2 <-renderText("")
        
        ## subject to change
        ## remove whole card 
        removeUI(
          selector = "#my_tinderLike_swiper"
        )
      }
    } 
  }) #close event observe.
  
  # ================Tab 1: Force network========================
  min_swipe_graph <- 10
  observeEvent(input$tabSwitch, {

      if (input$tabSwitch == "Graph"){
      print(paste0("Preparing:",input$tabSwitch))
      if (v$counter >= min_swipe_graph){
          
        swipe_list <- read.csv(file =local_results_csv, header = FALSE)
        # creates matrix of zeros
        new_mat = matrix(0, length(nodes$node_ID), length(nodes$node_ID))
  
        # rename rows and columns
        rownames(new_mat) <-nodes$node_ID
        colnames(new_mat) <-nodes$node_ID
  
        # loop through swipe results and store into new_mat
        for (row in 1:nrow(swipe_list)) {
          # IF rownames are node_ID
          source <- which(nodes$node_label==swipe_list["V2"][row,])
          target <- which(nodes$node_label==swipe_list["V3"][row,])
  
          # IF rownames are node_label
          # source <- swipe_list["V2"][row,]
          # target <- swipe_list["V3"][row,]
          val <- swipe_list["V5"][row,]
          new_mat[source,target] <- val
        }
  
        # now preprocess the nodes into a format for backend to consume
        new_items <- data.frame(nodes$node_ID, nodes$node_label)
        colnames(new_items) <- c("Item", "Item_name")
        new_items$Type <- "Strategic.Design" #dummy
        new_items$Category <- "Flexible and automated production" #dummy
  
  
        # save both new_mat and new_items 
        save(new_mat, file = "data/new_mat.RData")
        save(new_items, file = "data/new_items.RData")
        my_nodes <- new_items
        my_arcs <- new_mat
        
        source("prepare_d3.R")
        removeNotification("previousWarningMessage")
        
        output$value <- renderText({input$checkGroup })
        output$selection <- reactive(input$checkGroup)
        
        output$value2 <- renderText({input$SubOption})
        output$selection2 <- reactive(input$SubOption)
        
        # PARTITIONING
        # MUST MAKE SURE THAT Source/Target REMAINS zero-indexed AFTER FILTERING. This is required in JavaScript and so your plot may not render
        # 
        # re-indexing thread: https://stackoverflow.com/questions/51158295/how-to-renumber-group-id-sequentially-in-r
        # REACTIVE/dynamic filtering:
        # --- https://stackoverflow.com/questions/46150358/subset-a-column-of-a-dataframe-stored-as-a-reactive-expression-eventreactive
        # --- https://stackoverflow.com/questions/51153184/dynamic-filters-and-reactive-plot-in-shiny
        # --  https://stackoverflow.com/questions/56193071/how-to-connect-multiple-filters-in-shiny-range-selectinput-groupcheckbox
        # Value replacement using two dataframes: https://stackoverflow.com/questions/40177132/replace-values-from-another-dataframe-by-ids
        
        micmac_subset = reactive({
          req(input$checkGroup, input$SubOption)
          subset(MICMAC_ranking,  Type %in% input$SubOption & group %in% input$checkGroup)
        })
        
        nodes_subset = reactive({
          req(input$checkGroup, input$SubOption)
          subset(nodes_D3,  type %in% input$SubOption & group %in% input$checkGroup)  %>%
            mutate(id_dynamic = as.numeric(factor(id_dynamic, levels = unique(id_dynamic))) - 1)  
        }) 
        
        output$test_table_2 <- renderTable(nodes_subset(), rownames = TRUE)
        
        edges_SUBsubset = reactive({
          req(input$SubOption)
          subset(edges_D3, source_node_type %in% input$SubOption & target_node_type %in% input$SubOption) %>%  
            left_join(select(nodes_subset(), id, id_dynamic), by = c("source"="id")) %>%  
            mutate(source_dynamic=id_dynamic) %>%
            select(-id_dynamic) %>%
            left_join(select(nodes_subset(), id, id_dynamic), by = c("target"="id")) %>%
            mutate(target_dynamic=id_dynamic) %>%
            select(-id_dynamic)
        })
        
        edges_subset = reactive({
          req(input$checkGroup)
          subset(edges_SUBsubset(), source_node_group %in% input$checkGroup & target_node_group  %in% input$checkGroup) %>%  
            left_join(select(nodes_subset(), id, id_dynamic), by = c("source"="id")) %>%  
            mutate(source_dynamic=id_dynamic) %>%
            select(-id_dynamic) %>%
            left_join(select(nodes_subset(), id, id_dynamic), by = c("target"="id")) %>%
            mutate(target_dynamic=id_dynamic) %>%
            select(-id_dynamic)
        })
        
        output$test_table <- renderTable(edges_subset(), rownames = TRUE)
        
        output$value <- renderText({input$checkGroup })
        output$selection <- reactive(input$checkGroup)
        
        output$force <- renderForceNetwork({
          req(input$checkGroup)
          forceNetwork(
            Links = edges_subset(),
            Nodes = nodes_subset(),
            Source = "source_dynamic",
            Target = "target_dynamic",
            NodeID = "description", 
            Nodesize = "size", 
            Group = "group", 
            Value = "value", 
            arrows = TRUE, charge = -2000,  linkDistance = 50,
            colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);"),    # list of colour scales https://github.com/d3/d3-scale-chromatic/blob/master/README.md
            opacity = 1, opacityNoHover = TRUE, fontSize = 11, fontFamily = "calibri", zoom = TRUE, legend = TRUE)
        })
      }
      ## minimum swipes have not reached
      else{
        print(paste0("Minimum of ",min_swipe_graph," swipes are needed to generate the graph.", "Current: ",v$counter))
        
      }
    }
  })
  

}

#### Part 4: run ####
shinyApp(ui, server)