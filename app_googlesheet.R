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
library(fortunes) 

#### Part 1: Module UI for swipe card deck ####
Module_swipeCard_UI <- function(id, pass_img, pass_h3, pass_p){
  ns <- NS(id)
  tagList(                                      # a special type of layout function that allows you to bundle together multiple components without actually implying how they’ll be laid. Ch.19 https://mastering-shiny.org/scaling-modules.html
    tags$head(                                  # load our www/myjavascrip.js and www/mycss.css files, which are saved under a www subfolder.
      tags$script(src = "hammer-min.js"),       # Call a third-party script that deals with mouse and touch events
      tags$script(src = "test-V5.js"),          # My customised script that governs the swipe detection and card animation on swipe
      tags$link(rel = "stylesheet", type = "text/css", href = "test-V5.css")  # My customised card style
    ),
    tags$body(
      # create dependency: deck, card, card elements
      tags$div(id = id, class = "card-deck",
               tags$div(class = "card",
                        #tags$img( ...image file here, stored in subdirectory www ... , height = 300, width =  400),
                        #tags$html(pass_img, height = 100, width =  200),            # for images linked from web
                        tags$div(class = "randomImage", pass_img),
                        tags$h3(pass_h3),
                        # The variable text contents generated through Shiny are better placed in a div, not a paragraph
                        tags$div(class = "randomText", pass_p)
               )
      )
    )
  ) #end tag list.
}

#### Part 2: Module server for swipe cart deck  ####

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
  # -- the (module) server’s input$cardSwiped CARRIES the final swipe decision (e.g. left, right...)
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
  tabsetPanel(
    tabPanel("Deck",
             Module_swipeCard_UI("my_tinderLike_swiper",
                                 # These are the FIXED contents passed on to the card div
                                 htmlOutput("url"),                                                            # This trick helps accommodate randomly generated images retrieved from the web
                                 "here is a (trimmed) random quote",
                                 textOutput("quote")                                                           # the random text will be accommodated in a separate div
                                 
             )
    ),
    tabPanel("Swipes log",
             tableOutput("resultsTable")
    )
  )
)

server <- function(input, output, session){
  # This approach to invoking a module's server is obsolete but I stick to it
  # for an update see see Ch. 19 in https://mastering-shiny.org/
  card_swipe <- callModule(Module_swipeCard_serverOLD, "my_tinderLike_swiper")
  
  # The below follows pretty much the original quote sweeper example
  # except I don't track the author of a quote, and introduce random images
  appVals <- reactiveValues(
    quote = fortune(),
    random_image = paste0("<img src='","https://picsum.photos/320/320/?random=", round(runif(1)*1000000), "'>"),
    swipes = data.frame(quote = character(),
                        swipe = character()
    )
  )
  
  our_quote <- isolate(appVals$quote)
  output$quote <- renderText({ strtrim(our_quote$quote, 150) })
  output$url <- renderText({appVals$random_image})
  output$resultsTable <- renderDataTable({appVals$swipes})
  
  observeEvent( card_swipe(),{
    #Record last swipe results.
    appVals$swipes <- rbind(
      data.frame(quote = appVals$quote$quote,
                 swipe = card_swipe()
      ), appVals$swipes
    )
    #send results to the output.
    output$resultsTable <- renderTable({appVals$swipes})
    
    #update the quote and the image
    appVals$quote <-fortune()
    appVals$random_image <- paste0("<img src='", "https://picsum.photos/320/320/?random=", round(runif(1)*1000000), "'>")
    
    #send update to the ui.
    output$quote <- renderText({ strtrim(appVals$quote$quote, 150) })
    output$url <- renderText({appVals$random_image})
    
  }) #close event observe.
}

#### Part 4: run ####
shinyApp(ui, server)