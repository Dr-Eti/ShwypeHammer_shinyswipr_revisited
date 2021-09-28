// Swiping in Shiny: testbench for modifying shinySwiper.js so that is uses Hammer.js
// Notice that shinySwiper.js uses a "Class constructor" method. Therefore, I have re-done my swipe widget experiment following this tutorial
//        1: https://medium.com/@simonepm/build-a-full-featured-tinder-like-carousel-in-vanilla-javascript-part-i-44ca3a906450 
//        2: https://github.com/nstrayer/shinyswipr

// to view the console.log make sure you enable more tools -> web developer tools -> console (click on logs) in Firefox, in the webpage used by LiveServer to visualise the results




// BLOCK 1: The code below is triggered whenever the R Shiny's server (swipe module) sends a message over to javascript

class TinderlikeCarousel{
    constructor(id){
        console.log("initializing card with id", id);
        this.id = id;
        // I replaced shinyswipr's jQuery this.card = $("#"+id);
        // following "transversing" in https://gist.github.com/joyrexus/7307312
        this.deck = document.getElementById(id);   
        this.topCard = document.querySelector('.card');                                 // lists the cards. In reality there will be only one at a time if the reactions in Shiny work properly...                                     
                // this.count = 0;
        // console.log(this);
        // console.log(this.count);
        
        // reset previous movements if any
        
        if (this.mc) this.mc.destroy();                                                 // destroy previous Hammer instance, if present, to improve memory allocation
        
        // handle gesture, SEE FUNCTION BELOW
        // from here on it's my trial and errors. 
        // useful resources: https://hammerjs.github.io/examples/
        this.mc  = new Hammer(this.topCard);                                                // listens to pan gesture on the top card. 
        this.mc.get('pan').set({direction: Hammer.DIRECTION_ALL});                          // detect pan gesture   
        // DETECT GESTURE IN PROGRESS - I think uses jQuery
        this.mc.on("panleft panright panup pandown panend",                                 // Move the card horizontally and vertically along with the gesture detected
            function(ev){
                // FOR DEBUG this will log the pan-type event as we hover the mouse on the top-card (but not the other cards)
                var panmsg = ev.type + " gesture detected.";
                console.log(panmsg);  
                // The idea below is mine: Animate the card as the cursor moves BUT CONSTRAIN horizontally or vertically, to avoid ambiguous directions
                let horizORvert = Math.abs(ev.deltaX/ev.deltaY);                            // <1 it is a MOSTLY VERTICAL swipe else it is a MOSTLY HORIZONTAL swipe
                if (horizORvert >= 1){
                    ev.target.style.transform = 'translateX(' + ev.deltaX + 'px)';          // animate by changing the style of the card: you could also embellish with a rotation, but unnecessary here
                };
                if (horizORvert < 1) {
                    ev.target.style.transform = 'translateY(' + ev.deltaY + 'px)';
                };
                //FOR DEBUG
                //console.log(horizORvert); 
            }
        );
        // DETECT END OF GESTURE (= DECISION), evaluate if the card should be removed or snapped back into the deck    
        this.mc.on("panend", 
            function(ev){                     
            // threshold approach w/absolute delta and velocity (other possible)
            // a bit of a repetition
                let horizORvert = Math.abs(ev.deltaX/ev.deltaY);                           // <1 it is a MOSTLY VERTICAL swipe else it is a MOSTLY HORIZONTAL swipe
                if (horizORvert >= 1){
                    ev.target.style.transform = 'translateX(' + ev.deltaX + 'px)';         // animate by changing the style of the card: you could also embellish with a rotation, but unnecessary here
                };
                if (horizORvert < 1) {
                    ev.target.style.transform = 'translateY(' + ev.deltaY + 'px)';
                };
                // conclude if it counts as a swipe
                if (horizORvert >=1){
                    var keep = Math.abs(ev.deltaX) < 80 || Math.abs(ev.velocityX) < 0.5;   // it's NOT a (horizontal) swipe if it DOESN'T meet some pre-defined thresholds
                    var dirX = ev.deltaX < 0 ? -1 : 1;                                      // left (-1) or right (1)
                    //  Get info about direction of swiping TO PASS ON TO SHINY
                    var You_Swiped_right = (horizORvert)*dirX >= 1;
                    var You_Swiped_left = (horizORvert)*dirX< 0;
                    // generate message to display in console
                    if (You_Swiped_right){
                        You_Swiped = "right";
                    };
                    if(You_Swiped_left){
                        You_Swiped = "left";
                    }
                } else {
                    var keep = Math.abs(ev.deltaY) < 80 || Math.abs(ev.velocityY) < 0.9;     // it's NOT a (vertical) swipe if it DOESN'T meet some pre-defined thresholds
                    var dirY = ev.deltaY < 0 ? -1 : 1;                                       // up (-1) or down (1) 
                    //  Get info about direction of swiping TO PASS ON TO SHINY
                    var You_Swiped_up = (horizORvert)*dirY < 0;
                    var You_Swiped_down = (horizORvert)*dirY > 0;
                    if (You_Swiped_down){
                        You_Swiped = "down";
                    }; 
                    if (You_Swiped_up){
                        You_Swiped = "up";
                    };
                };   

                // modify token that will help bring up new card after succesful swipe                    
                // ev.target.classList.toggle('removed', !keep);                                          // 'removed' is a token which is removed from a list (or added, if no token). https://developer.mozilla.org/en-US/docs/Web/API/DOMTokenList/toggle

                if(keep){
                     ev.target.style.transform = '';                                                    // snap it back into position
                     You_Swiped = "NOSWIPE";
                } else {
                    // animate the 'destruction' of the card as it's being swiped away                
                    if (horizORvert >=1){                
                        var moveOutWidth = document.body.clientWidth;                                  // get border postion. could be body instead of deck
                        var endX = Math.max(Math.abs(ev.velocityX) * moveOutWidth, moveOutWidth);
                        var toX = ev.deltaX > 0 ? endX : -endX;                                        // if then else statement
                        ev.target.style.transform = 'translateX(' + toX + 'px)';
                    } else {
                        var moveOutHeight = document.body.clientHeight;                                // again, we only use deck (could be body) to get the  border position
                        var endY = Math.max(Math.abs(ev.velocityY) * moveOutHeight, moveOutHeight);
                        var toY = ev.deltaY > 0 ? endY : -endY;
                        ev.target.style.transform = 'translateY(' +(toY + ev.deltaY) + 'px)';                                                
                    };

                    //send the decision to shiny                       
                    // From JavaScript to R - seehttps://shiny.rstudio.com/articles/communicating-with-js.html                    
                    
                    // PLASE NOTICE YOU MUST ENFORCE REACTIVITY: https://shiny.rstudio.com/articles/js-send-message.html
                    //  ---  Add a random "nonce" (or a counter) to the message sent back to Shiny to enforce reactivity (e.g. via "observers")                                 
        
                    // without nonce (WONT'T REFERSH THE CARD DECK AFTER THE SWIPE)
                    //Shiny.setInputValue("cardSwiped", You_Swiped, {priority: "event"});                    // should cause the shiny server’s input$cardSwiped to be set to whatever value in You_Swiped.                           
                    
                    // with nonce (the key here is to "alter" something in the message sent back to R)                                    
                    // I am folloing the original script  but using a random nonce instead of counter
                    var send_dest = id + "-cardSwiped";
                    Shiny.onInputChange(send_dest, Math.random() + "-" + You_Swiped);
                    
                    // RESET CARD POSITION - this is what I was missing...
                    ev.target.style.transform = '';                                              // just make sure to reseet
                };

                // FOR DEBUG - display the displacement in the consol once the panning movement is over
                // console.log(ev.deltaX);
                // console.log(ev.deltaY);
                // console.log(dirX);
                // console.log(dirY);
                // console.log(horizORvert);
                // console.log(ev.direction);
                console.log(You_Swiped);
                // // console.log(built_in_direction);
                // // console.log(you_swiped_right_indeed);
                console.log(keep);       
                // console.log(message_nonce);                        
             }
        );
    };
};
                                        
    

// BLOCK 2:

// The following OUTER expression listens for "'shiny:connected" events on all DOM elements. 
// The INNER expression adds a new tinder carousel everytinme a module server in R Shiny sends a message across 

// on how R Shiny communicates with JS and back
// -- https://shiny.rstudio.com/articles/js-send-message.html

// the original shinyswipr code is a "jQuery" expression: $(document).on() binds a click event to the document and all child elements within it. 
// This method is referred to as delegated event handling: for the explanation see 
// --- https://stackoverflow.com/questions/40613527/document-on-in-plain-javascript
// --- https://stackoverflow.com/questions/41820906/what-is-different-between-document-on-and-element-on/41821075
// I have tried the following equivalent "plain" Javascript exquivalent but doesn't work
// --- document.addEventListener("shiny:connected", function(event) {                                                    
// here are some helpful resources in general on moving from jQuery to JavaScript
// --- see item n.4 (event delegation) in https://code.tutsplus.com/tutorials/from-jquery-to-javascript-a-reference--net-23703

let You_Swiped = "NOSWIPE";
var n = 0;                                                                                    // this will be used as a nonce to execute code in Shiny under "observe": https://github.com/FrissAnalytics/shinyJsTutorials/blob/master/tutorials/materials3/messageApp2/www/message.js

$(document).on('shiny:connected', function(event) {                                           // Shiny tutorial on this: https://shiny.rstudio.com/articles/js-events.html
    console.log("shiny is connected.")
    var cards = [];                                                                                                // an empty array in case we have more than one card.   

    // This function receives a message from R Shiny - see https://shiny.rstudio.com/articles/communicating-with-js.html
    Shiny.addCustomMessageHandler("initializeCard",                                                                 // this is the type, and should match what is written in R Shiny's session$sendCustomMessage 
        function(cardID) {                                                                                          // a function with argument equal to the message sent from R Shiny (the card id). I chose to replace the original arrow function: cardID =>
            var newCard = new TinderlikeCarousel(cardID);                                                           // triggers the code above
            cards.push(newCard);                                                                                    // add the newly generate card to the vector
        }
    );
});

// https://medium.com/@zappingseb/7-steps-that-make-custom-inputs-in-shiny-easy-504b303a2973
// var Swiper_Hammer_bind = new Shiny.InputBinding();
// $.extend(Swiper_Hammer_bind, {
//     find: function(scope){
//         return $(scope).find(".TinderlikeCarousel");
//     }
// })
// Shiny.InputBindings.register(Swiper_Hammer_bind);