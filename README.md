# ShwypeHammer: shinyswipr revisited

## Motivation
shinyswipr is an R package that enables users to build a 'swipe-able' card-deck interface in Shiny (https://github.com/nstrayer/shinyswipr)
Yet I've soon noticed that a Shiny app built using the original shinyswipr package won't work on devices that enable both touch and mouse input simultaneously (e.g. touchscreen laptop)
If you have that kind of device you can try by yourself by having a go at the demo provided by the author at https://nickstrayer.shinyapps.io/shinysense_swipr_demo/

## What's the contribution?
After opening an issue on shinyswipr's github - without much follow-up - I have taken the liberty to have a stab at the issue myself.

Specifically, I've realised that shinyswipr relies on TocuhSwipe.js for gesture detection. I think that Hammer.js does a better job in the presence of 'hybrid' devices, and therefore I wanted to change the piping to do the switch.
I didn't rewrite the package - I'm not that clever. But I propose here a modified version of the "quote swipr" example described in https://livefreeordichotomize.com/2017/03/12/introducing-shinyswipr-swipe-your-way-to-a-great-shiny-ui/

On the surface, the modified app
- Displays a pair of randomly generated quotes and images on a card deck 
- Uses a "multi page" layout so that the swipe log is displayed on a separate tab rather than under the card deck

Under the hood I changed the inner workings of shinyswipr as follows
- Self contained app, reading .js and .css from its own \www subfolder instead of relying on the shinywsipr package location
- Hammer.js replaces touchSwipe.js as a touch detection device
- own .js and .css files governing a different card swipe animation based on hammer.js (replace shinySwiper.js and swiprStyle.css) 

## Caveat
In wrapping my head around shinyswipr's inner workings I've realised that the following major updates have occurred since the package was first released:
- updated approach to writing modules server functions in shiny, see Ch. 19 in https://mastering-shiny.org/
- communicating from JavaSript back to Shiny: using Shiny.setInputValue instead of Shiny.onInputChange https://shiny.rstudio.com/articles/communicating-with-js.html

However, after fiddling a bit with these updates - without much success - I've decided to stick with the "old" approach: after all, the main motivation was to get the bloody think to recognise a swipe on a hybrid device. But conscious there is room for a neater code if the latest best practice is taken into account.

## What does it do?
Below I show few screenshots taken from running the app locally. The web browser (Firefox) is in web devloper mode to show the custom logs.

First, we start with the app in 'rest' state. The user is presented with a card containing a random quote from the fortunes R package, and a random image taken from the web. the logs displays a succesful connection from R Shiny to the custom JavaScript via a dedicated module server

![image](https://user-images.githubusercontent.com/55926257/135081817-57f2ad00-88ed-4ffa-b901-11be545b77e4.png)

Next, the user drags the card right, but without releasing it.  A pan movement is detected via Hammer.js and displayed in the console log.

![image](https://user-images.githubusercontent.com/55926257/135082132-e6bfe8c0-6817-436c-8c96-781e2ad2712f.png)

Let's assume the user is hesitant and doesn't quite swipe yet. Whilst a pan end movement is detected, the card snaps back in place. Whether a gesture qualifies as a swipe depends on some checks about the velocity and span of the movement. The console log shows "NOSWIPE" instead of a swipe direction. The logical "true" is associated with a state variable denoting whether the current card is to be kept in the deck or not. 

![image](https://user-images.githubusercontent.com/55926257/135082271-767f52e1-9936-462d-8f95-a53752dbfda0.png)

Now let's assume the user swipes right with sufficient intent. The top card has changed with a new random quote and image. The console log displays the swipe direction and the "keep" state variable takes the value "false". The card has been swiped indeed.

![image](https://user-images.githubusercontent.com/55926257/135082557-5aa8d0c4-a260-40ff-8093-c045e565b7ff.png)

A look a the "siwpe log" tab shows that we now have a row recording the quote and swipe direction associated with the card the user has just swiped.

![image](https://user-images.githubusercontent.com/55926257/135082924-734f941f-eedb-4911-b535-0d1bbe521172.png)

