# ShwypeHammer: shinyswipr revisited

## Motivation
shinyswipr is an R package that enables users to build a 'swipe-able' card-deck interface in Shiny (developed by @nnstrayer; https://github.com/nstrayer/shinyswipr)
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
- Hammer.js replace touchSwipe.js as a touch detection device
- own .js and .css files governing a different card swipe animation based on hammer.js (replace shinySwiper.js and swiprStyle.css) 

## Caveat
In findings my way around shinyswipr's inner workings I've realised that the following major updates have occurred
- updated approach to writing modules server functions in shiny, see Ch. 19 in https://mastering-shiny.org/
- communicating from JavaSript back to Shiny: using Shiny.setInputValue instead of Shiny.onInputChange https://shiny.rstudio.com/articles/communicating-with-js.html

However, after fiddling a bit with these updates - without much success - I've decided to stick with the "old" approach: after all, the main motivation was to get the bloody think to recognise a swipe on a hybrid device. But conscious there is room for a neater code if the latest best practice is taken into account.
