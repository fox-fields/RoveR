# RoveR

### <i> Your battery is low and it’s getting dark </i>

RoveR is a roguelike about a planetary rover created using [R](https://en.wikipedia.org/wiki/R_(programming_language)) and the package [Shiny](https://shiny.rstudio.com/). 

This is my entry in the [/r/roguelikedev](https://www.reddit.com/r/roguelikedev/) 2019 summer tutorial. I am not a developer, and this is entirely a hobby project. I’m not aware of any games made with R (perhaps for a good reason)! Let's see if it's possible.

### Mission Progress: 

#### Week 6: "Marsokhod"

This week's mission objectives included saving and loading, the ability to transit between sections of the overworld, and stairs into underground structures. Saving and loading was easy to implement - saving and loading data is local as a .Rdata files. This could be readily expanded to multiple save slots in the future.

I divided the overworld into a 8x8 grid of 50x50 cell sections. Transiting between them requires 


For the next two weeks, I'm going to be working on adding content, refining the core mechanic and polishing. If time permits, I'll profile and improve performance.

#### Week 5: "Lunokhod 3"

It became necessary this week to step back and spend time restructing how entity data is stored and subset, which greatly improved performance locally. Because of the limitations of R, I'll likely need to optomize again towards the end of the tutorials. In my defence, I've really only started to understand anything about R and [why it's a slow language](http://adv-r.had.co.nz/Performance.html). It looks as if I may need to write some functions by connecting [C++ to R](http://adv-r.had.co.nz/Rcpp.html#rcpp). I don't think anyone has attempted to write any games in R, and while I'm not the person to do so correctly, I'm having a lot of fun learning about R (and programming generally) as I follow these tutorials. 

In terms of progress, I've brought hover text (i.e. a cursor-based 'look'), line-of-sight targeting, items, an inventory, and actions (i.e. abilities for equipment) to RoveR. I've also made walls into entities, and they can now be destoryed with some effort. I've set things up nicely for the next two tutorials (I think) and I'm excited to polish some of these features for next week. In particular, the UI needs some attention.

Although performance is now improved locally, it is still awful with the online hosted demo application - I've decided to forego posting the link to the demo for this week.

![Lunokhod 3](/preview/lunokhod_3.png)

#### Week 4: "Prop-M"

This week's progress brings a simple user interface and combat. I've avoided any polish on the UI. I don't know what I will need from the UI, so a basic text display seems reasonable for now. Melee combat is implemented and any players will find themselves within a small dungeon (or 'station'). There are two enemy types with slightly different behaviour to seek out and destory. Placing the mouse cursor over enemies now displays their names, but will eventually reveal statistics about each entitiy. Performance worsened considerably this week - I didn't have time to profile and optimize, but I think it will be necessary before next week's tutorial. 

![Prop-M](/preview/prop-M.png)

#### Week 3: "Lunokhod 2"

This week's mission objectives included [planet generation](preview/lunokohd_2_map.png) and turn handling. I've decided to forego the field of view tutorial - I can implement later, if necessary - but my decision to have a small field of view might make any field of view mechanic redudant. Planet generation uses a modified random cluster method modified from [Saura and Martínez-Millán, 2000](https://link.springer.com/article/10.1023/A:1008107902848). This will be expanded later with environmental variables like thermal gradients across elevation and lattitude. Performance remains an issue when hosted online but is great locally. Profiling suggests the peformance bottleneck is mapping R objects to javascript (I think) with Plotly.js and not my R script per se. Because this only seems to be an issue with the hosted demo, I will save optimization until the end of the tutoial.

![Lunokhod_2](/preview/lunokhod_2.gif)

#### Week 2: "Apollo Lunar Rover"

This week's mission objectives included tile collision, non-player entities and a temporary 'dungeon' for exploring. Level generation is simple, with a central hall and rooms. Performance is great when run locally, but relatively slow when the shiny app is hosted.

![Apollo Lunear Rover](/preview/apollo_lunar_rover.gif)

#### Week 1: "Lunokhod 1"

Huzzah! By using R along with the package Shiny, I was able to create the basic environment and move the player rover "@" around the screen. 

![Lunokhod 1](/preview/lunokhod_1.gif)
