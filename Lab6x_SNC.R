# Lab 6 extra: R SHiny apps and COVID data

# Best when you already have data wrangled, maybe made some plots beforehand, then 
# mess with the data/graphs in R Shiny Apps

# Shiny overview and tutorials
# Must install the shiny package before starting, then import to the environment
library(shiny)

# Run an example to show how Shiny apps look 
runExample('01_hello')

# Seven good tutorials are given on the RStudio website. I will be going through them and 
# applying what is learned for the COVID-19 dataset

# Tutorial 1 - Welcome to Shiny

# Shiny is an R package that allows one to easily build interactive web applications.
# Once imported, the Shiny package contains eleven built-in examples to show how it works.

# Hello Shiny contains an example of a histogram of R's faithful data set with variable 
# numbers of bins that can be changed with a slider bar. See above for how to run examples.

## Structure of a Shiny App

# Shiny apps are contained in a single script called app.R, which is in it's own directory
# (to allow it to be self-contained with standard file names). app.R has three components -
# a user interface (ui) object, server function, and call to the shinyApp function.
# The ui object controls the layout and appearance of the app. The server function 
# contains the instructions to create the interactive objects. The shinyApp function 
# actually created the Shiny app objects for the ui/server pair.

## ui 
# The ui object contains functions(such as titlePanel or sidebarPanel) that create the objects.
# These objects include nested functions that define what is included in the layout (such 
# as sliderInput for sidebarPanel). The output is called using plotOutput (at least in Ex 1).

## server
# The server function defines the logic for making the plot. It uses a renderPlot call
# to make the output reactive (changes with changing input) and output a plot. This is 
# where you can format your graph and the inputs to make it interactive.

## shinyApp
# After the ui and server objects are formatted and created, the script ends with a call
# to the shinyApp function - shinyApp(ui = ui, server = server). You won't be able to do
# anything else in the terminal while the app is running.

## Running an App

# Every Shiny app has the same structure - the app.R file with the ui and server functions.
# Because of this, it is recommended to save a Shiny app to its own directory. You can 
# run an app by giving the name of its directory in the function runApp.

## Exercise 1 - 
# Make the following changes to the script for Example 1 - 
# Change the title to "Hello World!"; set the minimum value of the slider to 5; change 
# the histogram border color to orange. 

runApp('App-1')

# Extra note - to display an app in showcase mode (with the script below the main panel 
# that has the graph), set display.mode = 'showcase' - 
runApp('App-1', display.mode = 'showcase')

## Relaunching Apps

# You can relaunch apps by rerunning runApp() or opening an app.R script and clicking 
# the "Run App" button near the top or the keyboard shortcut Control+Shift+Enter.
# To run on an external web browser, you can click the arrow next to the Run App icon 
# and select 'Run External'.



## Tutorial 2
