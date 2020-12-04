#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(plotly)

prodqual <- read.csv("PRODQUAL.csv")
prodqual <- data.frame(prodqual)

# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel('Plots of PRODQUAL'),

    # Sidebar with 2 select inputs and a numeric input
    sidebarLayout(
        sidebarPanel(
            selectInput("mod", "Select Mode", c("Temperature ~ Quality + Pressure",
                                                "Quality ~ Pressure + Temperature",
                                                "Pressure ~ Temperature + Quality"))),

        # Shows the plot
        mainPanel(
            plotlyOutput("TDPlot1"),
            plotlyOutput("TDPlot2"),
            plotlyOutput("TDPlot3")
        )
    )
)


server <- (function(input, output, session) {
    output$TDPlot1 <- renderPlotly({
        if(input$mod == "Temperature ~ Quality + Pressure"){
            # define data
            # x1 <- input$var2 "Cannot coerce class "name" to a data frame
            # x2 <- input$var3
            #x3 <- rnorm(n)>0.5
            # y <- input$var1

            x1 <- prodqual$QUALITY #Error in as.vector: cannot coerce type 'symbol' to vector of type 'any'
            x2 <- prodqual$PRESSURE
            y <- prodqual$TEMP


            df <- data.frame(y, x1, x2)
            ylm <- lm(y ~ x1 + x2)
            cf.lm <-coef(ylm)

            # calculate z on grid of x-y values
            x1.s <- seq(min(x1), max(x1), length.out = 25)
            x2.s <- seq(min(x2), max(x2), length.out = 25)
            z <- t(outer(x1.s, x2.s, function(x,y) cf.lm[1] + cf.lm[2]*x + cf.lm[3]*y))

            # draw the plane
            cols <- c("pink", "blue")
            #cols <- cols[x3 + 1] # has x3, which I don't have

            plot_ly(
                x =~ x1.s,
                y =~ x2.s,
                z =~ z,
                colors = c("pink", "blue"),
                type = "surface") %>%
                add_trace(data=df, x=x1, y=x2, z=y, mode = "markers", type = "scatter3d",
                          marker = list(color = "red", opacity = 0.7, symbol = 105)) %>%
                layout(scene = list(aspectmode = "manual", aspectratio = list(x=1, y=1, z=1),
                                    xaxis = list(title = "X1", range = c(0, 100)),
                                    yaxis = list(title = "X2", range = c(0,100)),
                                    zaxis = list(title = "Y", range = pretty(z)[c(0,100)])
                )
                )
        }

    }
    )

    output$TDPlot2 <- renderPlotly({
        if(input$mod == "Quality ~ Pressure + Temperature"){
            # define data
            # x1 <- input$var2 "Cannot coerce class "name" to a data frame
            # x2 <- input$var3
            #x3 <- rnorm(n)>0.5
            # y <- input$var1

            x1 <- prodqual$PRESSURE #Error in as.vector: cannot coerce type 'symbol' to vector of type 'any'
            x2 <- prodqual$TEMP
            y <- prodqual$QUALITY


            df <- data.frame(y, x1, x2)
            ylm <- lm(y ~ x1 + x2)
            cf.lm <-coef(ylm)

            # calculate z on grid of x-y values
            x1.s <- seq(min(x1), max(x1), length.out = 25)
            x2.s <- seq(min(x2), max(x2), length.out = 25)
            z <- t(outer(x1.s, x2.s, function(x,y) cf.lm[1] + cf.lm[2]*x + cf.lm[3]*y))

            # draw the plane
            cols <- c("yellow", "red")
            #cols <- cols[x3 + 1] # has x3, which I don't have

            plot_ly(
                x =~ x1.s,
                y =~ x2.s,
                z =~ z,
                colors = c("yellow", "red"),
                type = "surface") %>%
                add_trace(data=df, x=x1, y=x2, z=y, mode = "markers", type = "scatter3d",
                          marker = list(color = "red", opacity = 0.7, symbol = 105)) %>%
                layout(scene = list(aspectmode = "manual", aspectratio = list(x=1, y=1, z=1),
                                    xaxis = list(title = "X1", range = c(0, 100)),
                                    yaxis = list(title = "X2", range = c(0,100)),
                                    zaxis = list(title = "Y", range = pretty(z)[c(0, 100)])
                )
                )
        }

    }
    )

    output$TDPlot3 <- renderPlotly({
        if(input$mod == "Pressure ~ Temperature + Quality"){
            # define data
            # x1 <- input$var2 "Cannot coerce class "name" to a data frame
            # x2 <- input$var3
            #x3 <- rnorm(n)>0.5
            # y <- input$var1

            x1 <- prodqual$TEMP #Error in as.vector: cannot coerce type 'symbol' to vector of type 'any'
            x2 <- prodqual$QUALITY
            y <- prodqual$PRESSURE


            df <- data.frame(y, x1, x2)
            ylm <- lm(y ~ x1 + x2)
            cf.lm <-coef(ylm)

            # calculate z on grid of x-y values
            x1.s <- seq(min(x1), max(x1), length.out = 25)
            x2.s <- seq(min(x2), max(x2), length.out = 25)
            z <- t(outer(x1.s, x2.s, function(x,y) cf.lm[1] + cf.lm[2]*x + cf.lm[3]*y))

            # draw the plane
            cols <- c("purple", "green")
            #cols <- cols[x3 + 1] # has x3, which I don't have

            plot_ly(
                x =~ x1.s,
                y =~ x2.s,
                z =~ z,
                colors = c("purple", "green"),
                type = "surface") %>%
                add_trace(data=df, x=x1, y=x2, z=y, mode = "markers", type = "scatter3d",
                          marker = list(color = "red", opacity = 0.7, symbol = 105)) %>%
                layout(scene = list(aspectmode = "manual", aspectratio = list(x=1, y=1, z=1),
                                    xaxis = list(title = "X1", range = c(0, 100)),
                                    yaxis = list(title = "X2", range = c(0,100)),
                                    zaxis = list(title = "Y", range = pretty(z)[c(0,100)])
                )
                )
        }

    }
    )
})

# Run the application
shinyApp(ui = ui, server = server)
