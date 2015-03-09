library(shiny)
shinyServer(function(input, output, session) {
    getdata <- reactive({
        if(is.null(input$uploaded)){
            read.table("http://www.meteo.psu.edu/holocene/public_html/shared/research/ONLINE-PREPRINTS/Millennium/DATA/RECONS/nhem-recon.dat"
                       , col.names=c("year","temp"))
        } else {
            read.csv(input$uploaded$datapath)
        }
    })
    
    output$selectResponse <- renderUI({
        dat <- getdata()
        if(is.null(dat)) return()
        selectInput("response", "Select Response (Y):", 
                    choices = names(dat), selected=names(dat)[dim(dat)[2]]
        )
    })
    
    output$selectPredictor <- renderUI({
        dat <- getdata()
        if(is.null(dat)) return()
        radioButtons("predictor", "Select Predictor (X):", 
                     choices = names(dat), selected=names(dat)[1]
        )
    })
    
    sliderUpdate <- reactive({
        dat <- getdata()
        if(is.null(dat) | is.null(input$predictor)) return()
        predictor <- dat[,input$predictor]
        smallest = min(predictor)
        largest = max(predictor)
        unit = 10^floor(log10(largest-smallest)-log10(4))
        list("largest"=largest, "smallest"=smallest, "unit"=unit)
    })
    
    output$kinkSlider <- renderUI({
        dat <- sliderUpdate()
        if(is.null(dat)) return()
        smallest = dat$smallest
        largest = dat$largest
        unit = dat$unit
        sliderInput("kinkPointX1", "Locate Kink Point:",
                    min = ceiling(smallest/unit)*unit, max = floor(largest/unit)*unit, 
                    value = floor((largest+smallest)/2/unit)*unit, step = unit)
    })
    
    output$times10 <- renderUI({
        dat <- sliderUpdate()
        kink <- input$kinkPointX1
        if(is.null(dat) | is.null(kink)) return()
        sliderInput("kinkPointX10", "X10 Zoom:",
                    min = kink-dat$unit, max = kink+dat$unit, 
                    value = kink, step = dat$unit/10)
    })
    
    fittedModel <- reactive({
        dat <- getdata()
        kink <- input$kinkPointX10
        if(is.null(dat) | is.null(kink) | is.null(input$predictor) | is.null(input$response)) return()
        predictor <- dat[,input$predictor]
        response <- dat[,input$response]
        splineTerm <- (predictor - kink) * (predictor > kink)
        lm(response~predictor+splineTerm)
    })
    
    output$scatterPlot <- renderPlot({
        dat <- fittedModel()
        kink <- input$kinkPointX10
        if(is.null(dat) | is.null(kink)) return()
        plot(dat$model$predictor, dat$model$response, 
             xlab="predictor", ylab="response", main="Scatter Plot with Fitted Curve")
        x <- c(dat$model$predictor,kink)
        yhat <- c(dat$fitted.values,
                  predict(dat, data.frame(predictor=kink, splineTerm=0)))
        lines(x[order(x)], yhat[order(x)], col="red", lwd=3)
        legend("topleft", bty="n", text.col="blue", cex=2.5, 
               legend=bquote(R^2 == .(format(summary(dat)$r.square, digits=4))))
    }, height = function() {0.618*session$clientData$output_scatterPlot_width})
    
    output$linearModel <- renderPrint({
        summary(fittedModel())
    })
    
    output$residualPlot <- renderPlot({
        plot(fittedModel(), which=1)
    }, height = function() {0.618*session$clientData$output_residualPlot_width})
    
    output$table <- renderDataTable({
        getdata()
    }, options = list(searching = FALSE))
    
    
})
