shinyServer(function(input, output) {
  
  #### First scatterplot tabpanel
  
  df1 <- reactive({
    
    minyear <- input$Inyear[1]
    maxyear <- input$Inyear[2]
    
    tempallwine <- subset(allwine,Year>minyear & Year<maxyear)
    
    if (input$Outcomey1 == "wine-resident"){
      
      finalallwine <- tempallwine[,c('Year','wine.resident')]
      
    }
    
    else
      
      if (input$Outcomey1 == "total-table"){
        
        finalallwine <- tempallwine[,c('Year','total.table')]
        
      }
    
    else
      
      if (input$Outcomey1 == "total-wine"){
        
        finalallwine <- tempallwine[,c('Year','total.wine')]
        
      }

  })
  
  df2 <- reactive({
  
    
    tempallwine <- subset(allwine,Year>=input$Inyear2[1] & Year<=input$Inyear2[2])
    
    if (input$type2 == "California"){
      
      finalallwine <- tempallwine[,c('Year','California')]
      
    }
    
    else
      
      if (input$type2 == "USA"){
        
        finalallwine <- tempallwine[,c('Year','U.S.')]
        
      }
    
    else
      
      if (input$type2 == "Both"){
        
        finalallwine <- tempallwine[,c('Year','U.S.','California')]
        
      }
    
  })
  
  df3 <- reactive({
    
    tempallwine <- subset(allwine,Year>=input$Inyear3[1] & Year<=input$Inyear3[2])
    
    if (input$type3 == "USA"){
      newtemp <- tempallwine
      
      if (input$Outcomey2 == "Exports"){
        
        final <- newtemp[,c('Year','USA.exports')]
        
      }
      
      else if (input$Outcomey2 == "Imports"){
        
        final <- newtemp[,c('Year','USA.imports')]
        
      }
      else if (input$Outcomey2 == "Production"){
        
        final <- newtemp[,c('Year','USA.production')]
        
      }
      else if (input$Outcomey2 == "Consumption"){
        
        final <- newtemp[,c('Year','USA.consumption')]
        
      }
    }
    else if (input$type3 == "France"){
      
      newtemp <- tempallwine
      
      if (input$Outcomey2 == "Exports"){
        
        final <- newtemp[,c('Year','France.exports')]
        
      }
      
      else if (input$Outcomey2 == "Imports"){
        
        final <- newtemp[,c('Year','France.imports')]
        
      }
      
      else if (input$Outcomey2 == "Production"){
        
        final <- newtemp[,c('Year','France.production')]
        
      }
      
      else if (input$Outcomey2 == "Consumption"){
        
        final <- newtemp[,c('Year','France.consumption')]
        
      }
    }
    else if (input$type3 == "Both"){
      newtemp <- tempallwine
      if (input$Outcomey2 == "Exports"){
          
        final <- newtemp[,c('Year','France.exports','USA.exports')]
        
      }
        
      else if (input$Outcomey2 == "Imports"){
        
        final <- newtemp[,c('Year','France.imports','USA.imports')]
          
      }
        
      else if (input$Outcomey2 == "Production"){
          
        final <- newtemp[,c('Year','France.production','USA.production')]
          
      }
        
       else if (input$Outcomey2 == "Consumption"){
          
        final <- newtemp[,c('Year','France.consumption','USA.consumption')]
          
      }  
    }
    
  })
  
  
    
    ##########  Return Main Panel Output  ####################
    
    output$plot1 <- renderPlot({
      
      data = df1()
      
      g <-     ggplot(data, aes(x=data[,1], y=data[,2], fill='#800000')) +
        geom_point(shape=19, size=4) +
        #scale_color_manual(values=c("1"="firebrick1", "2"="deepskyblue", "3"="forestgreen","4"="blueviolet")) +
        ylab("") + xlab("") +
        theme(legend.title=element_blank(),
              text = element_text(color="grey23", size=10),
              axis.text = element_text(size=rel(1.0)),
              axis.text.x = element_text(color="grey23",size=rel(1.6)),
              axis.text.y = element_text(color="grey23", size=rel(1.6)),
              axis.title.x = element_text(size=rel(1.6)),
              axis.title.y = element_text(size=rel(1.6)),
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(),
              panel.grid.major.y = element_line(color="grey80"),
              panel.grid.major.x = element_line(color="grey80"),
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill="ghostwhite"),
              plot.background = element_rect(fill="ghostwhite")
              
        )
      
      g1 <- g + stat_smooth(aes(), size=1, alpha = 0.05)
      
      ifelse(input$show.trend==T,
             print (g1),
             print(g))
      
      
    })
  
  #Need to change color based on user input, somehow!
  output$plot2 <- renderPlot({
    
    data = df2()

    if (input$type2=='Both'){
      mdf <- melt(data, id="Year")
      bg <- ggplot(mdf, aes(x=Year, y=value, group = variable, colour = variable)) +
        geom_point(shape=19, size=4) +
        #scale_color_manual(values=c("1"="firebrick1", "2"="deepskyblue", "3"="forestgreen","4"="blueviolet")) +
        ylab("") + xlab("") +
        theme(legend.title=element_blank(),
              text = element_text(color="grey23", size=10),
              axis.text = element_text(size=rel(1.0)),
              axis.text.x = element_text(color="grey23",size=rel(1.6)),
              axis.text.y = element_text(color="grey23", size=rel(1.6)),
              axis.title.x = element_text(size=rel(1.6)),
              axis.title.y = element_text(size=rel(1.6)),
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(),
              panel.grid.major.y = element_line(color="grey80"),
              panel.grid.major.x = element_line(color="grey80"),
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill="ghostwhite"),
              plot.background = element_rect(fill="ghostwhite"))
      
      bg1 <- bg + stat_smooth(aes(), size=1, alpha = 0.05)
      ifelse(input$show.trend2==T,print(bg1),print(bg))
    } else{
      g <-     ggplot(data, aes(x=data[,1], y=data[,2])) +
        geom_point(shape=19, size=4) +
        #scale_color_manual(values=c("1"="firebrick1", "2"="deepskyblue", "3"="forestgreen","4"="blueviolet")) +
        ylab("") + xlab("") +
        theme(legend.title=element_blank(),
              text = element_text(color="grey23", size=10),
              axis.text = element_text(size=rel(1.0)),
              axis.text.x = element_text(color="grey23",size=rel(1.6)),
              axis.text.y = element_text(color="grey23", size=rel(1.6)),
              axis.title.x = element_text(size=rel(1.6)),
              axis.title.y = element_text(size=rel(1.6)),
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(),
              panel.grid.major.y = element_line(color="grey80"),
              panel.grid.major.x = element_line(color="grey80"),
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill="ghostwhite"),
              plot.background = element_rect(fill="ghostwhite")
              
        )
      
      g1 <- g + stat_smooth(aes(), size=1, alpha = 0.05)
      
      ifelse(input$show.trend2==T,
             print (g1),
             print(g))
      }
  })
    
  
  output$plot3 <- renderPlot({
    
    data = df3()
    
    if (input$type3=='Both'){
      mdf <- melt(data, id="Year")
      bg <- ggplot(mdf, aes(x=Year, y=value, group = variable, colour = variable)) +
        geom_point(shape=19, size=4) +
        #scale_color_manual(values=c("1"="firebrick1", "2"="deepskyblue", "3"="forestgreen","4"="blueviolet")) +
        ylab("") + xlab("") +
        theme(legend.title=element_blank(),
              text = element_text(color="grey23", size=10),
              axis.text = element_text(size=rel(1.0)),
              axis.text.x = element_text(color="grey23",size=rel(1.6)),
              axis.text.y = element_text(color="grey23", size=rel(1.6)),
              axis.title.x = element_text(size=rel(1.6)),
              axis.title.y = element_text(size=rel(1.6)),
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(),
              panel.grid.major.y = element_line(color="grey80"),
              panel.grid.major.x = element_line(color="grey80"),
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill="ghostwhite"),
              plot.background = element_rect(fill="ghostwhite"))
      
      bg1 <- bg + stat_smooth(aes(), size=1, alpha = 0.05)
      ifelse(input$show.trend3==T,print(bg1),print(bg))
    } else{
      g <-     ggplot(data, aes(x=data[,1], y=data[,2])) +
        geom_point(shape=19, size=4) +
        #scale_color_manual(values=c("1"="firebrick1", "2"="deepskyblue", "3"="forestgreen","4"="blueviolet")) +
        ylab("") + xlab("") +
        theme(legend.title=element_blank(),
              text = element_text(color="grey23", size=10),
              axis.text = element_text(size=rel(1.0)),
              axis.text.x = element_text(color="grey23",size=rel(1.6)),
              axis.text.y = element_text(color="grey23", size=rel(1.6)),
              axis.title.x = element_text(size=rel(1.6)),
              axis.title.y = element_text(size=rel(1.6)),
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(),
              panel.grid.major.y = element_line(color="grey80"),
              panel.grid.major.x = element_line(color="grey80"),
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill="ghostwhite"),
              plot.background = element_rect(fill="ghostwhite")
              
        )
      
      g1 <- g + stat_smooth(aes(), size=1, alpha = 0.05)
      
      ifelse(input$show.trend3==T,
             print (g1),
             print(g))
    }    
})
    
    
  })
    
    
    