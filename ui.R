# Create UI

shinyUI(pageWithSidebar(
  
  # Application title
  
  headerPanel("Exploring French and US Wine Data"),
  
  # Create Sidebar
  
  sidebarPanel(
    
    
  # First Panel, US Wine 
    ## Create slider for years and checkbox for lines of best fit
    
    conditionalPanel(
      
      'input.dataset === "US Wine"',
      
      sliderInput("Inyear", "Select Years", min=1942, max=2014, value = c(1944, 2014),sep = "", step=1),
      
      br(),
      
      selectInput("Outcomey1", label = "Y Axis:",choices = 
                  list("Total Wine Per Resident (gallons)" = "wine-resident",
                       "Total Table Wine (millions of gallons)" = "total-table",
                       "Total Wine (millions of gallons)" = "total-wine"
                  ),selected="wine-resident"
      ),
      
      br(),
      
      
      checkboxInput("show.trend", "Show trendlines", FALSE),
      
      br(),
      
      # Create fields at bottom of page
      
      helpText("Explore season by season trends across all tiers on this page"),
      
      br(),
      
      helpText(h5("Authors: Byron King & Evan Romanko")),
      
      helpText(p("Please contact",
                 a(href ="https://github.com/ByronKKing", "Byron on GitHub",target = "_blank"),
                 " or Evan on ",
                 a(href ="https://www.linkedin.com/in/eromanko", "LinkedIn", target = "_blank"),
                 ", or, if you prefer, reach out to",
                 a(href ="https://www.facebook.com/GWDataScience/?fref=ts", "GW DATA",target = "_blank"),
                 ", for more information, to suggest improvements or report errors.")),

      br()
      
    ), # end first panel
    
    
  # Second Panel, US and California Wine
    ## Create slider for years, checkbox for best fit

    conditionalPanel(
      'input.dataset === "US and California"',
      
      sliderInput("Inyear2", "Select Years", min=1995, max=2014, value = c(1995, 2014),step=1,sep=""),
      
      br(),
      
      
      selectInput("type2", "Select California or USA to Display:",
                  choices = c("California",
                              "USA","Both")
                  ,selected="California"
      ),
      
      br(),
      
      checkboxInput("show.trend2", "Show trendlines", FALSE),
      
      br(),
      
      # Create fields at bottom of page
      
      helpText("Explore season by season trends across all tiers on this page"),
      
      br(),
      
      helpText(h5("Authors: Byron King & Evan Romanko")),
      
      helpText(p("Please contact",
                 a(href ="https://github.com/ByronKKing", "Byron on GitHub",target = "_blank"),
                 " or Evan on ",
                 a(href ="https://www.linkedin.com/in/eromanko", "LinkedIn", target = "_blank"),
                 ", or, if you prefer, reach out to",
                 a(href ="https://www.facebook.com/GWDataScience/?fref=ts", "GW DATA",target = "_blank"),
                 ", for more information, to suggest improvements or report errors.")),
      
      br()
      
    ), # end second panel

  
  # Third Panel, US and France Wine
    ## Slider input for years and trend lines check box

    conditionalPanel(
      'input.dataset === "US and France"',
  
      sliderInput("Inyear3", "Select Years", min=1995, max=2012, value = c(1995, 2012), step=1, sep=""),
  
      br(),
  
  
      selectInput("type3", "Select France or USA to Display:",
              choices = c("USA",
                          "France","Both")
              ,selected="USA"
      ),
  
      br(),
  
      selectInput("Outcomey2", label = "Y Axis:", choices = 
              list("Exports" = "Exports",
                   "Imports" = "Imports",
                   "Production" = "Production",
                   "Consumption" = "Consumption"
                   ),selected="Exports"
      ),
  
      br(),
  
      # Create fields at bottom of page
  
      checkboxInput("show.trend3", "Show trendlines", FALSE),
  
      br(),
  
  
      helpText("Explore season by season trends across all tiers on this page"),
  
      br(),
  
      helpText(h5("Authors: Byron King & Evan Romanko")),
  
      helpText(p("Please contact",
             a(href ="https://github.com/ByronKKing", "Byron on GitHub",target = "_blank"),
             " or Evan on ",
             a(href ="https://www.linkedin.com/in/eromanko", "LinkedIn", target = "_blank"),
             ", or, if you prefer, reach out to",
             a(href ="https://www.facebook.com/GWDataScience/?fref=ts", "GW DATA",target = "_blank"),
             " for more information, to suggest improvements or report errors.")),
  
      br()
  
    ) # end third panel

  ), # end sidebar panel
    
    
  # Show the main display
  
  mainPanel(
    tabsetPanel(id='dataset',
      tabPanel('US Wine', plotOutput("plot1")),
      tabPanel('US and California', plotOutput("plot2")),
      tabPanel('US and France', plotOutput("plot3"))
    )
  ) # end mainPanel
  
)) # end pageWithSidebar and UI

