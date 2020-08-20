library(shinydashboard)
library(shiny)
library(zoo)
library(shinythemes)
library(rugarch)
library(lubridate)
library(xts)
library(readr)
library(tidyverse)
library(ggfortify)
library(timetk)
library(tidyverse)
library(plotly)
library(ggplot2)
library(TSstudio)


# Load data

volraw = read_csv(file='FED-SVENY.csv',
                  col_types = cols(Date = col_date(format = "%Y-%m-%d"))) 

# Create xts and tibble versions of data
volxts = tk_xts(volraw, date_var = Date)
voltbl = as_tibble(volraw)

# Long format of data
vol_dplyr_long = 
    voltbl%>%
    gather(Assets,Yields,-Date) %>%
    group_by(Assets)

# Differentiate time series
volxtsdiff <- diff.xts(volxts)

voltbldiff_long = 
    volxtsdiff%>%
    # convert the index to a date
    data.frame(Date = index(.))%>%
    remove_rownames()%>%
    gather(Assets,Yields,-Date)%>%
    group_by(Assets)

voltbldiff_longer = voltbldiff_long %>%
    spread(Assets,Yields)

yieldnames = colnames(volxts)



ui <- dashboardPage(
    dashboardHeader(title = "Volatility Modeling"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Yield Plot", tabName = "plotting", icon = icon("dashboard")),
            menuItem("Modeling", tabName = "modeling", icon = icon("th"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "plotting",
        fluidRow(
            box(
                title = "Select Yield(s)",
                selectInput(
                    inputId = "yields",
                    label = "Yield(s):",
                    choices = yieldnames,
                    selected = 'SVENY01',
                    multiple = TRUE
                )
            )),
          fluidRow(
              box(
                  plotlyOutput("plot1", height = 250),width = 12)
                   
        ),
        fluidRow(
            box(
                plotlyOutput("plot2", height = 250),width = 12)
            
        )),
        
        #Second Tab Item
        tabItem(tabName = "modeling",
                fluidRow(
                    box(
                        title = "Select Yield(s)",
                        selectInput(
                            inputId = "yields2",
                            label = "Yield(s):",
                            choices = yieldnames,
                            selected = 'SVENY01'
                        )
                    )),
                fluidRow(
                    box(
                        plotlyOutput("plot3", height = 250),width = 12)),
                splitLayout(
                fluidRow(
                    box(
                        plotOutput("plot4", height = 250),width = 10)),
                fluidRow(
                    box(
                        plotOutput("plot5", height = 250),width = 10)))
                
                )
        )
    )
)

server <- function(input, output) {
    
    # Create reactive function to fit GARCH model
    
    garchfit= reactive({
        
        # Assign user selected yield to variable
        Yield = na.omit(as.numeric(unlist(voltbldiff_longer %>% select(input$yields))))
        
        # Define the benchmark distribution
        distribution <- qnorm
        
        # Specify the GARCH model with the skewed t-distribution
        spec <- ugarchspec(distribution.model = 'sstd')
        
        # Fit the model
        fit_1 = ugarchfit(Yield, spec = spec)
        
        # Save the volatilities and the rescaled residuals
        Volatility = sigma(fit_1)
        Residual = scale(residuals(fit_1, standardize = TRUE)) * sd(Yield) + mean(Yield)
        
        #Create dataframe of x and residual
        Statistics = cbind(Yield,Residual)
    })
    
    tabPanel2 = reactive({
        
        Yield = na.omit(as.numeric(unlist(voltbldiff_longer %>% select(input$yields2))))
        
        # Define the benchmark distribution
        distribution <- qnorm
        
        # Specify the GARCH model with the skewed t-distribution
        spec <- ugarchspec(distribution.model = 'sstd')
        
        # Fit the model
        fit_1 = ugarchfit(Yield, spec = spec)
        
        # Save the volatilities and the rescaled residuals
        Volatility = sigma(fit_1)
        Residual = scale(residuals(fit_1, standardize = TRUE)) * sd(Yield) + mean(Yield)
        
        Statistics = cbind(Yield,Residual,Volatility)
        
    })
    
    ## Plotting
    
    #Create user selected time-series plot 
    
    output$plot1 <- renderPlotly({
        
        ggplotly(ggplot(vol_dplyr_long %>% filter(Assets %in% input$yields), aes(x=Date)) 
                 + geom_line(aes(y=Yields, col = Assets))+theme_minimal())
        
    })
    
    output$plot2 <- renderPlotly({
        
        ts_plot(voltbldiff_long %>% filter(Assets %in% input$yields) %>% spread(Assets,Yields), type = 'multiple', width = 1)
        
    })
    
    # Create Density Plot
    
    output$plot3 <- renderPlotly({
        
        Yield = na.omit(as.numeric(unlist(voltbldiff_longer %>% select(input$yields2))))
        
        # Define the benchmark distribution
        distribution <- qnorm
        
        # Specify the GARCH model with the skewed t-distribution
        spec <- ugarchspec(distribution.model = 'sstd')
        
        # Fit the model
        fit_1 = ugarchfit(Yield, spec = spec)
        
        # Save the volatilities and the rescaled residuals
        Volatility = sigma(fit_1)
        Residual = scale(residuals(fit_1, standardize = TRUE)) * sd(Yield) + mean(Yield)
        Statistics = cbind(Yield,Residual,Volatility) 
        ts_plot(Statistics, type = 'multiple')
        
    })
    
    
    
    output$plot4 <- renderPlot({
        
        Yield = na.omit(as.numeric(unlist(voltbldiff_longer %>% select(input$yields2))))
        
        # Define the benchmark distribution
        distribution <- qnorm
        
        # Specify the GARCH model with the skewed t-distribution
        spec <- ugarchspec(distribution.model = 'sstd')
        
        # Fit the model
        fit_1 = ugarchfit(Yield, spec = spec)
        
        # Save the volatilities and the rescaled residuals
        Volatility = sigma(fit_1)
        Residual = scale(residuals(fit_1, standardize = TRUE)) * sd(Yield) + mean(Yield)
        
        Yield_density <- density(Yield)
        Residual_density <- density(Residual)
        
        Statistics = cbind(Yield, Residual)
        
        ggplot(Statistics, aes(x=Yield)) + geom_density(size = 1, color = "cornflowerblue") + xlim(-0.3,0.3)+ labs(x = "Yield", y = "Density") +
        geom_line(data = Residual_density, aes(x=Residual_density$x,y=Residual_density$y), colour = 'red',size = 1) +
        stat_function(fun = dnorm, args = list(mean = mean(Yield), sd = sd(Yield)), color = "orange", size = 1) + xlim(-0.3,0.3)+ theme_minimal()
        
    })
    
    # Create QQ-Plot 
    
    output$plot5 <- renderPlot({
        
        tabPanel2() %>% ggplot() + geom_qq(aes(sample = Yield)) + labs(x = "Theoretical", y = "Sample")+
        geom_qq(aes(sample = Residual*0.614256270265139), color = "red") + 
        geom_qq_line(aes(sample = Yield), distribution = qnorm, col = "gray") +
        geom_qq_line(aes(sample = Residual*0.6142562702651391), distribution = qnorm, col = "gray")+ ylim(-0.5,0.5)+theme_minimal()
        
    })
    
}


shinyApp(ui, server)

