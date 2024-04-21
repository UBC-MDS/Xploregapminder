library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(gapminder) 
library(RColorBrewer)
library(shinyWidgets)

column_names <- c("Life Expectancy" = "lifeExp", "Population" = "pop", "GDP per Capita" = "gdpPercap")


gapminder <- gapminder |> 
  mutate(continent = if_else(continent == "Oceania", "Asia", continent))


ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = 'lumen'),
  h1("XploreGapminder"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      pickerInput("yearInput", "Select Year:", choices = seq(1952, 2007, by = 5),
                  selected = 2007),
      pickerInput("xcol", "Select X:", choices = names(column_names),
                  multiple = FALSE, selected = "GDP per Capita"),
      pickerInput("ycol", "Select Y:", choices = names(column_names),
                  multiple = FALSE, selected = "Life Expectancy"),
      pickerInput("size", "Select size column:", choices = names(column_names),
                  multiple = FALSE, selected = "Population"),
      pickerInput("continentInput", "Select Continent:", 
                  choices = c("Asia", "Europe", "Africa", "Americas"),
                  options = list(`actions-box` = TRUE), multiple = TRUE, 
                  selected = c("Asia", "Europe", "Africa", "Americas"))
    ),
    mainPanel(
      plotlyOutput("scatterPlot"),
      hr()
    )
  ),
  
  # New fluidRow for the geoPlot and its specific controls
  fluidRow(
    column(
      width = 9,
      plotlyOutput("geoPlot")
    ),
    column(
      width = 2,
      wellPanel(
        pickerInput("geoColorVar", "Select Variable for Color:", 
                    choices = names(column_names),
                    selected = "Life Expectancy")
      )
    )
  )
)


## Server function
server <- function(input, output, session) {
  observe({
    updatePickerInput(
      session,
      inputId = "xcol",
      choices = setdiff(names(column_names), input$ycol)
    )
  })
  
  observe({
    updatePickerInput(
      session,
      inputId = "ycol",
      choices = setdiff(names(column_names), input$xcol)
    )
  })
  
  output$scatterPlot <- renderPlotly({
    gm_f <- gapminder %>%
      filter(year == as.numeric(input$yearInput), continent %in% input$continentInput)
    
    x_var <- column_names[input$xcol]
    y_var <- column_names[input$ycol]
    size_var <- column_names[input$size]
    
    cols = colnames(gm_f |> select(4:6))
    size_col <- setdiff(cols, c(x_var, y_var))
    
    gg <- ggplot(data = gm_f, aes_string(
      x = x_var, 
      y = y_var, 
      color = "continent", 
      size = size_var,
      text = "country"
    )) +
      geom_point(alpha = 0.7) +
      scale_color_manual(values = c("#FFDB6D", "#C3D7A4", "#D16103", "#56B4E9")) +
      theme_minimal() +
      labs(x = input$xcol, y = input$ycol)
    
    if (x_var == "gdpPercap") {
      gg <- gg + scale_x_log10(breaks = c(500, 1000, 2000, 4000, 8000, 16000, 32000, 64000))
    } else {
      gg <- gg + scale_x_continuous()
    }
    
    if (x_var == "gdpPercap") {
      gg <- gg + scale_y_log10(breaks = c(500, 1000, 2000, 4000, 8000, 16000, 32000, 64000))
    } else {
      gg <- gg + scale_y_continuous(breaks = seq(0, 90, by = 10))
    }
    
    ggplotly(gg, tooltip = c("text", "x", "y"))
    
  })
  
  output$geoPlot <- renderPlotly({
    gm_f <- gapminder |>
      filter(year == as.numeric(input$yearInput), continent %in% input$continentInput)
    
    orRd_colors <- colorRampPalette(brewer.pal(9, "OrRd"))(100)
    
    plot <- plot_geo(gm_f, locations = ~country, locationmode = 'country names') %>%
      add_trace(
        z = as.formula(paste0("~", column_names[input$geoColorVar])),
        colors = orRd_colors,  
        type = 'choropleth',
        marker = list(line = list(color = 'rgb(40,40,40)', width = 0.5))) |>
      colorbar(title = "Life Expectancy") |>
      layout(
        title = paste('Global', input$geoColorVar , 'in', input$yearInput),
        geo = list(
          scope = "world",
          projection = list(type = "natural earth"),
          showland = TRUE,
          landcolor = "lightgrey",
          showcountries = TRUE,
          countrycolor = "darkgrey",
          coastlinecolor = "black",
          framecolor = "white",  
          bgcolor = 'white'
        ),
        margin = list(l = 0, r = 0, t = 50, b = 0),
        paper_bgcolor = 'white', 
        plot_bgcolor = 'white' 
      )    
  })
  return(output)
}

shinyApp(ui = ui, server = server)
