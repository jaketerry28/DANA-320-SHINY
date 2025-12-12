### ----- Load packages ----

library(shiny)
library(tidyverse)
library(scales)
library(shinyWidgets)
library(plotly)


### ---- Define UI ----

# create vector of variable names
# -> take out the categorical variables

df <- read_csv("Real_Estate_Sales_2001-2023_GL.csv")

ui <- fluidPage(
  titlePanel("Shiny Real Estate Explorer"),
  
  sidebarPanel(
    sliderInput(
      "yearSlider",
      "Years",
      min = min(df$`List Year`, na.rm = TRUE),
      max = max(df$`List Year`, na.rm = TRUE),
      value = range(df$`List Year`, na.rm = TRUE),
      sep = ""
    ),
    selectInput(
      "yvar",
      "Y-Axis Variable:",
      choices = c("Sale Amount", "Assessed Value"),
      selected = "Sale Amount"
    ),
    pickerInput(
      inputId = "towns",
      label = "Choose Town(s):",
      choices = sort(unique(df$Town)),
      selected = unique(df$Town)[1:5],
      multiple = TRUE,
      options = list(
        `actions-box` = TRUE,
        `live-search` = TRUE,
        `selected-text-format` = "count > 3"
      )
    ),
    pickerInput(
      inputId = "types",
      label = "Choose Property Type(s):",
      choices = sort(unique(df$`Property Type`)),
      selected = unique(df$`Property Type`),
      multiple = TRUE,
      options = list(
        `actions-box` = TRUE,
        `live-search` = TRUE,
        `selected-text-format` = "count > 3"
      )
    ),
    radioGroupButtons(
      inputId = "groupChoice",
      label = "Group Data By:",
      choices = c("Town", "Residential Type"),
      selected = "Town",
      justified = TRUE
    )
  ),
  
  
  # create spot for plot
  mainPanel(
    plotlyOutput(outputId = "linePlot"),
    plotlyOutput(outputId = "barPlot")
  )
)


### ---- Define server ----

server <- function(input, output) {
  
  # Filtered by slider
  filtered <- reactive({
    req(input$yearSlider)  # ensure slider exists
    
    df[df$`List Year` >= as.numeric(input$yearSlider[1]) &
         df$`List Year` <= as.numeric(input$yearSlider[2]), ]
  })
  
  # Plot
  output$linePlot <- renderPlotly({
    data <- filtered()
    
    if (input$groupChoice == "Town") {
      
      data <- data %>% filter(Town %in% input$towns)
      group_var <- "Town"
      
    } else {
      
      data <- data %>% filter(!is.na(`Residential Type`))
      group_var <- "Residential Type"
    }
    
    df_grouped <- data %>%
      group_by(.data[[group_var]], `List Year`) %>%
      summarize(value = mean(.data[[input$yvar]], na.rm = TRUE), .groups = "drop")
    
    p <- ggplot(df_grouped, aes(
      x = `List Year`,
      y = value,
      color = .data[[group_var]],
      group = .data[[group_var]],
      text = paste0(
        "<b>", group_var, ":</b> ", .data[[group_var]], "<br>",
        "<b>Year:</b> ", `List Year`, "<br>",
        "<b>", input$yvar, ":</b> ", scales::comma(value)
      )
    )) +
      geom_line(linewidth = 1.1) +
      scale_y_continuous(labels = scales::comma) +
      geom_point(size = 2) +
      labs(
        title = paste("Average", input$yvar, "Over Time by", group_var),
        x = "List Year",
        y = input$yvar,
        color = group_var
      ) +
      theme_bw()
    
    ggplotly(p, tooltip = "text")
  })
  
  output$barPlot <- renderPlotly({
    data <- filtered()
    
    if (input$groupChoice == "Town") {
      data <- data %>% filter(Town %in% input$towns)
      group_var <- "Town"
    } else {
      data <- data %>% filter(!is.na(`Residential Type`))
      group_var <- "Residential Type"
    }
  
    counts <- data %>%
      count(group = .data[[group_var]], name = "Amount")
    
    counts$tooltip <- paste0(
      "<b>", group_var, ":</b> ", counts$group, "<br>",
      "<b>Amount:</b> ", scales::comma(counts$Amount)
    )
    
    p <- ggplot(counts, aes(
      x = group,
      y = Amount,
      fill = group,
      text = tooltip
    )) +
      geom_col() +
      labs(
        title = paste("Number of Sales by", group_var),
        x = group_var,
        y = "Number of Sales",
        fill = group_var
      ) +
      scale_y_continuous(labels = scales::comma) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p, tooltip = "text")
  })
}


### ---- Run app ----

shinyApp(ui, server)