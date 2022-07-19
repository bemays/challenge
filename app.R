library(shiny)
library(tidyverse)
library(plotly)
library(dplyr)
library(readr)
library(shinycssloaders)
library(trelliscopejs)
library(shinythemes)
library(shinydashboard)
library(readr)
library(homelessR)


hud <- hud_data
bea <- left_join(gdp_data, total_employment_data)
census <- census_data
full <- full_data
full$Year <- as.numeric(full$Year)
folder_paths <- list.files("www/built")
folder_names <- folder_paths %>%stringr::str_replace_all("_", " ") %>% stringr::str_to_title()
names(folder_paths) <- folder_names

  ui <- fluidPage(
    sidebarLayout(
      sidebarPanel(
        h3('Create your own trelliscope'),
        selectInput(inputId = 'x_axis', 
                    label = 'What column do you want for the x-axis?',
                    choices = colnames(full),
                    selected = 'Year'),
        selectInput(inputId = 'y_axis',
                    label = 'What column do you want for the y-axis?',
                    choices = colnames(full),
                    selected = "violent_crime"),
        selectInput(inputId = 'facet',
                    label = 'What would you like to facet by?',
                    choices = c('state', 'Year'),
                    selected = "state"),
        textInput(inputId = 'save_trell', value = "crime",
                  label = 'Name to Save'),
        actionButton(inputId = 'trells',
                     label = 'Create Trelliscope'),
        h3('View Pre-made Trelliscopes'),
        selectInput(inputId = 'trello',
                    label = 'Pre-made Trelliscopes',
                    choices = folder_paths),
        actionButton(inputId = 'view_trells',
                     label = "View") ## view_trells 
      ), # sidebarPanel
      
      mainPanel(
        withSpinner(trelliscopeOutput(outputId = 'plot')),
        textOutput(outputId = 'description_funding_graph')
      ) # mainPanel
    ) # sidebarLayout
  ) # fluidPage

server <- function(input, output, session) {
  observeEvent(input$view_trells, {
    showModal(modalDialog(title = 'Link to Trelliscope:',
                          helpText(a('Click the link to view trelliscope', 
                                     href = paste0('built/',input$trello ,'/index.html#display=', input$trello, '&nrow=2&ncol=2&arr=row&pg=1&labels=state&sort=state;asc&filter=&sidebar=-1&fv=',
                                                   target = 'blank'))),
                          easyClose = TRUE))
  })
  
  
  
  observeEvent(input$trells, {
    ## when the button is clicked, create the trelliscope in the shiny app
    output$plot <- renderTrelliscope({
      
      # text input for formulat in facet
      var <- paste("~", isolate(input$facet))
      print(var)
      
      full %>%
       group_by(.data[[isolate(input$facet)]]) %>%
       nest() %>%
       mutate(
         panel = map_plot(data, function(x) {
                            ggplot(data = x,
                              aes_string(
                                x = isolate(input$x_axis), 
                                y = isolate(input$y_axis))) +
                            geom_line() +
                            geom_point() +
                            theme_bw() +
                            ylab(isolate(input$y_axis)) }
         )) %>%
       ungroup() %>%
       trelliscope(
         name = isolate(input$save_trell),
         ncol = 2, nrow = 2, path = "www/")
      
    }) ## end of the trelliscope
    
  }) ## observeEvent for trells
  
}


# Run the application 
shinyApp(ui = ui, server = server)
