library(shiny)
library(shinyWidgets)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(ggwordcloud)

liras <- arrow::read_feather("~/Documents/Apps Shiny/lira_popular/datos/lira_datos_palabra.feather")

lista_liras <- liras |> select(titulo, n_id) |> distinct() |> tibble::deframe()

sdal <- readr::read_rds("~/Documents/Apps Shiny/lira_popular/datos/sdal_diccionario_afectos_español_expandido.rds")

topicos <- arrow::read_feather("~/Documents/Apps Shiny/lira_popular/datos/lira_diccionario_topicos_unicos.feather")

liras_topicos <- readr::read_rds("~/Documents/Apps Shiny/lira_popular/datos/lira_topicos.rds")





ui <- fluidPage(
  
  titlePanel("Lira popular"),
  
  fluidRow(
    
    column(12,
           
           pickerInput("lira", 
                       "Seleccionar lira:",
                       multiple = F,
                       choices = lista_liras,
                       selected = sample(lista_liras, 1)
           ),
           actionButton("lira_aleatoria",label = "Al azar")
    )
  ),
  
  fluidRow(
    column(12,
           tableOutput("tabla_lira")
    )
  ),
  
  fluidRow(
    column(6,
           
           
           # htmlOutput("texto_lira"),
           
           textOutput("texto_topico"),
           selectInput("topico", label = "topico",  choices = 1:4),
           htmlOutput("texto_lira_2")
           
    ),
    column(6,
           
           plotOutput("grafico_nube")
    ),
    column(6, 
           plotOutput("grafico_sentimientos")
    ),
    column(6, 
           plotOutput("grafico_conteo_palabras")
    )
  )
  
)



server <- function(input, output, session) {
  
  observeEvent(input$lira_aleatoria, {
    message("azar")
    # browser()
    updatePickerInput(session, inputId = "lira",
                      # choices = lista_liras,
                      selected = unname(sample(lista_liras, 1))
    )
  })
  
  observeEvent(input$lira, {
    updateSelectInput(session, "topico",
                      selected = lira_topico()$topico
    )
  })
  
  lira <- reactive({
    liras |> 
      filter(n_id == input$lira)
  })
  
  lira_texto <- reactive({
    lira() |> 
      select(txt_cuerpo) |> 
      pull() |> 
      unique()
  })
  
  # conteos ----
  lira_conteo_palabras <- reactive({
    # browser()
    lira() |>
      select(titulo, n_palabra, txt_palabra) |>
      count(txt_palabra) |>
      filter(n > 0) |> 
      arrange(desc(n)) |> 
      slice(1:20)
  })
  
  output$tabla_lira <- renderTable({
    lira() |> 
      select(1:8) |> 
      distinct()
  })
  
  output$grafico_conteo_palabras <- renderPlot({
    # browser()
    
    lira_conteo_palabras() |> 
      filter(n > 1) |> 
      mutate(txt_palabra = factor(txt_palabra, lira_conteo_palabras()$txt_palabra)) |> 
      ggplot(aes(y = txt_palabra, x = n)) +
      geom_col()
    
  })
  # output$texto_lira <- renderUI({
  #   # browser()
  #   
  #   texto_1 <- lira() |> 
  #     select(n_parrafo, txt_parrafo) |> 
  #     distinct() |> 
  #     rowwise() |> 
  #     mutate(txt = list(div(txt_parrafo, style = "margin-bottom: 6px;"))) |> 
  #     mutate(txt = as.character(txt)) |> 
  #     mutate(txt = str_replace_all(txt, "\\\n", "<br>"))
  #   
  #   texto_2 <- texto_1 |> 
  #     pull(txt) |> 
  #     paste(collapse = " ")
  #   
  #   
  #   return(HTML(texto_2))
  # })
  
  # sentimientos ----
  lira_sentimiento <- reactive({
    lira() |> 
      select(starts_with("n_"), starts_with("txt_")) |> 
      left_join(sdal, join_by(txt_palabra == word), relationship = "many-to-many")
  })
  
  

  
  lira_sentimiento_lineas <- reactive({
    lira_sentimiento() |> 
      group_by(n_linea, txt_palabra) |> 
      summarize(across(starts_with("s_"), ~mean(.x, na.rm=T))) |> 
      mutate(across(starts_with("s_"), ~replace_na(.x, 0)))
  })
  
  lira_sentimiento_lineas_long <- reactive({
    lira_sentimiento_lineas() |> 
      pivot_longer(cols = starts_with("s_"))
  })
  
  #topicos ----
  
  ## topico del documento ----
  lira_topico <- reactive({
    liras_topicos |> filter(document == input$lira)
  })
  
  output$texto_topico <- renderText({
    paste("El documento pertenece al tópico", lira_topico()$topico)
  })
  
  #diccionario de palabra del topico seleccionado ----
  palabras_topico_regex <- reactive({
    palabras_topico <- topicos |> 
      filter(topic == input$topico) |> 
      pull(term)
    
    palabras_topico_regex <- paste(paste0("\\b", palabras_topico, "\\b"), collapse = "|")
  })
  
  #texto con palabras del tópico destacadas ----
  output$texto_lira_2 <- renderUI({
    # browser()
    
    texto_1 <- lira() |> 
      select(n_parrafo, txt_parrafo) |>
      # select(n_linea, txt_linea) |>
      distinct() |> 
      rowwise() |> 
      mutate(txt = list(div(txt_parrafo, style = "margin-bottom: 8px;"))) |> 
      mutate(txt = as.character(txt)) |> 
      #saltos de línea
      mutate(txt = str_replace_all(txt, "\\\n", "<br>")) |> 
      #color 
      mutate(txt_color = str_replace_all(txt, palabras_topico_regex(),
                                         p("\\0", style = "background-color: purple; color: white; display: inline; padding: 2px; border-radius: 4px;") |> 
                                           as.character()
      )
      )
    
    texto_2 <- texto_1 |> 
      pull(txt_color) |> 
      paste(collapse = " ")
    
    
    return(HTML(texto_2))
  })
  
  
  # gráfico nube ----
  output$grafico_nube <- renderPlot({
    lira_conteo_palabras() |>
      ggplot(aes(label = txt_palabra, size = n)) +
      geom_text_wordcloud() +
      scale_size_area(max_size = 10, trans = power_trans(1/.5)) +
      theme_minimal()
  })
  
  # gráfico sentimiento ----
  output$grafico_sentimientos <- renderPlot({
    lira_sentimiento_lineas_long() |> 
      ggplot(aes(x = n_linea, y = value, fill = value)) +
      geom_col() +
      stat_smooth(method = lm, formula = y ~ poly(x, 10), se = FALSE, color = "orange") +
      scale_fill_gradient2(low = "green3", mid = "white", high = "purple2") +
      facet_wrap(~name, ncol = 1) +
      theme_minimal(base_line_size = 0) +
      coord_flip() +
      scale_y_continuous(limits = c(-1, 1))
  })
  
  
  
}

shinyApp(ui = ui, server = server)
