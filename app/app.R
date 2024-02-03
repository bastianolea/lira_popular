library(shiny)
library(shinyWidgets)
library(bslib)
library(thematic)
library(fresh)
library(dplyr)
library(tidyr)
library(glue)
library(stringr)
library(ggplot2)
library(ggwordcloud)

color_fondo = "#d4c599"
color_texto = "#684e42"
color_secundario = "#684e42"
color_detalle = "#684e42"
color_destacado = "#8e5d3d"

#tema automático
thematic_shiny(font = "auto", bg = color_fondo, fg = color_texto, accent = color_destacado)


# datos ----

#corpus de la lira popular, por palabras, línea, párrafo y documento
liras <- arrow::read_feather("~/Documents/Apps Shiny/lira_popular/datos/lira_datos_palabra.feather")

lista_liras <- liras |> select(titulo, n_id) |> distinct() |> tibble::deframe()

#diccionario de sentimientos
sdal <- readr::read_rds("~/Documents/Apps Shiny/lira_popular/datos/sdal_diccionario_afectos_español_expandido.rds")

#diccionario de palabras por tópico
topicos <- arrow::read_feather("~/Documents/Apps Shiny/lira_popular/datos/lira_diccionario_topicos_unicos.feather")

#topico por documento
liras_topicos <- readr::read_rds("~/Documents/Apps Shiny/lira_popular/datos/lira_topicos.rds")

#estilos ----
css <- function(text) {
  tags$style(glue(text, .open = "{{", .close = "}}"))
}

estilo_cuadritos = glue("background-color: {color_detalle}; color: white; padding: 12px; margin: 12px; border-radius: 4px;")

ui <- fluidPage(
  title = "Lira popular", 
  lang = "es",
  
  # use_googlefont("Gabarito"),
  
  theme = bslib::bs_theme(
    bg = color_fondo, fg = color_texto, primary = color_destacado, secondary = color_texto,
    base_font = font_google("Gabarito"),
    heading_font = font_link(
      "Slabo 27px", href = "https://fonts.googleapis.com/css2?family=Slabo+27px&display=swap"
    )
  ),
  
  css("h2 {
      margin-top: 24px;
      weight: bold;
  }"),
  
  # use_theme(create_theme(
  #   theme = "default",
  #   bs_vars_input(bg = color_fondo),
  #   bs_vars_global(body_bg = color_fondo, 
  #                  text_color = color_texto, 
  #                  link_color = color_secundario),
  #   bs_vars_font(size_base = "16px",
  #   ), 
  #   bs_vars_modal(content_bg = color_fondo, content_border_color = color_detalle, 
  #                 backdrop_bg = color_fondo, backdrop_opacity = "60%"),
  #   bs_vars_button(
  #     default_color = color_fondo,
  #     default_bg = color_secundario,
  #     default_border = color_fondo, 
  #     border_radius_base = "6px"
  #   )
  # )),
  
  # titlePanel("Lira popular"),
  fluidRow(
    column(12,
           tags$img(src = "liraXIX.png", style = "width: 100%; max-width: 500px;")
    )
  ),
  
  tabsetPanel(
    ## liras ----
    tabPanel("Liras", 
             h2("Análisis de texto por documento"),
             fluidRow(
               
               column(12, style = "margin-top: 12px;",
                      div(style = "display: inline-block;",
                          selectInput("lira", 
                                      h5("Seleccionar lira:"),
                                      # multiple = F,
                                      choices = lista_liras,
                                      selected = sample(lista_liras, 1),
                          )
                      ),
                      div(style = "display: inline-block;",
                          actionButton("lira_aleatoria", label = "Al azar", 
                                       style = glue("background_color: {color_secundario} !important; 
                                                color: white;")
                          )
                      )
               )
             ),
             
             # fluidRow(
             #   column(12,
             #          tableOutput("tabla_lira")
             #   )
             # ),
             
             ### topicos ----
             fluidRow(
               column(12,
                      
                      plotOutput("grafico_topico_documento", height = 80),
                      textOutput("texto_topico"),
                      selectInput("topico", 
                                  label = h5("Destacar tópico:"),
                                  choices = 1:4),
                      em("Destacar palabras clasificadas en el tópico al que pertenece el documento, u otro"),
                      
                      div(style = "padding: 20px;",
                      htmlOutput("texto_lira_2")
                      )
                      
               ),
               
               ### palabras ----
               column(12, 
                      div(style = "max-width: 500px;",
                      plotOutput("grafico_nube", height = 250)
                      )
               ),
               
               ### sentimientos ----
               column(12,
                      fluidRow(
                        column(4,
                               div(style = estilo_cuadritos,
                                 h3("Activación"),
                                 textOutput("sentimiento_valor_act"),
                                 textOutput("sentimiento_etiqueta_act"),
                                 
                               )
                        ),
                        column(4,
                               div(style = estilo_cuadritos,
                               h3("Imaginabilidad"),
                               textOutput("sentimiento_valor_ima"),
                               textOutput("sentimiento_etiqueta_ima")
                               ),
                               
                        ),
                        column(4,
                               div(style = estilo_cuadritos,
                                 h3("Agrado"),
                                 textOutput("sentimiento_valor_ple"),
                                 textOutput("sentimiento_etiqueta_ple")
                               )
                        )
                      )
               ),
               
               column(12, 
                      plotOutput("grafico_sentimientos")
               ),
               # column(12, 
               #        plotOutput("grafico_conteo_palabras")
               # )
             )
             
    ),
    
    ## autores ----
    tabPanel("Autores", 
             h2("Análisis de texto por autores")
    ),
    
    ## tópicos ----
    tabPanel("Tópicos", 
             h2("Análisis de texto por tópicos"),
             
             h3("Modelamiento de los tópicos"),
             p("Modelo de aprendizaje automático de asignación latente de Dirichlet")
    )
  ),
  
  
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
  
  # output$grafico_conteo_palabras <- renderPlot({
  #   # browser()
  #   
  #   lira_conteo_palabras() |> 
  #     filter(n > 1) |> 
  #     mutate(txt_palabra = factor(txt_palabra, lira_conteo_palabras()$txt_palabra) |> rev()) |> 
  #     ggplot(aes(y = txt_palabra, x = n)) +
  #     geom_col()
  #   
  # })
  
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
    # browser()
  })
  
  #sentimiento promedio del documento
  lira_sentimiento_documento <- reactive({
    lira_sentimiento() |> 
      summarize(across(starts_with("s_"), ~mean(.x, na.rm = T)))
  })
  
  #transformación del valor de sentimiento en etiquetas
  lira_sentimiento_documento_texto <- reactive({
    #   agrado (agradable / neutra / desagradable)
    #   activación (activa / neutra / pasiva)
    #   imaginabilidad (fácil de imaginar / neutra / difícil de imaginar)
    corte_1 = -1+0.66
    corte_2 = -1+0.66+0.66
    
    lira_sentimiento_documento() |> 
      pivot_longer(cols = everything()) |> 
      mutate(estado = case_when(name == "s_activation" & value > corte_2 ~ "activo",
                                name == "s_activation" & value >= corte_1 ~ "neutro",
                                name == "s_activation" & value < corte_1 ~ "pasivo",
                                #
                                name == "s_pleasantness" & value > corte_2 ~ "agradable",
                                name == "s_pleasantness" & value >= corte_1 ~ "neutro",
                                name == "s_pleasantness" & value < corte_1 ~ "desagradable",
                                #
                                name == "s_imagination" & value > corte_2 ~ "fácil de imaginar",
                                name == "s_imagination" & value >= corte_1 ~ "neutro",
                                name == "s_imagination" & value < corte_1 ~ "difícil de imaginar")
      ) |> 
      select(-value) |> 
      pivot_wider(names_from = name, values_from = estado)
  })
  
  output$sentimiento_valor_act = renderText(lira_sentimiento_documento()$s_activation)
  output$sentimiento_etiqueta_act = renderText(lira_sentimiento_documento_texto()$s_activation)
  
  output$sentimiento_valor_ima = renderText(lira_sentimiento_documento()$s_imagination)
  output$sentimiento_etiqueta_ima = renderText(lira_sentimiento_documento_texto()$s_imagination)
  
  output$sentimiento_valor_ple = renderText(lira_sentimiento_documento()$s_pleasantness)
  output$sentimiento_etiqueta_ple = renderText(lira_sentimiento_documento_texto()$s_pleasantness)
  
  
  #sentimiento promedio por líneas
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
    # browser()
  })
  
  output$grafico_topico_documento <- renderPlot({
  lira_topico() |> 
    pivot_longer(cols = where(is.double)) |> 
    ggplot(aes(x = name, y = 1, alpha = value)) +
    geom_label(aes(label = name), 
               fill = color_secundario, color = "white",
               size = 7, label.size = unit(0, "mm"),
               label.padding = unit(4, "mm")
               ) +
    coord_fixed(ratio = 3) +
    theme_void() +
    theme(legend.position = "none")
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
                                         p("\\0", 
                                           style = glue("background-color: {color_destacado}; color: white; display: inline; padding: 2px; border-radius: 4px;")) |> 
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
    # browser()
    # dev.new()
    lira_conteo_palabras() |>
      slice(1:20) |> 
      ggplot(aes(label = txt_palabra, size = n)) +
      geom_text_wordcloud(rm_outside = TRUE, area_corr = F) +
      scale_size_area(max_size = 16, expand = expansion(0),
                      trans = power_trans(1/.6)) +
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
