shinyUI(fluidPage(

    # Application title
    titlePanel("Análisis de texto: Lira Popular"),

    tabsetPanel(
      tabPanel(title = "Frecuencia de palabras",
    fluidRow(
      
      column(12,
             h3("Palabras más frecuentes"),
      ),
      
      column(3,
             radioGroupButtons("stopwords_1",
                               label = "Filtrar stopwords",
                              choices = c("Sí" = TRUE, 
                                          "No" = FALSE),
                              justified = T
             )
      ),
      column(9,
             selectizeInput("autores_1",
                            label = "Filtrar autores",
                            choices = c("Todos", v_autores), width = "100%"
                            ),
             
      ),
      
      column(12, hr() )
    ),
    
    fluidRow(
      column(12,
             dataTableOutput("tabla_palabras") |> shinycssloaders::withSpinner()
      )
    )
      ), #fin de pestaña
    
    #—----
    
    tabPanel(title = "Comparación de líneas",
             fluidRow(
               column(12,
                      br(),
                      #autores
                      selectizeInput("autores_2",
                                     label = "Elija un autor",
                                     choices = unique(lista_liras$autor), 
                                     selected = sample(unique(lista_liras$autor), 1),
                                     width = "100%", 
                      ),
                      
                      #liras
                      selectizeInput("liras_2",
                                     label = "Elija una lira",
                                     choices = NULL,
                                     width = "100%"
                      ),
                      
                      #sensibilidad
                      sliderInput("sensibilidad", label = "Sensibilidad de coincidencias",
                                  min = 0.6, max = 1, step = 0.05, 
                                  value = 0.7, width = "100%", ticks = F),
                      
                      hr(),
               )
             ),
             
             #resultados----
             
             fluidRow(
               column(4,
                      h3("Líneas más populares"),
                      p("Muestra la lira seleccionada entera, junto a la cantidad de veces que cada línea presenta coincidencias en otras liras del corpus, de acuerdo a la sensibilidad indicada."),
                      tableOutput("tabla_2_por_lineas") |> shinycssloaders::withSpinner()
               ),
               
               column(4,
                      h3("Autores que referencian esta lira"),
                      p("Indica un ranking de los autores cuya obra posee coincidencias con las líneas de esta lira, de acuerdo a la sensibilidad indicada."),
                      tableOutput("tabla_2_copiones") |> shinycssloaders::withSpinner()
               ),
               
               column(4,
                      h3("Referencias por línea"),
                      p("Seleccione una línea de la lira elegida para obtener las coincidencias de dicha línea en el resto del corpus."),
                      selectizeInput("lira_lineas_2", label = NULL, choices = NULL, width = "100%"),
                      tableOutput("tabla_2_lineas_coincidencias") |> shinycssloaders::withSpinner()
               ),
             )
    ) #fin de pestaña
    )#fin de panel de pestañas
))


#sudo ln -s ~/Otros/lira_popular/lira_popular_comparaciones /srv/shiny-server/