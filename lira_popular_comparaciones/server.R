shinyServer(function(input, output, session) {

 
  
  #datos ----
  datos <- reactive({
    
    #stopwords
    if (input$stopwords_1 == FALSE) {
      d <- liras_palabra
    } else {
      d <- liras_palabra |> 
        filter(!(txt_palabra %in% stopwords_2))
    }
    
    #autores
    if (input$autores_1 == "Todos") {
      d <- d
    } else {
      d <- d |> 
        filter(autor == input$autores_1)
    }
    
    return(d)
  })
  
    output$tabla_palabras <- renderDataTable({
        datos() |> 
        count(txt_palabra) |> 
        arrange(desc(n)) |> 
        slice(1:30)
    })

    
    
    #pestaña 2 ----
    
    #selector de liras
    observeEvent(input$autores_2, {
    updateSelectizeInput(session,
                         inputId = "liras_2",
                         choices = lista_liras |> 
                           filter(autor == input$autores_2) |> 
                           pull(titulo)
    )
    })
    
    #id lira elegida ----
    #retorna la id de la lira elegida en el selector principal
    lira_elegida <- reactive({
      req(input$liras_2 != "")
      
      l <- lista_liras |> 
        filter(autor == input$autores_2,
               titulo == input$liras_2) |> 
        pull(id_lira)
      message(l)
      return(l)
    })
    
    #preprocesar ----
    
    lira_info <- reactive({
      req(length(lira_elegida) != 0)
      
      liras_linea |> 
      filter(id_lira == lira_elegida()) |> 
      select(autor, fecha, tipo, url) |> 
      distinct() |> 
        print()
    })
    
    lira_origen <- reactive({
      req(input$liras_2 != "",
          lira_elegida(),
          length(lira_elegida) != 0)
      
      liras_linea |> 
      filter(id_lira == lira_elegida()) |> 
      select(id_lira, n_linea, txt_linea)
    })
    
    lira_analisis <- reactive({
      req(length(lira_elegida) != 0)
        
      comparadas_0 |> 
      #elegir lira por su id
      filter(id_lira.x == lira_elegida()) |> 
      #filtrar liras del mismo autor
      filter(!(autor.x == autor.y)) |> 
      #filtrar sensibilidad
      #filter(distancia >= sensibilidad) |> 
      #eliminar duplicados
      distinct(txt_linea.x, autor.x, txt_linea.y, autor.y,
               .keep_all = T) |> 
        print()
    })
    
    
    
    #líneas populares ----
    #la linea 1 aparece 4 veces, la 2 aparece 8 veces, la 40 x veces
    output$tabla_2_por_lineas <- renderTable({
      c_por_lineas <- lira_analisis() |> 
      #filtrar sensibilidad
      filter(distancia >= input$sensibilidad) |> 
      arrange(n_linea.x) |> 
      distinct() |> 
      group_by(id_lira.x, n_linea.x, txt_linea.x) |> 
      count() |> 
      ungroup() |> 
      print()
    
    #graficar texto con popularidad al lado, destacar las líneas
    lira <- lira_origen() |> 
      #adjuntar la popularidad al texto entero
      left_join(c_por_lineas |> select(n_linea = n_linea.x, n)) |> 
      select(-id_lira) |> 
      #reemplazar missing
      mutate(n = tidyr::replace_na(n, 0))
    
    return(lira)
    })
    
    #mayores copiones ----
    output$tabla_2_copiones <- renderTable({
    c_copiones <- lira_analisis() |> 
      #filtrar sensibilidad
      filter(distancia >= input$sensibilidad) |> 
      count(autor.y) |> 
      arrange(desc(n)) |> 
      print()
    })
    
    
    #coincidencias lineas ----
    observeEvent(input$liras_2, {
      updateSelectizeInput(session,
                           inputId = "lira_lineas_2", 
                           choices = lira_origen() |> 
                             select(txt_linea, n_linea) |> 
                             tibble::deframe()
      )
    })
    
    
    output$tabla_2_lineas_coincidencias <- renderTable({
      req(lira_origen(),
          input$lira_lineas_2 != "")
      
      lira_analisis() |>
        #filter(n_linea.x == 8) |>
        filter(n_linea.x == input$lira_lineas_2) |>
        #filtrar sensibilidad
        filter(distancia >= input$sensibilidad) |>
        select(txt_linea.y, autor.y, distancia)
    })
})
