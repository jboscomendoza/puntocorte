#' Asistente de punto de corte
#'
#' @param
#'
#' @return Una app de shiny es iniciada
#' @export
#'
#' @examples
#' \dontrun{
#' puntocorte()
#' }

puntocorte <- function(...){
  asignar_grupo <- function(datos, cortes) {
    cortes <- cortes[cortes != 0]
    total_grupos <- length(cortes) + 1

    if(total_grupos == 1) {
      etiquetas <- "Grupo 1"
    } else {
      etiquetas <- paste0("Grupo ", 1:total_grupos)
    }

    datos %>%
      mutate(Grupo = cut(theta,
                         breaks = c(0, cortes, Inf),
                         labels = etiquetas)) %>%
      count(Grupo, name = "Conteo") %>%
      mutate(Porcentaje = round((Conteo / sum(Conteo)) * 100, 2))
  }

  get_traslape <- function(datos, corte) {
    datos %>%
      mutate(Traslape = ifelse(
        (theta < corte & int_sup < corte) |
          (theta > corte & int_inf > corte),
        "Sin traslape", "Traslape"
      )) %>%
      count(Traslape, name = "Conteo") %>%
      mutate(Porcentaje = round((Conteo / sum(Conteo)) * 100, 2))
  }

  # ui ----
  ui <- fluidPage(
    tabsetPanel(
      tabPanel(
        title = "Archivo",
        fluidRow(
          column(width = 12, h3("Carga de archivo")),
          column(width = 12, fileInput(
            inputId = "archivo",
            label = "Archivo",
            buttonLabel = "Leer archivo"))
        ),
        fluidRow(
          column(width = 12, titlePanel("Muestra de los datos")),
          column(width = 12, tableOutput(outputId = "scores")),
        )
      ),
      tabPanel(
        title = "Area de trabajo",
        fluidRow(
          column(width = 12, h3("Cortes"))
        ),
        fluidRow(
          column(width = 3, numericInput(
            inputId = "corte_1",
            label = "Corte 1",
            value = 0, min = 0, step = 0.5)),
          column(width = 3, numericInput(
            inputId = "corte_2",
            label = "Corte 2",
            value = 0, min = 0, step = 0.5))
        ),
        fluidRow(
          column(width = 12, h3("Resumen"))
        ),
        fluidRow(
          column(width = 4, h4("Grupos")),
          column(width = 4, h4("Traslape Corte 1")),
          column(width = 4, h4("Traslape Corte 2"))
        ),
        fluidRow(
          column(width = 4, tableOutput("tabla_grupos")),
          column(width = 4, tableOutput("tabla_traslape_1")),
          column(width = 4, tableOutput("tabla_traslape_2"))
        ),
        fluidRow(
          column(width = 12, h3("Visualizacion")),
          column(width = 12, plotOutput("barras", height = "200px")),
          column(width = 12, plotOutput("densidad", height = "200px"))
        )
      )
    )
  )

  # Server ----
  server <- function(input, output) {
    puntajes <- reactive({
      ext <- tools::file_ext(input$archivo$datapath)
      req(input$archivo)
      validate(need((ext == "csv"),
                    "Extension de datos incorrecta.\nDebe ser un archivo csv."))

      read.csv(input$archivo$datapath) %>%
        mutate(
          int_inf = theta - (error * 1.96),
          int_sup = theta + (error * 1.96)
        ) %>%
        arrange(theta) %>%
        rowid_to_column(var = "id")
    })

    output$scores <- renderTable({
      rbind(
        head(puntajes()),
        tail(puntajes())
      )
    })

    tabla_grupos <- reactive({
      asignar_grupo(puntajes(), c(input$corte_1, input$corte_2))
    })

    output$tabla_grupos <- renderTable({
      tabla_grupos()
    })

    output$tabla_traslape_1 <- renderTable({
      if(input$corte_1 == 0) {
        return(NULL)
      }
      get_traslape(puntajes(), input$corte_1)
    })

    output$tabla_traslape_2 <- renderTable({
      if(input$corte_2 == 0) {
        return(NULL)
      }
      get_traslape(puntajes(), input$corte_2)
    })

    output$barras <- renderPlot({
      if(is.null(puntajes())) {
        return(NULL)
      }

      tabla_grupos() %>%
        mutate(Poblacion = "") %>%
        ggplot() +
        aes(x = Poblacion, y = Porcentaje, fill = Grupo) +
        geom_col(position = position_fill(reverse = TRUE)) +
        scale_y_continuous(expand = c(0, 0), labels = scales::percent_format()) +
        coord_flip() +
        theme_minimal() +
        theme(legend.position = "bottom")

    })

    output$densidad <- renderPlot({
      if(is.null(puntajes())) {
        return(NULL)
      }

      ancho_linea = 1.25

      densidad <- ggplot(puntajes()) +
        aes(theta) +
        geom_density(fill = "#eeeeee") +
        scale_x_continuous(limits = c(0, 100)) +
        scale_y_continuous(expand = c(0, 0)) +
        labs(y = "Densidad", x = "Puntaje") +
        theme_minimal()

      densidad <-
        densidad +
        geom_vline(xintercept = input$corte_1,
                   color = "#996600", size = ancho_linea)

      if(input$corte_2 > 0) {
        densidad <-
          densidad +
          geom_vline(xintercept = c(input$corte_1, input$corte_2),
                     color = c("#996600", "#006699"), size = ancho_linea)
      }

      densidad

    })

  }

  # Run ----
  shinyApp(ui = ui, server = server)
}
