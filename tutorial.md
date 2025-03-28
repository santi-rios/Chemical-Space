# Guía para el Desarrollo de Aplicaciones Shiny con R y Control de Versiones con Git

## Índice
1. Introducción
2. Fundamentos de Shiny
3. Estructura Básica de una Aplicación Shiny
4. Componentes Esenciales de la UI
5. Sistema de Reactividad en Shiny
6. Control de Versiones con Git
7. Flujo de Trabajo Git para Proyectos Shiny
8. Técnicas Avanzadas y Mejores Prácticas
9. Recursos Adicionales

## Introducción

Shiny es un framework de R que permite transformar análisis estadísticos en aplicaciones web interactivas sin requerir conocimientos avanzados de HTML, CSS o JavaScript. Git, por otro lado, es una herramienta esencial para el control de versiones que facilita el seguimiento de cambios y la colaboración en proyectos de programación.

Esta guía te ayudará a comprender los fundamentos de ambas tecnologías y cómo integrarlas en un flujo de trabajo eficiente para el desarrollo de aplicaciones de visualización de datos.

## Fundamentos de Shiny

### ¿Qué es Shiny?

Shiny es una biblioteca de R que permite crear aplicaciones web interactivas directamente desde R. Sus principales ventajas son:

- **Facilidad de uso:** No requiere conocimientos avanzados de desarrollo web
- **Interactividad:** Permite crear visualizaciones y dashboards reactivos
- **Integración completa con R:** Aprovecha todo el ecosistema de análisis de datos de R

### Instalación del Entorno

Para comenzar a trabajar con Shiny, necesitarás instalar:

```r
# Instalar paquetes esenciales
install.packages(c("shiny", "shinydashboard", "bslib", "thematic"))

# Paquetes recomendados para visualización
install.packages(c("plotly", "ggplot2", "DT", "leaflet"))
```

## Estructura Básica de una Aplicación Shiny

Toda aplicación Shiny consta de dos componentes principales:

1. **Interfaz de Usuario (UI)**: Define la apariencia de la aplicación
2. **Servidor**: Contiene la lógica y procesamiento de datos

### Ejemplo Mínimo Funcional

```r
# app.R
library(shiny)

# Definición de la interfaz de usuario
ui <- fluidPage(
  titlePanel("Mi Primera Aplicación Shiny"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("n", "Número de puntos:", min = 10, max = 1000, value = 100)
    ),
    mainPanel(
      plotOutput("scatterPlot")
    )
  )
)

# Definición del servidor
server <- function(input, output) {
  output$scatterPlot <- renderPlot({
    n <- input$n
    x <- rnorm(n)
    y <- rnorm(n)
    plot(x, y, main = paste(n, "puntos aleatorios"))
  })
}

# Crear y ejecutar la aplicación
shinyApp(ui = ui, server = server)
```

### Organización de Archivos

Existen dos formas principales de organizar una aplicación Shiny:

1. **Archivo único (`app.R`)**: Contiene tanto la UI como el servidor
2. **Archivos separados**: 
   - `ui.R`: Contiene solo la definición de la interfaz
   - `server.R`: Contiene solo la lógica del servidor
   - `global.R`: Código que se ejecuta una vez al iniciar la aplicación

Para aplicaciones más complejas, se recomienda la siguiente estructura de carpetas:

```
mi_app_shiny/
├── app.R (o ui.R y server.R)
├── global.R
├── www/                     # Archivos estáticos (CSS, imágenes, etc.)
├── R/                       # Funciones auxiliares en archivos .R separados
├── data/                    # Datos utilizados por la aplicación
└── docs/                    # Documentación
```

## Componentes Esenciales de la UI

### Layouts

Los layouts definen cómo se organizan los elementos en la pantalla:

```r
# Layout de página fluida
ui <- fluidPage(...)

# Layout con barra lateral
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(...),
    mainPanel(...)
  )
)

# Layout basado en cuadrícula
ui <- fluidPage(
  fluidRow(
    column(width = 4, ...),
    column(width = 8, ...)
  )
)

# Layout con pestañas
ui <- fluidPage(
  tabsetPanel(
    tabPanel("Gráfico 1", ...),
    tabPanel("Gráfico 2", ...),
    tabPanel("Tabla de datos", ...)
  )
)
```

### Widgets de Entrada

Los widgets permiten al usuario interactuar con la aplicación:

```r
# Widgets de entrada comunes
sliderInput("slider", "Selecciona un valor:", min = 0, max = 100, value = 50)
selectInput("select", "Elige una opción:", choices = c("A", "B", "C"))
checkboxInput("checkbox", "Mostrar detalles", value = FALSE)
dateRangeInput("dates", "Selecciona un rango de fechas:")
fileInput("file", "Sube un archivo:")
textInput("text", "Introduce un texto:")
```

### Elementos de Salida

Los elementos de salida muestran los resultados:

```r
# Elementos de salida comunes
plotOutput("grafico")    # Para gráficos estáticos de R
plotlyOutput("grafico")  # Para gráficos interactivos con plotly
tableOutput("tabla")     # Para tablas pequeñas
DTOutput("tabla")        # Para tablas interactivas con DT
leafletOutput("mapa")    # Para mapas interactivos
textOutput("texto")      # Para texto simple
htmlOutput("html")       # Para HTML formateado
```

## Sistema de Reactividad en Shiny

La reactividad es el concepto fundamental que permite que las aplicaciones Shiny respondan a las acciones del usuario.

### Expresiones Reactivas

```r
server <- function(input, output) {
  # Expresión reactiva: se recalcula solo cuando sus dependencias cambian
  filteredData <- reactive({
    dataset %>% 
      filter(year >= input$yearRange[1], year <= input$yearRange[2]) %>%
      filter(country %in% input$countries)
  })
  
  # Uso de la expresión reactiva
  output$plot <- renderPlot({
    ggplot(filteredData(), aes(x = year, y = value)) + 
      geom_line()
  })
  
  output$table <- renderTable({
    summary(filteredData())
  })
}
```

### Observadores y Efectos Secundarios

```r
# Observador: se ejecuta cuando sus dependencias cambian
observeEvent(input$saveButton, {
  # Código que se ejecuta cuando se presiona el botón
  saveData(input$userData)
  showNotification("¡Datos guardados!")
})

# Observador general: reacciona a cualquier cambio en sus dependencias
observe({
  req(input$dataset)  # Espera hasta que input$dataset no sea NULL
  updateSelectInput(session, "columns", 
                   choices = names(datasets[[input$dataset]]))
})
```

### Aislamiento y Control de la Reactividad

```r
# Aislar valores (no crea dependencia reactiva)
output$summary <- renderText({
  totalItems <- isolate(length(input$selection))
  paste("Has seleccionado", totalItems, "elementos")
})

# Retrasar la actualización para inputs que cambian frecuentemente
filteredData <- reactive({
  # Solo se actualiza 500ms después del último cambio
  input$searchText
  datasetFiltered <- dataset %>% filter(...)
}) %>% debounce(500)
```

## Control de Versiones con Git

Git es un sistema de control de versiones distribuido, esencial para el desarrollo de software colaborativo y la gestión de cambios en proyectos.

### Conceptos Básicos de Git

1. **Repositorio**: Espacio donde Git almacena el historial de cambios
2. **Commit**: Instantánea del proyecto en un momento dado
3. **Branch (Rama)**: Línea independiente de desarrollo
4. **Merge**: Combinación de cambios de diferentes ramas

### Configuración Inicial

```bash
# Configurar identidad
git config --global user.name "Tu Nombre"
git config --global user.email "tu.email@ejemplo.com"

# Configurar editor de texto para los commits
git config --global core.editor "code --wait"  # Para VS Code
```

### Comandos Fundamentales

```bash
# Iniciar un repositorio
git init

# Clonar un repositorio existente
git clone https://github.com/usuario/repositorio.git

# Verificar estado del repositorio
git status

# Añadir archivos al área de preparación
git add archivo.R          # Añadir archivo específico
git add .                  # Añadir todos los archivos

# Crear un commit
git commit -m "Descripción de los cambios"

# Ver historial de commits
git log
git log --oneline --graph  # Versión más visual
```

### Trabajo con Ramas

```bash
# Ver ramas existentes
git branch

# Crear nueva rama
git branch nombre-rama

# Cambiar a una rama
git checkout nombre-rama

# Crear y cambiar a una rama en un solo paso
git checkout -b nueva-rama

# Fusionar ramas
git checkout main         # Ir a la rama principal
git merge nombre-rama     # Fusionar cambios de nombre-rama a main
```

### Trabajo Remoto

```bash
# Añadir un repositorio remoto
git remote add origin https://github.com/usuario/repositorio.git

# Enviar cambios al repositorio remoto
git push origin main

# Obtener cambios del repositorio remoto
git pull origin main

# Ver repositorios remotos configurados
git remote -v
```

## Flujo de Trabajo Git para Proyectos Shiny

### Configuración de un Nuevo Proyecto

1. **Crear la estructura del proyecto Shiny**

```r
# En R, crear un proyecto con renv para gestión de dependencias
install.packages("renv")
renv::init()
```

2. **Inicializar el repositorio Git**

```bash
# En la terminal
cd ruta/a/mi_proyecto_shiny
git init
```

3. **Crear archivo .gitignore**

```
# Archivos a ignorar
.Rproj.user/
.Rhistory
.RData
.Ruserdata
*.Rproj
renv/library/
```

4. **Primer commit**

```bash
git add .
git commit -m "Configuración inicial del proyecto"
```

### Desarrollo Iterativo

Para cada nueva característica o corrección:

1. **Crear una rama para la característica**

```bash
git checkout -b feature/nueva-visualizacion
```

2. **Desarrollar la característica**

```r
# Añadir nuevo módulo o funcionalidad a tu app Shiny
```

3. **Hacer commits frecuentes**

```bash
# Después de implementar cada componente o función
git add .
git commit -m "Añadido gráfico de tendencias temporales"
```

4. **Fusionar los cambios a la rama principal**

```bash
git checkout main
git merge feature/nueva-visualizacion
```

### Colaboración en GitHub/GitLab

1. **Crear repositorio remoto** en GitHub/GitLab

2. **Vincular repositorio local con el remoto**

```bash
git remote add origin https://github.com/usuario/mi-app-shiny.git
```

3. **Subir cambios al repositorio remoto**

```bash
git push -u origin main
```

4. **Trabajar con Pull Requests**
   - Crear ramas para nuevas características
   - Hacer commits en estas ramas
   - Crear Pull Requests para revisar los cambios
   - Fusionar tras la revisión

## Técnicas Avanzadas y Mejores Prácticas

### Modularización en Shiny

```r
# Definición del módulo (en R/filtro_modulo.R)
filtroModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("variable"), "Variable:", choices = NULL),
    sliderInput(ns("rango"), "Rango:", min = 0, max = 100, value = c(0, 100))
  )
}

filtroModuleServer <- function(id, dataset) {
  moduleServer(id, function(input, output, session) {
    # Actualizar opciones basadas en el dataset
    observe({
      updateSelectInput(session, "variable", choices = names(dataset()))
    })
    
    # Devolver datos filtrados
    return(reactive({
      req(input$variable, input$rango)
      dataset() %>% 
        filter(.data[[input$variable]] >= input$rango[1],
               .data[[input$variable]] <= input$rango[2])
    }))
  })
}

# Uso del módulo en app.R
ui <- fluidPage(
  # Otros elementos UI...
  filtroModuleUI("filtros")
)

server <- function(input, output, session) {
  # Datos base
  datos <- reactive({ read.csv("datos.csv") })
  
  # Llamar al módulo
  datosFiltrados <- filtroModuleServer("filtros", datos)
  
  # Usar los datos filtrados
  output$grafico <- renderPlot({
    ggplot(datosFiltrados(), aes(x, y)) + geom_point()
  })
}
```

### Optimización del Rendimiento

```r
# Caché para operaciones costosas
datos_procesados <- reactive({
  # Procesamiento costoso aquí
  proceso_complicado(datos_iniciales)
}) %>% bindCache(input$parametro1, input$parametro2)

# Cargar datos grandes una sola vez
datos <- reactiveVal()
observeEvent(1, {
  # Se ejecuta solo una vez al iniciar
  datos(arrow::read_parquet("datos_grandes.parquet"))
}, once = TRUE)

# Retrasar actualizaciones para inputs que cambian frecuentemente
texto_filtrado <- reactive({
  input$texto_busqueda
  # Filtrado basado en el texto
}) %>% debounce(300)  # Espera 300ms de inactividad
```

### Pruebas Automatizadas para Shiny

```r
# Instalar paquete de pruebas
install.packages("shinytest2")

# En tests/test-basic.R
library(shinytest2)

test_that("La aplicación se inicia correctamente", {
  app <- AppDriver$new(app_dir = "../", name = "test-basic")
  
  # Comprobar que existe un elemento específico
  expect_true(app$get_js("$('#grafico').length > 0"))
  
  # Interactuar con la app
  app$set_inputs(slider = 75)
  app$click("boton")
  
  # Verificar resultados
  expect_equal(app$get_value(output = "resultado"), "75")
})
```

### Integración Continua con GitHub Actions

Archivo `.github/workflows/check-app.yaml`:

```yaml
name: Shiny App Check

on:
  push:
    branches: [ main ]
  pull_request:
    branches: [ main ]

jobs:
  check:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      
      - name: Set up R
        uses: r-lib/actions/setup-r@v2
        
      - name: Install dependencies
        run: |
          install.packages(c("shiny", "shinytest2", "testthat", "renv"))
          renv::restore()
        shell: Rscript {0}
        
      - name: Run tests
        run: |
          testthat::test_dir("tests/")
        shell: Rscript {0}
```

## Recursos Adicionales

### Para Shiny

- [Shiny Cheatsheet](https://shiny.rstudio.com/images/shiny-cheatsheet.pdf)
- [Galería de ejemplos Shiny](https://shiny.rstudio.com/gallery/)