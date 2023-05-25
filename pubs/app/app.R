# Libraries, functions & parameters
source("code/helpers.R")
source("code/param.R")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# User interface
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "yeti", version = 5),
  titlePanel("CovidFN"),
  sidebarLayout(
    # Menu 
    sidebarPanel(
      selectInput(
        "datasel", 
        label = "Select dataset (only 1 for now)", 
        choices = c("First Nations"),#"Canadian geographical names"), 
        multiple = FALSE, 
        selected = "First nations"
      ),
      br(),
      hr(),
      selectInput(
        "y", 
        label = "Y: COVID deaths or cases", 
        choices = c("Cases","Deaths"), 
        multiple = FALSE, 
        selected = "Cases"
      ),
      br(),
      sliderInput(
        "per",
        "Period covered",
        min = as.Date("2020-01-01","%Y-%m-%d"),
        max = as.Date("2023-05-01","%Y-%m-%d"),
        value=c(as.Date("2021-10-01","%Y-%m-%d"), as.Date("2022-04-01","%Y-%m-%d")),
        timeFormat="%Y-%m-%d"#,
        # step = ?
      ),
      br(),
      hr(),
      selectInput(
        "x", 
        label = "X: Indicators of social vulnerability", 
        choices = indicators$names, 
        multiple = FALSE, 
        selected = "Indigenous identity"
      ),
      width = 3
    ),
     
    # Main panel of application 
    mainPanel(
      tabsetPanel(
        # Panel 1 
        tabPanel("Scatterplot",
          plotOutput("scatter", width = '95%', height = '700px')
        ),
  
        # Panel 2
        tabPanel("Linear model",
          # dataTableOutput("table")
          h4("Adjusted R2"),
          textOutput("r2"),
          hr(),
          br(),
          h4("Table of coefficients"),
          dataTableOutput("linear_model")
        ),
        
        # Panel 3
        tabPanel("COVID map",
          leafletOutput("mapy", width = "100%", height = "85vh")
        ),
        
        # Panel 4
        tabPanel("Indicator map",
          leafletOutput("mapx", width = "100%", height = "85vh")
        )

      ),
      width = 9
    )
  )  
)

server <- function(input, output, session) {
  ydat <- reactive({
    idCols(pts, input$y, input$per)
  })
  
  xdat <- reactive({
    nm <- indicators$dotfiles[indicators$names == input$x]
    pts[,nm]
  })
  
  ygeo <- reactive({
    geoCovid(covid$tiffiles, input$y, input$per)
  })
  
  xgeo <- reactive({
    nm <- indicators$tiffiles[indicators$names == input$x]
    stars::read_stars(paste0("data/tifs/",nm))
  })
  
  lmdat <- reactive({
    data.frame(
      y = ydat(),
      x = xdat()
    ) |>
    na.omit()
  })
  
  lmmod <- reactive({
    summary(lm(lmdat()$y ~ lmdat()$x))
  })
  
  output$scatter <- renderPlot({
    plot(
      y = lmdat()$y,
      x = lmdat()$x,
      xlab = input$x, 
      ylab = paste0(input$y, " - ", input$per[1],"-", input$per[2]),
      pch = 20,
      col = "#1262b1",
      cex = 1.5,
      cex.lab = 1.5
    )
  })
  
  output$linear_model <- renderDataTable({
    dat <- as.data.frame(lmmod()$coefficients)
    dat$Variables <- rownames(dat)
    dat <- dplyr::select(dat, Variables, dplyr::everything())
  },
  options = list(pageLength = 10)
  )

  output$r2 <- renderText(lmmod()$adj.r.squared)
  
  output$mapy <- renderLeaflet({
    # Color palette
    rgeo <- range(ygeo()[[1]], na.rm=T)
    pal <- leaflet::colorNumeric(
      viridis::viridis_pal(option = "D")(100), 
      domain = rgeo
    )
  
    # Map
    leaflet(ygeo()) |> 
    setView(lng = -90, lat = 70, zoom = 3) |>
    addProviderTiles("CartoDB.Positron") |>
    addStarsImage(
      opacity = 1,
      color = viridis::viridis(100)
    ) |>
    addLegend(
      position = "bottomright",
      pal = pal,
      values = seq(rgeo[1], rgeo[2], length.out = 5),
      opacity = 1,
      title = ""
      )
  })
  
  output$mapx <- renderLeaflet({
    # Color palette
    rgeo <- range(xgeo()[[1]], na.rm=T)
    pal <- leaflet::colorNumeric(
      viridis::viridis_pal(option = "D")(100), 
      domain = rgeo
    )
  
    # Map
    leaflet(xgeo()) |> 
    setView(lng = -90, lat = 70, zoom = 3) |>
    addProviderTiles("CartoDB.Positron") |>
    addStarsImage(
      opacity = 1,
      color = viridis::viridis(100)
    ) |>
    addLegend(
      position = "bottomright",
      pal = pal,
      values = seq(rgeo[1], rgeo[2], length.out = 5),
      opacity = 1,
      title = ""
      )
  })
}

shinyApp(ui, server)