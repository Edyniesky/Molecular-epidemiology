##%######################################################%##
#                                                          #
####                     Packages                       ####
#                                                          #
##%######################################################%##


library(scales)
library(formatR)
library(leaflet)
library(leaflet.extras)
library(htmltools)
library(htmlwidgets)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(dashboardthemes)
library(readr)
library(tidyverse)
library(lubridate)
library(plotly)
library(DT)
library(ggdark)
library(bslib)

library(sparkline)
library(kableExtra)
library(formattable)

library(ggstream)
library(viridis)
library(hrbrthemes)
library(d3heatmap)
library(RColorBrewer)
#   __________________ #< beecd253476d735f7a42137013eae967 ># __________________
#   Database cleaning                                                       ####




metadata <- read_delim("nextstrain_ncov_global_metadata1.tsv",  "\t", escape_double = FALSE, trim_ws = TRUE)

## Selection of the Pongo lineage
pongo <- (metadata) %>%
    filter(Country == "Brazil") %>% 
    select(`PANGO Lineage`) %>% 
    distinct(`PANGO Lineage`) %>% 
    as.vector() %>% 
    print()

dataPong2 <- metadata %>% 
    filter(Country == "Brazil") %>% 
    group_by(`PANGO Lineage`, `Admin Division`) %>% # Admin Division
    count(`PANGO Lineage`, `Admin Division`) %>% 
    ungroup() %>% 
    rename(total = n, Linage = `PANGO Lineage`, Estado = `Admin Division`) %>% 
    mutate(Estado = ifelse(Estado == "Amazonas BR", "Amazonas", Estado)) %>% 
    select(Estado, Linage, total) %>% 
    spread(Linage, total, fill = 0) %>% 
    column_to_rownames(var = "Estado") %>% 
    print()


#   __________________ #< 206cc2ad885ede0b5041c3afdc0e1ac5 ># __________________
#   Function                                                                ####

server <- function(input, output) {
    
    dataPong <- reactive({
        metadata %>% 
            filter(Country == "Brazil", `PANGO Lineage` %in% c(input$pango), 
                   `Collection Data` >= input$dateSelect[1] & 
                    `Collection Data` <= input$dateSelect[2]) %>% 
            group_by(`Collection Data`, `PANGO Lineage`) %>% # Admin Division
            count(`PANGO Lineage`) %>% 
            ungroup() %>% 
            rename(data = `Collection Data`, total = n, Linage = `PANGO Lineage`)
        })
    
    output$plot1 <-  renderPlotly({
        
        plot <- dataPong() %>% 
            ggplot(aes(x = data, y  = total, fill = Linage, text = Linage)) + 
            geom_stream(type = "proportional", bw =  0.75, extra_span = 0.1, color = "white", 
                        alpha = 1, size = 0.1) +
            scale_fill_viridis(discrete = TRUE, option = "B") +
            scale_y_percent() +
            labs(x = "Data", y = "Frequência", fill = "Linhagem",
                 subtitle = paste("ARKEA das Archaeas:", today()),
                 caption = "https://nextstrain.org/ncov/global?c=location&lang=es") +
            theme_modern_rc(base_size = 12, axis_title_size = 12, ticks = TRUE)
            
        
        ggplotly(plot, tooltip = "text")
        
    })
    
    output$plot2 <-  renderD3heatmap({
        
       plot2i <- d3heatmap(
            dataPong2,
            colors = colorRampPalette(rev(brewer.pal(11, "PRGn")))(256),
            revC = TRUE,
            scale = 'none',
            k_col = 4,
            k_row = 4,
            show_grid = FALSE,
            #cexRow = 1,
            cexCol = 0.75,
            digits = 20,
            theme = "dark")
       
       #if (input$grid) {
        #   plot2i <- plot2i + d3heatmap(dataPong2, show_grid = TRUE)
       #}
       
    })
    
    
    
    utput$Res3 <- renderUI({
        
        metadata %>% 
            clean_names() %>% 
            filter(region != "Alexandr Shevtsov et al") %>% 
            select(region, country, pango_lineage) %>% 
            group_by(region, country) %>% 
            count(pango_lineage) %>% 
            ungroup() %>% 
            group_nest(region, country) %>% 
            mutate(
                mean = map(data, ~round(mean(.x$n), digits = 2)),
                sd = map(data, ~round(sd(.x$n), digits = 1)),
                fig = map(data, ~spk_chr(.x$n, type = 'box', boxFillColor = '#FFF8DC', lineWidth = 1.5))) %>% 
            unnest(c(data, mean, sd, fig)) %>% 
            rename(Região = region, País = country, `Número total de seqüências` = n, 
                   Média = mean, `Desvio padrão` = sd, `Distribuição dos dados` = fig, Linhagens = pango_lineage) %>% 
            format_table(align = c('l', 'l', 'l', 'c', 'c', 'c', 'r')) %>% 
            htmltools::HTML() %>%
            div() %>%
            spk_add_deps() %>%
            {column(width = 12, .)}
        
        res3
        
    })
    
    
    
    
     }

#   __________________ #< 1800f744d8f9aeaf319e084fbcb9b6bd ># __________________
#   Header                                                                  ####
    
header <- dashboardHeader(
    
    title = shinyDashboardLogo(
        theme = "flat_red",
        boldText = "Vigilância Molecular (ARKEA)",
        mainText = "App",
        badgeText = "v0.0.1"
        ),
    
    titleWidth = 360
)


#   __________________ #< 614893309be85aaf97989a89fbc667f9 ># __________________
#   Sidebar                                                                 ####

sidebar <- dashboardSidebar(
    width = "360px",
    collapsed = FALSE,
    
    sidebarMenu(
        
        menuItem(
            text = 'Frequência de Linhagem (PANGO)',
            icon = icon('fas fa-chart-bar'),
            
            selectInput(
                inputId = 'pango',
                label = tags$h5(HTML('<strong>PANGO</strong>')),
                choices = pongo$`PANGO Lineage`,
                multiple = TRUE,
                selected =  pongo$`PANGO Lineage` #c('P.2', 'B.1.1.28','B.1.1.33')
                ),
            
            tags$p(HTML("<br>Permite escolher uma ou mais linhagem"), style = "color:#000080"),
            
            dateRangeInput(
                inputId = 'dateSelect',
                label = tags$h5(HTML('<strong>PERIODO</strong>')),
                start = '2020-02-25',
                end = '2020-12-01',
                min = '2020-02-25',
                max = '2021-04-08',
                format = 'yyyy-mm-dd',
                startview = 'year',
                weekstart = 1,
                language = 'pt-BR',
                separator = tags$strong('Até'),
                width = '400px',
                autoclose = TRUE
                ),
            tags$p(HTML("<br>Permite filtrar os dados a partir de duas datas</br> adicionadas 
                        manualmente ou selecionadas</br> diretamente na caixa de diálogo."), 
                   style = "color:#000080")
            )
        )
    )


#   __________________ #< dfecf5d7f80c1a4e3e88607e968224af ># __________________
#   Body                                                                    ####


body <- dashboardBody(
    
    shinyDashboardThemes(
        theme = "purple_gradient"),
    
    fluidPage(
        
        tabsetPanel(
            selected = 'Linhagem (PANGO)',
            
            tabPanel(
                title = 'Linhagem (PANGO)',
                icon = icon('fas fa-chart-bar'),
                
                box(
                    title = "Figura A: Frequência de Linhagem (PANGO) no Brasil",
                    status = 'danger',
                    #background = 'black',
                    solidHeader = FALSE,
                    width = 6,
                    height = 640,
                    
                    plotlyOutput(
                        outputId = 'plot1',
                        width = "100%",
                        height = "585px",
                        inline = TRUE,
                        reportTheme = TRUE
                        )
                    ),
                box(
                    title = "Figura B: Número de linhagens (PANGO) por estados da Federação",
                    status = 'danger',
                    solidHeader = FALSE,
                    width = 6,
                    height = 640,
                    
                    d3heatmapOutput(
                        outputId = 'plot2', 
                        width = "100%", 
                        height = "585px"
                        )
                    )
                ),
            
            tabPanel(
                title = "Estatística descritiva",
                icon = icon('fas fa-chart-line'),
                
                box(
                    width = 12,
                    status = 'danger',
                    solidHeader = FALSE
                    )
                )
            )
        )
    )



##  .................. #< 933543121e490d5078311bfd9a747cae ># ..................
##  ui and shinyApp                                                         ####




ui <- dashboardPage(
    header = header,
    sidebar = sidebar,
    body = body
)



shinyApp(ui, server)

