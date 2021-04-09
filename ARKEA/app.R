##%######################################################%##
#                                                          #
####                     Packages                       ####
#                                                          #
##%######################################################%##


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
library(janitor)
#   __________________ #< beecd253476d735f7a42137013eae967 ># __________________
#   Database cleaning                                                       ####




metadata <- read_delim("nextstrain_ncov_global_metadata1.tsv",  "\t", escape_double = FALSE, trim_ws = TRUE) %>% 
    filter(Region != "Alexandr Shevtsov et al")

## Selection of the Pongo lineage
pongo <- (metadata) %>%
    filter(Country == "Brazil") %>% 
    select(`PANGO Lineage`) %>% 
    distinct(`PANGO Lineage`) %>% 
    as.vector() 

## Selection countries
list.countries <- metadata %>% 
    select(Country) %>% 
    distinct(Country) %>% 
    as.vector()



dataPong2 <- metadata %>% 
    filter(Country == "Brazil") %>% 
    group_by(`PANGO Lineage`, `Admin Division`) %>% # Admin Division
    count(`PANGO Lineage`, `Admin Division`) %>% 
    ungroup() %>% 
    rename(total = n, Linage = `PANGO Lineage`, Estado = `Admin Division`) %>% 
    mutate(Estado = ifelse(Estado == "Amazonas BR", "Amazonas", Estado)) %>% 
    select(Estado, Linage, total) %>% 
    spread(Linage, total, fill = 0) %>% 
    column_to_rownames(var = "Estado")

Sta.d <- metadata %>% 
    clean_names() %>% 
    filter(region != "Alexandr Shevtsov et al") %>% 
    select(region, country, pango_lineage)

### Spatial data 
cent <- read_delim("Export_Output.txt", ";", escape_double = FALSE, trim_ws = TRUE)


gis.data <- metadata %>% 
    filter(Country == "Brazil") %>% 
    mutate(`Admin Division` = str_to_upper(`Admin Division`),
           `Admin Division` = ifelse(`Admin Division` == "AMAZONAS BR", "AMAZONAS", `Admin Division`),
           `Admin Division` = ifelse(`Admin Division` == "ESPIRITO SANTO", "ESPÍRITO SANTO", `Admin Division`),
           `Admin Division` = ifelse(`Admin Division` == "AMAPA", "AMAPÁ", `Admin Division`),
           `Admin Division` = ifelse(`Admin Division` == "PARAIBA", "PARAÍBA", `Admin Division`),
           `Admin Division` = ifelse(`Admin Division` == "PARA", "PARÁ", `Admin Division`)) %>% 
    clean_names() %>% 
    select(strain, country, admin_division, age, sex, pango_lineage, clade, originating_lab, collection_data, 
           originating_lab, author) %>% 
    print()

gis.data <- left_join(gis.data, cent, by = c("admin_division" = "NM_ESTADO"))

gis.datai <- gis.data %>% 
    select(strain, country, admin_division, age, sex, pango_lineage, clade, originating_lab, collection_data, 
           originating_lab, author, x_cent, y_cent) %>% 
    group_by(admin_division, x_cent, y_cent) %>% 
    count(pango_lineage) %>% 
    spread(pango_lineage, n, fill = 0) %>% 
    print()






### Format for sparkline and formattable
unit.scale = function(x) (x - min(x)) / (max(x) - min(x))
customGreen0 <- "#CD2626"
customGreen <- "#EE5C42"


#   __________________ #< 206cc2ad885ede0b5041c3afdc0e1ac5 ># __________________
#   Function                                                                ####

server <- function(input, output) {
    
### Lineage distribution 
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
    
### Lineages by states of the federation
    
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
    })
    
### Descriptive statistics 

    sta.data <- reactive({
        metadata %>% 
            clean_names() %>% 
            filter(region == input$regiao) %>% 
            select(country, pango_lineage) %>% 
            group_by(country, .add = TRUE) %>% 
            count(pango_lineage) %>% 
            select(country, n) %>% 
            group_nest(country) %>% 
            mutate(
                mean = map(data, ~round(mean(.x$n), digits = 2)),
                sd = map(data, ~round(sd(.x$n), digits = 1)),
                suma = map(data, ~round(sum(.x$n), digits = 1)),
                fig = map(data, ~spk_chr(.x$n, type = 'bar', barColor = '#FFD700'))) %>% 
            unnest(c(suma, mean, sd, fig)) %>%
            distinct() %>% 
            rename(País = country, `Número total de sequenciamentos` = suma, 
                   Média = mean, `Desvio padrão` = sd, `Distribuição dos dados` = fig) %>%
            select(-data) %>% 
            select(País, `Número total de sequenciamentos`, Média, `Desvio padrão`, `Distribuição dos dados`) %>% 
            arrange(desc(`Número total de sequenciamentos`))
    })
    
    output$Res3 <- renderUI({
        
        res3 <- sta.data() %>% 
            format_table(pretty_names = TRUE,
                         align = c('l', 'l', 'c', 'c', 'c'), 
                         list(`Número total de sequenciamentos` = color_bar("#FA614B66", fun = unit.scale),
                              c("#66CDAA", "#458B74"),
                              #`Média` = color_tile(customGreen, customGreen0),
                              #`Desvio padrão` = color_tile(customGreen, customGreen0),
                         p_digits = "scientific")) %>% 
            htmltools::HTML() %>%
            div() %>%
            spk_add_deps() %>%
            {column(width = 12, .)}
        
        res3
    })
    
### Spatial data 
    output$map <- renderLeaflet({
        
        colors <- c("#7FFFD4", "#8A2BE2", "#1874CD", "#66CD00", "#EE2C2C", "#EEC900", "#FF6EB4", "#FF8247", "#00008B", "#8B3626")
        
        leaflet() %>% 
            addTiles() %>% 
            addMinicharts(gis.datai$x_cent, 
                          gis.datai$y_cent, 
                          type = "pie", 
                          chartdata = gis.datai[, c('B.1', 'B.1.1', 'B.1.1.28', 'B.1.1.33', 'B.1.1.378', 'B.1.195', 'B.40', 
                                                    'P.1', 'P.2')],
                          colorPalette = colors,
                          opacity = 0.8,
                          width = 45, 
                          height = 45,
                          legend = TRUE
                          ) %>% 
            addScaleBar(position = "bottomleft") %>% 
            addEasyButton(easyButton(
                icon = "fa-globe", title = "Aumentar ao nível 4",
                onClick = JS("function(btn, map){ map.setZoom(4); }"))) %>%
            addEasyButton(easyButton(
                icon = "fa-crosshairs", title = "Localize-me",
                onClick = JS("function(btn, map){ map.locate({setView: true}); }")))
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
                selected =  pongo$`PANGO Lineage`
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
            ),
        
        menuItem(
            text = 'Estatística descritiva',
            icon = icon('fas fa-chart-line'),
            
            selectInput(
                inputId = 'regiao',
                label = tags$h5(HTML('<strong>Região</strong>')),
                choices = list('Asia', 'Europe', 'Oceania', 'South America', 'North America', 'Africa'),
                multiple = FALSE,
                selected = 'South America'
                ),
            tags$p(HTML("<br>Permite escolher uma Região"), style = "color:#000080") 
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
            selected = 'Frequência de Linhagem (PANGO)',
            
            tabPanel(
                title = 'Frequência de Linhagem (PANGO)',
                icon = icon('fas fa-chart-bar'),
                fluidRow(
                
                
                box(
                    title = "Figura A: Frequência de Linhagem (PANGO) no Brasil",
                    status = 'danger',
                    #background = 'black',
                    solidHeader = FALSE,
                    width = 12,
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
                    width = 12,
                    height = 640,
                    
                    d3heatmapOutput(
                        outputId = 'plot2', 
                        width = "100%", 
                        height = "585px"
                        )
                    )
                )),
            
            tabPanel(
                title = "Estatística descritiva",
                icon = icon('fas fa-chart-line'),
                
                box(
                    title = "Tabela A: Estatísticas descritivas para o número de seqüências por país",
                    width = 12,
                    status = 'danger',
                    solidHeader = FALSE,
                    
                    htmlOutput('Res3')
                    )
                ),
             
            tabPanel(
                title = 'Dados espaciais',
                icon = icon('globe'),
                
                fluidPage( 
                
                box(
                    title = "Figura C: Distribuição espacial da linhagem Pongo pelos estados brasileiros",
                    width = 12,
                    status = 'danger',
                    solidHeader = FALSE,
                    
                    leafletOutput('map', height = 800)
                    
                    )
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

