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
library(shinythemes)
#library(ggdark)
#library(bslib)


library(sparkline)
library(kableExtra)
library(formattable)

library(ggstream)
library(viridis)
library(hrbrthemes)
library(iheatmapr)
#library(RColorBrewer)
library(janitor)
library(leaflet.minicharts)
library(ggthemes)
library(shinyjs)
library(readxl)
library(sf)

#   __________________ #< beecd253476d735f7a42137013eae967 ># __________________
#   Database cleaning                                                       ####




metadata <- read_delim("nextstrain_ncov_global_metadata.tsv",  "\t", escape_double = FALSE, trim_ws = TRUE) %>% 
    filter(Region != "Alexandr Shevtsov et al")

entropy <- read_delim("nextstrain_ncov_global_diversity.tsv", "\t", escape_double = FALSE, trim_ws = TRUE)

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

dataPong2i <- data.matrix(dataPong2, rownames.force = TRUE)


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
           `Admin Division` = ifelse(`Admin Division` == "PARA", "PARÁ", `Admin Division`),
           `Admin Division` = ifelse(`Admin Division` == "RONDONIA", "RONDÔNIA", `Admin Division`)) %>% 
    clean_names() %>% 
    select(strain, country, admin_division, age, sex, pango_lineage, clade, originating_lab, collection_data, 
           originating_lab, author)

table <- metadata %>% 
    #filter(Country == "Brazil") %>% 
    mutate(`Admin Division` = str_to_upper(`Admin Division`),
           `Admin Division` = ifelse(`Admin Division` == "AMAZONAS BR", "AMAZONAS", `Admin Division`),
           `Admin Division` = ifelse(`Admin Division` == "ESPIRITO SANTO", "ESPÍRITO SANTO", `Admin Division`),
           `Admin Division` = ifelse(`Admin Division` == "AMAPA", "AMAPÁ", `Admin Division`),
           `Admin Division` = ifelse(`Admin Division` == "PARAIBA", "PARAÍBA", `Admin Division`),
           `Admin Division` = ifelse(`Admin Division` == "PARA", "PARÁ", `Admin Division`),
           `Admin Division` = ifelse(`Admin Division` == "RONDONIA", "RONDÔNIA", `Admin Division`)) %>% 
    clean_names() %>% 
    select(strain, country, admin_division, age, sex, pango_lineage, clade, originating_lab, collection_data, 
           originating_lab, author) %>% 
    rename(Linhagem = strain, País = country, Estados = admin_division, Idade = age, Sexo = sex, `Linhagem (Pongo)` = pango_lineage,
           Clade = clade, `laboratório de origem` = originating_lab, `Data de coleta` = collection_data, 
           Autor = author)

gis.data <- left_join(gis.data, cent, by = c("admin_division" = "NM_ESTADO"))

gis.datai <- gis.data %>% 
    select(strain, country, admin_division, age, sex, pango_lineage, clade, originating_lab, collection_data, 
           originating_lab, author, x_cent, y_cent) %>% 
    group_by(admin_division, x_cent, y_cent) %>% 
    count(pango_lineage) %>% 
    spread(pango_lineage, n, fill = 0) %>% 
    ungroup() 

col.name <- gis.datai %>% 
  select(!c(admin_division, x_cent, y_cent)) 


### Format for sparkline and formattable
unit.scale = function(x) (x - min(x)) / (max(x) - min(x))
customGreen0 <- "#87CEFF"
customGreen <- "#BFEFFF"

data <- metadata %>% 
  clean_names() %>% 
  #filter(country == "Brazil") %>% 
  select(collection_data) %>% 
  mutate(collection_data = as_date(collection_data)) %>% 
  arrange(desc(collection_data)) %>% 
  mutate(collection_data = first(collection_data)) %>% 
  distinct() %>% 
  mutate(collection_data = format(as.Date(collection_data), format = "%d %B %Y"))


### Pernambuco data 

report <- read_excel("report.xlsx") %>% 
  clean_names()

report1 <- report %>% 
  select(data_extracao, localizacao, lineage) %>% 
  mutate(across( where(is.character), ~replace_na(., "NÃO INFORMADO"))) %>% 
  filter(lineage != "NA") %>% 
  rename(Municipio = localizacao, data = data_extracao) %>% 
  group_by(Municipio) %>% 
  count(lineage) %>% 
  print()


report1i <- report1 %>% 
  select(Municipio, lineage, n) %>% 
  spread(lineage, n, fill = 0) %>% 
  column_to_rownames(var = "Municipio") %>% 
  print()

report1i <- data.matrix(report1i, rownames.force = TRUE)


# mapa PE 
Cent <- read_excel("Cent.xls")

Cent <- Cent %>% 
  filter(Sigla == "PE") %>% 
  select(Municipio,  Point_x, Point_y) %>% 
  mutate(Municipio = stringr::str_to_upper(Municipio)) %>% 
  print()


report2 <- left_join(report1, Cent, by = "Municipio")


report2i <- report2 %>% 
  select(Point_x, Point_y, lineage, n) %>% 
  spread(lineage, n, fill = 0) %>% 
  ungroup() %>% 
  print()

col.name1 <- report2i %>% 
  select(!( Municipio:Point_y)) %>% 
  print()


estPE <- st_read("estadoPE.shp", quiet = TRUE) 
munPE <- st_read("municipioPE1.shp", quiet = TRUE)






#   __________________ #< 206cc2ad885ede0b5041c3afdc0e1ac5 ># __________________
#   Function                                                                ####

server <- function(input, output) {
  
    runjs({'
        var el2 = document.querySelector(".skin-blue");
        el2.className = "skin-blue sidebar-mini";
        var clicker = document.querySelector(".sidebar-toggle");
        clicker.id = "switchState";
    '})
    
    onclick('switchState', runjs({'
        var title = document.querySelector(".logo")
        if (title.style.visibility == "hidden") {
          title.style.visibility = "visible";
        } else {
          title.style.visibility = "hidden";
        }
  '}))
    
    output$date <- renderUI({
      paste("Última actualização", data$collection_data, sep = ": ")
    })
    
    
    
    
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
            ggplot(aes(x = data, y  = total, fill = Linage)) + #text = total
            geom_stream(type = "proportional", bw =  0.75, extra_span = 0.1, color = "white", 
                        alpha = 1, size = 0.1) +
            scale_fill_viridis(discrete = TRUE, option = "B") +
            #scale_y_percent() +
            labs(x = "Data", y = "Frequência", fill = "Linhagem",
                 subtitle = paste("ARKEA das Archaeas:", today()),
                 caption = "https://nextstrain.org/ncov/global?c=location&lang=es") +
            #theme_modern_rc(base_size = 12, axis_title_size = 12, ticks = TRUE)
            theme_hc(base_size = 14) +
            theme(axis.text = element_text(color = "dimgray", size = 14), axis.text.x = element_text(angle = 35, 
                hjust = 1)) +
            scale_x_date(date_labels = "%d %b %Y", date_breaks = "21 day")
        
        ggplotly(plot) #tooltip = "text"
        
    })
    
### Lineages by states of the federation
    
    output$plot2 <- renderIheatmap({
        
        plot2i <- main_heatmap(dataPong2i, name = "Número de linhagens") %>% 
            add_col_clustering(k = 4, name = "Agrupamento<br>das colunas") %>%
            add_row_clustering(k = 4, name = "Agrupamento<br>das linhas") %>%
            add_col_labels() %>% 
            add_row_labels() %>% 
            add_col_summary(layout = list(zeroline = TRUE, title = "Média (Linhagem)", font = list(size = 1)), 
                            colors  = "#104E8B") %>% 
            #add_col_title("Measles Cases from 1930 to 2001", side = "top") %>% 
            add_row_summary(groups = FALSE, title = "Média das linhas (Estados)",
                            type = "bar", colors  = "#104E8B", layout = list(title = "Média<br>(Estados)",font = list(size = 8))) # 
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
                fig = map(data, ~spk_chr(.x$n, type = 'bar', barColor = '#104E8B'))) %>% 
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
                              `Média` = color_tile(customGreen, customGreen0),
                              `Desvio padrão` = color_tile(customGreen, customGreen0),
                         p_digits = "scientific")) %>% 
            htmltools::HTML() %>%
            div() %>%
            spk_add_deps() %>%
            {column(width = 12, .)}
        
        res3
    })
    
### entropy 
### 
    gen <- reactive({
        entropy %>% 
            filter(gene %in% c(input$gene))
    })
    
    output$plot3i <-  renderPlotly({
        
        plot3 <- gen() %>%
            mutate(gene = factor(gene, levels = c('ORF1a', 'ORF1b', 'S', 'ORF3a', 'E', 'M', 'ORF6', 'ORF7a', 'ORF7b', 
                                                  'ORF8', 'ORF9b', 'N'), ordered = TRUE)) %>% 
            plot_ly(
                x = ~gene,
                y = ~entropy,
                split = ~gene,
                type = 'violin',
                box = list(visible = TRUE),
                meanline = list(visible = TRUE)) %>% 
            layout(
                xaxis = list(title = "Day"),
                yaxis = list(title = "Entropia", zeroline = FALSE))
    })
    
### Spatial data 
    output$map <- renderLeaflet({
      
      colors <- c("#EE7942", "#7FFFD4", "#838B8B", "#0000FF", "#8A2BE2", "#A52A2A", "#EE3B3B", "#8EE5EE", "#7FFF00", "#FF7F50", "#6495ED", "#FFB90F", "#556B2F", "#20B2AA", "#FFF68F", "#FF69B4", "#36648B", "#8B5A2B", "#551A8B")
      
      leaflet() %>% 
        addTiles() %>% 
        addMiniMap(tiles = providers$Esri.WorldStreetMap,
                   toggleDisplay = TRUE,
                   zoomLevelOffset = -8,
                   zoomAnimation = TRUE) %>%
        addMinicharts(gis.datai$x_cent, 
                      gis.datai$y_cent, 
                      type = "pie", 
                      opacity = .89,
                      chartdata = col.name,
                      colorPalette = colors,
                      #legendPosition = "topleft",
                      width = 45, 
                      height = 45
        ) %>% 
        
        #addGraticule(group = "Graticule", interval = 5, sphere = FALSE, 
        #style = list(color = "blue", weight = 1)) %>%
        addMeasure(secondaryLengthUnit = 'kilometers',
                   secondaryAreaUnit = 'sqmeters',
                   localization = 'pt_BR') %>%
        addScaleBar(position = "bottomleft") %>% 
        addLayersControl(overlayGroups = c("Graticule"), # , "daylight"
                         options = layersControlOptions(collapsed = TRUE)) %>% 
        
        addEasyButton(easyButton(
          icon = "fa-globe", title = "Aumentar ao nível 4",
          onClick = JS("function(btn, map){ map.setZoom(4); }"))) %>%
        addEasyButton(easyButton(
          icon = "fa-crosshairs", title = "Localize-me",
          onClick = JS("function(btn, map){ map.locate({setView: true}); }")))
    })
    
    # Table  
    output$tabela <- renderDataTable({
        DT::datatable(
            table,
            style = 'bootstrap',
            extensions = 'Buttons',
            callback = JS('table.page("next").draw(false);'),
            filter = 'top',
            escape = TRUE,
            options = list(
                deferRender = TRUE,
                pageLength = 25,
                autoWidth = TRUE,
                dom = 'Blfrtip', 
                buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                lengthMenu = list(c(10 , 25, 50, -1), c(10, 25, 50, "All")))
        )
    })
    
    ##### Pernambuco
    
    output$plotPE <- renderIheatmap({
      
      plotPE1 <- main_heatmap(report1i, name = "Número de linhagens", colors = "Blues") %>% 
        add_col_clustering(k = 4, name = "Agrupamento<br>das colunas") %>%
        add_row_clustering(k = 4, name = "Agrupamento<br>das linhas") %>%
        add_col_labels() %>% 
        add_row_labels(font = list(size = 8)) %>% # size = 0.3,font = list(size = 6)
        add_col_summary(layout = list(zeroline = TRUE, title = "Média (Linhagem)", font = list(size = 5)), 
                        colors  = "#104E8B") %>% 
        #add_col_title("Measles Cases from 1930 to 2001", side = "top") %>% 
        add_row_summary(groups = FALSE, title = "Média das linhas (Município)",
                        type = "bar", colors  = "#104E8B", layout = list(title = "Média<br>(Município)",font = list(size = 6))) # 
    })
    
    
    output$map2i <- renderLeaflet({
      
      colors1 <- c("#ADFF2F", "#8B8B83", "#FFA500", "#1E90FF", "#FFD700", "#EE2C2C", "#8470FF", "#006400")
      
      map2 <- leaflet() %>% 
        addTiles() %>% 
        addMiniMap(tiles = providers$Esri.WorldStreetMap,
                   toggleDisplay = TRUE,
                   zoomLevelOffset = -8,
                   zoomAnimation = TRUE) %>%
        addProviderTiles(providers$CartoDB.VoyagerNoLabels) %>%
        leaflet::addPolygons(
          data = estPE,
          #color = "#E0EEEE",
          weight = 3.5, 
          opacity = .7,  
          fillColor = "#E0EEEE",
          label = ~NM_ESTADO) %>% 
        leaflet::addPolygons(
          data = munPE,  
          weight = 0.8, 
          dashArray = 1,
          color = "black",
          opacity = 0.6,
          fillOpacity = 0.2,
          fillColor = "white",
          highlightOptions = highlightOptions(color = "red", 
                                              weight = 1.5,
                                              bringToFront = FALSE),
          label = ~NM_MUNICIP) %>% 
        addMeasure(secondaryLengthUnit = 'kilometers',
                   secondaryAreaUnit = 'sqmeters',
                   localization = 'pt_BR') %>%
        addScaleBar(position = "bottomleft") %>% 
        addEasyButton(easyButton(
          icon = "fa-globe", title = "Aumentar ao nível 4",
          onClick = JS("function(btn, map){ map.setZoom(4); }"))) %>%
        addEasyButton(easyButton(
          icon = "fa-crosshairs", title = "Localize-me",
          onClick = JS("function(btn, map){ map.locate({setView: true}); }"))) %>% 
        addMinicharts(report2i$Point_x, 
                      report2i$Point_y, 
                      type = "pie", 
                      chartdata = col.name1,
                      colorPalette = colors1, 
                      opacity = 0.8,
                      width = 45, 
                      height = 30,
                      legend = TRUE)  
        
    })
    
     }

#   __________________ #< 1800f744d8f9aeaf319e084fbcb9b6bd ># __________________
#   Header                                                                  ####
    
header <- dashboardHeader(
    
    title = shinyDashboardLogo(
        theme = "flat_red",
        boldText = "ARKEA DX Vigilância Molecular",
        mainText = "",
        badgeText = "v0.0.2"
        ),
    
    titleWidth = 360,
    disable = FALSE,
    
    tags$li(a(href = 'https://www.irrd.org/',
              img(src = "https://www.irrd.tech/arkea/images/irrd_logo.png",#"https://github.com/Edyniesky/logos-/raw/gh-pages/Captura4.png",
                  title = "IRRD - Instituto para Redução de Riscos e Desastres de Pernambuco ",
                  height = "35px"),
              style = "padding-top:10px; padding-bottom:10px;"),
            class = "dropdown"),
    
    
    tags$li(a(href = 'https://www.ufpe.br/lika',
              img(src = "https://www.irrd.tech/arkea/images/lika_logo.png",
                  title = "LIKA - Laboratório de Imunopatologia Keizo Asami - UFPE",
                  height = "35px"),
              style = "padding-top:10px; padding-bottom:10px;"),
            class = "dropdown"),
    
    tags$li(a(href = 'https://www.irrd.org/covid-19/',
              img(src = "https://www.irrd.tech/arkea/images/arkeadx_logo.png",
                  title = "ARKEA DX",
                  height = "35px"),
              style = "padding-top:10px; padding-bottom:10px;"),
            class = "dropdown"),
    
    dropdownMenu(type = "messages", badgeStatus = "danger",
                 messageItem(
                   from = "Suporte",
                   message = tags$p(HTML("<br>Bem-vindo ao aplicativo para a <br>exploração de dados de<br>
                                           sequenciamento genético do Covid-19.</br>
                                        ;-)")), # Para ver as instruções de uso, </br> clique aquí 
                   icon = icon("life-ring"),
                   time = now())
    )
    
    
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
      startExpanded = FALSE,
      
      selectInput(
        inputId = 'pango',
        label = tags$h5(HTML('<strong>PANGO</strong>')),
        choices = pongo$`PANGO Lineage`,
        multiple = TRUE,
        selected =  pongo$`PANGO Lineage`
      ),
      
      tags$p(HTML("<br>Permite escolher uma ou mais linhagem"), style = "color:#FFFFFF"),
      
      dateRangeInput(
        inputId = 'dateSelect',
        label = tags$h5(HTML('<strong>PERIODO</strong>')),
        start = '2021-01-01',
        end = today(),
        min = '2020-02-25',
        max = today(),
        format = 'dd-mm-yyyy',
        startview = 'year',
        weekstart = 1,
        language = 'pt-BR',
        separator = tags$strong('Até'),
        width = '400px',
        autoclose = TRUE
      ),
      tags$p(HTML("<br>Permite filtrar os dados a partir de duas datas</br> adicionadas 
                        manualmente ou selecionadas</br> diretamente na caixa de diálogo."), 
             style = "color:#FFFFFF")
    ),
    
    menuItem(
      text = 'Entropía',
      icon = icon('random'), #'fas fa-dna'
      
      selectInput(
        inputId = 'gene',
        label = tags$h5(HTML('<strong>Genes</strong>')),
        choices = list('ORF1a', 'ORF1b', 'S', 'ORF3a', 'E', 'M', 'ORF6', 'ORF7a', 'ORF7b', 'ORF8', 
                       'ORF9b', 'N'),
        multiple = TRUE,
        selected = list('ORF1a', 'ORF1b', 'S', 'ORF3a', 'E', 'M', 'ORF6', 'ORF7a', 'ORF7b', 'ORF8',
                        'ORF9b', 'N')
      ),
      tags$p(HTML("<br>Permite escolher uma ou mais genes"), style = "color:#FFFFFF")
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
      tags$p(HTML("<br>Permite escolher uma Região"), style = "color:#FFFFFF") 
    )
  )
)
    


#   __________________ #< dfecf5d7f80c1a4e3e88607e968224af ># __________________
#   Body                                                                    ####


body <- dashboardBody(
  
  useShinyjs(),
  
  shinyDashboardThemes(theme = "blue_gradient"),
  
  fluidPage(
    
    navbarPage(
      title = "CONTEÚDO:", 
      selected = "SOBRE",
      collapsible = TRUE, 
      inverse = TRUE, 
      theme = shinytheme("readable"), 
      fluid = TRUE,   #readable, lumen, yeti
      
      ### Pernambuco Data 
      tabPanel("DADOS PERNAMBUCO", 
               fluidPage(
               tabsetPanel(
      
                 tabPanel(
                   title = '1-Frequência de Linhagem (PANGO)',
                   icon = icon('fas fa-chart-bar'),
                   fluidPage(
                         
                     box(
                       title = "Figura A: Número de linhagens (PANGO) por Municípios",
                       status = 'danger',
                       solidHeader = FALSE,
                       #background = 'black',
                       width = 12,
                       height = 840,
                       
                       iheatmaprOutput(
                         outputId = 'plotPE', 
                         width = "100%", 
                         height = "700px"
                         )
                       )
                     )
                   )#,
                 #tabPanel(
                   #title = '2-Dados espaciais PE',
                   #icon = icon('globe'),
                   
                   #fluidPage(
                    # box(
                     #  title = "Mapa A: Distribuição de linhagens (Pongo) por Municípios de Pernambuco ",
                     #  width = 12,
                      # status = 'danger',
                   #    solidHeader = FALSE,
                       
                    #   leafletOutput('map2i', height = 700)
                    # )
                 #  )
                # )
                 
                 
                 
                 )
              )
               ),
               
               
      ### Brasil Data 
      tabPanel("DADOS BRASIL",
               
               fluidPage(
                 tabsetPanel(
                   
                   tabPanel(
                     title = '1-Frequência de Linhagem (PANGO)',
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
                         title = "Figura B: Número de linhagens (PANGO) por Estados da Federação",
                         status = 'danger',
                         solidHeader = FALSE,
                         #background = 'black',
                         width = 12,
                         height = 640,
                         
                         iheatmaprOutput(
                           outputId = 'plot2', 
                           width = "100%", 
                           height = "585px"
                           )
                         )
                       )
                     ),
                   
                   tabPanel(
                     title = '2-Dados espaciais BR',
                     icon = icon('globe'),
                     
                     fluidPage(
                       box(
                         title = "Mapa A: Distribuição de linhagens (Pongo) por Estados da Federação",
                         width = 12,
                         status = 'danger',
                         solidHeader = FALSE,
                         
                         leafletOutput('map', height = 800)
                         )
                       )
                     )
                   )
                 )
               ),
      
      ### World data 
      tabPanel("DADOS GLOBAIS",
               
               fluidPage(
                 tabsetPanel(
                   
                   tabPanel(
                     title = '1-Entropía',
                     icon = icon("random"), #'fas fa-dna' fas fa-chart-pie
                     
                     fluidRow(
                       
                       box(
                         title = "Figura A: Diversidade genética do Covid-19 por gene, de acordo com sua entropía",
                         status = 'danger',
                         #background = 'black',
                         solidHeader = FALSE,
                         width = 12,
                         height = 840,
                         
                         plotlyOutput(
                           outputId = 'plot3i',
                           width = "100%",
                           height = "700px",
                           inline = TRUE,
                           reportTheme = TRUE
                         )
                       )
                     )
                   ),
                   
                   tabPanel(
                     title = "2-Estatística descritiva",
                     icon = icon('fas fa-chart-line'),
                     
                     box(
                       title = "Tabela A: Estatísticas descritivas para o número de sequenciamentos por país",
                       width = 12,
                       status = 'danger',
                       solidHeader = FALSE,
                       
                       htmlOutput('Res3')
                     )
                   ),
                   
                   tabPanel(
                     title = '3-Metadata',
                     icon = icon('fas fa-table'),
                     
                     fluidPage(
                       box(
                         title = "Tabela B: Metadados de sequenciamento de Covid-19 por países",
                         width = 12,
                         status = 'danger',
                         solidHeader = FALSE,
                         
                         dataTableOutput(
                           outputId = 'tabela',
                           width = "100%", 
                           height = "auto")
                         )
                       )
                     )
                   )
                 )
               ),
      
      ### about the Application 
      tabPanel(
        title = 'SOBRE',
        icon = icon('fas fa-info-circle'),
        
        fluidPage(
          box(
            title = "Sobre o APP Vigilância Molecular",
            width = 12,
            #background = 'navy',
            status = 'navy',
            solidHeader = FALSE,
            gradient = FALSE,
            headerBorder = TRUE,
            
            tags$p(HTML("<p align='justify'>O objetivo do aplicativo é auxiliar os gerentes e pesquisadores na exploração de dados sobre  <b><acronym title='Epidemiologia molecular é um ramo da ciência médica que se preocupa com a definição, identificação, e monitorização de espécies, subespécies e estirpes patog|ênicas relevantes por meio de tecnologia molecular e biologia evolutiva.Este ramo surgiu do uso de ferramentas criadas para o estudo da genética populacional em investigações epidemiológicas.'>epidemiologia molecular</acronym></b> do novo coronavírus <b>(Covid-19)</b> para a tomada de decisões. Além disso, esta ferramenta pode simplificar a exploração desses dados para a população em geral. Todas as análises foram realizadas utilizando o software livre R <mark>R versão 4.1.0</mark> e as bibliotecas <mark>shiny</mark> e <mark>shinydashboard</mark>. A aplicação é dividida em cinco janelas mais a janela de informaçães. Para a janela de  <mark>Frequência de Linhage (Pongo) </mark>,  <mark>Estatística Descritiva </mark> e  <mark>Entropía </mark>, é possível usar a barra lateral que é exibida em três submenus, que permitem filtrar os dados exibidos em cada uma das janelas mencionadas acima.</p>")),
            #img(src="https://github.com/Edyniesky/logos-/raw/gh-pages/Captura2.png", height = 350, width = 350),
            
            HTML('<center><img src = "https://phil.cdc.gov//PHIL_Images/23313/23313_lores.jpg"                                width="400" height="250"></center>'), 
            tags$p(HTML("<br>*Os dados utilizados foram obtidos do site do <a href='https://www.gisaid.org/'><i>Global Influenza Surveillance and Response System</i><b> (GISRS)</b>.</a><br>**Para mais informações sobre o coronavírus, você pode acessar o <a href='https://www.irrd.org/'>Instituto para Redução de Riscos e Desastres de Pernambuco (IRRD)</a>.")),
            tags$p(h4(
              htmlOutput("date")
              )
              )
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

