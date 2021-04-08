##%######################################################%##
#                                                          #
####                     Livrarias                      ####
#                                                          #
##%######################################################%##

#library(scales)
#library(formatR)
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

#   __________________ #< 70ac38cf284fbace8a978cd4d932e91a ># __________________
#   Base de dados e código geral                                           ####

vesiT <- read_delim("vesiT.csv", ";", escape_double = FALSE, trim_ws = TRUE)


vesiTi <- vesiT %>% 
  select(Regions, SIGLA, Municipio, POINT_X, POINT_Y, DatNotific, year, Diagnostico, Doença, 
         EspeciePrincipal, Pupulacao, TL1, TL2) %>%
  filter(Doença %in% c('LT', 'ME', 'IQQ', 'Intx', 'C.Ext', 'Fots')) %>% 
  mutate(Diagnostico = ifelse(Diagnostico == 'Pos', 'Clínico-epidemiológico', Diagnostico)) 

vesiTii <- vesiT %>% 
  select(Regions, SIGLA, Municipio, POINT_X, POINT_Y, DatNotific, year, Diagnostico, Doença, 
         EspeciePrincipal, Pupulacao, TL1, TL2) %>%
  filter(Doença %in% c('FA', 'EV', 'SV.A', 'DVB', 'RIB', 'FCM',
                       'LA', 'VB', 'PVB', 'EC', 'ActinB', 'ActinM', 'PDB'))

vesiTiii <- bind_rows(vesiTi, vesiTii)

vesiT1 <- vesiTiii %>% 
  select(Regions, SIGLA, Municipio, POINT_X, POINT_Y, year, Diagnostico, Doença, 
         EspeciePrincipal, Pupulacao, TL1, TL2) %>%
  rename(log = POINT_X, lat = POINT_Y) %>%
  mutate(TL1 = abs(TL1), 
         TL2 = abs(TL2),
         TL1 = ifelse(TL1 >= 40, 40, TL1),
         TL2 = ifelse(TL2 >= 60, 60, TL2),
         Diagnostico = ifelse(Diagnostico == 'Clin', 'Clínico-epidemiológico', Diagnostico),
         Diagnostico = ifelse(Diagnostico == 'Pos', 'Positivo', Diagnostico),
         Diagnostico = ifelse(Diagnostico == 'Neg', 'Negativo', Diagnostico)
  ) %>% 
  group_by(SIGLA, year, Doença) %>% 
  mutate(meanTL1 = mean(TL1), meanTL2 = mean(TL2), sdTL1 = sd(TL1), sdTL2 = (TL2)) %>% 
  ungroup() %>% 
  group_by(SIGLA, Municipio, log, lat, Diagnostico, year, EspeciePrincipal, Pupulacao, TL1, TL2) %>% 
  count(Doença) %>% 
  ungroup() %>% 
  rename(Estados = SIGLA, Municípios = Municipio, Diagnóstico = Diagnostico, Doenças = Doença,
         Ano = year, `População do município` = Pupulacao, Espécie = EspeciePrincipal, `Tempo de ação` = TL1,
         `Tempo de reação` = TL2, `N° notificações` = n)


vesiT2 <- vesiTiii %>% 
  select(Regions, SIGLA, Municipio, DatNotific, year, Diagnostico, Doença, EspeciePrincipal, Pupulacao, TL1, TL2) %>%
  mutate(TL1 = abs(TL1), 
         TL2 = abs(TL2),
         TL1 = ifelse(TL1 >= 40, 45, TL1),
         TL2 = ifelse(TL2 >= 30, 30, TL2),
         Diagnostico = ifelse(Diagnostico == 'Clin', 'Clínico-epidemiológico', Diagnostico),
         Diagnostico = ifelse(Diagnostico == 'Pos', 'Positivo', Diagnostico),
         Diagnostico = ifelse(Diagnostico == 'Neg', 'Negativo', Diagnostico)) %>% 
  group_by(SIGLA, Regions, Municipio, Diagnostico, DatNotific, year, EspeciePrincipal, Pupulacao, TL1, TL2) %>% 
  count(Doença) %>% 
  ungroup() %>% 
  rename(Estados = SIGLA, Região  = Regions, Diagnóstico = Diagnostico, Doenças = Doença, Data = DatNotific,
         Ano = year, `População do município` = Pupulacao, Espécie = EspeciePrincipal, `Tempo de ação` = TL1,
         `Tempo de reação` = TL2, `N° notificações` = n)



customGreen0 = "#DeF7E9"

customGreen = "#71CA97"

customRed = "#ff7f7f"

improvement_formatter1 <- formatter("span", 
                                    style = x ~ style(font.weight = "bold", 
                                                      color = ifelse(x < 5, customGreen, ifelse(x >= 5, customRed, "black"))), 
                                    x ~ icontext(ifelse(x < 5,"arrow-down", "arrow-up" ), x))

improvement_formatter2 <- formatter("span", 
                                    style = x ~ style(font.weight = "bold", 
                                                      color = ifelse(x < 1, customGreen, ifelse(x >= 1, customRed, "black"))), 
                                    x ~ icontext(ifelse(x < 1,"arrow-down", "arrow-up" ), x))


#   __________________ #< 93ae6596f43a8dae21fbb3aab5fc66df ># __________________
#   Funções                                                                 ####


### Função leaflet
server <- function(input, output) {
  
  # data for leaflet
  DTi <- reactive({
    vesiT1 %>%
      filter(Ano == input$ano, Estados %in% c(input$estado), Diagnóstico %in% c(input$diagnostico ))
  })
  
  output$map <- renderLeaflet({
    leaflet(DTi()) %>%
      addTiles() %>% 
      #addTerminator(group = "daylight") %>% 
      addProviderTiles(providers$Esri.WorldStreetMap, options = providerTileOptions(noWrap = TRUE)) %>%
      addCircleMarkers(
        lng = DTi()$log,
        lat = DTi()$lat, 
        radius = sqrt(10^DTi()$`N° notificações`) * 3, 
        #label = ~paste('N° notificações',',', `N° notificações`), 
        #popup = ~paste(Municipio, ',', Doença),
        weight = 4,
        stroke = TRUE,
        color = "#F60D1D",
        fillColor = "#F60D1D",
        fillOpacity = 0.25,
        clusterOptions = markerClusterOptions(),
        popup = paste0(
          "<strong>Estado da Federação: </strong>", DTi()$Estados,"<br>",
          "<strong>Município: </strong>",DTi()$Municípios, "<br>",
          "<strong>Doença: </strong>", DTi()$Doenças, "<br>",
          "<strong>Notificaçoes: </strong>", DTi()$`N° notificações`, "<br>",
          "<strong>Resultado do Diagnóstico: </strong>", DTi()$Diagnóstico, "<br>",
          "<strong>Espécie: </strong>", DTi()$Espécie, "<br>",
          "<strong>População do Municipio: </strong>", DTi()$`População do município`, "<br>",
          "<strong>Tempo de ação (Notificação): </strong>", DTi()$`Tempo de ação`, "<strong> dias </strong>","<br>",
          "<strong>Tempo de reação (Serviços Veterinários): </strong>", DTi()$`Tempo de reação`, 
          "<strong> dias </strong>", "<br>")) %>%
      addMiniMap(tiles = providers$Esri.WorldStreetMap,
                 toggleDisplay = TRUE,
                 zoomLevelOffset = -8,
                 zoomAnimation = TRUE) %>%
      addGraticule(group = "Graticule", interval = 5, sphere = FALSE, style = list(color = "blue", weight = 1)) %>%
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
  
  # value box 1: dados de leaflet
  dataBox1 <- reactive({
    DTi() %>% 
      summarise(Tcasos = sum(`N° notificações`)) 
  })
  
  output$not <- renderInfoBox({
    infoBox(
      title = "Total de Notificações",
      value = dataBox1()$Tcasos,
      subtitle = "No período escolhido",
      icon = icon('fas fa-exclamation-triangle'),
      color = 'red', 
      width = 3,
      fill = FALSE
    )
  })
  
  # value box 2
  dataBox2 <- reactive({
    DTi() %>% 
      summarise(meanTL1 = round(mean( `Tempo de ação`, na.rm = TRUE), digits = 2)) %>% 
      mutate(meanTL1 = replace_na(meanTL1, 0))
  })
  
  output$boxTL1 <- renderInfoBox({
    infoBox(
      title = tags$p(HTML("<acronym title = 'Notificação pelo proprietário, terceiros ou através da vigilância ativa'> Tempo de ação </acronym>")),
      value = dataBox2()$meanTL1,
      subtitle = "Média do tempo (dias)",
      icon = icon('bullhorn'),
      color = 'orange', 
      width = 3,
      fill = FALSE,
      #href = 'https://onlinelibrary.wiley.com/doi/10.1111/tbed.13486'
    )
  })
  
  # value box 3
  dataBox3 <- reactive({
    DTi() %>% 
      summarise(meanTL2 = round(mean(`Tempo de reação` , na.rm = TRUE), digits = 2)) %>% 
      mutate(meanTL2 = replace_na(meanTL2, 0))
  })
  
  output$boxTL2 <- renderInfoBox({
    infoBox(
      title = tags$p(HTML("<acronym title ='Primeira visita dos Serviços Veterinários até o diagnóstico final'>Tempo de reação</acronym>")),
      value = dataBox3()$meanTL2,
      subtitle = 'Média do tempo (dias)',
      icon = icon('fas fa-history'),
      color = 'aqua', 
      width = 3,
      fill = FALSE
      #href = 'https://onlinelibrary.wiley.com/doi/10.1111/tbed.13486'
    )
  })
  
  # value box 4
  dataBox4 <- reactive({
    DTi() %>% 
      summarise(Tpopul = sum(`População do município`, na.rm = TRUE)) 
  })
  
  output$boxPopu <- renderInfoBox({
    infoBox(
      title = "População suscetível",
      value = dataBox4()$Tpopul,
      subtitle = "A população total suscetível",
      icon = icon("fas fa-ambulance"), #list-alt
      color = 'olive', 
      width = 3,
      fill = FALSE
    )
  })
  
  # tabela 
  output$tabela <- renderDataTable({
    DT::datatable(
      DTi(),
      style = 'bootstrap',
      extensions = 'Buttons',
      callback = JS('table.page("next").draw(false);'),
      filter = 'top',
      options = list(
        deferRender = TRUE,
        pageLength = 25,
        autoWidth = TRUE,
        dom = 'Blfrtip', 
        buttons = c('excel'),
        lengthMenu = list(c(10 , 25, 50, -1), c(10, 25, 50, "All")))
    )
  })
  
  # Total de notificações 
  
  dataTN <- reactive({
    vesiT2 %>% 
      filter(Doenças %in% c(input$doencaTL2), Ano >= input$anoTN[1] & Ano <= input$anoTN[2])
  })
  
  output$TN <-  renderPlotly({
    
    PlotTN <- dataTN() %>% 
      group_by(Diagnóstico, Estados) %>% 
      summarise(`N° notificações` = sum(`N° notificações`)) %>% 
      ggplot(aes(x = Estados, y = `N° notificações`, fill = Diagnóstico)) +
      geom_bar(position = 'stack', stat = 'identity', alpha = 0.6, width = 0.8999, color = NA) + #, 
      scale_fill_manual(values = c("#00CD00", "#00BFFF", "#FF3030")) +
      dark_theme_classic() +
      labs(y = 'N° notificações (Porcentagem)')
    
    ggplotly(PlotTN)
    
  })
  
  #Estatística descritiva total notificações dataTN
  
  output$Res3 <- renderUI({
    res3 <- vesiT2 %>% 
      select(Estados, Ano, Diagnóstico, `N° notificações`) %>% 
      pivot_wider(names_from = Diagnóstico, values_from = `N° notificações`, values_fill = 0, values_fn = sum) %>%
      select(Estados, Positivo, Negativo, `Clínico-epidemiológico`) %>% 
      group_nest(Estados) %>% 
      mutate(
        Negmean = map(data, ~round(mean(.x$Negativo), digits = 1)),
        Negsd = map(data, ~round(sd(.x$Negativo), digits = 1)),
        Negdata = map(data, ~spk_chr(.x$Negativo, type = 'box', boxFillColor = '#FFF8DC', lineWidth = 1.5)),
        
        CEmean = map(data, ~round(mean(.x$`Clínico-epidemiológico`), digits = 1)), 
        CEsd = map(data, ~round(sd(.x$`Clínico-epidemiológico`), digits = 1)),
        CEdata = map(data, ~spk_chr(.x$`Clínico-epidemiológico`, type = 'box', boxFillColor = '#FFF8DC', lineWidth = 1.5)),
        
        Posmean = map(data, ~round(mean(.x$Positivo), digits = 1)), 
        Possd = map(data, ~round(sd(.x$Positivo), digits = 1)),
        Posdata = map(data, ~spk_chr(.x$Positivo, type = 'box', boxFillColor = '#FFF8DC', lineWidth = 1.5))
      ) %>% 
      unnest(c(Negmean, Negsd, Negdata, CEmean, CEsd, CEdata, Posmean, Possd, Posdata)) %>% 
      select(-data) %>% 
      rename(`Média negativos` = Negmean,
             `Desvio padrão negativos` = Negsd, 
             `Distribuição negativos` = Negdata, 
             `Média Clínico.E` = CEmean, 
             `Desvio padrão Clínico.E` = CEsd, 
             `Distribuição Clínico.E` = CEdata, 
             `Média positivo` = Posmean, 
             `Desvio padrão positivo` = Possd, 
             `Distribuição positivo` = Posdata) %>% 
      format_table(align = c('l', 'c', 'c', 'r', 'c', 'c', 'r', 'c', 'c', 'r'), 
                   list(`Estados` = formatter("span", style = ~ style(color = "#000080",font.weight = "bold")),
                        `Desvio padrão negativos` = color_bar("#1E90FF"),
                        `Desvio padrão Clínico.E` = color_bar("#32CD32"),
                        `Desvio padrão positivo` = color_bar("#FF6347") 
                        #`Média negativos` = color_tile("#1C86EE", "#191970"),
                        #`Média Clínico.E` = color_tile("#458B74", "#006400"), 
                        #`Média positivo` = color_tile("#FF7256", "#EE2C2C")
                   )) %>%  
      htmltools::HTML() %>%
      div() %>%
      spk_add_deps() %>%
      {column(width = 12, .)}
    
    res3
  })
  
  # Timeliness 1 
  
  dataTL1 <- reactive({
    vesiT2 %>% 
      filter(Região == input$regiao, Doenças %in% c(input$doenca), Data >= input$dateSelect[1] & Data <= input$dateSelect[2])
  })
  
  output$TL1 <-  renderPlotly({
    plot1 <- dataTL1() %>% 
      ggplot(aes(x = Data, y = `Tempo de ação`)) +
      geom_point(aes(fill = Diagnóstico),size = 3.5, alpha = 0.7, color = NA) + 
      scale_fill_manual(values = c("#FFD700", '#00EE76', "#CD2626")) + #c("#FFC125", "#66CDAA", "#CD2626")
      dark_theme_classic() +
      labs(y = 'Tempo de ação (dias)')
    
    
    
    if (input$smooth) {
      plot1 <- plot1 + geom_smooth(se = TRUE, color = '#1E90FF')
    }
    ggplotly(plot1)})
  
  #Estatística descritiva TL1
  
  output$Res1 <- renderUI({
    res <- dataTL1() %>% 
      select(Estados, `Tempo de ação`) %>% 
      filter(!is.na( `Tempo de ação`)) %>% 
      group_nest(Estados) %>% 
      mutate( mean = map(data, ~round(mean(.x$`Tempo de ação`), digits = 1)), 
              sd = map(data, ~round(sd(.x$`Tempo de ação`), digits = 1)),
              data = map(data, ~spk_chr(.x$`Tempo de ação`, type = 'line', lineColor = '#0000EE', fillColor = '#F0FFFF', lineWidth = 1.9))) %>%  #, type = 'box'
      unnest(cols = c(data, mean, sd)) %>% 
      select(Estados, mean, sd, data) %>% 
      rename(Média = mean, `Desvio padrão` = sd, `Distribuição tempo de ação` = data) %>% 
      format_table(align = c('l', 'r', 'c', 'c'), 
                   list(`Média` = color_bar("#FFA500"),
                        `Desvio padrão` = improvement_formatter1)) %>% 
      htmltools::HTML() %>%
      div() %>%
      spk_add_deps() %>%
      {column(width = 12, .)}
    
    res
  })
  
  
  # Timeliness 2
  output$TL2 <-  renderPlotly({
    plot2 <- dataTL1() %>% 
      ggplot(aes(x = Data, y = `Tempo de reação`)) +
      geom_point(aes(fill = Diagnóstico),size = 3.5, alpha = 0.6, color = NA) + 
      scale_fill_manual(values = c("#FFD700", '#00EE76', "#CD2626")) +
      dark_theme_classic() +
      labs(y = 'Tempo de ação (dias)')
    
    if (input$smooth) {
      plot2 <- plot2 + geom_smooth(se = TRUE, color = '#1E90FF')
    }
    ggplotly(plot2)
    
  })
  
  #Estatística descritiva TL2 c("#0000EE", "#98F5FF")
  
  output$Res2 <- renderUI({
    res2 <- dataTL1() %>% 
      select(Estados, `Tempo de reação`) %>% 
      filter(!is.na( `Tempo de reação`)) %>% 
      group_nest(Estados) %>% 
      mutate( mean = map(data, ~round(mean(.x$`Tempo de reação`), digits = 1)), 
              sd = map(data, ~round(sd(.x$`Tempo de reação`), digits = 1)),
              data = map(data, ~spk_chr(.x$`Tempo de reação`, type = 'line', lineColor = '#0000EE', fillColor = '#F0FFFF', lineWidth = 1.9))) %>%  #, type = 'box'
      unnest(cols = c(data, mean, sd)) %>% 
      select(Estados, mean, sd, data) %>% 
      rename(Média = mean, `Desvio padrão` = sd, `Distribuição tempo de reação` = data) %>% 
      format_table(align = c('l', 'r', 'c', 'c'), 
                   list(`Média` = color_bar('#00B2EE'),
                        `Desvio padrão` = improvement_formatter2)) %>% 
      htmltools::HTML() %>%
      div() %>%
      spk_add_deps() %>%
      {column(width = 12, .)}
    
    res2
  })
  
}



#   __________________ #< 8ec25faf369bf80db98393ab2b9184d4 ># __________________
#   cabeçalho (header)                                                      ####


header <- dashboardHeader(
  
  ### changing logo
  title = shinyDashboardLogo(
    theme = "flat_red",
    boldText = "Doenças Vesiculares",
    mainText = "App",
    badgeText = "v0.0.3"
  ),
  
  titleWidth = 285,
  
  # código html para modificar a letra...''
  # 
  tags$li(a(href = 'http://ufape.edu.br/',
            img(src = "https://github.com/Edyniesky/logos-/raw/gh-pages/logo_ufape_0.png",
                title = "Instituição Executora",
                height = "30px"),
            style = "padding-top:10px; padding-bottom:10px;"),
          class = "dropdown"),
  
  tags$li(a(href = 'http://www.facepe.br/',
            img(src = 'https://github.com/Edyniesky/logos-/raw/gh-pages/Marca-FACEPE.png',
                title = "Instituição Financiadora", 
                height = "30px"),
            style = "padding-top:10px; padding-bottom:10px;"),
          class = "dropdown"),
  
  tags$li(a(href = 'http://www.adagro.pe.gov.br/',
            img(src = 'https://github.com/Edyniesky/logos-/raw/gh-pages/ADAGRO.png',
                title = "Instituição beneficiada", 
                height = "30px"),
            style = "padding-top:10px; padding-bottom:10px;"),
          class = "dropdown"),
  
  dropdownMenu(type = "messages", badgeStatus = "danger",
               messageItem(
                 from = "Suporte",
                 message = tags$p(HTML("<br>Bem-vindo ao aplicativo para a</br> exploração de síndromes vesiculares.</br>
                                        ;-)")), # Para ver as instruções de uso, </br> clique aquí 
                 icon = icon("life-ring"),
                 time = now())
               )
  )




#   __________________ #< 90283e635e5e18f42aac467e066894bc ># __________________
#   Barra lateral ( sidebar)                                                ####


sidebar <- dashboardSidebar(
  width = "340px",
  collapsed = FALSE,
  
  sidebarMenu(
    
    menuItem(
      text = 'FILTRO DADOS ESPACIAIS',
      icon = icon("globe"),
      startExpanded = FALSE,
      
      
      selectInput(
        inputId = 'ano',
        label = tags$h5(HTML('<strong>Ano (2004-2020)</strong>')), #, style = 'color:#FFFACD'
        choices = list('2004', '2005', '2006', '2007', '2008', '2009', '2009', '2010', '2011', 
                       '2012', '2013', '2014', '2015', '2016', '2017', '2018', '2019', '2020'),
        multiple = FALSE,
        selected = '2005'
      ),
      tags$p(HTML("<i>Permite escolher apenas um ano</i>"), style = "color:#000080"), # , style = "color:#000080"
      
      selectInput(
        inputId = 'estado',
        label = tags$h5(HTML('<strong>Estados da federação</strong>')), #, style = 'color:#FFFACD'
        choices = list('RO', 'AC','TO', 'AM', 'PA', 'AP', 'RR', 'SE', 'AL', 'BA', 'PE', 'PB', 'RN', 
                       'CE', 'PI', 'MA','MS', 'DF', 'GO', 'MT', 'RJ', 'SP', 'ES', 'MG', 'SC', 'PR', 'RS'),
        multiple = TRUE,
        selected = 'PE'
      ),
      tags$p(HTML("<br>Permite escolher um ou mais estados da federação"), style = "color:#000080"), # , style = "color:#FFEFDB"
      
      prettyCheckboxGroup(
        inputId = 'diagnostico',
        label = tags$h5(HTML('<strong>Resultado do Diagnóstico</strong>')), # , style = 'color:#FFFACD'
        choices = list("Positivo", "Negativo", "Clínico-epidemiológico"), 
        selected = "Positivo",
        status = 'danger',
        shape = "square",
        outline = TRUE,
        fill = FALSE,
        thick = TRUE,
        animation = 'smooth',
        icon = icon("check"),
        plain = FALSE,
        bigger = TRUE,
        inline = FALSE,
        width = '100%'
      ),
      
      tags$p(HTML("<br>É possível escolher casos negativos, positivos,</br> clínico-epidemiológico ou todos.</br> 
                Clínico-epidemiológicos são casos positivos</br> sem confirmação laboratorial"), style = "color:#000080"), 
      
      tags$p(HTML("<br>Esta seção permite filtrar os dados que serão</br> georreferenciados no mapa de acordo com os</br> critérios 
                selecionados pelo usuário.</br> Os mesmos dados serão tabulados na seção</br> inferior do mapa e podem ser 
                exportados</br> pelo usuário no formato Excel. No topo do</br> mapa há quatro caixas que resumem os</br> dados 
                selecionados pelo usuário (mostrados</br> no mapa e na tabela):"), style = "color:#6E6E6E"),
      
      tags$ul(HTML('<li>Total de notificações</li>'), style = "color:#6E6E6E"),
      tags$ul(HTML('<li>Tempo de ação médio para todas as</br> notificações selecionadas</li>'), style = "color:#6E6E6E"),
      tags$ul(HTML('<li>Tempo de reação médio para todas as</br> notificações selecionadas </li>'), style = "color:#6E6E6E"),
      tags$ul(HTML('<li>População total suscetível do total de</br> notificações</li>'), style = "color:#6E6E6E"),
      
      tags$p(HTML("Observe que as informações podem não estar</br> disponíveis para alguns estados e anos. Os</br> dados 
                georreferenciados aparecerão no mapa</br> como pontos vermelhos e seu tamanho pode</br> variar de acordo com 
                o número de</br> notificações. Informações específicas para</br> cada ponto podem ser acessadas clicando</br>
                sobre este. Além disso, no caso de eventos</br> múltiplos em regiões próximas, eles serão</br> agrupados."), style = "color:#6E6E6E")
    ),
    
    menuItem(
      text = 'FILTRO NÚMERO DE NOTIFICAÇÕES',
      icon = icon('fas fa-chart-bar'),
      
      selectInput(
        inputId = 'doencaTL2',
        label = tags$h5(HTML('<strong>Doenças</strong>')),
        choices = list('FA', 'EV','SV.A', 'DVB', 'RIB', 'FCM', 'LA', 'VB', 'PVB', 'EC', 'ActinB', 'ActinM', 'PDB',
                       'LT', 'ME', 'IQQ', 'Intx', 'C.Ext', 'Fots'),
        multiple = TRUE,
        selected = 'EV'
      ),
      tags$p(HTML("<br>Permite escolher uma ou mais doenças"), style = "color:#000080"),
      
      sliderInput(
        inputId = 'anoTN',
        label = tags$h5(HTML('<strong>Ano (2004-2020)</strong>')),
        min = 2004,
        max = 2020,
        value = c(2004, 2008),
        sep = '',
        step = 1,
        animate = animationOptions(
          loop = FALSE,
          playButton = 'Play',
          pauseButton = 'Pause')
      ),
      
      tags$p(HTML("<br>Permite escolher um ano ou um período de tempo</br> que pode começar em 2004 até 2020. Também é</br> 
                    possível usar o botão <strong>Play</strong> para animar o resultado"), style = "color:#000080"),
      
      tags$p(HTML("Esta seção permite filtrar os dados para gerar a</br> figura do número de notificações de síndromes</br> vesiculares. 
                    Os resultados mostram o número total</br> de notificações para uma ou mais síndromes</br> vesiculares, de acordo com o 
                    período escolhido.</br> Informações particulares sobre cada estado podem</br> ser observadas ao passar o cursor sobre 
                    as barras.</br> Também é possível animar os resultados através do</br> botão play abaixo da barra de tempo."),
             style = "color:#6E6E6E"),
      tags$p(HTML("O resumo estatístico cobre todo o período de estudo</br> (2004-2020). O valor médio e o desvio padrão</br>
                    correspondem aos valores anuais do número total de</br> notificações, de acordo com o resultado do</br> diagnóstico. 
                    Devido à falta de informações, nem</br> sempre é possível calcular esses valores. A tabela</br> também fornece 
                    uma figura de boxplot mostrando a</br> distribuição das notificações por estado.</justify>"), style = "color:#6E6E6E")
    ),
    
    menuItem(
      text = 'FILTRO TEMPO DE AÇÃO E REAÇÃO',
      icon = icon('fas fa-chart-line'),
      
      dateRangeInput(
        inputId = 'dateSelect',
        label = tags$h5(HTML('<strong>Periodo</strong>')),
        start = '2005-01-01',
        end = '2008-01-01',
        min = '2004-10-04',
        max = '2020-01-02',
        format = "yyyy-mm-dd",
        startview = "year",
        weekstart = 1,
        language = "pt-BR",
        separator = tags$strong('Até'),
        width = '400px',
        autoclose = TRUE
      ),
      tags$p(HTML("<br>Permite filtrar os dados a partir de duas datas</br> adicionadas manualmente ou selecionadas</br>
                    diretamente na caixa de diálogo."), style = "color:#000080"),
      
      selectInput(
        inputId = 'regiao',
        label = tags$h5(HTML('<strong>Região do Brasil</strong>')),
        choices = list('Norte', 'Nordeste','Centro Oeste', 'Sudeste', 'Sul'),
        multiple = FALSE,
        selected = 'Nordeste'
      ),
      tags$p(HTML("Permite selecionar uma das regiões do Brasil"), style = "color:#000080"),
      
      selectInput(
        inputId = 'doenca',
        label = tags$h5(HTML('<strong>Doenças</strong>')),
        choices = list('FA', 'EV','SV.A', 'DVB', 'RIB', 'FCM', 'LA', 'VB', 'PVB', 'EC', 'ActinB', 'ActinM', 'PDB',
                       'LT', 'ME', 'IQQ', 'Intx', 'C.Ext', 'Fots'),
        multiple = TRUE,
        selected = 'EV'
      ),
      tags$p(HTML("<br>Permite escolher uma ou mais doenças"), style = "color:#000080"),
      
      prettyCheckbox(
        inputId = 'smooth',
        label = tags$strong('Adicionar linha de tendência?', style = 'color:#CD2626') ,
        status = 'danger',
        outline = TRUE,
        fill = FALSE,
        thick = TRUE,
        animation = 'smooth',
        icon = icon("check"),
        plain = FALSE,
        bigger = TRUE,
        inline = FALSE,
        width = '100%'
      ), 
      tags$p(HTML("Esta seção permite filtrar os dados para gerar a</br> figura relativa ao tempo de ação e reação. Os dados</br> 
                    podem ser filtrados de acordo com datas específicas</br> na caixa de período. Também é possível 
                    escolher</br> entre as cinco regiões do Brasil e o tipo de síndrome</br> vesicular. Na parte final 
                    é possível adicionar uma</br> linha de tendência do dado através de um modelo de</br> regressão cuja 
                    fórmula será escolhida</br> automaticamente de acordo com os padrões</br>
                    observados."), style = "color:#6E6E6E"),
      
      tags$p(HTML("Para cada figura há uma tabela com a média,</br> o desvio padrão e a distribuição dos dados por</br> estados.
                    Estas tabelas serão atualizadas cada vez</br> que os parâmetros das figuras mudarem."), 
             style = "color:#6E6E6E")
      
    )
  )
  
)



#   __________________ #< 793cf18f48f5e48394cdb7538f20434a ># __________________
#   Corpo (body)                                                            ####



body <- dashboardBody(
  
  shinyDashboardThemes(
    theme = "onenote"),
  
  fluidRow(
    
    tabsetPanel(
      selected = 'Informações',
      
      tabPanel(
        title = 'Dados espaciais',
        icon = icon('globe'),
        
        box(
          status = 'primary',
          #background = 'light-blue',
          solidHeader = FALSE,
          width = 12,
          height = 110,
          
          infoBoxOutput(
            outputId = 'not',
            width = 3),
          infoBoxOutput(
            outputId = 'boxTL1',
            width = 3),
          infoBoxOutput(
            outputId = 'boxTL2',
            width = 3),
          infoBoxOutput(
            outputId = 'boxPopu',
            width = 3)
          ),
        box(
          width = 12,
          status = 'danger',
          
          leafletOutput('map', height = 600)
          ),
        
        fluidRow(
          box(
            title = 'Relatório das informações selecionadas',
            width = 12,
            status = 'warning',
            solidHeader = TRUE,
            collapsible = TRUE,
            
            dataTableOutput(
              outputId = 'tabela',
              width = "100%", 
              height = "auto")
            )
          )
        ),
      
      tabPanel(
        title = 'Número de Notificaçãoes',
        icon = icon('fas fa-chart-bar'),
        
        fluidRow(
          
          box(
            title = "Figura A: Numero de notificações por Estados da Federação",
            status = 'danger',
            #background = 'black',
            solidHeader = FALSE,
            width = 12,
            height = 640,
            
            plotlyOutput(
              outputId = 'TN',
              width = "100%",
              height = "585px",
              inline = TRUE,
              reportTheme = TRUE
            )
          ),
          box(
            title = "Tabela A: Resumo estátistico para total de notificações por Estado",
            status = 'warning',
            #background = 'black',
            solidHeader = TRUE,
            width = 12,
            height = 930,
            collapsible = TRUE,
            
            htmlOutput('Res3')
            )
          )
        ),
      tabPanel(
        title = 'Tempo de ação e reação',
        icon = icon('fas fa-chart-line'),
        
        fluidPage(
          
          box(
            title = "Figura A: Tempo desde a notificação até a data provável do evento",
            status = 'danger',
            #background = 'black',
            solidHeader = FALSE,
            width = 6,
            height = 640,
            
            plotlyOutput(
              outputId = 'TL1',
              width = "100%",
              height = "585px",
              inline = TRUE,
              reportTheme = TRUE
            )
          ),
          box(
            title = "Figura B: Tempo transcorrido desde a primeira visita dos serviços veterinários até o diagnóstico",
            status = 'danger',
            #background = 'black',
            solidHeader = FALSE,
            width = 6,
            height = 640,
            
            plotlyOutput(
              outputId = 'TL2',
              width = "100%",
              height = "585px",
              inline = FALSE,
              reportTheme = TRUE
            )
          ),
          box(
            title = "Tabela A: Resumo estatístico para tempo de ação",
            status = 'warning',
            #background = 'black',
            solidHeader = TRUE,
            width = 6,
            height = 360,
            collapsible = TRUE,
            
            htmlOutput('Res1')
          ),
          
          box(
            title = "Tabela B: Resumo estatístico para tempo de reação",
            status = 'warning',
            #background = 'black',
            solidHeader = TRUE,
            width = 6,
            height = 360,
            collapsible = TRUE,
            
            htmlOutput('Res2')
            )
          )
        ),
      
      tabPanel(
        title = "Informações",
        icon = icon('fas fa-info-circle'),
        
        box(
          width = 12,
          status = 'danger',
          solidHeader = FALSE,
          
          tags$h3(strong('Algumas considerações')),
          tags$p(HTML("O objetivo da aplicação é permitir que os Serviços Veterinários do Estado de Pernambuco 
                                <b><acronym title='Agência de Defesa e Fiscalização Agropecuária do Estado de Pernambuco
                                '>(ADAGRO)</acronym></b> explorem de forma rápida e prática possíveis 
                                surtos de <b>Febre Aftosa</b> no Brasil. A aplicação utilizou dados do Sistema 
                                de Vigilância Epidemiológica Continental <b>(SivCont)</b> para síndromes vesiculares 
                                no período de 2004-2020. O aplicativo permite a exploração espacial dos dados por município, 
                                ano, estado para 19 doenças vesiculares:")),
          tags$ul(HTML('<li>Febre aftosa (FA)</li>')),
          tags$ul(HTML('<li>Estomatite vesicular (EV)</li>')),
          tags$ul(HTML('<li>Sêneca vírus (SV.A)</li>')),
          tags$ul(HTML('<li>Diarreia viral Bovina (DVB)</li>')),
          tags$ul(HTML('<li>Rinotraqueíte infecciosa bovina (RIB)</li>')),
          tags$ul(HTML('<li>Febre catarral maligna (FCM)</li>')),
          tags$ul(HTML('<li>Língua azul (LA)</li>')),
          tags$ul(HTML('<li>Varíola bovina (VB)</li>')),
          tags$ul(HTML("<li>Pseudovaríola bovina (PVB)</li>")),
          tags$ul(HTML('<li>Ectima contagioso (EC)</li>')),
          tags$ul(HTML('<li>Actinobaciloses (ActinB)</li>')),
          tags$ul(HTML('<li>Actinomicoses (ActinM)</li>')),
          tags$ul(HTML('<li>Pododermatite bovina (PDB)</li>')),
          tags$ul(HTML('<li>Lesões traumáticas (LT)</li>')),
          tags$ul(HTML('<li>Mal de eucalipto (ME)</li>')),
          tags$ul(HTML('<li>Irritantes químicos e queimaduras (IQQ)</li>')),
          tags$ul(HTML('<li>Intoxicação (Intx)</li>')),
          tags$ul(HTML('<li>Corpo estranho (C.Ext)</li>')),
          tags$ul(HTML('<li>Fotossensibilização (Fots)</li>')),
          tags$p(HTML('Também foram calculados o tempo de ação e o tempo de reação dos proprietários e dos Serviços 
                                Veterinários oficiais (SV) para cada evento relatado. A primeira refere-se ao tempo decorrido desde 
                                a notificação ao SV e a data provável do início do evento e a segunda ao tempo decorrido desde a 
                                primeira visita dos SV ao diagnóstico final (resultado do laboratório). No caso do tempo de reação 
                                ser 0 significa que os serviços veterinários encerraram o evento nas primeiras 24 horas. 
                                Para maiores informações sobre os tempos de ação e reação em doenças vesiculares, veja o artigo 
                                publicado em 2020 no seguinte <a href="https://onlinelibrary.wiley.com/doi/10.1111/tbed.13486"
                                >link</a>.')),
          tags$p(HTML('<b>Para dúvidas ou sugestões, você pode nos contatar através dos seguintes e-mails:</b>'),
                 style = "color:#0000FF"),
          tags$address(HTML('<p><a href="mailto:edyferrer8123@gmail.com">edyferrer8123@gmail.com</a>, <a href="mailto:krsantoro01@gmail.com ">krsantoro01@gmail.com </a></p>'))
        ),
        
        box(
          status = 'warning',
          #background = 'red', 
          solidHeader = FALSE,
          width = 7,
          tags$h4(HTML('Realizado no <i>âmbito</i> do projeto de Pós-doutorado <b>PROJ-BFP-0042-20</b> financiado pela
                                 <b>FACEPE </b> e intitulado:')),
          tags$h4(strong('Otimização do sistema de vigilância vesiculares no Estado de Pernambuco através da priorização 
                                   baseada em risco', style = "color:#FF6A6A"))
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



