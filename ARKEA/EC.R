library(scales)
library(formatR)

library(readxl)
library(readr)

library(tidyverse)
library(broom)
library(broomExtra)
library(janitor)
library(lubridate)
library(tidylog)
library(eeptools)
library(Rclean)

library(plotly)
library(ggsci)
library(ggThemeAssist)
library(ggthemes)
library(gghighlight)
library(ggthemr)
library(mdthemes) # imagemagick-x11 and gtk+
library(patchwork)
library(ggpubr)
library(ggridges)
library(ggExtra)
library(ggparallel)
library(alluvial)
library(GGally)
library(cdparcoord)
library(RColorBrewer)
library(colorspace)
library(wesanderson)
library(data.table)
library(xlsx) # problemas JDK
library(cowplot)
library(viridis)
library(coronavirus)
library(incidence)
library(earlyR) # libsodium
library(EpiEstim) 
library(distcrete)
library(epitrix)
library(projections)


library(sp)# varios
library(spdep) # gdal  udunits
library(sf)
library(tmap)#fastmap
library(tmaptools)
library(raster)
library(spData)
library(spDataLarge) # v8
library(ggsn)
library(hrbrthemes)
library(gganimate)
library(anytime)
library(reshape2)
library(ggforce)
library(ggfortify)
library(ggedit)

library(geobr)
library(ggmap)
library(maps)
library(mapdata)
library(gridExtra)
library(grid)
library(ggspatial)
library(ggrepel)
library(googleway)
library(leaflet) # for interactive maps
library(mapview) # for interactive maps # tidyverse data visualization package
library(shiny)
library(rgdal)# for web applications
library(cartography)
library(magrittr)
library(pracma) #media movel 
library(zoo) #media movel
library(rgeos)
library(spgwr)
library(lctools) # cluster hots spot, moran 
library(geogrid) 

autor <- read_delim("nextstrain_ncov_global_authors.tsv", "\t", escape_double = FALSE, trim_ws = TRUE)

metadata <- read_delim("nextstrain_ncov_global_metadata.tsv",  "\t", escape_double = FALSE, trim_ws = TRUE)

entropy <- read_delim("nextstrain_ncov_global_diversity.tsv", "\t", escape_double = FALSE, trim_ws = TRUE)


library(ggstream)
library(streamgraph)

t <- metadata %>% filter(Country == "Brazil") %>% 
  group_by(`Collection Data`, `PANGO Lineage`) %>% # Admin Division
  count(`PANGO Lineage`) %>% 
  rename(data = `Collection Data`, total = n, Linage = `PANGO Lineage`) %>% 
  ungroup() %>% 
  group_by(data) %>% 
  mutate(sum = sum(total), porc = total / sum *100) %>% 
  #rename(data = `Collection Data`, total = n, Linage = `PANGO Lineage`) %>% 
  print()
  


pongo <- (metadata) %>%
  filter(Country == "Brazil") %>% 
  select(`PANGO Lineage`) %>% 
  distinct(`PANGO Lineage`) %>% 
  as.vector() %>% 
  print()



t1 <- t  %>% 
  ggplot(aes(x = data, y  = total, fill = Linage, text = porc)) + #text = total
  geom_stream(type = "proportional", bw =  0.75, extra_span = 0.1, color = "white", 
              alpha = 1, size = 0.1) +
  scale_fill_viridis(discrete = TRUE, option = "B") +
  scale_y_percent() +
  labs(x = "Data", y = "Frequência", fill = "Linhagem",
       subtitle = paste("ARKEA das Archaeas:", today()),
       caption = "https://nextstrain.org/ncov/global?c=location&lang=es") +
  #theme_modern_rc(base_size = 12, axis_title_size = 12, ticks = TRUE)
  theme_hc(base_size = 14) +
  theme(axis.text = element_text(color = "dimgray", size = 14), axis.text.x = element_text(angle = 360, 
                                                                                           hjust = 1)) 
  #scale_x_date(date_labels = "%d %b %Y", date_breaks = "21 day")


t1

ggplotly(t1, tooltip = "all", originalData = TRUE) #tooltip = "text"


t <- metadata %>% 
  filter(Country == "Brazil") %>% 
  group_by(`Collection Data`, `PANGO Lineage`) %>% # Admin Division
  count(`PANGO Lineage`) %>% 
  ungroup() %>% 
  rename(data = `Collection Data`, total = n, Linage = `PANGO Lineage`) %>% 
  print()


# Compute distances and hierarchical clustering
dd <- dist(scale(USArrests), method = "euclidean")
hc <- hclust(dd, method = "ward.D2")





t1 <- metadata %>% 
  filter(Country == "Brazil") %>% 
  group_by(`PANGO Lineage`) %>% # Admin Division
  count(`PANGO Lineage`) %>% 
  ungroup() %>% 
  rename(total = n, Linage = `PANGO Lineage`) %>% 
  column_to_rownames(var = "Linage") %>% 
  print()

library(factoextra)


dd <- dist(scale(t1), method = "euclidean")
hc <- hclust(dd, method = "ward.D2")

fviz_dend(hc, cex = 0.5)

fviz_dend(hc, cex = 0.5, 
          main = "Dendrogram - ward.D2",
          xlab = "Objects", ylab = "Distance", sub = "")

fviz_dend(hc, cex = 0.6, horiz = TRUE)


fviz_dend(hc, k = 5,                 # Cut in four groups
          cex = 0.5,                 # label size
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE,  # color labels by groups
          ggtheme = theme_classic2(),     # Change theme
          horiz = TRUE)





t1 <- metadata %>% 
  filter(Country == "Brazil") %>% 
  group_by(`PANGO Lineage`, `Admin Division`) %>% # Admin Division
  count(`PANGO Lineage`, `Admin Division`) %>% 
  ungroup() %>% 
  rename(total = n, Linage = `PANGO Lineage`, Estado = `Admin Division`) %>% 
  mutate(Estado = ifelse(Estado == "Amazonas BR", "Amazonas", Estado)) %>% 
  spread(Estado, total, fill = 0) %>% 
  column_to_rownames(var = "Linage") %>% 
  print()

library("heatmaply")


mat <- t1
mat[] <- paste("Esta célula é: Linhagem (Pongo) ", rownames(mat))
mat[] <- lapply(colnames(mat), function(colname) {
  paste0(mat[, colname], ", ", colname)
})


d3heatmap(
  t1,
  seriate = "mean",
  scale_fill_gradient_fun = ggplot2::scale_fill_gradient2(
    low = "#98F5FF", 
    high = "#FF4500", 
    midpoint = 6, 
    limits = c(0, 13)),
  cellnote = t1,
  custom_hovertext = mat,
  k_col = 4,
  k_row = 4,
  xlab = "Estados da Federação",
  ylab = "Linhagem (Pongo)", 
  main = "Número de linhagens por estados da Federação",
)


d3heatmap(t1)

library(iheatmapr)

t2 <- data.matrix(t1, rownames.force = TRUE)

t2


main_heatmap(t2, name = "Número de linhagens") %>% 
  add_col_clustering(k = 4) %>%
  add_row_clustering(k = 4) %>%
  add_col_labels() %>% 
  add_row_labels() %>% 
  add_col_summary(layout = list(zeroline = FALSE, title = "Average")) %>% 
  add_col_title("Measles Cases from 1930 to 2001", side = "top") %>% 
  add_row_summary(groups = TRUE, 
                  type = "bar",
                  layout = list(title = "Average<br>per<br>year",
                                font = list(size = 8)))





est <- st_read("Centroides_Estados.shp", quiet = TRUE) #Estado BR
est <- st_as_sf(est) 

Centroide <- st_centroid(est, of_largest_polygon = FALSE)

Centroide <- as_tibble(Centroide)



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




t  <- metadata %>% 
  clean_names() %>% 
  filter(region == 'South America') %>% 
  select(country, pango_lineage) %>% 
  group_by(country) %>% 
  count(pango_lineage) %>% 
  select(country, n) %>% 
  group_nest(country) %>% 
  mutate(
    mean = map(data, ~round(mean(.x$n), digits = 2)),
    sd = map(data, ~round(sd(.x$n), digits = 1)),
    suma = map(data, ~round(sum(.x$n), digits = 1)),
    fig = map(data, ~spk_chr(.x$n, type = 'bar', boxFillColor = '#FFF8DC', lineWidth = 1.5))) %>% 
  unnest(c(suma, mean, sd, fig)) %>%
  distinct() %>% 
  rename(País = country, `Número total de seqüências` = suma, 
         Média = mean, `Desvio padrão` = sd, `Distribuição dos dados` = fig) %>%
  select(-data) %>% 
  print()


formattable(t)



format_table(t, align = c('l', 'l', 'l', 'c', 'c', 'c', 'r')) %>% 
  htmltools::HTML() %>%
  div() %>%
  spk_add_deps()


out = as.htmlwidget(formattable(t))
out$dependencies = c(out$dependencies, htmlwidgets:::widget_dependencies("sparkline", "sparkline"))
out


metadata %>% 
  clean_names() %>% 
  print()
library(GenomeGraphs)
library(chromoMap)
library(ggbio)

library(epivizrData)

p <- entropy %>% 
  distinct(position, gene) %>% 
  print()

Ideogram(entropy$gene)



entropy %>% 
  ggplot(aes(x = position, y = entropy, color = gene, fill = gene)) +
  geom_bar(stat = "identity",
            position = "identity")





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
         originating_lab, author) %>% 
  print()

gis.data <- left_join(gis.data, cent, by = c("admin_division" = "NM_ESTADO"))

gis.datai <- gis.data %>% 
  select(strain, country, admin_division, age, sex, pango_lineage, clade, originating_lab, collection_data, 
         originating_lab, author, x_cent, y_cent) %>% 
  group_by(admin_division, x_cent, y_cent) %>% 
  count(pango_lineage) %>% 
  spread(pango_lineage, n, fill = 0) %>% 
  ungroup() %>% 
  print()


  
col.name <- gis.datai %>% 
  select(!c(admin_division, x_cent, y_cent)) %>% 
  print()


colors <- c("#7FFFD4", "#8A2BE2", "#1874CD", "#66CD00", "#EE2C2C", "#EEC900", "#FF6EB4", "#FF8247", "#00008B", "#8B3626")

leaflet() %>% 
  addTiles() %>% 
  addMinicharts(gis.datai$x_cent, 
                gis.datai$y_cent, 
                type = "pie", 
                chartdata = col.name ,
                colorPalette = colors,
                width = 30,
                opacity = 0.8
  )


#[, c('B.1', 'B.1.1', 'B.1.1.28', 'B.1.1.33', 'B.1.1.378', 'B.1.195', 'B.40', 
##·   'P.1', 'P.2')]


output$plot2 <-  renderD3heatmap({
  
  plot2i <- d3heatmap(
    dataPong2,
    colors = colorRampPalette(rev(brewer.pal(11, "PRGn")))(256),
    revC = TRUE,
    scale = 'none',
    k_col = 4,
    k_row = 4,
    show_grid = FALSE,
    cexRow = 1,
    cexCol = 0.75,
    digits = 20)
  #theme = "dark")
})


d3heatmapOutput(
  outputId = 'plot2', 
  width = "100%", 
  height = "585px"
)



gis.data <- metadata %>% 
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


#"gene"     "position" "entropy" 
#
i <- entropy %>% 
  mutate(gene = factor(gene, levels = c('ORF1a', 'ORF1b', 'S', 'ORF3a', 'E', 'M', 'ORF6', 'ORF7a', 'ORF7b', 'ORF8', 'ORF9b', 'N'), 
                       ordered = TRUE)) %>% 
  ggplot(aes(x = gene, y = entropy, fill = gene)) +
  geom_violin(width = 1.9, alpha = 0.7) +
  geom_boxplot(width = 0.3, color = "grey", alpha = 0.1, outlier.size = 1.4, outlier.color = "#FFDAB9") +
  scale_fill_manual(values = c("seagreen2", "goldenrod1", "firebrick1", "chartreuse4", "hotpink1", "dodgerblue", "olivedrab1", "cyan3", "coral", "lightpink2", "lightgoldenrod4", "maroon1")) +
  theme_modern_rc(base_size = 12, axis_title_size = 14, ticks = TRUE) +
  labs(x = "Gene", y = "Entropia", fill = "Gene")

ggplotly(i)


fig <- entropy %>%
  mutate(gene = factor(gene, levels = c('ORF1a', 'ORF1b', 'S', 'ORF3a', 'E', 'M', 'ORF6', 'ORF7a', 'ORF7b', 'ORF8', 'ORF9b', 'N'), 
                       ordered = TRUE)) %>% 
  plot_ly(
    x = ~gene,
    y = ~entropy,
    split = ~gene,
    type = 'violin',
    box = list(
      visible = T
    ),
    meanline = list(
      visible = T
    ), 
    fig.layout.template = 'plotly_dark'
  ) %>% 
  layout(
    xaxis = list(
      title = "Day"
    ),
    yaxis = list(
      title = "Entropia",
      zeroline = F
    )
  )
  
  
  


data <- metadata %>% 
  clean_names() %>% 
  #filter(country == "Brazil") %>% 
  select(collection_data) %>% 
  mutate(collection_data = as_date(collection_data)) %>% 
  arrange(desc(collection_data)) %>% 
  mutate(collection_data = first(collection_data)) %>% 
  distinct() %>% 
  mutate(collection_data = format(as.Date(collection_data), format = "%d %B %Y")) %>% 
  print()
  
paste("Última actualização", data$collection_data, sep = ": ")








geres <- read_excel("municipios_geres.xlsx")

report <- read_excel("report.xlsx") %>% 
  clean_names()

caso_full <- read_csv('https://data.brasil.io/dataset/covid19/caso_full.csv.gz')


caso_full <- caso_full %>% 
  select(date, state, city,new_confirmed, new_deaths) %>% 
  filter(state == "PE", city != "Importados/Indefinidos") %>% 
  mutate(city = stringr::str_to_upper(city), new_confirmed = abs(new_confirmed), new_deaths = abs(new_deaths)) %>% 
  print()


caso_full1 <- left_join(caso_full, geres, by = c("city" = "Municipio"))


fileData <- reactiveFileReader(1000, NULL, 'data.csv', read.csv)

caso_full <- read_csv('https://data.brasil.io/dataset/covid19/caso_full.csv.gz')

caso_full2 <- caso_full1 %>% 
  select(date, state, city, new_confirmed, new_deaths) %>% 
  filter(state == "PE", city != "Importados/Indefinidos") %>%
  print()


caso_full1 <- left_join(caso_full, geres, by = c("city" = "Municipio"))

caso_full1i <-caso_full1 %>% 
  mutate(city = stringr::str_to_upper(city), new_confirmed = abs(new_confirmed), new_deaths = abs(new_deaths)) %>% 
  filter(geres %in% c("II")) %>% 
  group_by(date) %>% 
  summarise(new_confirmed = sum(new_confirmed), new_deaths = sum(new_deaths)) %>% 
  mutate(rollmeanC = round(rollmean(new_confirmed, k = 7, fill = NA, align = "right"), digits = 2),
         rollmeanD = round(rollmean(new_deaths, k = 7, fill = NA, align = "right"), digits = 2)) %>% 
  ungroup() %>% 
  print()


caso_full1i %>% 
  ggplot(aes(x = date, y = new_confirmed)) + 
  geom_bar(position = "stack", stat = 'identity', fill = '#C1CDCD', alpha = 0.7 ) +
  geom_line(aes(y = rollmeanC), color = 'dodgerblue4', size = 1.2) +
  #theme_half_open(font_size = 14) +
  theme_clean() +
  scale_x_date(date_labels = "%b , %y", date_breaks = "30 day") +
  theme(axis.text = element_text(color = "dimgray", size = 12), axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = 'Data', y = 'Número de novos casos')


caso_full1i %>% 
  ggplot(aes(x = date, y = new_deaths)) + 
  geom_bar(position = "stack", stat = 'identity', fill = '#C1CDCD', alpha = 0.7 ) +
  geom_line(aes(y = rollmeanD), color = 'firebrick3', size = 1.2) +
  theme_clean() +
  scale_x_date(date_labels = "%b , %y", date_breaks = "30 day") +
  theme(axis.text = element_text(color = "dimgray", size = 12), axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = 'Date', 
       y = 'Number of new deaths')




WHO <- function(x){
  if (x == "B.1.1.7") {
    rep("Alpha", length(x));
  }else if (x == "B.1.351") {
    rep("Beta", length(x));
  } else if (x == "P.1") {
    rep("Gamma", length(x)); 
  }else if (x == "B.1.617.2") {
    rep("Delta", length(x));
  }else if (x == "B.1.427") {
    rep("Epsilon", length(x));
  } else if (x == "B.1.429") {
    rep("Epsilon", length(x)); 
  }else if (x == "P.2") {
    rep("Zeta", length(x));
  }else if (x == "B.1.525") {
    rep("Eta", length(x));
  } else if (x == "P.3") {
    rep("Theta", length(x)); 
  }else if (x == "B.1.526") {
    rep("Iota", length(x));
  }else if (x == "B.1.617.1") {
    rep("Kappa", length(x));
  }else{
    rep("Sem classificação OMS", length(x)) 
  }
}


report1 <- report %>% 
  select(data_extracao, localizacao, lineage) %>% 
  mutate(across( where(is.character), ~replace_na(., "NÃO INFORMADO"))) %>% 
  filter(lineage != "NA") %>% 
  rename(Municipio = localizacao, data = data_extracao) %>% 
  group_by(Municipio) %>% 
  count(lineage) %>% 
  mutate(Municipio = if_else(Municipio == "VERTENTE DO LERIO", "VERTENTE DO LÉRIO",
                             if_else(Municipio == "SOLIDAO", "SOLIDÃO",
                                     if_else(Municipio == "CALCADO", "CALÇADO",
                                             as.character(Municipio))))) %>% 
  print()

WHO <- function(x){
  if (x == "B.1.1.7") {
    rep("Alpha", length(x));
  }else if (x == "B.1.351") {
    rep("Beta", length(x));
  } else if (x == "P.1") {
    rep("Gamma", length(x)); 
  }else if (x == "B.1.617.2") {
    rep("Delta", length(x));
  }else if (x == "B.1.427") {
    rep("Epsilon", length(x));
  } else if (x == "B.1.429") {
    rep("Epsilon", length(x)); 
  }else if (x == "P.2") {
    rep("Zeta", length(x));
  }else if (x == "B.1.525") {
    rep("Eta", length(x));
  } else if (x == "P.3") {
    rep("Theta", length(x)); 
  }else if (x == "B.1.526") {
    rep("Iota", length(x));
  }else if (x == "B.1.617.1") {
    rep("Kappa", length(x));
  }else{
    rep("Sem classificação OMS", length(x)) 
  }
}


report1i <- report1 %>% 
  mutate(OMS = map(lineage, WHO)) %>% 
  unnest(OMS) %>% 
  print()



geres <- geres %>%
  mutate(Municipio = stringr::str_to_upper(Municipio),
         Municipio = stringr::str_replace_all(Municipio, c(
           "FREI MIGUELINO" = "FREI MIGUELINHO", 
           "SÃO CAETANO" = "SÃO CAITANO", 
           "ITAMARACÁ" = "ILHA DE ITAMARACÁ",
           "IGUARACI" = "IGUARACY"
         ))) %>% 
  print()


report1i <- left_join(report1i, geres)
