library(shiny)
library(ggplot2)
library(dplyr)
library(readxl)
library(bslib)
library(gt)
library(DT)
library(plotly)
library(leaflet)
library(sf)

#DANE
poznan_geo <- st_read("poznan.geojson") %>%
  st_transform(4326)

df <- readRDS("dane.rds")

centralne_dzielnice <- c("Centrum", "Stare Miasto", "Jeżyce", "Wilda", "Łazarz", "Rataje", "Winogrady")

#SZYBKA IMPUTACJA
df_clean <- df %>%
  mutate(
    flat_build_year = ifelse(flat_build_year <= 1800 | flat_build_year > 2024, NA, flat_build_year),
  ) %>%
  group_by(quarter) %>%
  mutate(
    flat_build_year = ifelse(is.na(flat_build_year), 
                             median(flat_build_year, na.rm = TRUE), 
                             flat_build_year)
  ) %>%
  ungroup() %>%
  mutate(
    flat_build_year = ifelse(is.na(flat_build_year), 
                             median(flat_build_year, na.rm = TRUE), 
                             flat_build_year),
    individual = ifelse(individual, "Prywatne", "Biuro")
  ) %>%
  filter(flat_area < 350 & price < 15000) %>%
  filter(flat_floor_no >= 0 & flat_floor_no <= 11)

# UI
ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),
  tags$h2("Raport ofert wynajmu nieruchomości w Poznaniu", 
          style = "font-size: 30px; font-weight: 900; letter-spacing: 1px; margin-bottom: 15px; margin-top: 15px;"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Filtry", style = "font-size: 20px;"),
      
      #FILTRY DLA SCATTERPLOT I BOXPLOT
      conditionalPanel(
        condition = "input.tab_panel == 'Static' || input.tab_panel == 'Box'",
        selectInput("selected_quarter", "Dzielnica:",
                    choices = c("Wszystkie", sort(unique(df_clean$quarter))),
                    selected = "Wszystkie")
      ),
      
      #FILTRY DLA WSZYSTKICH OPRÓCZ LOLLIPOP
      conditionalPanel(
        condition = "input.tab_panel != 'student'",
        sliderInput("price_range", "Cena (PLN):", min = 250, max = 10000, value = c(1000, 5000), step = 100),
        sliderInput("area_range", "Powierzchnia (m2):", min = 5, max = 250, value = c(20, 100)),
        hr(),
        radioButtons("is_individual", "Typ ogłoszenia:",
                     choices = c("Wszystkie" = "all", "Prywatne" = "Prywatne", "Biura" = "Biuro"))
      ),
      
      #FILTRY DLA LOLLIPOP
      conditionalPanel(
        condition = "input.tab_panel == 'student'",
        sliderInput("lollipop_max_area", "Pokaż mieszkania do wielkości (m2):", 
                    min = 10, max = 250, value = 65, step = 10),
        helpText("Ten wykres ignoruje filtry ceny i typu ogłoszenia."),
        hr()
      ),
      
      h4("Udogodnienia i AGD", style = "font-size: 18px;"),
      checkboxGroupInput("amenities", "Zaznacz wymagane:",
                         choices = c("Winda" = "flat_lift", 
                                     "Garaż/Miejsce" = "flat_garage",
                                     "Balkon" = "flat_balcony",
                                     "Pralka" = "flat_washmachine", 
                                     "Zmywarka" = "flat_dishwasher",
                                     "Lodówka" = "flat_fridge",
                                     "Piekarnik" = "flat_oven",
                                     "Umeblowane" = "flat_furnished",
                                     "Internet" = "flat_internet"),
                         selected = NULL),
      
      hr(),
      
      #SUNBURST INFO
      conditionalPanel(
        condition = "input.tab_panel == 'Sunburst'",
        helpText("Wykres sunburst automatycznie ogranicza dane do centralnych dzielnic Poznania.")
      )
    ),
    
    mainPanel(
      tabsetPanel(id = "tab_panel",
                  tabPanel("Mapa cena za m2", value = "Map",
                           leafletOutput("mapPlot", height = "600px"),
                           tags$small(style = "color: gray; font-style: italic;",
                                      "Uwaga: Niektóre oferty nie zostały uwzględnione na mapie ze względu na niestandardowe nazwy dzielnic w pliku źródłowym 
                                      lub brak możliwości dopasowania ich do oficjalnych granic administracyjnych.")),
                  
                  tabPanel("Różnice w cenach wg dzielnic: studenci i reszta", value = "student",
                           plotlyOutput("barStudentPlot", height = "600px")),
                  
                  tabPanel("Struktura mieszkań w centrum miasta (sunburst)", value = "Sunburst",
                           plotlyOutput("sunburstPlot", height = "600px")),
                  
                  tabPanel("Cena wg liczby pokoi", value = "Box",
                           plotlyOutput("boxPlot", height = "600px"), 
                           
                           tags$div(
                             style = "margin-top: 10px; font-size: 12px; color: #7f8c8d; font-family: Arial;",
                             tags$p(
                               HTML("<b>Słownik statystyk:</b> <i>Median</i> – mediana (wartość środkowa) | 
                                    <i>Q1/Q3</i> – dolny/górny kwartyl | 
                                    <i>Min/Max</i> – wartości skrajne | 
                                    <i>Lower/Upper Fences</i> – dolne/górne granice rozkładu")))),
                  
                  tabPanel("Wykres rozrzutu (cena/m2)", value = "Static",
                           plotOutput("scatterPlot", height = "600px")),
                  
                  tabPanel("Tabela podsumowująca", value = "TableGT",
                           gt_output("summaryTable"))
      )
    )
  )
)


# SERVER
server <- function(input, output) {

  filtered_data <- reactive({
    data <- df_clean
    if (input$selected_quarter != "Wszystkie") data <- data %>% filter(quarter == input$selected_quarter)
    data <- data %>% 
      filter(price >= input$price_range[1] & price <= input$price_range[2]) %>%
      filter(flat_area >= input$area_range[1] & flat_area <= input$area_range[2])
    
    if (input$is_individual != "all") data <- data %>% filter(individual == input$is_individual)
    
    if (!is.null(input$amenities)) {
      for (amenity in input$amenities) {
        data <- data[data[[amenity]] == TRUE, ]
      }
    }
    data
  })
  

  sunburst_data_source <- reactive({
    data <- df_clean

    data <- data %>% 
      filter(price >= input$price_range[1] & price <= input$price_range[2]) %>%
      filter(flat_area >= input$area_range[1] & flat_area <= input$area_range[2])

    if (!is.null(input$amenities)) {
      for (amenity in input$amenities) {
        data <- data[data[[amenity]] == TRUE, ]
      }
    }
    data
  })
  
  output$scatterPlot <- renderPlot({
    req(nrow(filtered_data()) > 0)
    
    ggplot(filtered_data(), aes(x = flat_area, y = price)) +
      geom_point(aes(fill = as.factor(flat_floor_no)), 
                 shape = 21, color = "grey90", size = 3, alpha = 0.6) +
      
      geom_smooth(method = "lm", color = "black", fill = "black", alpha = 0.1, size = 0.8, linetype = "dashed") +
      
      scale_fill_viridis_d(option = "inferno", name = "Piętro", direction = -1) +
      
      scale_y_continuous(labels = scales::label_number(suffix = " zł", big.mark = " ", decimal.mark = ",")) +
      scale_x_continuous(labels = scales::label_number(suffix = " m²")) +
      
      theme_minimal(base_size = 12, base_family = "Arial") +
      theme(
        plot.title = element_text(face = "bold", size = 18, color = "black"),
        plot.subtitle = element_text(color = "grey40", margin = margin(b = 15), size=12),
        panel.grid.minor = element_blank(),
        legend.position = "right",
        axis.title = element_text()
      ) +
      labs(
        title = "Analiza korelacji ceny i metrażu",
        subtitle = paste("Analiza na podstawie", nrow(filtered_data()), "wybranych ofert nieruchomości"),
        x = "Powierzchnia",
        y = "Cena wynajmu"
      )
  },res = 80, execOnResize = TRUE, alt = "Scatterplot")
  
#BOX PLOT
  output$boxPlot <- renderPlotly({
    req(nrow(filtered_data()) > 0)
    
    p <- ggplot(filtered_data(), aes(x = as.factor(flat_rooms), y = price, fill = as.factor(flat_rooms))) +
      geom_boxplot(
        outlier.size = 1.5,      
        outlier.alpha = 0.3,     
        color = "#2c3e50",    
        alpha = 0.8           
      ) +
      
      scale_fill_viridis_d(option = "inferno", begin = 0.2, end = 0.8) +
      
      scale_y_continuous(labels = scales::label_number(suffix = " zł", big.mark = " ")) +
      
      theme_minimal(base_size = 14) +
      theme(
        legend.position = "none",
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(face = "bold", size = 14), 
        axis.text = element_text(size = 10),            
        axis.title = element_text(size = 10)              
      ) +
      labs(
        title = "Rozkład cen względem liczby pokoi",
        x = "Liczba pokoi",
        y = "Cena [PLN]"
      )
    
    ggplotly(p, tooltip = c("x", "y")) %>%
      layout(
        showlegend = FALSE,
        font = list(size = 14) 
      ) %>%
      config(displayModeBar = FALSE)
  })

  #SUNBURST 
  output$sunburstPlot <- renderPlotly({
    data_sun <- sunburst_data_source() %>%
      filter(quarter %in% centralne_dzielnice) %>%
      filter(flat_rooms >= 1 & flat_rooms <= 3)
    
    req(nrow(data_sun) > 0)
    
    df_q <- data_sun %>%
      group_by(labels = quarter) %>%
      summarise(values = n(), .groups = "drop") %>%
      mutate(parents = "Centrum Poznania")
    
    df_r <- data_sun %>%
      mutate(
        ids = paste(flat_rooms, "pok.", "-", quarter), 
        label_clean = paste(flat_rooms, "pok.")
      ) %>%
      group_by(ids, labels = label_clean, parents = quarter) %>%
      summarise(values = n(), .groups = "drop")
    
    df_root <- data.frame(
      ids = "Centrum Poznania",
      labels = "Centrum Poznania",
      parents = "",
      values = nrow(data_sun)
    )
    df_q <- df_q %>% mutate(ids = labels)
    
    sunburst_data <- bind_rows(df_root, df_q, df_r)
    kolory_inferno <- rev(viridis::inferno(nrow(sunburst_data), begin = 0.2, end = 0.8))

    plot_ly(
      data = sunburst_data,
      ids= ~ids,
      labels = ~labels,
      parents = ~parents,
      values = ~values,
      type = 'sunburst',
      branchvalues = 'total',
      textinfo = "label",
      insidetextorientation = 'horizontal',
      insidetextfont = list(color = "#FFFFFF", size = 14, family = "Arial"),
      marker = list(colors = kolory_inferno),
      hovertemplate = paste(
        "<b>%{label}</b><br>",
        "Liczba ofert: %{value}<br>",
        "Udział: %{percentParent:.1%}<extra></extra>"
      )
    ) %>%
      layout(
        title = list(
          text = "<span style='font-weight: 800; letter-spacing: 1px;'>Struktura ogłoszeń dla pokoi 1-3 w centrum miasta</span>",
          font = list(size = 18, family = "Arial", color = "black"),
          x = 0.08
        ),
        font = list(size = 14, family = "Arial", color = "#FFFFFF"),
        hoverlabel = list(
          font = list(color = "#FFFFFF", family = "Arial", size = 12)
        ),
        margin = list(l = 10, r = 10, b = 10, t = 80)
      ) %>%
      config(displayModeBar = FALSE)
  })

#MAPA
  output$mapPlot <- renderLeaflet({

    mapa_czysta <- poznan_geo %>%
      st_make_valid() %>%
      mutate(name = case_when(
        name %in% c("Nowe Winogrady Południe", "Nowe Winogrady Wschód", 
                    "Nowe Winogrady Północ", "Stare Winogrady") ~ "Winogrady",
        
        name %in% c("Stary Grunwald", "Grunwald Południe", "Grunwald Północ") ~ "Grunwald",
        
        name == "Zielony Dębiec" ~ "Dębiec",
        name == "Krzesiny-Pokrzywno-Garaszewo" ~ "Krzesiny",
        name == "Krzyżowniki-Smochowice" ~ "Smochowice",
        name == "Antoninek-Zieliniec-Kobylepole" ~ "Antoninek",
        name == "Starołęka-Minikowo-Marlewo" ~ "Starołęka",
        name == "Szczepankowo-Spławie-Krzesinki" ~ "Szczepankowo",
        name == "Morasko-Radojewo" ~ "Morasko",
        name == "Warszawskie-Pomet-Maltańskie" ~ "Malta",
        name == "Ostrów Tumski-Śródka-Zawady-Komandoria" ~ "Śródka",
        name == "Fabianowo-Kotowo" ~ "Fabianowo",
        name == "Piątkowo Północ" ~ "Piątkowo",
      
        
        name %in% c("Jeżyce", "Wilda", "Stare Miasto", "Rataje", "Łazarz", 
                    "Piątkowo", "Naramowice", "Podolany", "Sołacz", 
                    "Winiary", "Górczyn", "Świerczewo", "Strzeszyn", 
                    "Wola", "Ławica", "Chartowo", "Żegrze", "Główna",
                    "Umultowo", "Junikowo", "Głuszyna", "Ogrody") ~ name,
        TRUE ~ NA_character_
      )) %>%
      filter(!is.na(name)) %>%
      group_by(name) %>%
      summarise(geometry = st_union(geometry)) %>% 
      st_make_valid() %>%
      ungroup()
    
    stats_map <- filtered_data() %>%
      mutate(price_per_m2 = price / flat_area) %>%
      group_by(quarter) %>%
      summarise(avg_price_m2 = mean(price_per_m2, na.rm = TRUE), n_offers = n())


    map_final <- left_join(mapa_czysta, stats_map, by = c("name" = "quarter")) %>%
      filter(!is.na(avg_price_m2))

    pal <- colorNumeric(
      palette = rev(viridis::inferno(100, begin=0.45)), 
      domain = map_final$avg_price_m2
    )
    
    pal_rev <- colorNumeric(
      palette = viridis::inferno(100, begin=0.45),
      domain = map_final$avg_price_m2
    )
    
    leaflet(map_final) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~pal(avg_price_m2),
        weight = 1.5, color = "white", fillOpacity = 0.8,
        highlightOptions = highlightOptions(weight = 3, color = "#2c3e50", bringToFront = TRUE),
        label = lapply(paste0(
          "<div style='font-size: 14px; font-family: Arial;'>",
          "<strong>", map_final$name, "</strong><br/>",
          "Śr. cena: ", format(round(map_final$avg_price_m2, 0), big.mark = " "), " PLN/m²<br/>",
          "Liczba ofert: ", map_final$n_offers,
          "</div>"
          ), htmltools::HTML),
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "10px"),
            textsize = "14px",
            direction = "auto"
          )
        ) %>%
        addLegend(
          pal = pal_rev, 
          values = ~avg_price_m2, 
          title = "Cena PLN/m²", 
          position = "bottomright",
          labFormat = labelFormat(big.mark = " ", transform = function(x) sort(x, decreasing = TRUE))
        )
  })

#LOLLIPOP 
  output$barStudentPlot <- renderPlotly({
    data_for_lollipop <- df_clean %>%
      filter(flat_area <= input$lollipop_max_area)

    if (!is.null(input$amenities)) {
      for (amenity in input$amenities) {
        data_for_lollipop <- data_for_lollipop[data_for_lollipop[[amenity]] == TRUE, ]
      }
    }

    valid_quarters <- data_for_lollipop %>%
      mutate(Typ = ifelse(flat_for_students, "Student", "Pozostale")) %>%
      group_by(quarter, Typ) %>%
      summarise(count = n(), .groups = "drop") %>%
      tidyr::pivot_wider(names_from = Typ, values_from = count, values_fill = 0) %>%
      filter(Student > 40 & Pozostale > 40) %>%
      pull(quarter)

    plot_data <- data_for_lollipop %>%
      filter(quarter %in% valid_quarters) %>%
      mutate(Typ = ifelse(flat_for_students, "Dla studentów", "Pozostałe")) %>%
      group_by(quarter, Typ) %>%
      summarise(avg_price = mean(price, na.rm = TRUE), .groups = "drop")

    p <- ggplot(plot_data, aes(x = avg_price, y = reorder(quarter, avg_price),
                               text = paste0("<b>", quarter, "</b><br>",
                                             "Kategoria: ", Typ, "<br>",
                                             "Śr. cena: ", format(round(avg_price, 0), big.mark = " "), " zł"))) +
      geom_line(aes(group = quarter), color = "#dfe6e9", size = 1.2) +
      geom_point(aes(color = Typ), size = 4, alpha=0.9) +
      scale_color_manual(values = c("Dla studentów" = "#f39c12", "Pozostałe" = "#8e44ad")) +
      scale_x_continuous(labels = scales::label_number(suffix = " zł", big.mark = " ")) +
      theme_minimal(base_size = 12) +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position = "top"
      ) +
      labs(x = "Średnia cena", y = "",
           color = "Kategoria")
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        title = list(
          text = paste0(
            "<b><span style='font-weight: 800; letter-spacing: 1px'>Relacja cen wg dzielnic dla studentów i pozostałych</span></b>",
            "<br><span style='font-size: 12px; color: grey; font-weight: normal;'>Dzielnice z bazą ofert większą niż 40</span>"
          ),
          font = list(family = "Arial", size = 18, color = "black"),
          x = 0.08,
          y = 0.95
        ),
        font = list(family = "Arial", size = 14),
        margin = list(l = 50, r = 50, b = 80, t = 100),
        legend = list(orientation = "h", x = 0.3, y = -0.25)
      ) %>%
      config(displayModeBar = FALSE)
  })

#TABELA
  output$summaryTable <- render_gt({
    req(filtered_data())
    
    table_data <- filtered_data() %>%
      mutate(price_per_m2 = price / flat_area) %>%
      group_by(Dzielnica = quarter) %>%
      summarise(
        `Liczba ofert` = n(),
        `Śr. cena m2` = mean(price_per_m2, na.rm = TRUE),
        `Śr. rok budowy` = mean(flat_build_year, na.rm = TRUE),
        `Dla studentów` = mean(flat_for_students, na.rm = TRUE) * 100,
        `Mediana pow. [m2]` = median(flat_area, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(`Liczba ofert` > 5) %>% 
      arrange(desc(`Liczba ofert`))
    
    if (nrow(table_data) == 0) {
      data.frame(Komunikat = "Brak danych dla wybranych filtrów.") %>%
        gt() %>%
        tab_header(title = "Uwaga") %>%
        tab_options(table.width = pct(100), table.font.size = 14)
    } else {
      table_data %>%
        gt() %>%
        tab_header(
          title = "Tabela ogólna",
          subtitle = "Sortowanie dzielnic według liczby ofert"
        ) %>%
        fmt_number(
          columns = `Śr. cena m2`, 
          decimals = 2,
          sep_mark = " ", 
          dec_mark = ",",
          pattern = "{x} zł"
        ) %>%
        fmt_number(
          columns = `Mediana pow. [m2]`, 
          decimals = 2, 
          sep_mark = "", 
          dec_mark = "."
        ) %>%
        fmt_number(
          columns = `Dla studentów`, 
          decimals = 1,
          pattern = "{x}%"
        ) %>%
        fmt_number(columns = `Śr. rok budowy`, decimals = 0, use_seps = FALSE) %>%
        data_color(
          columns = `Liczba ofert`,
          palette = "inferno",
          alpha = 0.8
        ) %>%
        data_color(
          columns = `Śr. rok budowy`,
          palette = "Purples",
          domain = c(1900, 2025)
        ) %>%
        tab_options(
          table.width = pct(100),
          column_labels.font.weight = "bold"
        ) %>%
        cols_align(align = "center", columns = -Dzielnica)
    }
  })
}

shinyApp(ui, server)