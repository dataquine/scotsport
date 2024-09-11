# File: server.R
# Author: Lesley Duff
# Date created: 2024-08-31
# Description:
#  Handle the server side of the ScotSport Shiny app

function(input, output, session) {
  scotland_areas <- reactive( # input$select_council,
    {
      selected_council_name <- input$select_council
      num_councils <- length(selected_council_name)

      areas <- df_sports_facilities_scotland |>
        group_by(la_name)

      if (num_councils > 0) {
        areas <- areas |>
          filter(la_name %in% selected_council_name)
      }

      areas <- areas |>
        ungroup()

      return(areas)
    }
  )

  # Build gt table object of counts by Facility type and subtype
  # This will be filtered by council area where user has made a selection
  tbl_facility_subtype_count <- reactive({
    selected_councils <- input$select_council

    scotland_areas() |>
      count(facility_type, facility_sub_type,
        name = "Facilities"
      ) |>
      #  arrange() |>  ##desc(Facilities)) |>
      # Rearrange subtypes according to highest facilities per subtype
      arrange(facility_type, desc(Facilities)) |>
      gt::gt(
        rowname_col = "facility_sub_type",
        groupname_col = "facility_type",
        #    rownames_to_stub = TRUE,
        id = "tbl_type"
      ) |>
      tab_header(
        title = md("**Type of sports facility**"),
        subtitle = md(glue::glue("Council area: *{selected_councils}*"))
      ) |>
      cols_label(
        facility_type = "**Type**",
        facility_sub_type = "**Subtype**",
        #  la_name = "**Council area**",
        Facilities = "**Facilities**",
        .fn = md
      ) |>
      data_color(
        columns = Facilities,
        method = "numeric",
        palette = "Blues",
        contrast_algo = "wcag" #  (Web Content Accessibility Guidelines)
      ) |>
      summary_rows(
        columns = Facilities,
        fns = list(
          id = "summary_total",
          label = "Total",
          fn = "sum"
        ),
        fmt = ~ fmt_integer(.),
        missing_text = "",
        side = "top"
      ) |>
      tab_stubhead(label = "Subtype") |>
      tab_style(
        style = list(cell_text(
          weight = "bold", size = "large",
          align = "center"
        )),
        locations = cells_row_groups()
      ) |>
      tab_style(
        style = list(
          cell_text(weight = "bold")
        ),
        locations = list(cells_stubhead(), cells_stub_summary())
      ) |>
      tab_style(
        style = list(
          cell_text(weight = "bold") # ,
          #   cell_fill(color = "grey95")
        ),
        locations = cells_summary(columns = Facilities)
      ) |>
      grand_summary_rows(
        columns = Facilities,
        fns = list(
          label = md("**Total**"), id = "grand_summary_total",
          fn = "sum"
        ),
        fmt = ~ fmt_integer(.),
        missing_text = "",
        side = "top"
      ) |>
      tab_style(
        style = list(
          cell_text(weight = "bold") # ,
          #      cell_fill(color = "grey80")
        ),
        locations = cells_grand_summary(columns = Facilities)
      )
  })

  towns_with_facilities <- reactive({
    # How many towns have facilities
    towns <- scotland_areas() |>
      # For town calculations make all (Near) entries the same town
      mutate(town = stringr::str_remove(town, " (Near)")) |>
      distinct(town)

    total <- nrow(towns)
    # print("towns_with_facilities total=")
    #  print(total)

    return(total) # list(towns = towns, total = total))
  })

  selected_town <- reactive({
    sel_town <- input$select_town
    req(sel_town)

    print("selected_town")
    print(sel_town)
    towns <- scotland_areas() |>
      filter(town == sel_town) |>
      drop_na(lng, lat)
    print(head(towns))
    towns
  })

  # sorted_selected_area_names <- reactive({
  sorted_selected_area_names <- function() {
    sort(input$select_council)
  }

  # Outputs ----------------------------------------------------------------------
  output$area_names_home <- renderText({
    build_area_names(sorted_selected_area_names())
  })

  output$area_names_types <- renderText({
    build_area_names(sorted_selected_area_names())
  })

  output$total_facilities_areas <- renderText({
    scales::label_comma()(nrow(scotland_areas()))
  })

  output$total_people_per_facility <- renderText({
    facilities_per_council <- scotland_areas() |>
      # How many facilities per council
      count(la_name)

    combined_council_info <- facilities_per_council |>
      inner_join(population_council, by = join_by(la_name == area_name))
    # print(combined_council_info)

    # Population divided by number of facilities
    people_per_facility <- sum(combined_council_info$population) /
      sum(combined_council_info$n)

    # selected_council_name <- input$select_council
    # num_councils <- length(selected_council_name)
    #
    # # All Scotland
    # people_per_facility <- 0
    # if (num_councils == 0) {
    #   # people_per_facility <- population_scotland / total_facilities_scotland
    #   people_per_facility <- population_country$population / total_facilities_scotland
    # } else {
    #   num_facilities <- nrow(scotland_areas())
    #
    #   population_selected_councils <- population_council |>
    #     filter(area_name %in% selected_council_name) |>
    #     # rename(population = last_col()) |>
    #     summarise(total_population = sum(population))
    #
    #   # print(population_selected_councils)
    #
    #   people_per_facility <- population_selected_councils$total_population /
    #     num_facilities
    # }

    scales::label_comma()(people_per_facility)
  })
  # people_per_facility <- population_scotland / total_facilities_scotland

  output$total_towns_with_facilities <- renderText({
    scales::label_comma()(towns_with_facilities())
  })

  output$plot_facility_type <- renderPlot(
    {
      # alt_text <- print(home_type())
      df_type <- scotland_areas() |>
        group_by(facility_type) |>
        summarise(n = n()) |>
        ungroup() |>
        mutate(facility_type = fct_reorder(as.factor(facility_type), n,
          .desc = FALSE
        ))

      # selected_council_name <- sort(input$select_council)
      # area_names <- build_area_names(selected_council_name)
      # print(area_names)

      plot_facility_type(df_type)
    }
    # ,
    # alt = reactive({
    #   # code to add alt text goes here
    # })
  )

  output$map_town <- renderLeaflet({
    # renderMapview({
    # renderUI({
    # renderLeaflet({
    town <- selected_town()
    # filter((town$lng != Inf) & (town$lat != Inf))
    req(town)

    min_lng <- min(town$lng)
    min_lat <- min(town$lat)
    max_lng <- max(town$lng)
    max_lat <- max(town$lat)

    print("#############################################")
    print(paste(min_lng, min_lat, max_lng, max_lat))

    town_types <- unique(town$facility_type)

    pal <- # colorBin("Blues", domain = town_types, bins = length(town_types))
      colorFactor("Blues",
        # c("navy", "red", "yellow", "green",
        #                    "navy", "red", "yellow", "green",
        #                    "navy", "red", "yellow", "green"),
        domain = town_types
      )
    print(pal)

    mymap <- leaflet(data = town) |>
      # Centre the empty map
      # setView((max_lng - min_lng)/2, (max_lat - min_lat)/2, zoom = 18) |>

      #  setMaxBounds(min_lng, min_lat, max_lng, max_lat) |>  # , options = list())
      addTiles() |>
      addScaleBar() |>
      # # Add a legend to the map
      # leaflet::addLegend(
      #   "topright",
      #   pal = pal,
      #   values = town_types, #~facility_type,
      #   title = "Type"
      # ) |>
      # leaflet::addProviderTiles("Esri.WorldStreetMap", group = "World Street Map") %>%
      # leaflet::addProviderTiles("CartoDB.DarkMatter", group = "Dark Matter") %>%
      # leaflet::addProviderTiles("Esri.WorldImagery", group = "World Imagery (satellite)") %>%
      addProviderTiles(
        provider = "Esri.WorldImagery",
        group = "World Imagery (satellite)"
      ) |>
      fitBounds(min_lng, min_lat, max_lng, max_lat) |>
      clearBounds() # world view

    for (i in 1:length(town_types)) {
      group_data <- town[town$facility_type == town_types[i], ]
      #    print(group_data)
      # Create the popup content
      popup_content <- paste0(
        "<table>",
        "<tr><th>Name:</th><td>", htmltools::htmlEscape(group_data$name), "</td></tr>",
        "<tr><th>Address:</th><td>", htmltools::htmlEscape(group_data$address), "</td></tr>",
        "<tr><th>Town:</th><td>", htmltools::htmlEscape(group_data$town), "</td></tr>",
        "<tr><th>Postcode:</th><td>", htmltools::htmlEscape(group_data$postcode), "</td></tr>",
        "<tr><th>Council area:</th><td>", htmltools::htmlEscape(group_data$la_name), "</td></tr>",
        "</table>",
        "<hr />",
        "<table>",
        "<tr><th>Type:</th><td>", htmltools::htmlEscape(group_data$facility_type), "</td></tr>",
        "<tr><th>Subtype:</th><td>", htmltools::htmlEscape(group_data$facility_sub_type), "</td></tr>",
        "</table>" # ,
        # "<hr />",
        # "<table>",
        # "<tr><th>Surface:</th><td>", htmltools::htmlEscape(group_data$surface), "</td></tr>",
        # "<tr><th>Flood lights:</th><td>", htmltools::htmlEscape(group_data$flood_lights), "</td></tr>",
        # "</table>"
      )

      mymap <- addMarkers(
        map = mymap,
        lng = group_data$lng,
        lat = group_data$lat,
        #     layerId = NULL,
        group = town_types[i], # htmltools::htmlEscape(group_data$facility_type),
        #  #    icon = NULL,
        popup = ~popup_content,
        #     # popupOptions = NULL,
        label = ~ htmltools::htmlEscape(group_data$name),
        # #     labelOptions = NULL,
        options = markerOptions(),
        clusterOptions = markerClusterOptions(
          removeOutsideVisibleBounds = TRUE,

          # when you mouse over a cluster it shows the bounds of its markers
          # showCoverageOnHover = TRUE

          # when you click a cluster we zoom to its bounds
          zoomToBoundsOnClick = TRUE

          # when you click a cluster at the bottom zoom level we spiderfy it so you can see all of its markers
          # spiderfyOnMaxZoom = TRUE,

          # spiderLegPolylineOptions = list(weight = 1.5, color = "#222", opacity = 0.5),
          # freezeAtZoom = FALSE,
        )
        # ,
        # clusterId = NULL,
        # data = getMapData(map)
      )


      # # print(group_data)
      # mymap <- addCircleMarkers(
      #   map = mymap,
      #   lng = group_data$lng,
      #   lat = group_data$lat,
      #   popup = ~popup_content,
      #   label = htmltools::htmlEscape(group_data$name),
      #   group = htmltools::htmlEscape(group_data$facility_type),
      #   # radius = ~ sqrt(anount / 1000),
      #   color = ~ pal(group_data$facility_type),
      #   clusterOptions = markerClusterOptions(
      #     removeOutsideVisibleBounds = TRUE,
      #
      #     # when you mouse over a cluster it shows the bounds of its markers
      #     # showCoverageOnHover = TRUE
      #
      #     # when you click a cluster we zoom to its bounds
      #     zoomToBoundsOnClick = TRUE
      #
      #     # when you click a cluster at the bottom zoom level we spiderfy it so you can see all of its markers
      #     # spiderfyOnMaxZoom = TRUE,
      #
      #     # spiderLegPolylineOptions = list(weight = 1.5, color = "#222", opacity = 0.5),
      #     # freezeAtZoom = FALSE,
      #   ),
      #   labelOptions = labelOptions(
      #     noHide = FALSE,
      #     direction = "auto"
      #     #
      #     # interactive = FALSE,
      #     # clickable = NULL,
      #     # noHide = NULL,
      #     # permanent = FALSE,
      #     # className = "",
      #     # direction = "auto",
      #     # offset = c(0, 0),
      #     # opacity = 1,
      #     # textsize = "10px",
      #     # textOnly = FALSE,
      #     # style = NULL,
      #     # zoomAnimation = NULL,
      #     # sticky = TRUE,
      #   )
      # )
    }
    # For controlling showing/hiding layers
    mymap <- addLayersControl(
      map = mymap,
      baseGroups = c(
        "OSM (default)",
        #       "Positron (minimal)",
        #   "World Street Map",
        #  "Dark Matter",
        "World Imagery (satellite)"
      ),
      overlayGroups = town_types, # "Types"), #1:length(town_types)),
      # position = "topright",
      options = layersControlOptions(
        collapsed = TRUE,

        # if TRUE, the control will automatically maintain the z-order of its
        # various groups as overlays are switched on and off.
        autoZIndex = TRUE
      )
    )

    mymap

    # town |>
    # leaflet()  |>
    #   addTiles() |>   # Add default OpenStreetMap map tiles
    #   #   addLegend(pal = pal, values = ~vble, opacity = 0.8)
    #   # addMiniMap()
    #
    #   addMarkers(lng=174.768, lat=-36.852, popup="The birthplace of R")
  })


  # plot helper functions --------------------------------------------------------
  plot_facility_type <- function(df,
                                 plot_title = "Facilites by Type",
                                 plot_fill_bar_chart = "dodgerblue4") {
    df |>
      ggplot(aes(x = facility_type, y = n)) +
      geom_col(fill = plot_fill_bar_chart) +
      # +
      #   geom_text(aes(label = paste0(scales::label_comma()(n), " (", ")"),
      #                 y = n + 0.05),
      #             colour = "white",
      #             position = position_stack(vjust = 0.5),
      #             #  vjust = 0
      #   ) +
      #   coord_flip() +
      scale_y_continuous(labels = scales::label_comma()) +
      theme_minimal(base_size = 16) +
      #          base_family = "Source Sans Pro"
      labs(
        title = plot_title,
        x = element_blank(),
        y = element_blank()
        #    alt = "A bar chart titled ..."
      ) +
      theme(
        #    legend.position = "none",
        #   panel.grid = element_blank()
      ) +
      coord_flip()
  }

  # Helper functions
  build_area_names <- function(selected_council_name) {
    num_councils <- length(selected_council_name)

    if (num_councils > 0) {
      councils <- paste(selected_council_name, collapse = ", ")
      areas <- paste("Council area(s):", councils)
    } else {
      areas <- "Area: All Scotland"
    }
    return(areas)
  }

  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      #    paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      #     write_csv(datasetInput(), file, row.names = FALSE)
    }
  )


  # Old ############################################################################

  # home_type <- reactiveVal(df_type)
  # home_type_council <- reactiveVal(df_type_council)

  total_councils <- reactive({
    selected_council_name <- input$select_council
    num_councils <- length(selected_council_name)
    count_councils <- df_sports_facilities_scotland |>
      group_by(la_name)

    total_councils <- 0

    if (num_councils > 0) {
      count_councils <- count_councils |>
        filter(la_name %in% selected_council_name)
      total_councils <- nrow(count_councils)
    } else {
      # home_type(df_type)
    }
    return(total_councils)
  })

  ### OLD ########################################################################

  plot_type_council <- function(df,
                                caption = caption_source_sport_scotland,
                                fill = "dodgerblue4") {
    filter_councils <- input$select_council
    num_councils <- length(filter_councils)

    df_plot <- df
    if (num_councils > 0) {
      print(filter_councils)
      print(names(df_plot))
      df_plot <- df_plot |>
        filter(la_name %in% filter_councils)
    }

    # print(df_plot, n = Inf)
    sorted_facilities <- df_plot |>
      group_by(facility_type) |>
      summarise(n = sum(n)) |>
      ungroup() |>
      mutate(facility_type = fct_reorder(
        as.factor(facility_type),
        n
      ))
    # print(sorted_facilities, n = Inf)

    sorted_facilities |>
      ggplot(aes(x = facility_type, y = n)) +
      geom_col(fill = fill) +
      # geom_text(
      #   mapping = aes(label = round(n, 0)),
      #   position = position_dodge(width = 0.7),
      #   hjust = 1.5,
      #   size = 6,
      #   fontface = "bold",
      #   colour = "white"
      # ) +
      coord_flip() +
      theme_minimal(
        # base_size = 16,
        #          base_family = "Source Sans Pro"
      ) +
      scale_y_continuous(
        labels = scales::label_comma()
      ) +
      labs(
        title = "Facilites per Type",
        x = element_blank(),
        y = "Facilities",
        caption = caption,
        alt = "A bar chart titled ..."
      ) +
      theme(
        legend.position = "none",
        # panel.grid.major.x = element_blank(),
        # panel.grid.minor.x = element_blank(),
        # panel.grid.minor.y = element_blank()
        panel.grid = element_blank()
      )
    #  plot.title = element_textbox_simple(face = "bold"))
  }

  gt_tbl_facility_detail <- reactive({
    my_council_name <- sort(input$select_council)

    # df_scotland_areas <- scotland_areas()
    # print(nrow(df_scotland_areas))

    my_sep <- paste0(", ", br())

    #   print(names(df_scotland_areas))
    #  print(names(df_sports_facilities_scotland))

    df_facility_detail <- scotland_areas() |>
      # df_facility_detail <-df_sports_facilities_scotland |>
      # We are displaying addresses as if for mailing purposes
      # Remove any "(Near)"
      mutate(town = stringr::str_remove(town, "(Near)")) |>
      dplyr::select(
        name, address, facility_type, facility_sub_type,
        town, postcode, la_name
      ) |>
      #  df_facility_detail <- df_facility_detail |>
      tidyr::unite("full_address",
        c(name, address, town, postcode),
        sep = my_sep,
        remove = FALSE,
        na.rm = TRUE
      ) |>
      mutate(facility_type = paste0(facility_type, sep = ": ", facility_sub_type)) |>
      relocate(full_address, .before = name) |>
      relocate(la_name, .after = full_address) |>
      select(-name, -address, -facility_sub_type, -town, -postcode) |>
      arrange(full_address, la_name, facility_type)

    df_facility_detail <- df_facility_detail |>
      group_by(full_address, la_name, facility_type) |>
      summarise(how_many = n()) |>
      ungroup()

    df_facility_detail |>
      # gt(rowname_col = "row",
      #    groupname_col = "full_address",
      #    row_group_as_column = TRUE
      #    ) |>
      gt() |>
      tab_header(
        title = md("**Sports facilities by address**"),
        subtitle = md(glue::glue("Council area: *{my_council_name}*"))
      ) |>
      cols_label(
        full_address = md("**Address**"),
        la_name = md("**Council area**"),
        facility_type = md("**Type**"),
        how_many = md("**How many?**")
      ) |>
      # cols_align(align = "center") |>
      opt_row_striping() |>
      #  opt_stylize(style = 6, color = "blue") |>
      opt_interactive(
        use_compact_mode = TRUE,
        use_search = TRUE,
        use_highlight = TRUE,
        use_page_size_select = TRUE
      )
  })

  output$facility_type_table <- render_gt(
    {
      tbl_facility_subtype_count()
    }
    # expr = gt_tbl_facility_type()
  )

  output$facility_detail_table <- render_gt({
    gt_tbl_facility_detail()
  })
}
