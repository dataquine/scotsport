# File: server.R
# Author: Lesley Duff
# Date created: 2024-08-31
# Description:
#  Handle the server side of the ScotSport Shiny app

function(input, output, session) {
  # Reactives ------------------------------------------------------------------
  scotland_areas <- reactive({
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
  }) |>
    bindCache(input$select_council)

  # Build gt table object of counts by Facility type and subtype
  # This will be filtered by council area where user has made a selection
  tbl_facility_subtype_count <- reactive({
    selected_councils <- input$select_council

    scotland_areas() |>
      count(facility_type, facility_sub_type,
        name = "Facilities"
      ) |>
      arrange(facility_type, desc(Facilities)) |>
      gt(
        rowname_col = "facility_sub_type",
        groupname_col = "facility_type",
        id = "tbl_type"
      ) |>
      tab_header(
        title = md("**Type of sports facility**"),
        subtitle = md(glue::glue("Council area: *{selected_councils}*"))
      ) |>
      cols_label(
        facility_type = "**Type**",
        facility_sub_type = "**Subtype**",
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
        side = "bottom"
      ) |>
      tab_stubhead(label = "Subtype") |>
      tab_style( # Table title
        style = cell_text(color = scotsport_text_colour),
        locations = cells_title(groups = "title")
      ) |>
      tab_style(
        style = cell_text(
          weight = "bold",
          size = "large",
          color = scotsport_text_colour,
          align = "center"
        ),
        locations = cells_row_groups()
      ) |>
      tab_style(
        style = list(
          cell_text(
            weight = "bold",
            color = scotsport_text_colour
          )
        ),
        locations = list(
          cells_stubhead(),
          cells_stub_summary(),
          cells_column_labels(),
          cells_summary(columns = Facilities)
        )
      ) |>
      grand_summary_rows(
        columns = Facilities,
        fns = list(
          label = md("**Total**"),
          id = "grand_summary_total",
          fn = "sum"
        ),
        fmt = ~ fmt_integer(.),
        missing_text = "",
        side = "bottom"
      ) |>
      tab_style(
        style = cell_text(
          color = scotsport_text_colour,
          weight = "bold"
        ),
        locations = list(
          cells_grand_summary(),
          cells_stub_grand_summary()
        )
      ) |>
      cols_align(
        align = "left",
        columns = where(is.factor)
      ) |>
      opt_table_font(
        font = list(
          google_font(name = scotsport_table_font_family),
          "Cochin", "serif"
        )
      )
  }) |>
    bindCache(input$select_council)

  total_towns_with_facilities <- reactive({
    # How many towns have facilities
    towns <- scotland_areas() |>
      # For town calculations make all (Near) entries the same town
      mutate(town = stringr::str_remove(town, " \\(Near\\)")) |>
      distinct(town)

    total_towns <- nrow(towns)

    return(total_towns)
  }) |>
    bindCache(input$select_council)

  selected_town <- reactive({
    sel_town <- input$select_town
    req(sel_town)

    scotland_areas() |>
      filter(town == sel_town)
  }) |>
    bindCache(input$select_town, input$select_council)

  # Scotland choropleth data ---------------------------------------------------
  scotland_choropleth <- reactive({
    facilities_by_council_area <- scotland_areas() |>
      count(la_name)

    combined_council_info <- facilities_by_council_area |>
      # Join the population info
      inner_join(population_council, by = join_by(la_name == area_name)) |>
      mutate(
        people_per_facility = round(population / n, digits = 0),
        .after = population
      ) |>
      # Join geospatial info
      inner_join(df_council_boundaries,
        by = join_by(la_name == local_authority)
      ) |>
      mutate(
        hectares_per_facility = round(hectares / n, digits = 0),
        .after = hectares
      ) |>
      rename(Facilities = n)

    combined_council_info
  }) |>
    bindCache(input$select_council)

  sorted_selected_area_names <- reactive({
    sort(input$select_council)
  }) |>
    bindCache(input$select_council)

  tbl_facility_detail <- reactive({
    selected_council_name <- sorted_selected_area_names()
    df_facility_detail <- scotland_areas() |>
      # We are displaying addresses as if for mailing purposes
      # Remove any "(Near)"
      mutate(town = stringr::str_remove(town, "\\(Near\\)")) |>
      dplyr::select(
        name, address, facility_type, facility_sub_type,
        town, postcode, la_name
      ) |>
      tidyr::unite("full_address",
        c(name, address, town, postcode),
        sep = paste0(", "),
        remove = FALSE,
        na.rm = TRUE
      ) |>
      mutate(facility_type = paste0(facility_type,
        sep = ": ",
        facility_sub_type
      )) |>
      relocate(full_address, .before = name) |>
      relocate(la_name, .after = full_address) |>
      select(-name, -address, -facility_sub_type, -town, -postcode) |>
      arrange(full_address, la_name, facility_type) |>
      # Work out how many of the same type at the same address
      group_by(full_address, la_name, facility_type) |>
      summarise(how_many = n()) |>
      ungroup() |>
      # Linebreak inside HTML table cell
      mutate(full_address = stringr::str_replace_all(
        full_address,
        ",", ",<br />"
      ))

    df_facility_detail |>
      gt(id = "facility_detail") |>
      tab_header(
        title = md("**Sports facilities by address**"),
        subtitle = md(glue::glue("Council area: *{selected_council_name}*"))
      ) |>
      cols_label(
        full_address = md("**Address**"),
        la_name = md("**Council area**"),
        facility_type = md("**Type**"),
        how_many = md("**How many?**")
      ) |>
      cols_align(
        align = "left",
        columns = where(is.factor)
      ) |>
      opt_row_striping() |>
      opt_table_font(
        font = list(
          google_font(name = scotsport_table_font_family),
          "Cochin", "serif"
        )
      ) |>
      opt_interactive(
        use_compact_mode = TRUE,
        use_filters = TRUE,
        use_highlight = TRUE,
        use_page_size_select = TRUE,
        use_search = TRUE
      )
  }) |>
    bindCache(input$select_council)

  # For use in pulldown select
  list_town_names <- reactive({
    town_names <- scotland_areas() |>
      count(town) |>
      drop_na() |> # Need more data cleaning to identify missing towns
      arrange(town)

    l_town_names <- town_names$town
    names(l_town_names) <- paste0(town_names$town, " (", town_names$n, ")")
    l_town_names
  }) |>
    bindCache(scotland_areas())

  # Outputs --------------------------------------------------------------------
  output$home_area_names <- renderText({
    build_area_names(sorted_selected_area_names())
  }) |>
    bindCache(sorted_selected_area_names())

  output$where_area_names <- renderText({
    build_area_names(sorted_selected_area_names())
  }) |>
    bindCache(sorted_selected_area_names())

  output$type_area_names <- renderText({
    build_area_names(sorted_selected_area_names())
  }) |>
    bindCache(sorted_selected_area_names())

  output$total_facilities_areas <- renderText({
    scales::label_comma()(nrow(scotland_areas()))
  }) |>
    bindCache(scotland_areas())

  output$total_people_per_facility <- renderText({
    facilities_per_council <- scotland_areas() |>
      # How many facilities per council
      count(la_name)

    # Join the population info with the facilities
    combined_council_info <- facilities_per_council |>
      inner_join(population_council, by = join_by(la_name == area_name))

    # Population divided by number of facilities
    people_per_facility <- sum(combined_council_info$population) /
      sum(combined_council_info$n)

    scales::label_comma()(people_per_facility)
  }) |>
    bindCache(scotland_areas())

  output$home_total_towns_with_facilities <- renderText({
    scales::label_comma()(total_towns_with_facilities())
  }) |>
    bindCache(total_towns_with_facilities())

  output$where_total_towns_with_facilities <- renderText({
    scales::label_comma()(total_towns_with_facilities())
  }) |>
    bindCache(total_towns_with_facilities())

  # Plots ----------------------------------------------------------------------
  output$plot_facility_type <- renderPlot({
    df_type <- scotland_areas() |>
      group_by(facility_type) |>
      summarise(n = n()) |>
      ungroup() |>
      mutate(facility_type = fct_reorder(facility_type, n, .desc = FALSE))

    plot_facility_type(df_type)
  }) |>
    bindCache(scotland_areas())

  output$map_town <- renderLeaflet({
    # Need to have selected a town in order to see a map
    req(input$select_town)

    town <- selected_town()
    town_types <- unique(town$facility_type)

    town_map <- town |>
      leaflet() |>
      addTiles() |>
      addProviderTiles(
        provider = "Esri.WorldImagery",
        group = "World Imagery (satellite)"
      ) |>
      addMiniMap(
        tiles = providers$Esri.WorldStreetMap,
        position = "bottomleft",
        toggleDisplay = TRUE
      ) |>
      addResetMapButton() |>
      addScaleBar(position = "bottomright")

    for (i in seq_along(town_types)) {
      group_data <- town[town$facility_type == town_types[i], ]

      # Create the popup content
      popup_content <- paste0(
        "<table>",
        "<tr><th>Name:</th><td>", htmltools::htmlEscape(group_data$name),
        "</td></tr>",
        "<tr><th>Address:</th><td>", htmltools::htmlEscape(group_data$address),
        "</td></tr>",
        "<tr><th>Town:</th><td>", htmltools::htmlEscape(group_data$town),
        "</td></tr>",
        "<tr><th>Postcode:</th><td>",
        htmltools::htmlEscape(group_data$postcode), "</td></tr>",
        "<tr><th>Council area:</th><td>",
        htmltools::htmlEscape(group_data$la_name), "</td></tr>",
        "</table>",
        "<hr />",
        "<table>",
        "<tr><th>Type:</th><td>",
        htmltools::htmlEscape(group_data$facility_type), "</td></tr>",
        "<tr><th>Subtype:</th><td>",
        htmltools::htmlEscape(group_data$facility_sub_type), "</td></tr>",
        "<tr><th>Last update:</th><td>",
        get_date(group_data$date_updated), "</td></tr>",
        "</table>"
      )

      # Add the map pins and clusters
      town_map <- addMarkers(
        map = town_map,
        lng = group_data$lng,
        lat = group_data$lat,
        group = town_types[i],
        popup = popup_content,
        label = group_data$name,
        options = markerOptions(),
        clusterOptions =
          markerClusterOptions(
            # when you click a cluster we zoom to its bounds
            zoomToBoundsOnClick = TRUE
          )
      )
    }

    # For controlling showing/hiding layers
    town_map <- addLayersControl(
      map = town_map,
      baseGroups = c(
        "Open Street Map (default)",
        "World Imagery (satellite)"
      ),
      overlayGroups = town_types,
      options = layersControlOptions(
        collapsed = FALSE,

        # if TRUE, the control will automatically maintain the z-order of its
        # various groups as overlays are switched on and off.
        autoZIndex = TRUE
      )
    )

    town_map
  }) |>
    bindCache(selected_town())

  # Scotland Choropleths plots -------------------------------------------------
  # Scotland by number of facilities
  output$plot_scotland_area_facilities <- renderPlot({
    plot_facilties_scotland(scotland_choropleth(),
      feature_cols = "Facilities",
      label_fill = "Facilities",
      plot_title = "Facilities by council area"
    )
  }) |>
    bindCache(scotland_choropleth())

  # Scotland by number of people per facility
  output$plot_scotland_area_people <- renderPlot({
    plot_facilties_scotland(scotland_choropleth(),
      feature_cols = "people_per_facility",
      label_fill = "People per facility",
      plot_title = "People per facility by council area"
    )
  }) |>
    bindCache(scotland_choropleth())

  output$facility_type_table <- render_gt({
    tbl_facility_subtype_count()
  }) |>
    bindCache(tbl_facility_subtype_count())

  output$facility_detail_table <- render_gt({
    tbl_facility_detail()
  }) |>
    bindCache(tbl_facility_detail())

  output$ui_select_town <- renderUI({
    selectizeInput("select_town",
      label = strong("Town (number of facilities):"),
      choices = c(
        "Choose a town   " = "",
        list_town_names()
      ),
      # This helps selectize go wider
      # https://stackoverflow.com/questions/76431862/how-can-i-make-a-shiny-selectinput-overflow-a-bslibnavset-card-pill
      options = list(dropdownParent = "body"),
      width = "100%",
      multiple = FALSE
    )
  }) |>
    bindCache(list_town_names())

  # plot helper functions ------------------------------------------------------
  plot_facility_type <- function(df,
                                 plot_title = "Facilites by type",
                                 plot_fill_bar_chart = "dodgerblue4") {
    df |>
      ggplot(aes(x = facility_type, y = n)) +
      geom_col(fill = plot_fill_bar_chart) +
      scale_y_continuous(labels = scales::label_comma()) +
      coord_flip() +
      labs(
        title = plot_title,
        x = element_blank(),
        y = element_blank(),
        alt = paste0(
          "A map showing the location of the facilities in the ",
          "selected town. You can access text data of towns in the Address ",
          "section"
        )
      ) +
      theme_minimal(base_size = scotsport_default_font_size) +
      theme(text = element_text(
        family = scotsport_default_font_family,
        colour = scotsport_text_colour
      ))
  }

  # Choropleth map of Scotland -------------------------------------------------
  plot_facilties_scotland <- function(df, feature_cols, label_fill,
                                      plot_title) {
    df |>
      ggplot() +
      # Do empty outline
      geom_sf(data = df_council_boundaries, aes(geometry = geometry)) +

      # Coloured selected councils
      geom_sf(aes(
        fill = .data[[feature_cols]],
        geometry = geometry
      )) +
      theme_void(base_size = scotsport_default_font_size) +
      theme(text = element_text(
        family = scotsport_default_font_family,
        colour = scotsport_text_colour
      )) +
      # Lato, -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto,
      # "Helvetica Neue", Arial, sans-serif, "Apple Color Emoji",
      # "Segoe UI Emoji", "Segoe UI Symbol"
      # make the lower numbers lighter
      scale_fill_viridis(direction = -1) +
      labs(
        fill = label_fill,
        title = plot_title
      )
  }

  # Helper functions -----------------------------------------------------------
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
}
