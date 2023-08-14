function(input, output, session) {
  # Create a reactiveValues object
  values <- reactiveValues(clickedID = NULL, clickLat = NULL, clickLng = NULL, clickedLayers = list(), bounds = NULL, zoom=NULL)

  
  # Define path to .rds file
  filePath <- "./data/real_ac_res.rds"
  
  parcel_entries <- shiny::reactive({
    parcel_entries <- base::readRDS('./output/newburg_parcel_data.rds')
    parcel_entries <- parcel_entries[which(!is.na(parcel_entries$county_taxable_value)),]
    return(parcel_entries)
  })
  
  
  #### TOGGLES #####
  output$data_color_dropdown_ui <- shiny::renderUI({
    if(!is.null(parcel_entries())) {
      
      shiny::selectInput(inputId = "color_var",
                         label = "Select variable for color:",
                         choices = names(parcel_entries()[,sapply(parcel_entries(), is.numeric)]),
                         selected = "tax_desparity",
                         selectize = TRUE,
                         multiple=FALSE)
    }
  })
  
  output$data_size_dropdown_ui <- shiny::renderUI({
    if(!is.null(parcel_entries())) {
      shiny::selectInput(inputId = "size_var",
                         label = "Select variable for size:",
                         choices = names(parcel_entries()[,sapply(parcel_entries(), is.numeric)]),
                         selected = "full_market_value",
                         selectize = TRUE,
                         multiple=FALSE)
    }
  })
  
  #### MAP #####
  output$mymap <- renderLeaflet({
    if(!is.null(parcel_entries()) & !is.null(input$color_var) & !is.null(input$size_var)) {


      lon_summary <- summary(parcel_entries()$lon)
      high_lon <- lon_summary['3rd Qu.'] + (lon_summary['3rd Qu.'] - lon_summary['Median'])
      low_lon <- lon_summary['1st Qu.'] - (lon_summary['Median'] - lon_summary['1st Qu.'])
      lat_summary <- summary(parcel_entries()$lat)
      high_lat <- lat_summary['3rd Qu.'] + (lat_summary['3rd Qu.'] - lat_summary['Median'])
      low_lat <- lat_summary['1st Qu.'] - (lat_summary['Median'] - lat_summary['1st Qu.'])
      ogbb <- unname(c(low_lon, low_lat, high_lon, high_lat))
      
      
      df_filtered <- parcel_entries()
      df_filtered$distance <- geosphere::distGeo(df_filtered[, c("lon", "lat")], c(lon_summary['Median'], lat_summary['Median']))
      df_filtered <- df_filtered[which(df_filtered$distance < input$radius*1609.34),]
      
      target_parcels <- df_filtered
      scaled_size <- scales::rescale(target_parcels[[input$size_var]], to = c(3, 10))
      #cat("SCALED SIZE: ", paste0(scaled_size, collapse=", "), "\n")
      
      color_vals <- target_parcels[[input$color_var]]
      unscaled_range <- range(color_vals[!is.na(color_vals)])
      unscaled_range <- base::prettyNum(base::round(seq(unscaled_range[1], unscaled_range[2], length.out = 10)), big.mark=",")
      scaled_colors <- scales::rescale(target_parcels[[input$color_var]], to = c(1, 10))
      color_bins <- colorNumeric("viridis", scaled_colors)
      target_parcels$colors <- color_bins(scaled_colors)

      mx <- m0 %>%
        leaflet::addCircles(
          lng = as.numeric(lon_summary['Median']), lat = as.numeric(lat_summary['Median']), radius = input$radius*1609.34,
          fillColor = "transparent", color = "#FF0000", weight = 2, fillOpacity = 0.5,
          group = "myCircles", layerId = "circle"
        ) %>%
        leaflet::addCircleMarkers(data = target_parcels, 
                            label = ~ lapply(paste0("<b> Lot Code: </b>", lot_code, "<br/>",
                                             "<b>Address: </b>", geo_address, "<br/>", 
                                             "<b>Full Market Value: </b>$", base::prettyNum(full_market_value, big.mark=","), "<br/>",
                                             "<b>County Taxable Value: </b>$", base::prettyNum(county_taxable_value, big.mark=","), "<br/>",
                                             "<b>City Taxable Value: </b>$", base::prettyNum(city_taxable_value, big.mark=","), "<br/>",
                                             "<b>Tax Discrepancy: </b>$", base::prettyNum(tax_desparity, big.mark=","), "<br/>",
                                             "<b>Tax Discrepancy Normalized: </b>", base::prettyNum(base::round(tax_desparity_normalized, 2), big.mark=","), "<br/>"), htmltools::HTML),
                            layerId= ~lot_code,
                            radius = scaled_size,
                            weight = .8, color=~color_bins(scaled_colors), group="Properties", fillOpacity=0.75, labelOptions = (interactive = FALSE)) %>%
        leaflet::fitBounds(ogbb[1], ogbb[2], ogbb[3], ogbb[4]) %>%
        leaflet::addLayersControl(
          baseGroups = c("CartoDB.Positron", "Satellite"),
          overlayGroups = c("Labels", "Properties"), #"Outliers Low Taxes", "Outliers High Taxes"),
          options = leaflet::layersControlOptions(collapsed = FALSE)) %>%
        addLegend(data=target_parcels, colors=viridis::viridis(length(unscaled_range)), labels = unscaled_range, title = input$color_var, opacity = 0.8) %>%
        base::suppressMessages()
    }
  })
  
  output$map_vals_dt_render <- DT::renderDT({
    if(!is.null(parcel_entries())){
      dt <- data.table::data.table(parcel_entries())
      new_col_order <- c(setdiff(colnames(dt), "page_contents"), "page_contents")
      dt <- dt[,..new_col_order]
      
      DT::datatable(dt,
                    class = "compact",
                    style = 'bootstrap5',
                    width="100%",
                    rownames = FALSE,
                    options = list(scrollX=TRUE,
                                   sScrollY = '75vh',
                                   scrollCollapse = TRUE,
                                   searching = TRUE,
                                   paging = TRUE,
                                   pageLength=50,
                                   lengthMenu=c(10,20,50,100),
                                   bInfo=TRUE),
                    extensions = list("Scroller"))
    }
  })
  
  # Handle click event on map
  observeEvent(input$mymap_marker_click, {
    click <- input$mymap_marker_click
    values$clickedID <- click$id  # Get clicked marker's ID
    values$clickLat <- click$lat  # Get clicked marker's latitude
    values$clickLng <- click$lng  # Get clicked marker's longitude

    cat("CLICK EVENT HAS BEEN OBSERVED\n")
  })

  
  #### MAP UPDATES ####
  observe({
    # Check if a marker has been clicked before trying to update the circle
    if (!is.null(values$clickedID) & !is.null(parcel_entries()) & !is.null(input$color_var)) {
      oldBounds <- values$bounds

      # Remove the old circle and add a new one with the updated radius
      leafletProxy("mymap") %>% removeShape(layerId = "circle")
      leafletProxy("mymap") %>% addCircles(
        lng = values$clickLng, lat = values$clickLat, radius = input$radius*1609.34,
        fillColor = "transparent", color = "#FF0000", weight = 2, fillOpacity = 0.5,
        group = "myCircles", layerId = "circle"
      )


      df_filtered <- parcel_entries()
      df_filtered$distance <- geosphere::distGeo(df_filtered[, c("lon", "lat")], c(values$clickLng, values$clickLat))
      df_filtered <- df_filtered[which(df_filtered$distance < input$radius*1609.34),]
      color_bins <- colorNumeric("viridis", df_filtered[[input$color_var]])

      leafletProxy("mymap") %>% clearGroup(group = "Properties")
      
      
      
      target_parcels <- df_filtered#[which(parcel_entries()$tax_desparity == 0 | is.na(parcel_entries()$tax_desparity)),]
      size_min <- min(target_parcels[[input$size_var]])
      size_max <- max(target_parcels[[input$size_var]])
      scaled_size <- scales::rescale(target_parcels[[input$size_var]], to = c(6, 20))
      #cat("SCALED SIZE: ", paste0(scaled_size, collapse=", "), "\n")
      
      
      color_vals <- target_parcels[[input$color_var]]
      unscaled_range <- range(color_vals[!is.na(color_vals)])
      unscaled_range <- base::prettyNum(base::round(seq(unscaled_range[1], unscaled_range[2], length.out = 10)), big.mark=",")
      scaled_colors <- scales::rescale(target_parcels[[input$color_var]], to = c(1, 10))
      color_bins <- colorNumeric("viridis", scaled_colors)
      
      leafletProxy("mymap") %>%
      clearControls() %>%
      clearMarkers() %>%
      leaflet::addCircleMarkers(data = target_parcels,
                                label = ~ lapply(paste0("<b> Lot Code: </b>", lot_code, "<br/>",
                                                        "<b>Address: </b>", geo_address, "<br/>", 
                                                        "<b>Full Market Value: </b>$", base::prettyNum(full_market_value, big.mark=","), "<br/>",
                                                        "<b>County Taxable Value: </b>$", base::prettyNum(county_taxable_value, big.mark=","), "<br/>",
                                                        "<b>City Taxable Value: </b>$", base::prettyNum(city_taxable_value, big.mark=","), "<br/>",
                                                        "<b>Tax Discrepancy: </b>$", base::prettyNum(tax_desparity, big.mark=","), "<br/>",
                                                        "<b>Tax Discrepancy Normalized: </b>", base::prettyNum(base::round(tax_desparity_normalized, 2), big.mark=","), "<br/>"), htmltools::HTML),
                                radius=scaled_size,
                                layerId= ~lot_code,
                                #label = ~paste0(address, ": ", full_market_value, " people"),
                                weight = .8, color=~color_bins(scaled_colors), group="Properties", fillOpacity=0.75, labelOptions = (interactive = TRUE)) %>%
        fitBounds(
          oldBounds$west, oldBounds$south,
          oldBounds$east, oldBounds$north
        ) %>%
        addLegend(data=target_parcels, colors=viridis::viridis(length(unscaled_range)), labels = unscaled_range, title = input$color_var, opacity = 0.8) %>%
        base::suppressMessages()
    }
  })
  
  # React to changes in clickedID
  observe({
    # Do something with values$clickedID...
    print(values$clickedID)
  })
}
