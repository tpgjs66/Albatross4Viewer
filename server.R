
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com


server <- function(input, output, session){
  ## Maximum file input size: 500 MB
  options(shiny.maxRequestSize = 500*1024^2)
  
  ## Load household data
  myhh <- reactive({
    if (is.null(input$household)) {
      hh <- read.csv("data/household-gen.txt")
      return(hh)
    }
    hh <- read.csv(input$household$datapath,
                   header = input$householdheader,
                   sep = input$householdsep,
                   quote = input$householdquote)
    # assign('hh',hh,envir=.GlobalEnv)
    return(hh)
  })
  
  ## Load household coordinates data
  myhhcoords <- reactive({
    if (is.null(input$hhcoords)) {
      hhcoords <- read.csv("data/hh-coords.txt")
      return(hhcoords)
    }
    hhcoords <- read.csv(input$hhcoords$datapath,
                   header = input$hhcoordsheader,
                   sep = input$hhcoordssep,
                   quote = input$hhcoordsquote)
    # assign('hh',hh,envir=.GlobalEnv)
    return(hhcoords)
  })
  
  ## Load schedule data
  mysched <- reactive({
    if (is.null(input$schedule)) {
      sched <- read.csv("data/schedule-gen.txt")
      return(sched)
    }
    sched <- read.csv(input$schedule$datapath,
                      header = input$scheduleheader,
                      sep = input$schedulesep,
                      quote = input$schedulequote)
    
    ## Time conversion
    leaveTime <- strsplit(as.character(sched$LeaveTime),":|\\+")
    leaveTime <- data.frame(matrix(unlist(leaveTime), nrow=length(leaveTime), byrow=T))
    leaveTime$X1 <- as.integer(as.character(leaveTime$X1))
    leaveTime$X2 <- as.integer(as.character(leaveTime$X2))
    leaveTime$X3 <- as.integer(as.character(leaveTime$X3))
    leaveTime$min <- leaveTime$X1*60 + leaveTime$X2 + leaveTime$X3*24*60
    sched$LeaveTime <- leaveTime$min
    
    beginTime <- strsplit(as.character(sched$BeginTime),":|\\+")
    beginTime <- data.frame(matrix(unlist(beginTime), nrow=length(beginTime), byrow=T))
    beginTime$X1 <- as.integer(as.character(beginTime$X1))
    beginTime$X2 <- as.integer(as.character(beginTime$X2))
    beginTime$X3 <- as.integer(as.character(beginTime$X3))
    beginTime$min <- beginTime$X1*60 + beginTime$X2 + beginTime$X3*24*60
    sched$BeginTime <- beginTime$min
    
    endTime <- strsplit(as.character(sched$EndTime),":|\\+")
    endTime <- data.frame(matrix(unlist(endTime), nrow=length(endTime), byrow=T))
    endTime$X1 <- as.integer(as.character(endTime$X1))
    endTime$X2 <- as.integer(as.character(endTime$X2))
    endTime$X3 <- as.integer(as.character(endTime$X3))
    endTime$min <- endTime$X1*60 + endTime$X2 + endTime$X3*24*60
    sched$EndTime <- endTime$min
    
    # Give unique ID
    sched$SchedID<-do.call(paste, c(sched[c("HHID", "MemID","EpisodeID")], sep = "-"))
    
    # assign('sched',sched,envir=.GlobalEnv)
    return(sched)
  })
  
  ## Load schedule data
  myschedcoords <- reactive({
    if (is.null(input$scheduleCoords)) {
      scheduleCoords <- read.csv("data/sched-coords.txt")
      return(scheduleCoords)
    }
    scheduleCoords <- read.csv(input$scheduleCoords$datapath,
                      header = input$scheduleCoordsheader,
                      sep = input$scheduleCoordssep,
                      quote = input$scheduleCoordsquote)
  })
  
  timeconverter <- function(sched) {
    ## Time conversion
    leaveTime <- strsplit(as.character(sched$LeaveTime),":|\\+")
    leaveTime <- data.frame(matrix(unlist(leaveTime), nrow=length(leaveTime), byrow=T))
    leaveTime$X1 <- as.integer(as.character(leaveTime$X1))
    leaveTime$X2 <- as.integer(as.character(leaveTime$X2))
    leaveTime$X3 <- as.integer(as.character(leaveTime$X3))
    leaveTime$min <- leaveTime$X1*60 + leaveTime$X2 + leaveTime$X3*24*60
    sched$LeaveTime <- leaveTime$min
    
    beginTime <- strsplit(as.character(sched$BeginTime),":|\\+")
    beginTime <- data.frame(matrix(unlist(beginTime), nrow=length(beginTime), byrow=T))
    beginTime$X1 <- as.integer(as.character(beginTime$X1))
    beginTime$X2 <- as.integer(as.character(beginTime$X2))
    beginTime$X3 <- as.integer(as.character(beginTime$X3))
    beginTime$min <- beginTime$X1*60 + beginTime$X2 + beginTime$X3*24*60
    sched$BeginTime <- beginTime$min
    
    endTime <- strsplit(as.character(sched$EndTime),":|\\+")
    endTime <- data.frame(matrix(unlist(endTime), nrow=length(endTime), byrow=T))
    endTime$X1 <- as.integer(as.character(endTime$X1))
    endTime$X2 <- as.integer(as.character(endTime$X2))
    endTime$X3 <- as.integer(as.character(endTime$X3))
    endTime$min <- endTime$X1*60 + endTime$X2 + endTime$X3*24*60
    sched$EndTime <- endTime$min
    
    # Give unique ID
    sched$SchedID<-do.call(paste, c(sched[c("HHID", "MemID","EpisodeID")], sep = "-"))
    
    return(sched)
  }
  
  ## Load 4 digits PPCS data
  myppcs <- reactive({
    shpdf <- input$shpFilePc4
    if (is.null(shpdf)) {
      ppcs <- rgdal::readOGR("data/ppcs_single.shp",layer = "ppcs_single", GDAL1_integer64_policy = TRUE)
      return(ppcs)
    }
     
    tempdirname <- dirname(shpdf$datapath[1])
    for(i in 1:nrow(shpdf)){
      file.rename(shpdf$datapath[i], paste0(tempdirname, "/", shpdf$name[i]))
    }
    ppcs <- rgdal::readOGR(paste(tempdirname, shpdf$name[grep(pattern = "*.shp$", shpdf$name)], sep = "/"),
                    layer = "ppcs_single", GDAL1_integer64_policy = TRUE)
    # ppcs <- spTransform(ppcs, CRS("+init=EPSG:4326"))
    # # Simplifying shape file
    # ppcs <- tmaptools::simplify_shape(ppcs)
    
    # assign('ppcs',ppcs,envir=.GlobalEnv)
    return(ppcs)
  })
  
  # ## Load 6 digits PPCS land-use data and make it global
  # pc6sf <<-  sf::read_sf("data/PC6_BBG2012.shp",
  #                        layer = "PC6_BBG2012")
  # pc6sf$PC4 <<- substr(pc6sf$Postcode,1,4)
  
  ## Output household data
  output$previewHousehold <- renderTable({
    
    req(input$household)
    if (input$householddisp == "head") {
      hh <- head(myhh())
    } else {
      hh <- myhh()
    }
    return(hh)
  })
  
  output$previewHouseholdTotal <- renderText({
    req(input$household)
    hh <- myhh()
    hhnum <- length(unique(hh$HHID))
    return((paste(hhnum, "households have been found.")))
  })
  
  output$householdTotal <- DT::renderDataTable({
    # Create input file error message
    if (is.null(input$household)) {
      showNotification("Please provide input files! (household)",
                       type = "error",
                       duration = 5)
      hh <- read.csv("data/household-gen.txt")
      return(hh)
    }
    req(input$household)
    hh <- myhh()
    hh <- DT::datatable(hh, filter = "top",options = list(scrollX = T))
    return(hh)
  })
  
  ## Output schedule data
  output$previewSchedule <- renderTable({
    req(input$schedule)
    if (input$scheduledisp == "head") {
      sched <- head(mysched())
    } else {
      sched <- read.csv(input$schedule$datapath,
                        header = input$scheduleheader,
                        sep = input$schedulesep,
                        quote = input$schedulequote)
    }
    return(sched)
  })
  
  output$previewScheduleTotal <- renderText({
    req(input$schedule)
    sched <- mysched()
    schednum <- length(sched$EpisodeID)
    hhnum <- length(unique(sched$HHID))
    return(paste(hhnum, "households have been found with ",
                 schednum ,"activitiy episodes."))
  })
  
  output$scheduleTotal <- DT::renderDataTable({
    # Create input file error message
    if (is.null(input$schedule)) {
      showNotification("Please provide input files! (schedule)",
                       type = "error",
                       duration = 5)
      sched <- read.csv("data/schedule-gen.txt")
      return(sched)
    }
    req(input$schedule)
    sched <- mysched()
    sched <- DT::datatable(sched, filter = "top",options = list(scrollX = TRUE))
    return(sched)
  })
  
  ##############################################################################
  ###########                  List schedule OD               ##################
  ##############################################################################
  
  filterschedod <- eventReactive(input$submitscheduleod,{
    # Load schedule file
    sched <- mysched()
    # Filter schedule by input selection
    sched <- sched[sched$ActivityType %in% input$listschedact,]
    sched <- sched[sched$Mode %in% input$listschedmode,]
    sched <- sched[which((sched$BeginTime > input$listschedtime[1] & sched$BeginTime < input$listschedtime[2])|
                           (sched$EndTime > input$listschedtime[1] & sched$EndTime < input$listschedtime[2])),]
    # Convert OD matrix to pairwise column
    flows <- as.data.frame(table(sched$OrigLoc,sched$DestLoc))
    colnames(flows) <- c("origin","destination","flow")
    
    flows$origin <- as.integer(levels(flows$origin))[flows$origin]
    flows$destination <- as.integer(levels(flows$destination))[flows$destination]
    flows$flow <- as.integer(flows$flow)
    
    flows
  })
  
  output$scheduleod <- DT::renderDataTable({
    
    # Create input file error message
    if (is.null(input$schedule)) {
      showNotification("Please provide input files! (schedule)",
                       type = "error",
                       duration = 5)
      # Load schedule file
      sched <- read.csv("data/schedule-gen.txt")
      # Filter schedule by input selection
      sched <- sched[sched$ActivityType %in% input$listschedact,]
      sched <- sched[sched$Mode %in% input$listschedmode,]
      # sched <- sched[which((sched$BeginTime > input$listschedtime[1] & sched$BeginTime < input$listschedtime[2])|
      #                        (sched$EndTime > input$listschedtime[1] & sched$EndTime < input$listschedtime[2])),]
      # Convert OD matrix to pairwise column
      flows <- as.data.frame(table(sched$OrigLoc,sched$DestLoc))
      colnames(flows) <- c("origin","destination","flow")
      
      flows$origin <- as.integer(levels(flows$origin))[flows$origin]
      flows$destination <- as.integer(levels(flows$destination))[flows$destination]
      flows$flow <- as.integer(flows$flow)
      
      flowsDT <- DT::datatable(flows, filter = "top")
      return(flowsDT)
    }
    
    # Filter O-D pair by input selection
    flows <- filterschedod()
    
    flowsDT <- DT::datatable(flows, filter = "top")
    
    output$downloadDTsched <- downloadHandler(
      filename = function() {
        paste("DTschedule", ".csv", sep = "")
      },
      content = function(file) {
        write.csv(flows, file, row.names = FALSE)
      }
    )
    
    return(flowsDT)
  })
  
  ##############################################################################
  ###########               Leaflet household map             ##################
  ##############################################################################
  filteredhh <- eventReactive(input$submitmaphh,{
    hh <- read.csv("data/hh-coords.txt")
  })
  
  output$maphh <- renderLeaflet({
    
    # Create input file error message
    if (is.null(input$household) | is.null(input$shpFilePc4)) {
      showNotification("Please provide input files! (household / shp file)",
                       type = "error",
                       duration = 5)
      # return()
    }
    
    # Load household data
    hh <<- filteredhh()
    
    # Load ppcs data
    ppcs <- myppcs()
    
    # Sample ppcs only where households are located
    ppcsSample <- subset(ppcs, ppcs@data$PC4 %in% hh$Home)
    
    # Add number of household within 4 ppcs
    ppcsSample$Household[match(names(table(hh$Home)),ppcsSample$PC4)]<- table(hh$Home)
    
    # Add household density by 4 ppc
    ppcsSample$HHDensity <- round(ppcsSample$Household/
                                    (ppcsSample$Shape_Area / 10^6),
                                  digits = 2)
    
    # ########### Loop for getting household location coordinate #################
    # ### The household file above didn’t have coordinates just 4 ppc area codes.
    # ### Here is a lookup that provides those and randomly disribute households.
    # 
    # coords <- c()
    # hhid <- c()
    # for (i in ppcsSample@data$PC4) {
    #   # total household within PC4
    #   n <- sum(ppcsSample$Household[ppcsSample$PC4 == i], na.rm = TRUE)
    #   # get household id within PC4
    #   hhid <- append(hhid,hh$HHID[hh$Home == i])
    #   
    #   polygon <- ppcsSample[ppcsSample@data$PC4 == i,]@polygons
    #   
    #   chosenPolygon <- 1
    #   for (j in 1:length(polygon)) {
    #     if (j > 1) {
    #       if (polygon[[j]]@area > polygon[[j-1]]@area){
    #         chosenPolygon <- j
    #       }
    #     }
    #   }
    #   
    #   if (class(polygon) == "list" & length(polygon) > 1) { ##For multi-polygons
    #     polygon <- polygon[[chosenPolygon]]
    #     if (length(polygon@Polygons) == 1) {
    #       
    #     } else {
    #       chosen <- (polygon@plotOrder)[1]
    #       polygon <- polygon@Polygons[[chosen]]
    #     }
    #   } else {
    #     polygon <- polygon[[chosenPolygon]]
    #     if (length(polygon@Polygons) == 1) {
    #       
    #     } else {
    #       chosen <- (polygon@plotOrder)[1]
    #       polygon <- polygon@Polygons[[chosen]]
    #     }
    #   }
    #   coords <- rbind(coords,spsample(polygon, n = n, type = "random")@coords)
    # }
    # hh$Lng <- coords[,1]
    # hh$Lat <- coords[,2]
    
    ############################################################################
    
    # ############# Loop for getting household PC6 coordinate ####################
    # ### The household file above didn’t have coordinates just 4 ppc area codes.
    # ### Here is a lookup that provides those and randomly disribute households.
    # 
    # 
    # coords <- c()
    # hhpc6 <- c()
    # 
    # withProgress(message = paste("Sampling location for ", nrow(hh),
    #                              "households..."), value = 0, {
    # 
    #                                for (i in 1:nrow(hh)) {
    #                                  # Increment the progress bar, and update the detail text.
    #                                  incProgress(1/nrow(hh),
    #                                              detail = paste("Sampled a location for",i,"-th household out of",
    #                                                             nrow(hh)))
    # 
    #                                  # Check if PC6 covers all PC4 polygons
    #                                  try(if (!all(is.element(hh$Home,unique(pc6sf$PC4))))
    #                                    stop("PC6 geometry does not contain all polygons in PC4!")
    #                                  )
    # 
    #                                  # filter pc6 where household belongs and residential area
    #                                  pc6 <- pc6sf[which(pc6sf$PC4 == hh$Home[i] & pc6sf$BG2012_maj == 20),]
    # 
    #                                  # Convert sf to sp
    #                                  pc6sp <- as(pc6,"Spatial")
    # 
    #                                  # get random coordinates within filtered pc6
    #                                  coordsTmp <- spsample(x = pc6sp, n = 1, type = "random", iter = 30,
    #                                                        prob = pc6sp$Aantal_adr, replace = TRUE)
    # 
    #                                  # Assign PC6 Home location and coordinates
    #                                  hhpc6 <- append(hhpc6,over(coordsTmp,pc6sp)$PC6)
    #                                  coords <- rbind(coords,coordsTmp@coords)
    #                                }
    # 
    #                              })
    # 
    # hh$Lng <<- coords[,1]
    # hh$Lat <<- coords[,2]
    # hh$PC6 <<- hhpc6
    ############################################################################
    
    ## Label setting
    labels <- sprintf(
      "<strong>PPC: %s</strong><br/>
      # of Households: %g <br/>
      Area: %g km<sup>2</sup><br/>
      Household density: % g households/km<sup>2</sup>",
      ppcsSample$PC4,ppcsSample$Household,
      ppcsSample$Shape_Area / 10^6,
      ppcsSample$HHDensity
    ) %>% lapply(htmltools::HTML)
    
    min <- min(ppcsSample$HHDensity,na.rm=T)
    max <- max(ppcsSample$HHDensity,na.rm=T)
    
    bins <- round(quantile(ppcsSample$HHDensity,probs = seq(0,1,length=9),
                           na.rm = TRUE),
                  digits = 2)
    
    pal <- colorBin("YlOrRd", domain = ppcsSample$HHDensity, bins = bins)
    
    ############################################################################
    ############################################################################
    
    # Write leaflet()
    leaflet() %>%
      setView(lng=5.00 , lat =52.00, zoom=8) %>%
      
      # Base groups
      addTiles(group = "OSM (default)",
               options = providerTileOptions(noWrap = TRUE)) %>%
      addProviderTiles(group = "Toner Lite", providers$Stamen.TonerLite) %>%
      
      # Overlay groups
      addPolygons(data = ppcsSample,
                  group = "4-digit postcode area",
                  color = "#444444",
                  weight = 1,
                  smoothFactor = 0.5,
                  opacity = 1.0,
                  fill = TRUE,
                  fillColor = "#A9F5BC",
                  fillOpacity = 0.5,
                  label = labels,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"),
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 3,
                                                      bringToFront = TRUE)) %>%
      addCircles(data = hh,
                 group = "Household location",
                 lng = ~Lng,
                 lat = ~Lat,
                 color = "blue",
                 radius = 50,
                 layerId = ~HHID,
                 # radius = 3,
                 # icon = icons,
                 label = ~(paste("HHID: ",as.character(HHID))),
                 highlightOptions = highlightOptions(color = "white",
                                                     weight = 3,
                                                     bringToFront = TRUE)) %>%
      
      addPolygons(data = ppcsSample,
                  group = "Household choropleth",
                  color = "#444444",
                  weight = 1,
                  smoothFactor = 0.5,
                  opacity = 1.0,
                  fill = TRUE,
                  fillColor = pal(ppcsSample$HHDensity),
                  fillOpacity = 0.5,
                  label = labels,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"),
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 3,
                                                      bringToFront = TRUE)) %>%
      # Add legend for Household choropleth layer
      addLegend(data = ppcsSample,
                group = "Household choropleth",
                pal = pal,
                values = ppcsSample$HHDensity,
                opacity = 0.7,
                title = "Household Density",
                position = "bottomright") %>%
      
      # Layer Control
      addLayersControl(
        baseGroups = c("OSM (default)", "Toner Lite"),
        overlayGroups = c("4-digit postcode area",
                          "Household choropleth",
                          "Household location"),
        options = layersControlOptions(collapsed = FALSE)) %>%
      
      # Hide some layers at first to reduce loading time
      hideGroup("4-digit postcode area") %>%
      hideGroup("Household choropleth") %>%
      # hideGroup("Household location") %>%
      
      addMiniMap(toggleDisplay = TRUE, position = "bottomleft")
  })
  
  observeEvent(input$maphh_shape_click, {
    
    click <- input$maphh_shape_click
    
    # Ignore other shapes than circle shape
    # Note: No layerIDs are assigned to polygons for postcode
    if (is.null(click) | is.null(click$id)){
      return()
    }
    
    output$clickedhhId<-renderText({
      text <- paste("You've selected Household", click$id)
      text
    })
    
    output$clickedhhTable <- DT::renderDataTable({
      
      table <- (subset(hh,HHID == click$id))
      table <- DT::datatable(data = (t(table)), colnames = "",
                             options = list(paging = F, searching = F, pagelength = 25))
      table
    })
    
    ## Add pulse marker to the clicked household
    proxy <- leafletProxy("maphh")
    if (click$id != "Selected") {
      proxy %>% setView(lng = click$lng, lat = click$lat, input$maphh_zoom, zoom = 13)
      proxy %>% addPulseMarkers(lng = click$lng,
                                lat = click$lat,
                                layerId = "Selected",
                                icon = makePulseIcon(heartbeat = 1))
    } else {
      # Remove previously clicked pulse marker
      proxy %>% removeMarker(layerId="Selected")
    }
    
  })
  
  ##############################################################################
  ############           Leaflet Activity location             #################
  ##############################################################################
  
  filterschedmapactloc <- eventReactive(input$submitmapactloc,{
    # Load schedule file
    # sched <- mysched()
    sched <- myschedcoords()
    sched <- timeconverter(sched)
    # Filter schedule by input selection
    sched <- sched[sched$ActivityType %in% input$mapactlocact,]
    sched <- sched[sched$Mode %in% input$mapactlocmode,]
    sched <- sched[sched$Charging %in% input$mapactloccharging,]
    sched <- sched[which((sched$BeginTime > input$mapactloctime[1] & sched$BeginTime < input$mapactloctime[2])|
                           (sched$EndTime > input$mapactloctime[1] & sched$EndTime < input$mapactloctime[2])),]
    sched
  })
  
  output$mapactloc <- renderLeaflet({
    
    # Create input file error message
    if (is.null(input$schedule) | is.null(input$shpFilePc4)) {
      showNotification("Please provide input files! (schedule / shp file)",
                       type = "error",
                       duration = 5)
      # return()
    }
    
    sched <<- filterschedmapactloc()
    
    # Print total number of activities
    if (nrow(sched) == 0) {
      showNotification("No activity was found. Please choose items as per panels above.",
                       type = "message",
                       duration = 5)
      return()
    }
    
    # Load shape file
    ppcs <- myppcs()
    
    # Sample ppcs only where activities are occured.
    ppcsSample <- subset(ppcs, ppcs@data$PC4 %in% sched$DestLoc)
    
    # Add number of activities within 4 ppcs
    ppcsSample$NumActs <- table(sched$DestLoc)[match(ppcsSample$PC4,names(table(sched$DestLoc)))]
    
    # Add household density by 4 ppc
    ppcsSample$ActDensity <- round(ppcsSample$NumActs/
                                     (ppcsSample$Shape_Area / 10^6),
                                   digits = 2)
    # Add postcode label
    labelsPpcs <- sprintf(
      "<strong>PPC: %s</strong><br/>",
      ppcsSample$PC4
      
    ) %>% lapply(htmltools::HTML)
    
    showNotification(paste(nrow(sched),"activities were found in",length(unique(sched$HHID)),
                           "households"),
                     type = "message",
                     duration = 5)
    
    #   ##########################################################################
    #   ############# Loop for getting activity PC6 coordinate ###################
    #   ### The schedule file above didn’t have coordinates just 4 ppc area codes.
    #   ### Here is a lookup that provides those and randomly disribute act locs.
    #   ##########################################################################
    #   
    #   ## Land use info
    #   landuse <- function(actType) {
    #     switch(as.character(actType),
    #            Home = c(20),
    #            Work = c(22,24),
    #            Business = c(22,24),
    #            BringGet = c(20,21,22,23,24),
    #            Groceries = c(21),
    #            NonGroc = c(21),
    #            Services = c(22,24),
    #            Leisure = c(23,40,41,42,43,44,75),
    #            Social = c(23,40,41,42,43,44,75),
    #            Touring = c(20,21,22,23,24,40,41,42,43,44,45,75),
    #            Other = c(20,21,22,23,24,40,41,42,43,44,45,75))
    #   }
    #   
    #   coords <- c()
    #   schedpc6 <- c()
    # 
    #   withProgress(message = paste("Sampling location for ", nrow(sched),
    #                                "activities..."), value = 0, {
    #   j <- 0
    #   for (i in sched$SchedID) {
    #     j = j + 1
    #     # Increment the progress bar, and update the detail text.
    #     incProgress(1/nrow(sched),
    #                 detail = paste("Sampled a location for",j,"activity out of",
    #                                nrow(sched),"activities"))
    # 
    #     destLoc <- sched[which(sched$SchedID == i),]$DestLoc
    #     actType <- sched[which(sched$SchedID == i),]$ActivityType
    #     
    #     # get pc6 where activity occurs in corresponding land use PC6
    #     pc6 <- pc6sf[which(pc6sf$PC4 == destLoc & pc6sf$BG2012_maj %in% landuse(actType)),]
    # 
    #     # Convert sf to sp
    #     pc6sp <- as(pc6,"Spatial")
    #     
    #     # Randomly sample if there is no appropriate land use in PC4
    #     if (nrow(pc6) == 0) {
    #       pc6 <- pc6sf[which(pc6sf$PC4 == destLoc),]
    #       
    #       # Convert sf to sp
    #       pc6sp <- as(pc6,"Spatial")
    #       
    #       # get random coordinates within filtered pc6
    #       coordsTmp <-  spsample(x = pc6sp, n = 1, type = "random", iter = 30,
    #                              prob = pc6sp$Aantal_adr, replace = TRUE)
    #     } else {
    #       # get random coordinates within filtered pc6
    #       coordsTmp <- spsample(x = pc6sp, n = 1, type = "random", iter = 30,
    #                             prob = pc6sp$Aantal_adr, replace = TRUE)
    #     }
    #     schedpc6 <- append(schedpc6,over(coordsTmp,pc6sp)$PC6)
    #     coords <- rbind(coords,coordsTmp@coords)
    #   }
    #   })
    #                                  
    #   sched$Lng <<- coords[,1]
    #   sched$Lat <<- coords[,2]
    #   sched$PC6 <<- schedpc6
    
    ############################################################################
    ############################################################################
    sched$Lng <<- sched$DestLng
    sched$Lat <<- sched$DestLat
    sched$PC6 <<- sched$DestPC6
    
    leaflet() %>%
      setView(lng=5.00 , lat =52.00, zoom=8) %>%
      
      # Base groups
      addTiles(group = "OSM (default)",
               options = providerTileOptions(noWrap = TRUE)) %>%
      addProviderTiles(group = "CartoDB Dark",
                       provider = providers$CartoDB.DarkMatterNoLabels) %>%
      # addProviderTiles(group = "NatGeo", providers$Esri.NatGeoWorldMap) %>%
      # addProviderTiles(group = "Toner Lite", providers$Stamen.TonerLite) %>%
      
      # Overlay groups
      addPolygons(data = ppcsSample,
                  group = "4-digit postcode area",
                  color = "#444444",
                  weight = 1,
                  smoothFactor = 0.5,
                  opacity = 1.0,
                  fill = TRUE,
                  fillColor = "#A9F5BC",
                  fillOpacity = 0.5,
                  label = labelsPpcs,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"),
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 3,
                                                      bringToFront = TRUE)) %>%
      
      addCircles(data = sched,
                 group = "Activity location",
                 lng = ~Lng,
                 lat = ~Lat,
                 radius = 50,
                 layerId = ~SchedID,
                 # radius = 3,
                 # icon = icons,
                 label = ~(paste("SchedID: ",as.character(SchedID))),
                 highlightOptions = highlightOptions(color = "white",
                                                     weight = 3,
                                                     bringToFront = TRUE)) %>%
      
      # Layer control
      addLayersControl(
        baseGroups = c("OSM (default)", "CartoDB Dark"),
        overlayGroups = c("4-digit postcode area", "Activity location"),
        options = layersControlOptions(collapsed = FALSE)) %>%
      
      # Hide some layers at first to reduce loading time
      hideGroup("4-digit postcode area") %>%
      
      # Add minimap
      addMiniMap(toggleDisplay = TRUE, position = "bottomleft")
    
  })
  
  observeEvent(input$mapactloc_shape_click, {
    
    click <- input$mapactloc_shape_click
    
    # Ignore other shapes than circle shape
    # Note: No layerIDs are assigned to polygons for postcode
    if (is.null(click) | is.null(click$id)){
      return()
    }
    
    output$clickedactlocId<-renderText({
      text <- paste("You've selected Activity", click$id)
      text
    })
    
    output$clickedactlocTable <- DT::renderDataTable({
      
      table <- (subset(sched,SchedID == input$mapactloc_shape_click$id))
      table <- DT::datatable(data = (t(table)), colnames = "",
                             options = list(paging = F, searching = F, pagelength = 25))
      table
    })
    
    ## Add pulse marker to the clicked activity
    proxy <- leafletProxy("mapactloc")
    if (click$id != "Selected") {
      proxy %>% setView(lng = click$lng, lat = click$lat, input$mapactloc_zoom, zoom = 13)
      proxy %>% addPulseMarkers(lng = click$lng,
                                lat = click$lat,
                                layerId = "Selected",
                                icon = makePulseIcon(heartbeat = 1))
    } else {
      # Remove previously clicked pulse marker
      proxy %>% removeMarker(layerId="Selected")
    }
  })
  
  ##############################################################################
  ###########                  Leaflet O-D flow               ##################
  ##############################################################################
  
  filterschedmapodflow <- eventReactive(input$submitmapodflow,{
    # Load schedule file
    sched <- mysched()
    # Filter schedule by input selection
    sched <- sched[sched$ActivityType %in% input$mapodflowact,]
    sched <- sched[sched$Charging %in% input$mapodflowcharging,]
    sched <- sched[sched$Mode %in% input$mapodflowmode,]
    # sched <- sched[which((sched$BeginTime > input$mapodflowtime[1] & sched$BeginTime < input$mapodflowtime[2])|
    #                        (sched$EndTime > input$mapodflowtime[1] & sched$EndTime < input$mapodflowtime[2])),]
    sched
  })
  
  filterodpairmapodflow <- eventReactive(input$submitmapodflow,{
    
    # Filter schedule by input selection
    sched <- filterschedmapodflow()
    
    # Convert OD matrix to pairwise column
    flows <- as.data.frame(table(sched$OrigLoc,sched$DestLoc))
    flows <- flows[with(flows, order(-Freq)), ]
    
    # Print message for empty data
    if (nrow(flows) == 0) {
      showNotification("No O-D pair was found. Please choose items as per panels above.",
                       type = "message",
                       duration = 5)
      return()
    }
    
    colnames(flows) <- c("origin","destination","flow")
    
    # Remove OD pairs without flow to reduce computation time
    flows <- flows[which(flows$flow > 0),]
    
    # Sample OD pairs with largest n.trips
    if (input$mapodflowshow){
      # Print message for wrong input number of O-D pairs
      if (nrow(flows) < input$mapodflownum) {
        showNotification("The number of O-D pairs you provide exceeds the total number of O-D pairs in data!",
                         type = "error",
                         duration = 5)
        return()
      }
      flows <- flows[1:input$mapodflownum,]
    }
    flows
  })
  
  output$mapodflow <- renderLeaflet({
    
    # Create input file error message
    if (is.null(input$schedule) | is.null(input$shpFilePc4)) {
      showNotification("Please provide input files! (schedule / shp file)",
                       type = "error",
                       duration = 5)
      # return()
    }
    
    # Load shape file
    ppcs <- myppcs()
    
    # Filter schedule by input selection
    sched <- filterschedmapodflow()
    
    # Filter O-D pair by input selection
    flows <- filterodpairmapodflow()
    
    # Print total number of O-D pairs
    showNotification(paste(sum(flows$flow), "trips have been found among",nrow(flows),"O-D pairs."),
                     type = "message",
                     duration = 5)
    
    # Add progress status
    withProgress(message = paste("Converting", nrow(flows),
                                 "O-D pairs to polyline..."), value = 0, {
                                   setProgress(value = NULL, message = NULL, detail = NULL,
                                               session = getDefaultReactiveDomain())
                                   ## Get OD polylines
                                   lines <-stplanr::od2line(flow = flows, zones = ppcs)
                                   # proj4string(lines)<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
                                 })
    # Add id for lines
    lines$id <- rownames(lines@data)
    
    ## Put labels on lines
    labels <- sprintf(
      "O.PPC: %s<br/>
      D.PPC: %s <br/>
      N.trips: %s",
      
      lines@data$origin,
      lines@data$destination,
      lines@data$flow
    ) %>% lapply(htmltools::HTML)
    
    # breaks for legend
    histinfo<-hist(flows$flow[which(flows$flow != 1)],plot = FALSE)
    bins <- histinfo$breaks
    pal <- colorBin("YlOrRd", domain = flows$flow, bins = bins)
    
    leaflet() %>%
      setView(lng=5.00 , lat =52.00, zoom=8) %>%
      
      # Base groups
      addProviderTiles(group = "CartoDB Dark (default)",
                       provider = providers$CartoDB.DarkMatterNoLabels) %>%
      addProviderTiles(group = "OSM BlackAndWhite",
                       provider = providers$OpenStreetMap.BlackAndWhite) %>%
      addTiles(group = "OSM",
               options = providerTileOptions(noWrap = F)) %>%
      # Overlay groups
      addPolylines(data=lines,
                   group = "O-D flows",
                   weight = 0.5*(flows$flow),
                   color = pal(flows$flow),
                   label = labels,
                   layerId = ~id,
                   opacity = 0.3,
                   highlightOptions = highlightOptions(color = "blue",
                                                       weight = 3,
                                                       bringToFront = TRUE)) %>%
      
      # Add legend
      addLegend(data = flows,
                group = "O-D flows",
                pal = pal,
                values = flows$flow,
                opacity = 0.7,
                title = "Number of trips",
                position = "bottomright") %>%
      
      # Layer control
      addLayersControl(
        baseGroups = c("CartoDB Dark (default)","OSM BlackAndWhite","OSM"),
        overlayGroups = c("O-D flows"),
        options = layersControlOptions(collapsed = TRUE))
    
    # hideGroup("O-D flows")
  })
  
  observeEvent(input$mapodflow_shape_click,{
    p <- input$mapodflow_shape_click
    print(p)
  })
  
  ##############################################################################
  ###########              Leaflet Route-Individual PC4       ##################
  ##############################################################################
  
  filterschedmaprouteind <- eventReactive(input$submitmaprouteind,{
    
    # Load schedule file
    sched <- mysched()
    
    # Sample only outgoing trips
    schedOnlyOut <- sched[which(sched$OrigLoc!=sched$DestLoc),]
    
    # Filter schedule by Activity type
    schedOnlyOut <- schedOnlyOut[schedOnlyOut$ActivityType %in% input$maprouteindact,]
    # Filter schedule by Transport mode
    if (!input$maprouteindmode == "All"){
      schedOnlyOut <- schedOnlyOut[schedOnlyOut$Mode == input$maprouteindmode,]
    }
    schedOnlyOut <- schedOnlyOut[schedOnlyOut$Charging %in% input$maprouteindcharging,]
    # # Filter schedule by Time of day
    # sched <- sched[which((sched$BeginTime > input$maprouteindtime[1] & sched$BeginTime < input$maprouteindtime[2])|
    #                        (sched$EndTime > input$maprouteindtime[1] & sched$EndTime < input$maprouteindtime[2])),]
    schedOnlyOut
  })
  
  filterodpairmaprouteind <- eventReactive(input$submitmaprouteind,{
    
    # Filter schedule by input selection
    schedOnlyOut <- filterschedmaprouteind()
    
    # Convert OD matrix to pairwise column
    myflows <- as.data.frame(table(schedOnlyOut$OrigLoc,schedOnlyOut$DestLoc))
    myflows <- myflows[with(myflows, order(-Freq)), ]
    
    # Print message for empty data  
    if (nrow(myflows) == 0) {
      showNotification("No O-D pair was found. Please choose items as per panels above.",
                       type = "message",
                       duration = 5)
      return()
    }
    
    colnames(myflows) <- c("origin","destination","flow")
    
    # Remove OD pairs without flow
    myflows <- myflows[which(myflows$flow > 0),]
    
    # Sample OD pairs with largest n.trips
    if (input$maprouteindshow){
      # Print message for wrong input number of O-D pairs  
      if (nrow(myflows) < input$maprouteindnum) {
        showNotification("The number of O-D pairs you provide exceeds the total number of O-D pairs in data!",
                         type = "error",
                         duration = 5)
        return()
      }
      myflows <- myflows[1:input$maprouteindnum,]
    }
    myflows
    
  })
  
  output$maprouteind <- renderLeaflet({
    
    # Create input file error message
    if (is.null(input$schedule) | is.null(input$shpFilePc4)) {
      showNotification("Please provide input files! (schedule/shp file)",
                       type = "error",
                       duration = 5)
      # return()
    }
    
    # Load schedule data
    sched <- mysched()
    
    # Filter schedule by input selection with more than 0 trip
    schedOnlyOut <- filterschedmaprouteind()
    
    # Filter O-D pair by input selection with more than 0 trip
    myflows <- filterodpairmaprouteind()
    
    # Load shape file
    ppcs <- myppcs()
    
    # Add progress status to UI
    withProgress(message = paste("Converting", nrow(myflows),
                                 "O-D pairs to polyline..."), value = 0, {
                                   setProgress(value = NULL, message = NULL, detail = NULL,
                                               session = getDefaultReactiveDomain())
                                   ## Get OD polylines
                                   lines <-stplanr::od2line(flow = myflows, zones = ppcs)
                                 })
    ## Add index to lines to match with routes
    lines@data$id <- seq.int(nrow(lines@data))
    
    ## Check internet connection
    if (!curl::has_internet()){
      showNotification("Check your internet connection!",
                       type = "error",
                       duration = 5)
      return()
    }
    
    ## Get Routes by transport mode
    if (input$maprouteindmode == "Car" | input$maprouteindmode == "Car as Passenger") {
      routes <- line2routeRetryS(lines, route_osrm, profile = "driving", n_trial = 1000,
                                 n_processes = 1)
      
      ## Get Routes from route_graphhopper (for Car)
      # routes <- line2routeRetryS(lines, route_fun = "route_graphhopper", n_trial = 1000, vehicle = "car",
      #                            n_processes = 1, pat = c("a0008794-1655-4b90-8fcc-bbe0822fdd23"))
      # colnames(routes@data) <- c("duration","distance","change_elev","error","id")
      # routes@data$duration <- routes@data$duration * 60
      
    } else if (input$maprouteindmode == "Walking or Biking") {
      # routes <- line2routeRetryS(lines, route_osrm, profile = "bike", n_trial = 1000,
      #                            n_processes = 1)
      
      ## Get Routes from route_graphhopper (for Slow)
      routes <- line2routeRetryS(lines, route_fun = "route_graphhopper", n_trial = 1000, vehicle = "foot",
                                 n_processes = 1, pat = c("a0008794-1655-4b90-8fcc-bbe0822fdd23"))
      colnames(routes@data) <- c("duration","distance","change_elev","error","id")
      routes@data$duration <- routes@data$duration * 60
      
    } else if (input$maprouteindmode == "Public Transport") {
      ## Get Routes from route_osrm (for Public Transport)
      routes <- line2routeRetryS(lines, route_osrm, profile = "driving", n_trial = 1000,
                                 n_processes = 1)
    } else {
      ## Get Routes from route_osrm (for All modes)
      routes <- line2routeRetryS(lines, route_osrm, profile = "driving", n_trial = 1000,
                                 n_processes = 1)
      
      # ## Get Routes from route_graphhopper (for All modes)
      # routes <- line2routeRetryS(lines, route_fun = "route_graphhopper", n_trial = 1000, vehicle = "car",
      #                            n_processes = 1, pat = c("a0008794-1655-4b90-8fcc-bbe0822fdd23"))
      # colnames(routes@data) <- c("duration","distance","change_elev","error","id")
      # routes@data$duration <- routes@data$duration * 60
    }
    
    ## Simplifying polylines
    # routes <- rmapshaper::ms_simplify(routes)
    
    ## Give more information on routes
    routes@data$origin <- lines@data$origin[which(lines@data$id == routes@data$id)]
    routes@data$destination <- lines@data$destination[which(lines@data$id == routes@data$id)]
    routes@data$flow <- lines@data$flow[which(lines@data$id == routes@data$id)]
    
    ## Make the routes global variable (used for getting info on the map)
    routesIndPC4 <<- routes
    
    ## Print summary
    showNotification(paste(sum(lines@data$flow),"trips have been routed among",
                           nrow(lines),"O-D pairs."),
                     type = "message",
                     duration = 5)
    
    ## Put labels on lines
    labelsRoutesInd <- sprintf(
      "<strong>O-D pair ID: %s</strong><br/>
      O.PPC: %s<br/>
      D.PPC: %s<br/>
      N.trips: %s<br/>
      Distance: %s km<br/>
      Duration: %s min",
      
      routesIndPC4@data$id,
      routesIndPC4@data$origin,
      routesIndPC4@data$destination,
      routesIndPC4@data$flow,
      round(routesIndPC4@data$distance/1000,digits=1),
      round(routesIndPC4@data$duration/60,digits=0)
    ) %>% lapply(htmltools::HTML)
    
    # Add number of activities
    ppcs$NumActs <- table(sched$DestLoc)[match(ppcs$PC4,names(table(sched$DestLoc)))]
    
    # Add household density by 4 ppc
    ppcs$ActDensity <- round(ppcs$NumActs/
                               (ppcs$Shape_Area / 10^6),
                             digits = 2)
    
    labelsPpcs <- sprintf(
      "<strong>PPC: %s</strong><br/>
      # of Activities: %g <br/>
      Area: %g km<sup>2</sup><br/>
      Population density: % g Activities/km<sup>2</sup>",
      ppcs$PC4,
      ppcs$NumActs,
      ppcs$Shape_Area / 10^6,
      ppcs$ActDensity
    ) %>% lapply(htmltools::HTML)
    
    leaflet() %>%
      setView(lng=5.00 , lat =52.00, zoom=8) %>%
      
      # Base groups
      addTiles(group = "OSM (default)",options = providerTileOptions(noWrap = F)) %>%
      addProviderTiles(group = "CartoDB Dark",
                       provider = providers$CartoDB.DarkMatterNoLabels) %>%
      
      # Overlay groups
      addPolygons(data = ppcs,
                  group = "4-digit postcode area",
                  color = "#444444",
                  weight = 1,
                  smoothFactor = 0.5,
                  opacity = 0.5,
                  fill = TRUE,
                  fillColor = "#A9F5BC",
                  fillOpacity = 0.5,
                  label = labelsPpcs,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"),
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 3,
                                                      bringToFront = TRUE)) %>%
      addPolylines(data=routesIndPC4,
                   group = "Routes-Individual",
                   weight = myflows$flow/mean(myflows$flow),
                   color = "red",
                   label = labelsRoutesInd,
                   layerId = ~id,
                   opacity = 1,
                   highlightOptions = highlightOptions(color = "blue",
                                                       weight = 3,
                                                       bringToFront = TRUE)) %>%
      # # Add legend
      # addLegend(data = r,
      #           group = "Routes",
      #           pal = pal,
      #           values = myflows$flow,
      #           opacity = 0.7,
      #           title = "Number of trips",
      #           position = "bottomright") %>%
      
      # Layer control
      addLayersControl(
        baseGroups = c("OSM (default)", "CartoDB Dark"),
        overlayGroups = c("Routes-Individual","4-digit postcode area"),
        options = layersControlOptions(collapsed = TRUE)) %>%
      
      # hideGroup("Routes-Individual") %>%
      hideGroup("4-digit postcode area")
  })
  
  observeEvent(input$maprouteind_shape_click,{
    
    click <- input$maprouteind_shape_click
    
    # Ignore other shapes than circle shape
    # Note: No layerIDs are assigned to polygons for postcode
    if (is.null(click) | is.null(click$id)){
      return()
    }
    
    output$clickedrouteindId<-renderText({
      text <- paste("You've selected route", click$id)
      text
    })
    
    output$clickedrouteindTable <- DT::renderDataTable({
      
      table <- (subset(routesIndPC4@data, id == click$id))
      table <- DT::datatable(data = (t(table)), colnames = "",
                             options = list(paging = F, searching = F, pagelength = 25))
      table
    })
    
    ## Add pulse marker to the clicked activity
    proxy <- leafletProxy("maprouteind")
    if (click$id != "Selected") {
      proxy %>% setView(lng = click$lng, lat = click$lat, input$maprouteind_zoom, zoom = 13)
      proxy %>% addPulseMarkers(lng = click$lng,
                                lat = click$lat,
                                layerId = "Selected",
                                icon = makePulseIcon(heartbeat = 1))
    } else {
      # Remove previously clicked pulse marker
      proxy %>% removeMarker(layerId="Selected")
    }
  })
  
  ##############################################################################
  ###########             Leaflet Route-Individual PC6        ##################
  ##############################################################################
  
  filterschedmaprouteindpc6 <- eventReactive(input$submitmaprouteindpc6,{
    
    # Load schedule file
    sched <- read.csv("data/sched-coords.txt")
    sched <- timeconverter(sched)
    
    # Filter schedule by Activity type
    sched <- sched[sched$ActivityType %in% input$maprouteindpc6act,]
    # Filter schedule by Transport mode
    if (!input$maprouteindpc6mode == "All"){
      sched <- sched[sched$Mode == input$maprouteindpc6mode,]
    }
    # Filter schedule by Charging Type
    sched <- sched[sched$Charging %in% input$maprouteindpc6charging,]

    if (input$maprouteindpc6show) {
    sched <- sched[1:input$maprouteindpc6num,]
    }
    
    # # Filter schedule by Time of day
    # sched <- sched[which((sched$BeginTime > input$maprouteindtime[1] & sched$BeginTime < input$maprouteindtime[2])|
    #                        (sched$EndTime > input$maprouteindtime[1] & sched$EndTime < input$maprouteindtime[2])),]
    sched
  })
  
  output$maprouteindpc6 <- renderLeaflet({
    
    # Create input file error message
    if (is.null(input$schedule) | is.null(input$shpFilePc4)) {
      showNotification("Please provide input files!`` (schedule/shp file)",
                       type = "error",
                       duration = 5)
      # return()
    }
    
    # Filter schedule by input selection with more than 0 trip
    sched <- filterschedmaprouteindpc6()
    
    # Load shape file
    ppcs <- myppcs()
    
    
    withProgress(message = paste("Routing", nrow(sched),
                                 "trips in the schedule..."), value = 0, {
                                   setProgress(value = NULL, message = NULL, detail = NULL,
                                               session = getDefaultReactiveDomain())
    routes <- c()
    for (i in 1:nrow(sched)) {
      incProgress(1/nrow(sched),
                  detail = paste("Routing",i,"th trip out of",nrow(sched)))
      # print(i)
      from = c(sched$OrigLng[i],sched$OrigLat[i])
      to = c(sched$DestLng[i],sched$DestLat[i])
      route <- route_osrmRetryS(from, to, n_trial=500)
      # routes[[i]] = list(sp::Lines(sp::Line(route@lines[[1]]@Lines[[1]]),ID = sched$SchedID[[i]]))
      routes[i] = list(sp::Lines(route@lines[[1]]@Lines[[1]],ID = sched$SchedID[[i]]))
    }
                                 })
    
    routes <- SpatialLines(routes)
    routes <- SpatialLinesDataFrame(sl = routes, data = sched[1:nrow(sched),], match.ID = FALSE)
  
    ## Make the routes global variable (used to get route-info on the map)
    routesIndPC6 <<- routes
    
    labelsRoutesPC6 <- sprintf(
      "SchedID: %s <br/>
      O.PC6: %s <br/>
      D.PC6: %s <br/>",
      routesIndPC6$SchedID,
      routesIndPC6$OrigPC6,
      routesIndPC6$DestPC6
    ) %>% lapply(htmltools::HTML)
    
    leaflet() %>%
      setView(lng=5.00 , lat=52.00, zoom=8) %>%
      
      # Base groups
      addTiles(group = "OSM (default)",options = providerTileOptions(noWrap = F)) %>%
      addProviderTiles(group = "CartoDB Dark",
                       provider = providers$CartoDB.DarkMatterNoLabels) %>%
      
      # # Overlay groups
      # addPolygons(data = ppcs,
      #             group = "4-digit postcode area",
      #             color = "#444444",
      #             weight = 1,
      #             smoothFactor = 0.5,
      #             opacity = 0.5,
      #             fill = TRUE,
      #             fillColor = "#A9F5BC",
      #             fillOpacity = 0.5,
      #             # label = labelsPpcs,
      #             labelOptions = labelOptions(
      #               style = list("font-weight" = "normal", padding = "3px 8px"),
      #               textsize = "15px",
      #               direction = "auto"),
      #             highlightOptions = highlightOptions(color = "white",
      #                                                 weight = 3,
      #                                                 bringToFront = TRUE)) %>%
      addPolylines(data=routesIndPC6,
                   group = "Routes-Individual",
                   color = "red",
                   weight = 1,
                   label = labelsRoutesPC6,
                   layerId = ~SchedID,
                   opacity = 1,
                   highlightOptions = highlightOptions(color = "blue",
                                                       weight = 3,
                                                       bringToFront = TRUE)) %>%
      
      # Layer control
      addLayersControl(
        baseGroups = c("OSM (default)", "CartoDB Dark"),
        overlayGroups = c("Routes-Individual"),
        options = layersControlOptions(collapsed = TRUE))
      
      # hideGroup("Routes-Individual") %>%
      # hideGroup("4-digit postcode area")
  })
  
  observeEvent(input$maprouteindpc6_shape_click,{
    
    click <- input$maprouteindpc6_shape_click
    
    # Ignore other shapes than circle shape
    # Note: No layerIDs are assigned to polygons for postcode
    if (is.null(click) | is.null(click$id)){
      return()
    }
    
    output$clickedrouteindIdpc6<-renderText({
      text <- paste("You've selected route", click$id)
      text
    })
    
    output$clickedrouteindTablepc6 <- DT::renderDataTable({
      
      table <- (subset(routesIndPC6@data, SchedID == click$id))
      table <- DT::datatable(data = (t(table)), colnames = "",
                             options = list(paging = F, searching = F, pagelength = 25))
      table
    })
    
    ## Add pulse marker to the clicked activity
    proxy <- leafletProxy("maprouteindpc6")
    if (click$id != "Selected") {
      proxy %>% setView(lng = click$lng, lat = click$lat, input$maprouteindpc6_zoom, zoom = 12)
      proxy %>% addPulseMarkers(lng = click$lng,
                                lat = click$lat,
                                layerId = "Selected",
                                icon = makePulseIcon(heartbeat = 1))
    } else {
      # Remove previously clicked pulse marker
      proxy %>% removeMarker(layerId="Selected")
    }
  })
  
  ##############################################################################
  ###########              Leaflet Route-Aggregated PC4       ##################
  ##############################################################################
  
  filterschedmaprouteagg <- eventReactive(input$submitmaprouteagg,{
    # Load schedule file
    sched <- mysched()
    
    # Sample only outgoing trips
    schedOnlyOut <- sched[which(sched$OrigLoc!=sched$DestLoc),]
    
    # Filter schedule by Activity type
    schedOnlyOut <- schedOnlyOut[schedOnlyOut$ActivityType %in% input$maprouteaggact,]
    # Filter schedule by Transport mode
    if (!input$maprouteaggmode == "All"){
      schedOnlyOut <- schedOnlyOut[schedOnlyOut$Mode == input$maprouteaggmode,]
    }
    # Filter schedule by Charging type
    schedOnlyOut <- schedOnlyOut[schedOnlyOut$Charging %in% input$maprouteaggcharging,]
    schedOnlyOut
  })
  
  filterodpairmaprouteagg <- eventReactive(input$submitmaprouteagg,{
    
    # Filter schedule by input selection
    schedOnlyOut <- filterschedmaprouteagg()
    
    # Convert OD matrix to pairwise column
    myflows <- as.data.frame(table(schedOnlyOut$OrigLoc,schedOnlyOut$DestLoc))
    myflows <- myflows[with(myflows, order(-Freq)), ]
    
    # Print message for empty data
    if (nrow(myflows) == 0) {
      showNotification("No O-D pair was found. Please choose items as per panels above.",
                       type = "message",
                       duration = 5)
      return()
    }
    
    colnames(myflows) <- c("origin","destination","flow")
    
    # Remove OD pairs without flow
    myflows <- myflows[which(myflows$flow > 0),]
    
    # Sample OD pairs with largest n.trips
    if (input$maprouteaggshow) {
      # Print message for wrong input number of O-D pairs
      if (nrow(myflows) < input$maprouteaggnum) {
        showNotification("The number of O-D pairs you provide exceeds the total number of O-D pairs in data!",
                         type = "error",
                         duration = 5)
        return()
      }
      myflows <- myflows[1:input$maprouteaggnum,]
    }
    myflows
  })
  
  output$maprouteagg <- renderLeaflet({
    
    # Create input file error message
    if (is.null(input$schedule) | is.null(input$shpFilePc4)) {
      showNotification("Please provide input files! (schedule/shp file)",
                       type = "error",
                       duration = 5)
      # return()
    }
    
    # Load schedule file
    sched <- mysched()
    
    # Filter schedule by input selection with more than 0 trip
    schedOnlyOut <- filterschedmaprouteagg()
    
    # Filter O-D pair by input selection with more than 0 trip
    myflows <- filterodpairmaprouteagg()
    
    # Load shape file
    ppcs <- myppcs()
    
    # Add progress status to UI
    withProgress(message = paste("Converting", nrow(myflows),
                                 "O-D pairs to polyline..."), value = 0, {
                                   setProgress(value = NULL, message = NULL, detail = NULL,
                                               session = getDefaultReactiveDomain())
                                   ## Get OD polylines
                                   lines <-stplanr::od2line(flow = myflows, zones = ppcs)
                                 })
    ## Add index to lines to match with routes
    lines@data$id <- seq.int(nrow(lines@data))
    
    ## Check internet connection
    if (!curl::has_internet()){
      showNotification("Check your internet connection!",
                       type = "error",
                       duration = 5)
      return()
    }
    
    ## Get Routes by transport mode
    if (input$maprouteaggmode == "Car" | input$maprouteaggmode == "Car as Passenger") {
      routes <- line2routeRetryS(lines, route_osrm, profile = "driving", n_trial = 1000,
                                 n_processes = 1)
      
      ## Get Routes from route_graphhopper (for Car)
      # routes <- line2routeRetryS(lines, route_fun = "route_graphhopper", n_trial = 1000, vehicle = "car",
      #                            n_processes = 1, pat = c("a0008794-1655-4b90-8fcc-bbe0822fdd23"))
      # colnames(routes@data) <- c("duration","distance","change_elev","error","id")
      # routes@data$duration <- routes@data$duration * 60
      
    } else if (input$maprouteaggmode == "Walking or Biking") {
      # routes <- line2routeRetryS(lines, route_osrm, profile = "bike", n_trial = 1000,
      #                            n_processes = 1)
      
      ## Get Routes from route_graphhopper (for Slow)
      routes <- line2routeRetryS(lines, route_fun = "route_graphhopper", n_trial = 1000, vehicle = "foot",
                                 n_processes = 1, pat = c("a0008794-1655-4b90-8fcc-bbe0822fdd23"))
      colnames(routes@data) <- c("duration","distance","change_elev","error","id")
      routes@data$duration <- routes@data$duration * 60
      
    } else if (input$maprouteaggmode == "Public Transport") {
      ## Get Routes from route_osrm (for Public Transport)
      routes <- line2routeRetryS(lines, route_osrm, profile = "driving", n_trial = 1000,
                                 n_processes = 1)
    } else {
      ## Get Routes from route_osrm (for All modes)
      routes <- line2routeRetryS(lines, route_osrm, profile = "driving", n_trial = 1000,
                                 n_processes = 1)
      
      # ## Get Routes from route_graphhopper (for All modes)
      # routes <- line2routeRetryS(lines, route_fun = "route_graphhopper", n_trial = 1000, vehicle = "car",
      #                            n_processes = 1, pat = c("a0008794-1655-4b90-8fcc-bbe0822fdd23"))
      # colnames(routes@data) <- c("duration","distance","change_elev","error","id")
      # routes@data$duration <- routes@data$duration * 60
    }
    
    ## Simplifying polylines
    # routes <- rmapshaper::ms_simplify(routes)
    
    ## Give more information on routes
    routes@data$origin <- lines@data$origin[which(lines@data$id == routes@data$id)]
    routes@data$destination <- lines@data$destination[which(lines@data$id == routes@data$id)]
    routes@data$flow <- lines@data$flow[which(lines@data$id == routes@data$id)]
    
    # Convert series of overlapping lines into a route network
    routes <- overline(routes, attrib = "flow", fun = sum)
    
    # Remove line with 0 flow
    routes <- routes[which(routes$flow > 0),]
    
    # Give id to aggregated routes
    routes$id <- seq.int(nrow(routes@data))
    
    ## Make the routes global variable (used for getting info on the map)
    routes <<- routes
    
    ## Print summary
    showNotification(paste(sum(lines@data$flow),"trips have been routed among",
                           nrow(lines),"O-D pairs."),
                     type = "message",
                     duration = 5)
    
    ## Put labels on lines
    labelsRoutesAgg <- sprintf(
      "<strong>Segment ID: %s</strong><br/>
      N.trips: %s",
      routes$id,
      routes$flow
    ) %>% lapply(htmltools::HTML)
    
    # Add number of activities
    ppcs$NumActs <- table(sched$DestLoc)[match(ppcs$PC4,names(table(sched$DestLoc)))]
    
    # Add household density by 4 ppc
    ppcs$ActDensity <- round(ppcs$NumActs/
                               (ppcs$Shape_Area / 10^6),
                             digits = 2)
    
    labelsPpcs <- sprintf(
      "<strong>PPC: %s</strong><br/>
      # of Activities: %g <br/>
      Area: %g km<sup>2</sup><br/>
      Population density: % g Activities/km<sup>2</sup>",
      ppcs$PC4,
      ppcs$NumActs,
      ppcs$Shape_Area / 10^6,
      ppcs$ActDensity
    ) %>% lapply(htmltools::HTML)
    
    leaflet() %>%
      setView(lng=5.00 , lat =52.00, zoom=8) %>%
      
      # Base groups
      addTiles(group = "OSM (default)",options = providerTileOptions(noWrap = F)) %>%
      addProviderTiles(group = "CartoDB Dark",
                       provider = providers$CartoDB.DarkMatterNoLabels) %>%
      
      # Overlay groups
      addPolygons(data = ppcs,
                  group = "4-digit postcode area",
                  color = "#444444",
                  weight = 1,
                  smoothFactor = 0.5,
                  opacity = 0.5,
                  fill = TRUE,
                  fillColor = "#A9F5BC",
                  fillOpacity = 0.5,
                  label = labelsPpcs,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"),
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 3,
                                                      bringToFront = TRUE)) %>%
      addPolylines(data=routes,
                   group = "Routes-Aggregated",
                   weight = routes$flow/mean(routes$flow),
                   color = "red",
                   label = labelsRoutesAgg,
                   layerId = ~id,
                   opacity = 1,
                   highlightOptions = highlightOptions(color = "blue",
                                                       weight = 3,
                                                       bringToFront = TRUE)) %>%
      # # Add legend
      # addLegend(data = r,
      #           group = "Routes",
      #           pal = pal,
      #           values = myflows$flow,
      #           opacity = 0.7,
      #           title = "Number of trips",
      #           position = "bottomright") %>%
      
      # Layer control
      addLayersControl(
        baseGroups = c("OSM (default)", "CartoDB Dark"),
        overlayGroups = c("Routes-Aggregated", "4-digit postcode area"),
        options = layersControlOptions(collapsed = TRUE)) %>%
      
      # hideGroup("Routes-Aggregated") %>%
      hideGroup("4-digit postcode area")
  })
  
  observeEvent(input$maprouteagg_shape_click,{
    click <- input$maprouteagg_shape_click
    
    # Ignore other shapes than circle shape
    # Note: No layerIDs are assigned to polygons for postcode
    if (is.null(click) | is.null(click$id)){
      return()
    }
    
    output$clickedrouteaggId<-renderText({
      text <- paste("You've selected route segment", click$id)
      text
    })
    
    output$clickedrouteaggTable <- DT::renderDataTable({
      
      table <- (subset(routes@data, id == click$id))
      table <- DT::datatable(data = (t(table)), colnames = "",
                             options = list(paging = F, searching = F, pagelength = 25))
      table
    })
    
    ## Add pulse marker to the clicked activity
    proxy <- leafletProxy("maprouteagg")
    if (click$id != "Selected") {
      proxy %>% setView(lng = click$lng, lat = click$lat, input$maprouteagg_zoom, zoom = 13)
      proxy %>% addPulseMarkers(lng = click$lng,
                                lat = click$lat,
                                layerId = "Selected",
                                icon = makePulseIcon(heartbeat = 1))
    } else {
      # Remove previously clicked pulse marker
      proxy %>% removeMarker(layerId="Selected")
    }
  })
  
  ##############################################################################
  ###########                  Leaflet Animation              ##################
  ##############################################################################
  
  points <- reactive({
    sched <- mysched()
    ppcs <- myppcs()
    
    # Sample ppcs only where activities are occured.
    ppcsSample <- subset(ppcs, ppcs@data$PC4 %in% sched$DestLoc)
    
    # Add number of activities within 4 ppcs
    ppcsSample$NumActs <- table(sched$DestLoc)[match(ppcsSample$PC4,names(table(sched$DestLoc)))]
    
    # Add household density by 4 ppc
    ppcsSample$ActDensity <- round(ppcsSample$NumActs/
                                     (ppcsSample$Shape_Area / 10^6),
                                   digits = 2)
    
    ## Get coordinates
    coords <- c()
    schedid <- c()
    for (i in ppcsSample$PC4) {
      # Get total number of activities by postcode
      n <- sum(ppcsSample$NumActs[ppcsSample$PC4 == i], na.rm = TRUE)
      
      schedid <- append(schedid,sched$SchedID[sched$DestLoc == i])
      
      polygon <- ppcsSample[ppcsSample@data$PC4 == i,]@polygons
      
      chosenPolygon <- 1
      for (j in 1:length(polygon)) {
        if (j > 1) {
          if (polygon[[j]]@area > polygon[[j-1]]@area){
            chosenPolygon <- j
          }
        }
      }
      
      if (class(polygon) == "list" & length(polygon) > 1) { ## For multi-polygons
        polygon <- polygon[[chosenPolygon]]
        if (length(polygon@Polygons) == 1) {
          
        } else {
          chosen <- (polygon@plotOrder)[1]
          polygon <- polygon@Polygons[[chosen]]
        }
      } else {
        polygon <- polygon[[chosenPolygon]]
        if (length(polygon@Polygons) == 1) {
          
        } else {
          chosen <- (polygon@plotOrder)[1]
          polygon <- polygon@Polygons[[chosen]]
        }
      }
      coords <- rbind(coords,spsample(polygon, n = n, type = "random")@coords)
    }
    sched$Lng <- coords[,1]
    sched$Lat <- coords[,2]
    
    sched <- sched[which((sched$BeginTime > input$mapanimtime[1] & sched$BeginTime < input$mapanimtime[2])|
                           (sched$EndTime > input$mapanimtime[1] & sched$EndTime < input$mapanimtime[2])),]
    return(sched)
  })
  
  observeEvent(input$animtime,{
    leafletProxy("mapanim") %>%
      clearShapes() %>%
      addCircles(data = points(),
                 lng= ~Lng,
                 lat= ~Lat,
                 layerId = ~SchedID,
                 radius = 8,
                 weight = 5
      )
  })
  
  output$mapanim <- renderLeaflet({
    leaflet() %>%
      setView(lng=5.4697 , lat =51.4416, zoom=7) %>%
      addProviderTiles(group = "CartoDB Dark",
                       provider = providers$CartoDB.DarkMatterNoLabels)
    
  })
  
  ##############################################################################
  ###########################     Custom Functions    ##########################
  ##############################################################################
  
  # Based on Robinlovelace's stplanr/R/od-funs.R
  # https://github.com/ropensci/stplanr/blob/master/R/od-funs.R
  line2routeRetryS <- function(l, route_fun = "route_osrm", n_trial = 200,
                               n_print = 10,list_output = FALSE, l_id = NA,
                               n_processes = 1,...){
    
    return_sf <- is(l, "sf")
    if(return_sf) {
      l <- as(l, "Spatial")
    }
    FUN <- match.fun(route_fun)
    ldf <- stplanr::line2df(l)
    n_ldf <- nrow(ldf)
    
    if(n_processes > 1){
      n_processes <- min(c(n_processes, n_ldf))
      cl <- parallel::makeCluster(n_processes)
      doParallel::registerDoParallel(cl)
    }
    if(n_processes > 1){
      if(!require(foreach)) {
        stop("You must install foreach before running this code")
      }
      
      rc <- foreach::foreach(i = 1:n_ldf, .errorhandling = "pass") %dopar% {
        
        FUN(from = c(ldf$fx[i], ldf$fy[i]),
            to = c(ldf$tx[i], ldf$ty[i]),...)
        
      }
      
      parallel::stopCluster(cl)
      
    } else {
      
      ## add progress bar
      withProgress(message = "Routing calculation is in progress...", value = 0, {
        rc <- as.list(rep(NA, length(l)))
        for(i in 1:n_ldf){
          # Increment the progress bar, and update the detail text.
          incProgress(1/n_ldf,
                      detail = paste("Routing",i,"th O-D pair out of",
                                     n_ldf,"pairs"))
          
          # Retry for intermittent errors while calculating route in OSRM
          attempt <- 1
          while(typeof(rc[[i]])!="S4" && attempt <= n_trial) {
            attempt = attempt + 1
            rc[[i]] <- try({
              FUN(from = c(ldf$fx[i], ldf$fy[i]),
                  to = c(ldf$tx[i], ldf$ty[i]),...)
            },silent = T)
          }
          # perc_temp <- i %% round(n_ldf / n_print)
          # # print % of distances calculated
          # if(!is.na(perc_temp) & perc_temp == 0){
          #   message(paste0(round(100 * i/n_ldf), " % out of ",
          #                  n_ldf, " distances calculated"))
          # }
        }
      })
    }
    
    if(list_output) {
      r <- rc
    } else {
      # Set the names based on the first non failing line (then exit loop)
      for(i in 1:n_ldf){
        if(grepl("Spatial.*DataFrame", class(rc[[i]]))[1]) {
          rdata <- data.frame(matrix(nrow = nrow(l), ncol = ncol(rc[[i]]) + 1))
          names(rdata) <- c(names(rc[[i]]), "error")
          r <- l
          r@data <- rdata
          break
        }
      }
      
      # Copy rc into r including the data or copy the error into r
      for(i in 1:n_ldf){
        if(grepl("Spatial.*DataFrame", class(rc[[i]]))[1]) {
          r@lines[[i]] <- sp::Lines(rc[[i]]@lines[[1]]@Lines, row.names(l[i,]))
          r@data[i,] <- c(rc[[i]]@data, error = NA)
        } else {
          r@data[i, "error"] <- rc[[i]][1]
        }
      }
      
      # Set the id in r
      l_ids <- c(l_id, "id")
      l_id <- l_ids[!is.na(l_ids)][1]
      r$id <- if(l_id %in% names(l)){
        l@data[[l_id]]
      } else {
        row.names(l)
      }
    }
    if(return_sf) {
      r <- sf::st_as_sf(r)
    }
    r
  }
  
  ##############################################################################
  
  route_osrmRetryS <- function(from, to , route_fun = "route_osrm", n_trial = 200,
                               ...){
    
    FUN <- match.fun(route_fun)
    
    rc <- c()
    
    # Retry for intermittent errors while calculating route in OSRM
    attempt <- 1
    while(class(rc) !="SpatialLinesDataFrame" && attempt <= n_trial) {
      attempt = attempt + 1
      rc <- try({
        FUN(from = from,
            to = to,...)
      },silent = T)
    }
    return(rc)
  }
}

