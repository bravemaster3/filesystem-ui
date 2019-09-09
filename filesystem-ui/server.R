
server <- function(input,output, session){
  #Defining reactive values to make it available to other modules
  rResult <- reactiveValues(lookup_locations = 0, avail_table = 0, avail_table_filt = 0)


  # # when the user changes tabs, save the state in the URL
  # observeEvent(input$navbar, {
  #   if (values$autoNavigating > 0) {
  #     values$autoNavigating <- values$autoNavigating - 1
  #     return()
  #   }
  #   
  #   if (input$navbar == "explore") {
  #     shinyjs::js$updateHistory(page = "explore", query = values$searchString)
  #   } else {
  #     shinyjs::js$updateHistory(page = input$navbar)
  #   }
  # })
  
  
  
  output$mymap2 <- output$mymap <- renderLeaflet({
    m <- leaflet() %>%
      addTiles() %>%
      addProviderTiles("Esri.WorldImagery", group = "ESRI Aerial") %>%
      addProviderTiles("Stamen.TerrainBackground", group = "Relief")%>%
      addProviderTiles("OpenStreetMap", group = "Topo") %>%
      #addMouseCoordinates(style = "basic") %>%
      addLayersControl(
        baseGroups = c("OpenStreetMap","ESRI Aerial","Relief"))%>%
      addDrawToolbar(polylineOptions = F, circleOptions = F, markerOptions = F,
                     circleMarkerOptions = F, polygonOptions = F) %>%
      setView(lng=map_center_x,lat=map_center_y,zoom=7)
  })
  
  #the following will toggle the side_bar on the click of "hide_controls_Btn" button
  observeEvent(input$hide_controls_Btn, {
    toggle(id = "controls") #expl_sidebar_div")
    #toggle(id = "hide_controls_Pnl")
    
  },ignoreInit=F)
  
  
  #Zoom to location on location selection
  # observeEvent(input$slct_location, {
  #   long_lat_slctd <- coordinates_spliter(input$slct_location)
  #   proxy <- leafletProxy("mymap")
  #   proxy %>% setView(input$mymap, lng = long_lat_slctd[1], lat=long_lat_slctd[2], zoom=9)
  # },ignoreInit = T)
  
  # output$info_table <- DT::renderDataTable({
  #   DT::datatable(availability_table("013733E045998N", "sensors"))
  #   })
  
 # observe({
 # 
 #    list_metadata <- paste(path_to_locations,input$slct_big_sq,"metadata.csv",sep="/")
 #    #print(list_metadata)
 #    #Look up locations loading
 #    lookup_locations <- ldply(list_metadata, read.table, sep=",", header=TRUE)
 #    #print(lookup_locations)
 #    
 #    #lookup_locations <- read.table(paste(path_to_locations,"metadata.csv", sep="/"), header = TRUE, sep=",", encoding = "UTF-8")
 #    #names(lookup_locations) <- c("location_names", "common_name")
 #    lookup_locations$location_names <- as.character(lookup_locations$location_names)
 #    lookup_locations$common_name <- as.character(lookup_locations$common_name)
 #    #return(lookup_locations)
 #  
 #  })
    
  observeEvent(input$validate_Btn, {
    
    #retrieving selected big squares and creating a list of paths to read and append metadata for lookup locations
    

#lookup_locations <- reactive({
    list_metadata <- paste(path_to_locations,input$slct_big_sq,"metadata.csv",sep="/")
    #print(list_metadata)
    #Look up locations loading
    lookup_locations <- ldply(list_metadata, read.table, sep=",", header=TRUE)
    #print(lookup_locations)

    #lookup_locations <- read.table(paste(path_to_locations,"metadata.csv", sep="/"), header = TRUE, sep=",", encoding = "UTF-8")
    #names(lookup_locations) <- c("location_names", "common_name")
    lookup_locations$location_names <- as.character(lookup_locations$location_names)
    lookup_locations$common_name <- as.character(lookup_locations$common_name)
    
    rResult$lookup_locations <- lookup_locations
    
    #lookup_locations = as.data.frame(lookup_locations)
    #source("module.R")
#})
#print(lookup_locations())
    #replacing location_dirs by a new one that is limited to  selected big squares
    
    #location_dirs <- list.dirs.depth.n(paste(path_to_locations,"012E042N",sep="/"),2)
    location_dirs <- list.dirs.depth.n(paste(path_to_locations,input$slct_big_sq,sep="/"),2)
    
    
    loc_geom_all <- loc_geom_creator(location_dirs,lookup_locations)
    loc_geom_photo <- loc_geom_pertype(location_dirs,"photo",lookup_locations)
    loc_geom_samples <- loc_geom_pertype(location_dirs,"samples",lookup_locations)
    loc_geom_sensors <- loc_geom_pertype(location_dirs,"sensors",lookup_locations)
    loc_geom_survey <- loc_geom_pertype(location_dirs,"survey",lookup_locations)
    
    
    #Updating the choice list of slct_location select input
    
    updateSelectInput(session, inputId="slct_location", choices = loc_geom_all$common_name,
                      selected = NULL)
    
    m <- leafletProxy("mymap")
    
    #check first if there is any photo, sensors, samples... at all before adding them to the map to avoir any eror
    m <- m%>% 
      markers_simplifier(map = m, sp_df = loc_geom_all, group_name = "All Locations", fill_color = "#A5DF00")
    
    #groups_string <- as.vector("All Locations")
    groups_string <- as.vector(NULL)
    if (!is.null(loc_geom_photo)){
      m <- m%>%
        markers_simplifier(map = m, sp_df = loc_geom_photo, group_name = "Photo", fill_color = "#FFBF00")
      groups_string <- c(groups_string,"Photo")
    }
    if (!is.null(loc_geom_samples)){
      m <- m%>%
        markers_simplifier(map = m, sp_df = loc_geom_samples, group_name = "Samples", fill_color = "#00BFFF")
      groups_string <- c(groups_string,"Samples")
    }
    if (!is.null(loc_geom_sensors)){
      m <- m%>%
        markers_simplifier(map = m, sp_df = loc_geom_sensors, group_name = "Sensors", fill_color = "#DF0101")
      groups_string <- c(groups_string,"Sensors")
    }
    if (!is.null(loc_geom_survey)){
      m <- m%>%
        markers_simplifier(map = m, sp_df = loc_geom_survey, group_name = "Survey", fill_color = "#FFFFFF")
      groups_string <- c(groups_string,"Survey")
    }
    
    #test_list <- c("Sensors", "Photo","Samples","Survey")
    m <- m%>%
      #addPolygons()%>%
      addLayersControl(
        baseGroups = c("OpenStreetMap","ESRI Aerial","Relief"),
        overlayGroups = c("All Locations", groups_string),
        options = layersControlOptions(collapsed = T)
      ) %>%
      hideGroup(groups_string)
    #setView(lng = coordinates_spliter("Gis vrt")[1], lat=coordinates_spliter("Gis vrt")[2], zoom=8)
    m
  }, ignoreInit = F)
    
    
  observeEvent(input$big_squares, {
    #long_lat_slctd <- coordinates_spliter(input$slct_location)

    if(input$big_squares){
      #proxy <- leafletProxy("mymap")
      #mymap_copy <-leafletProxy("mymap")
      leafletProxy("mymap") %>% addPolygons(data = big_squares, stroke = TRUE, fillColor = "blues", fill = TRUE, layerId = big_squares$sq_name, popup=big_squares$sq_name)#####layerId = c(paste("big",c(1:nrow(big_squares)),sep="_")), popup=big_squares$sq_name)#unique(6400)) #, popup = ~name
    }
    else{
      leafletProxy("mymap") %>% removeShape(layerId = big_squares$sq_name) # c(paste("big",c(1:nrow(big_squares)),sep="_")))

    }
    
  },ignoreInit = T)
  
  
  #let's use the shape_click event to choose a big square on map.
  observe({
    click = input$mymap_shape_click
    # if(is.null(click)) return ()
    if (!is.null(click)) {
      map_selected_big_sq <- as.character(big_squares[big_squares$sq_name %in% input$mymap_shape_click, "sq_name"]$sq_name)
      # new_selection <- input$slct_big_sq
      # if(map_selected_big_sq %in% input$slct_big_sq)
      #   {new_selection <- setdiff(input$slct_big_sq, map_selected_big_sq)}
      # else new_selection <- c(input$slct_big_sq, map_selected_big_sq)
      
      if(!(map_selected_big_sq %in% input$slct_big_sq)){
        updateSelectInput(session, inputId="slct_big_sq", selected = c(input$slct_big_sq,map_selected_big_sq))
        
        }

      # #print(map_selected_big_sq)
      # else {#(map_selected_big_sq %in% input$slct_big_sq){
      #  updateSelectInput(session, inputId="slct_big_sq", selected = setdiff(input$slct_big_sq,map_selected_big_sq))
      # }
    }
    
    #print(selected_big_sq)
    #input$slct_big_sq = c(input$slct_big_sq,map_selected_big_sq)
  })

  #test <- c("Krucmanove konte", "Gropajski bori")
  #slcted_datatype <- c("sensors")
  observeEvent(input$infoBtn, {
    #making a local copy of llookup_locations from the server rResult reactive value list...
    lookup_locations <- rResult$lookup_locations
    #std_name <- lookup_locations$location_names[which(lookup_locations$common_name %in% test)]
    std_name <- lookup_locations$location_names[which(lookup_locations$common_name %in% input$slct_location)]
    slcted_datatype <- input$slct_datatype
    
    #we'll first create the dataframe, then use it to generate the output table
    #looping through the list of locations, apply the function and return a binded dataframe
    df <- setNames(data.frame(matrix(ncol = 8, nrow = 0)), c("location_names","device","list_param","list_param_long","date_min", "date_max","step","datatype"))
    for (i in std_name){
      for (j in slcted_datatype){
        new <- availability_table_simpl(i, j)
        if (nrow(new) != 0) df <- rbind(df, new)
      }
    }
    #test <- as.data.frame(c("location_names","device","list_param","date_min", "date_max","step"),c(NA,NA,NA,NA,NA,NA))
    
    #################
    ####
    #min(as.yearmon(c("2018-01","2009-02")))
    ####
    ################
    
    #df <- availability_table_simpl(std_name, slcted_datatype)
    #df$year_month <- paste(year(df$year_month),sprintf("%02d",month(df$year_month)), sep="-")
    
    #let's get the starting date and ending dates from the table before transforming the format of the date
    #initializing start and end dates so that when the button is clicked without any selection, we don't get an error
    start_date <- end_date <- Sys.Date()
    try({
        start_date <- paste(year(min(df$date_min)),sprintf("%02d",month(min(df$date_min))),"01", sep="-");
        end_date <- paste(year(max(df$date_max)),sprintf("%02d",month(max(df$date_max))),"01", sep="-")
    }, silent = TRUE)
    
    #formating the year-month columns to a proper format for display since the yearmon object doesn't display in DT rendertable...
    df$date_min <- paste(year(df$date_min),sprintf("%02d",month(df$date_min)), sep="-")
    df$date_max <- paste(year(df$date_max),sprintf("%02d",month(df$date_max)), sep="-")
    df <- merge(df,lookup_locations,by="location_names")
    #df$year_month <- as.yearmon(df$year_month)
    
    #Now let's put together all parameters and collect a unique list of them
    all_param <- paste(df$list_param,sep=",")
    unique_param <- unique(unlist(strsplit(all_param, ", ")))
    
    all_param_long <- paste(df$list_param_long,sep=",")
    unique_param_long <- unique(unlist(strsplit(all_param_long, ", ")))
    
    #sorting both unique_param and unique_param_long
    unique_param_long_inter <- unique_param_long[sort(unique_param_long, index.return=TRUE)$ix]
    unique_param <- unique_param[sort(unique_param_long, index.return=TRUE)$ix]
    unique_param_long <- unique_param_long_inter
    
    #saving df to be usable outside this callback
    rResult$avail_table <- df
    # output$filter_param <- renderText({
    #   unique_param
    # })
    
    # output$filter_param <- renderText({
    #   unique_param
    # })
    
    # output$test <- renderUI({
    #   p(list_param_long)
    # })
    # 
    output$filters_wellpanel <- renderUI({
      tabsetPanel(
        tabPanel("Parameters",
                 wellPanel(style = "background: white", 
                        tags$head(tags$style(HTML(".multicol{font-size:12px;
                                                  height:auto;
                                                  -webkit-column-count: 7;
                                                  -moz-column-count: 7;
                                                  column-count: 7;
                                                  }"))),
                        tags$div(align = "left", 
                                 class = "multicol",
                        checkboxGroupInput(
                          inputId = "filter_param_check",
                          #paste0("checkboxfood_", i),
                          label = NULL,
                          #choices = c(sort(unique_param)),
                          inline = TRUE,
                          choiceNames = as.character(unique_param_long), ### for some reason it likes characters, not factors with extra levels
                          choiceValues = as.character(unique_param)
                          #selected = selected_ids
                        )
                      )
                      )
          ),
        tabPanel("date/time",
                 
                   fluidPage(style = "background: white",
                             fluidRow(
                             column(2,                          
                                    dateInput(inputId='datestart_filter', width="100%",label = 'Choose start date:', value = start_date), #Sys.Date()),
                                    dateInput(inputId='dateend_filter', width="100%",label = 'Choose end date:', value = end_date) #Sys.Date())
                                    ),
                             column(3,                          
                                    timeInput("timestart_filter", "Choose start time", value =  strptime("00:00:00", "%T")),
                                    timeInput("timeend_filter", "Choose end time", value =  strptime("00:00:00", "%T"))
                             ),
                             column(2,
                                    selectInput(inputId = "step_slct", label = "select timestep",
                                                choices = c("5","10","30","HOUR","DAY","WEEK","MONTH","SURVEY","YEAR"), selected="30")
                               
                             ),
                             column(2,
                                    actionButton("visualize_Btn","NEXT")
                                    )
                             )
                          )
                           
                 )
    )
    })
    
    # df_infotable <- df
    # df_infotable$
    
    #rendering df to the info_table already defined in ui.R
    output$info_table <- DT::renderDataTable({
      #df[,(!names(df)=="list_param_long")] this is aimed at removing the long names of parameters from the table
      DT::datatable(df[,(!names(df)=="list_param_long")], style ='bootstrap', class = "compact", filter="top")%>%
        formatStyle(colnames(df[,(!names(df)=="list_param_long")])[1:ncol(df[,(!names(df)=="list_param_long")])],  color = 'black', backgroundColor = 'white', fontWeight = 'normal')
    })
  }, ignoreInit = F)
  observeEvent(input$visualize_Btn, {
    updateTabsetPanel(session, "navbar", selected="get_data")
    output$test_selected_param <- renderText({
      input$filter_param_check
    })
    #The following will test if there are some parameters selected. If so, then they will be used to filter the table
    if(!(is.null(input$filter_param_check))) {
      rResult$avail_table_filt <- rResult$avail_table[which(str_detect(rResult$avail_table$list_param, paste(c(input$filter_param_check),collapse="|"))),]
      rResult$avail_table_filt$paths <- paste (path_to_locations,
                                               rResult$avail_table_filt$level1, rResult$avail_table_filt$level2,
                                               rResult$avail_table_filt$location_names,rResult$avail_table_filt$datatype,
                                               sep="/"
                                               )
      list_files_path_filt <- list.files(path = rResult$avail_table_filt$paths,
                               pattern = glob2rx(paste("*_",input$step_slct,"_*",sep="")),
                               full.names = TRUE, recursive = TRUE)
      
      #print(list_files_path_filt)
          #list_files_filt_read = lapply(list_files_path_filt, read.csv)
 #################################################################     
      read_personalized <- function(i){
        df <- read.csv(i)
        #names <- unlist(strsplit(i, "_"))
        name <- str_sub(i, -20, -5) #this will work as long as the file structure is kept
        df["location_names"] <- name
        df$time <- as.POSIXct(df$time,tz="GMT")
        return(df)
      }
      
      ######################################################################
      list_files_filt_read = lapply(list_files_path_filt, read_personalized) #trying the read_personalized instead of fread
      
      # factor_to_time <- function(i){
      #   i$time <- as.POSIXct(i$time,tz="GMT")
      #   i
      # }
      
      #list_files_filt_read = lapply(list_files_filt_read, factor_to_time)
      
      merged_file_filt <- as_tibble(rbindlist(list_files_filt_read, fill=TRUE, use.names=TRUE))
      
      merged_file_filt <- merge(x=merged_file_filt, y=rResult$lookup_locations[,c("location_names","common_name")], by="location_names", all.x = TRUE) #this is for merging the common names of locations
      merged_file_filt$location_names <- NULL
      
      #reordering the columns of merged_file_filt
      first_columns <- c("time","common_name")
      merged_file_filt <- merged_file_filt[, c(first_columns, setdiff(names(merged_file_filt),first_columns))] #sets time and common_name columns first

      #View(unique(merged_file_filt))
      
      #check if selected parameters are in the list of column_names of "merged_file_filt
      slctd_param_adding_mandatory <- c("time","common_name","oznaka_profila",input$filter_param_check)
      slctd_param_verified <- unique(intersect(slctd_param_adding_mandatory,colnames(merged_file_filt)))
      
      merged_file_filt_slct <- merged_file_filt[slctd_param_verified]
      final_merged <- unique(merged_file_filt_slct)
      
      #View(final_merged)
      #print(str(merged_file_filt))
      #print(rResult$lookup_locations)
      
    
      #rbind_all()
      
      
      #rendering the data table
      output$selected_param_table <- DT::renderDataTable({
        #df[,(!names(df)=="list_param_long")] this is aimed at removing the long names of parameters from the table
        DT::datatable(final_merged, style ='bootstrap', class = "compact", filter="top")#%>%
          #formatStyle(colnames(df[,(!names(df)=="list_param_long")])[1:ncol(df[,(!names(df)=="list_param_long")])],  color = 'black', backgroundColor = 'white', fontWeight = 'normal')
      })
      
      }
    
    # View(rResult$avail_table)
    #View(rResult$avail_table_filt)
      #rResult$avail_table[rResult$avail_table$list_param %in% input$filter_param_check,]
  },ignoreInit=F)
  
  
}