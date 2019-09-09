#library(shinythemes)
#https://bootswatch.com/yeti/

ui <- navbarPage(title = "GozdIS",
                 useShinyjs(), #important for shinyjs functions to work in server.R
                   # div(
                   #          div(
                   #            id = "logo-id",
                   #            img(src = "logo.png")
                   #          ),
                   #          "GozdIS"
                   #        )
                 #,
                 id="navbar", collapsible = T, fluid = T, #theme = shinytheme("simplex"),
                 theme = "styles.css",
                 #shinythemes::themeSelector(),
                 tabPanel("Explore", value="explore",
                          div(class="outer",
                            fluidRow(
                              #div(class="leaflet_explore",
                              column(width= 12, offset = 0,
                                     leafletOutput("mymap", width="100%", height = "85vh"),
                                     #div(id="expl_sidebar_div", class="outer_fixed",
                                       absolutePanel(id = "controls",style = "z-index:999;", class = "panel panel-default", fixed = FALSE, ##the z index was set to 1000 to be above the leaflet map, which itself is set to 999 at the bottom of the ui.R to solve the prbo   
                                                     draggable = FALSE, top = 0, left = 0, right = NULL, bottom = "auto",
                                                     width = 300, height = "auto",
                                                     h2("Data explorer"),
                                                     checkboxInput("big_squares", "Display squares", value = FALSE),
                                                     selectInput("slct_big_sq", "Select squares", multiple = TRUE,
                                                               choices = big_sq_dirs, selected = big_sq_dirs[1]),
                                                     actionButton("validate_Btn", "Validate Selection"),
                                                     selectInput("slct_location", "Select Location", multiple = TRUE,
                                                                 choices = ""),#loc_geom_all$common_name),
                                                     selectInput("slct_datatype", "Select Datatype", multiple = TRUE,
                                                                 choices = c("photo","samples","sensors","survey"), selected = c("photo","samples","sensors","survey")),
                                                     actionButton("infoBtn", "Fetch info")
                                       
                                     #)
                              ),
                              absolutePanel(id="hide_controls_Pnl",style = "z-index:1000;",left=269, #class = "panel panel-default", top=-1,left=268,width = "auto", height = "auto",
                                            actionButton("hide_controls_Btn", "☰",width="40",height=5)
                              )
                               
                              )
                            ),
                            
                            fluidRow(
                              div(class="filters",
                                  uiOutput("filters_wellpanel")
                              )
                            ),
                            
                            # fluidRow(
                            #   div(class="filters",
                            #       uiOutput("test")
                            #   )
                            # ),
                            
                            fluidRow(
                              div(class="avail_table",
                                  #h2("Data availability information"),
                                      DT::dataTableOutput("info_table")
                                  )
                              )
                          
                  )
                 ),
                 navbarMenu("alphanum data",
                            tabPanel("Get data", value="get_data",
                                     fluidRow(
                                       div(
                                           #h2("Data availability information"),
                                           uiOutput("test_selected_param")
                                       )
                                     ),
                                     
                                     #let's display here the table resulting from the selection
                                     fluidRow(
                                       div(class="avail_table",
                                           #h2("Data availability information"),
                                           DT::dataTableOutput("selected_param_table")
                                       )
                                     )
                                     
                            ),
                            tabPanel("Aggregation", value="aggregation"
                                     
                            )
                          ),
                 tabPanel("images"
                          
                          ),
                 navbarMenu("Geospatial",
                            tabPanel("shapefiles",
                              leafletOutput("mymap2", width="100vw", height = "93vh")
                            )
                          ),
                 
                 
                 tags$head(tags$style(".leaflet-top {z-index:999!important;}")) #this makes the leaflet controls not appear on top when the collapsed navbar expands
)
