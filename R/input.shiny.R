

#' @title input.trip
#' @import dplyr RSQLite shiny svDialogs shinyjs
#' @description Creates GUI for entering bycatch data by trip
#' @export

input.trip <- function(){

######################################################################################################################################
  ##### SETUP AND TABLE CREATION

dlg_message("In the following window, choose the directory where you want your database files to be stored.")
dat.dir <- dlg_dir(filter = dlg_filters["csv",])$res

## set connection
con <- dbConnect(RSQLite::SQLite(), paste0(dat.dir,"/INPUT_DATA.db"))

##check for and create db tables if they don't exist

table_name <- "FISH_INFO"
## look for existing table
query <- paste("SELECT COUNT(*) AS table_count FROM sqlite_master WHERE type='table' AND name='", table_name, "'", sep = "")
result <- dbGetQuery(con, query)
# If the table does not exist, create it
if (result[[1]] == 0) {
  print(paste0("Creating new table called: ",table_name))
  # Define the SQL statement to create the table
  sql_statement <- paste0("
    CREATE TABLE ",table_name," (
    TRAP_ID VARCHAR2(50),
    TRAP_NO VARCHAR2(50),
    FISH_NO VARCHAR2(50),
    SPECCD_ID VARCHAR2(50),
    COMMON VARCHAR2(100),
    FISH_LENGTH VARCHAR2(20),
    SEXCD_ID VARCHAR2(10),
    SHELL VARCHAR2(20),
    CONDITION VARCHAR2(100),
    DISEASE VARCHAR2(20),
    EGG_STAGE VARCHAR2(20),
    CLUTCH VARCHAR2(20),
    VNOTCH VARCHAR2(20),
    KEPT VARCHAR2(20),
    ABUNDANCE VARCHAR2(20),
    CULLS VARCHAR2(20),
    RELEASE_CD VARCHAR2(20)


)")

  # Execute the SQL statement to create table
  dbSendQuery(con, sql_statement)
}


table_name <- "TRAP_INFO"
## look for existing table
query <- paste("SELECT COUNT(*) AS table_count FROM sqlite_master WHERE type='table' AND name='", table_name, "'", sep = "")
result <- dbGetQuery(con, query)
# If the table does not exist, create it
if (result[[1]] == 0) {
  print(paste0("Creating new table called: ",table_name))
  # Define the SQL statement to create the table
  sql_statement <- paste0("
    CREATE TABLE ",table_name," (
    FISHSET_ID VARCHAR2(50),
    SET_NO VARCHAR2(50),
    TRAP_ID VARCHAR2(50),
    TRAP_NO VARCHAR2(50),
    BAIT_CD VARCHAR2(50),
    BAIT_CD2 VARCHAR2(50),
    BAIT_CD3 VARCHAR2(50),
    BAIT_TYPE1 VARCHAR2(10),
    BAIT_TYPE2 VARCHAR2(20),
    BAIT_TYPE3 VARCHAR2(100)

)")

  # Execute the SQL statement to create table
  dbSendQuery(con, sql_statement)
}

table_name <- "SET_INFO"
## look for existing table
query <- paste("SELECT COUNT(*) AS table_count FROM sqlite_master WHERE type='table' AND name='", table_name, "'", sep = "")
result <- dbGetQuery(con, query)
# If the table does not exist, create it
if (result[[1]] == 0) {
  print(paste0("Creating new table called: ",table_name))
  # Define the SQL statement to create the table
  sql_statement <- paste0("
    CREATE TABLE ",table_name," (
    TRIP_ID VARCHAR2(50),
    FISHSET_ID VARCHAR2(50),
    SET_NO VARCHAR2(50),
    SETCD_ID VARCHAR2(50),
    GEAR_ID VARCHAR2(50),
    SPECSCD_ID VARCHAR2(50),
    STRATUM_ID VARCHAR2(50),
    EST_CATCH VARCHAR2(50),
    NUM_TRAPS VARCHAR2(50),
    LATDDMM VARCHAR2(50),
    LONGDDMM VARCHAR2(50),
    DEPTH VARCHAR2(20),
    SOAK_DAYS VARCHAR2(100),
    SOURCE VARCHAR2(100),
    NUM_HOOK VARCHAR2(100),
    TRAP_TYPE VARCHAR2(50),
    VENT_CD VARCHAR2(100),
    NUM_VENTS VARCHAR2(50)

)")

  # Execute the SQL statement to create table
  dbSendQuery(con, sql_statement)
}

  table_name <- "TRIP_INFO"
  ## look for existing table
  query <- paste("SELECT COUNT(*) AS table_count FROM sqlite_master WHERE type='table' AND name='", table_name, "'", sep = "")
  result <- dbGetQuery(con, query)
  # If the table does not exist, create it
  if (result[[1]] == 0) {
    print(paste0("Creating new table called: ",table_name))
    # Define the SQL statement to create the table
    sql_statement <- paste0("
    CREATE TABLE ",table_name," (
    TRIP_ID VARCHAR2(50),
    TRIP VARCHAR2(50),
    OWNER_GROUP VARCHAR2(50),
    VESSEL_NAME VARCHAR2(50),
    VESSEL_NO VARCHAR2(50),
    LICENSE_NO VARCHAR2(50),
    PORT_NAME  VARCHAR2(50),
    BOARD_DATE VARCHAR2(50),
    LANDING_DATE VARCHAR2(50),
    SAMPLER_NAME VARCHAR2(50),
    COMAREA_ID VARCHAR2(50),
    CAPTAIN VARCHAR2(50),
    MARFIS_LICENSE_NO VARCHAR2(50),
    CREATED_BY VARCHAR2(50),
    CREATED_DATE VARCHAR2(50)
)")

  # Execute the SQL statement to create table
  dbSendQuery(con, sql_statement)
}


###########################################################################################
######## SHINY CODE



ui <- fluidPage(

  useShinyjs(), ##necessary for delay functions to work

# reduce general sizing / decrease padding between input fields /manage opacity
  tags$style(HTML("
     body {
      font-size: 95%; /* Shrink everything to 90% size */
      transform: scale(0.99); /* Zoom out */
      transform-origin: top left; /* Set zoom origin */
      padding-left: 10px;
    }
  # .shiny-input-container {
  #   margin-bottom: 0; /* Remove bottom margin between elements */
  #   padding: 2px; /* Reduce padding around elements */
  # }
  .col-sm-1, .col-sm-2, .col-sm-3 {
    padding: 0 5px; /* Reduce horizontal spacing between columns */
  }
       #dynamicRows {
  opacity: 1 !important;
}
 ")),

  ### create special classes of compact and wide rows with custom spacing
  tags$head(
    tags$style(HTML("
   .compact-row {
      display: flex;
      flex-wrap: nowrap; /* keeps all elements in one row */
      gap: 7px; /* Adds spacing between elements */
      margin-left: 0; /* Remove any default margin */
      padding-left: 0;
      margin-left: -10px;
   }
    .compact-row .compact-input {
      flex: 0 0 80px; /* Default width for compact inputs */
      max-width: 80px; /* Ensure they don’t exceed this width */
      box-sizing: border-box; /* Include padding/borders in width */
    }
    .compact-row .wide-input {
      flex: 0 0 200px; /* Fixed width for wide inputs */
      max-width: 200px; /* Ensure it doesn’t exceed this width */
      box-sizing: border-box;
    }
        .compact-row .mediumwide-input {
      flex: 0 0 100px; /* Fixed width for wide inputs */
      max-width: 100px; /* Ensure it doesn’t exceed this width */
      box-sizing: border-box;
    }

     .compact-row label {
      display: block; /* Ensures label is on its own line */
        height: 2.6em; /* Forces two rows for the label */
        line-height: 1.3em; /* Adjusts the line height for the label to fit within two rows */
        font-size: 1.1em; /* (1.1em is slightly larger than default) */
        margin-bottom: 0.2em; /* Reduces bottom margin for labels */
    }
    "))
  ),


## allow buttons to be inline with titles
  tags$style(HTML("
    .title-with-button {
      display: flex;
      align-items: center;
      justify-content: space-between;
      width: 100%;
    }

    .title-with-button .title-panel {
  white-space: nowrap;  /* Prevents title from wrapping */
  margin-right: auto;  /* Pushes buttons to the right */
}

.action-button {
  border: 2px solid #007bff;
  color: #007bff;
}
")),

## disable mouse scrolling of all numeric fields (too easy to accidentally change value)
tags$script(HTML("
  $(document).ready(function() {
    // Function to disable scrolling on numeric inputs
    function disableNumericScroll(target) {
      $(target).on('wheel', function(e) {
        e.preventDefault();
      });
    }

    // Disable scrolling for all current numeric inputs
    disableNumericScroll('input[type=\"number\"]');
    disableNumericScroll('.compact-row input[type=\"number\"]');

    // Monitor for new nodes being inserted into the DOM
    $(document).on('DOMNodeInserted', function(e) {
      // If a numeric input is added, disable scrolling
      if ($(e.target).is('input[type=\"number\"]')) {
        disableNumericScroll(e.target);
      }

      // If a compact row is added, target its numeric inputs
      if ($(e.target).is('.compact-row')) {
        disableNumericScroll($(e.target).find('input[type=\"number\"]'));
      }
    });
  });
")),


## back arrow buttons styling
tags$head(
  tags$style(HTML("
      .arrow-button {
        width: 50px;
        height: 30px;
        border: 2px solid #007bff;
        background-color: white;
        color: #007bff;
        border-radius: 0%;
        text-align: center;
        font-size: 24px;
        line-height: 25px;
        cursor: pointer;
      }
      .arrow-button:hover {
        background-color: #007bff;
        color: white;
      }
      .arrow-left::before {
        content: '\\2190'; /* Unicode for left arrow */
      }
    "))
),
tags$script(HTML("
    $(document).on('click', '.arrow-button', function() {
      var id = $(this).attr('id');
      Shiny.setInputValue(id, Math.random());
    });
")),



fluidRow(
  div(
    class = "title-with-button",
    titlePanel("TRIP INFO"),
    actionButton("submit.trip", "Finished, Submit Trip!")
  )
  ),

  fluidRow(
    column(2, dateInput("board.date", "BOARDING DATE",value = NA)),
    column(2, dateInput("land.date", "LANDING DATE",value = NA)),
    column(2, numericInput("vessel.num", "VESSEL REG #", value = NA, min = 0)),
    column(2, textInput("vessel.name", "VESSEL NAME"))

  ),
  fluidRow(
    column(1, numericInput("license.num", "LICENSE #", value = NA)),
    column(2, textInput("captain.name", "CAPTAIN NAME")),
    column(2, textInput("sampler.name", "SAMPLER NAME")),
    column(1, selectInput("lfa", "LFA:",choices = c("","L27", "L28","L29","L30","L31A","L31B","L32","L33","L34","L35","L36","L37","L38","L38B","L41")))
  ),
  fluidRow(
    column(2, textInput("entry.group", "DATA ENTRY GROUP")),
    column(2, textInput("entry.name", "DATA ENTRY NAME")),
    column(2, dateInput("entry.date", "DATA ENTRY DATE")),
    column(2, textInput("trip.code", "TRIP"))
  ),

  ################################################## SET INFO
fluidRow(
  div(
    class = "title-with-button",
    div(class = "title-panel", titlePanel("SET INFO")),  # Wrap the title in title-panel div
    column(12, align = "right",  ### arrow button
           tags$div(
             class = "arrow-button arrow-left",
             id = "set.back.btn"
           )
    ),
    actionButton("next.set", "Next Set")
  )
),


div(class = "compact-row",
    div(class= "compact-input", numericInput("set.num", "TRAWL / STRING#",value = NA, min = -1)),
    div(class= "compact-input", numericInput("num.traps", "#TRAPS IN SET",value = NA, min = 0)),
    div(class = "mediumwide-input", numericInput("lat", "LATITUDE (DDMM.MM)", value = NULL, max = 9059.99, min = -9059.99, step = 0.01)),
    div(class = "mediumwide-input", numericInput("lon", "LONGITUDE (DDMM.MM)", value = NULL, max = 18059.99, min = -18059.99, step = 0.01)),
    div(class= "compact-input", numericInput("grid.num", "GRID NO", value = NULL)),
    div(class= "compact-input", numericInput("depth", "DEPTH (FM)", value = NULL, min = 0)),
    div(class= "compact-input", numericInput("soak.days", "SOAK DAYS", value = NULL, min = 0)),
    div(class= "compact-input", textInput("trap.type", "TRAP TYPE", value = NULL)),
    div(class= "compact-input", numericInput("vent.size", "VENT SIZE (CODE)", value = NULL)),
    div(class= "compact-input", numericInput("num.vents", "# OF VENTS", value = NULL, min = 0))
  ),

 fluidRow(),

fluidRow(
  div(
    class = "title-with-button",
    div(class = "title-panel", titlePanel("TRAP INFO")),  # Wrap the title in title-panel div
    column(12, align = "right",  ### arrow button
           tags$div(
             class = "arrow-button arrow-left",
             id = "trap.back.btn"
           )
    ),
    actionButton("next.trap", "Next Trap")
  )
),

fluidRow(
  column(1, numericInput("trap.num", "TRAP NO",value = NA, min = 0)),
  column(1, textInput("bait.code", "BAIT CD",value = "")),
  column(1, textInput("bait.code2", "BAIT CD2",value = "")),
  column(1, textInput("bait.code3", "BAIT CD3",value = "")),
  column(2, textInput("bait.type1", "BAIT TYPE1",value = "")),
  column(2, textInput("bait.type2", "BAIT TYPE2",value = "")),
  column(2, textInput("bait.type3", "BAIT TYPE3",value = ""))
  ),

fluidRow(     ### use button formatted title class for FISH row just for easy formatting consistency
  div(
    class = "title-with-button",
    div(class = "title-panel", titlePanel("FISH INFO")))
  ),
## dynamically duplicating fish info row:
fluidRow(
  column(12, uiOutput("dynamicRows")) # Placeholder for dynamically generated rows
)


) ## End of UI



####################################################################################################################################
  ###SERVER CODE


server <- function(input, output, session) {

  ## bring in species code list so it only has to be uploaded once
  spec.tab <- readRDS(paste0(system.file("data", package = "Bycatch"),"/SPECIESCODES.rds"))

####################################################################################################################################
  ### DEFINE DATABASE UPDATING FUNCTIONS (FOR WHEN 'NEXT' BUTTONS ARE CLICKED)
  ##FISH
  update.fish <- function(db=NULL, trap.id = NULL){

  fish_columns <- dbListFields(db, "FISH_INFO")
  # Loop through each row and collect data for insertion
  data <- NULL
  for (row_id in row_ids()) {

    fish_num <- gsub("\\D", "", row_id)

    data.row <- data.frame(
      trap.id = trap.id,
      trap_num = input[[paste0("trap.num_", row_id)]],
      fish_num = fish_num,
      species_code = input[[paste0("spec.code_", row_id)]],
      common = input[[paste0("common_", row_id)]],
      length = input[[paste0("length_", row_id)]],
      sex = input[[paste0("sex_", row_id)]],
      shell = input[[paste0("shell_", row_id)]],
      condition = input[[paste0("cond_", row_id)]],
      disease = input[[paste0("disease_", row_id)]],
      egg_stage = input[[paste0("egg_", row_id)]],
      clutch_percent = input[[paste0("clutch_", row_id)]],
      vnotch = input[[paste0("vnotch_", row_id)]],
      kept = input[[paste0("kept_", row_id)]],
      abundance = input[[paste0("abund_", row_id)]],
      cull = input[[paste0("cull_", row_id)]],
      release = NA
    )
    data <- rbind(data,data.row)
  }
  data <- data %>% filter(!species_code %in% NA) ### remove last unfilled row where species code wasn't added
    # Insert data into the database (upload if no existing fish, update if fish found)
    checkfish <- paste("SELECT * FROM FISH_INFO WHERE TRAP_ID = '",trap.id, "'", sep = "")
    fish.result <- dbGetQuery(db, checkfish)
    fish.result <- fish.result %>% arrange(FISH_NO) ## make sure fish data is sorted by fish number (because this is equivalent to row# in the app)
    if(nrow(fish.result)==0){
    data[data == ""] <- NA
    colnames(data) = fish_columns
    print("no fish data found. Adding:")
    print(data)
    if(nrow(data)>0){
      dbWriteTable(db, "FISH_INFO", data, append = TRUE, row.names = FALSE)
    }
    }

    if(nrow(fish.result)>0){
      print("fish data found. Replacement:")
      print(data)
      data[data == ""] <- NA
      if(nrow(data)>0){
        for(i in 1:nrow(fish.result)){
          update_query <- paste("
    UPDATE FISH_INFO
    SET SPECCD_ID = '", data$species_code[i], "',
        COMMON = '", data$common[i], "',
        FISH_LENGTH = '", data$length[i], "',
        SEXCD_ID = '", data$sex[i], "',
        SHELL = '", data$shell[i], "',
        CONDITION = '", data$condition[i], "',
        DISEASE = '", data$disease[i], "',
        EGG_STAGE = '", data$egg_stage[i], "',
        CLUTCH = '", data$clutch_percent[i], "',
        VNOTCH = '", data$vnotch[i], "',
        KEPT = '", data$kept[i], "',
        ABUNDANCE = '", data$abundance[i], "',
        CULLS = '", data$cull[i], "',
        RELEASE_CD = '", data$release[i], "'
    WHERE FISH_NO = '", i, "' AND TRAP_ID = '",trap.id,"'", sep = "")

          dbExecute(db, update_query)
        }
        ## then need to add any new additional rows of data that aren't in the database
        data.add <- data %>% filter(fish_num > nrow(fish.result))
        if(nrow(data.add)>0){
          colnames(data.add) = fish_columns
          dbWriteTable(db, "FISH_INFO", data.add, append = TRUE, row.names = FALSE)
        }

      } ## replacement rows >0
      } ## fish data found

  }

  ### TRAP (Insert if the trap hasn't been created yet)
  update.trap <- function(db=NULL, set.id = NULL, trap.id = NULL){
    checktrap <- paste("SELECT * FROM TRAP_INFO WHERE TRAP_ID = '",trap.id, "'", sep = "")
    trap.result <- dbGetQuery(db, checktrap)
    if(nrow(trap.result)==0){
      trap_columns <- dbListFields(db, "TRAP_INFO")
      t.dat <- data.frame(
        set.id,
        input$set.num,
        trap.id,
        input$trap.num,
        input$bait.code,
        input$bait.code2,
        input$bait.code3,
        input$bait.type1,
        input$bait.type2,
        input$bait.type3
      )
      colnames(t.dat) = trap_columns
      dbWriteTable(db, "TRAP_INFO", t.dat, append = TRUE, row.names = FALSE)
    }

    ## if the trap exists, update all values if the user has made changes to its information
    if(nrow(trap.result)>0){
      update_query <- paste("
    UPDATE TRAP_INFO
    SET BAIT_CD = '", input$bait.code, "',
        BAIT_CD2 = '", input$bait.code2, "',
        BAIT_CD3 = '", input$bait.code3, "',
        BAIT_TYPE1 = '", input$bait.type1, "',
        BAIT_TYPE2 = '", input$bait.type2, "',
        BAIT_TYPE3 = '", input$bait.type3, "'
    WHERE TRAP_ID = '", trap.id, "'", sep = "")

      dbExecute(db, update_query)
    }
  }

  ### SET (Insert if the set hasn't been created yet)
  update.set <- function(db=NULL, trip.id = NULL, set.id = NULL){
    checkset <- paste("SELECT * FROM SET_INFO WHERE FISHSET_ID = '",set.id, "'", sep = "")
    set.result <- dbGetQuery(db, checkset)
    if(nrow(set.result)==0){
      set.dat <- data.frame(
        trip.id,
        set.id,
        input$set.num,
        set.code.id = NA,
        gear.id = NA,
        spec.code = 2550,
        input$grid.num,
        est.catch = NA,
        input$num.traps,
        input$lat,
        input$lon,
        input$depth,
        input$soak.days,
        source = NA,
        num.hooks = NA,
        input$trap.type,
        input$vent.size,
        input$num.vents
      )
      set_columns <- dbListFields(db, "SET_INFO")
      colnames(set.dat) = set_columns
      dbWriteTable(db, "SET_INFO", set.dat, append = TRUE, row.names = FALSE)
    }

    ## if the set exists, update all values if the user has made changes to its information
    if(nrow(set.result)>0){
      update_query <- paste("
    UPDATE SET_INFO
    SET STRATUM_ID = '", input$grid.num, "',
        NUM_TRAPS = '", input$num.traps, "',
        LATDDMM = '", input$lat, "',
        LONGDDMM = '", input$lon, "',
        DEPTH = '", input$depth, "',
        SOAK_DAYS = '", input$soak.days, "',
        TRAP_TYPE = '", input$trap.type, "',
        VENT_CD = '", input$vent.size, "',
        NUM_VENTS = '", input$num.vents, "'
    WHERE FISHSET_ID = '", set.id, "'", sep = "")

      dbExecute(db, update_query)
    }
  }

  ### TRIP
  update.trip <- function(db=NULL, trip.id = NULL){
    checktrip <- paste("SELECT * FROM TRIP_INFO WHERE TRIP_ID = '",trip.id, "'", sep = "")
    trip.result <- dbGetQuery(db, checktrip)
    if(nrow(trip.result)==0){
      trip.dat <- data.frame(
        trip.id,
        input$trip.code,
        input$entry.group,
        input$vessel.name,
        input$vessel.num,
        input$license.num,
        port = NA,
        input$board.date,
        input$land.date,
        input$sampler.name,
        input$lfa,
        input$captain.name,
        marfis.lic = NA,
        input$entry.name,
        input$entry.date

      )
      trip_columns <- dbListFields(db, "TRIP_INFO")
      colnames(trip.dat) = trip_columns
      dbWriteTable(db, "TRIP_INFO", trip.dat, append = TRUE, row.names = FALSE)
    }

    if(nrow(trip.result)>0){
      update_query <- paste("
    UPDATE TRIP_INFO
    SET OWNER_GROUP = '", input$entry.group, "',
        VESSEL_NAME = '", input$vessel.name, "',
        VESSEL_NO = '", input$vessel.num, "',
        LICENSE_NO = '", input$license.num, "',
        BOARD_DATE = '", input$board.date, "',
        LANDING_DATE = '", input$land.date, "',
        SAMPLER_NAME = '", input$sampler.name, "',
        COMAREA_ID = '", input$lfa, "',
        CAPTAIN = '", input$captain.name, "',
        CREATED_BY = '", input$entry.name, "',
        CREATED_DATE = '", input$entry.date, "'
    WHERE TRIP_ID = '", trip.id, "'", sep = "")

      dbExecute(db, update_query)
    }

  }

  ###### END OF DB UPDATING FUNCTIONS

  ###########################################################################################################################################
  ### AUTOFILLS and INITIAL CONDITIONS


  ## set reactive placeholder for trip.ID
  trip.id <- reactiveVal(NULL)


  ## create Trip code when enough info is entered
  observeEvent(
    list(input$vessel.num, input$board.date), {
      # This block will run only when both input$vessel.num and input$board.date are not NULL
      req(input$vessel.num, input$board.date)
        board.date <- format(input$board.date, "%d%m%y")
        updateTextInput(session, "trip.code", value = paste0(input$vessel.num,"-",board.date))

        ## set relational column
        TRIP.ID <- paste0(input$vessel.num,"_",board.date)
        trip.id(TRIP.ID)
  })


  #### for reactively adding fish Info rows when species code is entered
  # Reactive value to track row IDs
  row_ids <- reactiveVal(c("row_1"))

  # Reactive values to store row data
  row_data <- reactiveValues(data = list())


  # Template for a fish row
  create_row <- function(row_id) {
    div(id = paste0("row_container_", row_id), class = "compact-row",
        div(class = "compact-input",
            numericInput(paste0("trap.num_", row_id), "TRAP NO", value = NA, min = 0),
            style = "pointer-events: none; opacity: 0.5;"
        ),
        div(class = "compact-input",
            numericInput(paste0("spec.code_", row_id), "SPECIES CODE", value = NA, min = 0)
        ),
        div(class = "wide-input",
            textInput(paste0("common_", row_id), "COMMON", value = "")
        ),
        div(class = "compact-input",
            numericInput(paste0("length_", row_id), "LENGTH", value = NA, min = 0)
        ),
        div(class = "compact-input",
            numericInput(paste0("sex_", row_id), "SEX", min = 1, max = 3, value = NA)
        ),
        div(class = "compact-input",
            numericInput(paste0("shell_", row_id), "SHELL HARD", min = 1, max = 7, value = NA)
        ),
        div(class = "compact-input",
            numericInput(paste0("cond_", row_id), "CONDITION", min =0, max = 7,value = NA)
        ),
        div(class = "compact-input",
            numericInput(paste0("disease_", row_id), "SHELL DISEASE", value = NA, min = 0)
        ),
        div(class = "compact-input",
            numericInput(paste0("egg_", row_id), "EGG STAGE", min = 1, max = 4, value = NA)
        ),
        div(class = "compact-input",
            numericInput(paste0("clutch_", row_id), "CLUTCH %", value = NA, min = 0, max = 100)
        ),
        div(class = "compact-input",
            numericInput(paste0("vnotch_", row_id), "VNOTCH", min = 0, max = 5, value = NA)
        ),
        div(class = "compact-input",
            numericInput(paste0("kept_", row_id), "KEPT", min = 0, max = 1, value = NA)
        ),
        div(class = "compact-input",
            numericInput(paste0("abund_", row_id), "ABUNDANCE", value = NA, min = 0)
        ),
        div(class = "compact-input",
            numericInput(paste0("cull_", row_id), "CULL", min = 1, max = 3, value = NA)
        )
    )
  }

  # Add initial fish row to UI
  observe({
    insertUI(selector = "#dynamicRows", where = "beforeEnd", ui = create_row("row_1"))
  })

  # Update each row's trap.num field with the input$trap.num value
  observe({
    req(input$trap.num) # Ensure trap.num has a value
    delay(10, {  ## delay ensures that row_ids() has enough time to update after new action is taken (sometimes this can lag and rows get missed)
      current_rows <- row_ids()
      lapply(current_rows, function(row_id) {
        updateNumericInput(session, paste0("trap.num_", row_id), value = input$trap.num)
      })
    })
  })

  # Observe the species code of the last row to add a new row
    observeEvent(input[[paste0("spec.code_", tail(row_ids(), 1))]], {
      last_id <- tail(row_ids(), 1)
      if (!is.null(input[[paste0("spec.code_", last_id)]]) &
          !is.na(input[[paste0("spec.code_", last_id)]])) {
        new_id <- paste0("row_", length(row_ids()) + 1)
        row_ids(c(row_ids(), new_id))
        insertUI(selector = "#dynamicRows", where = "beforeEnd", ui = create_row(new_id))
      }
  })


  ## also continually observe spec code input to auto-fill common name
  observe({
    current_rows <- row_ids()
    # Create an observer for each spec.code field
    lapply(current_rows, function(row_id) {
      observeEvent(input[[paste0("spec.code_", row_id)]], {
        # Get the updated value
        new_value <- input[[paste0("spec.code_", row_id)]]
        ### reference species list to auto fill common name
        common <- spec.tab$COMMON[which(spec.tab$SPECIES_CODE %in% new_value)]
        updateTextInput(session, paste0("common_", row_id), value = common)

      }, ignoreInit = TRUE)  # Avoid triggering on initialization
    })
  })


  ## Autofill trap and fish data based on current trap number selection (this is also triggered when NEXT or BACK buttons pressed)
  observeEvent(input$trap.num, {

    print(input$trap.num)
    ## define IDs for relational columns
    trip.id <- trip.id()
    set.id <- NULL
    trap.id <- NULL
    if(!is.null(trip.id)){set.id <- paste0(trip.id(),"_",input$set.num)}
    if(!is.null(set.id)){trap.id <- paste0(trip.id(),"_",input$set.num,"_",input$trap.num)}

    if(!is.null(trip.id) & !is.null(set.id) & !is.null(trap.id)){
      # check database for existing trap
      db <- dbConnect(RSQLite::SQLite(), paste0(dat.dir,"/INPUT_DATA.db"))
      new.trap <- trap.id
      checktrap <- paste("SELECT * FROM TRAP_INFO WHERE TRAP_ID = '",new.trap, "'", sep = "")
      trap.result <- dbGetQuery(db, checktrap)
      dbDisconnect(db)

      print(new.trap)
      if(nrow(trap.result)==0){ ### if there's no pre-existing data for the new trap selection, clear all fields
        # Clear input fields for Trap and Fish INFO
        updateNumericInput(session, "bait.code", value = "")
        updateNumericInput(session, "bait.code2", value = "")
        updateNumericInput(session, "bait.code3", value = "")
        updateNumericInput(session, "bait.type1", value = "")
        updateNumericInput(session, "bait.type2", value = "")
        updateNumericInput(session, "bait.type3", value = "")

        for (row_id in row_ids()) {
          updateNumericInput(session, paste0("trap.num_", row_id), value = input$trap.num)
          updateNumericInput(session, paste0("spec.code_", row_id), value = NA)
          updateTextInput(session, paste0("common_", row_id), value = "")
          updateNumericInput(session, paste0("length_", row_id), value = NA)
          updateNumericInput(session, paste0("sex_", row_id), value = NA)
          updateNumericInput(session, paste0("shell_", row_id), value = NA)
          updateNumericInput(session, paste0("cond_", row_id), value = NA)
          updateNumericInput(session, paste0("disease_", row_id), value = NA)
          updateNumericInput(session, paste0("egg_", row_id), value = NA)
          updateNumericInput(session, paste0("clutch_",row_id), value = NA)
          updateNumericInput(session, paste0("vnotch_",row_id), value = NA)
          updateNumericInput(session, paste0("kept_",row_id), value = NA)
          updateNumericInput(session, paste0("abund_",row_id), value = NA)
          updateNumericInput(session, paste0("cull_",row_id), value = NA)
        }
      }else{
      ### if there is pre-existing data for trap selection:
        ## fill data for selected trap
        updateNumericInput(session, "bait.code", value = trap.result$BAIT_CD[1])
        updateNumericInput(session, "bait.code2", value = trap.result$BAIT_CD2[1])
        updateNumericInput(session, "bait.code3", value = trap.result$BAIT_CD3[1])
        updateNumericInput(session, "bait.type1", value = trap.result$BAIT_TYPE1[1])
        updateNumericInput(session, "bait.type2", value = trap.result$BAIT_TYPE2[1])
        updateNumericInput(session, "bait.type3", value = trap.result$BAIT_TYPE3[1])

          ## for fish, match GUI row number to existing fish rows in db, then update with the existing fish
          db <- dbConnect(RSQLite::SQLite(), paste0(dat.dir,"/INPUT_DATA.db"))
          existing.fish <-  paste("SELECT * FROM FISH_INFO WHERE TRAP_ID = '",new.trap, "'", sep = "")
          fish.result <- dbGetQuery(db, existing.fish)
          dbDisconnect(db)
          fish.result <- fish.result %>% arrange(FISH_NO)  ## ensures that fish rows are sorted by fish number, so these can be treated equivalent to GUI row number

          ## Determine number of rows needed
          num_fish_rows <- nrow(fish.result)
          current_rows <- row_ids()
          num_current_rows <- length(current_rows)

          ## Add more rows if needed
          if (num_current_rows < num_fish_rows) {
            rows_needed <- num_fish_rows - num_current_rows
            for (i in seq_len(rows_needed)) {
              new_row_id <- paste0("row_", num_current_rows + i)
              row_ids(c(row_ids(), new_row_id))
              insertUI(selector = "#dynamicRows", where = "beforeEnd", ui = create_row(new_row_id))
            }
          }

          ## Clear row data from previous trap
          lapply(row_ids(), function(row_id) {
            updateNumericInput(session, paste0("trap.num_", row_id), value = NA)
            updateNumericInput(session, paste0("spec.code_", row_id), value = NA)
            updateTextInput(session, paste0("common_", row_id), value = "")
            updateNumericInput(session, paste0("length_", row_id), value = NA)
            updateNumericInput(session, paste0("sex_", row_id), value = NA)
            updateNumericInput(session, paste0("shell_", row_id), value = NA)
            updateNumericInput(session, paste0("cond_", row_id), value = NA)
            updateNumericInput(session, paste0("disease_", row_id), value = NA)
            updateNumericInput(session, paste0("egg_", row_id), value = NA)
            updateNumericInput(session, paste0("clutch_", row_id), value = NA)
            updateNumericInput(session, paste0("vnotch_", row_id), value = NA)
            updateNumericInput(session, paste0("kept_", row_id), value = NA)
            updateNumericInput(session, paste0("abund_", row_id), value = NA)
            updateNumericInput(session, paste0("cull_", row_id), value = NA)
          })


          ## fill rows with new data
          delay(10,{    ## delay is needed for the new row_ids to properly update with the correct number before we try to fill them
            row.ids.vect <- row_ids()
            for (i in 1:nrow(fish.result)) {
              new.row <- row.ids.vect[i]
              updateNumericInput(session, paste0("trap.num_", new.row), value = fish.result$TRAP_NO[i])
              updateNumericInput(session, paste0("spec.code_", new.row), value = fish.result$SPECCD_ID[i])
              updateTextInput(session, paste0("common_", new.row), value = fish.result$COMMON[i])
              updateNumericInput(session, paste0("length_", new.row), value = fish.result$FISH_LENGTH[i])
              updateNumericInput(session, paste0("sex_", new.row), value = fish.result$SEXCD_ID[i])
              updateNumericInput(session, paste0("shell_", new.row), value = fish.result$SHELL[i])
              updateNumericInput(session, paste0("cond_", new.row), value = fish.result$CONDITION[i])
              updateNumericInput(session, paste0("disease_", new.row), value = fish.result$DISEASE[i])
              updateNumericInput(session, paste0("egg_", new.row), value = fish.result$EGG_STAGE[i])
              updateNumericInput(session, paste0("clutch_", new.row), value = fish.result$CLUTCH[i])
              updateNumericInput(session, paste0("vnotch_", new.row), value = fish.result$VNOTCH[i])
              updateNumericInput(session, paste0("kept_", new.row), value = fish.result$KEPT[i])
              updateNumericInput(session, paste0("abund_", new.row), value = fish.result$ABUNDANCE[i])
              updateNumericInput(session, paste0("cull_", new.row), value = fish.result$CULLS[i])
            }
          })
      }
    }

  }, ignoreInit = TRUE)



####################################################################################################################################################
##BEGINNING OF INTERACTIVE SERVER CODE (BUTTON CLICKS)
  ## SUBMIT LEVEL 1
  # When "next.trap" button is clicked
  observeEvent(input$next.trap, {

    ## define IDs for relational columns
    set.id <- paste0(trip.id(),"_",input$set.num)
    trap.id <- paste0(trip.id(),"_",input$set.num,"_",input$trap.num)
    trip.id <- trip.id()

    # Initialize database connection
    db <- dbConnect(RSQLite::SQLite(), paste0(dat.dir,"/INPUT_DATA.db"))

    ## Update downstream data
    update.trap(db,set.id = set.id, trap.id = trap.id)
    update.fish(db, trap.id = trap.id)

    ## update upstream data
    update.set(db, trip.id = trip.id, set.id = set.id)
    update.trip(db, trip.id)

    dbDisconnect(db)

    ## move to next trap number
    updateNumericInput(session, "trap.num", value = input$trap.num+1)


  }) ## observeEvent block


  ## When back button for trap clicked  (inverse of next trap) ## going backwards doesn't update database, just check for existing data to fill fields
  observeEvent(input$trap.back.btn, {

    if(!is.na(input$trap.num) & input$trap.num>0){
      updateNumericInput(session, "trap.num", value = input$trap.num-1)
    }
    # if(is.na(input$trap.num)){
    #   trap.max <- paste("SELECT MAX(TRAP_NO) AS MAX_TRAP_NO FROM TRAP_INFO WHERE FISHSET_ID = ","'",set.id,"'", sep = "")
    #   max.trap <- dbGetQuery(db, trap.max)
    #   new.trap <- paste0(set.id,"_", as.numeric(max.trap))
    # }

  }) ## observeEvent block


  ## when "next.set" is clicked
  observeEvent(input$next.set, {

    if(!is.na(input$set.num)){
    ## define IDs for relational columns
    set.id <- paste0(trip.id(),"_",input$set.num)
    trap.id <- paste0(trip.id(),"_",input$set.num,"_",input$trap.num)
    trip.id <- trip.id()

    # Initialize database connection
    db <- dbConnect(RSQLite::SQLite(), paste0(dat.dir,"/INPUT_DATA.db"))

    ## Check and update set and trip information again (in case user has made changes to set or no traps were entered for the set)
    update.set(db, trip.id = trip.id, set.id = set.id)
    update.trip(db, trip.id)
    if(!is.na(input$trap.num)){
      update.trap(db, set.id = set.id, trap.id = trap.id)
    }

    ### last step for click, check db for existing data at next set number and clear fields if there's none
    new.set <- paste0(trip.id(),"_",input$set.num+1)
    checkset <- paste("SELECT * FROM SET_INFO WHERE FISHSET_ID = '",new.set, "'", sep = "")
    set.result <- dbGetQuery(db, checkset)
    if(nrow(set.result)==0){
      # Clear input fields for SET INFO
      updateNumericInput(session, "set.num", value = NA)
      updateNumericInput(session, "num.traps", value = NA)
      updateNumericInput(session, "lat", value = NA)
      updateNumericInput(session, "lon", value = NA)
      updateNumericInput(session, "grid.num", value = NA)
      updateNumericInput(session, "depth", value = NA)
      updateNumericInput(session, "soak.days", value = NA)
      updateNumericInput(session, "vent.size", value = NA)
      updateNumericInput(session, "num.vents", value = NA)
      updateTextInput(session, "trap.type", value = "")
    }else{
      ## if there is data for current set, fill fields
      updateNumericInput(session, "set.num", value = set.result$SET_NO)
      updateNumericInput(session, "num.traps", value = set.result$NUM_TRAPS)
      updateNumericInput(session, "lat", value = set.result$LATDDMM)
      updateNumericInput(session, "lon", value = set.result$LONGDDMM)
      updateNumericInput(session, "grid.num", value = set.result$STRATUM_ID)
      updateNumericInput(session, "depth", value = set.result$DEPTH)
      updateNumericInput(session, "soak.days", value = set.result$SOAK_DAYS)
      updateNumericInput(session, "vent.size", value = set.result$VENT_CD)
      updateNumericInput(session, "num.vents", value = set.result$NUM_VENTS)
      updateTextInput(session, "trap.type", value = set.result$TRAP_TYPE)

    }

    # Close the database connection
    dbDisconnect(db)
    print("database updated")
    }

  })

  ## back button for set clicked (inverse operation of next set button) ## Going backwards does not update database
  observeEvent(input$set.back.btn, {

    # Initialize database connection
    db <- dbConnect(RSQLite::SQLite(), paste0(dat.dir,"/INPUT_DATA.db"))

    if(!is.na(input$set.num) & input$set.num>0){
      new.set <- paste0(trip.id(),"_",input$set.num-1)
    }
    if(is.na(input$set.num)){
      set.max <- paste("SELECT MAX(SET_NO) AS MAX_SET_NO FROM SET_INFO WHERE TRIP_ID = '",trip.id(), "'", sep = "")
      max.set <- dbGetQuery(db, set.max)
      new.set <- paste0(trip.id(),"_", as.numeric(max.set))
    }
    checkset <- paste("SELECT * FROM SET_INFO WHERE FISHSET_ID = '",new.set, "'", sep = "")
    set.result <- dbGetQuery(db, checkset)
    if(nrow(set.result)==0){
      # Clear input fields for SET INFO
      updateNumericInput(session, "set.num", value = NA)
      updateNumericInput(session, "num.traps", value = NA)
      updateNumericInput(session, "lat", value = NA)
      updateNumericInput(session, "lon", value = NA)
      updateNumericInput(session, "grid.num", value = NA)
      updateNumericInput(session, "depth", value = NA)
      updateNumericInput(session, "soak.days", value = NA)
      updateNumericInput(session, "vent.size", value = NA)
      updateNumericInput(session, "num.vents", value = NA)
      updateTextInput(session, "trap.type", value = "")
    }else{
      ## if there is data for current set, fill fields
      updateNumericInput(session, "set.num", value = set.result$SET_NO)
      updateNumericInput(session, "num.traps", value = set.result$NUM_TRAPS)
      updateNumericInput(session, "lat", value = set.result$LATDDMM)
      updateNumericInput(session, "lon", value = set.result$LONGDDMM)
      updateNumericInput(session, "grid.num", value = set.result$STRATUM_ID)
      updateNumericInput(session, "depth", value = set.result$DEPTH)
      updateNumericInput(session, "soak.days", value = set.result$SOAK_DAYS)
      updateNumericInput(session, "vent.size", value = set.result$VENT_CD)
      updateNumericInput(session, "num.vents", value = set.result$NUM_VENTS)
      updateTextInput(session, "trap.type", value = set.result$TRAP_TYPE)
    }

    # Close the database connection
    dbDisconnect(db)
    print("database updated")


  })


  ### When Trip is submitted
  observeEvent(input$submit.trip, {

    trip.id <- trip.id()
    # Initialize database connection
    db <- dbConnect(RSQLite::SQLite(), paste0(dat.dir,"/INPUT_DATA.db"))

    ##update all downstream data
    update.trip(db, trip.id)
    update.set(db, trip.id = trip.id, set.id = set.id)
    update.trap(db, set.id = set.id, trap.id = trap.id)

    # Close the database connection
    dbDisconnect(db)
    print("database updated")

    # Clear input fields for TRIP INFO
    updateTextInput(session, "trip.code", value = "")
    updateTextInput(session, "entry.group", value = "")
    updateTextInput(session, "vessel.name", value = "")
    updateTextInput(session, "vessel.name", value = "")
    updateNumericInput(session, "vessel.num", value = NA)
    updateNumericInput(session, "license.num", value = NA)
    updateDateInput(session, "board.date", value = NA)
    updateDateInput(session, "land.date", value = NA)
    updateTextInput(session, "sampler.name", value = "")
    updateTextInput(session, "lfa", value = "")
    updateTextInput(session, "captain.name", value = "")
    updateTextInput(session, "entry.name", value = "")
    updateDateInput(session, "entry.date", value = Sys.Date())


  })





} ## Server code



shinyApp(ui, server)

############### SCRAP

### make special class of numeric input that can't be scrolled at all
# tags$head(
#   tags$style(HTML("
#     .no-spinner input[type='number']::-webkit-outer-spin-button,
#     .no-spinner input[type='number']::-webkit-inner-spin-button {
#       -webkit-appearance: none;
#       margin: 0;
#     }
#     .no-spinner input[type='number'] {
#       -moz-appearance: textfield;
#     }
#   ")),
#   tags$script(HTML("
#     document.addEventListener('DOMContentLoaded', function() {
#       document.querySelectorAll('.no-spinner input[type=\"number\"]').forEach(function(el) {
#         // Prevent scrolling with the mouse wheel
#         el.addEventListener('wheel', function(e) { e.preventDefault(); });
#         // Prevent increment/decrement with keyboard arrow keys
#         el.addEventListener('keydown', function(e) {
#           if (e.key === 'ArrowUp' || e.key === 'ArrowDown') {
#             e.preventDefault();
#           }
#         });
#       });
#     });
#   "))
# ),

# library(shiny)
# #library(shinyjs)
# library(svDialogs)
# library(RSQLite)
# library(dplyr)


}
