

#' @title input.trip
#' @import dplyr RSQLite shiny svDialogs
#' @description Creates GUI for entering bycatch data by trip
#' @export

input.trip <- function(){

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

# reduce general sizing / decrease padding between input fields /manage opacity
  tags$style(HTML("
     body {
      font-size: 95%; /* Shrink everything to 90% size */
      transform: scale(0.99); /* Zoom out */
      transform-origin: top left; /* Set zoom origin */
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


fluidRow(
  div(
    class = "title-with-button",
    titlePanel("TRIP INFO"),
    actionButton("submit.trip", "Finished, Submit Trip!")
  )
  ),

  fluidRow(
    column(1, dateInput("board.date", "BOARDING DATE",value = NA)),
    column(1, dateInput("land.date", "LANDING DATE",value = NA)),
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
    column(1, dateInput("entry.date", "DATA ENTRY DATE")),
    column(1, textInput("trip.code", "TRIP"))
  ),

  ################################################## SET INFO
fluidRow(
  div(
    class = "title-with-button",
    titlePanel("SET INFO"),
    actionButton("next.set", "Next Set")
  )
),

div(class = "compact-row",
    div(class= "compact-input", numericInput("set.num", "TRAWL / STRING#",value = NA, min = 0)),
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
    titlePanel("TRAP INFO"),
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

titlePanel("FISH INFO"),
## dynamically duplicating fish info row:
fluidRow(
  column(12, uiOutput("dynamicRows")) # Placeholder for dynamically generated rows
)


)


server <- function(input, output, session) {

  ##set relational columns
  trip.id = 1
  set.id = 0
  trap.id = 0


  ## create Trip code when enough info is entered
  observeEvent(
    list(input$vessel.num, input$board.date), {
      # This block will run only when both input$vessel.num and input$board.date are not NULL
      req(input$vessel.num, input$board.date)
        board.date <- format(input$board.date, "%d%m%y")
        updateTextInput(session, "trip.code", value = paste0(input$vessel.num,"-",board.date))
  })


  #### for reactively adding fish Info rows when species code is entered
  # Reactive value to track row IDs
  row_ids <- reactiveVal(c("row_1"))

  # Reactive values to store row data
  row_data <- reactiveValues(data = list())

  # Template for a row
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

  observe({
    req(input$trap.num) # Ensure trap.num has a value
    # Get current row IDs
    current_rows <- row_ids()
    # Update each row's trap.num field with the input$trap.num value
    lapply(current_rows, function(row_id) {
      updateNumericInput(session, paste0("trap.num_", row_id), value = input$trap.num)
    })
  })


  # Add initial row to UI
  observe({
    insertUI(selector = "#dynamicRows", where = "beforeEnd", ui = create_row("row_1"))
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


  # When "next.trap" button is clicked
  observeEvent(input$next.trap, {

    ## define IDs for relational columns
    trap.id = trap.id+1
    set.id = input$set.num

    # Initialize database connection
    db <- dbConnect(RSQLite::SQLite(), paste0(dat.dir,"/INPUT_DATA.db"))

    ##Fish Data
    fish_columns <- dbListFields(db, "FISH_INFO")
    # Loop through each row and collect data for insertion
    for (row_id in row_ids()) {
      data <- data.frame(
        trap.id = trap.id,
        trap_num = input[[paste0("trap.num_", row_id)]],
        fish_num = row_id,
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

      # Insert data into the database
      data[data == ""] <- NA
      data <- data %>% filter(!species_code %in% NA) ### remove last unfilled row where species code wasn't added
      colnames(data) = fish_columns
      dbWriteTable(db, "FISH_INFO", data, append = TRUE, row.names = FALSE)
    }

    ## TRAP Data
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

    # Close the database connection
    dbDisconnect(db)
    print("database updated")


    # Clear input fields for Trap and Fish INFO
    updateNumericInput(session, "trap.num", value = NA)
    updateNumericInput(session, "bait.code", value = "")
    updateNumericInput(session, "bait.code2", value = "")
    updateNumericInput(session, "bait.code3", value = "")
    updateNumericInput(session, "bait.type1", value = "")
    updateNumericInput(session, "bait.type2", value = "")
    updateNumericInput(session, "bait.type3", value = "")

    for (row_id in row_ids()) {
    updateNumericInput(session, paste0("trap.num_", row_id), value = NA)
    updateNumericInput(session, paste0("spec.code_", row_id), value = NA)
    updateNumericInput(session, paste0("common_", row_id), value = "")
    updateNumericInput(session, paste0("length_", row_id), value = NA)
    updateNumericInput(session, paste0("sex_", row_id), value = "")
    updateNumericInput(session, paste0("shell_", row_id), value = "")
    updateNumericInput(session, paste0("cond_", row_id), value = "")
    updateNumericInput(session, paste0("disease_", row_id), value = NA)
    updateNumericInput(session, paste0("egg_", row_id), value = NA)
    updateTextInput(session, paste0("clutch_",row_id), value = NA)
    updateTextInput(session, paste0("vnotch_",row_id), value = "")
    updateTextInput(session, paste0("kept_",row_id), value = "")
    updateTextInput(session, paste0("abund_",row_id), value = NA)
    updateTextInput(session, paste0("cull_",row_id), value = "")
    }


  }) ## observe block


  ## when "next.set" is clicked
  observeEvent(input$next.set, {

    ## set relational column values
    set.id = input$set.num

    # Initialize database connection
    db <- dbConnect(RSQLite::SQLite(), paste0(dat.dir,"/INPUT_DATA.db"))

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

    # Close the database connection
    dbDisconnect(db)
    print("database updated")

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

  })


  ### When Trip is submitted
  observeEvent(input$submit.trip, {

    # Initialize database connection
    db <- dbConnect(RSQLite::SQLite(), paste0(dat.dir,"/INPUT_DATA.db"))

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
    updateDateInput(session, "board.date", value = "")
    updateDateInput(session, "land.date", value = "")
    updateTextInput(session, "sampler.name", value = "")
    updateTextInput(session, "lfa", value = "")
    updateTextInput(session, "captain.name", value = "")
    updateTextInput(session, "entry.name", value = "")
    updateDateInput(session, "entry.date", value = "")


  })





} ## Server code



shinyApp(ui, server)

# library(shiny)
# #library(shinyjs)
# library(svDialogs)
# library(RSQLite)
# library(dplyr)


}
