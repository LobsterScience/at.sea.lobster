

#' @title input.trip
#' @import dplyr RSQLite shiny svDialogs shinyjs shinyFeedback sf
#' @description Creates GUI for entering lobster At-Sea Sampling data by trip
#' @export

input.trip <- function(){

######################################################################################################################################
  ##### SETUP AND TABLE CREATION

dlg_message("In the following window, choose the directory where you want your trip data files to be stored (or where you've already stored trips that you want to update).")
dat.dir <- dlg_dir(filter = dlg_filters["csv",])$res
dat.dir.global <<- dat.dir ## for any other functions to reference

create.trip <- function(dat.dir = NULL, trip.id = NULL){

  ## set connection / create file
  con <- dbConnect(RSQLite::SQLite(), paste0(dat.dir,"/",trip.id,".db"))

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
  make.tab <- dbSendQuery(con, sql_statement)
  dbClearResult(make.tab)
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
  make.tab <- dbSendQuery(con, sql_statement)
  dbClearResult(make.tab)
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
  make.tab <- dbSendQuery(con, sql_statement)
  dbClearResult(make.tab)
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
  make.tab <- dbSendQuery(con, sql_statement)
  dbClearResult(make.tab)
}

}

###########################################################################################
######## SHINY CODE vv


suppressWarnings({
ui <- fluidPage(

######### GENERAL FORMATTING

  useShinyjs(), ##necessary for delay functions to work
  useShinyFeedback(),  ## necessary for inline error check warnings to work

## adjust functioanlity of specific keys for best user experience
## so Enter produces a Tab effect (good for number pad data entry)
tags$script(HTML("
  document.addEventListener('keydown', function(e) {
    // Check if Enter was pressed
    if (e.key === 'Enter') {
      e.preventDefault();  // Prevent default Enter action

      // Find all focusable elements
      let focusable = Array.prototype.filter.call(
        document.querySelectorAll('input, select, textarea, button, [tabindex]'),
        function(el) {
          return el.tabIndex >= 0 && !el.disabled && el.offsetParent !== null;
        }
      );

      let index = focusable.indexOf(document.activeElement);
      if (index > -1 && index + 1 < focusable.length) {
        let next = focusable[index + 1];
        next.focus();

        // If it's a text input or textarea, select its contents (like Tab does)
        if (next.tagName === 'INPUT' || next.tagName === 'TEXTAREA') {
          next.select();
        }
      }
    }
  });
")),

## so Esc produces undo effect like Ctrl+z
tags$script(HTML("
    document.addEventListener('keydown', function(e) {
      // Map Esc to Undo
      if (e.key === 'Escape') {
        e.preventDefault();
        document.execCommand('undo');
      }
    });
  ")),

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
  input[type='number'] {
    padding-right: 3px !important; /* Fixes number scroll arrows to far right */
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


## Delete info button styling
tags$style(HTML("
  .delete-button {
    display: flex;
    justify-content: flex-end;
    width: 100%;
  }

  .delete-action-button {
    background-color: red;
    color: white;
    border: none;
    padding: 4px 10px;
    font-size: 12px;
    cursor: pointer;
    border-radius: 4px;
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

## formatting for error/warning notifications (pop-up flags)
tags$head(
  tags$style(HTML("
      .shiny-notification {
        position: fixed;
        top: 80vh;
        left: 50vw;
        transform: translate(-50%, -50%);
        z-index: 1050;
        width: 300px; /* Increase width */
        height: 100px; /* Increase height */
        font-size: 18px; /* Make text bigger */
        padding: 20px; /* Add more padding */
        text-align: center; /* Center text */
      }
    "))
),

###################################### TRIP INFO


fluidRow(
  div(
    class = "title-with-button",
    titlePanel("TRIP INFO"),
    actionButton("submit_trip", "Finished for now, Close Trip!")
  )
  ),

  fluidRow(
    column(2, dateInput("board_date", "BOARDING DATE",value = NA)),
    column(2, numericInput("vessel_num", "VESSEL REG #", value = NA, min = 0, max = 999999, step = 1)),
    column(2, actionButton("go", "Make/Find Trip")),
    column(2, dateInput("land_date", "LANDING DATE",value = NA)),
    column(2, textInput("vessel_name", "VESSEL NAME"))

  ),
  fluidRow(
    column(2, numericInput("license_num", "LICENSE #", value = NA)),
    column(2, textInput("captain_name", "CAPTAIN NAME")),
    column(2, textInput("sampler_name", "SAMPLER NAME")),
    column(1, selectInput("lfa", "LFA:",choices = c("","L27", "L28","L29","L30","L31A","L31B","L32","L33","L34","L35","L36","L37","L38","L38B","L41")))
  ),
  fluidRow(
    column(2, selectInput("entry_group", "DATA ENTRY GROUP", choices= c("","CBFH","CMM","DFO","ESFPA","GCIFA","RCIFA","SWLSS"))),
    column(2, textInput("entry_name", "KEY PUNCHER NAME")),
    column(2, dateInput("entry_date", "DATA ENTRY DATE")),
    column(2, textInput("trip_code", "TRIP"), style = "pointer-events: none; opacity: 0.5;")
  ),


  ################################################## SET INFO
fluidRow(
  div(
    class = "delete-button",
    actionButton("delete_set", "Delete Set", class = "delete-action-button")
  )
),

fluidRow(
  div(
    class = "title-with-button",
    div(class = "title-panel", titlePanel("SET INFO")),  # Wrap the title in title-panel div
    # column(12, align = "right",  ### arrow button
    #        tags$div(
    #          class = "arrow-button arrow-left",
    #          id = "set_back_btn"
    #        )
    # ),
    actionButton("prev_set", "Save and Previous Set"),
    actionButton("next_set", "Save and Next Set")
  )
),


div(class = "compact-row",
    div(class= "compact-input", numericInput("set_num", "SET / TRAWL / STRING#",value = NA, min = 0)),
    div(class= "compact-input", numericInput("num_traps", "#TRAPS IN SET",value = NA, min = 0)),
    div(class = "mediumwide-input", numericInput("lat", "LATITUDE (DDMM.MM)", value = NA, max = 9059.99, min = -9059.99, step = 0.01)),
    div(class = "mediumwide-input", numericInput("lon", "LONGITUDE (DDMM.MM)", value = NA, max = 18059.99, min = -18059.99, step = 0.01)),
    div(class= "compact-input", numericInput("grid_num", "GRID NO", value = NA)),
    div(class= "compact-input", numericInput("depth", "DEPTH (FM)", value = NA, min = 0, max = 300)),
    div(class= "compact-input", numericInput("soak_days", "SOAK DAYS", value = NA, min = 0, max = 30)),
    div(class= "compact-input", numericInput("trap_type", "TRAP TYPE", value = NA, min = 1, max = 4)),
    div(class= "compact-input", numericInput("vent_size", "VENT SIZE (CODE)", value = NA, min = 1, max = 5)),
    div(class= "compact-input", numericInput("num_vents", "# OF VENTS", value = NA, min = 0))
  ),

 fluidRow(),

fluidRow(
  div(
    class = "delete-button",
    actionButton("delete_trap", "Delete Trap", class = "delete-action-button")
  )
),

fluidRow(
  div(
    class = "title-with-button",
    div(class = "title-panel", titlePanel(HTML("<span style='font-size: 24px;'> TRAP INFO</span> <span style='font-size: 16px;'>*Only traps with bait codes are submitted when you hit Next...</span>"))),  # Wrap the title in title-panel div
    # column(12, align = "right",  ### arrow button
    #        tags$div(
    #          class = "arrow-button arrow-left",
    #          id = "trap_back_btn"
    #        )
    # ),
    actionButton("next_trap", "Save and New Trap")
  )
),

fluidRow(
  column(1, numericInput("trap_num", "TRAP NO",value = NA, min = 0)),
  column(1, numericInput("bait_code", "BAIT CD1",value = NA, min = 0)),
  column(1, numericInput("bait_code2", "BAIT CD2",value = NA, min = 0)),
  column(1, numericInput("bait_code3", "BAIT CD3",value = NA, min = 0)),
  column(1, numericInput("bait_type1", "BAIT TYPE1",value = NA, min = 1, max = 4)),
  column(1, numericInput("bait_type2", "BAIT TYPE2",value = NA, min = 1, max = 4)),
  column(1, numericInput("bait_type3", "BAIT TYPE3",value = NA, min = 1, max = 4))
  ),

fluidRow(     ### use button formatted title class for FISH row just for easy formatting consistency
  div(
    class = "title-with-button",
    div(class = "title-panel", titlePanel(HTML("<span style='font-size: 24px;'> FISH INFO</span> <span style='font-size: 16px;'>* For Empty Trap: Species Code = 9999, Abundance = 0 (Only rows with species code are submitted)</span>"))))
  ),
## dynamically duplicating fish info row:
fluidRow(
  column(12, uiOutput("dynamicRows")) # Placeholder for dynamically generated rows
)

) ## End of UI

}) ## suppress warnings


####################################################################################################################################
  ###SERVER CODE


server <- function(input, output, session) {
suppressWarnings({

  ## DATA FILE IMPORTS
  ## bring in species and other code lists so they only have to be uploaded once
  spec.tab <- readRDS(paste0(system.file("data", package = "at.sea.lobster"),"/SPECIESCODES.rds"))
  ## remove code 2553 code for berried lobster (rdundan and potentially confusing)
  spec.tab <- spec.tab %>% filter(!SPECIES_CODE %in% 2553)
  #crustaceans <- spec.tab[grepl("crab|lobster", spec.tab$COMMON, ignore.case = TRUE), ]
  crust.codes <- c(2550,2552,2511,2513,2520,2523,2526,2531) ## doesn't include lobster larvae (2551)
  #wsu <- spec.tab[grepl("whelk|starfish|urchin", spec.tab$COMMON, ignore.case = TRUE), ]
  abund.species <- c(4210,2559,6400,6100,4330,2100,4321,8520)
  code.tab <- readRDS(paste0(system.file("data", package = "at.sea.lobster"), "/codes.rds"))
  condition <- readRDS(paste0(system.file("data", package = "at.sea.lobster"), "/condition.rds"))
  lfa.data <- readRDS(paste0(system.file("data", package = "at.sea.lobster"), "/LFAdata.rds"))
  ## Spatial files
  lfapolys <- readRDS(paste0(system.file("data", package = "at.sea.lobster"), "/LFAPolysSF.rds"))
  lfapolys$LFA <- paste0("L",lfapolys$LFA)
  lfapolys <- lfapolys %>% mutate(LFA = ifelse(LFA %in% "L311","L31A",
                                                ifelse(LFA %in% "L312","L31B",
                                                       LFA)))
  gridpolys <- readRDS(paste0(system.file("data", package = "at.sea.lobster"), "/GridPolysSF.rds"))
  gridpolys$LFA <- paste0("L",gridpolys$LFA)
  gridpolys <- gridpolys %>% mutate(LFA = ifelse(LFA %in% "L311","L31A",
                                                ifelse(LFA %in% "L312","L31B",
                                                       LFA)))

####################################################################################################################################
  ### DEFINE DATABASE UPDATING FUNCTIONS (FOR WHEN 'NEXT' BUTTONS ARE CLICKED)
  ##FISH
  update.fish <- function(db=NULL, trap.id = NULL){

  fish_columns <- dbListFields(db, "FISH_INFO")
  # Loop through each row and collect data for insertion
  data <- NULL
  for (row_id in row_ids()) {

    fish_num <- round(as.numeric(gsub("\\D", "", row_id)))

    data.row <- data.frame(
      trap.id = trap.id,
      trap_num = input[[paste0("trap_num_", row_id)]],
      fish_num = fish_num,
      species_code = input[[paste0("spec_code_", row_id)]],
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

  ## specific edits to data before upload
  data <- data %>% filter(!species_code %in% NA) %>% arrange(fish_num) ### remove last unfilled row where species code wasn't added and make sure table is sorted by fish number (will be the same as row number)
  data <- data %>% mutate(fish_num = as.character(fish_num)) ## convert back to character for upload

    # Insert data into the database (upload if no existing fish, update if fish found)
    checkfish <- paste("SELECT * FROM FISH_INFO WHERE TRAP_ID = '",trap.id, "'", sep = "")
    fish.result <- dbGetQuery(db, checkfish)
    fish.result <- fish.result %>% arrange(as.numeric(FISH_NO)) ## make sure fish data is sorted by fish number (because this is equivalent to row# in the app)
    if(nrow(fish.result)==0){
      print(paste0("No previously existing fish rows found. Adding ",nrow(data)," fish"))
    data[data == ""] <- NA
    colnames(data) = fish_columns
    if(nrow(data)>0){
      dbWriteTable(db, "FISH_INFO", data, append = TRUE, row.names = FALSE)
    }
    }

    if(nrow(fish.result)>0){
      data[data == ""] <- NA
      if(nrow(data)>0){
        new.rows <- nrow(data)-nrow(fish.result)
        print(paste0("Previously existing fish rows found. Adding ",new.rows," new fish, ","updating ",nrow(fish.result)," existing fish rows"))
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
        data.add <- data %>% filter(as.numeric(fish_num) > nrow(fish.result))
        if(nrow(data.add)>0){
          colnames(data.add) = fish_columns
          dbWriteTable(db, "FISH_INFO", data.add, append = TRUE, row.names = FALSE)
        }

      } ## replacement rows >0
      } ## fish data found

    print("Fish table updated")
    print("                  ")
  }

  ### TRAP (Insert if the trap hasn't been created yet)
  update.trap <- function(db=NULL, set.id = NULL, trap.id = NULL, delete = FALSE){
    if(!delete){
    #if(!is.na(input$bait_code)){  ## don't upload blank traps (curently redudant)
    checktrap <- paste("SELECT * FROM TRAP_INFO WHERE TRAP_ID = '",trap.id, "'", sep = "")
    trap.result <- dbGetQuery(db, checktrap)

    if(nrow(trap.result)==0){
      trap_columns <- dbListFields(db, "TRAP_INFO")
      t.dat <- data.frame(
        set.id,
        input$set_num,
        trap.id,
        input$trap_num,
        input$bait_code,
        input$bait_code2,
        input$bait_code3,
        input$bait_type1,
        input$bait_type2,
        input$bait_type3
      )
      colnames(t.dat) = trap_columns
      dbWriteTable(db, "TRAP_INFO", t.dat, append = TRUE, row.names = FALSE)
    }

    ## if the trap exists, update all values if the user has made changes to its information
    if(nrow(trap.result)>0){
      update_query <- paste("
    UPDATE TRAP_INFO
    SET BAIT_CD = '", input$bait_code, "',
        BAIT_CD2 = '", input$bait_code2, "',
        BAIT_CD3 = '", input$bait_code3, "',
        BAIT_TYPE1 = '", input$bait_type1, "',
        BAIT_TYPE2 = '", input$bait_type2, "',
        BAIT_TYPE3 = '", input$bait_type3, "'
    WHERE TRAP_ID = '", trap.id, "'", sep = "")

      dbExecute(db, update_query)
    }
    print("Trap table updated")
   # }
    }

    if(delete){ ## removes whole row based on trap ID
      delete_trap_query <- paste0("DELETE FROM TRAP_INFO WHERE TRAP_ID = '", trap.id, "'")
      dbExecute(db, delete_trap_query)
      ## Also delete all fish rows for deleted trap
      delete_fish_query <- paste0("DELETE FROM FISH_INFO WHERE TRAP_ID = '", trap.id, "'")
      dbExecute(db, delete_fish_query)
    }

  }

  ### SET (Insert if the set hasn't been created yet)
  update.set <- function(db=NULL, trip.id = NULL, set.id = NULL, delete = FALSE){
    if(!delete){
    checkset <- paste("SELECT * FROM SET_INFO WHERE FISHSET_ID = '",set.id, "'", sep = "")
    set.result <- dbGetQuery(db, checkset)
    if(nrow(set.result)==0){
      set.dat <- data.frame(
        trip.id,
        set.id,
        input$set_num,
        set.code.id = NA,
        gear.id = NA,
        spec.code = "2550",
        input$grid_num,
        est.catch = NA,
        input$num_traps,
        input$lat,
        input$lon,
        input$depth,
        input$soak_days,
        source = NA,
        num.hooks = NA,
        input$trap_type,
        input$vent_size,
        input$num_vents
      )
      set_columns <- dbListFields(db, "SET_INFO")
      colnames(set.dat) = set_columns
      dbWriteTable(db, "SET_INFO", set.dat, append = TRUE, row.names = FALSE)
    }

    ## if the set exists, update all values if the user has made changes to its information
    if(nrow(set.result)>0){
      update_query <- paste("
    UPDATE SET_INFO
    SET STRATUM_ID = '", input$grid_num, "',
        NUM_TRAPS = '", input$num_traps, "',
        LATDDMM = '", input$lat, "',
        LONGDDMM = '", input$lon, "',
        DEPTH = '", input$depth, "',
        SOAK_DAYS = '", input$soak_days, "',
        TRAP_TYPE = '", input$trap_type, "',
        VENT_CD = '", input$vent_size, "',
        NUM_VENTS = '", input$num_vents, "'
    WHERE FISHSET_ID = '", set.id, "'", sep = "")

      dbExecute(db, update_query)
    }
    print("Set table updated")
    }

    if(delete){ ## delete set row from database
      delete_set_query <- paste0("DELETE FROM SET_INFO WHERE FISHSET_ID = '", set.id, "'")
      dbExecute(db, delete_set_query)
    }

  }

  ### TRIP
  update.trip <- function(db=NULL, trip.id = NULL){
    checktrip <- paste("SELECT * FROM TRIP_INFO WHERE TRIP_ID = '",trip.id, "'", sep = "")
    trip.result <- dbGetQuery(db, checktrip)
    bdate <- as.character(input$board_date) ## ensure dates get uplaoded as strings
    if(length(input$land_date)>0){ldate <- as.character(input$land_date)}else{ldate <- NA}
    if(length(input$entry_date)>0){edate <- as.character(input$entry_date)}else{edate <- NA}
    if(!is.na(input$license_num)){lic_num <- sprintf("%06d",input$license_num)}else{lic_num <- NA}

    ## clean special characters (apostrophes etc. in names)
    escape_special_chars <- function(x) {
      if (is.character(x)) {
        # Escape single quotes (') and dashes (-) for Oracle
        x <- gsub("'", "''", x)
        x <- gsub("-", "\\-", x)
      }
      return(x)
    }

    vessel_name <- escape_special_chars(input$vessel_name)
    sampler_name <- escape_special_chars(input$sampler_name)
    captain_name <- escape_special_chars(input$captain_name)
    entry_name <- escape_special_chars(input$entry_name)


    if(nrow(trip.result)==0){
      trip.dat <- data.frame(
        trip.id,
        input$trip_code,
        input$entry_group,
        vessel_name,
        sprintf("%06d",input$vessel_num), ## ensure leading zeros
        lic_num,
        port = NA,
        bdate,
        ldate,
        sampler_name,
        input$lfa,
        captain_name,
        marfis.lic = NA,
        entry_name,
        edate

      )
      trip_columns <- dbListFields(db, "TRIP_INFO")
      colnames(trip.dat) = trip_columns
      dbWriteTable(db, "TRIP_INFO", trip.dat, append = TRUE, row.names = FALSE)
    }

    if(nrow(trip.result)>0){
      update_query <- paste("
    UPDATE TRIP_INFO
    SET OWNER_GROUP = '", input$entry_group, "',
        VESSEL_NAME = '", vessel_name, "',
        VESSEL_NO = '", sprintf("%06d",input$vessel_num), "',
        LICENSE_NO = '", lic_num, "',
        BOARD_DATE = '", input$board_date, "',
        LANDING_DATE = '", input$land_date, "',
        SAMPLER_NAME = '", sampler_name, "',
        COMAREA_ID = '", input$lfa, "',
        CAPTAIN = '", captain_name, "',
        CREATED_BY = '", entry_name, "',
        CREATED_DATE = '", input$entry_date, "'
    WHERE TRIP_ID = '", trip.id, "'", sep = "")

      dbExecute(db, update_query)
    }
    print("Trip table updated")
  }

  ###### END OF DB UPDATING FUNCTIONS

  ###########################################################################################################################################
  ### INITIAL CONDITIONS and AUTOFILLS

  ## INITIAL CONDITIONS ######################


  ## GREY-OUTS (Disabling of fields)
  ungrey.fish <- reactiveVal(FALSE)  ## fish fields need to be reactively enabled later in server code
  ## on start-up, disable all fields until a trip is found/created
    greyouts.main <- function(enable.main = T){

    if(enable.main){
    ##reset all fields
    shinyjs::enable("trap_num")
    shinyjs::enable("bait_code")
    shinyjs::enable("bait_code2")
    shinyjs::enable("bait_code3")
    shinyjs::enable("bait_type1")
    shinyjs::enable("bait_type2")
    shinyjs::enable("bait_type3")

    shinyjs::enable("land_date")
    shinyjs::enable("vessel_name")
    shinyjs::enable("license_num")
    shinyjs::enable("captain_name")
    shinyjs::enable("sampler_name")
    shinyjs::enable("lfa")
    shinyjs::enable("entry_group")
    shinyjs::enable("entry_name")
    shinyjs::enable("entry_date")
    shinyjs::enable("trip_code")

    shinyjs::enable("set_num")
    shinyjs::enable("num_traps")
    shinyjs::enable("lat")
    shinyjs::enable("lon")
    shinyjs::enable("grid_num")
    shinyjs::enable("depth")
    shinyjs::enable("soak_days")
    shinyjs::enable("trap_type")
    shinyjs::enable("vent_size")
    shinyjs::enable("num_vents")

    }else{
      shinyjs::disable("trap_num")
      shinyjs::disable("bait_code")
      shinyjs::disable("bait_code2")
      shinyjs::disable("bait_code3")
      shinyjs::disable("bait_type1")
      shinyjs::disable("bait_type2")
      shinyjs::disable("bait_type3")

      shinyjs::disable("land_date")
      shinyjs::disable("vessel_name")
      shinyjs::disable("license_num")
      shinyjs::disable("captain_name")
      shinyjs::disable("sampler_name")
      shinyjs::disable("lfa")
      shinyjs::disable("entry_group")
      shinyjs::disable("entry_name")
      shinyjs::disable("entry_date")
      shinyjs::disable("trip_code")

      shinyjs::disable("set_num")
      shinyjs::disable("num_traps")
      shinyjs::disable("lat")
      shinyjs::disable("lon")
      shinyjs::disable("grid_num")
      shinyjs::disable("depth")
      shinyjs::disable("soak_days")
      shinyjs::disable("trap_type")
      shinyjs::disable("vent_size")
      shinyjs::disable("num_vents")
    }
      }

  ## grey-outs function (contains rules for greying out fields by species, sex)
  greyouts.fish <- function(row_id = NULL, enable.fish = T){

    if(enable.fish){
      shinyjs::disable(paste0("set_num_", row_id) )
      shinyjs::disable(paste0("trap_num_", row_id) )
        shinyjs::enable(paste0("spec_code_", row_id) )
        shinyjs::disable(paste0("common_", row_id) )
        shinyjs::enable(paste0("length_", row_id) )
        shinyjs::enable(paste0("sex_", row_id) )
        shinyjs::enable(paste0("shell_", row_id) )
        shinyjs::enable(paste0("cond_", row_id) )
        shinyjs::enable(paste0("disease_", row_id) )
        shinyjs::enable(paste0("egg_", row_id) )
        shinyjs::enable(paste0("clutch_", row_id) )
        shinyjs::enable(paste0("vnotch_", row_id) )
        shinyjs::enable(paste0("kept_", row_id) )
        shinyjs::disable(paste0("abund_", row_id) )
        shinyjs::disable(paste0("cull_", row_id) )  ## fully disabled, only CBFH uses it. Just need to change to enable on this line if you want to renable for lobster

        ### fish type options
        ## 1. Crustacean (not lobster)
        ## 2. Lobster
        ## 3. abundance only species
        ## 4. Other fish
        ## 5. Empty Trap (9999)

        ## if species ia crustacean
        if(input[[paste0("spec_code_", row_id)]] %in% crust.codes && !input[[paste0("spec_code_", row_id)]] %in% c(2550,2552)){
          shinyjs::disable(paste0("shell_", row_id))
          shinyjs::disable(paste0("disease_", row_id))
          shinyjs::disable(paste0("egg_", row_id))
          shinyjs::disable(paste0("clutch_", row_id))
          shinyjs::disable(paste0("vnotch_", row_id))
          shinyjs::disable(paste0("cull_", row_id))
        } ## Crustacean

        ## if species is lobster
        if(input[[paste0("spec_code_", row_id)]] %in% c(2550,2552)){
          if(input[[paste0("spec_code_", row_id)]] %in% c(2550,2552) & !input[[paste0("sex_", row_id)]] %in% c(2,3)){
            shinyjs::disable(paste0("egg_", row_id))
            shinyjs::disable(paste0("clutch_", row_id))
            shinyjs::disable(paste0("vnotch_", row_id))
          }else{
            if(input[[paste0("spec_code_", row_id)]] %in% c(2550,2552) & input[[paste0("sex_", row_id)]] %in% 2){
              shinyjs::disable(paste0("egg_", row_id))
              shinyjs::disable(paste0("clutch_", row_id))
            }
          }
        } ## species is lobster

        ## if species is abundance only species
        if(input[[paste0("spec_code_", row_id)]] %in% abund.species){
          shinyjs::disable(paste0("length_", row_id) )
          shinyjs::disable(paste0("sex_", row_id) )
          shinyjs::disable(paste0("shell_", row_id) )
          shinyjs::disable(paste0("cond_", row_id) )
          shinyjs::disable(paste0("disease_", row_id) )
          shinyjs::disable(paste0("egg_", row_id) )
          shinyjs::disable(paste0("clutch_", row_id) )
          shinyjs::disable(paste0("vnotch_", row_id) )
          shinyjs::enable(paste0("abund_", row_id))
          shinyjs::disable(paste0("cull_", row_id) )
        } ##species is abundance only


        ## If species is Other Fish
        if(!input[[paste0("spec_code_", row_id)]] %in% crust.codes && !input[[paste0("spec_code_", row_id)]] %in% c(2550,2552) && !input[[paste0("spec_code_", row_id)]] %in% abund.species){
          shinyjs::disable(paste0("sex_", row_id) )
          shinyjs::disable(paste0("shell_", row_id))
          shinyjs::disable(paste0("disease_", row_id))
          shinyjs::disable(paste0("egg_", row_id))
          shinyjs::disable(paste0("clutch_", row_id))
          shinyjs::disable(paste0("vnotch_", row_id))
          shinyjs::disable(paste0("cull_", row_id))

        } ## Other fish

        ## Empty Trap (9999)
        if(input[[paste0("spec_code_", "row_1")]] %in% 9999){
          shinyjs::disable(paste0("length_", row_id) )
          shinyjs::disable(paste0("sex_", row_id) )
          shinyjs::disable(paste0("shell_", row_id) )
          shinyjs::disable(paste0("cond_", row_id) )
          shinyjs::disable(paste0("disease_", row_id) )
          shinyjs::disable(paste0("egg_", row_id) )
          shinyjs::disable(paste0("clutch_", row_id) )
          shinyjs::disable(paste0("vnotch_", row_id) )
          shinyjs::disable(paste0("kept_", row_id) )
          shinyjs::enable(paste0("abund_", row_id))
          shinyjs::disable(paste0("cull_", row_id) )
        }

    }else{## on start-up, disable all fields until a trip is found/created

        shinyjs::disable(paste0("set_num_", row_id) )
        shinyjs::disable(paste0("trap_num_", row_id) )
        shinyjs::disable(paste0("spec_code_", row_id) )
        shinyjs::disable(paste0("common_", row_id) )
        shinyjs::disable(paste0("length_", row_id) )
        shinyjs::disable(paste0("sex_", row_id) )
        shinyjs::disable(paste0("shell_", row_id) )
        shinyjs::disable(paste0("cond_", row_id) )
        shinyjs::disable(paste0("disease_", row_id) )
        shinyjs::disable(paste0("egg_", row_id) )
        shinyjs::disable(paste0("clutch_", row_id) )
        shinyjs::disable(paste0("vnotch_", row_id) )
        shinyjs::disable(paste0("kept_", row_id) )
        shinyjs::disable(paste0("abund_", row_id) )
        shinyjs::disable(paste0("cull_", row_id) )
    }
  }

  ## on start-up, disable all fields until a trip is found/created
  greyouts.main(enable.main = F)

  ## set reactive placeholder for trip.ID
  trip.id <- reactiveVal(NULL)

  ## set reactive success/failure check variables for every field to allow user to click NEXT or not
  checks <- reactiveValues(check1 = T,
                           check2 = T,
                           check3 = T,
                           check4 = T,
                           check5 = T,
                           check6 = T,
                           check7 = T,
                           check8 = T,
                           check9 = T,
                           check10 = T,
                           check11 = T,
                           check12 = T,
                           check13 = T,
                           check14 = T,
                           check15 = T,
                           check16 = T,
                           check17 = T,
                           check18 = T,
                           check19 = T,
                           check20 = T,
                           check21 = T,
                           check22 = T,
                           check23 = T,
                           check24 = T,
                           check25 = T,
                           check26 = T,
                           check27 = T,
                           check28 = T,
                           check29 = T,
                           check30 = T,
                           check31 = T,
                           check32 = T,
                           check33 = T,
                           check34 = T,
                           check35 = T,
                           check36 = T,
                           check37 = T,
                           check38 = T,
                           check39 = T,
                           check40 = T,
                           check41 = T,
                           check42 = T,
                           check43 = T)



  #### for reactively adding fish Info rows when species code is entered
  # Reactive value to track row IDs
  row_ids <- reactiveVal(c("row_1"))

  # Reactive values to store row data
  row_data <- reactiveValues(data = list())

  ## reactive vals for if any fish, trap or set data has changed and not been saved
  unsaved.fish <- reactiveVal(F)
  unsaved.trap <- reactiveVal(F)
  unsaved.set <- reactiveVal(F)


  ## reactive value for fish table every time new fish data is retrieved from the database (to activate filling in of GUI rows)
  new.fish.data <- reactiveVal(NULL)
  rerun.fish <- reactiveVal(0)  ## for looping to check if fish row number is right before filling with data
  suppress_spec_fill <- reactiveVal(FALSE) ## for inactivating other observe blocks while fish rows are filling from data

  # Template for a fish row
  create_row <- function(row_id) {
    div(id = paste0("row_container_", row_id), class = "compact-row",
        div(class = "compact-input",
            numericInput(paste0("set_num_", row_id), "SET NO", value = NA, min = 0),
            style = "pointer-events: none; opacity: 0.8;"
        ),
        div(class = "compact-input",
            numericInput(paste0("trap_num_", row_id), "TRAP NO", value = NA, min = 0),
            style = "pointer-events: none; opacity: 0.8;"
        ),
        div(class = "compact-input",
            numericInput(paste0("spec_code_", row_id), "SPECIES CODE", value = NA, min = 0)
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
            numericInput(paste0("disease_", row_id), "SHELL DISEASE", value = NA, min = 0, max = 3)
        ),
        div(class = "compact-input",
            numericInput(paste0("egg_", row_id), "EGG STAGE", min = 1, max = 4, value = NA)
        ),
        div(class = "compact-input",
            numericInput(paste0("clutch_", row_id), "CLUTCH %", value = NA, min = 1, max = 3)
        ),
        div(class = "compact-input",
            numericInput(paste0("vnotch_", row_id), "VNOTCH", min = 0, max = 5, value = NA)
        ),
        div(class = "compact-input",
            numericInput(paste0("kept_", row_id), "KEPT (0=N, 1=Y)", value = NA, min = 0, max = 1)
        ),
        div(class = "compact-input",
            numericInput(paste0("abund_", row_id), "ABUNDANCE", value = 1, min = 0)
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


  #### AUTOFILLS ###############################

  ## activate make/find trip button only when both board date and vessel reg# are entered
  observe({
    if(is.null(input$board_date) || is.na(input$board_date) || is.null(input$vessel_num) || is.na(input$vessel_num)){
      disable("go")
    }else{
      enable("go")
    }
  })

  ## create Trip code and look for trip when enough info is entered and user hits go button
  observeEvent(input$go, {
    req(input$board_date)
    req(input$vessel_num)
      if (length(input$board_date)==0  || is.na(input$vessel_num) || input$vessel_num == "") {
        updateTextInput(session, "trip_code", value = NA)
        trip.id(NULL)
      }
      # This block will run only when both input$vessel.num and input$board.date are not NULL
      req(input$vessel_num, input$board_date)
        board.date <- format(input$board_date, "%d%m%y")
        vrn <- sprintf("%06d",input$vessel_num) ## ensures that the vessel number in TRIP always has 6 digits
        updateTextInput(session, "trip_code", value = paste0(vrn,"-",board.date))
        ## set relational column
        TRIP.ID <- paste0(vrn,"_",board.date)
        trip.id(TRIP.ID)
  }, ignoreInit = TRUE)

  ## then, whenever trip code changes, first autofill trip info if trip is found
  observeEvent(input$trip_code, {
    if(input$trip_code %in% ""){
      ## use delay and change from different value to trigger reset of downstream values
      updateNumericInput(session, "set_num", value = 1)
      delay(5,{
        updateNumericInput(session, "set_num", value = NA)
      })

      # Clear input fields for trip INFO
      suppressWarnings(updateDateInput(session, "land_date", value = NA))
      updateTextInput(session, "vessel_name", value = "")
      updateNumericInput(session, "license_num", value = NA)
      updateTextInput(session, "captain_name", value = "")
      updateTextInput(session, "sampler_name", value = "")
      updateSelectInput(session, "lfa", selected = "")
      updateSelectInput(session, "entry_group", selected = "")
      updateTextInput(session, "entry_name", value = NA)
      #suppressWarnings(updateDateInput(session, "entry_date", value = NA))

    }else{
      new.trip <- trip.id()
      if(!is.null(new.trip)){
        files <- list.files(dat.dir)
        continue.create <- "yes"
        if(!paste0(new.trip,".db") %in% files){
          continue.create <- dlgMessage(type = "yesno", message = "A .db file for this trip does not yet exist in your chosen directory. Create it?.")
          continue.create <- continue.create$res
        }
        if(continue.create %in% "yes"){
          disable("board_date")
          disable("vessel_num")
          disable("go")
          ungrey.fish(TRUE) ## used reactively in species code reaction code later
          greyouts.main(enable.main =T) ## use grey-outs function to enable all downstream fields
        ## set last trip for check.table function
        last.trip <<- new.trip
        # check database for existing set (create if missing)
        create.trip(dat.dir = dat.dir, trip.id = new.trip)
        db <- dbConnect(RSQLite::SQLite(), paste0(dat.dir,"/",new.trip,".db"))
        checktrip <- paste("SELECT * FROM TRIP_INFO WHERE TRIP_ID = '",new.trip, "'", sep = "")
        trip.result <- dbGetQuery(db, checktrip)
        dbDisconnect(db)

        if(nrow(trip.result)==0){
          # Clear input fields for trip INFO
          suppressWarnings(updateDateInput(session, "land_date", value = NA))
          updateTextInput(session, "vessel_name", value = "")
          updateNumericInput(session, "license_num", value = NA)
          updateTextInput(session, "captain_name", value = "")
          updateTextInput(session, "sampler_name", value = "")
          updateSelectInput(session, "lfa", selected = "")
          updateSelectInput(session, "entry_group", selected = "")
          updateTextInput(session, "entry_name", value = NA)
          #suppressWarnings(updateDateInput(session, "entry_date", value = NA))


        }else{
          ## if there is data for current trip, fill fields
          suppressWarnings(updateDateInput(session, "land_date", value = as.Date(trip.result$LANDING_DATE)))
          updateTextInput(session, "vessel_name", value = trip.result$VESSEL_NAME)
          updateNumericInput(session, "license_num", value = trip.result$LICENSE_NO)
          updateTextInput(session, "captain_name", value = trip.result$CAPTAIN)
          updateTextInput(session, "sampler_name", value = trip.result$SAMPLER_NAME)
          updateSelectInput(session, "lfa", selected = trip.result$COMAREA_ID)
          updateSelectInput(session, "entry_group", selected = trip.result$OWNER_GROUP)
          updateTextInput(session, "entry_name", value = trip.result$CREATED_BY)
          suppressWarnings(updateDateInput(session, "entry_date", value = as.Date(trip.result$CREATED_DATE)))
        }

        ### then begin set # at 1 (or clear if trip code is cleared)
        updateNumericInput(session, "set_num", value = NA)
        delay(5,{
          updateNumericInput(session, "set_num", value = 1)
        })

        }else{ ## if user declines to create new trip, just clear the vessel reg # so they must choose a new one
          updateNumericInput(session, "vessel_num", value = NA)
      }

      }



    }
  }, ignoreInit = T)


  ## Autofill SET fields based on current set number selection (this is also triggered when NEXT or BACK buttons pressed)
  observeEvent(input$set_num, {
    ## define IDs for relational columns
    trip.id <- trip.id()
    new.set <- NULL
    if(!is.null(trip.id) & !is.na(input$set_num) & !is.null(input$set_num)){new.set <- paste0(trip.id(),"_",input$set_num)}

    if(!is.null(new.set)){
      # check database for existing set
      db <- dbConnect(RSQLite::SQLite(), paste0(dat.dir,"/",trip.id,".db"))
      checkset <- paste("SELECT * FROM SET_INFO WHERE FISHSET_ID = '",new.set, "'", sep = "")
      set.result <- dbGetQuery(db, checkset)
      dbDisconnect(db)

      if(nrow(set.result)==0){
        # Clear input fields for SET INFO
        updateNumericInput(session, "num_traps", value = NA)
        updateNumericInput(session, "lat", value = NA)
        updateNumericInput(session, "lon", value = NA)
        updateNumericInput(session, "grid_num", value = NA)
        updateNumericInput(session, "depth", value = NA)
        updateNumericInput(session, "soak_days", value = NA)
        updateNumericInput(session, "vent_size", value = NA)
        updateNumericInput(session, "num_vents", value = NA)
        updateNumericInput(session, "trap_type", value = NA)
      }else{
        ## if there is data for current set, fill fields
        updateNumericInput(session, "num_traps", value = set.result$NUM_TRAPS)
        updateNumericInput(session, "lat", value = set.result$LATDDMM)
        updateNumericInput(session, "lon", value = set.result$LONGDDMM)
        updateNumericInput(session, "grid_num", value = set.result$STRATUM_ID)
        updateNumericInput(session, "depth", value = set.result$DEPTH)
        updateNumericInput(session, "soak_days", value = set.result$SOAK_DAYS)
        updateNumericInput(session, "vent_size", value = set.result$VENT_CD)
        updateNumericInput(session, "num_vents", value = set.result$NUM_VENTS)
        updateNumericInput(session, "trap_type", value = set.result$TRAP_TYPE)
      }

    }

    ## whenever the set # is changed, Set trap # to 1 which will cause cascade down to autofill any existing data for first trap
    ## unless the field is just cleared, in which case clear trap # too
    ## Make sure it always changes from a different(with delay) to trigger reset of downstream values
    # if(is.na(input$set_num)){
    #   updateNumericInput(session, "trap_num", value = 1)
    #   delay(5, {
    #     updateNumericInput(session, "trap_num", value = NA)
    #   })
    # }else{
    #   updateNumericInput(session, "trap_num", value = NA)
    #   delay(5, {
    #     updateNumericInput(session, "trap_num", value = 1)
    #   })
    # }

    ## User request, just clearn Trap Number anytime Set is changed so user has to input this (don't need above code)

    updateNumericInput(session, "trap_num", value = NA)

  }, ignoreInit = TRUE)


  ## Autofill TRAP and fish data based on current trap number selection (this is also triggered when NEXT or BACK buttons pressed)
  observeEvent(input$trap_num, {

    ## define IDs for relational columns
    trip.id <- trip.id()
    set.id <- NULL
    new.trap <- NULL
    if(!is.null(trip.id) & !is.na(input$set_num) & !is.null(input$set_num)){set.id <- paste0(trip.id(),"_",input$set_num)}
    if(!is.null(set.id) & !is.na(input$trap_num) & !is.null(input$trap_num)){new.trap <- paste0(trip.id(),"_",input$set_num,"_",input$trap_num)}

    if(!is.null(trip.id) & !is.null(set.id) & !is.null(new.trap)){
      # check database for existing trap
      db <- dbConnect(RSQLite::SQLite(), paste0(dat.dir,"/",trip.id,".db"))
      checktrap <- paste("SELECT * FROM TRAP_INFO WHERE TRAP_ID = '",new.trap, "'", sep = "")
      trap.result <- dbGetQuery(db, checktrap)
      dbDisconnect(db)

      if(nrow(trap.result)==0){ ### if there's no pre-existing data for the new trap selection, clear all fields
        new.fish.data(NULL) ## and reset fish.data
        # Clear input fields for Trap and Fish INFO
        updateNumericInput(session, "bait_code", value = NA)
        updateNumericInput(session, "bait_code2", value = NA)
        updateNumericInput(session, "bait_code3", value = NA)
        updateNumericInput(session, "bait_type1", value = NA)
        updateNumericInput(session, "bait_type2", value = NA)
        updateNumericInput(session, "bait_type3", value = NA)

###### FISH row Autofilling begins here

        for (row_id in row_ids()) {
           disable(paste0("set_num_", row_id))## fish row set and trap fields should always be disabled(greyed out for user):
           disable(paste0("trap_num_", row_id))
          ##updateNumericInput(session, paste0("trap_num_", row_id), value = input$trap_num)  ## we have a seperate continuous observer for fish row trap number
          updateNumericInput(session, paste0("spec_code_", row_id), value = NA)
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
          updateNumericInput(session, paste0("abund_",row_id), value = 1)
          updateNumericInput(session, paste0("cull_",row_id), value = NA)
        }
      }else{

        ### if there is pre-existing data for trap selection:
        ## fill data for selected trap
        updateNumericInput(session, "bait_code", value = trap.result$BAIT_CD[1])
        updateNumericInput(session, "bait_code2", value = trap.result$BAIT_CD2[1])
        updateNumericInput(session, "bait_code3", value = trap.result$BAIT_CD3[1])
        updateNumericInput(session, "bait_type1", value = trap.result$BAIT_TYPE1[1])
        updateNumericInput(session, "bait_type2", value = trap.result$BAIT_TYPE2[1])
        updateNumericInput(session, "bait_type3", value = trap.result$BAIT_TYPE3[1])

        ## for fish, match GUI row number to existing fish rows in db, then update with the existing fish
        db <- dbConnect(RSQLite::SQLite(), paste0(dat.dir,"/",trip.id,".db"))
        existing.fish <-  paste("SELECT * FROM FISH_INFO WHERE TRAP_ID = '",new.trap, "'", sep = "")
        fish.result <- dbGetQuery(db, existing.fish)
        dbDisconnect(db)
        fish.result <- fish.result %>% arrange(as.numeric(FISH_NO))  ## ensures that fish rows are sorted by fish number, so these can be treated equivalent to GUI row number
        ## update the reactive table for use later
        new.fish.data(fish.result)

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

        ## Clear fish row data from previous trap
        lapply(row_ids(), function(row_id) {
          #updateNumericInput(session, paste0("trap_num_", row_id), value = NA)
          updateNumericInput(session, paste0("spec_code_", row_id), value = NA)
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
          updateNumericInput(session, paste0("abund_", row_id), value = 1)
          updateNumericInput(session, paste0("cull_", row_id), value = NA)
        })


      }
    }

    ## If at any time the trap # field is cleared, always clear downstream fields
    if(is.na(input$trap_num)){
      # Clear input fields for Trap and Fish INFO
      updateNumericInput(session, "bait_code", value = NA)
      updateNumericInput(session, "bait_code2", value = NA)
      updateNumericInput(session, "bait_code3", value = NA)
      updateNumericInput(session, "bait_type1", value = NA)
      updateNumericInput(session, "bait_type2", value = NA)
      updateNumericInput(session, "bait_type3", value = NA)

      for (row_id in row_ids()) {
        updateNumericInput(session, paste0("trap_num_", row_id), value = input$trap_num)
        updateNumericInput(session, paste0("spec_code_", row_id), value = NA)
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
        updateNumericInput(session, paste0("abund_",row_id), value = 1)
        updateNumericInput(session, paste0("cull_",row_id), value = NA)
      }
    }

  }, ignoreInit = TRUE)

  ## Once all necessary fish rows are built to match found fish data, fill rows with new data

  observeEvent(list(new.fish.data(),rerun.fish()),{
    req(rerun.fish())
    req(new.fish.data())
    req(input$trap_num)
    req(row_ids())
    if(length(row_ids()) >= nrow(new.fish.data())){
      suppress_spec_fill(TRUE) ## suppress any autofilling action while data is being retrieved
    row.ids.vect <- row_ids()
    fish.dat <- new.fish.data()
    delay(10,{   ### delay needed to make sure GUI finishes generating rows before trying to fill
    for (i in 1:nrow(fish.dat)) {
      new.row <- row.ids.vect[i]
      ##updateNumericInput(session, paste0("trap_num_", new.row), value = input$trap_num)
      updateNumericInput(session, paste0("spec_code_", new.row), value = fish.dat$SPECCD_ID[i])
      updateTextInput(session, paste0("common_", new.row), value = fish.dat$COMMON[i])
      updateNumericInput(session, paste0("length_", new.row), value = fish.dat$FISH_LENGTH[i])
      updateNumericInput(session, paste0("sex_", new.row), value = fish.dat$SEXCD_ID[i])
      updateNumericInput(session, paste0("shell_", new.row), value = fish.dat$SHELL[i])
      updateNumericInput(session, paste0("cond_", new.row), value = fish.dat$CONDITION[i])
      updateNumericInput(session, paste0("disease_", new.row), value = fish.dat$DISEASE[i])
      updateNumericInput(session, paste0("egg_", new.row), value = fish.dat$EGG_STAGE[i])
      updateNumericInput(session, paste0("clutch_", new.row), value = fish.dat$CLUTCH[i])
      updateNumericInput(session, paste0("vnotch_", new.row), value = fish.dat$VNOTCH[i])
      updateNumericInput(session, paste0("kept_", new.row), value = fish.dat$KEPT[i])
      updateNumericInput(session, paste0("abund_", new.row), value = fish.dat$ABUNDANCE[i])
      updateNumericInput(session, paste0("cull_", new.row), value = fish.dat$CULLS[i])
      delay(2000,{  ### suppress autofilling action until well after the data is retrieved
        suppress_spec_fill(FALSE)
      })
    }
    })

    }else{
      rerun.fish(rerun.fish()+1)
    }

  }, ignoreInit = TRUE)



  ##  Autofill Update each fish row's trap.num and set.num field with the current input$trap_num and input$setvalue
  observeEvent(list(input$trap_num, row_ids()), {
    req(input$trap_num) # Ensure trap.num has a value
    delay(5, {  ## delay ensures that row_ids() has enough time to update after new action is taken (sometimes this can lag and rows get missed)
      current_rows <- row_ids()
      lapply(current_rows, function(row_id) {
        updateNumericInput(session, paste0("trap_num_", row_id), value = input$trap_num)
      })
    })
  })
  observeEvent(list(input$set_num, row_ids()), {
    req(input$set_num) # Ensure trap.num has a value
    delay(5, {  ## delay ensures that row_ids() has enough time to update after new action is taken (sometimes this can lag and rows get missed)
      current_rows <- row_ids()
      lapply(current_rows, function(row_id) {
        updateNumericInput(session, paste0("set_num_", row_id), value = input$set_num)
      })
    })
  })

  # ## Autofill species names for bait codes (subscript)
  observeEvent(input$bait_code, {
      if(!input$bait_code %in% c(NA,NULL,"")){
        hideFeedback("bait_code")
        new.bait <- input$bait_code
        b.spec <- spec.tab$COMMON[which(spec.tab$SPECIES_CODE %in% new.bait)]
        showFeedback("bait_code",b.spec)
      }else{hideFeedback("bait_code")}
      }, ignoreInit = T)

  observeEvent(input$bait_code2, {
      if(!input$bait_code2 %in% c(NA,NULL,"")){
        hideFeedback("bait_code2")
        new.bait2 <- input$bait_code2
        b.spec2 <- spec.tab$COMMON[which(spec.tab$SPECIES_CODE %in% new.bait2)]
        showFeedback("bait_code2",b.spec2)
      }else{hideFeedback("bait_code2")}
  }, ignoreInit = T)

  observeEvent(input$bait_code3, {
      if(!input$bait_code3 %in% c(NA,NULL,"")){
        hideFeedback("bait_code3")
        new.bait3 <- input$bait_code3
        b.spec3 <- spec.tab$COMMON[which(spec.tab$SPECIES_CODE %in% new.bait3)]
        showFeedback("bait_code3",b.spec3)
      }else{hideFeedback("bait_code3")}
  }, ignoreInit = T)

  ## Also autofill code descriptions for Trap Type, Vent size, and Bait Types
  observeEvent(input$trap_type, {
    if(!input$trap_type %in% c(NA,NULL,"")){
      hideFeedback("trap_type")
      t.type <- code.tab$Name[which(code.tab$Field %in% "Trap Type" & code.tab$Code %in% input$trap_type)]
      showFeedback("trap_type",t.type)
    }else{hideFeedback("trap_type")}
  }, ignoreInit = T)

  observeEvent(input$vent_size, {
    if(!input$vent_size %in% c(NA,NULL,"")){
      hideFeedback("vent_size")
      v.size <- code.tab$Name[which(code.tab$Field %in% "Vent Size" & code.tab$Code %in% input$vent_size)]
      showFeedback("vent_size",v.size)
    }else{hideFeedback("vent_size")}
  }, ignoreInit = T)

  # observeEvent(input$bait_type1, {
  #   if(!input$bait_type1 %in% c(NA,NULL,"")){
  #     hideFeedback("bait_type1")
  #     b.type <- code.tab$Name[which(code.tab$Field %in% "Bait Type" & code.tab$Code %in% input$bait_type1)]
  #     showFeedback("bait_type1",b.type)
  #   }else{hideFeedback("bait_type1")}
  # }, ignoreInit = T)

  # observeEvent(input$bait_type2, {
  #   if(!input$bait_type2 %in% c(NA,NULL,"")){
  #     hideFeedback("bait_type2")
  #     b.type <- code.tab$Name[which(code.tab$Field %in% "Bait Type" & code.tab$Code %in% input$bait_type2)]
  #     showFeedback("bait_type2",b.type)
  #   }else{hideFeedback("bait_type2")}
  # }, ignoreInit = T)

  # observeEvent(input$bait_type3, {
  #   if(!input$bait_type3 %in% c(NA,NULL,"")){
  #     hideFeedback("bait_type3")
  #     b.type <- code.tab$Name[which(code.tab$Field %in% "Bait Type" & code.tab$Code %in% input$bait_type3)]
  #     showFeedback("bait_type3",b.type)
  #   }else{hideFeedback("bait_type3")}
  # }, ignoreInit = T)


  # Observe when catch species code is added to last row and add a new row
  observeEvent(input[[paste0("spec_code_", tail(row_ids(), 1))]], {
    last_id <- tail(row_ids(), 1)
    if (!is.null(input[[paste0("spec_code_", last_id)]]) &
        !is.na(input[[paste0("spec_code_", last_id)]])) {
      new_id <- paste0("row_", length(row_ids()) + 1)
      row_ids(c(row_ids(), new_id))
      insertUI(selector = "#dynamicRows", where = "beforeEnd", ui = create_row(new_id))
    }
  })




  ### Automated fill responses for FISH fields:


  ## continually observe catch spec code input to auto-fill common name
  observe({
    #req(!suppress_spec_fill())  ## most of below code should only work for manual filling of rows (shouldnt be triggered at all by imported data filling, except for greyouts)
    current_rows <- row_ids()
    # Create an observer for each spec.code field
    lapply(current_rows, function(row_id) {
      #disable(paste0("common_", row_id) ) ## user interaction is always disabled for common name
      observeEvent(input[[paste0("spec_code_", row_id)]], {
        req(!suppress_spec_fill()) ## don't do anything if change is result of a data import
        # Get the updated value
        new_spec <- input[[paste0("spec_code_", row_id)]]
        ### reference species list to auto fill common name
        common <- spec.tab$COMMON[which(spec.tab$SPECIES_CODE %in% new_spec)]
        updateTextInput(session, paste0("common_", row_id), value = common)

      }, ignoreInit = TRUE)  # Avoid triggering on initialization
      ###
      ## control contextual restrictions to fish row field usage (which fields can be used for what species/sex selections etc.)
      observeEvent(list(input[[paste0("spec_code_", row_id)]],input[[paste0("sex_", row_id)]], ungrey.fish()),{
        ## Activate greyouts function to grey disabled fields (even if all other autofilling is suppressed)
        ungrey.fish <- ungrey.fish()
        greyouts.fish(row_id, enable.fish = ungrey.fish)
        req(!suppress_spec_fill()) ## don't do anything else if change is result of a data import
        ## clear all fields if species code becomes empty
        if(input[[paste0("spec_code_", row_id)]] %in% c(NULL,NA)){
          updateNumericInput(session, paste0("length_", row_id), value = NA)
          updateNumericInput(session, paste0("sex_",row_id), value = NA)
          updateNumericInput(session, paste0("cond_", row_id), value = NA)
          updateNumericInput(session, paste0("kept_", row_id), value = NA)
          updateNumericInput(session, paste0("abund_", row_id), value = 1)
        }
        ## abundance is alway 1 unless it's an abundance onyl species
        updateNumericInput(session, paste0("abund_", row_id), value = 1)
        ## if non-lobster value entered
        if(!input[[paste0("spec_code_", row_id)]] %in% c(2550,2552)){

          ## if it's still a crustacean, clear abundance
          # if(input[[paste0("spec_code_", row_id)]] %in% crust.codes){
          #   updateNumericInput(session, paste0("abund_", row_id), value = 1)
          # }

          ## need to clear any disabled fields if theres already values in them
          updateNumericInput(session, paste0("egg_", row_id), value = NA)
          updateNumericInput(session, paste0("vnotch_",row_id), value = NA)
          updateNumericInput(session, paste0("clutch_", row_id), value = NA)
          updateNumericInput(session, paste0("shell_", row_id), value = NA)
          updateNumericInput(session, paste0("cull_",row_id), value = NA)
          updateNumericInput(session, paste0("disease_", row_id), value = NA)

          ### if species is 9999 (empty trap) silence all remaining fields except abundance
          if(input[[paste0("spec_code_", "row_1")]] %in% 9999){
            updateNumericInput(session, paste0("length_", "row_1"), value = NA)
            updateNumericInput(session, paste0("sex_","row_1"), value = NA)
            updateNumericInput(session, paste0("cond_", "row_1"), value = NA)
            updateNumericInput(session, paste0("kept_", "row_1"), value = NA)
            updateNumericInput(session, paste0("abund_", row_id), value = 0)
            ## also clear any warnings/messages (use delay to out-wait warnings generated anywhere else)
            delay(5,{
              hideFeedback(paste0("length_", "row_1"))
              hideFeedback(paste0("sex_", "row_1"))
              hideFeedback(paste0("cond_", "row_1"))
              hideFeedback(paste0("kept_", "row_1"))
            })
            ## Clear and silence any remaining rows
            lapply(row_ids(), function(row_id) {
              if(!row_id %in% "row_1" ){
                updateNumericInput(session, paste0("spec_code_", row_id), value = NA)
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
                updateNumericInput(session, paste0("cull_", row_id), value = NA)
              }
            })

          }else{
            ## if it's in the abundance only species
            if(input[[paste0("spec_code_", row_id)]] %in% abund.species){
              updateNumericInput(session, paste0("length_", row_id), value = NA)
              updateNumericInput(session, paste0("sex_", row_id), value = NA)
              updateNumericInput(session, paste0("cond_", row_id), value = NA)
              updateNumericInput(session, paste0("abund_", row_id), value = NA)
            }
          }
        }else{ ## is a lobster
           if(input[[paste0("spec_code_", row_id)]] %in% c(2550,2552) & !input[[paste0("sex_", row_id)]] %in% c(2,3)){

          updateNumericInput(session, paste0("egg_", row_id), value = NA)
          updateNumericInput(session, paste0("vnotch_",row_id), value = NA)
          updateNumericInput(session, paste0("clutch_", row_id), value = NA)
          updateNumericInput(session, paste0("disease_", row_id), value = 0)
        }else{
          if(input[[paste0("spec_code_", row_id)]] %in% c(2550,2552) & input[[paste0("sex_", row_id)]] %in% 2){
            updateNumericInput(session, paste0("egg_", row_id), value = NA)
            updateNumericInput(session, paste0("clutch_", row_id), value = NA)
          }
        }
        }

      }, ignoreInit = T)

    })

  })



####################################################################################################################################################
##BEGINNING OF INTERACTIVE SERVER CODE (BUTTON CLICKS)
  ## SUBMIT LEVEL 1
  # When "next.trap" button is clicked
  observeEvent(input$next_trap, {

        ## define IDs for relational columns
        set.id <- NULL
        trap.id <- NULL
        trip.id <- trip.id()
        continue = T
        if(is.null(trip.id)){
          warning("No TRIP ID Found!")
          showNotification("No TRIP ID FOUND!", type = "error")
          continue = F
        }
        if(continue){
          if(!is.null(trip.id) & !is.na(input$set_num) & !is.null(input$set_num)){set.id <- paste0(trip.id(),"_",input$set_num)
          }else{
            warning("No Set ID Found!")
            showNotification("No SET ID FOUND!", type = "error")
            continue = F
          }
        }
        if(continue){
          if(!is.null(set.id) & !is.na(input$trap_num) & !is.null(input$trap_num)){trap.id <- paste0(trip.id(),"_",input$set_num,"_",input$trap_num)
          }else{
            warning("No Trap ID Found!")
            showNotification("No TRAP ID FOUND!", type = "error")
            continue = F
          }
        }

        if(continue){
          delay(10,{ ## give delay to make sure all relational variables are properly set before updating db
            # Initialize database connection
            db <- dbConnect(RSQLite::SQLite(), paste0(dat.dir,"/",trip.id,".db"))

            ## Update upstream and downstream data
            update.trip(db, trip.id)
            update.set(db, trip.id = trip.id, set.id = set.id)  ###  ^^ upstream
            if(!is.na(input$bait_code) && !is.null(input$bait_code)){ ## don't upload blank traps
              update.trap(db,set.id = set.id, trap.id = trap.id)  ###  vv downstream
              update.fish(db, trap.id = trap.id)
            }

            #######################
            ## as a last cleanup step, convert any character 'NA' values that got introduced to sql NULL to unify missing values format
            tables <- c("TRIP_INFO", "SET_INFO", "TRAP_INFO", "FISH_INFO")

            for (table in tables) {
              col_info <- dbGetQuery(db, paste0("PRAGMA table_info(", table, ");"))
              for (col in col_info$name) {
                query <- paste0("UPDATE ", table, " SET ", col, " = NULL WHERE ", col, " = 'NA'")
                dbExecute(db, query)
              }
            }
            #################################

            dbDisconnect(db)

            updateNumericInput(session, "trap_num", value = NA)
          })
          ## quickly change set num and change back to make sure it shows save effects
          delay(500, {
            set.num <- input$set_num
            updateNumericInput(session, "set_num", value = NA)
            updateNumericInput(session, "set_num", value = set.num)
          })
        }



  }) ## observeEvent block


  ## When back button for trap clicked  (inverse of next trap) ## going backwards doesn't update database, just check for existing data to fill fields
  # observeEvent(input$trap_back_btn, {
  #
  #   if(!is.na(input$trap_num) & input$trap_num>0){
  #     updateNumericInput(session, "trap_num", value = input$trap_num-1)
  #   }
  #   # if(input$trap_num==0){
  #   #   trap.max <- paste("SELECT MAX(TRAP_NO) AS MAX_TRAP_NO FROM TRAP_INFO WHERE FISHSET_ID = ","'",set.id,"'", sep = "")
  #   # }
  # }) ## observeEvent block

  ## When 'Delete Trap' button is clicked (need this option to remove any traps wrongly created)
  observeEvent(input$delete_trap, {
    ## define IDs for relational columns
    set.id <- NULL
    trap.id <- NULL
    trip.id <- trip.id()
    continue = T
    if(is.null(trip.id)){
      warning("No TRIP ID Found!")
      showNotification("No TRIP ID FOUND!", type = "error")
      continue = F
    }
    if(continue){
      if(!is.null(trip.id) & !is.na(input$set_num) & !is.null(input$set_num)){set.id <- paste0(trip.id(),"_",input$set_num)
      }else{
        warning("No Set ID Found!")
        showNotification("No SET ID FOUND!", type = "error")
        continue = F
      }
    }
    if(continue){
      if(!is.null(set.id) & !is.na(input$trap_num) & !is.null(input$trap_num)){trap.id <- paste0(trip.id(),"_",input$set_num,"_",input$trap_num)
      }else{
        warning("No Trap ID Found!")
        showNotification("No TRAP ID FOUND!", type = "error")
        continue = F
      }
    }

    if(continue){
      sure.delete <- dlgMessage(type = "yesno", message = "Are you sure you want to delete all trap information and fish caught for this trap?")
      if(sure.delete$res %in% "yes"){
      delay(10,{ ## give delay to make sure all relational variables are properly set before updating db
        # Initialize database connection
        db <- dbConnect(RSQLite::SQLite(), paste0(dat.dir,"/",trip.id,".db"))

        ## delete all trap and fish infor for trap
        update.trap(db,set.id = set.id, trap.id = trap.id, delete = T)

        dbDisconnect(db)

        ## resetting trap number will clear all downstream fields since set data is now deleted
        delay(5,{
          current.trap <- input$trap_num
          updateNumericInput(session, "trap_num", value = NA)
          delay(5,{
            updateNumericInput(session, "trap_num", value = current.trap)
          })
        })

      })
        }
    }

  }) ## observEvent block (delete)


  ## SUBMIT LEVEL 2
  ## when "next_set" is clicked
  observeEvent(input$next_set, {

    ### Currently, next_set works exactly the same as next_trap (all upstream and downstream data visible in user's view is updated in db)
    ### EXCEPT: A Trap ID is not required

    ## define IDs for relational columns
    set.id <- NULL
    trap.id <- NULL
    trip.id <- trip.id()
    continue = T
    if(is.null(trip.id)){
      warning("No TRIP ID Found!")
      showNotification("No TRIP ID FOUND!", type = "error")
      continue = F
    }
    if(continue){
      if(!is.null(trip.id) & !is.na(input$set_num) & !is.null(input$set_num)){set.id <- paste0(trip.id(),"_",input$set_num)
      }else{
        warning("No Set ID Found!")
        showNotification("No SET ID FOUND!", type = "error")
        continue = F
      }
    }
    if(continue){
      if(!is.null(set.id) & !is.na(input$trap_num) & !is.null(input$trap_num)){trap.id <- paste0(trip.id(),"_",input$set_num,"_",input$trap_num)
      }else{
        ## warning("No Trap ID Found!")
        ## continue = F
      }
    }

    ## if set is being saved with no current trap/bait code, check if there are any traps in data for the set being saved
    if(continue){
      # Initialize database connection
      db <- dbConnect(RSQLite::SQLite(), paste0(dat.dir,"/",trip.id,".db"))

      if(is.null(input$trap_num) | is.na(input$trap_num) | is.null(input$bait_code) | is.na(input$bait_code)){
        check.for.traps <- paste("SELECT * FROM TRAP_INFO WHERE FISHSET_ID = '",set.id, "'", sep = "")
        for.traps <- dbGetQuery(db, check.for.traps)
        dbDisconnect(db)

        if(nrow(for.traps)==0){
          save.set <- dlgMessage(type = "yesno", message = "No traps found in data or on-screen for this set, do you want to save this empty set? If you don't want to save but just want to check on later sets, just change number in SET/TRAWL/STRING field. ")
          if(save.set$res %in% "no"){
            continue = F
          }
        }
      }
    }

    if(continue){
      delay(10, {
        # Initialize database connection
        db <- dbConnect(RSQLite::SQLite(), paste0(dat.dir,"/",trip.id,".db"))

        ## update upstream data
        update.trip(db, trip.id)
        ## Update downstream data
        update.set(db, trip.id = trip.id, set.id = set.id)
        if(!is.null(trap.id)){         ## vv downstream
          if(!is.na(input$bait_code) && !is.null(input$bait_code)){ ## don't upload blank traps
            update.trap(db,set.id = set.id, trap.id = trap.id)
            update.fish(db, trap.id = trap.id)
          }
        }

        #######################
        ## as a last cleanup step, convert any character 'NA' values that got introduced to sql NULL to unify missing values format
        tables <- c("TRIP_INFO", "SET_INFO", "TRAP_INFO", "FISH_INFO")

        for (table in tables) {
          col_info <- dbGetQuery(db, paste0("PRAGMA table_info(", table, ");"))
          for (col in col_info$name) {
            query <- paste0("UPDATE ", table, " SET ", col, " = NULL WHERE ", col, " = 'NA'")
            dbExecute(db, query)
          }
        }
        #################################

        dbDisconnect(db)

        ## move to next set number
        updateNumericInput(session, "set_num", value = input$set_num+1)
      })
    }

  })

  ## back button for set clicked (inverse operation of next set button) ## Going backwards does not update database
  observeEvent(input$prev_set, {

    ## going backwards for Set has same saving effect as going forward

    ## define IDs for relational columns
    set.id <- NULL
    trap.id <- NULL
    trip.id <- trip.id()
    continue = T
    if(is.null(trip.id)){
      warning("No TRIP ID Found!")
      showNotification("No TRIP ID FOUND!", type = "error")
      continue = F
    }
    if(continue){
      if(!is.null(trip.id) & !is.na(input$set_num) & !is.null(input$set_num)){set.id <- paste0(trip.id(),"_",input$set_num)
      }else{
        warning("No Set ID Found!")
        showNotification("No SET ID FOUND!", type = "error")
        continue = F
      }
    }
    if(continue){
      if(!is.null(set.id) & !is.na(input$trap_num) & !is.null(input$trap_num)){trap.id <- paste0(trip.id(),"_",input$set_num,"_",input$trap_num)
      }else{
        ## warning("No Trap ID Found!")
        ## continue = F
      }
    }

    ## if set is being saved with no current trap/bait code, check if there are any traps in data for the set being saved
    if(continue){
      # Initialize database connection
      db <- dbConnect(RSQLite::SQLite(), paste0(dat.dir,"/",trip.id,".db"))

      if(is.null(input$trap_num) | is.na(input$trap_num) | is.null(input$bait_code) | is.na(input$bait_code)){
        check.for.traps <- paste("SELECT * FROM TRAP_INFO WHERE FISHSET_ID = '",set.id, "'", sep = "")
        for.traps <- dbGetQuery(db, check.for.traps)
        dbDisconnect(db)

        if(nrow(for.traps)==0){
          save.set <- dlgMessage(type = "yesno", message = "No traps found in data or on-screen for this set, do you want to save this empty set? If you don't want to save but just want to check on previous sets, just change number in SET/TRAWL/STRING field. ")
          if(save.set$res %in% "no"){
            continue = F
          }
        }
      }
      }

    if(continue){
      delay(10, {
        # Initialize database connection
        db <- dbConnect(RSQLite::SQLite(), paste0(dat.dir,"/",trip.id,".db"))
        ## update upstream data
        update.trip(db, trip.id)
        ## Update downstream data
        update.set(db, trip.id = trip.id, set.id = set.id)
        if(!is.null(trap.id)){         ## vv downstream
          if(!is.na(input$bait_code) && !is.null(input$bait_code)){ ## don't upload blank traps
            update.trap(db,set.id = set.id, trap.id = trap.id)
            update.fish(db, trap.id = trap.id)
          }
        }

        #######################
        ## as a last cleanup step, convert any character 'NA' values that got introduced to sql NULL to unify missing values format
        tables <- c("TRIP_INFO", "SET_INFO", "TRAP_INFO", "FISH_INFO")

        for (table in tables) {
          col_info <- dbGetQuery(db, paste0("PRAGMA table_info(", table, ");"))
          for (col in col_info$name) {
            query <- paste0("UPDATE ", table, " SET ", col, " = NULL WHERE ", col, " = 'NA'")
            dbExecute(db, query)
          }
        }
        #################################

        dbDisconnect(db)

        if(!is.na(input$set_num) & input$set_num>0){
          updateNumericInput(session, "set_num", value = input$set_num-1)
        }

      })
    }

  })

  ## When 'Delete Set' button is clicked (Currently works the same as Delete Trap but includes upstream Set data)
  observeEvent(input$delete_set, {
    ## define IDs for relational columns
    set.id <- NULL
    trap.id <- NULL
    trip.id <- trip.id()
    continue = T
    if(is.null(trip.id)){
      warning("No TRIP ID Found!")
      showNotification("No TRIP ID FOUND!", type = "error")
      continue = F
    }
    if(continue){
      if(!is.null(trip.id) & !is.na(input$set_num) & !is.null(input$set_num)){set.id <- paste0(trip.id(),"_",input$set_num)
      }else{
        warning("No Set ID Found!")
        showNotification("No SET ID FOUND!", type = "error")
        continue = F
      }
    }
    if(continue){
      if(!is.null(set.id) & !is.na(input$trap_num) & !is.null(input$trap_num)){trap.id <- paste0(trip.id(),"_",input$set_num,"_",input$trap_num)
      }else{
        # warning("No Trap ID Found!")
        # showNotification("No TRAP ID FOUND!", type = "error")
        # continue = F
      }
    }

    if(continue){
      sure.delete <- dlgMessage(type = "yesno", message = "Are you sure you want to delete all information for this set? All downstream trap and fish data for this set will also be deleted.")
      if(sure.delete$res %in% "yes"){
        delay(10,{ ## give delay to make sure all relational variables are properly set before updating db
          # Initialize database connection
          db <- dbConnect(RSQLite::SQLite(), paste0(dat.dir,"/",trip.id,".db"))

          ## delete all set, trap and fish infor for trap
          update.set(db, trip.id = trip.id, set.id = set.id, delete = T)
          update.trap(db,set.id = set.id, trap.id = trap.id, delete = T)

          dbDisconnect(db)

          ## resetting set number will clear all downstream fields since set data is now deleted
          delay(5,{
            current.set <- input$set_num
            updateNumericInput(session, "set_num", value = NA)
            delay(5,{
              updateNumericInput(session, "set_num", value = current.set)
            })
          })

        })
      }
    }

  }) ## observEvent block (delete)



  ## SUBMIT LEVEL 3
  ### When Trip is submitted
  observeEvent(input$submit_trip, {

    ## save everything on the screen when this is clicked
    ## define IDs for relational columns
    set.id <- NULL
    trap.id <- NULL
    trip.id <- trip.id()
    continue = T
    if(is.null(trip.id)){
      warning("No TRIP ID Found! No Data Saved!")
      showNotification("No TRIP ID FOUND! Please Complete TRIP INFO!", type = "error")
      continue = F
    }
    if(continue){
      if(!is.null(trip.id) & !is.na(input$set_num) & !is.null(input$set_num)){set.id <- paste0(trip.id(),"_",input$set_num)
      }else{
        # warning("No Set ID Found!")
        # showNotification("No SET ID FOUND!", type = "error")
        # continue = F
      }
    }
    if(continue){
      if(!is.null(set.id) & !is.na(input$trap_num) & !is.null(input$trap_num)){trap.id <- paste0(trip.id(),"_",input$set_num,"_",input$trap_num)
      }else{
        # warning("No Trap ID Found!")
        # showNotification("No TRAP ID FOUND!", type = "error")
        # continue = F
      }
    }
    delay(10,{ ## give delay to make sure all relational variables are properly set before updating db
    if(continue){
        # Initialize database connection
        db <- dbConnect(RSQLite::SQLite(), paste0(dat.dir,"/",trip.id,".db"))
        ## Update trip always
        update.trip(db, trip.id)
        ## ask user if they want to save the onscreen data
        save.set <- dlgMessage(type = "yesno", message = "Saving TRIP INFO. Do you want to save the current (on-screen) SET INFO? Any baited traps and fish currently on-screen will also be saved.")
        if(save.set$res %in% "yes"){
          update.set(db, trip.id = trip.id, set.id = set.id)
          if(!is.na(input$bait_code) && !is.null(input$bait_code)){ ## don't upload blank traps
            update.trap(db,set.id = set.id, trap.id = trap.id)
            update.fish(db, trap.id = trap.id)
          }
          }

      ## as a last cleanup step, convert any character 'NA' values that got introduced to sql NULL to unify missing values format
      tables <- c("TRIP_INFO", "SET_INFO", "TRAP_INFO", "FISH_INFO")

      for (table in tables) {
        col_info <- dbGetQuery(db, paste0("PRAGMA table_info(", table, ");"))
        for (col in col_info$name) {
          query <- paste0("UPDATE ", table, " SET ", col, " = NULL WHERE ", col, " = 'NA'")
          dbExecute(db, query)
        }
      }

      print(paste0("Trip Entered. The data for your trip is in ",dat.dir,"/",trip.id,".db"))
      print("Use check.table() to view the data tables.")
      dbDisconnect(db)
      stopApp()
    }
    }) #delay
  }) ### observEvent block



}) ## suppress warnings


  ################################################################################################
  ###############################################################################################
  ################ ERROR CHECKING


#########  REAL TIME ######################################################################


#### TRIP ROWS

## 1 Boarding Date
## 1:1 No future dates
  observeEvent(input$board_date,{
    hideFeedback("board_date")
    checks$check1 <- T
    req(input$board_date)
    if(input$board_date > Sys.Date()){
      showFeedbackDanger("board_date", "Dates can't be in the future!")
      checks$check1 <- F
    }
  },ignoreInit = T)

## 2 Landing Date
## 2:1 No future dates
  observeEvent(input$land_date,{
    hideFeedback("land_date")
    checks$check2 <- T
    req(input$land_date)
    if(!is.null(input$land_date) && !is.na(input$land_date)){
      if(input$land_date > Sys.Date()){
        showFeedbackDanger("land_date", "Dates can't be in the future!")
        checks$check2 <- F
      }
    }

  },ignoreInit = T)


## 3 Vessel Reg #
##3:1 range  0 - 999999 (violation impossible)
  observe({
    runjs('
    $("#vessel_num").on("input", function() {
      var value = $(this).val();
      // Remove non-numeric characters (including negative sign)
      value = value.replace(/[^0-9]/g, "");
      // Limit input to 6 digits
      if (value.length > 6) {
        value = value.substring(0, 6);
      }
      $(this).val(value);
    });
  ')
  })

## 4 Vessel Name
## 4:1 range < 40 characters (violation impossible)
  observe({
    runjs('
      $("#vessel_name").on("input", function() {
        var value = $(this).val();
        if (value.length > 40) {
          $(this).val(value.substring(0, 40));  // Limit input to 40 characters
        }
      });
    ')
  })

## 5 License #
## 5:1 range  0 - 999999999 (violation impossible)
  observe({
    runjs('
    $("#license_num").on("input", function() {
      var value = $(this).val();
      // Remove non-numeric characters (including negative sign)
      value = value.replace(/[^0-9]/g, "");
      // Limit input to 9 digits
      if (value.length > 9) {
        value = value.substring(0, 9);
      }
      $(this).val(value);
    });
  ')
  })

## 6 Captain Name
## 6:1 range < 40 characters (violation impossible)
  observe({
    runjs('
      $("#captain_name").on("input", function() {
        var value = $(this).val();
        if (value.length > 40) {
          $(this).val(value.substring(0, 40));  // Limit input to 40 characters
        }
      });
    ')
  })

## 7 Sampler Name
## 7:1 range < 40 characters (violation impossible)
  observe({
    runjs('
      $("#sampler_name").on("input", function() {
        var value = $(this).val();
        if (value.length > 40) {
          $(this).val(value.substring(0, 40));  // Limit input to 40 characters
        }
      });
    ')
  })

## 8 LFA
## 8:1  Should have value (Warning)
  observeEvent(input$lfa, {
    if(input$lfa %in% c(NA,NULL,"")){
      showFeedbackWarning("lfa", "Warning: No LFA Chosen!")
    }else{
      hideFeedback("lfa")
    }
  })

## 9 Data Entry Group
## 9:1  range = Dropdown list (violation impossible)

## 10 Data Entry Name
## 10:1  range < 40 characters (violation impossible)
  observe({
    runjs('
      $("#entry_name").on("input", function() {
        var value = $(this).val();
        if (value.length > 40) {
          $(this).val(value.substring(0, 40));  // Limit input to 40 characters
        }
      });
    ')
  })

## 11 Data Entry Date
## 11:1 No Restrictions

## 12 Trip
## 12:1 Must have Value
  observeEvent(input$trip_code, {
      if(input$trip_code %in% c(NA,NULL,"")){
        showFeedbackDanger("trip_code", "Error: No Trip Code!")
        checks$check12 <- F
      }else{
        hideFeedback("trip_code")
        checks$check12 <- T
      }
  }, ignoreInit = T)


########## SET ROW

## 13 Set/Trawl/String
## 13:1 Range >=0 (violation impossible) and give warning if there are any unsaved downstream changes
  observe({
    runjs('
    $("#set_num").on("input", function() {
      var value = $(this).val();
      // Remove non-numeric characters (including negative sign)
      value = value.replace(/[^0-9]/g, "");
      $(this).val(value);
    });
  ')
  })

  ## 13:2 give warning if set data has changed since the last save
  observeEvent(list(input$set_num, input$num_traps, input$lat,input$lon, input$grid_num, input$depth, input$soak_days, input$trap_type, input$vent_size, input$num_vents),{
    na.set.fields <- c(input$num_traps, input$lat, input$lon, input$grid_num, input$depth, input$soak_days, input$trap_type, input$vent_size, input$num_vents)
    unsaved.set(F)
    trip.id <- trip.id()
    if(!is.null(trip.id) & !is.na(input$set_num) & !is.null(input$set_num)){
      set.id <- paste0(trip.id(),"_",input$set_num)
      db <- dbConnect(RSQLite::SQLite(), paste0(dat.dir,"/",trip.id,".db"))
      check.this.set <- paste("SELECT * FROM SET_INFO WHERE FISHSET_ID = '",set.id, "'", sep = "")
      cs <- dbGetQuery(db, check.this.set)
      if(nrow(cs)>0){

        ## in case any non proper NA values get in through data table, clean these to NA to avoid crashing
        cs[] <- lapply(cs, function(x) {
          # Convert factors to character first to avoid level issues
          if (is.factor(x)) x <- as.character(x)

          # Replace empty strings, "NA", or "NULL" with proper NA
          x[x %in% c("", "NA", "NULL")] <- NA

          return(x)
        })

        if (is_different(input$num_traps,  cs$NUM_TRAPS))   unsaved.set(TRUE)
        if (is_different(input$lat,        cs$LATDDMM))    unsaved.set(TRUE)
        if (is_different(input$lon,        cs$LONGDDMM))   unsaved.set(TRUE)
        if (is_different(input$grid_num,   cs$STRATUM_ID))  unsaved.set(TRUE)
        if (is_different(input$depth,      cs$DEPTH))       unsaved.set(TRUE)
        if (is_different(input$soak_days,  cs$SOAK_DAYS))   unsaved.set(TRUE)
        if (is_different(input$trap_type,  cs$TRAP_TYPE))   unsaved.set(TRUE)
        if (is_different(input$vent_size,  cs$VENT_CD))     unsaved.set(TRUE)
        if (is_different(input$num_vents,  cs$NUM_VENTS))   unsaved.set(TRUE)

      }else{
        if(any(!na.set.fields %in% NA)){unsaved.set(TRUE)}
      }

      dbDisconnect(db)
    }

  }, ignoreInit = TRUE)



## 14 #Traps in Set
## 14:1 Range >=0 (violation impossible)
  observe({
    runjs('
    $("#num_traps").on("input", function() {
      var value = $(this).val();
      // Remove non-numeric characters (including negative sign)
      value = value.replace(/[^0-9]/g, "");
      $(this).val(value);
    });
  ')
  })

## 15 Latitude
## 15:1 Range + Coordinate should fall within selected LFA and Grid Number (with some margin for error)
## 16 Longitude
## 16:1 Range + Coordinate should fall within selected LFA and Grid Number (with some margin for error)
  observeEvent(list(input$lon, input$lat, input$lfa, input$grid_num),{
    hideFeedback("lon")
    hideFeedback("lat")
    checks$check16 <- T
    if(!input$lon %in% c(NULL,NA) && !input$lat %in% c(NULL,NA) &&
       (input$lon>17959 | input$lon< -17959 | input$lat > 8959.99| input$lat < -8959.99)){
      hideFeedback("lon")
      hideFeedback("lat")
      showFeedbackDanger("lon","Not a valid coordinate!")
      showFeedbackDanger("lat","Not a valid coordinate!")
      checks$check16 <- F
    }else{
      if(!input$lon %in% c(NULL,NA) && !input$lat %in% c(NULL,NA)){
        # convert coordinate format to decimal degrees
        # lon1 <- floor(input$lon)
        # lon2 <- round(input$lon - lon1,2)
        # londeg <- lon1 %/% 100
        # lonmin <- lon1 %% 100
        # lonmin <- lonmin+lon2
        # lonmin <- lonmin/60
        # lon <- -(londeg+lonmin)
        # print(lon)
        #
        # lat1 <- floor(input$lat)
        # lat2 <- round(input$lat - lat1,2)
        # latdeg <- lat1 %/% 100
        # latmin <- lat1 %% 100
        # latmin <- latmin+lat2
        # latmin <- latmin/60
        # lat <- latdeg+latmin
        # print(lat)

        lon <- format(input$lon, scientific = FALSE)
        londeg.txt <- substr(lon, 1, 2)
        londeg <- as.numeric(londeg.txt)
        lonmin.txt <- substr(lon, 3, nchar(lon))
        lonmin <- round(as.numeric(lonmin.txt),2)
        lonmin <- lonmin/60
        lon <-  -(londeg+lonmin)

        lat <- format(input$lat, scientific = FALSE)
        latdeg.txt <- substr(lat, 1, 2)
        latdeg <- as.numeric(latdeg.txt)
        latmin.txt <- substr(lat, 3, nchar(lat))
        latmin <- round(as.numeric(latmin.txt),2)
        latmin <- latmin/60
        lat <-  latdeg+latmin

        showFeedback("lon",paste0(londeg.txt,"° ",lonmin.txt,"' W"))
        showFeedback("lat",paste0(latdeg.txt,"° ",latmin.txt,"' N"))

        if(!lon %in% c(NULL,NA) && !lat %in% c(NULL,NA) && !input$lfa %in% c(NULL,NA,"")){
          point <-  st_sfc(st_point(c(lon, lat)), crs = st_crs(lfapolys))
          point_sf <- st_sf(geometry = point)
          chose.lfa <- lfapolys %>% filter(LFA %in% input$lfa)
          lfa.result <- any(st_within(point_sf, chose.lfa, sparse = FALSE))
          lfa.pass <- T ## set lfa pass until proven failed
          if(lfa.result %in% FALSE){
            ## check if the point is very nearby the border before throwing a warning
              l.dist_m <- st_distance(point_sf, chose.lfa)
              ld <- as.numeric(min(l.dist_m))
              if(ld>2000){ ## 2km allowed margin of error
                hideFeedback("lon")
                hideFeedback("lat")
                showFeedbackWarning("lon","Coordinates do not seem to fall within chosen LFA! Please check!")
                showFeedbackWarning("lat","Coordinates do not seem to fall within chosen LFA! Please check!")
                lfa.pass <- F
              }
          }
            ## if passed LFA test, then check grid
            if(lfa.pass && !lon %in% c(NULL,NA) && !lat %in% c(NULL,NA) && !input$lfa %in% c(NULL,NA,"") && !input$grid_num %in% c(NULL,NA)){
              chose.grid <- gridpolys %>% filter(LFA %in% input$lfa, GRID_NO %in% input$grid_num)
              grid.result <- any(st_within(point_sf, chose.grid, sparse = FALSE))
              if(grid.result %in% FALSE){
                ## check if the point is very nearby the border before throwing a warning
                g.dist_m <- st_distance(point_sf, chose.grid)
                gd <- as.numeric(min(g.dist_m))
                if(gd>200){ ## 200m allowed margin of error
                  hideFeedback("lon")
                  hideFeedback("lat")
                  showFeedbackWarning("lon","Coordinates do not seem to fall within chosen Grid! Please check!")
                  showFeedbackWarning("lat","Coordinates do not seem to fall within chosen Grid! Please check!")
                }
              }
            }
        }
      }
    }
  }, ignoreInit = T)


## 17 Grid No
## 17:1 range (Grid number should fall within selected LFA)
  observeEvent(list(input$grid_num,input$lfa),{
    hideFeedback("grid_num")
    checks$check17 <- T
    if(!input$grid_num %in% c(NULL,NA) && !input$lfa %in% c(NULL,NA,"")){
      chose.lfa <- gridpolys %>% filter(LFA %in% input$lfa)
      if(!input$grid_num %in% chose.lfa$GRID_NO){
        showFeedbackDanger("grid_num","Grid Number is not in the chosen LFA")
        checks$check17 <- F
      }
    }
  }, ignoreInit = T)

## 18 Depth (FM)
## 18:1 range and warning if > 200 m
observeEvent(input$depth,{
  hideFeedback("depth")
  checks$check18<- T
  if(!input$depth %in% c(NA,NULL,"") & (input$depth < 0 | input$depth > 300)){
    hideFeedback("depth")
    showFeedbackDanger("depth", "Depth must be between 0 and 300 m")
    checks$check18<- F
  }else{
    if(!input$depth %in% c(NA,NULL,"") & input$depth>200){
      showFeedbackWarning("depth","Warning: are you sure it was that deep?")
      checks$check18<- T
    }
  }
}, ignoreInit = T)

## 19 Soak Days
## 19:1 range and warning if > 15 days
observeEvent(input$soak_days,{
  if(!input$soak_days %in% c(NA,NULL,"") & (input$soak_days < 0 | input$soak_days > 30)){
    hideFeedback("soak_days")
    showFeedbackDanger("soak_days", "Must be between 0 and 30 days")
    checks$check19<- F
  }else{
    if(!input$soak_days %in% c(NA,NULL,"") & input$soak_days>15){
      showFeedbackWarning("soak_days","Warning: was soak time really that long?")
      checks$check19<- T
    }else{
      hideFeedback("soak_days")
      checks$check19<- T
    }

  }
}, ignoreInit = T)

## 20 Trap Type
## 20: 1 range 1-4
observeEvent(input$trap_type,{
  if(!input$trap_type %in% c(NA,NULL,"") & (input$trap_type < 0 | input$trap_type > 4)){
    hideFeedback("trap_type")
    showFeedbackDanger("trap_type", "Valid trap type codes are 1-4")
    checks$check20<- F
  }else{
    hideFeedback("trap_type")
    checks$check20<- T
    if(!input$trap_type %in% c(NA,NULL,"")){ ## if error is cleared but there's still a value, need to replenish the lookup table name
      t.type <- code.tab$Name[which(code.tab$Field %in% "Trap Type" & code.tab$Code %in% input$trap_type)]
      showFeedback("trap_type",t.type)
    }
  }
  })

## 21 Vent Code
## 21: 1 range 1-5
observeEvent(input$vent_size,{
  if(!input$vent_size %in% c(NA,NULL,"") & (input$vent_size < 0 | input$vent_size > 5)){
    hideFeedback("vent_size")
    showFeedbackDanger("vent_size", "Valid vent codes are 1-5")
    checks$check21<- F
  }else{
    hideFeedback("vent_size")
    checks$check21<- T
    if(!input$vent_size %in% c(NA,NULL,"")){ ## if error is cleared but there's still a value, need to replenish the lookup table name
      v.size <- code.tab$Name[which(code.tab$Field %in% "Vent Size" & code.tab$Code %in% input$vent_size)]
      showFeedback("vent_size",v.size)
    }
  }
})

## 22 # of Vents
## 22:1 range >=0 (Violation Impossible)
observe({
  runjs('
    $("#num_vents").on("input", function() {
      var value = $(this).val();
      // Remove non-numeric characters (including negative sign)
      value = value.replace(/[^0-9]/g, "");
      $(this).val(value);
    });
  ')
})


######## TRAP ROW


## 23 Trap No
## 23:1 range >=0 (Violation impossible). And must have value if downstream fields have values (can rely on bait cd and spec code)
observe({
  runjs('
    $("#trap_num").on("input", function() {
      var value = $(this).val();
      // Remove non-numeric characters (including negative sign)
      value = value.replace(/[^0-9]/g, "");
      $(this).val(value);
    });
  ')
})
observeEvent(list(input$num_traps,input$trap_num,input$bait_code,input$spec_code_row_1),{
  hideFeedback("trap_num")
  checks$check23 <- T
  if(is.na(input$trap_num) & (!input$bait_code %in% c(NULL,NA,"") | !input$spec_code_row_1 %in% c(NULL,NA,""))){
    showFeedbackDanger("trap_num", "Error: No Trap Number Selected")
    checks$check23 <- F
  }
  # else{
  #   if(!input$trap_num %in% c(NULL,NA) && !input$num_traps %in% c(NULL,NA)){
  #     if(input$trap_num>input$num_traps){
  #       showFeedbackWarning("trap_num", "Trap number is higher than #Traps in Set")
  #       checks$check23 <- T
  #     }
  #   }
  # }
}, ignoreInit = T)

## 23: 2 give warning in trap_num if user has made unsaved changes to fish or trap data

## function for checking differences between fields and database variables (used for fields 13 and 23)
is_different <- function(a, b) {
  #print(c(a,b))
  ## check if one value is missing
  if (is.na(a) && is.na(b)) return(FALSE)
  if (is.na(a) && !is.na(b)) return(TRUE)
  if (!is.na(a) && is.na(b)) return(TRUE)

  # Otherwise, see if values are the same
  return(a != b)
}

## TRAP save check
observeEvent(list(input$trap_num, input$bait_code,input$bait_code2, input$bait_code3, input$bait_type1, input$bait_type2, input$bait_type3),{
  na.trap.fields <- c(input$bait_code,input$bait_code2, input$bait_code3, input$bait_type1, input$bait_type2, input$bait_type3)
  unsaved.trap(F)
  trip.id <- trip.id()

  if(!is.null(trip.id) & !is.na(input$set_num) & !is.null(input$set_num)){
    set.id <- paste0(trip.id(),"_",input$set_num)}

  if(!is.null(set.id) & !is.na(input$trap_num) & !is.null(input$trap_num)){
    trap.id <- paste0(trip.id(),"_",input$set_num,"_",input$trap_num)

    db <- dbConnect(RSQLite::SQLite(), paste0(dat.dir,"/",trip.id,".db"))
    check.this.trap <- paste("SELECT * FROM TRAP_INFO WHERE TRAP_ID = '",trap.id, "'", sep = "")
    ct <- dbGetQuery(db, check.this.trap)
    dbDisconnect(db)

    if(nrow(ct)>0){

      ## in case any non proper NA values get in through data table, clean these to NA to avoid crashing
      ct[] <- lapply(ct, function(x) {
        # Convert factors to character first to avoid level issues
        if (is.factor(x)) x <- as.character(x)

        # Replace empty strings, "NA", or "NULL" with proper NA
        x[x %in% c("", "NA", "NULL")] <- NA

        return(x)
      })

      if (is_different(input$bait_code,  ct$BAIT_CD))   unsaved.trap(TRUE)
      if (is_different(input$bait_code2,  ct$BAIT_CD2))   unsaved.trap(TRUE)
      if (is_different(input$bait_code3,  ct$BAIT_CD3))   unsaved.trap(TRUE)
      if (is_different(input$bait_type1,  ct$BAIT_TYPE1))   unsaved.trap(TRUE)
      if (is_different(input$bait_type2,  ct$BAIT_TYPE2))   unsaved.trap(TRUE)
      if (is_different(input$bait_type3,  ct$BAIT_TYPE3))   unsaved.trap(TRUE)

    }else{
      if(any(!na.trap.fields %in% NA)){unsaved.trap(TRUE)}
    }

  }

}, ignoreInit = TRUE)

### FISH save check (harder)
observe({
  current_rows_decoy <- row_ids() ## buy delay
  delay(1000,{
    current_rows <- row_ids()
    current_rows <- current_rows[-length(current_rows)]
    lapply(current_rows, function(row_id) {
      if(!is.na(input[[paste0("spec_code_", row_id)]])){
        # Wrap the lists of all fields in a reactive so dependencies are tracked
        fish.fields <- reactive({
          list(input[[paste0("trap_num_", row_id)]], input[[paste0("spec_code_", row_id)]], input[[paste0("common_", row_id)]], input[[paste0("length_", row_id)]], input[[paste0("sex_", row_id)]],
               input[[paste0("shell_", row_id)]], input[[paste0("cond_", row_id)]], input[[paste0("disease_", row_id)]], input[[paste0("egg_", row_id)]],
               input[[paste0("clutch_", row_id)]], input[[paste0("vnotch_", row_id)]], input[[paste0("kept_", row_id)]], input[[paste0("abund_", row_id)]]
          )})
        na.fish.fields <- reactive({ ## seprate list of only the fish fields that are supposed to be NA (don't have default starting values)
          list(input[[paste0("spec_code_", row_id)]], input[[paste0("length_", row_id)]], input[[paste0("sex_", row_id)]],
               input[[paste0("shell_", row_id)]], input[[paste0("cond_", row_id)]], input[[paste0("disease_", row_id)]], input[[paste0("egg_", row_id)]],
               input[[paste0("clutch_", row_id)]], input[[paste0("vnotch_", row_id)]], input[[paste0("kept_", row_id)]]
          )})

        observeEvent(fish.fields(), {

          unsaved.fish(F)
          na.fish.fields <- unlist(na.fish.fields())

          row.num <- as.numeric(gsub("\\D", "", row_id))
          trip.id <- trip.id()
          set.id <- NULL

          ########## check unsaved trap and fish  data
          if(!is.null(trip.id) & !is.na(input$set_num) & !is.null(input$set_num)){
            set.id <- paste0(trip.id(),"_",input$set_num)
            }

          if(!is.null(set.id) & !is.na(input$trap_num) & !is.null(input$trap_num)){
            trap.id <- paste0(trip.id(),"_",input$set_num,"_",input$trap_num)

            db <- dbConnect(RSQLite::SQLite(), paste0(dat.dir,"/",trip.id,".db"))
            check.fish.table <- paste("SELECT * FROM FISH_INFO WHERE TRAP_ID = '",trap.id, "'", sep = "")
            check.fish.result <- dbGetQuery(db, check.fish.table)
            dbDisconnect(db)
            cf <- check.fish.result %>% arrange(as.numeric(FISH_NO)) ## make sure fish data is sorted by fish number (because this is equivalent to row# in the app)

            if(nrow(cf)>0){
              ## in case any non proper NA values get in through data table, clean these to NA to avoid crashing
              cf[] <- lapply(cf, function(x) {
                if (is.factor(x)) x <- as.character(x)
                x[x %in% c("", "NA", "NULL")] <- NA
                return(x)
              })


              if (is_different(input[[paste0("spec_code_", row_id)]],  cf$SPECCD_ID[row.num]))   unsaved.fish(TRUE)
              if (is_different(input[[paste0("common_", row_id)]],        cf$COMMON[row.num]))    unsaved.fish(TRUE)
              if (is_different(input[[paste0("length_", row_id)]],        cf$FISH_LENGTH[row.num]))   unsaved.fish(TRUE)
              if (is_different(input[[paste0("sex_", row_id)]],   cf$SEXCD_ID[row.num]))  unsaved.fish(TRUE)
              if (is_different(input[[paste0("shell_", row_id)]],      cf$SHELL[row.num]))       unsaved.fish(TRUE)
              if (is_different(input[[paste0("cond_", row_id)]],  cf$CONDITION[row.num]))   unsaved.fish(TRUE)
              if (is_different(input[[paste0("disease_", row_id)]],  cf$DISEASE[row.num]))   unsaved.fish(TRUE)
              if (is_different(input[[paste0("egg_", row_id)]],  cf$EGG_STAGE[row.num]))     unsaved.fish(TRUE)
              if (is_different(input[[paste0("clutch_", row_id)]],  cf$CLUTCH[row.num]))   unsaved.fish(TRUE)
              if (is_different(input[[paste0("vnotch_", row_id)]],  cf$VNOTCH[row.num]))   unsaved.fish(TRUE)
              if (is_different(input[[paste0("kept_", row_id)]],  cf$KEPT[row.num]))   unsaved.fish(TRUE)
              if (is_different(input[[paste0("abund_", row_id)]],  cf$ABUNDANCE[row.num]))   unsaved.fish(TRUE)

            }else{
              if(any(!na.fish.fields %in% NA)){unsaved.fish(TRUE)}
            }
          }
        }, ignoreInit = TRUE)
      }
    })
  }) ## delay
})

## this observer coordinates saving warnings for both fields (23:trap and 13:set num)
observe({
  unsaved.fish <- unsaved.fish()
  unsaved.trap <- unsaved.trap()
  unsaved.set <- unsaved.set()
  if(unsaved.fish){
    hideFeedback("set_num")
    hideFeedback("trap_num")
    showFeedbackWarning("trap_num", "There are UNSAVED fish data for this trap! Save before changing trap", color = "purple")
    showFeedbackWarning("set_num", "There are UNSAVED fish data for this set! Save before changing set", color = "purple")
  }
  if(!unsaved.fish & unsaved.trap){
    hideFeedback("trap_num")
    showFeedbackWarning("trap_num", "There is UNSAVED trap info for this trap! Save before changing trap", color = "purple")
  }
  if(!unsaved.fish & unsaved.set){
    hideFeedback("set_num")
    showFeedbackWarning("set_num", "There is UNSAVED set info for this set! Save before changing set", color = "purple")
  }
  if(!unsaved.trap & !unsaved.fish){
    hideFeedback("trap_num")
  }
  if(!unsaved.fish & !unsaved.set & unsaved.trap){
    hideFeedback("set_num")
    showFeedbackWarning("set_num", "There is UNSAVED trap info for this set! Save before changing set", color = "purple")
  }
  if(!unsaved.set & !unsaved.trap & !unsaved.fish){
    hideFeedback("set_num")
    hideFeedback("trap_num")
  }
})

## 24  bait code
## 24:1 Range and must have a value if there are fish catch data
  observeEvent(list(input$bait_code,input$spec_code_row_1),{
      if (input$bait_code %in% c("",NA,NULL) & !input$spec_code_row_1 %in% c(NULL,NA,"")) {
        hideFeedback("bait_code")
        showFeedbackDanger("bait_code", "To submit data for this trap, it must have a bait code!")
        checks$check24<- F
      }else{
        hideFeedback("bait_code")
        ## need to return species name if it's still needed
        if(!input$bait_code %in% c("",NA,NULL)){
          new.bait <- input$bait_code
          b.spec <- spec.tab$COMMON[which(spec.tab$SPECIES_CODE %in% new.bait)]
          showFeedback("bait_code",b.spec)
        }
        checks$check24<- T
      }
      }, ignoreInit = T)

## 25 bait code 2
## 25:1 should only be values in bait code 2 if bait code has values
  observe({
    if (input$bait_code %in% c("",NA,NULL) & !input$bait_code2 %in% c("",NA,NULL)) {
      hideFeedback("bait_code2")
      showFeedbackDanger("bait_code2", "No BAIT CD Entered")
      checks$check25<- F
    }else{
      hideFeedback("bait_code2")
      ## need to return species name if it's still needed
      if(!input$bait_code2 %in% c("",NA,NULL)){
        new.bait2 <- input$bait_code2
        b.spec2 <- spec.tab$COMMON[which(spec.tab$SPECIES_CODE %in% new.bait2)]
        showFeedback("bait_code2",b.spec2)
      }
      checks$check25<- T
    }
  })

## 26 bait code 3
## 26:1 should only be values in bait code 3 if bait code2 has values
  observe({
    if (input$bait_code2 %in% c("",NA,NULL) & !input$bait_code3 %in% c("",NA,NULL)) {
      hideFeedback("bait_code3")
      showFeedbackDanger("bait_code3", "No BAIT CD2 entered")
      checks$check26<- F
    }else{
      hideFeedback("bait_code3")
      ## need to return species name if it's still needed
      if(!input$bait_code3 %in% c("",NA,NULL)){
        new.bait3 <- input$bait_code3
        b.spec3 <- spec.tab$COMMON[which(spec.tab$SPECIES_CODE %in% new.bait3)]
        showFeedback("bait_code3",b.spec3)
      }
      checks$check26<- T
    }
  })

## 27 bait type 1
## 27:1 range 1-4
  observeEvent(input$bait_type1,{
    if(!input$bait_type1 %in% c(NA,NULL,"") & (input$bait_type1 < 0 | input$bait_type1 > 4)){
      hideFeedback("bait_type1")
      showFeedbackDanger("bait_type1", "Valid bait type codes are 1-4")
      checks$check27<- F
    }else{
      hideFeedback("bait_type1")
      checks$check27<- T
      if(!input$bait_type1 %in% c(NA,NULL,"")){ ## if error is cleared but there's still a value, need to replenish the lookup table name
        b.type <- code.tab$Name[which(code.tab$Field %in% "Bait Type" & code.tab$Code %in% input$bait_type1)]
        showFeedback("bait_type1",b.type)
      }
      }
  })

## 28 bait type 2
## 28:1 range 1-4 and can only have value if BAIT CD2 has value
  observe({
    if(!input$bait_type2 %in% c(NA,NULL,"") & (input$bait_type2 < 0 | input$bait_type2 > 4)){
      hideFeedback("bait_type2")
      showFeedbackDanger("bait_type2", "Valid bait type codes are 1-4")
      checks$check28<- F
    }else{
      hideFeedback("bait_type2")
      checks$check28<- T
      if(!input$bait_type2 %in% c(NA,NULL,"")){ ## if error is cleared but there's still a value, need to replenish the lookup table name
        if(input$bait_type1 %in% c(NA,NULL,"")){
          showFeedbackDanger("bait_type2", "No BAIT TYPE 1 entered")
          checks$check28<- F
        }else{
          b.type <- code.tab$Name[which(code.tab$Field %in% "Bait Type" & code.tab$Code %in% input$bait_type2)]
          showFeedback("bait_type2",b.type)
        }
      }
    }
  })

## 29 bait type 3
## 29:1 range 1-4
  observe({
    if(!input$bait_type3 %in% c(NA,NULL,"") & (input$bait_type3 < 0 | input$bait_type3 > 4)){
      hideFeedback("bait_type3")
      showFeedbackDanger("bait_type3", "Valid bait type codes are 1-4")
      checks$check29<- F
    }else{
      hideFeedback("bait_type3")
      checks$check29<- T
      if(!input$bait_type3 %in% c(NA,NULL,"")){ ## if error is cleared but there's still a value, need to replenish the lookup table name
        if(input$bait_type2 %in% c(NA,NULL,"")){
          showFeedbackDanger("bait_type3", "No BAIT TYPE 2 entered")
          checks$check29<- F
        }else{
          b.type <- code.tab$Name[which(code.tab$Field %in% "Bait Type" & code.tab$Code %in% input$bait_type3)]
          showFeedback("bait_type3",b.type)
        }
      }
    }
  })

#### FISH ROWS
  observe({      ## general observer for row numbers
    current_rows <- row_ids()

    lapply(current_rows, function(row_id) {

      ## 30 Trap No
      ## 30:1 Must match Trap No in Trap row
      observeEvent(input[[paste0("trap_num_", row_id)]],{
       if((!input$trap_num %in% c(NULL,NA,"") & !input[[paste0("trap_num_", row_id)]] %in% input$trap_num) |
          (input$trap_num %in% c(NULL,NA,"") & !input[[paste0("trap_num_", row_id)]] %in% c(NULL,NA,""))){
         showFeedbackDanger(paste0("trap_num_", row_id),"Error: Trap Numbers must match Trap Info")
         checks$check30 <- F
       }else{
         hideFeedback(paste0("trap_num_", row_id))
         checks$check30 <- T
       }
      }, ignoreInit = T)


      ## 31 Species Code
      ## 31:1 range (lookup table) and rows must be filled sequentially
      ## 31:2 First row Cannot be blank if trap is baited
      observe({
        hideFeedback(paste0("spec_code_", row_id))
        checks$check31 <- T
        row.num <- as.numeric(gsub("\\D", "", row_id))
        num.rows <- max(as.numeric(gsub("\\D", "", current_rows)))
        spec_minus_1 <- input[[paste0("spec_code_row_", row.num - 1)]]
        spec_plus_1  <- input[[paste0("spec_code_row_", row.num + 1)]]
        if(input$spec_code_row_1 %in% c(NULL,NA) && !input$bait_code  %in% c(NULL,NA)){
          showFeedbackDanger("spec_code_row_1","First row must have species code to submit trap")
          checks$check31 <- F
        }else{
          if(!is.null(spec_minus_1) && !is.null(spec_plus_1)){
          if((is.numeric(row.num) && row.num>1 && !input[[paste0("spec_code_", row_id)]] %in% c(NULL,NA) &&
              spec_minus_1 %in% c(NULL,NA)) ||
             (is.numeric(row.num) && is.numeric(num.rows) && row.num<num.rows && input[[paste0("spec_code_", row_id)]] %in% c(NULL,NA) &&
              !spec_plus_1 %in% c(NULL,NA))
          ){
            hideFeedback(paste0("spec_code_", row_id))
            showFeedbackDanger(paste0("spec_code_", row_id),"Rows must be filled sequentially!")
            checks$check31 <- F
          }else{

      ## 31:3 code 9999 can only be entered in first row
            if(!row_id %in% "row_1" && !is.null(input[[paste0("spec_code_", row_id)]]) &&
              input[[paste0("spec_code_", row_id)]] %in% 9999){
              hideFeedback(paste0("spec_code_", row_id))
              showFeedbackDanger(paste0("spec_code_", row_id),"Code 9999 can only be entered in first row!")
              checks$check31 <- F
            }else{
              ## check that chosen code exists in species code table
              if(!is.null(input[[paste0("spec_code_", row_id)]]) && !is.na(input[[paste0("spec_code_", row_id)]]) &&
                 !input[[paste0("spec_code_", row_id)]] %in% spec.tab$SPECIES_CODE){
                showFeedbackWarning(paste0("spec_code_", row_id),"Warning! This code is not on the list of known species!")
              }
            }
          }
          }
        }
      })



      ## 32 COMMON
      ## 32:1 No restrictions

      ## 33 Length
      ## 33: 1 length should have a value if there is a valid (measurable species) species choice (Includes autofill of units cm/mm)
      observeEvent(list(input[[paste0("spec_code_", row_id)]],input[[paste0("length_", row_id)]]),{
        hideFeedback(paste0("length_", row_id))
        if(!input[[paste0("spec_code_", row_id)]] %in% c(NULL,NA) &
           !input[[paste0("spec_code_", row_id)]] %in% abund.species &
           input[[paste0("length_", row_id)]] %in% c(NA,NULL)){
          showFeedbackWarning(paste0("length_", row_id), "Missing Value!")
        }else{
          if(input[[paste0("spec_code_", row_id)]] %in% c(crust.codes,2551)){ ## includes lobster larvae 2551
            showFeedback(paste0("length_", row_id), "mm")
            if(input[[paste0("spec_code_", row_id)]] %in% c(2550,2552) &&
               (input[[paste0("length_", row_id)]] < 20 | input[[paste0("length_", row_id)]] > 250)){
              hideFeedback(paste0("length_", row_id))
              showFeedbackWarning(paste0("length_", row_id), "This is an unlikely length for caught lobster!")
            }
          }
          if(!input[[paste0("spec_code_", row_id)]] %in% c(NULL,NA) & !input[[paste0("spec_code_", row_id)]] %in% c(2550,2551,2552)){
            showFeedback(paste0("length_", row_id), "cm")
          }
        }
      }, ignoreInit = T)
      ## 33:2

      ## 34 Sex
      ## 34:1 Range 1-3 and should only be for crustaceans (includes autofilling descriptions)
      observeEvent(list(input[[paste0("sex_", row_id)]], input[[paste0("spec_code_", row_id)]]),{
        if(!input[[paste0("sex_", row_id)]] %in% c(NULL,NA,1:3)){
          hideFeedback(paste0("sex_", row_id))
          showFeedbackDanger(paste0("sex_", row_id), "Allowed sex values are 1,2,3")
          checks$check34 <- F
        }else{
          hideFeedback(paste0("sex_", row_id))
          checks$check34 <- T
          ### Autofill Sex descriptions
              sex <-code.tab$Name[which(code.tab$Field %in% "Sex" & code.tab$Code %in% input[[paste0("sex_", row_id)]])]
              showFeedback(paste0("sex_", row_id), sex)
          if(!input[[paste0("sex_", row_id)]] %in% c(NULL,NA) & !input[[paste0("spec_code_", row_id)]] %in% crust.codes){
            hideFeedback(paste0("sex_", row_id))
            showFeedbackWarning(paste0("sex_", row_id), "Sex codes are for crustaceans only")
          }
          ## 34:2 should have a value if species is crustacean
          if(input[[paste0("spec_code_", row_id)]] %in% crust.codes & input[[paste0("sex_", row_id)]] %in% c(NULL,NA)){
            hideFeedback(paste0("sex_", row_id))
            showFeedbackWarning(paste0("sex_", row_id), "Missing Value!")
          }
        }
      }, ignoreInit = T)
      ## 34:3 If lobster sex = 1 then can't use egg or vnotch fields (Violation impossible - see Autofills)

      ## 35 Shell hard
      ## 35:1  Range + can only contain values if species code is lobster (2550) (Violation impossible, see autofills - below is redundant:)
      ## Also contains autofills of descriptions
      # Create an observer for each spec_code and shell_hard field
      observeEvent(list(input[[paste0("shell_", row_id)]], input[[paste0("spec_code_", row_id)]]),{
        range35 <- c(1,2,3,4,5,6,7)
        if(!input[[paste0("shell_", row_id)]] %in% c(NULL,NA,range35)){
          hideFeedback(paste0("shell_", row_id))
          showFeedbackDanger(paste0("shell_", row_id), paste0("Shell Hardness range is ",paste0(range35, collapse = ",")))
          checks$check35<- F
        }else{
          if(!is.na(input[[paste0("spec_code_", row_id)]])){
            # Get the updated value
            new_spec <- input[[paste0("spec_code_", row_id)]]
            new_shell <- input[[paste0("shell_", row_id)]]
            if(!new_spec %in% c(2550,2552) & !is.na(new_shell)){
              showFeedbackDanger(paste0("shell_", row_id), "Shell Hardness is for lobster only")
              checks$check35<- F
            }else{
              hideFeedback(paste0("shell_", row_id))
              checks$check35<- T
              ## autofill descriptions
              if(input[[paste0("spec_code_", row_id)]] %in% c(2550,2552) & input[[paste0("shell_", row_id)]] %in% range35){
                shell<-code.tab$Name[which(code.tab$Field %in% "Shell Hardness" & code.tab$Code %in% input[[paste0("shell_", row_id)]])]
                showFeedback(paste0("shell_", row_id), shell)
              }
              ## 35:2 Should have a value if species is lobster
              if(input[[paste0("spec_code_", row_id)]] %in% c(2550,2552) & input[[paste0("shell_", row_id)]] %in% c(NULL,NA)){
                showFeedbackWarning(paste0("shell_", row_id), "Missing Value!")
              }
            }
          }else{
            hideFeedback(paste0("shell_", row_id))
            checks$check35<- T
          }
        }
      }, ignoreInit = TRUE)  # Avoid triggering on initialization

      ## 36  Condition
      ## 36 : 1 range 0:7 and warning if using fish codes for crustacean or vice versa (includes description autofilling)
      observeEvent(list(input[[paste0("spec_code_", row_id)]],input[[paste0("cond_", row_id)]]),{
        hideFeedback(paste0("cond_", row_id))
        if(!input[[paste0("cond_", row_id)]] %in% c(NULL,NA) & (input[[paste0("cond_", row_id)]]<0 | input[[paste0("cond_", row_id)]]>7)){
          showFeedbackDanger(paste0("cond_", row_id), "Not a valid condition code")
          checks$check36 <- F
        }else{
          checks$check36 <- T
          if(input[[paste0("spec_code_", row_id)]] %in% crust.codes & !input[[paste0("cond_", row_id)]] %in% c(NULL,NA)){
            c <- condition$lob_cond[which(condition$code %in% input[[paste0("cond_", row_id)]])]
            if(c %in% NA){showFeedbackWarning(paste0("cond_", row_id), "This is not a crustacean code")}else{
              showFeedback(paste0("cond_", row_id),c)
            }
          }
          if(!input[[paste0("spec_code_", row_id)]] %in% crust.codes & !input[[paste0("cond_", row_id)]] %in% c(NULL,NA)){
            c <- condition$fish_cond[which(condition$code %in% input[[paste0("cond_", row_id)]])]
            if(c %in% NA){showFeedbackWarning(paste0("cond_", row_id), "This is a crustacean code")}else{
              showFeedback(paste0("cond_", row_id),c)
            }
          }
          ## 36: 2 should have a value if valid (measureable species) species is chosen
          if(!input[[paste0("spec_code_", row_id)]] %in% c(NULL,NA) &
             !input[[paste0("spec_code_", row_id)]] %in% abund.species &
             input[[paste0("cond_", row_id)]] %in% c(NULL,NA)){
            showFeedbackWarning(paste0("cond_", row_id), "Missing value!")
          }
        }
      }, ignoreInit = T)

      ## 37 Shell Disease
      ## 37:1 Can only have values if species is lobster (violation impossible - see autofills)
      ## 37:2 Range + should have a value if species is lobster (includes autofilling of descritptions)
      observeEvent(list(input[[paste0("spec_code_", row_id)]],input[[paste0("disease_", row_id)]]),{
        hideFeedback(paste0("disease_", row_id))
        checks$check37 <- T
        if(!input[[paste0("disease_", row_id)]] %in% c(NULL,NA,0:3)){
          showFeedbackDanger(paste0("disease_", row_id), "Allowed values are 0,1,2,3")
          checks$check37 <- F
        }
        if(input[[paste0("spec_code_", row_id)]] %in% c(2550,2552) & input[[paste0("disease_", row_id)]] %in% c(0:3)){
          disease<-code.tab$Name[which(code.tab$Field %in% "Shell Disease" & code.tab$Code %in% input[[paste0("disease_", row_id)]])]
          showFeedback(paste0("disease_", row_id), disease)
        }
        if(input[[paste0("spec_code_", row_id)]] %in% c(2550,2552) & input[[paste0("disease_", row_id)]] %in% c(NULL,NA)){
          showFeedbackWarning(paste0("disease_", row_id), "Missing value!")
        }
      }, ignoreInit = T)

      ## 38 Egg stage
      ## 38:1 range + Can only have value if species is berried lobster (violation impossible - see autofills)
      ## (below includes autofills of descriptions)
      observeEvent(list(input[[paste0("spec_code_", row_id)]],input[[paste0("egg_", row_id)]]),{
        hideFeedback(paste0("egg_", row_id))
        checks$check38 <- T
        if(!input[[paste0("egg_", row_id)]] %in% c(NULL,NA,1:4)){
          showFeedbackDanger(paste0("egg_", row_id), "Allowed values are 1,2,3,4")
          checks$check38 <- F
        }
        if(input[[paste0("spec_code_", row_id)]] %in% c(2550,2552) & input[[paste0("egg_", row_id)]] %in% c(1:4)){
          egg<-code.tab$Name[which(code.tab$Field %in% "Lobster Egg" & code.tab$Code %in% input[[paste0("egg_", row_id)]])]
          showFeedback(paste0("egg_", row_id), egg)
        }
      }, ignoreInit = T)

      ## 39 Clutch
      ## 39:1 range + Can only have values for berried lobster (violation impossible - see autofills)
      ## (also includes autofills of descriptions below)
      observeEvent(list(input[[paste0("spec_code_", row_id)]],input[[paste0("clutch_", row_id)]]),{
        hideFeedback(paste0("clutch_", row_id))
        checks$check39 <- T
        if(!input[[paste0("clutch_", row_id)]] %in% c(NULL,NA,1:3)){
          showFeedbackDanger(paste0("clutch_", row_id), "Allowed values are 1,2,3")
          checks$check39 <- F
        }
        if(input[[paste0("spec_code_", row_id)]] %in% c(2550,2552) & input[[paste0("clutch_", row_id)]] %in% c(1:3)){
          clutch<-code.tab$Name[which(code.tab$Field %in% "Clutch" & code.tab$Code %in% input[[paste0("clutch_", row_id)]])]
          showFeedback(paste0("clutch_", row_id), clutch)
        }
      }, ignoreInit = T)

      ## 40 Vnotch
      ## 40:1 range + can only have values for female lobster (violation impossible - see autofills)
      observeEvent(list(input[[paste0("spec_code_", row_id)]],input[[paste0("vnotch_", row_id)]]),{
        hideFeedback(paste0("vnotch_", row_id))
        checks$check40 <- T
        if(!input[[paste0("vnotch_", row_id)]] %in% c(NULL,NA,0:5)){
          showFeedbackDanger(paste0("vnotch_", row_id), "Allowed values are 0,1,2,3,4,5")
          checks$check40 <- F
        }else{
          vnotch<-code.tab$Name[which(code.tab$Field %in% "vnotch" & code.tab$Code %in% input[[paste0("vnotch_", row_id)]])]
          showFeedback(paste0("vnotch_", row_id), vnotch)
        }

      }, ignoreInit = T)

      ## 41 Kept
      ## Range + shouldn't be kept if other conditions are met
      observeEvent(list(input[[paste0("spec_code_", row_id)]],input[[paste0("sex_", row_id)]], input[[paste0("kept_", row_id)]], input[[paste0("length_", row_id)]], input$lfa,
                        input[[paste0("vnotch_", row_id)]]),{

        hideFeedback(paste0("kept_", row_id))
        checks$check41 <- T
        checked = 0
        should.be.kept = T

        ## range
        if(!input[[paste0("kept_", row_id)]] %in% c(NULL,NA,0,1)){
          showFeedbackDanger(paste0("kept_", row_id), "Allowed values are 0 (not kept) or 1 (kept)")
          checks$check41 <- F
        }

        ### SHOULD NOT be kept conditions:
        ## 41:1 Shouldn't be kept if length < MLS
          if(!input[[paste0("spec_code_", row_id)]] %in% c(NULL,NA) && input[[paste0("spec_code_", row_id)]] %in% c(2550,2552) &&
             !input[[paste0("length_", row_id)]] %in% c(NULL,NA) && !input$lfa %in% c(NULL,NA,"")
             && input[[paste0("length_", row_id)]] < lfa.data$MLS[which(lfa.data$LFA %in% input$lfa)]){
            warn.mess <- "Warning!: you are recording retaining a lobster below Minimum Legal Size for chosen LFA"
            should.be.kept = F
          }
          ## 41:2 shouldn't be kept if vnotched in prohibited LFAs
          if(!input[[paste0("spec_code_", row_id)]] %in% c(NULL,NA) && input[[paste0("spec_code_", row_id)]] %in% c(2550,2552) &&
             !input[[paste0("vnotch_", row_id)]] %in% c(NULL,NA,0) && !input$lfa %in% c(NULL,NA,"") &&
             input$lfa %in% lfa.data$LFA[which(!lfa.data$vnotch.rule %in% "none")]){
            warn.mess <- "Warning!: There are restrictions for retaining V-notched lobster in chosen LFA. Check specific regulations."
            should.be.kept = F
          }
          ## 41:3 shouldn't be kept if female between 114-124mm in LFA 31A
          if(!input[[paste0("spec_code_", row_id)]] %in% c(NULL,NA) && input[[paste0("spec_code_", row_id)]] %in% c(2550,2552) &&
             !input[[paste0("length_", row_id)]] %in% c(NULL,NA) && !input$lfa %in% c(NULL,NA,"") &&
             input$lfa %in% "L31A" && input[[paste0("length_", row_id)]]>=114 && input[[paste0("length_", row_id)]]<=124 &&
             input[[paste0("sex_", row_id)]] %in% c(2,3)){
            warn.mess <- "Warning!: Retaining female lobster between 114 and 124 mm is prohibited in LFA 31A."
            should.be.kept = F
          }
          ## 41:4 shouldn't be kept if female > 135 in LFA30
          if(!input[[paste0("spec_code_", row_id)]] %in% c(NULL,NA) && input[[paste0("spec_code_", row_id)]] %in% c(2550,2552) &&
             !input[[paste0("length_", row_id)]] %in% c(NULL,NA) && !input$lfa %in% c(NULL,NA,"") &&
             input$lfa %in% "L30" && input[[paste0("length_", row_id)]]>135 &&
             input[[paste0("sex_", row_id)]] %in% c(2,3)){
            warn.mess <- "Warning!: Retaining female lobster greater than 135 mm is prohibited in LFA 30."
            should.be.kept = F
          }
          # 41:5 shouldn't be kept if berried
          if(!input[[paste0("spec_code_", row_id)]] %in% c(NULL,NA) && input[[paste0("spec_code_", row_id)]] %in% c(2550,2552) &&
             input[[paste0("sex_", row_id)]] %in% c(3)){
            warn.mess <- "Warning!: You are recording retaining a berried female. This is illegal."
           should.be.kept = F
          }


        ## if lobster is being recorded as kept when it shoudln't be for any reason:
        if(!should.be.kept){
          if(input[[paste0("kept_", row_id)]] %in% 1){
            hideFeedback(paste0("kept_", row_id))
            showFeedbackWarning(paste0("kept_", row_id), warn.mess)
          }
        }

        ## 41:6 if is a lobster and all required fields to check keepability have values but still no recorded as kept:
        if(!input[[paste0("spec_code_", row_id)]] %in% c(NULL,NA) && input[[paste0("spec_code_", row_id)]] %in% c(2550,2552) &&
           !input$lfa %in% c(NULL,NA,"") && !input[[paste0("sex_", row_id)]] %in% c(NULL,NA) && !input[[paste0("length_", row_id)]] %in% c(NULL,NA)){

          if(should.be.kept){ ## kept conditions have all passed
            #  double check if a legal animal blank for kept
            if(is.na(input[[paste0("kept_", row_id)]]) | is.null(input[[paste0("kept_", row_id)]])){
              hideFeedback(paste0("kept_", row_id))
              showFeedbackWarning(paste0("kept_", row_id), "Was this legal animal kept?")
            }
            # double check if legal animal is being marked as not kept
            if(input[[paste0("kept_", row_id)]] %in% 0){
              hideFeedback(paste0("kept_", row_id))
              showFeedbackWarning(paste0("kept_", row_id), "Sure this legal animal wasn't kept?")
            }
          }

        }


      }, ignoreInit = T)

      ## 42 Abundance
      ## 42:1 Range + Shouldn't be > 1 for anything except whelks, starfish, urchins, shrimp, etc. (non-measureable species)
      observeEvent(list(input[[paste0("spec_code_", row_id)]],input[[paste0("abund_", row_id)]]),{
        hideFeedback(paste0("abund_", row_id))
        checks$check42 <- T
        if(!input[[paste0("abund_", row_id)]] %in% c(NULL,NA) & input[[paste0("abund_", row_id)]] < 0){
          showFeedbackDanger(paste0("abund_", row_id), "Abundance must be 0 or greater")
          checks$check42 <- F
        }else{
          if(!input[[paste0("abund_", row_id)]] %in% c(NULL,NA) & input[[paste0("abund_", row_id)]] > 1 &&
             !input[[paste0("spec_code_", row_id)]] %in% abund.species){
            showFeedbackWarning(paste0("abund_", row_id), "Abundance should only be greater than 1 for certain 'Non-Measureable' species (Whelk, Hermit Crab, etc)")
          }
        }
        ## 42:2 Cannot be blank if spec code is 9999 (should be 0)
        if(input[[paste0("spec_code_", row_id)]] %in% 9999 && input[[paste0("abund_", row_id)]] %in% c(NULL,NA)){
          hideFeedback(paste0("abund_", row_id))
          showFeedbackDanger(paste0("abund_", row_id), "Needs value if Species Code = 9999 (should be 0)")
          checks$check42 <- F
        }
        if(input[[paste0("spec_code_", row_id)]] %in% 9999 && !input[[paste0("abund_", row_id)]] %in% c(NULL,NA,0)){
          hideFeedback(paste0("abund_", row_id))
          showFeedbackWarning(paste0("abund_", row_id), "Should be 0 for Species Code = 9999")
        }
      }, ignoreInit = T)

      ## 43 Cull
      ## 43:1 Range + Can only have values if species is lobster (Violation impossible, see autofills)
      observeEvent(list(input[[paste0("spec_code_", row_id)]],input[[paste0("cull_", row_id)]]),{
        hideFeedback(paste0("cull_", row_id))
        checks$check43 <- T
        if(!input[[paste0("cull_", row_id)]] %in% c(NULL,NA,1:3)){
          showFeedbackDanger(paste0("cull_", row_id), "Allowed values are 1,2,3")
          checks$check43 <- F
        }else{
          cull<-code.tab$Name[which(code.tab$Field %in% "Cull" & code.tab$Code %in% input[[paste0("cull_", row_id)]])]
          showFeedback(paste0("cull_", row_id), cull)
        }
      }, ignoreInit = T)


    }) ## lapply


  }) ## observer for all fish row data




### BUTTON CLICKABILITY
  observe({
    any_false <- any(unlist(reactiveValuesToList(checks)) %in% FALSE)

    if (any_false) {
      disable("next_trap")
      disable("next_set")
      disable("prev_set")
      disable("submit_trip")
    } else {
      enable("next_trap")
      enable("next_set")
      enable("prev_set")
      enable("submit_trip")
    }
  })


} ## Server code


suppressWarnings({
  shinyApp(ui, server)
})


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
