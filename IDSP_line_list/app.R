# Load necessary libraries
library(shiny)
library(bslib)
library(bsicons) # Required for value box icons
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(DT)

# --- USER INTERFACE ---
ui <- page_fluid(
  title = "IDSP Line List Generator",
  
  # Change theme to ESI Maroonish color
  theme = bs_theme(bootswatch = "flatly", primary = "#800000"), 
  
  # Custom Institutional Header
  div(
    class = "d-flex justify-content-between align-items-center p-3 mb-3 mt-2",
    style = "background-color: #ffffff; border-bottom: 4px solid #800000; border-radius: 5px; box-shadow: 0 4px 6px -6px #222;",
    
    # Left Logo (ESIC)
    tags$img(src = "esic_logo.png", height = "90px"),
    
    # Center Titles
    div(
      class = "text-center flex-grow-1",
      h1("Department of Community Medicine", style = "margin: 0; font-weight: 700; color: #800000;"),
      h4("ESIC Medical College & Hospital, KK Nagar, Chennai", style = "margin: 0; color: #6c757d;"),
      h2("OPD Data Parser and IDSP Line List Generator", style = "margin-top: 15px; font-weight: 600; color: #333;")
    ),
    
    # Right Logo (Community Medicine)
    tags$img(src = "cm_logo.png", height = "90px")
  ),
  
  # Main Application Layout
  layout_sidebar(
    sidebar = sidebar(
      width = 350,
      
      fileInput("upload", "Upload Raw Excel File (.xls or .xlsx) or Raw .aspx file", 
                accept = c(".xls", ".xlsx", ".aspx")),
      
      # Privacy & Security Assurance Badge
      div(
        class = "p-2 mb-3 rounded",
        style = "background-color: #f8f9fa; border: 1px solid #d1e7dd; border-left: 4px solid #198754; font-size: 0.85em; color: #0f5132;",
        HTML("<strong>🔒 100% Private & Secure:</strong><br>Processed entirely inside your computer via WebAssembly. Data is <u>never</u> sent out or uploaded to any server or cloud.")
      ),
      
      # Download buttons (disabled until data is processed)
      uiOutput("download_ui"),
      
      hr(),
      
      # Embedded SOP Accordion for quick reference
      accordion(
        open = FALSE,
        accordion_panel(
          "ICD-10 Coding SOP Quick Reference",
          icon = bsicons::bs_icon("info-circle"),
          HTML("
            <b>Fever (PUO):</b> R50.9<br>
            <b>Altered Sensorium:</b> R40.2 / G04.9<br>
            <b>Cough (any duration):</b> R05<br>
            <b>Jaundice (< 4 wks):</b> R17<br>
            <b>Acute Diarrhoeal Disease:</b> A09<br>
            <b>ARI / ILI:</b> J06.9 / J11<br>
            <b>SARI:</b> J22 / J18.9<br>
            <b>Animal Bites:</b> Dog (W54), Snake (T63.0/X20)
            <br><br>
            <i>Note: All Chapter 1 (A00-B99) infectious diseases are automatically captured.</i>
          ")
        )
      ),
      
      # Credits pushed to the absolute bottom of the sidebar
      div(
        class = "mt-auto", 
        style = "background-color: #800000; color: #ffffff; border: 2px solid #4a0000; border-radius: 5px; padding: 12px; font-size: 0.85em; text-align: center;",
        HTML("This website is made for institutional use by <strong>Dr. D. Vignesh</strong> and <strong>Dr. Nalam Middleton A.</strong>, from Department of Community Medicine, ESIC Medical College and Hospital, KK Nagar, Chennai"),
        br(),
        tags$a(
          href = "https://github.com/redgyaradoz/esicmch_kkn_chn_idsp-dashboard/tree/main", 
          target = "_blank", 
          style = "color: #ffcccc; text-decoration: underline; margin-right: 15px;",
          "GitHub Repository"
        ),
        "|",
        tags$a(
          href = "https://creativecommons.org/licenses/by-nc/4.0/", 
          target = "_blank", 
          style = "color: #ffcccc; text-decoration: underline; margin-left: 15px;",
          "CC BY-NC 4.0 License"
        )
      )
    ), # <-- Properly closes sidebar()
    
    # --- MAIN PANEL CONTENTS ---
    # Epidemiological Summary Metrics
    layout_columns(
      fill = FALSE,
      value_box(
        title = "Total Records Parsed",
        value = textOutput("total_records"),
        showcase = bsicons::bs_icon("file-earmark-spreadsheet"),
        theme = "secondary"
      ),
      value_box(
        title = "IDSP Cases Flagged",
        value = textOutput("idsp_cases"),
        showcase = bsicons::bs_icon("activity"),
        theme = "primary" 
      ),
      value_box(
        title = "Top Syndrome",
        value = textOutput("top_syndrome"),
        showcase = bsicons::bs_icon("bar-chart"),
        theme = "info"
      )
    ),
    
    # Tabbed Data Table Card with full-screen expansion
    navset_card_underline(
      full_screen = TRUE,
      title = "Data Preview",
      
      # Tab 1: IDSP Line List
      nav_panel(
        title = "IDSP Line List", 
        DTOutput("table_idsp")
      ),
      
      # Tab 2: Full Cleaned Data
      nav_panel(
        title = "Full Cleaned Data", 
        DTOutput("table_raw")
      )
    )
  )
)

# --- SERVER LOGIC ---
server <- function(input, output, session) {
  
  # Reactive 1: Base data parsing (Full cleaned raw data)
  base_data <- reactive({
    req(input$upload)
    
    raw_data <- read_excel(input$upload$datapath, col_names = FALSE)
    
    raw_data <- raw_data %>%
      filter(!if_all(everything(), is.na)) %>%
      select(where(~ any(!is.na(.x))))
    
    new_colnames <- as.character(raw_data[5, ])
    colnames(raw_data) <- make.names(new_colnames, unique = TRUE)
    
    raw_data <- raw_data[-c(1:5), ]
    
    raw_data %>%
      mutate(disease_name = ifelse(startsWith(as.character(.[[2]]), "Disease Name :"), 
                                   as.character(.[[2]]), NA_character_)) %>%
      fill(disease_name, .direction = "down") %>%
      mutate(`ICD-10 code` = str_sub(disease_name, 16, 21),
             `ICD-10 code` = str_replace_all(`ICD-10 code`, "[ -]", "")) %>%
      filter(!is.na(suppressWarnings(as.numeric(Insurance.No.)))) %>%
      select(where(~ any(!is.na(.x)))) %>%
      rename(`Check_in_No.` = NA..1) %>%
      mutate(SNOMED_Name = str_trim(str_match(disease_name, "SNOMED Name\\s*:\\s*(.*)")[, 2]),
             Diagnosis.Date = as.Date(as.numeric(Diagnosis.Date), origin = "1899-12-30"))
  })
  
  # Reactive 2: Filtered IDSP Line List
  filtered_data <- reactive({
    req(base_data())
    
    idsp_other_syndromes <- c(
      "G04", "G03", "G82", "R50", "R05", "R17", "R40", 
      "J06", "J09", "J10", "J11", "J12", "J18", "J22", 
      "W53", "W54", "W55", "T63", "X20"
    )
    
    base_data() %>%
      mutate(icd_base = str_sub(`ICD-10 code`, 1, 3)) %>%
      filter(
        str_detect(`ICD-10 code`, "^A|^B") | icd_base %in% idsp_other_syndromes
      ) %>%
      select(Patient.Name, Insurance.No., Diagnosis.Date, `ICD-10 code`, disease_name, Specialisation) %>%
      rename(`IP Number` = Insurance.No.)
  })
  
  # --- RENDER VALUE BOXES ---
  output$total_records <- renderText({
    req(base_data())
    nrow(base_data())
  })
  
  output$idsp_cases <- renderText({
    req(filtered_data())
    nrow(filtered_data())
  })
  
  output$top_syndrome <- renderText({
    req(filtered_data())
    df <- filtered_data()
    if(nrow(df) == 0) return("None")
    
    top_icd <- df %>% count(`ICD-10 code`, sort = TRUE) %>% slice(1) %>% pull(`ICD-10 code`)
    return(top_icd)
  })
  
  # --- RENDER TABLES & DOWNLOAD BUTTONS ---
  
  # Render Tab 1: IDSP List
  output$table_idsp <- renderDT({
    req(filtered_data())
    datatable(filtered_data(), 
              options = list(pageLength = 10, scrollX = TRUE),
              rownames = FALSE)
  })
  
  # Render Tab 2: Full Cleaned Data
  output$table_raw <- renderDT({
    req(base_data())
    datatable(base_data(), 
              options = list(pageLength = 10, scrollX = TRUE),
              rownames = FALSE)
  })
  
  # Dynamically render both download buttons vertically stacked
  output$download_ui <- renderUI({
    req(filtered_data(), base_data())
    div(class = "d-grid gap-2",
        downloadButton("download_idsp", "Download IDSP Line List", class = "btn-primary"),
        downloadButton("download_raw", "Download Full Cleaned Data", class = "btn-outline-primary")
    )
  })
  
  # Handler for the filtered IDSP List
  output$download_idsp <- downloadHandler(
    filename = function() {
      random_num <- sprintf("%05d", sample(0:99999, 1))
      paste0("IDSP_line_list_", Sys.Date(), "_", random_num, ".csv")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
  
  # Handler for the Full Cleaned Raw Data
  output$download_raw <- downloadHandler(
    filename = function() {
      random_num <- sprintf("%05d", sample(0:99999, 1))
      paste0("Full_Cleaned_Raw_Data_", Sys.Date(), "_", random_num, ".csv")
    },
    content = function(file) {
      write.csv(base_data(), file, row.names = FALSE)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)