# Load Libraries
library(shiny)
library(tidyverse)
library(readxl)
library(ggtext)
library(grid)
library(purrr)
library(gridExtra)
library(grDevices)
library(svglite)
library(extrafont)
library(stringr)
library(DT)
library(openxlsx)  # New library for Excel operations
library(patchwork) # For combining plots
library(viridis)  # For color scales


# Load plot choices for biochemistry and hematology
plotChoices_biochemistry <- c(
  "Fasting Blood Glucose",
  "Albumin",
  "AST",
  "Total Protein",
  "GGT",
  "Alkaline Phosphatase",
  "LDH",
  "HB-A1C",
  "Total Bilirubin",
  "Direct Bilirubin",
  "Indirect Bilirubin",
  "Creatinine",
  "Urea",
  "HDL-C",
  "Total Cholesterol",
  "LDL-C",
  "Triglycerides",
  "VLDL-C",
  "Troponin",
  "Uric Acid",
  "Globulin",
  "Sodium",
  "Potassium",
  "Phosphorus",
  "Calcium",
  "Magnesium",
  "Chloride",
  "ALT"
)


plotChoices_hematology <- c(
  "WBC", "RBC", "HGB", "Hematocrit", "MCV", "MCH", "MCHC", 
  "MPV", "RDW%", "RDW Count", "Platelets %", "Platelets Count", 
  "Neutrophils %", "Neutrophils Count", "Lymphocytes %", 
  "Lymphocytes Count", "Monocytes %", "Monocytes Count", 
  "Basophils %", "Basophils Count", "Eosinophils %", 
  "Eosinophils Count", "PDW", "PCT", "Granulocytes %", 
  "Granulocytes Count"
)



# Define UI for the Shiny app
ui <- fluidPage(
  titlePanel("Reference Interval Graph Plotter"),
  helpText("This Shiny app takes in Excel data in the specified format and returns a downloadable plot in .svg format."),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("plotData", label = "Choose Data File", multiple = FALSE,
                accept = c(".csv", ".xls", ".xlsx", "text/csv", "application/vnd.ms-excel", "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")),
      radioButtons("category", label = "Select Data Category", choices = c("Biochemistry", "Hematology"), selected = "Biochemistry"),
      uiOutput("selectAnalyte"),
      uiOutput("unitsInput"),
      radioButtons("plotType", label = "Select the plot to display", choices = c("Child Male", "Child Female", "Adult Male", "Adult Female")),
      sliderInput("height", "height", min = 400, max = 1400, value = 700),
      sliderInput("width", "width", min = 400, max = 1400, value = 750)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Data Preview", 
                 DTOutput("dataPreview")
        ),
        tabPanel("Plot", 
                 downloadButton("downloadPlot", "Download Plot as SVG"),
                 plotOutput("plot")
        ),
        tabPanel("Processed Data Statistics",  # New Tab for Processed Data Statistics
                 downloadButton("downloadProcessed", "Download Processed Data Statistics as Excel"),
                 DTOutput("processedDataPreview")
  )
)
)
)
)

# Define server logic 
server <- function(input, output, session) {
  output$selectAnalyte <- renderUI({
    if (input$category == "Biochemistry") {
      selectInput("analyte", label = "Select the Analyte data for the plot", choices = plotChoices_biochemistry)
    } else if (input$category == "Hematology") {
      selectInput("analyte", label = "Select the Analyte data for the plot", choices = plotChoices_hematology)
    }
  })
  
  #Data Preview
  output$dataPreview <- renderDT({
    req(input$plotData)
    data <- readxl::read_excel(input$plotData$datapath, sheet = input$analyte, range = cell_rows(1:100))
    datatable(data, options = list(pageLength = 10))
  })
  
  defaultUnit <- reactive({
    req(input$plotData)
    # Try to read the Units column from the data
    tryCatch({
      data <- readxl::read_excel(input$plotData$datapath, sheet = input$analyte)
      if("Units" %in% names(data)) {
        # Get the first non-NA value
        unit <- data$Units[!is.na(data$Units)][1]
        if(is.null(unit) || is.na(unit) || unit == "") {
          return("mmol/L") # Default if no valid value found
        } else {
          return(unit)
        }
      } else {
        return("mmol/L") # Default if no Units column
      }
    }, error = function(e) {
      return("mmol/L") # Default in case of error
    })
  })
  
  # Render the units input with the default value from data
  output$unitsInput <- renderUI({
    textInput("units", label = "Type your units here", value = defaultUnit())
  })
  
  plottingData <- reactive({
    req(input$plotData)
    readxl::read_excel(input$plotData$datapath, sheet = input$analyte, range = cell_rows(1:100)) %>%
      drop_na()
  })
  
  sort_data <- function(data, analyzer, ll, ul, gender, age_group) {
    data <- data %>%
      drop_na() %>%
      mutate(
        vendor = str_extract({{ analyzer }}, "^[^-]+"),
        vendor = gsub("\\s+$", "", vendor),
        Number = row_number(),
        gender = if_else({{ gender }} == 1, "Male", "Female"),
        age_group = if_else({{ age_group }} == 1, "Adult", "Child"),
        Analyzer = paste0(vendor, "-", Number)
      ) %>%
      select(vendor, L.L = {{ ll }}, U.L = {{ ul }}, gender, age_group, Analyzer)
    
    return(data)
  }
  
  
  
  
  
  # Combine sorted data
  combined_data <- reactive({
    bind_rows(
      sort_data(plottingData(), Analyzer, AM_L.L, AM_U.L, Gender_male, Age_adult),
      sort_data(plottingData(), Analyzer, AF_L.L, AF_U.L, Gender_female, Age_adult),
      sort_data(plottingData(), Analyzer, CM_L.L, CM_U.L, Gender_male, Age_child),
      sort_data(plottingData(), Analyzer, CF_L.L, CF_U.L, Gender_female, Age_child)
    )
  })
  
  # Process combined data
  all_data <- reactive({ 
    data <- combined_data() %>%
      mutate(diff = round(U.L - L.L, 3)) %>%
      filter(diff > 0) %>%
      mutate(x_pos = L.L + (diff / 2))
    
    # Create a new variable to store the order for reordering the plots
    data <- data %>%
      mutate(ord = nrow(data):1)
    
    data
  })
  
  
  #Stats calculation
  stats <- reactive({ all_data() %>%
      pivot_longer(cols = c(L.L, U.L), names_to = "hb", values_to = "value") %>%
      group_by(gender, age_group, hb) %>%
      summarise(mean = mean(value), SD = sd(value)) %>%
      mutate(meanpos = mean + 1 * SD, meanneg = mean - 1 * SD)
  })
  
  #Filters and stores mean and standard deviation for each group in a specific variable.
  stats_adult_male <- reactive({
    stats() %>%
      filter(gender == "Male", age_group == "Adult")
  })
  
  stats_child_male <- reactive({
    stats() %>%
      filter(gender == "Male", age_group == "Child")
  })
  
  stats_adult_female <- reactive({
    stats() %>%
      filter(gender == "Female", age_group == "Adult")
  })
  
  stats_child_female <- reactive({
    stats() %>%
      filter(gender == "Female", age_group == "Child")
  })
  
  
  # Find mean and standard deviation for annotation
  find_mean_sd <- function(gender_age) {
    L_L_data <- gender_age %>%
      filter(hb == "L.L")
    U_L_data <- gender_age %>%
      filter(hb == "U.L")
    
    # Extract mean and sd for L.L
    mean_L.L <- round(L_L_data$mean, 2)
    sd_L.L <- round(L_L_data$SD, 2)
    
    # Extract mean and sd for U.L
    mean_U.L <- round(U_L_data$mean, 2)
    sd_U.L <- round(U_L_data$SD, 2)
    
    # Create a data frame to store the results
    results <- data.frame(
      A = c("LRL", "URL"),
      Mean = c(mean_L.L, mean_U.L),
      SD = c(sd_L.L, sd_U.L)
    )
    colnames(results)<- c(" ", "Mean", "SD")
    return(results)
  }
  
  child_male_mean_sd <- reactive({find_mean_sd(stats_child_male())})
  child_female_mean_sd <- reactive({find_mean_sd(stats_child_female())})
  adult_male_mean_sd <- reactive({find_mean_sd(stats_adult_male())})
  adult_female_mean_sd <- reactive({find_mean_sd(stats_adult_female())})
  
  
  # Separate data by gender and age_group for plotting
  adult_Male <- reactive({all_data() |> filter(gender == "Male", age_group == "Adult")})
  child_Male <- reactive({all_data() |> filter(gender == "Male", age_group == "Child")})
  adult_Female <- reactive({all_data() |> filter(gender == "Female", age_group == "Adult")})
  child_Female <- reactive({all_data() |> filter(gender == "Female", age_group == "Child")})
  
  #List of the analyzers
  analyzer_name <- reactive({all_data() %>%
      select(vendor)
  })
  
  
  #Generate plot
  generate_plot <- function(data, gender_age_stats, first_analyzer, last_analyzer, annotation, title, gender, sign, unit) {
    data$Analyzer <- as.factor(data$Analyzer)
    
    # Calculate the position for table placement
    max_url <- max(data$U.L) * 1.05  # Position slightly right of the maximum URL
    
    # Create the main plot
    main_plot <- ggplot(data) +
      geom_rect(aes(xmin = gender_age_stats %>%
                      filter(hb == "U.L") %>%
                      pull(meanneg),
                    xmax = gender_age_stats %>%
                      filter(hb == "U.L") %>%
                      pull(meanpos),
                    ymin = -Inf, ymax = Inf,
                    group = interaction(gender, age_group)),
                fill = "#00BFC4", alpha = 0.05) +
      geom_vline(data = gender_age_stats %>% filter(hb == "U.L"),
                 aes(xintercept = mean),
                 color = "#762a83", linetype = "dashed", size = 0.5, alpha = 0.5) +
      geom_rect(aes(xmin = gender_age_stats %>%
                      filter(hb == "L.L") %>%
                      pull(meanneg),
                    xmax = gender_age_stats %>%
                      filter(hb == "L.L") %>%
                      pull(meanpos),
                    ymin = -Inf, ymax = Inf,
                    group = interaction(gender, age_group)),
                fill = "#F8766D", alpha = 0.05) +
      geom_vline(data = gender_age_stats %>% filter(hb == "L.L"),
                 aes(xintercept = mean),
                 color = "#762a83", linetype = "dashed", size = 0.5, alpha = 0.5) +
      geom_segment(aes(y = reorder(Analyzer, ord),
                       x = L.L,
                       yend = reorder(Analyzer, ord),
                       xend = U.L,
                       group = Analyzer),
                   color = "#000000", size = 4.5, alpha = 0.8) +
      geom_point(aes(x = L.L, y = reorder(Analyzer, ord), color = "#F8766D"), size = 4.5, show.legend = FALSE) +
      geom_point(aes(x = U.L, y = reorder(Analyzer, ord), color = "#00BFC4"), size = 4.5, show.legend = FALSE) +
      scale_color_manual(values = c("#00BFC4", "#F8766D")) +
      geom_text(data = data,
                aes(label = paste("D: ", diff), x = x_pos, y = Analyzer),
                color = "white", size = 2.5, family = "Segoe UI Semibold") +
      theme_minimal() +
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.y = element_text(size = 10),
        axis.line = element_line(color = "#4a4e4d"),
        text = element_text(family = "Segoe UI Semibold", color = "#4a4e4d"),
        plot.background = element_rect(fill = "white", color = "white"),
        plot.margin = margin(1, 1, 0.2, 1, "cm"),
        plot.caption = element_markdown(hjust = 0, lineheight = 1.5),
        plot.subtitle = element_markdown(size = 12, hjust = 0.3),
        plot.title = element_markdown(size = 16, hjust = 0.3)
      ) +
      labs(
        title = paste0("<span style = 'color: #F8766D;'>**Variations in Reference Intervals**</span> <br/> ", title, " (", gender, " ", sign, " 18)"),
        x = paste("Reference Intervals from Different Clinical Laboratories (", unit, ")"),
        y = "Analyzer",
        subtitle = paste(
          data %>% pull(L.L) %>% min(),
          " ≤ LRL ≤ ",
          data %>% pull(L.L) %>% max(),
          ",",
          data %>% pull(U.L) %>% min(),
          " ≤ URL ≤ ",
          data %>% pull(U.L) %>% max(),
          ",",
          data %>% pull(diff) %>% min() %>% round(2),
          " ≤ δ ≤ ",
          data %>% pull(diff) %>% max()
        )
      ) +
      scale_y_discrete(labels = rev(as.factor(data$vendor))) +
      coord_cartesian(xlim = c(min(data$L.L), max(data$U.L) * 1.2))  # Extended x-axis to make room for table
    
    # Create the tableGrob
    annotation_table <- tableGrob(annotation, rows = NULL)
    
    # Add the table to the bottom right of the main plot
    table_x <- max_url*1.06
    table_y <- min(as.numeric(data$ord)) * 0.05  # Position near the bottom of the plot
    
    # Add the table as an annotation to the main plot
    main_plot <- main_plot +
      annotation_custom(
        grob = annotation_table,
        xmin = table_x,
        xmax = table_x + (max(data$U.L) - min(data$L.L)) * 0.15,  # Table width
        ymin = table_y,
        ymax = table_y + nrow(data) * 0.25  # Table height
      )
    
    return(main_plot)
  }
  
  
  plot_child_Male <- reactive({
    generate_plot(child_Male(), stats_child_male(),"AA", "AI", child_male_mean_sd(),input$analyte, "Male", "<", input$units)
  })
  plot_child_Female <- reactive({
    generate_plot(child_Female(), stats_child_female(),"AA", "AI", child_female_mean_sd(),input$analyte, "Female", "<", input$units)
  })
  plot_adult_Male <- reactive({
    generate_plot(adult_Male(), stats_adult_male(),"AA", "AI", adult_male_mean_sd(),input$analyte, "Male", ">", input$units)
  })
  plot_adult_Female <- reactive({
    generate_plot(adult_Female(), stats_adult_female(),"AA", "AI", adult_female_mean_sd(),input$analyte, "Female", ">", input$units)
  })
  
  # Render the selected plot based on radio button input
  output$plot <- renderPlot({
    plot_type <- input$plotType
    
    if (plot_type == "Child Male") {
      plot_child_Male()
    } else if (plot_type == "Child Female") {
      plot_child_Female()
    } else if (plot_type == "Adult Male") {
      plot_adult_Male()
    } else if (plot_type == "Adult Female") {
      plot_adult_Female()
    }
  }, 
  width = function() input$width,
  height = function() input$height,
  res = 96)
  
  output$value2 <- renderTable({
    adult_Female()
  })
  
  # save plot
  
  # save plot
  generate_and_save_plot <- function(plot_type, analyte, width, height, file_path) {
    plot_data <- switch(plot_type,
                        "Child Male" = plot_child_Male(),
                        "Child Female" = plot_child_Female(),
                        "Adult Male" = plot_adult_Male(),
                        "Adult Female" = plot_adult_Female()
    )
    
    ggsave(
      file = file_path,
      plot = plot_data,
      width = width / 96, # Convert pixels to inches
      height = height / 96, # Convert pixels to inches
      device = "svg"
    )
  }
  
  #handle download
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste0(input$plotType, "_", input$analyte, ".svg")
    },
    content = function(file) {
      generate_and_save_plot(input$plotType, input$analyte, input$width, input$height, file)
    }
  )
  
  #Processed Data Statistics
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

