# ============================================================================
# STUDENT DEBT TRAJECTORY DEMO DASHBOARD
# Functional Data Analysis Demo for CPUT ED Finance Meeting
# Simulated data for 2,000 students | Monthly data over 24 months
# ============================================================================

# Install if needed
install.packages('rsconnect')
library('rsconnect')

# This creates manifest.json in your current directory
rsconnect::writeManifest()

# Create your personal library folder
dir.create("C:/Users/linde/AppData/Local/R/win-library/4.6", showWarnings = FALSE, recursive = TRUE)

# Set it as the default install location
.libPaths("C:/Users/linde/AppData/Local/R/win-library/4.6")

# Install required packages (run once)
# Set library path again (do this each time you open R, or set it permanently)
.libPaths("C:/Users/linde/AppData/Local/R/win-library/4.6")

# Install packages
install.packages(c("shiny", "tidyverse", "fda", "fda.usc", "plotly", "DT", "scales"))
packages <- c("shiny", "tidyverse", "fda", "fda.usc", "plotly", "DT", "scales")
install_if_missing <- function(p) {
  if (!require(p, character.only = TRUE)) install.packages(p)
}
invisible(lapply(packages, install_if_missing))

# Load libraries
library(shiny)
library(tidyverse)
library(fda)
library(fda.usc)
library(plotly)
library(DT)
library(scales)

# ============================================================================
# PART 1: GENERATE SIMULATED DATA FOR 2,000 STUDENTS
# ============================================================================

set.seed(2026)  # reproducible

n_students <- 2000
months <- 24  # 24 months = 2 years

# Student IDs
student_id <- 1:n_students

# ---- Demographics ----
gender <- sample(c("Female", "Male"), n_students, prob = c(0.55, 0.45), replace = TRUE)
age_group <- sample(c("20-25", "26-30", "31+"), n_students, 
                    prob = c(0.6, 0.3, 0.1), replace = TRUE)
school_quintile <- sample(1:5, n_students, prob = c(0.15, 0.2, 0.25, 0.25, 0.15), 
                          replace = TRUE)

# ---- Financial Aid (correlated with school quintile) ----
# Lower quintile -> more likely NSFAS
aid_prob <- function(q) {
  ifelse(q <= 2, 0.7, ifelse(q <= 3, 0.5, 0.2))
}
nsfas_prob <- sapply(school_quintile, aid_prob)
financial_aid <- sapply(nsfas_prob, function(p) {
  sample(c("NSFAS", "Bursary", "Self-funded"), 1, 
         prob = c(p, 0.25, 1 - p - 0.25))
})

# ---- Residence ----
residence <- sample(c("On-campus", "Off-campus", "With family"), n_students,
                    prob = c(0.3, 0.5, 0.2), replace = TRUE)

# ---- Academic and Graduation (correlated with debt trajectory) ----
# Base GPA (will be adjusted by financial aid and debt later)
gpa_base <- rnorm(n_students, mean = 65, sd = 12)
gpa <- pmax(0, pmin(100, gpa_base))  # clamp 0-100

# Failed modules count (poisson, influenced by GPA)
failed_modules <- round(pmax(0, pmin(5, 3 - (gpa - 50)/20 + rnorm(n_students, 0, 0.5))))

# Graduation flag (correlated with GPA, failed modules, and debt)
graduation_prob <- 1 / (1 + exp(-(0.05 * (gpa - 50) - 0.3 * failed_modules - 0.5)))
graduation <- rbinom(n_students, 1, graduation_prob)

# ============================================================================
# PART 2: GENERATE DEBT TRAJECTORIES (Monthly, 24 months)
# ============================================================================

# Create time vector (months 1-24)
time <- 1:months

# Function to generate debt trajectory based on student characteristics
generate_debt_trajectory <- function(aid_type, residence_type, gpa_score, grad_status) {
  
  # Base parameters
  base_debt <- 0
  slope <- case_when(
    aid_type == "NSFAS" ~ 200,
    aid_type == "Bursary" ~ 800,
    aid_type == "Self-funded" ~ 1500
  )
  
  # Residence effect
  residence_effect <- case_when(
    residence_type == "On-campus" ~ 5000,
    residence_type == "Off-campus" ~ 2000,
    residence_type == "With family" ~ 500
  )
  
  # Academic effect (lower GPA = higher debt accumulation)
  academic_effect <- (65 - gpa_score) * 50
  
  # Graduation effect (non-graduates have higher debt)
  grad_effect <- ifelse(grad_status == 0, 3000, 0)
  
  # Registration spikes (months 1, 13)
  registration_spike <- function(t) {
    spike <- 0
    if(t == 1) spike <- 8000 + residence_effect
    if(t == 13) spike <- 7000 + residence_effect * 0.8
    return(spike)
  }
  
  # Payment dips (months 3, 4, 15, 16, and monthly for self-funded)
  payment_dip <- function(t, aid) {
    dip <- 0
    # Semester payment dips
    if(t %in% c(3, 4, 15, 16)) dip <- -3000
    # Monthly payments for self-funded
    if(aid == "Self-funded" && t %% 3 == 0) dip <- dip - 1000
    return(dip)
  }
  
  # Build trajectory
  trajectory <- numeric(months)
  current_debt <- base_debt + residence_effect + academic_effect + grad_effect
  
  for(t in 1:months) {
    # Add registration spike
    current_debt <- current_debt + registration_spike(t)
    
    # Add monthly accumulation (slope)
    current_debt <- current_debt + slope
    
    # Add payment dips
    current_debt <- current_debt + payment_dip(t, aid_type)
    
    # Random variation (realistic noise)
    current_debt <- current_debt + rnorm(1, 0, 500)
    
    # Ensure debt doesn't go negative (can't have negative debt)
    current_debt <- max(0, current_debt)
    
    trajectory[t] <- current_debt
  }
  
  # Cap extremely high debt
  trajectory <- pmin(trajectory, 150000)
  
  return(trajectory)
}

# Generate trajectories for all students
cat("Generating debt trajectories for 2,000 students...\n")
debt_matrix <- matrix(NA, nrow = n_students, ncol = months)

for(i in 1:n_students) {
  debt_matrix[i, ] <- generate_debt_trajectory(
    financial_aid[i], residence[i], gpa[i], graduation[i]
  )
  if(i %% 200 == 0) cat("Completed", i, "of", n_students, "\n")
}

# Convert to long format for dashboard
debt_long <- expand.grid(student_id = student_id, month = 1:months)
debt_long$debt <- as.vector(t(debt_matrix))

# Create full dataset
student_data <- data.frame(
  student_id = student_id,
  gender = gender,
  age_group = age_group,
  school_quintile = school_quintile,
  financial_aid = financial_aid,
  residence = residence,
  gpa = round(gpa, 1),
  failed_modules = failed_modules,
  graduation = graduation
)

# Add trajectory summary stats
student_data$avg_debt <- rowMeans(debt_matrix)
student_data$final_debt <- debt_matrix[, months]
student_data$max_debt <- apply(debt_matrix, 1, max)
student_data$debt_slope <- (debt_matrix[, months] - debt_matrix[, 1]) / months

# Merge with long format
debt_long <- debt_long %>%
  left_join(student_data, by = "student_id")

cat("Data generation complete!\n")

# ============================================================================
# PART 3: FUNCTIONAL DATA ANALYSIS
# ============================================================================

# Create functional data object
debt_t <- t(debt_matrix)

# Basis functions (B-splines, 12 basis functions for 24 months)
basis <- create.bspline.basis(rangeval = c(1, months), nbasis = 12)

# Smooth the data
fd_obj <- smooth.basis(1:months, debt_t, basis)$fd

# Functional Principal Component Analysis
pca_fd_obj <- pca.fd(fd_obj, nharm = 5)

# Extract scores for clustering
pca_scores <- pca_fd_obj$scores[, 1:3]

# K-means clustering on PCA scores
set.seed(2026)
kmeans_result <- kmeans(pca_scores, centers = 5, nstart = 25)
student_data$cluster <- kmeans_result$cluster

# Add cluster to long format
debt_long <- debt_long %>%
  left_join(student_data %>% select(student_id, cluster), by = "student_id")

# Label clusters with descriptive names
cluster_names <- c("Stable Low Debt", "Moderate Increasing", "High Rapid Debt", 
                   "Delayed Payment", "Early Peak Recovery")
student_data$cluster_name <- factor(student_data$cluster, 
                                    levels = 1:5, 
                                    labels = cluster_names)
debt_long$cluster_name <- factor(debt_long$cluster, 
                                 levels = 1:5, 
                                 labels = cluster_names)

# ============================================================================
# PART 4: SHINY DASHBOARD
# ============================================================================

ui <- fluidPage(
  
  titlePanel(
    div(
      h2("CPUT Student Debt Trajectory Dashboard", style = "color: #003366;"),
      h5("Functional Data Analysis Demo | Simulated Data for Illustrative Purposes"),
      div("⚠️ DEMO ONLY — Not based on actual CPUT data", 
          style = "color: red; font-weight: bold; background-color: #ffeeee; padding: 5px;")
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      h4("Filters", style = "color: #003366;"),
      
      selectInput("cluster_filter", "Debt Cluster",
                  choices = c("All", cluster_names),
                  selected = "All"),
      
      selectInput("aid_filter", "Financial Aid",
                  choices = c("All", "NSFAS", "Bursary", "Self-funded"),
                  selected = "All"),
      
      selectInput("residence_filter", "Residence",
                  choices = c("All", "On-campus", "Off-campus", "With family"),
                  selected = "All"),
      
      selectInput("gender_filter", "Gender",
                  choices = c("All", "Female", "Male"),
                  selected = "All"),
      
      selectInput("graduation_filter", "Graduation Status",
                  choices = c("All", "Yes", "No"),
                  selected = "All"),
      
      sliderInput("gpa_range", "GPA Range",
                  min = 0, max = 100, value = c(0, 100)),
      
      hr(),
      
      h5("What you are seeing:"),
      p("Each line is one student's debt over 24 months. 
        Colours represent 5 trajectory clusters identified by Functional Data Analysis."),
      
      hr(),
      
      p(em("Disclaimer: This dashboard uses simulated data to demonstrate 
            Functional Data Analysis methodology. Real CPUT data would 
            produce different patterns."), 
        style = "font-size: 11px; color: gray;")
    ),
    
    mainPanel(
      width = 9,
      
      tabsetPanel(
        tabPanel("Debt Trajectories",
                 br(),
                 plotlyOutput("trajectory_plot", height = "500px"),
                 br(),
                 h4("Interpretation"),
                 verbatimTextOutput("interpretation")
        ),
        
        tabPanel("Cluster Comparison",
                 br(),
                 plotlyOutput("cluster_plot", height = "450px"),
                 br(),
                 h4("Cluster Statistics"),
                 DT::dataTableOutput("cluster_stats")
        ),
        
        tabPanel("Subgroup Analysis",
                 br(),
                 plotlyOutput("subgroup_plot", height = "450px"),
                 br(),
                 h4("Subgroup Summary"),
                 verbatimTextOutput("subgroup_summary")
        ),
        
        tabPanel("Student Data",
                 br(),
                 DT::dataTableOutput("data_table")
        ),
        
        tabPanel("About",
                 br(),
                 h4("About This Demo"),
                 p("This dashboard demonstrates Functional Data Analysis (FDA) 
                   applied to student debt trajectories."),
                 p("Key features:"),
                 tags$ul(
                   tags$li("2,000 simulated students"),
                   tags$li("24 months of monthly debt balances"),
                   tags$li("5 debt trajectory clusters identified via FDA + k-means"),
                   tags$li("Filters allow exploration by funding type, residence, gender, GPA, and graduation"),
                   tags$li("Built with R Shiny, fda, and plotly")
                 ),
                 p("Once real CPUT data is available, this same dashboard can be 
                   adapted to show actual student debt patterns."),
                 br(),
                 h4("Data Variables"),
                 tags$ul(
                   tags$li("Demographics: Gender, age group, school quintile"),
                   tags$li("Financial: NSFAS, Bursary, Self-funded"),
                   tags$li("Residence: On-campus, Off-campus, With family"),
                   tags$li("Academic: GPA, failed modules"),
                   tags$li("Outcome: Graduation flag")
                 )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive filtered data
  filtered_data <- reactive({
    data <- debt_long
    
    # Apply filters
    if(input$cluster_filter != "All") {
      data <- data %>% filter(cluster_name == input$cluster_filter)
    }
    if(input$aid_filter != "All") {
      data <- data %>% filter(financial_aid == input$aid_filter)
    }
    if(input$residence_filter != "All") {
      data <- data %>% filter(residence == input$residence_filter)
    }
    if(input$gender_filter != "All") {
      data <- data %>% filter(gender == input$gender_filter)
    }
    if(input$graduation_filter != "All") {
      grad_flag <- ifelse(input$graduation_filter == "Yes", 1, 0)
      data <- data %>% filter(graduation == grad_flag)
    }
    
    data <- data %>% filter(gpa >= input$gpa_range[1] & gpa <= input$gpa_range[2])
    
    data
  })
  
  # Main trajectory plot
  output$trajectory_plot <- renderPlotly({
    df <- filtered_data()
    
    # Sample for performance (show max 500 lines)
    if(length(unique(df$student_id)) > 500) {
      sampled_ids <- sample(unique(df$student_id), 500)
      df <- df %>% filter(student_id %in% sampled_ids)
    }
    
    p <- ggplot(df, aes(x = month, y = debt, group = student_id, 
                        color = cluster_name, text = paste("Student:", student_id,
                                                           "<br>Debt: R", comma(debt),
                                                           "<br>Cluster:", cluster_name))) +
      geom_line(alpha = 0.3, size = 0.4) +
      scale_color_manual(values = c("#2E86AB", "#A23B72", "#F18F01", "#C73E1D", "#6A994E")) +
      scale_y_continuous(labels = comma, name = "Debt (Rands)") +
      scale_x_continuous(name = "Month", breaks = seq(0, 24, 6), 
                         labels = c("Start", "Month 6", "Month 12", "Month 18", "Month 24")) +
      theme_minimal() +
      theme(legend.position = "bottom") +
      labs(title = "Student Debt Trajectories Over 24 Months",
           color = "Debt Cluster")
    
    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation = "h", y = -0.2))
  })
  
  # Interpretation text
  output$interpretation <- renderPrint({
    df <- filtered_data()
    n_students_filtered <- length(unique(df$student_id))
    avg_final_debt <- df %>% group_by(student_id) %>% 
      filter(month == max(month)) %>% 
      summarise(final = first(debt)) %>%
      pull(final) %>% mean(na.rm = TRUE)
    
    cat("Number of students shown:", n_students_filtered, "\n")
    cat("Average final debt (Month 24): R", comma(round(avg_final_debt)), "\n\n")
    cat("Cluster interpretation:\n")
    cat("• Stable Low Debt: Consistently low balances, likely NSFAS-funded\n")
    cat("• Moderate Increasing: Steady accumulation, typical of bursary students\n")
    cat("• High Rapid Debt: Fast accumulation, often self-funded with poor academic outcomes\n")
    cat("• Delayed Payment: Spikes followed by dips (late payments)\n")
    cat("• Early Peak Recovery: High initial debt at registration that gradually clears\n")
  })
  
  # Cluster comparison plot
  output$cluster_plot <- renderPlotly({
    df <- filtered_data()
    
    cluster_means <- df %>%
      group_by(month, cluster_name) %>%
      summarise(mean_debt = mean(debt, na.rm = TRUE), .groups = "drop")
    
    p <- ggplot(cluster_means, aes(x = month, y = mean_debt, color = cluster_name)) +
      geom_line(size = 1.2) +
      scale_color_manual(values = c("#2E86AB", "#A23B72", "#F18F01", "#C73E1D", "#6A994E")) +
      scale_y_continuous(labels = comma, name = "Mean Debt (Rands)") +
      scale_x_continuous(name = "Month", breaks = seq(0, 24, 6)) +
      theme_minimal() +
      labs(title = "Mean Debt Trajectory by Cluster",
           color = "Cluster")
    
    ggplotly(p)
  })
  
  # Cluster statistics table
  output$cluster_stats <- DT::renderDataTable({
    df <- filtered_data()
    
    cluster_summary <- df %>%
      group_by(cluster_name, student_id) %>%
      summarise(
        final_debt = last(debt),
        avg_debt = mean(debt),
        max_debt = max(debt),
        gpa = first(gpa),
        graduation_rate = first(graduation),
        .groups = "drop"
      ) %>%
      group_by(cluster_name) %>%
      summarise(
        Count = n(),
        `Final Debt (Mean)` = comma(round(mean(final_debt))),
        `Avg Debt (Mean)` = comma(round(mean(avg_debt))),
        `Max Debt (Mean)` = comma(round(mean(max_debt))),
        `Mean GPA` = round(mean(gpa), 1),
        `Graduation Rate` = paste0(round(mean(graduation_rate) * 100), "%")
      )
    
    DT::datatable(cluster_summary, options = list(pageLength = 10, dom = 't'),
                  rownames = FALSE)
  })
  
  # Subgroup analysis plot
  output$subgroup_plot <- renderPlotly({
    df <- filtered_data()
    req(nrow(df) > 0)
    
    # Get the first filter that is not "All" for comparison
    if(input$aid_filter == "All" && input$residence_filter == "All" && 
       input$gender_filter == "All" && input$graduation_filter == "All") {
      # Default: compare by financial aid
      subgroup_means <- df %>%
        group_by(month, financial_aid) %>%
        summarise(mean_debt = mean(debt, na.rm = TRUE), .groups = "drop")
      
      p <- ggplot(subgroup_means, aes(x = month, y = mean_debt, color = financial_aid)) +
        geom_line(size = 1.2) +
        scale_color_manual(values = c("#2E86AB", "#A23B72", "#F18F01")) +
        labs(title = "Mean Debt Trajectory by Financial Aid Type",
             color = "Financial Aid")
    } else if(input$residence_filter != "All") {
      subgroup_means <- df %>%
        group_by(month, residence) %>%
        summarise(mean_debt = mean(debt, na.rm = TRUE), .groups = "drop")
      
      p <- ggplot(subgroup_means, aes(x = month, y = mean_debt, color = residence)) +
        geom_line(size = 1.2) +
        scale_color_manual(values = c("#2E86AB", "#A23B72", "#F18F01")) +
        labs(title = "Mean Debt Trajectory by Residence Type",
             color = "Residence")
    } else if(input$gender_filter != "All") {
      subgroup_means <- df %>%
        group_by(month, gender) %>%
        summarise(mean_debt = mean(debt, na.rm = TRUE), .groups = "drop")
      
      p <- ggplot(subgroup_means, aes(x = month, y = mean_debt, color = gender)) +
        geom_line(size = 1.2) +
        scale_color_manual(values = c("#2E86AB", "#F18F01")) +
        labs(title = "Mean Debt Trajectory by Gender",
             color = "Gender")
    } else if(input$graduation_filter != "All") {
      subgroup_means <- df %>%
        group_by(month, graduation) %>%
        summarise(mean_debt = mean(debt, na.rm = TRUE), .groups = "drop") %>%
        mutate(graduation = ifelse(graduation == 1, "Graduated", "Did Not Graduate"))
      
      p <- ggplot(subgroup_means, aes(x = month, y = mean_debt, color = graduation)) +
        geom_line(size = 1.2) +
        scale_color_manual(values = c("#2E86AB", "#F18F01")) +
        labs(title = "Mean Debt Trajectory by Graduation Status",
             color = "Graduation")
    } else {
      # Fallback: show by cluster
      subgroup_means <- df %>%
        group_by(month, cluster_name) %>%
        summarise(mean_debt = mean(debt, na.rm = TRUE), .groups = "drop")
      
      p <- ggplot(subgroup_means, aes(x = month, y = mean_debt, color = cluster_name)) +
        geom_line(size = 1.2) +
        scale_color_manual(values = c("#2E86AB", "#A23B72", "#F18F01", "#C73E1D", "#6A994E")) +
        labs(title = "Mean Debt Trajectory by Cluster",
             color = "Cluster")
    }
    
    p <- p +
      scale_y_continuous(labels = comma, name = "Mean Debt (Rands)") +
      scale_x_continuous(name = "Month", breaks = seq(0, 24, 6)) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Subgroup summary text
  output$subgroup_summary <- renderPrint({
    df <- filtered_data()
    
    if(input$aid_filter != "All") {
      cat("Currently filtered by:", input$aid_filter, "\n")
      cat("The chart above shows debt trajectories broken down by residence type.\n")
      cat("This helps identify which housing arrangements correlate with debt patterns.")
    } else if(input$residence_filter != "All") {
      cat("Currently filtered by:", input$residence_filter, "\n")
      cat("The chart above shows debt trajectories broken down by financial aid type.\n")
      cat("This reveals how NSFAS vs self-funded students differ in debt accumulation.")
    } else if(input$gender_filter != "All") {
      cat("Currently filtered by:", input$gender_filter, "\n")
      cat("The chart above shows debt trajectories by gender.\n")
    } else if(input$graduation_filter != "All") {
      cat("Currently filtered by: Graduation =", input$graduation_filter, "\n")
      cat("The chart above compares debt trajectories of graduates vs non-graduates.\n")
    } else {
      cat("No subgroup filter applied.\n")
      cat("Select a financial aid, residence, gender, or graduation filter above\n")
      cat("to see debt trajectories broken down by a second variable.")
    }
  })
  
  # Data table
  output$data_table <- DT::renderDataTable({
    df <- filtered_data() %>%
      select(student_id, financial_aid, residence, gender, gpa, 
             failed_modules, graduation, cluster_name, month, debt) %>%
      distinct() %>%
      head(1000)
    
    DT::datatable(df, options = list(pageLength = 20, scrollX = TRUE),
                  rownames = FALSE) %>%
      formatCurrency(columns = "debt", currency = "R", interval = 3, mark = ",")
  })
}

# Run the dashboard
shinyApp(ui = ui, server = server)

