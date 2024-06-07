


# List of required packages
required_packages <- c("shiny", "tidyverse", "viridis", "plotly", "DT")

# Function to check and install missing packages
install_if_missing <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# Apply the function to each required package
sapply(required_packages, install_if_missing)


library(shiny)
library(tidyverse)
library(viridis)
library(plotly)
library(DT)

# Function to read and preprocess data
read_and_preprocess_data <- function(file_path, paper_labels, what_levels) {
  read.table(file_path, dec = ",", header = TRUE, fill = TRUE, skipNul = TRUE, sep = ",",
             stringsAsFactors = FALSE, encoding = "UTF-8") %>%
    #left_join(., "data/papers_prosody_on_spectrum.csv", by = "Paper")
    mutate(Label = case_when(!!!paper_labels),
           What_f = factor(What, levels = what_levels),
           Label = as.factor(Label),
           Label = factor(Label, levels = rev(levels(Label))),
           source = str_c(Link))
           #source = str_c("https://www.google.com/"))
}


# Function to render plot
render_plot <- function(data, title) {
  ggplot(data) +
    geom_segment(aes(x = Label, y = min, xend = Label, yend = max, group = Label, colour = differences), lineend = "round", stat = "identity", size = 5) +
    geom_text(aes(x = Label, y = sqrt(min * max), label = Label), colour = "black", size = 3.3, hjust = 0.5, vjust = 0.4) +
    ggtitle(title) +
    coord_flip() +
    theme_bw() +
    scale_colour_viridis_d(begin = 0.98, end = 0.66, option = "cividis") +
    theme_common() +
    ylab("age range") +
    scale_y_continuous(trans = 'log2', breaks = c(4, 8, 12, 16, 20, 30, 40, 50, 60)) +
    facet_grid(What_f ~ ., scales = "free_y", space = "free", drop = TRUE) +
    theme(strip.background = element_rect(fill = "white"), strip.text.y = element_text(size = 10, angle = 0))
  
}


theme_common <- function() {
  theme(legend.position = "top", legend.direction = "horizontal",
        legend.title = element_blank(), legend.text = element_text(size = 16),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 14),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.ticks.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 18))
}


# UI
ui <- fluidPage(
  titlePanel("Prosody on the spectrum"),
  
  tags$p('This website was created to accompany our review article "Linguistic Prosody in Autism Spectrum Disorder—An Overview" (Grice, Wehrle, Krüger, Spaniol, Cangemi & Vogeley, 2023) published in Language and Linguistics Compass. We plan to update all figures and tables as and when new papers on this topic are published. We therefore encourage researchers to inform us of any work that they believe should feature in the online version of this overview, which is intended be updated continuously and indefinitely. Stay tuned!'),
  
  tags$h4("Submitting a new paper"),
  tags$p("If you would like to submit a new paper, ", 
         a("follow this link ", href = "https://docs.google.com/forms/d/e/1FAIpQLSd2ihjBOqpgrEdd3IRdpeLmzFm-93F5QWYzqtjn-XQKEYgNkw/viewform", target = "_blank"),"to the paper submission form."),
  
  
  mainPanel(
    tabsetPanel(
      tabPanel("Perception",
               div(style = "height: 700px", plotOutput("perceptionPlot")),
               div(style = "margin-top: 20px;", DTOutput("perceptionTable"))
      ),
      tabPanel("Production",
               div(style = "height: 700px;", plotOutput("productionPlot")),
               div(style = "margin-top: 20px;", DTOutput("productionTable"))
      ),
      tabPanel("General Characteristics",
               div(style = "height: 700px;", plotOutput("generalPlot")),
               div(style = "margin-top: 20px;", DTOutput("generalTable"))
      )
      
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Preprocessing General Characteristics Data
  generalData <- reactive({
    paper_labels <- alist(
      Paper == "Bonneh et al. 2011" ~ "Bonneh et al. 2011",
      TRUE ~ Paper
    )
    what_levels <- c("pauses", "pitch contours", "pitch dynamics", "pitch range", 'rhythm')
    read_and_preprocess_data("data/general_characteristics_SW_0722.csv", paper_labels, what_levels)
  })
  
  # Rendering General Characteristics Plot
  output$generalPlot <- renderPlot({
    render_plot(generalData(), "General Characteristics")
  }, res = 72, height = 700, width = 900)
  
  # Preprocessing Perception Data
  perceptionData <- reactive({
    paper_labels <- alist(
      Paper == "Zhang et al. 2018" ~ "Zhang",
      Paper == "Chevallier et al. 2011" ~ "Chevallier",
      Paper == "Hesling et al. 2010" ~ "Hesling",
      Paper == "Filipe et al. 2014" ~ "Filipe",
      Paper == "Wang et al. 2021" ~ "Wang 2021",
      TRUE ~ Paper
    )
    what_levels <- c('lexical tone', 'lexical stress', 'syntactic structure', 'speech acts', 'turn-taking', 'information structure', 'intentions', 'emotional states')
    read_and_preprocess_data("data/perception_SW_0722.csv", paper_labels, what_levels)
  })
  
  # Rendering Perception Plot
  output$perceptionPlot <- renderPlot({
    render_plot(perceptionData(), "Perception")
  }, res = 72, height = 700, width = 900)
  #res = 72, height = function() session$clientData$output_perceptionPlot_height, width = function() session$clientData$output_perceptionPlot_width)
  
  
  # Preprocessing Production Data
  productionData <- reactive({
    paper_labels <- alist(
      Paper == "Hesling et al. 2010" ~ "Hesling",
      Paper == "Filipe et al. 2014" ~ "Filipe",
      Paper == "Gargan & Andrianopoulos 2022" ~ "Gargan 2022",
      TRUE ~ Paper
    )
    what_levels <- c('lexical tone', 'lexical stress', 'syntactic structure', 'speech acts', 'turn-taking', 'info. structure - focus', 'info. structure - contrast', 'emotional states')
    read_and_preprocess_data("data/production_SW_0722.csv", paper_labels, what_levels)
  })
  
  # Rendering Production Plot
  output$productionPlot <- renderPlot({
    render_plot(productionData(), "Production")
  }, res = 72, height = 700, width = 900)
  
  # Extracting Label and Source info for Perception
  perceptionLabelSource <- reactive({
    perceptionData() %>%
      select(Label, source, What, differences, min, max, participants, language) %>%
      distinct() %>%
      mutate(source = lapply(source, function(url) as.character(a(href = url, target="_blank", url))))
  })
  
  
  
  # Rendering Perception Table
  output$perceptionTable <- renderDT({
    datatable(perceptionLabelSource(), filter = 'top')
  })
  
  # Extracting Label and Source info for General
  generalLabelSource <- reactive({
    generalData() %>%
      select(Label, source, What, differences, min, max, participants, language) %>%
      distinct() %>%
      mutate(source = lapply(source, function(url) as.character(a(href = url, target="_blank", url))))
  })
  
  # Rendering Perception Table
  output$generalTable <- renderDT({
    datatable(generalLabelSource(), filter = 'top')
  })
  
  # Extracting Label and Source info for Production
  productionLabelSource <- reactive({
    productionData() %>%
      select(Label, source, What, differences, min, max, participants, language) %>%
      distinct() |> 
      mutate(source = lapply(source, function(url) as.character(a(href = url, target="_blank", url))))
    
  })
  
  # Rendering Production Table
  #changed renderTable to renderDT to make it filterable
  # Use DTOutput instead of tableOutput in the UI to create the space for the tables.
  # Use renderDT instead of renderTable in the server to render the tables.
  
  output$productionTable <- renderDT({
    productionLabelSource()
  })
  
}


# Run the application 
shinyApp(ui = ui, server = server)
