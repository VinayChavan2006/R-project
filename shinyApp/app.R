# Loading Necessary libraries

library(shiny)
library(bslib)
library(ggplot2)
library(shinythemes)
library(dplyr)
library(gganimate)
library(reshape2)
library(tibble)
library(plotly)
library(rvest)
library(tidyverse)
library(gifski)
library(png)
library(shinycssloaders)
library(DT)
library(shinyjs)


# Define a custom theme 
my_theme <- bs_theme( bg = "#343138", fg = "white", primary = "#F39F5A", secondary = "#AE445A")


# Loading Data Required
load(url("https://raw.githubusercontent.com/VinayChavan2006/R-project/main/Data/datasets/Continent.Rdata"))
load(url("https://raw.githubusercontent.com/VinayChavan2006/R-project/main/Data/datasets/olympicsDat.Rdata"))
load(url("https://raw.githubusercontent.com/VinayChavan2006/R-project/main/Data/datasets/SexRatio.RData"))
load(url("https://raw.githubusercontent.com/VinayChavan2006/R-project/main/Data/datasets/NominalGDP.Rdata"))
load(url("https://raw.githubusercontent.com/VinayChavan2006/R-project/main/Data/datasets/Lifeexp.Rdata"))

# Slight Modifications in Colnames
colnames(olympics)[colnames(olympics) == "Team"] = "Country"
colnames(sex_ratio_data)[colnames(sex_ratio_data) == '`Country/region`'] = "Country"

# Grouping and Joining of Data
olympics <- olympics %>%
  inner_join(continent_data,by = 'Country')
conts <- unique(olympics$Continent)

medalDat <- olympics %>% select(Medal,Country,Year) %>%
  group_by(Country) %>% 
  summarise( Gold = sum(Medal == "Gold", na.rm = TRUE),
             Silver = sum(Medal == "Silver", na.rm = TRUE),
             Bronze = sum(Medal == "Bronze", na.rm = TRUE), 
             Total = Gold + Silver + Bronze ) %>% arrange(desc(Total)) %>%
  inner_join(continent_data,by = 'Country')

sports_data <- olympics %>% group_by(Country,Sport) %>%
  summarise( Gold = sum(Medal == "Gold", na.rm = TRUE),
             Silver = sum(Medal == "Silver", na.rm = TRUE), 
             Bronze = sum(Medal == "Bronze", na.rm = TRUE), 
             Total = Gold + Silver + Bronze ) %>% 
  arrange(desc(Total)) %>% head(50)


# UI
ui <- fluidPage(
  useShinyjs(),
  theme = my_theme,
  titlePanel("Olympic Data Visualization"),
  sidebarLayout(
    sidebarPanel(
      # Inputs for the Overview tab
      conditionalPanel(
        condition = "input.tabs == 'Social Factors'",
        selectInput(
          "factorSelection",
          "Select Social Factor:",
          choices = c("Sex Ratio", "GDP", "Life Expectancy")
        ),
        
      ),
      # Input for the Social Factors tab
      conditionalPanel(
        condition = "input.tabs == 'Overview'",
        sliderInput("bins", "Number of Top Countries:", min = 1, max = 30, value = 15),
        selectInput(
          "continentSelection",
          "Select Continents:",
          choices = unique(continent_data$Continent),
          selected = unique(continent_data$Continent),
          multiple = TRUE
        ),
        selectInput(
          "medalFactorType",
          "Choose Medal Type:",
          choices = c("Gold", "Silver", "Bronze", "Total"),
          selected = "Total"
        )
      )
    ),
    mainPanel(
      tabsetPanel(
        id = "tabs",
        tabPanel("Overview",
                 tabsetPanel(
                   tabPanel("Plot", withSpinner(plotOutput("TopPlot"))),
                   tabPanel("Data", withSpinner(DTOutput("medalTable")))
                 )
        ),
        tabPanel("Social Factors",
                 tabsetPanel(
                   tabPanel("Plot", withSpinner(plotOutput("socialPlot"))),
                   tabPanel("Data", withSpinner(DTOutput("socialData")))
                 )
        ),
        tabPanel("Trends In Olympics",
                 tabsetPanel(
                   fluidRow(
                     column(12, withSpinner(plotOutput("heatmapPlot"))),
                     column(6, withSpinner(plotOutput("piePlot"))),
                     column(6, withSpinner(plotOutput("genderPiePlot")))
                   )
                 )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive filtered data based on selected continents
  filtered_olympics <- reactive({
    olympics %>%
      filter(Continent %in% input$continentSelection)
  })
  
  # Medal Table
  output$medalTable <- renderDT({
    medalDat <- filtered_olympics() %>%
      group_by(Country) %>%
      summarise(
        Gold = sum(Medal == "Gold", na.rm = TRUE),
        Silver = sum(Medal == "Silver", na.rm = TRUE),
        Bronze = sum(Medal == "Bronze", na.rm = TRUE),
        Total = Gold + Silver + Bronze
      ) %>%
      arrange(desc(Total))
    
    datatable(medalDat, options = list(pageLength = 10, autoWidth = TRUE))
  })
  
  # Overview plot
  output$TopPlot <- renderPlot({
    n <- input$bins
    medal_type <- input$medalFactorType
    
    medalDat <- filtered_olympics() %>%
      group_by(Country) %>%
      summarise(
        Gold = sum(Medal == "Gold", na.rm = TRUE),
        Silver = sum(Medal == "Silver", na.rm = TRUE),
        Bronze = sum(Medal == "Bronze", na.rm = TRUE),
        Total = Gold + Silver + Bronze
      ) %>%
      arrange(desc(Total)) %>%
      head(n)
    
    top_vals <- medalDat %>%
      select(Country, all_of(medal_type))
    
    # Convert the data to long format
    top_vals_long <- melt(top_vals, id.vars = "Country", variable.name = "Medal", value.name = "Count")
    
    # Set the factor levels for Country based on the sorted order
    top_vals_long$Country <- factor(top_vals_long$Country, levels = top_vals$Country)
    
    ggplot(data = top_vals_long, aes(x = Country, y = Count, fill = Medal)) +
      geom_bar(stat = "identity", alpha = 0.7) + 
      scale_fill_manual(values = c("Gold" = "#FFD700", "Silver" = "#D7D7D7", "Bronze" = "#CC6600", "Total" = "#F39F5A")) +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#343138", color = "white"),
        plot.background = element_rect(fill = "#343138", color = "white"),
        legend.background = element_rect(fill = "#343138", color = "white"),
        axis.title.x = element_text(color = "white"),
        axis.title.y = element_text(color = "white"),
        plot.title = element_text(color = "white"),
        axis.text.x = element_text(angle = 30, hjust = 1),
        axis.text = element_text(color = "white"),
        legend.text = element_text(color = "white"),
        legend.title = element_text(color = "white")
      ) + 
      labs(
        x = "Country",
        y = "Medals",
        title = paste0("Medals (", medal_type, ") won by top ", n, " countries (1896-2016)")
      )
  })
  
  
  # Social Factors Plot
  output$socialPlot <- renderPlot({
    medal_type <- input$medalFactorType
    if (input$factorSelection == "Sex Ratio") {
      # PARTICIPATION OF MALE AND FEMALE ATHLETES OVER TIME, WITHOUT ART COPETITIONS
      
      dataOlympics <- olympics %>% 
        filter(Sport != "Art Competitions")
      
      
      # AFTER 1992, CHANGE THE YEAR OF THE WINTER GAMES TO COINCIDE WITH THE NEXT SUMMER GAMES. THE TERM "YEAR" CURRENTLY REFERS TO THE OLYMPICS TOOK PLACE
      
      original <- c(1994,1998,2002,2006,2010,2014)
      
      new <- c(1996,2000,2004,2008,2012,2016)
      
      for (i in 1:length(original)) {
        dataOlympics$Year <- gsub(original[i], new[i], dataOlympics$Year)
      }
      
      dataOlympics$Year <- as.integer(dataOlympics$Year)
      
      
      # COUNT NUMBER OF ATHLETES BY SEX AND YEAR
      
      countsSex <- dataOlympics %>% 
        group_by(Year, Sex) %>%
        summarize(Athletes = length(unique(ID)))
      
      countsSex$Year <- as.integer(countsSex$Year)
      
      p <- ggplot(countsSex, aes(x=Year, y=Athletes, group=Sex, color=Sex)) +
        geom_point(size=2) +
        geom_line()  +
        scale_color_manual(values=c("#FFDDAE","#CDC1FF")) +
        theme_minimal() +
        theme(
          panel.grid = element_blank(),
          panel.background = element_rect(fill = "#343138", color = "white"),
          plot.background = element_rect(fill = "#343138", color = "white"), 
          legend.background = element_rect(fill = "#343138", color = "white"), 
          axis.title.x = element_text(color = "white"),
          axis.title.y = element_text(color = "white"), 
          plot.title = element_text(color = "white"),
          axis.text = element_text(color = "white"),
          legend.text = element_text(color = "white"),
          legend.title = element_text(color = "white")
        ) +
        labs(x = "Year", y = "Athletes", 
             title="Male and Female athletes over time", 
             subtitle = "Olympic Games (1896 to 2016)")
      p
    }else if (input$factorSelection == "GDP") {
      gdp_data <- gdp_data %>% select(Country,`2016`)
      #olympics <- olympics %>%
      #  inner_join(continent_data,by = 'Country')
      conts <- unique(olympics$Continent)
      
      gdpDat <- olympics %>%
        filter(Continent %in% conts) %>%
        filter(Sex == "F") %>%
        group_by(Country) %>%
        summarise( Gold = sum(Medal == "Gold", na.rm = TRUE),
                   Silver = sum(Medal == "Silver", na.rm = TRUE), 
                   Bronze = sum(Medal == "Bronze", na.rm = TRUE), 
                   Total = Gold + Silver + Bronze ) %>% 
        arrange(desc(Total)) %>% 
        inner_join(gdp_data,by = 'Country') %>% 
        mutate(GDP_category = ifelse(`2016` >= mean(`2016`), "Above Average", "Below Average"))
      # Create the box plots
      ggplot(gdpDat, aes(y = GDP_category, x = Total)) +
        geom_boxplot(aes(fill = GDP_category), color = "#FFDDAE", outlier.color = "#FFDDAE") + 
        scale_x_log10() +
        theme_minimal() + 
        labs(title = "Total Medals by GDP Category", y = "GDP Category", x = "Total Medals") + 
        theme_minimal() + theme(
          panel.grid = element_blank(), 
          panel.background = element_rect(fill = "#343138", color = "white"),
          plot.background = element_rect(fill = "#343138", color = "white"), 
          legend.background = element_rect(fill = "#343138", color = "white"), 
          axis.title.x = element_text(color = "white"),
          axis.title.y = element_text(color = "white"), 
          plot.title = element_text(color = "white"),
          axis.text = element_text(color = "white"),
          legend.text = element_text(color = "white"),
          legend.title = element_text(color = "white")
        )

    } else if (input$factorSelection == "Life Expectancy") {
      lifeExpDat <- olympics %>%
        filter(Continent %in% conts) %>%
        filter(Sex == "F") %>%
        group_by(Country) %>%
        summarise( Gold = sum(Medal == "Gold", na.rm = TRUE),
                   Silver = sum(Medal == "Silver", na.rm = TRUE), 
                   Bronze = sum(Medal == "Bronze", na.rm = TRUE), 
                   Total = Gold + Silver + Bronze ) %>% 
        arrange(desc(Total)) %>% 
        inner_join(life_expectancy_data,by = 'Country')  %>%
        mutate(Total_Medals = ifelse(Total >= mean(Total), "Above Average", "Below Average")) %>%
        inner_join(continent_data,by = 'Country')
      
      # Add a custom color column
      lifeExpDat$Custom_Color <- ifelse(lifeExpDat$Country == "India", "India", lifeExpDat$Total_Medals)
      lifeExpDat$Custom_Color <- factor(lifeExpDat$Custom_Color, levels = c("India", "Above Average", "Below Average"))
      
      # Plot with continent-based coloring
      lifeexp_plot <- ggplot(data = lifeExpDat, aes(x = `Females  Life Expectancy`, y = Total, text = Country, col = Continent)) +
        geom_point() +
        scale_color_manual(values = c(
          "Africa" = "#FF6347", "Asia" = "#4682B4", "Europe" = "#32CD32", 
          "North America" = "#FFD700", "Oceania" = "#8A2BE2", "South America" = "#FF69B4")) +  # Adjust colors as needed
        geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
        scale_x_continuous(limits = c(60, 100)) + 
        scale_y_continuous(limits = c(1, 1000)) + 
        scale_y_log10() +
        labs(title = "Total Medals Won by Females (Country-wise)", x = "Life Expectancy of females", y = "Total Medals") +
        theme_minimal() + 
        theme( 
          panel.grid = element_blank(),
          panel.background = element_rect(fill = "#343138", color = "white"),
          plot.background = element_rect(fill = "#343138", color = "white"), 
          legend.background = element_rect(fill = "#343138", color = "white"), 
          axis.title.x = element_text(color = "white"),
          axis.title.y = element_text(color = "white"), 
          plot.title = element_text(color = "white"),
          axis.text = element_text(color = "white"),
          legend.text = element_text(color = "white"),
          legend.title = element_text(color = "white")
        )
      lifeexp_plot

    }
  })
    
  output$socialData <- renderDT({
    medal_type <- input$medalFactorType
    if (input$factorSelection == "Sex Ratio") {
      datatable(sex_ratio_data %>% select(`Country/region`,Total), options = list(pageLength = 10, autoWidth = TRUE)) 
    }else if (input$factorSelection == "GDP") {
      datatable(gdp_data %>% select(Country,`2016`), options = list(pageLength = 10, autoWidth = TRUE)) 
    }else if (input$factorSelection == "Life Expectancy") {
      datatable(life_expectancy_data %>% select(Country,`Females  Life Expectancy`), options = list(pageLength = 10, autoWidth = TRUE)) 
    }
  })
  
  output$heatmapPlot <- renderPlot({
    sports_data <- olympics %>% group_by(Country,Sport) %>%
      summarise( Gold = sum(Medal == "Gold", na.rm = TRUE),
                 Silver = sum(Medal == "Silver", na.rm = TRUE), 
                 Bronze = sum(Medal == "Bronze", na.rm = TRUE), 
                 Total = Gold + Silver + Bronze ) %>% 
      arrange(desc(Total)) %>% head(50)
    
    
    # Transform the data to wide format
    sports_data_wide <- dcast(sports_data, Country ~ Sport, value.var = "Total", fill = 0)
    # Melt the data back to long format for ggplot2 
    sports_data_long <- melt(sports_data_wide, id.vars = "Country") 
    # Create the heat map
    heatmapPlot <- ggplot(sports_data_long, aes(x = variable, y = Country, fill = value,text = value)) + 
      geom_tile(color = "white") + 
      scale_fill_gradient(low = "white", high = "brown") + 
      labs(title = "Heat Map of Medals Won by Country and Sport", x = "Sport", y = "Country", fill = "Medals") +
      theme_minimal() + 
      theme( panel.grid = element_blank(), 
             axis.text.x = element_text(angle = 45, hjust = 1),
             panel.background = element_rect(fill = "#343138", color = "white"),
             plot.background = element_rect(fill = "#343138", color = "white"), 
             legend.background = element_rect(fill = "#343138", color = "white"), 
             axis.title.x = element_text(color = "white"),
             axis.title.y = element_text(color = "white"), 
             plot.title = element_text(color = "white"),
             axis.text = element_text(color = "white"),
             legend.text = element_text(color = "white"),
             legend.title = element_text(color = "white")
      )
    heatmapPlot
  })
  output$piePlot <- renderPlot({
    continent_medals <- medalDat %>%
      group_by(Continent) %>%
      summarize(TotalMedals = sum(Total))
    ggplot(continent_medals, aes(x = "", y = TotalMedals, fill = Continent)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y") +
      ggtitle("Medal Share by Continent") +
      theme_minimal() + theme(
        panel.grid = element_blank(), 
        panel.background = element_rect(fill = "#ECDFCC"),
        plot.background = element_rect(fill = "#ECDFCC"), 
        legend.background = element_rect(fill = "#ECDFCC"), 
      )
  })
  output$genderPiePlot <- renderPlot({
    # Assuming your dataset is 'medalDat' and it has columns 'Gender' and 'Total'
    dat <- olympics %>% group_by(Country) %>%
      summarise( Gold = sum(Medal == "Gold", na.rm = TRUE),
                 Silver = sum(Medal == "Silver", na.rm = TRUE), 
                 Bronze = sum(Medal == "Bronze", na.rm = TRUE), 
                 Total = Gold + Silver + Bronze ) 
    gender_medals <- dat %>%
      inner_join(olympics) %>% select(Sex,Total) %>%
      group_by(Sex) %>%
      summarise(TotalMedals = sum(Total))
    
    # Create the pie chart
    ggplot(gender_medals, aes(x = "", y = TotalMedals, fill = Sex)) + 
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y") +
      ggtitle("Medals Won by Males vs Females") +
      theme_minimal() +
      theme(axis.text = element_blank(), axis.title = element_blank()) +
      scale_fill_manual(values = c("pink", "blue"))  # Blue for males, pink for females
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
