library(shiny)
read.csv("/Users/Caspar/Documents/GitHub_Project/pokemon_sword-shield/pokemon_list.csv")
# Use a fluid Bootstrap layout
fluidPage(    
  
  # Give the page a title
  titlePanel("Pokemons by Primary Type"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with one input
    sidebarPanel(
      selectInput("type_1","Primary Type:", 
                  choices=unique(pokemon_list$type_1)),
      hr(),
      helpText("Data from IGN")
    ),
    
    # Create a spot for the scatterplot
    mainPanel(
      plotOutput("Plot")  
    )
    
  )
)

server <- function(input, output) {
  
  # Define a server for the Shiny app
  function(input, output) {
    
    # Fill in the spot we created for a plot
    output$Plot <- renderPlot({
      
      # Render a barplot
      ggplot(data=pokemon_list,
             aes(x=attack,y=defense,color=input))+
        geom_point()
    })
  }
}
shinyApp(ui = ui, server = server)