library(shiny)
library(gh)
library(ECharts2Shiny)

ui = fluidPage(
  loadEChartsLibrary(),
  
  column(12,offset=4, titlePanel("GitHub Repository Language Breakdown")),
  
  sidebarLayout(
    sidebarPanel(textInput("text", label = h4("Search GitHub username:")),
                 uiOutput('variables')
    ),
    
    mainPanel(
      h3("Selected Repository Statistics"),
      tags$div(id="test", style="width:50%;height:400px;"),
      deliverChart(div_id = "test"),
      h3("All Repository Statistics"),
      tags$div(id="test1", style="width:50%;height:400px;"),
      deliverChart(div_id = "test1")
    ))
)

server = function(input, output){
  
  values = reactiveValues()
  
  outVar = reactive({
    if(input$text > 0) {
      url = paste("/users/", input$text, sep = "")
      
      tryCatch({
        repo = gh(url, .token = '3XXXX')
        values$variable = repo$name
        repos_path = paste("/users/", input$text, "/repos", sep = "")
        j_repos = gh(repos_path, .token = '3XXXX')
        values$repo_list = vapply(j_repos, "[[", "", "name")
        return(values$repo_list)
        
      }, error = function(e) {
        values$variable = "GitHub username does not exist"
        return("Invalid User")
      })
      
    }
  })
  
  output$variables = renderUI({
    name = paste(values$variable, " Repositories")
    selectInput('variables2', name, outVar())
  })
  
  
  
  pie_react = reactive({
    tryCatch({
      url2 = paste("GET /repos/", input$text, "/", input$variables2, "/languages", sep = "")
      language_list = gh(url2, .token = '3XXXX')
      language = c()
      df = c()
      for(i in 1:length(language_list)) {
        language = names(language_list)[i]
        df = c(df, c(rep(names(language_list)[i], language_list[[language]])))
      } 
      return(df)
      
    }, error = function(e) {
      print("error Repository failed to load for pie chart")
    }, warning = function(w) {
      print("warning Repository failed to load for pie chart")
    }) 
    
  })
  
  bar_react = reactive({
    tryCatch({
      language3 = c()
      languages_to_unique = c()
      df3 = c()
      for (i in values$repo_list) {
        url3 = paste ("GET /repos/", input$text, "/", i, "/languages", sep = "")
        language_list3 = gh(url3, .token = '3XXXX')
        print(language_list3)
        if(!is.list(language_list3)) {
          next()
        }
        for (j in 1:length(language_list3)) {
          language3 = names(language_list3)[j]
          df3 = c(df3, c(rep(names(language_list3)[j], language_list3[[language3]])))
        }
      }
      if(is.null(df3)) {
        return(NULL)
      }
      df3 = table(df3)
      print(df3)
      name = c()
      number = c()
      for(i in 1:length(df3)) {
        name = c(name, (names(df3)[i]))
      }
      for(i in df3) {
        number = c(number, i)
      }
      dfz = data.frame()
      print(dfz)
      for(i in 1:length(df3)) {
        dfz = rbind(dfz, c(names(df3)[i]))
      }
      
      dat = data.frame(number)
      row.names(dat) = name
      print(dat)
      return(dat)
    }, error = function(e) {
      print("error Repository failed to load for bar chart")
    }, warning = function(w) {
      print("warning Repository failed to load for bar chart")
    })
    
    
  })
  
  observeEvent(pie_react(),
               {
                 renderPieChart(div_id = "test", 
                                data = pie_react())
               })
  
  observeEvent(bar_react(),
               {
                 print(bar_react())
                 
                 renderBarChart(div_id = "test1", grid_left = '1%', direction = "vertical",
                                data = bar_react())
               })

}

shinyApp(ui = ui, server = server)