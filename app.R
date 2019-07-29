library(shiny)
library(gh)
library(ECharts2Shiny)




ui = bootstrapPage(
    loadEChartsLibrary(),
    
    column(12,offset=4, titlePanel("GitHub Repository Language Breakdown")),
    
    column(12, offset = 4, uiOutput('variables')),
    
    tags$div(id="test", style="width:50%;height:400px;"),
    deliverChart(div_id = "test"),
    
    
    column(12, offset =1,textInput("text", label = h4("Search GitHub username:")))
    
)
    
server = function(input, output){
    
    values = reactiveValues()
    
    outVar = reactive({
        if(input$text > 0) {
            url = paste("/users/", input$text, sep = "")
            
            tryCatch({
                repo = gh(url)
                values$variable = repo$name
                repos_path = paste("/users/", input$text, "/repos", sep = "")
                j_repos = gh(repos_path)
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
        

        
    repo_react = reactive({
            tryCatch({
                print("hi1")
                url2 = paste("GET /repos/", input$text, "/", input$variables2, "/languages", sep = "")
                print("hi2")
                language_list = gh(url2)
                print("hi3")
                total = 0
                language = c()
                df = c()
                print("hi4")
                for(i in 1:length(language_list)) {
                    language = names(language_list)[i]
                    df = c(df, c(rep(names(language_list)[i], language_list[[language]])))
                }
                print("hi5")
                print(df)    
                return(df)
            
            }, error = function(e) {
                print("error Repository failed to load")
                #return(c(rep("No Values", 1)))
            }, warning = function(w) {
                print("warning Repository failed to load")
                #return(c(rep("No Values", 1)))
            
            }) 
            
    })
   
    observeEvent(repo_react(),
                 {
                    renderPieChart(div_id = "test", 
                                   data = repo_react())
                 })

}

shinyApp(ui = ui, server = server)
