library(shiny)
library(gh)
library(ECharts2Shiny)

dat <- c(rep("Type-A", 8),
         rep("Type-B", 5),
         rep("Type-C", 1))


    ui = bootstrapPage(
        loadEChartsLibrary(),
        
        column(12,offset=4, titlePanel("GitHub Repository Language Breakdown")),
        column(12, offset =1,textInput("text", label = h4("Search GitHub username:"))),
        
        column(12, offset = 4, uiOutput('variables')),
        textOutput('selected_repo'),
        
        tags$div(id="test", style="width:50%;height:400px;"),
        deliverChart(div_id = "test")
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
                    url2 = paste("GET /repos/", input$text, "/", input$variables2, "/languages", sep = "")
                    language_list = gh(url2)
                    total = 0
                    print(language_list)
                    for(i in 1:length(language_list)) {
                        language = names(language_list)[i]
                        total = total + language_list[[language]]
                    }
                    
                    #cmd = ""
                    df = data.frame()
                    for (i in 1:length(language_list)) {
                        language = names(language_list)[i]
                        percent = (language_list[[language]] / total) * 100
                        df = rbind(df, data.frame(rep(language, percent)))
                        #cmd = paste(cmd, language, " : ", percent, "%", sep = "")
                        #cmd = paste(cmd, "<br />", sep = "")
                    }
                    return(df)
                
                }, error = function(e) {
                    print("Repository failed to load")
                    return(data.frame(c(rep("No Values", 1))))
                }, warning = function(w) {
                    print("Repository failed to load")
                    return(data.frame(c(rep("No Values", 1))))
                })
            
        })
        
        output$selected_repo = renderUI({
            repo_react() })
        
        renderPieChart(div_id = "test",
                       data = dat)
        
        

    }
    
    shinyApp(ui = ui, server = server)
