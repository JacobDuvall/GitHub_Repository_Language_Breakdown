library(shiny)
library(gh)

    ui = bootstrapPage(
        column(12,offset=4, titlePanel("GitHub Repository Language Breakdown")),
        column(12, offset =1,textInput("text", label = h4("Search GitHub username:"))),
        
        column(12, offset = 4, uiOutput('variables')),
        uiOutput('selected_repo')
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
            if(input$text > 0) {
                tryCatch({
                    url2 = paste("GET /repos/", input$text, "/", input$variables2, "/languages", sep = "")
                    language_list = gh(url2)
                    total = 0
                    for(i in 1:length(language_list)) {
                        language = names(language_list)[i]
                        total = total + language_list[[language]]
                    }
                    for (i in 1:length(language_list)) {
                        language = names(language_list)[i]
                        percent = (language_list[[language]] / total) * 100
                        cmd = paste(cmd, language, " : ", percent, "%\n", sep = "")
                    }
                    print(cmd)
                    return(cmd)
                
                }, error = function(e) {
                    return("Repository failed to load")
                }, warning = function(w) {
                    return("Repository failed to load")
                })
            }
        })
        
        output$selected_repo = renderText({
            repo_react() })
        

    }
    
    shinyApp(ui = ui, server = server)
