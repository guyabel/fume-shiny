dd <- as.vector(1:4, "list")
names(dd) <- c("Summary",
               "One Dimension",
               "Two Dimensions",
               "Three Dimensions")
dd[[1]] <- c("Summary" = "basic")
dd[[2]] <- c("Age" = "by_age", 
             "Sex" = "by_sex", 
             "Education" = "by_edattain", 
             "Nativity" = "by_nativity", 
             "Country of Birth" = "by_birth")
dd[[3]] <- c("Age and Sex"  = "by_age_sex",
             "Age and Education" = "by_age_edattain",
             "Age and Nativity" = "by_age_nativity",
             "Sex and Education" = "by_sex_edattain",
             "Sex and Nativity" = "by_sex_nativity",
             "Education and Nativity" = "by_edattain_nativity")
dd[[4]] <- c("Age, Sex and Education" = "by_age_sex_edattain",
             "Age, Sex and Nativity" = "by_age_sex_nativity")


fs::dir_ls("./data", type = "directory") %>%
  str_sub(8)

ss <- c("Benchmark" = 'benchmark',
        "Ukraine War" = "ukraine-war",
        "Recovery in Europe" = "recovery-in-europe",
        "Rise of the East" =  "rise-of-the-east" ,
        "No Migration" = "no-migration")

shinyApp(
  ui = tagList(
    navbarPage(
      " ",
      tabPanel("Data Explorer",
               sidebarPanel(
                 selectizeInput(
                   inputId = 'data', label = 'Data', 
                   choices = dd, selected = dd[[1]][1]
                 ),
                 selectizeInput(
                   inputId = 'scenario', label = 'Scenario', 
                   choices = ss, selected = ss[[1]][1], multiple = TRUE
                 )
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Data",
                            dataTableOutput("table")
                            ),
                   tabPanel("Measure Details"),
                   tabPanel("Scenario Details",
                            includeMarkdown("scenario.md")
                            ),
                 )
               )
      ),
      tabPanel("Graphic Explorer", "This panel is intentionally left blank"),
      tabPanel("About", "This panel is intentionally left blank")
    )
  ),
  server = function(input, output) {
    output$txtout <- renderText({
      paste(input$txt, input$slider, format(input$date), sep = ", ")
    })
    output$table <- renderDataTable({
      input$data %>%
        paste0("./data/",.,".csv") %>%
        read_csv() %>%
        filter(scenario %in% input$scenario) %>%
        {if(str_detect(input$data, pattern = "edu"))
          mutate(., edu = case_when(
            "No Education" = 1,
            "Some Primary" = 2,
            "Primary" = 3, 
            "Lower Secondary" = 4,
            "Upper Secondary" =5, 
            "Post Secondary" = 6))
            # `1` = "No Education",
            # `2` = "Some Primary",
            # `3` = "Primary", 
            # `4` = "Lower Secondary",
            # `5` = "Upper Secondary", 
            # `6` = "Post Secondary")))
          else .}
    })
  }
)

