

# SERVER ####
server_carto <- function(input, output) {
  
  do_eat2_extract_data <- eventReactive(input$btn_eat2_extract_data, {
    f_eat2_extract_data(d = df_eat2,
                        food_input = input$eat2_single_food,
                        subs_input = input$eat2_single_subs,
                        food_table = food_tab_eat2,
                        subs_table = subs_tab_eat2,
                        vars = com_var_eat2) %>%
      dplyr::select(., -c("Groupe de la nomenclature INCA 2", "Unité")) %>%
      dplyr::arrange(., `Type`, `Région`, `Vague`, `Date`) -> extracted_d
    
    ## Translating column names
    if (app_lang == "EN") { 
      extracted_d %>%
        dplyr::rename(`Food item` = `Libellé`,
                      `Region` = `Région`,
                      `Campaign` = `Vague`) -> extracted_d
    }
    extracted_d
  })
  output$eat2_extracted_data <- renderDataTable({
    do_eat2_extract_data()
  }, options = list(pageLength = 16, searching = FALSE))
  
  
  do_eat2_carto_conta <- eventReactive(input$btn_eat2_extract_data, {
    f_eat2_carto_conta(d = df_eat2,
                       food_input = input$eat2_single_food,
                       subs_input = input$eat2_single_subs,
                       food_table = food_tab_eat2,
                       subs_table = subs_tab_eat2,
                       vars = com_var_eat2,
                       frsf = sf_FRA1,
                       rds_raw = raw_rds_FRA1,
                       lang = app_lang) ## app_lang initialized as global variable at the beginning of the dashboard
  })
  
  output$eat2_carto_conta <- renderPlot({
    do_eat2_carto_conta()
  })
  
}