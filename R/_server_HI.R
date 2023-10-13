

# SERVER ####
server_HI <- function(input, output) {
  
  do_eat2_identify_hazards <- eventReactive(input$btn_eat2_identify_hazards, {
    f_eat2_identify_hazards(d = df_eat2,
                            fi = input$eat2_food_4_identify_hazards,
                            food_table = food_tab_eat2,
                            subs_table = subs_tab_eat2,
                            vars = com_var_eat2,
                            lang = app_lang)
    
  })
  
  output$eat2_identify_hazards <- renderDataTable({
    do_eat2_identify_hazards()
  })
  
}