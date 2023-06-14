

# SERVER ####
server_PS <- function(input, output) {
  
  do_eat2_plot_check <- eventReactive(input$btn_eat2_plot_check, {
    f_eat2_plot_check(df_mp = df_mat_presence_eat2,
                      food_input = input$eat2_multiple_food,
                      subs_input = input$eat2_multiple_subs)
  })
  
  output$eat2_plot_check <- renderPlot({
    do_eat2_plot_check()
  }, res = 96)
  
  # do_eat2_extract_data <- eventReactive(input$btn_eat2_extract_data, {
  #   f_eat2_extract_data(d = df_eat2,
  #                       food_input = input$eat2_single_food,
  #                       subs_input = input$eat2_single_subs,
  #                       food_table = food_tab_eat2,
  #                       subs_table = subs_tab_eat2,
  #                       vars = com_var_eat2) %>%
  #     dplyr::select(., -c("Groupe de la nomenclature INCA 2", "Unité"))
  #   
  # })
  # output$eat2_extracted_data <- renderDataTable({
  #   do_eat2_extract_data()
  # }, options = list(pageLength = 16))
  # 
  # 
  # do_eat2_carto_conta <- eventReactive(input$btn_eat2_extract_data, {
  #   f_eat2_carto_conta(d = df_eat2,
  #                      food_input = input$eat2_single_food,
  #                      subs_input = input$eat2_single_subs,
  #                      food_table = food_tab_eat2,
  #                      subs_table = subs_tab_eat2,
  #                      vars = com_var_eat2,
  #                      frsf = sf_FRA1,
  #                      rds_raw = raw_rds_FRA1)
  # })
  # 
  # output$eat2_carto_conta <- renderPlot({
  #   do_eat2_carto_conta()
  # })
  
  
  do_eati_plot_check <- eventReactive(input$btn_eati_plot_check, {
    f_eat2_plot_check(df_mp = df_mat_presence_eati,
                      food_input = input$eati_multiple_food,
                      subs_input = input$eati_multiple_subs)
  })
  
  output$eati_plot_check <- renderPlot({
    do_eati_plot_check()
  }, res = 96)
  
  # do_eati_conta_order <- eventReactive(input$btn_eati_order, {
  #   f_eati_order(df = df_eati,
  #                subs_input = input$eati_single_subs,
  #                ndisplay = input$eati_ndisplay)
  # })
  # output$eati_conta_order <- renderPlot({
  #   do_eati_conta_order()
  # }, res = 96) 
  
}