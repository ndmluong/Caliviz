

# SERVER ####
server_conta <- function(input, output) {
  
  do_eat2_show_conta_bygrp <- eventReactive(input$btn_eat2_show_conta_allgrp, {
    f_eat2_plot_conta_bygrp(d = df_eat2_censored,
                            subs_input = input$eat2_single_subs_hyp,
                            hyp = input$eat2_conta_hyp,
                            food_table = food_tab_eat2,
                            subs_table = subs_tab_eat2,
                            vars = com_var_eat2,
                            lang = app_lang) ## app_lang initialized as global variable at the beginning of the dashboard
  })
  output$eat2_show_conta_bygrp <- renderPlotly({
    do_eat2_show_conta_bygrp()
  })
  
  
  do_eat2_show_conta_byfood <- eventReactive(input$btn_eat2_show_conta_onegrp, {
    f_eat2_plot_conta_byfood(d = df_eat2_censored,
                             subs_input = input$eat2_single_subs_hyp,
                             food_grp_input = input$eat2_single_food_grp,
                             hyp = input$eat2_conta_hyp,
                             food_table = food_tab_eat2,
                             subs_table = subs_tab_eat2,
                             vars = com_var_eat2,
                             lang = app_lang) ## app_lang initialized as global variable at the beginning of the dashboard
  })
  output$eat2_show_conta_byfood <- renderPlotly({
    do_eat2_show_conta_byfood()
  })
  
  
  
  do_eati_conta_order <- eventReactive(input$btn_eati_order, {
    f_eati_order(df = df_eati,
                 subs_input = input$eati_single_subs,
                 ndisplay = input$eati_ndisplay)
  })
  output$eati_conta_order <- renderPlot({
    do_eati_conta_order()
  }, res = 96)
  
  
}





