### rebalancing simulations - portfolio stocks
library(reshape)
library(dplyr)
library(openxlsx)

########## importing raw data with stock price variation
Sys.Date()

raw_df <- read.xlsx(
  "C:/Users/Bruno/Desktop/pastas/personal/projects/finances/Simulacoes/Simulacoes/dados_asset_classes.xlsx",
  sheet = 1,
  startRow = 1,
  colNames = TRUE)

raw_df <- melt(raw_df, id = c("Data"))

raw_df <- raw_df %>% filter(variable == 'Rendimento_Diario_Ibovespa'
                            | variable == 'Rendimento_Diario_S&P_500'
                            | variable == 'Rendimento_Diario_IMA-B'
                            | variable == 'Rendimento_Diario_IDA-DI'
                            | variable == 'Rendimento_Diario_IHFA'
                            | variable == 'Rendimento_Diario_CDI')

raw_df$Data <- openxlsx::convertToDate(raw_df$Data)

raw_df <- raw_df %>% arrange(Data)

#raw_df <- raw_df %>% filter(Data != min(raw_df$Data))
raw_df <- raw_df %>% filter(Data != 2009-12-31)


raw_df <- raw_df %>% rename(
  'tipo' = variable,
  'variacao_diaria' = value
)

raw_df$variacao_diaria <- raw_df$variacao_diaria+1 

########## variables settings

# total investment
patrimonio_inicial <- 1000

# stocks types and  percentage stock composition
tipoAcoes_df <- data.frame('tipo' = c('Rendimento_Diario_Ibovespa', 'Rendimento_Diario_S&P_500'
                                      ,'Rendimento_Diario_IMA-B','Rendimento_Diario_IDA-DI'
                                      ,'Rendimento_Diario_IHFA','Rendimento_Diario_CDI')
                           ,'percentage' = c(0.025,0.025
                                             ,0.1,0.4
                                             ,0.1,0.35))

# defined periods to rebalance
periodos_realocacao <- c(3,6,12,18,24,99999)
#periodos_realocacao <- c(1,2)

# percentage variation to rebalance
variacoes_percentuais_realocacao <- c(0.1,0.5,1,99998)
#variacoes_percentuais_realocacao <- c(0.01,0.05)


# initial investment by each stock
patrimonio_df_initial <- tipoAcoes_df %>% 
  mutate(investimento = percentage * patrimonio_inicial)

########## scenarios simulation
#raw_df <- raw_df %>% filter(Data >= '2010-01-01')
#raw_df <- raw_df %>% filter(Data >= '2010-12-23')

df_dates <- raw_df %>% select(Data) %>% group_by(Data) %>% summarise()

df_dates$Month_Yr <- format(as.Date(df_dates$Data), "%Y-%m")

df_dates <- df_dates %>% mutate(month_count = dense_rank((Month_Yr)))

df_dates <- df_dates %>% 
  group_by(month_count) %>%
  mutate(flag = row_number())

variation_df <- raw_df %>% select(tipo,Data,variacao_diaria)

patrimonio_df_historical_with_periods_flag <- ''

for (variacao_percentual_realocacao_loop in variacoes_percentuais_realocacao) {
  print(variacao_percentual_realocacao_loop)
  realocar_percentual_tipoAcoes_positivo <<- tipoAcoes_df %>% mutate(percentage = percentage * (1+variacao_percentual_realocacao_loop))
  
  realocar_percentual_tipoAcoes_negativo <<- tipoAcoes_df %>% mutate(percentage = percentage * (1-variacao_percentual_realocacao_loop))
  
  for (n in periodos_realocacao) {
    print(n)
    df_dates_simulating <- df_dates %>% mutate(transaction_period = ifelse(month_count%%n==0 & flag ==1,1,0))
    
    patrimonio_df_month_historical <- ''
    patrimonio_df <- patrimonio_df_initial 
    
    #for (i in 1:45) {
    1:nrow(df_dates) { 
      print(df_dates[[i,1]])
      
      flag_rebalanceado <- 0
      tipo_rebalanceado <- 'none'
      
      if(data.frame(df_dates_simulating %>% filter(Data == df_dates[[i,1]])) %>% select(transaction_period) ==1) {
        print('Rebalanceou por periodo')
        
        # rebalance - period
        patrimonio_df <- patrimonio_df %>% inner_join(tipoAcoes_df,by = 'tipo') %>% 
          mutate(investimento = total_investimento * percentage.y) %>%
          select(tipo,investimento,percentage.y) %>% 
          rename(percentage = percentage.y)
        print(patrimonio_df)
        flag_rebalanceado <- 1
        
        tipo_rebalanceado <- 'periodo'
      } 
      
      test_realocar_percentual_tipoAcoes_positivo <- patrimonio_df %>% inner_join(realocar_percentual_tipoAcoes_positivo, by =  'tipo') %>% 
        mutate(flag_rebalancear_percentage = ifelse(percentage.x >= percentage.y,1,0))
      
      test_realocar_percentual_tipoAcoes_negativo <- patrimonio_df %>% inner_join(realocar_percentual_tipoAcoes_negativo, by =  'tipo') %>% 
        mutate(flag_rebalancear_percentage = ifelse(percentage.x <= percentage.y,1,0))

      
      if(sum(test_realocar_percentual_tipoAcoes_positivo$flag_rebalancear_percentage)>=1|sum(test_realocar_percentual_tipoAcoes_negativo$flag_rebalancear_percentage)>=1) {
        print('Rebalanceou por variacao percentual')
        
        # rebalance - variation
        patrimonio_df <- patrimonio_df %>% inner_join(tipoAcoes_df,by = 'tipo') %>% 
          mutate(investimento = total_investimento * percentage.y) %>%
          select(tipo,investimento,percentage.y) %>% 
          rename(percentage = percentage.y)
        print(patrimonio_df)
        flag_rebalanceado <- 1
        
        tipo_rebalanceado <- 'variacao_percentual'
      }
      
      
      patrimonio_df_month <- variation_df %>% filter(Data == df_dates[[i,1]]) %>% 
        right_join(patrimonio_df, by = 'tipo') %>% 
        mutate(investimento = ifelse(is.na(variacao_diaria),investimento,investimento*variacao_diaria))
      
      
      patrimonio_df_month$flag_rebalanceado <- flag_rebalanceado
      patrimonio_df_month$tipo_rebalanceado <- tipo_rebalanceado  
      #print(patrimonio_df_month)
      
      total_investimento <- sum(patrimonio_df_month$investimento)
      #print(total_investimento)
      
      patrimonio_df <- patrimonio_df_month %>% select(tipo,investimento) %>%
        mutate(percentage = investimento / total_investimento)
      #print(patrimonio_df)
      
      
      patrimonio_df_month <- patrimonio_df_month %>% 
        mutate(percentage = investimento / total_investimento)
      
      patrimonio_df_month_historical <- rbind(patrimonio_df_month_historical,patrimonio_df_month)
      
    }
    
    patrimonio_df_month_historical <- patrimonio_df_month_historical[-1,]
    patrimonio_df_month_historical$periods_flag <- n
    patrimonio_df_month_historical$variacao_percentual_flag <- variacao_percentual_realocacao_loop
    
    patrimonio_df_historical_with_periods_flag <- rbind(patrimonio_df_historical_with_periods_flag,
                                                        patrimonio_df_month_historical)
    
    
    # end of periods list loop
  }
  
  # end of variation list loop
}

patrimonio_df_historical_with_periods_flag <- patrimonio_df_historical_with_periods_flag[-1,]

########## Final transformation

# create unique key by combination of flags

transform_patrimonio_df_historical_with_periods_flag <- patrimonio_df_historical_with_periods_flag %>% 
  mutate(key_flags = paste0(periods_flag,"_",variacao_percentual_flag),
         type_rebalanceamento = ifelse(periods_flag == 99999 & variacao_percentual_flag == 99999, '-',
                                       ifelse(variacao_percentual_flag == 99999, 'uniq_periodo',
                                              ifelse(periods_flag == 99999, 'uniq_varPerc','combination_periodo_varPerc'))))


transform_patrimonio_df_historical_with_periods_flag <- transform_patrimonio_df_historical_with_periods_flag %>% 
  mutate(flag_stock_perc_desc = tipo_rebalanceado) %>%
  mutate(tipo_rebalanceado = ifelse(tipo_rebalanceado=='variacao_percentual_positiva'|tipo_rebalanceado=='variacao_percentual_negativa',
                                  'variacao_percentual',tipo_rebalanceado))



contagem_rebalanceamentos_portipo <- transform_patrimonio_df_historical_with_periods_flag %>% 
  group_by(Data,key_flags,tipo_rebalanceado) %>%
  summarise(total_rebalanceamentos = sum(as.numeric(flag_rebalanceado)))  %>%
  filter(tipo_rebalanceado != 'none') %>% 
  group_by(key_flags,tipo_rebalanceado) %>%
  tally()  %>%
  rename(total_rebalanceamentos = n)



###
#
contagem_rebalanceamentos_portipo_perc_desc_both <- transform_patrimonio_df_historical_with_periods_flag %>% 
  group_by(Data,key_flags,tipo_rebalanceado,flag_stock_perc_desc,flag_stock_perc) %>%
  summarise(total_rebalanceamentos = sum(as.numeric(flag_rebalanceado)))  %>%
  filter(flag_stock_perc_desc != 'none' & flag_stock_perc != 'none') %>% 
  group_by(key_flags,tipo_rebalanceado,flag_stock_perc_desc,flag_stock_perc) %>%
  tally() %>%
  rename(total_rebalanceamentos = n)

#####




contagem_rebalanceamentos_historic_total <- transform_patrimonio_df_historical_with_periods_flag %>% 
  group_by(Data,key_flags) %>%
  summarise(total_rebalanceamentos = sum(as.numeric(flag_rebalanceado))) %>%
  mutate(total_rebalanceamentos = ifelse(total_rebalanceamentos>=1,1,total_rebalanceamentos))


contagem_rebalanceamentos_total <- transform_patrimonio_df_historical_with_periods_flag %>% 
  group_by(Data,key_flags) %>%
  summarise(total_rebalanceamentos = sum(as.numeric(flag_rebalanceado))) %>%
  filter(total_rebalanceamentos>=1) %>%
  group_by(key_flags) %>%
  tally()  %>%
  rename(total_rebalanceamentos = n)



# Group - date and flag

df_output <- transform_patrimonio_df_historical_with_periods_flag %>% 
  group_by(Data,periods_flag,variacao_percentual_flag,key_flags,type_rebalanceamento) %>%
  summarise(montante = sum(as.numeric(investimento))) %>%
  mutate(rendimento = (montante - patrimonio_inicial)/patrimonio_inicial)


########## Export results for Power BI visualization

write.csv(df_output,
          "C:/Users/Bruno/Desktop/pastas/personal/projects/finances/Simulacoes/Simulacoes/with_negative_15_01/df_output.csv"
          , row.names=FALSE)

write.csv(contagem_rebalanceamentos_portipo,
          "C:/Users/Bruno/Desktop/pastas/personal/projects/finances/Simulacoes/Simulacoes/with_negative_15_01/contagem_rebalanceamentos_portipo.csv"
          , row.names=FALSE)


write.csv(transform_patrimonio_df_historical_with_periods_flag,
          "C:/Users/Bruno/Desktop/pastas/personal/projects/finances/Simulacoes/Simulacoes/with_negative_15_01/transform_patrimonio_df_historical_with_periods_flag.csv"
          , row.names=FALSE)


write.csv(contagem_rebalanceamentos_portipo_perc_desc_both,
          "C:/Users/Bruno/Desktop/pastas/personal/projects/finances/Simulacoes/Simulacoes/with_negative_15_01/contagem_rebalanceamentos_portipo_perc_desc_both.csv"
          , row.names=FALSE)


write.csv(contagem_rebalanceamentos_historic_total,
          "C:/Users/Bruno/Desktop/pastas/personal/projects/finances/Simulacoes/Simulacoes/with_negative_15_01/contagem_rebalanceamentos_historic_total.csv"
          , row.names=FALSE)


write.csv(contagem_rebalanceamentos_total,
          "C:/Users/Bruno/Desktop/pastas/personal/projects/finances/Simulacoes/Simulacoes/with_negative_15_01/contagem_rebalanceamentos_total.csv"
          , row.names=FALSE)



write.csv(patrimonio_df_historical_with_periods_flag,
          "C:/Users/Bruno/Desktop/pastas/personal/projects/finances/Simulacoes/Simulacoes/with_negative_15_01/patrimonio_df_historical_with_periods_flag.csv"
          , row.names=FALSE)


write.csv(tipoAcoes_df,
          "C:/Users/Bruno/Desktop/pastas/personal/projects/finances/Simulacoes/Simulacoes/with_negative_15_01/tipoAcoes_df.csv"
          , row.names=FALSE)


