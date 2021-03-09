### simulacoes transacoes portfolio de acoes
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

########## configuracao de variaveis

# patrimonio total investido
patrimonio_inicial <- 1000

# tipo acoes e composicao de percentual do portfolio
tipoAcoes_df <- data.frame('tipo' = c('Rendimento_Diario_Ibovespa', 'Rendimento_Diario_S&P_500'
                                      ,'Rendimento_Diario_IMA-B','Rendimento_Diario_IDA-DI'
                                      ,'Rendimento_Diario_IHFA','Rendimento_Diario_CDI')
                           ,'percentage' = c(0.025,0.025
                                             ,0.1,0.4
                                             ,0.1,0.35))

# periodos definidos de realocacao para simulacao
#periodos_realocacao <- c(3,4,6,8,9,12,18,24,99999)
periodos_realocacao <- c(2,3)

# variacao percentual para realocar os recursos
#variacoes_percentuais_realocacao <- c(0.05,0.1,0.15,0.2,99998)
variacoes_percentuais_realocacao <- c(0.05,0.1)
variacoes_percentuais_realocacao <- variacoes_percentuais_realocacao + 1

#variacao_percentual_realocacao <- 1+0.1
#realocar_percentual_tipoAcoes <- tipoAcoes_df %>% mutate(percentage = percentage * variacao_percentual_realocacao)



# composicao realizada de patrimonio
patrimonio_df_initial <- tipoAcoes_df %>% 
  mutate(investimento = percentage * patrimonio_inicial)

########## simulacao de cenarios
#raw_df <- raw_df %>% filter(Data >= '2010-01-01')
#raw_df <- raw_df %>% filter(Data >= '2010-10-01')
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
  realocar_percentual_tipoAcoes <<- tipoAcoes_df %>% mutate(percentage = percentage * variacao_percentual_realocacao_loop)
  
  realocar_percentual_tipoAcoes <<- tipoAcoes_df %>% mutate(percentage = percentage * variacao_percentual_realocacao_loop)
    
  for (n in periodos_realocacao) {
    print(n)
    df_dates_simulating <- df_dates %>% mutate(transaction_period = ifelse(month_count%%n==0 & flag ==1,1,0))
    
    patrimonio_df_month_historical <- ''
    patrimonio_df <- patrimonio_df_initial 
    
    #1:nrow(df_dates)
    for (i in 1:45) {
      print(df_dates[[i,1]])
      
      flag_rebalanceado <- 0
      tipo_rebalanceado <- 'none'
      
      if(data.frame(df_dates_simulating %>% filter(Data == df_dates[[i,1]])) %>% select(transaction_period) ==1) {
        print('Rebalanceou por periodo')
        
        #fazer a transformacao de rebalanceamento
        patrimonio_df <- patrimonio_df %>% inner_join(tipoAcoes_df,by = 'tipo') %>% 
          mutate(investimento = total_investimento * percentage.y) %>%
          select(tipo,investimento,percentage.y) %>% 
          rename(percentage = percentage.y)
        print(patrimonio_df)
        flag_rebalanceado <- 1
        
        tipo_rebalanceado <- 'periodo'
      } 
      
      test_realocar_percentual_tipoAcoes <- patrimonio_df %>% inner_join(realocar_percentual_tipoAcoes, by =  'tipo') %>% 
        mutate(flag_rebalancear_percentage = ifelse(percentage.x >= percentage.y,1,0))
      
      if(sum(test_realocar_percentual_tipoAcoes$flag_rebalancear_percentage)>=1) {
        print('Rebalanceou por variacao percentual')
        
        #fazer a transformacao de rebalanceamento
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
      
      # fim do loop de vetor de periodos de rebalanceamento
    }
    
    patrimonio_df_month_historical <- patrimonio_df_month_historical[-1,]
    patrimonio_df_month_historical$periods_flag <- n
    patrimonio_df_month_historical$variacao_percentual_flag <- variacao_percentual_realocacao_loop
    
    patrimonio_df_historical_with_periods_flag <- rbind(patrimonio_df_historical_with_periods_flag,
                                                        patrimonio_df_month_historical)
    
    # fim do loop de vetor de periodos de rebalanceamento 
  }
  
  # fim do loop de vetor de variacoes percentuais de rebalanceamento 
}


patrimonio_df_historical_with_periods_flag <- patrimonio_df_historical_with_periods_flag[-1,]
#buffer 01

########## Transformacoes finais

# create unique key by combination of flags

transform_patrimonio_df_historical_with_periods_flag <- patrimonio_df_historical_with_periods_flag %>% 
  mutate(key_flags = paste0(periods_flag,"_",variacao_percentual_flag),
         type_rebalanceamento = ifelse(periods_flag == 99999 & variacao_percentual_flag == 1001, '-',
                                       ifelse(variacao_percentual_flag == 1001, 'uniq_periodo',
                                              ifelse(periods_flag == 99999, 'uniq_varPerc','combination_periodo_varPerc'))))


# count rebalanceamentos
contagem_rebalanceamentos_portipo <- transform_patrimonio_df_historical_with_periods_flag %>% 
  group_by(key_flags,tipo_rebalanceado) %>%
  summarise(total_rebalanceamentos = sum(as.numeric(flag_rebalanceado))/2)  %>%
  filter(tipo_rebalanceado != 'none')


contagem_rebalanceamentos_total <- transform_patrimonio_df_historical_with_periods_flag %>% 
  group_by(key_flags) %>%
  summarise(total_rebalanceamentos = sum(as.numeric(flag_rebalanceado))/2)


#Agrupamento por data e flag
df_output <- transform_patrimonio_df_historical_with_periods_flag %>% 
  group_by(Data,periods_flag,variacao_percentual_flag,key_flags,type_rebalanceamento) %>%
  summarise(montante = sum(as.numeric(investimento))) %>%
  mutate(rendimento = (montante - patrimonio_inicial)/patrimonio_inicial)


# testar datas



########## Exportar Resultados

write.csv(df_output,
          "C:/Users/Bruno/Desktop/pastas/personal/projects/finances/Simulacoes/Simulacoes/df_output_11_01.csv")

write.csv(contagem_rebalanceamentos_portipo,
          "C:/Users/Bruno/Desktop/pastas/personal/projects/finances/Simulacoes/Simulacoes/count_rebalanceamentos_11_01.csv")


write.csv(patrimonio_df_historical_with_periods_flag,
          "C:/Users/Bruno/Desktop/pastas/personal/projects/finances/Simulacoes/Simulacoes/patrimonio_df_historical_with_periods_flag_11_01.csv")
