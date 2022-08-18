#' Detecta periodos de sell out nao observado de um determinado produto. necessário construir a lista de cnpjs a analisar, uma tabela chamada 'aux' contendo campos cnpj,dtt apenas das vendas do item de interesse e uma tabela chamada dt2_data com todas as datas presentes na base original em ordem crescente.
#'
#' @export
#' @param cli_nome cnpj que sera analisado
#' @return dataframe com periodos de ruptura do item ou conjunto de item selecionado na construção do dataframe 'aux'. Onde prim é a primeira semana de ruptura, lastbut1 é a última semana de ruptura e gapsize é o duração da ruptura em dias
gap_detector <- function(cli_nome){
library(tidyverse)

  aux2 <- aux %>%
    filter(cnpj == cli_nome)

  aux_gap <- dt2_data %>%
    left_join(aux2,'dtt') %>%
    filter(dtt >= min(aux2$dtt),
           dtt <= max(aux2$dtt)) %>%
    #lag de cnpj é diferente de NA
    mutate(ident = !is.na(lag(cnpj)),
           soma = cumsum(ident),
           rnum =  row_number()) %>%
    group_by(soma) %>%
    summarise(n=n(),
              prim = min(dtt),
              ult = max(dtt),
              #ultima data sem sellout do produto registrado
              last_but1 = sort(dtt, decreasing = TRUE)[2L])

  # conta a cada 50 cnpjs
  if(which(cliente_list$cnpj == cli_nome) %% 50 == 0){
    print(round((which(cliente_list$cnpj == cli_nome)/nrow(cliente_list))*100,2))
  }

  if(any(aux_gap$n > 1)){
    dados_gap <- aux_gap %>%
      filter(n > 1) %>%
      mutate(gap_size = last_but1-prim) %>%
      mutate(across(everything(), as.character))


    return(dados_gap)
  }else{
    return(NA)
  }
}
