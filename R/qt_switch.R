#' algoritmo de migracao
#' @export

qt_switch <- function(i){
library(tidyverse)
  # precisa existir no ambiente
  # gasto periodo 1
  # temp_din1 = CUST_ID,PROD_ID,qt_marca
  # gasto periodo 2
  # temp_din2 = CUST_ID,PROD_ID,qt_marca
  # tt_list = uma lista vazia

  t1 <- full_join(temp1_qt[temp1_qt$CUST_ID == customers[i],],gg[gg$CUST_ID == customers[i],], by="PROD_ID") %>%
    dplyr::mutate(qt_marca = ifelse(is.na(qt_marca),0,qt_marca))

  t2 <- full_join(temp2_qt[temp2_qt$CUST_ID == customers[i],],gg[gg$CUST_ID == customers[i],], by="PROD_ID") %>%
    dplyr::mutate(qt_marca = ifelse(is.na(qt_marca),0,qt_marca))

  t3<-inner_join(t1,t2,by="PROD_ID") %>%
    dplyr::select(qt_marca.x,PROD_ID,qt_marca.y) %>%
    dplyr::mutate(qt_marca.x = ifelse(is.na(qt_marca.x),0,qt_marca.x),
                  qt_marca.y = ifelse(is.na(qt_marca.y),0,qt_marca.y)) %>%
    dplyr::mutate(diff = qt_marca.x - qt_marca.y) %>%
    dplyr::mutate(xdiff = ifelse(diff >= 0,diff,0),
                  ydiff = ifelse(diff <= 0,-diff,0),
                  PROD_ID = as.character(PROD_ID))

  xtotal <- sum(t3$xdiff) ; ytotal <- sum(t3$ydiff)

  #perda
  #if(sum(t1$qt_marca) > sum(t2$qt_marca)){
    if(sum(t3$qt_marca.x) > sum(t3$qt_marca.y)){
    aux <- t3[1,] %>% dplyr::mutate(PROD_ID = "mercado",
                                    qt_marca.x = 0,
                                    qt_marca.y = sum(t3$qt_marca.x)-sum(t3$qt_marca.y),
                                    diff = -(sum(t3$qt_marca.x)-sum(t3$qt_marca.y)),
                                    xdiff = 0,
                                    ydiff = sum(t3$qt_marca.x)-sum(t3$qt_marca.y))
    t3 <- bind_rows(t3,aux)
  }
  #ganho
  #if(sum(t1$qt_marca) < sum(t2$qt_marca)){
    if(sum(t3$qt_marca.x) < sum(t3$qt_marca.y)){
    aux <- t3[1,] %>% dplyr::mutate(PROD_ID = "mercado",
                                    qt_marca.x = sum(t3$qt_marca.y)-sum(t3$qt_marca.x),
                                    qt_marca.y = 0,
                                    diff = sum(t3$qt_marca.y)-sum(t3$qt_marca.x),
                                    xdiff = sum(t3$qt_marca.y)-sum(t3$qt_marca.x),
                                    ydiff = 0)
    t3 <- bind_rows(t3,aux)
  }


  #diagonais
  diag <- t3 %>% dplyr::group_by(PROD_ID) %>% dplyr::summarise(valor_diag=min(qt_marca.x,qt_marca.y))

  tt3<- expand.grid(t3$PROD_ID,t3$PROD_ID) %>%
    dplyr::mutate(antes = Var1,depois = Var2) %>%
    left_join(diag,by = c('antes' = 'PROD_ID')) %>%
    mutate(valor = ifelse(antes == depois,valor_diag,0)) %>%
    select(-valor_diag, -Var1, - Var2)

  tt3 <- tt3 %>%
    left_join(t3, by = c("antes" = "PROD_ID")) %>%
    select(antes,depois,valor,xdiff) %>%
    left_join(t3, by = c('depois' = 'PROD_ID')) %>%
    select(antes,depois,valor,xdiff = xdiff.x,ydiff) %>%
    mutate(xtotal,ytotal)

  denom <- max(xtotal,ytotal)

  tt3 <- tt3 %>%
    mutate(valor = case_when(ydiff > 0 & antes != depois  ~ (xdiff/denom)*ydiff,
                             TRUE ~ valor)
    ) %>%
    select(1:3)

  #tt_list[[i]] <- tt3 %>% filter(valor > 0 )
  if(i %% 10 == 0){ cat( (i/length(customers))*100, "% concluido \n \n")}
  return(tt3 %>% filter(valor > 0 ))
}
