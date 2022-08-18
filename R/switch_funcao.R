#' funcao facilitadora para gerar a migracao
#' @export

switch_funcao <-function (antes_base, depois_base, CNPJ_input = "cnpj",
          PROD_ID_input = "sku_desc", prim_input = NA)
{
  # antes_base <- antes_p1
  # depois_base <- depois_p1
  # CNPJ_input = "cnpj"
  # PROD_ID_input = "sku_desc"
  # prim_input = NA
  # entrada_periodo = 32

  if (!is.na(prim_input)){
    if(class(antes_base %>% select(v1=prim_input) %>% pull(v1)) != "Date")stop("prim precisa ser data")
    if(is.na(antes_base %>% select(v1=prim_input) %>% pull(v1)) != "Date")stop("prim nao pode ser na")
  }

  if (!is.data.frame(antes_base) | !is.data.frame(depois_base))
    stop("antes_base e depois_base precisam ser setados, e devem ter formato data.frame ou de outra tabela")

  out <- out_qt <- list()
  if (!is.na(prim_input)) {
    gg <- bind_rows(antes_base, depois_base) %>% select(prim = prim_input,
                                                        CUST_ID = CNPJ_input, PROD_ID = PROD_ID_input, qt_marca = qt) %>%
      mutate(prim = str_replace_all(prim, pattern = "-",
                                    replacement = "_")) %>% mutate(CUST_ID = paste0(str_sub(prim,
                                                                                            6, 10), "_", CUST_ID)) %>% dplyr::group_by(CUST_ID,
                                                                                                                                       PROD_ID) %>% dplyr::count() %>% dplyr::select(-n)

    tt_list <- list()

    temp1_qt <- antes_base %>% ungroup() %>% select(prim = prim_input,
                                                    CUST_ID = CNPJ_input, PROD_ID = PROD_ID_input, qt_marca = qt) %>%
      mutate(prim = str_replace_all(prim, pattern = "-",
                                    replacement = "_")) %>% mutate(CUST_ID = paste0(str_sub(prim,
                                                                                            6, 10), "_", CUST_ID)) %>% select(CUST_ID,
                                                                                                                              PROD_ID, qt_marca)
    temp2_qt <- depois_base %>% ungroup() %>% select(prim = prim_input,
                                                     CUST_ID = CNPJ_input, PROD_ID = PROD_ID_input, qt_marca = qt) %>%
      mutate(prim = str_replace_all(prim, pattern = "-",
                                    replacement = "_")) %>% mutate(CUST_ID = paste0(str_sub(prim,
                                                                                            6, 10), "_", CUST_ID)) %>% select(CUST_ID,
                                                                                                                              PROD_ID, qt_marca)
  }else {
    gg <- bind_rows(antes_base, depois_base) %>% select(CUST_ID = CNPJ_input,
                                                        PROD_ID = PROD_ID_input, qt_marca = qt) %>% dplyr::group_by(CUST_ID,
                                                                                                                    PROD_ID) %>% dplyr::count() %>% dplyr::select(-n)
    customers <- unique(gg$CUST_ID)
    tt_list <- list()
    N <- n_distinct(gg$CUST_ID)
    temp1_qt <- antes_base %>% ungroup() %>% select(CUST_ID = CNPJ_input,
                                                    PROD_ID = PROD_ID_input, qt_marca = qt) %>% select(CUST_ID,
                                                                                                       PROD_ID, qt_marca)
    temp2_qt <- depois_base %>% ungroup() %>% select(CUST_ID = CNPJ_input,
                                                     PROD_ID = PROD_ID_input, qt_marca = qt) %>% select(CUST_ID,
                                                                                                        PROD_ID, qt_marca)
  }
  return(list(gg,temp1_qt,temp2_qt))
}
