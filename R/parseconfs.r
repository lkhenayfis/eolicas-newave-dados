################### FUNCOES PARA LEITURA E TRATAMENTO DE ARQUIVOS DE CONFIGURACAO ##################

#' Interpretador De Configuracoes Para \code{clusteriza_usinas}
#' 
#' Le e processa um arquivo de configuracao para clusteriza_usinas.r
#' 
#' @param arq_conf caminho do arquivo de configuracao
#' 
#' @return lista contendo as configuracoes ja processadas

parseconf_clustusi <- function(arq_conf) {

    CONF <- jsonlite::read_json(arq_conf, TRUE)
    CONF$subsistemas <- structure(CONF$subsistemas, names = CONF$subsistemas)

    # Parse dos modelos de compactacao -----------------------------------

    depth_compact <- maxdepth(CONF$mod_compact)
    if(depth_compact == 2) {
        CONF$mod_compact <- lapply(CONF$subsistemas, function(i) CONF$mod_compact)
    }
    CONF$mod_compact <- lapply(CONF$mod_compact, function(l1) {
        lapply(l1, function(cc) {
            cc[[1]] <- paste0("clust", cc[[1]])
            cc[[1]] <- str2lang(cc[[1]])
            as.call(cc)
        })
    })

    # Parse dos modelos de clusterizacao ---------------------------------

    depth_cluster <- maxdepth(CONF$mod_cluster)
    if(depth_cluster == 2) {
        CONF$mod_cluster <- lapply(CONF$subsistemas, function(i) CONF$mod_cluster)
    }
    CONF$mod_cluster <- lapply(CONF$mod_cluster, function(l1) {
        lapply(l1, function(cc) {
            cc[[1]] <- paste0("clust", cc[[1]])
            cc[[1]] <- str2lang(cc[[1]])
            as.call(cc)
        })
    })

    return(CONF)
}

# HELPERS ------------------------------------------------------------------------------------------

#' Profundidade De Listas
#' 
#' Identifica o maior nivel de nesting em listas no padrao dos arquivos de configuracao
#' 
#' @param lista uma lista de listas correspondentes a alguma configuracao
#' @param i contador do nivel de lista, nao deve ser fornecido
#' 
#' @return numero de niveis aninhados em \code{lista}

maxdepth <- function(lista, i = 0) {
    if(is.list(lista)) {
        maxdepth(lista[[1]], i = i + 1)
    } else {
        return(i)
    }
}
