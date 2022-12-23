# dados_ccee

## New features

### Minor

* Muda montagem de dados de entrada para usar planilhas da CCEE oficiais do PMO
  * `usinas.csv` agora tem a coluna `CEG`

## Bug fixes

* O tratamento de modificações na capacidade instalada de usinas foi reformulado para ser mais 
  robusto, evitando alguns erros possíveis na implementação anterior

# eolicas-newave-dados 1.0.3

## Bug fixes

* Chave `data_ref` nao estava sendo usada completamente. Embora fossem selecionadas apenas as usinas
  com inicio de operacao ate este valor, os historicos de reanalise e verificados nao estavam sendo
  restringidos, todo o dado era usado. De fato, isto nao deveria causar grandes impactos dado que a
  analise e por usina e os comportamentos nao mudam significativamente. De toda forma, isto foi
  corrigido

# eolicas-newave-dados 1.0.2

## Bug fixes

* Corrige nome da coluna `Clusters` -> `clusters` no dado de coeficientes das FTMs
* Corrige dado de vento medio que estava sem coluna de `data_hora`
* Corrige nomes de clusters em `capinst_acum_cluster.csv`

# eolicas-newave-dados 1.0.1

## New

* Muda diretorio de instalacao de `/tmp/eolicas-newave/dados` para `/usr/lib/eolicas-newave/dados`
* Scripts `install.sh` e `eolicas-newave-dados` ja comitados com permissao de execucao
* Adicionado desinstalador `uninstall.sh` tambem com permissao de execucao

# eolicas-newave-dados 1.0

Este repositório contem o conjunto de ferramentas necessário para determinação dos Parques Eólicos Equivalentes (PEEs), estimação de suas Funções de Transferência Mensais (FTMs) e posterior agregação de demais usinas que não tenham sido contempladas na clusterização inicial dos PEEs.