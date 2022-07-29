# devel

## Bug fixes

* Corrige nome da coluna `Clusters` -> `clusters` no dado de coeficientes das FTMs
* Corrige dado de vento medio que estava sem coluna de `data_hora`

# eolicas-newave-dados 1.0.1

## New

* Muda diretorio de instalacao de `/tmp/eolicas-newave/dados` para `/usr/lib/eolicas-newave/dados`
* Scripts `install.sh` e `eolicas-newave-dados` ja comitados com permissao de execucao
* Adicionado desinstalador `uninstall.sh` tambem com permissao de execucao

# eolicas-newave-dados 1.0

Este repositório contem o conjunto de ferramentas necessário para determinação dos Parques Eólicos Equivalentes (PEEs), estimação de suas Funções de Transferência Mensais (FTMs) e posterior agregação de demais usinas que não tenham sido contempladas na clusterização inicial dos PEEs.