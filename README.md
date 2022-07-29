# eolicas-newave-dados

Este repositório contem o conjunto de ferramentas necessário para determinação dos Parques Eólicos 
Equivalentes (PEEs), estimação de suas Funções de Transferência Mensais (FTMs) e posterior agregação
de demais usinas que não tenham sido contempladas na clusterização inicial dos PEEs.

## Pré-requisitos

Antes de instalar a aplicação propriamente dita, é necessário garantir que todos os pré-requisitos 
sejam atendidos. Atualmente apenas sistemas operacionais Linux são suportados, visto que este é o 
ambiente de execução do modelo Newave.

### Versão do R

A versão do R mínima suportada é a 4.1. Instruções para instalação da versão mais recente podem ser 
encontradas [aqui](https://vps.fmvz.usp.br/CRAN/). É possível verificar que a versão correta está 
instalada com

```
R --version
```

Que deve retornar um output similar a

```sh
#> R version 4.1.0 (2021-05-18) -- "Camp Pontanezen"
#> Copyright (C) 2021 The R Foundation for Statistical Computing
#> Platform: x86_64-w64-mingw32/x64 (64-bit)
#> 
#> R is free software and comes with ABSOLUTELY NO WARRANTY.    
#> You are welcome to redistribute it under the terms of the    
#> GNU General Public License versions 2 or 3.
#> For more information about these matters see
#> https://www.gnu.org/licenses/.
```

### Dependências de sistema

Uma vez instalado o R na versão apropriada, o usuário deve garantir que algumas dependências de 
sistema estejam presentes. Estas dependências não são instaláveis automaticamente pelo próprio R 
durante a instalação de pacotes e devem estar presentes para compilação dos mesmos. Em sistemas 
operacionais Ubuntu, por exemplo, a instalação deve ser realizada com

```sh
sudo apt install make gcc gfortran libpq-dev liblapack-dev libblas-dev
```

## Instalação

Para instalação da aplicação, o usuário deve obter uma cópia do repositório na branch *master* por 
qualquer meio que julgar mais conveniente (clone, download de zip etc).

Dentro do repositório se encontra o um shell script denominado *install.sh*, responsável pela 
instalação da aplicação. Para executá-lo, o usuário deve navegar pelo terminal até a raiz do 
repositório e executar o comando

```sh
bash install.sh
```

Isto cria a estrutura necessária para execução da aplicação, instala todas as dependências internas
do R necessárias e adiciona o programa *eolicas-newave-dados* ao diretório *$HOME/bin*. Para que 
este script seja executavel, o usuario deve utilizar o comando

```sh
~$ sudo chmod +x $HOME/bin/eolicas-newave-dados
```

Este processo assume que *$HOME/bin* se encontra no PATH do sistema. Caso isto não seja verdade, o 
usuário pode movê-lo para outro diretório, utiliz-a-lo sempre com o caminho completo ou adicionar 
este diretório ao PATH incluindo a seguinte linha no arquivo *~/.bash_profile*.

```sh
export PATH=$PATH:$HOME/bin/
```

Em segundo lugar, o script *eolicas-newave-dados* executa os programas em R através de uma chamada 
simples da ferramente *Rscript*. Caso existam múltiplas instalações de R na máquina, é recomendável
que o usuário modifique esta linha com o qualificador de caminho completo até uma versão adequada.

## Execução

A execução do programa está descrita em detalhes nas páginas da Wiki.