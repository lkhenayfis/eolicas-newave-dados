#! /bin/bash

VERSION="1.0.0"
DATE="29/07/2022"

echo "Instalação da aplicação eolicas-newave-dados"
echo "Gerência de Metodologias e Modelos Energéticos - PEM / ONS"
echo "Versão ${VERSION} - ${DATE}"

# Checks if Python 3 is installed
command -v R >/dev/null 2>&1
R_INSTALLED=$?
if [ $R_INSTALLED -ne 0 ]; then
    echo "R não foi encontrado!"
    exit $R_INSTALLED
else
    echo "Instalação do R encontrada..."
fi

# Creates installation directory in /tmp
echo "Criando diretório de instalação..." 
TMPDIR=$(dirname $(mktemp -u))
INSTALLDIR=${TMPDIR}/eolicas-newave/dados
[ ! -d $INSTALLDIR ] && mkdir -p $INSTALLDIR

# Copies necessary files
echo "Copiando arquivos necessários..."
cp -r main/ $INSTALLDIR
cp -r R/ $INSTALLDIR
cp -r conf/ $INSTALLDIR
cp eolicas-newave-dados $INSTALLDIR

# Creates venv 
echo "Criando ambiente virtual..."
cp -r renv/ $INSTALLDIR
cp renv.lock $INSTALLDIR
cp .Rprofile $INSTALLDIR

# installs dependencies
echo "Instalando dependências..."
CURDIR=$(pwd)
cd $INSTALLDIR
Rscript -e "renv::restore()"

# Copies the executable to a folder in the system's PATH
[ ! -d $HOME/bin ] && mkdir $HOME/bin
EXECPATH=$HOME/bin/eolicas-newave-dados
echo "Copiando executável para ${EXECPATH}" 
cp eolicas-newave-dados $EXECPATH

# Deactivates venv
echo "Finalizando instalação..."
cd $CURDIR
