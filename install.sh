#! /bin/bash

VERSION="1.0.3"
DATE="13/10/2022"

echo "Instalação da aplicação eolicas-newave-dados"
echo "Gerência de Metodologias e Modelos Energéticos - PEM / ONS"
echo "Versão ${VERSION} - ${DATE}"

# Checks if R is installed
command -v R >/dev/null 2>&1
R_INSTALLED=$?
if [ $R_INSTALLED -ne 0 ]; then
    echo "R não foi encontrado!"
    exit $R_INSTALLED
else
    echo "Instalação do R encontrada..."
fi

# Creates installation directory in /usr
echo "Criando diretório de instalação..." 
USERINSTALLDIR=/usr/lib
INSTALLDIR=${USERINSTALLDIR}/eolicas-newave/dados
[ ! -d $INSTALLDIR ] && mkdir -p $INSTALLDIR

# Installs dependencies
echo "Instalando dependências..."
sudo -u $SUDO_USER Rscript -e "renv::restore()"

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

# Copies the executable to a folder in the system's PATH
EXECPATH=/usr/bin/eolicas-newave-dados
echo "Copiando executável para ${EXECPATH}" 
cp eolicas-newave-dados $EXECPATH


echo "Finalizando instalação..."
