#! /bin/bash

VERSION="1.0.0"
DATE="29/07/2022"

echo "eolicas-newave-dados"
echo "Gerência de Metodologias e Modelos Energéticos - PEM / ONS"
echo "Versão ${VERSION} - ${DATE}"

TMPDIR=$(dirname $(mktemp -u))
INSTALLDIR=${TMPDIR}/eolicas-newave-dados

# Checks if application is installed
if [ ! -d $INSTALLDIR ]; then
    echo "Aplicação não encontrada..."
    exit 1
fi

RUN=$INSTALLDIR/execucao_completa.r

echo "Executando"
echo "------------------"

echo Rscript $RUN $*
Rscript $RUN $*
