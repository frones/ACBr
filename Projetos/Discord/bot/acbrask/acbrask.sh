#!/bin/sh
# Status
#   Exit = 0 - Rodando
#   Exit = 2 - Não está em exeução
# Start / ReStart
#   Exit = 0 - Iniciado
#   Exit = 2 - Não iniciado
# Stop
#   Exit = 0 - Parado com sucesso, ou não estava em execução
#   Exit = 3 - Erro ao parar o serviço

# Pasta da Aplicação (Path)
AppPath=
#AppPath=bin/

# Nome da aplicação
AppName=node

# Parametros para execução da aplicação
AppParams=index.js

# Tempo para esperar a aplicação fechar
SegundosAguardar=3

# Numero máximo de tentativas para aguardar o serviço encerrar
TentativasEncerrar=5

# Variaveis de trabalho
count=0
psid=0
erro=0

echo "Executando comando $1, Path: $AppPath, Aplicação: $AppName, Parâmetros: $AppParams"

# Função para obter quantos processos estão rodando
function GetCount()
{
  count=$(ps -ef | grep $AppName | wc -lc | awk '{print $1}')
}

# Função para obter o PID do processo
function GetPID()
{
  psid=$(ps -ef | grep $AppName | head -n1 | awk '{print $2}')
}

function DoStatus()
{
   GetCount

   if [ $count -lt 2 ] ; then
      echo "Servico $AppName parado"
      erro=2
   else
      GetPID
      echo "Servico $AppName em execucao. Processo: $psid"
      erro=0
   fi
}

function DoStart()
{
   GetCount

   if [ $count -lt 2 ] ; then
      if [ -n "$AppPath" ] ; then
        cd $AppPath
      fi

      $AppName $AppParams &
      DoStatus
   else
      GetPID
      echo "O servico $AppName já está em execucao. Processo: $psid"
   fi
}

function DoStop()
{
   GetCount

   if [ $count -gt 1 ] ; then
      GetPID
      echo "Fechando Servico $AppName. Processo: $psid"

      # Fecha com SIGQUIT (3) para permitir a aplicação encerrar corretamente
      kill -3 $psid

      # Verifica se a aplicação foi fechada. Aguarda $SegundosAguardar Segundos, por $TentativasEncerrar vezes
      sleep 1
      GetCount
      c=1
      while [ $c -le $TentativasEncerrar ] && [ $count -gt 1 ]
      do
        echo "Aguardando Termino do Serviço por: $SegundosAguardar segundos! Tentativa: $c/$TentativasEncerrar"
        sleep $SegundosAguardar

        (( c++ ))
        GetCount
      done

      # Se ainda não conseguiu fechar... Mate com Kill -9
      GetCount
      if [ $count -gt 1 ] ; then
        echo "Ainda não encerrado... Processo será Morto..."
        kill -9 $psid
      fi

      DoStatus
      if [ $erro -gt 1 ] ; then
        erro=3
      else
        erro=0
      fi
   else
      echo "O servico $AppName nao esta em execucao."
   fi
}

 function DoRestart()
 {
   DoStop
   DoStart
 }


if [ "$1" = start ] ;
then
  DoStart

elif [ "$1" = status ] ;
then
  DoStatus

elif [ "$1" = stop ] ;
then
  DoStop

elif [ "$1" = restart ] ;
then
  DoRestart

elif [ "$1" = "" ] ;
then
  echo "Opções: start, stop, status, restart"
  erro=1

else
  echo "Opção inválida: $1"
  erro=1
fi

exit $erro
