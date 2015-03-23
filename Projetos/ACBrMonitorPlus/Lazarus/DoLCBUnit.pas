{******************************************************************************}
{ Projeto: ACBr Monitor                                                        }
{  Executavel multiplataforma que faz uso do conjunto de componentes ACBr para }
{ criar uma interface de comunicação com equipamentos de automacao comercial.  }
{                                                                              }
{ Direitos Autorais Reservados (c) 2010 Daniel Simões de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{                                                                              }
{  Você pode obter a última versão desse arquivo na página do Projeto ACBr     }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{  Este programa é software livre; você pode redistribuí-lo e/ou modificá-lo   }
{ sob os termos da Licença Pública Geral GNU, conforme publicada pela Free     }
{ Software Foundation; tanto a versão 2 da Licença como (a seu critério)       }
{ qualquer versão mais nova.                                                   }
{                                                                              }
{  Este programa é distribuído na expectativa de ser útil, mas SEM NENHUMA     }
{ GARANTIA; nem mesmo a garantia implícita de COMERCIALIZAÇÃO OU DE ADEQUAÇÃO A}
{ QUALQUER PROPÓSITO EM PARTICULAR. Consulte a Licença Pública Geral GNU para  }
{ obter mais detalhes. (Arquivo LICENCA.TXT ou LICENSE.TXT)                    }
{                                                                              }
{  Você deve ter recebido uma cópia da Licença Pública Geral GNU junto com este}
{ programa; se não, escreva para a Free Software Foundation, Inc., 59 Temple   }
{ Place, Suite 330, Boston, MA 02111-1307, USA. Você também pode obter uma     }
{ copia da licença em:  http://www.opensource.org/licenses/gpl-license.php     }
{                                                                              }
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{       Rua Coronel Aureliano de Camargo, 973 - Tatuí - SP - 18270-170         }
{                                                                              }
{******************************************************************************}

{$mode objfpc}{$H+}

unit DoLCBUnit ;

interface
Uses Classes, TypInfo, SysUtils, CmdUnit ;

Procedure DoLCB( Cmd : TACBrCmd ) ;

implementation
uses  ACBrUtil,
  {$IFNDEF NOGUI}ACBrMonitor1 {$ELSE}ACBrMonitorConsoleDM {$ENDIF} ;

Procedure DoLCB( Cmd : TACBrCmd ) ;
begin
  with {$IFNDEF NOGUI}FrmACBrMonitor.ACBrLCB1 {$ELSE}dm.ACBrLCB1 {$ENDIF} do
  begin
     try
        if Cmd.Metodo = 'ativar' then  { Ativa o Leitor }
           Ativar

        else if Cmd.Metodo = 'desativar' then
           Desativar 

        else if Cmd.Metodo = 'ativo' then
           Cmd.Resposta := BoolToStr(Ativo, true)

        else if Cmd.Metodo = 'porta' then
           Cmd.Resposta := Porta

        else if Cmd.Metodo = 'intervalo' then
           Cmd.Resposta := IntToStr( Intervalo )

        else if Cmd.Metodo = 'setintervalo' then
           Intervalo := StrToInt( Cmd.Params(0) )

        else if Cmd.Metodo = 'lerfila' then
           Cmd.Resposta := LerFila

        else if Cmd.Metodo = 'apagarfila' then
           ApagarFila

        else if Cmd.Metodo = 'filacount' then
           Cmd.Resposta := IntToStr( FilaCount )

        else if Cmd.Metodo = 'prefixoaexcluir' then
           Cmd.Resposta := PrefixoAExcluir

        else if Cmd.Metodo = 'setprefixoaexcluir' then
         begin
           PrefixoAExcluir := Cmd.Params(0) ;
           {$IFNDEF NOGUI}FrmACBrMonitor.edLCBPreExcluir.Text := PrefixoAExcluir ;{$ENDIF}
         end

        else if Cmd.Metodo = 'sufixo' then
           Cmd.Resposta := Sufixo

        else if Cmd.Metodo = 'setsufixo' then
         begin
           Sufixo := Cmd.Params(0) ;
           {$IFNDEF NOGUI}FrmACBrMonitor.cbLCBSufixoLeitor.Text := Sufixo ;{$ENDIF}
         end

        else if Cmd.Metodo = 'excluirsufixo' then
           Cmd.Resposta := BoolToStr( ExcluirSufixo, True)

        else if Cmd.Metodo = 'setexcluirsufixo' then
         begin
           ExcluirSufixo := StrToBool( Cmd.Params(0) ) ;
           {$IFNDEF NOGUI}FrmACBrMonitor.chLCBExcluirSufixo.Checked := ExcluirSufixo ;{$ENDIF}
         end

        else if Cmd.Metodo = 'usarfila' then
           Cmd.Resposta := BoolToStr( UsarFila, True )

        else if Cmd.Metodo = 'setusarfila' then
         begin
           UsarFila := StrToBool( Cmd.Params(0) ) ;
           {$IFNDEF NOGUI}FrmACBrMonitor.rbLCBFila.Checked := UsarFila ;{$ENDIF}
         end

        else if Cmd.Metodo = 'filamaxitens' then
           Cmd.Resposta := IntToStr( FilaMaxItens )

        else if Cmd.Metodo = 'setfilamaxitens' then
         begin
            FilaMaxItens := StrToInt( Cmd.Params(0) ) ;
//          FrmACBrMonitor.sedLCBFilaMaxItens.Value := FilaMaxItens ;
         end

        else if Cmd.Metodo = 'ultimaleitura' then
           Cmd.Resposta := UltimaLeitura

        else if Cmd.Metodo = 'ultimocodigo' then
           Cmd.Resposta := UltimoCodigo

        else if Cmd.Metodo = 'enviarstring' then
            EnviarString( Cmd.Params(0) )

        else if Cmd.Metodo = 'lerstring' then
           Cmd.Resposta := LerString 

        ELSE
           raise Exception.Create('Comando inválido ('+Cmd.Comando+')') ;

     finally
        { Nada a fazer aqui por enquanto... :) }
     end ;
  end ;
end ;

end.

