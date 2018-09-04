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

unit DoCHQUnit ;

interface
Uses Classes, TypInfo, SysUtils, CmdUnit ;

Procedure DoCHQ( Cmd : TACBrCmd ) ;

implementation
uses ACBrCHQ, ACBrUtil,  DoECFUnit,
  {$IFNDEF NOGUI}ACBrMonitor1 {$ELSE}ACBrMonitorConsoleDM {$ENDIF} ;

Procedure DoCHQ( Cmd : TACBrCmd ) ;
Var Linhas : TStringList ;

begin
  with {$IFNDEF NOGUI}FrmACBrMonitor.ACBrCHQ1 {$ELSE}dm.ACBrCHQ1 {$ENDIF} do
  begin
     try
        if Cmd.Metodo = 'ativar' then  { Ativa a Impress.Cheque }
           Ativar

        else if Cmd.Metodo = 'desativar' then
           Desativar 

        else if Cmd.Metodo = 'ativo' then
           Cmd.Resposta := BoolToStr(Ativo, true)

        else if Cmd.Metodo = 'modelostr' then
           Cmd.Resposta := ModeloStr

        else if Cmd.Metodo = 'modelo' then
           Cmd.Resposta := GetEnumName(TypeInfo(TACBrCHQModelo),Integer(Modelo))

        else if Cmd.Metodo = 'porta' then
           Cmd.Resposta := Porta

        else if Cmd.Metodo = 'chequepronto' then
           Cmd.Resposta := BoolToStr(ChequePronto, true)

        else if Cmd.Metodo = 'banco' then
           Cmd.Resposta := Banco

        else if Cmd.Metodo = 'setbanco' then
           Banco := Cmd.Params(0)

        else if Cmd.Metodo = 'cidade' then
           Cmd.Resposta := Cidade

        else if Cmd.Metodo = 'setcidade' then
         begin
           Cidade := Cmd.Params(0) ;
           {$IFNDEF NOGUI}FrmACBrMonitor.edCHQCidade.Text := Cidade ;{$ENDIF}
         end

        else if Cmd.Metodo = 'favorecido' then
           Cmd.Resposta := Favorecido

        else if Cmd.Metodo = 'setfavorecido' then
         begin
           Favorecido := Cmd.Params(0) ;
           {$IFNDEF NOGUI}FrmACBrMonitor.edCHQFavorecido.Text := Favorecido ;{$ENDIF}
         end

        else if Cmd.Metodo = 'observacao' then
           Cmd.Resposta := Observacao

        else if Cmd.Metodo = 'setobservacao' then
           Observacao := Cmd.Params(0)

        else if Cmd.Metodo = 'valor' then
           Cmd.Resposta := FloatToStr(Valor) 

        else if Cmd.Metodo = 'setvalor' then
           Valor := StringToFloat( Cmd.Params(0) )

        else if Cmd.Metodo = 'data' then
           Cmd.Resposta := FormatDateTime('dd/mm/yy', Data )
        else if Cmd.Metodo = 'setdata' then
           Data := StringToDateTime( Cmd.Params(0) )

        else if Cmd.Metodo = 'bompara' then
           Cmd.Resposta := FormatDateTime('dd/mm/yy', BomPara)
        else if Cmd.Metodo = 'setbompara' then
           BomPara := StringToDateTime(Cmd.Params(0))

        else if Cmd.Metodo = 'imprimircheque' then
         begin
           {$IFNDEF NOGUI}
             if FrmACBrMonitor.chCHQVerForm.Checked and (not ChequePronto) then
           {$ELSE}
             if dm.VerificaCheque then
           {$ENDIF}
              raise Exception.Create('Formulário de Cheque não posicionado');

           ImprimirCheque;
         end

        else if Cmd.Metodo = 'travarcheque' then
           TravarCheque

        else if Cmd.Metodo = 'destravarcheque' then
           DestravarCheque

        else if Cmd.Metodo = 'cmc7' then
           Cmd.Resposta := CMC7

        else if Cmd.Metodo = 'imprimirlinha' then
           ImprimirLinha( Cmd.Params(0) )

        else if Cmd.Metodo = 'imprimirverso' then
         begin
           Linhas := TStringList.Create ;
           try
              //StringToMemo( Cmd.Params(0), Linhas ); {Linha separadas por | (pipe)}
              ImprimirVerso( Linhas );
           finally
              Linhas.Free ;
           end ;
         end
         
        ELSE
           raise Exception.Create('Comando inválido ('+Cmd.Comando+')') ;

     finally
        { Nada a fazer aqui por enquanto... :) }
     end ;
  end ;
end ;

end.

