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

unit DoGAVUnit ;

interface
Uses Classes, TypInfo, SysUtils, CmdUnit;

Procedure DoGAV( Cmd : TACBrCmd ) ;

implementation
uses ACBrGAV, ACBrUtil, ACBrDevice,
  {$IFNDEF NOGUI}ACBrMonitor1 {$ELSE}ACBrMonitorConsoleDM {$ENDIF} ;

Procedure DoGAV( Cmd : TACBrCmd ) ;
begin
  with {$IFNDEF NOGUI}FrmACBrMonitor.ACBrGAV1  {$ELSE}dm.ACBrGAV1  {$ENDIF} do
  begin
     try
        if Cmd.Metodo = 'ativar' then  { Ativa a Gaveta }
         begin
           Ativar ;
           {$IFNDEF NOGUI}FrmACBrMonitor.AvaliaEstadoTsGAV ;{$ENDIF}
         end

        else if Cmd.Metodo = 'desativar' then
         begin
           Desativar ;
           {$IFNDEF NOGUI}FrmACBrMonitor.AvaliaEstadoTsGAV ;{$ENDIF}
         end

        else if Cmd.Metodo = 'ativo' then
           Cmd.Resposta := BoolToStr(Ativo, true)

        else if Cmd.Metodo = 'modelostr' then
           Cmd.Resposta := ModeloStr

        else if Cmd.Metodo = 'modelo' then
           Cmd.Resposta := GetEnumName(TypeInfo(TACBrGAVModelo),Integer(Modelo))

        else if Cmd.Metodo = 'porta' then
           Cmd.Resposta := Porta

        else if Cmd.Metodo = 'abregaveta' then
           AbreGaveta

        else if Cmd.Metodo = 'gavetaaberta' then
           Cmd.Resposta := BoolToStr( GavetaAberta, true )

        else if Cmd.Metodo = 'strcomando' then
           Cmd.Resposta := StrComando

        else if Cmd.Metodo = 'setstrcomando' then
         begin
           StrComando := Cmd.Params(0) ;
           {$IFNDEF NOGUI}FrmACBrMonitor.cbGAVStrAbre.Text := StrComando ;{$ENDIF}
         end

        else if Cmd.Metodo = 'aberturaintervalo' then
           Cmd.Resposta := IntToStr( AberturaIntervalo )

        else if Cmd.Metodo = 'setaberturaintervalo' then
         begin
           AberturaIntervalo := StrToIntDef( Cmd.Params(0), AberturaIntervalo) ;
           {$IFNDEF NOGUI}FrmACBrMonitor.sedGAVIntervaloAbertura.Value := AberturaIntervalo ;{$ENDIF}
         end

        else if Cmd.Metodo = 'aberturaantecipada' then
           Cmd.Resposta := GetEnumName(TypeInfo(TACBrGAVAberturaAntecipada),Integer(AberturaAntecipada))

        ELSE
           raise Exception.Create('Comando inválido ('+Cmd.Comando+')') ;

     finally
        { Nada a fazer aqui por enquanto... :) }
     end ;
  end ;
end ;

end.


