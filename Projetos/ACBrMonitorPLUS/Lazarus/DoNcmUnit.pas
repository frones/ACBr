{******************************************************************************}
{ Projeto: ACBr Monitor                                                        }
{  Executavel multiplataforma que faz uso do conjunto de componentes ACBr para }
{ criar uma interface de comunicação com equipamentos de automacao comercial.  }
{                                                                              }
{ Direitos Autorais Reservados (c) 2010 Daniel Simões de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Juliana Rodrigues Prado Tamizou                 }
{                              Jean Patrick F. dos Santos (envio de e-mails)   }
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
unit DoNcmUnit;


interface

uses
  Classes, SysUtils, CmdUnit;

procedure DoNcm(Cmd: TACBrCmd);
function ValidarNcm(aNcm: string): string;
function SalvarListaNcm(FileName: string): string;

implementation

uses ACBrUtil, TypInfo,
{$IFNDEF NOGUI}ACBrMonitor1 {$ELSE}ACBrMonitorConsoleDM {$ENDIF} ;
procedure DoNcm(Cmd: TACBrCmd);
begin
  with {$IFNDEF NOGUI}FrmACBrMonitor.ACBrNCMs1 {$ELSE}dm.ACBrNCMs1 {$ENDIF} do
  begin
    if Cmd.Metodo = 'validar' then
      Cmd.Resposta := ValidarNcm( OnlyNumber(Cmd.Params(0)) )

    else if cmd.Metodo = 'baixarlista' then
      Cmd.Resposta := SalvarListaNcm( Cmd.Params(0) )

    else if cmd.Metodo = 'descricaoncm' then
      Cmd.Resposta := DescricaoNcm( Cmd.Params(0) )

    else
      raise Exception.Create('Comando inválido (' + Cmd.Comando + ')');
  end;
end;

function ValidarNcm(aNcm: string): string;
begin
  Result := 'NCM Valido';
  with {$IFNDEF NOGUI}FrmACBrMonitor.ACBrNCMs1 {$ELSE}dm.ACBrNCMs1 {$ENDIF} do
  begin
    if (Length(aNcm) <> 8) then
      raise Exception.Create('O codigo do NCM deve conter 8 Caracteres');

    if not validar(aNcm) then
      raise Exception.Create('NCM Invalido');
  end;
end;

function SalvarListaNcm(FileName: string): string;
begin
  with {$IFNDEF NOGUI}FrmACBrMonitor.ACBrNCMs1 {$ELSE}dm.ACBrNCMs1 {$ENDIF} do
  begin
    ListarNcms();
    NCMS.SaveToFile( FileName );
  end;

  Result := 'Arquivo salvo em: ' + FileName;
end;

end.

