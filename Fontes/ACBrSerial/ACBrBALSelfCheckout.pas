{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2023 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Elias César Vieira                              }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }
{                                                                              }
{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }
{                                                                              }
{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{******************************************************************************}

{$I ACBr.inc}

unit ACBrBALSelfCheckout;

interface

uses
  Classes, SysUtils, ACBrBALClass, ACBrBase;
  
type

  { TACBrBALSelfCheckout }

  TACBrBALSelfCheckout = class(TACBrBALClass)
  protected
    function CorrigirRespostaPeso(const aResposta: AnsiString): AnsiString;
  public
    procedure AtivarTara;
    procedure DesativarTara;
    procedure ZerarDispositivo;
    procedure LigarDisplay;
    procedure DesligarDisplay;
  end;

implementation

{ TACBrBALSelfCheckout }

function TACBrBALSelfCheckout.CorrigirRespostaPeso(const aResposta: AnsiString): AnsiString;
begin
  Result := Trim(aResposta);

  if (Result = EmptyStr) then
    Exit;

  { Linha Self-Checkout possui retorno diferente:
    [ STX ] [ +PPP,PPP ] [ ETX ]  - Peso Estável;
    [ STX ] [ +III,III ] [ ETX ]  - Peso Instável;
    [ STX ] [ -III,III ] [ ETX ]  - Peso Negativo;
    [ STX ] [ +SSS,SSS ] [ ETX ]  - Peso Acima (Sobrecarga) }

  Result := StringReplace(Result, '+III,III', 'IIIII', [rfReplaceAll]);
  Result := StringReplace(Result, '-III,III', 'NNNNN', [rfReplaceAll]);
  Result := StringReplace(Result, '+SSS,SSS', 'SSSSS', [rfReplaceAll]);
  Result := StringReplace(Result, '+', '', [rfReplaceAll]);
  Result := StringReplace(Result, '-', '', [rfReplaceAll]);
end;

procedure TACBrBALSelfCheckout.AtivarTara;
begin
  fpDevice.EnviaString('T');
  GravarLog(' - ' + FormatDateTime('hh:nn:ss:zzz', Now) + ' Ativar Tara');
end;

procedure TACBrBALSelfCheckout.DesativarTara;
begin
  fpDevice.EnviaString('T');
  GravarLog(' - ' + FormatDateTime('hh:nn:ss:zzz', Now) + ' Desativar Tara');
end;

procedure TACBrBALSelfCheckout.ZerarDispositivo;
begin
  fpDevice.EnviaString('Z');
  GravarLog(' - ' + FormatDateTime('hh:nn:ss:zzz', Now) + ' Zerar Dispositivo');
end;

procedure TACBrBALSelfCheckout.LigarDisplay;
begin
  fpDevice.EnviaString('L');
  GravarLog(' - ' + FormatDateTime('hh:nn:ss:zzz', Now) + ' LigarDisplay');
end;

procedure TACBrBALSelfCheckout.DesligarDisplay;
begin
  fpDevice.EnviaString('L');
  GravarLog(' - ' + FormatDateTime('hh:nn:ss:zzz', Now) + ' DesligarDisplay');
end;

end.

