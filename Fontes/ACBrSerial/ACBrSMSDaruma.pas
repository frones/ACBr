{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:   Regys Silveira                                }
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

unit ACBrSMSDaruma;

interface

uses
  ACBrSMSClass, Classes;

type
  TACBrSMSDaruma = class(TACBrSMSClass)
  private

  public
    constructor Create(AOwner: TComponent);
    procedure TrocarBandeja(const ASimCard: TACBrSMSSimCard); override;
  end;

implementation

uses
  ACBrUtil, SysUtils;

{ TACBrSMSDaruma }

constructor TACBrSMSDaruma.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fpBandejasSimCard := 2;
end;

procedure TACBrSMSDaruma.TrocarBandeja(const ASimCard: TACBrSMSSimCard);
var
  cmd: String;
  Tentativas: Integer;
  Str: String;
  ListaParametro: TStringList;
begin
  if ASimCard = SimCard then
    Exit;

  case ASimCard of
    simCard1: cmd := 'ATL1';
    simCard2: cmd := 'ATL2';
  end;

  fpDevice.Serial.Purge;
  Self.EnviarComando(cmd);

  if Self.ATResult then
  begin
    Self.SimCard := ASimCard;

    ListaParametro := TStringList.Create;
    try
      Tentativas := 0;
      repeat

        Sleep(500);
        Self.EnviarComando('AT+CIND?');
        //exemplo de retorno:   +CIND: 5,99,1,0,0,0,0,1,4

        Str := Self.UltimaResposta;
        Str := Trim(Copy(Str, 7, Length(Str) - 9));
        Str := StringReplace(Str, ',', sLineBreak, [rfReplaceAll]);

        ListaParametro.Text := Str;
        Str := ListaParametro[2];

        Inc(Tentativas);

      until (Str = '1') or (Tentativas >= 30);

      if Tentativas > 30 then
        raise EACBrSMSException.Create('Não foi possivel sincronizar o SinCard com a operadora de telefonia.');
    finally
      ListaParametro.Free;
    end;
  end
  else
    raise EACBrSMSException.Create('Não foi possível efetuar a troca da bandeja.')
end;

end.

