{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:  Gabriel Baltazar                               }
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

unit ACBrOpenDeliveryException;

interface

uses
  ACBrBase,
  Classes,
  SysUtils;

type
  EACBrOpenDeliveryException = class(EACBrException)
  public
    constructor Create(const AMsg: string);
    constructor CreateDef(const AMsg: string; ADummy: Integer = 0);
  end;

  EACBrOpenDeliveryHTTPException = class(EACBrOpenDeliveryException)
  private
    FStatus: Integer;
    FTitle: string;
  public
    constructor Create(const AStatus: Integer; const ATitle: string); reintroduce;

    property Status: Integer read FStatus;
    property Title: string read FTitle;
  end;

implementation

uses
  ACBrUtil.Strings;

{ EACBrOpenDeliveryException }

constructor EACBrOpenDeliveryException.Create(const AMsg: string);
begin
  inherited Create(ACBrStr(AMsg));
end;

constructor EACBrOpenDeliveryException.CreateDef(const AMsg: string; ADummy: Integer);
begin
  inherited Create(AMsg);
end;

{ EACBrOpenDeliveryHTTPException }

constructor EACBrOpenDeliveryHTTPException.Create(const AStatus: Integer; const ATitle: string);
var
  LMsg: string;
begin
  LMsg := Format('%d: %s', [AStatus, ATitle]);
  inherited Create(LMsg);
  FStatus := AStatus;
  FTitle := ATitle;
end;

end.

