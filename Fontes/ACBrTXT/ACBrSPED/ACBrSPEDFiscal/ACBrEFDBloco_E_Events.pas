{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Isaque Pinheiro                                 }
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

unit ACBrEFDBloco_E_Events;

interface

uses
  SysUtils, Classes, ACBrSped;

type
  { TEventsBloco_E }
  TEventsBloco_E = class(TComponent)
  private
    FOwner: TComponent;

    FOnBeforeWriteRegistroE990: TWriteRegistroEvent;

    FOnWriteRegistroE990: TWriteRegistroEvent;

    FOnAfterWriteRegistroE990: TWriteRegistroEvent;

    function GetOnAfterWriteRegistroE990: TWriteRegistroEvent;
    function GetOnBeforeWriteRegistroE990: TWriteRegistroEvent;
    function GetOnWriteRegistroE990: TWriteRegistroEvent;

    procedure SetOnAfterWriteRegistroE990(const Value: TWriteRegistroEvent);
    procedure SetOnBeforeWriteRegistroE990(const Value: TWriteRegistroEvent);
    procedure SetOnWriteRegistroE990(const Value: TWriteRegistroEvent);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property OnBeforeWriteRegistroE990: TWriteRegistroEvent read GetOnBeforeWriteRegistroE990 write SetOnBeforeWriteRegistroE990;

    property OnWriteRegistroE990: TWriteRegistroEvent read GetOnWriteRegistroE990 write SetOnWriteRegistroE990;

    property OnAfterWriteRegistroE990: TWriteRegistroEvent read GetOnAfterWriteRegistroE990 write SetOnAfterWriteRegistroE990;
  end;

implementation

uses ACBrSpedFiscal;

{ TEventsBloco_0 }

constructor TEventsBloco_E.Create(AOwner: TComponent);
begin
   inherited Create(AOwner);
   FOwner := AOwner;
end;

destructor TEventsBloco_E.Destroy;
begin
   FOwner := nil;
   inherited Destroy;
end;

function TEventsBloco_E.GetOnAfterWriteRegistroE990: TWriteRegistroEvent;
begin
  Result := FOnAfterWriteRegistroE990;
end;

function TEventsBloco_E.GetOnBeforeWriteRegistroE990: TWriteRegistroEvent;
begin
  Result := FOnBeforeWriteRegistroE990;
end;

function TEventsBloco_E.GetOnWriteRegistroE990: TWriteRegistroEvent;
begin
  Result := FOnWriteRegistroE990;
end;

procedure TEventsBloco_E.SetOnAfterWriteRegistroE990( const Value: TWriteRegistroEvent);
begin
  FOnAfterWriteRegistroE990 := Value;

  TACBrSPEDFiscal(FOwner).Bloco_E.OnAfterWriteRegistroE990 := Value;
end;


procedure TEventsBloco_E.SetOnBeforeWriteRegistroE990( const Value: TWriteRegistroEvent);
begin
  FOnBeforeWriteRegistroE990:= Value;

  TACBrSPEDFiscal(FOwner).Bloco_E.OnBeforeWriteRegistroE990 := Value;
end;

procedure TEventsBloco_E.SetOnWriteRegistroE990( const Value: TWriteRegistroEvent);
begin
  FOnWriteRegistroE990 := Value;

  TACBrSPEDFiscal(FOwner).Bloco_E.OnWriteRegistroE990 := Value;
end;

end.
