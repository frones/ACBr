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

unit ACBrEFDBloco_D_Events;

interface

uses
  SysUtils, Classes, ACBrSped;

type
  { TEventsBloco_D }
  TEventsBloco_D = class(TComponent)
  private
    FOwner: TComponent;

    FOnBeforeWriteRegistroD100: TWriteRegistroEvent;
    FOnBeforeWriteRegistroD110: TWriteRegistroEvent;
    FOnBeforeWriteRegistroD510: TWriteRegistroEvent;

    FOnWriteRegistroD100: TWriteRegistroEvent;
    FOnWriteRegistroD110: TWriteRegistroEvent;
    FOnWriteRegistroD510: TWriteRegistroEvent;

    FOnAfterWriteRegistroD100: TWriteRegistroEvent;
    FOnAfterWriteRegistroD110: TWriteRegistroEvent;
    FOnAfterWriteRegistroD510: TWriteRegistroEvent;

    function GetOnAfterWriteRegistroD100: TWriteRegistroEvent;
    function GetOnAfterWriteRegistroD110: TWriteRegistroEvent;
    function GetOnAfterWriteRegistroD510: TWriteRegistroEvent;
    function GetOnBeforeWriteRegistroD100: TWriteRegistroEvent;
    function GetOnBeforeWriteRegistroD110: TWriteRegistroEvent;
    function GetOnBeforeWriteRegistroD510: TWriteRegistroEvent;
    function GetOnWriteRegistroD100: TWriteRegistroEvent;
    function GetOnWriteRegistroD110: TWriteRegistroEvent;
    function GetOnWriteRegistroD510: TWriteRegistroEvent;
    procedure SetOnAfterWriteRegistroD100(const Value: TWriteRegistroEvent);
    procedure SetOnAfterWriteRegistroD110(const Value: TWriteRegistroEvent);
    procedure SetOnAfterWriteRegistroD510(const Value: TWriteRegistroEvent);
    procedure SetOnBeforeWriteRegistroD100(const Value: TWriteRegistroEvent);
    procedure SetOnBeforeWriteRegistroD110(const Value: TWriteRegistroEvent);
    procedure SetOnBeforeWriteRegistroD510(const Value: TWriteRegistroEvent);
    procedure SetOnWriteRegistroD100(const Value: TWriteRegistroEvent);
    procedure SetOnWriteRegistroD110(const Value: TWriteRegistroEvent);
    procedure SetOnWriteRegistroD510(const Value: TWriteRegistroEvent);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property OnBeforeWriteRegistroD100: TWriteRegistroEvent read GetOnBeforeWriteRegistroD100 write SetOnBeforeWriteRegistroD100;
    property OnBeforeWriteRegistroD110: TWriteRegistroEvent read GetOnBeforeWriteRegistroD110 write SetOnBeforeWriteRegistroD110;
    property OnBeforeWriteRegistroD510: TWriteRegistroEvent read GetOnBeforeWriteRegistroD510 write SetOnBeforeWriteRegistroD510;

    property OnWriteRegistroD100: TWriteRegistroEvent read GetOnWriteRegistroD100 write SetOnWriteRegistroD100;
    property OnWriteRegistroD110: TWriteRegistroEvent read GetOnWriteRegistroD110 write SetOnWriteRegistroD110;
    property OnWriteRegistroD510: TWriteRegistroEvent read GetOnWriteRegistroD510 write SetOnWriteRegistroD510;

    property OnAfterWriteRegistroD100: TWriteRegistroEvent read GetOnAfterWriteRegistroD100 write SetOnAfterWriteRegistroD100;
    property OnAfterWriteRegistroD110: TWriteRegistroEvent read GetOnAfterWriteRegistroD110 write SetOnAfterWriteRegistroD110;
    property OnAfterWriteRegistroD510: TWriteRegistroEvent read GetOnAfterWriteRegistroD510 write SetOnAfterWriteRegistroD510;
  end;

implementation

uses ACBrSpedFiscal;

{ TEventsBloco_0 }

constructor TEventsBloco_D.Create(AOwner: TComponent);
begin
   inherited Create(AOwner);
   FOwner := AOwner;
end;

destructor TEventsBloco_D.Destroy;
begin
   FOwner := nil;
   inherited Destroy;
end;

function TEventsBloco_D.GetOnAfterWriteRegistroD100: TWriteRegistroEvent;
begin
   Result := FOnAfterWriteRegistroD100;
end;

function TEventsBloco_D.GetOnAfterWriteRegistroD110: TWriteRegistroEvent;
begin
   Result := FOnAfterWriteRegistroD110;
end;

function TEventsBloco_D.GetOnAfterWriteRegistroD510: TWriteRegistroEvent;
begin
   Result := FOnAfterWriteRegistroD510;
end;

function TEventsBloco_D.GetOnBeforeWriteRegistroD100: TWriteRegistroEvent;
begin
   Result := FOnBeforeWriteRegistroD100;
end;

function TEventsBloco_D.GetOnBeforeWriteRegistroD110: TWriteRegistroEvent;
begin
   Result := FOnBeforeWriteRegistroD110;
end;

function TEventsBloco_D.GetOnBeforeWriteRegistroD510: TWriteRegistroEvent;
begin
   Result := FOnBeforeWriteRegistroD510;
end;

function TEventsBloco_D.GetOnWriteRegistroD100: TWriteRegistroEvent;
begin
   Result := FOnWriteRegistroD100;
end;

function TEventsBloco_D.GetOnWriteRegistroD110: TWriteRegistroEvent;
begin
   Result := FOnWriteRegistroD110;
end;

function TEventsBloco_D.GetOnWriteRegistroD510: TWriteRegistroEvent;
begin
   Result := FOnWriteRegistroD510;
end;

procedure TEventsBloco_D.SetOnAfterWriteRegistroD100(
  const Value: TWriteRegistroEvent);
begin
  FOnAfterWriteRegistroD100 := Value;

  TACBrSPEDFiscal(FOwner).Bloco_D.OnAfterWriteRegistroD100 := Value;
end;

procedure TEventsBloco_D.SetOnAfterWriteRegistroD110(
  const Value: TWriteRegistroEvent);
begin
  FOnAfterWriteRegistroD110 := Value;

  TACBrSPEDFiscal(FOwner).Bloco_D.OnAfterWriteRegistroD110 := Value;
end;

procedure TEventsBloco_D.SetOnAfterWriteRegistroD510(
  const Value: TWriteRegistroEvent);
begin
  FOnAfterWriteRegistroD510 := Value;

  TACBrSPEDFiscal(FOwner).Bloco_D.OnAfterWriteRegistroD510 := Value;
end;

procedure TEventsBloco_D.SetOnBeforeWriteRegistroD100(
  const Value: TWriteRegistroEvent);
begin
  FOnBeforeWriteRegistroD100 := Value;

  TACBrSPEDFiscal(FOwner).Bloco_D.OnBeforeWriteRegistroD100 := Value;
end;

procedure TEventsBloco_D.SetOnBeforeWriteRegistroD110(
  const Value: TWriteRegistroEvent);
begin
  FOnBeforeWriteRegistroD110 := Value;

  TACBrSPEDFiscal(FOwner).Bloco_D.OnBeforeWriteRegistroD110 := Value;
end;

procedure TEventsBloco_D.SetOnBeforeWriteRegistroD510(
  const Value: TWriteRegistroEvent);
begin
  FOnBeforeWriteRegistroD510 := Value;

  TACBrSPEDFiscal(FOwner).Bloco_D.OnBeforeWriteRegistroD510 := Value;
end;

procedure TEventsBloco_D.SetOnWriteRegistroD100(
  const Value: TWriteRegistroEvent);
begin
  FOnWriteRegistroD100 := Value;

  TACBrSPEDFiscal(FOwner).Bloco_D.OnWriteRegistroD100 := Value;
end;

procedure TEventsBloco_D.SetOnWriteRegistroD110(
  const Value: TWriteRegistroEvent);
begin
  FOnWriteRegistroD110 := Value;

  TACBrSPEDFiscal(FOwner).Bloco_D.OnWriteRegistroD110 := Value;
end;

procedure TEventsBloco_D.SetOnWriteRegistroD510(
  const Value: TWriteRegistroEvent);
begin
  FOnWriteRegistroD510 := Value;

  TACBrSPEDFiscal(FOwner).Bloco_D.OnWriteRegistroD510 := Value;
end;

end.
