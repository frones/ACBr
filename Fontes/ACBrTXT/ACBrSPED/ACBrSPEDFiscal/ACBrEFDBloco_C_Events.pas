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

unit ACBrEFDBloco_C_Events;

interface

uses
  SysUtils, Classes, ACBrSped;

type
  TChecksBloco_C = class(TComponent)
  private
    FOwner: TComponent;

    FOnCheckRegistroC100: TCheckRegistroEvent;

    function GetOnCheckRegistroC100: TCheckRegistroEvent;
    procedure SetOnCheckRegistroC100(const AValue: TCheckRegistroEvent);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property OnCheckRegistroC100: TCheckRegistroEvent read GetOnCheckRegistroC100 write SetOnCheckRegistroC100;
  end;

  { TEventsBloco_0 }
  TEventsBloco_C = class(TComponent)
  private
    FOwner: TComponent;

    FOnBeforeWriteRegistroC111: TWriteRegistroEvent;
    FOnBeforeWriteRegistroC120: TWriteRegistroEvent;
    FOnBeforeWriteRegistroC170: TWriteRegistroEvent;
    FOnBeforeWriteRegistroC470: TWriteRegistroEvent;
    FOnBeforeWriteRegistroC465: TWriteRegistroEvent;
    FOnBeforeWriteRegistroC510: TWriteRegistroEvent;

    FOnWriteRegistroC111: TWriteRegistroEvent;
    FOnWriteRegistroC120: TWriteRegistroEvent;
    FOnWriteRegistroC170: TWriteRegistroEvent;
    FOnWriteRegistroC470: TWriteRegistroEvent;
    FOnWriteRegistroC465: TWriteRegistroEvent;
    FOnWriteRegistroC510: TWriteRegistroEvent;

    FOnAfterWriteRegistroC111: TWriteRegistroEvent;
    FOnAfterWriteRegistroC120: TWriteRegistroEvent;
    FOnAfterWriteRegistroC170: TWriteRegistroEvent;
    FOnAfterWriteRegistroC470: TWriteRegistroEvent;
    FOnAfterWriteRegistroC465: TWriteRegistroEvent;
    FOnAfterWriteRegistroC510: TWriteRegistroEvent;
    FOnWriteRegistroC460: TWriteRegistroEvent;

    function GetOnAfterWriteRegistroC111: TWriteRegistroEvent;
    function GetOnAfterWriteRegistroC120: TWriteRegistroEvent;
    function GetOnAfterWriteRegistroC170: TWriteRegistroEvent;
    function GetOnAfterWriteRegistroC470: TWriteRegistroEvent;
    function GetOnAfterWriteRegistroC465: TWriteRegistroEvent;
    function GetOnAfterWriteRegistroC510: TWriteRegistroEvent;
    function GetOnBeforeWriteRegistroC111: TWriteRegistroEvent;
    function GetOnBeforeWriteRegistroC120: TWriteRegistroEvent;
    function GetOnBeforeWriteRegistroC170: TWriteRegistroEvent;
    function GetOnBeforeWriteRegistroC470: TWriteRegistroEvent;
    function GetOnBeforeWriteRegistroC465: TWriteRegistroEvent;
    function GetOnBeforeWriteRegistroC510: TWriteRegistroEvent;
    function GetOnWriteRegistroC111: TWriteRegistroEvent;
    function GetOnWriteRegistroC120: TWriteRegistroEvent;
    function GetOnWriteRegistroC170: TWriteRegistroEvent;
    function GetOnWriteRegistroC460: TWriteRegistroEvent;
    function GetOnWriteRegistroC470: TWriteRegistroEvent;
    function GetOnWriteRegistroC465: TWriteRegistroEvent;
    function GetOnWriteRegistroC510: TWriteRegistroEvent;
    procedure SetOnAfterWriteRegistroC111(const Value: TWriteRegistroEvent);
    procedure SetOnAfterWriteRegistroC120(const Value: TWriteRegistroEvent);
    procedure SetOnAfterWriteRegistroC170(const Value: TWriteRegistroEvent);
    procedure SetOnAfterWriteRegistroC470(const Value: TWriteRegistroEvent);
    procedure SetOnAfterWriteRegistroC465(const Value: TWriteRegistroEvent);
    procedure SetOnAfterWriteRegistroC510(const Value: TWriteRegistroEvent);
    procedure SetOnBeforeWriteRegistroC111(const Value: TWriteRegistroEvent);
    procedure SetOnBeforeWriteRegistroC120(const Value: TWriteRegistroEvent);
    procedure SetOnBeforeWriteRegistroC170(const Value: TWriteRegistroEvent);
    procedure SetOnBeforeWriteRegistroC470(const Value: TWriteRegistroEvent);
    procedure SetOnBeforeWriteRegistroC465(const Value: TWriteRegistroEvent);
    procedure SetOnBeforeWriteRegistroC510(const Value: TWriteRegistroEvent);
    procedure SetOnWriteRegistroC111(const Value: TWriteRegistroEvent);
    procedure SetOnWriteRegistroC120(const Value: TWriteRegistroEvent);
    procedure SetOnWriteRegistroC170(const Value: TWriteRegistroEvent);
    procedure SetOnWriteRegistroC460(const Value: TWriteRegistroEvent);
    procedure SetOnWriteRegistroC470(const Value: TWriteRegistroEvent);
    procedure SetOnWriteRegistroC465(const Value: TWriteRegistroEvent);
    procedure SetOnWriteRegistroC510(const Value: TWriteRegistroEvent);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property OnBeforeWriteRegistroC111: TWriteRegistroEvent read GetOnBeforeWriteRegistroC111 write SetOnBeforeWriteRegistroC111;
    property OnBeforeWriteRegistroC120: TWriteRegistroEvent read GetOnBeforeWriteRegistroC120 write SetOnBeforeWriteRegistroC120;
    property OnBeforeWriteRegistroC170: TWriteRegistroEvent read GetOnBeforeWriteRegistroC170 write SetOnBeforeWriteRegistroC170;
    property OnBeforeWriteRegistroC470: TWriteRegistroEvent read GetOnBeforeWriteRegistroC470 write SetOnBeforeWriteRegistroC470;
    property OnBeforeWriteRegistroC465: TWriteRegistroEvent read GetOnBeforeWriteRegistroC465 write SetOnBeforeWriteRegistroC465;
    property OnBeforeWriteRegistroC510: TWriteRegistroEvent read GetOnBeforeWriteRegistroC510 write SetOnBeforeWriteRegistroC510;

    property OnWriteRegistroC111: TWriteRegistroEvent read GetOnWriteRegistroC111 write SetOnWriteRegistroC111;
    property OnWriteRegistroC120: TWriteRegistroEvent read GetOnWriteRegistroC120 write SetOnWriteRegistroC120;
    property OnWriteRegistroC170: TWriteRegistroEvent read GetOnWriteRegistroC170 write SetOnWriteRegistroC170;
    property OnWriteRegistroC460: TWriteRegistroEvent read GetOnWriteRegistroC460 write SetOnWriteRegistroC460;
    property OnWriteRegistroC465: TWriteRegistroEvent read GetOnWriteRegistroC465 write SetOnWriteRegistroC465;
    property OnWriteRegistroC470: TWriteRegistroEvent read GetOnWriteRegistroC470 write SetOnWriteRegistroC470;
    property OnWriteRegistroC510: TWriteRegistroEvent read GetOnWriteRegistroC510 write SetOnWriteRegistroC510;

    property OnAfterWriteRegistroC111: TWriteRegistroEvent read GetOnAfterWriteRegistroC111 write SetOnAfterWriteRegistroC111;
    property OnAfterWriteRegistroC120: TWriteRegistroEvent read GetOnAfterWriteRegistroC120 write SetOnAfterWriteRegistroC120;
    property OnAfterWriteRegistroC170: TWriteRegistroEvent read GetOnAfterWriteRegistroC170 write SetOnAfterWriteRegistroC170;
    property OnAfterWriteRegistroC465: TWriteRegistroEvent read GetOnAfterWriteRegistroC465 write SetOnAfterWriteRegistroC465;
    property OnAfterWriteRegistroC470: TWriteRegistroEvent read GetOnAfterWriteRegistroC470 write SetOnAfterWriteRegistroC470;
    property OnAfterWriteRegistroC510: TWriteRegistroEvent read GetOnAfterWriteRegistroC510 write SetOnAfterWriteRegistroC510;
  end;

implementation



{ TEventsBloco_0 }

constructor TEventsBloco_C.Create(AOwner: TComponent);
begin
   inherited Create(AOwner);
   FOwner := AOwner;
end;

destructor TEventsBloco_C.Destroy;
begin
   FOwner := nil;
   inherited Destroy;
end;

function TEventsBloco_C.GetOnAfterWriteRegistroC111: TWriteRegistroEvent;
begin
   Result := FOnAfterWriteRegistroC111;
end;

function TEventsBloco_C.GetOnAfterWriteRegistroC120: TWriteRegistroEvent;
begin
   Result := FOnAfterWriteRegistroC120;
end;

function TEventsBloco_C.GetOnAfterWriteRegistroC170: TWriteRegistroEvent;
begin
   Result := FOnAfterWriteRegistroC170;
end;

function TEventsBloco_C.GetOnAfterWriteRegistroC470: TWriteRegistroEvent;
begin
   Result := FOnAfterWriteRegistroC470;
end;

function TEventsBloco_C.GetOnAfterWriteRegistroC465: TWriteRegistroEvent;
begin
   Result := FOnAfterWriteRegistroC465;
end;

function TEventsBloco_C.GetOnAfterWriteRegistroC510: TWriteRegistroEvent;
begin
   Result := FOnAfterWriteRegistroC510;
end;

function TEventsBloco_C.GetOnBeforeWriteRegistroC111: TWriteRegistroEvent;
begin
   Result := FOnBeforeWriteRegistroC111;
end;

function TEventsBloco_C.GetOnBeforeWriteRegistroC120: TWriteRegistroEvent;
begin
   Result := FOnBeforeWriteRegistroC120;
end;

function TEventsBloco_C.GetOnBeforeWriteRegistroC170: TWriteRegistroEvent;
begin
   Result := FOnBeforeWriteRegistroC170;
end;

function TEventsBloco_C.GetOnBeforeWriteRegistroC470: TWriteRegistroEvent;
begin
   Result := FOnBeforeWriteRegistroC470;
end;

function TEventsBloco_C.GetOnBeforeWriteRegistroC465: TWriteRegistroEvent;
begin
   Result := FOnBeforeWriteRegistroC465;
end;

function TEventsBloco_C.GetOnBeforeWriteRegistroC510: TWriteRegistroEvent;
begin
   Result := FOnBeforeWriteRegistroC510;
end;

function TEventsBloco_C.GetOnWriteRegistroC111: TWriteRegistroEvent;
begin
   Result := FOnWriteRegistroC111;
end;

function TEventsBloco_C.GetOnWriteRegistroC120: TWriteRegistroEvent;
begin
   Result := FOnWriteRegistroC120;
end;

function TEventsBloco_C.GetOnWriteRegistroC170: TWriteRegistroEvent;
begin
   Result := FOnWriteRegistroC170;
end;

function TEventsBloco_C.GetOnWriteRegistroC460: TWriteRegistroEvent;
begin
   Result := FOnWriteRegistroC460;
end;

function TEventsBloco_C.GetOnWriteRegistroC470: TWriteRegistroEvent;
begin
   Result := FOnWriteRegistroC470;
end;

function TEventsBloco_C.GetOnWriteRegistroC465: TWriteRegistroEvent;
begin
   Result := FOnWriteRegistroC465;
end;

function TEventsBloco_C.GetOnWriteRegistroC510: TWriteRegistroEvent;
begin
   Result := FOnWriteRegistroC510;
end;

procedure TEventsBloco_C.SetOnAfterWriteRegistroC111(
  const Value: TWriteRegistroEvent);
begin
  FOnAfterWriteRegistroC111 := Value;

//  TACBrSPEDFiscal(FOwner).Bloco_C.OnAfterWriteRegistroC111 := Value;
end;

procedure TEventsBloco_C.SetOnAfterWriteRegistroC120(
  const Value: TWriteRegistroEvent);
begin
  FOnAfterWriteRegistroC120 := Value;

//  TACBrSPEDFiscal(FOwner).Bloco_C.OnAfterWriteRegistroC120 := Value;
end;

procedure TEventsBloco_C.SetOnAfterWriteRegistroC170(
  const Value: TWriteRegistroEvent);
begin
  FOnAfterWriteRegistroC170 := Value;

//  TACBrSPEDFiscal(FOwner).Bloco_C.OnAfterWriteRegistroC170 := Value;
end;

procedure TEventsBloco_C.SetOnAfterWriteRegistroC470(
  const Value: TWriteRegistroEvent);
begin
  FOnAfterWriteRegistroC470 := Value;

//  TACBrSPEDFiscal(FOwner).Bloco_C.OnAfterWriteRegistroC470 := Value;
end;

procedure TEventsBloco_C.SetOnAfterWriteRegistroC465(
  const Value: TWriteRegistroEvent);
begin
  FOnAfterWriteRegistroC465 := Value;

//  TACBrSPEDFiscal(FOwner).Bloco_C.OnAfterWriteRegistroC465 := Value;
end;

procedure TEventsBloco_C.SetOnAfterWriteRegistroC510(
  const Value: TWriteRegistroEvent);
begin
  FOnAfterWriteRegistroC510 := Value;

//  TACBrSPEDFiscal(FOwner).Bloco_C.OnAfterWriteRegistroC510 := Value;
end;

procedure TEventsBloco_C.SetOnBeforeWriteRegistroC111(
  const Value: TWriteRegistroEvent);
begin
  FOnBeforeWriteRegistroC111 := Value;

//  TACBrSPEDFiscal(FOwner).Bloco_C.OnBeforeWriteRegistroC111 := Value;
end;

procedure TEventsBloco_C.SetOnBeforeWriteRegistroC120(
  const Value: TWriteRegistroEvent);
begin
  FOnBeforeWriteRegistroC120 := Value;

//  TACBrSPEDFiscal(FOwner).Bloco_C.OnBeforeWriteRegistroC120 := Value;
end;

procedure TEventsBloco_C.SetOnBeforeWriteRegistroC170(
  const Value: TWriteRegistroEvent);
begin
  FOnBeforeWriteRegistroC170 := Value;

//  TACBrSPEDFiscal(FOwner).Bloco_C.OnBeforeWriteRegistroC170 := Value;
end;

procedure TEventsBloco_C.SetOnBeforeWriteRegistroC470(
  const Value: TWriteRegistroEvent);
begin
  FOnBeforeWriteRegistroC470 := Value;

//  TACBrSPEDFiscal(FOwner).Bloco_C.OnBeforeWriteRegistroC470 := Value;
end;

procedure TEventsBloco_C.SetOnBeforeWriteRegistroC465(
  const Value: TWriteRegistroEvent);
begin
  FOnBeforeWriteRegistroC465 := Value;

//  TACBrSPEDFiscal(FOwner).Bloco_C.OnBeforeWriteRegistroC465 := Value;
end;

procedure TEventsBloco_C.SetOnBeforeWriteRegistroC510(
  const Value: TWriteRegistroEvent);
begin
  FOnBeforeWriteRegistroC510 := Value;

//  TACBrSPEDFiscal(FOwner).Bloco_C.OnBeforeWriteRegistroC510 := Value;
end;

procedure TEventsBloco_C.SetOnWriteRegistroC111(
  const Value: TWriteRegistroEvent);
begin
  FOnWriteRegistroC111 := Value;

//  TACBrSPEDFiscal(FOwner).Bloco_C.OnWriteRegistroC111 := Value;
end;

procedure TEventsBloco_C.SetOnWriteRegistroC120(
  const Value: TWriteRegistroEvent);
begin
  FOnWriteRegistroC120 := Value;

//  TACBrSPEDFiscal(FOwner).Bloco_C.OnWriteRegistroC120 := Value;
end;

procedure TEventsBloco_C.SetOnWriteRegistroC170(
  const Value: TWriteRegistroEvent);
begin
  FOnWriteRegistroC170 := Value;

//  TACBrSPEDFiscal(FOwner).Bloco_C.OnWriteRegistroC170 := Value;
end;

procedure TEventsBloco_C.SetOnWriteRegistroC460(
  const Value: TWriteRegistroEvent);
begin
  FOnWriteRegistroC460 := Value;

//  TACBrSPEDFiscal(FOwner).Bloco_C.OnWriteRegistroC460 := Value;
end;

procedure TEventsBloco_C.SetOnWriteRegistroC470(
  const Value: TWriteRegistroEvent);
begin
  FOnWriteRegistroC470 := Value;

//  TACBrSPEDFiscal(FOwner).Bloco_C.OnWriteRegistroC470 := Value;
end;

procedure TEventsBloco_C.SetOnWriteRegistroC465(
  const Value: TWriteRegistroEvent);
begin
  FOnWriteRegistroC465 := Value;

//  TACBrSPEDFiscal(FOwner).Bloco_C.OnWriteRegistroC465 := Value;
end;

procedure TEventsBloco_C.SetOnWriteRegistroC510(
  const Value: TWriteRegistroEvent);
begin
  FOnWriteRegistroC510 := Value;

//  TACBrSPEDFiscal(FOwner).Bloco_C.OnWriteRegistroC510 := Value;
end;

{ TChecksBloco_C }

constructor TChecksBloco_C.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOwner := AOwner;
end;

destructor TChecksBloco_C.Destroy;
begin
  FOwner := nil;
  inherited Destroy;
end;

function TChecksBloco_C.GetOnCheckRegistroC100: TCheckRegistroEvent;
begin
   Result := FOnCheckRegistroC100;
end;

procedure TChecksBloco_C.SetOnCheckRegistroC100(
  const AValue: TCheckRegistroEvent);
begin
   FOnCheckRegistroC100 := AValue;
//      TACBrSPEDFiscal(FOwner).Bloco_C.OnCheckRegistroC100 := Value;
end;

end.
