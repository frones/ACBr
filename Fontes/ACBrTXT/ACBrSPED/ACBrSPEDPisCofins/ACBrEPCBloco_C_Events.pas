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

unit ACBrEPCBloco_C_Events;

interface

uses
  SysUtils, Classes, ACBrEPCBloco_C_Class;

type
  { TEventsBloco_0 }
  TEventsBloco_C = class(TComponent)
  private
    FOwner: TComponent;
    function GetOnBeforeWriteRegistroC481: TWriteRegistroC481Event;
    function GetOnBeforeWriteRegistroC485: TWriteRegistroC485Event;
    procedure SetOnBeforeWriteRegistroC481(const Value: TWriteRegistroC481Event);
    procedure SetOnBeforeWriteRegistroC485(const Value: TWriteRegistroC485Event);
    function GetOnBeforeWriteRegistroC491: TWriteRegistroC491Event;
    function GetOnBeforeWriteRegistroC495: TWriteRegistroC495Event;
    procedure SetOnBeforeWriteRegistroC491(const Value: TWriteRegistroC491Event);
    procedure SetOnBeforeWriteRegistroC495(const Value: TWriteRegistroC495Event);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property OnBeforeWriteRegistroC481: TWriteRegistroC481Event read GetOnBeforeWriteRegistroC481 write SetOnBeforeWriteRegistroC481;
    property OnBeforeWriteRegistroC485: TWriteRegistroC485Event read GetOnBeforeWriteRegistroC485 write SetOnBeforeWriteRegistroC485;
    property OnBeforeWriteRegistroC491: TWriteRegistroC491Event read GetOnBeforeWriteRegistroC491 write SetOnBeforeWriteRegistroC491;
    property OnBeforeWriteRegistroC495: TWriteRegistroC495Event read GetOnBeforeWriteRegistroC495 write SetOnBeforeWriteRegistroC495;
  end;

implementation

uses ACBrSpedPisCofins;

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

function TEventsBloco_C.GetOnBeforeWriteRegistroC481: TWriteRegistroC481Event;
begin
   Result := TACBrSPEDPisCofins(FOwner).Bloco_C.OnBeforeWriteRegistroC481;
end;

function TEventsBloco_C.GetOnBeforeWriteRegistroC485: TWriteRegistroC485Event;
begin
   Result := TACBrSPEDPisCofins(FOwner).Bloco_C.OnBeforeWriteRegistroC485;
end;

function TEventsBloco_C.GetOnBeforeWriteRegistroC491: TWriteRegistroC491Event;
begin
   Result := TACBrSPEDPisCofins(FOwner).Bloco_C.OnBeforeWriteRegistroC491;
end;

function TEventsBloco_C.GetOnBeforeWriteRegistroC495: TWriteRegistroC495Event;
begin
   Result := TACBrSPEDPisCofins(FOwner).Bloco_C.OnBeforeWriteRegistroC495;
end;

procedure TEventsBloco_C.SetOnBeforeWriteRegistroC481(
  const Value: TWriteRegistroC481Event);
begin
  TACBrSPEDPisCofins(FOwner).Bloco_C.OnBeforeWriteRegistroC481 := Value;
end;

procedure TEventsBloco_C.SetOnBeforeWriteRegistroC485(
  const Value: TWriteRegistroC485Event);
begin
  TACBrSPEDPisCofins(FOwner).Bloco_C.OnBeforeWriteRegistroC485 := Value;
end;

procedure TEventsBloco_C.SetOnBeforeWriteRegistroC491(
  const Value: TWriteRegistroC491Event);
begin
  TACBrSPEDPisCofins(FOwner).Bloco_C.OnBeforeWriteRegistroC491 := Value;
end;

procedure TEventsBloco_C.SetOnBeforeWriteRegistroC495(
  const Value: TWriteRegistroC495Event);
begin
  TACBrSPEDPisCofins(FOwner).Bloco_C.OnBeforeWriteRegistroC495 := Value;
end;

end.
