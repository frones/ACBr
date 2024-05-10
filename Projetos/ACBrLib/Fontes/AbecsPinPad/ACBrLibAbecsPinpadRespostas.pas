{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Antonio Carlos Junior                           }
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

unit ACBrLibAbecsPinpadRespostas;

interface

uses
  Classes, SysUtils, ACBrBase, ACBrAbecsPinPad, ACBrLibResposta, ACBrLibAbecsPinpadConsts;

type

  { TLibAbecsPinpadCapabilitiesResposta }
  TLibAbecsPinpadCapabilitiesResposta = class(TACBrLibRespostaBase)
    private
      fSerialNumber: String;
      fPartNumber: String;
      fModel: String;
      fMemory: String;
      fManufacturer: String;
      fSupportContactless: Boolean;
      fDisplayIsGraphic: Boolean;
      fDisplayIsColor: Boolean;
      fSpecificationVersion: Double;
      fDisplayTextModeDimensionsRows: LongWord;
      fDisplayTextModeDimensionsCols: LongWord;
      fDisplayGraphicPixelsRows: LongWord;
      fDisplayGraphicPixelsCols: LongWord;
      fMediaPNGisSupported: Boolean;
      fMediaJPGisSupported: Boolean;
      fMediaGIFisSupported: Boolean;

    public
      constructor Create(const aTipo: TACBrLibRespostaTipo; const aFormato: TACBrLibCodificacao); reintroduce;
      destructor Destroy; override;

      procedure Clear;
      procedure Processar(const AbecsPinpadCapabilities: TACBrAbecsPinPadCapabilities);

    published
      property SerialNumber: String read fSerialNumber write fSerialNumber;
      property PartNumber: String read fPartNumber write fPartNumber;
      property Model: String read fModel write fModel;
      property Memory: String read fMemory write fMemory;
      property Manufacturer: String read fManufacturer write fManufacturer;
      property SupportContactless: Boolean read fSupportContactless write fSupportContactless;
      property DisplayIsGraphic: Boolean read fDisplayIsGraphic write fDisplayIsGraphic;
      property DisplayIsColor: Boolean read fDisplayIsColor write fDisplayIsColor;
      property SpecificationVersion: Double read fSpecificationVersion write fSpecificationVersion;
      property DisplayTextModeDimensionsRows: LongWord read fDisplayTextModeDimensionsRows write fDisplayTextModeDimensionsRows;
      property DisplayTextModeDimensionsCols: LongWord read fDisplayTextModeDimensionsCols write fDisplayTextModeDimensionsCols;
      property DisplayGraphicPixelsRows: LongWord read fDisplayGraphicPixelsRows write fDisplayGraphicPixelsRows;
      property DisplayGraphicPixelsCols: LongWord read fDisplayGraphicPixelsCols write fDisplayGraphicPixelsCols;
      property MediaPNGisSupported: Boolean read fMediaPNGisSupported write fMediaPNGisSupported;
      property MediaJPGisSupported: Boolean read fMediaJPGisSupported write fMediaJPGisSupported;
      property MediaGIFisSupported: Boolean read fMediaGIFisSupported write fMediaGIFisSupported;
  end;

  { TLibAbecsPinpadRespostaLoadMedia }
  TLibAbecsPinpadRespostaLoadMedia = class(TACBrLibRespostaBase)
    private
      fSTAT: Integer;
      fRespostaLoadMedia: AnsiString;

    public
      constructor Create(const aTipo: TACBrLibRespostaTipo; const aFormato: TACBrLibCodificacao); reintroduce;
      destructor Destroy; override;

      procedure Clear;
      procedure ProcessarLoadMedia(const RespostaLoadMedia: TACBrAbecsResponse);

    published
      property STAT: Integer read fSTAT write fSTAT;
      property RespostaLoadMedia: AnsiString read fRespostaLoadMedia write fRespostaLoadMedia;
  end;

  { TLibAbecsPinpadRespostaMNU }
  TLibAbecsPinpadRespostaMNU = class(TACBrLibRespostaBase)
    private
      fSTAT: Integer;
      fRespostaMNU: AnsiString;

    public
      constructor Create(const aTipo: TACBrLibRespostaTipo; const aFormato: TACBrLibCodificacao); reintroduce;
      destructor Destroy; override;

      procedure Clear;
      procedure ProcessarMNU(const RespostaMNU: TACBrAbecsResponse);

    published
      property STAT: Integer read fSTAT write fSTAT;
      property RespostaMNU: AnsiString read fRespostaMNU write fRespostaMNU;
  end;

  { TLibAbecsPinpadRespostaCEX }
  TLibAbecsPinpadRespostaCEX = class(TACBrLibRespostaBase)
    private
      fSTAT: Integer;
      fRespostaCEX: AnsiString;

    public
      constructor Create(const aTipo: TACBrLibRespostaTipo; const aFormato: TACBrLibCodificacao); reintroduce;
      destructor Destroy; override;

      procedure Clear;
      procedure ProcessarCEX(const RespostaCEX: TACBrAbecsResponse);

    published
      property STAT: Integer read fSTAT write fSTAT;
      property RespostaCEX: AnsiString read fRespostaCEX write fRespostaCEX;
  end;

  { TLibAbecsPinpadRespostaGCD }
  TLibAbecsPinpadRespostaGCD = class(TACBrLibRespostaBase)
    private
      fSTAT: Integer;
      fRespostaGCD: AnsiString;

    public
      constructor Create(const aTipo: TACBrLibRespostaTipo; const aFormato: TACBrLibCodificacao); reintroduce;
      destructor Destroy; override;

      procedure Clear;
      procedure ProcessarGCD(const RespostaGCD: TACBrAbecsResponse);

    published
      property STAT: Integer read fSTAT write fSTAT;
      property RespostaGCD: AnsiString read fRespostaGCD write fRespostaGCD;
  end;

  { TLibAbecsPinpadRespostaGIX }
  TLibAbecsPinpadRespostaGIX = class(TACBrLibRespostaBase)
    private
      fSTAT: Integer;
      fRespostaGIX: AnsiString;

    public
      constructor Create(const aTipo: TACBrLibRespostaTipo; const aFormato: TACBrLibCodificacao); reintroduce;
      destructor Destroy; override;

      procedure Clear;
      procedure ProcessarGIX(const RespostaGIX: TACBrAbecsResponse);

    published
      property STAT: Integer read fSTAT write fSTAT;
      property RespostaGIX: AnsiString read fRespostaGIX write fRespostaGIX;
  end;

  { TLibAbecsPinpadRespostaGIN }
  TLibAbecsPinpadRespostaGIN = class(TACBrLibRespostaBase)
    private
      fSTAT: Integer;
      fRespostaGIN: AnsiString;

    public
      constructor Create(const aTipo: TACBrLibRespostaTipo; const aFormato: TACBrLibCodificacao); reintroduce;
      destructor Destroy; override;

      procedure Clear;
      procedure ProcessarGIN(const RespostaGIN: TACBrAbecsResponse);

    published
      property STAT: Integer read fSTAT write fSTAT;
      property RespostaGIN: AnsiString read fRespostaGIN write fRespostaGIN;
  end;

implementation

{ TLibAbecsPinpadRespostaGIN }
constructor TLibAbecsPinpadRespostaGIN.Create(const aTipo: TACBrLibRespostaTipo; const aFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespAbecsPinpadGIN, ATipo, AFormato);
end;

destructor TLibAbecsPinpadRespostaGIN.Destroy;
begin
  inherited Destroy;
end;

procedure TLibAbecsPinpadRespostaGIN.Clear;
begin
  fSTAT := 0;
end;

procedure TLibAbecsPinpadRespostaGIN.ProcessarGIN(const RespostaGIN: TACBrAbecsResponse);
begin
  fSTAT := RespostaGIN.STAT;
  fRespostaGIN := RespostaGIN.GetResponseData;
end;

{ TLibAbecsPinpadRespostaGIX }
constructor TLibAbecsPinpadRespostaGIX.Create(const aTipo: TACBrLibRespostaTipo; const aFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespAbecsPinpadGIX, ATipo, AFormato);
end;

destructor TLibAbecsPinpadRespostaGIX.Destroy;
begin
  inherited Destroy;
end;

procedure TLibAbecsPinpadRespostaGIX.Clear;
begin
  fSTAT := 0;
end;

procedure TLibAbecsPinpadRespostaGIX.ProcessarGIX(const RespostaGIX: TACBrAbecsResponse);
var
  i: Integer;
  Values: TStrings;
begin
  fSTAT := RespostaGIX.STAT;
  Values := TStringList.Create;
  try
    RespostaGIX.GetResponseAsValues(Values);

    fRespostaGIX := '';
    for i := 0 to Values.Count - 1 do
    begin
      if fRespostaGIX <> '' then
      fRespostaGIX := fRespostaGIX + #13#10;
      fRespostaGIX := fRespostaGIX + Values[i];
    end;
  finally
    Values.Free;
  end;
end;

{ TLibAbecsPinpadRespostaGCD }
constructor TLibAbecsPinpadRespostaGCD.Create(const aTipo: TACBrLibRespostaTipo; const aFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespAbecsPinpadGCD, ATipo, AFormato);
end;

destructor TLibAbecsPinpadRespostaGCD.Destroy;
begin
  inherited Destroy;
end;

procedure TLibAbecsPinpadRespostaGCD.Clear;
begin
  fSTAT := 0;
end;

procedure TLibAbecsPinpadRespostaGCD.ProcessarGCD(const RespostaGCD: TACBrAbecsResponse);
begin
  fSTAT := RespostaGCD.STAT;
  fRespostaGCD := RespostaGCD.GetResponseFromTagValue(PP_VALUE);
end;

{ TLibAbecsPinpadRespostaCEX }
constructor TLibAbecsPinpadRespostaCEX.Create(const aTipo: TACBrLibRespostaTipo; const aFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespAbecsPinpadCEX, ATipo, AFormato);
end;

destructor TLibAbecsPinpadRespostaCEX.Destroy;
begin
  inherited Destroy;
end;

procedure TLibAbecsPinpadRespostaCEX.Clear;
begin
  fSTAT := 0;
end;

procedure TLibAbecsPinpadRespostaCEX.ProcessarCEX(const RespostaCEX: TACBrAbecsResponse);
begin
  fSTAT := RespostaCEX.STAT;
  fRespostaCEX := RespostaCEX.GetResponseFromTagValue(PP_EVENT);
end;

{ TLibAbecsPinpadRespostaMNU }
constructor TLibAbecsPinpadRespostaMNU.Create(const aTipo: TACBrLibRespostaTipo; const aFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespAbecsPinpadMNU, ATipo, AFormato);
end;

destructor TLibAbecsPinpadRespostaMNU.Destroy;
begin
  inherited Destroy;
end;

procedure TLibAbecsPinpadRespostaMNU.Clear;
begin
  fSTAT := 0;
end;

procedure TLibAbecsPinpadRespostaMNU.ProcessarMNU(const RespostaMNU: TACBrAbecsResponse);
begin
  fSTAT := RespostaMNU.STAT;
  fRespostaMNU := RespostaMNU.GetResponseFromTagValue(PP_VALUE);
end;

{ TLibAbecsPinpadRespostaLoadMedia }
constructor TLibAbecsPinpadRespostaLoadMedia.Create(const aTipo: TACBrLibRespostaTipo; const aFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespAbecsPinpadLoadMedia, ATipo, AFormato);
end;

destructor TLibAbecsPinpadRespostaLoadMedia.Destroy;
begin
  inherited Destroy;
end;

procedure TLibAbecsPinpadRespostaLoadMedia.Clear;
begin
  fSTAT := 0;
end;

procedure TLibAbecsPinpadRespostaLoadMedia.ProcessarLoadMedia(const RespostaLoadMedia: TACBrAbecsResponse);
var
  sl: TStringList;
begin
  fSTAT := RespostaLoadMedia.STAT;
  sl := TStringList.Create;
  try
    RespostaLoadMedia.GetResponseFromTagValue(PP_MFNAME, sl);
    sl.Assign(sl);
    fRespostaLoadMedia := Format('%d media file(s)', [sl.Count]);
  finally
    sl.Free;
  end;
end;

{ TLibAbecsPinpadCapabilitiesResposta }
constructor TLibAbecsPinpadCapabilitiesResposta.Create(const aTipo: TACBrLibRespostaTipo; const aFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespAbecsPinpad, ATipo, AFormato);
  Clear;
end;

destructor TLibAbecsPinpadCapabilitiesResposta.Destroy;
begin
  inherited Destroy;
end;

procedure TLibAbecsPinpadCapabilitiesResposta.Clear;
begin
  fSerialNumber := '';
  fPartNumber := '';
  fModel := '';
  fMemory := '';
  fManufacturer := '';
  fSupportContactless := False;
  fDisplayIsGraphic := False;
  fDisplayIsColor := False;
  fSpecificationVersion := 0;
  fDisplayTextModeDimensionsRows := 0;
  fDisplayTextModeDimensionsCols := 0;
  fDisplayGraphicPixelsRows := 0;
  fDisplayGraphicPixelsCols := 0;
  fMediaPNGisSupported := False;
  fMediaJPGisSupported := False;
  fMediaGIFisSupported := False;
end;

procedure TLibAbecsPinpadCapabilitiesResposta.Processar(const AbecsPinpadCapabilities: TACBrAbecsPinPadCapabilities);
begin
  fSerialNumber := AbecsPinpadCapabilities.SerialNumber;
  fPartNumber := AbecsPinpadCapabilities.PartNumber;
  fModel := AbecsPinpadCapabilities.Model;
  fMemory := AbecsPinpadCapabilities.Memory;
  fManufacturer := AbecsPinpadCapabilities.Manufacturer;
  fSupportContactless := AbecsPinpadCapabilities.SupportContactless;
  fDisplayIsGraphic := AbecsPinpadCapabilities.DisplayIsGraphic;
  fDisplayIsColor := AbecsPinpadCapabilities.DisplayIsColor;
  fSpecificationVersion := AbecsPinpadCapabilities.SpecificationVersion;
  fDisplayTextModeDimensionsRows := AbecsPinpadCapabilities.DisplayTextModeDimensions.Rows;
  fDisplayTextModeDimensionsCols := AbecsPinpadCapabilities.DisplayTextModeDimensions.Cols;
  fDisplayGraphicPixelsRows := AbecsPinpadCapabilities.DisplayGraphicPixels.Rows;
  fDisplayGraphicPixelsCols := AbecsPinpadCapabilities.DisplayGraphicPixels.Cols;
  fMediaPNGisSupported := AbecsPinpadCapabilities.MediaPNGisSupported;
  fMediaJPGisSupported := AbecsPinpadCapabilities.MediaJPGisSupported;
  fMediaGIFisSupported := AbecsPinpadCapabilities.MediaGIFisSupported;
end;

end.

