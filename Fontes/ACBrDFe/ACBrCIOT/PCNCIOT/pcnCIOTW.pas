
{$I ACBr.inc}

unit pcnCIOTW;

interface

uses
  SysUtils, Classes,
  ACBrUtil,
  pcnAuxiliar, pcnConversao, pcnGerador, pcnConversaoCIOT, pcnCIOT;

type

  TCIOTW = class;
  TGeradorOpcoes = class;

  { TCIOTWClass }

  TCIOTWClass = class
  private
  protected
    FpCIOTW: TCIOTW;

    FGerador: TGerador;
    FOpcoes: TGeradorOpcoes;

    FCIOT: TCIOT;
  public
    constructor Create(ACIOTW: TCIOTW); virtual;
    destructor  Destroy; Override;

    function ObterNomeArquivo: String; virtual;
    function GerarXml: Boolean; virtual;

    property Gerador: TGerador      read FGerador   write FGerador;
    property Opcoes: TGeradorOpcoes read FOpcoes    write FOpcoes;
    property CIOT: TCIOT  read FCIOT write FCIOT;
  end;


  { TCIOTW }

  TCIOTW = class
  private
    FIntegradora: TCIOTIntegradora;
    FCIOTWClass: TCIOTWClass;
    FCIOT: TCIOT;

    procedure SetIntegradora(AIntegradora: TCIOTIntegradora);
  public
    constructor Create(AOwner: TCIOT);
    procedure Clear;
    destructor Destroy; override;

    function GerarXml: Boolean;

    property Integradora: TCIOTIntegradora read FIntegradora write SetIntegradora;
    property CIOTWClass: TCIOTWClass read FCIOTWClass;
    property CIOT: TCIOT read FCIOT write FCIOT;
  end;

  TGeradorOpcoes = class(TPersistent)
  private
    FAjustarTagNro: Boolean;
    FNormatizarMunicipios: Boolean;
    FPathArquivoMunicipios: String;
    FValidarInscricoes: Boolean;
  published
    property AjustarTagNro: Boolean        read FAjustarTagNro         write FAjustarTagNro;
    property NormatizarMunicipios: Boolean read FNormatizarMunicipios  write FNormatizarMunicipios;
    property PathArquivoMunicipios: String read FPathArquivoMunicipios write FPathArquivoMunicipios;
    property ValidarInscricoes: Boolean    read FValidarInscricoes     write FValidarInscricoes;
  end;

implementation

uses
  ACBrDFeException,
  pcnCIOTW_eFrete, pcnCIOTW_REPOM;


{ TCIOTWClass }

constructor TCIOTWClass.Create(ACIOTW: TCIOTW);
begin
  FpCIOTW := ACIOTW;

  FGerador := TGerador.Create;

  FOpcoes := TGeradorOpcoes.Create;
  FOpcoes.FAjustarTagNro        := True;
  FOpcoes.FNormatizarMunicipios := False;
  FOpcoes.FValidarInscricoes    := False;
end;

destructor TCIOTWClass.Destroy;
begin
  FOpcoes.Free;
  FGerador.Free;

  inherited Destroy;
end;

function TCIOTWClass.GerarXml: Boolean;
begin
  Result := False;
  raise EACBrDFeException.Create(ClassName + '.GerarXml, não implementado');
end;

function TCIOTWClass.ObterNomeArquivo: String;
begin
  Result := '';
  raise EACBrDFeException.Create(ClassName + '.ObterNomeArquivo, não implementado');
end;

{ TCIOTW }

procedure TCIOTW.Clear;
begin
  FIntegradora := iNone;

  if Assigned(FCIOTWClass) then
    FCIOTWClass.Free;

  FCIOTWClass := TCIOTWClass.Create(Self);
end;

constructor TCIOTW.Create(AOwner: TCIOT);
begin
  inherited Create;

  FCIOT := AOwner;

  Clear;
end;

destructor TCIOTW.Destroy;
begin
  if Assigned(FCIOTWClass) then
    FreeAndNil(FCIOTWClass);

  inherited Destroy;
end;

function TCIOTW.GerarXml: Boolean;
begin
  Result := FCIOTWClass.GerarXml;
end;

procedure TCIOTW.SetIntegradora(AIntegradora: TCIOTIntegradora);
begin
  if AIntegradora = FIntegradora then
    exit;

  if Assigned(FCIOTWClass) then
    FreeAndNil(FCIOTWClass);

  case AIntegradora of
    ieFrete: FCIOTWClass := TCIOTW_eFrete.Create(Self);
    iREPOM:  FCIOTWClass := TCIOTW_REPOM.Create(Self);
  else
    FCIOTWClass := TCIOTWClass.Create(Self);
  end;

  FCIOTWClass.FCIOT := FCIOT;

  FIntegradora := AIntegradora;
end;

end.

