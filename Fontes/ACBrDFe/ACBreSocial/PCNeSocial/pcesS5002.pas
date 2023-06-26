{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Jurisato Junior                           }
{                              Jean Carlo Cantu                                }
{                              Tiago Ravache                                   }
{                              Guilherme Costa                                 }
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

{******************************************************************************
|* Historico
|*
|* 27/10/2015: Jean Carlo Cantu, Tiago Ravache
|*  - Doação do componente para o Projeto ACBr
|* 28/08/2017: Leivio Fontenele - leivio@yahoo.com.br
|*  - Implementação comunicação, envelope, status e retorno do componente com webservice.
******************************************************************************}

{$I ACBr.inc}

unit pcesS5002;

interface

uses
  SysUtils, Classes,
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$IfEnd}
  ACBrBase,
  pcnConversao, pcnLeitor,
  pcesCommon, pcesConversaoeSocial;

type
  TInfoIrrfCollectionItem = class;
  TbasesIrrfCollection = class;
  TbasesIrrfCollectionItem = class;
  TirrfCollection = class;
  TirrfCollectionItem = class;
  TideTrabalhador4 = class;
  TdmDevCollection = class;
  TdmDevCollectionItem = class;
  TinfoirCollection = class;
  TinfoirCollectionItem = class;
  TidePgtoExt = class;
  TEvtIrrfBenef = class;
  TtotApurMenCollection = class;
  TtotApurMenCollectionItem = class;
  TtotApurDiaCollection = class;
  TtotApurDiaCollectionItem = class;

  TS5002 = class(TInterfacedObject, IEventoeSocial)
  private
    FTipoEvento: TTipoEvento;
    FEvtirrfBenef: TEvtirrfBenef;

    function GetXml : string;
    procedure SetXml(const Value: string);
    function GetTipoEvento : TTipoEvento;
  public
    constructor Create;
    destructor Destroy; override;

    function GetEvento : TObject;
    property Xml: String read GetXml write SetXml;
    property TipoEvento: TTipoEvento read GetTipoEvento;
    property EvtirrfBenef: TEvtirrfBenef read FEvtirrfBenef write FEvtirrfBenef;
  end;

  TInfoDep = class(TObject)
  private
    FvrDedDep: Double;
  public
    property vrDedDep: Double read FvrDedDep;
  end;

  TInfoIrrfCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TInfoIrrfCollectionItem;
    procedure SetItem(Index: Integer; Value: TInfoIrrfCollectionItem);
  public
    function Add: TInfoIrrfCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TInfoIrrfCollectionItem;
    property Items[Index: Integer]: TInfoIrrfCollectionItem read GetItem write SetItem;
  end;

  TInfoIrrfCollectionItem = class(TObject)
  private
    FCodCateg: integer;
    FindResBr: String;
    FbasesIrrf: TbasesIrrfCollection;
    Firrf: TirrfCollection;
    FidePgtoExt: TidePgtoExt;

    procedure SetbasesIrrf(const Value: TbasesIrrfCollection);
    procedure Setirrf(const Value: TirrfCollection);
  public
    constructor Create;
    destructor Destroy; override;

    property CodCateg: integer read FCodCateg;
    property indResBr: String read FindResBr;
    property basesIrrf: TbasesIrrfCollection read FbasesIrrf write SetbasesIrrf;
    property irrf: TirrfCollection read Firrf write Setirrf;
    property idePgtoExt: TidePgtoExt read FidePgtoExt write FidePgtoExt;
  end;

  TinfoIRCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TinfoIRCollectionItem;
    procedure SetItem(Index: Integer; Value: TinfoIRCollectionItem);
  public
    function Add: TinfoIRCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TinfoIRCollectionItem;
    property Items[Index: Integer]: TinfoIRCollectionItem read GetItem write SetItem;
  end;

  TinfoIRCollectionItem = class(TObject)
  private
    FtpInfoIR: Integer;
    Fvalor: Double;
  public
    property tpInfoIR: Integer read FtpInfoIR;
    property valor: Double read Fvalor;
  end;

  TdmDevCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TdmDevCollectionItem;
    procedure SetItem(Index: Integer; Value: TdmDevCollectionItem);
  public
    function Add: TdmDevCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TdmDevCollectionItem;
    property Items[Index: Integer]: TdmDevCollectionItem read GetItem write SetItem;
  end;

  TdmDevCollectionItem = class(TObject)
  private
    FperRef    : String;
    FideDmDev  : String;
    FtpPgto    : Integer;
    FdtPgto    : TDateTime;
    FcodCateg  : Integer;
    FinfoIR    : TinfoIRCollection;
    FtotApurMen: TtotApurMenCollection;
    FtotApurDia: TtotApurDiaCollection;

    function getInfoIR(): TInfoIRCollection;
    function getTotApurMen(): TTotApurMenCollection;
    function getTotApurDia(): TTotApurDiaCollection;
  public
    constructor Create;
    destructor Destroy; override;

    function infoIRInst(): Boolean;
    function totApurMenInst(): Boolean;
    function totApurDiaInst(): Boolean;

    property perRef    : String read FperRef write FperRef;
    property ideDmDev  : String read FideDmDev write FideDmDev;
    property tpPgto    : Integer read FtpPgto write FtpPgto;
    property dtPgto    : TDateTime read FdtPgto write FdtPgto;
    property codCateg  : Integer read FcodCateg write FcodCateg;
    property infoIR    : TInfoIRCollection read getInfoIR write FinfoIR;
    property totApurMen: TtotApurMenCollection read getTotApurMen write FtotApurMen;
    property totApurDia: TtotApurDiaCollection read getTotApurDia write FtotApurDia;
  end;

  TideTrabalhador4 = class(TIdeTrabalhador3)
  private
    FcpfBenef: string;
    FdmDev: TdmDevCollection;

    function getDmDev: TDmDevCollection;
  public
    constructor Create;
    destructor Destroy; override;

    function dmDevInst(): boolean;

    property cpfBenef: string read FcpfBenef write FcpfBenef;
    property dmDev: TdmDevCollection read getDmDev write FdmDev;
  end;

  TbasesIrrfCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TbasesIrrfCollectionItem;
    procedure SetItem(Index: Integer; Value: TbasesIrrfCollectionItem);
  public
    function Add: TbasesIrrfCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TbasesIrrfCollectionItem;
    property Items[Index: Integer]: TbasesIrrfCollectionItem read GetItem write SetItem;
  end;

  TbasesIrrfCollectionItem = class(TObject)
  private
    Fvalor: Double;
    FtpValor: Integer;
  public
    property tpValor: Integer read FtpValor;
    property valor: Double read Fvalor;
  end;

  TirrfCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TirrfCollectionItem;
    procedure SetItem(Index: Integer; Value: TirrfCollectionItem);
  public
    function Add: TirrfCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TirrfCollectionItem;
    property Items[Index: Integer]: TirrfCollectionItem read GetItem write SetItem;
  end;

  TirrfCollectionItem = class(TObject)
  private
    FtpCR: string;
    FvrIrrfDesc: Double;
  public
    property tpCR: string read FtpCR;
    property vrIrrfDesc: Double read FvrIrrfDesc;
  end;

  TidePgtoExt = class(TObject)
  private
    FidePais: TidePais;
    FendExt: TendExt;
  public
    constructor Create;
    destructor Destroy; override;

    property idePais: TidePais read FidePais write FidePais;
    property endExt: TendExt read FendExt write FendExt;
  end;

  TTotApurMenCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TTotApurMenCollectionItem;
    procedure SetItem(Index: Integer; Value: TTotApurMenCollectionItem);
  public
    function New: TTotApurMenCollectionItem;
    property Items[Index: Integer]: TTotApurMenCollectionItem read GetItem write SetItem;
  end;

  TTotApurMenCollectionItem = class
  private
   FCRMen: string;
   FVlrCRMen: Double;
   FVlrCRMenSusp: Double;
  public
   property CRMen: string read FCRMen;
   property vlrCRMen: Double read FVlrCRMen;
   property vlrCRMenSusp: Double read FVlrCRMenSusp;
  end;

  TTotApurDiaCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TTotApurDiaCollectionItem;
    procedure SetItem(Index: Integer; Value: TTotApurDiaCollectionItem);
  public
    function New: TTotApurDiaCollectionItem;
    property Items[Index: Integer]: TTotApurDiaCollectionItem read GetItem write SetItem;
  end;

  TTotApurDiaCollectionItem = class
  private
   FPerApurDia: Integer;
   FCRDia: string;
   FVlrCRDia: Double;
   FVlrCRDiaSusp: Double;
  public
   property perApurDia: Integer read FPerApurDia;
   property CRDia: string read FCRDia;
   property vlrCRDia: Double read FVlrCRDia;
   property vlrCRDiaSusp: Double read FVlrCRDiaSusp;
  end;

  TEvtIrrfBenef = class(TObject)
  private
    FLeitor: TLeitor;
    FId: String;
    FXML: String;

    FIdeEvento      : TIdeEvento5;
    FIdeEmpregador  : TIdeEmpregador;
    FIdeTrabalhador : TIdeTrabalhador4;
    FInfoDep        : TInfoDep;
    FInfoIrrf       : TInfoIrrfCollection;
    FVersaoDF       : TVersaoeSocial;

    procedure SetInfoIrrf(const Value: TInfoIrrfCollection);
  public
    constructor Create;
    destructor  Destroy; override;

    function LerXML: boolean;
    function SalvarINI: boolean;

    property IdeEvento      : TIdeEvento5 read FIdeEvento write FIdeEvento;
    property IdeEmpregador  : TIdeEmpregador read FIdeEmpregador write FIdeEmpregador;
    property IdeTrabalhador : TIdeTrabalhador4 read FIdeTrabalhador write FIdeTrabalhador;
    property InfoDep        : TInfoDep read FInfoDep write FInfoDep;
    property InfoIrrf       : TInfoIrrfCollection read FInfoIrrf write SetInfoIrrf;
    property Leitor         : TLeitor read FLeitor write FLeitor;
    property Id             : String read FId;
    property XML            : String read FXML;
    property VersaoDF       : TVersaoeSocial read FVersaoDF write FVersaoDF;
  end;

implementation

uses
  IniFiles,
  ACBrUtil.Base;

{ TS5002 }

constructor TS5002.Create;
begin
  inherited Create;
  FTipoEvento   := teS5002;
  FEvtIrrfBenef := TEvtIrrfBenef.Create;
end;

destructor TS5002.Destroy;
begin
  FEvtIrrfBenef.Free;

  inherited;
end;

function TS5002.GetEvento : TObject;
begin
  Result := self;
end;

function TS5002.GetXml : string;
begin
  Result := FEvtIrrfBenef.XML;
end;

procedure TS5002.SetXml(const Value: string);
begin
  if Value = FEvtIrrfBenef.XML then Exit;

  FEvtIrrfBenef.FXML := Value;
  FEvtIrrfBenef.Leitor.Arquivo := Value;
  FEvtIrrfBenef.LerXML;

end;

function TS5002.GetTipoEvento : TTipoEvento;
begin
  Result := FTipoEvento;
end;

{ TInfoIrrfCollection }

function TInfoIrrfCollection.Add: TInfoIrrfCollectionItem;
begin
  Result := Self.New;
end;

function TInfoIrrfCollection.GetItem(
  Index: Integer): TInfoIrrfCollectionItem;
begin
  Result := TInfoIrrfCollectionItem(inherited Items[Index]);
end;

procedure TInfoIrrfCollection.SetItem(Index: Integer;
  Value: TInfoIrrfCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TInfoIrrfCollection.New: TInfoIrrfCollectionItem;
begin
  Result := TInfoIrrfCollectionItem.Create;
  Self.Add(Result);
end;

{ TInfoIrrfCollectionItem }

constructor TInfoIrrfCollectionItem.Create;
begin
  inherited Create;
  FbasesIrrf  := TbasesIrrfCollection.Create;
  Firrf       := TirrfCollection.Create;
  FidePgtoExt := TidePgtoExt.Create;
end;

destructor TInfoIrrfCollectionItem.Destroy;
begin
  FbasesIrrf.Free;
  Firrf.Free;
  FidePgtoExt.Free;

  inherited;
end;

procedure TInfoIrrfCollectionItem.SetbasesIrrf(const Value: TbasesIrrfCollection);
begin
  FbasesIrrf := Value;
end;

procedure TInfoIrrfCollectionItem.Setirrf(const Value: TirrfCollection);
begin
  Firrf := Value;
end;

{ TbaseIrrfCollection }

function TbasesIrrfCollection.Add: TbasesIrrfCollectionItem;
begin
  Result := Self.New;
end;

function TbasesIrrfCollection.GetItem(
  Index: Integer): TbasesIrrfCollectionItem;
begin
  Result := TbasesIrrfCollectionItem(inherited Items[Index]);
end;

procedure TbasesIrrfCollection.SetItem(Index: Integer;
  Value: TbasesIrrfCollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TDmDevCollection }

function TDmDevCollection.Add: TDmDevCollectionItem;
begin
  Result := Self.New;
end;

function TDmDevCollection.GetItem(Index: Integer): TDmDevCollectionItem;
begin
  Result := TDmDevCollectionItem(inherited Items[Index]);
end;

procedure TDmDevCollection.SetItem(Index: Integer;
  Value: TDmDevCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TDmDevCollection.New: TDmDevCollectionItem;
begin
  Result := TDmDevCollectionItem.Create;
  Self.Add(Result);
end;

{ TDmDevColletionItem }

constructor TDmDevCollectionItem.Create;
begin
  inherited;

  FinfoIR     := nil;
  FtotApurMen := nil;
  FtotApurDia := nil;
end;

destructor TDmDevCollectionItem.Destroy;
begin
  if infoIRInst() then
    FreeAndNil(FinfoIR);
  if totApurMenInst() then
    FreeAndNil(FtotApurMen);
  if totApurDiaInst() then
    FreeAndNil(FtotApurDia);

  inherited;
end;

function TDmDevCollectionItem.infoIRInst(): Boolean;
begin
  Result := Assigned(FinfoIR);
end;

function TDmDevCollectionItem.getInfoIR: TInfoIRCollection;
begin
  if not(Assigned(FinfoIR)) then
    FinfoIR := TinfoIRCollection.Create;
  Result := FinfoIR;
end;

function TDmDevCollectionItem.totApurMenInst(): Boolean;
begin
  Result := Assigned(FtotApurMen);
end;

function TDmDevCollectionItem.getTotApurMen: TtotApurMenCollection;
begin
  if not(Assigned(FtotApurMen)) then
    FtotApurMen := TtotApurMenCollection.Create;
  Result := FtotApurMen;
end;

function TDmDevCollectionItem.totApurDiaInst(): Boolean;
begin
  Result := Assigned(FtotApurDia);
end;

function TDmDevCollectionItem.getTotApurDia: TtotApurDiaCollection;
begin
  if not(Assigned(FtotApurDia)) then
    FtotApurDia := TtotApurDiaCollection.Create;
  Result := FtotApurDia;
end;

{ TinfoIRCollection }

function TinfoIRCollection.Add: TinfoIRCollectionItem;
begin
  Result := Self.New;
end;

function TinfoIRCollection.GetItem(Index: Integer): TinfoIRCollectionItem;
begin
  Result := TinfoIRCollectionItem(inherited Items[Index]);
end;

procedure TinfoIRCollection.SetItem(Index: Integer;
  Value: TinfoIRCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TinfoIRCollection.New: TinfoIRCollectionItem;
begin
  Result := TinfoIRCollectionItem.Create;
  Self.Add(Result);
end;

{ TirrfCollection }

function TirrfCollection.Add: TirrfCollectionItem;
begin
  Result := Self.New;
end;

function TirrfCollection.GetItem(Index: Integer): TirrfCollectionItem;
begin
  Result := TirrfCollectionItem(inherited Items[Index]);
end;

procedure TirrfCollection.SetItem(Index: Integer;
  Value: TirrfCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TirrfCollection.New: TirrfCollectionItem;
begin
  Result := TirrfCollectionItem.Create;
  Self.Add(Result);
end;

{ TidePgtoExt }

constructor TidePgtoExt.Create;
begin
  inherited Create;
  FidePais := TidePais.Create;
  FendExt  := TendExt.Create;
end;

destructor TidePgtoExt.Destroy;
begin
  FidePais.Free;
  FendExt.Free;

  inherited;
end;

{ TIdeTrabalhador4 }

constructor TIdeTrabalhador4.Create;
begin
 inherited Create;

 FdmDev := nil;
end;

destructor TIdeTrabalhador4.Destroy;
begin
 if dmDevInst() = True then
   FreeAndNil(FdmDev);

 inherited;
end;

function TIdeTrabalhador4.getDmDev: TDmDevCollection;
begin
  if not(Assigned(FDmDev)) then
    FDmDev := TDmDevCollection.Create;
  Result := FDmDev;
end;

function TIdeTrabalhador4.dmDevInst(): boolean;
begin
  Result := Assigned(FDmDev);
end;

{ TEvtIrrfBenef }

constructor TEvtIrrfBenef.Create;
begin
  inherited Create;
  FLeitor          := TLeitor.Create;
  FIdeEvento       := TIdeEvento5.Create;
  FIdeEmpregador   := TIdeEmpregador.Create;
  FIdeTrabalhador  := TIdeTrabalhador4.Create;
  FInfoDep         := TInfoDep.Create;
  FInfoIrrf        := TInfoIrrfCollection.Create;
end;

destructor TEvtIrrfBenef.Destroy;
begin
  FLeitor.Free;
  FIdeEvento.Free;
  FIdeEmpregador.Free;
  FIdeTrabalhador.Free;
  FInfoDep.Free;
  FInfoIrrf.Free;

  inherited;
end;

procedure TEvtIrrfBenef.SetInfoIrrf(const Value: TInfoIrrfCollection);
begin
  FInfoIrrf := Value;
end;

function TEvtIrrfBenef.LerXML: boolean;
var
  ok: Boolean;
  i, j: Integer;
  s: String;
begin
  Result := False;
  try
    FXML := Leitor.Arquivo;

    // Capturar a versão do evento
    s := Copy(FXML, Pos('/evt/evtIrrfBenef/', FXML)+18, 16);
    s := Copy(s, 1, Pos('"', s)-1);
    Self.VersaoDF := StrToVersaoeSocialSchemas(s);

    if leitor.rExtrai(1, 'evtIrrfBenef') <> '' then
    begin
      FId := Leitor.rAtributo('Id=');

      if leitor.rExtrai(1, 'ideEvento') <> '' then
      begin
        IdeEvento.nrRecArqBase := leitor.rCampo(tcStr, 'nrRecArqBase');

        if VersaoDF <= ve02_05_00 then
          IdeEvento.IndApuracao  := eSStrToIndApuracao(ok, leitor.rCampo(tcStr, 'IndApuracao'));

        IdeEvento.perApur      := leitor.rCampo(tcStr, 'perApur');
      end;

      if leitor.rExtrai(1, 'ideEmpregador') <> '' then
      begin
        IdeEmpregador.TpInsc := eSStrToTpInscricao(ok, leitor.rCampo(tcStr, 'tpInsc'));
        IdeEmpregador.NrInsc := leitor.rCampo(tcStr, 'nrInsc');
      end;

      if leitor.rExtrai(1, 'ideTrabalhador') <> '' then
      begin
        if VersaoDF <= ve02_05_00 then
          IdeTrabalhador.cpfTrab := leitor.rCampo(tcStr, 'cpfTrab')
        else
          IdeTrabalhador.cpfBenef := leitor.rCampo(tcStr, 'cpfBenef');
      end;

      if VersaoDF <= ve02_05_00 then
      begin
        if leitor.rExtrai(2, 'infoDep') <> '' then
          infoDep.FvrDedDep := leitor.rCampo(tcDe2, 'vrDedDep');

        i := 0;
        while Leitor.rExtrai(2, 'infoIrrf', '', i + 1) <> '' do
        begin
          InfoIrrf.New;
          InfoIrrf.Items[i].FCodCateg := leitor.rCampo(tcInt, 'codCateg');
          InfoIrrf.Items[i].FindResBr := leitor.rCampo(tcStr, 'indResBr');

          j := 0;
          while Leitor.rExtrai(3, 'basesIrrf', '', j + 1) <> '' do
          begin
            InfoIrrf.Items[i].basesIrrf.New;
            InfoIrrf.Items[i].basesIrrf.Items[j].FtpValor := leitor.rCampo(tcInt, 'tpValor');
            InfoIrrf.Items[i].basesIrrf.Items[j].Fvalor   := leitor.rCampo(tcDe2, 'valor');
            inc(j);
          end;

          j := 0;
          while Leitor.rExtrai(3, 'irrf', '', j + 1) <> '' do
          begin
            InfoIrrf.Items[i].irrf.New;
            InfoIrrf.Items[i].irrf.Items[j].FtpCR       := leitor.rCampo(tcStr, 'tpCR');
            InfoIrrf.Items[i].irrf.Items[j].FvrIrrfDesc := leitor.rCampo(tcDe2, 'vrIrrfDesc');
            inc(j);
          end;

          if leitor.rExtrai(3, 'idePgtoExt') <> '' then
          begin
            if leitor.rExtrai(4, 'idePais') <> '' then
            begin
              InfoIrrf.Items[i].idePgtoExt.idePais.codPais  := leitor.rCampo(tcStr, 'codPais');
              InfoIrrf.Items[i].idePgtoExt.idePais.indNIF   := eSStrToIndNIF(ok, leitor.rCampo(tcStr, 'indNIF'));
              InfoIrrf.Items[i].idePgtoExt.idePais.nifBenef := leitor.rCampo(tcStr, 'nifBenef');
            end;

            if leitor.rExtrai(4, 'endExt') <> '' then
            begin
              InfoIrrf.Items[i].idePgtoExt.endExt.dscLograd := leitor.rCampo(tcStr, 'dscLograd');
              InfoIrrf.Items[i].idePgtoExt.endExt.nrLograd  := leitor.rCampo(tcStr, 'nrLograd');
              InfoIrrf.Items[i].idePgtoExt.endExt.complem   := leitor.rCampo(tcStr, 'complem');
              InfoIrrf.Items[i].idePgtoExt.endExt.bairro    := leitor.rCampo(tcStr, 'bairro');
              InfoIrrf.Items[i].idePgtoExt.endExt.nmCid     := leitor.rCampo(tcStr, 'nmCid');
              InfoIrrf.Items[i].idePgtoExt.endExt.codPostal := leitor.rCampo(tcStr, 'codPostal');
            end;
          end;

          inc(i);
        end;
      end
      else
      begin
        i := 0;
        while Leitor.rExtrai(1, 'dmDev', '', i + 1) <> '' do
        begin
          IdeTrabalhador.DmDev.New;
          IdeTrabalhador.DmDev.Items[i].FperRef   := leitor.rCampo(tcStr, 'perRef');
          IdeTrabalhador.DmDev.Items[i].FideDmDev := leitor.rCampo(tcStr, 'ideDmDev');
          IdeTrabalhador.DmDev.Items[i].FtpPgto   := leitor.rCampo(tcInt, 'tpPgto');
          IdeTrabalhador.DmDev.Items[i].FdtPgto   := leitor.rCampo(tcDat, 'dtPgto');
          IdeTrabalhador.DmDev.Items[i].FcodCateg := leitor.rCampo(tcInt, 'codCateg');

          j := 0;
          while Leitor.rExtrai(2, 'infoIR', '', j + 1) <> '' do
          begin
            IdeTrabalhador.DmDev.Items[i].infoIR.New;
            IdeTrabalhador.DmDev.Items[i].infoIR.Items[j].FtpInfoIR := leitor.rCampo(tcInt, 'tpInfoIR');
            IdeTrabalhador.DmDev.Items[i].infoIR.Items[j].Fvalor    := leitor.rCampo(tcDe2, 'valor');

            inc(j);
          end;

          j := 0;
          while Leitor.rExtrai(2, 'totApurMen', '', j + 1) <> '' do
          begin
            IdeTrabalhador.DmDev.Items[i].totApurMen.New;
            IdeTrabalhador.DmDev.Items[i].totApurMen.Items[j].FCRMen        := leitor.rCampo(tcStr, 'CRMen');
            IdeTrabalhador.DmDev.Items[i].totApurMen.Items[j].FvlrCRMen     := leitor.rCampo(tcDe2, 'vlrCRMen');
            IdeTrabalhador.DmDev.Items[i].totApurMen.Items[j].FvlrCRMenSusp := leitor.rCampo(tcDe2, 'vlrCRMenSusp');

            inc(j);
          end;

          j := 0;
          while Leitor.rExtrai(2, 'totApurDia', '', j + 1) <> '' do
          begin
            IdeTrabalhador.DmDev.Items[i].totApurDia.New;
            IdeTrabalhador.DmDev.Items[i].totApurDia.Items[j].FperApurDia   := leitor.rCampo(tcInt, 'perApurDia');
            IdeTrabalhador.DmDev.Items[i].totApurDia.Items[j].FCRDia        := leitor.rCampo(tcStr, 'CRDia');
            IdeTrabalhador.DmDev.Items[i].totApurDia.Items[j].FvlrCRDia     := leitor.rCampo(tcDe2, 'vlrCRDia');
            IdeTrabalhador.DmDev.Items[i].totApurDia.Items[j].FvlrCRDiaSusp := leitor.rCampo(tcDe2, 'vlrCRDiaSusp');

            inc(j);
          end;

          inc(i);
        end;
      end;

      Result := True;
    end;
  except
    Result := False;
  end;
end;

function TEvtIrrfBenef.SalvarINI: boolean;
var
  AIni: TMemIniFile;
  sSecao: String;
  i, j: Integer;
begin
  Result := True;

  AIni := TMemIniFile.Create('');
  try
    with Self do
    begin
      sSecao := 'evtIrrfBenef';
      AIni.WriteString(sSecao, 'Id', Id);

      sSecao := 'ideEvento';
      AIni.WriteString(sSecao, 'nrRecArqBase', IdeEvento.nrRecArqBase);
      AIni.WriteString(sSecao, 'perApur',      IdeEvento.perApur);

      sSecao := 'ideEmpregador';
      AIni.WriteString(sSecao, 'tpInsc', eSTpInscricaoToStr(IdeEmpregador.TpInsc));
      AIni.WriteString(sSecao, 'nrInsc', IdeEmpregador.nrInsc);

      sSecao := 'ideTrabalhador';
      AIni.WriteString(sSecao, 'cpfTrab', ideTrabalhador.cpfTrab);

      sSecao := 'infoDep';
      AIni.WriteFloat(sSecao, 'vrDedDep', infoDep.vrDedDep);

      for i := 0 to infoIrrf.Count -1 do
      begin
        sSecao := 'infoIrrf' + IntToStrZero(I, 1);

        AIni.WriteInteger(sSecao, 'codCateg', infoIrrf.Items[i].CodCateg);
        AIni.WriteString(sSecao, 'indResBr',  infoIrrf.Items[i].indResBr);

        for j := 0 to InfoIrrf.Items[i].basesIrrf.Count -1 do
        begin
          sSecao := 'basesIrrf' + IntToStrZero(I, 1) + IntToStrZero(j, 2);

          AIni.WriteInteger(sSecao, 'tpValor', InfoIrrf.Items[i].basesIrrf.Items[j].tpValor);
          AIni.WriteFloat(sSecao, 'valor',     InfoIrrf.Items[i].basesIrrf.Items[j].valor);
        end;

        for j := 0 to InfoIrrf.Items[i].irrf.Count -1 do
        begin
          sSecao := 'irrf' + IntToStrZero(I, 1) + IntToStrZero(j, 2);

          AIni.WriteString(sSecao, 'tpCR',      InfoIrrf.Items[i].irrf.Items[j].tpCR);
          AIni.WriteFloat(sSecao, 'vrIrrfDesc', InfoIrrf.Items[i].irrf.Items[j].vrIrrfDesc);
        end;

        sSecao := 'idePais' + IntToStrZero(I, 1);

        AIni.WriteString(sSecao, 'codPais',  infoIrrf.Items[i].idePgtoExt.idePais.codPais);
        AIni.WriteString(sSecao, 'indNIF',   eSIndNIFToStr(infoIrrf.Items[i].idePgtoExt.idePais.indNIF));
        AIni.WriteString(sSecao, 'nifBenef', infoIrrf.Items[i].idePgtoExt.idePais.nifBenef);

        sSecao := 'endExt' + IntToStrZero(I, 1);

        AIni.WriteString(sSecao, 'dscLograd', infoIrrf.Items[i].idePgtoExt.endExt.dscLograd);
        AIni.WriteString(sSecao, 'nrLograd',  infoIrrf.Items[i].idePgtoExt.endExt.nrLograd);
        AIni.WriteString(sSecao, 'complem',   infoIrrf.Items[i].idePgtoExt.endExt.complem);
        AIni.WriteString(sSecao, 'bairro',    infoIrrf.Items[i].idePgtoExt.endExt.bairro);
        AIni.WriteString(sSecao, 'nmCid',     infoIrrf.Items[i].idePgtoExt.endExt.nmCid);
        AIni.WriteString(sSecao, 'codPostal', infoIrrf.Items[i].idePgtoExt.endExt.codPostal);
      end;
    end;
  finally
    AIni.Free;
  end;
end;

function TbasesIrrfCollection.New: TbasesIrrfCollectionItem;
begin
  Result := TbasesIrrfCollectionItem.Create;
  Self.Add(Result);
end;

{ TTotApurMenCollection }

function TTotApurMenCollection.GetItem(
  Index: Integer): TTotApurMenCollectionItem;
begin
  Result := TTotApurMenCollectionItem(inherited Items[Index]);
end;

procedure TTotApurMenCollection.SetItem(Index: Integer;
  Value: TTotApurMenCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TTotApurMenCollection.New: TTotApurMenCollectionItem;
begin
  Result := TTotApurMenCollectionItem.Create;
  Self.Add(Result);
end;

{ TTotApurDiaCollection }

function TTotApurDiaCollection.GetItem(
  Index: Integer): TTotApurDiaCollectionItem;
begin
  Result := TTotApurDiaCollectionItem(inherited Items[Index]);
end;

procedure TTotApurDiaCollection.SetItem(Index: Integer;
  Value: TTotApurDiaCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TTotApurDiaCollection.New: TTotApurDiaCollectionItem;
begin
  Result := TTotApurDiaCollectionItem.Create;
  Self.Add(Result);
end;

end.
