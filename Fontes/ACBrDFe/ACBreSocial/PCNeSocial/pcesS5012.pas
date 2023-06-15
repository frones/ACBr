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

unit pcesS5012;

interface

uses
  SysUtils, 
  Classes,
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
		System.Generics.Collections, 
		System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
		System.Contnrs,
  {$IfEnd}
  ACBrBase, 
	pcnConversao, 
	pcnLeitor,
  pcesCommon, 
	pcesConversaoeSocial;

type
  TS5012 = class;
  TInfoIRRF = class;
  TInfoCRContribCollection = class;
  TInfoCRContribCollectionItem = class;
  TInfoCRMenCollection = class;
  TInfoCRMenCollectionItem = class;
  TInfoCRDiaCollection = class;
  TInfoCRDiaCollectionItem = class;

  TEvtIrrf = class;

  TS5012 = class(TInterfacedObject, IEventoeSocial)
  private
    FTipoEvento: TTipoEvento;
    FEvtIrrf: TEvtIrrf;

    function GetXml : string;
    procedure SetXml(const Value: string);
    function GetTipoEvento : TTipoEvento;
  public
    constructor Create;
    destructor Destroy; override;

    function GetEvento : TObject;
    property Xml: String read GetXml write SetXml;
    property TipoEvento: TTipoEvento read GetTipoEvento;
    property EvtIrrf: TEvtIrrf read FEvtIrrf write FEvtIrrf;

  end;

  TInfoCRContribCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TInfoCRContribCollectionItem;
    procedure SetItem(Index: Integer; Value: TInfoCRContribCollectionItem);
  public
    function Add: TInfoCRContribCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TInfoCRContribCollectionItem;
    property Items[Index: Integer]: TInfoCRContribCollectionItem read GetItem write SetItem;
  end;

  TInfoCRContribCollectionItem = class(TObject)
  private
    FtpCR: String;
    FvrCR: Double;
  public
    property tpCR: String read FtpCR;
    property vrCR: Double read FvrCR;
  end;

  TInfoIRRF = class(TObject)
  private
    FnrRecArqBase: String;
    FindExistInfo: Integer;
    FInfoCRContrib: TInfoCRContribCollection;
    FInfoCRMen: TInfoCRMenCollection;
    FInfoCRDia: TInfoCRDiaCollection;

    function getInfoCRMen(): TInfoCRMenCollection;
    function getInfoCRDia(): TInfoCRDiaCollection;
  public
    constructor Create;
    destructor Destroy; override;

    function InfoCRMenInst(): Boolean;
    function InfoCRDiaInst(): Boolean;

    property nrRecArqBase: String read FnrRecArqBase;
    property indExistInfo: Integer read FindExistInfo;
    property InfoCRContrib: TInfoCRContribCollection read FInfoCRContrib;
    property InfoCRMen: TInfoCRMenCollection read getInfoCRMen;
    property InfoCRDia: TInfoCRDiaCollection read getInfoCRDia;
  end;

  TEvtIrrf = class(TObject)
  private
    FLeitor: TLeitor;
    FId: String;
    FXML: String;
    FVersaoDF: TVersaoeSocial;
    FIdeEvento: TIdeEvento5;
    FIdeEmpregador: TIdeEmpregador;
    FInfoIRRF: TInfoIRRF;
  public
    constructor Create;
    destructor  Destroy; override;

    function LerXML: Boolean;
    function SalvarINI: boolean;
    property IdeEvento: TIdeEvento5 read FIdeEvento write FIdeEvento;
    property IdeEmpregador: TIdeEmpregador read FIdeEmpregador write FIdeEmpregador;
    property InfoIRRF: TInfoIRRF read FInfoIRRF write FInfoIRRF;
    property Leitor: TLeitor read FLeitor write FLeitor;
    property Id: String read FId;
    property XML: String read FXML;
    property VersaoDF: TVersaoeSocial read FVersaoDF write FVersaoDF;
  end;

  TInfoCRMenCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TInfoCRMenCollectionItem;
    procedure SetItem(Index: Integer; Value: TInfoCRMenCollectionItem);
  public
    function New: TInfoCRMenCollectionItem;
    property Items[Index: Integer]: TInfoCRMenCollectionItem read GetItem write SetItem;
  end;

  TInfoCRMenCollectionItem = class
  private
   FCRMen: string;
   FVrCRMen: Double;
   FVrCRMenSusp: Double;
  public
   property CRMen: string read FCRMen;
   property vrCRMen: Double read FVrCRMen;
   property vrCRMenSusp: Double read FVrCRMenSusp;
  end;

  TInfoCRDiaCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TInfoCRDiaCollectionItem;
    procedure SetItem(Index: Integer; Value: TInfoCRDiaCollectionItem);
  public
    function New: TInfoCRDiaCollectionItem;
    property Items[Index: Integer]: TInfoCRDiaCollectionItem read GetItem write SetItem;
  end;

  TInfoCRDiaCollectionItem = class
  private
   FPerApurDia: Integer;
   FCRDia: string;
   FVrCRDia: Double;
   FVrCRDiaSusp: Double;
  public
   property perApurDia: Integer read FPerApurDia;
   property CRDia: string read FCRDia;
   property vrCRDia: Double read FVrCRDia;
   property vrCRDiaSusp: Double read FVrCRDiaSusp;
  end;

implementation

uses
  IniFiles,
  ACBrUtil.Base;

{ TS5012 }

constructor TS5012.Create;
begin
  inherited Create;
  FTipoEvento := teS5012;
  FEvtIrrf    := TEvtIrrf.Create;
end;

destructor TS5012.Destroy;
begin
  FEvtIrrf.Free;

  inherited;
end;

function TS5012.GetEvento : TObject;
begin
  Result := Self;
end;

function TS5012.GetXml : string;
begin
  Result := FEvtIrrf.XML;
end;

procedure TS5012.SetXml(const Value: string);
begin
  if Value = FEvtIrrf.XML then Exit;

  FEvtIrrf.FXML := Value;
  FEvtIrrf.Leitor.Arquivo := Value;
  FEvtIrrf.LerXML;

end;

function TS5012.GetTipoEvento : TTipoEvento;
begin
  Result := FTipoEvento;
end;

{ TEvtIrrf }

constructor TEvtIrrf.Create;
begin
  inherited Create;
  FLeitor        := TLeitor.Create;
  FIdeEvento     := TIdeEvento5.Create;
  FIdeEmpregador := TIdeEmpregador.Create;
  FInfoIRRF      := TInfoIRRF.Create;
end;

destructor TEvtIrrf.Destroy;
begin
  FLeitor.Free;
  FIdeEvento.Free;
  FIdeEmpregador.Free;
  FInfoIRRF.Free;

  inherited;
end;

function TEvtIrrf.LerXML: Boolean;
var
  ok: Boolean;
  i: Integer;
  s: String;
begin
  Result := False;
  try
    FXML := Leitor.Arquivo;

    // Capturar a versão do evento
    s := Copy(FXML, Pos('/evt/evtIrrf/', FXML)+13, 12);
    Self.VersaoDF := StrToVersaoeSocialSchemas(s);

    if leitor.rExtrai(1, 'evtIrrf') <> '' then
    begin
      FId := Leitor.rAtributo('Id=');

      if leitor.rExtrai(2, 'ideEvento') <> '' then
        IdeEvento.perApur := leitor.rCampo(tcStr, 'perApur');

      if leitor.rExtrai(2, 'ideEmpregador') <> '' then
      begin
        IdeEmpregador.TpInsc := eSStrToTpInscricao(ok, leitor.rCampo(tcStr, 'tpInsc'));
        IdeEmpregador.NrInsc := leitor.rCampo(tcStr, 'nrInsc');
      end;

      if leitor.rExtrai(2, 'infoIRRF') <> '' then
      begin
        infoIRRF.FnrRecArqBase := leitor.rCampo(tcStr, 'nrRecArqBase');
        infoIRRF.FindExistInfo := leitor.rCampo(tcInt, 'indExistInfo');

        if VersaoDF <= ve02_05_00 then
        begin
          i := 0;
          while Leitor.rExtrai(3, 'infoCRContrib', '', i + 1) <> '' do
          begin
            infoIRRF.infoCRContrib.New;
            infoIRRF.infoCRContrib.Items[i].FtpCR := leitor.rCampo(tcStr, 'tpCR');
            infoIRRF.infoCRContrib.Items[i].FvrCR := leitor.rCampo(tcDe2, 'vrCR');
            inc(i);
          end;
        end
        else if VersaoDF >= veS01_01_00 then
        begin
          i := 0;
          while Leitor.rExtrai(3, 'infoCRMen', '', i + 1) <> '' do
          begin
            InfoIRRF.InfoCRMen.New;
            InfoIRRF.InfoCRMen.Items[i].FCRMen       := leitor.rCampo(tcStr, 'CRMen');
            InfoIRRF.InfoCRMen.Items[i].FvrCRMen     := leitor.rCampo(tcDe2, 'vrCRMen');
            InfoIRRF.InfoCRMen.Items[i].FvrCRMenSusp := leitor.rCampo(tcDe2, 'vrCRMenSusp');

            inc(i);
          end;

          i := 0;
          while Leitor.rExtrai(3, 'infoCRDia', '', i + 1) <> '' do
          begin
            InfoIRRF.InfoCRDia.New;
            InfoIRRF.InfoCRDia.Items[i].FperApurDia  := leitor.rCampo(tcInt, 'perApurDia');
            InfoIRRF.InfoCRDia.Items[i].FCRDia       := leitor.rCampo(tcStr, 'CRDia');
            InfoIRRF.InfoCRDia.Items[i].FvrCRDia     := leitor.rCampo(tcDe2, 'vrCRDia');
            InfoIRRF.InfoCRDia.Items[i].FvrCRDiaSusp := leitor.rCampo(tcDe2, 'vrCRDiaSusp');

            inc(i);
          end;
        end;
      end;
    end;

    Result := True;
  except
    Result := False;
  end;
end;

function TEvtIrrf.SalvarINI: boolean;
var
  AIni: TMemIniFile;
  sSecao: String;
  i: Integer;
begin
  Result := True;

  AIni := TMemIniFile.Create('');
  try
    with Self do
    begin
      sSecao := 'evtIrrf';
      AIni.WriteString(sSecao, 'Id', Id);

      sSecao := 'ideEvento';
      AIni.WriteString(sSecao, 'perApur', IdeEvento.perApur);

      sSecao := 'ideEmpregador';
      AIni.WriteString(sSecao, 'tpInsc', eSTpInscricaoToStr(IdeEmpregador.TpInsc));
      AIni.WriteString(sSecao, 'nrInsc', IdeEmpregador.nrInsc);

      sSecao := 'infoIRRF';
      AIni.WriteString(sSecao, 'nrRecArqBase',  infoIRRF.nrRecArqBase);
      AIni.WriteInteger(sSecao, 'indExistInfo', infoIRRF.indExistInfo);

      for i := 0 to infoIRRF.InfoCRContrib.Count -1 do
      begin
        sSecao := 'InfoCRContrib' + IntToStrZero(I, 1);

        with infoIRRF.InfoCRContrib.Items[i] do
        begin
          AIni.WriteString(sSecao, 'tpCR', tpCR);
          AIni.WriteFloat(sSecao, 'vrCR',  vrCR);
        end;
      end;
    end;
  finally
    AIni.Free;
  end;
end;

{ TInfoCRContribCollection }

function TInfoCRContribCollection.Add: TInfoCRContribCollectionItem;
begin
  Result := Self.New;
end;

function TInfoCRContribCollection.GetItem(
  Index: Integer): TInfoCRContribCollectionItem;
begin
  Result := TInfoCRContribCollectionItem(inherited Items[Index]);
end;

procedure TInfoCRContribCollection.SetItem(Index: Integer;
  Value: TInfoCRContribCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TInfoCRContribCollection.New: TInfoCRContribCollectionItem;
begin
  Result := TInfoCRContribCollectionItem.Create;
  Self.Add(Result);
end;

{ TInfoIRRF }

constructor TInfoIRRF.Create;
begin
  inherited Create;

  FInfoCRContrib := TInfoCRContribCollection.Create;
  FInfoCRMen     := nil;
  FInfoCRDia     := nil;
end;

destructor TInfoIRRF.Destroy;
begin
  FInfoCRContrib.Free;

  if InfoCRMenInst() then
    FreeAndNil(FInfoCRMen);
  if InfoCRDiaInst() then
    FreeAndNil(FInfoCRDia);

  inherited;
end;

function TInfoIRRF.InfoCRMenInst(): Boolean;
begin
  Result := Assigned(FInfoCRMen);
end;

function TInfoIRRF.getInfoCRMen: TInfoCRMenCollection;
begin
  if not(Assigned(FInfoCRMen)) then
    FInfoCRMen := TInfoCRMenCollection.Create;
  Result := FInfoCRMen;
end;

function TInfoIRRF.InfoCRDiaInst(): Boolean;
begin
  Result := Assigned(FInfoCRDia);
end;

function TInfoIRRF.getInfoCRDia: TInfoCRDiaCollection;
begin
  if not(Assigned(FInfoCRDia)) then
    FInfoCRDia := TInfoCRDiaCollection.Create;
  Result := FInfoCRDia;
end;

{ TInfoCRMenCollection }

function TInfoCRMenCollection.GetItem(
  Index: Integer): TInfoCRMenCollectionItem;
begin
  Result := TInfoCRMenCollectionItem(inherited Items[Index]);
end;

procedure TInfoCRMenCollection.SetItem(Index: Integer; Value: TInfoCRMenCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TInfoCRMenCollection.New: TInfoCRMenCollectionItem;
begin
  Result := TInfoCRMenCollectionItem.Create;
  Self.Add(Result);
end;

{ TInfoCRDiaCollection }

function TInfoCRDiaCollection.GetItem(
  Index: Integer): TInfoCRDiaCollectionItem;
begin
  Result := TInfoCRDiaCollectionItem(inherited Items[Index]);
end;

procedure TInfoCRDiaCollection.SetItem(Index: Integer;
  Value: TInfoCRDiaCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TInfoCRDiaCollection.New: TInfoCRDiaCollectionItem;
begin
  Result := TInfoCRDiaCollectionItem.Create;
  Self.Add(Result);
end;

end.
