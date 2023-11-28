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

unit pcesS5501;

interface

uses
  SysUtils, Classes,
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$IfEnd}
  ACBrBase, pcnConversao, pcnLeitor,
  pcesCommon, pcesConversaoeSocial, Dialogs;

type
  TS5501 = class;
  TEvtTribProcTrab = class;
  TIdeProc = class;
  TInfoTributosCollection = class;
  TInfoTributosCollectionItem = class;
  TInfoCRContribCollection = class;
  TInfoCRContribCollectionItem = class;
  TInfoCRIRRFCollection = class;
  TInfoCRIRRFCollectionItem = class;

  TS5501 = class(TInterfacedObject, IEventoeSocial)
  private
    FTipoEvento: TTipoEvento;
    FEvtTribProcTrab: TEvtTribProcTrab;

    function GetXml : string;
    procedure SetXml(const Value: string);
    function GetTipoEvento : TTipoEvento;
  public
    constructor Create;
    destructor Destroy; override;

    function GetEvento : TObject;
    property Xml: String read GetXml write SetXml;
    property TipoEvento: TTipoEvento read GetTipoEvento;
    property evtTribProcTrab: TEvtTribProcTrab read FEvtTribProcTrab write FEvtTribProcTrab;
  end;

  TEvtTribProcTrab = class(TObject)
  private
    FLeitor: TLeitor;
    FId: string;
    FXML: string;
    FVersaoDF: TVersaoeSocial;
    FIdeEvento: TIdeEvento5;
    FIdeEmpregador: TIdeEmpregador;
    FIdeProc: TIdeProc;
  public
    constructor Create;
    destructor  Destroy; override;

    function LerXML: Boolean;
    function SalvarINI: boolean;
    property ideEvento: TIdeEvento5 read FIdeEvento write FIdeEvento;
    property ideEmpregador: TIdeEmpregador read FIdeEmpregador write FIdeEmpregador;
    property ideProc: TIdeProc read FIdeProc write FIdeProc;
    property Leitor: TLeitor read FLeitor write FLeitor;
    property id: String read FId;
    property XML: String read FXML;
    property VersaoDF: TVersaoeSocial read FVersaoDF write FVersaoDF;
  end;

  TIdeProc = class(TObject)
  private
    FNrProcTrab: string;
    FPerApur: string;
    FInfoTributos: TInfoTributosCollection;
    FinfoCRIRRF: TInfoCRIRRFCollection;

    function getInfoTributos(): TInfoTributosCollection;
    function getInfoCRIRRF(): TInfoCRIRRFCollection;
  public
    constructor Create;
    destructor Destroy; override;

    function instInfoTributos(): boolean;
    function instInfoCRIRRF(): boolean;

    property nrProcTrab: string read FNrProcTrab write FNrProcTrab;
    property perApur: string read FPerApur write FPerApur;
    property infoTributos: TInfoTributosCollection read getInfoTributos write FInfoTributos;
    property infoCRIRRF: TInfoCRIRRFCollection read getInfoCRIRRF write FinfoCRIRRF;
  end;

  TInfoTributosCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TInfoTributosCollectionItem;
    procedure SetItem(Index: Integer; Value: TInfoTributosCollectionItem);
  public
    function New: TInfoTributosCollectionItem;
    property Items[Index: Integer]: TInfoTributosCollectionItem read GetItem write SetItem;
  end;

  TInfoTributosCollectionItem = class(TObject)
  private
    FPerRef: string;
    FinfoCRContrib: TInfoCRContribCollection;

    function getInfoCRContrib(): TInfoCRContribCollection;
  public
    constructor Create;
    destructor  Destroy; override;

    function instInfoCRContrib(): boolean;

    property perRef: string read FPerRef write FPerRef;
    property infoCRContrib: TInfoCRContribCollection read getInfoCRContrib write FinfoCRContrib;
 end;

  TInfoCRContribCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TInfoCRContribCollectionItem;
    procedure SetItem(Index: Integer; Value: TInfoCRContribCollectionItem);
  public
    function New: TInfoCRContribCollectionItem;
    property Items[Index: Integer]: TInfoCRContribCollectionItem read GetItem write SetItem; default;
  end;

  TInfoCRContribCollectionItem = class(TObject)
  private
    FtpCR: string;
    FvrCR: double;
  public
    property tpCR: string read FtpCR write FtpCR;
    property vrCR: double read FvrCR write FvrCR;
  end;

  TInfoCRIRRFCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TInfoCRIRRFCollectionItem;
    procedure SetItem(Index: Integer; Value: TInfoCRIRRFCollectionItem);
  public
    function New: TInfoCRIRRFCollectionItem;
    property Items[Index: Integer]: TInfoCRIRRFCollectionItem read GetItem write SetItem; default;
  end;

  TInfoCRIRRFCollectionItem = class(TObject)
  private
    FtpCR: string;
    FvrCR: double;
  public
    property tpCR: string read FtpCR write FtpCR;
    property vrCR: double read FvrCR write FvrCR;
  end;

implementation

uses
  IniFiles,
  ACBrUtil.Base;

{ TS5501 }

constructor TS5501.Create;
begin
  inherited Create;

  FTipoEvento      := teS5501;
  FEvtTribProcTrab := TEvtTribProcTrab.Create;
end;

destructor TS5501.Destroy;
begin
  FEvtTribProcTrab.Free;

  inherited;
end;

function TS5501.GetEvento: TObject;
begin
  Result := Self;
end;

function TS5501.GetXml: string;
begin
  Result := FEvtTribProcTrab.XML;
end;

procedure TS5501.SetXml(const Value: string);
begin
  if Value = FEvtTribProcTrab.XML then
    exit;

  FEvtTribProcTrab.FXML := Value;
  FEvtTribProcTrab.Leitor.Arquivo := Value;
  FEvtTribProcTrab.LerXML;
end;

function TS5501.GetTipoEvento: TTipoEvento;
begin
  Result := FTipoEvento;
end;

{ TIdeProc }

constructor TIdeProc.Create;
begin
  inherited Create;

  FInfoTributos := nil;
  FInfoCRIRRF   := nil;
end;

destructor TIdeProc.Destroy;
begin
  if instInfoTributos() then
    FreeAndNil(FInfoTributos);
  if instInfoCRIRRF() then
    FreeAndNil(FInfoCRIRRF);

  inherited;
end;

function TIdeProc.getInfoTributos(): TInfoTributosCollection;
begin
  if not Assigned(FInfoTributos) then
    FInfoTributos := TInfoTributosCollection.Create;
  Result := FInfoTributos;
end;

function TIdeProc.getInfoCRIRRF(): TInfoCRIRRFCollection;
begin
  if not Assigned(FInfoCRIRRF) then
    FInfoCRIRRF := TInfoCRIRRFCollection.Create;
  Result := FInfoCRIRRF;
end;

function TIdeProc.instInfoTributos(): boolean;
begin
  Result := Assigned(FInfoTributos);
end;

function TIdeProc.instInfoCRIRRF(): boolean;
begin
  Result := Assigned(FInfoCRIRRF);
end;

{ TInfoTributosCollection }

function TInfoTributosCollection.GetItem(Index: Integer): TInfoTributosCollectionItem;
begin
  Result := TInfoTributosCollectionItem(inherited Items[Index]);
end;

procedure TInfoTributosCollection.SetItem(Index: Integer;Value: TInfoTributosCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TInfoTributosCollection.New: TInfoTributosCollectionItem;
begin
  Result := TInfoTributosCollectionItem.Create;
  Self.Add(Result);
end;

{ TInfoTributosCollectionItem }

constructor TInfoTributosCollectionItem.Create;
begin
  inherited Create;

  FinfoCRContrib := nil;
end;

destructor TInfoTributosCollectionItem.Destroy;
begin
  if instInfoCRContrib() then
    FreeAndNil(FinfoCRContrib);

  inherited;
end;

function TInfoTributosCollectionItem.getInfoCRContrib(): TInfoCRContribCollection;
begin
  if not Assigned(FinfoCRContrib) then
    FinfoCRContrib := TInfoCRContribCollection.Create;
  Result := FinfoCRContrib;
end;

function TInfoTributosCollectionItem.instInfoCRContrib(): boolean;
begin
  Result := Assigned(FInfoCRContrib);
end;

{ TInfoCRContribCollection }

function TInfoCRContribCollection.GetItem(Index: Integer): TInfoCRContribCollectionItem;
begin
  Result := TInfoCRContribCollectionItem(inherited Items[Index]);
end;

procedure TInfoCRContribCollection.SetItem(Index: Integer; Value: TInfoCRContribCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TInfoCRContribCollection.New: TInfoCRContribCollectionItem;
begin
  Result := TInfoCRContribCollectionItem.Create;
  Self.Add(Result);
end;

{ TInfoCRIRRFCollection }

function TInfoCRIRRFCollection.GetItem(Index: Integer): TInfoCRIRRFCollectionItem;
begin
  Result := TInfoCRIRRFCollectionItem(inherited Items[Index]);
end;

procedure TInfoCRIRRFCollection.SetItem(Index: Integer; Value: TInfoCRIRRFCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TInfoCRIRRFCollection.New: TInfoCRIRRFCollectionItem;
begin
  Result := TInfoCRIRRFCollectionItem.Create;
  Self.Add(Result);
end;

{ TEvtTribProcTrab }

constructor TEvtTribProcTrab.Create;
begin
  inherited Create;

  FLeitor        := TLeitor.Create;
  FIdeEvento     := TIdeEvento5.Create;
  FIdeEmpregador := TIdeEmpregador.Create;
  FIdeProc       := TIdeProc.Create;
end;

destructor TEvtTribProcTrab.Destroy;
begin
  FLeitor.Free;
  FIdeEvento.Free;
  FIdeEmpregador.Free;
  FIdeProc.Free;

  inherited;
end;

function TEvtTribProcTrab.LerXML: Boolean;
var
  ok: Boolean;
  i, j: Integer;
begin
  Result := False;
  try
    FXML := Leitor.Arquivo;

    if leitor.rExtrai(1, 'evtTribProcTrab') <> '' then
    begin
      FId := Leitor.rAtributo('Id=');

      if leitor.rExtrai(2, 'ideEvento') <> '' then
        IdeEvento.NrRecArqBase := leitor.rCampo(tcStr, 'nrRecArqBase');

      if leitor.rExtrai(2, 'ideEmpregador') <> '' then
      begin
        IdeEmpregador.TpInsc := eSStrToTpInscricao(ok, leitor.rCampo(tcStr, 'tpInsc'));
        IdeEmpregador.NrInsc := leitor.rCampo(tcStr, 'nrInsc');
      end;

      if leitor.rExtrai(2, 'ideProc') <> '' then
      begin
        IdeProc.FNrProcTrab := leitor.rCampo(tcStr, 'nrProcTrab');
        IdeProc.FPerApur    := leitor.rCampo(tcStr, 'perApur');

        i := 0;
        while Leitor.rExtrai(3, 'infoTributos', '', i +1) <> '' do
        begin
          IdeProc.InfoTributos.New;
          IdeProc.InfoTributos.Items[i].perRef := leitor.rCampo(tcStr, 'perRef');

          j := 0;
          while Leitor.rExtrai(4, 'infoCRContrib', '', j + 1) <> '' do
          begin
            IdeProc.InfoTributos.Items[i].InfoCRContrib.New;
            IdeProc.InfoTributos.Items[i].InfoCRContrib.Items[j].tpCR := leitor.rCampo(tcStr, 'tpCR');
            IdeProc.InfoTributos.Items[i].InfoCRContrib.Items[j].vrCR := leitor.rCampo(tcDe2, 'vrCR');

            inc(j);
          end;
          
          inc(i);
        end;

        i := 0;
        while Leitor.rExtrai(3, 'infoCRIRRF', '', i + 1) <> '' do
        begin
          IdeProc.infoCRIRRF.New;
          IdeProc.infoCRIRRF.Items[i].tpCR := leitor.rCampo(tcStr, 'tpCR');
          IdeProc.infoCRIRRF.Items[i].vrCR := leitor.rCampo(tcDe2, 'vrCR');

          inc(i);
        end;
      end;
    end;

    Result := True;
  except
    Result := False;
  end;
end;

function TEvtTribProcTrab.SalvarINI: boolean;
var
  AIni: TMemIniFile;
  sSecao: string;
  i, j: Integer;
begin
  Result := True;

  AIni := TMemIniFile.Create('');
  try
    with Self do
    begin
      sSecao := 'EvtTribProcTrab';
      AIni.WriteString(sSecao, 'Id', Id);

      sSecao := 'ideEvento';
      AIni.WriteString(sSecao, 'nrRecArqBase', IdeEvento.nrRecArqBase);

      sSecao := 'ideEmpregador';
      AIni.WriteString(sSecao, 'tpInsc', eSTpInscricaoToStr(IdeEmpregador.TpInsc));
      AIni.WriteString(sSecao, 'nrInsc', IdeEmpregador.nrInsc);

      sSecao := 'ideProc';
      AIni.WriteString(sSecao, 'nrProcTrab',  ideProc.nrProcTrab);
      AIni.WriteString(sSecao, 'perApur', ideProc.perApur);

      for i := 0 to ideProc.InfoTributos.Count -1 do
      begin
        sSecao := 'infoTributos' + IntToStrZero(i, 1);

        AIni.WriteString(sSecao, 'perRef',  ideProc.infoTributos.Items[i].perRef);

        for j := 0 to ideProc.InfoTributos.Items[i].InfoCRContrib.Count -1 do
        begin
          sSecao := 'infoCRContrib' + IntToStrZero(j, 1);

          with ideProc.InfoTributos.Items[i].infoCRContrib.Items[j] do
          begin
            AIni.WriteString(sSecao, 'tpCR', tpCR);
            AIni.WriteFloat(sSecao, 'vrCR',  vrCR);
          end;
        end;
      end;

      for i := 0 to ideProc.InfoCRIRRF.Count -1 do
      begin
        sSecao := 'infoCRIRRF' + IntToStrZero(i, 1);

        with ideProc.InfoCRIRRF.Items[i] do
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

end.