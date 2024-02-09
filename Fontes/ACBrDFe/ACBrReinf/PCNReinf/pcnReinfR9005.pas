{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Renato Tanchela Rubinho                         }
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

unit pcnReinfR9005;

interface

uses
  Classes, Sysutils, Contnrs, pcnLeitor,
  pcnCommonReinf, pcnConversaoReinf;

type

  TR9005 = class;
  TevtRet = class;
  TInfoRecEv = class;
  TinfoTotal = class;
  TideEstab = class;
  TtotApurMenCollection = class;
  TtotApurMenCollectionItem = class;
  TtotApurTribMen = class;
  TtotApurQuiCollection = class;
  TtotApurQuiCollectionItem = class;
  TtotApurTribQui = class;
  TtotApurDecCollection = class;
  TtotApurDecCollectionItem = class;
  TtotApurTribDec = class;
  TtotApurSemCollection = class;
  TtotApurSemCollectionItem = class;
  TtotApurTribSem = class;
  TtotApurDiaCollection = class;
  TtotApurDiaCollectionItem = class;
  TtotApurTribDia = class;

  { TR9005 }
  
  TR9005 = class(TInterfacedObject, IEventoReinf)
  private
    FTipoEvento: TTipoEvento;
    FevtRet: TevtRet;

    function GetXml: string;
    procedure SetXml(const Value: string);
    function GetTipoEvento: TTipoEvento;
  public
    constructor Create;
    destructor Destroy; override;

    function GetEvento: TObject;

    property Xml: String read GetXml write SetXml;
    property TipoEvento: TTipoEvento read GetTipoEvento;
    property evtRet: TevtRet read FevtRet write FevtRet;
  end;

  { TevtRet }

  TevtRet = class(TObject)
  private
    FLeitor: TLeitor;
    FId: String;
    FXML: String;

    FIdeEvento: TIdeEvento1;
    FIdeContrib: TIdeContrib;
    FIdeStatus: TIdeStatus;
    FInfoRecEv: TInfoRecEv;
    FInfoTotal: TInfoTotal;
  public
    constructor Create;
    destructor  Destroy; override;

    function LerXML: boolean;
    function SalvarINI: boolean;

    property IdeEvento: TIdeEvento1 read FIdeEvento write FIdeEvento;
    property IdeContrib: TIdeContrib read FIdeContrib write FIdeContrib;
    property IdeStatus: TIdeStatus read FIdeStatus write FIdeStatus;
    property InfoRecEv: TInfoRecEv read FInfoRecEv write FInfoRecEv;
    property InfoTotal: TInfoTotal read FInfoTotal write FInfoTotal;

    property Leitor: TLeitor read FLeitor write FLeitor;
    property Id: String      read FId;
    property XML: String     read FXML;
  end;

  { TInfoRecEv }

  TInfoRecEv = class(TObject)
  private
    FnrRecArqBase: String;
    FnrProtEntr: String;
    FnrProtLote: String;
    FdhProcess: TDateTime;
    FdhRecepcao: TDateTime;
    FtpEv: String;
    FidEv: String;
    Fhash: String;
    FfechRet: TtpFechRet;
  public
    property nrRecArqBase: String read FnrRecArqBase write FnrRecArqBase;
    property nrProtEntr: String read FnrProtEntr write FnrProtEntr;
    property nrProtLote: String read FnrProtLote write FnrProtLote;
    property dhProcess: TDateTime read FdhProcess write FdhProcess;
    property dhRecepcao: TDateTime read FdhRecepcao write FdhRecepcao;
    property tpEv: String read FtpEv write FtpEv;
    property idEv: String read FidEv write FidEv;
    property hash: String read Fhash write Fhash;
    property fechRet: TtpFechRet read FfechRet write FfechRet;
  end;

  TInfoTotal = class(TObject)
  private
    FnrRecArqBase: String;
    FideEstab: TideEstab;
  public
    constructor Create;
    destructor Destroy; override;

    property nrRecArqBase: string read FnrRecArqBase;
    property ideEstab: TideEstab read FideEstab write FideEstab;
  end;

  { TideEstab }

  TideEstab = class(TObject)
  private
    FtpInsc: TtpInsc;
    FnrInsc: string;
    FnrInscBenef: string;
    FnmBenef: string;
    FideEvtAdic: string;
    FtotApurMen: TtotApurMenCollection;
    FtotApurQui: TtotApurQuiCollection;
    FtotApurDec: TtotApurDecCollection;
    FtotApurSem: TtotApurSemCollection;
    FtotApurDia: TtotApurDiaCollection;

    procedure SettotApurMen(const Value: TtotApurMenCollection);
    procedure SettotApurQui(const Value: TtotApurQuiCollection);
    procedure SettotApurDec(const Value: TtotApurDecCollection);
    procedure SettotApurSem(const Value: TtotApurSemCollection);
    procedure SettotApurDia(const Value: TtotApurDiaCollection);
  public
    constructor Create;
    destructor Destroy; override;

    property tpInsc: TtpInsc read FtpInsc write FtpInsc default tiCNPJ;
    property nrInsc: string read FnrInsc write FnrInsc;
    property nrInscBenef: string read FnrInscBenef write FnrInscBenef;
    property nmBenef: string read FnmBenef write FnmBenef;
    property ideEvtAdic: string read FideEvtAdic write FideEvtAdic;
    property totApurMen: TtotApurMenCollection read FtotApurMen write SettotApurMen;
    property totApurQui: TtotApurQuiCollection read FtotApurQui write SettotApurQui;
    property totApurDec: TtotApurDecCollection read FtotApurDec write SettotApurDec;
    property totApurSem: TtotApurSemCollection read FtotApurSem write SettotApurSem;
    property totApurDia: TtotApurDiaCollection read FtotApurDia write SettotApurDia;
  end;

  { TtotApurMenCollection }

  TtotApurMenCollection = class(TObjectList)
  private
    function GetItem(Index: Integer): TtotApurMenCollectionItem;
    procedure SetItem(Index: Integer; Value: TtotApurMenCollectionItem);
  public
    function Add: TtotApurMenCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TtotApurMenCollectionItem;

    property Items[Index: Integer]: TtotApurMenCollectionItem read GetItem write SetItem;
  end;

  { TtotApurMenCollectionItem }

  TtotApurMenCollectionItem = class(TObject)
  private
    FCRMen: string;
    FvlrBaseCRMen: double;
    FvlrBaseCRMenSusp: double;
    FnatRend: string;
    FtotApurTribMen: TtotApurTribMen;
  public
    constructor Create;
    destructor Destroy; override;

    property CRMen: string read FCRMen;
    property vlrBaseCRMen: double read FvlrBaseCRMen;
    property vlrBaseCRMenSusp: double read FvlrBaseCRMenSusp;
    property natRend: string read FnatRend;
    property totApurTribMen: TtotApurTribMen read FtotApurTribMen;
  end;

  { TtotApurTribMen }

  TtotApurTribMen = class(TObject)
  private
    FvlrCRMenInf: double;
    FvlrCRMenCalc: double;
    FvlrCRMenSuspInf: double;
    FvlrCRMenSuspCalc: double;
  public
    property vlrCRMenInf: double read FvlrCRMenInf;
    property vlrCRMenCalc: double read FvlrCRMenCalc;
    property vlrCRMenSuspInf: double read FvlrCRMenSuspInf;
    property vlrCRMenSuspCalc: double read FvlrCRMenSuspCalc;
  end;

  { TtotApurQuiCollection }

  TtotApurQuiCollection = class(TObjectList)
  private
    function GetItem(Index: Integer): TtotApurQuiCollectionItem;
    procedure SetItem(Index: Integer; Value: TtotApurQuiCollectionItem);
  public
    function Add: TtotApurQuiCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TtotApurQuiCollectionItem;

    property Items[Index: Integer]: TtotApurQuiCollectionItem read GetItem write SetItem;
  end;

  { TtotApurQuiCollectionItem }

  TtotApurQuiCollectionItem = class(TObject)
  private
    FperApurQui: TtpPerApurQui;
    FCRQui: string;
    FvlrBaseCRQui: double;
    FvlrBaseCRQuiSusp: double;
    FnatRend: string;
    FtotApurTribQui: TtotApurTribQui;
  public
    constructor Create;
    destructor Destroy; override;

    property perApurQui: TtpPerApurQui read FperApurQui;
    property CRQui: string read FCRQui;
    property vlrBaseCRQui: double read FvlrBaseCRQui;
    property vlrBaseCRQuiSusp: double read FvlrBaseCRQuiSusp;
    property natRend: string read FnatRend;
    property totApurTribQui: TtotApurTribQui read FtotApurTribQui;
  end;

  { TtotApurTribQui }

  TtotApurTribQui = class(TObject)
  private
    FvlrCRQuiInf: double;
    FvlrCRQuiCalc: double;
    FvlrCRQuiSuspInf: double;
    FvlrCRQuiSuspCalc: double;
  public
    property vlrCRQuiInf: double read FvlrCRQuiInf;
    property vlrCRQuiCalc: double read FvlrCRQuiCalc;
    property vlrCRQuiSuspInf: double read FvlrCRQuiSuspInf;
    property vlrCRQuiSuspCalc: double read FvlrCRQuiSuspCalc;
  end;

  { TtotApurDecCollection }

  TtotApurDecCollection = class(TObjectList)
  private
    function GetItem(Index: Integer): TtotApurDecCollectionItem;
    procedure SetItem(Index: Integer; Value: TtotApurDecCollectionItem);
  public
    function Add: TtotApurDecCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TtotApurDecCollectionItem;

    property Items[Index: Integer]: TtotApurDecCollectionItem read GetItem write SetItem;
  end;

  { TtotApurDecCollectionItem }

  TtotApurDecCollectionItem = class(TObject)
  private
    FperApurDec: TtpPerApurDec;
    FCRDec: string;
    FvlrBaseCRDec: double;
    FvlrBaseCRDecSusp: double;
    FnatRend: string;
    FtotApurTribDec: TtotApurTribDec;
  public
    constructor Create;
    destructor Destroy; override;

    property perApurDec: TtpPerApurDec read FperApurDec;
    property CRDec: string read FCRDec;
    property vlrBaseCRDec: double read FvlrBaseCRDec;
    property vlrBaseCRDecSusp: double read FvlrBaseCRDecSusp;
    property natRend: string read FnatRend;
    property totApurTribDec: TtotApurTribDec read FtotApurTribDec;
  end;

  { TtotApurTribDec }

  TtotApurTribDec = class(TObject)
  private
    FvlrCRDecInf: double;
    FvlrCRDecCalc: double;
    FvlrCRDecSuspInf: double;
    FvlrCRDecSuspCalc: double;
  public
    property vlrCRDecInf: double read FvlrCRDecInf;
    property vlrCRDecCalc: double read FvlrCRDecCalc;
    property vlrCRDecSuspInf: double read FvlrCRDecSuspInf;
    property vlrCRDecSuspCalc: double read FvlrCRDecSuspCalc;
  end;

  { TtotApurDecCollection }

  TtotApurSemCollection = class(TObjectList)
  private
    function GetItem(Index: Integer): TtotApurSemCollectionItem;
    procedure SetItem(Index: Integer; Value: TtotApurSemCollectionItem);
  public
    function Add: TtotApurSemCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TtotApurSemCollectionItem;

    property Items[Index: Integer]: TtotApurSemCollectionItem read GetItem write SetItem;
  end;

  { TtotApurSemCollectionItem }

  TtotApurSemCollectionItem = class(TObject)
  private
    FperApurSem: TtpPerApurSem;
    FCRSem: string;
    FvlrBaseCRSem: double;
    FvlrBaseCRSemSusp: double;
    FnatRend: string;
    FtotApurTribSem: TtotApurTribSem;
  public
    constructor Create;
    destructor Destroy; override;

    property perApurSem: TtpPerApurSem read FperApurSem;
    property CRSem: string read FCRSem;
    property vlrBaseCRSem: double read FvlrBaseCRSem;
    property vlrBaseCRSemSusp: double read FvlrBaseCRSemSusp;
    property natRend: string read FnatRend;
    property totApurTribSem: TtotApurTribSem read FtotApurTribSem;
  end;

  { TtotApurTribSem }

  TtotApurTribSem = class(TObject)
  private
    FvlrCRSemInf: double;
    FvlrCRSemCalc: double;
    FvlrCRSemSuspInf: double;
    FvlrCRSemSuspCalc: double;
  public
    property vlrCRSemInf: double read FvlrCRSemInf;
    property vlrCRSemCalc: double read FvlrCRSemCalc;
    property vlrCRSemSuspInf: double read FvlrCRSemSuspInf;
    property vlrCRSemSuspCalc: double read FvlrCRSemSuspCalc;
  end;

  { TtotApurDiaCollection }

  TtotApurDiaCollection = class(TObjectList)
  private
    function GetItem(Index: Integer): TtotApurDiaCollectionItem;
    procedure SetItem(Index: Integer; Value: TtotApurDiaCollectionItem);
  public
    function Add: TtotApurDiaCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TtotApurDiaCollectionItem;

    property Items[Index: Integer]: TtotApurDiaCollectionItem read GetItem write SetItem;
  end;

  { TtotApurDiaCollectionItem }

  TtotApurDiaCollectionItem = class(TObject)
  private
    FperApurDia: string;
    FCRDia: string;
    FvlrBaseCRDia: double;
    FvlrBaseCRDiaSusp: double;
    FnatRend: string;
    FtotApurTribDia: TtotApurTribDia;
  public
    constructor Create;
    destructor Destroy; override;

    property perApurDia: string read FperApurDia;
    property CRDia: string read FCRDia;
    property vlrBaseCRDia: double read FvlrBaseCRDia;
    property vlrBaseCRDiaSusp: double read FvlrBaseCRDiaSusp;
    property natRend: string read FnatRend;
    property totApurTribDia: TtotApurTribDia read FtotApurTribDia;
  end;

  { TtotApurTribDia }

  TtotApurTribDia = class(TObject)
  private
    FvlrCRDiaInf: double;
    FvlrCRDiaCalc: double;
    FvlrCRDiaSuspInf: double;
    FvlrCRDiaSuspCalc: double;
  public
    property vlrCRDiaInf: double read FvlrCRDiaInf;
    property vlrCRDiaCalc: double read FvlrCRDiaCalc;
    property vlrCRDiaSuspInf: double read FvlrCRDiaSuspInf;
    property vlrCRDiaSuspCalc: double read FvlrCRDiaSuspCalc;
  end;

implementation

uses
  IniFiles,
  ACBrUtil.Base,
  ACBrUtil.Strings,
  pcnConversao, DateUtils;

{ TR9005 }

constructor TR9005.Create;
begin
  FTipoEvento := teR9005;
  FevtRet := TevtRet.Create;
end;

destructor TR9005.Destroy;
begin
  FevtRet.Free;

  inherited;
end;

function TR9005.GetXml : string;
begin
  Result := FevtRet.XML;
end;

procedure TR9005.SetXml(const Value: string);
begin
  if Value = FevtRet.XML then Exit;

  FevtRet.FXML := Value;
  FevtRet.Leitor.Arquivo := Value;
  FevtRet.LerXML;
end;

function TR9005.GetTipoEvento : TTipoEvento;
begin
  Result := FTipoEvento;
end;

function TR9005.GetEvento: TObject;
begin
  Result := Self;
end;

{ TevtRet }

constructor TevtRet.Create();
begin
  FLeitor := TLeitor.Create;

  FIdeEvento  := TIdeEvento1.Create;
  FIdeContrib := TIdeContrib.Create;
  FIdeStatus  := TIdeStatus.Create;
  FInfoRecEv  := TInfoRecEv.Create;
  FInfoTotal  := TInfoTotal.Create;
end;

destructor TevtRet.Destroy;
begin
  FLeitor.Free;

  FIdeEvento.Free;
  FIdeContrib.Free;
  FIdeStatus.Free;
  FInfoRecEv.Free;
  FInfoTotal.Free;

  inherited;
end;

function TevtRet.LerXML: boolean;
var
  ok: Boolean;
  i: Integer;
  totApurTribMen: TtotApurTribMen;
  totApurTribQui: TtotApurTribQui;
  totApurTribDec: TtotApurTribDec;
  totApurTribSem: TtotApurTribSem;
  totApurTribDia: TtotApurTribDia;
begin
  Result := True;
  try
    FXML := Leitor.Arquivo;

    if leitor.rExtrai(1, 'evtRet') <> '' then
    begin
      FId := Leitor.rAtributo('id=');

      if leitor.rExtrai(2, 'ideEvento') <> '' then
        IdeEvento.perApur := leitor.rCampo(tcStr, 'perApur');

      if leitor.rExtrai(2, 'ideContri') <> '' then
      begin
        IdeContrib.TpInsc := StrToTpInscricao(ok, leitor.rCampo(tcStr, 'tpInsc'));
        IdeContrib.NrInsc := leitor.rCampo(tcStr, 'nrInsc');
      end;

      if leitor.rExtrai(2, 'ideRecRetorno') <> '' then
      begin
        if leitor.rExtrai(3, 'ideStatus') <> '' then
        begin
          IdeStatus.cdRetorno   := leitor.rCampo(tcStr, 'cdRetorno');
          IdeStatus.descRetorno := leitor.rCampo(tcStr, 'descRetorno');

          i := 0;
          while Leitor.rExtrai(4, 'regOcorrs', '', i + 1) <> '' do
          begin
            IdeStatus.regOcorrs.New;
            IdeStatus.regOcorrs.Items[i].tpOcorr        := leitor.rCampo(tcInt, 'tpOcorr');
            IdeStatus.regOcorrs.Items[i].localErroAviso := leitor.rCampo(tcStr, 'localErroAviso');
            IdeStatus.regOcorrs.Items[i].codResp        := leitor.rCampo(tcStr, 'codResp');
            IdeStatus.regOcorrs.Items[i].dscResp        := leitor.rCampo(tcStr, 'dscResp');
            inc(i);
          end;
        end;
      end;

      if leitor.rExtrai(2, 'infoRecEv') <> '' then
      begin
        infoRecEv.FnrRecArqBase := leitor.rCampo(tcStr, 'nrRecArqBase');
        infoRecEv.FnrProtLote := leitor.rCampo(tcStr, 'nrProtLote');
        infoRecEv.FdhProcess  := leitor.rCampo(tcDatHor, 'dhProcess');
        infoRecEv.FdhRecepcao := leitor.rCampo(tcDatHor, 'dhRecepcao');
        infoRecEv.FtpEv       := leitor.rCampo(tcStr, 'tpEv');
        infoRecEv.FidEv       := leitor.rCampo(tcStr, 'idEv');
        infoRecEv.Fhash       := leitor.rCampo(tcStr, 'hash');
      end;

      if leitor.rExtrai(2, 'infoTotal') <> '' then
      begin
        infoTotal.FnrRecArqBase := leitor.rCampo(tcStr, 'nrRecArqBase');

        if leitor.rExtrai(3, 'ideEstab') <> '' then
        begin
          infoTotal.ideEstab.tpInsc      := StrToTpInscricao(ok, leitor.rCampo(tcStr, 'tpInsc'));
          infoTotal.ideEstab.nrInsc      := leitor.rCampo(tcStr, 'nrInsc');
          infoTotal.ideEstab.nrInscBenef := leitor.rCampo(tcStr, 'nrInscBenef');
          infoTotal.ideEstab.nmBenef     := leitor.rCampo(tcStr, 'nmBenef');
          infoTotal.ideEstab.ideEvtAdic  := leitor.rCampo(tcStr, 'ideEvtAdic');

          i := 0;
          while Leitor.rExtrai(4, 'totApurMen', '', i + 1) <> '' do
          begin
            infoTotal.ideEstab.totApurMen.New;
            infoTotal.ideEstab.totApurMen.Items[i].FCRMen            := leitor.rCampo(tcStr, 'CRMen');
            infoTotal.ideEstab.totApurMen.Items[i].FvlrBaseCRMen     := leitor.rCampo(tcDe2, 'vlrBaseCRMen');
            infoTotal.ideEstab.totApurMen.Items[i].FvlrBaseCRMenSusp := leitor.rCampo(tcDe2, 'vlrBaseCRMenSusp');
            infoTotal.ideEstab.totApurMen.Items[i].FnatRend          := leitor.rCampo(tcStr, 'natRend');

            totApurTribMen := infoTotal.ideEstab.totApurMen.Items[i].totApurTribMen;
            totApurTribMen.FvlrCRMenInf      := leitor.rCampo(tcDe2, 'vlrCRMenInf');
            totApurTribMen.FvlrCRMenCalc     := leitor.rCampo(tcDe2, 'vlrCRMenCalc');
            totApurTribMen.FvlrCRMenSuspInf  := leitor.rCampo(tcDe2, 'vlrCRMenSuspInf');
            totApurTribMen.FvlrCRMenSuspCalc := leitor.rCampo(tcDe2, 'vlrCRMenSuspCalc');

            inc(i);
          end;

          i := 0;
          while Leitor.rExtrai(4, 'totApurQui', '', i + 1) <> '' do
          begin
            infoTotal.ideEstab.totApurQui.New;
            infoTotal.ideEstab.totApurQui.Items[i].FperApurQui       := StrToTpPerApurQui(ok, leitor.rCampo(tcStr, 'perApurQui'));
            infoTotal.ideEstab.totApurQui.Items[i].FCRQui            := leitor.rCampo(tcStr, 'CRQui');
            infoTotal.ideEstab.totApurQui.Items[i].FvlrBaseCRQui     := leitor.rCampo(tcDe2, 'vlrBaseCRQui');
            infoTotal.ideEstab.totApurQui.Items[i].FvlrBaseCRQuiSusp := leitor.rCampo(tcDe2, 'vlrBaseCRQuiSusp');
            infoTotal.ideEstab.totApurQui.Items[i].FnatRend          := leitor.rCampo(tcStr, 'natRend');

            totApurTribQui := infoTotal.ideEstab.totApurQui.Items[i].totApurTribQui;
            totApurTribQui.FvlrCRQuiInf      := leitor.rCampo(tcDe2, 'vlrCRQuiInf');
            totApurTribQui.FvlrCRQuiCalc     := leitor.rCampo(tcDe2, 'vlrCRQuiCalc');
            totApurTribQui.FvlrCRQuiSuspInf  := leitor.rCampo(tcDe2, 'vlrCRQuiSuspInf');
            totApurTribQui.FvlrCRQuiSuspCalc := leitor.rCampo(tcDe2, 'vlrCRQuiSuspCalc');

            inc(i);
          end;

          i := 0;
          while Leitor.rExtrai(4, 'totApurDec', '', i + 1) <> '' do
          begin
            infoTotal.ideEstab.totApurDec.New;
            infoTotal.ideEstab.totApurDec.Items[i].FperApurDec       := StrToTpPerApurDec(ok, leitor.rCampo(tcStr, 'perApurDec'));
            infoTotal.ideEstab.totApurDec.Items[i].FCRDec            := leitor.rCampo(tcStr, 'CRDec');
            infoTotal.ideEstab.totApurDec.Items[i].FvlrBaseCRDec     := leitor.rCampo(tcDe2, 'vlrBaseCRDec');
            infoTotal.ideEstab.totApurDec.Items[i].FvlrBaseCRDecSusp := leitor.rCampo(tcDe2, 'vlrBaseCRDecSusp');
            infoTotal.ideEstab.totApurDec.Items[i].FnatRend          := leitor.rCampo(tcStr, 'natRend');

            totApurTribDec := infoTotal.ideEstab.totApurDec.Items[i].totApurTribDec;
            totApurTribDec.FvlrCRDecInf      := leitor.rCampo(tcDe2, 'vlrCRDecInf');
            totApurTribDec.FvlrCRDecCalc     := leitor.rCampo(tcDe2, 'vlrCRDecCalc');
            totApurTribDec.FvlrCRDecSuspInf  := leitor.rCampo(tcDe2, 'vlrCRDecSuspInf');
            totApurTribDec.FvlrCRDecSuspCalc := leitor.rCampo(tcDe2, 'vlrCRDecSuspCalc');

            inc(i);
          end;

          i := 0;
          while Leitor.rExtrai(4, 'totApurSem', '', i + 1) <> '' do
          begin
            infoTotal.ideEstab.totApurSem.New;
            infoTotal.ideEstab.totApurSem.Items[i].FperApurSem       := StrToTpPerApurSem(ok, leitor.rCampo(tcStr, 'perApurSem'));
            infoTotal.ideEstab.totApurSem.Items[i].FCRSem            := leitor.rCampo(tcStr, 'CRSem');
            infoTotal.ideEstab.totApurSem.Items[i].FvlrBaseCRSem     := leitor.rCampo(tcDe2, 'vlrBaseCRSem');
            infoTotal.ideEstab.totApurSem.Items[i].FvlrBaseCRSemSusp := leitor.rCampo(tcDe2, 'vlrBaseCRSemSusp');
            infoTotal.ideEstab.totApurSem.Items[i].FnatRend          := leitor.rCampo(tcStr, 'natRend');

            totApurTribSem := infoTotal.ideEstab.totApurSem.Items[i].totApurTribSem;
            totApurTribSem.FvlrCRSemInf      := leitor.rCampo(tcDe2, 'vlrCRSemInf');
            totApurTribSem.FvlrCRSemCalc     := leitor.rCampo(tcDe2, 'vlrCRSemCalc');
            totApurTribSem.FvlrCRSemSuspInf  := leitor.rCampo(tcDe2, 'vlrCRSemSuspInf');
            totApurTribSem.FvlrCRSemSuspCalc := leitor.rCampo(tcDe2, 'vlrCRSemSuspCalc');

            inc(i);
          end;

          i := 0;
          while Leitor.rExtrai(4, 'totApurDia', '', i + 1) <> '' do
          begin
            infoTotal.ideEstab.totApurDia.New;
            infoTotal.ideEstab.totApurDia.Items[i].FperApurDia       := leitor.rCampo(tcStr, 'perApurDia');
            infoTotal.ideEstab.totApurDia.Items[i].FCRDia            := leitor.rCampo(tcStr, 'CRDia');
            infoTotal.ideEstab.totApurDia.Items[i].FvlrBaseCRDia     := leitor.rCampo(tcDe2, 'vlrBaseCRDia');
            infoTotal.ideEstab.totApurDia.Items[i].FvlrBaseCRDiaSusp := leitor.rCampo(tcDe2, 'vlrBaseCRDiaSusp');
            infoTotal.ideEstab.totApurDia.Items[i].FnatRend          := leitor.rCampo(tcStr, 'natRend');

            totApurTribDia := infoTotal.ideEstab.totApurDia.Items[i].totApurTribDia;
            totApurTribDia.FvlrCRDiaInf      := leitor.rCampo(tcDe2, 'vlrCRDiaInf');
            totApurTribDia.FvlrCRDiaCalc     := leitor.rCampo(tcDe2, 'vlrCRDiaCalc');
            totApurTribDia.FvlrCRDiaSuspInf  := leitor.rCampo(tcDe2, 'vlrCRDiaSuspInf');
            totApurTribDia.FvlrCRDiaSuspCalc := leitor.rCampo(tcDe2, 'vlrCRDiaSuspCalc');

            inc(i);
          end;
        end;
      end;
    end;
  except
    Result := False;
  end;
end;

function TevtRet.SalvarINI: boolean;
var
  AIni: TMemIniFile;
  sSecao: String;
  i: Integer;
  totApurTribMen: TtotApurTribMen;
  totApurTribQui: TtotApurTribQui;
  totApurTribDec: TtotApurTribDec;
  totApurTribSem: TtotApurTribSem;
  totApurTribDia: TtotApurTribDia;
begin
  Result := True;

  AIni := TMemIniFile.Create('');
  try
    with Self do
    begin
      sSecao := 'evtRet';
      AIni.WriteString(sSecao, 'Id', Id);

      sSecao := 'ideEvento';
      AIni.WriteString(sSecao, 'perApur', IdeEvento.perApur);

      sSecao := 'ideContri';
      AIni.WriteString(sSecao, 'tpInsc', TpInscricaoToStr(IdeContrib.TpInsc));
      AIni.WriteString(sSecao, 'nrInsc', IdeContrib.nrInsc);

      sSecao := 'ideStatus';
      AIni.WriteString(sSecao, 'cdRetorno', ideStatus.cdRetorno);
      AIni.WriteString(sSecao, 'descRetorno', ideStatus.descRetorno);

      for i := 0 to ideStatus.regOcorrs.Count -1 do
      begin
        sSecao := 'regOcorrs' + IntToStrZero(I, 3);

        AIni.WriteInteger(sSecao, 'tpOcorr',       ideStatus.regOcorrs.Items[i].tpOcorr);
        AIni.WriteString(sSecao, 'localErroAviso', ideStatus.regOcorrs.Items[i].localErroAviso);
        AIni.WriteString(sSecao, 'codResp',        ideStatus.regOcorrs.Items[i].codResp);
        AIni.WriteString(sSecao, 'dscResp',        ideStatus.regOcorrs.Items[i].dscResp);
      end;

      sSecao := 'infoRecEv';
      AIni.WriteString(sSecao, 'nrRecArqBase', infoRecEv.nrRecArqBase);
      AIni.WriteString(sSecao, 'nrProtLote', infoRecEv.nrProtLote);
      AIni.WriteString(sSecao, 'dhProcess',  DateToStr(infoRecEv.dhProcess));
      AIni.WriteString(sSecao, 'dhRecepcao', DateToStr(infoRecEv.dhRecepcao));
      AIni.WriteString(sSecao, 'tpEv',       infoRecEv.tpEv);
      AIni.WriteString(sSecao, 'idEv',       infoRecEv.idEv);
      AIni.WriteString(sSecao, 'hash',       infoRecEv.hash);

      with InfoTotal do
      begin
        sSecao := 'infoTotal';
        AIni.WriteString(sSecao, 'nrRecArqBase', nrRecArqBase);

        sSecao := 'ideEstab';
        AIni.WriteString(sSecao, 'tpInsc',      TpInscricaoToStr(infoTotal.ideEstab.tpInsc));
        AIni.WriteString(sSecao, 'nrInsc',      infoTotal.ideEstab.nrInsc);
        AIni.WriteString(sSecao, 'nrInscBenef', infoTotal.ideEstab.nrInscBenef);
        AIni.WriteString(sSecao, 'nmBenef',     infoTotal.ideEstab.nmBenef);
        AIni.WriteString(sSecao, 'ideEvtAdic',  infoTotal.ideEstab.ideEvtAdic);

        for i := 0 to infoTotal.ideEstab.totApurMen.Count - 1 do
        begin
          sSecao := 'totApurMen' + IntToStrZero(I, 1);

          AIni.WriteString(sSecao, 'CRMen',            infoTotal.ideEstab.totApurMen.Items[i].CRMen);
          AIni.WriteFloat(sSecao,  'vlrBaseCRMen',     infoTotal.ideEstab.totApurMen.Items[i].vlrBaseCRMen);
          AIni.WriteFloat(sSecao,  'vlrBaseCRMenSusp', infoTotal.ideEstab.totApurMen.Items[i].vlrBaseCRMenSusp);
          AIni.WriteString(sSecao, 'natRend',          infoTotal.ideEstab.totApurMen.Items[i].natRend);

          totApurTribMen := infoTotal.ideEstab.totApurMen.Items[i].totApurTribMen;
          AIni.WriteFloat(sSecao,  'vlrCRMenInf',      totApurTribMen.FvlrCRMenInf);
          AIni.WriteFloat(sSecao,  'vlrCRMenCalc',     totApurTribMen.FvlrCRMenCalc);
          AIni.WriteFloat(sSecao,  'vlrCRMenSuspInf',  totApurTribMen.FvlrCRMenSuspInf);
          AIni.WriteFloat(sSecao,  'vlrCRMenSuspCalc', totApurTribMen.FvlrCRMenSuspCalc);
        end;

        for i := 0 to infoTotal.ideEstab.totApurQui.Count - 1 do
        begin
          sSecao := 'totApurQui' + IntToStrZero(I, 1);

          AIni.WriteString(sSecao, 'perApurQui',       tpPerApurQuiToStr(infoTotal.ideEstab.totApurQui.Items[i].perApurQui));
          AIni.WriteString(sSecao, 'CRQui',            infoTotal.ideEstab.totApurQui.Items[i].CRQui);
          AIni.WriteFloat(sSecao,  'vlrBaseCRQui',     infoTotal.ideEstab.totApurQui.Items[i].vlrBaseCRQui);
          AIni.WriteFloat(sSecao,  'vlrBaseCRQuiSusp', infoTotal.ideEstab.totApurQui.Items[i].vlrBaseCRQuiSusp);
          AIni.WriteString(sSecao, 'natRend',          infoTotal.ideEstab.totApurQui.Items[i].natRend);

          totApurTribQui := infoTotal.ideEstab.totApurQui.Items[i].totApurTribQui;
          AIni.WriteFloat(sSecao,  'vlrCRQuiInf',      totApurTribQui.FvlrCRQuiInf);
          AIni.WriteFloat(sSecao,  'vlrCRQuiCalc',     totApurTribQui.FvlrCRQuiCalc);
          AIni.WriteFloat(sSecao,  'vlrCRQuiSuspInf',  totApurTribQui.FvlrCRQuiSuspInf);
          AIni.WriteFloat(sSecao,  'vlrCRQuiSuspCalc', totApurTribQui.FvlrCRQuiSuspCalc);
        end;

        for i := 0 to infoTotal.ideEstab.totApurDec.Count - 1 do
        begin
          sSecao := 'totApurDec' + IntToStrZero(I, 1);

          AIni.WriteString(sSecao, 'perApurDec',       tpPerApurDecToStr(infoTotal.ideEstab.totApurDec.Items[i].perApurDec));
          AIni.WriteString(sSecao, 'CRDec',            infoTotal.ideEstab.totApurDec.Items[i].CRDec);
          AIni.WriteFloat(sSecao,  'vlrBaseCRDec',     infoTotal.ideEstab.totApurDec.Items[i].vlrBaseCRDec);
          AIni.WriteFloat(sSecao,  'vlrBaseCRDecSusp', infoTotal.ideEstab.totApurDec.Items[i].vlrBaseCRDecSusp);
          AIni.WriteString(sSecao, 'natRend',          infoTotal.ideEstab.totApurDec.Items[i].natRend);

          totApurTribDec := infoTotal.ideEstab.totApurDec.Items[i].totApurTribDec;
          AIni.WriteFloat(sSecao,  'vlrCRDecInf',      totApurTribDec.FvlrCRDecInf);
          AIni.WriteFloat(sSecao,  'vlrCRDecCalc',     totApurTribDec.FvlrCRDecCalc);
          AIni.WriteFloat(sSecao,  'vlrCRDecSuspInf',  totApurTribDec.FvlrCRDecSuspInf);
          AIni.WriteFloat(sSecao,  'vlrCRDecSuspCalc', totApurTribDec.FvlrCRDecSuspCalc);
        end;

        for i := 0 to infoTotal.ideEstab.totApurSem.Count - 1 do
        begin
          sSecao := 'totApurSem' + IntToStrZero(I, 1);

          AIni.WriteString(sSecao, 'perApurSem',       tpPerApurSemToStr(infoTotal.ideEstab.totApurSem.Items[i].perApurSem));
          AIni.WriteString(sSecao, 'CRSem',            infoTotal.ideEstab.totApurSem.Items[i].CRSem);
          AIni.WriteFloat(sSecao,  'vlrBaseCRSem',     infoTotal.ideEstab.totApurSem.Items[i].vlrBaseCRSem);
          AIni.WriteFloat(sSecao,  'vlrBaseCRSemSusp', infoTotal.ideEstab.totApurSem.Items[i].vlrBaseCRSemSusp);
          AIni.WriteString(sSecao, 'natRend',          infoTotal.ideEstab.totApurSem.Items[i].natRend);

          totApurTribSem := infoTotal.ideEstab.totApurSem.Items[i].totApurTribSem;
          AIni.WriteFloat(sSecao,  'vlrCRSemInf',      totApurTribSem.FvlrCRSemInf);
          AIni.WriteFloat(sSecao,  'vlrCRSemCalc',     totApurTribSem.FvlrCRSemCalc);
          AIni.WriteFloat(sSecao,  'vlrCRSemSuspInf',  totApurTribSem.FvlrCRSemSuspInf);
          AIni.WriteFloat(sSecao,  'vlrCRSemSuspCalc', totApurTribSem.FvlrCRSemSuspCalc);
        end;

        for i := 0 to infoTotal.ideEstab.totApurDia.Count - 1 do
        begin
          sSecao := 'totApurDia' + IntToStrZero(I, 1);

          AIni.WriteString(sSecao, 'perApurDia',       infoTotal.ideEstab.totApurDia.Items[i].perApurDia);
          AIni.WriteString(sSecao, 'CRDia',            infoTotal.ideEstab.totApurDia.Items[i].CRDia);
          AIni.WriteFloat(sSecao,  'vlrBaseCRDia',     infoTotal.ideEstab.totApurDia.Items[i].vlrBaseCRDia);
          AIni.WriteFloat(sSecao,  'vlrBaseCRDiaSusp', infoTotal.ideEstab.totApurDia.Items[i].vlrBaseCRDiaSusp);
          AIni.WriteString(sSecao, 'natRend',          infoTotal.ideEstab.totApurDia.Items[i].natRend);

          totApurTribDia := infoTotal.ideEstab.totApurDia.Items[i].totApurTribDia;
          AIni.WriteFloat(sSecao,  'vlrCRDiaInf',      totApurTribDia.FvlrCRDiaInf);
          AIni.WriteFloat(sSecao,  'vlrCRDiaCalc',     totApurTribDia.FvlrCRDiaCalc);
          AIni.WriteFloat(sSecao,  'vlrCRDiaSuspInf',  totApurTribDia.FvlrCRDiaSuspInf);
          AIni.WriteFloat(sSecao,  'vlrCRDiaSuspCalc', totApurTribDia.FvlrCRDiaSuspCalc);
        end;
      end;
    end;
  finally
    AIni.Free;
  end;
end;

{ TInfoTotal }

constructor TInfoTotal.Create;
begin
  inherited;

  FideEstab := TideEstab.Create;
end;

destructor TInfoTotal.Destroy;
begin
  FideEstab.Free;

  inherited;
end;

{ TideEstab }

constructor TideEstab.Create;
begin
  FtotApurMen := TtotApurMenCollection.Create;
  FtotApurQui := TtotApurQuiCollection.Create;
  FtotApurDec := TtotApurDecCollection.Create;
  FtotApurSem := TtotApurSemCollection.Create;
  FtotApurDia := TtotApurDiaCollection.Create;
end;

destructor TideEstab.Destroy;
begin
  FtotApurMen.Free;
  FtotApurQui.Free;
  FtotApurDec.Free;
  FtotApurSem.Free;
  FtotApurDia.Free;

  inherited;
end;

procedure TideEstab.SettotApurMen(const Value: TtotApurMenCollection);
begin
  FtotApurMen := Value;
end;

procedure TideEstab.SettotApurQui(const Value: TtotApurQuiCollection);
begin
  FtotApurQui := Value;
end;

procedure TideEstab.SettotApurDec(const Value: TtotApurDecCollection);
begin
  FtotApurDec := Value;
end;

procedure TideEstab.SettotApurSem(const Value: TtotApurSemCollection);
begin
  FtotApurSem := Value;
end;

procedure TideEstab.SettotApurDia(const Value: TtotApurDiaCollection);
begin
  FtotApurDia := Value;
end;

{ TtotApurMenCollection }

function TtotApurMenCollection.Add: TtotApurMenCollectionItem;
begin
  Result := Self.New;
end;

function TtotApurMenCollection.GetItem(
  Index: Integer): TtotApurMenCollectionItem;
begin
  Result := TtotApurMenCollectionItem(inherited GetItem(Index));
end;

function TtotApurMenCollection.New: TtotApurMenCollectionItem;
begin
  Result := TtotApurMenCollectionItem.Create;
  Self.Add(Result);
end;

procedure TtotApurMenCollection.SetItem(Index: Integer;
  Value: TtotApurMenCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TtotApurMenCollectionItem }

constructor TtotApurMenCollectionItem.Create;
begin
  FtotApurTribMen := TtotApurTribMen.Create;
end;

destructor TtotApurMenCollectionItem.Destroy;
begin
  FtotApurTribMen.Free;

  inherited;
end;

{ TtotApurQuiCollection }

function TtotApurQuiCollection.Add: TtotApurQuiCollectionItem;
begin
  Result := Self.New;
end;

function TtotApurQuiCollection.GetItem(
  Index: Integer): TtotApurQuiCollectionItem;
begin
  Result := TtotApurQuiCollectionItem(inherited GetItem(Index));
end;

function TtotApurQuiCollection.New: TtotApurQuiCollectionItem;
begin
  Result := TtotApurQuiCollectionItem.Create;
  Self.Add(Result);
end;

procedure TtotApurQuiCollection.SetItem(Index: Integer;
  Value: TtotApurQuiCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TtotApurQuiCollectionItem }

constructor TtotApurQuiCollectionItem.Create;
begin
  FtotApurTribQui := TtotApurTribQui.Create;
end;

destructor TtotApurQuiCollectionItem.Destroy;
begin
  FtotApurTribQui.Free;

  inherited;
end;

{ TtotApurDecCollection }

function TtotApurDecCollection.Add: TtotApurDecCollectionItem;
begin
  Result := Self.New;
end;

function TtotApurDecCollection.GetItem(
  Index: Integer): TtotApurDecCollectionItem;
begin
  Result := TtotApurDecCollectionItem(inherited GetItem(Index));
end;

function TtotApurDecCollection.New: TtotApurDecCollectionItem;
begin
  Result := TtotApurDecCollectionItem.Create;
  Self.Add(Result);
end;

procedure TtotApurDecCollection.SetItem(Index: Integer;
  Value: TtotApurDecCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TtotApurDecCollectionItem }

constructor TtotApurDecCollectionItem.Create;
begin
  FtotApurTribDec := TtotApurTribDec.Create;
end;

destructor TtotApurDecCollectionItem.Destroy;
begin
  FtotApurTribDec.Free;

  inherited;
end;

{ TtotApurSemCollection }

function TtotApurSemCollection.Add: TtotApurSemCollectionItem;
begin
  Result := Self.New;
end;

function TtotApurSemCollection.GetItem(
  Index: Integer): TtotApurSemCollectionItem;
begin
  Result := TtotApurSemCollectionItem(inherited GetItem(Index));
end;

function TtotApurSemCollection.New: TtotApurSemCollectionItem;
begin
  Result := TtotApurSemCollectionItem.Create;
  Self.Add(Result);
end;

procedure TtotApurSemCollection.SetItem(Index: Integer;
  Value: TtotApurSemCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TtotApurSemCollectionItem }

constructor TtotApurSemCollectionItem.Create;
begin
  FtotApurTribSem := TtotApurTribSem.Create;
end;

destructor TtotApurSemCollectionItem.Destroy;
begin
  FtotApurTribSem.Free;

  inherited;
end;

{ TtotApurDiaCollection }

function TtotApurDiaCollection.Add: TtotApurDiaCollectionItem;
begin
  Result := Self.New;
end;

function TtotApurDiaCollection.GetItem(
  Index: Integer): TtotApurDiaCollectionItem;
begin
  Result := TtotApurDiaCollectionItem(inherited GetItem(Index));
end;

function TtotApurDiaCollection.New: TtotApurDiaCollectionItem;
begin
  Result := TtotApurDiaCollectionItem.Create;
  Self.Add(Result);
end;

procedure TtotApurDiaCollection.SetItem(Index: Integer;
  Value: TtotApurDiaCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TtotApurDiaCollectionItem }

constructor TtotApurDiaCollectionItem.Create;
begin
  FtotApurTribDia := TtotApurTribDia.Create;
end;

destructor TtotApurDiaCollectionItem.Destroy;
begin
  FtotApurTribDia.Free;

  inherited;
end;

end.
