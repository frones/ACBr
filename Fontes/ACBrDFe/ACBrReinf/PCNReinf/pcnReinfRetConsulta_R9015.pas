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

unit pcnReinfRetConsulta_R9015;

interface

uses
  SysUtils, Classes,
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$IFEND}
  ACBrBase,
  ACBrUtil.Strings, pcnAuxiliar, pcnConversao, pcnLeitor,
  pcnCommonReinf, pcnConversaoReinf;

type
  TRetConsulta_R9015 = class;
  TevtRetCons = class;
  TInfoRecEv = class;
  TRetornoEventosCollection = class;
  TRetornoEventosCollectionItem = class;
  TinfoCR_CNR = class;
  TtotApurMenCollection = class;
  TtotApurMenCollectionItem = class;
  TtotApurQuiCollection = class;
  TtotApurQuiCollectionItem = class;
  TtotApurDecCollection = class;
  TtotApurDecCollectionItem = class;
  TtotApurSemCollection = class;
  TtotApurSemCollectionItem = class;
  TtotApurDiaCollection = class;
  TtotApurDiaCollectionItem = class;
  TinfoTotalCR = class;

  { TRetConsulta_R9015 }
  TRetConsulta_R9015 = class(TObject)
  private
    FLeitor: TLeitor;
    FevtRetCons: TevtRetCons;
    FXML: String;
  public
    constructor Create;
    destructor Destroy; override;

    function LerXml: boolean;
    function SalvarINI: boolean;

    property Leitor: TLeitor read FLeitor write FLeitor;
    property evtRetCons: TevtRetCons read FevtRetCons write FevtRetCons;
    property XML: String read FXML;
  end;

  { TevtRetCons }
  TevtRetCons = class(TObject)
  private
    FId: String;

    FIdeEvento: TIdeEvento1;
    FIdeContri: TIdeContrib;
    FIdeStatus: TIdeStatus;
    FInfoRecEv: TInfoRecEv;
    FinfoCR_CNR: TinfoCR_CNR;
    FinfoTotalCR: TinfoTotalCR;
    FRetornoEventos: TRetornoEventosCollection;
  public
    constructor Create;
    destructor  Destroy; override;

    property Id: String read FId write FId;
    property IdeEvento: TIdeEvento1 read FIdeEvento write FIdeEvento;
    property IdeContri: TIdeContrib read FIdeContri write FIdeContri;
    property IdeStatus: TIdeStatus read FIdeStatus write FIdeStatus;
    property InfoRecEv: TInfoRecEv read FInfoRecEv write FInfoRecEv;
    property infoCR_CNR: TinfoCR_CNR read FinfoCR_CNR write FinfoCR_CNR;
    property infoTotalCR: TinfoTotalCR read FinfoTotalCR write FinfoTotalCR;
    property RetornoEventos: TRetornoEventosCollection read FRetornoEventos write FRetornoEventos;
  end;

  { TInfoRecEv }
  TInfoRecEv = class(TObject)
  private
    FnrRecArqBase: String;
    FnrProtLote: String;
    FdhProcess: TDateTime;
    FdhRecepcao: TDateTime;
    FtpEv: String;
    FidEv: String;
    Fhash: String;
    FfechRet: TtpFechRet;
  public
    property nrRecArqBase: String read FnrRecArqBase;
    property nrProtLote: String read FnrProtLote;
    property dhProcess: TDateTime read FdhProcess;
    property dhRecepcao: TDateTime read FdhRecepcao;
    property tpEv: String read FtpEv;
    property idEv: String read FidEv;
    property hash: String read Fhash;
    property fechRet: TtpFechRet read FfechRet;
  end;

  { TRetornoEventosCollection }
  TRetornoEventosCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TRetornoEventosCollectionItem;
    procedure SetItem(Index: Integer; Value: TRetornoEventosCollectionItem);
  public
    function Add: TRetornoEventosCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TRetornoEventosCollectionItem;

    property Items[Index: Integer]: TRetornoEventosCollectionItem read GetItem write SetItem;
  end;

  { TRetornoEventosCollectionItem }
  TRetornoEventosCollectionItem = class(TObject)
  private
    FId: String;
    FnrRecibo: String;
    FdtHoraRecebimento: String;
    FsituacaoEvento: String;
    FaplicacaoRecepcao: String;
    FiniValid: String;
    FfimValid: string;
    FnrProtocolo: String;
  public
    property id: String read FId write FId;
    property dtHoraRecebimento: String read FdtHoraRecebimento write FdtHoraRecebimento;
    property nrRecibo: String read FnrRecibo write FnrRecibo;
    property situacaoEvento: String read FsituacaoEvento write FsituacaoEvento;
    property aplicacaoRecepcao: String read FaplicacaoRecepcao write FaplicacaoRecepcao;
    property iniValid: String read FiniValid write FiniValid;
    property fimValid: String read FfimValid write FfimValid;
    property nrProtocolo : string read FnrProtocolo write FnrProtocolo;
  end;

  { TinfoCR_CNR }
  TinfoCR_CNR = class(TObject)
  private
    FnrRecArqBase: String;
    FindExistInfo: TindExistInfo;
    FidentEscritDCTF: String;

    FtotApurMen: TtotApurMenCollection;
    FtotApurQui: TtotApurQuiCollection;
    FtotApurDec: TtotApurDecCollection;
    FtotApurSem: TtotApurSemCollection;
    FtotApurDia: TtotApurDiaCollection;
  public
    constructor Create;
    destructor Destroy; override;

    property nrRecArqBase: String read FnrRecArqBase;
    property indExistInfo: TindExistInfo read FindExistInfo;
    property identEscritDCTF: String read FidentEscritDCTF;

    property totApurMen: TtotApurMenCollection read FtotApurMen;
    property totApurQui: TtotApurQuiCollection read FtotApurQui;
    property totApurDec: TtotApurDecCollection read FtotApurDec;
    property totApurSem: TtotApurSemCollection read FtotApurSem;
    property totApurDia: TtotApurDiaCollection read FtotApurDia;
  end;

  { TtotApurMenCollection }
  TtotApurMenCollection = class(TACBrObjectList)
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
    FvlrCRMenInf: double;
    FvlrCRMenCalc: double;
    FvlrCRMenDCTF: double;
    FvlrCRMenSuspInf: double;
    FvlrCRMenSuspCalc: double;
    FvlrCRMenSuspDCTF: double;
    FnatRend: string;
  public
    property CRMen: string read FCRMen;
    property vlrCRMenInf: double read FvlrCRMenInf;
    property vlrCRMenCalc: double read FvlrCRMenCalc;
    property vlrCRMenDCTF: double read FvlrCRMenDCTF;
    property vlrCRMenSuspInf: double read FvlrCRMenSuspInf;
    property vlrCRMenSuspCalc: double read FvlrCRMenSuspCalc;
    property vlrCRMenSuspDCTF: double read FvlrCRMenSuspDCTF;
    property natRend: string read FnatRend;
  end;

  { TtotApurQuiCollection }
  TtotApurQuiCollection = class(TACBrObjectList)
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
    FperApurQui: string;
    FCRQui: string;
    FvlrCRQuiInf: double;
    FvlrCRQuiCalc: double;
    FvlrCRQuiDCTF: double;
    FvlrCRQuiSuspInf: double;
    FvlrCRQuiSuspCalc: double;
    FvlrCRQuiSuspDCTF: double;
    FnatRend: string;
  public
    property perApurQui: string read FperApurQui;
    property CRQui: string read FCRQui;
    property vlrCRQuiInf: double read FvlrCRQuiInf;
    property vlrCRQuiCalc: double read FvlrCRQuiCalc;
    property vlrCRQuiDCTF: double read FvlrCRQuiDCTF;
    property vlrCRQuiSuspInf: double read FvlrCRQuiSuspInf;
    property vlrCRQuiSuspCalc: double read FvlrCRQuiSuspCalc;
    property vlrCRQuiSuspDCTF: double read FvlrCRQuiSuspDCTF;
    property natRend: string read FnatRend;
  end;

  { TtotApurDecCollection }
  TtotApurDecCollection = class(TACBrObjectList)
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
    FperApurDec: string;
    FCRDec: string;
    FvlrCRDecInf: double;
    FvlrCRDecCalc: double;
    FvlrCRDecDCTF: double;
    FvlrCRDecSuspInf: double;
    FvlrCRDecSuspCalc: double;
    FvlrCRDecSuspDCTF: double;
    FnatRend: string;
  public
    property perApurDec: string read FperApurDec;
    property CRDec: string read FCRDec;
    property vlrCRDecInf: double read FvlrCRDecInf;
    property vlrCRDecCalc: double read FvlrCRDecCalc;
    property vlrCRDecDCTF: double read FvlrCRDecDCTF;
    property vlrCRDecSuspInf: double read FvlrCRDecSuspInf;
    property vlrCRDecSuspCalc: double read FvlrCRDecSuspCalc;
    property vlrCRDecSuspDCTF: double read FvlrCRDecSuspDCTF;
    property natRend: string read FnatRend;
  end;

  { TtotApurSemCollection }
  TtotApurSemCollection = class(TACBrObjectList)
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
    FperApurSem: string;
    FCRSem: string;
    FvlrCRSemInf: double;
    FvlrCRSemCalc: double;
    FvlrCRSemDCTF: double;
    FvlrCRSemSuspInf: double;
    FvlrCRSemSuspCalc: double;
    FvlrCRSemSuspDCTF: double;
    FnatRend: string;
  public
    property perApurSem: string read FperApurSem;
    property CRSem: string read FCRSem;
    property vlrCRSemInf: double read FvlrCRSemInf;
    property vlrCRSemCalc: double read FvlrCRSemCalc;
    property vlrCRSemDCTF: double read FvlrCRSemDCTF;
    property vlrCRSemSuspInf: double read FvlrCRSemSuspInf;
    property vlrCRSemSuspCalc: double read FvlrCRSemSuspCalc;
    property vlrCRSemSuspDCTF: double read FvlrCRSemSuspDCTF;
    property natRend: string read FnatRend;
  end;

  { TtotApurDiaCollection }
  TtotApurDiaCollection = class(TACBrObjectList)
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
    FvlrCRDiaInf: double;
    FvlrCRDiaCalc: double;
    FvlrCRDiaDCTF: double;
    FvlrCRDiaSuspInf: double;
    FvlrCRDiaSuspCalc: double;
    FvlrCRDiaSuspDCTF: double;
    FnatRend: string;
  public
    property perApurDia: string read FperApurDia;
    property CRDia: string read FCRDia;
    property vlrCRDiaInf: double read FvlrCRDiaInf;
    property vlrCRDiaCalc: double read FvlrCRDiaCalc;
    property vlrCRDiaDCTF: double read FvlrCRDiaDCTF;
    property vlrCRDiaSuspInf: double read FvlrCRDiaSuspInf;
    property vlrCRDiaSuspCalc: double read FvlrCRDiaSuspCalc;
    property vlrCRDiaSuspDCTF: double read FvlrCRDiaSuspDCTF;
    property natRend: string read FnatRend;
  end;

  { TinfoTotalCR }
  TinfoTotalCR = class(TObject)
  private
    FtotApurMen: TtotApurMenCollection;
    FtotApurQui: TtotApurQuiCollection;
    FtotApurDec: TtotApurDecCollection;
    FtotApurSem: TtotApurSemCollection;
    FtotApurDia: TtotApurDiaCollection;
  public
    constructor Create;
    destructor Destroy; override;

    property totApurMen: TtotApurMenCollection read FtotApurMen;
    property totApurQui: TtotApurQuiCollection read FtotApurQui;
    property totApurDec: TtotApurDecCollection read FtotApurDec;
    property totApurSem: TtotApurSemCollection read FtotApurSem;
    property totApurDia: TtotApurDiaCollection read FtotApurDia;
  end;


implementation

uses
  IniFiles, DateUtils;

{ TRetConsulta_R9015 }

constructor TRetConsulta_R9015.Create;
begin
  FLeitor := TLeitor.Create;
  FevtRetCons := TevtRetCons.Create;
end;

destructor TRetConsulta_R9015.Destroy;
begin
  FLeitor.Free;
  FevtRetCons.Free;

  inherited;
end;

function TRetConsulta_R9015.LerXml: boolean;
var
  i: Integer;
  Ok: Boolean;
begin
  Result := True;
  try
    Leitor.Grupo := Leitor.Arquivo;

    FXML := Leitor.Arquivo;

    if (leitor.rExtrai(1, 'evtRetCons') <> '') then
    begin
      with evtRetCons do
      begin
        FId := Leitor.rAtributo('id=');

        if leitor.rExtrai(2, 'ideEvento') <> '' then
          IdeEvento.perApur := leitor.rCampo(tcStr, 'perApur');

        if leitor.rExtrai(2, 'ideContri') <> '' then
        begin
          IdeContri.TpInsc := StrToTpInscricao(Ok, leitor.rCampo(tcStr, 'tpInsc'));
          IdeContri.NrInsc := leitor.rCampo(tcStr, 'nrInsc');
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
          infoRecEv.FfechRet    := StrTotpFechRet(Ok, leitor.rCampo(tcStr, 'fechRet'));
        end;

        if leitor.rExtrai(2, 'infoCR_CNR') <> '' then
        begin
          with infoCR_CNR do
          begin
            FnrRecArqBase := leitor.rCampo(tcStr, 'nrRecArqBase');
            FindExistInfo := StrToindExistInfo(Ok, leitor.rCampo(tcStr, 'indExistInfo'));
            FidentEscritDCTF := leitor.rCampo(tcStr, 'identEscritDCTF');

            i := 0;
            while Leitor.rExtrai(3, 'totApurMen', '', i + 1) <> '' do
            begin
              totApurMen.New;
              totApurMen.Items[i].FCRMen            := leitor.rCampo(tcStr, 'CRMen');
              totApurMen.Items[i].FvlrCRMenInf      := leitor.rCampo(tcDe2, 'vlrCRMenInf');
              totApurMen.Items[i].FvlrCRMenCalc     := leitor.rCampo(tcDe2, 'vlrCRMenCalc');
              totApurMen.Items[i].FvlrCRMenDCTF     := leitor.rCampo(tcDe2, 'vlrCRMenDCTF');
              totApurMen.Items[i].FvlrCRMenSuspInf  := leitor.rCampo(tcDe2, 'vlrCRMenSuspInf');
              totApurMen.Items[i].FvlrCRMenSuspCalc := leitor.rCampo(tcDe2, 'vlrCRMenSuspCalc');
              totApurMen.Items[i].FvlrCRMenSuspDCTF := leitor.rCampo(tcDe2, 'vlrCRMenSuspDCTF');
              totApurMen.Items[i].FnatRend          := leitor.rCampo(tcStr, 'natRend');

              inc(i);
            end;

            i := 0;
            while Leitor.rExtrai(3, 'totApurQui', '', i + 1) <> '' do
            begin
              totApurQui.New;
              totApurQui.Items[i].FperApurQui       := leitor.rCampo(tcStr, 'perApurQui');
              totApurQui.Items[i].FCRQui            := leitor.rCampo(tcStr, 'CRQui');
              totApurQui.Items[i].FvlrCRQuiInf      := leitor.rCampo(tcDe2, 'vlrCRQuiInf');
              totApurQui.Items[i].FvlrCRQuiCalc     := leitor.rCampo(tcDe2, 'vlrCRQuiCalc');
              totApurQui.Items[i].FvlrCRQuiDCTF     := leitor.rCampo(tcDe2, 'vlrCRQuiDCTF');
              totApurQui.Items[i].FvlrCRQuiSuspInf  := leitor.rCampo(tcDe2, 'vlrCRQuiSuspInf');
              totApurQui.Items[i].FvlrCRQuiSuspCalc := leitor.rCampo(tcDe2, 'vlrCRQuiSuspCalc');
              totApurQui.Items[i].FvlrCRQuiSuspDCTF := leitor.rCampo(tcDe2, 'vlrCRQuiSuspDCTF');
              totApurQui.Items[i].FnatRend          := leitor.rCampo(tcStr, 'natRend');

              inc(i);
            end;

            i := 0;
            while Leitor.rExtrai(3, 'totApurDec', '', i + 1) <> '' do
            begin
              totApurDec.New;
              totApurDec.Items[i].FperApurDec       := leitor.rCampo(tcStr, 'perApurDec');
              totApurDec.Items[i].FCRDec            := leitor.rCampo(tcStr, 'CRDec');
              totApurDec.Items[i].FvlrCRDecInf      := leitor.rCampo(tcDe2, 'vlrCRDecInf');
              totApurDec.Items[i].FvlrCRDecCalc     := leitor.rCampo(tcDe2, 'vlrCRDecCalc');
              totApurDec.Items[i].FvlrCRDecDCTF     := leitor.rCampo(tcDe2, 'vlrCRDecDCTF');
              totApurDec.Items[i].FvlrCRDecSuspInf  := leitor.rCampo(tcDe2, 'vlrCRDecSuspInf');
              totApurDec.Items[i].FvlrCRDecSuspCalc := leitor.rCampo(tcDe2, 'vlrCRDecSuspCalc');
              totApurDec.Items[i].FvlrCRDecSuspDCTF := leitor.rCampo(tcDe2, 'vlrCRDecSuspDCTF');
              totApurDec.Items[i].FnatRend          := leitor.rCampo(tcStr, 'natRend');

              inc(i);
            end;

            i := 0;
            while Leitor.rExtrai(3, 'totApurSem', '', i + 1) <> '' do
            begin
              totApurSem.New;
              totApurSem.Items[i].FperApurSem       := leitor.rCampo(tcStr, 'perApurSem');
              totApurSem.Items[i].FCRSem            := leitor.rCampo(tcStr, 'CRSem');
              totApurSem.Items[i].FvlrCRSemInf      := leitor.rCampo(tcDe2, 'vlrCRSemInf');
              totApurSem.Items[i].FvlrCRSemCalc     := leitor.rCampo(tcDe2, 'vlrCRSemCalc');
              totApurSem.Items[i].FvlrCRSemDCTF     := leitor.rCampo(tcDe2, 'vlrCRSemDCTF');
              totApurSem.Items[i].FvlrCRSemSuspInf  := leitor.rCampo(tcDe2, 'vlrCRSemSuspInf');
              totApurSem.Items[i].FvlrCRSemSuspCalc := leitor.rCampo(tcDe2, 'vlrCRSemSuspCalc');
              totApurSem.Items[i].FvlrCRSemSuspDCTF := leitor.rCampo(tcDe2, 'vlrCRSemSuspDCTF');
              totApurSem.Items[i].FnatRend          := leitor.rCampo(tcStr, 'natRend');

              inc(i);
            end;

            i := 0;
            while Leitor.rExtrai(3, 'totApurDia', '', i + 1) <> '' do
            begin
              totApurDia.New;
              totApurDia.Items[i].FperApurDia       := leitor.rCampo(tcStr, 'perApurDia');
              totApurDia.Items[i].FCRDia            := leitor.rCampo(tcStr, 'CRDia');
              totApurDia.Items[i].FvlrCRDiaInf      := leitor.rCampo(tcDe2, 'vlrCRDiaInf');
              totApurDia.Items[i].FvlrCRDiaCalc     := leitor.rCampo(tcDe2, 'vlrCRDiaCalc');
              totApurDia.Items[i].FvlrCRDiaDCTF     := leitor.rCampo(tcDe2, 'vlrCRDiaDCTF');
              totApurDia.Items[i].FvlrCRDiaSuspInf  := leitor.rCampo(tcDe2, 'vlrCRDiaSuspInf');
              totApurDia.Items[i].FvlrCRDiaSuspCalc := leitor.rCampo(tcDe2, 'vlrCRDiaSuspCalc');
              totApurDia.Items[i].FvlrCRDiaSuspDCTF := leitor.rCampo(tcDe2, 'vlrCRDiaSuspDCTF');
              totApurDia.Items[i].FnatRend          := leitor.rCampo(tcStr, 'natRend');

              inc(i);
            end;
          end;
        end;

        if leitor.rExtrai(2, 'infoTotalCR') <> '' then
        begin
          with infoTotalCR do
          begin
            i := 0;
            while Leitor.rExtrai(3, 'totApurMen', '', i + 1) <> '' do
            begin
              totApurMen.New;
              totApurMen.Items[i].FCRMen            := leitor.rCampo(tcStr, 'CRMen');
              totApurMen.Items[i].FvlrCRMenDCTF     := leitor.rCampo(tcDe2, 'vlrCRMenDCTF');
              totApurMen.Items[i].FvlrCRMenSuspDCTF := leitor.rCampo(tcDe2, 'vlrCRMenSuspDCTF');

              inc(i);
            end;

            i := 0;
            while Leitor.rExtrai(3, 'totApurQui', '', i + 1) <> '' do
            begin
              totApurQui.New;
              totApurQui.Items[i].FperApurQui       := leitor.rCampo(tcStr, 'perApurQui');
              totApurQui.Items[i].FCRQui            := leitor.rCampo(tcStr, 'CRQui');
              totApurQui.Items[i].FvlrCRQuiDCTF     := leitor.rCampo(tcDe2, 'vlrCRQuiDCTF');
              totApurQui.Items[i].FvlrCRQuiSuspDCTF := leitor.rCampo(tcDe2, 'vlrCRQuiSuspDCTF');

              inc(i);
            end;

            i := 0;
            while Leitor.rExtrai(3, 'totApurDec', '', i + 1) <> '' do
            begin
              totApurDec.New;
              totApurDec.Items[i].FperApurDec       := leitor.rCampo(tcStr, 'perApurDec');
              totApurDec.Items[i].FCRDec            := leitor.rCampo(tcStr, 'CRDec');
              totApurDec.Items[i].FvlrCRDecDCTF     := leitor.rCampo(tcDe2, 'vlrCRDecDCTF');
              totApurDec.Items[i].FvlrCRDecSuspDCTF := leitor.rCampo(tcDe2, 'vlrCRDecSuspDCTF');

              inc(i);
            end;

            i := 0;
            while Leitor.rExtrai(3, 'totApurSem', '', i + 1) <> '' do
            begin
              totApurSem.New;
              totApurSem.Items[i].FperApurSem       := leitor.rCampo(tcStr, 'perApurSem');
              totApurSem.Items[i].FCRSem            := leitor.rCampo(tcStr, 'CRSem');
              totApurSem.Items[i].FvlrCRSemDCTF     := leitor.rCampo(tcDe2, 'vlrCRSemDCTF');
              totApurSem.Items[i].FvlrCRSemSuspDCTF := leitor.rCampo(tcDe2, 'vlrCRSemSuspDCTF');

              inc(i);
            end;

            i := 0;
            while Leitor.rExtrai(3, 'totApurDia', '', i + 1) <> '' do
            begin
              totApurDia.New;
              totApurDia.Items[i].FperApurDia       := leitor.rCampo(tcStr, 'perApurDia');
              totApurDia.Items[i].FCRDia            := leitor.rCampo(tcStr, 'CRDia');
              totApurDia.Items[i].FvlrCRDiaDCTF     := leitor.rCampo(tcDe2, 'vlrCRDiaDCTF');
              totApurDia.Items[i].FvlrCRDiaSuspDCTF := leitor.rCampo(tcDe2, 'vlrCRDiaSuspDCTF');

              inc(i);
            end;
          end;
        end;
      end;
    end
    else
    if (leitor.rExtrai(1, 'Reinf') <> '') then
    begin
      with evtRetCons do
      begin
        if leitor.rExtrai(2, 'ideStatus') <> '' then
        begin
          IdeStatus.cdRetorno   := leitor.rCampo(tcStr, 'cdRetorno');
          IdeStatus.descRetorno := leitor.rCampo(tcStr, 'descRetorno');

          i := 0;
          while Leitor.rExtrai(3, 'regOcorrs', '', i + 1) <> '' do
          begin
            IdeStatus.regOcorrs.New;
            IdeStatus.regOcorrs.Items[i].tpOcorr        := leitor.rCampo(tcInt, 'tpOcorr');
            IdeStatus.regOcorrs.Items[i].localErroAviso := leitor.rCampo(tcStr, 'localErroAviso');
            IdeStatus.regOcorrs.Items[i].codResp        := leitor.rCampo(tcStr, 'codResp');
            IdeStatus.regOcorrs.Items[i].dscResp        := leitor.rCampo(tcStr, 'dscResp');

            inc(i);
          end;
        end;

        if leitor.rExtrai(2, 'retornoEventos') <> '' then
        begin
          i := 0;
          while Leitor.rExtrai(3, 'evento', '', i + 1) <> '' do
          begin
            with RetornoEventos.New do
            begin
              id                := Leitor.rAtributo('id=');
              iniValid          := leitor.rCampo(tcStr, 'iniValid');
              fimValid          := leitor.rCampo(tcStr, 'fimValid');
              dtHoraRecebimento := leitor.rCampo(tcStr, 'dtHoraRecebimento');
              nrProtocolo       := leitor.rCampo(tcStr, 'nrProtocolo');
              nrRecibo          := leitor.rCampo(tcStr, 'nrRecibo');
              situacaoEvento    := leitor.rCampo(tcStr, 'situacaoEvento');
              aplicacaoRecepcao := leitor.rCampo(tcStr, 'aplicacaoRecepcao');

              inc(i);
            end;
          end;
        end;
      end;
    end;
  except
    Result := False;
  end;
end;

function TRetConsulta_R9015.SalvarINI: boolean;
var
  AIni: TMemIniFile;
begin
  Result := True;

  AIni := TMemIniFile.Create('');
  try
    with Self do
    begin
      (*
      with evtRetCons do
      begin
        sSecao := 'evtRetCons';
        AIni.WriteString(sSecao, 'Id', Id);

        sSecao := 'ideEvento';
        AIni.WriteString(sSecao, 'perApur', IdeEvento.perApur);

        sSecao := 'ideContri';
        AIni.WriteString(sSecao, 'tpInsc', TpInscricaoToStr(IdeContri.TpInsc));
        AIni.WriteString(sSecao, 'nrInsc', IdeContri.nrInsc);

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
        AIni.WriteString(sSecao, 'nrProtEntr', infoRecEv.nrProtEntr);
        AIni.WriteString(sSecao, 'dhProcess',  DateToStr(infoRecEv.dhProcess));
        AIni.WriteString(sSecao, 'dhRecepcao', DateToStr(infoRecEv.dhRecepcao));
        AIni.WriteString(sSecao, 'tpEv',       infoRecEv.tpEv);
        AIni.WriteString(sSecao, 'idEv',       infoRecEv.idEv);
        AIni.WriteString(sSecao, 'hash',       infoRecEv.hash);
        AIni.WriteString(sSecao, 'fechRet',    infoRecEv.fechRet);

        sSecao := 'infoTotalContrib';
        AIni.WriteString(sSecao, 'nrRecArqBase', infoTotalContrib.nrRecArqBase);
        AIni.WriteString(sSecao, 'indExistInfo', indExistInfoToStr(infoTotalContrib.indExistInfo));

        with infoTotalContrib do
        begin
          for i := 0 to RTom.Count -1 do
          begin
            sSecao := 'RTom' + IntToStrZero(I, 3);

            AIni.WriteString(sSecao, 'cnpjPrestador',    RTom.Items[i].cnpjPrestador);
            AIni.WriteFloat(sSecao, 'vlrTotalBaseRet',   RTom.Items[i].vlrTotalBaseRet);
            AIni.WriteFloat(sSecao, 'vlrTotalRetPrinc',  RTom.Items[i].vlrTotalRetPrinc);
            AIni.WriteFloat(sSecao, 'vlrTotalRetAdic',   RTom.Items[i].vlrTotalRetAdic);
            AIni.WriteFloat(sSecao, 'vlrTotalNRetPrinc', RTom.Items[i].vlrTotalNRetPrinc);
            AIni.WriteFloat(sSecao, 'vlrTotalNRetAdic',  RTom.Items[i].vlrTotalNRetAdic);

            // Versão 1.03.02
            for j := 0 to RTom.Items[i].infoCRTom.Count -1 do
            begin
              sSecao := 'infoCRTom' + IntToStrZero(I, 3) + IntToStrZero(J, 1);

              AIni.WriteString(sSecao, 'CRTom',    RTom.Items[i].infoCRTom.Items[J].CRTom);
              AIni.WriteFloat(sSecao, 'VlrCRTom',   RTom.Items[i].infoCRTom.Items[J].VlrCRTom);
              AIni.WriteFloat(sSecao, 'VlrCRTomSusp',  RTom.Items[i].infoCRTom.Items[J].VlrCRTomSusp);
            end;
          end;

          for i := 0 to RPrest.Count -1 do
          begin
            sSecao := 'RPrest' + IntToStrZero(I, 3);

            AIni.WriteString(sSecao, 'tpInscTomador',    TpInscricaoToStr(RPrest.Items[i].tpInscTomador));
            AIni.WriteString(sSecao, 'nrInscTomador',    RPrest.Items[i].nrInscTomador);
            AIni.WriteFloat(sSecao, 'vlrTotalBaseRet',   RPrest.Items[i].vlrTotalBaseRet);
            AIni.WriteFloat(sSecao, 'vlrTotalRetPrinc',  RPrest.Items[i].vlrTotalRetPrinc);
            AIni.WriteFloat(sSecao, 'vlrTotalRetAdic',   RPrest.Items[i].vlrTotalRetAdic);
            AIni.WriteFloat(sSecao, 'vlrTotalNRetPrinc', RPrest.Items[i].vlrTotalNRetPrinc);
            AIni.WriteFloat(sSecao, 'vlrTotalNRetAdic',  RPrest.Items[i].vlrTotalNRetAdic);
          end;

          for i := 0 to RRecRepAD.Count -1 do
          begin
            sSecao := 'RRecRepAD' + IntToStrZero(I, 3);

            AIni.WriteString(sSecao, 'cnpjAssocDesp', RRecRepAD.Items[i].cnpjAssocDesp);
            AIni.WriteFloat(sSecao, 'vlrTotalRep',    RRecRepAD.Items[i].vlrTotalRep);
            AIni.WriteFloat(sSecao, 'vlrTotalRet',    RRecRepAD.Items[i].vlrTotalRet);
            AIni.WriteFloat(sSecao, 'vlrTotalNRet',   RRecRepAD.Items[i].vlrTotalNRet);
          end;

          for i := 0 to RComl.Count -1 do
          begin
            sSecao := 'RComl' + IntToStrZero(I, 1);

            AIni.WriteFloat(sSecao, 'vlrCPApur',    RComl.Items[i].vlrCPApur);
            AIni.WriteFloat(sSecao, 'vlrRatApur',   RComl.Items[i].vlrRatApur);
            AIni.WriteFloat(sSecao, 'vlrSenarApur', RComl.Items[i].vlrSenarApur);
            AIni.WriteFloat(sSecao, 'vlrCPSusp',    RComl.Items[i].vlrCPSusp);
            AIni.WriteFloat(sSecao, 'vlrRatSusp',   RComl.Items[i].vlrRatSusp);
            AIni.WriteFloat(sSecao, 'vlrSenarSusp', RComl.Items[i].vlrSenarSusp);

            // Versão 1.03.02
            AIni.WriteString(sSecao, 'CRComl',       RComl.Items[i].CRComl);
            AIni.WriteFloat(sSecao, 'vlrCRComl',     RComl.Items[i].vlrCRComl);
            AIni.WriteFloat(sSecao, 'vlrCRComlSusp', RComl.Items[i].vlrCRComlSusp);
          end;

          for i := 0 to RCPRB.Count -1 do
          begin
            sSecao := 'RCPRB' + IntToStrZero(I, 1);

            AIni.WriteInteger(sSecao, 'codRec',       RCPRB.Items[i].codRec);
            AIni.WriteFloat(sSecao, 'vlrCPApurTotal', RCPRB.Items[i].vlrCPApurTotal);
            AIni.WriteFloat(sSecao, 'vlrCPRBSusp',    RCPRB.Items[i].vlrCPRBSusp);

            // Versão 1.03.02
            AIni.WriteString(sSecao, 'CRCPRB',       RCPRB.Items[i].CRCPRB);
            AIni.WriteFloat(sSecao, 'vlrCRCPRB',     RCPRB.Items[i].vlrCRCPRB);
            AIni.WriteFloat(sSecao, 'vlrCRCPRBSusp', RCPRB.Items[i].vlrCRCPRBSusp);
          end;
        end;
      end;
      *)
    end;
  finally
    AIni.Free;
  end;
end;

{ TevtRetCons }

constructor TevtRetCons.Create;
begin
  FIdeEvento      := TIdeEvento1.Create;
  FIdeContri      := TIdeContrib.Create;
  FIdeStatus      := TIdeStatus.Create;
  FInfoRecEv      := TInfoRecEv.Create;
  FinfoCR_CNR     := TinfoCR_CNR.Create;
  FinfoTotalCR    := TinfoTotalCR.Create;
  FRetornoEventos := TRetornoEventosCollection.Create;
end;

destructor TevtRetCons.Destroy;
begin
  FIdeEvento.Free;
  FIdeContri.Free;
  FIdeStatus.Free;
  FInfoRecEv.Free;
  FinfoCR_CNR.Free;
  FinfoTotalCR.Free;
  FRetornoEventos.Free;
  inherited;
end;
 
{ TRetornoEventosCollection }

function TRetornoEventosCollection.Add: TRetornoEventosCollectionItem;
begin
  Result := Self.New;
end;

function TRetornoEventosCollection.GetItem(
  Index: Integer): TRetornoEventosCollectionItem;
begin
  Result := TRetornoEventosCollectionItem(inherited Items[Index]);
end;

function TRetornoEventosCollection.New: TRetornoEventosCollectionItem;
begin
  Result := TRetornoEventosCollectionItem.Create;
  Self.Add(Result);
end;

procedure TRetornoEventosCollection.SetItem(Index: Integer;
  Value: TRetornoEventosCollectionItem);
begin
  inherited Items[Index] := Value;
end;
                    
{ TinfoCR_CNR }

constructor TinfoCR_CNR.Create;
begin
  FtotApurMen := TtotApurMenCollection.Create;
  FtotApurQui := TtotApurQuiCollection.Create;
  FtotApurDec := TtotApurDecCollection.Create;
  FtotApurSem := TtotApurSemCollection.Create;
  FtotApurDia := TtotApurDiaCollection.Create;
end;

destructor TinfoCR_CNR.Destroy;
begin
  FtotApurMen.Free;
  FtotApurQui.Free;
  FtotApurDec.Free;
  FtotApurSem.Free;
  FtotApurDia.Free;
  inherited;
end;

{ TtotApurMenCollection }

function TtotApurMenCollection.Add: TtotApurMenCollectionItem;
begin
  Result := Self.New;
end;

function TtotApurMenCollection.GetItem(
  Index: Integer): TtotApurMenCollectionItem;
begin
  Result := TtotApurMenCollectionItem(inherited items[Index]);
end;

function TtotApurMenCollection.New: TtotApurMenCollectionItem;
begin
  Result := TtotApurMenCollectionItem.Create;
  Self.Add(Result);
end;

procedure TtotApurMenCollection.SetItem(Index: Integer;
  Value: TtotApurMenCollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TtotApurQuiCollection }

function TtotApurQuiCollection.Add: TtotApurQuiCollectionItem;
begin
  Result := Self.New;
end;

function TtotApurQuiCollection.GetItem(
  Index: Integer): TtotApurQuiCollectionItem;
begin
  Result := TtotApurQuiCollectionItem(inherited  Items[Index]);
end;

function TtotApurQuiCollection.New: TtotApurQuiCollectionItem;
begin
  Result := TtotApurQuiCollectionItem.Create;
  Self.Add(Result);
end;

procedure TtotApurQuiCollection.SetItem(Index: Integer;
  Value: TtotApurQuiCollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TtotApurDecCollection }

function TtotApurDecCollection.Add: TtotApurDecCollectionItem;
begin
  Result := Self.New;
end;

function TtotApurDecCollection.GetItem(
  Index: Integer): TtotApurDecCollectionItem;
begin
  Result := TtotApurDecCollectionItem(inherited Items[Index]);
end;

function TtotApurDecCollection.New: TtotApurDecCollectionItem;
begin
  Result := TtotApurDecCollectionItem.Create;
  Self.Add(Result);
end;

procedure TtotApurDecCollection.SetItem(Index: Integer;
  Value: TtotApurDecCollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TtotApurSemCollection }

function TtotApurSemCollection.Add: TtotApurSemCollectionItem;
begin
  Result := Self.New;
end;

function TtotApurSemCollection.GetItem(
  Index: Integer): TtotApurSemCollectionItem;
begin
  Result := TtotApurSemCollectionItem( inherited Items[Index]);
end;

function TtotApurSemCollection.New: TtotApurSemCollectionItem;
begin
  Result := TtotApurSemCollectionItem.Create;
  Self.Add(Result);
end;

procedure TtotApurSemCollection.SetItem(Index: Integer;
  Value: TtotApurSemCollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TtotApurDiaCollection }

function TtotApurDiaCollection.Add: TtotApurDiaCollectionItem;
begin
  Result := Self.New;
end;

function TtotApurDiaCollection.GetItem(
  Index: Integer): TtotApurDiaCollectionItem;
begin
  Result := TtotApurDiaCollectionItem(inherited Items[Index]);
end;

function TtotApurDiaCollection.New: TtotApurDiaCollectionItem;
begin
  Result := TtotApurDiaCollectionItem.Create;
  Self.Add(Result);
end;

procedure TtotApurDiaCollection.SetItem(Index: Integer;
  Value: TtotApurDiaCollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TinfoTotalCR }

constructor TinfoTotalCR.Create;
begin
  FtotApurMen := TtotApurMenCollection.Create;
  FtotApurQui := TtotApurQuiCollection.Create;
  FtotApurDec := TtotApurDecCollection.Create;
  FtotApurSem := TtotApurSemCollection.Create;
  FtotApurDia := TtotApurDiaCollection.Create;
end;

destructor TinfoTotalCR.Destroy;
begin
  FtotApurMen.Free;
  FtotApurQui.Free;
  FtotApurDec.Free;
  FtotApurSem.Free;
  FtotApurDia.Free;

  inherited;
end;

end.
