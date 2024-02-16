{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Rafael Teno Dias                                }
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

unit ACBrLibReinfRespostas;

interface

uses
  Classes, SysUtils, StrUtils, ACBrLibResposta,
  ACBrUtil.Base, ACBrReinf, ACBrLibReinfConsts,
  pcnConversaoReinf, pcnReinfRetEventos, pcnCommonReinf,
  pcnReinfR9005, pcnReinfRetConsulta_R9015,
  pcnReinfRetConsulta_R9011;

type

  { TPadraoReinfResposta }

  TPadraoReinfResposta = class(TACBrLibRespostaBase)
  private
    FCodigo: String;
    FMensagem: String;
  public
    constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo;
      const AFormato: TACBrLibCodificacao); reintroduce;

  published
    property Codigo: String read FCodigo write FCodigo;
    property Mensagem: String read FMensagem write FMensagem;
  end;

  { TEnvioResposta }

  TEnvioResposta = class(TPadraoReinfResposta)
  private
    FId: String;
  public
    constructor Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;

    procedure Processar(const AACBrReinf: TACBrReinf);
  published
    property Id: String read FId write FId;
  end;

  { TEnvioRespostaideTransmissor }

  TEnvioRespostaideTransmissor = class(TPadraoReinfResposta)
  private
    FIdTransmissor: String;
  public
    constructor Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;

    procedure Processar(const AACBrReinf: TACBrReinf);
  published
    property IdTransmissor: String read FIdTransmissor write FIdTransmissor;
  end;

  { TEnvioRespostastatus }

  TEnvioRespostastatus = class(TPadraoReinfResposta)
  private
    FcdStatus: Integer;
    FdescRetorno: String;
  public
    constructor Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;

    procedure Processar(const AACBrReinf: TACBrReinf);
  published
    property cdStatus: Integer read FcdStatus write FcdStatus;
    property descRetorno: String read FdescRetorno write FdescRetorno;
  end;

  { TEnvioRespostadadosRecepcaoLote }

  TEnvioRespostadadosRecepcaoLote = class(TACBrLibRespostaBase) //TPadraoReinfResposta)
  private
    FdhRecepcao: TDateTime;
    FversaoAplicativoRecepcao: String;
    FprotocoloEnvio: String;
  public
    constructor Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;

    procedure Processar(const AItem: TRetEnvioLote);
  published
    property dhRecepcao: TDateTime read FdhRecepcao write FdhRecepcao;
    property versaoAplicativoRecepcao: String read FversaoAplicativoRecepcao write FversaoAplicativoRecepcao;
    property protocoloEnvio: String read FprotocoloEnvio write FprotocoloEnvio;
  end;

  { TEnvioRespostaOcorrencias }
  TEnvioRespostaOcorrencias = class(TPadraoReinfResposta)
  private
    Ftipo: Byte;
    FlocalizacaoErroAviso: String;
    Fcodigo: Integer;
    Fdescricao: String;
  public
    constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo;
      const AFormato: TACBrLibCodificacao); reintroduce;

    procedure Processar(const AACBrReinf: TACBrReinf; const ACont: Integer);
  published
    property tipo: Byte read Ftipo write Ftipo;
    property localizacaoErroAviso: String read FlocalizacaoErroAviso write FlocalizacaoErroAviso;
    property codigo: Integer read Fcodigo write Fcodigo;
    property descricao: String read Fdescricao write Fdescricao;
  end;

  { TEnvioRespostaevento }
  TEnvioRespostaevento = class(TPadraoReinfResposta)
  private
    FId: String;
  public
    constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo;
      const AFormato: TACBrLibCodificacao); reintroduce;

    procedure Processar(const AACBrReinf: TACBrReinf; const ACont: Integer);
  published
    property Id: String read FId write FId;
  end;

  { TEnvioRespostaRRecEspetDesp }
  TEnvioRespostaRRecEspetDesp = class(TPadraoReinfResposta)
  private
    FCRRecEspetDesp: String;
    FvlrCPApurTotal: Double;
    FvlrCPSuspTotal: Double;
    FvlrCRRecEspetDesp: Double;
    FvlrCRRecEspetDespSusp: Double;
    FvlrReceitaTotal: Double;
  public
    constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo;
      const AFormato: TACBrLibCodificacao); reintroduce;

    procedure Processar(const AItem: TeventoCollectionItem);
  published
    property vlrReceitaTotal: Double read FvlrReceitaTotal write FvlrReceitaTotal;
    property vlrCPApurTotal: Double read FvlrCPApurTotal write FvlrCPApurTotal;
    property vlrCPSuspTotal: Double read FvlrCPSuspTotal write FvlrCPSuspTotal;
    property CRRecEspetDesp: String read FCRRecEspetDesp write FCRRecEspetDesp;
    property vlrCRRecEspetDesp: Double read FvlrCRRecEspetDesp write FvlrCRRecEspetDesp;
    property vlrCRRecEspetDespSusp: Double read FvlrCRRecEspetDespSusp write FvlrCRRecEspetDespSusp;
  end;

  { TConsultaResposta }
  TConsultaResposta = class(TPadraoReinfResposta)
  private
    FId: String;
  public
    constructor Create(const ATipo: TACBrLibRespostaTipo;
      const AFormato: TACBrLibCodificacao; const ASessao: String = ''); reintroduce;

    procedure Processar(const AId: String);
  published
    property Id: String read FId write FId;
  end;

  { TRespostaideEvento }
  TRespostaideEvento = class(TPadraoReinfResposta)
  private
    FperApur: String;
  public
    constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo;
      const AFormato: TACBrLibCodificacao); reintroduce;

    procedure Processar(const AItem: TIdeEvento1);
  published
    property perApur: String read FperApur write FperApur;
  end;

  { TRespostaideContri }
  TRespostaideContri = class(TPadraoReinfResposta)
  private
    FtpInsc: String;
    FnrInsc: String;
  public
    constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo;
      const AFormato: TACBrLibCodificacao); reintroduce;

    procedure Processar(const AItem: TideContrib; const AStatus: TStatus = nil);
  published
    property tpInsc: String read FtpInsc write FtpInsc;
    property nrInsc: String read FnrInsc write FnrInsc;
  end;

  { TRespostaideStatus }
  TRespostaideStatus = class(TPadraoReinfResposta)
  private
    FcdRetorno: String;
    FdescRetorno: String;
  public
    constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo;
      const AFormato: TACBrLibCodificacao); reintroduce;

    procedure Processar(const AItem: TIdeStatus);
    procedure ProcessarInfoTotalContrib(const AACBrReinf: TACBrReinf);
  published
    property cdRetorno: String read FcdRetorno write FcdRetorno;
    property descRetorno: String read FdescRetorno write FdescRetorno;
  end;

  { TRespostaregOcorrs }

  TRespostaregOcorrs = class(TPadraoReinfResposta)
  private
    FtpOcorr: integer;
    FlocalErroAviso: String;
    FcodResp: String;
    FdscResp: String;
  public
    constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo;
      const AFormato: TACBrLibCodificacao); reintroduce;

    procedure Processar(const AContX: Integer; const AItem: TIdeStatus);
    procedure ProcessarInfoTotalContrib(const AACBrReinf: TACBrReinf;
      const ACont: Integer);
  published
    property tpOcorr: integer read FtpOcorr write FtpOcorr;
    property localErroAviso: String read FlocalErroAviso write FlocalErroAviso;
    property codResp: String read FcodResp write FcodResp;
    property dscResp: String read FdscResp write FdscResp;
  end;

  { TRespostainfoRecEv }
  TRespostainfoRecEv = class(TPadraoReinfResposta)
  private
    FnrRecArqBase: String;
    FnrProtLote: String;
    FdhRecepcao: TDateTime;
    FdhProcess: TDateTime;
    Fhash: String;
    FidEv: String;
    FnrProtEntr: String;
    FtpEv: String;
  public
    constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo;
      const AFormato: TACBrLibCodificacao); reintroduce;

    procedure Processar(const AItem: TInfoRecEv);
  published
    property nrRecArqBase: String read FnrRecArqBase write FnrRecArqBase;
    property nrProtLote: String read FnrProtLote write FnrProtLote;
    property dhRecepcao: TDateTime read FdhRecepcao write FdhRecepcao;
    property nrProtEntr: String read FnrProtEntr write FnrProtEntr;
    property dhProcess: TDateTime read FdhProcess write FdhProcess;
    property tpEv: String read FtpEv write FtpEv;
    property idEv: String read FidEv write FidEv;
    property hash: String read Fhash write Fhash;
  end;

  { TRespostainfoTotal_infoTotalContrib }
  TRespostainfoTotal_infoTotalContrib = class(TPadraoReinfResposta)
  private
    FindExistInfo: String;
    FnrRecArqBase: String;
  public
    constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo;
      const AFormato: TACBrLibCodificacao); reintroduce;

    procedure Processar(const AACBrReinf: TACBrReinf; const ACont: Integer);
    procedure ProcessarInfoTotalContrib(const AACBrReinf: TACBrReinf);
  published
    property nrRecArqBase: String read FnrRecArqBase write FnrRecArqBase;
    property indExistInfo: String read FindExistInfo write FindExistInfo;
  end;

  { TRespostaRTom }

  TRespostaRTom = class(TPadraoReinfResposta)
  private
    FcnpjPrestador: String;
    FvlrTotalBaseRet: Double;
    FvlrTotalNRetAdic: Double;
    FvlrTotalNRetPrinc: Double;
    FvlrTotalRetAdic: Double;
    FvlrTotalRetPrinc: Double;
  public
    constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo;
      const AFormato: TACBrLibCodificacao); reintroduce;

    procedure Processar(const AItem: TeventoCollectionItem);
    procedure ProcessarInfoTotalContrib(const AACBrReinf: TACBrReinf;
      const ACont: Integer);
  published
    property cnpjPrestador: String read FcnpjPrestador write FcnpjPrestador;
    property vlrTotalBaseRet: Double read FvlrTotalBaseRet write FvlrTotalBaseRet;
    property vlrTotalRetPrinc: Double read FvlrTotalRetPrinc write FvlrTotalRetPrinc;
    property vlrTotalRetAdic: Double read FvlrTotalRetAdic write FvlrTotalRetAdic;
    property vlrTotalNRetPrinc: Double read FvlrTotalNRetPrinc write FvlrTotalNRetPrinc;
    property vlrTotalNRetAdic: Double read FvlrTotalNRetAdic write FvlrTotalNRetAdic;
  end;

  { TRespostainfoCRTom }

  TRespostainfoCRTom = class(TPadraoReinfResposta)
  private
    FCRTom: string;
    FVlrCRTom: Double;
    FVlrCRTomSusp: Double;
  public
    constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo;
      const AFormato: TACBrLibCodificacao); reintroduce;

    procedure Processar(const ACont2: Integer; const AItem: TeventoCollectionItem);
    procedure ProcessarInfoTotalContrib(const AACBrReinf: TACBrReinf; const ACont, ACont2: Integer);
  published
    property CRTom: string read FCRTom write FCRTom;
    property VlrCRTom: Double read FVlrCRTom write FVlrCRTom;
    property VlrCRTomSusp: Double read FVlrCRTomSusp write FVlrCRTomSusp;
  end;

  { TRespostaRPrest }

  TRespostaRPrest = class(TPadraoReinfResposta)
  private
    FnrInscTomador: String;
    FtpInscTomador: String;
    FvlrTotalBaseRet: Double;
    FvlrTotalNRetAdic: Double;
    FvlrTotalNRetPrinc: Double;
    FvlrTotalRetAdic: Double;
    FvlrTotalRetPrinc: Double;
  public
    constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo;
      const AFormato: TACBrLibCodificacao); reintroduce;

    procedure Processar(const AItem: TeventoCollectionItem);
    procedure ProcessarInfoTotalContrib(const AACBrReinf: TACBrReinf; const ACont: Integer);
  published
    property tpInscTomador: String read FtpInscTomador write FtpInscTomador;
    property nrInscTomador: String read FnrInscTomador write FnrInscTomador;
    property vlrTotalBaseRet: Double read FvlrTotalBaseRet write FvlrTotalBaseRet;
    property vlrTotalRetPrinc: Double read FvlrTotalRetPrinc write FvlrTotalRetPrinc;
    property vlrTotalRetAdic: Double read FvlrTotalRetAdic write FvlrTotalRetAdic;
    property vlrTotalNRetPrinc: Double read FvlrTotalNRetPrinc write FvlrTotalNRetPrinc;
    property vlrTotalNRetAdic: Double read FvlrTotalNRetAdic write FvlrTotalNRetAdic;
  end;

  { TRespostaRRecRepAD }

  TRespostaRRecRepAD = class(TPadraoReinfResposta)
  private
    FcnpjAssocDesp: string;
    FCRRecRepAD: String;
    FvlrCRRecRepAD: Double;
    FvlrCRRecRepADSusp: Double;
    FvlrTotalNRet: Double;
    FvlrTotalRep: Double;
    FvlrTotalRet: Double;
  public
    constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo;
      const AFormato: TACBrLibCodificacao); reintroduce;

    procedure Processar(const ACont2: Integer; const AItem: TeventoCollectionItem);
    procedure ProcessarInfoTotalContrib(const AACBrReinf: TACBrReinf; const ACont: Integer);
  published
    property cnpjAssocDesp: string read FcnpjAssocDesp write FcnpjAssocDesp;
    property vlrTotalRep: Double read FvlrTotalRep write FvlrTotalRep;
    property vlrTotalRet: Double read FvlrTotalRet write FvlrTotalRet;
    property vlrTotalNRet: Double read FvlrTotalNRet write FvlrTotalNRet;
    property CRRecRepAD: String read FCRRecRepAD write FCRRecRepAD;
    property vlrCRRecRepAD: Double read FvlrCRRecRepAD write FvlrCRRecRepAD;
    property vlrCRRecRepADSusp: Double read FvlrCRRecRepADSusp write FvlrCRRecRepADSusp;
  end;

  { TRespostaRComl }

  TRespostaRComl = class(TPadraoReinfResposta)
  private
    FCRComl: String;
    FvlrCPApur: Double;
    FvlrCPSusp: Double;
    FvlrCRComl: Double;
    FvlrCRComlSusp: Double;
    FvlrRatApur: Double;
    FvlrRatSusp: Double;
    FvlrSenarApur: Double;
    FvlrSenarSusp: Double;
  public
    constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo;
      const AFormato: TACBrLibCodificacao); reintroduce;

    procedure Processar(const ACont2: Integer; const AItem: TeventoCollectionItem);
    procedure ProcessarInfoTotalContrib(const AACBrReinf: TACBrReinf; const ACont: Integer);
  published
    property vlrCPApur: Double read FvlrCPApur write FvlrCPApur;
    property vlrRatApur: Double read FvlrRatApur write FvlrRatApur;
    property vlrSenarApur: Double read FvlrSenarApur write FvlrSenarApur;
    property vlrCPSusp: Double read FvlrCPSusp write FvlrCPSusp;
    property vlrRatSusp: Double read FvlrRatSusp write FvlrRatSusp;
    property vlrSenarSusp: Double read FvlrSenarSusp write FvlrSenarSusp;
    property CRComl: String read FCRComl write FCRComl;
    property vlrCRComl: Double read FvlrCRComl write FvlrCRComl;
    property vlrCRComlSusp: Double read FvlrCRComlSusp write FvlrCRComlSusp;
  end;

  { TRespostaRAquis }

  TRespostaRAquis = class(TPadraoReinfResposta)
  private
    FCRAquis: String;
    FvlrCRAquis: Double;
    FvlrCRAquisSusp: Double;
  public
    constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo;
      const AFormato: TACBrLibCodificacao); reintroduce;

    procedure Processar(const ACont2: Integer; const AItem: TeventoCollectionItem);
  published
    property CRAquis: String read FCRAquis write FCRAquis;
    property vlrCRAquis: Double read FvlrCRAquis write FvlrCRAquis;
    property vlrCRAquisSusp: Double read FvlrCRAquisSusp write FvlrCRAquisSusp;
  end;

  { TRespostaRCPRB }

  TRespostaRCPRB = class(TPadraoReinfResposta)
  private
    FcodRec: Integer;
    FCRCPRB: String;
    FvlrCPApurTotal: Double;
    FvlrCPRBSusp: Double;
    FvlrCRCPRB: Double;
    FvlrCRCPRBSusp: Double;
  public
    constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo;
      const AFormato: TACBrLibCodificacao); reintroduce;

    procedure Processar(const ACont2: Integer; const AItem: TeventoCollectionItem);
    procedure ProcessarInfoTotalContrib(const AACBrReinf: TACBrReinf; const ACont: Integer);
  published
    property codRec: Integer read FcodRec write FcodRec;
    property vlrCPApurTotal: Double read FvlrCPApurTotal write FvlrCPApurTotal;
    property vlrCPRBSusp: Double read FvlrCPRBSusp write FvlrCPRBSusp;
    property CRCPRB: String read FCRCPRB write FCRCPRB;
    property vlrCRCPRB: Double read FvlrCRCPRB write FvlrCRCPRB;
    property vlrCRCPRBSusp: Double read FvlrCRCPRBSusp write FvlrCRCPRBSusp;
  end;

  { TEnvioRespostaevtTotal }
  TEnvioRespostaevtTotal = class(TPadraoReinfResposta)
  private
    FId: String;
  public
    constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo;
      const AFormato: TACBrLibCodificacao); reintroduce;

    procedure Processar(const AACBrReinf: TACBrReinf; const ACont: Integer);
  published
    property Id: String read FId write FId;
  end;

  { TRespostaEventoRecibo }
  TRespostaEventoRecibo = class(TPadraoReinfResposta)
  private
    FId: String;
    FInicioValidade: String;
    FDataHoraReceb: String;
    FNrRecibo: String;
    FSituacaoEvento: String;
    FAplicacaoRecepcao: String;
  public
    constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo;
      const AFormato: TACBrLibCodificacao); reintroduce;

    procedure ProcessarInfoTotalContrib(const AACBrReinf: TACBrReinf;
      const ACont: Integer);
  published
    property Id: String read FId write FId;
    property InicioValidade: String read FInicioValidade write FInicioValidade;
    property DataHoraReceb: String read FDataHoraReceb write FDataHoraReceb;
    property NrRecibo: String read FNrRecibo write FNrRecibo;
    property SituacaoEvento: String read FSituacaoEvento write FSituacaoEvento;
    property AplicacaoRecepcao: String read FAplicacaoRecepcao write FAplicacaoRecepcao;
  end;

  { TEnvioRespostaideEstab }
  TEnvioRespostaideEstab = class(TACBrLibRespostaBase) //TPadraoReinfResposta)
  private
    FtpInsc: String;
    FnrInsc: String;
    FnrInscBenef: String;
    FnmBenef: String;
    FideEvtAdic: String;
  public
    constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo;
      const AFormato: TACBrLibCodificacao); reintroduce;

    procedure Processar(const AItem: TideEstab);
  published
    property tpInsc: String read FtpInsc write FtpInsc;
    property nrInsc: String read FnrInsc write FnrInsc;
    property nrInscBenef: String read FnrInscBenef write FnrInscBenef;
    property nmBenef: String read FnmBenef write FnmBenef;
    property ideEvtAdic: String read FideEvtAdic write FideEvtAdic;
  end;

  { TEnvioRespostatotApurMen }
  TEnvioRespostatotApurMen = class(TACBrLibRespostaBase) //TPadraoReinfResposta)
  private
    FCRMen: string;
    FvlrBaseCRMen: double;
    FvlrBaseCRMenSusp: double;
    FnatRend: string;
  public
    constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo;
      const AFormato: TACBrLibCodificacao); reintroduce;

    procedure Processar(const AACBrReinf: TACBrReinf; const ACont, ACont2: Integer);
  published
    property CRMen: string read FCRMen write FCRMen;
    property vlrBaseCRMen: double read FvlrBaseCRMen write FvlrBaseCRMen;
    property vlrBaseCRMenSusp: double read FvlrBaseCRMenSusp write FvlrBaseCRMenSusp;
    property natRend: string read FnatRend write FnatRend;
  end;

  { TEnvioRespostatotApurTribMen }
  TEnvioRespostatotApurTribMen = class(TACBrLibRespostaBase) //TPadraoReinfResposta)
  private
    FvlrCRMenInf: double;
    FvlrCRMenCalc: double;
    FvlrCRMenSuspInf: double;
    FvlrCRMenSuspCalc: double;
  public
    constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo;
      const AFormato: TACBrLibCodificacao); reintroduce;

    procedure Processar(const AACBrReinf: TACBrReinf; const ACont, ACont2: Integer);
  published
    property vlrCRMenInf: double read FvlrCRMenInf write FvlrCRMenInf;
    property vlrCRMenCalc: double read FvlrCRMenCalc write FvlrCRMenCalc;
    property vlrCRMenSuspInf: double read FvlrCRMenSuspInf write FvlrCRMenSuspInf;
    property vlrCRMenSuspCalc: double read FvlrCRMenSuspCalc write FvlrCRMenSuspCalc;
  end;

  { TEnvioRespostatotApurQui }
  TEnvioRespostatotApurQui = class(TACBrLibRespostaBase) //TPadraoReinfResposta)
  private
    FperApurQui: string;
    FCRQui: string;
    FvlrBaseCRQui: double;
    FvlrBaseCRQuiSusp: double;
    FnatRend: string;
  public
    constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo;
      const AFormato: TACBrLibCodificacao); reintroduce;

    procedure Processar(const AACBrReinf: TACBrReinf; const ACont, ACont2: Integer);
  published
    property perApurQui: string read FperApurQui write FperApurQui;
    property CRQui: string read FCRQui write FCRQui;
    property vlrBaseCRQui: double read FvlrBaseCRQui write FvlrBaseCRQui;
    property vlrBaseCRQuiSusp: double read FvlrBaseCRQuiSusp write FvlrBaseCRQuiSusp;
    property natRend: string read FnatRend write FnatRend;
  end;

  { TEnvioRespostatotApurTribQui }
  TEnvioRespostatotApurTribQui = class(TACBrLibRespostaBase) //TPadraoReinfResposta)
  private
    FvlrCRQuiInf: double;
    FvlrCRQuiCalc: double;
    FvlrCRQuiSuspInf: double;
    FvlrCRQuiSuspCalc: double;
  public
    constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo;
      const AFormato: TACBrLibCodificacao); reintroduce;

    procedure Processar(const AACBrReinf: TACBrReinf; const ACont, ACont2: Integer);
  published
    property vlrCRQuiInf: double read FvlrCRQuiInf write FvlrCRQuiInf;
    property vlrCRQuiCalc: double read FvlrCRQuiCalc write FvlrCRQuiCalc;
    property vlrCRQuiSuspInf: double read FvlrCRQuiSuspInf write FvlrCRQuiSuspInf;
    property vlrCRQuiSuspCalc: double read FvlrCRQuiSuspCalc write FvlrCRQuiSuspCalc;
  end;

  { TEnvioRespostatotApurDec }
  TEnvioRespostatotApurDec = class(TACBrLibRespostaBase) //TPadraoReinfResposta)
  private
    FperApurDec: string;
    FCRDec: string;
    FvlrBaseCRDec: double;
    FvlrBaseCRDecSusp: double;
    FnatRend: string;
  public
    constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo;
      const AFormato: TACBrLibCodificacao); reintroduce;

    procedure Processar(const AACBrReinf: TACBrReinf; const ACont, ACont2: Integer);
  published
    property perApurDec: string read FperApurDec write FperApurDec;
    property CRDec: string read FCRDec write FCRDec;
    property vlrBaseCRDec: double read FvlrBaseCRDec write FvlrBaseCRDec;
    property vlrBaseCRDecSusp: double read FvlrBaseCRDecSusp write FvlrBaseCRDecSusp;
    property natRend: string read FnatRend write FnatRend;
  end;

  { TEnvioRespostatotApurTribDec }
  TEnvioRespostatotApurTribDec = class(TACBrLibRespostaBase) //TPadraoReinfResposta)
  private
    FvlrCRDecInf: double;
    FvlrCRDecCalc: double;
    FvlrCRDecSuspInf: double;
    FvlrCRDecSuspCalc: double;
  public
    constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo;
      const AFormato: TACBrLibCodificacao); reintroduce;

    procedure Processar(const AACBrReinf: TACBrReinf; const ACont, ACont2: Integer);
  published
    property vlrCRDecInf: double read FvlrCRDecInf write FvlrCRDecInf;
    property vlrCRDecCalc: double read FvlrCRDecCalc write FvlrCRDecCalc;
    property vlrCRDecSuspInf: double read FvlrCRDecSuspInf write FvlrCRDecSuspInf;
    property vlrCRDecSuspCalc: double read FvlrCRDecSuspCalc write FvlrCRDecSuspCalc;
  end;

  { TEnvioRespostatotApurSem }
  TEnvioRespostatotApurSem = class(TACBrLibRespostaBase) //TPadraoReinfResposta)
  private
    FperApurSem: string;
    FCRSem: string;
    FvlrBaseCRSem: double;
    FvlrBaseCRSemSusp: double;
    FnatRend: string;
  public
    constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo;
      const AFormato: TACBrLibCodificacao); reintroduce;

    procedure Processar(const AACBrReinf: TACBrReinf; const ACont, ACont2: Integer);
  published
    property perApurSem: string read FperApurSem write FperApurSem;
    property CRSem: string read FCRSem write FCRSem;
    property vlrBaseCRSem: double read FvlrBaseCRSem write FvlrBaseCRSem;
    property vlrBaseCRSemSusp: double read FvlrBaseCRSemSusp write FvlrBaseCRSemSusp;
    property natRend: string read FnatRend write FnatRend;
  end;

  { TEnvioRespostatotApurTribSem }
  TEnvioRespostatotApurTribSem = class(TACBrLibRespostaBase) //TPadraoReinfResposta)
  private
    FvlrCRSemInf: double;
    FvlrCRSemCalc: double;
    FvlrCRSemSuspInf: double;
    FvlrCRSemSuspCalc: double;
  public
    constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo;
      const AFormato: TACBrLibCodificacao); reintroduce;

    procedure Processar(const AACBrReinf: TACBrReinf; const ACont, ACont2: Integer);
  published
    property vlrCRSemInf: double read FvlrCRSemInf write FvlrCRSemInf;
    property vlrCRSemCalc: double read FvlrCRSemCalc write FvlrCRSemCalc;
    property vlrCRSemSuspInf: double read FvlrCRSemSuspInf write FvlrCRSemSuspInf;
    property vlrCRSemSuspCalc: double read FvlrCRSemSuspCalc write FvlrCRSemSuspCalc;
  end;

  { TEnvioRespostatotApurDia }
  TEnvioRespostatotApurDia = class(TACBrLibRespostaBase) //TPadraoReinfResposta)
  private
    FperApurDia: string;
    FCRDia: string;
    FvlrBaseCRDia: double;
    FvlrBaseCRDiaSusp: double;
    FnatRend: string;
  public
    constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo;
      const AFormato: TACBrLibCodificacao); reintroduce;

    procedure Processar(const AACBrReinf: TACBrReinf; const ACont, ACont2: Integer);
  published
    property perApurDia: string read FperApurDia write FperApurDia;
    property CRDia: string read FCRDia write FCRDia;
    property vlrBaseCRDia: double read FvlrBaseCRDia write FvlrBaseCRDia;
    property vlrBaseCRDiaSusp: double read FvlrBaseCRDiaSusp write FvlrBaseCRDiaSusp;
    property natRend: string read FnatRend write FnatRend;
  end;

  { TEnvioRespostatotApurTribDia }
  TEnvioRespostatotApurTribDia = class(TACBrLibRespostaBase) //TPadraoReinfResposta)
  private
    FvlrCRDiaInf: double;
    FvlrCRDiaCalc: double;
    FvlrCRDiaSuspInf: double;
    FvlrCRDiaSuspCalc: double;
  public
    constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo;
      const AFormato: TACBrLibCodificacao); reintroduce;

    procedure Processar(const AACBrReinf: TACBrReinf; const ACont, ACont2: Integer);
  published
    property vlrCRDiaInf: double read FvlrCRDiaInf write FvlrCRDiaInf;
    property vlrCRDiaCalc: double read FvlrCRDiaCalc write FvlrCRDiaCalc;
    property vlrCRDiaSuspInf: double read FvlrCRDiaSuspInf write FvlrCRDiaSuspInf;
    property vlrCRDiaSuspCalc: double read FvlrCRDiaSuspCalc write FvlrCRDiaSuspCalc;
  end;

  { TConsultaRespostainfoCR_CNR }
  TConsultaRespostainfoCR_CNR = class(TACBrLibRespostaBase) //TPadraoReinfResposta)
  private
    FindExistInfo: String;
    FidentEscritDCTF: String;
  public
    constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo;
      const AFormato: TACBrLibCodificacao); reintroduce;

    procedure Processar(const AItem: TinfoCR_CNR);
  published
    property indExistInfo: String read FindExistInfo write FindExistInfo;
    property identEscritDCTF: String read FidentEscritDCTF write FidentEscritDCTF;
  end;

  { TConsultaRespostatotApurMen }
  TConsultaRespostatotApurMen = class(TACBrLibRespostaBase) //TPadraoReinfResposta)
  private
    FCRMen: String;
    FvlrCRMenInf: double;
    FvlrCRMenCalc: double;
    FvlrCRMenDCTF: double;
    FvlrCRMenSuspInf: double;
    FvlrCRMenSuspCalc: double;
    FvlrCRMenSuspDCTF: double;
    FnatRend: String;
  public
    constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo;
      const AFormato: TACBrLibCodificacao); reintroduce;

    procedure Processar(const ACont: Integer;
      const AItem: TtotApurMenCollection);
  published
    property CRMen: String read FCRMen write FCRMen;
    property vlrCRMenInf: double read FvlrCRMenInf write FvlrCRMenInf;
    property vlrCRMenCalc: double read FvlrCRMenCalc write FvlrCRMenCalc;
    property vlrCRMenDCTF: double read FvlrCRMenDCTF write FvlrCRMenDCTF;
    property vlrCRMenSuspInf: double read FvlrCRMenSuspInf write FvlrCRMenSuspInf;
    property vlrCRMenSuspCalc: double read FvlrCRMenSuspCalc write FvlrCRMenSuspCalc;
    property vlrCRMenSuspDCTF: double read FvlrCRMenSuspDCTF write FvlrCRMenSuspDCTF;
    property natRend: String read FnatRend write FnatRend;
  end;

  { TConsultaRespostatotApurQui }
  TConsultaRespostatotApurQui = class(TACBrLibRespostaBase) //TPadraoReinfResposta)
  private
    FperApurQui: String;
    FCRQui: String;
    FvlrCRQuiInf: double;
    FvlrCRQuiCalc: double;
    FvlrCRQuiDCTF: double;
    FvlrCRQuiSuspInf: double;
    FvlrCRQuiSuspCalc: double;
    FvlrCRQuiSuspDCTF: double;
    FnatRend: String;
  public
    constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo;
      const AFormato: TACBrLibCodificacao); reintroduce;

    procedure Processar(const ACont: Integer;
      const AItem: TtotApurQuiCollection);
  published
    property perApurQui: String read FperApurQui write FperApurQui;
    property CRQui: String read FCRQui write FCRQui;
    property vlrCRQuiInf: double read FvlrCRQuiInf write FvlrCRQuiInf;
    property vlrCRQuiCalc: double read FvlrCRQuiCalc write FvlrCRQuiCalc;
    property vlrCRQuiDCTF: double read FvlrCRQuiDCTF write FvlrCRQuiDCTF;
    property vlrCRQuiSuspInf: double read FvlrCRQuiSuspInf write FvlrCRQuiSuspInf;
    property vlrCRQuiSuspCalc: double read FvlrCRQuiSuspCalc write FvlrCRQuiSuspCalc;
    property vlrCRQuiSuspDCTF: double read FvlrCRQuiSuspDCTF write FvlrCRQuiSuspDCTF;
    property natRend: String read FnatRend write FnatRend;
  end;

  { TConsultaRespostatotApurDec }
  TConsultaRespostatotApurDec = class(TACBrLibRespostaBase) //TPadraoReinfResposta)
  private
    FperApurDec: String;
    FCRDec: String;
    FvlrCRDecInf: double;
    FvlrCRDecCalc: double;
    FvlrCRDecDCTF: double;
    FvlrCRDecSuspInf: double;
    FvlrCRDecSuspCalc: double;
    FvlrCRDecSuspDCTF: double;
    FnatRend: String;
  public
    constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo;
      const AFormato: TACBrLibCodificacao); reintroduce;

    procedure Processar(const ACont: Integer;
      const AItem: TtotApurDecCollection);
  published
    property perApurDec: String read FperApurDec write FperApurDec;
    property CRDec: String read FCRDec write FCRDec;
    property vlrCRDecInf: double read FvlrCRDecInf write FvlrCRDecInf;
    property vlrCRDecCalc: double read FvlrCRDecCalc write FvlrCRDecCalc;
    property vlrCRDecDCTF: double read FvlrCRDecDCTF write FvlrCRDecDCTF;
    property vlrCRDecSuspInf: double read FvlrCRDecSuspInf write FvlrCRDecSuspInf;
    property vlrCRDecSuspCalc: double read FvlrCRDecSuspCalc write FvlrCRDecSuspCalc;
    property vlrCRDecSuspDCTF: double read FvlrCRDecSuspDCTF write FvlrCRDecSuspDCTF;
    property natRend: String read FnatRend write FnatRend;
  end;

  { TConsultaRespostatotApurSem }
  TConsultaRespostatotApurSem = class(TACBrLibRespostaBase) //TPadraoReinfResposta)
  private
    FperApurSem: String;
    FCRSem: String;
    FvlrCRSemInf: double;
    FvlrCRSemCalc: double;
    FvlrCRSemDCTF: double;
    FvlrCRSemSuspInf: double;
    FvlrCRSemSuspCalc: double;
    FvlrCRSemSuspDCTF: double;
    FnatRend: String;
  public
    constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo;
      const AFormato: TACBrLibCodificacao); reintroduce;

    procedure Processar(const ACont: Integer;
      const AItem: TtotApurSemCollection);
  published
    property perApurSem: String read FperApurSem write FperApurSem;
    property CRSem: String read FCRSem write FCRSem;
    property vlrCRSemInf: double read FvlrCRSemInf write FvlrCRSemInf;
    property vlrCRSemCalc: double read FvlrCRSemCalc write FvlrCRSemCalc;
    property vlrCRSemDCTF: double read FvlrCRSemDCTF write FvlrCRSemDCTF;
    property vlrCRSemSuspInf: double read FvlrCRSemSuspInf write FvlrCRSemSuspInf;
    property vlrCRSemSuspCalc: double read FvlrCRSemSuspCalc write FvlrCRSemSuspCalc;
    property vlrCRSemSuspDCTF: double read FvlrCRSemSuspDCTF write FvlrCRSemSuspDCTF;
    property natRend: String read FnatRend write FnatRend;
  end;

  { TConsultaRespostatotApurDia }
  TConsultaRespostatotApurDia = class(TACBrLibRespostaBase) //TPadraoReinfResposta)
  private
    FperApurDia: String;
    FCRDia: String;
    FvlrCRDiaInf: double;
    FvlrCRDiaCalc: double;
    FvlrCRDiaDCTF: double;
    FvlrCRDiaSuspInf: double;
    FvlrCRDiaSuspCalc: double;
    FvlrCRDiaSuspDCTF: double;
    FnatRend: String;
  public
    constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo;
      const AFormato: TACBrLibCodificacao); reintroduce;

    procedure Processar(const ACont: Integer;
      const AItem: TtotApurDiaCollection);
  published
    property perApurDia: String read FperApurDia write FperApurDia;
    property CRDia: String read FCRDia write FCRDia;
    property vlrCRDiaInf: double read FvlrCRDiaInf write FvlrCRDiaInf;
    property vlrCRDiaCalc: double read FvlrCRDiaCalc write FvlrCRDiaCalc;
    property vlrCRDiaDCTF: double read FvlrCRDiaDCTF write FvlrCRDiaDCTF;
    property vlrCRDiaSuspInf: double read FvlrCRDiaSuspInf write FvlrCRDiaSuspInf;
    property vlrCRDiaSuspCalc: double read FvlrCRDiaSuspCalc write FvlrCRDiaSuspCalc;
    property vlrCRDiaSuspDCTF: double read FvlrCRDiaSuspDCTF write FvlrCRDiaSuspDCTF;
    property natRend: String read FnatRend write FnatRend;
  end;

  { TRespostas }
  TRespostas = class
  private
    FACBrReinf: TACBrReinf;
    FTpResp: TACBrLibRespostaTipo;
    FFormato: TACBrLibCodificacao;
    FResposta: AnsiString;
  public
    constructor Create(const AACBrReinf: TACBrReinf; const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);

    procedure RespostaEnvio;
    procedure RespostaEnvioRetorno;
    procedure RespostaEnvioideTransmissor;
    procedure RespostaEnviostatus;
    procedure RespostaEnviodadosRecepcaoLote(const AItem: TRetEnvioLote);
    procedure RespostaEnvioOcorrencias(const ACont: Integer);
    procedure RespostaEnvioevento(const ACont: Integer);
    procedure RespostaEnvioevtTotal(const ACont: Integer);
    procedure RespostaEnvioideEvento(const ACont: Integer);
    procedure RespostaEnvioideContri(const ACont: Integer);
    procedure RespostaEnvioideStatus(const ACont: Integer; const AItem: TIdeStatus);
    procedure RespostaEnvioregOcorrs(const ACont, ACont2: Integer; const AItem: TIdeStatus);
    procedure RespostaEnvioinfoRecEv(const ACont: Integer; const AItem: TInfoRecEv);
    procedure RespostaEnvioinfoTotal(const ACont: Integer);
    procedure RespostaEnvioRTom(const ACont: Integer; const AItem: TeventoCollectionItem);
    procedure RespostaEnvioinfoCRTom(const ACont, ACont2: Integer; const AItem: TeventoCollectionItem);
    procedure RespostaEnvioRPrest(const ACont: Integer; const AItem: TeventoCollectionItem);
    procedure RespostaEnvioRRecRepAD(const ACont, ACont2: Integer; const AItem: TeventoCollectionItem);
    procedure RespostaEnvioRComl(const ACont, ACont2: Integer; const AItem: TeventoCollectionItem);
    procedure RespostaEnvioRAquis(const ACont, ACont2: Integer; const AItem: TeventoCollectionItem);
    procedure RespostaEnvioRCPRB(const ACont, ACont2: Integer; const AItem: TeventoCollectionItem);
    procedure RespostaEnvioRRecEspetDest(const ACont: Integer; const AItem: TeventoCollectionItem);

    procedure RespostaConsultaideContri(const ASessao: String; const AItem: TideContrib; const AStatus: TStatus = nil);
    procedure RespostaConsultaRetorno;
    procedure RespostaConsulta(const ASessao: String; const AId: String);
    procedure RespostaConsultaideEvento(const AItem: TIdeEvento1);
    procedure RespostainfoTotalevtRet(const ACont: Integer);
    procedure RespostaideEstab(const ACont: Integer);
    procedure RespostatotApurMen(const ACont, ACont2: Integer);
    procedure RespostatotApurTribMen(const ACont, ACont2: Integer);
    procedure RespostatotApurQui(const ACont, ACont2: Integer);
    procedure RespostatotApurTribQui(const ACont, ACont2: Integer);
    procedure RespostatotApurDec(const ACont, ACont2: Integer);
    procedure RespostatotApurTribDec(const ACont, ACont2: Integer);
    procedure RespostatotApurSem(const ACont, ACont2: Integer);
    procedure RespostatotApurTribSem(const ACont, ACont2: Integer);
    procedure RespostatotApurDia(const ACont, ACont2: Integer);
    procedure RespostatotApurTribDia(const ACont, ACont2: Integer);

    procedure RespostaConsultaregOcorrs(const ACont: Integer; const AItem: TIdeStatus);
    procedure RespostaConsultainfoCR_CNR(const AItem: TinfoCR_CNR);
    procedure RespostaConsultatotApurMen(const ACont: Integer; const AItem: TtotApurMenCollection; const APrefixo: String);
    procedure RespostaConsultatotApurQui(const ACont: Integer; const AItem: TtotApurQuiCollection; const APrefixo: String);
    procedure RespostaConsultatotApurDec(const ACont: Integer; const AItem: TtotApurDecCollection; const APrefixo: String);
    procedure RespostaConsultatotApurSem(const ACont: Integer; const AItem: TtotApurSemCollection; const APrefixo: String);
    procedure RespostaConsultatotApurDia(const ACont: Integer; const AItem: TtotApurDiaCollection; const APrefixo: String);
    procedure RespostaConsultainfoTotalCR;
    procedure RespostaConsultaideStatus;
    procedure RespostaConsultainfoRecEv;
    procedure RespostaConsultainfoTotalContrib;
    procedure RespostaConsultaRTom(const ACont: Integer);
    procedure RespostaConsultainfoCRTom(const ACont, ACont2: Integer);
    procedure RespostaConsultaRPrest(const ACont: Integer);
    procedure RespostaConsultaRRecRepAD(const ACont: Integer);
    procedure RespostaConsultaRComl(const ACont: Integer);
    procedure RespostaConsultaRCPRB(const ACont: Integer);

    procedure RespostaConsultaRecibo;
    procedure RespostaConsultaReciboStatus;
    procedure RespostaConsultaReciboOcorrs(const ACont: Integer);
    procedure RespostaEventoRecibo(const ACont: Integer);

    property TpResp: TACBrLibRespostaTipo read FTpResp write FTpResp;
    property Formato: TACBrLibCodificacao read FFormato write FFormato;
    property Resposta: AnsiString read FResposta write FResposta;
  end;


implementation

{ TRespostas }

constructor TRespostas.Create(const AACBrReinf: TACBrReinf; const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  FACBrReinf := AACBrReinf;
  FTpResp := ATipo;
  FFormato := AFormato;
end;

procedure TRespostas.RespostaEnvio;
var
  i, j: Integer;
begin
  with FACBrReinf.WebServices.EnvioLote.RetEnvioLote do
  begin
    RespostaEnvioRetorno;
    RespostaEnvioideTransmissor;
    RespostaEnviostatus;
    RespostaEnviodadosRecepcaoLote(FACBrReinf.WebServices.EnvioLote.RetEnvioLote);

    for i := 0 to Status.Ocorrencias.Count - 1 do
      RespostaEnvioOcorrencias(i);

    for i := 0 to evento.Count - 1 do
    begin
      RespostaEnvioevento(i);
      RespostaEnvioevtTotal(i);
      RespostaEnvioideEvento(i);
      RespostaEnvioideContri(i);
      RespostaEnvioideStatus(i, evento.Items[i].evtTotal.IdeStatus);

      for j := 0 to evento.Items[i].evtTotal.IdeStatus.regOcorrs.Count -1 do
        RespostaEnvioregOcorrs(i, j, evento.Items[i].evtTotal.IdeStatus);

      RespostaEnvioinfoRecEv(i, evento.Items[i].evtTotal.InfoRecEv);
      RespostaEnvioinfoTotal(i);
      RespostaEnvioRTom(i, evento.Items[i]);

      for j := 0 to evento.Items[i].evtTotal.InfoTotal.RTom.infoCRTom.Count -1 do
        RespostaEnvioinfoCRTom(i, j, evento.Items[i]);

      RespostaEnvioRPrest(i, evento.Items[i]);

      for j := 0 to evento.Items[i].evtTotal.InfoTotal.RRecRepAD.Count -1 do
        RespostaEnvioRRecRepAD(i, j, evento.Items[i]);

      for j := 0 to evento.Items[i].evtTotal.InfoTotal.RComl.Count -1 do
        RespostaEnvioRComl(i, j, evento.Items[i]);

      for j := 0 to evento.Items[i].evtTotal.InfoTotal.RComl.Count -1 do
        RespostaEnvioRComl(i, j, evento.Items[i]);

      for j := 0 to evento.Items[i].evtTotal.InfoTotal.RCPRB.Count -1 do
        RespostaEnvioRCPRB(i, j, evento.Items[i]);

      RespostaEnvioRRecEspetDest(i, evento.Items[i]);
    end;
  end;
end;

procedure TRespostas.RespostaEnvioRetorno;
var
  Resp: TEnvioResposta;
begin
  Resp := TEnvioResposta.Create(FTpResp, FFormato);
  try
    Resp.Processar(FACBrReinf);
    Resposta := Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TRespostas.RespostaEnvioideTransmissor;
var
  Resp: TEnvioRespostaideTransmissor;
begin
  Resp := TEnvioRespostaideTransmissor.Create(TpResp, codUTF8);
  try
    Resp.Processar(FACBrReinf);
    Resposta := Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TRespostas.RespostaEnviostatus;
var
  Resp: TEnvioRespostastatus;
begin
  Resp := TEnvioRespostastatus.Create(TpResp, codUTF8);
  try
    Resp.Processar(FACBrReinf);
    Resposta := Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TRespostas.RespostaEnviodadosRecepcaoLote(const AItem: TRetEnvioLote);
var
  Resp: TEnvioRespostadadosRecepcaoLote;
begin
  Resp := TEnvioRespostadadosRecepcaoLote.Create(TpResp, codUTF8);
  try
    Resp.Processar(AItem);
    Resposta := Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TRespostas.RespostaEnvioOcorrencias(const ACont: Integer);
var
  Resp: TEnvioRespostaOcorrencias;
begin
  Resp := TEnvioRespostaOcorrencias.Create(CSessaoRespEnvioocorrencias + IntToStrZero(Int64(ACont+1), 3), TpResp, codUTF8);
  try
    Resp.Processar(FACBrReinf, ACont);
    Resposta := Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TRespostas.RespostaEnvioevento(const ACont: Integer);
var
  Resp: TEnvioRespostaevento;
begin
  Resp := TEnvioRespostaevento.Create(CSessaoRespEnvioevento + IntToStrZero(Int64(ACont+1), 3), TpResp, codUTF8);
  try
    Resp.Processar(FACBrReinf, ACont);
    Resposta := Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TRespostas.RespostaEnvioevtTotal(const ACont: Integer);
var
  Resp: TEnvioRespostaevtTotal;
begin
  Resp := TEnvioRespostaevtTotal.Create(CSessaoRespEnvioevtTotal + IntToStrZero(Int64(ACont+1), 3), TpResp, codUTF8);
  try
    Resp.Processar(FACBrReinf, ACont);
    Resposta := Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TRespostas.RespostaEnvioideEvento(const ACont: Integer);
var
  Resp: TRespostaideEvento;
begin
  Resp := TRespostaideEvento.Create(CSessaoRetornoideEvento + IntToStrZero(Int64(ACont+1), 3), TpResp, codUTF8);
  try
    Resp.Processar(FACBrReinf.WebServices.EnvioLote.RetEnvioLote.evento.Items[ACont].evtTotal.IdeEvento);
    Resposta := Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TRespostas.RespostaEnvioideContri(const ACont: Integer);
var
  Resp: TRespostaideContri;
begin
  Resp := TRespostaideContri.Create(CSessaoRetornoideContri + IntToStrZero(Int64(ACont+1), 3), TpResp, codUTF8);
  try
    Resp.Processar(FACBrReinf.WebServices.EnvioLote.RetEnvioLote.evento.Items[ACont].evtTotal.IdeContrib);
    Resposta := Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TRespostas.RespostaEnvioideStatus(const ACont: Integer; const AItem: TIdeStatus);
var
  Resp: TRespostaideStatus;
begin
  Resp := TRespostaideStatus.Create(CSessaoRetornoideStatus + IntToStrZero(Int64(ACont+1), 3), TpResp, codUTF8);
  try
    Resp.Processar(AItem);
    Resposta := Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TRespostas.RespostaEnvioregOcorrs(const ACont, ACont2: Integer; const AItem: TIdeStatus);
var
  Resp: TRespostaregOcorrs;
begin
  Resp := TRespostaregOcorrs.Create(CSessaoRetornoregOcorrs +
                      IntToStrZero(Int64(ACont+1), 3) + IntToStrZero(Int64(ACont2+1), 3), TpResp, codUTF8);
  try
    Resp.Processar(ACont2, AItem);
    Resposta := Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TRespostas.RespostaEnvioinfoRecEv(const ACont: Integer; const AItem: TInfoRecEv);
var
  Resp: TRespostainfoRecEv;
begin
  Resp := TRespostainfoRecEv.Create(CSessaoRetornoinfoRecEv + IntToStrZero(Int64(ACont+1), 3), TpResp, codUTF8);
  try
    Resp.Processar(AItem);
    Resposta := Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TRespostas.RespostaEnvioinfoTotal(const ACont: Integer);
var
  Resp: TRespostainfoTotal_infoTotalContrib;
begin
  Resp := TRespostainfoTotal_infoTotalContrib.Create(CSessaoRespEnvioinfoTotal +
                                              IntToStrZero(Int64(ACont+1), 3), TpResp, codUTF8);
  try
    Resp.Processar(FACBrReinf, ACont);
    Resposta := Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TRespostas.RespostaEnvioRTom(const ACont: Integer; const AItem: TeventoCollectionItem);
var
  Resp: TRespostaRTom;
begin
  Resp := TRespostaRTom.Create(CSessaoRetornoRTom + IntToStrZero(Int64(ACont+1), 3), TpResp, codUTF8);
  try
    Resp.Processar(AItem);
    Resposta := Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TRespostas.RespostaEnvioinfoCRTom(const ACont, ACont2: Integer;
 const AItem: TeventoCollectionItem);
var
  Resp: TRespostainfoCRTom;
begin
  Resp := TRespostainfoCRTom.Create(CSessaoRetornoinfoCRTom +
            IntToStrZero(Int64(ACont+1), 3) + IntToStrZero(Int64(ACont2+1), 1), TpResp, codUTF8);
  try
    Resp.Processar(ACont2, AItem);
    Resposta := Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TRespostas.RespostaEnvioRPrest(const ACont: Integer;
 const AItem: TeventoCollectionItem);
var
  Resp: TRespostaRPrest;
begin
  Resp := TRespostaRPrest.Create(CSessaoRetornoRPrest +
            IntToStrZero(Int64(ACont+1), 3), TpResp, codUTF8);
  try
    Resp.Processar(AItem);
    Resposta := Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TRespostas.RespostaEnvioRRecRepAD(const ACont, ACont2: Integer;
 const AItem: TeventoCollectionItem);
var
  Resp: TRespostaRRecRepAD;
begin
  Resp := TRespostaRRecRepAD.Create(CSessaoRetornoRRecRepAD +
            IntToStrZero(Int64(ACont+1), 3) + IntToStrZero(Int64(ACont2+1), 3), TpResp, codUTF8);
  try
    Resp.Processar(ACont2, AItem);
    Resposta := Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TRespostas.RespostaEnvioRComl(const ACont, ACont2: Integer;
 const AItem: TeventoCollectionItem);
var
  Resp: TRespostaRComl;
begin
  Resp := TRespostaRComl.Create(CSessaoRetornoRComl +
            IntToStrZero(Int64(ACont+1), 3) + IntToStrZero(Int64(ACont2+1), 1), TpResp, codUTF8);
  try
    Resp.Processar(ACont2, AItem);
    Resposta := Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TRespostas.RespostaEnvioRAquis(const ACont, ACont2: Integer;
 const AItem: TeventoCollectionItem);
var
  Resp: TRespostaRAquis;
begin
  Resp := TRespostaRAquis.Create(CSessaoRetornoRAquis +
            IntToStrZero(Int64(ACont+1), 3) + IntToStrZero(Int64(ACont2+1), 1), TpResp, codUTF8);
  try
    Resp.Processar(ACont2, AItem);
    Resposta := Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TRespostas.RespostaEnvioRCPRB(const ACont, ACont2: Integer;
 const AItem: TeventoCollectionItem);
var
  Resp: TRespostaRCPRB;
begin
  Resp := TRespostaRCPRB.Create(CSessaoRetornoRCPRB +
            IntToStrZero(Int64(ACont+1), 3) + IntToStrZero(Int64(ACont2+1), 1), TpResp, codUTF8);
  try
    Resp.Processar(ACont2, AItem);
    Resposta := Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TRespostas.RespostaEnvioRRecEspetDest(const ACont: Integer;
 const AItem: TeventoCollectionItem);
var
  Resp: TEnvioRespostaRRecEspetDesp;
begin
  Resp := TEnvioRespostaRRecEspetDesp.Create(CSessaoRetornoRRecEspetDesp +
            IntToStrZero(Int64(ACont+1), 3), TpResp, codUTF8);
  try
    Resp.Processar(AItem);
    Resposta := Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TRespostas.RespostaConsulta(const ASessao: String; const AId: String);
var
  Resp: TConsultaResposta;
begin
  Resp := TConsultaResposta.Create(TpResp, codUTF8, ASessao);
  try
    Resp.Processar(AId);
    Resposta := Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TRespostas.RespostaConsultaideEvento(const AItem: TIdeEvento1);
var
  Resp: TRespostaideEvento;
begin
  Resp := TRespostaideEvento.Create(CSessaoRetornoideEvento, TpResp, codUTF8);
  try
    resp.perApur := AItem.perApur;

    Resp.Processar(AItem);
    Resposta := Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TRespostas.RespostainfoTotalevtRet(const ACont: Integer);
var
  Resp: TPadraoReinfResposta;
begin
  Resp := TPadraoReinfResposta.Create(CSessaoRespEnvioinfoTotal +
                                      IntToStrZero(ACont+1, 3), TpResp, codUTF8);
  try
    Resposta := Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TRespostas.RespostaideEstab(const ACont: Integer);
var
  Resp: TEnvioRespostaideEstab;
begin
  Resp := TEnvioRespostaideEstab.Create(CSessaoRespideEstab +
                                        IntToStrZero(ACont+1, 3), TpResp, codUTF8);
  try
    Resp.Processar(FACBrReinf.WebServices.Consultar.RetEnvioLote.evento.Items[ACont].evtRet.InfoTotal.ideEstab);
    Resposta := Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TRespostas.RespostatotApurMen(const ACont, ACont2: Integer);
var
  Resp: TEnvioRespostatotApurMen;
begin
  Resp := TEnvioRespostatotApurMen.Create(CSessaoResptottotApurMen +
            IntToStrZero(ACont+1, 3) + IntToStrZero(ACont2+1, 3), TpResp, codUTF8);
  try
    Resp.Processar(FACBrReinf, ACont, ACont2);
    Resposta := Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TRespostas.RespostatotApurTribMen(const ACont, ACont2: Integer);
var
  Resp: TEnvioRespostatotApurTribMen;
begin
  Resp := TEnvioRespostatotApurTribMen.Create(CSessaoResptotApurTribMen +
            IntToStrZero(ACont+1, 3) + IntToStrZero(ACont2+1, 3), TpResp, codUTF8);
  try
    Resp.Processar(FACBrReinf, ACont, ACont2);
    Resposta := Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TRespostas.RespostatotApurQui(const ACont, ACont2: Integer);
var
  Resp: TEnvioRespostatotApurQui;
begin
  Resp := TEnvioRespostatotApurQui.Create(CSessaoResptottotApurQui +
            IntToStrZero(ACont+1, 3) + IntToStrZero(ACont2+1, 3), TpResp, codUTF8);
  try
    Resp.Processar(FACBrReinf, ACont, ACont2);
    Resposta := Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TRespostas.RespostatotApurTribQui(const ACont, ACont2: Integer);
var
  Resp: TEnvioRespostatotApurTribQui;
begin
  Resp := TEnvioRespostatotApurTribQui.Create(CSessaoResptotApurTribQui +
            IntToStrZero(ACont+1, 3) + IntToStrZero(ACont2+1, 3), TpResp, codUTF8);
  try
    Resp.Processar(FACBrReinf, ACont, ACont2);
    Resposta := Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TRespostas.RespostatotApurDec(const ACont, ACont2: Integer);
var
  Resp: TEnvioRespostatotApurDec;
begin
  Resp := TEnvioRespostatotApurDec.Create(CSessaoResptottotApurDec +
            IntToStrZero(ACont+1, 3) + IntToStrZero(ACont2+1, 3), TpResp, codUTF8);
  try
    Resp.Processar(FACBrReinf, ACont, ACont2);
    Resposta := Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TRespostas.RespostatotApurTribDec(const ACont, ACont2: Integer);
var
  Resp: TEnvioRespostatotApurTribDec;
begin
  Resp := TEnvioRespostatotApurTribDec.Create(CSessaoResptotApurTribDec +
            IntToStrZero(ACont+1, 3) + IntToStrZero(ACont2+1, 3), TpResp, codUTF8);
  try
    Resp.Processar(FACBrReinf, ACont, ACont2);
    Resposta := Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TRespostas.RespostatotApurSem(const ACont, ACont2: Integer);
var
  Resp: TEnvioRespostatotApurSem;
begin
  Resp := TEnvioRespostatotApurSem.Create(CSessaoResptottotApurSem +
            IntToStrZero(ACont+1, 3) + IntToStrZero(ACont2+1, 3), TpResp, codUTF8);
  try
    Resp.Processar(FACBrReinf, ACont, ACont2);
    Resposta := Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TRespostas.RespostatotApurTribSem(const ACont, ACont2: Integer);
var
  Resp: TEnvioRespostatotApurTribSem;
begin
  Resp := TEnvioRespostatotApurTribSem.Create(CSessaoResptotApurTribSem +
            IntToStrZero(ACont+1, 3) + IntToStrZero(ACont2+1, 3), TpResp, codUTF8);
  try
    Resp.Processar(FACBrReinf, ACont, ACont2);
    Resposta := Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TRespostas.RespostatotApurDia(const ACont, ACont2: Integer);
var
  Resp: TEnvioRespostatotApurDia;
begin
  Resp := TEnvioRespostatotApurDia.Create(CSessaoResptottotApurDia +
            IntToStrZero(ACont+1, 3) + IntToStrZero(ACont2+1, 3), TpResp, codUTF8);
  try
    Resp.Processar(FACBrReinf, ACont, ACont2);
    Resposta := Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TRespostas.RespostatotApurTribDia(const ACont, ACont2: Integer);
var
  Resp: TEnvioRespostatotApurTribDia;
begin
  Resp := TEnvioRespostatotApurTribDia.Create(CSessaoResptotApurTribDia +
            IntToStrZero(ACont+1, 3) + IntToStrZero(ACont2+1, 3), TpResp, codUTF8);
  try
    Resp.Processar(FACBrReinf, ACont, ACont2);
    Resposta := Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TRespostas.RespostaConsultaregOcorrs(const ACont: Integer; const AItem: TIdeStatus);
var
  Resp: TRespostaregOcorrs;
begin
  Resp := TRespostaregOcorrs.Create(CSessaoRetornoregOcorrs + IntToStrZero(ACont+1, 3), TpResp, codUTF8);
  try
    Resp.Processar(ACont, AItem);
    Resposta := Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TRespostas.RespostaConsultainfoCR_CNR(const AItem: TinfoCR_CNR);
var
  Resp: TConsultaRespostainfoCR_CNR;
begin
  Resp := TConsultaRespostainfoCR_CNR.Create(CSessaoRespinfoCR_CNR, TpResp, codUTF8);
  try
    Resp.Processar(AItem);
    Resposta := Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TRespostas.RespostaConsultatotApurMen(const ACont: Integer; const AItem: TtotApurMenCollection;
 const APrefixo: String);
var
  Resp: TConsultaRespostatotApurMen;
begin
  Resp := TConsultaRespostatotApurMen.Create(APrefixo + CSessaoResptottotApurMen +
            IntToStrZero(ACont+1, 3), TpResp, codUTF8);
  try
    Resp.Processar(ACont, AItem);
    Resposta := Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TRespostas.RespostaConsultatotApurQui(const ACont: Integer; const AItem: TtotApurQuiCollection;
 const APrefixo: String);
var
  Resp: TConsultaRespostatotApurQui;
begin
  Resp := TConsultaRespostatotApurQui.Create(APrefixo + CSessaoResptottotApurQui +
            IntToStrZero(ACont+1, 3), TpResp, codUTF8);
  try
    Resp.Processar(ACont, AItem);
    Resposta := Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TRespostas.RespostaConsultatotApurDec(const ACont: Integer; const AItem: TtotApurDecCollection;
 const APrefixo: String);
var
  Resp: TConsultaRespostatotApurDec;
begin
  Resp := TConsultaRespostatotApurDec.Create(APrefixo + CSessaoResptottotApurDec +
            IntToStrZero(ACont+1, 3), TpResp, codUTF8);
  try
    Resp.Processar(ACont, AItem);
    Resposta := Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TRespostas.RespostaConsultatotApurSem(const ACont: Integer; const AItem: TtotApurSemCollection;
 const APrefixo: String);
var
  Resp: TConsultaRespostatotApurSem;
begin
  Resp := TConsultaRespostatotApurSem.Create(APrefixo + CSessaoResptottotApurSem +
            IntToStrZero(ACont+1, 3), TpResp, codUTF8);
  try
    Resp.Processar(ACont, AItem);
    Resposta := Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TRespostas.RespostaConsultatotApurDia(const ACont: Integer; const AItem: TtotApurDiaCollection;
 const APrefixo: String);
var
  Resp: TConsultaRespostatotApurDia;
begin
  Resp := TConsultaRespostatotApurDia.Create(APrefixo + CSessaoResptottotApurDia +
            IntToStrZero(ACont+1, 3), TpResp, codUTF8);
  try
    Resp.Processar(ACont, AItem);
    Resposta := Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TRespostas.RespostaConsultainfoTotalCR;
var
  Resp: TPadraoReinfResposta;
begin
  Resp := TPadraoReinfResposta.Create(CSessaoRespinfoTotalCR, TpResp, codUTF8);
  try
    Resposta := Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TRespostas.RespostaConsultaideStatus;
var
  Resp: TRespostaideStatus;
begin
  Resp := TRespostaideStatus.Create(CSessaoRetornoideStatus, TpResp, codUTF8);
  try
    Resp.Processar(FACBrReinf.WebServices.Consultar.evtTotalContribVersao.IdeStatus);
    Resposta := Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TRespostas.RespostaConsultainfoRecEv;
var
  Resp: TRespostainfoRecEv;
begin
  Resp := TRespostainfoRecEv.Create(CSessaoRetornoinfoRecEv, TpResp, codUTF8);
  try
    Resp.Processar(FACBrReinf.WebServices.Consultar.evtTotalContribVersao.InfoRecEv);
    Resposta := Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TRespostas.RespostaConsultainfoTotalContrib;
var
  Resp: TRespostainfoTotal_infoTotalContrib;
begin
  Resp := TRespostainfoTotal_infoTotalContrib.Create(CSessaoRespConsultainfoTotalContrib, TpResp, codUTF8);
  try
    Resp.ProcessarInfoTotalContrib(FACBrReinf);
    Resposta := Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TRespostas.RespostaConsultaRTom(const ACont: Integer);
var
  Resp: TRespostaRTom;
begin
  Resp := TRespostaRTom.Create(CSessaoRetornoRTom + IntToStrZero(ACont+1, 3), TpResp, codUTF8);
  try
    Resp.ProcessarInfoTotalContrib(FACBrReinf, ACont);
    Resposta := Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TRespostas.RespostaConsultainfoCRTom(const ACont, ACont2: Integer);
var
  Resp: TRespostainfoCRTom;
begin
  Resp := TRespostainfoCRTom.Create(CSessaoRetornoinfoCRTom +
            intToStrZero(ACont+1, 3) + IntToStrZero(ACont2+1, 1), TpResp, codUTF8);
  try
    Resp.ProcessarInfoTotalContrib(FACBrReinf, ACont, ACont2);
    Resposta := Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TRespostas.RespostaConsultaRPrest(const ACont: Integer);
var
  Resp: TRespostaRPrest;
begin
  Resp := TRespostaRPrest.Create(CSessaoRetornoRPrest + IntToStrZero(ACont+1, 3), TpResp, codUTF8);
  try
    Resp.ProcessarInfoTotalContrib(FACBrReinf, ACont);
    Resposta := Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TRespostas.RespostaConsultaRRecRepAD(const ACont: Integer);
var
  Resp: TRespostaRRecRepAD;
begin
  Resp := TRespostaRRecRepAD.Create(CSessaoRetornoRRecRepAD + IntToStrZero(ACont+1, 3), TpResp, codUTF8);
  try
    Resp.ProcessarInfoTotalContrib(FACBrReinf, ACont);
    Resposta := Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TRespostas.RespostaConsultaRComl(const ACont: Integer);
var
  Resp: TRespostaRComl;
begin
  Resp := TRespostaRComl.Create(CSessaoRetornoRComl + IntToStrZero(ACont+1, 1), TpResp, codUTF8);
  try
    Resp.ProcessarInfoTotalContrib(FACBrReinf, ACont);
    Resposta := Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TRespostas.RespostaConsultaRCPRB(const ACont: Integer);
var
  Resp: TRespostaRCPRB;
begin
  Resp := TRespostaRCPRB.Create(CSessaoRetornoRCPRB + IntToStrZero(ACont+1, 1), TpResp, codUTF8);
  try
    Resp.ProcessarInfoTotalContrib(FACBrReinf, ACont);
    Resposta := Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TRespostas.RespostaConsultaRecibo;
var
  i: Integer;
begin
  with FACBrReinf.WebServices.ConsultarReciboEvento.evtTotalContribVersao do
  begin
    RespostaConsultaReciboStatus;

    for i := 0 to IdeStatus.regOcorrs.Count -1 do
      RespostaConsultaReciboOcorrs(i);

    for i := 0 to RetornoEventos.Count -1 do
      RespostaEventoRecibo(i);
  end;
end;

procedure TRespostas.RespostaConsultaReciboStatus;
var
  Resp: TRespostaideStatus;
begin
  Resp := TRespostaideStatus.Create(CSessaoRetornoideStatus, TpResp, codUTF8);
  try
    Resp.ProcessarInfoTotalContrib(FACBrReinf);
    Resposta := Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TRespostas.RespostaConsultaReciboOcorrs(const ACont: Integer);
var
  Resp: TRespostaregOcorrs;
begin
  Resp := TRespostaregOcorrs.Create(CSessaoRetornoregOcorrs + IntToStrZero(ACont+1, 3), TpResp, codUTF8);
  try
    Resp.ProcessarInfoTotalContrib(FACBrReinf, ACont);
    Resposta := Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TRespostas.RespostaEventoRecibo(const ACont: Integer);
var
  Resp: TRespostaEventoRecibo;
begin
  Resp := TRespostaEventoRecibo.Create(CSessaoRetornoEventoRecibo + IntToStrZero(ACont+1, 3), TpResp, codUTF8);
  try
    Resp.ProcessarInfoTotalContrib(FACBrReinf, ACont);
    Resposta := Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TRespostas.RespostaConsultaRetorno;
var
  i, j: Integer;
begin
  with FACBrReinf.WebServices.Consultar.RetEnvioLote do
  begin
    RespostaConsultaideContri(CSessaoRetornoideContribuinte, IdeContribuinte, status);
    RespostaEnviodadosRecepcaoLote(FACBrReinf.WebServices.Consultar.RetEnvioLote);

    for i:=0 to evento.Count - 1 do
    begin
      with evento.Items[i] do
      begin
        if evtTotal.id <> '' then
        begin
          with evtTotal do
          begin
            RespostaConsulta(CSessaoRespEnvioevtTotal, id);
            RespostaConsultaideEvento(IdeEvento);
            RespostaConsultaideContri(CSessaoRetornoideContri, IdeContrib);
            RespostaEnvioideStatus(i, IdeStatus);

            for j := 0 to IdeStatus.regOcorrs.Count -1 do
              RespostaEnvioregOcorrs(i, j, evento.Items[i].evtTotal.IdeStatus);

            RespostaEnvioinfoRecEv(i,evento.Items[i].evtTotal.InfoRecEv);
            RespostaEnvioinfoTotal(i);
            RespostaEnvioRTom(i, evento.Items[i]);

            for j := 0 to evento.Items[i].evtTotal.InfoTotal.RTom.infoCRTom.Count -1 do
              RespostaEnvioinfoCRTom(i, j, evento.Items[i]);

            RespostaEnvioRPrest(i, evento.Items[i]);

            for j := 0 to evento.Items[i].evtTotal.InfoTotal.RRecRepAD.Count -1 do
              RespostaEnvioRRecRepAD(i, j, evento.Items[i]);

            for j := 0 to evento.Items[i].evtTotal.InfoTotal.RComl.Count -1 do
              RespostaEnvioRComl(i, j, evento.Items[i]);

            for j := 0 to evento.Items[i].evtTotal.InfoTotal.RAquis.Count -1 do
              RespostaEnvioRAquis(i, j, evento.Items[i]);

            for j := 0 to evento.Items[i].evtTotal.InfoTotal.RCPRB.Count -1 do
              RespostaEnvioRCPRB(i, j, evento.Items[i]);

            RespostaEnvioRRecEspetDest(i, evento.Items[i]);
          end;
        end;

        if evtRet.Id <> '' then
        begin
          with evtRet do
          begin
            RespostaConsulta(CSessaoRespEnvioevtRet, id);
            RespostaConsultaideEvento(IdeEvento);
            RespostaConsultaideContri(CSessaoRetornoideContri, IdeContrib);
            RespostaEnvioideStatus(i, IdeStatus);

            for j := 0 to IdeStatus.regOcorrs.Count -1 do
              RespostaEnvioregOcorrs(i, j, evento.Items[i].evtRet.IdeStatus);

            RespostaEnvioinfoRecEv(i,evento.Items[i].evtRet.InfoRecEv);

            RespostainfoTotalevtRet(i);
            RespostaideEstab(i);

            for j := 0 to evento.Items[i].evtRet.InfoTotal.ideEstab.totApurMen.Count -1 do
            begin
              RespostatotApurMen(i, j);

              RespostatotApurTribMen(i, j);
            end;

            for j := 0 to evento.Items[i].evtRet.InfoTotal.ideEstab.totApurQui.Count -1 do
            begin
              RespostatotApurQui(i, j);

              RespostatotApurTribQui(i, j);
            end;

            for j := 0 to evento.Items[i].evtRet.InfoTotal.ideEstab.totApurDec.Count -1 do
            begin
              RespostatotApurDec(i, j);

              RespostatotApurTribDec(i, j);
            end;

            for j := 0 to evento.Items[i].evtRet.InfoTotal.ideEstab.totApurSem.Count -1 do
            begin
              RespostatotApurSem(i, j);

              RespostatotApurTribSem(i, j);
            end;

            for j := 0 to evento.Items[i].evtRet.InfoTotal.ideEstab.totApurDia.Count -1 do
            begin
              RespostatotApurDia(i, j);

              RespostatotApurTribDia(i, j);
            end;
          end;
        end;
      end;
    end;
  end;

  if FACBrReinf.WebServices.Consultar.RetConsulta_R9015.evtRetCons.Id <> '' then
  begin
    with FACBrReinf.WebServices.Consultar.RetConsulta_R9015.evtRetCons do
    begin
      RespostaConsulta(CSessaoRespEnvioevtRetCons, Id);
      RespostaConsultaideEvento(IdeEvento);
      RespostaConsultaideContri(CSessaoRetornoideContri, IdeContri);
      RespostaEnvioideStatus(0, IdeStatus);

      for i := 0 to IdeStatus.regOcorrs.Count -1 do
        RespostaConsultaregOcorrs(i, IdeStatus);

      RespostaEnvioinfoRecEv(0, InfoRecEv);
      RespostaConsultainfoCR_CNR(infoCR_CNR);

      for i := 0 to infoCR_CNR.totApurMen.Count -1 do
        RespostaConsultatotApurMen(i, infoCR_CNR.totApurMen, 'infoCR_CNR.');

      for i := 0 to infoCR_CNR.totApurQui.Count -1 do
        RespostaConsultatotApurQui(i, infoCR_CNR.totApurQui, 'infoCR_CNR.');

      for i := 0 to infoCR_CNR.totApurDec.Count -1 do
        RespostaConsultatotApurDec(i, infoCR_CNR.totApurDec, 'infoCR_CNR.');

      for i := 0 to infoCR_CNR.totApurSem.Count -1 do
        RespostaConsultatotApurSem(i, infoCR_CNR.totApurSem, 'infoCR_CNR.');

      for i := 0 to infoCR_CNR.totApurDia.Count -1 do
        RespostaConsultatotApurDia(i, infoCR_CNR.totApurDia, 'infoCR_CNR.');

      RespostaConsultainfoTotalCR;

      for i := 0 to infoTotalCR.totApurMen.Count -1 do
        RespostaConsultatotApurMen(i, infoTotalCR.totApurMen, 'infoTotalCR.');

      for i := 0 to infoTotalCR.totApurQui.Count -1 do
        RespostaConsultatotApurQui(i, infoTotalCR.totApurQui, 'infoTotalCR.');

      for i := 0 to infoTotalCR.totApurDec.Count -1 do
        RespostaConsultatotApurDec(i, infoTotalCR.totApurDec, 'infoTotalCR.');

      for i := 0 to infoTotalCR.totApurSem.Count -1 do
        RespostaConsultatotApurSem(i, infoTotalCR.totApurSem, 'infoTotalCR.');

      for i := 0 to infoTotalCR.totApurDia.Count -1 do
        RespostaConsultatotApurDia(i, infoTotalCR.totApurDia, 'infoTotalCR.');
    end;
  end;

  if FACBrReinf.WebServices.Consultar.evtTotalContribVersao.Id <> '' then
  begin
    with fACBrReinf.WebServices.Consultar.evtTotalContribVersao do
    begin
      RespostaConsulta(CSessaoRespConsulta, Id);
      RespostaConsultaideEvento(IdeEvento);
      RespostaConsultaideContri(CSessaoRetornoideContri, IdeContri);
      RespostaConsultaideStatus;

      for i := 0 to IdeStatus.regOcorrs.Count -1 do
        RespostaConsultaregOcorrs(i, IdeStatus);

      RespostaConsultainfoRecEv;
      RespostaConsultainfoTotalContrib;

      for i := 0 to infoTotalContrib.RTom.Count -1 do
      begin
        RespostaConsultaRTom(i);

        for j := 0 to infoTotalContrib.RTom.Items[i].infoCRTom.Count - 1 do
           RespostaConsultainfoCRTom(i, j);
      end;

      for i := 0 to infoTotalContrib.RPrest.Count -1 do
        RespostaConsultaRPrest(i);

      for i := 0 to infoTotalContrib.RRecRepAD.Count -1 do
        RespostaConsultaRRecRepAD(i);

      for i := 0 to infoTotalContrib.RComl.Count -1 do
        RespostaConsultaRComl(i);

      for i := 0 to infoTotalContrib.RCPRB.Count -1 do
        RespostaConsultaRCPRB(i);
    end;
  end;
end;

procedure TRespostas.RespostaConsultaideContri(const ASessao: String; const AItem: TideContrib; const AStatus: TStatus);
var
  Resp: TRespostaideContri;
begin
  Resp := TRespostaideContri.Create(ASessao, TpResp, codUTF8);
  try
    Resp.Processar(AItem, AStatus);
    Resposta := Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;


{ TConsultaRespostatotApurDia }

constructor TConsultaRespostatotApurDia.Create(const ASessao: String;
 const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
end;

procedure TConsultaRespostatotApurDia.Processar(const ACont: Integer;
 const AItem: TtotApurDiaCollection);
begin
  FperApurDia := AItem.Items[ACont].perApurDia;
  FCRDia := AItem.Items[ACont].CRDia;
  FvlrCRDiaInf := AItem.Items[ACont].vlrCRDiaInf;
  FvlrCRDiaCalc := AItem.Items[ACont].vlrCRDiaCalc;
  FvlrCRDiaDCTF := AItem.Items[ACont].vlrCRDiaDCTF;
  FvlrCRDiaSuspInf := AItem.Items[ACont].vlrCRDiaSuspInf;
  FvlrCRDiaSuspCalc := AItem.Items[ACont].vlrCRDiaSuspCalc;
  FvlrCRDiaSuspDCTF := AItem.Items[ACont].vlrCRDiaSuspDCTF;
  FnatRend := AItem.Items[ACont].natRend;
end;

{ TConsultaRespostatotApurSem }

constructor TConsultaRespostatotApurSem.Create(const ASessao: String;
 const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
end;

procedure TConsultaRespostatotApurSem.Processar(const ACont: Integer;
 const AItem: TtotApurSemCollection);
begin
  FperApurSem := AItem.Items[ACont].perApurSem;
  FCRSem := AItem.Items[ACont].CRSem;
  FvlrCRSemInf := AItem.Items[ACont].vlrCRSemInf;
  FvlrCRSemCalc := AItem.Items[ACont].vlrCRSemCalc;
  FvlrCRSemDCTF := AItem.Items[ACont].vlrCRSemDCTF;
  FvlrCRSemSuspInf := AItem.Items[ACont].vlrCRSemSuspInf;
  FvlrCRSemSuspCalc := AItem.Items[ACont].vlrCRSemSuspCalc;
  FvlrCRSemSuspDCTF := AItem.Items[ACont].vlrCRSemSuspDCTF;
  FnatRend := AItem.Items[ACont].natRend;
end;

{ TConsultaRespostatotApurDec }

constructor TConsultaRespostatotApurDec.Create(const ASessao: String;
 const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
end;

procedure TConsultaRespostatotApurDec.Processar(const ACont: Integer;
 const AItem: TtotApurDecCollection);
begin
  FperApurDec := AItem.Items[ACont].perApurDec;
  FCRDec := AItem.Items[ACont].CRDec;
  FvlrCRDecInf := AItem.Items[ACont].vlrCRDecInf;
  FvlrCRDecCalc := AItem.Items[ACont].vlrCRDecCalc;
  FvlrCRDecDCTF := AItem.Items[ACont].vlrCRDecDCTF;
  FvlrCRDecSuspInf := AItem.Items[ACont].vlrCRDecSuspInf;
  FvlrCRDecSuspCalc := AItem.Items[ACont].vlrCRDecSuspCalc;
  FvlrCRDecSuspDCTF := AItem.Items[ACont].vlrCRDecSuspDCTF;
  FnatRend := AItem.Items[ACont].natRend;
end;

{ TConsultaRespostatotApurQui }

constructor TConsultaRespostatotApurQui.Create(const ASessao: String;
 const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
end;

procedure TConsultaRespostatotApurQui.Processar(const ACont: Integer;
 const AItem: TtotApurQuiCollection);
begin
  FperApurQui := AItem.Items[ACont].perApurQui;
  FCRQui := AItem.Items[ACont].CRQui;
  FvlrCRQuiInf := AItem.Items[ACont].vlrCRQuiInf;
  FvlrCRQuiCalc := AItem.Items[ACont].vlrCRQuiCalc;
  FvlrCRQuiDCTF := AItem.Items[ACont].vlrCRQuiDCTF;
  FvlrCRQuiSuspInf := AItem.Items[ACont].vlrCRQuiSuspInf;
  FvlrCRQuiSuspCalc := AItem.Items[ACont].vlrCRQuiSuspCalc;
  FvlrCRQuiSuspDCTF := AItem.Items[ACont].vlrCRQuiSuspDCTF;
  FnatRend := AItem.Items[ACont].natRend;
end;

{ TConsultaRespostatotApurMen }

constructor TConsultaRespostatotApurMen.Create(const ASessao: String;
 const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
end;

procedure TConsultaRespostatotApurMen.Processar(const ACont: Integer;
 const AItem: TtotApurMenCollection);
begin
  FCRMen := AItem.Items[ACont].CRMen;
  FvlrCRMenInf := AItem.Items[ACont].vlrCRMenInf;
  FvlrCRMenCalc := AItem.Items[ACont].vlrCRMenCalc;
  FvlrCRMenDCTF := AItem.Items[ACont].vlrCRMenDCTF;
  FvlrCRMenSuspInf := AItem.Items[ACont].vlrCRMenSuspInf;
  FvlrCRMenSuspCalc := AItem.Items[ACont].vlrCRMenSuspCalc;
  FvlrCRMenSuspDCTF := AItem.Items[ACont].vlrCRMenSuspDCTF;
  FnatRend := AItem.Items[ACont].natRend;
end;

{ TConsultaRespostainfoCR_CNR }

constructor TConsultaRespostainfoCR_CNR.Create(const ASessao: String;
 const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
end;

procedure TConsultaRespostainfoCR_CNR.Processar(const AItem: TinfoCR_CNR);
begin
  FindExistInfo := indExistInfoToStr(AItem.indExistInfo);
  FidentEscritDCTF := AItem.identEscritDCTF;
end;

{ TEnvioRespostatotApurTribDia }

constructor TEnvioRespostatotApurTribDia.Create(const ASessao: String;
 const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
end;

procedure TEnvioRespostatotApurTribDia.Processar(const AACBrReinf: TACBrReinf;
 const ACont, ACont2: Integer);
begin
  with AACBrReinf.WebServices.Consultar.RetEnvioLote.evento.Items[ACont].evtRet.InfoTotal.ideEstab.totApurDia.Items[ACont2] do
  begin
    FvlrCRDiaInf := totApurTribDia.vlrCRDiaInf;
    FvlrCRDiaCalc := totApurTribDia.vlrCRDiaCalc;
    FvlrCRDiaSuspInf := totApurTribDia.vlrCRDiaSuspInf;
    FvlrCRDiaSuspCalc := totApurTribDia.vlrCRDiaSuspCalc;
  end;
end;

{ TEnvioRespostatotApurDia }

constructor TEnvioRespostatotApurDia.Create(const ASessao: String;
 const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
end;

procedure TEnvioRespostatotApurDia.Processar(const AACBrReinf: TACBrReinf;
 const ACont, ACont2: Integer);
begin
  with AACBrReinf.WebServices.Consultar.RetEnvioLote.evento.Items[ACont].evtRet.InfoTotal.ideEstab do
  begin
    FperApurDia := totApurDia.Items[ACont2].perApurDia;
    FCRDia := totApurDia.Items[ACont2].CRDia;
    FvlrBaseCRDia := totApurDia.Items[ACont2].vlrBaseCRDia;
    FvlrBaseCRDiaSusp := totApurDia.Items[ACont2].vlrBaseCRDiaSusp;
    FnatRend := totApurDia.Items[ACont2].natRend;
  end;
end;

{ TEnvioRespostatotApurTribSem }

constructor TEnvioRespostatotApurTribSem.Create(const ASessao: String;
 const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
end;

procedure TEnvioRespostatotApurTribSem.Processar(const AACBrReinf: TACBrReinf;
 const ACont, ACont2: Integer);
begin
  with AACBrReinf.WebServices.Consultar.RetEnvioLote.evento.Items[ACont].evtRet.InfoTotal.ideEstab.totApurSem.Items[ACont2] do
  begin
    FvlrCRSemInf := totApurTribSem.vlrCRSemInf;
    FvlrCRSemCalc := totApurTribSem.vlrCRSemCalc;
    FvlrCRSemSuspInf := totApurTribSem.vlrCRSemSuspInf;
    FvlrCRSemSuspCalc := totApurTribSem.vlrCRSemSuspCalc;
  end;
end;

{ TEnvioRespostatotApurSem }

constructor TEnvioRespostatotApurSem.Create(const ASessao: String;
 const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
end;

procedure TEnvioRespostatotApurSem.Processar(const AACBrReinf: TACBrReinf;
 const ACont, ACont2: Integer);
begin
  with AACBrReinf.WebServices.Consultar.RetEnvioLote.evento.Items[ACont].evtRet.InfoTotal.ideEstab do
  begin
    FperApurSem := tpPerApurSemToStr(totApurSem.Items[ACont2].perApurSem);
    FCRSem := totApurSem.Items[ACont2].CRSem;
    FvlrBaseCRSem := totApurSem.Items[ACont2].vlrBaseCRSem;
    FvlrBaseCRSemSusp := totApurSem.Items[ACont2].vlrBaseCRSemSusp;
    FnatRend := totApurSem.Items[ACont2].natRend;
  end;
end;

{ TEnvioRespostatotApurTribDec }

constructor TEnvioRespostatotApurTribDec.Create(const ASessao: String;
 const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
end;

procedure TEnvioRespostatotApurTribDec.Processar(const AACBrReinf: TACBrReinf;
 const ACont, ACont2: Integer);
begin
  with AACBrReinf.WebServices.Consultar.RetEnvioLote.evento.Items[ACont].evtRet.InfoTotal.ideEstab.totApurDec.Items[ACont2] do
  begin
    FvlrCRDecInf := totApurTribDec.vlrCRDecInf;
    FvlrCRDecCalc := totApurTribDec.vlrCRDecCalc;
    FvlrCRDecSuspInf := totApurTribDec.vlrCRDecSuspInf;
    FvlrCRDecSuspCalc := totApurTribDec.vlrCRDecSuspCalc;
  end;
end;

{ TEnvioRespostatotApurDec }

constructor TEnvioRespostatotApurDec.Create(const ASessao: String;
 const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
end;

procedure TEnvioRespostatotApurDec.Processar(const AACBrReinf: TACBrReinf;
 const ACont, ACont2: Integer);
begin
  with AACBrReinf.WebServices.Consultar.RetEnvioLote.evento.Items[ACont].evtRet.InfoTotal.ideEstab do
  begin
    FperApurDec := tpPerApurDecToStr(totApurDec.Items[ACont2].perApurDec);
    FCRDec := totApurDec.Items[ACont2].CRDec;
    FvlrBaseCRDec := totApurDec.Items[ACont2].vlrBaseCRDec;
    FvlrBaseCRDecSusp := totApurDec.Items[ACont2].vlrBaseCRDecSusp;
    FnatRend := totApurDec.Items[ACont2].natRend;
  end;
end;

{ TEnvioRespostatotApurTribQui }

constructor TEnvioRespostatotApurTribQui.Create(const ASessao: String;
 const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
end;

procedure TEnvioRespostatotApurTribQui.Processar(const AACBrReinf: TACBrReinf;
 const ACont, ACont2: Integer);
begin
  with AACBrReinf.WebServices.Consultar.RetEnvioLote.evento.Items[ACont].evtRet.InfoTotal.ideEstab.totApurQui.Items[ACont2] do
  begin
    FvlrCRQuiInf := totApurTribQui.vlrCRQuiInf;
    FvlrCRQuiCalc := totApurTribQui.vlrCRQuiCalc;
    FvlrCRQuiSuspInf := totApurTribQui.vlrCRQuiSuspInf;
    FvlrCRQuiSuspCalc := totApurTribQui.vlrCRQuiSuspCalc;
  end;
end;

{ TEnvioRespostatotApurQui }

constructor TEnvioRespostatotApurQui.Create(const ASessao: String;
 const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
end;

procedure TEnvioRespostatotApurQui.Processar(const AACBrReinf: TACBrReinf;
 const ACont, ACont2: Integer);
begin
  with AACBrReinf.WebServices.Consultar.RetEnvioLote.evento.Items[ACont].evtRet.InfoTotal.ideEstab do
  begin
    FperApurQui := tpPerApurQuiToStr(totApurQui.Items[ACont2].perApurQui);
    FCRQui := totApurQui.Items[ACont2].CRQui;
    FvlrBaseCRQui := totApurQui.Items[ACont2].vlrBaseCRQui;
    FvlrBaseCRQuiSusp := totApurQui.Items[ACont2].vlrBaseCRQuiSusp;
    FnatRend := totApurQui.Items[ACont2].natRend;
  end;
end;

{ TEnvioRespostatotApurTribMen }

constructor TEnvioRespostatotApurTribMen.Create(const ASessao: String;
 const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
end;

procedure TEnvioRespostatotApurTribMen.Processar(const AACBrReinf: TACBrReinf;
 const ACont, ACont2: Integer);
begin
  with AACBrReinf.WebServices.Consultar.RetEnvioLote.evento.Items[ACont].evtRet.InfoTotal.ideEstab.totApurMen.Items[ACont2] do
  begin
    FvlrCRMenInf := totApurTribMen.vlrCRMenInf;
    FvlrCRMenCalc := totApurTribMen.vlrCRMenCalc;
    FvlrCRMenSuspInf := totApurTribMen.vlrCRMenSuspInf;
    FvlrCRMenSuspCalc := totApurTribMen.vlrCRMenSuspCalc;
  end;
end;

{ TEnvioRespostatotApurMen }

constructor TEnvioRespostatotApurMen.Create(const ASessao: String;
 const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
end;

procedure TEnvioRespostatotApurMen.Processar(const AACBrReinf: TACBrReinf;
 const ACont, ACont2: Integer);
begin
  with AACBrReinf.WebServices.Consultar.RetEnvioLote.evento.Items[ACont].evtRet.InfoTotal.ideEstab do
  begin
    FCRMen := totApurMen.Items[ACont2].CRMen;
    FvlrBaseCRMen := totApurMen.Items[ACont2].vlrBaseCRMen;
    FvlrBaseCRMenSusp := totApurMen.Items[ACont2].vlrBaseCRMenSusp;
    FnatRend := totApurMen.Items[ACont2].natRend;
  end;
end;

{ TEnvioRespostaideEstab }

constructor TEnvioRespostaideEstab.Create(const ASessao: String;
  const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
end;

procedure TEnvioRespostaideEstab.Processar(const AItem: TideEstab);
begin
  FtpInsc := TpInscricaoToStr(AItem.tpInsc);
  FnrInsc := AItem.nrInsc;
  FnrInscBenef := AItem.nrInscBenef;
  FnmBenef := AItem.nmBenef;
  FideEvtAdic := AItem.ideEvtAdic;
end;

{ TRespostaEventoRecibo }

constructor TRespostaEventoRecibo.Create(const ASessao: String;
  const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
end;

procedure TRespostaEventoRecibo.ProcessarInfoTotalContrib(
 const AACBrReinf: TACBrReinf; const ACont: Integer);
begin
  with AACBrReinf.WebServices.ConsultarReciboEvento.evtTotalContribVersao do
  begin
    Fid := RetornoEventos.Items[ACont].id;
    FInicioValidade := RetornoEventos.Items[ACont].iniValid;
    FDataHoraReceb := RetornoEventos.Items[ACont].dtHoraRecebimento;
    FNrRecibo := RetornoEventos.Items[ACont].nrRecibo;
    FSituacaoEvento := RetornoEventos.Items[ACont].situacaoEvento;
    FAplicacaoRecepcao := RetornoEventos.Items[ACont].aplicacaoRecepcao;
  end;
end;

{ TEnvioRespostaRRecEspetDesp }

constructor TEnvioRespostaRRecEspetDesp.Create(const ASessao: String;
  const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
end;

procedure TEnvioRespostaRRecEspetDesp.Processar(
 const AItem: TeventoCollectionItem);
begin
  FvlrReceitaTotal := AItem.evtTotal.InfoTotal.RRecEspetDesp.vlrReceitaTotal;
  FvlrCPApurTotal := AItem.evtTotal.InfoTotal.RRecEspetDesp.vlrCPApurTotal;
  FvlrCPSuspTotal := AItem.evtTotal.InfoTotal.RRecEspetDesp.vlrCPSuspTotal;

  // Versão 1.03.02
  FCRRecEspetDesp := AItem.evtTotal.InfoTotal.RRecEspetDesp.CRRecEspetDesp;
  FvlrCRRecEspetDesp := AItem.evtTotal.InfoTotal.RRecEspetDesp.vlrCRRecEspetDesp;
  FvlrCRRecEspetDespSusp := AItem.evtTotal.InfoTotal.RRecEspetDesp.vlrCRRecEspetDespSusp;
end;

{ TEnvioRespostaevtTotal }

constructor TEnvioRespostaevtTotal.Create(const ASessao: String;
  const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
end;

procedure TEnvioRespostaevtTotal.Processar(const AACBrReinf: TACBrReinf; const ACont: Integer);
begin
  with AACBrReinf.WebServices.EnvioLote.RetEnvioLote.evento.Items[ACont].evtTotal do
  begin
    FId := Id;
  end;
end;

{ TEnvioRespostaevento }

constructor TEnvioRespostaevento.Create(const ASessao: String;
  const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
end;

procedure TEnvioRespostaevento.Processar(const AACBrReinf: TACBrReinf; const ACont: Integer);
begin
  with AACBrReinf.WebServices.EnvioLote.RetEnvioLote do
  begin
    FId := evento.Items[ACont].Id;
  end;
end;

{ TEnvioRespostaOcorrencias }

constructor TEnvioRespostaOcorrencias.Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo;
  const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
end;

procedure TEnvioRespostaOcorrencias.Processar(const AACBrReinf: TACBrReinf; const ACont: Integer);
begin
  with AACBrReinf.WebServices.EnvioLote.RetEnvioLote do
  begin
    Ftipo := Status.Ocorrencias.Items[ACont].tipo;
    FlocalizacaoErroAviso := Status.Ocorrencias.Items[ACont].Localizacao;
    Fcodigo := Status.Ocorrencias.Items[ACont].Codigo;
    Fdescricao := Status.Ocorrencias.Items[ACont].Descricao;
  end;
end;

{ TEnvioRespostastatus }

constructor TEnvioRespostastatus.Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespEnviostatus, ATipo, AFormato);
end;

procedure TEnvioRespostastatus.Processar(const AACBrReinf: TACBrReinf);
begin
  with AACBrReinf.WebServices.EnvioLote.RetEnvioLote do
  begin
    FcdStatus := Status.cdStatus;
    FdescRetorno := Status.descRetorno;
  end;
end;

{ TEnvioRespostadadosRecepcaoLote }

constructor TEnvioRespostadadosRecepcaoLote.Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespEnviodadosRecepcaoLote, ATipo, AFormato);
end;

procedure TEnvioRespostadadosRecepcaoLote.Processar(const AItem: TRetEnvioLote);
begin
  FdhRecepcao := AItem.dadosRecepcaoLote.dhRecepcao;
  FversaoAplicativoRecepcao := AItem.dadosRecepcaoLote.versaoAplicativoRecepcao;
  FprotocoloEnvio := AItem.dadosRecepcaoLote.protocoloEnvio;
end;

{ TEnvioRespostaideTransmissor }

constructor TEnvioRespostaideTransmissor.Create(
  const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespEnvioideTransmissor, ATipo, AFormato);
end;

procedure TEnvioRespostaideTransmissor.Processar(const AACBrReinf: TACBrReinf);
begin
  with AACBrReinf.WebServices.EnvioLote.RetEnvioLote do
  begin
    FIdTransmissor := ideTransmissor.IdTransmissor;
  end;
end;

{ TEnvioResposta }

constructor TEnvioResposta.Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespEnvio, ATipo, AFormato);
end;

procedure TEnvioResposta.Processar(const AACBrReinf: TACBrReinf);
begin
  with AACBrReinf.WebServices.EnvioLote.RetEnvioLote do
  begin
    FId := Id;
  end;
end;

{ TRespostaRCPRB }

constructor TRespostaRCPRB.Create(const ASessao: String;
  const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
end;

procedure TRespostaRCPRB.Processar(const ACont2: Integer;
 const AItem: TeventoCollectionItem);
begin
  FcodRec := AItem.evtTotal.InfoTotal.RCPRB.Items[ACont2].codRec;
  FvlrCPApurTotal := AItem.evtTotal.InfoTotal.RCPRB.Items[ACont2].vlrCPApurTotal;
  FvlrCPRBSusp := AItem.evtTotal.InfoTotal.RCPRB.Items[ACont2].vlrCPRBSusp;

  // Versão 1.03.02
  FCRCPRB := AItem.evtTotal.InfoTotal.RCPRB.Items[ACont2].CRCPRB;
  FvlrCRCPRB := AItem.evtTotal.InfoTotal.RCPRB.Items[ACont2].vlrCRCPRB;
  FvlrCRCPRBSusp := AItem.evtTotal.InfoTotal.RCPRB.Items[ACont2].vlrCRCPRBSusp;
end;

procedure TRespostaRCPRB.ProcessarInfoTotalContrib(
 const AACBrReinf: TACBrReinf; const ACont: Integer);
begin
  with AACBrReinf.WebServices.Consultar.evtTotalContribVersao do
  begin
    FcodRec := InfoTotalContrib.RCPRB.Items[ACont].codRec;
    FvlrCPApurTotal := InfoTotalContrib.RCPRB.Items[ACont].vlrCPApurTotal;
    FvlrCPRBSusp := InfoTotalContrib.RCPRB.Items[ACont].vlrCPRBSusp;

    // Versão 1.03.02
    FCRCPRB := InfoTotalContrib.RCPRB.Items[ACont].CRCPRB;
    FvlrCRCPRB := InfoTotalContrib.RCPRB.Items[ACont].vlrCRCPRB;
    FvlrCRCPRBSusp := InfoTotalContrib.RCPRB.Items[ACont].vlrCRCPRBSusp;
  end;
end;

{ TRespostaRComl }

constructor TRespostaRComl.Create(const ASessao: String;
  const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
end;

procedure TRespostaRComl.Processar(const ACont2: Integer;
 const AItem: TeventoCollectionItem);
begin
  FvlrCPApur := AItem.evtTotal.InfoTotal.RComl.Items[ACont2].vlrCPApur;
  FvlrRatApur := AItem.evtTotal.InfoTotal.RComl.Items[ACont2].vlrRatApur;
  FvlrSenarApur := AItem.evtTotal.InfoTotal.RComl.Items[ACont2].vlrSenarApur;
  FvlrCPSusp := AItem.evtTotal.InfoTotal.RComl.Items[ACont2].vlrCPSusp;
  FvlrRatSusp := AItem.evtTotal.InfoTotal.RComl.Items[ACont2].vlrRatSusp;
  FvlrSenarSusp := AItem.evtTotal.InfoTotal.RComl.Items[ACont2].vlrSenarSusp;

  // Versão 1.03.02
  FCRComl := AItem.evtTotal.InfoTotal.RComl.Items[ACont2].CRComl;
  FvlrCRComl := AItem.evtTotal.InfoTotal.RComl.Items[ACont2].vlrCRComl;
  FvlrCRComlSusp := AItem.evtTotal.InfoTotal.RComl.Items[ACont2].vlrCRComlSusp;
end;

procedure TRespostaRComl.ProcessarInfoTotalContrib(
 const AACBrReinf: TACBrReinf; const ACont: Integer);
begin
  with AACBrReinf.WebServices.Consultar.evtTotalContribVersao do
  begin
    FvlrCPApur     := InfoTotalContrib.RComl.Items[ACont].vlrCPApur;
    FvlrRatApur    := InfoTotalContrib.RComl.Items[ACont].vlrRatApur;
    FvlrSenarApur  := InfoTotalContrib.RComl.Items[ACont].vlrSenarApur;
    FvlrCPSusp     := InfoTotalContrib.RComl.Items[ACont].vlrCPSusp;
    FvlrRatSusp    := InfoTotalContrib.RComl.Items[ACont].vlrRatSusp;
    FvlrSenarSusp  := InfoTotalContrib.RComl.Items[ACont].vlrSenarSusp;

    // Versão 1.03.02
    FCRComl        := InfoTotalContrib.RComl.Items[ACont].CRComl;
    FvlrCRComl     := InfoTotalContrib.RComl.Items[ACont].vlrCRComl;
    FvlrCRComlSusp := InfoTotalContrib.RComl.Items[ACont].vlrCRComlSusp;
  end;
end;

{ TRespostaRAquis }

constructor TRespostaRAquis.Create(const ASessao: String;
 const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
end;

procedure TRespostaRAquis.Processar(const ACont2: Integer;
 const AItem: TeventoCollectionItem);
begin
  FCRAquis := AItem.evtTotal.InfoTotal.RAquis.Items[ACont2].CRAquis;
  FvlrCRAquis := AItem.evtTotal.InfoTotal.RAquis.Items[ACont2].vlrCRAquis;
  FvlrCRAquisSusp := AItem.evtTotal.InfoTotal.RAquis.Items[ACont2].vlrCRAquisSusp;
end;

{ TRespostaRRecRepAD }

constructor TRespostaRRecRepAD.Create(const ASessao: String;
  const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
end;

procedure TRespostaRRecRepAD.Processar(const ACont2: Integer;
 const AItem: TeventoCollectionItem);
begin
  FcnpjAssocDesp := AItem.evtTotal.InfoTotal.RRecRepAD.Items[ACont2].cnpjAssocDesp;
  FvlrTotalRep := AItem.evtTotal.InfoTotal.RRecRepAD.Items[ACont2].vlrTotalRep;
  FvlrTotalRet := AItem.evtTotal.InfoTotal.RRecRepAD.Items[ACont2].vlrTotalRet;
  FvlrTotalNRet := AItem.evtTotal.InfoTotal.RRecRepAD.Items[ACont2].vlrTotalNRet;

  // Versão 1.03.02
  FCRRecRepAD := AItem.evtTotal.InfoTotal.RRecRepAD.Items[ACont2].CRRecRepAD;
  FvlrCRRecRepAD := AItem.evtTotal.InfoTotal.RRecRepAD.Items[ACont2].vlrCRRecRepAD;
  FvlrCRRecRepADSusp := AItem.evtTotal.InfoTotal.RRecRepAD.Items[ACont2].vlrCRRecRepADSusp;
end;

procedure TRespostaRRecRepAD.ProcessarInfoTotalContrib(
 const AACBrReinf: TACBrReinf; const ACont: Integer);
begin
  with AACBrReinf.WebServices.Consultar.evtTotalContribVersao do
  begin
    FcnpjAssocDesp := InfoTotalContrib.RRecRepAD.Items[ACont].cnpjAssocDesp;
    FvlrTotalRep := InfoTotalContrib.RRecRepAD.Items[ACont].vlrTotalRep;
    FvlrTotalRet := InfoTotalContrib.RRecRepAD.Items[ACont].vlrTotalRet;
    FvlrTotalNRet := InfoTotalContrib.RRecRepAD.Items[ACont].vlrTotalNRet;

    // Versão 1.03.02
    FCRRecRepAD := InfoTotalContrib.RRecRepAD.Items[ACont].CRRecRepAD;
    FvlrCRRecRepAD := InfoTotalContrib.RRecRepAD.Items[ACont].vlrCRRecRepAD;
    FvlrCRRecRepADSusp := InfoTotalContrib.RRecRepAD.Items[ACont].vlrCRRecRepADSusp;
  end;
end;

{ TRespostaRPrest }

constructor TRespostaRPrest.Create(const ASessao: String;
  const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
end;

procedure TRespostaRPrest.Processar(const AItem: TeventoCollectionItem);
begin
  FtpInscTomador := TpInscricaoToStr(AItem.evtTotal.InfoTotal.RPrest.tpInscTomador);
  FnrInscTomador := AItem.evtTotal.InfoTotal.RPrest.nrInscTomador;
  FvlrTotalBaseRet := AItem.evtTotal.InfoTotal.RPrest.vlrTotalBaseRet;
  FvlrTotalRetPrinc := AItem.evtTotal.InfoTotal.RPrest.vlrTotalRetPrinc;
  FvlrTotalRetAdic := AItem.evtTotal.InfoTotal.RPrest.vlrTotalRetAdic;
  FvlrTotalNRetPrinc := AItem.evtTotal.InfoTotal.RPrest.vlrTotalNRetPrinc;
  FvlrTotalNRetAdic := AItem.evtTotal.InfoTotal.RPrest.vlrTotalNRetAdic;
end;

procedure TRespostaRPrest.ProcessarInfoTotalContrib(
 const AACBrReinf: TACBrReinf; const ACont: Integer);
begin
  with AACBrReinf.WebServices.Consultar.evtTotalContribVersao do
  begin
    FtpInscTomador := TpInscricaoToStr(InfoTotalContrib.RPrest.Items[ACont].tpInscTomador);
    FnrInscTomador := InfoTotalContrib.RPrest.Items[ACont].nrInscTomador;
    FvlrTotalBaseRet := InfoTotalContrib.RPrest.Items[ACont].vlrTotalBaseRet;
    FvlrTotalRetPrinc := InfoTotalContrib.RPrest.Items[ACont].vlrTotalRetPrinc;
    FvlrTotalRetAdic := InfoTotalContrib.RPrest.Items[ACont].vlrTotalRetAdic;
    FvlrTotalNRetPrinc := InfoTotalContrib.RPrest.Items[ACont].vlrTotalNRetPrinc;
    FvlrTotalNRetAdic := InfoTotalContrib.RPrest.Items[ACont].vlrTotalNRetAdic;
  end;
end;

{ TRespostainfoCRTom }

constructor TRespostainfoCRTom.Create(const ASessao: String;
  const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
end;

procedure TRespostainfoCRTom.Processar(const ACont2: Integer;
 const AItem: TeventoCollectionItem);
begin
  FCRTom := AItem.evtTotal.InfoTotal.RTom.infoCRTom.Items[ACont2].CRTom;
  FVlrCRTom := AItem.evtTotal.InfoTotal.RTom.infoCRTom.Items[ACont2].VlrCRTom;
  FVlrCRTomSusp := AItem.evtTotal.InfoTotal.RTom.infoCRTom.Items[ACont2].VlrCRTomSusp;
end;

procedure TRespostainfoCRTom.ProcessarInfoTotalContrib(
 const AACBrReinf: TACBrReinf; const ACont, ACont2: Integer);
begin
  with AACBrReinf.WebServices.Consultar.evtTotalContribVersao do
  begin
    FCRTom := InfoTotalContrib.RTom.Items[ACont].infoCRTom.Items[ACont2].CRTom;
    FVlrCRTom := InfoTotalContrib.RTom.Items[ACont].infoCRTom.Items[ACont2].VlrCRTom;
    FVlrCRTomSusp := InfoTotalContrib.RTom.Items[ACont].infoCRTom.Items[ACont2].VlrCRTomSusp;
  end;
end;

{ TRespostaRTom }

constructor TRespostaRTom.Create(const ASessao: String;
  const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
end;

procedure TRespostaRTom.Processar(const AItem: TeventoCollectionItem);
begin
  with AItem.evtTotal do
  begin
    FcnpjPrestador := InfoTotal.RTom.cnpjPrestador;
    FvlrTotalBaseRet := InfoTotal.RTom.vlrTotalBaseRet;
    FvlrTotalRetPrinc := InfoTotal.RTom.vlrTotalRetPrinc;
    FvlrTotalRetAdic := InfoTotal.RTom.vlrTotalRetAdic;
    FvlrTotalNRetPrinc := InfoTotal.RTom.vlrTotalNRetPrinc;
    FvlrTotalNRetAdic := InfoTotal.RTom.vlrTotalNRetAdic;
  end;
end;

procedure TRespostaRTom.ProcessarInfoTotalContrib(const AACBrReinf: TACBrReinf;
  const ACont: Integer);
begin
  with AACBrReinf.WebServices.Consultar.evtTotalContribVersao do
  begin
    FcnpjPrestador := InfoTotalContrib.RTom.Items[ACont].cnpjPrestador;
    FvlrTotalBaseRet := InfoTotalContrib.RTom.Items[ACont].vlrTotalBaseRet;
    FvlrTotalRetPrinc := InfoTotalContrib.RTom.Items[ACont].vlrTotalRetPrinc;
    FvlrTotalRetAdic := InfoTotalContrib.RTom.Items[ACont].vlrTotalRetAdic;
    FvlrTotalNRetPrinc := InfoTotalContrib.RTom.Items[ACont].vlrTotalNRetPrinc;
    FvlrTotalNRetAdic := InfoTotalContrib.RTom.Items[ACont].vlrTotalNRetAdic;
  end;
end;

{ TRespostainfoTotal_infoTotalContrib }

constructor TRespostainfoTotal_infoTotalContrib.Create(const ASessao: String;
  const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
end;

procedure TRespostainfoTotal_infoTotalContrib.Processar(const AACBrReinf: TACBrReinf; const ACont: Integer);
begin
  if AACBrReinf.Configuracoes.Geral.VersaoDF < v2_01_01 then
  begin
    with AACBrReinf.WebServices.EnvioLote.RetEnvioLote.evento.Items[ACont].evtTotal do
    begin
      FnrRecArqBase := InfoTotal.nrRecArqBase;
    end;
  end;
end;

procedure TRespostainfoTotal_infoTotalContrib.ProcessarInfoTotalContrib(
 const AACBrReinf: TACBrReinf);
begin
  FnrRecArqBase := AACBrReinf.WebServices.Consultar.evtTotalContribVersao.InfoTotalContrib.nrRecArqBase;
  FindExistInfo := indExistInfoToStr(AACBrReinf.WebServices.Consultar.evtTotalContribVersao.InfoTotalContrib.indExistInfo);
end;

{ TRespostainfoRecEv }

constructor TRespostainfoRecEv.Create(const ASessao: String;
  const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
end;

procedure TRespostainfoRecEv.Processar(const AItem: TInfoRecEv);
begin
  FnrRecArqBase := AItem.nrRecArqBase;
  FnrProtLote := AItem.nrProtLote;
  FdhRecepcao := AItem.dhRecepcao;
  FnrProtEntr := AItem.nrProtEntr;
  FdhProcess := AItem.dhProcess;
  FtpEv := AItem.tpEv;
  FidEv := AItem.idEv;
  Fhash := AItem.hash;
end;

{ TRespostaregOcorrs }

constructor TRespostaregOcorrs.Create(const ASessao: String;
  const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
end;

procedure TRespostaregOcorrs.Processar(const AContX: Integer; const AItem: TIdeStatus);
begin
  FtpOcorr := AItem.regOcorrs.Items[AContX].tpOcorr;
  FlocalErroAviso := AItem.regOcorrs.Items[AContX].localErroAviso;
  FcodResp := AItem.regOcorrs.Items[AContX].codResp;
  FdscResp := AItem.regOcorrs.Items[AContX].dscResp;
end;

procedure TRespostaregOcorrs.ProcessarInfoTotalContrib(
 const AACBrReinf: TACBrReinf; const ACont: Integer);
begin
  with AACBrReinf.WebServices.ConsultarReciboEvento.evtTotalContribVersao do
  begin
    FtpOcorr := IdeStatus.regOcorrs.Items[ACont].tpOcorr;
    FlocalErroAviso := IdeStatus.regOcorrs.Items[ACont].localErroAviso;
    FcodResp := IdeStatus.regOcorrs.Items[ACont].codResp;
    FdscResp := IdeStatus.regOcorrs.Items[ACont].dscResp;
  end;
end;

{ TRespostaideStatus }

constructor TRespostaideStatus.Create(const ASessao: String;
  const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
end;

procedure TRespostaideStatus.Processar(const AItem: TIdeStatus);
begin
  FcdRetorno := AItem.cdRetorno;
  FdescRetorno := AItem.descRetorno;
end;

procedure TRespostaideStatus.ProcessarInfoTotalContrib(
 const AACBrReinf: TACBrReinf);
begin
  with AACBrReinf.WebServices.ConsultarReciboEvento.evtTotalContribVersao do
  begin
    FcdRetorno := IdeStatus.cdRetorno;
    FdescRetorno := IdeStatus.descRetorno;
  end;
end;

{ TRespostaideContri }

constructor TRespostaideContri.Create(const ASessao: String;
  const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
end;

procedure TRespostaideContri.Processar(const AItem: TideContrib; const AStatus: TStatus);
begin
  if Assigned(AStatus) then
  begin
    FCodigo := IntToStr(AStatus.cdStatus);
    FMensagem := AStatus.descRetorno;
  end;

  FtpInsc := TpInscricaoToStr(AItem.TpInsc);
  FnrInsc := AItem.nrInsc;
end;

{ TRespostaideEvento }

constructor TRespostaideEvento.Create(const ASessao: String;
  const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
end;

procedure TRespostaideEvento.Processar(const AItem: TIdeEvento1);
begin
  FperApur := AItem.perApur;
end;

{ TConsultaResposta }

constructor TConsultaResposta.Create(const ATipo: TACBrLibRespostaTipo;
  const AFormato: TACBrLibCodificacao; const ASessao: String);
begin
  inherited Create(ifThen(ASessao = '', CSessaoRespConsulta, ASessao), ATipo, AFormato);
end;

procedure TConsultaResposta.Processar(const AId: String);
begin
  FId := AId;
end;

{ TPadraoReinfResposta }

constructor TPadraoReinfResposta.Create(const ASessao: String;
  const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
end;

end.

