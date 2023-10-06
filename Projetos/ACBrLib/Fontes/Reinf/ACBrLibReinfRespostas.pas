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
  ACBrLibReinfConsts, pcnConversaoReinf;

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

  published
    property Id: String read FId write FId;
  end;

  { TEnvioRespostaideTransmissor }

  TEnvioRespostaideTransmissor = class(TPadraoReinfResposta)
  private
    FIdTransmissor: String;
  public
    constructor Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;

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

implementation

{ TConsultaRespostatotApurDia }

constructor TConsultaRespostatotApurDia.Create(const ASessao: String;
 const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
end;

{ TConsultaRespostatotApurSem }

constructor TConsultaRespostatotApurSem.Create(const ASessao: String;
 const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
end;

{ TConsultaRespostatotApurDec }

constructor TConsultaRespostatotApurDec.Create(const ASessao: String;
 const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
end;

{ TConsultaRespostatotApurQui }

constructor TConsultaRespostatotApurQui.Create(const ASessao: String;
 const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
end;

{ TConsultaRespostatotApurMen }

constructor TConsultaRespostatotApurMen.Create(const ASessao: String;
 const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
end;

{ TConsultaRespostainfoCR_CNR }

constructor TConsultaRespostainfoCR_CNR.Create(const ASessao: String;
 const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
end;

{ TEnvioRespostatotApurTribDia }

constructor TEnvioRespostatotApurTribDia.Create(const ASessao: String;
 const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
end;

{ TEnvioRespostatotApurDia }

constructor TEnvioRespostatotApurDia.Create(const ASessao: String;
 const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
end;

{ TEnvioRespostatotApurTribSem }

constructor TEnvioRespostatotApurTribSem.Create(const ASessao: String;
 const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
end;

{ TEnvioRespostatotApurSem }

constructor TEnvioRespostatotApurSem.Create(const ASessao: String;
 const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
end;

{ TEnvioRespostatotApurTribDec }

constructor TEnvioRespostatotApurTribDec.Create(const ASessao: String;
 const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
end;

{ TEnvioRespostatotApurDec }

constructor TEnvioRespostatotApurDec.Create(const ASessao: String;
 const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
end;

{ TEnvioRespostatotApurTribQui }

constructor TEnvioRespostatotApurTribQui.Create(const ASessao: String;
 const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
end;

{ TEnvioRespostatotApurQui }

constructor TEnvioRespostatotApurQui.Create(const ASessao: String;
 const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
end;

{ TEnvioRespostatotApurTribMen }

constructor TEnvioRespostatotApurTribMen.Create(const ASessao: String;
 const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
end;

{ TEnvioRespostatotApurMen }

constructor TEnvioRespostatotApurMen.Create(const ASessao: String;
 const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
end;

{ TEnvioRespostaideEstab }

constructor TEnvioRespostaideEstab.Create(const ASessao: String;
  const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
end;

{ TRespostaEventoRecibo }

constructor TRespostaEventoRecibo.Create(const ASessao: String;
  const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
end;

{ TEnvioRespostaRRecEspetDesp }

constructor TEnvioRespostaRRecEspetDesp.Create(const ASessao: String;
  const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
end;

{ TEnvioRespostaevtTotal }

constructor TEnvioRespostaevtTotal.Create(const ASessao: String;
  const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
end;

{ TEnvioRespostaevento }

constructor TEnvioRespostaevento.Create(const ASessao: String;
  const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
end;

{ TEnvioRespostaOcorrencias }

constructor TEnvioRespostaOcorrencias.Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo;
  const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
end;

{ TEnvioRespostastatus }

constructor TEnvioRespostastatus.Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespEnviostatus, ATipo, AFormato);
end;

{ TEnvioRespostadadosRecepcaoLote }

constructor TEnvioRespostadadosRecepcaoLote.Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespEnviodadosRecepcaoLote, ATipo, AFormato);
end;

{ TEnvioRespostaideTransmissor }

constructor TEnvioRespostaideTransmissor.Create(
  const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespEnvioideTransmissor, ATipo, AFormato);
end;

{ TEnvioResposta }

constructor TEnvioResposta.Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespEnvio, ATipo, AFormato);
end;

{ TRespostaRCPRB }

constructor TRespostaRCPRB.Create(const ASessao: String;
  const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
end;

{ TRespostaRComl }

constructor TRespostaRComl.Create(const ASessao: String;
  const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
end;

{ TRespostaRAquis }

constructor TRespostaRAquis.Create(const ASessao: String;
 const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
end;
{ TRespostaRRecRepAD }

constructor TRespostaRRecRepAD.Create(const ASessao: String;
  const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
end;

{ TRespostaRPrest }

constructor TRespostaRPrest.Create(const ASessao: String;
  const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
end;

{ TRespostainfoCRTom }

constructor TRespostainfoCRTom.Create(const ASessao: String;
  const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
end;

{ TRespostaRTom }

constructor TRespostaRTom.Create(const ASessao: String;
  const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
end;

{ TRespostainfoTotal_infoTotalContrib }

constructor TRespostainfoTotal_infoTotalContrib.Create(const ASessao: String;
  const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
end;

{ TRespostainfoRecEv }

constructor TRespostainfoRecEv.Create(const ASessao: String;
  const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
end;

{ TRespostaregOcorrs }

constructor TRespostaregOcorrs.Create(const ASessao: String;
  const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
end;

{ TRespostaideStatus }

constructor TRespostaideStatus.Create(const ASessao: String;
  const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
end;

{ TRespostaideContri }

constructor TRespostaideContri.Create(const ASessao: String;
  const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
end;

{ TRespostaideEvento }

constructor TRespostaideEvento.Create(const ASessao: String;
  const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
end;

{ TConsultaResposta }

constructor TConsultaResposta.Create(const ATipo: TACBrLibRespostaTipo;
  const AFormato: TACBrLibCodificacao; const ASessao: String);
begin
  inherited Create(ifThen(ASessao = '', CSessaoRespConsulta, ASessao), ATipo, AFormato);
end;

{ TPadraoReinfResposta }

constructor TPadraoReinfResposta.Create(const ASessao: String;
  const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
end;

end.

