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
  Classes, SysUtils, ACBrLibResposta, ACBrLibReinfConsts;

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
    constructor Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;

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
    FdhProcess: TDateTime;
    Fhash: String;
    FidEv: String;
    FnrProtEntr: String;
    FtpEv: String;
  public
    constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo;
      const AFormato: TACBrLibCodificacao); reintroduce;

  published
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

implementation

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

constructor TConsultaResposta.Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespConsulta, ATipo, AFormato);
end;

{ TPadraoReinfResposta }

constructor TPadraoReinfResposta.Create(const ASessao: String;
  const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
end;

end.

