{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }

{ Direitos Autorais Reservados (c) 2018 Daniel Simoes de Almeida               }

{ Colaboradores nesse arquivo: Rafael Teno Dias                                }

{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }

{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }

{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }

{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/gpl-license.php                           }

{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{        Rua Cel.Aureliano de Camargo, 973 - Tatuí - SP - 18270-170            }

{******************************************************************************}

{$I ACBr.inc}

unit ACBrLibBPeRespostas;

interface

uses
  SysUtils, Classes, ACBrLibResposta;

type

  { TLibBPeResposta }
  TLibBPeResposta = class(TACBrLibRespostaBase)
  private
    FMsg: string;
    Fversao: string;
    FtpAmb: string;
    FverAplic: string;
    FcStat: integer;
    FxMotivo: string;
    FcUF: integer;
    FdhRecbto: TDateTime;
  public
    constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo;
      const AFormato: TACBrLibCodificacao); reintroduce;

  published
    property Msg: string read FMsg write FMsg;
    property Versao: string read Fversao write Fversao;
    property tpAmb: string read FtpAmb write FtpAmb;
    property VerAplic: string read FverAplic write FverAplic;
    property CStat: integer read FcStat write FcStat;
    property XMotivo: string read FxMotivo write FxMotivo;
    property CUF: integer read FcUF write FcUF;
    property DhRecbto: TDateTime read FdhRecbto write FdhRecbto;
  end;

  { TStatusServicoResposta }
  TStatusServicoResposta = class(TLibBPeResposta)
  private
    FTMed: integer;
    FdhRetorno: TDateTime;
    FxObs: string;
  public
    constructor Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;

  published
    property TMed: integer read FTMed write FTMed;
    property DhRetorno: TDateTime read FdhRetorno write FdhRetorno;
    property XObs: string read FxObs write FxObs;
  end;

  { TConsultaBPeResposta }
  TConsultaBPeResposta = class(TLibBPeResposta)
  private
    FChBPe: String;
    FNProt: String;
    FDigVal: String;
  public
    constructor Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;

  published
    property ChBPe: String read FChBPe write FChBPe;
    property NProt: String read FNProt write FNProt;
    property DigVal: String read FDigVal write FDigVal;
  end;

  { TEnvioResposta }

  TEnvioResposta = class(TLibBPeResposta)
  private
    FtMed: integer;
    FnRec: string;
    FNProt: String;
  public
    constructor Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;

  published
   property TMed: integer read FtMed write FtMed;
   property NRec: string read FnRec write FnRec;
   property NProt: String read FNProt write FNProt;
  end;

  { TRetornoResposta }

  TRetornoResposta = class(TLibBPeResposta)
  private
    FnRec: string;
  public
    constructor Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;

  published
    property nRec: string read FnRec write FnRec;
  end;

  { TRetornoItemResposta }

  TRetornoItemResposta = class(TLibBPeResposta)
  private
    FchBPe: string;
    FnProt: string;
    FdigVal: string;
    Farquivo: string;
  public
    constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo;
      const AFormato: TACBrLibCodificacao); reintroduce;

  published
    property chBPe: string read FchBPe write FchBPe;
    property nProt: String read FnProt write FnProt;
    property digVal: String read FdigVal write FdigVal;
    property arquivo: String read Farquivo write Farquivo;
  end;

  { TCancelamentoResposta }

  TCancelamentoResposta = class(TLibBPeResposta)
  private
    FchBPe: string;
    FnProt: string;
    FtpEvento: string;
    FxEvento: string;
    FnSeqEvento: integer;
    FCNPJDest: string;
    FemailDest: string;
    Fxml: string;
  public
    constructor Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;

  published
    property chBPe: string read FchBPe write FchBPe;
    property nProt: string read FnProt write FnProt;
    property tpEvento: string read FtpEvento write FtpEvento;
    property xEvento: string read FxEvento write FxEvento;
    property nSeqEvento: integer read FnSeqEvento write FnSeqEvento;
    property CNPJDest: string read FCNPJDest write FCNPJDest;
    property emailDest: string read FemailDest write FemailDest;
    property XML: string read Fxml write Fxml;
  end;

  { TEventoResposta }

  TEventoResposta = class(TLibBPeResposta)
  private
    FidLote: Integer;
    FcOrgao: Integer;
  public
    constructor Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;

  published
    property idLote: Integer read FidLote write FidLote;
    property cOrgao: Integer read FcOrgao write FcOrgao;
  end;

  { TEventoItemResposta }

  TEventoItemResposta = class(TLibBPeResposta)
  private
    Farquivo: String;
    FchBPe: string;
    FCNPJDest: string;
    FcOrgao: integer;
    FdhRegEvento: TDateTime;
    FdigVal: String;
    FemailDest: string;
    FId: string;
    FnProt: String;
    FnSeqEvento: Integer;
    FtpEvento: string;
    FxEvento: string;
    FXML: string;
  public
    constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo;
      const AFormato: TACBrLibCodificacao); reintroduce;

  published
    property chBPe: string read FchBPe write FchBPe;
    property nProt: String read FnProt write FnProt;
    property arquivo: String read Farquivo write Farquivo;
    property digVal: String read FdigVal write FdigVal;
    property Id: string read FId write FId;
    property cOrgao: integer read FcOrgao write FcOrgao;
    property dhRegEvento: TDateTime read FdhRegEvento write FdhRegEvento;
    property tpEvento: string read FtpEvento write FtpEvento;
    property xEvento: string read FxEvento write FxEvento;
    property nSeqEvento: Integer read FnSeqEvento write FnSeqEvento;
    property CNPJDest: string read FCNPJDest write FCNPJDest;
    property emailDest: string read FemailDest write FemailDest;
    property XML: string read FXML write FXML;
  end;

  { TDistribuicaoDFeResposta }

  TDistribuicaoDFeResposta = class(TLibBPeResposta)
  private
    Farquivo: string;
    FdhResp: TDateTime;
    FindCont: string;
    FmaxNSU: string;
    FultNSU: string;
  public
    constructor Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;

  published
    property dhResp: TDateTime read FdhResp write FdhResp;
    property ultNSU: string read FultNSU write FultNSU;
    property maxNSU: string read FmaxNSU write FmaxNSU;
    property arquivo: string read Farquivo write Farquivo;
    property indCont: string read FindCont write FindCont;
  end;

  { TDistribuicaoDFeItemResposta }

  TDistribuicaoDFeItemResposta = class(TLibBPeResposta)
  private
    Farquivo: String;
    FCNPJ: string;
    FCNPJDest: string;
    FcOrgao: integer;
    FcOrgaoAutor: integer;
    FcSitBPe: String;
    FBPeChvBPe: String;
    FBPeDhemi: TDateTime;
    FBPeDhRebcto: TDateTime;
    FBPeModal: string;
    FBPeNProt: string;
    FdescEvento: string;
    FdhEmi: TDateTime;
    FdhEvento: TDateTime;
    FdhRegEvento: TDateTime;
    FdigVal: String;
    FemailDest: string;
    FEmiCNPJ: string;
    FEmiIE: string;
    FEmixNome: string;
    FId: string;
    FIE: String;
    FnProt: String;
    FnSeqEvento: Integer;
    FNSU: string;
    FchBPe: string;
    FCNPJCPF: string;
    Fschema: String;
    FtpEvento: string;
    FtpNF: String;
    FverEvento: string;
    FvNF: Currency;
    FxEvento: string;
    FxJust: string;
    FXML: string;
    FxNome: string;
  public
    constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo;
      const AFormato: TACBrLibCodificacao); reintroduce;

  published
    property NSU: string read FNSU write FNSU;
    property chBPe: string read FchBPe write FchBPe;
    property CNPJCPF: string read FCNPJCPF write FCNPJCPF;
    property xNome: string read FxNome write FxNome;
    property IE: String read FIE write FIE;
    property dhEmi: TDateTime read FdhEmi write FdhEmi;
    property tpNF: String read FtpNF write FtpNF;
    property vNF: Currency read FvNF write FvNF;
    property digVal: String read FdigVal write FdigVal;
    property dhRecbto: TDateTime read FdhRecbto write FdhRecbto;
    property cSitBPe: String read FcSitBPe write FcSitBPe;
    property nProt: String read FnProt write FnProt;
    property XML: string read FXML write FXML;
    property arquivo: String read Farquivo write Farquivo;
    property schema: String read Fschema write Fschema;
    property dhEvento: TDateTime read FdhEvento write FdhEvento;
    property tpEvento: string read FtpEvento write FtpEvento;
    property xEvento: string read FxEvento write FxEvento;
    property nSeqEvento: Integer read FnSeqEvento write FnSeqEvento;
    property cOrgao: integer read FcOrgao write FcOrgao;
    property CNPJ: string read FCNPJ write FCNPJ;
    property Id: string read FId write FId;
    property verEvento: string read FverEvento write FverEvento;
    property descEvento: string read FdescEvento write FdescEvento;
    property xJust: string read FxJust write FxJust;
    property xMotivo: string read FxMotivo write FxMotivo;
    property EmiCNPJ: string read FEmiCNPJ write FEmiCNPJ;
    property EmiIE: string read FEmiIE write FEmiIE;
    property EmixNome: string read FEmixNome write FEmixNome;
    property BPeNProt: string read FBPeNProt write FBPeNProt;
    property BPeChvBPe: String read FBPeChvBPe write FBPeChvBPe;
    property BPeDhemi: TDateTime read FBPeDhemi write FBPeDhemi;
    property BPeDhRebcto: TDateTime read FBPeDhRebcto write FBPeDhRebcto;
    property BPeModal: string read FBPeModal write FBPeModal;
    property CNPJDest: string read FCNPJDest write FCNPJDest;
    property cOrgaoAutor: integer read FcOrgaoAutor write FcOrgaoAutor;
    property dhRegEvento: TDateTime read FdhRegEvento write FdhRegEvento;
    property emailDest: string read FemailDest write FemailDest;
  end;

implementation

uses
  ACBrLibBPeConsts;

{ TDistribuicaoDFeItemResposta }

constructor TDistribuicaoDFeItemResposta.Create(const ASessao: String;
  const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
end;

{ TDistribuicaoDFeResposta }

constructor TDistribuicaoDFeResposta.Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespDistribuicaoDFe, ATipo, AFormato);
end;

{ TEventoItemResposta }

constructor TEventoItemResposta.Create(const ASessao: String;
  const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
end;

{ TEventoResposta }

constructor TEventoResposta.Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespEvento, ATipo, AFormato);
end;

{ TCancelamentoResposta }

constructor TCancelamentoResposta.Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespCancelamento, ATipo, AFormato);
end;

{ TRetornoItemResposta }

constructor TRetornoItemResposta.Create(const ASessao: String;
  const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
end;

{ TRetornoResposta }

constructor TRetornoResposta.Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespRetorno, ATipo, AFormato);
end;

{ TEnvioResposta }

constructor TEnvioResposta.Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespEnvio, ATipo, AFormato);
end;

constructor TStatusServicoResposta.Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespStatus, ATipo, AFormato);
end;

constructor TConsultaBPeResposta.Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespConsulta, ATipo, AFormato);
end;

{ TLibBPeResposta }

constructor TLibBPeResposta.Create(const ASessao: String;
  const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
end;

end.

