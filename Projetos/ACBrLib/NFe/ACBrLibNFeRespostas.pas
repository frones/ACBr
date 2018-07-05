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

unit ACBrLibNFeRespostas;

interface

uses
  SysUtils, Classes, ACBrLibResposta;

type

  { TLibNFeResposta }
  TLibNFeResposta = class(TACBrLibResposta)
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
    constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo); reintroduce;

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
  TStatusServicoResposta = class(TLibNFeResposta)
  private
    FTMed: integer;
    FdhRetorno: TDateTime;
    FxObs: string;
  public
    constructor Create(const ATipo: TACBrLibRespostaTipo); reintroduce;

  published
    property TMed: integer read FTMed write FTMed;
    property DhRetorno: TDateTime read FdhRetorno write FdhRetorno;
    property XObs: string read FxObs write FxObs;
  end;

  { TInutilizarNFeResposta }
  TInutilizarNFeResposta = class(TLibNFeResposta)
  private
    FNomeArquivo: String;
    FXml: String;
  public
    constructor Create(const ATipo: TACBrLibRespostaTipo); reintroduce;

  published
    property NomeArquivo: String read FNomeArquivo write FNomeArquivo;
    property Xml: String read FXml write FXml;
  end;

  { TConsultaNFeResposta }
  TConsultaNFeResposta = class(TLibNFeResposta)
  private
    FChNFe: String;
    FNProt: String;
    FDigVal: String;
  public
    constructor Create(const ATipo: TACBrLibRespostaTipo); reintroduce;

  published
    property ChNFe: String read FChNFe write FChNFe;
    property NProt: String read FNProt write FNProt;
    property DigVal: String read FDigVal write FDigVal;
  end;

  { TEnvioResposta }

  TEnvioResposta = class(TLibNFeResposta)
  private
    FtMed: integer;
    FnRec: string;
  public
    constructor Create(const ATipo: TACBrLibRespostaTipo); reintroduce;

  published
   property TMed: integer read FtMed write FtMed;
   property NRec: string read FnRec write FnRec;
  end;

  { TRetornoResposta }

  TRetornoResposta = class(TLibNFeResposta)
  private
    FnRec: string;
  public
    constructor Create(const ATipo: TACBrLibRespostaTipo); reintroduce;

  published
    property nRec: string read FnRec write FnRec;
  end;

  { TRetornoItemResposta }

  TRetornoItemResposta = class(TLibNFeResposta)
  private
    FchNFe: string;
    FnProt: string;
    FdigVal: string;
    Farquivo: string;
  public
    constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo); reintroduce;

  published
    property chNFe: string read FchNFe write FchNFe;
    property nProt: String read FnProt write FnProt;
    property digVal: String read FdigVal write FdigVal;
    property arquivo: String read Farquivo write Farquivo;
  end;

  { TCancelamentoResposta }

  TCancelamentoResposta = class(TLibNFeResposta)
  private
    FchNFe: string;
    FnProt: string;
    FtpEvento: string;
    FxEvento: string;
    FnSeqEvento: integer;
    FCNPJDest: string;
    FemailDest: string;
    Fxml: string;
  public
    constructor Create(const ATipo: TACBrLibRespostaTipo); reintroduce;

  published
    property chNFe: string read FchNFe write FchNFe;
    property nProt: string read FnProt write FnProt;
    property tpEvento: string read FtpEvento write FtpEvento;
    property xEvento: string read FxEvento write FxEvento;
    property nSeqEvento: integer read FnSeqEvento write FnSeqEvento;
    property CNPJDest: string read FCNPJDest write FCNPJDest;
    property emailDest: string read FemailDest write FemailDest;
    property XML: string read Fxml write Fxml;
  end;

  { TConsultaCadastroResposta }

  TConsultaCadastroResposta = class(TLibNFeResposta)
  private
    FIE: string;
    FCNPJ: string;
    FCPF: string;
    FUF: string;
    FdhCons: TDateTime;
  public
    constructor Create(const ATipo: TACBrLibRespostaTipo); reintroduce;

  published
    property IE: string read FIE write FIE;
    property CNPJ: string read FCNPJ write FCNPJ;
    property CPF: string read FCPF write FCPF;
    property UF: string read FUF write FUF;
    property dhCons: TDateTime read FdhCons write FdhCons;
  end;

  { TConsultaCadastroItemResposta }

  TConsultaCadastroItemResposta = class(TLibNFeResposta)
  private
    Farquivo: string;
    FCEP: Integer;
    FcMun: Integer;
    FCNAE: Integer;
    FcSit: Integer;
    FdBaixa: TDateTime;
    FdIniAtiv: TDateTime;
    FdUltSit: TDateTime;
    FIE: string;
    FCNPJ: string;
    FCPF: string;
    FIEAtual: String;
    FIEUnica: String;
    Fnro: String;
    FUF: String;
    FxBairro: String;
    FxCpl: String;
    FxFant: String;
    FxLgr: String;
    FxMun: String;
    FxNome: String;
    FxRegApur: String;
  public
    constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo); reintroduce;

  published
    property arquivo: String read Farquivo write Farquivo;
    property IE: string read FIE write FIE;
    property CNPJ: String read FCNPJ write FCNPJ;
    property CPF: String read FCPF write FCPF;
    property UF: String read FUF write FUF;
    property cSit: Integer read FcSit write FcSit;
    property xNome: String read FxNome write FxNome;
    property xFant: String read FxFant write FxFant;
    property xRegApur: String read FxRegApur write FxRegApur;
    property CNAE: Integer read FCNAE write FCNAE;
    property dIniAtiv: TDateTime read FdIniAtiv write FdIniAtiv;
    property dUltSit: TDateTime read FdUltSit write FdUltSit;
    property dBaixa: TDateTime read FdBaixa write FdBaixa;
    property IEUnica: String read FIEUnica write FIEUnica;
    property IEAtual: String read FIEAtual write FIEAtual;
    property xLgr: String read FxLgr write FxLgr;
    property nro: String read Fnro write Fnro;
    property xCpl: String read FxCpl write FxCpl;
    property xBairro: String read FxBairro write FxBairro;
    property cMun: Integer read FcMun write FcMun;
    property xMun: String read FxMun write FxMun;
    property CEP: Integer read FCEP write FCEP;
  end;

  { TEventoResposta }

  TEventoResposta = class(TLibNFeResposta)
  private
    FidLote: Integer;
    FcOrgao: Integer;
  public
    constructor Create(const ATipo: TACBrLibRespostaTipo); reintroduce;

  published
    property idLote: Integer read FidLote write FidLote;
    property cOrgao: Integer read FcOrgao write FcOrgao;
  end;

  { TEventoItemResposta }

  TEventoItemResposta = class(TLibNFeResposta)
  private
    Farquivo: String;
    FchNFe: string;
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
    constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo); reintroduce;

  published
    property chNFe: string read FchNFe write FchNFe;
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

  TDistribuicaoDFeResposta = class(TLibNFeResposta)
  private
    Farquivo: string;
    FdhResp: TDateTime;
    FindCont: string;
    FmaxNSU: string;
    FultNSU: string;
  public
    constructor Create(const ATipo: TACBrLibRespostaTipo); reintroduce;

  published
    property dhResp: TDateTime read FdhResp write FdhResp;
    property ultNSU: string read FultNSU write FultNSU;
    property maxNSU: string read FmaxNSU write FmaxNSU;
    property arquivo: string read Farquivo write Farquivo;
    property indCont: string read FindCont write FindCont;
  end;

  { TDistribuicaoDFeItemResposta }

  TDistribuicaoDFeItemResposta = class(TLibNFeResposta)
  private
    Farquivo: String;
    FCNPJ: string;
    FCNPJDest: string;
    FcOrgao: integer;
    FcOrgaoAutor: integer;
    FcSitNFe: String;
    FcteChvCte: String;
    FcteDhemi: TDateTime;
    FcteDhRebcto: TDateTime;
    FcteModal: string;
    FcteNProt: string;
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
    FchNFe: string;
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
    constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo); reintroduce;

  published
    property NSU: string read FNSU write FNSU;
    property chNFe: string read FchNFe write FchNFe;
    property CNPJCPF: string read FCNPJCPF write FCNPJCPF;
    property xNome: string read FxNome write FxNome;
    property IE: String read FIE write FIE;
    property dhEmi: TDateTime read FdhEmi write FdhEmi;
    property tpNF: String read FtpNF write FtpNF;
    property vNF: Currency read FvNF write FvNF;
    property digVal: String read FdigVal write FdigVal;
    property dhRecbto: TDateTime read FdhRecbto write FdhRecbto;
    property cSitNFe: String read FcSitNFe write FcSitNFe;
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
    property cteNProt: string read FcteNProt write FcteNProt;
    property cteChvCte: String read FcteChvCte write FcteChvCte;
    property cteDhemi: TDateTime read FcteDhemi write FcteDhemi;
    property cteDhRebcto: TDateTime read FcteDhRebcto write FcteDhRebcto;
    property cteModal: string read FcteModal write FcteModal;
    property CNPJDest: string read FCNPJDest write FCNPJDest;
    property cOrgaoAutor: integer read FcOrgaoAutor write FcOrgaoAutor;
    property dhRegEvento: TDateTime read FdhRegEvento write FdhRegEvento;
    property emailDest: string read FemailDest write FemailDest;
  end;

implementation

uses
  ACBrLibNFeConsts;

{ TLibNFeResposta }

constructor TLibNFeResposta.Create(const ASessao: String;
  const ATipo: TACBrLibRespostaTipo);
begin
  inherited Create(ASessao, ATipo);
end;

{ TDistribuicaoDFeItemResposta }

constructor TDistribuicaoDFeItemResposta.Create(const ASessao: String;
  const ATipo: TACBrLibRespostaTipo);
begin
  inherited Create(ASessao, ATipo);
end;

{ TDistribuicaoDFeResposta }

constructor TDistribuicaoDFeResposta.Create(const ATipo: TACBrLibRespostaTipo);
begin
  inherited Create(CSessaoRespDistribuicaoDFe, ATipo);
end;

{ TEventoItemResposta }

constructor TEventoItemResposta.Create(const ASessao: String;
  const ATipo: TACBrLibRespostaTipo);
begin
  inherited Create(ASessao, ATipo);
end;

{ TEventoResposta }

constructor TEventoResposta.Create(const ATipo: TACBrLibRespostaTipo);
begin
  inherited Create(CSessaoRespEvento, ATipo);
end;

{ TConsultaCadastroItemResposta }

constructor TConsultaCadastroItemResposta.Create(const ASessao: String;
  const ATipo: TACBrLibRespostaTipo);
begin
  inherited Create(ASessao, ATipo);
end;

{ TConsultaCadastroResposta }

constructor TConsultaCadastroResposta.Create(const ATipo: TACBrLibRespostaTipo);
begin
  inherited Create(CSessaoRespConsultaCadastro, ATipo);
end;

{ TCancelamentoResposta }

constructor TCancelamentoResposta.Create(const ATipo: TACBrLibRespostaTipo);
begin
  inherited Create(CSessaoRespCancelamento, ATipo);
end;

{ TRetornoItemResposta }

constructor TRetornoItemResposta.Create(const ASessao: String;
  const ATipo: TACBrLibRespostaTipo);
begin
  inherited Create(ASessao, ATipo);
end;

{ TRetornoResposta }

constructor TRetornoResposta.Create(const ATipo: TACBrLibRespostaTipo);
begin
  inherited Create(CSessaoRespRetorno, ATipo);
end;

{ TEnvioResposta }

constructor TEnvioResposta.Create(const ATipo: TACBrLibRespostaTipo);
begin
  inherited Create(CSessaoRespEnvio, ATipo);
end;

constructor TStatusServicoResposta.Create(const ATipo: TACBrLibRespostaTipo);
begin
  inherited Create(CSessaoRespStatus, ATipo);
end;

constructor TInutilizarNFeResposta.Create(const ATipo: TACBrLibRespostaTipo);
begin
  inherited Create(CSessaoRespInutilizacao, ATipo);
end;

constructor TConsultaNFeResposta.Create(const ATipo: TACBrLibRespostaTipo);
begin
  inherited Create(CSessaoRespConsulta, ATipo);
end;

end.

