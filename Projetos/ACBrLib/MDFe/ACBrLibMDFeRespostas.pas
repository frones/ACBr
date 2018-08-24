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

unit ACBrLibMDFeRespostas;

interface

uses
  SysUtils, Classes, ACBrLibResposta;

type

  { TPadraoMDFeResposta }

  TPadraoMDFeResposta = class(TACBrLibResposta)
  private
    Fversao: string;
    FtpAmb: string;
    FverAplic: string;
    FcStat: integer;
    FxMotivo: string;
    FcUF: integer;
  public
    constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo); reintroduce;

  published
    property Versao: string read Fversao write Fversao;
    property TpAmb: string read FtpAmb write FtpAmb;
    property VerAplic: string read FverAplic write FverAplic;
    property CStat: integer read FcStat write FcStat;
    property XMotivo: string read FxMotivo write FxMotivo;
    property CUF: integer read FcUF write FcUF;

  end;

  { TStatusServicoResposta }

  TStatusServicoResposta = class(TPadraoMDFeResposta)
  private
    FMsg: string;
    FdhRecbto: TDateTime;
    FTMed: integer;
    FdhRetorno: TDateTime;
    FxObs: string;
  public
    constructor Create(const ATipo: TACBrLibRespostaTipo); reintroduce;

  published
    property Msg: string read FMsg write FMsg;
    property DhRecbto: TDateTime read FdhRecbto write FdhRecbto;
    property TMed: integer read FTMed write FTMed;
    property DhRetorno: TDateTime read FdhRetorno write FdhRetorno;
    property XObs: string read FxObs write FxObs;

  end;

  { TConsultaResposta }

  TConsultaResposta = class(TPadraoMDFeResposta)
  private
    FMsg: string;
    FchMDFe: string;
    FdhRecbto: TDateTime;
    FnProt: string;
    FdigVal: string;

  public
    constructor Create(const ATipo: TACBrLibRespostaTipo); reintroduce;

  published
    property Msg: string read FMsg write FMsg;
    property ChMDFe: string read FchMDFe write FchMDFe;
    property DhRecbto: TDateTime read FdhRecbto write FdhRecbto;
    property NProt: string read FnProt write FnProt;
    property DigVal: string read FdigVal write FdigVal;

  end;

  { TCancelamentoResposta }

  TCancelamentoResposta = class(TPadraoMDFeResposta)
  private
    FArquivo: string;
    FchMDFe: string;
    FdhRecbto: TDateTime;
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
    property ChMDFe: string read FchMDFe write FchMDFe;
    property DhRecbto: TDateTime read FdhRecbto write FdhRecbto;
    property NProt: string read FnProt write FnProt;
    property TpEvento: string read FtpEvento write FtpEvento;
    property XEvento: string read FxEvento write FxEvento;
    property NSeqEvento: integer read FnSeqEvento write FnSeqEvento;
    property CNPJDest: string read FCNPJDest write FCNPJDest;
    property EmailDest: string read FemailDest write FemailDest;
    property XML: string read Fxml write Fxml;
    property Arquivo: string read FArquivo write FArquivo;

  end;

  { TEncerramentoResposta }

  TEncerramentoResposta = class(TPadraoMDFeResposta)
  private
    FArquivo: string;
    FchMDFe: string;
    FdhRecbto: TDateTime;
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
    property ChMDFe: string read FchMDFe write FchMDFe;
    property DhRecbto: TDateTime read FdhRecbto write FdhRecbto;
    property NProt: string read FnProt write FnProt;
    property TpEvento: string read FtpEvento write FtpEvento;
    property XEvento: string read FxEvento write FxEvento;
    property NSeqEvento: integer read FnSeqEvento write FnSeqEvento;
    property CNPJDest: string read FCNPJDest write FCNPJDest;
    property EmailDest: string read FemailDest write FemailDest;
    property XML: string read Fxml write Fxml;
    property Arquivo: string read FArquivo write FArquivo;

  end;

  { TNaoEncerradosResposta }

  TNaoEncerradosResposta = class(TPadraoMDFeResposta)
  private
    Fmsg: string;
    FCNPJ: string;
    FchMDFe: string;
    FnProt: string;

  public
    constructor Create(const ATipo: TACBrLibRespostaTipo); reintroduce;

  published
    property Msg: string read Fmsg write Fmsg;
    property CNPJ: string read FCNPJ write FCNPJ;
    property ChMDFe: string read FchMDFe write FchMDFe;
    property NProt: string read FnProt write FnProt;

  end;

  { TEnvioResposta }

  TEnvioResposta = class(TPadraoMDFeResposta)
  private
    Fmsg: string;
    FdhRecbto: TDateTime;
    FtMed: integer;
    FnRec: string;

  public
    constructor Create(const ATipo: TACBrLibRespostaTipo); reintroduce;

  published
    property Msg: string read Fmsg write Fmsg;
    property DhRecbto: TDateTime read FdhRecbto write FdhRecbto;
    property TMed: integer read FtMed write FtMed;
    property NRec: string read FnRec write FnRec;

  end;

  { TRetornoResposta }

  TRetornoResposta = class(TPadraoMDFeResposta)
  private
    Fmsg: string;
    FnRec: string;
    FchMDFe: string;
    FnProt: string;
    FmotivoMDFe: string;

  public
    constructor Create(const ATipo: TACBrLibRespostaTipo); reintroduce;

  published
    property Msg: string read Fmsg write Fmsg;
    property NRec: string read FnRec write FnRec;
    property ChMDFe: string read FchMDFe write FchMDFe;
    property NProt: string read FnProt write FnProt;
    property MotivoMDFe: string read FmotivoMDFe write FmotivoMDFe;

  end;

  { TRetornoItemResposta }

  TRetornoItemResposta = class(TPadraoMDFeResposta)
  private
    FchMDFe: string;
    FdhRecbto: TDateTime;
    FnProt: string;
    FdigVal: string;
    Farquivo: string;

  public
    constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo); reintroduce;

  published
    property ChMDFe: string read FchMDFe write FchMDFe;
    property DhRecbto: TDateTime read FdhRecbto write FdhRecbto;
    property NProt: String read FnProt write FnProt;
    property DigVal: String read FdigVal write FdigVal;
    property Arquivo: String read Farquivo write Farquivo;

  end;

implementation

uses
  ACBrLibMDFeConsts;

{ TPadraoResposta }

constructor TPadraoMDFeResposta.Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo);
begin
  inherited Create(ASessao, ATipo);
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

{ TNaoEncerradosResposta }

constructor TNaoEncerradosResposta.Create(const ATipo: TACBrLibRespostaTipo);
begin
  inherited Create(CSessaoRespNaoEncerrados, ATipo);
end;

{ TEncerramentoResposta }

constructor TEncerramentoResposta.Create(const ATipo: TACBrLibRespostaTipo);
begin
  inherited Create(CSessaoRespEncerramento, ATipo);
end;

{ TCancelamentoResposta }

constructor TCancelamentoResposta.Create(const ATipo: TACBrLibRespostaTipo);
begin
  inherited Create(CSessaoRespCancelamento, ATipo);
end;

{ TConsultaResposta }

constructor TConsultaResposta.Create(const ATipo: TACBrLibRespostaTipo);
begin
  inherited Create(CSessaoRespConsulta, ATipo);
end;

 { TStatusServicoResposta }
constructor TStatusServicoResposta.Create(const ATipo: TACBrLibRespostaTipo);
begin
  inherited Create(CSessaoRespStatus, ATipo);
end;

end.

