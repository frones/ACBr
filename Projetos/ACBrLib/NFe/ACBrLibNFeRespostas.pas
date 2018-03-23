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

    property ChNFe: String read FChNFe write FChNFe;
    property NProt: String read FNProt write FNProt;
    property DigVal: String read FDigVal write FDigVal;
  end;

implementation

uses
  ACBrLibNFeConsts;

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

