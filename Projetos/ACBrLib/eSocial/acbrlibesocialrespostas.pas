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
unit ACBrLibeSocialRespostas;

interface

uses
  Classes, SysUtils, ACBrLibResposta, ACBrLibeSocialConsts;

type

  { TPadraoeSocialResposta }

    TPadraoeSocialResposta = class(TACBrLibResposta)
    private
      FCodigo: Integer;
      FMensagem: String;

    public
      constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo); reintroduce;

    published
      property Codigo: Integer read FCodigo write FCodigo;
      property Mensagem: String read FMensagem write FMensagem;

    end;

  { TEnvioResposta }

  TEnvioResposta = class(TPadraoeSocialResposta)
  private
    FTpInscEmpreg: String;
    FNrInscEmpreg: String;
    FTpInscTransm: String;
    FNrInscTransm: String;
    FDhRecepcao: TDateTime;
    FVersaoAplic: String;
    FProtocolo: String;

  public
    constructor Create(const ATipo: TACBrLibRespostaTipo); reintroduce;

  published
    property TpInscEmpreg: String read FTpInscEmpreg write FTpInscEmpreg;
    property NrInscEmpreg: String read FNrInscEmpreg write FNrInscEmpreg;
    property TpInscTransm: String read FTpInscTransm write FTpInscTransm;
    property NrInscTransm: String read FNrInscTransm write FNrInscTransm;
    property DhRecepcao: TDateTime read FDhRecepcao write FDhRecepcao;
    property VersaoAplic: String read FVersaoAplic write FVersaoAplic;
    property Protocolo: String read FProtocolo write FProtocolo;

  end;

  { TOcorrenciaResposta }

  TOcorrenciaResposta = class(TPadraoeSocialResposta)
  private
    FCodigoOco: Integer;
    FDescricao: String;
    FTipo: Integer;
    FLocalizacao: String;

  public
    constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo); reintroduce;

  published
    property CodigoOco: Integer read FCodigoOco write FCodigoOco;
    property Descricao: String read FDescricao write FDescricao;
    property Tipo: Integer read FTipo write FTipo;
    property Localizacao: String read FLocalizacao write FLocalizacao;

  end;

  { TConsultaResposta }

  TConsultaResposta = class(TPadraoeSocialResposta)
  private
    FcdResposta: Integer;
    FdescResposta: String;
    FversaoAplicProcLote: String;
    FdhProcessamento: TDateTime;
    FnrRecibo: String;
    Fhash: String;

  public
    constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo); reintroduce;

  published
    property cdResposta: Integer read FcdResposta write FcdResposta;
    property descResposta: String read FdescResposta write FdescResposta;
    property versaoAplicProcLote: String read FversaoAplicProcLote write FversaoAplicProcLote;
    property dhProcessamento: TDateTime read FdhProcessamento write FdhProcessamento;
    property nrRecibo: String read FnrRecibo write FnrRecibo;
    property hash: String read Fhash write Fhash;

  end;

  { TConsultaTotResposta }

  TConsultaTotResposta = class(TPadraoeSocialResposta)
  private
    FTipo : String;
    FID : String;
    FNrRecArqBase : String;

  public
    constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo); reintroduce;

  published
    property Tipo: String read FTipo write FTipo;
    property ID: String read FID write FID;
    property NrRecArqBase: String read FNrRecArqBase write FNrRecArqBase;

  end;

implementation

{ TConsultaTotResposta }

constructor TConsultaTotResposta.Create(const ASessao: String;
  const ATipo: TACBrLibRespostaTipo);
begin
  inherited Create(ASessao, ATipo);
end;

{ TConsultaResposta }

constructor TConsultaResposta.Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo);
begin
  inherited Create(ASessao, ATipo);
end;

{ TEnvioResposta }

constructor TEnvioResposta.Create(const ATipo: TACBrLibRespostaTipo);
begin
  inherited Create(CSessaoRespEnvio, ATipo);
end;

{ TOcorrenciaResposta }

constructor TOcorrenciaResposta.Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo);
begin
  inherited Create(ASessao, ATipo);
end;

{ TPadraoeSocialResposta }

constructor TPadraoeSocialResposta.Create(const ASessao: String;
  const ATipo: TACBrLibRespostaTipo);
begin
  inherited Create(ASessao, ATipo);
end;

end.

