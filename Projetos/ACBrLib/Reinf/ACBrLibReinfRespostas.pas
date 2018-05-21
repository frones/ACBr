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

unit ACBrLibReinfRespostas;

interface

uses
  Classes, SysUtils, ACBrLibResposta, ACBrLibReinfConsts, pcnReinfRetConsulta;

type

  { TPadraoReinfResposta }

    TPadraoReinfResposta = class(TACBrLibResposta)
    private
      FCodigo: String;
      FMensagem: String;

    public
      constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo); reintroduce;

    published
      property Codigo: String read FCodigo write FCodigo;
      property Mensagem: String read FMensagem write FMensagem;

    end;

  { TEnvioResposta }

  TEnvioResposta = class(TPadraoReinfResposta)
  private
    FIdTransmissor: String;
//    FDhRecepcao: TDateTime;
//    FVersaoAplic: String;
//    FProtocolo: String;

  public
    constructor Create(const ATipo: TACBrLibRespostaTipo); reintroduce;

  published
    property IdTransmissor: String read FIdTransmissor write FIdTransmissor;
//    property DhRecepcao: TDateTime read FDhRecepcao write FDhRecepcao;
//    property VersaoAplic: String read FVersaoAplic write FVersaoAplic;
//    property Protocolo: String read FProtocolo write FProtocolo;

  end;

  { TOcorrenciaResposta }

  TOcorrenciaResposta = class(TPadraoReinfResposta)
  private
    FTipo: Integer;
    FLocalizacao: String;
    FCodigoOco: Integer;
    FDescricao: String;

  public
    constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo); reintroduce;

  published
    property Tipo: Integer read FTipo write FTipo;
    property Localizacao: String read FLocalizacao write FLocalizacao;
    property CodigoOco: Integer read FCodigoOco write FCodigoOco;
    property Descricao: String read FDescricao write FDescricao;

  end;

  { TConsultaResposta }

  TConsultaResposta = class(TPadraoReinfResposta)
  private
    FevtTotalContrib: TEvtTotalContrib;

    FTpInscContri: String;
    FNrInscContri: String;
    FcdRetorno: Integer;
    FdescRetorno: String;

  public
    constructor Create(const ATipo: TACBrLibRespostaTipo); reintroduce;

  published
    property evtTotalContrib: TEvtTotalContrib read FevtTotalContrib write FevtTotalContrib;

    property TpInscContri: String read FTpInscContri write FTpInscContri;
    property NrInscContri: String read FNrInscContri write FNrInscContri;
    property cdRetorno: Integer read FcdRetorno write FcdRetorno;
    property descRetorno: String read FdescRetorno write FdescRetorno;

  end;

implementation

{ TConsultaResposta }

constructor TConsultaResposta.Create(const ATipo: TACBrLibRespostaTipo);
begin
  inherited Create(CSessaoRespConsulta, ATipo);
  FevtTotalContrib := TEvtTotalContrib.Create;
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

{ TPadraoReinfResposta }

constructor TPadraoReinfResposta.Create(const ASessao: String;
  const ATipo: TACBrLibRespostaTipo);
begin
  inherited Create(ASessao, ATipo);
end;

end.

