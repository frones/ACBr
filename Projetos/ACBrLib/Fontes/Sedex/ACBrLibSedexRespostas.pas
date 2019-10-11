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

unit ACBrLibSedexRespostas;

interface

uses
  SysUtils, Classes, ACBrLibResposta;

type

  { TLibSedexRastreio }
  TLibSedexRastreio = class(TACBrLibResposta)
  private
    FDataHora: TDateTime;
    FLocal: string;
    FObservacao: string;
    FSituacao: string;
  public
    constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo;
      const AFormato: TACBrLibCodResposta); reintroduce;

  published
    property DataHora: TDateTime read FDataHora write FDataHora;
    property Local: string read FLocal write FLocal;
    property Situacao: string read FSituacao write FSituacao;
    property Observacao: string read FObservacao write FObservacao;
  end;

  { TLibSedexConsulta }

  TLibSedexConsulta = class(TACBrLibResposta)
  private
    FCodigoServico: string;
    FEntregaDomiciliar: string;
    FEntregaSabado: string;
    FErro: Integer;
    FMsgErro: string;
    FPrazoEntrega: Integer;
    FValor: Currency;
    FValorAvisoRecebimento: Currency;
    FValorMaoPropria: Currency;
    FValorSemAdicionais: Currency;
    FValorValorDeclarado: Currency;
  public
    constructor Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodResposta); reintroduce;

  published
    property CodigoServico: string read FCodigoServico write FCodigoServico;
    property Valor: Currency read FValor write FValor;
    property PrazoEntrega: Integer read FPrazoEntrega write FPrazoEntrega;
    property ValorSemAdicionais: Currency read FValorSemAdicionais write FValorSemAdicionais;
    property ValorMaoPropria: Currency read FValorMaoPropria write FValorMaoPropria;
    property ValorAvisoRecebimento: Currency read FValorAvisoRecebimento write FValorAvisoRecebimento;
    property ValorValorDeclarado: Currency read FValorValorDeclarado write FValorValorDeclarado;
    property EntregaDomiciliar: string read FEntregaDomiciliar write FEntregaDomiciliar;
    property EntregaSabado: string read FEntregaSabado write FEntregaSabado;
    property Erro: Integer read FErro write FErro;
    property MsgErro: string read FMsgErro write FMsgErro;
  end;

implementation

uses
  ACBrLibSedexConsts;

{ TLibSedexConsulta }

constructor TLibSedexConsulta.Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodResposta);
begin
  inherited Create(CSessaoRespConsulta, ATipo, AFormato);
end;

{ TLibSedexRastreio }

constructor TLibSedexRastreio.Create(const ASessao: String;
  const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodResposta);
begin
  inherited Create(ASessao, ATipo, AFormato);
end;

end.

