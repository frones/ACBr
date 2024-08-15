{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
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

unit ACBrLibGNReRespostas;

interface

uses
  SysUtils, Classes, contnrs, ACBrLibResposta, ACBrLibConfig, ACBrGNRE2, pgnreRetConsResLoteGNRE;

type

   { TGNReRetorno }
  TGNReRetorno = class(TACBrLibRespostaBase)
  private
    FXML: string;
    FTXT: String;
    FArquivo: string;
  public
    constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo;
      const AFormato: TACBrLibCodificacao); reintroduce;

    procedure Processar(const ARetRecepcao: TGuiaCollectionItem);

  published
    property XML: string read FXML write FXML;
    property TXT: string read FTXT write FTXT;
    property Arquivo: string read FArquivo write FArquivo;
  end;

  { TLibGNReEnvio }
  TLibGNReEnvio = class(TACBrLibResposta<TACBrGNRE>)
  private
    FAmbiente: string;
    FCodigo: string;
    FDescricao: string;
    FProtocolo: string;
    FRecibo: string;
    FItems: TObjectList;
  public
    constructor Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;
    destructor Destroy; override;

    procedure Processar(const Control: TACBrGNRE); override;

  published
    property Ambiente: string read FAmbiente write FAmbiente;
    property Codigo: string read FCodigo write FCodigo;
    property Descricao: string read FDescricao write FDescricao;
    property Recibo: string read FRecibo write FRecibo;
    property Protocolo: string read FProtocolo write FProtocolo;
    property Items: TObjectList read FItems;
  end;

  { TLibGNReConsulta }

  TLibGNReConsulta = class(TACBrLibResposta<TACBrGNRE>)
  private
    FAmbiente: string;
    FCodigo: string;
    FDescricao: string;
    FExigeReceita: string;
    FExigeUfFavorecida: string;
    FUF: string;
  public
    constructor Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;

    procedure Processar(const Control: TACBrGNRE); override;

  published
    property Ambiente: string read FAmbiente write FAmbiente;
    property Codigo: string read FCodigo write FCodigo;
    property Descricao: string read FDescricao write FDescricao;
    property UF: string read FUF write FUF;
    property ExigeUfFavorecida: string read FExigeUfFavorecida write FExigeUfFavorecida;
    property ExigeReceita: string read FExigeReceita write FExigeReceita;
  end;

implementation

uses
  strutils,
  ACBrLibGNReConsts, pcnConversao, pcnAuxiliar;

{ TGNReRetorno }

constructor TGNReRetorno.Create(const ASessao: String;
  const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
end;

procedure TGNReRetorno.Processar(const ARetRecepcao: TGuiaCollectionItem);
begin
  XML := ARetRecepcao.XML;
  TXT := ARetRecepcao.TXT;
  Arquivo := ARetRecepcao.NomeArq;
end;

{ TLibGNReConsulta }

constructor TLibGNReConsulta.Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespConsulta, ATipo, AFormato);
end;

procedure TLibGNReConsulta.Processar(const Control: TACBrGNRE);
begin
  with Control.WebServices.ConsultaUF do
  begin
    Self.Ambiente := TpAmbToStr(ambiente);
    Self.Codigo := IntToStr(codigo);
    Self.Descricao := Descricao;
    Self.UF := Uf;
    Self.ExigeUfFavorecida := IfThen(exigeUfFavorecida = 'S', 'SIM', 'NÃO');
    Self.ExigeReceita := IfThen(exigeReceita = 'S', 'SIM', 'NÃO');
  end;
end;

{ TLibGNReEnvio }

constructor TLibGNReEnvio.Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespEnvio, ATipo, AFormato);
  FItems := TObjectList.Create;
end;

destructor TLibGNReEnvio.Destroy;
begin
  FItems.Clear;
  FItems.Free;

  inherited Destroy;
end;

procedure TLibGNReEnvio.Processar(const Control: TACBrGNRE);
Var
  I: Integer;
  Item: TGNReRetorno;
begin
  with Control.WebServices.Retorno do
  begin
    Self.Ambiente := TpAmbToStr(ambiente);
    Self.Codigo := IntToStr(codigo);
    Self.Descricao := Descricao;
    Self.Recibo := numeroRecibo;
    Self.Protocolo := Protocolo;

    for I := 0 to Control.WebServices.Retorno.GNRERetorno.resGuia.Count - 1 do
    begin
      Item := TGNReRetorno.Create('Retorno' + Trim(IntToStrZero(I +1, 3)), Tipo, Codificacao);
      Item.Processar(Control.WebServices.Retorno.GNRERetorno.resGuia[I]);
      FItems.Add(Item);
    end;


  end;
end;

end.

