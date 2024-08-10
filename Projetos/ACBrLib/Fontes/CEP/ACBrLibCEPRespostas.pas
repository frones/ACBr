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

unit ACBrLibCEPRespostas;

interface

uses
  SysUtils, Classes, contnrs,
  ACBrLibResposta, ACBrLibConfig,
  ACBrCEP;

type

  { TEnderecoResposta }
  TEnderecoResposta = class(TACBrLibRespostaBase)
  private
    FBairro: string;
    FCEP: string;
    FComplemento: string;
    FIBGE_Municipio: string;
    FIBGE_UF: string;
    FLogradouro: string;
    FMunicipio: string;
    FTipo_Logradouro: string;
    FUF: string;

  public
    constructor Create(const Id: Integer; const ATipo: TACBrLibRespostaTipo;
      const AFormato: TACBrLibCodificacao); reintroduce;

    procedure Processar(const AEndereco: TACBrCEPEndereco);

  published
    property CEP: string read FCEP write FCEP;
    property Tipo_Logradouro: string read FTipo_Logradouro write FTipo_Logradouro;
    property Logradouro: string read FLogradouro write FLogradouro;
    property Complemento: string read FComplemento write FComplemento;
    property Bairro: string read FBairro write FBairro;
    property Municipio: string read FMunicipio write FMunicipio;
    property UF: string read FUF write FUF;
    property IBGE_Municipio: string read FIBGE_Municipio write FIBGE_Municipio;
    property IBGE_UF: string read FIBGE_UF write FIBGE_UF;

  end;

  { TCepResposta }
  TCepResposta = class(TACBrLibRespostaBase)
  private
    FQtd: Integer;
    FItems: TObjectList;

  public
    constructor Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;
    destructor Destroy; override;

    procedure Processar(const ACBrCEP: TACBrCEP);

  published
    property Quantidade: Integer read FQtd write FQtd;
    property Items: TObjectList read FItems;

  end;

implementation

Uses
  ACBrLibCEPConsts;

{ TEnderecoResposta }
constructor TEnderecoResposta.Create(const Id: Integer; const ATipo: TACBrLibRespostaTipo;
  const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespConsulta + IntToStr(Id), ATipo, AFormato);
end;

procedure TEnderecoResposta.Processar(const AEndereco: TACBrCEPEndereco);
begin
  with AEndereco do
  begin
    Self.CEP := CEP;
    Self.Tipo_Logradouro := Tipo_Logradouro;
    Self.Logradouro := Logradouro;
    Self.Logradouro := Logradouro;
    Self.Complemento := Complemento;
    Self.Bairro := Bairro;
    Self.Municipio := Municipio;
    Self.UF := UF;
    Self.IBGE_Municipio := IBGE_Municipio;
    Self.IBGE_UF := IBGE_UF;
  end;
end;

{ TCepResposta }
constructor TCepResposta.Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoCEP, ATipo, AFormato);

  FItems := TObjectList.Create(True);
end;

destructor TCepResposta.Destroy;
begin
  FItems.Free;

  inherited Destroy;
end;

procedure TCepResposta.Processar(const ACBrCEP: TACBrCEP);
Var
  I: Integer;
  Item: TEnderecoResposta;
begin
  FQtd := ACBrCEP.Enderecos.Count;
  for I := 0 to ACBrCEP.Enderecos.Count - 1 do
  begin
    Item := TEnderecoResposta.Create(I + 1, Tipo, Codificacao);
    Item.Processar(ACBrCEP.Enderecos[I]);
    FItems.Add(Item);
  end;
end;

end.
