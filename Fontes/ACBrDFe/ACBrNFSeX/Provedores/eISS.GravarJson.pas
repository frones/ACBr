{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2023 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Giurizzato Junior                         }
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

unit eISS.GravarJson;

interface

uses
  SysUtils, Classes, Variants, StrUtils,
  ACBrXmlBase, ACBrXmlDocument, ACBrJSON,
  ACBrNFSeXParametros, ACBrNFSeXGravarXml,
  ACBrNFSeXConversao, ACBrNFSeXConsts;

type
  { TNFSeW_eISS }

  TNFSeW_eISS = class(TNFSeWClass)
  protected
    procedure Configuracao; override;

    function GerarDadosNota: String;

    function GerarTomador: TACBrJSONObject;
    function GerarTomadorDocumento: TACBrJSONObject;
    function GerarTomadorEndereco: TACBrJSONObject;
    function GerarItens: TACBrJSONArray;
  public
    function GerarXml: Boolean; override;
  end;

implementation

uses
  DateUtils,
  ACBrNFSeX,
  ACBrUtil.Base, ACBrUtil.Strings,
  ACBrConsts;

//==============================================================================
// Essa unit tem por finalidade exclusiva gerar o Json do RPS do provedor:
//     eISS
//==============================================================================

{ TNFSeW_eISS }

procedure TNFSeW_eISS.Configuracao;
begin
  inherited Configuracao;

end;

function TNFSeW_eISS.GerarXml: Boolean;
begin
  Configuracao;

  Opcoes.QuebraLinha := FpAOwner.ConfigGeral.QuebradeLinha;

  ListaDeAlertas.Clear;

  FDocument.Clear();

  FConteudoTxt.Clear;

  {$IFDEF FPC}
  FConteudoTxt.LineBreak := CRLF;
  {$ELSE}
    {$IFDEF DELPHI2006_UP}
    FConteudoTxt.LineBreak := CRLF;
    {$ENDIF}
  {$ENDIF}

  FConteudoTxt.Text := GerarDadosNota;
  Result := True;
end;

function TNFSeW_eISS.GerarDadosNota: String;
var
  AJSon: TACBrJSONObject;
  ForaMunicipio: Boolean;
  Vencimento: string;
begin
  Vencimento := FormatDateTime('dd/MM/yyyy', NFSe.DataEmissao);

  ForaMunicipio := NFSe.Prestador.Endereco.CodigoMunicipio <>
                                          NFSe.Tomador.Endereco.CodigoMunicipio;

  AJSon := TACBrJsonObject.Create;
  try
    AJSon
      .AddPairJSONObject('NotaFiscal', EmptyStr)
      .AsJSONObject['NotaFiscal']
        .AddPair('MesPrestacao', MonthOf(NFSe.DataEmissao))
        .AddPair('AnoPrestacao', YearOf(NFSe.DataEmissao))
        .AddPair('CodigoServico', NFSe.Servico.ItemListaServico)
        .AddPair('CodigoIbgeMunicipioPrestacao', StrToIntDef(NFSe.Prestador.Endereco.CodigoMunicipio, 0))
        .AddPair('Vencimento', Vencimento)
        .AddPair('ForaDoMunicipio', ForaMunicipio)
        .AddPair('Valor',  NFSe.Servico.Valores.ValorServicos)
        .AddPair('ValorDeducao', NFSe.Servico.Valores.ValorDeducoes)
        .AddPair('ValorDescontoCondicional', NFSe.Servico.Valores.DescontoCondicionado)
        .AddPair('ValorDescontoIncondicional', NFSe.Servico.Valores.DescontoIncondicionado)
        .AddPair('ValorPis', NFSe.Servico.Valores.ValorPis)
        .AddPair('ValorCofins', NFSe.Servico.Valores.ValorCofins)
        .AddPair('ValorIrrf', NFSe.Servico.Valores.ValorIr)
        .AddPair('ValorInss', NFSe.Servico.Valores.ValorInss)
        .AddPair('ValorCsll', NFSe.Servico.Valores.ValorCsll)
        .AddPair('ValorOutrasRetencoes', NFSe.Servico.Valores.OutrasRetencoes)
        .AddPair('Aliquota', NFSe.Servico.Valores.Aliquota)
        .AddPair('Tributacao', FpAOwner.TributacaoToStr(NFSe.Servico.Tributacao))
        .AddPair('PercentualIbpt', 0)
        .AddPair('Observacoes', NFSe.OutrasInformacoes)
        .AddPair('PaisPrestacao', NFSe.Servico.CodigoPais)
        .AddPair('CodigoArt', NFSe.ConstrucaoCivil.Art)
        .AddPair('MatriculaCei', NFSe.ConstrucaoCivil.nCei)
        .AddPair('Correlacao', NFSe.IdentificacaoRps.Numero)
        .AddPair('Tomador', GerarTomador)
        .AddPair('TomadorPais', NFSe.Tomador.Endereco.xPais)
        .AddPair('Itens', GerarItens);

    Result := AJSon.ToJSON;
    Result := TiraAcentos(Result);
  finally
    AJSon.Free;
  end;
end;

function TNFSeW_eISS.GerarTomador: TACBrJSONObject;
begin
  Result := TACBrJSONObject.Create
              .AddPair('Ccm', NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal)
              .AddPair('Documento', GerarTomadorDocumento)
              .AddPair('Nome', NFSe.Tomador.RazaoSocial)
              .AddPair('RgIe', NFSe.Tomador.IdentificacaoTomador.InscricaoEstadual)
              .AddPair('Telefone', NFSe.Tomador.Contato.Telefone)
              .AddPair('Email', NFSe.Tomador.Contato.Email)
              .AddPair('Endereco', GerarTomadorEndereco);
end;

function TNFSeW_eISS.GerarTomadorDocumento: TACBrJSONObject;
var
  wTipoPessoa: String;
begin
  case NFSe.Tomador.IdentificacaoTomador.Tipo of
    tpPF: wTipoPessoa := '1';
    tpPJforaPais: wTipoPessoa := '3';
  else
    wTipoPessoa := '2';
  end;

  Result := TACBrJSONObject.Create
              .AddPair('Tipo', wTipoPessoa)
              .AddPair('Valor', NFSe.Tomador.IdentificacaoTomador.CpfCnpj);
end;

function TNFSeW_eISS.GerarTomadorEndereco: TACBrJSONObject;
begin
  Result := TACBrJSONObject.Create
              .AddPair('Logradouro', NFSe.Tomador.Endereco.Endereco)
              .AddPair('Numero', NFSe.Tomador.Endereco.Numero)
              .AddPair('Complemento', NFSe.Tomador.Endereco.Complemento)
              .AddPair('Bairro', NFSe.Tomador.Endereco.Bairro)
              .AddPair('Cep', StrToIntDef(NFSe.Tomador.Endereco.CEP, 0))
              .AddPair('Cidade', NFSe.Tomador.Endereco.xMunicipio)
              .AddPair('Estado', NFSe.Tomador.Endereco.UF)
              .AddPair('Pais', NFSe.Tomador.Endereco.xPais);
end;

function TNFSeW_eISS.GerarItens: TACBrJSONArray;
var
  jo: TACBrJSONObject;
  i: Integer;
begin
  Result := TACBrJSONArray.Create;
  for i := 0 to NFSe.Servico.ItemServico.Count - 1 do
  begin
    jo := TACBrJSONObject.Create
      .AddPair('Quantidade', NFSe.Servico.ItemServico[i].Quantidade)
      .AddPair('ValorUnitario', NFSe.Servico.ItemServico[i].ValorUnitario)
      .AddPair('Descricao', NFSe.Servico.ItemServico[i].Descricao);

    Result.AddElementJSON(jo);
  end;
end;

end.
