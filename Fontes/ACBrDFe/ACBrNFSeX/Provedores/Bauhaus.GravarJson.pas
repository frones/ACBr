{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
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

unit Bauhaus.GravarJson;

interface

uses
  SysUtils, Classes, Variants, StrUtils,
  ACBrXmlBase, ACBrXmlDocument, ACBrJSON,
  ACBrNFSeXParametros, ACBrNFSeXGravarXml,
  ACBrNFSeXConversao, ACBrNFSeXConsts;

type
  { TNFSeW_Bauhaus }

  TNFSeW_Bauhaus = class(TNFSeWClass)
  protected
    procedure Configuracao; override;

    function GerarDadosNota: String;

    function GerarAtividade: TACBrJSONObject;
    function GerarPrestador: TACBrJSONObject;
    function GerarTomador: TACBrJSONObject;
    function GerarTomadorEndereco: TACBrJSONObject;
    function GerarTomadorContato: TACBrJSONObject;
    function GerarRps: TACBrJSONObject;
    function GerarServicos: TACBrJSONArray;
    function GerarValores: TACBrJSONObject;
  public
    function GerarXml: Boolean; override;
  end;

implementation

uses
  ACBrNFSeX,
  ACBrUtil.Base, ACBrUtil.Strings,
  ACBrConsts;

//==============================================================================
// Essa unit tem por finalidade exclusiva gerar o Json do RPS do provedor:
//     Bauhaus
//==============================================================================

{ TNFSeW_Bauhaus }

procedure TNFSeW_Bauhaus.Configuracao;
begin
  inherited Configuracao;

end;

function TNFSeW_Bauhaus.GerarXml: Boolean;
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

function TNFSeW_Bauhaus.GerarDadosNota: String;
var
  AJSon: TACBrJSONObject;
begin
  AJSon := TACBrJsonObject.Create;
  try
    AJSon
      .AddPairJSONObject('DadosNota', EmptyStr)
      .AsJSONObject['DadosNota']
        .AddPair('MunicipioPrestacao', StrToIntDef(NFSe.Servico.CodigoMunicipio, 0))
        .AddPair('NaturezaOperacao', StrToIntDef(NaturezaOperacaoToStr(NFSe.NaturezaOperacao), 0))
        .AddPair('IssRetido', IfThen((NFSe.Servico.Valores.IssRetido = stRetencao), 'S' , 'N'))
        .AddPair('Observacoes', NFSe.OutrasInformacoes)
        .AddPair('Atividade', GerarAtividade)
        .AddPair('Prestador', GerarPrestador)
        .AddPair('Tomador', GerarTomador)
        .AddPair('Rps', GerarRps)
        .AddPair('Servicos', GerarServicos)
        .AddPair('Valores', GerarValores);

    Result := AJSon.ToJSON;
  finally
    AJSon.Free;
  end;
end;

function TNFSeW_Bauhaus.GerarAtividade: TACBrJSONObject;
var
  wCodTribMun: String;
begin
  wCodTribMun := NFSe.Servico.CodigoTributacaoMunicipio;

  Result := TACBrJSONObject.Create
              .AddPair('Codigo', StrToIntDef(wCodTribMun, 0))
              .AddPair('CodigoCnae', NFSe.Servico.CodigoCnae);
end;

function TNFSeW_Bauhaus.GerarPrestador: TACBrJSONObject;
begin
  Result := TACBrJSONObject.Create
              .AddPair('InscricaoMunicipal', OnlyNumber(NFSe.Prestador.IdentificacaoPrestador.InscricaoMunicipal));
end;

function TNFSeW_Bauhaus.GerarTomador: TACBrJSONObject;
var
  wTipoPessoa: String;
begin
  case NFSe.Tomador.IdentificacaoTomador.Tipo of
    tpPF: wTipoPessoa := 'F';
    tpPJforaPais: wTipoPessoa := 'E';
  else
    wTipoPessoa := 'J';
  end;

  Result := TACBrJSONObject.Create
              .AddPair('TipoPessoa', wTipoPessoa)
              .AddPair('NrDocumento', NFSe.Tomador.IdentificacaoTomador.CpfCnpj)
              .AddPair('RazaoSocial', NFSe.Tomador.RazaoSocial)
              .AddPair('Endereco', GerarTomadorEndereco)
              .AddPair('Contato', GerarTomadorContato);
end;

function TNFSeW_Bauhaus.GerarTomadorEndereco: TACBrJSONObject;
begin
  Result := TACBrJSONObject.Create
              .AddPair('Logradouro', NFSe.Tomador.Endereco.Endereco)
              .AddPair('Numero', NFSe.Tomador.Endereco.Numero)
              .AddPair('Complemento', NFSe.Tomador.Endereco.Complemento)
              .AddPair('Bairro', NFSe.Tomador.Endereco.Bairro)
              .AddPair('Municipio', StrToIntDef(NFSe.Tomador.Endereco.CodigoMunicipio, 0))
              .AddPair('Cep', StrToIntDef(NFSe.Tomador.Endereco.CEP, 0));
end;

function TNFSeW_Bauhaus.GerarTomadorContato: TACBrJSONObject;
begin
  Result := TACBrJSONObject.Create
              .AddPair('Telefone', NFSe.Tomador.Contato.Telefone)
              .AddPair('Email', NFSe.Tomador.Contato.Email);
end;

function TNFSeW_Bauhaus.GerarRps: TACBrJSONObject;
begin
  Result := TACBrJSONObject.Create
              .AddPair('Numero', StrToIntDef(NFSe.IdentificacaoRps.Numero, 0))
              .AddPair('Serie', StrToIntDef(NFSe.IdentificacaoRps.Serie, 0))
              .AddPair('Tipo', 1)
              .AddPairISODate('DataEmissao', NFSe.DataEmissaoRps);
end;

function TNFSeW_Bauhaus.GerarServicos: TACBrJSONArray;
var
  jo: TACBrJSONObject;
  i: Integer;
begin
  Result := TACBrJSONArray.Create;
  for i := 0 to NFSe.Servico.ItemServico.Count - 1 do
  begin
    jo := TACBrJSONObject.Create
      .AddPair('Unidade', NFSe.Servico.ItemServico[i].Unidade)
      .AddPair('Quantidade', NFSe.Servico.ItemServico[i].Quantidade)
      .AddPair('Descricao', NFSe.Servico.ItemServico[i].Descricao)
      .AddPair('ValorUnitario', NFSe.Servico.ItemServico[i].ValorUnitario);
                                           
    Result.AddElementJSON(jo);
  end;
end;

function TNFSeW_Bauhaus.GerarValores: TACBrJSONObject;
begin
  with NFSe.Servico.Valores do
    Result := TACBrJSONObject.Create
      .AddPair('ValorServicos',  ValorServicos)
      .AddPair('ValorDeducoes', ValorDeducoes)
      .AddPair('ValorOutrasDeducoes', 0)
      .AddPair('ValorPis', ValorPis)
      .AddPair('ValorCofins', ValorCofins)
      .AddPair('ValorInss', ValorInss)
      .AddPair('ValorIr', ValorIr)
      .AddPair('ValorCsll', ValorCsll)
      .AddPair('OutrasRetencoes', OutrasRetencoes)
      .AddPair('DescontoIncondicionado', DescontoIncondicionado)
      .AddPair('DescontoCondicionado', DescontoCondicionado)
      .AddPair('BaseCalculo', BaseCalculo)
      .AddPair('Aliquota', Aliquota)
      .AddPair('ValorIss', ValorIss)
      .AddPair('ValorLiquidoNota', ValorLiquidoNfse)
      .AddPair('ValorTotalTributos', ValorTotalTributos)
      .AddPair('ValorCredito', NFSe.ValorCredito)
      .AddPair('ValorTotalNota', ValorLiquidoNfse);
end;

end.
