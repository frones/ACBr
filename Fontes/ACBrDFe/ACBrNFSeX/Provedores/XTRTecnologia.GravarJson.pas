{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
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

unit XTRTecnologia.GravarJson;

interface

uses
  SysUtils, Classes, Variants, StrUtils,
  ACBrJSON,
  ACBrNFSeXGravarXml,
  ACBrNFSeXConversao;

type
  { TNFSeW_XTRTecnologia }

  TNFSeW_XTRTecnologia = class(TNFSeWClass)
  protected
    procedure Configuracao; override;

    function GerarDadosNota: string;

    function GerarItens: TACBrJSONArray;
  public
    function GerarXml: Boolean; override;
  end;

implementation

uses
  DateUtils,
  ACBrXmlBase,
  ACBrUtil.Strings,
  ACBrConsts;

//==============================================================================
// Essa unit tem por finalidade exclusiva gerar o Json do RPS do provedor:
//     XTRTecnologia
//==============================================================================

{ TNFSeW_XTRTecnologia }

procedure TNFSeW_XTRTecnologia.Configuracao;
begin
  inherited Configuracao;

end;

function TNFSeW_XTRTecnologia.GerarXml: Boolean;
begin
  Configuracao;

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

function TNFSeW_XTRTecnologia.GerarDadosNota: string;
var
  AJSon: TACBrJSONObject;
  Token, ItemServico: string;
begin
  Token := ChaveAcesso;

  if Ambiente = taHomologacao then
    Token := 'homologacao';

  ItemServico :=  OnlyNumber(NFSe.Servico.ItemListaServico);

  AJSon := TACBrJsonObject.Create;
  try
    AJSon
        .AddPair('userid', Usuario)
        .AddPair('token', Token)
        .AddPair('id', 0)
        .AddPair('nr_nota', 0)
        .AddPair('ano', YearOf(NFSe.Competencia))
        .AddPair('mes', MonthOf(NFSe.Competencia))
        .AddPair('data_emissao', DateTimeToStr(NFSe.DataEmissao)) // "data_emissao": "02/01/2019 09:53",
        .AddPair('valor_nota', NFSe.Servico.Valores.ValorServicos)
        .AddPair('obs', NFSe.OutrasInformacoes)
        .AddPair('cod_issqn', ItemServico)
        .AddPair('cod_cnae', NFSe.Servico.CodigoCnae)
        .AddPair('modalidade', FPAOwner.TipoTributacaoRPSToStr(NFSe.TipoTributacaoRPS))
        .AddPair('nrrps', StrToIntDef(NFSe.IdentificacaoRps.Numero, 0))
        .AddPair('nrnotasub', '')
        .AddPair('local', NFSe.Servico.MunicipioPrestacaoServico)
        .AddPair('nome_tomador', NFSe.Tomador.RazaoSocial)
        .AddPair('tinscricao', NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal)
        .AddPair('tcnpjcpf', NFSe.Tomador.IdentificacaoTomador.CpfCnpj)
        .AddPair('tlogradouro', NFSe.Tomador.Endereco.Endereco)
        .AddPair('tnumero', NFSe.Tomador.Endereco.Numero)
        .AddPair('tcomplemento', NFSe.Tomador.Endereco.Complemento)
        .AddPair('tbairro', NFSe.Tomador.Endereco.Bairro)
        .AddPair('tcidade', NFSe.Tomador.Endereco.xMunicipio)
        .AddPair('tuf', NFSe.Tomador.Endereco.UF)
        .AddPair('tcep', StrToIntDef(NFSe.Tomador.Endereco.CEP, 0))
        .AddPair('temail', NFSe.Tomador.Contato.Email)
        .AddPair('tfone', NFSe.Tomador.Contato.Telefone)
        .AddPair('valor_base', NFSe.Servico.Valores.BaseCalculo)
        .AddPair('valor_desc', NFSe.Servico.Valores.ValorIr)
        .AddPair('valor_desci', NFSe.Servico.Valores.ValorPis)
        .AddPair('aliquota_iss', NFSe.Servico.Valores.Aliquota)
        .AddPair('valor_iss', NFSe.Servico.Valores.ValorIss)
        .AddPair('valor_outros', NFSe.Servico.Valores.ValorDeducoes)
        .AddPair('valor_inss', NFSe.Servico.Valores.ValorInss)
        .AddPair('valor_ir', NFSe.Servico.Valores.ValorIr)
        .AddPair('valor_pis', NFSe.Servico.Valores.ValorPis)
        .AddPair('valor_cofins', NFSe.Servico.Valores.ValorCofins)
        .AddPair('valor_csll', NFSe.Servico.Valores.ValorCsll)
        .AddPair('valor_materiais', 0)
        .AddPair('codigo_obra', NFSe.ConstrucaoCivil.CodigoObra)
        .AddPair('nr_art', NFSe.ConstrucaoCivil.Art)
        .AddPair('motivo_canc', '')
        .AddPair('itens', GerarItens);

    Result := AJSon.ToJSON;
  finally
    AJSon.Free;
  end;
end;

function TNFSeW_XTRTecnologia.GerarItens: TACBrJSONArray;
var
  jo: TACBrJSONObject;
  i: Integer;
  trib: string;
begin
  Result := TACBrJSONArray.Create;
  for i := 0 to NFSe.Servico.ItemServico.Count - 1 do
  begin
    trib := 'N';

    if NFSe.Servico.ItemServico[i].Tributavel = snSim then
      trib := 'S';

    jo := TACBrJSONObject.Create
      .AddPair('item', i + 1)
      .AddPair('descricao', NFSe.Servico.ItemServico[i].Descricao)
      .AddPair('valor_total', NFSe.Servico.ItemServico[i].ValorTotal)
      .AddPair('tributavel', trib);

    Result.AddElementJSON(jo);
  end;
end;

end.
