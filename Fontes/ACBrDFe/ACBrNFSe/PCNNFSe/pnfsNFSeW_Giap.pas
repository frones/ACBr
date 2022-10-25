{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Jurisato Junior                           }
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

unit pnfsNFSeW_Giap;
interface
uses
{$IFDEF FPC}
  LResources, 
  Controls, 
{$ELSE}
{$ENDIF}
  SysUtils, 
  Classes, 
  StrUtils,
  ACBrConsts,
  pnfsNFSeW, 
  pcnAuxiliar, 
  pcnConversao, 
  pcnGerador, 
  pnfsNFSe, 
  pnfsConversao;
  
type
  { TNFSeW_Giap }
  TNFSeW_Giap = class(TNFSeWClass)
  private
  protected
    procedure GerarIdentificacaoRPS;
    procedure GerarRPSSubstituido;
    procedure GerarPrestador;
    procedure GerarTomador;
    procedure GerarIntermediarioServico;
    procedure GerarServicoValores;
    procedure GerarListaServicos;
    procedure GerarValoresServico;
    procedure GerarConstrucaoCivil;
    procedure GerarCondicaoPagamento;
    procedure GerarTransportadora;
    procedure GerarXML_Giap;
  public
    constructor Create(ANFSeW: TNFSeW); override;
    function ObterNomeArquivo: String; override;
    function GerarXml: Boolean; override;
  end;
implementation
uses
  ACBrUtil.Strings;
{ TNFSeW_Giap }
constructor TNFSeW_Giap.Create(ANFSeW: TNFSeW);
begin
  inherited Create(ANFSeW);
end;
procedure TNFSeW_Giap.GerarCondicaoPagamento;
begin
end;
procedure TNFSeW_Giap.GerarConstrucaoCivil;
begin
end;
procedure TNFSeW_Giap.GerarIdentificacaoRPS;
begin
end;
procedure TNFSeW_Giap.GerarIntermediarioServico;
begin
end;
procedure TNFSeW_Giap.GerarListaServicos;
begin
  Gerador.wGrupo('item');
  Gerador.wCampo(tcDe2, '', 'aliquota'      , 01, 015, 1, FNFSe.Servico.Valores.Aliquota, '');
  Gerador.wCampo(tcStr, '', 'cnae'          , 01, 08, 1, OnlyNumber(FNFSe.Servico.CodigoCnae), '');
  Gerador.wCampo(tcStr, '', 'codigo'     , 01, 04, 1, OnlyNumber(FNFSe.Servico.ItemListaServico), '');
  Gerador.wCampo(tcStr, '', 'descricao'  , 01, 4000, 1, StringReplace( NFSe.Servico.Discriminacao, ';', FQuebradeLinha, [rfReplaceAll, rfIgnoreCase] ), '');
  Gerador.wCampo(tcDe2, '', 'valor' , 01, 015, 1, FNFSe.Servico.Valores.ValorServicos, '');
  Gerador.wGrupo('/item');
end;
procedure TNFSeW_Giap.GerarPrestador;
begin
  Gerador.wGrupo('dadosPrestador');
  Gerador.wCampo(tcDatVcto, '', 'dataEmissao', 01, 21, 1, FNFSe.DataEmissaoRps, '');
  Gerador.wCampo(tcStr,     '', 'im', 01, 11, 1, FNFSe.Prestador.InscricaoMunicipal, '');
  Gerador.wCampo(tcStr,     '', 'numeroRps', 01, 11, 1, FNFSe.IdentificacaoRps.Numero, '');
  if Trim(FNFSe.Numero) <> EmptyStr then
    Gerador.wCampo(tcStr,     '', 'numeroNota', 01, 11, 1, FNFSe.Numero, '');
  if Trim(FNFSe.CodigoVerificacao) <> EmptyStr then
    Gerador.wCampo(tcStr,     '', 'codigoVerificacao', 01, 11, 1, FNFSe.CodigoVerificacao, '');
  if Trim(FNFSe.Link) <> EmptyStr then
    Gerador.wCampo(tcStr,     '', 'link', 01, 11, 1, FNFSe.Link, '');
  Gerador.wGrupo('/dadosPrestador');
  Gerador.wGrupo('dadosServico');
  Gerador.wCampo(tcStr,     '', 'bairro', 01, 25, 1, FNFSe.PrestadorServico.Endereco.Bairro, '');
  Gerador.wCampo(tcStr,     '', 'cep', 01, 09, 1, OnlyNumber(FNFSe.PrestadorServico.Endereco.CEP), '');
  Gerador.wCampo(tcStr,     '', 'cidade', 01, 30, 1, FNFSe.PrestadorServico.Endereco.xMunicipio, '');
  Gerador.wCampo(tcStr,     '', 'complemento', 01, 30, 1, FNFSe.PrestadorServico.Endereco.Complemento, '');
  Gerador.wCampo(tcStr,     '', 'logradouro', 01, 50, 1, FNFSe.PrestadorServico.Endereco.Endereco, '');
  Gerador.wCampo(tcStr,     '', 'numero', 01, 10, 1, FNFSe.PrestadorServico.Endereco.Numero, '');
  Gerador.wCampo(tcStr,     '', 'pais', 01, 09, 1, FNFSe.PrestadorServico.Endereco.xPais, '');
  Gerador.wCampo(tcStr,     '', 'uf', 01, 02, 1, FNFSe.PrestadorServico.Endereco.UF, '');
  Gerador.wGrupo('/dadosServico');
end;
procedure TNFSeW_Giap.GerarRPSSubstituido;
begin
end;
procedure TNFSeW_Giap.GerarServicoValores;
begin
  Gerador.wGrupo('detalheServico');
  Gerador.wCampo(tcDe2, '', 'cofins', 01, 02, 1, FNFSe.Servico.Valores.ValorCofins, '');
  Gerador.wCampo(tcDe2, '', 'csll', 01, 02, 1, FNFSe.Servico.Valores.ValorCsll, '');
  Gerador.wCampo(tcDe2, '', 'deducaoMaterial', 01, 02, 1, FNFSe.Servico.Valores.ValorDeducoes, '');
  Gerador.wCampo(tcDe2, '', 'descontoIncondicional', 01, 02, 1, FNFSe.Servico.Valores.DescontoIncondicionado, '');
  Gerador.wCampo(tcDe2, '', 'inss', 01, 02, 1, FNFSe.Servico.Valores.ValorInss, '');
  Gerador.wCampo(tcDe2, '', 'ir', 01, 02, 1, FNFSe.Servico.Valores.ValorIr, '');
  Gerador.wCampo(tcDe2, '', 'issRetido', 01, 02, 1, FNFSe.Servico.Valores.ValorIssRetido, '');
  GerarListaServicos;
  Gerador.wCampo(tcStr, '', 'obs', 01, 4000, 1, FNFSe.OutrasInformacoes, '');
  Gerador.wCampo(tcDe2, '', 'pisPasep', 01, 02, 1, FNFSe.Servico.Valores.ValorPis, '');
  Gerador.wGrupo('/detalheServico');
end;
procedure TNFSeW_Giap.GerarTomador;
begin
  Gerador.wGrupo('dadosTomador');
  Gerador.wCampo(tcStr, '', 'bairro',             01,  50, 1, FNFSe.Tomador.Endereco.Bairro, '');
  Gerador.wCampo(tcStr, '', 'cep',                01,  09, 1, OnlyNumber(FNFSe.Tomador.Endereco.CEP), '');
  if FNFSe.Tomador.Endereco.CodigoMunicipio = '9999999' then
    Gerador.wCampo(tcStr, '', 'cidade', 01, 50, 1, FNFSe.Tomador.Endereco.xMunicipio, '')
  else
    Gerador.wCampo(tcStr, '', 'cidade', 01, 50, 1, CodCidadeToCidade(StrToInt64Def(FNFSe.Tomador.Endereco.CodigoMunicipio, 0)), '');
  Gerador.wCampo(tcStr, '', 'complemento',               01,  60, 1, FNFSe.Tomador.Endereco.Complemento, '');
  Gerador.wCampo(tcStr, '', 'documento',            01,  14, 1, OnlyNumber(FNFSe.Tomador.IdentificacaoTomador.CpfCnpj), '');
  Gerador.wCampo(tcStr, '', 'email',              01,  60, 1, FNFSe.Tomador.Contato.Email, '');
  Gerador.wCampo(tcStr, '', 'ie',  01,  14, 1, OnlyNumber(FNFSe.Tomador.IdentificacaoTomador.InscricaoEstadual), '');
  Gerador.wCampo(tcStr, '', 'logradouro',           01,  50, 1, FNFSe.Tomador.Endereco.Endereco, '');
  Gerador.wCampo(tcStr, '', 'nomeTomador', 01, 120, 1, FNFSe.Tomador.RazaoSocial, '');
  Gerador.wCampo(tcStr, '', 'numero',            01,  50, 1, FNFSe.Tomador.Endereco.Numero, '');
  Gerador.wCampo(tcStr, '', 'pais',               01,  50, 1, FNFSe.Tomador.Endereco.xPais, '');
  if length(OnlyNumber(FNFSe.Tomador.IdentificacaoTomador.CpfCnpj)) = 11 then
    Gerador.wCampo(tcStr, '', 'tipoDoc', 01, 120, 1, 'F', '')
  else
    Gerador.wCampo(tcStr, '', 'tipoDoc', 01, 120, 1, 'J', '');
  Gerador.wCampo(tcStr, '', 'uf',             01,  50, 1, FNFSe.Tomador.Endereco.UF, '');
  Gerador.wGrupo('/dadosTomador');
end;
procedure TNFSeW_Giap.GerarTransportadora;
begin
end;
procedure TNFSeW_Giap.GerarValoresServico;
begin
end;
function TNFSeW_Giap.GerarXml: Boolean;
//Var
//  Gerar: boolean;
begin
  Gerador.ArquivoFormatoXML := '';
  Gerador.Prefixo := FPrefixo4;
  Gerador.Opcoes.DecimalChar := '.';
  Gerador.Opcoes.QuebraLinha := FQuebradeLinha;
  Atributo := '';
  Gerador.wGrupo('notaFiscal');
  FNFSe.InfID.ID := OnlyNumber(FNFSe.IdentificacaoRps.Numero) + FNFSe.IdentificacaoRps.Serie;
  GerarXML_Giap;
  Gerador.wGrupo('/notaFiscal');
  Gerador.gtAjustarRegistros(NFSe.InfID.ID);
  Result := (Gerador.ListaDeAlertas.Count = 0);
end;
procedure TNFSeW_Giap.GerarXML_Giap;
begin
  GerarPrestador;
  GerarTomador;
  GerarServicoValores;
end;
function TNFSeW_Giap.ObterNomeArquivo: String;
begin
  Result := OnlyNumber(NFSe.infID.ID) + '.xml';
end;
end.
