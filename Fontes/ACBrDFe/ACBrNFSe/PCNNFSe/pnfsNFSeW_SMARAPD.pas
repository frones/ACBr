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

unit pnfsNFSeW_SMARAPD;

interface

uses
{$IFDEF FPC}
  LResources, 
  Controls, 
{$ELSE}

{$ENDIF}
  SysUtils, 
  Classes,
  ACBrConsts,
  pnfsNFSeW, 
  pcnAuxiliar, 
  pcnConversao, 
  pcnGerador, 
  pnfsNFSe, 
  pnfsConversao;

type
  { TNFSeW_SMARAPD }

  TNFSeW_SMARAPD = class(TNFSeWClass)
  private
  protected

    procedure GerarIdentificacaoRPS;

    procedure GerarTomador;
    procedure GerarIntermediarioServico;

    procedure GerarServicoValores;
    procedure GerarListaServicos;

    procedure GerarConstrucaoCivil;
    procedure GerarCondicaoPagamento;

    procedure GerarTransportadora;

    procedure GerarXML_SMARAPD;

  public
    constructor Create(ANFSeW: TNFSeW); override;

    function ObterNomeArquivo: String; override;
    function GerarXml: Boolean; override;
  end;

implementation

uses
  ACBrUtil.Strings;

{ TNFSeW_SMARAPD }

constructor TNFSeW_SMARAPD.Create(ANFSeW: TNFSeW);
begin
  inherited Create(ANFSeW);
end;

procedure TNFSeW_SMARAPD.GerarCondicaoPagamento;
var
  i: Integer;
begin
  Gerador.wGrupo('tbfatura');
  for i := 0 to FNFSe.CondicaoPagamento.Parcelas.Count - 1 do
  begin
    Gerador.wGrupo('fatura');
    Gerador.wCampo(tcStr,    '', 'numfatura',        01, 12, 1, FNFSe.CondicaoPagamento.Parcelas.Items[i].Parcela, '');
    Gerador.wCampo(tcDatVcto,'', 'vencimentofatura', 01, 12, 1, FNFSe.CondicaoPagamento.Parcelas.Items[i].DataVencimento, '');
    Gerador.wCampo(tcDe2,    '', 'valorfatura',      01, 12, 1, FNFSe.CondicaoPagamento.Parcelas.Items[i].Valor, '');
    Gerador.wGrupo('/fatura');
  end;
  Gerador.wGrupo('/tbfatura');
end;

procedure TNFSeW_SMARAPD.GerarConstrucaoCivil;
begin
  if (FNFSe.Servico.Valores.ValorDeducoes > 0) then
  begin
    Gerador.wCampo(tcStr, '', 'descdeducoesconstrucao', 01, 255, 1, FNFSe.Servico.Valores.JustificativaDeducao, '');
    Gerador.wCampo(tcStr, '', 'totaldeducoesconstrucao', 01, 15, 1, FormatCurr('0.00', FNFSe.Servico.Valores.ValorDeducoes), '');
  end
  else
  begin
    Gerador.wCampo(tcStr, '', 'descdeducoesconstrucao', 01, 255, 1, '', '');
    Gerador.wCampo(tcStr, '', 'totaldeducoesconstrucao', 01, 15, 1, '', '');
  end;
end;

procedure TNFSeW_SMARAPD.GerarIdentificacaoRPS;
begin
  Gerador.wCampo(tcStr,     '', 'numeronfd',                 01, 12, 1, '0', '');
  Gerador.wCampo(tcStr,     '', 'codseriedocumento',         01, 12, 1, FNFSe.IdentificacaoRps.Serie, '');
  Gerador.wCampo(tcStr,     '', 'codnaturezaoperacao',       01, 12, 1, NaturezaOperacaoToStr(FNFSe.NaturezaOperacao), '');
  Gerador.wCampo(tcStr,     '', 'codigocidade',              01, 12, 1, '3', '');
  Gerador.wCampo(tcStr,     '', 'inscricaomunicipalemissor', 01, 11, 1, FNFSe.Prestador.InscricaoMunicipal, '');
  Gerador.wCampo(tcDatVcto, '', 'dataemissao',               01, 21, 1, FNFSe.DataEmissao, '');
end;

procedure TNFSeW_SMARAPD.GerarIntermediarioServico;
begin
  if (NFSe.IntermediarioServico.RazaoSocial<>'') or
     (NFSe.IntermediarioServico.CpfCnpj <> '') then
  begin
    Gerador.wGrupo('IntermediarioServico');
    Gerador.wCampo(tcStr, '', 'RazaoSocial', 001, 115, 0, NFSe.IntermediarioServico.RazaoSocial, '');
    Gerador.wCampo(tcStr, '', 'CpfCnpj'    , 14, 14, 1, OnlyNumber(NFSe.IntermediarioServico.CpfCnpj), '');

    if Length(OnlyNumber(NFSe.IntermediarioServico.CpfCnpj)) <= 11 then
      Gerador.wCampo(tcStr, '', 'IndicacaoCpfCnpj', 01, 01, 1, '1', '')
    else
      Gerador.wCampo(tcStr, '', 'IndicacaoCpfCnpj', 01, 01, 1, '2', '');

    Gerador.wCampo(tcStr, '', 'InscricaoMunicipal', 01, 15, 0, NFSe.IntermediarioServico.InscricaoMunicipal, '');
    Gerador.wGrupo('/IntermediarioServico');
  end;
end;

procedure TNFSeW_SMARAPD.GerarListaServicos;
var
  i: Integer;
begin
  Gerador.wGrupo('tbservico');
  for i := 0 to NFSe.Servico.ItemServico.Count - 1 do
  begin
    Gerador.wGrupo('servico');
    Gerador.wCampo(tcDe2, '', 'quantidade'    , 01, 015, 1, FNFSe.Servico.ItemServico[i].Quantidade, '');
    Gerador.wCampo(tcStr, '', 'descricao'     , 01, 255, 1, FNFSe.Servico.ItemServico[i].Descricao, '');
    Gerador.wCampo(tcStr, '', 'codatividade'  , 01, 020, 1, FNFSe.Servico.ItemServico[i].CodLCServ, '');
    Gerador.wCampo(tcDe2, '', 'valorunitario' , 01, 015, 1, FNFSe.Servico.ItemServico[i].ValorUnitario, '');
    Gerador.wCampo(tcDe2, '', 'aliquota'      , 01, 015, 1, FNFSe.Servico.ItemServico[i].Aliquota, '');
    if FNFSe.Servico.Valores.IssRetido in [stNormal,stSubstituicao] then
      Gerador.wCampo(tcStr, '', 'impostoretido', 01, 005, 1, 'false', '')
    else
      Gerador.wCampo(tcStr, '', 'impostoretido', 01, 005, 1, 'true', '');
    Gerador.wGrupo('/servico');
  end;
  Gerador.wGrupo('/tbservico');
end;

procedure TNFSeW_SMARAPD.GerarServicoValores;
begin
  Gerador.wCampo(tcDe2, '', 'pis', 01, 02, 1, FNFSe.Servico.Valores.ValorPis, '');
  Gerador.wCampo(tcDe2, '', 'cofins', 01, 02, 1, FNFSe.Servico.Valores.ValorCofins, '');
  Gerador.wCampo(tcDe2, '', 'csll', 01, 02, 1, FNFSe.Servico.Valores.ValorCsll, '');
  Gerador.wCampo(tcDe2, '', 'irrf', 01, 02, 1, FNFSe.Servico.Valores.ValorIr, '');
  Gerador.wCampo(tcDe2, '', 'inss', 01, 02, 1, FNFSe.Servico.Valores.ValorInss, '');
  Gerador.wCampo(tcStr, '', 'descdeducoesconstrucao', 01, 500, 1, FNFSe.Servico.Valores.JustificativaDeducao, '');
  Gerador.wCampo(tcDe2, '', 'totaldeducoesconstrucao', 01, 02, 1, FNFSe.Servico.Valores.ValorDeducoes, '');
end;

procedure TNFSeW_SMARAPD.GerarTomador;
begin
  Gerador.wCampo(tcStr, '', 'razaotomador', 01, 120, 1, FNFSe.Tomador.RazaoSocial, '');

  if length(OnlyNumber(FNFSe.Tomador.IdentificacaoTomador.CpfCnpj)) = 11 then
    Gerador.wCampo(tcStr, '', 'tppessoa', 01, 120, 1, 'F', '')
  else
    Gerador.wCampo(tcStr, '', 'tppessoa', 01, 120, 1, 'J', '');

  if FNFSe.Tomador.Endereco.CodigoMunicipio = '9999999' then
    Gerador.wCampo(tcStr, '', 'tppessoa', 01, 120, 1, 'O', '');

  Gerador.wCampo(tcStr, '', 'nomefantasiatomador',       01, 120, 1, FNFSe.Tomador.RazaoSocial, '');
  Gerador.wCampo(tcStr, '', 'enderecotomador',           01,  50, 1, FNFSe.Tomador.Endereco.Endereco, '');
  Gerador.wCampo(tcStr, '', 'numeroendereco',            01,  50, 1, FNFSe.Tomador.Endereco.Numero, '');

  if FNFSe.Tomador.Endereco.CodigoMunicipio = '9999999' then
    Gerador.wCampo(tcStr, '', 'cidadetomador', 01, 50, 1, FNFSe.Tomador.Endereco.xMunicipio, '')
  else
    Gerador.wCampo(tcStr, '', 'cidadetomador', 01, 50, 1, CodCidadeToCidade(StrToInt64Def(FNFSe.Tomador.Endereco.CodigoMunicipio, 3202405)), '');

  Gerador.wCampo(tcStr, '', 'estadotomador',             01,  50, 1, FNFSe.Tomador.Endereco.UF, '');
  Gerador.wCampo(tcStr, '', 'paistomador',               01,  50, 1, FNFSe.Tomador.Endereco.xPais, '');
  Gerador.wCampo(tcStr, '', 'fonetomador',               01,  60, 1, FNFSe.Tomador.Contato.Telefone, '');
  Gerador.wCampo(tcStr, '', 'faxtomador',                01,  60, 1, '', '');
  Gerador.wCampo(tcStr, '', 'ceptomador',                01,  08, 1, OnlyNumber(FNFSe.Tomador.Endereco.CEP), '');
  Gerador.wCampo(tcStr, '', 'bairrotomador',             01,  50, 1, FNFSe.Tomador.Endereco.Bairro, '');
  Gerador.wCampo(tcStr, '', 'emailtomador',              01,  60, 1, FNFSe.Tomador.Contato.Email, '');
  Gerador.wCampo(tcStr, '', 'cpfcnpjtomador',            01,  14, 1, OnlyNumber(FNFSe.Tomador.IdentificacaoTomador.CpfCnpj), '');
  Gerador.wCampo(tcStr, '', 'inscricaoestadualtomador',  01,  14, 1, OnlyNumber(FNFSe.Tomador.IdentificacaoTomador.InscricaoEstadual), '');
  Gerador.wCampo(tcStr, '', 'inscricaomunicipaltomador', 01,  14, 1, OnlyNumber(FNFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal), '');
  Gerador.wCampo(tcStr, '', 'observacao',                01, 500, 1, FNFSe.OutrasInformacoes,'');
end;

procedure TNFSeW_SMARAPD.GerarTransportadora;
begin
  Gerador.wCampo(tcStr, '', 'razaotransportadora',    01, 255, 1, FNFSe.Transportadora.xNomeTrans, '');
  Gerador.wCampo(tcStr, '', 'cpfcnpjtransportadora',  01,  20, 1, OnlyNumber(FNFSe.Transportadora.xCpfCnpjTrans), '');
  Gerador.wCampo(tcStr, '', 'enderecotransportadora', 01, 255, 1, FNFSe.Transportadora.xEndTrans, '');
  Gerador.wCampo(tcStr, '', 'tipofrete',              01, 255, 1, 2, '');
  Gerador.wCampo(tcStr, '', 'quantidade',             01, 255, 1, '', '');
  Gerador.wCampo(tcStr, '', 'especie',                01, 255, 1, '', '');
  Gerador.wCampo(tcStr, '', 'pesoliquido',            01, 255, 1, '0', '');
  Gerador.wCampo(tcStr, '', 'pesobruto',              01, 255, 1, '0', '');
end;

function TNFSeW_SMARAPD.GerarXml: Boolean;
begin
  Gerador.ArquivoFormatoXML := '';
  Gerador.Prefixo           := FPrefixo4;

  Gerador.Opcoes.DecimalChar := ',';
  Gerador.Opcoes.QuebraLinha := FQuebradeLinha;

  Atributo := '';
  Gerador.wGrupo('tbnfd');
  FNFSe.InfID.ID := OnlyNumber(FNFSe.IdentificacaoRps.Numero) + FNFSe.IdentificacaoRps.Serie;
  GerarXML_Smarapd;

  Gerador.wGrupo('/tbnfd');
  Gerador.gtAjustarRegistros(NFSe.InfID.ID);
  Result := (Gerador.ListaDeAlertas.Count = 0);
end;

procedure TNFSeW_SMARAPD.GerarXML_SMARAPD;
begin
  Gerador.Prefixo := '';
  Gerador.wGrupo('nfd');
  GerarIdentificacaoRPS;
  GerarTomador;
  GerarCondicaoPagamento;
  GerarListaServicos;
  GerarTransportadora;
  GerarServicoValores;
  GerarConstrucaoCivil;

  if FNFSe.Tomador.Endereco.CodigoMunicipio <> FNFSe.PrestadorServico.Endereco.CodigoMunicipio then
    Gerador.wCampo(tcStr,     '', 'tributadonomunicipio', 01,  5, 1, 'false', '')
  else
    Gerador.wCampo(tcStr,     '', 'tributadonomunicipio', 01,  5, 1, 'true', '');

  Gerador.wCampo(tcStr,     '', 'numerort',             01, 02, 1, FNFSe.IdentificacaoRps.Numero, '');
  Gerador.wCampo(tcStr,     '', 'codigoseriert',        01, 02, 1, '17', '');
  Gerador.wCampo(tcDatVcto, '', 'dataemissaort',        01, 21, 1, FNFSe.DataEmissaoRps, '');
  if NFSe.Competencia <> '' then
    Gerador.wCampo(tcStr, '', 'fatorgerador',           01, 21, 1, FNFSe.Competencia, '');
  Gerador.wGrupo('/nfd');
end;

function TNFSeW_SMARAPD.ObterNomeArquivo: String;
begin
  Result := OnlyNumber(NFSe.infID.ID) + '.xml';
end;

end.
