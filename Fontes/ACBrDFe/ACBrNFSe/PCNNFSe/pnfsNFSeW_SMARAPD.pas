{******************************************************************************}
{ Projeto: Componente ACBrNFSe                                                 }
{  Biblioteca multiplataforma de componentes Delphi                            }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do Projeto ACBr     }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr           }
{                                                                              }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{$I ACBr.inc}

unit pnfsNFSeW_SMARAPD;

interface

uses
{$IFDEF FPC}
  LResources, Controls, Graphics, Dialogs,
{$ELSE}

{$ENDIF}
  SysUtils, Classes,
  ACBrConsts,
  pnfsNFSeW, pcnAuxiliar, pcnConversao, pcnGerador, pnfsNFSe, pnfsConversao;

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
  ACBrUtil;

{ TNFSeW_SMARAPD }

constructor TNFSeW_SMARAPD.Create(ANFSeW: TNFSeW);
begin
  inherited Create(ANFSeW);
end;

procedure TNFSeW_SMARAPD.GerarCondicaoPagamento;
var
  i: Integer;
begin
  Gerador.wGrupoNFSe('tbfatura');
  for i := 0 to FNFSe.CondicaoPagamento.Parcelas.Count - 1 do
  begin
    Gerador.wGrupoNFSe('fatura');
    Gerador.wCampoNFSe(tcStr,    '', 'numfatura',        01, 12, 1, FNFSe.CondicaoPagamento.Parcelas.Items[i].Parcela, '');
    Gerador.wCampoNFSe(tcDatVcto,'', 'vencimentofatura', 01, 12, 1, FNFSe.CondicaoPagamento.Parcelas.Items[i].DataVencimento, '');
    Gerador.wCampoNFSe(tcDe2,    '', 'valorfatura',      01, 12, 1, FNFSe.CondicaoPagamento.Parcelas.Items[i].Valor, '');
    Gerador.wGrupoNFSe('/fatura');
  end;
  Gerador.wGrupoNFSe('/tbfatura');
end;

procedure TNFSeW_SMARAPD.GerarConstrucaoCivil;
begin
  if (FNFSe.Servico.Valores.ValorDeducoes > 0) then
  begin
    Gerador.wCampoNFSe(tcStr, '', 'descdeducoesconstrucao', 01, 255, 1, FNFSe.Servico.Valores.JustificativaDeducao, '');
    Gerador.wCampoNFSe(tcStr, '', 'totaldeducoesconstrucao', 01, 15, 1, FormatCurr('0.00', FNFSe.Servico.Valores.ValorDeducoes), '');
  end
  else
  begin
    Gerador.wCampoNFSe(tcStr, '', 'descdeducoesconstrucao', 01, 255, 1, '', '');
    Gerador.wCampoNFSe(tcStr, '', 'totaldeducoesconstrucao', 01, 15, 1, '', '');
  end;
end;

procedure TNFSeW_SMARAPD.GerarIdentificacaoRPS;
begin
  Gerador.wCampoNFSe(tcStr,     '', 'numeronfd',                 01, 12, 1, '0', '');
  Gerador.wCampoNFSe(tcStr,     '', 'codseriedocumento',         01, 12, 1, FNFSe.IdentificacaoRps.Serie, '');
  Gerador.wCampoNFSe(tcStr,     '', 'codnaturezaoperacao',       01, 12, 1, NaturezaOperacaoToStr(FNFSe.NaturezaOperacao), '');
  Gerador.wCampoNFSe(tcStr,     '', 'codigocidade',              01, 12, 1, '3', '');
  Gerador.wCampoNFSe(tcStr,     '', 'inscricaomunicipalemissor', 01, 11, 1, FNFSe.Prestador.InscricaoMunicipal, '');
  Gerador.wCampoNFSe(tcDatVcto, '', 'dataemissao',               01, 21, 1, FNFSe.DataEmissao, '');
end;

procedure TNFSeW_SMARAPD.GerarIntermediarioServico;
begin
  if (NFSe.IntermediarioServico.RazaoSocial<>'') or
     (NFSe.IntermediarioServico.CpfCnpj <> '') then
  begin
    Gerador.wGrupoNFSe('IntermediarioServico');
    Gerador.wCampoNFSe(tcStr, '', 'RazaoSocial', 001, 115, 0, NFSe.IntermediarioServico.RazaoSocial, '');
    Gerador.wCampoNFSe(tcStr, '', 'CpfCnpj'    , 14, 14, 1, OnlyNumber(NFSe.IntermediarioServico.CpfCnpj), '');

    if Length(OnlyNumber(NFSe.IntermediarioServico.CpfCnpj)) <= 11 then
      Gerador.wCampoNFSe(tcStr, '', 'IndicacaoCpfCnpj', 01, 01, 1, '1', '')
    else
      Gerador.wCampoNFSe(tcStr, '', 'IndicacaoCpfCnpj', 01, 01, 1, '2', '');

    Gerador.wCampoNFSe(tcStr, '', 'InscricaoMunicipal', 01, 15, 0, NFSe.IntermediarioServico.InscricaoMunicipal, '');
    Gerador.wGrupoNFSe('/IntermediarioServico');
  end;
end;

procedure TNFSeW_SMARAPD.GerarListaServicos;
var
  i: Integer;
begin
  Gerador.wGrupoNFSe('tbservico');
  for i := 0 to NFSe.Servico.ItemServico.Count - 1 do
  begin
    Gerador.wGrupoNFSe('servico');
    Gerador.wCampoNFSe(tcDe2, '', 'quantidade'    , 01, 015, 1, FNFSe.Servico.ItemServico[i].Quantidade, '');
    Gerador.wCampoNFSe(tcStr, '', 'descricao'     , 01, 255, 1, FNFSe.Servico.ItemServico[i].Descricao, '');
    Gerador.wCampoNFSe(tcStr, '', 'codatividade'  , 01, 020, 1, FNFSe.Servico.ItemServico[i].CodLCServ, '');
    Gerador.wCampoNFSe(tcDe2, '', 'valorunitario' , 01, 015, 1, FNFSe.Servico.ItemServico[i].ValorUnitario, '');
    Gerador.wCampoNFSe(tcDe2, '', 'aliquota'      , 01, 015, 1, FNFSe.Servico.ItemServico[i].Aliquota, '');
    if FNFSe.Servico.Valores.IssRetido in [stNormal,stSubstituicao] then
      Gerador.wCampoNFSe(tcStr, '', 'impostoretido', 01, 005, 1, 'False', '')
    else
      Gerador.wCampoNFSe(tcStr, '', 'impostoretido', 01, 005, 1, 'True', '');
    Gerador.wGrupoNFSe('/servico');
  end;
  Gerador.wGrupoNFSe('/tbservico');
end;

procedure TNFSeW_SMARAPD.GerarServicoValores;
begin
  Gerador.wCampoNFSe(tcDe2, '', 'pis', 01, 02, 1, FNFSe.Servico.Valores.ValorPis, '');
  Gerador.wCampoNFSe(tcDe2, '', 'cofins', 01, 02, 1, FNFSe.Servico.Valores.ValorCofins, '');
  Gerador.wCampoNFSe(tcDe2, '', 'csll', 01, 02, 1, FNFSe.Servico.Valores.ValorCsll, '');
  Gerador.wCampoNFSe(tcDe2, '', 'irrf', 01, 02, 1, FNFSe.Servico.Valores.ValorIr, '');
  Gerador.wCampoNFSe(tcDe2, '', 'inss', 01, 02, 1, FNFSe.Servico.Valores.ValorInss, '');
  Gerador.wCampoNFSe(tcStr, '', 'descdeducoesconstrucao', 01, 500, 1, FNFSe.Servico.Valores.JustificativaDeducao, '');
  Gerador.wCampoNFSe(tcDe2, '', 'totaldeducoesconstrucao', 01, 02, 1, FNFSe.Servico.Valores.ValorDeducoes, '');
end;

procedure TNFSeW_SMARAPD.GerarTomador;
begin
  Gerador.wCampoNFSe(tcStr, '', 'razaotomador', 01, 120, 1, FNFSe.Tomador.RazaoSocial, '');

  if length(OnlyNumber(FNFSe.Tomador.IdentificacaoTomador.CpfCnpj)) = 11 then
    Gerador.wCampoNFSe(tcStr, '', 'tppessoa', 01, 120, 1, 'F', '')
  else
    Gerador.wCampoNFSe(tcStr, '', 'tppessoa', 01, 120, 1, 'J', '');

  if FNFSe.Tomador.Endereco.CodigoMunicipio = '9999999' then
    Gerador.wCampoNFSe(tcStr, '', 'tppessoa', 01, 120, 1, 'O', '');

  Gerador.wCampoNFSe(tcStr, '', 'nomefantasiatomador',       01, 120, 1, FNFSe.Tomador.RazaoSocial, '');
  Gerador.wCampoNFSe(tcStr, '', 'enderecotomador',           01,  50, 1, FNFSe.Tomador.Endereco.Endereco, '');
  Gerador.wCampoNFSe(tcStr, '', 'numeroendereco',            01,  50, 1, FNFSe.Tomador.Endereco.Numero, '');

  if FNFSe.Tomador.Endereco.CodigoMunicipio = '9999999' then
    Gerador.wCampoNFSe(tcStr, '', 'cidadetomador', 01, 50, 1, FNFSe.Tomador.Endereco.xMunicipio, '')
  else
    Gerador.wCampoNFSe(tcStr, '', 'cidadetomador', 01, 50, 1, CodCidadeToCidade(StrToInt64Def(FNFSe.Tomador.Endereco.CodigoMunicipio, 3202405)), '');

  Gerador.wCampoNFSe(tcStr, '', 'estadotomador',             01,  50, 1, FNFSe.Tomador.Endereco.UF, '');
  Gerador.wCampoNFSe(tcStr, '', 'paistomador',               01,  50, 1, FNFSe.Tomador.Endereco.xPais, '');
  Gerador.wCampoNFSe(tcStr, '', 'fonetomador',               01,  60, 1, FNFSe.Tomador.Contato.Telefone, '');
  Gerador.wCampoNFSe(tcStr, '', 'faxtomador',                01,  60, 1, '', '');
  Gerador.wCampoNFSe(tcStr, '', 'ceptomador',                01,  08, 1, OnlyNumber(FNFSe.Tomador.Endereco.CEP), '');
  Gerador.wCampoNFSe(tcStr, '', 'bairrotomador',             01,  50, 1, FNFSe.Tomador.Endereco.Bairro, '');
  Gerador.wCampoNFSe(tcStr, '', 'emailtomador',              01,  60, 1, FNFSe.Tomador.Contato.Email, '');
  Gerador.wCampoNFSe(tcStr, '', 'cpfcnpjtomador',            01,  14, 1, OnlyNumber(FNFSe.Tomador.IdentificacaoTomador.CpfCnpj), '');
  Gerador.wCampoNFSe(tcStr, '', 'inscricaoestadualtomador',  01,  14, 1, OnlyNumber(FNFSe.Tomador.IdentificacaoTomador.InscricaoEstadual), '');
  Gerador.wCampoNFSe(tcStr, '', 'inscricaomunicipaltomador', 01,  14, 1, OnlyNumber(FNFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal), '');
  Gerador.wCampoNFSe(tcStr, '', 'observacao',                01, 500, 1, FNFSe.OutrasInformacoes,'');
end;

procedure TNFSeW_SMARAPD.GerarTransportadora;
begin
  Gerador.wCampoNFSe(tcStr, '', 'razaotransportadora',    01, 255, 1, FNFSe.Transportadora.xNomeTrans, '');
  Gerador.wCampoNFSe(tcStr, '', 'cpfcnpjtransportadora',  01,  20, 1, OnlyNumber(FNFSe.Transportadora.xCpfCnpjTrans), '');
  Gerador.wCampoNFSe(tcStr, '', 'enderecotransportadora', 01, 255, 1, FNFSe.Transportadora.xEndTrans, '');
  Gerador.wCampoNFSe(tcStr, '', 'tipofrete',              01, 255, 1, 2, '');
  Gerador.wCampoNFSe(tcStr, '', 'quantidade',             01, 255, 1, '', '');
  Gerador.wCampoNFSe(tcStr, '', 'especie',                01, 255, 1, '', '');
  Gerador.wCampoNFSe(tcStr, '', 'pesoliquido',            01, 255, 1, '0', '');
  Gerador.wCampoNFSe(tcStr, '', 'pesobruto',              01, 255, 1, '0', '');
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
  Gerador.wGrupoNFSe('nfd');
  GerarIdentificacaoRPS;
  GerarTomador;
  GerarCondicaoPagamento;
  GerarListaServicos;
  GerarTransportadora;
  GerarServicoValores;
  GerarConstrucaoCivil;
  Gerador.wCampoNFSe(tcStr,     '', 'tributadonomunicipio', 01,  5, 1, 'true', '');
  Gerador.wCampoNFSe(tcStr,     '', 'numerort',             01, 02, 1, FNFSe.IdentificacaoRps.Numero, '');
  Gerador.wCampoNFSe(tcStr,     '', 'codigoseriert',        01, 02, 1, '17', '');
  Gerador.wCampoNFSe(tcDatVcto, '', 'dataemissaort',        01, 21, 1, FNFSe.DataEmissaoRps, '');
  if NFSe.Competencia <> '' then
    Gerador.wCampoNFSe(tcStr, '', 'fatorgerador',           01, 21, 1, FNFSe.Competencia, '');
  Gerador.wGrupoNFSe('/nfd');
end;

function TNFSeW_SMARAPD.ObterNomeArquivo: String;
begin
  Result := OnlyNumber(NFSe.infID.ID) + '.xml';
end;

end.
