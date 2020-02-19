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

unit pnfsNFSeW_Elotech;

interface

uses
{$IFDEF FPC}
  LResources, Controls, Graphics, Dialogs,
{$ELSE}

{$ENDIF}
  SysUtils, Classes, StrUtils,
  synacode, ACBrConsts,
  pcnAuxiliar, pcnConversao, pcnGerador,
  pnfsNFSeW, pnfsNFSe, pnfsConversao, pnfsConsts;

type
  { TNFSeW_Elotech }

  TNFSeW_Elotech = class(TNFSeWClass)
  private
    FSituacao: String;
    FTipoRecolhimento: String;
  protected
    procedure GerarIdentificacaoRequerente;
    procedure GerarLoteRps;
    procedure GerarListaRps;
    procedure GerarDeclaracaoPrestacaoServico;
    procedure GerarInfDeclaracaoPrestacaoServico;
    procedure GerarRps;
    procedure GerarIdentificacaoRPS;
    procedure GerarServico;
    procedure GerarValores;
    procedure GerarListaItensServico;
    procedure GerarDadosPrestador;
    procedure GerarIdentificacaoPrestador;
    procedure GerarDadosPrestadorEndereco;
    procedure GerarDadosPrestadorContato;

    procedure GerarTomador;
    procedure GerarIdentificacaoTomador;
    procedure GerarEnderecoTomador;
    procedure GerarContatoTomador;

    procedure GerarXML_Elotech;

  public
    Homologacao : Boolean;
    constructor Create(ANFSeW: TNFSeW); override;

    function ObterNomeArquivo: String; override;
    function GerarXml: Boolean; override;

    property Situacao: String         read FSituacao;
    property TipoRecolhimento: String read FTipoRecolhimento;
  end;

implementation

uses
  ACBrUtil;

{==============================================================================}
{ Essa unit tem por finalidade exclusiva de gerar o XML do RPS segundo o       }
{ layout da Elotech.                                                           }
{ Sendo assim só será criado uma nova unit para um novo layout.                }
{==============================================================================}

{ TNFSeW_Elotech }

procedure TNFSeW_Elotech.GerarIdentificacaoRequerente;
begin
  Gerador.wGrupoNFSe('IdentificacaoRequerente');

  Gerador.wGrupoNFSe('CpfCnpj');
  if length(OnlyNumber(NFSe.Prestador.Cnpj)) <= 11 then
    Gerador.wCampoNFSe(tcStr, '#34', 'Cpf ', 11, 11, 1, OnlyNumber(NFSe.Prestador.Cnpj), DSC_CPF)
  else
    Gerador.wCampoNFSe(tcStr, '#34', 'Cnpj', 14, 14, 1, OnlyNumber(NFSe.Prestador.Cnpj), DSC_CNPJ);
  Gerador.wGrupoNFSe('/CpfCnpj');

  Gerador.wCampoNFSe(tcStr, '', 'InscricaoMunicipal', 0, 20, 0, NFSe.Prestador.InscricaoMunicipal);
  Gerador.wCampoNFSe(tcStr, '', 'Senha', 6, 30, 1, NFSe.Prestador.Senha);

  if NFSe.Producao = snNao then
    Gerador.wCampoNFSe(tcStr, '', 'Homologa', 1, 1, 1, '1')
  else
    Gerador.wCampoNFSe(tcStr, '', 'Homologa', 1, 1, 1, '0');

  Gerador.wGrupoNFSe('/IdentificacaoRequerente');
end;

procedure TNFSeW_Elotech.GerarLoteRps;
begin
  Gerador.wGrupoNFSe('LoteRps');
  Gerador.wCampoNFSe(tcStr, '', 'NumeroLote', 1, 15, 1, NFSe.IdentificacaoRps.Numero);
  Gerador.wCampoNFSe(tcStr, '', 'QuantidadeRps', 1, 4, 1, '1');
  GerarListaRps;
  Gerador.wGrupoNFSe('/LoteRps');
end;

procedure TNFSeW_Elotech.GerarListaRps;
begin
  Gerador.wGrupoNFSe('ListaRps');
  GerarDeclaracaoPrestacaoServico;
  Gerador.wGrupoNFSe('/ListaRps');
end;

procedure TNFSeW_Elotech.GerarDeclaracaoPrestacaoServico;
begin
  Gerador.wGrupoNFSe('DeclaracaoPrestacaoServico');
  GerarInfDeclaracaoPrestacaoServico;
  Gerador.wGrupoNFSe('/DeclaracaoPrestacaoServico');
end;

procedure TNFSeW_Elotech.GerarInfDeclaracaoPrestacaoServico;
begin
  Gerador.wGrupoNFSe('InfDeclaracaoPrestacaoServico');
  GerarRps;
  Gerador.wCampoNFSe(tcStr, '', 'Competencia', 10, 10, 1, FormatDateTimeBr(NFSe.DataEmissaoRps, 'yyyy-mm-dd'), DSC_DEMI);
  GerarServico;
  GerarDadosPrestador;
  GerarTomador;
  Gerador.wCampoNFSe(tcStr, '', 'RegimeEspecialTributacao', 1, 1, 0, RegimeEspecialTributacaoToStr(NFSe.RegimeEspecialTributacao));
  Gerador.wCampoNFSe(tcStr, '', 'IncentivoFiscal', 1, 1, 1, '2'); //1 - Sim / 2 – Não
  Gerador.wGrupoNFSe('/InfDeclaracaoPrestacaoServico');
end;

procedure TNFSeW_Elotech.GerarRps;
begin
  Gerador.wGrupoNFSe('Rps');
  GerarIdentificacaoRps;
  Gerador.wCampoNFSe(tcStr, '', 'Status', 1, 1, 1, '1'); //Código de status do RPS (1 – Normal / 2 – Cancelado)
  Gerador.wCampoNFSe(tcStr, '', 'DataEmissao', 1, 10, 1, FormatDateTimeBr(NFSe.DataEmissaoRps, 'yyyy-mm-dd'), DSC_DEMI);
  Gerador.wGrupoNFSe('/Rps');
end;

procedure TNFSeW_Elotech.GerarIdentificacaoRPS;
begin
  Gerador.wGrupoNFSe('IdentificacaoRps');
  Gerador.wCampoNFSe(tcStr, '', 'Numero', 1, 15, 1, NFSe.IdentificacaoRps.Numero);
  Gerador.wCampoNFSe(tcStr, '', 'Serie', 1, 5, 1, NFSe.IdentificacaoRps.Serie);
  Gerador.wCampoNFSe(tcStr, '', 'Tipo', 1, 1, 1, '1'); // 1 - RPS / 2 – Nota Fiscal Conjugada(Mista) / 3 – Cupom / 4 – Nota Fiscal Série Única
  Gerador.wGrupoNFSe('/IdentificacaoRps');
end;

procedure TNFSeW_Elotech.GerarServico;
begin
  Gerador.wGrupoNFSe('Servico');
  GerarValores;

  if NFSe.Servico.Valores.ValorIssRetido > 0 then
    Gerador.wCampoNFSe(tcStr, '', 'IssRetido', 1, 1, 1, '1') //1 - Sim
  else
    Gerador.wCampoNFSe(tcStr, '', 'IssRetido', 1, 1, 1, '2'); //2 - Não

  Gerador.wCampoNFSe(tcStr, '', 'Discriminacao', 1, 2000, 0, NFSe.Servico.Discriminacao);
  Gerador.wCampoNFSe(tcStr, '', 'CodigoMunicipio', 0, 7, 0, NFSe.Servico.CodigoMunicipio);

  //1 – Exigível / 2 – Não incidência / 3 – Isenção / 4 – Exportação / 5 – Imunidade
  //6 – Exigibilidade Suspensa por Decisão Judicial / 7 – Exigibilidade Suspensa por Processo Administrativo
  Gerador.wCampoNFSe(tcStr, '', 'ExigibilidadeISS', 1, 1, 1, '1');

  Gerador.wCampoNFSe(tcStr, '', 'MunicipioIncidencia', 0, 7, 0, NFSe.Servico.MunicipioIncidencia);
  GerarListaItensServico;
  Gerador.wGrupoNFSe('/Servico');
end;

procedure TNFSeW_Elotech.GerarValores;
begin
  Gerador.wGrupoNFSe('Valores');
  Gerador.wCampoNFSe(tcDe2, '', 'ValorServicos', 1, 17, 1, NFSe.Servico.Valores.ValorServicos);
  Gerador.wCampoNFSe(tcDe2, '', 'Aliquota', 0, 6, 0, NFSe.Servico.Valores.Aliquota);
  Gerador.wCampoNFSe(tcDe2, '', 'DescontoIncondicionado', 0, 17, 0, NFSe.Servico.Valores.DescontoIncondicionado);
  Gerador.wGrupoNFSe('/Valores');
end;

procedure TNFSeW_Elotech.GerarListaItensServico;
var
  i: Integer;
begin
  Gerador.wGrupoNFSe('ListaItensServico');

  for I := 0 to NFSe.Servico.ItemServico.Count - 1 do
  begin
    Gerador.wGrupoNFSe('ItemServico');
    Gerador.wCampoNFSe(tcStr, '', 'ItemListaServico', 1, 6, 1, NFSe.Servico.ItemServico[i].ItemListaServico);
    Gerador.wCampoNFSe(tcStr, '', 'CodigoCnae', 1, 7, 0, NFSe.Servico.CodigoCnae);
    Gerador.wCampoNFSe(tcStr, '', 'Descricao', 1, 20, 0, NFSe.Servico.ItemServico[i].Descricao);
    Gerador.wCampoNFSe(tcStr, '', 'Tributavel', 1, 1, 0, SimNaoToStr(NFSe.Servico.ItemServico[i].Tributavel));
    Gerador.wCampoNFSe(tcDe2, '', 'Quantidade', 0, 17, 0, NFSe.Servico.ItemServico[i].Quantidade);
    Gerador.wCampoNFSe(tcDe2, '', 'ValorUnitario', 0, 17, 0, NFSe.Servico.ItemServico[i].ValorUnitario);
    Gerador.wCampoNFSe(tcDe2, '', 'ValorDesconto', 0, 17, 1, NFSe.Servico.ItemServico[i].DescontoCondicionado);
    Gerador.wCampoNFSe(tcDe2, '', 'ValorLiquido', 0, 17, 1, NFSe.Servico.ItemServico[i].ValorTotal);
    Gerador.wGrupoNFSe('/ItemServico');
  end;
  Gerador.wGrupoNFSe('/ListaItensServico');
end;

procedure TNFSeW_Elotech.GerarDadosPrestador;
begin
  Gerador.wGrupoNFSe('DadosPrestador');
  GerarIdentificacaoPrestador;
  Gerador.wCampoNFSe(tcStr, '', 'RazaoSocial', 1, 150, 1, NFSe.Prestador.RazaoSocial);
  Gerador.wCampoNFSe(tcStr, '', 'NomeFantasia', 1, 60, 0, NFSe.Prestador.Fantasia);
  GerarDadosPrestadorEndereco;
  GerarDadosPrestadorContato;
  Gerador.wGrupoNFSe('/DadosPrestador');
end;

procedure TNFSeW_Elotech.GerarIdentificacaoPrestador;
begin
  Gerador.wGrupoNFSe('IdentificacaoPrestador');

  Gerador.wGrupoNFSe('CpfCnpj');
  if length(OnlyNumber(NFSe.Prestador.Cnpj)) <= 11 then
    Gerador.wCampoNFSe(tcStr, '#34', 'Cpf ', 11, 11, 1, OnlyNumber(NFSe.Prestador.Cnpj), DSC_CPF)
  else
    Gerador.wCampoNFSe(tcStr, '#34', 'Cnpj', 14, 14, 1, OnlyNumber(NFSe.Prestador.Cnpj), DSC_CNPJ);
  Gerador.wGrupoNFSe('/CpfCnpj');

  Gerador.wCampoNFSe(tcStr, '', 'InscricaoMunicipal', 0, 20, 0, NFSe.Prestador.InscricaoMunicipal);
  Gerador.wGrupoNFSe('/IdentificacaoPrestador');
end;

procedure TNFSeW_Elotech.GerarDadosPrestadorEndereco;
begin
  Gerador.wGrupoNFSe('Endereco');
  Gerador.wCampoNFSe(tcStr, '', 'Endereco', 1, 125, 0, NFSe.Prestador.Endereco.Endereco);
  Gerador.wCampoNFSe(tcStr, '', 'Numero', 1, 10, 0, NFSe.Prestador.Endereco.Numero);
  Gerador.wCampoNFSe(tcStr, '', 'Complemento', 1, 60, 0, NFSe.Prestador.Endereco.Complemento);
  Gerador.wCampoNFSe(tcStr, '', 'Bairro', 1, 60, 0, NFSe.Prestador.Endereco.Bairro);
  Gerador.wCampoNFSe(tcStr, '', 'CodigoMunicipio', 0, 7, 0, NFSe.Prestador.Endereco.CodigoMunicipio);
  Gerador.wCampoNFSe(tcStr, '', 'CidadeNome', 1, 125, 0, NFSe.Prestador.Endereco.xMunicipio);
  Gerador.wCampoNFSe(tcStr, '', 'Uf', 1, 2, 0, NFSe.Prestador.Endereco.UF);
  Gerador.wCampoNFSe(tcStr, '', 'CodigoPais', 1, 4, 0, NFSe.Prestador.Endereco.CodigoPais);
  Gerador.wCampoNFSe(tcStr, '', 'Cep', 0, 8, 0, OnlyNumber(NFSe.Prestador.Endereco.CEP));
  Gerador.wGrupoNFSe('/Endereco');
end;

procedure TNFSeW_Elotech.GerarDadosPrestadorContato;
begin
  Gerador.wGrupoNFSe('Contato');
  Gerador.wCampoNFSe(tcStr, '', 'Telefone', 1, 20, 0, OnlyNumber(NFSe.Prestador.Telefone));
  Gerador.wCampoNFSe(tcStr, '', 'Email', 1, 80, 0, NFSe.Prestador.Email);
  Gerador.wGrupoNFSe('/Contato');
end;

procedure TNFSeW_Elotech.GerarTomador;
begin
  Gerador.wGrupoNFSe('Tomador');
  GerarIdentificacaoTomador;
  Gerador.wCampoNFSe(tcStr, '', 'RazaoSocial', 1, 150, 0, NFSe.Tomador.RazaoSocial);
  GerarEnderecoTomador;
  GerarContatoTomador;
  Gerador.wGrupoNFSe('/Tomador');
end;

procedure TNFSeW_Elotech.GerarIdentificacaoTomador;
begin
  Gerador.wGrupoNFSe('IdentificacaoTomador');

  Gerador.wGrupoNFSe('CpfCnpj');
  if length(OnlyNumber(NFSe.Tomador.IdentificacaoTomador.CpfCnpj)) <= 11 then
    Gerador.wCampoNFSe(tcStr, '#34', 'Cpf ', 11, 11, 1, OnlyNumber(NFSe.Tomador.IdentificacaoTomador.CpfCnpj), DSC_CPF)
  else
    Gerador.wCampoNFSe(tcStr, '#34', 'Cnpj', 14, 14, 1, OnlyNumber(NFSe.Tomador.IdentificacaoTomador.CpfCnpj), DSC_CNPJ);
  Gerador.wGrupoNFSe('/CpfCnpj');

  Gerador.wGrupoNFSe('/IdentificacaoTomador');
end;

procedure TNFSeW_Elotech.GerarEnderecoTomador;
begin
  Gerador.wGrupoNFSe('Endereco');
  Gerador.wCampoNFSe(tcStr, '', 'Endereco', 1, 125, 0, NFSe.Tomador.Endereco.Endereco);
  Gerador.wCampoNFSe(tcStr, '', 'Numero', 1, 10, 0, NFSe.Tomador.Endereco.Numero);
  Gerador.wCampoNFSe(tcStr, '', 'Bairro', 1, 60, 0, NFSe.Tomador.Endereco.Bairro);
  Gerador.wCampoNFSe(tcStr, '', 'CodigoMunicipio', 0, 7, 0, NFSe.Tomador.Endereco.CodigoMunicipio);
  Gerador.wCampoNFSe(tcStr, '', 'CidadeNome', 1, 125, 0, NFSe.Tomador.Endereco.xMunicipio);
  Gerador.wCampoNFSe(tcStr, '', 'Uf', 1, 2, 0, NFSe.Tomador.Endereco.UF);
  Gerador.wCampoNFSe(tcStr, '', 'Cep', 0, 8, 0, OnlyNumber(NFSe.Tomador.Endereco.CEP));
  Gerador.wGrupoNFSe('/Endereco');
end;

procedure TNFSeW_Elotech.GerarContatoTomador;
begin
  Gerador.wGrupoNFSe('Contato');
  Gerador.wCampoNFSe(tcStr, '', 'Telefone', 1, 20, 0, NFSe.Tomador.Contato.Telefone);
  Gerador.wGrupoNFSe('/Contato');
end;

procedure TNFSeW_Elotech.GerarXML_Elotech;
begin
  Gerador.Opcoes.DecimalChar := '.';

  Gerador.Prefixo := '';

  Gerador.wGrupoNFSe('EnviarLoteRpsSincronoEnvio');
  GerarIdentificacaoRequerente;
  GerarLoteRps;
  Gerador.wGrupoNFSe('/EnviarLoteRpsSincronoEnvio');
end;

constructor TNFSeW_Elotech.Create(ANFSeW: TNFSeW);
begin
  Homologacao := False;
  inherited Create(ANFSeW);
end;

function TNFSeW_Elotech.ObterNomeArquivo: String;
begin
  Result := OnlyNumber(NFSe.infID.ID) + '.xml';
end;

function TNFSeW_Elotech.GerarXml: Boolean;
begin
  Gerador.ListaDeAlertas.Clear;

  Gerador.ArquivoFormatoXML := '';
  Gerador.Prefixo           := FPrefixo4;

  Gerador.Opcoes.QuebraLinha := FQuebradeLinha;

  FDefTipos := FServicoEnviar;

  if (RightStr(FURL, 1) <> '/') and (FDefTipos <> '')
    then FDefTipos := '/' + FDefTipos;

  if Trim(FPrefixo4) <> ''
    then Atributo := ' xmlns:' + StringReplace(Prefixo4, ':', '', []) + '="' + FURL + FDefTipos + '"'
    else Atributo := ' xmlns="' + FURL + FDefTipos + '"';

  FNFSe.InfID.ID := FNFSe.IdentificacaoRps.Numero;

  GerarXML_Elotech;

  Gerador.gtAjustarRegistros(NFSe.InfID.ID);
  Result := (Gerador.ListaDeAlertas.Count = 0);
end;

end.
