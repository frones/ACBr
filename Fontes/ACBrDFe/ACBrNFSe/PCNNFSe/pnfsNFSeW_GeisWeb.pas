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

unit pnfsNFSeW_GeisWeb;

interface

uses
{$IFDEF FPC}
  LResources, Controls, Graphics, Dialogs,
{$ELSE}

{$ENDIF}
  SysUtils, Classes, StrUtils,
  ACBrConsts,
  pnfsNFSeW, pcnAuxiliar, pcnConversao, pcnGerador,
  pnfsNFSe, pnfsConversao, pnfsConsts;

type
  { TNFSeW_GeisWeb }

  TNFSeW_GeisWeb = class(TNFSeWClass)
  private
    procedure GerarPrestador;
    procedure GerarTomador;
  protected

    procedure GerarListaServicos;
    procedure GerarValoresServico;

    procedure GerarXML_GeisWeb;

  public
    constructor Create(ANFSeW: TNFSeW); override;

    function ObterNomeArquivo: String; override;
    function GerarXml: Boolean; override;
  end;

implementation

uses
  ACBrUtil.Strings;

{==============================================================================}
{ Essa unit tem por finalidade exclusiva de gerar o XML do RPS segundo o       }
{ layout do GeisWeb.                                                           }
{ Sendo assim só será criado uma nova unit para um novo layout.                }
{==============================================================================}

{ TNFSeW_GeisWeb }

procedure TNFSeW_GeisWeb.GerarPrestador;
begin
  Gerador.wGrupo('PrestadorServico');
  Gerador.Prefixo := Prefixo3;
  Gerador.wGrupo('IdentificacaoPrestador');
  Gerador.Prefixo := Prefixo3;
  Gerador.wCampo(tcStr, '', 'CnpjCpf', 01, 14, 1, NFSe.PrestadorServico.IdentificacaoPrestador.Cnpj, '');
  Gerador.wCampo(tcStr, '', 'InscricaoMunicipal', 01, 16, 1, NFSe.PrestadorServico.IdentificacaoPrestador.InscricaoMunicipal, '');
  Gerador.wCampo(tcInt, '', 'Regime', 01, 01, 1, 6, '');  {revisar - ta chumbado a opção auto-alocado que consta no manual (6)}
  Gerador.wGrupo('/IdentificacaoPrestador');
  Gerador.wGrupo('/PrestadorServico');
end;

procedure TNFSeW_GeisWeb.GerarTomador;
begin
  Gerador.wGrupo('TomadorServico');
  Gerador.Prefixo := Prefixo3;
  Gerador.wGrupo('IdentificacaoTomador');
  Gerador.Prefixo := Prefixo3;
  Gerador.wCampo(tcStr, '', 'CnpjCpf', 01, 14, 1, Nfse.Tomador.IdentificacaoTomador.CpfCnpj, '');
  Gerador.wGrupo('/IdentificacaoTomador');

  Gerador.wCampo(tcStr, '', 'RazaoSocial', 01, 100, 1, Nfse.Tomador.RazaoSocial, '');

  Gerador.wGrupo('Endereco');
  Gerador.Prefixo := Prefixo3;
  Gerador.wCampo(tcStr, '', 'Rua', 01, 60, 1, Nfse.Tomador.Endereco.Endereco, '');
  Gerador.wCampo(tcStr, '', 'Numero', 01, 10, 1, Nfse.Tomador.Endereco.Numero, '');
  Gerador.wCampo(tcStr, '', 'Bairro', 01, 50, 1, Nfse.Tomador.Endereco.Bairro, '');
  Gerador.wCampo(tcStr, '', 'Cidade', 01, 40, 1, Nfse.Tomador.Endereco.CodigoMunicipio, '');
  Gerador.wCampo(tcStr, '', 'Estado', 01, 02, 1, Nfse.Tomador.Endereco.UF, '');
  Gerador.wCampo(tcStr, '', 'Cep', 01, 11, 1, Nfse.Tomador.Endereco.CEP, '');
  Gerador.wGrupo('/Endereco');
  Gerador.wGrupo('/TomadorServico');
end;

procedure TNFSeW_GeisWeb.GerarListaServicos;
begin
  Gerador.wGrupo('Servico');
  Gerador.Prefixo := Prefixo3;

  GerarValoresServico;

  Gerador.wCampo(tcStr, '', 'CodigoServico', 01, 04, 1, NFSe.Servico.ItemListaServico, '');
  Gerador.wCampo(tcStr, '', 'TipoLancamento', 01, 01, 1, 'P', ''); {revisar}
  Gerador.wCampo(tcStr, '', 'Discriminacao ', 01, 1500, 1, Nfse.Servico.Discriminacao, '');
  Gerador.wCampo(tcStr, '', 'MunicipioPrestacaoServico', 01, 100, 1, Nfse.Servico.CodigoMunicipio, ''); {revisar}
  Gerador.wGrupo('/Servico');
end;

procedure TNFSeW_GeisWeb.GerarValoresServico;
begin
  Gerador.wGrupo('Valores');
  Gerador.Prefixo := Prefixo3;
  Gerador.wCampo(tcDe2, '', 'ValorServicos', 01, 13, 1, Nfse.Servico.Valores.ValorServicos, '');
  Gerador.wCampo(tcDe2, '', 'BaseCalculo', 01, 013, 1, Nfse.Servico.Valores.BaseCalculo, '');
  Gerador.wCampo(tcDe2, '', 'Aliquota', 01, 013, 1, Nfse.Servico.Valores.Aliquota, '');
  Gerador.wGrupo('/Valores');
end;

procedure TNFSeW_GeisWeb.GerarXML_GeisWeb;
begin
  Gerador.Prefixo := Prefixo4;

  Gerador.wGrupo('Rps ' + 'xmlns="http://www.geisweb.net.br/xsd/envio_lote_rps.xsd"');
  Gerador.wGrupo('IdentificacaoRps');
  Gerador.wCampo(tcInt, '', 'NumeroRps', 01, 08, 1, NFSe.IdentificacaoRps.Numero, '');
  Gerador.wGrupo('/IdentificacaoRps');
  Gerador.wCampo(tcStr, '', 'DataEmissao', 01, 10, 1, formatdatetime('dd/mm/yyyy', NFSe.DataEmissao), '');

  GerarListaServicos;
  GerarPrestador;
  GerarTomador;

  Gerador.wGrupo('OrgaoGerador');
  Gerador.Prefixo := Prefixo3;
  Gerador.wCampo(tcStr, '', 'CodigoMunicipio ', 01, 10, 1, Nfse.Servico.CodigoMunicipio, '');
  Gerador.wCampo(tcStr, '', 'Uf', 01, 02, 1, Nfse.Servico.UFPrestacao, '');
  Gerador.wGrupo('/OrgaoGerador');

  Gerador.wGrupo('OutrosImpostos');
  Gerador.Prefixo := Prefixo3;
  Gerador.wCampo(tcDe2, '', 'Pis ', 01, 13, 1, Nfse.Servico.Valores.ValorPis, '');
  Gerador.wCampo(tcInt, '', 'Cofins', 01, 13, 1, Nfse.Servico.Valores.ValorCofins, '');
  Gerador.wCampo(tcInt, '', 'Csll ', 01, 13, 1, Nfse.Servico.Valores.ValorCsll, '');
  Gerador.wCampo(tcInt, '', 'Irrf', 01, 13, 1, Nfse.Servico.Valores.ValorIr, '');   {revisar}
  Gerador.wCampo(tcInt, '', 'Inss ', 01, 13, 1, Nfse.Servico.Valores.ValorInss, '');
  Gerador.wGrupo('/OutrosImpostos');

  Gerador.wGrupo('/Rps');
end;

constructor TNFSeW_GeisWeb.Create(ANFSeW: TNFSeW);
begin
  inherited Create(ANFSeW);
end;

function TNFSeW_GeisWeb.ObterNomeArquivo: String;
begin
  Result := OnlyNumber(NFSe.infID.ID) + '.xml';
end;

function TNFSeW_GeisWeb.GerarXml: Boolean;
begin
  Gerador.ListaDeAlertas.Clear;

  Gerador.Opcoes.DecimalChar := '.';
  Gerador.Opcoes.QuebraLinha := FQuebradeLinha;

  GerarXML_GeisWeb;

  Gerador.gtAjustarRegistros(NFSe.InfID.ID);
  Result := (Gerador.ListaDeAlertas.Count = 0);
end;

end.
