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
{ Fernando Castelano Banhos -  fernado@lucedata.com.br  -  www.lucedata.com.br }
{ Alameda Demetrio Cavlak, 1377 - Lucélia - SP - 17780-000                     }
{                                                                              }
{******************************************************************************}

{$I ACBr.inc}

unit pnfsNFSeW_WEBFISCO;

interface

uses
{$IFDEF FPC}
  LResources, Controls, Graphics, Dialogs,
{$ELSE}

{$ENDIF}
  SysUtils, Classes, StrUtils,
  synacode, ACBrConsts,
  pnfsNFSeW,
  pcnAuxiliar, pcnConversao, pcnGerador,
  pnfsNFSe, pnfsConversao, pnfsConsts;

type
  { TNFSeW_WEBFISCOPublico }

  TNFSeW_WEBFISCO = class(TNFSeWClass)
  private
    FOpcoes: TGeradorOpcoes;
  protected
    procedure GerarNotas;
  public
    constructor Create(ANFSeW: TNFSeW); override;

    function ObterNomeArquivo: String; override;
    function GerarXml: Boolean; override;

    property Opcoes: TGeradorOpcoes read FOpcoes write FOpcoes;
  end;

implementation

uses
  ACBrUtil, MaskUtils;

{==============================================================================}
{ Essa unit tem por finalidade exclusiva de gerar o XML do RPS segundo o       }
{ layout do provedor WEBFISCO.                                          }
{ Sendo assim só será criado uma nova unit para um novo layout.                }
{==============================================================================}

{ TNFSeW_WEBFISCO }

constructor TNFSeW_WEBFISCO.Create(ANFSeW: TNFSeW);
begin
  inherited Create(ANFSeW);

  FOpcoes := TGeradorOpcoes.Create;
  FOpcoes.TagVaziaNoFormatoResumido := False;
end;

procedure TNFSeW_WEBFISCO.GerarNotas;
Var
  cSimples: Boolean;
begin
  cSimples := (NFSe.OptanteSimplesNacional = snSim);
  //............................................................................

  Gerador.wGrupo('EnvNfe');

  Gerador.wCampoNFSe(tcInt, '', 'usuario', 1,  6, 1, NFSe.Prestador.Usuario, '');
  Gerador.wCampoNFSe(tcInt, '', 'pass'   , 1,  6, 1, NFSe.Prestador.Senha, '');
  Gerador.wCampoNFSe(tcStr, '', 'prf'    , 1, 18, 1, NFSe.Prestador.CNPJ_Prefeitura, '');
  Gerador.wCampoNFSe(tcStr, '', 'usr'    , 1, 18, 1, NFSe.Prestador.Cnpj, '');
  Gerador.wCampoNFSe(tcStr, '', 'ctr'    , 1,  8, 1, NFSe.IdentificacaoRps.Numero, '');
  Gerador.wCampoNFSe(tcStr, '', 'cnpj'   , 1, 18, 1, NFSe.Tomador.IdentificacaoTomador.CpfCnpj, '');
  Gerador.wCampoNFSe(tcStr, '', 'cnpjn'  , 1, 60, 1, NFSe.Tomador.RazaoSocial, '');
  Gerador.wCampoNFSe(tcStr, '', 'ie'     , 1, 20, 1, NFSe.Tomador.IdentificacaoTomador.InscricaoEstadual, '');
  Gerador.wCampoNFSe(tcStr, '', 'im'     , 1, 15, 1, NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal, '');
  Gerador.wCampoNFSe(tcStr, '', 'lgr'    , 1, 60, 1, NFSe.Tomador.Endereco.Endereco, '');
  Gerador.wCampoNFSe(tcStr, '', 'num'    , 1,  6, 1, NFSe.Tomador.Endereco.Numero, '');
  Gerador.wCampoNFSe(tcStr, '', 'cpl'    , 1, 20, 1, NFSe.Tomador.Endereco.Complemento, '');
  Gerador.wCampoNFSe(tcStr, '', 'bai'    , 1, 40, 1, NFSe.Tomador.Endereco.Bairro, '');
  Gerador.wCampoNFSe(tcStr, '', 'cid'    , 1, 40, 1, NFSe.Tomador.Endereco.xMunicipio, '');
  Gerador.wCampoNFSe(tcStr, '', 'est'    , 1,  2, 1, NFSe.Tomador.Endereco.UF, '');
  Gerador.wCampoNFSe(tcStr, '', 'cep'    , 1,  8, 1, NFSe.Tomador.Endereco.CEP, '');
  Gerador.wCampoNFSe(tcStr, '', 'fon'    , 1, 12, 1, NFSe.Tomador.Contato.Telefone, '');
  Gerador.wCampoNFSe(tcStr, '', 'mail'   , 1, 50, 1, NFSe.Tomador.Contato.Email, '');

  Gerador.wCampoNFSe(tcDatVcto, '', 'dat', 1, 10, 1, NFSe.DataEmissao, '');

  Gerador.wCampoNFSe(tcStr, '', 'f1n', 1 ,15, 1, '', '');
  Gerador.wCampoNFSe(tcStr, '', 'f1d', 1, 10, 1, '', '');
  Gerador.wCampoNFSe(tcStr, '', 'f1v', 1, 12, 1, '', '');
  Gerador.wCampoNFSe(tcStr, '', 'f2n', 1, 15, 1, '', '');
  Gerador.wCampoNFSe(tcStr, '', 'f2d', 1, 10, 1, '', '');
  Gerador.wCampoNFSe(tcStr, '', 'f2v', 1, 12, 1, '', '');
  Gerador.wCampoNFSe(tcStr, '', 'f3n', 1, 15, 1, '', '');
  Gerador.wCampoNFSe(tcStr, '', 'f3d', 1, 10, 1, '', '');
  Gerador.wCampoNFSe(tcStr, '', 'f3v', 1, 12, 1, '', '');
  Gerador.wCampoNFSe(tcStr, '', 'f4n', 1 ,15, 1, '', '');
  Gerador.wCampoNFSe(tcStr, '', 'f4d', 1, 10, 1, '', '');
  Gerador.wCampoNFSe(tcStr, '', 'f4v', 1, 12, 1, '', '');
  Gerador.wCampoNFSe(tcStr, '', 'f5n', 1 ,15, 1, '', '');
  Gerador.wCampoNFSe(tcStr, '', 'f5d', 1, 10, 1, '', '');
  Gerador.wCampoNFSe(tcStr, '', 'f5v', 1, 12, 1, '', '');
  Gerador.wCampoNFSe(tcStr, '', 'f6n', 1 ,15, 1, '', '');
  Gerador.wCampoNFSe(tcStr, '', 'f6d', 1, 10, 1, '', '');
  Gerador.wCampoNFSe(tcStr, '', 'f6v', 1, 12, 1, '', '');

  Gerador.wCampoNFSe(tcStr, '', 'item1', 1,  5, 1, NFSe.Servico.ItemListaServico, '');
  Gerador.wCampoNFSe(tcStr, '', 'item2', 1,  5, 1, '', '');
  Gerador.wCampoNFSe(tcStr, '', 'item3', 1,  5, 1, '', '');
  Gerador.wCampoNFSe(tcDe2, '', 'aliq1', 1,  5, 1, NFSe.Servico.Valores.Aliquota, '');
  Gerador.wCampoNFSe(tcDe2, '', 'aliq2', 1,  5, 1, 0.00, '');
  Gerador.wCampoNFSe(tcDe2, '', 'aliq3', 1,  5, 1, 0.00, '');
  Gerador.wCampoNFSe(tcDe2, '', 'val1' , 1, 12, 1, NFSe.Servico.ItemServico.Items[0].ValorUnitario, '');
  Gerador.wCampoNFSe(tcDe2, '', 'val2' , 1, 12, 1, 0.00, '');
  Gerador.wCampoNFSe(tcDe2, '', 'val3' , 1, 12, 1, 0.00, '');

  //Código da localidade de execução do serviço, se no local do estabelecimento do prestador, deixar como 0000...
  if (NFSe.PrestadorServico.Endereco.CodigoMunicipio <> IntToStr(NFSe.Servico.MunicipioIncidencia)) then
    Gerador.wCampoNFSe(tcStr, '', 'loc', 1, 4, 1, NFSe.Servico.CodigoMunicipio, '')
  else
    Gerador.wCampoNFSe(tcStr, '', 'loc', 1, 4, 1, '0000', '');

  if NFSe.Servico.Valores.IssRetido = stRetencao then
    Gerador.wCampoNFSe(tcStr, '', 'ret', 1, 3, 1, 'SIM', '')
  else
    Gerador.wCampoNFSe(tcStr, '', 'ret', 1, 3, 1, 'NAO', '');

  Gerador.wCampoNFSe(tcStr, '', 'txt'    , 1, 720, 1, NFSe.Servico.ItemServico[0].Descricao, '');
  Gerador.wCampoNFSe(tcDe2, '', 'val'    , 1,  12, 1, NFSe.Servico.Valores.ValorServicos, '');
  Gerador.wCampoNFSe(tcDe2, '', 'valtrib', 1,  12, 1, NFSe.Servico.Valores.ValorServicos, '');
  Gerador.wCampoNFSe(tcDe2, '', 'iss'    , 1,  12, 1, NFSe.Servico.Valores.ValorIss, '');
  Gerador.wCampoNFSe(tcDe2, '', 'issret' , 1,  12, 1, NFSe.Servico.Valores.ValorIssRetido, '');
  Gerador.wCampoNFSe(tcDe2, '', 'desci'  , 1,  12, 1, NFSe.Servico.Valores.DescontoIncondicionado, '');
  Gerador.wCampoNFSe(tcDe2, '', 'desco'  , 1,  12, 1, NFSe.Servico.Valores.DescontoCondicionado, '');
  Gerador.wCampoNFSe(tcDe2, '', 'binss'  , 1,  12, 1, 0.00, '');
  Gerador.wCampoNFSe(tcDe2, '', 'birrf'  , 1,  12, 1, 0.00, '');
  Gerador.wCampoNFSe(tcDe2, '', 'bcsll'  , 1,  12, 1, 0.00, '');
  Gerador.wCampoNFSe(tcDe2, '', 'bpis'   , 1,  12, 1, 0.00, '');
  Gerador.wCampoNFSe(tcDe2, '', 'bcofins', 1,  12, 1, 0.00, '');
  Gerador.wCampoNFSe(tcDe2, '', 'ainss'  , 1,   6, 1, 0.00, '');
  Gerador.wCampoNFSe(tcDe2, '', 'airrf'  , 1,   6, 1, 0.00, '');
  Gerador.wCampoNFSe(tcDe2, '', 'acsll'  , 1,   6, 1, 0.00, '');
  Gerador.wCampoNFSe(tcDe2, '', 'apis'   , 1,   6, 1, 0.00, '');
  Gerador.wCampoNFSe(tcDe2, '', 'acofins', 1,   6, 1, 0.00, '');
  Gerador.wCampoNFSe(tcDe2, '', 'inss'   , 1,  12, 1, NFSe.Servico.Valores.ValorInss, '');
  Gerador.wCampoNFSe(tcDe2, '', 'irrf'   , 1,  12, 1, 0.00, '');
  Gerador.wCampoNFSe(tcDe2, '', 'csll'   , 1,  12, 1, 0.00, '');
  Gerador.wCampoNFSe(tcDe2, '', 'pis'    , 1,  12, 1, NFSe.Servico.Valores.ValorPis, '');
  Gerador.wCampoNFSe(tcDe2, '', 'cofins' , 1,  12, 1, NFSe.Servico.Valores.ValorCofins, '');
  Gerador.wCampoNFSe(tcStr, '', 'item4'  , 1,   5, 1, '', '');
  Gerador.wCampoNFSe(tcStr, '', 'item5'  , 1,   5, 1, '', '');
  Gerador.wCampoNFSe(tcStr, '', 'item6'  , 1,   5, 1, '', '');
  Gerador.wCampoNFSe(tcStr, '', 'item7'  , 1,   5, 1, '', '');
  Gerador.wCampoNFSe(tcStr, '', 'item8'  , 1,   5, 1, '', '');
  Gerador.wCampoNFSe(tcDe2, '', 'aliq4'  , 1,   5, 1, 0.00, '');
  Gerador.wCampoNFSe(tcDe2, '', 'aliq5'  , 1,   5, 1, 0.00, '');
  Gerador.wCampoNFSe(tcDe2, '', 'aliq6'  , 1,   5, 1, 0.00, '');
  Gerador.wCampoNFSe(tcDe2, '', 'aliq7'  , 1,   5, 1, 0.00, '');
  Gerador.wCampoNFSe(tcDe2, '', 'aliq8'  , 1,   5, 1, 0.00, '');
  Gerador.wCampoNFSe(tcDe2, '', 'val4'   , 1,  12, 1, 0.00, '');
  Gerador.wCampoNFSe(tcDe2, '', 'val5'   , 1,  12, 1, 0.00, '');
  Gerador.wCampoNFSe(tcDe2, '', 'val6'   , 1,  12, 1, 0.00, '');
  Gerador.wCampoNFSe(tcDe2, '', 'val7'   , 1,  12, 1, 0.00, '');
  Gerador.wCampoNFSe(tcDe2, '', 'val8'   , 1,  12, 1, 0.00, '');
  Gerador.wCampoNFSe(tcStr, '', 'iteser1', 1,   5, 1, '', '');
  Gerador.wCampoNFSe(tcStr, '', 'iteser2', 1,   5, 1, '', '');
  Gerador.wCampoNFSe(tcStr, '', 'iteser3', 1,   5, 1, '', '');
  Gerador.wCampoNFSe(tcStr, '', 'iteser4', 1,   5, 1, '', '');
  Gerador.wCampoNFSe(tcStr, '', 'iteser5', 1,   5, 1, '', '');
  Gerador.wCampoNFSe(tcStr, '', 'iteser6', 1,   5, 1, '', '');
  Gerador.wCampoNFSe(tcStr, '', 'iteser7', 1,   5, 1, '', '');
  Gerador.wCampoNFSe(tcStr, '', 'iteser8', 1,   5, 1, '', '');
  Gerador.wCampoNFSe(tcDe2, '', 'alqser1', 1,   5, 1, 0.00, '');
  Gerador.wCampoNFSe(tcDe2, '', 'alqser2', 1,   5, 1, 0.00, '');
  Gerador.wCampoNFSe(tcDe2, '', 'alqser3', 1,   5, 1, 0.00, '');
  Gerador.wCampoNFSe(tcDe2, '', 'alqser4', 1,   5, 1, 0.00, '');
  Gerador.wCampoNFSe(tcDe2, '', 'alqser5', 1,   5, 1, 0.00, '');
  Gerador.wCampoNFSe(tcDe2, '', 'alqser6', 1,   5, 1, 0.00, '');
  Gerador.wCampoNFSe(tcDe2, '', 'alqser7', 1,   5, 1, 0.00, '');
  Gerador.wCampoNFSe(tcDe2, '', 'alqser8', 1,   5, 1, 0.00, '');
  Gerador.wCampoNFSe(tcDe2, '', 'valser1', 1,  12, 1, 0.00, '');
  Gerador.wCampoNFSe(tcDe2, '', 'valser2', 1,  12, 1, 0.00, '');
  Gerador.wCampoNFSe(tcDe2, '', 'valser3', 1,  12, 1, 0.00, '');
  Gerador.wCampoNFSe(tcDe2, '', 'valser4', 1,  12, 1, 0.00, '');
  Gerador.wCampoNFSe(tcDe2, '', 'valser5', 1,  12, 1, 0.00, '');
  Gerador.wCampoNFSe(tcDe2, '', 'valser6', 1,  12, 1, 0.00, '');
  Gerador.wCampoNFSe(tcDe2, '', 'valser7', 1,  12, 1, 0.00, '');
  Gerador.wCampoNFSe(tcDe2, '', 'valser8', 1,  12, 1, 0.00, '');
  Gerador.wCampoNFSe(tcStr, '', 'paisest', 1,  60, 1, '', '');

  Gerador.wCampoNFSe(tcDe2, '', 'sssrecbr', 1, 12, 1, IIf(cSimples = True, NFSe.Prestador.ValorReceitaBruta, 0.00), '');

  Gerador.wCampoNFSe(tcStr, '', 'ssanexo', 1, 15, 1, '', '');
  Gerador.wCampoNFSe(tcStr, '', 'ssdtini', 1, 10, 1, IIf(cSimples = True, FormatDateTime('DD/MM/YYYY', NFSe.Prestador.DataInicioAtividade), ' '), '');
  Gerador.wCampoNFSe(tcDe2, '', 'percded', 1,  6, 1, 0.00, '');

  Gerador.wGrupo('/EnvNfe');
end;

function TNFSeW_WEBFISCO.GerarXml: Boolean;
begin
  Gerador.ListaDeAlertas.Clear;

  Gerador.ArquivoFormatoXML := '';
  Gerador.Prefixo           := FPrefixo4;

  FDefTipos := FServicoEnviar;

  GerarNotas;

  Result := (Gerador.ListaDeAlertas.Count = 0);
end;

function TNFSeW_WEBFISCO.ObterNomeArquivo: String;
begin
  Result := OnlyNumber(NFSe.infID.ID) + '.xml';
end;

end.
