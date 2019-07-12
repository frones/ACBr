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

unit pnfsNFSeW_AssessorPublico;

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
  { TNFSeW_AssessorPublico }

  TNFSeW_AssesorPublico = class(TNFSeWClass)
  protected
    procedure GerarNotas;
    procedure GerarServicos;
  public
    constructor Create(ANFSeW: TNFSeW); override;

    function ObterNomeArquivo: String; override;
    function GerarXml: Boolean; override;
  end;

implementation

uses
  ACBrUtil, MaskUtils;

{==============================================================================}
{ Essa unit tem por finalidade exclusiva de gerar o XML do RPS segundo o       }
{ layout do provedor Assesor Publico.                                          }
{ Sendo assim só será criado uma nova unit para um novo layout.                }
{==============================================================================}

{ TNFSeW_AssesorPublico }

constructor TNFSeW_AssesorPublico.Create(ANFSeW: TNFSeW);
begin
  inherited Create(ANFSeW);
end;

procedure TNFSeW_AssesorPublico.GerarNotas;
begin
  Gerador.wGrupo('NOTA');

  Gerador.wCampoNFSe(tcStr, '', 'LOTE', 1, 1, 1, NFSe.NumeroLote, '');
  Gerador.wCampoNFSe(tcStr, '', 'SEQUENCIA', 1, 1, 1, NFSe.IdentificacaoRps.Numero, '');
  Gerador.wCampoNFSe(tcStr, '', 'DATAEMISSAO', 1, 1, 1, FormatDateTime('dd/MM/yyyy',NFSe.DataEmissao), '');
  Gerador.wCampoNFSe(tcHor, '', 'HORAEMISSAO', 1, 1, 1, NFSe.DataEmissao, '');

  if NFSe.Servico.CodigoMunicipio = IntToStr(NFSe.Servico.MunicipioIncidencia) then begin
    Gerador.wCampoNFSe(tcStr, '', 'LOCAL', 1, 1, 1, 'D', '');
  end
  else begin
    Gerador.wCampoNFSe(tcStr, '', 'LOCAL', 1, 1, 1, 'F', '');
//    Gerador.wCampoNFSe(tcStr, '', 'UFFORA', 1, 1, 1, '1', '');
//    Gerador.wCampoNFSe(tcStr, '', 'MUNICIPIOFORA', 1, 1, 1, '1', '');
//    Gerador.wCampoNFSe(tcStr, '', 'PAISFORA', 1, 1, 1, '1', '');
  end;


// devem ser previamente cadastrados no sistema
  Gerador.wCampoNFSe(tcStr, '', 'SITUACAO', 1, 1, 1, '1', '');

  if NFSe.Servico.Valores.IssRetido = stRetencao then
    Gerador.wCampoNFSe(tcStr, '', 'RETIDO', 1, 1, 1, 'S', '')
  else
    Gerador.wCampoNFSe(tcStr, '', 'RETIDO', 1, 1, 1, 'N', '');

  Gerador.wCampoNFSe(tcStr, '', 'ATIVIDADE', 1, 1, 1, NFSe.Servico.ItemListaServico, '');
  Gerador.wCampoNFSe(tcDe2, '', 'ALIQUOTAAPLICADA', 1, 1, 1, NFSe.Servico.Valores.Aliquota, '');
  Gerador.wCampoNFSe(tcDe2, '', 'DEDUCAO', 1, 1, 1,  NFSe.Servico.Valores.ValorDeducoes, '');
  Gerador.wCampoNFSe(tcDe2, '', 'IMPOSTO', 1, 1, 1, NFSe.Servico.Valores.valorOutrasRetencoes, '');
  Gerador.wCampoNFSe(tcDe2, '', 'RETENCAO', 1, 1, 1, NFSe.Servico.Valores.ValorIssRetido, '');
  Gerador.wCampoNFSe(tcStr, '', 'OBSERVACAO', 1, 1, 1, NFSe.Servico.Discriminacao, '');
  Gerador.wCampoNFSe(tcStr, '', 'CPFCNPJ', 1, 1, 1, NFSe.Tomador.IdentificacaoTomador.CpfCnpj, '');
  Gerador.wCampoNFSe(tcStr, '', 'RGIE', 1, 1, 1, NFSe.Tomador.IdentificacaoTomador.InscricaoEstadual, '');
  Gerador.wCampoNFSe(tcStr, '', 'NOMERAZAO', 1, 1, 1, NFSe.Tomador.RazaoSocial, '');
  Gerador.wCampoNFSe(tcStr, '', 'NOMEFANTASIA', 1, 1, 1, NFSe.Tomador.RazaoSocial, '');
  Gerador.wCampoNFSe(tcStr, '', 'MUNICIPIO', 1, 1, 1, NFSe.Tomador.Endereco.CodigoMunicipio, '');
  Gerador.wCampoNFSe(tcStr, '', 'BAIRRO', 1, 1, 1, NFSe.Tomador.Endereco.Bairro, '');
  Gerador.wCampoNFSe(tcStr, '', 'CEP', 1, 1, 1, NFSe.Tomador.Endereco.CEP, '');
  Gerador.wCampoNFSe(tcStr, '', 'PREFIXO', 1, 1, 1, 'RUA', '');
  Gerador.wCampoNFSe(tcStr, '', 'LOGRADOURO', 1, 1, 1, NFSe.Tomador.Endereco.Endereco, '');
  Gerador.wCampoNFSe(tcStr, '', 'COMPLEMENTO', 1, 1, 1, NFSe.Tomador.Endereco.Complemento, '');
  Gerador.wCampoNFSe(tcStr, '', 'NUMERO', 1, 1, 1, NFSe.Tomador.Endereco.Numero, '');
//  Gerador.wCampoNFSe(tcStr, '', 'EMAIL', 1, 1, 1, NFSe.Tomador.Contato.Email, '');
  Gerador.wCampoNFSe(tcStr, '', 'DENTROPAIS', 1, 1, 1, 'S', '');
  Gerador.wCampoNFSe(tcDe2, '', 'PIS', 1, 1, 1,NFSe.Servico.Valores.ValorPis, '');
  Gerador.wCampoNFSe(tcDe2, '', 'COFINS', 1, 1, 1, NFSe.Servico.Valores.ValorCofins, '');
  Gerador.wCampoNFSe(tcDe2, '', 'INSS', 1, 1, 1, NFSe.Servico.Valores.ValorInss , '');
  Gerador.wCampoNFSe(tcDe2, '', 'IR', 1, 1, 1, NFSe.Servico.Valores.ValorIr, '');
  Gerador.wCampoNFSe(tcDe2, '', 'CSLL', 1, 1, 1,  NFSe.Servico.Valores.ValorCsll  , '');

  GerarServicos;

  Gerador.wGrupo('/NOTA');
end;

procedure TNFSeW_AssesorPublico.GerarServicos;
var
  i : integer;
begin
  Gerador.wGrupo('SERVICOS');

  for i := 0 to NFSe.Servico.ItemServico.Count - 1 do begin
    Gerador.wGrupo('SERVICO');

    Gerador.wCampoNFSe(tcStr, '', 'DESCRICAO', 1, 1, 1, NFSe.Servico.ItemServico.Items[i].Descricao, '');
    Gerador.wCampoNFSe(tcDe2, '', 'VALORUNIT', 1, 1, 1, NFSe.Servico.ItemServico.Items[i].ValorUnitario, '');
    Gerador.wCampoNFSe(tcDe4, '', 'QUANTIDADE', 1, 1, 1, NFSe.Servico.ItemServico.Items[i].Quantidade, '');

    Gerador.wGrupo('/SERVICO');
  end;

  Gerador.wGrupo('/SERVICOS');
end;

function TNFSeW_AssesorPublico.GerarXml: Boolean;
begin
  Gerador.ListaDeAlertas.Clear;
  Gerador.ArquivoFormatoXML := '';
  Gerador.Prefixo           := FPrefixo4;

  FDefTipos := FServicoEnviar;

  GerarNotas;

  Result := (Gerador.ListaDeAlertas.Count = 0);
end;

function TNFSeW_AssesorPublico.ObterNomeArquivo: String;
begin
  Result := OnlyNumber(NFSe.infID.ID) + '.xml';
end;

end.
