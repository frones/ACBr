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

unit pnfsNFSeW_IPM;

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
  { TNFSeW_IPM }

  TNFSeW_IPM = class(TNFSeWClass)
  private
  protected

    procedure GerarRPS;
    procedure GerarPrestador;
    procedure GerarTomador;
    procedure GerarListaServicos;
    procedure GerarNF;
    procedure GerarXML_IPM;

  public
    constructor Create(ANFSeW: TNFSeW); override;

    function ObterNomeArquivo: String; override;
    function GerarXml: Boolean; override;
  end;

implementation

uses
  ACBrUtil;

{==============================================================================}
{ Essa unit tem por finalidade exclusiva de gerar o XML do RPS segundo o       }
{ layout do IPM.                                                                }
{ Sendo assim só será criado uma nova unit para um novo layout.                }
{==============================================================================}

{ TNFSeW_IPM }

procedure TNFSeW_IPM.GerarRPS;
begin
  Gerador.wGrupoNFSe('rps');
  Gerador.wCampoNFSe(tcStr, '#1', 'nro_recibo_provisorio', 01, 15, 1, OnlyNumber(NFSe.IdentificacaoRps.Numero), DSC_NUMRPS);
  Gerador.wCampoNFSe(tcStr, '#2', 'serie_recibo_provisorio ', 01, 05, 1, NFSe.IdentificacaoRps.Serie, DSC_SERIERPS);
  Gerador.wCampoNFSe(tcStr, '#3', 'data_emissao_recibo_provisorio  ', 01, 10, 1, FormatDateTime('DD/MM/YYYY', NFSe.DataEmissao));
  Gerador.wCampoNFSe(tcStr, '#4', 'hora_emissao_recibo_provisorio  ', 01, 08, 1, FormatDateTime('hh:mm:ss', NFSe.DataEmissao));
  Gerador.wGrupoNFSe('/rps');
end;

procedure TNFSeW_IPM.GerarPrestador;
var
  xMun: String;
begin
  Gerador.wGrupoNFSe('prestador');
  Gerador.wCampoNFSe(tcStr, '#5', 'cpfcnpj'        , 11, 014, 1, OnlyNumber(NFSe.Prestador.Cnpj), '');
  Gerador.wCampoNFSe(tcStr, '#6', 'CodigoMunicipio', 09, 009, 0, OnlyNumber(NFSe.PrestadorServico.Endereco.CodigoMunicipio), '');
  Gerador.wGrupoNFSe('/prestador');
end;

procedure TNFSeW_IPM.GerarTomador;
var
  xMun: String;
begin
  Gerador.wGrupoNFSe('tomador');
  if Length(SomenteNumeros(NFSe.Tomador.IdentificacaoTomador.CpfCnpj)) <= 11 then
    Gerador.wCampoNFSe(tcStr, '#7', 'tipo', 01, 001, 1, 'F', '')
  else
    Gerador.wCampoNFSe(tcStr, '#7', 'tipo', 01, 001, 1, 'J', '');
  Gerador.wCampoNFSe(tcStr, '#8',  'estado', 02, 002, 0, NFSe.Tomador.Endereco.UF, '');
  Gerador.wCampoNFSe(tcStr, '#9',  'pais', 01, 100, 0, NFSe.Tomador.Endereco.xPais, '');
  Gerador.wCampoNFSe(tcStr, '#10', 'cpfcnpj', 11, 014, 1, SomenteNumeros(NFSe.Tomador.IdentificacaoTomador.CpfCnpj), '');
  Gerador.wCampoNFSe(tcStr, '#11', 'ie', 00, 016, 1, SomenteNumeros(NFSe.Tomador.IdentificacaoTomador.InscricaoEstadual), '');
  Gerador.wCampoNFSe(tcStr, '#12', 'nome_razao_social', 01, 100, 0, NFSe.Tomador.RazaoSocial, '');
  Gerador.wCampoNFSe(tcStr, '#13', 'sobrenome_nome_fantasia', 01, 100, 0, NFSe.Tomador.RazaoSocial, '');
  Gerador.wCampoNFSe(tcStr, '#14', 'logradouro', 01, 70, 0, NFSe.Tomador.Endereco.Endereco, '');
  Gerador.wCampoNFSe(tcStr, '#15', 'email', 01, 100, 0, NFSe.Tomador.Contato.Email, '');
  Gerador.wCampoNFSe(tcStr, '#16', 'numero_residencia'     , 01, 008, 0, NFSe.Tomador.Endereco.Numero, '');
  Gerador.wCampoNFSe(tcStr, '#17', 'complemento', 01, 050, 0, NFSe.Tomador.Endereco.Complemento, '');
  Gerador.wCampoNFSe(tcStr, '#18', 'ponto_referencia', 00, 050, 0, '', '');
  Gerador.wCampoNFSe(tcStr, '#19', 'bairro'               , 01, 060, 0, NFSe.Tomador.Endereco.Bairro, '');
  Gerador.wCampoNFSe(tcStr, '#20', 'cidade'      , 07, 009, 0, SomenteNumeros(NFSe.Tomador.Endereco.CodigoMunicipio), '');
  Gerador.wCampoNFSe(tcStr, '#21', 'cep', 08, 008, 0, SomenteNumeros(NFSe.Tomador.Endereco.CEP), '');
  Gerador.wCampoNFSe(tcStr, '#22', 'ddd_fone_comercial', 01, 003, 0, Copy(SomenteNumeros(NFSe.Tomador.Contato.Telefone), 1, 3), '');
  Gerador.wCampoNFSe(tcStr, '#23', 'fone_comercial', 01, 009, 0, Copy(SomenteNumeros(NFSe.Tomador.Contato.Telefone), 3, 9), '');
  Gerador.wCampoNFSe(tcStr, '#24', 'ddd_fone_residencial', 01, 003, 0, Copy(SomenteNumeros(NFSe.Tomador.Contato.Telefone), 1, 3), '');
  Gerador.wCampoNFSe(tcStr, '#25', 'fone_residencial', 01, 009, 0, Copy(SomenteNumeros(NFSe.Tomador.Contato.Telefone), 3, 9), '');
  Gerador.wCampoNFSe(tcStr, '#26', 'ddd_fax', 00, 003, 0, '', '');
  Gerador.wCampoNFSe(tcStr, '#27', 'fone_fax', 00, 009, 0, '', '');
  Gerador.wGrupoNFSe('/tomador');
end;

procedure TNFSeW_IPM.GerarListaServicos;
var
  i: Integer;
begin
  Gerador.wGrupoNFSe('itens');

  //for i := 0 to NFSe.Servico.ItemServico.Count - 1 do
  begin
    Gerador.wGrupoNFSe('lista');
    Gerador.wCampoNFSe(tcStr, '#28', 'tributa_municipio_prestador', 01, 007, 1, 'S', '');
    Gerador.wCampoNFSe(tcStr, '#29', 'codigo_local_prestacao_servico', 01, 009, 1, SomenteNumeros(NFSe.Tomador.Endereco.CodigoMunicipio), '');
    Gerador.wCampoNFSe(tcStr, '#30', 'codigo_item_lista_servico'  , 01, 009, 1, NFSe.Servico.ItemListaServico, '');
    Gerador.wCampoNFSe(tcStr, '#31', 'descritivo'                 , 01, 1000, 1, NFSe.Servico.Discriminacao, '');
    Gerador.wCampoNFSe(tcDe2, '#32', 'aliquota_item_lista_servico', 01, 015, 1, NFSe.Servico.Valores.Aliquota, '');
    Gerador.wCampoNFSe(tcStr, '#33', 'situacao_tributaria'        , 01, 004, 1, '0', '');
    Gerador.wCampoNFSe(tcDe2, '#34', 'valor_tributavel'           , 01, 015, 1, NFSe.Servico.Valores.ValorServicos, ''); //NFSe.Servico.ItemServico[i].ValorServicos, '');
    Gerador.wCampoNFSe(tcDe2, '#35', 'valor_issrf'                , 01, 015, 1, NFSe.Servico.Valores.ValorIssRetido, '');
    Gerador.wCampoNFSe(tcDe2, '#36', 'valor_deducao'              , 01, 015, 1, NFSe.Servico.Valores.ValorDeducoes, '');
    Gerador.wGrupoNFSe('/lista');
  end;

  Gerador.wGrupoNFSe('/itens');
end;

procedure TNFSeW_IPM.GerarNF;
begin
  Gerador.wGrupoNFSe('nf');
  Gerador.wCampoNFSe(tcDe2, '#37', 'valor_total'              , 01, 15, 1, NFSe.Servico.Valores.ValorLiquidoNfse, '');
  Gerador.wCampoNFSe(tcDe2, '#38', 'valor_desconto'           , 01, 15, 1, NFSe.Servico.Valores.ValorDeducoes, '');
  Gerador.wCampoNFSe(tcDe2, '#39', 'valor_ir'                 , 01, 15, 1, NFSe.Servico.Valores.ValorIr, '');
  Gerador.wCampoNFSe(tcDe2, '#40', 'valor_inss'               , 01, 15, 1, NFSe.Servico.Valores.ValorInss, '');
  Gerador.wCampoNFSe(tcDe2, '#41', 'valor_contribuicao_social', 01, 15, 1, NFSe.Servico.Valores.ValorCsll, '');
  Gerador.wCampoNFSe(tcDe2, '#42', 'valor_rps'                , 01, 15, 1, NFSe.Servico.Valores.ValorServicos, '');
  Gerador.wCampoNFSe(tcDe2, '#43', 'valor_pis'                , 01, 15, 1, NFSe.Servico.Valores.ValorPis, '');
  Gerador.wCampoNFSe(tcDe2, '#44', 'valor_cofins'             , 01, 15, 1, NFSe.Servico.Valores.ValorCofins, '');
  Gerador.wCampoNFSe(tcStr, '#45', 'observacao'               , 01, 1000, 1, NFSe.OutrasInformacoes, '');
  Gerador.wGrupoNFSe('/nf');
end;

procedure TNFSeW_IPM.GerarXML_IPM;
begin
  GerarNF;
  GerarPrestador;
  GerarTomador;
  GerarListaServicos;
end;

////////////////////////////////////////////////////////////////////////////////

constructor TNFSeW_IPM.Create(ANFSeW: TNFSeW);
begin
  inherited Create(ANFSeW);

end;

function TNFSeW_IPM.ObterNomeArquivo: String;
begin
  Result := OnlyNumber(NFSe.infID.ID) + '.xml';
end;

function TNFSeW_IPM.GerarXml: Boolean;
var
  Gerar: Boolean;
begin
  Gerador.ListaDeAlertas.Clear;
  Gerador.ArquivoFormatoXML  := '';
  Gerador.Prefixo            := FPrefixo4;
  Gerador.Opcoes.DecimalChar := ',';
  if (RightStr(FURL, 1) <> '/') and (FDefTipos <> '')
    then FDefTipos := '/' + FDefTipos;

  if Trim(FPrefixo4) <> ''
    then Atributo := ' xmlns:' + StringReplace(Prefixo4, ':', '', []) + '="' + FURL + FDefTipos + '"'
    else Atributo := ' xmlns="' + FURL + FDefTipos + '"';

  Gerador.wGrupo('nfse');

  GerarXML_IPM;

  if FOpcoes.GerarTagAssinatura <> taNunca then
  begin
    Gerar := true;
    if FOpcoes.GerarTagAssinatura = taSomenteSeAssinada then
      Gerar := ((NFSe.signature.DigestValue <> '') and
                (NFSe.signature.SignatureValue <> '') and
                (NFSe.signature.X509Certificate <> ''));
    if FOpcoes.GerarTagAssinatura = taSomenteParaNaoAssinada then
      Gerar := ((NFSe.signature.DigestValue = '') and
                (NFSe.signature.SignatureValue = '') and
                (NFSe.signature.X509Certificate = ''));
    if Gerar then
    begin
      FNFSe.signature.URI := FNFSe.InfID.ID;
      FNFSe.signature.Gerador.Opcoes.IdentarXML := Gerador.Opcoes.IdentarXML;
      FNFSe.signature.GerarXMLNFSe;
      Gerador.ArquivoFormatoXML := Gerador.ArquivoFormatoXML +
                                   FNFSe.signature.Gerador.ArquivoFormatoXML;
    end;
  end;

  Gerador.wGrupo('/nfse');

  Gerador.gtAjustarRegistros(NFSe.InfID.ID);
  Result := (Gerador.ListaDeAlertas.Count = 0);
end;

end.
