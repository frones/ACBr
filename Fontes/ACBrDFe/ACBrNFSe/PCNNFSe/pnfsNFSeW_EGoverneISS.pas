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

unit pnfsNFSeW_EGoverneISS;

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
  pnfsNFSe, pnfsConversao;

type
  { TNFSeW_EGoverneISS }

  TNFSeW_EGoverneISS = class(TNFSeWClass)
  private
  protected

    procedure GerarPrestador;
    procedure GerarTomador;

    procedure GerarValoresServico;

    procedure GerarXML_EGoverneISS;

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
{ layout do EGoverneISS.                                                       }
{ Sendo assim só será criado uma nova unit para um novo layout.                }
{==============================================================================}

{ TNFSeW_EGoverneISS }

procedure TNFSeW_EGoverneISS.GerarPrestador;
begin
  Gerador.wCampoNFSe(tcStr, '', 'CEPPrestacaoServico',      1, 36, 1, NFSe.PrestadorServico.Endereco.CEP, '');
  Gerador.wCampoNFSe(tcStr, '', 'ChaveAutenticacao',        1, 36, 1, NFSe.Prestador.ChaveAcesso, '');
  Gerador.wCampoNFSe(tcStr, '', 'CidadePrestacaoServico',   1, 36, 1, NFSe.PrestadorServico.Endereco.xMunicipio, '');
  Gerador.wCampoNFSe(tcStr, '', 'EnderecoPrestacaoServico', 1, 36, 1, NFSe.PrestadorServico.Endereco.Endereco, '');
  Gerador.wCampoNFSe(tcStr, '', 'EstadoPrestacaoServico',   1, 36, 1, NFSe.PrestadorServico.Endereco.UF, '');
end;

procedure TNFSeW_EGoverneISS.GerarTomador;
begin
  if NFSE.Tomador.IdentificacaoTomador.CpfCnpj <> '' then
  begin
    Gerador.wGrupoNFSe('Tomador');
    Gerador.Prefixo := 'rgm2:';

    if Length(NFSE.Tomador.IdentificacaoTomador.CpfCnpj) > 11 then
    begin
      Gerador.wCampoNFSe(tcStr, '', 'CNPJ', 01, 14, 1, NFSe.Tomador.IdentificacaoTomador.CpfCnpj, '');
      Gerador.wCampoNFSe(tcStr, '', 'CPF',  01, 14, 1, '', '');
    end
    else
    begin
      Gerador.wCampoNFSe(tcStr, '', 'CNPJ', 01, 14, 1, '', '');
      Gerador.wCampoNFSe(tcStr, '', 'CPF',  01, 14, 1, NFSe.Tomador.IdentificacaoTomador.CpfCnpj, '');
    end;

    if Length(OnlyNumber(NFSe.Tomador.Contato.Telefone)) = 11 then
      Gerador.wCampoNFSe(tcStr, '', 'DDD', 00, 03, 0, LeftStr(OnlyNumber(NFSe.Tomador.Contato.Telefone), 3), '')
    else
      if Length(OnlyNumber(NFSe.Tomador.Contato.Telefone)) = 10 then
        Gerador.wCampoNFSe(tcStr, '', 'DDD', 00, 03, 1, LeftStr(OnlyNumber(NFSe.Tomador.Contato.Telefone), 2), '')
      else
        Gerador.wCampoNFSe(tcStr, '', 'DDD', 00, 03, 1, '', '');

    Gerador.wCampoNFSe(tcStr, '', 'Email', 01, 120, 1, NFSe.Tomador.Contato.Email, '');

    Gerador.wGrupoNFSe('Endereco');
    Gerador.wCampoNFSe(tcStr, '', 'Bairro',         01, 50,  1, NFSe.Tomador.Endereco.Bairro, '');
    Gerador.wCampoNFSe(tcStr, '', 'CEP',            01, 08,  1, OnlyNumber(NFSe.Tomador.Endereco.CEP), '');
    Gerador.wCampoNFSe(tcStr, '', 'Cidade',         01, 50,  1, NFSe.Tomador.Endereco.xMunicipio, '');
    Gerador.wCampoNFSe(tcStr, '', 'Complemento',    01, 30,  0, NFSe.Tomador.Endereco.Complemento, '');
    Gerador.wCampoNFSe(tcStr, '', 'Estado',         01, 08,  1, NFSe.Tomador.Endereco.UF, '');
    Gerador.wCampoNFSe(tcStr, '', 'Logradouro',     01, 50,  1, NFSe.Tomador.Endereco.Endereco, '');
    Gerador.wCampoNFSe(tcStr, '', 'Numero',         01, 09,  1, NFSe.Tomador.Endereco.Numero, '');
    Gerador.wCampoNFSe(tcStr, '', 'Pais',           01, 08,  1, NFSe.Tomador.Endereco.xPais, '');
    Gerador.wCampoNFSe(tcStr, '', 'TipoLogradouro', 00, 10,  1, NFSe.Tomador.Endereco.TipoLogradouro, '');
    Gerador.wGrupoNFSe('/Endereco');

    Gerador.wCampoNFSe(tcStr, '', 'InscricaoMunicipal', 01, 11,  0, NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal, '');
    Gerador.wCampoNFSe(tcStr, '', 'Nome',               01, 120, 1, NFSe.Tomador.RazaoSocial, '');

    Gerador.wCampoNFSe(tcStr, '', 'Telefone', 00, 08, 1, RightStr(OnlyNumber(NFSe.Tomador.Contato.Telefone), 8), '');

    Gerador.Prefixo := 'rgm1:';
    Gerador.wGrupoNFSe('/Tomador');
  end;

  if (Trim(NFSe.Tomador.Endereco.xPais) <> '') and (NFSe.Tomador.Endereco.xPais <> 'BRASIL') then
    Gerador.wCampoNFSe(tcStr, '', 'TomadorEstrangeiro', 05, 05, 1, 'true', '')
  else
    Gerador.wCampoNFSe(tcStr, '', 'TomadorEstrangeiro', 05, 05, 1, 'false', '');
end;

procedure TNFSeW_EGoverneISS.GerarValoresServico;
begin
  Gerador.wCampoNFSe(tcDe2, '', 'Valor',         01, 15, 1, NFSe.Servico.Valores.ValorServicos, '');
  Gerador.wCampoNFSe(tcDe2, '', 'ValorCSLL',     01, 15, 0, NFSe.Servico.Valores.ValorCsll, '');
  Gerador.wCampoNFSe(tcDe2, '', 'ValorCOFINS',   01, 15, 0, NFSe.Servico.Valores.ValorCofins, '');
  Gerador.wCampoNFSe(tcDe2, '', 'ValorDeducao',  01, 15, 1, NFSe.Servico.Valores.ValorDeducoes, '');
  Gerador.wCampoNFSe(tcDe2, '', 'ValorINSS',     01, 15, 0, NFSe.Servico.Valores.ValorInss, '');
  Gerador.wCampoNFSe(tcDe2, '', 'ValorIR',       01, 15, 0, NFSe.Servico.Valores.ValorIr, '');
  Gerador.wCampoNFSe(tcDe2, '', 'ValorOutrosImpostos', 01, 15, 0, NFSe.Servico.Valores.OutrasRetencoes, '');
  Gerador.wCampoNFSe(tcDe2, '', 'ValorPisPasep', 01, 15, 0, NFSe.Servico.Valores.ValorPis, '');
end;

procedure TNFSeW_EGoverneISS.GerarXML_EGoverneISS;
begin
  Gerador.Prefixo := 'rgm:';
  Gerador.wGrupoNFSe('NotaFiscal');
  Gerador.Prefixo := 'rgm1:';
  Gerador.wCampoNFSe(tcDe4, '', 'Aliquota',  1, 15, 1, NFSe.Servico.Valores.Aliquota, '');
  Gerador.wCampoNFSe(tcStr, '', 'Atividade', 1, 09, 1, NFSe.Servico.CodigoTributacaoMunicipio, '');

  GerarPrestador;

  Gerador.wCampoNFSe(tcStr, '', 'Homologacao', 4, 5, 1, ifThen(SimNaoToStr(NFSe.Producao) = '1', 'false', 'true'), '');
  Gerador.wCampoNFSe(tcStr, '', 'InformacoesAdicionais', 0, 2300, 0, NFSe.OutrasInformacoes, '');
  //Gerador.wCampoNFSe(tcStr, '', 'NotificarTomadorPorEmail', 5, 5, 1, 'false', '');
  Gerador.wCampoNFSe(tcStr, '', 'NotificarTomadorPorEmail', 5, 5, 1, ifThen(NFSe.Tomador.Contato.Email = '', 'false', 'true'), '');
  Gerador.wCampoNFSe(tcStr, '', 'SubstituicaoTributaria', 5, 5, 1, 'false', '');

  GerarTomador;

  GerarValoresServico;

  Gerador.Prefixo := 'rgm:';
  Gerador.wGrupoNFSe('/NotaFiscal');
end;

constructor TNFSeW_EGoverneISS.Create(ANFSeW: TNFSeW);
begin
  inherited Create(ANFSeW);

end;

function TNFSeW_EGoverneISS.ObterNomeArquivo: String;
begin
  Result := OnlyNumber(NFSe.infID.ID) + '.xml';
end;

function TNFSeW_EGoverneISS.GerarXml: Boolean;
begin
  Gerador.ListaDeAlertas.Clear;
  Gerador.ArquivoFormatoXML := '';
  Gerador.Prefixo := FPrefixo4;
  FNFSe.InfID.ID := OnlyNumber(FNFSe.IdentificacaoRps.Numero) +
                      FNFSe.IdentificacaoRps.Serie;

  GerarXML_EGoverneISS;

  Gerador.gtAjustarRegistros(NFSe.InfID.ID);
  Result := (Gerador.ListaDeAlertas.Count = 0);
end;

end.
