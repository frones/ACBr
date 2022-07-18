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

unit pnfsNFSeW_EGoverneISS;

interface

uses
{$IFDEF FPC}
  LResources, Controls, Graphics, Dialogs,
{$ELSE}

{$ENDIF}
  SysUtils, Classes, StrUtils,
  ACBrConsts,
  pnfsNFSeW, pcnAuxiliar, pcnConversao, pcnGerador,
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
  ACBrUtil.Strings;

{==============================================================================}
{ Essa unit tem por finalidade exclusiva de gerar o XML do RPS segundo o       }
{ layout do EGoverneISS.                                                       }
{ Sendo assim só será criado uma nova unit para um novo layout.                }
{==============================================================================}

{ TNFSeW_EGoverneISS }

procedure TNFSeW_EGoverneISS.GerarPrestador;
begin
  Gerador.wCampo(tcStr, '', 'CEPPrestacaoServico',      1, 36, 1, NFSe.PrestadorServico.Endereco.CEP, '');
  Gerador.wCampo(tcStr, '', 'ChaveAutenticacao',        1, 36, 1, NFSe.Prestador.ChaveAcesso, '');
  Gerador.wCampo(tcStr, '', 'CidadePrestacaoServico',   1, 36, 1, NFSe.PrestadorServico.Endereco.xMunicipio, '');
  Gerador.wCampo(tcStr, '', 'EnderecoPrestacaoServico', 1, 36, 1, NFSe.PrestadorServico.Endereco.Endereco, '');
  Gerador.wCampo(tcStr, '', 'EstadoPrestacaoServico',   1, 36, 1, NFSe.PrestadorServico.Endereco.UF, '');
end;

procedure TNFSeW_EGoverneISS.GerarTomador;
begin
  if NFSE.Tomador.IdentificacaoTomador.CpfCnpj <> '' then
  begin
    Gerador.wGrupo('Tomador');
    Gerador.Prefixo := 'rgm2:';

    if Length(NFSE.Tomador.IdentificacaoTomador.CpfCnpj) > 11 then
    begin
      Gerador.wCampo(tcStr, '', 'CNPJ', 01, 14, 1, NFSe.Tomador.IdentificacaoTomador.CpfCnpj, '');
      Gerador.wCampo(tcStr, '', 'CPF',  01, 14, 1, '', '');
    end
    else
    begin
      Gerador.wCampo(tcStr, '', 'CNPJ', 01, 14, 1, '', '');
      Gerador.wCampo(tcStr, '', 'CPF',  01, 14, 1, NFSe.Tomador.IdentificacaoTomador.CpfCnpj, '');
    end;

    if Length(OnlyNumber(NFSe.Tomador.Contato.Telefone)) = 11 then
      Gerador.wCampo(tcStr, '', 'DDD', 00, 03, 0, LeftStr(OnlyNumber(NFSe.Tomador.Contato.Telefone), 3), '')
    else
      if Length(OnlyNumber(NFSe.Tomador.Contato.Telefone)) = 10 then
        Gerador.wCampo(tcStr, '', 'DDD', 00, 03, 1, LeftStr(OnlyNumber(NFSe.Tomador.Contato.Telefone), 2), '')
      else
        Gerador.wCampo(tcStr, '', 'DDD', 00, 03, 1, '', '');

    Gerador.wCampo(tcStr, '', 'Email', 01, 120, 1, NFSe.Tomador.Contato.Email, '');

    Gerador.wGrupo('Endereco');
    Gerador.wCampo(tcStr, '', 'Bairro',         01, 50,  1, NFSe.Tomador.Endereco.Bairro, '');
    Gerador.wCampo(tcStr, '', 'CEP',            01, 08,  1, OnlyNumber(NFSe.Tomador.Endereco.CEP), '');
    Gerador.wCampo(tcStr, '', 'Cidade',         01, 50,  1, NFSe.Tomador.Endereco.xMunicipio, '');
    Gerador.wCampo(tcStr, '', 'Complemento',    01, 30,  0, NFSe.Tomador.Endereco.Complemento, '');
    Gerador.wCampo(tcStr, '', 'Estado',         01, 08,  1, NFSe.Tomador.Endereco.UF, '');
    Gerador.wCampo(tcStr, '', 'Logradouro',     01, 50,  1, NFSe.Tomador.Endereco.Endereco, '');
    Gerador.wCampo(tcStr, '', 'Numero',         01, 09,  1, NFSe.Tomador.Endereco.Numero, '');
    Gerador.wCampo(tcStr, '', 'Pais',           01, 08,  1, NFSe.Tomador.Endereco.xPais, '');
    Gerador.wCampo(tcStr, '', 'TipoLogradouro', 00, 10,  1, NFSe.Tomador.Endereco.TipoLogradouro, '');
    Gerador.wGrupo('/Endereco');

    Gerador.wCampo(tcStr, '', 'InscricaoMunicipal', 01, 11,  0, NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal, '');
    Gerador.wCampo(tcStr, '', 'Nome',               01, 120, 1, NFSe.Tomador.RazaoSocial, '');

    Gerador.wCampo(tcStr, '', 'Telefone', 00, 08, 1, RightStr(OnlyNumber(NFSe.Tomador.Contato.Telefone), 8), '');

    Gerador.Prefixo := 'rgm1:';
    Gerador.wGrupo('/Tomador');
  end;

  if (Trim(NFSe.Tomador.Endereco.xPais) <> '') and (NFSe.Tomador.Endereco.xPais <> 'BRASIL') then
    Gerador.wCampo(tcStr, '', 'TomadorEstrangeiro', 05, 05, 1, 'true', '')
  else
    Gerador.wCampo(tcStr, '', 'TomadorEstrangeiro', 05, 05, 1, 'false', '');
end;

procedure TNFSeW_EGoverneISS.GerarValoresServico;
begin
  Gerador.wCampo(tcDe2, '', 'Valor',         01, 15, 1, NFSe.Servico.Valores.ValorServicos, '');
  Gerador.wCampo(tcDe2, '', 'ValorCSLL',     01, 15, 0, NFSe.Servico.Valores.ValorCsll, '');
  Gerador.wCampo(tcDe2, '', 'ValorCOFINS',   01, 15, 0, NFSe.Servico.Valores.ValorCofins, '');
  Gerador.wCampo(tcDe2, '', 'ValorDeducao',  01, 15, 1, NFSe.Servico.Valores.ValorDeducoes, '');
  Gerador.wCampo(tcDe2, '', 'ValorINSS',     01, 15, 0, NFSe.Servico.Valores.ValorInss, '');
  Gerador.wCampo(tcDe2, '', 'ValorIR',       01, 15, 0, NFSe.Servico.Valores.ValorIr, '');
  Gerador.wCampo(tcDe2, '', 'ValorOutrosImpostos', 01, 15, 0, NFSe.Servico.Valores.OutrasRetencoes, '');
  Gerador.wCampo(tcDe2, '', 'ValorPisPasep', 01, 15, 0, NFSe.Servico.Valores.ValorPis, '');
end;

procedure TNFSeW_EGoverneISS.GerarXML_EGoverneISS;
begin
  Gerador.Prefixo := 'rgm:';
  Gerador.wGrupo('NotaFiscal');
  Gerador.Prefixo := 'rgm1:';
  Gerador.wCampo(tcDe4, '', 'Aliquota',  1, 15, 1, NFSe.Servico.Valores.Aliquota, '');
  Gerador.wCampo(tcStr, '', 'Atividade', 1, 09, 1, NFSe.Servico.CodigoTributacaoMunicipio, '');

  GerarPrestador;

  Gerador.wCampo(tcStr, '', 'Homologacao', 4, 5, 1, ifThen(SimNaoToStr(NFSe.Producao) = '1', 'false', 'true'), '');
  Gerador.wCampo(tcStr, '', 'InformacoesAdicionais', 0, 2300, 0, NFSe.OutrasInformacoes, '');
  //Gerador.wCampo(tcStr, '', 'NotificarTomadorPorEmail', 5, 5, 1, 'false', '');
  Gerador.wCampo(tcStr, '', 'NotificarTomadorPorEmail', 5, 5, 1, ifThen(NFSe.Tomador.Contato.Email = '', 'false', 'true'), '');
  Gerador.wCampo(tcStr, '', 'SubstituicaoTributaria', 5, 5, 1, 'false', '');

  GerarTomador;

  GerarValoresServico;

  Gerador.Prefixo := 'rgm:';
  Gerador.wGrupo('/NotaFiscal');
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
  Gerador.Prefixo           := FPrefixo4;

  Gerador.Opcoes.QuebraLinha := FQuebradeLinha;

  FNFSe.InfID.ID := OnlyNumber(FNFSe.IdentificacaoRps.Numero) +
                      FNFSe.IdentificacaoRps.Serie;

  GerarXML_EGoverneISS;

  Gerador.gtAjustarRegistros(NFSe.InfID.ID);
  Result := (Gerador.ListaDeAlertas.Count = 0);
end;

end.
