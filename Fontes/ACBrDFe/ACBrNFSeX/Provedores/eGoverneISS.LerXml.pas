{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Giurizzato Junior                         }
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

unit eGoverneISS.LerXml;

interface

uses
  SysUtils, Classes, StrUtils,
  ACBrXmlBase, ACBrXmlDocument,
  ACBrNFSeXConversao, ACBrNFSeXLerXml;

type
  { TNFSeR_eGoverneISS }

  TNFSeR_eGoverneISS = class(TNFSeRClass)
  protected

    procedure LerTomador(const ANode: TACBrXmlNode);
    procedure LerEnderecoTomador(const ANode: TACBrXmlNode);
  public
    function LerXml: Boolean; override;
    function LerXmlRps(const ANode: TACBrXmlNode): Boolean;
    function LerXmlNfse(const ANode: TACBrXmlNode): Boolean;
  end;

implementation

uses
  ACBrUtil.Base;

//==============================================================================
// Essa unit tem por finalidade exclusiva ler o XML do provedor:
//     eGoverneISS
//==============================================================================

{ TNFSeR_eGoverneISS }

procedure TNFSeR_eGoverneISS.LerEnderecoTomador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('Endereco');

  if AuxNode <> nil then
  begin
    with NFSe.Tomador.Endereco do
    begin
      Bairro := ObterConteudo(AuxNode.Childrens.FindAnyNs('Bairro'), tcStr);

      CEP := ObterConteudo(AuxNode.Childrens.FindAnyNs('CEP'), tcStr);

      xMunicipio := ObterConteudo(AuxNode.Childrens.FindAnyNs('Cidade'), tcStr);

      Complemento := ObterConteudo(AuxNode.Childrens.FindAnyNs('Complemento'), tcStr);

      UF := ObterConteudo(AuxNode.Childrens.FindAnyNs('Estado'), tcStr);

      Endereco := ObterConteudo(AuxNode.Childrens.FindAnyNs('Logradouro'), tcStr);

      Numero := ObterConteudo(AuxNode.Childrens.FindAnyNs('Numero'), tcStr);

      xPais := ObterConteudo(AuxNode.Childrens.FindAnyNs('Pais'), tcStr);

      TipoLogradouro := ObterConteudo(AuxNode.Childrens.FindAnyNs('TipoLogradouro'), tcStr);
    end;
  end;
end;

procedure TNFSeR_eGoverneISS.LerTomador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  aValor: string;
begin
  AuxNode := ANode.Childrens.FindAnyNs('Tomador');

  if AuxNode <> nil then
  begin
    with NFSe.Tomador do
    begin
      RazaoSocial := ObterConteudo(AuxNode.Childrens.FindAnyNs('Nome'), tcStr);

      IdentificacaoTomador.CpfCnpj := ObterCNPJCPF(AuxNode);

      IdentificacaoTomador.InscricaoMunicipal := ObterConteudo(AuxNode.Childrens.FindAnyNs('InscricaoMunicipal'), tcStr);

      Contato.Email := ObterConteudo(AuxNode.Childrens.FindAnyNs('Email'), tcStr);

      aValor := ObterConteudo(AuxNode.Childrens.FindAnyNs('DDD'), tcStr);

      Contato.Telefone := aValor + ObterConteudo(AuxNode.Childrens.FindAnyNs('Telefone'), tcStr);

      LerEnderecoTomador(AuxNode);
    end;
  end;
end;

function TNFSeR_eGoverneISS.LerXml: Boolean;
var
  XmlNode: TACBrXmlNode;
begin
  FpQuebradeLinha := FpAOwner.ConfigGeral.QuebradeLinha;

  if EstaVazio(Arquivo) then
    raise Exception.Create('Arquivo xml não carregado.');

  LerParamsTabIni(True);

  Arquivo := NormatizarXml(Arquivo);

  if FDocument = nil then
    FDocument := TACBrXmlDocument.Create();

  Document.Clear();
  Document.LoadFromXml(Arquivo);

  if (Pos('eis:NotaFiscal', Arquivo) > 0) then
    tpXML := txmlNFSe
  else
    tpXML := txmlRPS;

  XmlNode := Document.Root;

  if XmlNode = nil then
    raise Exception.Create('Arquivo xml vazio.');

  if tpXML = txmlNFSe then
    Result := LerXmlNfse(XmlNode)
  else
    Result := LerXmlRps(XmlNode);

  if NFSe.Tomador.RazaoSocial = '' then
    NFSe.Tomador.RazaoSocial := 'Tomador Não Identificado';

  FreeAndNil(FDocument);
end;

function TNFSeR_eGoverneISS.LerXmlNfse(const ANode: TACBrXmlNode): Boolean;
//var
//  AuxNode: TACBrXmlNode;
begin
  Result := True;

  if not Assigned(ANode) then Exit;

  // Falta Implementar (Não tem schema para tomar como base)

  (*

  AuxNode := ANode.Childrens.FindAnyNs('notasFiscais');

  if AuxNode = nil then
    AuxNode := ANode.Childrens.FindAnyNs('nfeRpsNotaFiscal');
  *)

  LerCampoLink;
end;

function TNFSeR_eGoverneISS.LerXmlRps(const ANode: TACBrXmlNode): Boolean;
var
  aValor: string;
begin
  Result := True;

  if not Assigned(ANode) then Exit;

  with NFSe do
  begin
    aValor := ObterConteudo(ANode.Childrens.FindAnyNs('Homologacao'), tcStr);

    if aValor = 'true' then
      Producao := snSim
    else
      Producao := snNao;

    OutrasInformacoes := ObterConteudo(ANode.Childrens.FindAnyNs('InformacoesAdicionais'), tcStr);
    OutrasInformacoes := StringReplace(OutrasInformacoes, FpQuebradeLinha,
                                      sLineBreak, [rfReplaceAll, rfIgnoreCase]);

    Servico.CodigoTributacaoMunicipio := ObterConteudo(ANode.Childrens.FindAnyNs('Atividade'), tcStr);

    Servico.Valores.Aliquota := ObterConteudo(ANode.Childrens.FindAnyNs('Aliquota'), tcDe4);

    Servico.Valores.ValorServicos := ObterConteudo(ANode.Childrens.FindAnyNs('Valor'), tcDe2);

    Servico.Valores.ValorCsll := ObterConteudo(ANode.Childrens.FindAnyNs('ValorCSLL'), tcDe2);

    Servico.Valores.ValorCofins := ObterConteudo(ANode.Childrens.FindAnyNs('ValorCOFINS'), tcDe2);

    Servico.Valores.ValorDeducoes := ObterConteudo(ANode.Childrens.FindAnyNs('ValorDeducao'), tcDe2);

    Servico.Valores.ValorInss := ObterConteudo(ANode.Childrens.FindAnyNs('ValorINSS'), tcDe2);

    Servico.Valores.ValorIr := ObterConteudo(ANode.Childrens.FindAnyNs('ValorIR'), tcDe2);

    Servico.Valores.OutrasRetencoes := ObterConteudo(ANode.Childrens.FindAnyNs('ValorOutrosImpostos'), tcDe2);

    Servico.Valores.ValorPis := ObterConteudo(ANode.Childrens.FindAnyNs('ValorPisPasep'), tcDe2);

    Servico.Valores.RetencoesFederais := Servico.Valores.ValorPis + Servico.Valores.ValorCofins +
      Servico.Valores.ValorInss + Servico.Valores.ValorIr + Servico.Valores.ValorCsll;

    Prestador.Endereco.Endereco := ObterConteudo(ANode.Childrens.FindAnyNs('EnderecoPrestacaoServico'), tcStr);

    Prestador.Endereco.CEP := ObterConteudo(ANode.Childrens.FindAnyNs('CEPPrestacaoServico'), tcStr);

    Prestador.Endereco.xMunicipio := ObterConteudo(ANode.Childrens.FindAnyNs('CidadePrestacaoServico'), tcStr);

    Prestador.Endereco.UF := ObterConteudo(ANode.Childrens.FindAnyNs('EstadoPrestacaoServico'), tcStr);

    LerTomador(ANode);

    aValor := ObterConteudo(ANode.Childrens.FindAnyNs('TomadorEstrangeiro'), tcStr);

    if aValor = 'false' then
      Tomador.Endereco.xPais := 'BRASIL';
  end;
end;

end.
