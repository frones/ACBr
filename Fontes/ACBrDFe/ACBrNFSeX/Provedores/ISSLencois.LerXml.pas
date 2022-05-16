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

unit ISSLencois.LerXml;

interface

uses
  SysUtils, Classes, StrUtils,
  ACBrXmlBase, ACBrXmlDocument,
  ACBrNFSeXConversao, ACBrNFSeXLerXml;

type
  { Provedor com layout próprio }
  { TNFSeR_ISSLencois }

  TNFSeR_ISSLencois = class(TNFSeRClass)
  protected

    procedure LerPASNF(const ANode: TACBrXmlNode);
    procedure LerTomador(const ANode: TACBrXmlNode);
    procedure LerEnderecoTomador(const ANode: TACBrXmlNode);
    procedure LerRecolhimentoFora(const ANode: TACBrXmlNode);
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
//     Lencois
//==============================================================================

{ TNFSeR_ISSLencois }

procedure TNFSeR_ISSLencois.LerEnderecoTomador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('Endereco');

  if AuxNode <> nil then
  begin
    with NFSe.Tomador.Endereco do
    begin
      Endereco := ObterConteudo(AuxNode.Childrens.FindAnyNs('Logradouro'), tcStr);
      Numero := ObterConteudo(AuxNode.Childrens.FindAnyNs('Numero'), tcStr);
      Complemento := ObterConteudo(AuxNode.Childrens.FindAnyNs('Complemento'), tcStr);
      Bairro := ObterConteudo(AuxNode.Childrens.FindAnyNs('Bairro'), tcStr);
      CodigoMunicipio := ObterConteudo(AuxNode.Childrens.FindAnyNs('Municipio'), tcStr);
    end;
  end;
end;

procedure TNFSeR_ISSLencois.LerPASNF(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('PASNF');

  if AuxNode <> nil then
  begin
    with NFSe do
    begin
      Numero := ObterConteudo(AuxNode.Childrens.FindAnyNs('Numero'), tcStr);
      DataEmissao := ObterConteudo(AuxNode.Childrens.FindAnyNs('Data'), tcDat);
    end;
  end;
end;

procedure TNFSeR_ISSLencois.LerRecolhimentoFora(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  aValor: string;
begin
  AuxNode := ANode.Childrens.FindAnyNs('RecolhimentoFora');

  if AuxNode <> nil then
  begin
    with NFSe.Servico do
    begin
      aValor := ObterConteudo(AuxNode.Childrens.FindAnyNs('Obrigacao'), tcStr);

      if aValor = '1' then
        ResponsavelRetencao := rtTomador
      else
        ResponsavelRetencao := rtPrestador;

      Valores.Aliquota := ObterConteudo(AuxNode.Childrens.FindAnyNs('Aliquota'), tcDe6);
    end;
  end;
end;

procedure TNFSeR_ISSLencois.LerTomador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('Tomador');

  if AuxNode <> nil then
  begin
    with NFSe do
    begin
      with Tomador do
      begin
        with IdentificacaoTomador do
        begin
          CpfCnpj := ObterConteudo(AuxNode.Childrens.FindAnyNs('CPF_CNPJ'), tcStr);
        end;

        RazaoSocial := ObterConteudo(AuxNode.Childrens.FindAnyNs('Nome'), tcStr);

        LerEnderecoTomador(AuxNode);

        with Contato do
        begin
          Email := ObterConteudo(ANode.Childrens.FindAnyNs('Email'), tcStr);
        end;
      end;
    end;
  end;
end;

function TNFSeR_ISSLencois.LerXml: Boolean;
var
  XmlNode: TACBrXmlNode;
begin
  if EstaVazio(Arquivo) then
    raise Exception.Create('Arquivo xml não carregado.');

  Arquivo := NormatizarXml(Arquivo);

  if FDocument = nil then
    FDocument := TACBrXmlDocument.Create();

  Document.Clear();
  Document.LoadFromXml(Arquivo);

  if (Pos('Nota', Arquivo) > 0) then
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

  FreeAndNil(FDocument);
end;

function TNFSeR_ISSLencois.LerXmlNfse(const ANode: TACBrXmlNode): Boolean;
//var
//  AuxNode: TACBrXmlNode;
begin
  Result := False;

  // Falta Implementar (Não tem schema para tomar como base)

  (*
  AuxNode := ANode.Childrens.FindAnyNs('NFe');

  if AuxNode = nil then
    AuxNode := ANode.Childrens.FindAnyNs('CompNfse');

  if AuxNode = nil then Exit;

  *)
end;

function TNFSeR_ISSLencois.LerXmlRps(const ANode: TACBrXmlNode): Boolean;
begin
  Result := True;

  with NFSe do
  begin
    with Prestador do
    begin
      with IdentificacaoPrestador do
      begin
        InscricaoMunicipal := ObterConteudo(ANode.Childrens.FindAnyNs('InscricaoMunicipal'), tcStr);
      end;
    end;

    LerPASNF(ANode);
    LerTomador(ANode);

    with Servico do
    begin
      CodigoMunicipio := ObterConteudo(ANode.Childrens.FindAnyNs('CidadeExecucao'), tcStr);
      Descricao := ObterConteudo(ANode.Childrens.FindAnyNs('Descricao'), tcStr);

      with Valores do
      begin
        ValorServicos := ObterConteudo(ANode.Childrens.FindAnyNs('ValorTotal'), tcDe2);
        ValorDeducoes := ObterConteudo(ANode.Childrens.FindAnyNs('ValorDeducao'), tcDe2);
        Aliquota := ObterConteudo(ANode.Childrens.FindAnyNs('Aliquota'), tcDe6);
        ValorPis := ObterConteudo(ANode.Childrens.FindAnyNs('ValorPIS'), tcDe2);
        ValorCofins := ObterConteudo(ANode.Childrens.FindAnyNs('ValorCOFINS'), tcDe2);
        ValorIr := ObterConteudo(ANode.Childrens.FindAnyNs('RetencaoIRRF'), tcDe2);
        ValorInss := ObterConteudo(ANode.Childrens.FindAnyNs('RetencaoINSS'), tcDe2);
        ValorPis := ObterConteudo(ANode.Childrens.FindAnyNs('RetencaoPIS'), tcDe2);
        ValorCofins := ObterConteudo(ANode.Childrens.FindAnyNs('RetencaoCOFINS'), tcDe2);
        ValorCsll := ObterConteudo(ANode.Childrens.FindAnyNs('RetencaoCSLL'), tcDe2);
      end;
    end;

    LerRecolhimentoFora(ANode);
  end;
end;

end.
