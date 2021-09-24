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

unit Lencois.LerXml;

interface

uses
{$IFDEF FPC}
  LResources, Controls, Graphics, Dialogs,
{$ENDIF}
  SysUtils, Classes, StrUtils,
  ACBrUtil,
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
      Endereco := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('Logradouro'), tcStr);
      Numero := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('Numero'), tcStr);
      Complemento := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('Complemento'), tcStr);
      Bairro := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('Bairro'), tcStr);
      CodigoMunicipio := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('Municipio'), tcStr);
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
      Numero := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('Numero'), tcStr);
      DataEmissao := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('Data'), tcDat);
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
      aValor := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('Obrigacao'), tcStr);

      if aValor = '1' then
        ResponsavelRetencao := rtTomador
      else
        ResponsavelRetencao := rtPrestador;

      Valores.Aliquota := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('Aliquota'), tcDe6);
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
          CpfCnpj := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('CPF_CNPJ'), tcStr);
        end;

        RazaoSocial := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('Nome'), tcStr);

        LerEnderecoTomador(AuxNode);

        with Contato do
        begin
          Email := ProcessarConteudo(ANode.Childrens.FindAnyNs('Email'), tcStr);
        end;
      end;
    end;
  end;
end;

function TNFSeR_ISSLencois.LerXml: Boolean;
var
  XmlNode: TACBrXmlNode;
  xRetorno: string;
begin
  xRetorno := TratarXmlRetorno(Arquivo);

  if EstaVazio(xRetorno) then
    raise Exception.Create('Arquivo xml não carregado.');

  if FDocument = nil then
    FDocument := TACBrXmlDocument.Create();

  Document.Clear();
  Document.LoadFromXml(xRetorno);

  if (Pos('Nota', xRetorno) > 0) then
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
        InscricaoMunicipal := ProcessarConteudo(ANode.Childrens.FindAnyNs('InscricaoMunicipal'), tcStr);
      end;
    end;

    LerPASNF(ANode);
    LerTomador(ANode);

    with Servico do
    begin
      CodigoMunicipio := ProcessarConteudo(ANode.Childrens.FindAnyNs('CidadeExecucao'), tcStr);
      Descricao := ProcessarConteudo(ANode.Childrens.FindAnyNs('Descricao'), tcStr);

      with Valores do
      begin
        ValorServicos := ProcessarConteudo(ANode.Childrens.FindAnyNs('ValorTotal'), tcDe2);
        ValorDeducoes := ProcessarConteudo(ANode.Childrens.FindAnyNs('ValorDeducao'), tcDe2);
        Aliquota := ProcessarConteudo(ANode.Childrens.FindAnyNs('Aliquota'), tcDe6);
        ValorPis := ProcessarConteudo(ANode.Childrens.FindAnyNs('ValorPIS'), tcDe2);
        ValorCofins := ProcessarConteudo(ANode.Childrens.FindAnyNs('ValorCOFINS'), tcDe2);
        ValorIr := ProcessarConteudo(ANode.Childrens.FindAnyNs('RetencaoIRRF'), tcDe2);
        ValorInss := ProcessarConteudo(ANode.Childrens.FindAnyNs('RetencaoINSS'), tcDe2);
        ValorPis := ProcessarConteudo(ANode.Childrens.FindAnyNs('RetencaoPIS'), tcDe2);
        ValorCofins := ProcessarConteudo(ANode.Childrens.FindAnyNs('RetencaoCOFINS'), tcDe2);
        ValorCsll := ProcessarConteudo(ANode.Childrens.FindAnyNs('RetencaoCSLL'), tcDe2);
      end;
    end;

    LerRecolhimentoFora(ANode);
  end;
end;

end.
