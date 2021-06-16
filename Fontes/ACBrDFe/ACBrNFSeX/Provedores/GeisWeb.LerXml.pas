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

unit GeisWeb.LerXml;

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
  { TNFSeR_GeisWeb }

  TNFSeR_GeisWeb = class(TNFSeRClass)
  protected
    // Leitura da NFS-e
    procedure LerIdentificacaoNfse(const ANode: TACBrXmlNode);
    procedure LerEnderecoPrestador(const ANode: TACBrXmlNode);
    procedure LerContatoPrestador(const ANode: TACBrXmlNode);

    // Leitura do Rps
    procedure LerIdentificacaoRps(const ANode: TACBrXmlNode);
    procedure LerServico(const ANode: TACBrXmlNode);
    procedure LerValores(const ANode: TACBrXmlNode);
    procedure LerPrestadorServico(const ANode: TACBrXmlNode);
    procedure LerIdentificacaoPrestador(const ANode: TACBrXmlNode);
    procedure LerTomadorServico(const ANode: TACBrXmlNode);
    procedure LerIdentificacaoTomador(const ANode: TACBrXmlNode);
    procedure LerEnderecoTomador(const ANode: TACBrXmlNode);
    procedure LerOrgaoGerador(const ANode: TACBrXmlNode);
    procedure LerOutrosImpostos(const ANode: TACBrXmlNode);
  public
    function LerXml: Boolean; override;
    function LerXmlRps(const ANode: TACBrXmlNode): Boolean;
    function LerXmlNfse(const ANode: TACBrXmlNode): Boolean;
  end;

implementation

//==============================================================================
// Essa unit tem por finalidade exclusiva ler o XML do provedor:
//     GeisWeb
//==============================================================================

{ TNFSeR_GeisWeb }

procedure TNFSeR_GeisWeb.LerContatoPrestador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.Find('Contato');

  if AuxNode <> nil then
  begin
    with NFSe.Prestador.Contato do
    begin
      Telefone := ProcessarConteudo(AuxNode.Childrens.Find('Telefone'), tcStr);
      Email    := ProcessarConteudo(AuxNode.Childrens.Find('Email'), tcStr);
    end;
  end;
end;

procedure TNFSeR_GeisWeb.LerEnderecoPrestador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.Find('Endereco');

  if AuxNode <> nil then
  begin
    with NFSe.Prestador.Endereco do
    begin
      Endereco        := ProcessarConteudo(AuxNode.Childrens.Find('Rua'), tcStr);
      Numero          := ProcessarConteudo(AuxNode.Childrens.Find('Numero'), tcStr);
      Bairro          := ProcessarConteudo(AuxNode.Childrens.Find('Bairro'), tcStr);
      CodigoMunicipio := ProcessarConteudo(AuxNode.Childrens.Find('Cidade'), tcStr);
      UF              := ProcessarConteudo(AuxNode.Childrens.Find('Estado'), tcStr);
      CEP             := ProcessarConteudo(AuxNode.Childrens.Find('Cep'), tcStr);
    end;
  end;
end;

procedure TNFSeR_GeisWeb.LerEnderecoTomador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.Find('Endereco');

  if AuxNode <> nil then
  begin
    with NFSe.Tomador.Endereco do
    begin
      Endereco := ProcessarConteudo(AuxNode.Childrens.Find('Rua'), tcStr);
      Numero := ProcessarConteudo(AuxNode.Childrens.Find('Numero'), tcStr);
      Bairro := ProcessarConteudo(AuxNode.Childrens.Find('Bairro'), tcStr);
      CodigoMunicipio := ProcessarConteudo(AuxNode.Childrens.Find('Cidade'), tcStr);
      UF := ProcessarConteudo(AuxNode.Childrens.Find('Estado'), tcStr);
      CEP := ProcessarConteudo(AuxNode.Childrens.Find('Cep'), tcStr);
    end;
  end;
end;

procedure TNFSeR_GeisWeb.LerIdentificacaoNfse(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.Find('IdentificacaoNfse');

  if AuxNode <> nil then
  begin
    with NFSe do
    begin
      numero := ProcessarConteudo(AuxNode.Childrens.Find('NumeroNfse'), tcStr);
      CodigoVerificacao := ProcessarConteudo(AuxNode.Childrens.Find('CodigoVerificacao'), tcStr);
    end;
  end;
end;

procedure TNFSeR_GeisWeb.LerIdentificacaoPrestador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.Find('IdentificacaoPrestador');

  if AuxNode <> nil then
  begin
    with NFSe.Prestador.IdentificacaoPrestador do
    begin
      Cnpj := ProcessarConteudo(AuxNode.Childrens.Find('CnpjCpf'), tcStr);
      InscricaoMunicipal := ProcessarConteudo(AuxNode.Childrens.Find('InscricaoMunicipal'), tcStr);
    end;
  end;
end;

procedure TNFSeR_GeisWeb.LerIdentificacaoRps(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.Find('IdentificacaoRps');

  if AuxNode <> nil then
  begin
    with NFSe.IdentificacaoRps do
    begin
      Numero := ProcessarConteudo(AuxNode.Childrens.Find('NumeroRps'), tcStr);
    end;
  end;
end;

procedure TNFSeR_GeisWeb.LerIdentificacaoTomador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.Find('IdentificacaoTomador');

  if AuxNode <> nil then
  begin
    with NFSe.Tomador.IdentificacaoTomador do
    begin
      CpfCnpj := ProcessarConteudo(AuxNode.Childrens.Find('CnpjCpf'), tcStr);
    end;
  end;
end;

procedure TNFSeR_GeisWeb.LerOrgaoGerador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.Find('OrgaoGerador');

  if AuxNode <> nil then
  begin
    with NFSe.Servico do
    begin
      CodigoMunicipio := ProcessarConteudo(AuxNode.Childrens.Find('CodigoMunicipio'), tcStr);
      UFPrestacao := ProcessarConteudo(AuxNode.Childrens.Find('Uf'), tcStr);
    end;
  end;
end;

procedure TNFSeR_GeisWeb.LerOutrosImpostos(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.Find('OutrosImpostos');

  if AuxNode <> nil then
  begin
    with NFSe.Servico.Valores do
    begin
      ValorPis := ProcessarConteudo(AuxNode.Childrens.Find('Pis'), tcDe2);
      ValorCofins := ProcessarConteudo(AuxNode.Childrens.Find('Cofins'), tcDe2);
      ValorCsll := ProcessarConteudo(AuxNode.Childrens.Find('Csll'), tcDe2);
      ValorIr := ProcessarConteudo(AuxNode.Childrens.Find('Irrf'), tcDe2);
      ValorInss := ProcessarConteudo(AuxNode.Childrens.Find('Inss'), tcDe2);
    end;
  end;
end;

procedure TNFSeR_GeisWeb.LerPrestadorServico(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.Find('PrestadorServico');

  if AuxNode <> nil then
  begin
    LerIdentificacaoPrestador(AuxNode);

    with NFSe.Prestador do
    begin
      RazaoSocial := ProcessarConteudo(AuxNode.Childrens.Find('RazaoSocial'), tcStr);

      LerEnderecoPrestador(AuxNode);
      LerContatoPrestador(AuxNode);
    end;
  end;
end;

procedure TNFSeR_GeisWeb.LerServico(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.Find('Servico');

  if AuxNode <> nil then
  begin
    with NFSe.Servico do
    begin
      LerValores(AuxNode);

      ItemListaServico := ProcessarConteudo(AuxNode.Childrens.Find('CodigoServico'), tcStr);
      Discriminacao := ProcessarConteudo(AuxNode.Childrens.Find('Discriminacao'), tcStr);
      CodigoMunicipio := ProcessarConteudo(AuxNode.Childrens.Find('MunicipioPrestacaoServico'), tcStr);

//                    <xs:element name="TipoLancamento" type="xs:string"></xs:element>
    end;
  end;
end;

procedure TNFSeR_GeisWeb.LerTomadorServico(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.Find('TomadorServico');

  if AuxNode <> nil then
  begin
    LerIdentificacaoTomador(AuxNode);

    with NFSe.Tomador do
    begin
      RazaoSocial := ProcessarConteudo(AuxNode.Childrens.Find('RazaoSocial'), tcStr);
    end;

    LerEnderecoTomador(AuxNode);
  end;
end;

procedure TNFSeR_GeisWeb.LerValores(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.Find('Valores');

  if AuxNode <> nil then
  begin
    with NFSe.Servico.Valores do
    begin
      ValorServicos := ProcessarConteudo(AuxNode.Childrens.Find('ValorServicos'), tcDe2);
      BaseCalculo := ProcessarConteudo(AuxNode.Childrens.Find('BaseCalculo'), tcDe2);
      Aliquota := ProcessarConteudo(AuxNode.Childrens.Find('Aliquota'), tcDe2);
      ValorIss := ProcessarConteudo(AuxNode.Childrens.Find('IssDevido'), tcDe2);
      ValorIssRetido := ProcessarConteudo(AuxNode.Childrens.Find('IssRetido'), tcDe2);
    end;
  end;
end;

function TNFSeR_GeisWeb.LerXml: Boolean;
var
  XmlNode: TACBrXmlNode;
begin
  if EstaVazio(Arquivo) then
    raise Exception.Create('Arquivo xml não carregado.');

  Document.Clear();
  Document.LoadFromXml(Arquivo);

  if (Pos('Nfse', Arquivo) > 0) then
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

function TNFSeR_GeisWeb.LerXmlNfse(const ANode: TACBrXmlNode): Boolean;
begin
  Result := True;

  with NFSe do
  begin
    LerIdentificacaoNfse(ANode);

    DataEmissao := ProcessarConteudo(ANode.Childrens.Find('DataEmissao'), tcDat);
    Competencia := ProcessarConteudo(ANode.Childrens.Find('Competencia'), tcDat);
//           <xs:element name="DataLancamento" type="xs:string"></xs:element>
//           <xs:element name="Regime" type="xs:string"></xs:element>

    LerServico(ANode);
    LerPrestadorServico(ANode);
    LerTomadorServico(ANode);
    LerOrgaoGerador(ANode);
    LerOutrosImpostos(ANode);

    Link := ProcessarConteudo(ANode.Childrens.Find('LinkConsulta'), tcStr);
  end;
end;

function TNFSeR_GeisWeb.LerXmlRps(const ANode: TACBrXmlNode): Boolean;
begin
  Result := True;

  with NFSe do
  begin
    DataEmissao := StrToDate(ProcessarConteudo(ANode.Childrens.Find('DataEmissao'), tcStr));

    LerIdentificacaoRps(ANode);
    LerServico(ANode);
    LerPrestadorServico(ANode);
    LerTomadorServico(ANode);
    LerOrgaoGerador(ANode);
    LerOutrosImpostos(ANode);
  end;
end;

end.
