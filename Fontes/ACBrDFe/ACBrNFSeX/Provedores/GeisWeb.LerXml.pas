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
  SysUtils, Classes, StrUtils,
  ACBrUtil,
  ACBrXmlBase, ACBrXmlDocument,
  ACBrNFSeXConversao, ACBrNFSeXLerXml;

type
  { TNFSeR_GeisWeb }

  TNFSeR_GeisWeb = class(TNFSeRClass)
  protected
    function StrToRegime(out ok: boolean; const s: string): TnfseRegimeEspecialTributacao;

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

function TNFSeR_GeisWeb.StrToRegime(out ok: boolean;
  const s: string): TnfseRegimeEspecialTributacao;
begin
  Result := StrToEnumerado(ok, s,
                          ['1', '2', '4', '6'],
                          [retSimplesNacional, retMicroempresarioIndividual,
                           retImune, retOutros]);
end;

procedure TNFSeR_GeisWeb.LerContatoPrestador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('Contato');

  if AuxNode <> nil then
  begin
    with NFSe.Prestador.Contato do
    begin
      Telefone := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('Telefone'), tcStr);
      Email    := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('Email'), tcStr);
    end;
  end;
end;

procedure TNFSeR_GeisWeb.LerEnderecoPrestador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('Endereco');

  if AuxNode <> nil then
  begin
    with NFSe.Prestador.Endereco do
    begin
      Endereco        := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('Rua'), tcStr);
      Numero          := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('Numero'), tcStr);
      Bairro          := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('Bairro'), tcStr);
      CodigoMunicipio := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('Cidade'), tcStr);
      UF              := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('Estado'), tcStr);
      CEP             := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('Cep'), tcStr);
    end;
  end;
end;

procedure TNFSeR_GeisWeb.LerEnderecoTomador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('Endereco');

  if AuxNode <> nil then
  begin
    with NFSe.Tomador.Endereco do
    begin
      Endereco := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('Rua'), tcStr);
      Numero := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('Numero'), tcStr);
      Bairro := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('Bairro'), tcStr);
      CodigoMunicipio := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('Cidade'), tcStr);
      UF := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('Estado'), tcStr);
      CEP := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('Cep'), tcStr);
    end;
  end;
end;

procedure TNFSeR_GeisWeb.LerIdentificacaoNfse(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('IdentificacaoNfse');

  if AuxNode <> nil then
  begin
    with NFSe do
    begin
      numero := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('NumeroNfse'), tcStr);
      CodigoVerificacao := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('CodigoVerificacao'), tcStr);
    end;
  end;
end;

procedure TNFSeR_GeisWeb.LerIdentificacaoPrestador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('IdentificacaoPrestador');

  if AuxNode <> nil then
  begin
    with NFSe.Prestador.IdentificacaoPrestador do
    begin
      Cnpj := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('CnpjCpf'), tcStr);
      InscricaoMunicipal := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('InscricaoMunicipal'), tcStr);
    end;
  end;
end;

procedure TNFSeR_GeisWeb.LerIdentificacaoRps(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('IdentificacaoRps');

  if AuxNode <> nil then
  begin
    with NFSe.IdentificacaoRps do
    begin
      Numero := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('NumeroRps'), tcStr);
    end;
  end;
end;

procedure TNFSeR_GeisWeb.LerIdentificacaoTomador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('IdentificacaoTomador');

  if AuxNode <> nil then
  begin
    with NFSe.Tomador.IdentificacaoTomador do
    begin
      CpfCnpj := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('CnpjCpf'), tcStr);
    end;
  end;
end;

procedure TNFSeR_GeisWeb.LerOrgaoGerador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('OrgaoGerador');

  if AuxNode <> nil then
  begin
    with NFSe.Servico do
    begin
      CodigoMunicipio := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('CodigoMunicipio'), tcStr);
      UFPrestacao := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('Uf'), tcStr);
    end;
  end;
end;

procedure TNFSeR_GeisWeb.LerOutrosImpostos(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('OutrosImpostos');

  if AuxNode <> nil then
  begin
    with NFSe.Servico.Valores do
    begin
      ValorPis := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('Pis'), tcDe2);
      ValorCofins := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('Cofins'), tcDe2);
      ValorCsll := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('Csll'), tcDe2);
      ValorIr := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('Irrf'), tcDe2);
      ValorInss := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('Inss'), tcDe2);
    end;
  end;
end;

procedure TNFSeR_GeisWeb.LerPrestadorServico(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('PrestadorServico');

  if AuxNode <> nil then
  begin
    LerIdentificacaoPrestador(AuxNode);

    with NFSe.Prestador do
    begin
      RazaoSocial := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('RazaoSocial'), tcStr);

      LerEnderecoPrestador(AuxNode);
      LerContatoPrestador(AuxNode);
    end;
  end;
end;

procedure TNFSeR_GeisWeb.LerServico(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('Servico');

  if AuxNode <> nil then
  begin
    with NFSe.Servico do
    begin
      LerValores(AuxNode);

      ItemListaServico := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('CodigoServico'), tcStr);
      Discriminacao := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('Discriminacao'), tcStr);
      CodigoMunicipio := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('MunicipioPrestacaoServico'), tcStr);

//                    <xs:element name="TipoLancamento" type="xs:string"></xs:element>
    end;
  end;
end;

procedure TNFSeR_GeisWeb.LerTomadorServico(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('TomadorServico');

  if AuxNode <> nil then
  begin
    LerIdentificacaoTomador(AuxNode);

    with NFSe.Tomador do
    begin
      RazaoSocial := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('RazaoSocial'), tcStr);
    end;

    LerEnderecoTomador(AuxNode);
  end;
end;

procedure TNFSeR_GeisWeb.LerValores(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('Valores');

  if AuxNode <> nil then
  begin
    with NFSe.Servico.Valores do
    begin
      ValorServicos := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('ValorServicos'), tcDe2);
      BaseCalculo := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('BaseCalculo'), tcDe2);
      Aliquota := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('Aliquota'), tcDe2);
      ValorIss := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('IssDevido'), tcDe2);
      ValorIssRetido := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('IssRetido'), tcDe2);
    end;
  end;
end;

function TNFSeR_GeisWeb.LerXml: Boolean;
var
  XmlNode: TACBrXmlNode;
  xRetorno: string;
begin
  xRetorno := TratarXmlRetorno(Arquivo);
  xRetorno := TiraAcentos(xRetorno);

  if EstaVazio(xRetorno) then
    raise Exception.Create('Arquivo xml não carregado.');

  if FDocument = nil then
    FDocument := TACBrXmlDocument.Create();

  Document.Clear();
  Document.LoadFromXml(xRetorno);

  if (Pos('Nfse', xRetorno) > 0) then
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

function TNFSeR_GeisWeb.LerXmlNfse(const ANode: TACBrXmlNode): Boolean;
var
  Ok: Boolean;
begin
  Result := True;

  with NFSe do
  begin
    LerIdentificacaoNfse(ANode);

    DataEmissao := ProcessarConteudo(ANode.Childrens.FindAnyNs('DataEmissao'), tcDat);
    Competencia := ProcessarConteudo(ANode.Childrens.FindAnyNs('Competencia'), tcDat);
//           <xs:element name="DataLancamento" type="xs:string"></xs:element>
    RegimeEspecialTributacao := StrToRegime(Ok, ProcessarConteudo(ANode.Childrens.FindAnyNs('Regime'), tcStr));

    LerServico(ANode);
    LerPrestadorServico(ANode);
    LerTomadorServico(ANode);
    LerOrgaoGerador(ANode);
    LerOutrosImpostos(ANode);

    Link := ProcessarConteudo(ANode.Childrens.FindAnyNs('LinkConsulta'), tcStr);
  end;
end;

function TNFSeR_GeisWeb.LerXmlRps(const ANode: TACBrXmlNode): Boolean;
begin
  Result := True;

  with NFSe do
  begin
    DataEmissao := ProcessarConteudo(ANode.Childrens.FindAnyNs('DataEmissao'), tcDat);

    LerIdentificacaoRps(ANode);
    LerServico(ANode);
    LerPrestadorServico(ANode);
    LerTomadorServico(ANode);
    LerOrgaoGerador(ANode);
    LerOutrosImpostos(ANode);
  end;
end;

end.
