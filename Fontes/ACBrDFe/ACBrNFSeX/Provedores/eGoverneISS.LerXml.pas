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
{$IFDEF FPC}
  LResources, Controls, Graphics, Dialogs,
{$ENDIF}
  SysUtils, Classes, StrUtils,
  ACBrUtil,
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

//==============================================================================
// Essa unit tem por finalidade exclusiva ler o XML do provedor:
//     eGoverneISS
//==============================================================================

{ TNFSeR_eGoverneISS }

procedure TNFSeR_eGoverneISS.LerEnderecoTomador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.Find('Endereco');

  if AuxNode <> nil then
  begin
    with NFSe.Tomador.Endereco do
    begin
      Bairro := ProcessarConteudo(AuxNode.Childrens.Find('Bairro'), tcStr);

      CEP := ProcessarConteudo(AuxNode.Childrens.Find('CEP'), tcStr);

      xMunicipio := ProcessarConteudo(AuxNode.Childrens.Find('Cidade'), tcStr);

      Complemento := ProcessarConteudo(AuxNode.Childrens.Find('Complemento'), tcStr);

      UF := ProcessarConteudo(AuxNode.Childrens.Find('Estado'), tcStr);

      Endereco := ProcessarConteudo(AuxNode.Childrens.Find('Logradouro'), tcStr);

      Numero := ProcessarConteudo(AuxNode.Childrens.Find('Numero'), tcStr);

      xPais := ProcessarConteudo(AuxNode.Childrens.Find('Pais'), tcStr);

      TipoLogradouro := ProcessarConteudo(AuxNode.Childrens.Find('TipoLogradouro'), tcStr);
    end;
  end;
end;

procedure TNFSeR_eGoverneISS.LerTomador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  aValor: string;
begin
  AuxNode := ANode.Childrens.Find('Tomador');

  if AuxNode <> nil then
  begin
    with NFSe.Tomador do
    begin
      RazaoSocial := ProcessarConteudo(AuxNode.Childrens.Find('Nome'), tcStr);

      with IdentificacaoTomador do
      begin
        aValor := ProcessarConteudo(AuxNode.Childrens.Find('CNPJ'), tcStr);

        if aValor = '' then
          aValor := ProcessarConteudo(AuxNode.Childrens.Find('CPF'), tcStr);

        CpfCnpj := aValor;

        InscricaoMunicipal := ProcessarConteudo(AuxNode.Childrens.Find('InscricaoMunicipal'), tcStr);
      end;

      with Contato do
      begin
        Email := ProcessarConteudo(AuxNode.Childrens.Find('Email'), tcStr);

        aValor := ProcessarConteudo(AuxNode.Childrens.Find('DDD'), tcStr);

        Telefone := aValor + ProcessarConteudo(AuxNode.Childrens.Find('Telefone'), tcStr);
      end;

      LerEnderecoTomador(AuxNode);
    end;
  end;
end;

function TNFSeR_eGoverneISS.LerXml: Boolean;
var
  XmlNode: TACBrXmlNode;
begin
  if EstaVazio(Arquivo) then
    raise Exception.Create('Arquivo xml não carregado.');

  Document.Clear();
  Document.LoadFromXml(Arquivo);

  XmlNode := Document.Root;

  if XmlNode = nil then
    raise Exception.Create('Arquivo xml vazio.');

  if tpXML = txmlNFSe then
    Result := LerXmlNfse(XmlNode)
  else
    Result := LerXmlRps(XmlNode);
end;

function TNFSeR_eGoverneISS.LerXmlNfse(const ANode: TACBrXmlNode): Boolean;
//var
//  AuxNode: TACBrXmlNode;
begin
  Result := True;

  // Falta Implementar (Não tem schema para tomar como base)

  (*

  if not Assigned(ANode) or (ANode = nil) then Exit;

  AuxNode := ANode.Childrens.Find('notasFiscais');

  if AuxNode = nil then
    AuxNode := ANode.Childrens.Find('nfeRpsNotaFiscal');
  *)
end;

function TNFSeR_eGoverneISS.LerXmlRps(const ANode: TACBrXmlNode): Boolean;
var
  aValor: string;
begin
  Result := True;

  if not Assigned(ANode) or (ANode = nil) then Exit;

  with NFSe do
  begin
    aValor := ProcessarConteudo(ANode.Childrens.Find('Homologacao'), tcStr);

    if aValor = 'true' then
      Producao := snSim
    else
      Producao := snNao;

    OutrasInformacoes := ProcessarConteudo(ANode.Childrens.Find('InformacoesAdicionais'), tcStr);

    with Servico do
    begin
      CodigoTributacaoMunicipio := ProcessarConteudo(ANode.Childrens.Find('Atividade'), tcStr);

      with Valores do
      begin
        Aliquota := ProcessarConteudo(ANode.Childrens.Find('Aliquota'), tcDe4);

        ValorServicos := ProcessarConteudo(ANode.Childrens.Find('Valor'), tcDe2);

        ValorCsll := ProcessarConteudo(ANode.Childrens.Find('ValorCSLL'), tcDe2);

        ValorCofins := ProcessarConteudo(ANode.Childrens.Find('ValorCOFINS'), tcDe2);

        ValorDeducoes := ProcessarConteudo(ANode.Childrens.Find('ValorDeducao'), tcDe2);

        ValorInss := ProcessarConteudo(ANode.Childrens.Find('ValorINSS'), tcDe2);

        ValorIr := ProcessarConteudo(ANode.Childrens.Find('ValorIR'), tcDe2);

        OutrasRetencoes := ProcessarConteudo(ANode.Childrens.Find('ValorOutrosImpostos'), tcDe2);

        ValorPis := ProcessarConteudo(ANode.Childrens.Find('ValorPisPasep'), tcDe2);
      end;
    end;

    with Prestador do
    begin
      with Endereco do
      begin
        Endereco := ProcessarConteudo(ANode.Childrens.Find('EnderecoPrestacaoServico'), tcStr);

        CEP := ProcessarConteudo(ANode.Childrens.Find('CEPPrestacaoServico'), tcStr);

        xMunicipio := ProcessarConteudo(ANode.Childrens.Find('CidadePrestacaoServico'), tcStr);

        UF := ProcessarConteudo(ANode.Childrens.Find('EstadoPrestacaoServico'), tcStr);
      end;
    end;

    LerTomador(ANode);

    aValor := ProcessarConteudo(ANode.Childrens.Find('TomadorEstrangeiro'), tcStr);

    if aValor = 'false' then
      Tomador.Endereco.xPais := 'BRASIL';
  end;
end;

end.
