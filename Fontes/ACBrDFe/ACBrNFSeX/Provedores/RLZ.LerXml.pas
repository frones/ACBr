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

unit RLZ.LerXml;

interface

uses
  SysUtils, Classes, StrUtils,
  ACBrXmlBase, ACBrXmlDocument,
  ACBrNFSeXLerXml_ABRASFv2, ACBrNFSeXConversao, ACBrNFSeXLerXml;

type
  { TNFSeR_RLZ }

  TNFSeR_RLZ = class(TNFSeRClass)
  protected
    procedure LerServicos(const ANode: TACBrXmlNode);
    procedure LerPrestador(const ANode: TACBrXmlNode);
    procedure LerTomador(const ANode: TACBrXmlNode);
  public
    function LerXml: Boolean; override;
    function LerXmlNfse(const ANode: TACBrXmlNode): Boolean;
  end;

  { TNFSeR_RLZ203 }

  TNFSeR_RLZ203 = class(TNFSeR_ABRASFv2)
  protected

  public

  end;

implementation

uses
  ACBrUtil.Base, ACBrDFeUtil;

//==============================================================================
// Essa unit tem por finalidade exclusiva ler o XML do provedor:
//     RLZ
//==============================================================================

{ TNFSeR_RLZ }

procedure TNFSeR_RLZ.LerServicos(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  ANodes: TACBrXmlNodeArray;
  i: Integer;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('servicos');

  if AuxNode <> nil then
  begin
    ANodes := AuxNode.Childrens.FindAllAnyNs('servico');

    for i := 0 to Length(ANodes) - 1 do
    begin
      NFSe.Servico.ItemServico.New;
      with NFSe.Servico.ItemServico[i] do
      begin
        Quantidade := ObterConteudo(ANodes[i].Childrens.FindAnyNs('quantidade'), tcDe6);
        Descricao := ObterConteudo(ANodes[i].Childrens.FindAnyNs('atividade'), tcStr);
        Descricao := StringReplace(Descricao, FpQuebradeLinha,
                                      sLineBreak, [rfReplaceAll, rfIgnoreCase]);
        ValorUnitario := ObterConteudo(ANodes[i].Childrens.FindAnyNs('valor'), tcDe2);
        DescontoIncondicionado := ObterConteudo(ANodes[i].Childrens.FindAnyNs('deducao'), tcDe2);
        ItemListaServico := ObterConteudo(ANodes[i].Childrens.FindAnyNs('codigoservico'), tcStr);
        Aliquota := ObterConteudo(ANodes[i].Childrens.FindAnyNs('aliquota'), tcDe2);
        ValorINSS := ObterConteudo(ANodes[i].Childrens.FindAnyNs('inss'), tcDe2);
        ValorTotal := ObterConteudo(ANodes[i].Childrens.FindAnyNs('total'), tcDe2);
      end;
    end;
  end;
end;

procedure TNFSeR_RLZ.LerPrestador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  xUF: string;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('prestador');

  if AuxNode <> nil then
  begin
    with NFSe.Prestador do
    begin
      with Endereco do
      begin
        Endereco := ObterConteudo(AuxNode.Childrens.FindAnyNs('endereco'), tcStr);
        Numero := ObterConteudo(AuxNode.Childrens.FindAnyNs('numero'), tcStr);
        Complemento := ObterConteudo(AuxNode.Childrens.FindAnyNs('complemento'), tcStr);
        Bairro := ObterConteudo(AuxNode.Childrens.FindAnyNs('bairro'), tcStr);
        CEP := ObterConteudo(AuxNode.Childrens.FindAnyNs('cep'), tcStr);
        CodigoMunicipio := ObterConteudo(AuxNode.Childrens.FindAnyNs('cidade'), tcStr);
        UF := ObterConteudo(AuxNode.Childrens.FindAnyNs('uf'), tcStr);
        xMunicipio := ObterNomeMunicipio(StrToIntDef(CodigoMunicipio, 0), xUF, '', False);

        if UF = '' then
          UF := xUF;
      end;

      RazaoSocial := ObterConteudo(AuxNode.Childrens.FindAnyNs('nome'), tcStr);

      with IdentificacaoPrestador do
      begin
        InscricaoMunicipal := ObterConteudo(AuxNode.Childrens.FindAnyNs('inscricao'), tcStr);
        CpfCnpj := ObterConteudo(AuxNode.Childrens.FindAnyNs('cpfcnpj'), tcStr);
      end;
    end;
  end;
end;

procedure TNFSeR_RLZ.LerTomador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  xUF: string;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('tomador');

  if AuxNode <> nil then
  begin
    with NFSe.Tomador do
    begin
      with Endereco do
      begin
        Endereco := ObterConteudo(AuxNode.Childrens.FindAnyNs('endereco'), tcStr);
        Numero := ObterConteudo(AuxNode.Childrens.FindAnyNs('numero'), tcStr);
        Complemento := ObterConteudo(AuxNode.Childrens.FindAnyNs('complemento'), tcStr);
        Bairro := ObterConteudo(AuxNode.Childrens.FindAnyNs('bairro'), tcStr);
        CEP := ObterConteudo(AuxNode.Childrens.FindAnyNs('cep'), tcStr);
        CodigoMunicipio := ObterConteudo(AuxNode.Childrens.FindAnyNs('cidade'), tcStr);
        UF := ObterConteudo(AuxNode.Childrens.FindAnyNs('uf'), tcStr);
        xMunicipio := ObterNomeMunicipio(StrToIntDef(CodigoMunicipio, 0), xUF, '', False);

        if UF = '' then
          UF := xUF;
      end;

      RazaoSocial := ObterConteudo(AuxNode.Childrens.FindAnyNs('nome'), tcStr);

      with IdentificacaoTomador do
      begin
        InscricaoMunicipal := ObterConteudo(AuxNode.Childrens.FindAnyNs('inscricao'), tcStr);
        CpfCnpj := ObterConteudo(AuxNode.Childrens.FindAnyNs('cpfcnpj'), tcStr);
      end;
    end;
  end;
end;

function TNFSeR_RLZ.LerXml: Boolean;
var
  XmlNode: TACBrXmlNode;
begin
  FpQuebradeLinha := FpAOwner.ConfigGeral.QuebradeLinha;

  if EstaVazio(Arquivo) then
    raise Exception.Create('Arquivo xml não carregado.');

  Arquivo := NormatizarXml(Arquivo);

  if FDocument = nil then
    FDocument := TACBrXmlDocument.Create();

  Document.Clear();
  Document.LoadFromXml(Arquivo);

  XmlNode := Document.Root;

  if XmlNode = nil then
    raise Exception.Create('Arquivo xml vazio.');

  Result := LerXmlNfse(XmlNode);

  FreeAndNil(FDocument);
end;

function TNFSeR_RLZ.LerXmlNfse(const ANode: TACBrXmlNode): Boolean;
var
  Ok: Boolean;
begin
  Result := True;

  if not Assigned(ANode) or (ANode = nil) then Exit;

  with NFSe do
  begin
    // <guia>?</guia>
    Numero := ObterConteudo(ANode.Childrens.FindAnyNs('numero'), tcStr);
    // <mes>?</mes>
    // <cidade>?</cidade>
    // <uf>?</uf>
    // <exercicio>?</exercicio>
    DataEmissao := ObterConteudo(ANode.Childrens.FindAnyNs('data'), tcDat);
    // <modelo>?</modelo>
    SeriePrestacao := ObterConteudo(ANode.Childrens.FindAnyNs('serie'), tcStr);
    // <apuracao>?</apuracao>
    // <valor>/valor>
    // <valorimposto>?</valorimposto>
    OptanteSimplesNacional := FpAOwner.StrToSimNao(Ok, ObterConteudo(ANode.Childrens.FindAnyNs('optantesimples'), tcStr));
    // <situacao>?</situacao>
    // <deducao>?</deducao>
    // <basecalculo>?</basecalculo>
    // <retido>?</ retido >
    // <incidencia>?</incidencia>
    Link := ObterConteudo(ANode.Childrens.FindAnyNs('url'), tcStr);
    Link := StringReplace(Link, '&amp;', '&', [rfReplaceAll]);

    LerServicos(ANode);
    LerPrestador(ANode);
    LerTomador(ANode);
  end;
end;

end.
