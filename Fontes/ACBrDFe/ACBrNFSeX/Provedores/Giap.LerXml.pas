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

unit Giap.LerXml;

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
  { TNFSeR_Giap }

  TNFSeR_Giap = class(TNFSeRClass)
  protected

    procedure LerDadosPrestador(const ANode: TACBrXmlNode);
    procedure LerDadosServico(const ANode: TACBrXmlNode);
    procedure LerDadosTomador(const ANode: TACBrXmlNode);
    procedure LerDetalheServico(const ANode: TACBrXmlNode);
    procedure LerItem(const ANode: TACBrXmlNode);
  public
    function LerXml: Boolean; override;
    function LerXmlRps(const ANode: TACBrXmlNode): Boolean;
    function LerXmlNfse(const ANode: TACBrXmlNode): Boolean;
  end;

implementation

//==============================================================================
// Essa unit tem por finalidade exclusiva ler o XML do provedor:
//     Giap
//==============================================================================

{ TNFSeR_Giap }

procedure TNFSeR_Giap.LerDadosPrestador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('dadosPrestador');

  if AuxNode <> nil then
  begin
    with NFSe do
    begin
      DataEmissao := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('dataEmissao'), tcDatVcto);
      Competencia := DataEmissao;

      Numero := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('numeroNota'), tcStr);

      with IdentificacaoRps do
      begin
        Numero := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('numeroRps'), tcStr);
        Serie  := '';
        Tipo   := trRPS;
      end;

      Status    := srNormal;
      Cancelada := snNao;

      CodigoVerificacao := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('codigoVerificacao'), tcStr);

      with Prestador.IdentificacaoPrestador do
      begin
        InscricaoMunicipal := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('im'), tcStr);
      end;
    end;
  end;
end;

procedure TNFSeR_Giap.LerDadosServico(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('dadosServico');

  if AuxNode <> nil then
  begin
    with NFSe.Prestador do
    begin
      RazaoSocial                 := '';
      IdentificacaoPrestador.Cnpj := '';

      with Endereco do
      begin
        Endereco    := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('logradouro'), tcStr);
        Numero      := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('numero'), tcStr);
        Bairro      := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('bairro'), tcStr);
        Complemento := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('complemento'), tcStr);
        xMunicipio  := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('cidade'), tcStr);
        UF          := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('uf'), tcStr);
        CEP         := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('cep'), tcStr);
        xPais       := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('pais'), tcStr);

        with Contato do
          Telefone := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('numero'), tcStr);
      end;
    end;
  end;
end;

procedure TNFSeR_Giap.LerDadosTomador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('dadosTomador');

  if AuxNode <> nil then
  begin
    with NFSe do
    begin
      with Tomador do
      begin
        RazaoSocial := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('nomeTomador'), tcStr);

        with IdentificacaoTomador do
        begin
          InscricaoMunicipal := '';
          CpfCnpj            := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('documento'), tcStr);
          InscricaoEstadual  := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('ie'), tcStr);
        end;

        with Endereco do
        begin
          TipoLogradouro  := '';
          Endereco        := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('logradouro'), tcStr);
          Numero          := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('numero'), tcStr);
          Complemento     := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('complemento'), tcStr);
          TipoBairro      := '';
          Bairro          := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('bairro'), tcStr);
          CodigoMunicipio := '0';
          xMunicipio      := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('cidade'), tcStr);
          UF              := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('uf'), tcStr);
          CEP             := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('cep'), tcStr);
        end;

        with Contato do
        begin
          Email    := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('email'), tcStr);
          Telefone := '';
        end;
      end;
    end;
  end;
end;

procedure TNFSeR_Giap.LerDetalheServico(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('detalheServico');

  if AuxNode <> nil then
  begin
    with NFSe.Servico.Valores do
    begin
      Aliquota               := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('Aliquota'), tcDe3);
      ValorPis               := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('pisPasep'), tcDe2);
      ValorCofins            := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('cofins'), tcDe2);
      ValorInss              := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('inss'), tcDe2);
      ValorIr                := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('ir'), tcDe2);
      ValorCsll              := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('csll'), tcDe2);
      ValorIssRetido         := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('issRetido'), tcDe2);
      ValorDeducoes          := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('deducaoMaterial'), tcDe2);
      DescontoIncondicionado := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('descontoIncondicional'), tcDe2);

      if ValorIssRetido > 0 then
        IssRetido := stRetencao
      else
        IssRetido := stNormal;

      AliquotaPIS    := 0;
      AliquotaCOFINS := 0;
      AliquotaINSS   := 0;
      AliquotaIR     := 0;
      AliquotaCSLL   := 0;
    end;

    NFSe.OutrasInformacoes := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('obs'), tcStr);

    LerItem(AuxNode);

    with NFSe.Servico.Valores do
    begin
      ValorIss         := (ValorServicos * Aliquota) / 100;
      ValorLiquidoNfse := ValorServicos -
      (ValorDeducoes + DescontoCondicionado + DescontoIncondicionado +
                                                                ValorIssRetido);
      BaseCalculo      := ValorLiquidoNfse;
    end;
  end;
end;

procedure TNFSeR_Giap.LerItem(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('item');

  if AuxNode <> nil then
  begin
    with NFSe.Servico do
    begin
      CodigoCnae       := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('cnae'), tcStr);
      ItemListaServico := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('codigo'), tcStr);
      Discriminacao    := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('descricao'), tcStr);

      with Valores do
      begin
        Aliquota      := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('aliquota'), tcDe3);
        ValorServicos := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('valor'), tcDe2);
      end;
    end;
  end;
end;

function TNFSeR_Giap.LerXml: Boolean;
var
  XmlNode: TACBrXmlNode;
  xRetorno: string;
begin
  xRetorno := TratarRetorno(Arquivo);

  if EstaVazio(xRetorno) then
    raise Exception.Create('Arquivo xml não carregado.');

  if FDocument = nil then
    FDocument := TACBrXmlDocument.Create();

  Document.Clear();
  Document.LoadFromXml(xRetorno);

  if (Pos('notaFiscal', xRetorno) > 0) then
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

function TNFSeR_Giap.LerXmlNfse(const ANode: TACBrXmlNode): Boolean;
var
  AuxNode: TACBrXmlNode;
begin
  Result := True;

  if not Assigned(ANode) or (ANode = nil) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('notaFiscal');

  if AuxNode = nil then Exit;

  LerDadosPrestador(AuxNode);

  NFSe.InfID.ID := OnlyNumber(NFSe.Numero);

  LerDadosServico(AuxNode);
  LerDadosTomador(AuxNode);
  LerDetalheServico(AuxNode);
end;

function TNFSeR_Giap.LerXmlRps(const ANode: TACBrXmlNode): Boolean;
begin
  Result := LerXmlNfse(ANode);
end;

end.
