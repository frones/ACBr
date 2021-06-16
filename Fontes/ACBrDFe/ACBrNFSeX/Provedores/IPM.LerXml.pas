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

unit IPM.LerXml;

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
  { TNFSeR_IPM }

  TNFSeR_IPM = class(TNFSeRClass)
  protected

    procedure LerRps(const ANode: TACBrXmlNode);
    procedure LerNota(const ANode: TACBrXmlNode);
    procedure LerPrestador(const ANode: TACBrXmlNode);
    procedure LerTomador(const ANode: TACBrXmlNode);
    procedure LerItens(const ANode: TACBrXmlNode);
  public
    function LerXml: Boolean; override;
    function LerXmlRps(const ANode: TACBrXmlNode): Boolean;
    function LerXmlNfse(const ANode: TACBrXmlNode): Boolean;
  end;

  { TNFSeR_IPMV110 }

  TNFSeR_IPMV110 = class(TNFSeR_IPM)

  end;

  { TNFSeR_IPMa }

  TNFSeR_IPMa = class(TNFSeR_IPM)
  protected

  end;

implementation

//==============================================================================
// Essa unit tem por finalidade exclusiva ler o XML do provedor:
//     IPM
//==============================================================================

{ TNFSeR_IPM }

procedure TNFSeR_IPM.LerItens(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  ANodes: TACBrXmlNodeArray;
  i: Integer;
  aValor: string;
  Ok: Boolean;
begin
  AuxNode := ANode.Childrens.Find('itens');

  if AuxNode <> nil then
  begin
    with NFSe.Servico do
    begin
      Valores.ValorIssRetido := 0;
      Valores.BaseCalculo    := 0;
      Valores.ValorIss       := 0;

      ANodes := AuxNode.Childrens.FindAll('lista');

      for i := 0 to Length(ANodes) - 1 do
      begin
        aValor := ProcessarConteudo(ANodes[i].Childrens.Find('tributa_municipio_prestador'), tcStr);

        NFSe.NaturezaOperacao := StrToNaturezaOperacao(Ok, aValor);

        CodigoMunicipio           := ProcessarConteudo(ANodes[i].Childrens.Find('codigo_local_prestacao_servico'), tcStr);
        CodigoTributacaoMunicipio := ProcessarConteudo(ANodes[i].Childrens.Find('situacao_tributaria'), tcStr);

        aValor := ProcessarConteudo(ANodes[i].Childrens.Find('situacao_tributaria'), tcStr);
        Valores.IssRetido := StrToSituacaoTributaria(Ok, aValor);

        ItemServico.New;
        with ItemServico[i] do
        begin
          aValor := ProcessarConteudo(ANodes[i].Childrens.Find('codigo_item_lista_servico'), tcStr);
          ItemListaServico := PadLeft(aValor, 4, '0');

          aValor := ProcessarConteudo(ANodes[i].Childrens.Find('unidade_codigo'), tcStr);
          TipoUnidade := StrToUnidade(Ok, aValor);

          Quantidade    := ProcessarConteudo(ANodes[i].Childrens.Find('unidade_quantidade'), tcDe3);
          ValorUnitario := ProcessarConteudo(ANodes[i].Childrens.Find('unidade_valor_unitario'), tcDe2);
          Descricao     := ProcessarConteudo(ANodes[i].Childrens.Find('descritivo'), tcStr);
          Aliquota      := ProcessarConteudo(ANodes[i].Childrens.Find('aliquota_item_lista_servico'), tcDe2);
          ValorTotal    := ProcessarConteudo(ANodes[i].Childrens.Find('valor_tributavel'), tcDe2);
          ValorDeducoes := ProcessarConteudo(ANodes[i].Childrens.Find('valor_deducao'), tcDe2);
          BaseCalculo   := ProcessarConteudo(ANodes[i].Childrens.Find('valor_tributavel'), tcDe2);
          ValorIss      := BaseCalculo * Aliquota / 100;

          Valores.ValorIssRetido := Valores.ValorIssRetido +
              ProcessarConteudo(ANodes[i].Childrens.Find('valor_issrf'), tcDe2);
          Valores.BaseCalculo    := Valores.BaseCalculo + BaseCalculo;
          Valores.ValorIss       := Valores.ValorIss + ValorIss;
        end;
      end;
    end;
  end;
end;

procedure TNFSeR_IPM.LerNota(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  aValor: string;
begin
  AuxNode := ANode.Childrens.Find('nf');

  if AuxNode <> nil then
  begin
    with NFSe do
    begin
      Numero            := ProcessarConteudo(AuxNode.Childrens.Find('numero'), tcStr);
      CodigoVerificacao := ProcessarConteudo(AuxNode.Childrens.Find('codigo_autenticidade'), tcStr);

      // campos presentes ao baixar do site da prefeitura
      if Numero = '' then
      begin
        Numero         := ProcessarConteudo(AuxNode.Childrens.Find('numero_nfse'), tcStr);
        SeriePrestacao := ProcessarConteudo(AuxNode.Childrens.Find('serie_nfse'), tcStr);

        aValor := ProcessarConteudo(AuxNode.Childrens.Find('data_nfse'), tcStr);
        aValor := aValor + ' ' +
                  ProcessarConteudo(AuxNode.Childrens.Find('hora_nfse'), tcStr);

        DataEmissao := StrToDateTimeDef(aValor, 0);
      end;

      aValor := ProcessarConteudo(AuxNode.Childrens.Find('situacao'), tcStr);

      if aValor = 'C' then
      begin
        Status := srCancelado;
        Cancelada := snSim;
      end
      else
      begin
        Status := srNormal;
        Cancelada := snNao;
      end;

      OutrasInformacoes := ProcessarConteudo(AuxNode.Childrens.Find('observacao'), tcStr);

      with Servico.Valores do
      begin
        ValorServicos := ProcessarConteudo(AuxNode.Childrens.Find('valor_total'), tcDe2);
        ValorIr       := ProcessarConteudo(AuxNode.Childrens.Find('valor_ir'), tcDe2);
        ValorInss     := ProcessarConteudo(AuxNode.Childrens.Find('valor_inss'), tcDe2);
        ValorCsll     := ProcessarConteudo(AuxNode.Childrens.Find('valor_contribuicao_social'), tcDe2);
        ValorPis      := ProcessarConteudo(AuxNode.Childrens.Find('valor_pis'), tcDe2);
        ValorCofins   := ProcessarConteudo(AuxNode.Childrens.Find('valor_cofins'), tcDe2);

        DescontoIncondicionado := ProcessarConteudo(AuxNode.Childrens.Find('valor_desconto'), tcDe2);

        ValorLiquidoNfse := ValorServicos -
                            (ValorPis + ValorCofins + ValorInss + ValorIr +
                             ValorCsll + ValorDeducoes + DescontoCondicionado +
                             DescontoIncondicionado + ValorIssRetido);
      end;
    end;
  end;
end;

procedure TNFSeR_IPM.LerPrestador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.Find('prestador');

  if AuxNode <> nil then
  begin
    with NFSe do
    begin

      with Prestador.IdentificacaoPrestador do
      begin
        Cnpj := ProcessarConteudo(AuxNode.Childrens.Find('cpfcnpj'), tcStr);
        Cnpj := PadLeft(Cnpj, 14, '0');
      end;

      with Prestador.Endereco do
      begin
        CodigoMunicipio := ProcessarConteudo(AuxNode.Childrens.Find('cidade'), tcStr);
      end;
    end;
  end;
end;

procedure TNFSeR_IPM.LerRps(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  aValor: string;
begin
  AuxNode := ANode.Childrens.Find('rps');

  if AuxNode <> nil then
  begin
    aValor := ProcessarConteudo(AuxNode.Childrens.Find('data_emissao_recibo_provisorio'), tcStr);
    aValor := aValor + ' ' +
              ProcessarConteudo(AuxNode.Childrens.Find('hora_emissao_recibo_provisorio'), tcStr);

    NFSe.DataEmissao := StrToDateTimeDef(aValor, 0);

    with NFSe.IdentificacaoRps do
    begin
      Numero := ProcessarConteudo(AuxNode.Childrens.Find('nro_recibo_provisorio'), tcStr);
      Serie  := ProcessarConteudo(AuxNode.Childrens.Find('serie_recibo_provisorio'), tcStr);
    end;
  end;
end;

procedure TNFSeR_IPM.LerTomador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  aValor: string;
begin
  AuxNode := ANode.Childrens.Find('tomador');

  if AuxNode <> nil then
  begin
    with NFSe.Tomador do
    begin
      RazaoSocial := ProcessarConteudo(AuxNode.Childrens.Find('nome_razao_social'), tcStr);

      NomeFantasia := ProcessarConteudo(AuxNode.Childrens.Find('sobrenome_nome_fantasia'), tcStr);

      with IdentificacaoTomador do
      begin
        CpfCnpj := ProcessarConteudo(AuxNode.Childrens.Find('cpfcnpj'), tcStr);
        aValor  := ProcessarConteudo(AuxNode.Childrens.Find('tipo'), tcStr);

        if aValor = 'J' then
          CpfCnpj := PadLeft(CpfCnpj, 14, '0')
        else
          CpfCnpj := PadLeft(CpfCnpj, 11, '0');

        InscricaoEstadual := ProcessarConteudo(AuxNode.Childrens.Find('ie'), tcStr);
      end;

      with Endereco do
      begin
        Endereco        := ProcessarConteudo(AuxNode.Childrens.Find('logradouro'), tcStr);
        Numero          := ProcessarConteudo(AuxNode.Childrens.Find('numero_residencia'), tcStr);
        Complemento     := ProcessarConteudo(AuxNode.Childrens.Find('complemento'), tcStr);
        Bairro          := ProcessarConteudo(AuxNode.Childrens.Find('bairro'), tcStr);
        CodigoMunicipio := ProcessarConteudo(AuxNode.Childrens.Find('cidade'), tcStr);
        CEP             := ProcessarConteudo(AuxNode.Childrens.Find('cep'), tcStr);
      end;

      with Contato do
      begin
        aValor := ProcessarConteudo(AuxNode.Childrens.Find('ddd_fone_comercial'), tcStr);
        aValor := aValor +
                  ProcessarConteudo(AuxNode.Childrens.Find('fone_comercial'), tcStr);

        Telefone := aValor;
        Email    := ProcessarConteudo(AuxNode.Childrens.Find('email'), tcStr);
      end;
    end;
  end;
end;

function TNFSeR_IPM.LerXml: Boolean;
var
  XmlNode: TACBrXmlNode;
begin
  if EstaVazio(Arquivo) then
    raise Exception.Create('Arquivo xml não carregado.');

  Document.Clear();
  Document.LoadFromXml(Arquivo);

  if (Pos('nfse', Arquivo) > 0) then
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

function TNFSeR_IPM.LerXmlNfse(const ANode: TACBrXmlNode): Boolean;
var
  AuxNode: TACBrXmlNode;
begin
  Result := True;

  if not Assigned(ANode) or (ANode = nil) then Exit;

  AuxNode := ANode.Childrens.Find('nfse');

  if AuxNode = nil then Exit;

  LerRps(AuxNode);
  LerNota(AuxNode);
  LerPrestador(AuxNode);
  LerTomador(AuxNode);
  LerItens(AuxNode);
end;

function TNFSeR_IPM.LerXmlRps(const ANode: TACBrXmlNode): Boolean;
begin
  Result := LerXmlNfse(ANode);
end;

end.
