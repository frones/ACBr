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

unit SmarAPD.LerXml;

interface

uses
{$IFDEF FPC}
  LResources, Controls, Graphics, Dialogs,
{$ENDIF}
  SysUtils, Classes, StrUtils,
  ACBrUtil,
  ACBrXmlBase, ACBrXmlDocument,
  ACBrNFSeXConversao, ACBrNFSeXLerXml, ACBrNFSeXLerXml_ABRASFv2;

type
  { TNFSeR_SmarAPD }

  TNFSeR_SmarAPD = class(TNFSeRClass)
  protected

    procedure LerItens(const ANode: TACBrXmlNode);
    procedure LerFatura(const ANode: TACBrXmlNode);
    procedure LerFaturas(const ANode: TACBrXmlNode);
    procedure LerServico(const ANode: TACBrXmlNode);
    procedure LerServicos(const ANode: TACBrXmlNode);
  public
    function LerXml: Boolean; override;
    function LerXmlRps(const ANode: TACBrXmlNode): Boolean;
    function LerXmlNfse(const ANode: TACBrXmlNode): Boolean;
  end;

  { TNFSeR_SmarAPDv203 }

  TNFSeR_SmarAPDv203 = class(TNFSeR_ABRASFv2)
  protected

  public

  end;

  { TNFSeR_SmarAPDv204 }

  TNFSeR_SmarAPDv204 = class(TNFSeR_ABRASFv2)
  protected

  public

  end;

implementation

//==============================================================================
// Essa unit tem por finalidade exclusiva ler o XML do provedor:
//     SmarAPD
//==============================================================================

{ TNFSeR_SmarAPD }

procedure TNFSeR_SmarAPD.LerFatura(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.Find('tbfatura');

  if AuxNode = nil then Exit;

  LerFaturas(AuxNode);
end;

procedure TNFSeR_SmarAPD.LerFaturas(const ANode: TACBrXmlNode);
var
  ANodes: TACBrXmlNodeArray;
  i: Integer;
begin
  ANodes := ANode.Childrens.FindAll('fatura');

  for i := 0 to Length(ANodes) - 1 do
  begin
    NFSe.CondicaoPagamento.Parcelas.New;

    with NFSe.CondicaoPagamento.Parcelas[i] do
    begin
      Parcela := ProcessarConteudo(ANodes[i].Childrens.Find('numfatura'), tcInt);

      DataVencimento := ProcessarConteudo(ANodes[i].Childrens.Find('vencimentofatura'), tcDatVcto);

      Valor := ProcessarConteudo(ANodes[i].Childrens.Find('valorfatura'), tcDe2);
    end;
  end;
end;

procedure TNFSeR_SmarAPD.LerItens(const ANode: TACBrXmlNode);
var
  ANodes: TACBrXmlNodeArray;
  i: Integer;
begin
  ANodes := ANode.Childrens.FindAll('ITENS');

  for i := 0 to Length(ANodes) - 1 do
  begin
    NFSe.Servico.ItemServico.New;

    with NFSe.Servico.ItemServico[i] do
    begin
      if NFSe.Servico.Discriminacao = '' then
        NFSe.Servico.Discriminacao := ProcessarConteudo(ANodes[i].Childrens.Find('Servico'), tcStr);

      Descricao     := ProcessarConteudo(ANodes[i].Childrens.Find('Servico'), tcStr);

      Quantidade    := ProcessarConteudo(ANodes[i].Childrens.Find('Quantidade'), tcDe2);

      ValorUnitario := ProcessarConteudo(ANodes[i].Childrens.Find('ValorUnitario'), tcDe2);

      ValorTotal    := ProcessarConteudo(ANodes[i].Childrens.Find('ValorTotal'), tcDe2);

      Aliquota      := ProcessarConteudo(ANodes[i].Childrens.Find('Aliquota'), tcDe2);

      Tributavel := snSim;

      NFSe.Servico.Valores.ValorServicos := (NFSe.Servico.Valores.ValorServicos +
                                                                  ValorTotal);
    end;
  end;
end;

procedure TNFSeR_SmarAPD.LerServico(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.Find('tbservico');

  if AuxNode = nil then Exit;

  LerServicos(AuxNode);
end;

procedure TNFSeR_SmarAPD.LerServicos(const ANode: TACBrXmlNode);
var
  ANodes: TACBrXmlNodeArray;
  i: Integer;
  aValor: string;
begin
  ANodes := ANode.Childrens.FindAll('servico');

  for i := 0 to Length(ANodes) - 1 do
  begin
    NFSe.Servico.ItemServico.New;

    with NFSe.Servico.ItemServico[i] do
    begin
      Quantidade := ProcessarConteudo(ANodes[i].Childrens.Find('quantidade'), tcDe2);

      Descricao := ProcessarConteudo(ANodes[i].Childrens.Find('descricao'), tcStr);

      CodServ := ProcessarConteudo(ANodes[i].Childrens.Find('codatividade'), tcStr);

      ValorUnitario := ProcessarConteudo(ANodes[i].Childrens.Find('valorunitario'), tcDe2);

      Aliquota := ProcessarConteudo(ANodes[i].Childrens.Find('aliquota'), tcDe2);

      aValor := ProcessarConteudo(ANodes[i].Childrens.Find('impostoretido'), tcStr);

      if aValor = 'True' then
        NFSe.Servico.Valores.IssRetido := stRetencao
      else
        NFSe.Servico.Valores.IssRetido := stNormal;
    end;
  end;
end;

function TNFSeR_SmarAPD.LerXml: Boolean;
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

function TNFSeR_SmarAPD.LerXmlNfse(const ANode: TACBrXmlNode): Boolean;
var
  AuxNode: TACBrXmlNode;
  aValor: string;
  Ok: Boolean;
begin
  Result := True;

  if not Assigned(ANode) or (ANode = nil) then Exit;

  AuxNode := ANode.Childrens.Find('NFe');

  if AuxNode = nil then
    AuxNode := ANode.Childrens.Find('CompNfse');

  if AuxNode = nil then Exit;

  with NFSe do
  begin
    Numero            := ProcessarConteudo(AuxNode.Childrens.Find('NumeroNota'), tcStr);
    CodigoVerificacao := ProcessarConteudo(AuxNode.Childrens.Find('ChaveValidacao'), tcStr);
    DataEmissaoRps    := ProcessarConteudo(AuxNode.Childrens.Find('DataEmissao'), tcDatHor);
    Competencia       := DataEmissaoRps;
    DataEmissao       := DataEmissaoRps;
    dhRecebimento     := DataEmissaoRps;

    aValor := ProcessarConteudo(AuxNode.Childrens.Find('NaturezaOperacao'), tcStr);
    NaturezaOperacao := StrToNaturezaOperacao(Ok, aValor);

    Protocolo         := ProcessarConteudo(AuxNode.Childrens.Find('ChaveValidacao'), tcStr);
    OutrasInformacoes := ProcessarConteudo(AuxNode.Childrens.Find('Observacao'), tcStr);

    MotivoCancelamento           := '';
    IntermediarioServico.CpfCnpj := '';

    aValor := ProcessarConteudo(AuxNode.Childrens.Find('SituacaoNf'), tcStr);

    if aValor = 'Cancelada' then
    begin
      Status    := srCancelado;
      Cancelada := snSim;
      NfseCancelamento.DataHora := DataEmissao;
    end
    else
    begin
      Status    := srNormal;
      Cancelada := snNao;
    end;

    IdentificacaoRps.Numero := ProcessarConteudo(AuxNode.Childrens.Find('NumeroRps'), tcStr);
    IdentificacaoRps.Tipo   := trRPS;
    InfID.ID                := OnlyNumber(NFSe.Numero);

    with Prestador do
    begin
      RazaoSocial := ProcessarConteudo(AuxNode.Childrens.Find('TimbreContribuinteLinha1'), tcStr);

      with Endereco do
      begin
        aValor := ProcessarConteudo(AuxNode.Childrens.Find('TimbreContribuinteLinha2'), tcStr);
        Endereco := Trim(copy(aValor, 1, pos(',', aValor) -1));
        Numero   := Trim(copy(aValor, pos(',', aValor) +1,
                                     (pos('-', aValor) - pos(',', aValor)) -1));
        Bairro   := Trim(copy(aValor, pos('-', aValor) +1, length(aValor) -1));
        Complemento := '';
        aValor := ProcessarConteudo(AuxNode.Childrens.Find('TimbreContribuinteLinha3'), tcStr);
        aValor := Trim(copy(aValor, pos('-', aValor) +1, length(aValor) -1));
        xMunicipio := Trim(copy(aValor, 1, pos('-', aValor) -1));
        UF         := Trim(copy(aValor, pos('-', aValor) +1, length(aValor) -1));

        with Contato do
        begin
          Email    := '';
          Telefone := ProcessarConteudo(AuxNode.Childrens.Find('TelefoneTomador'), tcStr);
        end;
      end;

      aValor := ProcessarConteudo(AuxNode.Childrens.Find('TimbreContribuinteLinha4'), tcStr);

      with IdentificacaoPrestador do
      begin
        InscricaoMunicipal := Trim(copy(aValor, 23, (pos('CPF/CNPJ:', aValor) -24)));
        Cnpj               := Trim(copy(aValor, pos('CPF/CNPJ:', aValor) +10,
                                                            length(aValor) -1));
      end;
    end;

    with Tomador do
    begin
      RazaoSocial := ProcessarConteudo(AuxNode.Childrens.Find('ClienteNomeRazaoSocial'), tcStr);

      with IdentificacaoTomador do
      begin
       InscricaoMunicipal := ProcessarConteudo(AuxNode.Childrens.Find('ClienteInscricaoMunicipal'), tcStr);
       CpfCnpj            := ProcessarConteudo(AuxNode.Childrens.Find('ClienteCNPJCPF'), tcStr);
      end;

      with Endereco do
      begin
        TipoLogradouro  := '';
        Endereco        := ProcessarConteudo(AuxNode.Childrens.Find('ClienteEndereco'), tcStr);
        Numero          := ProcessarConteudo(AuxNode.Childrens.Find('ClienteNumeroLogradouro'), tcStr);
        Complemento     := '';
        TipoBairro      := '';
        Bairro          := ProcessarConteudo(AuxNode.Childrens.Find('ClienteBairro'), tcStr);
        CodigoMunicipio := '0';
        xMunicipio      := ProcessarConteudo(AuxNode.Childrens.Find('ClienteCidade'), tcStr);
        UF              := ProcessarConteudo(AuxNode.Childrens.Find('ClienteUF'), tcStr);
        CEP             := ProcessarConteudo(AuxNode.Childrens.Find('ClienteCEP'), tcStr);
      end;

      with Contato do
      begin
        Email    := ProcessarConteudo(AuxNode.Childrens.Find('ClienteEmail'), tcStr);
        Telefone := ProcessarConteudo(AuxNode.Childrens.Find('ClienteFone'), tcStr);
      end;
    end;

    with Servico do
    begin
      CodigoCnae := ProcessarConteudo(AuxNode.Childrens.Find('CodigoAtividade'), tcStr);
      CodigoMunicipio := '';

      with Valores do
      begin
        Aliquota := ProcessarConteudo(AuxNode.Childrens.Find('Aliquota'), tcDe3);

        aValor := ProcessarConteudo(AuxNode.Childrens.Find('ImpostoRetido'), tcStr);

        if aValor = 'true' then
        begin
          IssRetido := stRetencao;
          ValorIssRetido := ProcessarConteudo(AuxNode.Childrens.Find('ISSQNCliente'), tcDe2);
        end
        else
          IssRetido := stNormal;

        ValorPis       := ProcessarConteudo(AuxNode.Childrens.Find('Pis'), tcDe2);
        ValorCofins    := ProcessarConteudo(AuxNode.Childrens.Find('Cofins'), tcDe2);
        ValorInss      := ProcessarConteudo(AuxNode.Childrens.Find('Inss'), tcDe2);
        ValorIr        := ProcessarConteudo(AuxNode.Childrens.Find('Irrf'), tcDe2);
        ValorCsll      := ProcessarConteudo(AuxNode.Childrens.Find('Csll'), tcDe2);
        AliquotaPIS    := 0;
        AliquotaCOFINS := 0;
        AliquotaINSS   := 0;
        AliquotaIR     := 0;
        AliquotaCSLL   := 0;
      end;

      Discriminacao             := '';
      CodigoTributacaoMunicipio := ProcessarConteudo(AuxNode.Childrens.Find('CodigoAtividade'), tcStr);
    end;

    LerItens(AuxNode);

    with Servico.Valores do
    begin
      ValorIss         := (ValorServicos * Aliquota) /100;
      ValorLiquidoNfse := ValorServicos -
                                      (ValorDeducoes + DescontoCondicionado +
                                       DescontoIncondicionado + ValorIssRetido);
      BaseCalculo      := ValorLiquidoNfse;
    end;
  end;
end;

function TNFSeR_SmarAPD.LerXmlRps(const ANode: TACBrXmlNode): Boolean;
var
  aValor: string;
begin
  Result := True;

  with NFSe do
  begin
    with IdentificacaoRps do
    begin
      Serie := ProcessarConteudo(ANode.Childrens.Find('codseriedocumento'), tcStr);
      Numero := ProcessarConteudo(ANode.Childrens.Find('numerort'), tcStr);
    end;

    DataEmissaoRps := ProcessarConteudo(ANode.Childrens.Find('dataemissaort'), tcDatVcto);

    Competencia := ProcessarConteudo(ANode.Childrens.Find('fatorgerador'), tcDatVcto);

    NaturezaOperacao := ProcessarConteudo(ANode.Childrens.Find('codnaturezaoperacao'), tcStr);

    with Prestador do
    begin
      with IdentificacaoPrestador do
      begin
        InscricaoMunicipal := ProcessarConteudo(ANode.Childrens.Find('inscricaomunicipalemissor'), tcStr);
      end;
    end;

    DataEmissao := ProcessarConteudo(ANode.Childrens.Find('dataemissao'), tcDatVcto);

    with Tomador do
    begin
      RazaoSocial := ProcessarConteudo(ANode.Childrens.Find('razaotomador'), tcStr);

      NomeFantasia := ProcessarConteudo(ANode.Childrens.Find('nomefantasiatomador'), tcStr);

      with IdentificacaoTomador do
      begin
        CpfCnpj := ProcessarConteudo(ANode.Childrens.Find('cpfcnpjtomador'), tcStr);

        InscricaoEstadual := ProcessarConteudo(ANode.Childrens.Find('inscricaoestadualtomador'), tcStr);

        InscricaoMunicipal := ProcessarConteudo(ANode.Childrens.Find('inscricaomunicipaltomador'), tcStr);
      end;

      with Endereco do
      begin
        aValor := ProcessarConteudo(ANode.Childrens.Find('tppessoa'), tcStr);

        if aValor = 'O' then
          CodigoMunicipio := '9999999'
        else
          CodigoMunicipio := '0'; // precisa implementar uma função que retorna o
                                  // codigo da cidade informando o nome dela.

        Endereco := ProcessarConteudo(ANode.Childrens.Find('enderecotomador'), tcStr);

        Numero := ProcessarConteudo(ANode.Childrens.Find('numeroendereco'), tcStr);

        xMunicipio := ProcessarConteudo(ANode.Childrens.Find('cidadetomador'), tcStr);

        UF := ProcessarConteudo(ANode.Childrens.Find('estadotomador'), tcStr);

        xPais := ProcessarConteudo(ANode.Childrens.Find('paistomador'), tcStr);

        CEP := ProcessarConteudo(ANode.Childrens.Find('ceptomador'), tcStr);

        Bairro := ProcessarConteudo(ANode.Childrens.Find('bairrotomador'), tcStr);
      end;

      with Contato do
      begin
        Telefone := ProcessarConteudo(ANode.Childrens.Find('fonetomador'), tcStr);

        Email := ProcessarConteudo(ANode.Childrens.Find('emailtomador'), tcStr);
      end;
    end;

    OutrasInformacoes := ProcessarConteudo(ANode.Childrens.Find('observacao'), tcStr);

    LerFatura(ANode);
    LerServico(ANode);

    with Transportadora do
    begin
      xNomeTrans := ProcessarConteudo(ANode.Childrens.Find('razaotransportadora'), tcStr);

      xCpfCnpjTrans := ProcessarConteudo(ANode.Childrens.Find('cpfcnpjtransportadora'), tcStr);

      xEndTrans := ProcessarConteudo(ANode.Childrens.Find('enderecotransportadora'), tcStr);
    end;

    with Servico do
    begin
      with Valores do
      begin
        ValorPis := ProcessarConteudo(ANode.Childrens.Find('pis'), tcDe2);

        ValorCofins := ProcessarConteudo(ANode.Childrens.Find('cofins'), tcDe2);

        ValorCsll := ProcessarConteudo(ANode.Childrens.Find('csll'), tcDe2);

        ValorIr := ProcessarConteudo(ANode.Childrens.Find('irrf'), tcDe2);

        ValorInss := ProcessarConteudo(ANode.Childrens.Find('inss'), tcDe2);

        JustificativaDeducao := ProcessarConteudo(ANode.Childrens.Find('descdeducoesconstrucao'), tcStr);

        ValorDeducoes := ProcessarConteudo(ANode.Childrens.Find('totaldeducoesconstrucao'), tcDe2);
      end;
    end;
  end;
end;

end.
