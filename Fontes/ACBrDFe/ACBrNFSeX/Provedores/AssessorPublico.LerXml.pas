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

unit AssessorPublico.LerXml;

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
  { TNFSeR_AssessorPublico }

  TNFSeR_AssessorPublico = class(TNFSeRClass)
  protected

  public
    function LerXml: Boolean; override;
    function LerXmlRps(const ANode: TACBrXmlNode): Boolean;
    function LerXmlNfse(const ANode: TACBrXmlNode): Boolean;
  end;

implementation

//==============================================================================
// Essa unit tem por finalidade exclusiva ler o XML do provedor:
//     AssessorPublico
//==============================================================================

{ TNFSeR_AssessorPublico }

function TNFSeR_AssessorPublico.LerXml: Boolean;
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

  if (Pos('NOTA', xRetorno) > 0) then
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

function TNFSeR_AssessorPublico.LerXmlNfse(const ANode: TACBrXmlNode): Boolean;
var
  AuxNode: TACBrXmlNode;
  aValor: String;
  ANodes: TACBrXmlNodeArray;
  i: integer;
begin
  Result := True;

  if not Assigned(ANode) or (ANode = nil) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('NOTA');

  if AuxNode = nil then Exit;

  NFSe.Link       := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('LINK'), tcStr);
  NFSe.NumeroLote := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('LOTE'), tcStr);
  NFSe.Numero     := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('COD'), tcStr);

  NFSe.InfID.ID := NFSe.Numero;

  NFSe.IdentificacaoRps.Numero := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('RPS'), tcStr);
  NFSe.IdentificacaoRps.Serie  := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('SEQUENCIA'), tcStr);

  NFSe.Competencia := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('MESCOMP'), tcStr) +
                      ProcessarConteudo(AuxNode.Childrens.FindAnyNs('ANOCOMP'), tcStr);

  aValor := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('DATA'), tcStr) +
            ProcessarConteudo(AuxNode.Childrens.FindAnyNs('HORA'), tcStr);

  NFSe.DataEmissao := StringToDateTime(aValor, 'DD/MM/YYYY hh:nn:ss');

  NFSe.OptanteSimplesNacional := snNao;

  aValor := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('PRESTSUPERSIMP'), tcStr);

  if aValor = 'S' then
    NFSe.OptanteSimplesNacional := snSim;

  with NFSe.Servico do
  begin
    Discriminacao     := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('OBSSERVICO'), tcStr);
    ItemListaServico  := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('ATIVCOD'), tcStr);
    xItemListaServico := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('ATIVDESC'), tcStr);
    CodigoMunicipio   := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('TOMMUNICIPIOCOD'), tcStr);

    MunicipioIncidencia := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('TOMMUNICIPIOCOD'), tcInt);
  end;

  with NFSe.Servico.Valores do
  begin
    aValor := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('RETIDO'), tcStr);

    if aValor = 'N' then
      IssRetido := stNormal;

    BaseCalculo   := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('BASECALC'), tcDe2);
    ValorServicos := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('VALORTOTALSERVICOS'), tcDe2);
    ValorIss      := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('IMPOSTO'), tcDe2);
    ValorDeducoes := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('DEDUCAO'), tcDe2);
    ValorOutrasRetencoes := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('RETENCAO'), tcDe2);
    ValorPis      := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('PIS'), tcDe2);
    ValorCofins   := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('COFINS'), tcDe2);
    ValorInss     := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('INSS'), tcDe2);
    ValorIr       := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('IR'), tcDe2);
    ValorCsll     := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('CSLL'), tcDe2);

    OutrasRetencoes := OutrasRetencoes +
           ProcessarConteudo(AuxNode.Childrens.FindAnyNs('ICMS'), tcDe2) +
           ProcessarConteudo(AuxNode.Childrens.FindAnyNs('IOF'), tcDe2) +
           ProcessarConteudo(AuxNode.Childrens.FindAnyNs('CIDE'), tcDe2) +
           ProcessarConteudo(AuxNode.Childrens.FindAnyNs('OUTROSTRIBUTOS'), tcDe2) +
           ProcessarConteudo(AuxNode.Childrens.FindAnyNs('OUTRASRETENCOES'), tcDe2) +
           ProcessarConteudo(AuxNode.Childrens.FindAnyNs('IPI'), tcDe2);

    Aliquota := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('ALIQUOTA'), tcDe4);
  end;

  with NFSe.Prestador do
  begin
    RazaoSocial  := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('PRESTNOMERAZAO'), tcStr);
    NomeFantasia := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('PRESTNOMERAZAO'), tcStr);
  end;

  with NFSe.Prestador.IdentificacaoPrestador do
  begin
    Cnpj               := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('PRESTCPFCNPJ'), tcStr);
    InscricaoMunicipal := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('PRESTCODMOBILIARIO'), tcStr);
  end;

  with NFSe.Prestador.Endereco do
  begin
    Endereco := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('PRESTPREFIXODESC'), tcStr) +
         ' ' + ProcessarConteudo(AuxNode.Childrens.FindAnyNs('PRESTLOGDESC'), tcStr);
    Numero := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('PRESTNUMERO'), tcStr);

    Bairro          := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('PRESTBAIRRODESC'), tcStr);
    CodigoMunicipio := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('PRESTMUNICIPIOCOD'), tcStr);
    xMunicipio      := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('PRESTMUNICIPIODESC'), tcStr);
    UF              := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('PRESTMUNICIPIOUF'), tcStr);
    CEP             := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('PRESTCEP'), tcStr);
  end;

  with NFSe.Tomador do
  begin
    RazaoSocial := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('TOMNOMERAZAO'), tcStr);
  end;

  with NFSe.Tomador.IdentificacaoTomador do
  begin
    CpfCnpj            := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('TOMCPFCNPJ'), tcStr);
    InscricaoMunicipal := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('TOMINSCRICAOMUN'), tcStr);
  end;

  with NFSe.Tomador.Endereco do
  begin
    Endereco := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('TOMPREFIXODESC'), tcStr) +
           ' ' + ProcessarConteudo(AuxNode.Childrens.FindAnyNs('TOMLOGDESC'), tcStr);
    Numero   := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('TOMNUMERO'), tcStr);

    Bairro          := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('TOMBAIRRODESC'), tcStr);
    CodigoMunicipio := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('TOMMUNICIPIOCOD'), tcStr);
    xMunicipio      := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('TOMMUNICIPIODESC'), tcStr);
    UF              := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('TOMMUNICIPIOUF'), tcStr);
    CEP             := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('TOMCEP'), tcStr);
  end;

  AuxNode := ANode.Childrens.FindAnyNs('SERVICOS');

  if AuxNode <> nil then
  begin
    ANodes := AuxNode.Childrens.FindAllAnyNs('SERVICO');

    for i := 0 to Length(ANodes) - 1 do
    begin
      NFSe.Servico.ItemServico.New;
      with NFSe.Servico.ItemServico[i] do
      begin
        CodServ       := ProcessarConteudo(ANodes[i].Childrens.FindAnyNs('CODIGO'), tcStr);
        Descricao     := ProcessarConteudo(ANodes[i].Childrens.FindAnyNs('DESCRICAO'), tcStr);
        Quantidade    := ProcessarConteudo(ANodes[i].Childrens.FindAnyNs('QUANTIDADE'), tcDe2);
        ValorUnitario := ProcessarConteudo(ANodes[i].Childrens.FindAnyNs('VALOR'), tcDe2);

        DescontoIncondicionado := ProcessarConteudo(ANodes[i].Childrens.FindAnyNs('DESCONTO'), tcDe2);

        ValorTotal := Quantidade + ValorUnitario;
        Tributavel := snSim;
      end;
    end;
  end;

  with NFSe.Servico.Valores do
  begin
    ValorLiquidoNfse := ValorServicos -
        (ValorDeducoes + DescontoCondicionado +
         DescontoIncondicionado + ValorIssRetido);
  end;
end;

function TNFSeR_AssessorPublico.LerXmlRps(const ANode: TACBrXmlNode): Boolean;
var
  AuxNode: TACBrXmlNode;
  ANodes: TACBrXmlNodeArray;
  i: Integer;
  aValor: string;
begin
  Result := True;

  if not Assigned(ANode) or (ANode = nil) then Exit;

  with NFSe do
  begin
    NumeroLote := ProcessarConteudo(ANode.Childrens.FindAnyNs('LOTE'), tcStr);

    with IdentificacaoRps do
    begin
      Numero := ProcessarConteudo(ANode.Childrens.FindAnyNs('SEQUENCIA'), tcStr);
    end;

    aValor := ProcessarConteudo(ANode.Childrens.FindAnyNs('DATAEMISSAO'), tcStr);
    aValor := aValor + ' ' +
              ProcessarConteudo(ANode.Childrens.FindAnyNs('HORAEMISSAO'), tcStr);

    DataEmissao := StrToDateTimeDef(aValor, 0);

    Situacao := ProcessarConteudo(ANode.Childrens.FindAnyNs('SITUACAO'), tcStr);

    with Servico do
    begin
      ItemListaServico := ProcessarConteudo(ANode.Childrens.FindAnyNs('ATIVIDADE'), tcStr);

      Discriminacao := ProcessarConteudo(ANode.Childrens.FindAnyNs('OBSERVACAO'), tcStr);

      with Valores do
      begin
        aValor := ProcessarConteudo(ANode.Childrens.FindAnyNs('RETIDO'), tcStr);

        if aValor = 'S' then
          IssRetido := stRetencao
        else
          IssRetido := stNormal;

        Aliquota := ProcessarConteudo(ANode.Childrens.FindAnyNs('ALIQUOTAAPLICADA'), tcDe2);

        ValorDeducoes := ProcessarConteudo(ANode.Childrens.FindAnyNs('DEDUCAO'), tcDe2);

        valorOutrasRetencoes := ProcessarConteudo(ANode.Childrens.FindAnyNs('IMPOSTO'), tcDe2);

        ValorIssRetido := ProcessarConteudo(ANode.Childrens.FindAnyNs('RETENCAO'), tcDe2);

        ValorPis := ProcessarConteudo(ANode.Childrens.FindAnyNs('PIS'), tcDe2);

        ValorCofins := ProcessarConteudo(ANode.Childrens.FindAnyNs('COFINS'), tcDe2);

        ValorInss := ProcessarConteudo(ANode.Childrens.FindAnyNs('INSS'), tcDe2);

        ValorIr := ProcessarConteudo(ANode.Childrens.FindAnyNs('IR'), tcDe2);

        ValorCsll := ProcessarConteudo(ANode.Childrens.FindAnyNs('CSLL'), tcDe2);
      end;
    end;

    with Tomador do
    begin
      with IdentificacaoTomador do
      begin
        CpfCnpj := ProcessarConteudo(ANode.Childrens.FindAnyNs('CPFCNPJ'), tcStr);

        InscricaoEstadual := ProcessarConteudo(ANode.Childrens.FindAnyNs('RGIE'), tcStr);
      end;

      RazaoSocial := ProcessarConteudo(ANode.Childrens.FindAnyNs('NOMERAZAO'), tcStr);

      NomeFantasia := ProcessarConteudo(ANode.Childrens.FindAnyNs('NOMEFANTASIA'), tcStr);

      with Endereco do
      begin
        CodigoMunicipio := ProcessarConteudo(ANode.Childrens.FindAnyNs('MUNICIPIO'), tcStr);

        Bairro := ProcessarConteudo(ANode.Childrens.FindAnyNs('BAIRRO'), tcStr);

        CEP := ProcessarConteudo(ANode.Childrens.FindAnyNs('CEP'), tcStr);

        TipoLogradouro := ProcessarConteudo(ANode.Childrens.FindAnyNs('PREFIXO'), tcStr);

        Endereco := ProcessarConteudo(ANode.Childrens.FindAnyNs('LOGRADOURO'), tcStr);

        Complemento := ProcessarConteudo(ANode.Childrens.FindAnyNs('COMPLEMENTO'), tcStr);

        Numero := ProcessarConteudo(ANode.Childrens.FindAnyNs('NUMERO'), tcStr);

        Contato.Email := ProcessarConteudo(ANode.Childrens.FindAnyNs('EMAIL'), tcStr);
      end;
    end;

    AuxNode := ANode.Childrens.FindAnyNs('SERVICOS');

    if AuxNode <> nil then
    begin
      ANodes := AuxNode.Childrens.FindAllAnyNs('SERVICO');

      for i := 0 to Length(ANodes) - 1 do
      begin
        Servico.ItemServico.New;
        with NFSe.Servico.ItemServico[i] do
        begin
          Descricao := ProcessarConteudo(ANodes[i].Childrens.FindAnyNs('DESCRICAO'), tcStr);

          ValorUnitario := ProcessarConteudo(ANodes[i].Childrens.FindAnyNs('VALORUNIT'), tcDe2);

          Quantidade := ProcessarConteudo(ANodes[i].Childrens.FindAnyNs('QUANTIDADE'), tcDe4);

          DescontoIncondicionado := ProcessarConteudo(ANodes[i].Childrens.FindAnyNs('DESCONTO'), tcDe2);
        end;
      end;
    end;
  end;
end;

end.
