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
begin
  if EstaVazio(Arquivo) then
    raise Exception.Create('Arquivo xml não carregado.');

  Document.Clear();
  Document.LoadFromXml(Arquivo);

  if (Pos('NOTA', Arquivo) > 0) then
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

function TNFSeR_AssessorPublico.LerXmlNfse(const ANode: TACBrXmlNode): Boolean;
var
  AuxNode: TACBrXmlNode;
  aValor: String;
  ANodes: TACBrXmlNodeArray;
  i: integer;
begin
  Result := True;

  if not Assigned(ANode) or (ANode = nil) then Exit;

  AuxNode := ANode.Childrens.Find('NOTA');

  if AuxNode = nil then Exit;

  NFSe.Link       := ProcessarConteudo(AuxNode.Childrens.Find('LINK'), tcStr);
  NFSe.NumeroLote := ProcessarConteudo(AuxNode.Childrens.Find('LOTE'), tcStr);
  NFSe.Numero     := ProcessarConteudo(AuxNode.Childrens.Find('COD'), tcStr);

  NFSe.IdentificacaoRps.Numero := ProcessarConteudo(AuxNode.Childrens.Find('RPS'), tcStr);
  NFSe.IdentificacaoRps.Serie  := ProcessarConteudo(AuxNode.Childrens.Find('SEQUENCIA'), tcStr);

  NFSe.Competencia := ProcessarConteudo(AuxNode.Childrens.Find('MESCOMP'), tcStr) +
                      ProcessarConteudo(AuxNode.Childrens.Find('ANOCOMP'), tcStr);

  aValor := ProcessarConteudo(AuxNode.Childrens.Find('DATA'), tcStr) +
            ProcessarConteudo(AuxNode.Childrens.Find('HORA'), tcStr);

  NFSe.DataEmissao := StringToDateTime(aValor, 'DD/MM/YYYY hh:nn:ss');

  NFSe.OptanteSimplesNacional := snNao;

  aValor := ProcessarConteudo(AuxNode.Childrens.Find('PRESTSUPERSIMP'), tcStr);

  if aValor = 'S' then
    NFSe.OptanteSimplesNacional := snSim;

  with NFSe.Servico do
  begin
    Discriminacao     := ProcessarConteudo(AuxNode.Childrens.Find('OBSSERVICO'), tcStr);
    ItemListaServico  := ProcessarConteudo(AuxNode.Childrens.Find('ATIVCOD'), tcStr);
    xItemListaServico := ProcessarConteudo(AuxNode.Childrens.Find('ATIVDESC'), tcStr);
    CodigoMunicipio   := ProcessarConteudo(AuxNode.Childrens.Find('TOMMUNICIPIOCOD'), tcStr);

    MunicipioIncidencia := ProcessarConteudo(AuxNode.Childrens.Find('TOMMUNICIPIOCOD'), tcInt);
  end;

  with NFSe.Servico.Valores do
  begin
    aValor := ProcessarConteudo(AuxNode.Childrens.Find('RETIDO'), tcStr);

    if aValor = 'N' then
      IssRetido := stNormal;

    BaseCalculo   := ProcessarConteudo(AuxNode.Childrens.Find('BASECALC'), tcDe2);
    ValorServicos := ProcessarConteudo(AuxNode.Childrens.Find('VALORTOTALSERVICOS'), tcDe2);
    ValorIss      := ProcessarConteudo(AuxNode.Childrens.Find('IMPOSTO'), tcDe2);
    ValorDeducoes := ProcessarConteudo(AuxNode.Childrens.Find('DEDUCAO'), tcDe2);
    ValorOutrasRetencoes := ProcessarConteudo(AuxNode.Childrens.Find('RETENCAO'), tcDe2);
    ValorPis      := ProcessarConteudo(AuxNode.Childrens.Find('PIS'), tcDe2);
    ValorCofins   := ProcessarConteudo(AuxNode.Childrens.Find('COFINS'), tcDe2);
    ValorInss     := ProcessarConteudo(AuxNode.Childrens.Find('INSS'), tcDe2);
    ValorIr       := ProcessarConteudo(AuxNode.Childrens.Find('IR'), tcDe2);
    ValorCsll     := ProcessarConteudo(AuxNode.Childrens.Find('CSLL'), tcDe2);

    OutrasRetencoes := OutrasRetencoes +
           ProcessarConteudo(AuxNode.Childrens.Find('ICMS'), tcDe2) +
           ProcessarConteudo(AuxNode.Childrens.Find('IOF'), tcDe2) +
           ProcessarConteudo(AuxNode.Childrens.Find('CIDE'), tcDe2) +
           ProcessarConteudo(AuxNode.Childrens.Find('OUTROSTRIBUTOS'), tcDe2) +
           ProcessarConteudo(AuxNode.Childrens.Find('OUTRASRETENCOES'), tcDe2) +
           ProcessarConteudo(AuxNode.Childrens.Find('IPI'), tcDe2);

    Aliquota := ProcessarConteudo(AuxNode.Childrens.Find('ALIQUOTA'), tcDe4);
  end;

  with NFSe.Prestador do
  begin
    RazaoSocial  := ProcessarConteudo(AuxNode.Childrens.Find('PRESTNOMERAZAO'), tcStr);
    NomeFantasia := ProcessarConteudo(AuxNode.Childrens.Find('PRESTNOMERAZAO'), tcStr);
  end;

  with NFSe.Prestador.IdentificacaoPrestador do
  begin
    Cnpj               := ProcessarConteudo(AuxNode.Childrens.Find('PRESTCPFCNPJ'), tcStr);
    InscricaoMunicipal := ProcessarConteudo(AuxNode.Childrens.Find('PRESTCODMOBILIARIO'), tcStr);
  end;

  with NFSe.Prestador.Endereco do
  begin
    Endereco := ProcessarConteudo(AuxNode.Childrens.Find('PRESTPREFIXODESC'), tcStr) +
         ' ' + ProcessarConteudo(AuxNode.Childrens.Find('PRESTLOGDESC'), tcStr);
    Numero := ProcessarConteudo(AuxNode.Childrens.Find('PRESTNUMERO'), tcStr);

    Bairro          := ProcessarConteudo(AuxNode.Childrens.Find('PRESTBAIRRODESC'), tcStr);
    CodigoMunicipio := ProcessarConteudo(AuxNode.Childrens.Find('PRESTMUNICIPIOCOD'), tcStr);
    xMunicipio      := ProcessarConteudo(AuxNode.Childrens.Find('PRESTMUNICIPIODESC'), tcStr);
    UF              := ProcessarConteudo(AuxNode.Childrens.Find('PRESTMUNICIPIOUF'), tcStr);
    CEP             := ProcessarConteudo(AuxNode.Childrens.Find('PRESTCEP'), tcStr);
  end;

  with NFSe.Tomador do
  begin
    RazaoSocial := ProcessarConteudo(AuxNode.Childrens.Find('TOMNOMERAZAO'), tcStr);
  end;

  with NFSe.Tomador.IdentificacaoTomador do
  begin
    CpfCnpj            := ProcessarConteudo(AuxNode.Childrens.Find('TOMCPFCNPJ'), tcStr);
    InscricaoMunicipal := ProcessarConteudo(AuxNode.Childrens.Find('TOMINSCRICAOMUN'), tcStr);
  end;

  with NFSe.Tomador.Endereco do
  begin
    Endereco := ProcessarConteudo(AuxNode.Childrens.Find('TOMPREFIXODESC'), tcStr) +
           ' ' + ProcessarConteudo(AuxNode.Childrens.Find('TOMLOGDESC'), tcStr);
    Numero   := ProcessarConteudo(AuxNode.Childrens.Find('TOMNUMERO'), tcStr);

    Bairro          := ProcessarConteudo(AuxNode.Childrens.Find('TOMBAIRRODESC'), tcStr);
    CodigoMunicipio := ProcessarConteudo(AuxNode.Childrens.Find('TOMMUNICIPIOCOD'), tcStr);
    xMunicipio      := ProcessarConteudo(AuxNode.Childrens.Find('TOMMUNICIPIODESC'), tcStr);
    UF              := ProcessarConteudo(AuxNode.Childrens.Find('TOMMUNICIPIOUF'), tcStr);
    CEP             := ProcessarConteudo(AuxNode.Childrens.Find('TOMCEP'), tcStr);
  end;

  AuxNode := ANode.Childrens.Find('SERVICOS');

  if AuxNode <> nil then
  begin
    ANodes := AuxNode.Childrens.FindAll('SERVICO');

    for i := 0 to Length(ANodes) - 1 do
    begin
      NFSe.Servico.ItemServico.New;
      with NFSe.Servico.ItemServico[i] do
      begin
        CodServ       := ProcessarConteudo(ANodes[i].Childrens.Find('CODIGO'), tcStr);
        Descricao     := ProcessarConteudo(ANodes[i].Childrens.Find('DESCRICAO'), tcStr);
        Quantidade    := ProcessarConteudo(ANodes[i].Childrens.Find('QUANTIDADE'), tcDe2);
        ValorUnitario := ProcessarConteudo(ANodes[i].Childrens.Find('VALOR'), tcDe2);

        DescontoIncondicionado := ProcessarConteudo(ANodes[i].Childrens.Find('DESCONTO'), tcDe2);

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
    NumeroLote := ProcessarConteudo(ANode.Childrens.Find('LOTE'), tcStr);

    with IdentificacaoRps do
    begin
      Numero := ProcessarConteudo(ANode.Childrens.Find('SEQUENCIA'), tcStr);
    end;

    aValor := ProcessarConteudo(ANode.Childrens.Find('DATAEMISSAO'), tcStr);
    aValor := aValor + ' ' +
              ProcessarConteudo(ANode.Childrens.Find('HORAEMISSAO'), tcStr);

    DataEmissao := StrToDateTimeDef(aValor, 0);

    Situacao := ProcessarConteudo(ANode.Childrens.Find('SITUACAO'), tcStr);

    with Servico do
    begin
      ItemListaServico := ProcessarConteudo(ANode.Childrens.Find('ATIVIDADE'), tcStr);

      Discriminacao := ProcessarConteudo(ANode.Childrens.Find('OBSERVACAO'), tcStr);

      with Valores do
      begin
        aValor := ProcessarConteudo(ANode.Childrens.Find('RETIDO'), tcStr);

        if aValor = 'S' then
          IssRetido := stRetencao
        else
          IssRetido := stNormal;

        Aliquota := ProcessarConteudo(ANode.Childrens.Find('ALIQUOTAAPLICADA'), tcDe2);

        ValorDeducoes := ProcessarConteudo(ANode.Childrens.Find('DEDUCAO'), tcDe2);

        valorOutrasRetencoes := ProcessarConteudo(ANode.Childrens.Find('IMPOSTO'), tcDe2);

        ValorIssRetido := ProcessarConteudo(ANode.Childrens.Find('RETENCAO'), tcDe2);

        ValorPis := ProcessarConteudo(ANode.Childrens.Find('PIS'), tcDe2);

        ValorCofins := ProcessarConteudo(ANode.Childrens.Find('COFINS'), tcDe2);

        ValorInss := ProcessarConteudo(ANode.Childrens.Find('INSS'), tcDe2);

        ValorIr := ProcessarConteudo(ANode.Childrens.Find('IR'), tcDe2);

        ValorCsll := ProcessarConteudo(ANode.Childrens.Find('CSLL'), tcDe2);
      end;
    end;

    with Tomador do
    begin
      with IdentificacaoTomador do
      begin
        CpfCnpj := ProcessarConteudo(ANode.Childrens.Find('CPFCNPJ'), tcStr);

        InscricaoEstadual := ProcessarConteudo(ANode.Childrens.Find('RGIE'), tcStr);
      end;

      RazaoSocial := ProcessarConteudo(ANode.Childrens.Find('NOMERAZAO'), tcStr);

      NomeFantasia := ProcessarConteudo(ANode.Childrens.Find('NOMEFANTASIA'), tcStr);

      with Endereco do
      begin
        CodigoMunicipio := ProcessarConteudo(ANode.Childrens.Find('MUNICIPIO'), tcStr);

        Bairro := ProcessarConteudo(ANode.Childrens.Find('BAIRRO'), tcStr);

        CEP := ProcessarConteudo(ANode.Childrens.Find('CEP'), tcStr);

        TipoLogradouro := ProcessarConteudo(ANode.Childrens.Find('PREFIXO'), tcStr);

        Endereco := ProcessarConteudo(ANode.Childrens.Find('LOGRADOURO'), tcStr);

        Complemento := ProcessarConteudo(ANode.Childrens.Find('COMPLEMENTO'), tcStr);

        Numero := ProcessarConteudo(ANode.Childrens.Find('NUMERO'), tcStr);

        Contato.Email := ProcessarConteudo(ANode.Childrens.Find('EMAIL'), tcStr);
      end;
    end;

    AuxNode := ANode.Childrens.Find('SERVICOS');

    if AuxNode <> nil then
    begin
      ANodes := AuxNode.Childrens.FindAll('SERVICO');

      for i := 0 to Length(ANodes) - 1 do
      begin
        Servico.ItemServico.New;
        with NFSe.Servico.ItemServico[i] do
        begin
          Descricao := ProcessarConteudo(ANodes[i].Childrens.Find('DESCRICAO'), tcStr);

          ValorUnitario := ProcessarConteudo(ANodes[i].Childrens.Find('VALORUNIT'), tcDe2);

          Quantidade := ProcessarConteudo(ANodes[i].Childrens.Find('QUANTIDADE'), tcDe4);

          DescontoIncondicionado := ProcessarConteudo(ANodes[i].Childrens.Find('DESCONTO'), tcDe2);
        end;
      end;
    end;
  end;
end;

end.
