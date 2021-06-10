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

unit Conam.LerXml;

interface

uses
{$IFDEF FPC}
  LResources, Controls, Graphics, Dialogs,
{$ENDIF}
  SysUtils, Classes, StrUtils,
  ACBrUtil,
  ACBrXmlBase, ACBrXmlDocument,
  ACBrNFSeXParametros, ACBrNFSeXConversao, ACBrNFSeXLerXml;

type
  { TNFSeR_Conam }

  TNFSeR_Conam = class(TNFSeRClass)
  protected

    procedure LerReg30(const ANode: TACBrXmlNode);
  public
    function LerXml: Boolean; override;
    function LerXmlRps(const ANode: TACBrXmlNode): Boolean;
    function LerXmlNfse(const ANode: TACBrXmlNode): Boolean;
  end;

implementation

//==============================================================================
// Essa unit tem por finalidade exclusiva ler o XML do provedor:
//     Conam
//==============================================================================

{ TNFSeR_Conam }

procedure TNFSeR_Conam.LerReg30(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  ANodes: TACBrXmlNodeArray;
  i: Integer;
  aValor: string;
begin
  AuxNode := ANode.Childrens.Find('Reg30');

  if AuxNode <> nil then
  begin
    ANodes := AuxNode.Childrens.FindAll('Reg30Item');

    for i := 0 to Length(ANodes) - 1 do
    begin
      aValor := ProcessarConteudo(ANodes[i].Childrens.Find('TributoSigla'), tcStr);

      with NFSe.Servico.Valores do
      begin
        if aValor = 'IR' then
        begin
          AliquotaIr := ProcessarConteudo(ANodes[i].Childrens.Find('TributoAliquota'), tcDe2);
          ValorIr    := ProcessarConteudo(ANodes[i].Childrens.Find('TributoValor'), tcDe2);
        end;

        if aValor = 'PIS' then
        begin
          AliquotaPis := ProcessarConteudo(ANodes[i].Childrens.Find('TributoAliquota'), tcDe2);
          ValorPis    := ProcessarConteudo(ANodes[i].Childrens.Find('TributoValor'), tcDe2);
        end;

        if aValor = 'COFINS' then
        begin
          AliquotaCofins := ProcessarConteudo(ANodes[i].Childrens.Find('TributoAliquota'), tcDe2);
          ValorCofins    := ProcessarConteudo(ANodes[i].Childrens.Find('TributoValor'), tcDe2);
        end;

        if aValor = 'CSLL' then
        begin
          AliquotaCsll := ProcessarConteudo(ANodes[i].Childrens.Find('TributoAliquota'), tcDe2);
          ValorCsll    := ProcessarConteudo(ANodes[i].Childrens.Find('TributoValor'), tcDe2);
        end;

        if aValor = 'INSS' then
        begin
          AliquotaInss := ProcessarConteudo(ANodes[i].Childrens.Find('TributoAliquota'), tcDe2);
          ValorInss    := ProcessarConteudo(ANodes[i].Childrens.Find('TributoValor'), tcDe2);
        end;
      end;
    end;
  end;
end;

function TNFSeR_Conam.LerXml: Boolean;
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

//  FpVersao := ConfigGeral.VersaoProv;

  if tpXML = txmlNFSe then
    Result := LerXmlNfse(XmlNode)
  else
    Result := LerXmlRps(XmlNode);
end;

function TNFSeR_Conam.LerXmlNfse(const ANode: TACBrXmlNode): Boolean;
var
  AuxNode: TACBrXmlNode;
  aValor: string;
  ValorIssRet: Double;
begin
  Result := True;

  if not Assigned(ANode) or (ANode = nil) then Exit;

  AuxNode := ANode.Childrens.Find('Reg20Item');

  if AuxNode = nil then
    AuxNode := ANode.Childrens.Find('CompNfse');

  if AuxNode = nil then Exit;

  with NFSe do
  begin
    Protocolo      := ProcessarConteudo(AuxNode.Childrens.Find('NumNf'), tcStr);
    SeriePrestacao := ProcessarConteudo(AuxNode.Childrens.Find('SerNf'), tcStr);

    aValor := ProcessarConteudo(AuxNode.Childrens.Find('SitNf'), tcStr);

    if aValor = '2' then
      Cancelada := snSim
    else
      Cancelada := snNao;

    MotivoCancelamento := ProcessarConteudo(AuxNode.Childrens.Find('MotivoCncNf'), tcStr);

    aValor := ProcessarConteudo(AuxNode.Childrens.Find('DtEmi'), tcStr);

    if aValor = '' then
      aValor := ProcessarConteudo(AuxNode.Childrens.Find('DtEmiNf'), tcStr);

    if aValor <> '' then
    begin
      DataEmissao := StrToDate(aValor);
      Competencia := DataEmissao;
    end;

    aValor := ProcessarConteudo(AuxNode.Childrens.Find('DtEmiRps'), tcStr);

    DataEmissaoRps := StrToDate(aValor);

    aValor := ProcessarConteudo(AuxNode.Childrens.Find('DtHrGerNf'), tcStr);

    dhRecebimento := StrToDateTimeDef(aValor, Now);

    with IdentificacaoRps do
    begin
      Numero := ProcessarConteudo(AuxNode.Childrens.Find('NumRps'), tcStr);
      Serie  := ProcessarConteudo(AuxNode.Childrens.Find('SerRps'), tcStr);
    end;

    InfID.ID := OnlyNumber(NFSe.IdentificacaoRps.Numero) + NFSe.IdentificacaoRps.Serie;

    CodigoVerificacao := ProcessarConteudo(AuxNode.Childrens.Find('CodVernf'), tcStr);

    aValor := ProcessarConteudo(AuxNode.Childrens.Find('TipoTribPre'), tcStr);

    if aValor = 'SN' then
      OptanteSimplesNacional := snSim
    else
      OptanteSimplesNacional := snNao;

    aValor := ProcessarConteudo(AuxNode.Childrens.Find('DtAdeSN'), tcStr);

    if (aValor <> '') and (aValor <> '/  /') then
      DataOptanteSimplesNacional := StrToDate(aValor);

    with Prestador do
    begin
      RazaoSocial := ProcessarConteudo(AuxNode.Childrens.Find('RazSocPre'), tcStr);

      with IdentificacaoPrestador do
      begin
        Cnpj              := ProcessarConteudo(AuxNode.Childrens.Find('CpfCnpjPre'), tcStr);
        InscricaoEstadual := ProcessarConteudo(AuxNode.Childrens.Find('IEPr'), tcStr);
        InscricaoMunicipal := ProcessarConteudo(AuxNode.Childrens.Find('CodCadBic'), tcStr);
      end;

      with Endereco do
      begin
        Endereco    := ProcessarConteudo(AuxNode.Childrens.Find('LogPre'), tcStr);
        Numero      := ProcessarConteudo(AuxNode.Childrens.Find('NumEndPre'), tcStr);
        Bairro      := ProcessarConteudo(AuxNode.Childrens.Find('BairroPre'), tcStr);
        Complemento := ProcessarConteudo(AuxNode.Childrens.Find('ComplEndPre'), tcStr);
        xMunicipio  := ProcessarConteudo(AuxNode.Childrens.Find('MunPre'), tcStr);
        CEP         := ProcessarConteudo(AuxNode.Childrens.Find('CepPre'), tcStr);
        UF          := ProcessarConteudo(AuxNode.Childrens.Find('SiglaUFPre'), tcStr);
      end;
    end;

    with Servico do
    begin
      Discriminacao    := ProcessarConteudo(AuxNode.Childrens.Find('DiscrSrv'), tcStr);
      ItemListaServico := ProcessarConteudo(AuxNode.Childrens.Find('CodSrv'), tcStr);

      if FAOwner.ConfigGeral.TabServicosExt then
        xItemListaServico := ObterDescricaoServico(OnlyNumber(ItemListaServico))
      else
        xItemListaServico := CodItemServToDesc(OnlyNumber(ItemListaServico));
    end;

    ValorIssRet := ProcessarConteudo(AuxNode.Childrens.Find('VlIssRet'), tcDe2);

    with ValoresNfse do
    begin
      ValorLiquidoNfse := ProcessarConteudo(AuxNode.Childrens.Find('VlNFS'), tcDe2);

      if ValorIssRet > 0 then
        ValorLiquidoNfse := ValorLiquidoNfse - ValorIssRet;

      BaseCalculo := ProcessarConteudo(AuxNode.Childrens.Find('VlBasCalc'), tcDe2);
      Aliquota    := ProcessarConteudo(AuxNode.Childrens.Find('AlqIss'), tcDe2);
      ValorIss    := ProcessarConteudo(AuxNode.Childrens.Find('VlIss'), tcDe2);
    end;

    with Servico.Valores do
    begin
      ValorServicos  := ValoresNfse.ValorLiquidoNfse;
      BaseCalculo    := ValoresNfse.BaseCalculo;
      Aliquota       := ValoresNfse.Aliquota;
      ValorIssRetido := ValorIssRet;

      if ValorIssRet > 0 then
      begin
        IssRetido := stRetencao;
        ValorIss  := 0;
      end
      else
      begin
        IssRetido := stNormal;
        ValorIss  := ProcessarConteudo(AuxNode.Childrens.Find('VlIss'), tcDe2);
      end;

      ValorDeducoes := ProcessarConteudo(AuxNode.Childrens.Find('VlDed'), tcDe2);

      JustificativaDeducao := ProcessarConteudo(AuxNode.Childrens.Find('DiscrDed'), tcStr);
    end;

    with Tomador do
    begin
      RazaoSocial := ProcessarConteudo(AuxNode.Childrens.Find('RazSocTom'), tcStr);

      with IdentificacaoTomador do
        CpfCnpj := ProcessarConteudo(AuxNode.Childrens.Find('CpfCnpjTom'), tcStr);

      with  Endereco do
      begin
        Endereco    := ProcessarConteudo(AuxNode.Childrens.Find('LogTom'), tcStr);
        Numero      := ProcessarConteudo(AuxNode.Childrens.Find('NumEndTom'), tcStr);
        Complemento := ProcessarConteudo(AuxNode.Childrens.Find('ComplEndTom'), tcStr);
        Bairro      := ProcessarConteudo(AuxNode.Childrens.Find('BairroTom'), tcStr);
        xMunicipio  := ProcessarConteudo(AuxNode.Childrens.Find('MunTom'), tcStr);
        UF          := ProcessarConteudo(AuxNode.Childrens.Find('SiglaUFTom'), tcStr);
        CEP         := ProcessarConteudo(AuxNode.Childrens.Find('CepTom'), tcStr);
      end;
    end;

    //valores dos tributos
    LerReg30(AuxNode);
  end;
end;

function TNFSeR_Conam.LerXmlRps(const ANode: TACBrXmlNode): Boolean;
var
  AuxNode: TACBrXmlNode;
  aValor: string;
  i: Integer;
begin
  Result := True;

  if not Assigned(ANode) or (ANode = nil) then Exit;

  AuxNode := ANode.Childrens.Find('Reg20Item');

  if AuxNode = nil then Exit;

  with NFSe do
  begin
    with IdentificacaoRps do
    begin
      aValor := ProcessarConteudo(AuxNode.Childrens.Find('TipoNFS'), tcStr);

      if aValor = 'RPS' then
        Tipo := trRPS
      else
        Tipo := trNone;

      Numero := ProcessarConteudo(AuxNode.Childrens.Find('NumRps'), tcStr);

      Serie := ProcessarConteudo(AuxNode.Childrens.Find('SerRps'), tcStr);
    end;

    aValor := ProcessarConteudo(AuxNode.Childrens.Find('DtEmi'), tcStr);

    if aValor <> '' then
    begin
      DataEmissaoRps := StrToDate(aValor);
      Competencia := DataEmissao;
    end;

    with Servico do
    begin
      ItemListaServico := ProcessarConteudo(AuxNode.Childrens.Find('CodSrv'), tcStr);

      Discriminacao := ProcessarConteudo(AuxNode.Childrens.Find('DiscrSrv'), tcStr);

      with Valores do
      begin
        aValor := ProcessarConteudo(AuxNode.Childrens.Find('RetFonte'), tcStr);

        if aValor <> 'NAO' then
          IssRetido := stNormal
        else
          IssRetido := stRetencao;

        ValorServicos := ProcessarConteudo(AuxNode.Childrens.Find('VlNFS'), tcDe2);

        ValorDeducoes := ProcessarConteudo(AuxNode.Childrens.Find('VlDed'), tcDe2);

        JustificativaDeducao := ProcessarConteudo(AuxNode.Childrens.Find('DiscrDed'), tcStr);

        BaseCalculo := ProcessarConteudo(AuxNode.Childrens.Find('VlBasCalc'), tcDe2);

        Aliquota := ProcessarConteudo(AuxNode.Childrens.Find('AlqIss'), tcDe2);

        ValorIss := ProcessarConteudo(AuxNode.Childrens.Find('VlIss'), tcDe2);

        ValorIssRetido := ProcessarConteudo(AuxNode.Childrens.Find('VlIssRet'), tcDe2);
      end;
    end;

    with Tomador do
    begin
      RazaoSocial := ProcessarConteudo(AuxNode.Childrens.Find('RazSocTom'), tcStr);

      with IdentificacaoTomador do
      begin
        CpfCnpj := ProcessarConteudo(AuxNode.Childrens.Find('CpfCnpTom'), tcStr);

        InscricaoMunicipal := ProcessarConteudo(AuxNode.Childrens.Find('InscricaoMunicipal'), tcStr);
      end;

      with  Endereco do
      begin
        TipoLogradouro := ProcessarConteudo(AuxNode.Childrens.Find('TipoLogtom'), tcStr);

        Endereco    := ProcessarConteudo(AuxNode.Childrens.Find('LogTom'), tcStr);
        Numero      := ProcessarConteudo(AuxNode.Childrens.Find('NumEndTom'), tcStr);
        Complemento := ProcessarConteudo(AuxNode.Childrens.Find('ComplEndTom'), tcStr);
        Bairro      := ProcessarConteudo(AuxNode.Childrens.Find('BairroTom'), tcStr);
        xMunicipio  := ProcessarConteudo(AuxNode.Childrens.Find('MunTom'), tcStr);
        UF          := ProcessarConteudo(AuxNode.Childrens.Find('SiglaUFTom'), tcStr);
        CEP         := ProcessarConteudo(AuxNode.Childrens.Find('CepTom'), tcStr);
      end;

      with Contato do
      begin
        Telefone := ProcessarConteudo(AuxNode.Childrens.Find('Telefone'), tcStr);
        Email := ProcessarConteudo(AuxNode.Childrens.Find('Email1'), tcStr);
      end;
    end;

    with Prestador do
    begin
      with Endereco do
      begin
        Endereco := ProcessarConteudo(AuxNode.Childrens.Find('LogLocPre'), tcStr);

        if Endereco = Tomador.Endereco.Endereco then
          LogradouLocalPrestacaoServico := llpTomador
        else
          LogradouLocalPrestacaoServico := llpPrestador;

        TipoLogradouro := ProcessarConteudo(AuxNode.Childrens.Find('TipoLogLocPre'), tcStr);
        Numero      := ProcessarConteudo(AuxNode.Childrens.Find('NumEndLocPre'), tcStr);
        Complemento := ProcessarConteudo(AuxNode.Childrens.Find('ComplEndLocPre'), tcStr);
        Bairro      := ProcessarConteudo(AuxNode.Childrens.Find('BairroLocPre'), tcStr);
        xMunicipio  := ProcessarConteudo(AuxNode.Childrens.Find('MunLocPre'), tcStr);
        UF          := ProcessarConteudo(AuxNode.Childrens.Find('SiglaUFLocpre'), tcStr);
        CEP         := ProcessarConteudo(AuxNode.Childrens.Find('CepLocPre'), tcStr);
      end;
    end;

    i:= 0;

    aValor := ProcessarConteudo(AuxNode.Childrens.Find('Email2'), tcStr);

    if aValor <> '' then
    begin
      email.New;
      email[i].emailCC := aValor;
      Inc(i);
    end;

    aValor := ProcessarConteudo(AuxNode.Childrens.Find('Email3'), tcStr);

    if aValor <> '' then
    begin
      email.New;
      email[i].emailCC := aValor;
//      Inc(i);
    end;

    //valores dos tributos
    LerReg30(AuxNode);
  end;
end;

end.
