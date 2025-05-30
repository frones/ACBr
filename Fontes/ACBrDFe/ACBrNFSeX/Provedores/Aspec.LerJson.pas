{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2025 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Willian Delan de Oliveira                       }
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

unit Aspec.LerJson;

interface

uses
  SysUtils, Classes, StrUtils,
  IniFiles,
  ACBrUtil.Base, ACBrUtil.Strings,
  ACBrXmlBase, ACBrXmlDocument, ACBrNFSeXClass,
  ACBrNFSeXConversao, ACBrNFSeXLerXml, ACBrJSON;

type
  { Provedor com layout próprio }
  { TNFSeR_Aspec }

  TNFSeR_Aspec = class(TNFSeRClass)
  private
    FpTipoXML: string;
  protected
    procedure LerNota(aJson: TACBrJSONObject; FRps: Boolean);
    procedure LerSituacaoNfse(aJson: TACBrJSONObject);
    procedure LerIdentificacaoPrestador(aJson: TACBrJSONObject);
    procedure LerPrestador(aJson: TACBrJSONObject);
    procedure LerTomador(aJson: TACBrJSONObject);
    procedure LerEndereco(aJson: TACBrJSONObject; aEndereco: TEndereco);
    procedure LerContato(aJson: TACBrJSONObject; aContato: TContato);
    procedure LerRps(aJson: TACBrJSONObject);
    procedure LerServicos(aJson: TACBrJSONObject);

    function LerJsonNfse(const ArquivoRetorno: String): Boolean;
    function LerJsonRps(const ArquivoRetorno: String): Boolean;

    //====== Ler o Arquivo INI===========================================
    procedure LerINIIdentificacaoNFSe(AINIRec: TMemIniFile);
    procedure LerINIIdentificacaoPrestador(AINIRec: TMemIniFile);
    procedure LerINIDadosTomador(AINIRec: TMemIniFile);
    procedure LerINIIdentificacaoRps(AINIRec: TMemIniFile);
    procedure LerINIDadosServico(AINIRec: TMemIniFile);
    procedure LerINIDadosValores(AINIRec: TMemIniFile);
    procedure LerINIDadosPrestador(AINIRec: TMemIniFile);
    procedure LerINIInformacoesCancelamento(AINIRec: TMemIniFile);

    procedure LerIniRps(AINIRec: TMemIniFile);
    procedure LerIniNfse(AINIRec: TMemIniFile);
  public
    function LerXml: Boolean; override;
    function LerIni: Boolean; override;
  end;

implementation

uses
  ACBrUtil.FilesIO,
  ACBrUtil.DateTime;

//==============================================================================
// Essa unit tem por finalidade exclusiva ler o Json do provedor:
//     Aspec
//==============================================================================

{ TNFSeR_Aspec }

function TNFSeR_Aspec.LerXml: Boolean;
begin
  if EstaVazio(Arquivo) then
    raise Exception.Create('Arquivo Json não carregado.');

  if (Pos('DadosNfse', Arquivo) > 0) then
    Result := LerJsonNfse(TiraAcentos(Arquivo))
  else
    Result := LerJsonRps(TiraAcentos(Arquivo));
end;

function TNFSeR_Aspec.LerJsonNfse(const ArquivoRetorno: String): Boolean;
var
  jsRet: TACBrJSONObject;
begin
  Result := False;
  tpXML := txmlNFSe;
  jsRet := nil;

  try
    jsRet := TACBrJSONObject.Parse(String(ArquivoRetorno));

    if Assigned(jsRet.AsJSONObject['DadosNfse']) then
    begin
      LerNota(jsRet.AsJSONObject['DadosNfse'], False);
      LerPrestador(jsRet.AsJSONObject['DadosNfse']);
      LerTomador(jsRet.AsJSONObject['DadosNfse']);
      LerRps(jsRet.AsJSONObject['DadosNfse']);
      LerServicos(jsRet.AsJSONObject['DadosNfse']);

      Result := True;
    end;
  finally
    jsRet.Free;
  end;
end;

function TNFSeR_Aspec.LerJsonRps(const ArquivoRetorno: String): Boolean;
var
  jsRet: TACBrJSONObject;
begin
  Result := False;
  tpXML := txmlRPS;
  jsRet := nil;

  try
    jsRet := TACBrJSONObject.Parse(String(ArquivoRetorno));

    if Assigned(jsRet.AsJSONObject['DadosNota']) then
    begin
      LerNota(jsRet.AsJSONObject['DadosNota'], True);
      LerIdentificacaoPrestador(jsRet.AsJSONObject['DadosNota']);
      LerTomador(jsRet.AsJSONObject['DadosNota']);
      LerRps(jsRet.AsJSONObject['DadosNota']);
      LerServicos(jsRet.AsJSONObject['DadosNota']);

      Result := True;
    end;
  finally
    jsRet.Free;
  end;
end;

procedure TNFSeR_Aspec.LerRps(aJson: TACBrJSONObject);
var
  jsAux: TACBrJSONObject;
begin
  jsAux := aJson.AsJSONObject['Rps'];

  if Assigned(jsAux) then
  begin
    NFSe.DataEmissaoRps := jsAux.AsISODate['DataEmissao'];

    with NFSe.IdentificacaoRps do
    begin
      Numero := jsAux.AsString['Numero'];
      Serie := jsAux.AsString['Serie'];
    end;
  end;
end;

procedure TNFSeR_Aspec.LerNota(aJson: TACBrJSONObject; FRps: Boolean);
var
  jsAux: TACBrJSONObject;
  OK: Boolean;
begin
  if Assigned(aJson) then
  begin
    with NFSe do
    begin
      Servico.CodigoMunicipio := aJson.AsString['MunicipioPrestacao'];

      if (FRps) then
      begin
        NaturezaOperacao := StrToNaturezaOperacao(OK, aJson.AsString['NaturezaOperacao']);
        jsAux := aJson.AsJSONObject['Atividade'];

        if Assigned(jsAux) then
        begin
          Servico.CodigoTributacaoMunicipio := jsAux.AsString['Codigo'];
          Servico.CodigoCnae := jsAux.AsString['CodigoCnae'];
          Servico.ItemListaServico := jsAux.AsString['CodigoLc116'];
          Servico.xItemListaServico := ItemListaServicoDescricao(Servico.ItemListaServico);
        end;
      end
      else
      begin
        Numero := aJson.AsString['NumeroNfse'];
        CodigoVerificacao := aJson.AsString['CodigoValidacao'];
        Link := aJson.AsString['LinkNfse'];
        Link := StringReplace(Link, '&amp;', '&', [rfReplaceAll]);

        jsAux := aJson.AsJSONObject['Rps'];

        if Assigned(jsAux) then
          SeriePrestacao := jsAux.AsString['Serie'];

        DataEmissao := aJson.AsISODateTime['DataEmissao'];
        Competencia := aJson.AsISODate['Competencia'];

        LerSituacaoNfse(aJson);
      end;

      OutrasInformacoes := aJson.AsString['Observacoes'];
      OutrasInformacoes := StringReplace(OutrasInformacoes, FpQuebradeLinha,
                                      sLineBreak, [rfReplaceAll, rfIgnoreCase]);

      jsAux := aJson.AsJSONObject['Valores'];

      if Assigned(jsAux) then
      begin
        if (aJson.AsString['IssRetido'] = 'N') then
          Servico.Valores.IssRetido := stNormal
        else
          Servico.Valores.IssRetido := stRetencao;

        Servico.Valores.ValorServicos := jsAux.AsFloat['ValorServicos'];
        Servico.Valores.ValorDeducoes := jsAux.AsFloat['ValorDeducoes'];
        Servico.Valores.ValorPis := jsAux.AsFloat['ValorPis'];
        Servico.Valores.ValorCofins := jsAux.AsFloat['ValorCofins'];
        Servico.Valores.ValorInss := jsAux.AsFloat['ValorInss'];
        Servico.Valores.ValorIr := jsAux.AsFloat['ValorIr'];
        Servico.Valores.ValorCsll := jsAux.AsFloat['ValorCsll'];
        Servico.Valores.OutrasRetencoes := jsAux.AsFloat['OutrasRetencoes'];
        Servico.Valores.DescontoIncondicionado := jsAux.AsFloat['DescontoIncondicionado'];
        Servico.Valores.DescontoCondicionado := jsAux.AsFloat['DescontoCondicionado'];
        Servico.Valores.BaseCalculo := jsAux.AsFloat['BaseCalculo'];
        Servico.Valores.Aliquota := jsAux.AsFloat['Aliquota'];
        Servico.Valores.ValorIss := jsAux.AsFloat['ValorIss'];
        Servico.Valores.ValorTotalTributos := jsAux.AsFloat['ValorTotalTributos'];
        ValorCredito := jsAux.AsFloat['ValorCredito'];

        Servico.Valores.RetencoesFederais := Servico.Valores.ValorPis +
          Servico.Valores.ValorCofins + Servico.Valores.ValorInss +
          Servico.Valores.ValorIr + Servico.Valores.ValorCsll;

        Servico.Valores.ValorLiquidoNfse := Servico.Valores.ValorServicos -
          (Servico.Valores.RetencoesFederais + Servico.Valores.ValorDeducoes +
           Servico.Valores.DescontoCondicionado +
           Servico.Valores.DescontoIncondicionado + Servico.Valores.ValorIssRetido);

        Servico.Valores.ValorTotalNotaFiscal := Servico.Valores.ValorServicos -
          Servico.Valores.DescontoCondicionado - Servico.Valores.DescontoIncondicionado;
      end;
    end;
  end;
end;

procedure TNFSeR_Aspec.LerSituacaoNfse(aJson: TACBrJSONObject);
var
  jsAux: TACBrJSONObject;
begin
  jsAux := aJson.AsJSONObject['Cancelamento'];

  if Assigned(jsAux) then
  begin
    NFSe.Situacao := StrToIntDef(aJson.AsString['SituacaoNfse'], 0);

    case NFSe.Situacao of
      -2:
        begin
          NFSe.SituacaoNfse := snCancelado;
          NFSe.MotivoCancelamento := aJson.AsString['Motivo'];
        end;
      -8: NFSe.SituacaoNfse := snNormal;
    end;
  end;
end;

procedure TNFSeR_Aspec.LerIdentificacaoPrestador(aJson: TACBrJSONObject);
var
  jsAux: TACBrJSONObject;
begin
  jsAux := aJson.AsJSONObject['Prestador'];

  if Assigned(jsAux) then
  begin
    NFSe.Prestador.IdentificacaoPrestador.InscricaoMunicipal := jsAux.AsString['InscricaoMunicipal'];
  end;
end;

procedure TNFSeR_Aspec.LerPrestador(aJson: TACBrJSONObject);
var
  jsAux: TACBrJSONObject;
begin
  jsAux := aJson.AsJSONObject['Prestador'];

  if Assigned(jsAux) then
  begin
    with NFSe.Prestador do
    begin
      RazaoSocial := jsAux.AsString['RazaoSocial'];
      NomeFantasia := jsAux.AsString['NomeFantasia'];

      IdentificacaoPrestador.CpfCnpj := OnlyNumber(jsAux.AsString['Cnpj']);
      IdentificacaoPrestador.CpfCnpj := PadLeft(IdentificacaoPrestador.CpfCnpj, 14, '0');
      IdentificacaoPrestador.InscricaoMunicipal := jsAux.AsString['InscricaoMunicipal'];

      LerEndereco(jsAux, Endereco);
      LerContato(jsAux, Contato);
    end;
  end;
end;

procedure TNFSeR_Aspec.LerTomador(aJson: TACBrJSONObject);
var
  jsAux: TACBrJSONObject;
  aValor: string;
begin
  jsAux := aJson.AsJSONObject['Tomador'];

  if Assigned(jsAux) then
  begin
    with NFSe.Tomador do
    begin
      RazaoSocial := jsAux.AsString['RazaoSocial'];
      NomeFantasia := jsAux.AsString['NomeFantasia'];

      IdentificacaoTomador.CpfCnpj := OnlyNumber(jsAux.AsString['NrDocumento']);
      aValor  := jsAux.AsString['TipoPessoa'];

      if ((aValor = 'J') or (aValor = '2')) then
      begin
        IdentificacaoTomador.CpfCnpj := PadLeft(IdentificacaoTomador.CpfCnpj, 14, '0');
      end
      else
      begin
        IdentificacaoTomador.CpfCnpj := PadLeft(IdentificacaoTomador.CpfCnpj, 11, '0');
        IdentificacaoTomador.Tipo := tpPF;
      end;

      LerEndereco(jsAux, Endereco);
      LerContato(jsAux, Contato);

      if Endereco.CodigoMunicipio = NFSe.Prestador.Endereco.CodigoMunicipio then
        IdentificacaoTomador.Tipo := tpPJdoMunicipio
      else
        IdentificacaoTomador.Tipo := tpPJforaMunicipio;
    end;
  end;
end;

procedure TNFSeR_Aspec.LerEndereco(aJson: TACBrJSONObject; aEndereco: TEndereco);
var
  jsAux: TACBrJSONObject;
  xUF: string;
begin
  jsAux := aJson.AsJSONObject['Endereco'];

  if Assigned(jsAux) then
  begin
    aEndereco.Endereco := jsAux.AsString['Logradouro'];
    aEndereco.Numero := jsAux.AsString['Numero'];
    aEndereco.Complemento := jsAux.AsString['Complemento'];
    aEndereco.Bairro := jsAux.AsString['Bairro'];

    aEndereco.CodigoMunicipio := jsAux.AsString['Municipio'];

    aEndereco.CodigoMunicipio := NormatizarCodigoMunicipio(aEndereco.CodigoMunicipio);

    aEndereco.xMunicipio := ObterNomeMunicipioUF(StrToIntDef(aEndereco.CodigoMunicipio, 0), xUF);

    if aEndereco.UF = '' then
      aEndereco.UF := xUF;

    aEndereco.CEP := jsAux.AsString['Cep'];
  end;
end;

procedure TNFSeR_Aspec.LerContato(aJson: TACBrJSONObject; aContato: TContato);
var
  jsAux: TACBrJSONObject;
begin
  jsAux := aJson.AsJSONObject['Contato'];

  if Assigned(jsAux) then
  begin
    aContato.Telefone := jsAux.AsString['Telefone'];
    aContato.Email := jsAux.AsString['Email'];
  end;
end;

procedure TNFSeR_Aspec.LerServicos(aJson: TACBrJSONObject);
var
  jsAux: TACBrJSONObject;
  jsArr: TACBrJSONArray;
  i: Integer;
begin
  jsArr := aJson.AsJSONArray['Servicos'];

  if Assigned(jsArr) then
  begin
    for i := 0 to jsArr.Count - 1 do
    begin
      jsAux := jsArr.ItemAsJSONObject[i];

      NFSe.Servico.ItemServico.New;
      with NFSe.Servico.ItemServico[i] do
      begin
        Unidade := jsAux.AsString['Unidade'];
        Descricao := jsAux.AsString['Descricao'];
        Descricao := StringReplace(Descricao, FpQuebradeLinha,
                                      sLineBreak, [rfReplaceAll, rfIgnoreCase]);
        Quantidade := jsAux.AsFloat['Quantidade'];
        ValorUnitario := jsAux.AsCurrency['ValorUnitario'];
        ValorTotal := ValorUnitario * Quantidade;
      end;
    end;
  end;
end;

function TNFSeR_Aspec.LerIni: Boolean;
var
  INIRec: TMemIniFile;
begin
  INIRec := TMemIniFile.Create('');

  // Usar o FpAOwner em vez de  FProvider

  try
    LerIniArquivoOuString(Arquivo, INIRec);

    FpTipoXML := INIRec.ReadString('IdentificacaoNFSe', 'TipoXML', '');

    if FpTipoXML = 'NFSE' then
      LerIniNfse(INIRec)
    else
      LerIniRps(INIRec);

  finally
    INIRec.Free;
  end;

  Result := True;
end;

procedure TNFSeR_Aspec.LerIniNfse(AINIRec: TMemIniFile);
begin
  NFSe.tpXML := txmlNFSe;

  LerINIIdentificacaoNFSe(AINIRec);
  LerINIInformacoesCancelamento(AINIRec);
  LerINIDadosPrestador(AINIRec);
  LerINIDadosTomador(AINIRec);
  LerINIIdentificacaoRps(AINIRec);
  LerINIDadosServico(AINIRec);
  LerINIDadosValores(AINIRec);
end;

procedure TNFSeR_Aspec.LerIniRps(AINIRec: TMemIniFile);
begin
  NFSe.tpXML := txmlRPS;

  LerINIIdentificacaoNFSe(AINIRec);
  LerINIIdentificacaoPrestador(AINIRec);
  LerINIDadosTomador(AINIRec);
  LerINIIdentificacaoRps(AINIRec);
  LerINIDadosServico(AINIRec);
  LerINIDadosValores(AINIRec);
end;

procedure TNFSeR_Aspec.LerINIIdentificacaoNFSe(AINIRec: TMemIniFile);
var
  sSecao: string;
  Ok: Boolean;
begin
  sSecao := 'IdentificacaoNFSe';

  if AINIRec.SectionExists(sSecao) then
  begin
    if NFSe.tpXML = txmlNFSe then
    begin
      NFSe.Numero := AINIRec.ReadString(sSecao, 'Numero', '');
      NFSe.CodigoVerificacao := AINIRec.ReadString(sSecao, 'CodigoVerificacao', '');
      NFSe.Link := AINIRec.ReadString(sSecao, 'Link', '');
      NFSe.ValorCredito := StrToFloatDef(AINIRec.ReadString(sSecao, 'ValorCredito', ''), 0);
      NFSe.SituacaoNFSe := StrToStatusNFSe(Ok, AINIRec.ReadString(sSecao, 'StatusNFSe', ''));
    end;
  end;
end;

procedure TNFSeR_Aspec.LerINIIdentificacaoPrestador(AINIRec: TMemIniFile);
var
  sSecao: string;
begin
  sSecao := 'Prestador';
  if AINIRec.SectionExists(sSecao) then
  begin
    NFSe.Prestador.IdentificacaoPrestador.CpfCnpj := AINIRec.ReadString(sSecao, 'CNPJ', '');
  end;
end;

procedure TNFSeR_Aspec.LerINIDadosTomador(AINIRec: TMemIniFile);
var
  sSecao: string;
begin
  sSecao := 'Tomador';
  if AINIRec.SectionExists(sSecao) then
  begin
    NFSe.Tomador.IdentificacaoTomador.CpfCnpj := AINIRec.ReadString(sSecao, 'CNPJCPF', '');
    NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal := AINIRec.ReadString(sSecao, 'InscricaoMunicipal', '');
    NFSe.Tomador.IdentificacaoTomador.InscricaoEstadual := AINIRec.ReadString(sSecao, 'InscricaoEstadual', '');

    NFSe.Tomador.RazaoSocial := AINIRec.ReadString(sSecao, 'RazaoSocial', '');
    NFSe.Tomador.NomeFantasia := AINIRec.ReadString(sSecao, 'NomeFantasia', '');

    NFSe.Tomador.Endereco.CodigoMunicipio := AINIRec.ReadString(sSecao, 'CodigoMunicipio', '');
    NFSe.Tomador.Endereco.Bairro := AINIRec.ReadString(sSecao, 'Bairro', '');
    NFSe.Tomador.Endereco.Endereco := AINIRec.ReadString(sSecao, 'Logradouro', '');
    NFSe.Tomador.Endereco.Numero := AINIRec.ReadString(sSecao, 'Numero', '');
    NFSe.Tomador.Endereco.Complemento := AINIRec.ReadString(sSecao, 'Complemento', '');
    NFSe.Tomador.Endereco.CodigoPais := AINIRec.ReadInteger(sSecao, 'CodigoPais', 0);

    NFSe.Tomador.Contato.Telefone := AINIRec.ReadString(sSecao, 'Telefone', '');
    NFSe.Tomador.Contato.Email := AINIRec.ReadString(sSecao, 'Email', '');
  end;
end;

procedure TNFSeR_Aspec.LerINIIdentificacaoRps(AINIRec: TMemIniFile);
var
  sSecao, sData: string;
  Ok: Boolean;
begin
  sSecao := 'IdentificacaoRps';
  if AINIRec.SectionExists(sSecao) then
  begin
    NFSe.IdentificacaoRps.Numero := AINIRec.ReadString(sSecao, 'Numero', '0');
    NFSe.IdentificacaoRps.Serie := AINIRec.ReadString(sSecao, 'Serie', '0');

    sData := AINIRec.ReadString(sSecao, 'DataEmissaoRps', '');
    if sData <> '' then
      NFSe.DataEmissaoRps := StringToDateTimeDef(sData, 0);

    if NFSe.tpXML = txmlNFSe then
    begin
      NFSe.NaturezaOperacao := StrToNaturezaOperacao(Ok, AINIRec.ReadString(sSecao, 'NaturezaOperacao', '0'));
      NFSe.SeriePrestacao := AINIRec.ReadString(sSecao, 'SeriePrestacao', '');
      NFSe.OutrasInformacoes := AINIRec.ReadString(sSecao, 'OutrasInformacoes', '');
      NFSe.OutrasInformacoes := StringReplace(NFSe.OutrasInformacoes,
                FpAOwner.ConfigGeral.QuebradeLinha, sLineBreak, [rfReplaceAll]);

      sData := AINIRec.ReadString(sSecao, 'Competencia', '');
      if sData <> '' then
        NFSe.Competencia := StringToDateTimeDef(sData, 0);
    end;
  end;
end;

procedure TNFSeR_Aspec.LerINIDadosServico(AINIRec: TMemIniFile);
var
  sSecao: string;
begin
  sSecao := 'Servico';
  if AINIRec.SectionExists(sSecao) then
  begin
    NFSe.Servico.ItemListaServico := AINIRec.ReadString(sSecao, 'ItemListaServico', '');
    NFSe.Servico.xItemListaServico := AINIRec.ReadString(sSecao, 'xItemListaServico', '');
    NFSe.Servico.Discriminacao := AINIRec.ReadString(sSecao, 'Discriminacao', '');
    NFSe.Servico.Discriminacao := StringReplace(NFSe.Servico.Discriminacao, FpAOwner.ConfigGeral.QuebradeLinha, sLineBreak, [rfReplaceAll]);
    NFSe.Servico.CodigoTributacaoMunicipio := AINIRec.ReadString(sSecao, 'CodigoTributacaoMunicipio', '');
    NFSe.Servico.CodigoCnae := AINIRec.ReadString(sSecao, 'CodigoCnae', '');

    if NFSe.tpXML = txmlNFSe then
    begin
      NFSe.Servico.CodigoMunicipio := AINIRec.ReadString(sSecao, 'CodigoMunicipio', '');
    end;
  end;
end;

procedure TNFSeR_Aspec.LerINIDadosValores(AINIRec: TMemIniFile);
var
  sSecao: string;
  Ok: Boolean;
begin
  sSecao := 'Valores';
  if AINIRec.SectionExists(sSecao) then
  begin
    NFSe.Servico.Valores.Aliquota := StringToFloatDef(AINIRec.ReadString(sSecao, 'Aliquota', ''), 0);
    NFSe.Servico.Valores.ISSRetido := FpAOwner.StrToSituacaoTributaria(Ok, AINIRec.ReadString(sSecao, 'ISSRetido', '0'));
    NFSe.Servico.Valores.ValorServicos := StringToFloatDef(AINIRec.ReadString(sSecao, 'ValorServicos', ''), 0);
    NFSe.Servico.Valores.ValorDeducoes := StringToFloatDef(AINIRec.ReadString(sSecao, 'ValorDeducoes', ''), 0);
    NFSe.Servico.Valores.DescontoCondicionado := StringToFloatDef(AINIRec.ReadString(sSecao, 'DescontoCondicionado', ''), 0);
    NFSe.Servico.Valores.DescontoIncondicionado := StringToFloatDef(AINIRec.ReadString(sSecao, 'DescontoIncondicionado', ''), 0);
    NFSe.Servico.Valores.BaseCalculo := StringToFloatDef(AINIRec.ReadString(sSecao, 'BaseCalculo', ''), 0);
    NFSe.Servico.Valores.ValorCofins := StringToFloatDef(AINIRec.ReadString(sSecao, 'ValorCofins', ''), 0);
    NFSe.Servico.Valores.ValorCsll := StringToFloatDef(AINIRec.ReadString(sSecao, 'ValorCsll', ''), 0);
    NFSe.Servico.Valores.ValorInss := StringToFloatDef(AINIRec.ReadString(sSecao, 'ValorInss', ''), 0);
    NFSe.Servico.Valores.ValorIr := StringToFloatDef(AINIRec.ReadString(sSecao, 'ValorIr', ''), 0);
    NFSe.Servico.Valores.ValorPis := StringToFloatDef(AINIRec.ReadString(sSecao, 'ValorPis', ''), 0);

    if NFSe.tpXML = txmlNFSe then
    begin
      NFSe.Servico.Valores.OutrasRetencoes := StringToFloatDef(AINIRec.ReadString(sSecao, 'OutrasRetencoes', ''), 0);
      NFSe.Servico.Valores.ValorIss := StringToFloatDef(AINIRec.ReadString(sSecao, 'ValorIss', ''), 0);
      NFSe.Servico.Valores.ValorTotalTributos := StringToFloatDef(AINIRec.ReadString(sSecao, 'ValorTotalTributos', ''), 0);
    end;

    NFSe.Servico.Valores.RetencoesFederais := NFSe.Servico.Valores.ValorPis +
          NFSe.Servico.Valores.ValorCofins + NFSe.Servico.Valores.ValorInss +
          NFSe.Servico.Valores.ValorIr + NFSe.Servico.Valores.ValorCsll;

    NFSe.Servico.Valores.ValorLiquidoNfse := NFSe.Servico.Valores.ValorServicos -
          (NFSe.Servico.Valores.RetencoesFederais + NFSe.Servico.Valores.ValorDeducoes +
           NFSe.Servico.Valores.DescontoCondicionado +
           NFSe.Servico.Valores.DescontoIncondicionado + NFSe.Servico.Valores.ValorIssRetido);

    NFSe.Servico.Valores.ValorTotalNotaFiscal := NFSe.Servico.Valores.ValorServicos -
          NFSe.Servico.Valores.DescontoCondicionado - NFSe.Servico.Valores.DescontoIncondicionado;
  end;
end;

procedure TNFSeR_Aspec.LerINIDadosPrestador(AINIRec: TMemIniFile);
var
  sSecao: string;
begin
  sSecao := 'DadosPrestador';
  if AINIRec.SectionExists(sSecao) then
  begin
    NFSe.Prestador.RazaoSocial := AINIRec.ReadString(sSecao, 'RazaoSocial', '');
    NFSe.Prestador.NomeFantasia := AINIRec.ReadString(sSecao, 'NomeFantasia', '');

    NFSe.Prestador.IdentificacaoPrestador.CpfCnpj := AINIRec.ReadString(sSecao, 'CNPJ', '');
    NFSe.Prestador.IdentificacaoPrestador.InscricaoMunicipal := AINIRec.ReadString(sSecao, 'InscricaoMunicipal', '');

    NFSe.Prestador.Endereco.Endereco := AINIRec.ReadString(sSecao, 'Logradouro', '');
    NFSe.Prestador.Endereco.Numero := AINIRec.ReadString(sSecao, 'Numero', '');
    NFSe.Prestador.Endereco.Complemento := AINIRec.ReadString(sSecao, 'Complemento', '');
    NFSe.Prestador.Endereco.Bairro := UTF8ToNativeString(AINIRec.ReadString(sSecao, 'Bairro', ''));
    NFSe.Prestador.Endereco.CodigoMunicipio := AINIRec.ReadString(sSecao, 'CodigoMunicipio', '');
    NFSe.Prestador.Endereco.xMunicipio := AINIRec.ReadString(sSecao, 'xMunicipio', '');
    NFSe.Prestador.Endereco.UF := AINIRec.ReadString(sSecao, 'UF', '');
    NFSe.Prestador.Endereco.CEP := AINIRec.ReadString(sSecao, 'CEP', '');

    NFSe.Prestador.Contato.Telefone := AINIRec.ReadString(sSecao, 'Telefone', '');
    NFSe.Prestador.Contato.Email := AINIRec.ReadString(sSecao, 'Email', '');
  end;
end;

procedure TNFSeR_Aspec.LerINIInformacoesCancelamento(AINIRec: TMemIniFile);
var
  sSecao: string;
begin
  sSecao := 'NFSeCancelamento';
  if AINIRec.SectionExists(sSecao) then
  begin
    NFSe.MotivoCancelamento := AINIRec.ReadString(sSecao, 'MotivoCancelamento', '');
  end;
end;

end.
