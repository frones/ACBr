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

unit Bauhaus.GravarJson;

interface

uses
  SysUtils, Classes, Variants, StrUtils, IniFiles,
  ACBrJSON,
  ACBrNFSeXGravarXml,
  ACBrNFSeXConversao;

type
  { TNFSeW_Bauhaus }

  TNFSeW_Bauhaus = class(TNFSeWClass)
  protected
    procedure Configuracao; override;

    function GerarDadosNota: String;

    function GerarAtividade: TACBrJSONObject;
    function GerarPrestador: TACBrJSONObject;
    function GerarTomador: TACBrJSONObject;
    function GerarTomadorEndereco: TACBrJSONObject;
    function GerarTomadorContato: TACBrJSONObject;
    function GerarRps: TACBrJSONObject;
    function GerarServicos: TACBrJSONArray;
    function GerarValores: TACBrJSONObject;

    // Gerar o arquivo INI
    procedure GerarINIIdentificacaoNFSe(AINIRec: TMemIniFile);
    procedure GerarINIIdentificacaoPrestador(AINIRec: TMemIniFile);
    procedure GerarINIDadosTomador(AINIRec: TMemIniFile);
    procedure GerarINIIdentificacaoRps(AINIRec: TMemIniFile);
    procedure GerarINIDadosServico(AINIRec: TMemIniFile);
    procedure GerarINIListaServico(AINIRec: TMemIniFile);
    procedure GerarINIDadosValores(AINIRec: TMemIniFile);
    procedure GerarINIDadosPrestador(AINIRec: TMemIniFile);
    procedure GerarINIInformacoesCancelamento(AINIRec: TMemIniFile);

    procedure GerarIniRps(AINIRec: TMemIniFile);
    procedure GerarIniNfse(AINIRec: TMemIniFile);
  public
    function GerarXml: Boolean; override;
    function GerarIni: string; override;
  end;

implementation

uses
  ACBrUtil.Base,
  ACBrUtil.Strings,
  ACBrConsts;

//==============================================================================
// Essa unit tem por finalidade exclusiva gerar o Json do RPS do provedor:
//     Bauhaus
//==============================================================================

{ TNFSeW_Bauhaus }

procedure TNFSeW_Bauhaus.Configuracao;
begin
  inherited Configuracao;

end;

function TNFSeW_Bauhaus.GerarXml: Boolean;
begin
  Configuracao;

  ListaDeAlertas.Clear;

  FDocument.Clear();

  FConteudoTxt.Clear;

  {$IFDEF FPC}
  FConteudoTxt.LineBreak := CRLF;
  {$ELSE}
    {$IFDEF DELPHI2006_UP}
    FConteudoTxt.LineBreak := CRLF;
    {$ENDIF}
  {$ENDIF}

  FConteudoTxt.Text := GerarDadosNota;
  Result := True;
end;

function TNFSeW_Bauhaus.GerarDadosNota: String;
var
  AJSon: TACBrJSONObject;
begin
  AJSon := TACBrJsonObject.Create;
  try
    AJSon
      .AddPairJSONObject('DadosNota', EmptyStr)
      .AsJSONObject['DadosNota']
        .AddPair('MunicipioPrestacao', StrToIntDef(NFSe.Servico.CodigoMunicipio, 0))
        .AddPair('NaturezaOperacao', StrToIntDef(NaturezaOperacaoToStr(NFSe.NaturezaOperacao), 0))
        .AddPair('IssRetido', IfThen((NFSe.Servico.Valores.IssRetido = stRetencao), 'S' , 'N'))
        .AddPair('Observacoes', NFSe.OutrasInformacoes)
        .AddPair('Atividade', GerarAtividade)
        .AddPair('Prestador', GerarPrestador)
        .AddPair('Tomador', GerarTomador)
        .AddPair('Rps', GerarRps)
        .AddPair('Servicos', GerarServicos)
        .AddPair('Valores', GerarValores);

    Result := AJSon.ToJSON;
  finally
    AJSon.Free;
  end;
end;

function TNFSeW_Bauhaus.GerarAtividade: TACBrJSONObject;
var
  wCodTribMun: String;
begin
  wCodTribMun := NFSe.Servico.CodigoTributacaoMunicipio;

  Result := TACBrJSONObject.Create
              .AddPair('Codigo', StrToIntDef(wCodTribMun, 0))
              .AddPair('CodigoCnae', NFSe.Servico.CodigoCnae);
end;

function TNFSeW_Bauhaus.GerarPrestador: TACBrJSONObject;
begin
  Result := TACBrJSONObject.Create
              .AddPair('InscricaoMunicipal', OnlyNumber(NFSe.Prestador.IdentificacaoPrestador.InscricaoMunicipal));
end;

function TNFSeW_Bauhaus.GerarTomador: TACBrJSONObject;
var
  wTipoPessoa: String;
begin
  case NFSe.Tomador.IdentificacaoTomador.Tipo of
    tpPF: wTipoPessoa := 'F';
    tpPJforaPais: wTipoPessoa := 'E';
  else
    wTipoPessoa := 'J';
  end;

  Result := TACBrJSONObject.Create
              .AddPair('TipoPessoa', wTipoPessoa)
              .AddPair('NrDocumento', NFSe.Tomador.IdentificacaoTomador.CpfCnpj)
              .AddPair('RazaoSocial', NFSe.Tomador.RazaoSocial)
              .AddPair('Endereco', GerarTomadorEndereco)
              .AddPair('Contato', GerarTomadorContato);
end;

function TNFSeW_Bauhaus.GerarTomadorEndereco: TACBrJSONObject;
begin
  Result := TACBrJSONObject.Create
              .AddPair('Logradouro', NFSe.Tomador.Endereco.Endereco)
              .AddPair('Numero', NFSe.Tomador.Endereco.Numero)
              .AddPair('Complemento', NFSe.Tomador.Endereco.Complemento)
              .AddPair('Bairro', NFSe.Tomador.Endereco.Bairro)
              .AddPair('Municipio', StrToIntDef(NFSe.Tomador.Endereco.CodigoMunicipio, 0))
              .AddPair('Cep', StrToIntDef(NFSe.Tomador.Endereco.CEP, 0));
end;

function TNFSeW_Bauhaus.GerarTomadorContato: TACBrJSONObject;
begin
  Result := TACBrJSONObject.Create
              .AddPair('Telefone', NFSe.Tomador.Contato.Telefone)
              .AddPair('Email', NFSe.Tomador.Contato.Email);
end;

function TNFSeW_Bauhaus.GerarRps: TACBrJSONObject;
begin
  Result := TACBrJSONObject.Create
              .AddPair('Numero', StrToIntDef(NFSe.IdentificacaoRps.Numero, 0))
              .AddPair('Serie', StrToIntDef(NFSe.IdentificacaoRps.Serie, 0))
              .AddPair('Tipo', 1)
              .AddPairISODate('DataEmissao', NFSe.DataEmissaoRps);
end;

function TNFSeW_Bauhaus.GerarServicos: TACBrJSONArray;
var
  jo: TACBrJSONObject;
  i: Integer;
begin
  Result := TACBrJSONArray.Create;
  for i := 0 to NFSe.Servico.ItemServico.Count - 1 do
  begin
    jo := TACBrJSONObject.Create
      .AddPair('Unidade', NFSe.Servico.ItemServico[i].Unidade)
      .AddPair('Quantidade', NFSe.Servico.ItemServico[i].Quantidade)
      .AddPair('Descricao', NFSe.Servico.ItemServico[i].Descricao)
      .AddPair('ValorUnitario', NFSe.Servico.ItemServico[i].ValorUnitario);
                                           
    Result.AddElementJSON(jo);
  end;
end;

function TNFSeW_Bauhaus.GerarValores: TACBrJSONObject;
begin
  with NFSe.Servico.Valores do
    Result := TACBrJSONObject.Create
      .AddPair('ValorServicos',  ValorServicos)
      .AddPair('ValorDeducoes', ValorDeducoes)
      .AddPair('ValorOutrasDeducoes', 0)
      .AddPair('ValorPis', ValorPis)
      .AddPair('ValorCofins', ValorCofins)
      .AddPair('ValorInss', ValorInss)
      .AddPair('ValorIr', ValorIr)
      .AddPair('ValorCsll', ValorCsll)
      .AddPair('OutrasRetencoes', OutrasRetencoes)
      .AddPair('DescontoIncondicionado', DescontoIncondicionado)
      .AddPair('DescontoCondicionado', DescontoCondicionado)
      .AddPair('BaseCalculo', BaseCalculo)
      .AddPair('Aliquota', Aliquota)
      .AddPair('ValorIss', ValorIss)
      .AddPair('ValorLiquidoNota', ValorLiquidoNfse)
      .AddPair('ValorTotalTributos', ValorTotalTributos)
      .AddPair('ValorCredito', NFSe.ValorCredito)
      .AddPair('ValorTotalNota', ValorLiquidoNfse);
end;

function TNFSeW_Bauhaus.GerarIni: string;
var
  INIRec: TMemIniFile;
  IniNFSe: TStringList;
begin
  Result:= '';
// Usar FpAOwner no lugar de FProvider

  INIRec := TMemIniFile.Create('');
  try
    if NFSe.tpXML = txmlRPS then
      GerarIniRps(INIRec)
    else
      GerarIniNfse(INIRec);
  finally
    IniNFSe := TStringList.Create;
    try
      INIRec.GetStrings(IniNFSe);
      INIRec.Free;

      Result := StringReplace(IniNFSe.Text, sLineBreak + sLineBreak, sLineBreak, [rfReplaceAll]);
    finally
      IniNFSe.Free;
    end;
  end;
end;

procedure TNFSeW_Bauhaus.GerarIniNfse(AINIRec: TMemIniFile);
begin
  GerarINIIdentificacaoNFSe(AINIRec);
  GerarINIInformacoesCancelamento(AINIRec);
  GerarINIDadosPrestador(AINIRec);
  GerarINIDadosTomador(AINIRec);
  GerarINIIdentificacaoRps(AINIRec);
  GerarINIDadosServico(AINIRec);
  GerarINIDadosValores(AINIRec);
end;

procedure TNFSeW_Bauhaus.GerarIniRps(AINIRec: TMemIniFile);
begin
  GerarINIIdentificacaoNFSe(AINIRec);
  GerarINIIdentificacaoPrestador(AINIRec);
  GerarINIDadosTomador(AINIRec);
  GerarINIIdentificacaoRps(AINIRec);
  GerarINIDadosServico(AINIRec);
  GerarINIDadosValores(AINIRec);
end;

procedure TNFSeW_Bauhaus.GerarINIIdentificacaoNFSe(AINIRec: TMemIniFile);
var
  sSecao: string;
begin
  sSecao:= 'IdentificacaoNFSe';

  if NFSe.tpXML = txmlRPS then
    AINIRec.WriteString(sSecao, 'TipoXML', 'RPS')
  else
  begin
    AINIRec.WriteString(sSecao, 'TipoXML', 'NFSE');

    AINIRec.WriteString(sSecao, 'Numero', NFSe.Numero);
    AINIRec.WriteString(sSecao, 'CodigoVerificacao', NFSe.CodigoVerificacao);
    AINIRec.WriteString(sSecao, 'Link', NFSe.Link);
    AINIRec.WriteString(sSecao, 'StatusNFSe', StatusNFSeToStr(NFSe.SituacaoNfse));
  end;

  AINIRec.WriteFloat(sSecao, 'ValorCredito', NFSe.ValorCredito);
end;

procedure TNFSeW_Bauhaus.GerarINIIdentificacaoPrestador(AINIRec: TMemIniFile);
var
  sSecao: string;
begin
  sSecao:= 'Prestador';

  AINIRec.WriteString(sSecao, 'InscricaoMunicipal', NFSe.Prestador.IdentificacaoPrestador.InscricaoMunicipal);
end;

procedure TNFSeW_Bauhaus.GerarINIDadosTomador(AINIRec: TMemIniFile);
var
  sSecao: string;
begin
  sSecao:= 'Tomador';

  AINIRec.WriteString(sSecao, 'Tipo', FpAOwner.TipoPessoaToStr(NFSe.Tomador.IdentificacaoTomador.Tipo));
  AINIRec.WriteString(sSecao, 'CNPJCPF', NFSe.Tomador.IdentificacaoTomador.CpfCnpj);
  AINIRec.WriteString(sSecao, 'RazaoSocial', NFSe.Tomador.RazaoSocial);
  AINIRec.WriteString(sSecao, 'Logradouro', NFSe.Tomador.Endereco.Endereco);
  AINIRec.WriteString(sSecao, 'Numero', NFSe.Tomador.Endereco.Numero);
  AINIRec.WriteString(sSecao, 'Complemento', NFSe.Tomador.Endereco.Complemento);
  AINIRec.WriteString(sSecao, 'Bairro', NFSe.Tomador.Endereco.Bairro);
  AINIRec.WriteString(sSecao, 'CodigoMunicipio', NFSe.Tomador.Endereco.CodigoMunicipio);
  AINIRec.WriteString(sSecao, 'CEP', NFSe.Tomador.Endereco.CEP);
  AINIRec.WriteString(sSecao, 'Telefone', NFSe.Tomador.Contato.Telefone);
  AINIRec.WriteString(sSecao, 'Email', NFSe.Tomador.Contato.Email);
end;

procedure TNFSeW_Bauhaus.GerarINIIdentificacaoRps(AINIRec: TMemIniFile);
var
  sSecao: string;
begin
  sSecao := 'IdentificacaoRps';

  AINIRec.WriteString(sSecao, 'NaturezaOperacao', NaturezaOperacaoToStr(NFSe.NaturezaOperacao));
  AINIRec.WriteString(sSecao, 'OutrasInformacoes', StringReplace(NFSe.OutrasInformacoes, sLineBreak, FpAOwner.ConfigGeral.QuebradeLinha, [rfReplaceAll]));
  AINIRec.WriteString(sSecao, 'Numero', NFSe.IdentificacaoRps.Numero);
  AINIRec.WriteString(sSecao, 'Serie', NFSe.IdentificacaoRps.Serie);
  AINIRec.WriteString(sSecao, 'DataEmissaoRps', DateTimeToStr(NFSe.DataEmissaoRps));
  {

   if NFSe.tpXML = txmlNFSe then
  begin
    AINIRec.WriteString(sSecao, 'SeriePrestacao', NFSe.SeriePrestacao);

    if NFSe.Competencia > 0 then
      AINIRec.WriteDate(sSecao, 'Competencia', NFSe.Competencia)
    else
      AIniRec.WriteDate(sSecao, 'Competencia', Now);
  end;
  }
end;

procedure TNFSeW_Bauhaus.GerarINIDadosServico(AINIRec: TMemIniFile);
var
  sSecao: string;
begin
  sSecao := 'Servico';

  AINIRec.WriteString(sSecao, 'CodigoMunicipio', NFSe.Servico.CodigoMunicipio);
  AINIRec.WriteString(sSecao, 'CodigoCnae', NFSe.Servico.CodigoCnae);
  AINIRec.WriteString(sSecao, 'CodigoTributacaoMunicipio', NFSe.Servico.CodigoTributacaoMunicipio);
  {
  AINIRec.WriteString(sSecao, 'ItemListaServico', NFSe.Servico.ItemListaServico);
  AINIRec.WriteString(sSecao, 'xItemListaServico', NFSe.Servico.xItemListaServico);
  AINIRec.WriteString(sSecao, 'Discriminacao',
    ChangeLineBreak(NFSe.Servico.Discriminacao, FpAOwner.ConfigGeral.QuebradeLinha));

  if NFSe.tpXML = txmlNFSe then
  begin
  end;
  }
end;

procedure TNFSeW_Bauhaus.GerarINIListaServico(AINIRec: TMemIniFile);
var
  I: Integer;
  sSecao: string;
begin
  for I := 0 to NFSe.Servico.ItemServico.Count - 1 do
  begin
    sSecao:= 'Itens' + IntToStrZero(I + 1, 3);

    AINIRec.WriteString(sSecao, 'Descricao',
      ChangeLineBreak(NFSe.Servico.ItemServico.Items[I].Descricao, FpAOwner.ConfigGeral.QuebradeLinha));
    AINIRec.WriteString(sSecao, 'Unidade', NFSe.Servico.ItemServico.Items[I].Unidade);
    AINIRec.WriteFloat(sSecao, 'Quantidade', NFSe.Servico.ItemServico.Items[I].Quantidade);
    AINIRec.WriteFloat(sSecao, 'ValorUnitario', NFSe.Servico.ItemServico.Items[I].ValorUnitario);
  end;
end;

procedure TNFSeW_Bauhaus.GerarINIDadosValores(AINIRec: TMemIniFile);
var
  sSecao: string;
begin
  sSecao := 'Valores';

  AINIRec.WriteString(sSecao, 'ISSRetido', FpAOwner.SituacaoTributariaToStr(NFSe.Servico.Valores.ISSRetido));
  AINIRec.WriteFloat(sSecao, 'ValorServicos', NFSe.Servico.Valores.ValorServicos);
  AINIRec.WriteFloat(sSecao, 'ValorDeducoes', NFSe.Servico.Valores.ValorDeducoes);
  AINIRec.WriteFloat(sSecao, 'ValorPis', NFSe.Servico.Valores.ValorPis);
  AINIRec.WriteFloat(sSecao, 'ValorCofins', NFSe.Servico.Valores.ValorCofins);
  AINIRec.WriteFloat(sSecao, 'ValorInss', NFSe.Servico.Valores.ValorInss);
  AINIRec.WriteFloat(sSecao, 'ValorIr', NFSe.Servico.Valores.ValorIr);
  AINIRec.WriteFloat(sSecao, 'ValorCsll', NFSe.Servico.Valores.ValorCsll);
  AINIRec.WriteFloat(sSecao, 'OutrasRetencoes', NFSe.Servico.Valores.OutrasRetencoes);
  AINIRec.WriteFloat(sSecao, 'DescontoCondicionado', NFSe.Servico.Valores.DescontoCondicionado);
  AINIRec.WriteFloat(sSecao, 'DescontoIncondicionado', NFSe.Servico.Valores.DescontoIncondicionado);
  AINIRec.WriteFloat(sSecao, 'Aliquota', NFSe.Servico.Valores.Aliquota);
  AINIRec.WriteFloat(sSecao, 'BaseCalculo', NFSe.Servico.Valores.BaseCalculo);
  AINIRec.WriteFloat(sSecao, 'ValorIss', NFSe.Servico.Valores.ValorIss);
  AINIRec.WriteFloat(sSecao, 'ValorTotalTributos', NFSe.Servico.Valores.ValorTotalTributos);
  AINIRec.WriteFloat(sSecao, 'ValorLiquidoNfse', NFSe.Servico.Valores.ValorLiquidoNfse);
end;

procedure TNFSeW_Bauhaus.GerarINIDadosPrestador(AINIRec: TMemIniFile);
begin

end;

procedure TNFSeW_Bauhaus.GerarINIInformacoesCancelamento(AINIRec: TMemIniFile);
begin

end;

(*
procedure TNFSeW_Aspec.GerarINIIdentificacaoNFSe(AINIRec: TMemIniFile);
end;

procedure TNFSeW_Aspec.GerarINIIdentificacaoPrestador(AINIRec: TMemIniFile);
var
  sSecao: string;
begin
  sSecao := 'Prestador';

  AINIRec.WriteString(sSecao, 'CNPJ', NFSe.Prestador.IdentificacaoPrestador.CpfCnpj);
end;

procedure TNFSeW_Aspec.GerarINIDadosTomador(AINIRec: TMemIniFile);
var
  sSecao: string;
begin
  sSecao := 'Tomador';

  AINIRec.WriteString(sSecao, 'CNPJCPF', NFSe.Tomador.IdentificacaoTomador.CpfCnpj);
  AINIRec.WriteString(sSecao, 'InscricaoMunicipal', NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal);
  AINIRec.WriteString(sSecao, 'InscricaoEstadual', NFSe.Tomador.IdentificacaoTomador.InscricaoEstadual);

  AINIRec.WriteString(sSecao, 'RazaoSocial', NFSe.Tomador.RazaoSocial);
  AINIRec.WriteString(sSecao, 'NomeFantasia', NFSe.Tomador.NomeFantasia);

  AINIRec.WriteString(sSecao, 'CodigoMunicipio', NFSe.Tomador.Endereco.CodigoMunicipio);
  AINIRec.WriteString(sSecao, 'Bairro', NFSe.Tomador.Endereco.Bairro);
  AINIRec.WriteString(sSecao, 'Logradouro', NFSe.Tomador.Endereco.Endereco);
  AINIRec.WriteString(sSecao, 'Numero', NFSe.Tomador.Endereco.Numero);
  AINIRec.WriteString(sSecao, 'Complemento', NFSe.Tomador.Endereco.Complemento);
  AINIRec.WriteInteger(sSecao, 'CodigoPais', NFSe.Tomador.Endereco.CodigoPais);

  AINIRec.WriteString(sSecao, 'Telefone', NFSe.Tomador.Contato.Telefone);
  AINIRec.WriteString(sSecao, 'Email', NFSe.Tomador.Contato.Email);
end;

procedure TNFSeW_Aspec.GerarINIDadosValores(AINIRec: TMemIniFile);
end;

procedure TNFSeW_Aspec.GerarINIDadosPrestador(AINIRec: TMemIniFile);
var
  sSecao: string;
begin
  sSecao:= 'DadosPrestador';

  AINIRec.WriteString(sSecao, 'RazaoSocial', NFSe.Prestador.RazaoSocial);
  AINIRec.WriteString(sSecao, 'NomeFantasia', NFSe.Prestador.NomeFantasia);
  AINIRec.WriteString(sSecao, 'CNPJ', NFSe.Prestador.IdentificacaoPrestador.CpfCnpj);
  AINIRec.WriteString(sSecao, 'InscricaoMunicipal', NFSe.Prestador.IdentificacaoPrestador.InscricaoMunicipal);

  AINIRec.WriteString(sSecao, 'Logradouro', NFSe.Prestador.Endereco.Endereco);
  AINIRec.WriteString(sSecao, 'Numero', NFSe.Prestador.Endereco.Numero);
  AINIRec.WriteString(sSecao, 'Complemento', NFSe.Prestador.Endereco.Complemento);
  AINIRec.WriteString(sSecao, 'Bairro', NFSe.Prestador.Endereco.Bairro);
  AINIRec.WriteString(sSecao, 'CodigoMunicipio', NFSe.Prestador.Endereco.CodigoMunicipio);
  AINIRec.WriteString(sSecao, 'xMunicipio', NFSe.Prestador.Endereco.xMunicipio);
  AINIRec.WriteString(sSecao, 'UF',  NFSe.Prestador.Endereco.UF);
  AINIRec.WriteString(sSecao, 'CEP', NFSe.Prestador.Endereco.CEP);

  AINIRec.WriteString(sSecao, 'Telefone', NFSe.Prestador.Contato.Telefone);
  AINIRec.WriteString(sSecao, 'Email', NFSe.Prestador.Contato.Email);
end;

procedure TNFSeW_Aspec.GerarINIInformacoesCancelamento(AINIRec: TMemIniFile);
var
  sSecao: string;
begin
  sSecao := 'NFSeCancelamento';
  if NFSe.MotivoCancelamento <> '' then
  begin
    AINIRec.WriteString(sSecao, 'MotivoCancelamento', NFSe.MotivoCancelamento);
  end;
end;

*)
end.
