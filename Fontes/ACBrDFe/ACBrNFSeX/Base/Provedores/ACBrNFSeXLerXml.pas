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

unit ACBrNFSeXLerXml;

interface

uses
  SysUtils, Classes, IniFiles, types,
  ACBrXmlBase, ACBrXmlReader,
  ACBrXmlDocument,
  ACBrNFSeXInterface, ACBrNFSeXClass, ACBrNFSeXConversao;

type
  { TNFSeRClass }

  TNFSeRClass = class(TACBrXmlReader)
  private
    FNFSe: TNFSe;
    FProvedor: TnfseProvedor;
    FtpXML: TtpXML;
    FAmbiente: TACBrTipoAmbiente;
    FIniParams: TMemIniFile;
    FParamsTab: TStrings;
    FIniParamsTab: TMemIniFile;

    procedure SetParamsTab(const Value: TStrings);
  protected
    FpAOwner: IACBrNFSeXProvider;
    FpQuebradeLinha: string;
    FpIniParamsTabCarregado: Boolean;

    procedure Configuracao; virtual;

    function NormatizarItemListaServico(const Codigo: string): string;
    function ItemListaServicoDescricao(const Codigo: string): string;
    function TipodeXMLLeitura(const aArquivo: string): TtpXML; virtual;
    function NormatizarXml(const aXml: string): string; virtual;
    function NormatizarAliquota(const Aliquota: Double): Double;
    function LerLinkURL: string;
    function ObterNomeMunicipioUF(ACodigoMunicipio: Integer; var xUF: string): string;
    function LerParamsTabIniServicos: AnsiString;
    function LerParamsTabInterno: AnsiString;
    function LerDescricaoServico(const ACodigo: string): string;

    procedure VerificarSeConteudoEhLista(const aDiscriminacao: string);
    procedure LerListaJson(const aDiscriminacao: string);
    procedure LerListaTabulada(const aDiscriminacao: string);

    procedure LerParamsTabIni(ApenasSeNaoLido: Boolean);
    procedure LerParamsTab;
  public
    constructor Create(AOwner: IACBrNFSeXProvider);
    destructor Destroy; override;

    function LerXml: Boolean; Override;
    procedure LerCampoLink;

    property NFSe: TNFSe             read FNFSe     write FNFSe;
    property Provedor: TnfseProvedor read FProvedor write FProvedor;
    property tpXML: TtpXML           read FtpXML    write FtpXML;
    property Ambiente: TACBrTipoAmbiente read FAmbiente write FAmbiente default taHomologacao;
    property IniParams: TMemIniFile read FIniParams write FIniParams;
    property ParamsTab: TStrings read FParamsTab write SetParamsTab;
  end;

implementation

uses
  synautil,
  StrUtils, StrUtilsEx,
  ACBrJSON,
  ACBrUtil.Strings, ACBrUtil.XMLHTML, ACBrUtil.FilesIO,
  ACBrDFeException;

{ TNFSeRClass }

procedure TNFSeRClass.Configuracao;
begin
  FpQuebradeLinha := FpAOwner.ConfigGeral.QuebradeLinha;

  FpAOwner.ConfigGeral.ImprimirLocalPrestServ := not FpAOwner.ConfigGeral.Params.TemParametro('NaoImprimirLocalPrestServ');
end;

constructor TNFSeRClass.Create(AOwner: IACBrNFSeXProvider);
begin
  FpAOwner := AOwner;
  FParamsTab := TStringList.Create;

  FIniParamsTab := TMemIniFile.Create('');
  FpIniParamsTabCarregado := False;

  Configuracao;
end;

destructor TNFSeRClass.Destroy;
begin
  FParamsTab.Free;
  FIniParamsTab.Free;
  inherited;
end;

function TNFSeRClass.ItemListaServicoDescricao(const Codigo: string): string;
var
  xCodigo: string;
begin
  xCodigo := OnlyNumber(Codigo);

  if FpAOwner.ConfigGeral.TabServicosExt then
    Result := ObterDescricaoServico(xCodigo)
  else
    Result := LerDescricaoServico(xCodigo);
end;

function TNFSeRClass.LerXml: Boolean;
begin
  Result := False;
  raise EACBrDFeException.Create(ClassName + '.LerXml, não implementado');
end;

function TNFSeRClass.NormatizarAliquota(const Aliquota: Double): Double;
begin
  if Aliquota < 1 then
    Result := Aliquota * 100
  else
    Result := Aliquota;
end;

function TNFSeRClass.NormatizarItemListaServico(const Codigo: string): string;
var
  Item: Integer;
  xCodigo: string;
begin
  Result := Codigo;

  if Length(Codigo) <= 5 then
  begin
    xCodigo := Codigo;

    Item := StrToIntDef(OnlyNumber(xCodigo), 0);
    if Item < 100 then
      Item := Item * 100 + 1;

    xCodigo := FormatFloat('0000', Item);

    Result := Copy(xCodigo, 1, 2) + '.' + Copy(xCodigo, 3, 2);
  end;
end;

function TNFSeRClass.NormatizarXml(const aXml: string): string;
begin
{$IfDef FPC}
  Result := aXml;
{$Else}
  Result := ParseText(aXml);
//  Result := ParseText(aXml, True, False);
  Result := FastStringReplace(Result, '&', '&amp;', [rfReplaceAll]);
{$EndIf}
end;

function TNFSeRClass.ObterNomeMunicipioUF(ACodigoMunicipio: Integer; var xUF: string): string;
var
  CodIBGE: string;
begin
  CodIBGE := IntToStr(ACodigoMunicipio);

  xUF := IniParams.ReadString(CodIBGE, 'UF', '');
  Result := IniParams.ReadString(CodIBGE, 'Nome', '');
end;

procedure TNFSeRClass.SetParamsTab(const Value: TStrings);
begin
  FParamsTab.Assign(Value);
end;

function TNFSeRClass.TipodeXMLLeitura(const aArquivo: string): TtpXML;
var
  aXML: string;
begin
  aXML := RemoverPrefixosDesnecessarios(aArquivo);

  if (Pos('/infnfse>', LowerCase(aXML)) > 0) then
    Result := txmlNFSe
  else
    Result := txmlRPS;
end;

procedure TNFSeRClass.VerificarSeConteudoEhLista(const aDiscriminacao: string);
begin
  FpAOwner.ConfigGeral.DetalharServico := False;

  if (Pos('[', aDiscriminacao) > 0) and (Pos(']', aDiscriminacao) > 0) and
     (Pos('{', aDiscriminacao) > 0) and (Pos('}', aDiscriminacao) > 0) then
  begin
    FpAOwner.ConfigGeral.DetalharServico := True;

    if Pos('":', aDiscriminacao) > 0 then
      LerListaJson(aDiscriminacao)
    else
      LerListaTabulada(aDiscriminacao);
  end;
end;

function TNFSeRClass.LerLinkURL: string;
var
  LinkNFSeParam: TLinkNFSeParam;
begin
  LinkNFSeParam := TLinkNFSeParam.Create;
  try
    LinkNFSeParam.Ambiente := Integer(FpAOwner.ConfigGeral.Ambiente);
    LinkNFSeParam.ProLinkURL := FpAOwner.ConfigWebServices.Producao.LinkURL;
    LinkNFSeParam.HomLinkURL := FpAOwner.ConfigWebServices.Homologacao.LinkURL;
    LinkNFSeParam.xMunicipio := FpAOwner.ConfigGeral.xMunicipio;

    Result := NFSe.LinkNFSe(LinkNFSeParam);
  finally
    LinkNFSeParam.Free;
  end;
end;

procedure TNFSeRClass.LerListaJson(const aDiscriminacao: string);
var
  xDiscriminacao: string;
  json, jsonItem: TACBrJsonObject;
  i: Integer;
begin
  xDiscriminacao := '{"a": ' + aDiscriminacao + '}';
  Json := TACBrJsonObject.Parse(xDiscriminacao);

  for i := 0 to json.AsJSONArray['a'].Count -1 do
  begin
    jsonItem := json.AsJSONArray['a'].ItemAsJSONObject[i];

    with NFSe.Servico.ItemServico.New do
    begin
      Descricao := jsonItem.AsString['Descricao'];
      ValorUnitario := jsonItem.AsCurrency['ValorUnitario'];
      Quantidade := jsonItem.AsCurrency['Quantidade'];
      ValorTotal := jsonItem.AsCurrency['ValorTotal'];
    end;
  end;
end;

procedure TNFSeRClass.LerListaTabulada(const aDiscriminacao: string);
var
  xDiscriminacao, xDescricao, xItemServico: string;
  fQuantidade, fValorUnitario, fValorServico, fValorBC, fAliquota: Double;
  i, j: Integer;

  function ExtraiValorCampo(aCampo: string; aCampoNumerico: Boolean): string;
  begin
    i := PosEx(aCampo, xDiscriminacao, j) + Length(aCampo) + 1;

    if i = Length(aCampo) + 1 then
      Result := ''
    else
    begin
      j := PosEx(']', xDiscriminacao, i);
      Result := Copy(xDiscriminacao, i, j-i);

      if aCampoNumerico then
        Result := StringReplace(Result, '.', ',', [rfReplaceAll])
    end;
  end;
begin
  xDiscriminacao := aDiscriminacao;
  J := 1;

  while true do
  begin
    xDescricao := ExtraiValorCampo('Descricao', False);

    if xDescricao = '' then
      Break;

    xItemServico := ExtraiValorCampo('ItemServico', False);
    fQuantidade := StrToFloatDef(ExtraiValorCampo('Quantidade', True), 0);
    fValorUnitario := StrToFloatDef(ExtraiValorCampo('ValorUnitario', True), 0);
    fValorServico := StrToFloatDef(ExtraiValorCampo('ValorServico', True), 0);
    fValorBC := StrToFloatDef(ExtraiValorCampo('ValorBaseCalculo', True), 0);
    fAliquota := StrToFloatDef(ExtraiValorCampo('Aliquota', True), 0);

    with NFSe.Servico.ItemServico.New do
    begin
      Descricao := xDescricao;
      ItemListaServico := xItemServico;
      Quantidade := fQuantidade;
      ValorUnitario := fValorUnitario;
      ValorTotal := fValorServico;
      ValorBCINSS := fValorBC;
      BaseCalculo := fValorBC;
      Aliquota := fAliquota;
    end;
  end;
end;

procedure TNFSeRClass.LerParamsTab;
var
  ConteudoParams: AnsiString;
begin
  ConteudoParams := LerParamsTabIniServicos;

  if ConteudoParams = '' then
    ConteudoParams := LerParamsTabInterno;

  FParamsTab.Text := ConteudoParams;
end;

procedure TNFSeRClass.LerParamsTabIni(ApenasSeNaoLido: Boolean);
begin
  if ApenasSeNaoLido and FpIniParamsTabCarregado then
    exit;

  if ParamsTab.Count = 0 then
    LerParamsTab;

  FIniParamsTab.SetStrings(ParamsTab);
  FpIniParamsTabCarregado := True;
end;

function TNFSeRClass.LerParamsTabIniServicos: AnsiString;
var
  ArqIni: String;
  FS: TFileStream;
begin
  Result := '';
  ArqIni := Trim(FpAOwner.ConfigGeral.IniTabServicos);

  if (ArqIni <> '') and FileExists(ArqIni) then
  begin
    FS := TFileStream.Create(ArqIni, fmOpenRead or fmShareDenyNone);  // Thread Safe
    try
      FS.Position := 0;
      Result := ReadStrFromStream(FS, FS.Size);
    finally
      FS.Free;
    end;
  end;
end;

function TNFSeRClass.LerParamsTabInterno: AnsiString;
var
  RS: TResourceStream;
begin
  Result := '';

  RS := TResourceStream.Create(HInstance, 'TabServicos', RT_RCDATA);
  try
    RS.Position := 0;
    Result := ReadStrFromStream(RS, RS.Size);
  finally
    RS.Free;
  end;
end;

function TNFSeRClass.LerDescricaoServico(const ACodigo: string): string;
begin
  FIniParamsTab.SetStrings(ParamsTab);

  Result := FIniParamsTab.ReadString(OnlyNumber(ACodigo), 'Descricao', '');
end;

procedure TNFSeRClass.LerCampoLink;
var
  Link: string;
  i: Integer;

function ExtrairURL(PosIni: Integer; Texto: string): string;
var
  j: Integer;
  Url: string;
begin
  Url := '';
  j := PosIni;

  while (j <= Length(Texto)) and (Texto[j] <> ' ') do
  begin
    Url := Url + Texto[j];
    Inc(j);
  end;

  Result := Url;
end;

begin
  if NFSe.Link = '' then
  begin
    Link := '';

    i := pos('http://', LowerCase(NFSe.OutrasInformacoes));

    if i > 0 then
      Link := ExtrairURL(i, NFSe.OutrasInformacoes)
    else
    begin
      i := pos('https://', LowerCase(NFSe.OutrasInformacoes));

      if i > 0 then
        Link := ExtrairURL(i, NFSe.OutrasInformacoes);
    end;

    if Link = '' then
      Link := LerLinkURL;

    NFSe.Link := Trim(Link);
  end;
end;

end.
