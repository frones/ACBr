{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Régys Silveira                                                 }
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

unit ACBrIBPTax;

interface

uses
  SysUtils, Variants, Classes,
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$Else}
   Contnrs,
  {$IfEnd}
  ACBrBase, ACBrSocket;

type
  EACBrIBPTax = class(Exception);

  TACBrIBPTaxExporta = (exTXT, exCSV, exDSV, exXML, exHTML);

  TACBrIBPTaxTabela = (tabNCM, tabNBS, tabLST);

  TACBrIBPTaxErroImportacao = procedure(const ALinha: String; const AErro: String) of object;

  TACBrIBPTaxProdutoDTO = record
    Codigo    : String;
    UF        : String;
    EX        : Integer;
    Descricao : String;
    Nacional  : Double;
    Estadual  : Double;
    Municipal : Double;
    Importado : Double;

    VigenciaInicio: TDateTime;
    VigenciaFim: TDateTime;
    Chave: String;
    Versao: String;
    Fonte: String;

    Valor: Double;
    ValorTributoNacional: Currency;
    ValorTributoEstadual: Currency;
    ValorTributoMunicipal: Currency;
    ValorTributoImportado: Currency;

    JSON      : String;
  end;

  TACBrIBPTaxServicoDTO = record
    Codigo    : String;
    UF        : String;
    Descricao : String;
    Tipo      : String;
    Nacional  : Double;
    Estadual  : Double;
    Municipal : Double;
    Importado : Double;

    VigenciaInicio: TDateTime;
    VigenciaFim: TDateTime;
    Chave: String;
    Versao: String;
    Fonte: String;

    Valor: Double;
    ValorTributoNacional: Currency;
    ValorTributoEstadual: Currency;
    ValorTributoMunicipal: Currency;
    ValorTributoImportado: Currency;

    JSON      : String;
  end;

  TACBrIBPTaxRegistro = class
  private
    FTabela: TACBrIBPTaxTabela;
    FExcecao: String;
    FNCM: string;
    FDescricao: string;
    FEstadual: Double;
    FFederalNacional: Double;
    FFederalImportado: Double;
    FMunicipal: Double;
  public
    property NCM: string read FNCM write FNCM;
    property Descricao: string read FDescricao write FDescricao;
    property Excecao: String read FExcecao write FExcecao;
    property Tabela: TACBrIBPTaxTabela read FTabela write FTabela;
    property FederalNacional: Double read FFederalNacional write FFederalNacional;
    property FederalImportado: Double read FFederalImportado write FFederalImportado;
    property Estadual: Double read FEstadual write FEstadual;
    property Municipal: Double read FMunicipal write FMunicipal;
  end;

  TACBrIBPTaxRegistros = class(TObjectList{$IfDef HAS_SYSTEM_GENERICS}<TACBrIBPTaxRegistro>{$EndIf})
  private
    function GetItem(Index: integer): TACBrIBPTaxRegistro;
    procedure SetItem(Index: integer; const Value: TACBrIBPTaxRegistro);
  public
    function New: TACBrIBPTaxRegistro;
    property Items[Index: integer]: TACBrIBPTaxRegistro read GetItem write SetItem; default;
  end;

  { TACBrIBPTax }

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrIBPTax = class(TACBrHTTP)
  private
    FArquivo: TStringList;
    FChaveArquivo: String;
    FVersaoArquivo: String;
    FURLDownload: String;
    FItens: TACBrIBPTaxRegistros;
    FVigenciaFim: TDateTime;
    FVigenciaInicio: TDateTime;
    FOnErroImportacao: TACBrIBPTaxErroImportacao;
    FFonte: string;
    FToken: String;
    FCNPJEmpresa: String;
    procedure ExportarCSV(const AArquivo: String);
    procedure ExportarDSV(const AArquivo: String);
    procedure ExportarHTML(const AArquivo: String);
    procedure ExportarXML(const AArquivo: String);
    procedure ExportarTXT(const AArquivo: String);
    function PopularItens: Integer;
    function TraduzStrToAnsi(const AValue : String) : string;
    procedure EventoErroImportacao(const Alinha, AErro: String);
  public
    destructor Destroy; override;
    constructor Create(AOwner: TComponent); override;

    function DownloadTabela: Boolean;

    function AbrirTabela(const AFileName: TFileName): Boolean; overload;
    function AbrirTabela(const AConteudoArquivo: TStream): Boolean; overload;
    function AbrirTabela(const AConteudoArquivo: TStringList): Boolean; overload;
    procedure Exportar(const AArquivo: String; ATipo: TACBrIBPTaxExporta); overload;
    procedure Exportar(const AArquivo, ADelimitador: String; const AQuoted: Boolean); overload;
    function Procurar(const ACodigo: String; var ex, descricao: String;
      var tabela: Integer; var aliqFedNac, aliqFedImp, aliqEstadual,
      aliqMunicipal: Double ; const BuscaParcial: Boolean = False): Boolean; overload;
    function Procurar(const ACodigo, AEX: String; const ATabela: TACBrIBPTaxTabela; const BuscaParcial: Boolean): TACBrIBPTaxRegistro; overload;
    function API_ConsultarProduto(const ANCM: String; const AUF: String;
      const AExcecao: Integer = 0; const ACodigoProprio: String = '';
      const ADescricaoProduto: String = ''; const AUnidadeMedida: String = '';
      const AValorUnitario: Double = 0.00; const AGtin: String = ''): TACBrIBPTaxProdutoDTO;

    function API_ConsultarServico(const ANBS_LC116: String; const AUF: String;
      const ADescricaoServico: String = ''; const AUnidadeMedida: String = '';
      const AValorUnitario: Double = 0.00): TACBrIBPTaxServicoDTO;

    property Itens: TACBrIBPTaxRegistros read FItens;
  published
    property OnErroImportacao: TACBrIBPTaxErroImportacao read FOnErroImportacao write FOnErroImportacao;
    property Fonte: string read FFonte write FFonte;
    property VersaoArquivo: String read FVersaoArquivo;
    property ChaveArquivo: String read FChaveArquivo;
    property VigenciaInicio: TDateTime read FVigenciaInicio;
    property VigenciaFim: TDateTime read FVigenciaFim;
    property URLDownload: String read FURLDownload write FURLDownload;
    property Arquivo: TStringList read FArquivo write FArquivo;
    property Token: String read FToken write FToken;
    property CNPJEmpresa: String read FCNPJEmpresa write FCNPJEmpresa;
  end;

  function TabelaToString(const ATabela: TACBrIBPTaxTabela): String;
  function StringToTabela(const ATabela: String): TACBrIBPTaxTabela;

implementation

uses
  StrUtils, ACBrValidador, ACBrUtil.Base, ACBrUtil.FilesIO, ACBrUtil.Strings,
  ACBrUtil.DateTime, ACBrUtil.XMLHTML;

function TabelaToString(const ATabela: TACBrIBPTaxTabela): String;
begin
  case ATabela of
    tabNCM: Result := '0';
    tabNBS: Result := '1';
    tabLST: Result := '2';
  end;
end;

function StringToTabela(const ATabela: String): TACBrIBPTaxTabela;
var
  TabelaTmp: Integer;
begin
  TabelaTmp := StrToIntDef(ATabela, -1);

  if TabelaTmp >= 0 then
    Result := TACBrIBPTaxTabela(TabelaTmp)
  else
    raise EACBrIBPTax.Create(ACBrStr(Format('Tipo de tabela desconhecido "%s".', [ATabela])));
end;

function SimpleJSONToList(const AJSONString: String): TStringList;
var
  Texto: String;
begin
  Result := TStringList.Create;

  Texto := AJSONString;
  Texto := String( RemoveStrings(AnsiString(Texto), ['{', '}']));
  Texto := StringReplace(Texto, ',"', sLineBreak + '"', [rfReplaceAll]);
  Texto := StringReplace(Texto, '"', '', [rfReplaceAll]);
  Texto := StringReplace(Texto, ':', '=', [rfReplaceAll]);
  //Texto := StringReplace(Texto, '.', ',', [rfReplaceAll]);

  Result.Text := Texto;
end;

{ TACBrIBPTaxRegistros }

function TACBrIBPTaxRegistros.GetItem(Index: integer): TACBrIBPTaxRegistro;
begin
  Result := TACBrIBPTaxRegistro(inherited Items[Index]);
end;

function TACBrIBPTaxRegistros.New: TACBrIBPTaxRegistro;
begin
  Result := TACBrIBPTaxRegistro.Create;
  Add(Result);
end;

procedure TACBrIBPTaxRegistros.SetItem(Index: integer;
  const Value: TACBrIBPTaxRegistro);
begin
  inherited Items[Index] := Value;
end;

{ TACBrIBPTax }

constructor TACBrIBPTax.Create(AOwner: TComponent);
begin
  inherited;

  FItens := TACBrIBPTaxRegistros.Create( True );
  FArquivo := TStringList.Create;
  FFonte := '';
  FURLDownload := '';
  FVersaoArquivo := '';
  FVigenciaInicio := 0;
  FVigenciaFim := 0;
  FOnErroImportacao := nil;
end;

destructor TACBrIBPTax.Destroy;
begin
  FItens.Free;
  FArquivo.Free;
  inherited;
end;

function TACBrIBPTax.PopularItens: Integer;
var
  Item: TStringList;
  I: Integer;
const
  COUNT_COLUN = 13;
begin
  if Arquivo.Count <= 0 then
    raise EACBrIBPTax.Create(ACBrStr('Arquivo de itens não foi baixado!'));

  FVersaoArquivo := '';
  FFonte := '';
  FChaveArquivo := '';
  FVigenciaFim := 0;
  FVigenciaInicio := 0;

  Itens.Clear;
  Item := TStringList.Create;
  try
    // primeira linha contem os cabecalhos de campo e versão do arquivo
    // segunda linha possui os dados do primeiro item e outros dados
    QuebrarLinha(Arquivo.Strings[1], Item);
    if Item.Count = COUNT_COLUN then
    begin
      FVigenciaInicio := StringToDateTimeDef(Item.Strings[8], 0.0, 'dd/mm/yyyy');
      FVigenciaFim    := StringToDateTimeDef(Item.Strings[9], 0.0, 'dd/mm/yyyy');
      FChaveArquivo   := Item.Strings[10];
      FVersaoArquivo  := Item.Strings[11];
      FFonte          := Item.Strings[12];
    end;

    // proximas linhas contem os registros
    for I := 1 to Arquivo.Count - 1 do
    begin
      QuebrarLinha(Trim(Arquivo.Strings[I]), Item);

      // ajustes para erros conhecidos nos arquivos
      if Item.Count <> COUNT_COLUN then
        QuebrarLinha(StringReplace(Arquivo.Strings[I], '"Ex"', 'Ex', []), Item);

      if(Item.Count <= 0)then
        break;

      if Trim(Item.Strings[2]) <> '' then
      begin
        if Item.Count = COUNT_COLUN then
        begin
          try
            // codigo;ex;tabela;descricao;aliqNac;aliqImp;0.0.2
            with Itens.New do
            begin
              NCM           := Trim(Item.Strings[0]);
              Excecao       := Trim(Item.Strings[1]);
              Tabela        := TACBrIBPTaxTabela(StrToInt(Trim(Item.Strings[2]))) ;
              Descricao     := Trim(Item.Strings[3]);

              FederalNacional  := StringToFloatDef(Item.Strings[4], 0.00);
              FederalImportado := StringToFloatDef(Item.Strings[5], 0.00);
              Estadual         := StringToFloatDef(Item.Strings[6], 0.00);
              Municipal        := StringToFloatDef(Item.Strings[7], 0.00);
            end;
          except
            on E: Exception do
            begin
              EventoErroImportacao(Arquivo.Strings[I], Format('Linha %d: ', [I+1]) + E.Message);
            end;
          end;
        end
        else
        begin
          EventoErroImportacao(
            Arquivo.Strings[I],
            Format('Linha %d: Registro inválido, quantidade de colunas "%d" diferente do esperado "%d"!', [I, Item.Count, COUNT_COLUN])
          );
        end;
      end
      else
      begin
        EventoErroImportacao(
          Arquivo.Strings[I],
          Format('Linha %d: Registro inválido, registro em branco!', [I, Item.Count, COUNT_COLUN])
        );
      end;
    end;
  finally
    Item.Free;
  end;

  Result := Itens.Count;
end;

function TACBrIBPTax.Procurar(const ACodigo, AEX: String;
  const ATabela: TACBrIBPTaxTabela; const BuscaParcial: Boolean): TACBrIBPTaxRegistro;
var
  I: Integer;
  Igual: Boolean;
begin

  if Itens.Count <= 0 then
    raise EACBrIBPTax.Create(ACBrStr('Tabela de itens ainda não foi aberta!'));

  Result := nil;
  
  for I := 0 to Itens.Count - 1 do
  begin

    if BuscaParcial then
      Igual := Pos(Trim(ACodigo), Trim(Itens[I].NCM)) > 0
    else
      Igual := SameText(Trim(ACodigo), Trim(Itens[I].NCM));

    Igual := Igual and SameText(Trim(AEX), Trim(Itens[I].Excecao)) and (ATabela = Itens[I].Tabela);

    if Igual Then
    begin
      Result := TACBrIBPTaxRegistro(Itens[I]);
      Exit;
    end;
    
  end;

end;

function TACBrIBPTax.TraduzStrToAnsi(const AValue: String): string;
begin
 Result := string( ACBrStrToAnsi( AValue ) );
end;

function TACBrIBPTax.DownloadTabela: Boolean;
begin

  if Trim(FURLDownload) = '' then
    raise EACBrIBPTax.Create(ACBrStr('URL do arquivo .csv não foi informado em "URLDownload!"'));

  // baixar a tabela
  HTTPGet( FURLDownload );
  FArquivo.Text := DecodeToString(HTTPResponse, RespIsUTF8);
  Result := True;

  PopularItens;
end;

function TACBrIBPTax.AbrirTabela(const AFileName: TFileName): Boolean;
begin
  // se foi passado nome do arquivo abrir do arquivo
  if Trim(AFileName) <> '' then
  begin
    if not FileExists(AFileName) then
      raise EACBrIBPTax.Create(ACBrStr(Format('Arquivo informado "%s" não encontrando!', [AFileName])))
    else
      Arquivo.LoadFromFile(AFileName);

    PopularItens;
  end
  else
  begin
    // se não foi passado fazer o downloda da tabela utilizando o endereço web informado
    if Arquivo.Count <= 0 then
    begin
      if Trim(URLDownload) = EmptyStr then
        raise EACBrIBPTax.Create(ACBrStr('URL para download da tabela IBPT não foi informada!'));

      DownloadTabela;
    end;
  end;

  Result := Itens.Count > 0;
end;

function TACBrIBPTax.AbrirTabela(const AConteudoArquivo: TStream): Boolean;
begin
  Arquivo.Clear;
  Arquivo.LoadFromStream(AConteudoArquivo);

  Result := PopularItens > 0;
end;

function TACBrIBPTax.AbrirTabela(const AConteudoArquivo: TStringList): Boolean;
begin
  if Assigned(AConteudoArquivo) then
  begin
    Arquivo.Clear;
    Arquivo.Text := AConteudoArquivo.Text;
  end
  else
    raise EFilerError.Create(ACBrStr('TStringList não pode ser vazio!'));

  Result := PopularItens > 0;
end;

function TACBrIBPTax.API_ConsultarProduto(const ANCM, AUF: String;
  const AExcecao: Integer; const ACodigoProprio, ADescricaoProduto,
  AUnidadeMedida: String; const AValorUnitario: Double;
  const AGtin: String): TACBrIBPTaxProdutoDTO;
var
  UrlConsulta, aux: String;
  json: TStringList;
begin
  if Trim(FToken) = '' then
    raise EACBrIBPTax.Create(ACBrStr('Token não foi configurado no componente ACBrIBPTax.'));

  if Trim(FCNPJEmpresa) = '' then
    raise EACBrIBPTax.Create(ACBrStr('CNPJ não foi configurado no componente ACBrIBPTax.'));

  if ValidarCNPJ(FCNPJEmpresa) <> '' then
    raise EACBrIBPTax.Create(ACBrStr('CNPJ configurado é inválido.'));

  if Trim(ANCM) = '' then
    raise EACBrIBPTax.Create(ACBrStr('NCM não foi informado.'));

  if Trim(AUF) = '' then
    raise EACBrIBPTax.Create(ACBrStr('UF não foi informado.'));

  if AExcecao < 0 then
    raise EACBrIBPTax.Create(ACBrStr('Informe 0 quando não houver exceção ou o código da exceção.'));

//  if Trim(ADescricaoProduto) = '' then
//    raise EACBrIBPTax.Create(ACBrStr('Descrição do produto não foi informada.'));
//
//  if Trim(AUnidadeMedida) = '' then
//    raise EACBrIBPTax.Create(ACBrStr('Unidade de medida não foi informada.'));
//
//  if Trim(AGtin) = '' then
//    raise EACBrIBPTax.Create(ACBrStr('GTIN não foi informado.'));

  UrlConsulta := 'https://apidoni.ibpt.org.br/api/v1/produtos' +
                   '?token='  + Self.AjustaParam(FToken) +
                   '&cnpj='   + Self.AjustaParam(OnlyNumber(FCNPJEmpresa)) +
                   '&codigo=' + Self.AjustaParam(ANCM) +
                   '&uf='     + Self.AjustaParam(AUF) +
                   '&ex='     + Self.AjustaParam(IntToStr(AExcecao)) +
                   '&descricao=' + Self.AjustaParam(ADescricaoProduto) +
                   '&unidadeMedida=' + Self.AjustaParam(AUnidadeMedida) +
                   '&valor=' + Self.AjustaParam(FloatToString(AValorUnitario, '.', '0.##')) +
                   '&gtin=' + Self.AjustaParam(AGtin);

  // parâmetros opcionais
  if Trim(ACodigoProprio) <> '' then
    UrlConsulta := UrlConsulta + '&codigoInterno=' + Self.AjustaParam(ACodigoProprio);

  // enviar consulta
  Self.HTTPGet(TraduzStrToAnsi(UrlConsulta));

  // retorno em JSON
  aux := DecodeToString(HTTPResponse, RespIsUTF8);
  Result.JSON := TraduzStrToAnsi(aux);
  json := SimpleJSONToList(Result.JSON);
  try
    Result.Codigo    := json.Values['Codigo'];
    Result.UF        := json.Values['UF'];
    Result.EX        := StrToIntDef(json.Values['EX'], 0);
    Result.Descricao := json.Values['Descricao'];
    Result.Nacional  := StringToFloatDef(json.Values['Nacional'], 0);
    Result.Estadual  := StringToFloatDef(json.Values['Estadual'], 0);
    Result.Municipal := StringToFloatDef(json.Values['Municipal'], 0);
    Result.Importado := StringToFloatDef(json.Values['Importado'], 0);

    Result.VigenciaInicio := StringToDateTimeDef(json.Values['VigenciaInicio'], 0, 'dd/mm/yyyy');
    Result.VigenciaFim := StringToDateTimeDef(json.Values['VigenciaFim'], 0, 'dd/mm/yyyy');
    Result.Chave := json.Values['Chave'];
    Result.Versao := json.Values['Versao'];
    Result.Fonte := json.Values['Fonte'];

    Result.Valor := StringToFloatDef(json.Values['Valor'], 0);
    Result.ValorTributoNacional := StringToFloatDef(json.Values['ValorTributoNacional'], 0);
    Result.ValorTributoEstadual := StringToFloatDef(json.Values['ValorTributoEstadual'], 0);
    Result.ValorTributoMunicipal := StringToFloatDef(json.Values['ValorTributoMunicipal'], 0);
    Result.ValorTributoImportado := StringToFloatDef(json.Values['ValorTributoImportado'], 0);

  finally
    json.Free;
  end;
end;

function TACBrIBPTax.API_ConsultarServico(const ANBS_LC116, AUF,
  ADescricaoServico, AUnidadeMedida: String;
  const AValorUnitario: Double): TACBrIBPTaxServicoDTO;
var
  UrlConsulta, aux: String;
  json: TStringList;
begin
  if Trim(FToken) = '' then
    raise EACBrIBPTax.Create(ACBrStr('Token não foi configurado no componente ACBrIBPTax.'));

  if Trim(FCNPJEmpresa) = '' then
    raise EACBrIBPTax.Create(ACBrStr('CNPJ não foi configurado no componente ACBrIBPTax.'));

  if ValidarCNPJ(FCNPJEmpresa) <> '' then
    raise EACBrIBPTax.Create(ACBrStr('CNPJ configurado é inválido.'));

  if Trim(ANBS_LC116) = '' then
    raise EACBrIBPTax.Create(ACBrStr('Código NBS/LC-116 não foi informado.'));

  if Trim(AUF) = '' then
    raise EACBrIBPTax.Create(ACBrStr('UF não foi informado.'));

//  if Trim(ADescricaoServico) = '' then
//    raise EACBrIBPTax.Create(ACBrStr('Descrição do serviço não foi informada.'));
//
//  if Trim(AUnidadeMedida) = '' then
//    raise EACBrIBPTax.Create(ACBrStr('Unidade de medida não foi informada.'));

  UrlConsulta := 'https://apidoni.ibpt.org.br/api/v1/servicos' +
                   '?token='  + Self.AjustaParam(FToken) +
                   '&cnpj='   + Self.AjustaParam(OnlyNumber(FCNPJEmpresa)) +
                   '&codigo=' + Self.AjustaParam(ANBS_LC116) +
                   '&uf='     + Self.AjustaParam(AUF) +
                   '&descricao=' + Self.AjustaParam(ADescricaoServico) +
                   '&unidadeMedida=' + Self.AjustaParam(AUnidadeMedida) +
                   '&valor=' + Self.AjustaParam(FloatToString(AValorUnitario, '.', '0.##'));

  // enviar consulta
  Self.HTTPGet(TraduzStrToAnsi(UrlConsulta));

  // retorno em JSON
  aux := DecodeToString(HTTPResponse, RespIsUTF8);
  Result.JSON := TraduzStrToAnsi(aux);
  json := SimpleJSONToList(Result.JSON);
  try
    Result.Codigo    := json.Values['Codigo'];
    Result.UF        := json.Values['UF'];
    Result.Descricao := json.Values['Descricao'];
    Result.Tipo      := json.Values['Tipo'];
    Result.Nacional  := StringToFloatDef(json.Values['Nacional'], 0);
    Result.Estadual  := StringToFloatDef(json.Values['Estadual'], 0);
    Result.Municipal := StringToFloatDef(json.Values['Municipal'], 0);
    Result.Importado := StringToFloatDef(json.Values['Importado'], 0);

    Result.VigenciaInicio := StringToDateTimeDef(json.Values['VigenciaInicio'], 0, 'dd/mm/yyyy');
    Result.VigenciaFim := StringToDateTimeDef(json.Values['VigenciaFim'], 0, 'dd/mm/yyyy');
    Result.Chave := json.Values['Chave'];
    Result.Versao := json.Values['Versao'];
    Result.Fonte := json.Values['Fonte'];

    Result.Valor := StringToFloatDef(json.Values['Valor'], 0);
    Result.ValorTributoNacional := StringToFloatDef(json.Values['ValorTributoNacional'], 0);
    Result.ValorTributoEstadual := StringToFloatDef(json.Values['ValorTributoEstadual'], 0);
    Result.ValorTributoMunicipal := StringToFloatDef(json.Values['ValorTributoMunicipal'], 0);
    Result.ValorTributoImportado := StringToFloatDef(json.Values['ValorTributoImportado'], 0);

  finally
    json.Free;
  end;
end;

function TACBrIBPTax.Procurar(const ACodigo: String; var ex, descricao: String;
  var tabela: Integer; var aliqFedNac, aliqFedImp, aliqEstadual, aliqMunicipal: Double ;
  const BuscaParcial: Boolean): Boolean;
var
  I: Integer;
  Igual: Boolean;
begin
  if Itens.Count <= 0 then
    raise EACBrIBPTax.Create(ACBrStr('Tabela de itens ainda não foi aberta!'));

  Result := False;
  for I := 0 to Itens.Count - 1 do
  begin
    if BuscaParcial then
      Igual := Pos(Trim(ACodigo), Trim(Itens[I].NCM)) > 0
    else
      Igual := SameText(Trim(ACodigo), Trim(Itens[I].NCM));

    if Igual Then
    begin
     ex            := Itens[I].Excecao;
     descricao     := Itens[I].Descricao;
     tabela        := Integer(Itens[I].Tabela);
     aliqFedNac    := Itens[I].FederalNacional;
     aliqFedImp    := Itens[I].FederalImportado;
     aliqEstadual  := Itens[I].Estadual;
     aliqMunicipal := Itens[I].Municipal;

     Result := True;
     Exit;
    end;
  end;
end;

procedure TACBrIBPTax.Exportar(const AArquivo: String; ATipo: TACBrIBPTaxExporta);
begin
  if Itens.Count <= 0 then
    raise EACBrIBPTax.Create(ACBrStr('Tabela de itens ainda não foi aberta!'));

  case ATipo of
    exTXT:  ExportarTXT(AArquivo);
    exCSV:  ExportarCSV(AArquivo);
    exDSV:  ExportarDSV(AArquivo);
    exXML:  ExportarXML(AArquivo);
    exHTML: ExportarHTML(AArquivo);
  end;
end;

procedure TACBrIBPTax.EventoErroImportacao(const Alinha, AErro: String);
begin
  if Assigned(FOnErroImportacao) then
    FOnErroImportacao(Alinha, AErro);
end;

procedure TACBrIBPTax.Exportar(const AArquivo, ADelimitador: String;
  const AQuoted: Boolean);
var
  I: Integer;
  Texto: String;

  function AddQuoted(const AValor: String): String;
  begin
    if AQuoted then
      Result := '"' + AValor + '"'
    else
      Result := AValor
  end;
begin
  if Itens.Count <= 0 then
    raise EACBrIBPTax.Create(ACBrStr('Tabela de itens ainda não foi aberta!'));

  Texto := '';
  for I := 0 to Itens.Count - 1 do
  begin
    Texto := Texto +
      AddQuoted(Itens[I].NCM) + ADelimitador +
      AddQuoted(Itens[I].Excecao) + ADelimitador +
      AddQuoted(IntToStr(Integer(Itens[I].Tabela))) + ADelimitador +
      AddQuoted(FloatToString(Itens[I].FederalNacional)) + ADelimitador +
      AddQuoted(FloatToString(Itens[I].FederalImportado)) + ADelimitador +
      AddQuoted(FloatToString(Itens[I].Estadual)) + ADelimitador +
      AddQuoted(FloatToString(Itens[I].Municipal)) + ADelimitador +
      AddQuoted(Itens[I].Descricao) + ADelimitador +
      sLineBreak;
  end;

  if Trim(Texto) <> '' then
    WriteToTXT(AArquivo, AnsiString(Texto), False, False);
end;

procedure TACBrIBPTax.ExportarTXT(const AArquivo: String);
var
  I: Integer;
  Texto: String;
begin
  if Itens.Count <= 0 then
    raise EACBrIBPTax.Create(ACBrStr('Tabela de itens ainda não foi aberta!'));

  Texto := '';
  for I := 0 to Itens.Count - 1 do
  begin
    Texto := Texto +
      PadRight(Itens[I].NCM, 10) +
      PadRight(Itens[I].Excecao, 2) +
      PadRight(IntToStr(Integer(Itens[I].Tabela)), 1) +
      PadLeft(FloatToString(Itens[I].FederalNacional * 100), 4, '0') +
      PadLeft(FloatToString(Itens[I].FederalImportado * 100), 4, '0') +
      PadLeft(FloatToString(Itens[I].Estadual * 100), 4, '0') +
      PadLeft(FloatToString(Itens[I].Municipal * 100), 4, '0') +
      PadRight(Itens[I].Descricao, 400) +
      sLineBreak;
  end;

  if Trim(Texto) <> '' then
    WriteToTXT(AArquivo, AnsiString(Texto), False, False);
end;

procedure TACBrIBPTax.ExportarCSV(const AArquivo: String);
begin
  Exportar(AArquivo, ';', True);
end;

procedure TACBrIBPTax.ExportarDSV(const AArquivo: String);
var
  I: Integer;
  Texto: String;

  function AddAspasDuplas(const ATexto: String): String;
  begin
    Result := '"' + ATexto + '"';
  end;

begin
  if Itens.Count <= 0 then
    raise EACBrIBPTax.Create(ACBrStr('Tabela de itens ainda não foi aberta!'));

  Texto := '';
  for I := 0 to Itens.Count - 1 do
  begin
    Texto := Texto +
      AddAspasDuplas(Itens[I].NCM) + ',' +
      AddAspasDuplas(Itens[I].Excecao) + ',' +
      AddAspasDuplas(IntToStr(Integer(Itens[I].Tabela))) + ',' +
      AddAspasDuplas(FloatToString(Itens[I].FederalNacional)) + ',' +
      AddAspasDuplas(FloatToString(Itens[I].FederalImportado)) + ',' +
      AddAspasDuplas(FloatToString(Itens[I].Estadual)) + ',' +
      AddAspasDuplas(FloatToString(Itens[I].Municipal)) + ',' +
      AddAspasDuplas(Itens[I].Descricao) +
      sLineBreak;
  end;

  if Trim(Texto) <> '' then
    WriteToTXT(AArquivo, AnsiString(Texto), False, False);
end;

procedure TACBrIBPTax.ExportarXML(const AArquivo: String);
var
  I: Integer;
  Texto: String;
begin
  if Itens.Count <= 0 then
    raise EACBrIBPTax.Create(ACBrStr('Tabela de itens ainda não foi aberta!'));

  Texto := '<?xml version="1.0" encoding="ISO-8859-1"?><IBPTax>';
  for I := 0 to Itens.Count - 1 do
  begin
    Texto := Texto +
      '<imposto>' +
        '<ncm>' + Itens[I].NCM + '</ncm>' +
        '<ex>' + Itens[I].Excecao + '</ex>' +
        '<tabela>' + IntToStr(Integer(Itens[I].Tabela)) + '</tabela>' +
        '<aliqFedNac>' + FloatToString(Itens[I].FederalNacional) + '</aliqFedNac>' +
        '<aliqFedImp>' + FloatToString(Itens[I].FederalImportado) + '</aliqFedImp>' +
        '<aliqEst>' + FloatToString(Itens[I].Estadual) + '</aliqEst>' +
        '<aliqMun>' + FloatToString(Itens[I].Municipal) + '</aliqMun>' +
        '<descricao>' + ACBrUtil.XMLHTML.ParseText( AnsiString( Itens[I].Descricao ), False, False) + '</descricao>' +
      '</imposto>';
  end;
  Texto := Texto + '</IBPTax>';

  if Trim(Texto) <> '' then
    WriteToTXT(AArquivo, AnsiString(Texto), False, True);
end;

procedure TACBrIBPTax.ExportarHTML(const AArquivo: String);
var
  I: Integer;
  Texto: String;
begin
  if Itens.Count <= 0 then
    raise EACBrIBPTax.Create(ACBrStr('Tabela de itens ainda não foi aberta!'));

  Texto :=
    '<html>' + slineBreak +
    '<head>' + slineBreak +
    '    <title>Tabela Imposto no Cupom</title>' + slineBreak +
    '    <style type="text/css">' + slineBreak +
    '        body{font-family: Arial;}' + slineBreak +
    '        th{color:white; font-size:8pt; background-color: black;}' + slineBreak +
		'        tr{font-size:8pt;}' + slineBreak +
    '        tr:nth-child(2n+1) {background-color: #DCDCDC;}' + slineBreak +
    '    </style>' + slineBreak +
    '</head>' + slineBreak +
    '<body>' + slineBreak +
    '    <table>' + slineBreak +
		'        <tr>' + slineBreak +
    '          <th>NCM</th>' + slineBreak +
    '          <th>Exceção</th>' + slineBreak +
    '          <th>Tabela</th>' + slineBreak +
    '          <th>Aliq. Federal Nacional</th>' + slineBreak +
    '          <th>Aliq. Federal Importado</th>' + slineBreak +
    '          <th>Aliq. Estadual</th>' + slineBreak +
    '          <th>Aliq. Municipal</th>' + slineBreak +
    '          <th>Descrição</th>' + slineBreak +
		'        </tr>' + slineBreak;

  for I := 0 to Itens.Count - 1 do
  begin
    Texto := Texto +
      '<tr>' + slineBreak +
        '<td>' + Itens[I].NCM + '</td>' + slineBreak +
        '<td>' + Itens[I].Excecao + '</td>' + slineBreak +
        '<td>' + IntToStr(Integer(Itens[I].Tabela)) + '</td>' + slineBreak +
        '<td>' + FloatToStr(Itens[I].FederalNacional) + '</td>' + slineBreak +
        '<td>' + FloatToStr(Itens[I].FederalImportado) + '</td>' + slineBreak +
        '<td>' + FloatToStr(Itens[I].Estadual) + '</td>' + slineBreak +
        '<td>' + FloatToStr(Itens[I].Municipal) + '</td>' + slineBreak +
        '<td>' + ACBrUtil.XMLHTML.ParseText( AnsiString( Itens[I].Descricao ), False, False) + '</td>' + slineBreak +
      '</tr>' + slineBreak;
  end;

  Texto := Texto +
    '    </table>' + slineBreak +
    '</body>' + slineBreak +
    '</html>' + slineBreak;

  if Trim(Texto) <> '' then
    WriteToTXT(AArquivo, AnsiString(Texto), False, True);
end;

end.
