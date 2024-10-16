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

unit ACBrNFSeXNotasFiscais;

interface

uses
  Classes, SysUtils,
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$Else}
   Contnrs,
  {$IfEnd}
  IniFiles,
  ACBrBase, ACBrDFe, ACBrNFSeXConfiguracoes, ACBrNFSeXClass, ACBrNFSeXConversao;

type

  { TNotaFiscal }

  TNotaFiscal = class(TCollectionItem)
  private
    FNFSe: TNFSe;
    FACBrNFSe: TACBrDFe;

    FAlertas: string;
    FNomeArq: string;
    FNomeArqRps: string;
    FConfirmada: Boolean;
    FXmlRps: string;
    FXmlNfse: string;
    FXmlEspelho: string;

    function CalcularNomeArquivo: string;
    function CalcularPathArquivo: string;
    procedure SetXmlNfse(const Value: string);
    function GetXmlNfse: string;

    function LerRpsIni(AINIRec: TMemIniFile): Boolean;
    function LerNfseIni(AINIRec: TMemIniFile): Boolean;
  public
    constructor Create(AOwner: TACBrDFe);
    destructor Destroy; override;

    procedure Imprimir;
    procedure ImprimirPDF; overload;
    procedure ImprimirPDF(AStream: TStream); overload;

    function LerXML(const AXML: string): Boolean;
    function LerArqIni(const AIniString: string): Boolean;
    function GerarNFSeIni: string;

    function GerarXML: string;
    function GravarXML(const NomeArquivo: string = '';
      const PathArquivo: string = ''; aTipo: TtpXML = txmlNFSe): Boolean;

    function GravarStream(AStream: TStream): Boolean;

    procedure EnviarEmail(const sPara, sAssunto: string; sMensagem: TStrings = nil;
      EnviaPDF: Boolean = True; sCC: TStrings = nil; Anexos: TStrings = nil;
      sReplyTo: TStrings = nil; ManterPDFSalvo: Boolean = True);

    property NomeArq: string    read FNomeArq    write FNomeArq;
    property NomeArqRps: string read FNomeArqRps write FNomeArqRps;

    function CalcularNomeArquivoCompleto(NomeArquivo: string = '';
      PathArquivo: string = ''): string;

    property NFSe: TNFSe read FNFSe;

    property XmlRps: string read FXmlRps write FXmlRps;
    property XmlNfse: string read GetXmlNfse write SetXmlNfse;
    property XmlEspelho: string read FXmlEspelho write FXmlEspelho;

    property Confirmada: Boolean read FConfirmada write FConfirmada;
    property Alertas: string     read FAlertas;

  end;

  { TNotasFiscais }

  TNotasFiscais = class(TACBrObjectList)
  private
    FTransacao: Boolean;
    FNumeroLote: string;
    FACBrNFSe: TACBrDFe;
    FConfiguracoes: TConfiguracoesNFSe;
    FXMLLoteOriginal: string;
    FXMLLoteAssinado: string;
    FAlertas: string;

    function GetItem(Index: integer): TNotaFiscal;
    procedure SetItem(Index: integer; const Value: TNotaFiscal);

    procedure VerificarDANFSE;
  public
    constructor Create(AOwner: TACBrDFe);

    procedure GerarNFSe;
    procedure Imprimir;
    procedure ImprimirPDF; overload;
    procedure ImprimirPDF(AStream: TStream); overload;

    function New: TNotaFiscal; reintroduce;
    function Add(ANota: TNotaFiscal): Integer; reintroduce;
    Procedure Insert(Index: Integer; ANota: TNotaFiscal); reintroduce;
    function FindByRps(const ANumRPS: string): TNotaFiscal;
    function FindByNFSe(const ANumNFSe: string): TNotaFiscal;
    function FindByCnpjCpfSerieRps(const CnpjCpf, Serie, ANumRPS: string): TNotaFiscal;

    property Items[Index: integer]: TNotaFiscal read GetItem write SetItem; default;

    function GetNamePath: string;

    // Incluido o Parametro AGerarNFSe que determina se após carregar os dados da NFSe
    // para o componente, será gerado ou não novamente o XML da NFSe.
    function LoadFromFile(const CaminhoArquivo: string; AGerarNFSe: Boolean = True): Boolean;
    function LoadFromStream(AStream: TStringStream; AGerarNFSe: Boolean = True): Boolean;
    function LoadFromString(const AXMLString: string; AGerarNFSe: Boolean = True): Boolean;
    function LoadFromIni(const AIniString: string): Boolean;
    function LoadFromLoteNfse(const CaminhoArquivo: string): Boolean;

    function GerarIni: string;
    function GravarXML(const PathNomeArquivo: string = ''): Boolean;

    property XMLLoteOriginal: string read FXMLLoteOriginal write FXMLLoteOriginal;
    property XMLLoteAssinado: string read FXMLLoteAssinado write FXMLLoteAssinado;
    property NumeroLote: string      read FNumeroLote      write FNumeroLote;
    property Transacao: Boolean      read FTransacao       write FTransacao;
    property Alertas: string         read FAlertas;

    property ACBrNFSe: TACBrDFe read FACBrNFSe;
  end;

  function CompRpsPorNumero(const Item1,
    Item2: {$IfDef HAS_SYSTEM_GENERICS}TObject{$Else}Pointer{$EndIf}): Integer;
  function CompNFSePorNumero(const Item1,
    Item2: {$IfDef HAS_SYSTEM_GENERICS}TObject{$Else}Pointer{$EndIf}): Integer;
  function CompRpsPorCnpjCpfSerieNumero(const Item1,
    Item2: {$IfDef HAS_SYSTEM_GENERICS}TObject{$Else}Pointer{$EndIf}): Integer;

implementation

uses
  synautil, StrUtilsEx,
  ACBrUtil.DateTime,
  ACBrUtil.Base, ACBrUtil.Strings, ACBrUtil.FilesIO, ACBrUtil.XMLHTML,
  ACBrDFeUtil,
  ACBrNFSeX, ACBrNFSeXInterface;

function CompRpsPorNumero(const Item1,
  Item2: {$IfDef HAS_SYSTEM_GENERICS}TObject{$Else}Pointer{$EndIf}): Integer;
var
  NumRps1, NumRps2: Integer;
begin
  NumRps1 := StrToIntDef(TNotaFiscal(Item1).NFSe.IdentificacaoRps.Numero, 0);
  NumRps2 := StrToIntDef(TNotaFiscal(Item2).NFSe.IdentificacaoRps.Numero, 0);

  if NumRps1 < NumRps2 then
    Result := -1
  else if NumRps1 > NumRps2 then
    Result := 1
  else
    Result := 0;
end;

function CompNFSePorNumero(const Item1,
  Item2: {$IfDef HAS_SYSTEM_GENERICS}TObject{$Else}Pointer{$EndIf}): Integer;
var
  NumNFSe1, NumNFSe2: Int64;
begin
  NumNFSe1 := StrToInt64Def(TNotaFiscal(Item1).NFSe.Numero, 0);
  NumNFSe2 := StrToInt64Def(TNotaFiscal(Item2).NFSe.Numero, 0);

  if NumNFSe1 < NumNFSe2 then
    Result := -1
  else if NumNFSe1 > NumNFSe2 then
    Result := 1
  else
    Result := 0;
end;

function CompRpsPorCnpjCpfSerieNumero(const Item1,
  Item2: {$IfDef HAS_SYSTEM_GENERICS}TObject{$Else}Pointer{$EndIf}): Integer;
var
  NumRps1, NumRps2: string;
begin
  NumRps1 :=
    PadLeft(TNotaFiscal(Item1).NFSe.Prestador.IdentificacaoPrestador.CpfCnpj, 14, '0') +
    PadLeft(TNotaFiscal(Item1).NFSe.IdentificacaoRps.Serie, 5, '0') +
    PadLeft(TNotaFiscal(Item1).NFSe.IdentificacaoRps.Numero, 15, '0');
  NumRps2 :=
    PadLeft(TNotaFiscal(Item2).NFSe.Prestador.IdentificacaoPrestador.CpfCnpj, 14, '0') +
    PadLeft(TNotaFiscal(Item2).NFSe.IdentificacaoRps.Serie, 5, '0') +
    PadLeft(TNotaFiscal(Item2).NFSe.IdentificacaoRps.Numero, 15, '0');

  if NumRps1 < NumRps2 then
    Result := -1
  else if NumRps1 > NumRps2 then
    Result := 1
  else
    Result := 0;
end;

{ TNotaFiscal }

constructor TNotaFiscal.Create(AOwner: TACBrDFe);
begin
  if not (AOwner is TACBrNFSeX) then
    raise EACBrNFSeException.Create('AOwner deve ser do tipo TACBrNFSeX');

  FACBrNFSe := TACBrNFSeX(AOwner);
  FNFSe := TNFSe.Create;
end;

destructor TNotaFiscal.Destroy;
begin
  FNFSe.Free;

  inherited Destroy;
end;

procedure TNotaFiscal.Imprimir;
begin
  with TACBrNFSeX(FACBrNFSe) do
  begin
    DANFSE.Provedor := Configuracoes.Geral.Provedor;

    if Configuracoes.WebServices.AmbienteCodigo = 1 then
      DANFSE.Producao := snSim
    else
      DANFSE.Producao := snNao;

    if not Assigned(DANFSE) then
      raise EACBrNFSeException.Create('Componente DANFSE não associado.')
    else
      DANFSE.ImprimirDANFSE(NFSe);
  end;
end;

procedure TNotaFiscal.ImprimirPDF;
begin
  with TACBrNFSeX(FACBrNFSe) do
  begin
    DANFSE.Provedor := Configuracoes.Geral.Provedor;

    if Configuracoes.WebServices.AmbienteCodigo = 1 then
      DANFSE.Producao := snSim
    else
      DANFSE.Producao := snNao;

    if not Assigned(DANFSE) then
      raise EACBrNFSeException.Create('Componente DANFSE não associado.')
    else
      DANFSE.ImprimirDANFSEPDF(NFSe);
  end;
end;

procedure TNotaFiscal.ImprimirPDF(AStream: TStream);
begin
  with TACBrNFSeX(FACBrNFSe) do
  begin
    DANFSE.Provedor := Configuracoes.Geral.Provedor;

    if not Assigned(DANFSE) then
      raise EACBrNFSeException.Create('Componente DANFSE não associado.')
    else
    begin
      AStream.Size := 0;
      DANFSE.ImprimirDANFSEPDF(AStream, NFSe);
    end;
  end;
end;

function TNotaFiscal.LerRpsIni(AINIRec: TMemIniFile): Boolean;
var
  FProvider: IACBrNFSeXProvider;
  sSecao, sFim: string;
  Ok: Boolean;
  i: Integer;
  Item: TItemServicoCollectionItem;
  ItemDocDeducao: TDocDeducaoCollectionItem;
  ItemParcelas: TParcelasCollectionItem;
begin
  FProvider := TACBrNFSeX(FACBrNFSe).Provider;

  if not Assigned(FProvider) then
    raise EACBrNFSeException.Create(ERR_SEM_PROVEDOR);

  with FNFSe do
  begin
    // Provedor Infisc - Layout Proprio
    sSecao := 'IdentificacaoNFSe';
    if AINIRec.SectionExists(sSecao) then
    begin
      Numero := AINIRec.ReadString(sSecao, 'Numero', '');
      cNFSe := GerarCodigoDFe(StrToIntDef(Numero, 0));
      NumeroLote := AINIRec.ReadString(sSecao, 'NumeroLote', '');
      ModeloNFSe := AINIRec.ReadString(sSecao, 'ModeloNFSe', '');
      refNF := AINIRec.ReadString(sSecao, 'refNF', '');
      TipoEmissao := StrToTipoEmissao(Ok, AINIRec.ReadString(sSecao, 'TipoEmissao', 'N'));
      Canhoto := StrToCanhoto(Ok, AINIRec.ReadString(sSecao, 'Canhoto', '0'));
      EmpreitadaGlobal := StrToEmpreitadaGlobal(Ok, AINIRec.ReadString(sSecao, 'EmpreitadaGlobal', '2'));
    end;

    sSecao := 'IdentificacaoRps';
    if AINIRec.SectionExists(sSecao) then
    begin
      SituacaoTrib := FProvider.StrToSituacaoTrib(Ok, AINIRec.ReadString(sSecao, 'SituacaoTrib', 'tp'));

      //Provedores CTA, ISSBarueri, ISSSDSF, ISSSaoPaulo, Simple e SmarAPD.
      if AINIRec.ReadString(sSecao, 'TipoTributacaoRPS', 'FIM') <> 'FIM' then
        TipoTributacaoRPS := FProvider.StrToTipoTributacaoRPS(Ok, AINIRec.ReadString(sSecao, 'TipoTributacaoRPS', ''));

      // Provedor AssessorPublico
      Situacao := AINIRec.ReadInteger(sSecao, 'Situacao', 0);

      Producao := FProvider.StrToSimNao(Ok, AINIRec.ReadString(sSecao, 'Producao', '1'));
      StatusRps := FProvider.StrToStatusRPS(Ok, AINIRec.ReadString(sSecao, 'Status', '1'));
      OutrasInformacoes := AINIRec.ReadString(sSecao, 'OutrasInformacoes', '');

      // Provedores: Infisc, ISSDSF e Siat
      SeriePrestacao := AINIRec.ReadString(sSecao, 'SeriePrestacao', '');

      IdentificacaoRps.Numero := AINIRec.ReadString(sSecao, 'Numero', '0');
      IdentificacaoRps.Serie := AINIRec.ReadString(sSecao, 'Serie', '0');
      IdentificacaoRps.Tipo := FProvider.StrToTipoRPS(Ok, AINIRec.ReadString(sSecao, 'Tipo', '1'));

      sFim := AINIRec.ReadString(sSecao, 'DataEmissao', '');
      if sFim <> '' then
        DataEmissao := StringToDateTimeDef(sFim, 0);

      sFim := AINIRec.ReadString(sSecao, 'Competencia', '');
      if sFim <> '' then
        Competencia := StringToDateTimeDef(sFim, 0);
      DataEmissaoRPS := DataEmissao;

      sFim := AINIRec.ReadString(sSecao, 'Vencimento', '');
      if sFim <> '' then
        Vencimento := StringToDateTimeDef(sFim, 0);
      NaturezaOperacao := StrToNaturezaOperacao(Ok, AINIRec.ReadString(sSecao, 'NaturezaOperacao', '0'));

      // Provedor Tecnos
      PercentualCargaTributaria := StringToFloatDef(AINIRec.ReadString(sSecao, 'PercentualCargaTributaria', ''), 0);
      ValorCargaTributaria := StringToFloatDef(AINIRec.ReadString(sSecao, 'ValorCargaTributaria', ''), 0);
      PercentualCargaTributariaMunicipal := StringToFloatDef(AINIRec.ReadString(sSecao, 'PercentualCargaTributariaMunicipal', ''), 0);
      ValorCargaTributariaMunicipal := StringToFloatDef(AINIRec.ReadString(sSecao, 'ValorCargaTributariaMunicipal', ''), 0);
      PercentualCargaTributariaEstadual := StringToFloatDef(AINIRec.ReadString(sSecao, 'PercentualCargaTributariaEstadual', ''), 0);
      ValorCargaTributariaEstadual := StringToFloatDef(AINIRec.ReadString(sSecao, 'ValorCargaTributariaEstadual', ''), 0);

      // Provedor PadraoNacional
      verAplic := AINIRec.ReadString(sSecao, 'verAplic', 'ACBrNFSeX-1.00');
      tpEmit := StrTotpEmit(Ok, AINIRec.ReadString(sSecao, 'tpEmit', '1'));

      //Provedor Governa
      RegRec := StrToRegRec(Ok, AINIRec.ReadString(sSecao, 'RegRec', ''));

      InformacoesComplementares := AINIRec.ReadString(sSecao, 'InformacoesComplementares', '');
    end;

    sSecao := 'RpsSubstituido';
    if AINIRec.SectionExists(sSecao) then
    begin
      RpsSubstituido.Numero := AINIRec.ReadString(sSecao, 'Numero', '');
      RpsSubstituido.Serie := AINIRec.ReadString(sSecao, 'Serie', '');
      RpsSubstituido.Tipo := FProvider.StrToTipoRPS(Ok, AINIRec.ReadString(sSecao, 'Tipo', '1'));
    end;

    sSecao := 'Prestador';
    if AINIRec.SectionExists(sSecao) then
    begin
      RegimeEspecialTributacao := FProvider.StrToRegimeEspecialTributacao(Ok, AINIRec.ReadString(sSecao, 'Regime', '0'));
      OptanteSimplesNacional := FProvider.StrToSimNao(Ok, AINIRec.ReadString(sSecao, 'OptanteSN', '1'));
      OptanteSN := StrToOptanteSN(Ok, AINIRec.ReadString(sSecao, 'opSimpNac', '2'));

      IncentivadorCultural := FProvider.StrToSimNao(Ok, AINIRec.ReadString(sSecao, 'IncentivadorCultural', '1'));

      if AINIRec.ReadString(sSecao, 'RegimeApuracaoSN', '') <> '' then
        RegimeApuracaoSN := StrToRegimeApuracaoSN(Ok, AINIRec.ReadString(sSecao, 'RegimeApuracaoSN', '1'));

      Prestador.IdentificacaoPrestador.CpfCnpj := AINIRec.ReadString(sSecao, 'CNPJ', '');
      Prestador.IdentificacaoPrestador.InscricaoMunicipal := AINIRec.ReadString(sSecao, 'InscricaoMunicipal', '');

      Prestador.IdentificacaoPrestador.Nif := AINIRec.ReadString(sSecao, 'NIF', '');
      Prestador.IdentificacaoPrestador.cNaoNIF := StrToNaoNIF(Ok, AINIRec.ReadString(sSecao, 'cNaoNIF', '0'));
      Prestador.IdentificacaoPrestador.CAEPF := AINIRec.ReadString(sSecao, 'CAEPF', '');

      // Para o provedor ISSDigital deve-se informar também:
      Prestador.cUF := UFparaCodigoUF(AINIRec.ReadString(sSecao, 'UF', 'SP'));

      Prestador.RazaoSocial := AINIRec.ReadString(sSecao, 'RazaoSocial', '');
      Prestador.NomeFantasia := AINIRec.ReadString(sSecao, 'NomeFantasia', '');

      Prestador.Endereco.Endereco := AINIRec.ReadString(sSecao, 'Logradouro', '');
      Prestador.Endereco.Numero := AINIRec.ReadString(sSecao, 'Numero', '');
      Prestador.Endereco.Complemento := AINIRec.ReadString(sSecao, 'Complemento', '');
      Prestador.Endereco.Bairro := AINIRec.ReadString(sSecao, 'Bairro', '');
      Prestador.Endereco.CodigoMunicipio := AINIRec.ReadString(sSecao, 'CodigoMunicipio', '');
      Prestador.Endereco.xMunicipio := AINIRec.ReadString(sSecao, 'xMunicipio', '');
      Prestador.Endereco.UF := AINIRec.ReadString(sSecao, 'UF', '');
      Prestador.Endereco.CodigoPais := AINIRec.ReadInteger(sSecao, 'CodigoPais', 0);
      Prestador.Endereco.xPais := AINIRec.ReadString(sSecao, 'xPais', '');
      Prestador.Endereco.CEP := AINIRec.ReadString(sSecao, 'CEP', '');

      Prestador.Contato.Telefone := AINIRec.ReadString(sSecao, 'Telefone', '');
      Prestador.Contato.Email := AINIRec.ReadString(sSecao, 'Email', '');

      // Para o provedor WebFisco
      Prestador.Anexo := AINIRec.ReadString(sSecao, 'Anexo', '');
      Prestador.ValorReceitaBruta := StringToFloatDef(AINIRec.ReadString(sSecao, 'ValorReceitaBruta', ''), 0);
    end;

    sSecao := 'Tomador';
    if AINIRec.SectionExists(sSecao) then
    begin
      Tomador.IdentificacaoTomador.Tipo := FProvider.StrToTipoPessoa(Ok, AINIRec.ReadString(sSecao, 'Tipo', '1'));
      Tomador.IdentificacaoTomador.CpfCnpj := AINIRec.ReadString(sSecao, 'CNPJCPF', '');
      Tomador.IdentificacaoTomador.InscricaoMunicipal := AINIRec.ReadString(sSecao, 'InscricaoMunicipal', '');
      Tomador.IdentificacaoTomador.InscricaoEstadual := AINIRec.ReadString(sSecao, 'InscricaoEstadual', '');

      Tomador.IdentificacaoTomador.Nif := AINIRec.ReadString(sSecao, 'NIF', '');
      Tomador.IdentificacaoTomador.cNaoNIF := StrToNaoNIF(Ok, AINIRec.ReadString(sSecao, 'cNaoNIF', '0'));
      Tomador.IdentificacaoTomador.CAEPF := AINIRec.ReadString(sSecao, 'CAEPF', '');

      Tomador.RazaoSocial := AINIRec.ReadString(sSecao, 'RazaoSocial', '');
      Tomador.NomeFantasia := AINIRec.ReadString(sSecao, 'NomeFantasia', '');

      Tomador.Endereco.EnderecoInformado := FProvider.StrToSimNaoOpc(Ok, AINIRec.ReadString(sSecao, 'EnderecoInformado', ''));
      Tomador.Endereco.TipoLogradouro := AINIRec.ReadString(sSecao, 'TipoLogradouro', '');
      Tomador.Endereco.Endereco := AINIRec.ReadString(sSecao, 'Logradouro', '');
      Tomador.Endereco.Numero := AINIRec.ReadString(sSecao, 'Numero', '');
      Tomador.Endereco.Complemento := AINIRec.ReadString(sSecao, 'Complemento', '');
      Tomador.Endereco.PontoReferencia := AINIRec.ReadString(sSecao, 'PontoReferencia', '');
      Tomador.Endereco.Bairro := AINIRec.ReadString(sSecao, 'Bairro', '');
      Tomador.Endereco.CodigoMunicipio := AINIRec.ReadString(sSecao, 'CodigoMunicipio', '');
      Tomador.Endereco.xMunicipio := AINIRec.ReadString(sSecao, 'xMunicipio', '');
      Tomador.Endereco.UF := AINIRec.ReadString(sSecao, 'UF', '');
      Tomador.Endereco.CodigoPais := AINIRec.ReadInteger(sSecao, 'CodigoPais', 0);
      Tomador.Endereco.CEP := AINIRec.ReadString(sSecao, 'CEP', '');
      // Provedor Equiplano é obrigatório o pais e IE
      Tomador.Endereco.xPais := AINIRec.ReadString(sSecao, 'xPais', '');

      Tomador.Contato.DDD := AINIRec.ReadString(sSecao, 'DDD', '');
      Tomador.Contato.Telefone := AINIRec.ReadString(sSecao, 'Telefone', '');
      Tomador.Contato.Email := AINIRec.ReadString(sSecao, 'Email', '');

      Tomador.AtualizaTomador := FProvider.StrToSimNao(Ok, AINIRec.ReadString(sSecao, 'AtualizaTomador', '1'));
      Tomador.TomadorExterior := FProvider.StrToSimNao(Ok, AINIRec.ReadString(sSecao, 'TomadorExterior', '2'));
    end;

    sSecao := 'Intermediario';
    if AINIRec.SectionExists(sSecao)then
    begin
      Intermediario.Identificacao.CpfCnpj := AINIRec.ReadString(sSecao, 'CNPJCPF', '');
      Intermediario.Identificacao.InscricaoMunicipal := AINIRec.ReadString(sSecao, 'InscricaoMunicipal', '');
      Intermediario.Identificacao.Nif := AINIRec.ReadString(sSecao, 'NIF', '');
      Intermediario.Identificacao.cNaoNIF := StrToNaoNIF(Ok, AINIRec.ReadString(sSecao, 'cNaoNIF', '0'));
      Intermediario.Identificacao.CAEPF := AINIRec.ReadString(sSecao, 'CAEPF', '');

      Intermediario.RazaoSocial := AINIRec.ReadString(sSecao, 'RazaoSocial', '');

      Intermediario.Endereco.CodigoMunicipio := AINIRec.ReadString(sSecao, 'CodigoMunicipio', '');
      Intermediario.Endereco.CEP := AINIRec.ReadString(sSecao, 'CEP', '');
      Intermediario.Endereco.CodigoPais := AINIRec.ReadInteger(sSecao, 'CodigoPais', 0);
      Intermediario.Endereco.xMunicipio := AINIRec.ReadString(sSecao, 'xMunicipio', '');
      Intermediario.Endereco.UF := AINIRec.ReadString(sSecao, 'UF', '');
      Intermediario.Endereco.Endereco := AINIRec.ReadString(sSecao, 'Logradouro', '');
      Intermediario.Endereco.Numero := AINIRec.ReadString(sSecao, 'Numero', '');
      Intermediario.Endereco.Complemento := AINIRec.ReadString(sSecao, 'Complemento', '');
      Intermediario.Endereco.Bairro := AINIRec.ReadString(sSecao, 'Bairro', '');

      Intermediario.Contato.Telefone := AINIRec.ReadString(sSecao, 'Telefone', '');
      Intermediario.Contato.Email := AINIRec.ReadString(sSecao, 'Email', '');
    end;

    sSecao := 'ConstrucaoCivil';
    if AINIRec.SectionExists(sSecao) then
    begin
      ConstrucaoCivil.CodigoObra := AINIRec.ReadString(sSecao, 'CodigoObra', '');
      ConstrucaoCivil.Art := AINIRec.ReadString(sSecao, 'Art', '');
      ConstrucaoCivil.inscImobFisc := AINIRec.ReadString(sSecao, 'inscImobFisc', '');

      ConstrucaoCivil.Endereco.CEP := AINIRec.ReadString(sSecao, 'CEP', '');
      ConstrucaoCivil.Endereco.xMunicipio := AINIRec.ReadString(sSecao, 'xMunicipio', '');
      ConstrucaoCivil.Endereco.UF := AINIRec.ReadString(sSecao, 'UF', '');
      ConstrucaoCivil.Endereco.Endereco := AINIRec.ReadString(sSecao, 'Logradouro', '');
      ConstrucaoCivil.Endereco.Numero := AINIRec.ReadString(sSecao, 'Numero', '');
      ConstrucaoCivil.Endereco.Complemento := AINIRec.ReadString(sSecao, 'Complemento', '');
      ConstrucaoCivil.Endereco.Bairro := AINIRec.ReadString(sSecao, 'Bairro', '');
    end;

    sSecao := 'Servico';
    if AINIRec.SectionExists(sSecao) then
    begin
      Servico.ItemListaServico := AINIRec.ReadString(sSecao, 'ItemListaServico', '');
      Servico.CodigoCnae := AINIRec.ReadString(sSecao, 'CodigoCnae', '');
      Servico.CodigoTributacaoMunicipio := AINIRec.ReadString(sSecao, 'CodigoTributacaoMunicipio', '');
      Servico.Discriminacao := AINIRec.ReadString(sSecao, 'Discriminacao', '');
      Servico.CodigoMunicipio := AINIRec.ReadString(sSecao, 'CodigoMunicipio', '');
      Servico.CodigoPais := AINIRec.ReadInteger(sSecao, 'CodigoPais', 0);
      Servico.ExigibilidadeISS := FProvider.StrToExigibilidadeISS(Ok, AINIRec.ReadString(sSecao, 'ExigibilidadeISS', '1'));
      Servico.MunicipioIncidencia := AINIRec.ReadInteger(sSecao, 'MunicipioIncidencia', 0);
      Servico.UFPrestacao := AINIRec.ReadString(sSecao, 'UFPrestacao', '');
      Servico.ResponsavelRetencao := FProvider.StrToResponsavelRetencao(Ok, AINIRec.ReadString(sSecao, 'ResponsavelRetencao', ''));
      Servico.TipoLancamento := StrToTipoLancamento(Ok, AINIRec.ReadString(sSecao, 'TipoLancamento', 'P'));

      // Provedor ISSDSF
      Servico.Operacao := StrToOperacao(Ok, AINIRec.ReadString(sSecao, 'Operacao', ''));
      Servico.Tributacao := FProvider.StrToTributacao(Ok, AINIRec.ReadString(sSecao, 'Tributacao', ''));
      // Provedor ISSSaoPaulo
      Servico.ValorTotalRecebido := StringToFloatDef(AINIRec.ReadString(sSecao, 'ValorTotalRecebido', ''), 0);

      // Provedor IssNet e Padrão Nacional
      Servico.CodigoNBS := AINIRec.ReadString(sSecao, 'CodigoNBS', '');
      Servico.CodigoInterContr := AINIRec.ReadString(sSecao, 'CodigoInterContr', '');

      // Provedor SoftPlan
      Servico.CFPS := AINIRec.ReadString(sSecao, 'CFPS', '');

      // Provedor Giap Informações sobre o Endereço da Prestação de Serviço
      Servico.Endereco.Bairro := AINIRec.ReadString(sSecao, 'Bairro', '');
      Servico.Endereco.CEP := AINIRec.ReadString(sSecao, 'CEP', '');
      Servico.Endereco.xMunicipio := AINIRec.ReadString(sSecao, 'xMunicipio', '');
      Servico.Endereco.Complemento := AINIRec.ReadString(sSecao, 'Complemento', '');
      Servico.Endereco.Endereco := AINIRec.ReadString(sSecao, 'Logradouro', '');
      Servico.Endereco.Numero := AINIRec.ReadString(sSecao, 'Numero', '');
      Servico.Endereco.xPais := AINIRec.ReadString(sSecao, 'xPais', '');
      Servico.Endereco.UF := AINIRec.ReadString(sSecao, 'UF', '');

      // Provedor ISSBarueri
      Servico.LocalPrestacao := StrToLocalPrestacao(Ok, AINIRec.ReadString(sSecao, 'LocalPrestacao', '1'));
      Servico.PrestadoEmViasPublicas := AINIRec.ReadBool(sSecao, 'PrestadoEmViasPublicas', True);

      // Provedor SigISSWeb
      Servico.xFormaPagamento := AINIRec.ReadString(sSecao, 'FormaPagamento', '');
    end;

    i := 1;
    while true do
    begin
      sSecao := 'Itens' + IntToStrZero(i, 3);
      sFim := AINIRec.ReadString(sSecao, 'Descricao'  ,'FIM');

      if (sFim = 'FIM') then
        break;

      Item := Servico.ItemServico.New;

      Item.Descricao := sFim;
      Item.ItemListaServico := AINIRec.ReadString(sSecao, 'ItemListaServico', '');
      Item.CodServ := AINIRec.ReadString(sSecao, 'CodServico', '');
      Item.codLCServ := AINIRec.ReadString(sSecao, 'codLCServico', '');
      Item.CodigoCnae := AINIRec.ReadString(sSecao, 'CodigoCnae', '');

      Item.TipoUnidade := StrToUnidade(Ok, AINIRec.ReadString(sSecao, 'TipoUnidade', '2'));
      Item.Unidade := AINIRec.ReadString(sSecao, 'Unidade', '');
      Item.Quantidade := StringToFloatDef(AINIRec.ReadString(sSecao, 'Quantidade', ''), 0);
      Item.ValorUnitario := StringToFloatDef(AINIRec.ReadString(sSecao, 'ValorUnitario', ''), 0);

      Item.QtdeDiaria := StringToFloatDef(AINIRec.ReadString(sSecao, 'QtdeDiaria', ''), 0);
      Item.ValorTaxaTurismo := StringToFloatDef(AINIRec.ReadString(sSecao, 'ValorTaxaTurismo', ''), 0);

      Item.ValorDeducoes := StringToFloatDef(AINIRec.ReadString(sSecao, 'ValorDeducoes', ''), 0);
      Item.xJustDeducao := AINIRec.ReadString(sSecao, 'xJustDeducao', '');

      Item.AliqReducao := StringToFloatDef(AINIRec.ReadString(sSecao, 'AliqReducao', ''), 0);
      Item.ValorReducao := StringToFloatDef(AINIRec.ReadString(sSecao, 'ValorReducao', ''), 0);

      Item.ValorISS := StringToFloatDef(AINIRec.ReadString(sSecao, 'ValorISS', ''), 0);
      Item.Aliquota := StringToFloatDef(AINIRec.ReadString(sSecao, 'Aliquota', ''), 0);
      Item.BaseCalculo := StringToFloatDef(AINIRec.ReadString(sSecao, 'BaseCalculo', ''), 0);
      Item.DescontoIncondicionado := StringToFloatDef(AINIRec.ReadString(sSecao, 'DescontoIncondicionado', ''), 0);
      Item.DescontoCondicionado := StringToFloatDef(AINIRec.ReadString(sSecao, 'DescontoCondicionado', ''), 0);

      Item.AliqISSST := StringToFloatDef(AINIRec.ReadString(sSecao, 'AliqISSST', ''), 0);
      Item.ValorISSST := StringToFloatDef(AINIRec.ReadString(sSecao, 'ValorISSST', ''), 0);

      Item.ValorBCCSLL := StringToFloatDef(AINIRec.ReadString(sSecao, 'ValorBCCSLL', ''), 0);
      Item.AliqRetCSLL := StringToFloatDef(AINIRec.ReadString(sSecao, 'AliqRetCSLL', ''), 0);
      Item.ValorCSLL := StringToFloatDef(AINIRec.ReadString(sSecao, 'ValorCSLL', ''), 0);

      Item.ValorBCPIS := StringToFloatDef(AINIRec.ReadString(sSecao, 'ValorBCPIS', ''), 0);
      Item.AliqRetPIS := StringToFloatDef(AINIRec.ReadString(sSecao, 'AliqRetPIS', ''), 0);
      Item.ValorPIS := StringToFloatDef(AINIRec.ReadString(sSecao, 'ValorPIS', ''), 0);

      Item.ValorBCCOFINS := StringToFloatDef(AINIRec.ReadString(sSecao, 'ValorBCCOFINS', ''), 0);
      Item.AliqRetCOFINS := StringToFloatDef(AINIRec.ReadString(sSecao, 'AliqRetCOFINS', ''), 0);
      Item.ValorCOFINS := StringToFloatDef(AINIRec.ReadString(sSecao, 'ValorCOFINS', ''), 0);

      Item.ValorBCINSS := StringToFloatDef(AINIRec.ReadString(sSecao, 'ValorBCINSS', ''), 0);
      Item.AliqRetINSS := StringToFloatDef(AINIRec.ReadString(sSecao, 'AliqRetINSS', ''), 0);
      Item.ValorINSS := StringToFloatDef(AINIRec.ReadString(sSecao, 'ValorINSS', ''), 0);

      Item.ValorBCRetIRRF := StringToFloatDef(AINIRec.ReadString(sSecao, 'ValorBCRetIRRF', ''), 0);
      Item.AliqRetIRRF := StringToFloatDef(AINIRec.ReadString(sSecao, 'AliqRetIRRF', ''), 0);
      Item.ValorIRRF := StringToFloatDef(AINIRec.ReadString(sSecao, 'ValorIRRF', ''), 0);

      Item.ValorTotal := StringToFloatDef(AINIRec.ReadString(sSecao, 'ValorTotal', ''), 0);

      Item.Tributavel := FProvider.StrToSimNao(Ok, AINIRec.ReadString(sSecao, 'Tributavel', '1'));

      // IPM
      Item.TribMunPrestador := FProvider.StrToSimNao(Ok, AINIRec.ReadString(sSecao, 'TribMunPrestador', '1'));
      Item.CodMunPrestacao := AINIRec.ReadString(sSecao, 'CodMunPrestacao', '');
      Item.SituacaoTributaria := AINIRec.ReadInteger(sSecao, 'SituacaoTributaria', 0);
      Item.ValorISSRetido := StringToFloatDef(AINIRec.ReadString(sSecao, 'ValorISSRetido', ''), 0);
      Item.ValorTributavel := StringToFloatDef(AINIRec.ReadString(sSecao, 'ValorTributavel', ''), 0);
      Item.CodCNO := AINIRec.ReadString(sSecao, 'CodCNO', '');

      Inc(i);
    end;

    //Padrão Nacional
    sSecao := 'ComercioExterior';
    if AINIRec.SectionExists(sSecao) then
    begin
      Servico.comExt.mdPrestacao := StrTomdPrestacao(Ok, AINIRec.ReadString(sSecao, 'mdPrestacao', '0'));
      Servico.comExt.vincPrest := StrTovincPrest(Ok, AINIRec.ReadString(sSecao, 'vincPrest', '0'));
      Servico.comExt.tpMoeda := AINIRec.ReadInteger(sSecao, 'tpMoeda', 0);
      Servico.comExt.vServMoeda := StringToFloatDef(AINIRec.ReadString(sSecao, 'vServMoeda', '0'), 0);
      Servico.comExt.mecAFComexP := StrTomecAFComexP(Ok, AINIRec.ReadString(sSecao, 'mecAFComexP', '00'));
      Servico.comExt.mecAFComexT := StrTomecAFComexT(Ok, AINIRec.ReadString(sSecao, 'mecAFComexT', '00'));
      Servico.comExt.movTempBens := StrToMovTempBens(Ok, AINIRec.ReadString(sSecao, 'movTempBens', '00'));
      Servico.comExt.nDI := AINIRec.ReadString(sSecao, 'nDI', '');
      Servico.comExt.nRE := AINIRec.ReadString(sSecao, 'nRE', '');
      Servico.comExt.mdic := AINIRec.ReadInteger(sSecao, 'mdic', 0);
    end;

    //Padrão Nacional
    sSecao := 'LocacaoSubLocacao';
    if AINIRec.SectionExists(sSecao) then
    begin
      Servico.Locacao.categ := StrTocateg(Ok, AINIRec.ReadString(sSecao, 'categ', '1'));
      Servico.Locacao.objeto := StrToobjeto(Ok, AINIRec.ReadString(sSecao, 'objeto', '1'));
      Servico.Locacao.extensao := AINIRec.ReadString(sSecao, 'extensao', '');
      Servico.Locacao.nPostes := AINIRec.ReadInteger(sSecao, 'nPostes', 0);
    end;

    //Padrão Nacional
    sSecao := 'Evento';
    if AINIRec.SectionExists(sSecao) then
    begin
      Servico.Evento.xNome := AINIRec.ReadString(sSecao, 'xNome', '');
      Servico.Evento.dtIni := AINIRec.ReadDate(sSecao, 'dtIni', Now);
      Servico.Evento.dtFim := AINIRec.ReadDate(sSecao, 'dtFim', Now);
      Servico.Evento.idAtvEvt := AINIRec.ReadString(sSecao, 'idAtvEvt', '');

      Servico.Evento.Endereco.CEP := AINIRec.ReadString(sSecao, 'CEP', '');
      Servico.Evento.Endereco.xMunicipio := AINIRec.ReadString(sSecao, 'xMunicipio', '');
      Servico.Evento.Endereco.UF := AINIRec.ReadString(sSecao, 'UF', '');
      Servico.Evento.Endereco.Endereco := AINIRec.ReadString(sSecao, 'Logradouro', '');
      Servico.Evento.Endereco.Numero := AINIRec.ReadString(sSecao, 'Numero', '');
      Servico.Evento.Endereco.Complemento := AINIRec.ReadString(sSecao, 'Complemento', '');
      Servico.Evento.Endereco.Bairro := AINIRec.ReadString(sSecao, 'Bairro', '');
    end;

    //Padrão Nacional
    sSecao := 'Rodoviaria';
    if AINIRec.SectionExists(sSecao) then
    begin
      Servico.explRod.categVeic := StrTocategVeic(Ok, AINIRec.ReadString(sSecao, 'categVeic', '00'));
      Servico.explRod.nEixos := AINIRec.ReadInteger(sSecao, 'nEixos', 0);
      Servico.explRod.rodagem := StrTorodagem(Ok, AINIRec.ReadString(sSecao, 'rodagem', '1'));
      Servico.explRod.sentido := AINIRec.ReadString(sSecao, 'sentido', '');
      Servico.explRod.placa := AINIRec.ReadString(sSecao, 'placa', '');
      Servico.explRod.codAcessoPed := AINIRec.ReadString(sSecao, 'codAcessoPed', '');
      Servico.explRod.codContrato := AINIRec.ReadString(sSecao, 'codContrato', '');
    end;

    //Padrão Nacional
    sSecao := 'InformacoesComplementares';
    if AINIRec.SectionExists(sSecao) then
    begin
      Servico.infoCompl.idDocTec := AINIRec.ReadString(sSecao, 'idDocTec', '');
      Servico.infoCompl.docRef := AINIRec.ReadString(sSecao, 'docRef', '');
      Servico.infoCompl.xInfComp := AINIRec.ReadString(sSecao, 'xInfComp', '');
    end;

    sSecao := 'Valores';
    if AINIRec.SectionExists(sSecao) then
    begin
      Servico.Valores.ValorServicos := StringToFloatDef(AINIRec.ReadString(sSecao, 'ValorServicos', ''), 0);
      Servico.Valores.ValorDeducoes := StringToFloatDef(AINIRec.ReadString(sSecao, 'ValorDeducoes', ''), 0);
      Servico.Valores.AliquotaDeducoes := StringToFloatDef(AINIRec.ReadString(sSecao, 'AliquotaDeducoes', ''), 0);

      Servico.Valores.ValorPis := StringToFloatDef(AINIRec.ReadString(sSecao, 'ValorPis', ''), 0);
      Servico.Valores.AliquotaPis := StringToFloatDef(AINIRec.ReadString(sSecao, 'AliquotaPis', ''), 0);

      Servico.Valores.ValorCofins := StringToFloatDef(AINIRec.ReadString(sSecao, 'ValorCofins', ''), 0);
      Servico.Valores.AliquotaCofins := StringToFloatDef(AINIRec.ReadString(sSecao, 'AliquotaCofins', ''), 0);

      Servico.Valores.ValorInss := StringToFloatDef(AINIRec.ReadString(sSecao, 'ValorInss', ''), 0);
      Servico.Valores.AliquotaInss := StringToFloatDef(AINIRec.ReadString(sSecao, 'AliquotaInss', ''), 0);

      Servico.Valores.ValorIr := StringToFloatDef(AINIRec.ReadString(sSecao, 'ValorIr', ''), 0);
      Servico.Valores.AliquotaIr := StringToFloatDef(AINIRec.ReadString(sSecao, 'AliquotaIr', ''), 0);

      Servico.Valores.ValorCsll := StringToFloatDef(AINIRec.ReadString(sSecao, 'ValorCsll', ''), 0);
      Servico.Valores.AliquotaCsll := StringToFloatDef(AINIRec.ReadString(sSecao, 'AliquotaCsll', ''), 0);

      Servico.Valores.ISSRetido := FProvider.StrToSituacaoTributaria(Ok, AINIRec.ReadString(sSecao, 'ISSRetido', '0'));

      Servico.Valores.OutrasRetencoes := StringToFloatDef(AINIRec.ReadString(sSecao, 'OutrasRetencoes', ''), 0);
      Servico.Valores.DescontoIncondicionado := StringToFloatDef(AINIRec.ReadString(sSecao, 'DescontoIncondicionado', ''), 0);
      Servico.Valores.DescontoCondicionado := StringToFloatDef(AINIRec.ReadString(sSecao, 'DescontoCondicionado', ''), 0);

      Servico.Valores.BaseCalculo := StringToFloatDef(AINIRec.ReadString(sSecao, 'BaseCalculo', ''), 0);
      Servico.Valores.Aliquota := StringToFloatDef(AINIRec.ReadString(sSecao, 'Aliquota', ''), 0);
      Servico.Valores.AliquotaSN := StringToFloatDef(AINIRec.ReadString(sSecao, 'AliquotaSN', ''), 0);
      Servico.Valores.ValorIss := StringToFloatDef(AINIRec.ReadString(sSecao, 'ValorIss', ''), 0);
      Servico.Valores.ValorIssRetido := StringToFloatDef(AINIRec.ReadString(sSecao, 'ValorIssRetido', ''), 0);

      Servico.Valores.ValorLiquidoNfse := StringToFloatDef(AINIRec.ReadString(sSecao, 'ValorLiquidoNfse', ''), 0);

      Servico.Valores.ValorTotalNotaFiscal := StringToFloatDef(AINIRec.ReadString(sSecao, 'ValorTotalNotaFiscal', ''), 0);
      if Servico.Valores.ValorTotalNotaFiscal = 0 then
        Servico.Valores.ValorTotalNotaFiscal := Servico.Valores.ValorServicos - Servico.Valores.DescontoCondicionado - Servico.Valores.DescontoIncondicionado;

      //Padrão Nacional
      Servico.Valores.ValorRecebido := StringToFloatDef(AINIRec.ReadString(sSecao, 'ValorRecebido', ''), 0);
    end;

    //Padrão Nacional
    i := 1;
    while true do
    begin
      sSecao := 'DocumentosDeducoes' + IntToStrZero(i, 3);

      if not AINIRec.SectionExists(sSecao) then
        break;

      ItemDocDeducao := Servico.Valores.DocDeducao.New;

      ItemDocDeducao.chNFSe := AINIRec.ReadString(sSecao,'chNFSe', '');
      ItemDocDeducao.chNFe := AINIRec.ReadString(sSecao, 'chNFe', '');
      ItemDocDeducao.nDocFisc := AINIRec.ReadString(sSecao, 'nDocFisc', '');
      ItemDocDeducao.nDoc := AINIRec.ReadString(sSecao, 'nDoc', '');
      ItemDocDeducao.tpDedRed := StrTotpDedRed(Ok, AINIRec.ReadString(sSecao, 'tpDedRed', '1'));
      ItemDocDeducao.xDescOutDed := AINIRec.ReadString(sSecao, 'xDescOutDed', '');
      ItemDocDeducao.dtEmiDoc := AINIRec.ReadDate(sSecao, 'dtEmiDoc', Now);
      ItemDocDeducao.vDedutivelRedutivel := StringToFloatDef(AINIRec.ReadString(sSecao, 'vDedutivelRedutivel', ''), 0);
      ItemDocDeducao.vDeducaoReducao := StringToFloatDef(AINIRec.ReadString(sSecao, 'vDeducaoReducao', ''), 0);

      ItemDocDeducao.NFSeMun.cMunNFSeMun := AINIRec.ReadString(sSecao, 'cMunNFSeMun', '');
      ItemDocDeducao.NFSeMun.nNFSeMun := AINIRec.ReadString(sSecao, 'nNFSeMun', '');
      ItemDocDeducao.NFSeMun.cVerifNFSeMun := AINIRec.ReadString(sSecao, 'cVerifNFSeMun', '');

      ItemDocDeducao.NFNFS.nNFS := AINIRec.ReadString(sSecao, 'nNFS', '');
      ItemDocDeducao.NFNFS.modNFS := AINIRec.ReadString(sSecao, 'modNFS', '');
      ItemDocDeducao.NFNFS.serieNFS := AINIRec.ReadString(sSecao, 'serieNFS', '');

      sSecao := 'DocumentosDeducoesFornecedor' + IntToStrZero(i, 3);
      if AINIRec.SectionExists(sSecao) then
      begin
        ItemDocDeducao.fornec.Identificacao.CpfCnpj := AINIRec.ReadString(sSecao, 'CNPJCPF', '');
        ItemDocDeducao.fornec.Identificacao.InscricaoMunicipal := AINIRec.ReadString(sSecao, 'InscricaoMunicipal', '');
        ItemDocDeducao.fornec.Identificacao.Nif := AINIRec.ReadString(sSecao, 'NIF', '');
        ItemDocDeducao.fornec.Identificacao.cNaoNIF := StrToNaoNIF(Ok, AINIRec.ReadString(sSecao, 'cNaoNIF', '0'));
        ItemDocDeducao.fornec.Identificacao.CAEPF := AINIRec.ReadString(sSecao, 'CAEPF', '');

        ItemDocDeducao.fornec.Endereco.CEP := AINIRec.ReadString(sSecao, 'CEP', '');
        ItemDocDeducao.fornec.Endereco.xMunicipio := AINIRec.ReadString(sSecao, 'xMunicipio', '');
        ItemDocDeducao.fornec.Endereco.UF := AINIRec.ReadString(sSecao, 'UF', '');
        ItemDocDeducao.fornec.Endereco.Endereco := AINIRec.ReadString(sSecao, 'Logradouro', '');
        ItemDocDeducao.fornec.Endereco.Numero := AINIRec.ReadString(sSecao, 'Numero', '');
        ItemDocDeducao.fornec.Endereco.Complemento := AINIRec.ReadString(sSecao, 'Complemento', '');
        ItemDocDeducao.fornec.Endereco.Bairro := AINIRec.ReadString(sSecao, 'Bairro', '');

        ItemDocDeducao.fornec.Contato.Telefone := AINIRec.ReadString(sSecao, 'Telefone', '');
        ItemDocDeducao.fornec.Contato.Email := AINIRec.ReadString(sSecao, 'Email', '');
      end;

      Inc(i);
    end;

    //Padrão Nacional
    sSecao := 'tribMun';
    if AINIRec.SectionExists(sSecao) then
    begin
      Servico.Valores.tribMun.tribISSQN := StrTotribISSQN(Ok, AINIRec.ReadString(sSecao, 'tribISSQN', '1'));
      Servico.Valores.tribMun.cPaisResult := AINIRec.ReadInteger(sSecao, 'cPaisResult', 0);
      Servico.Valores.tribMun.tpBM := StrTotpBM(Ok, AINIRec.ReadString(sSecao, 'tpBM', '1'));
      Servico.Valores.tribMun.nBM := AINIRec.ReadString(sSecao, 'nBM', '');
      Servico.Valores.tribMun.vRedBCBM := StringToFloatDef(AINIRec.ReadString(sSecao, 'vRedBCBM', ''), 0);
      Servico.Valores.tribMun.pRedBCBM := StringToFloatDef(AINIRec.ReadString(sSecao, 'pRedBCBM', ''), 0);
      Servico.Valores.tribMun.tpSusp := StrTotpSusp(Ok, AINIRec.ReadString(sSecao, 'tpSusp', ''));
      Servico.Valores.tribMun.nProcesso := AINIRec.ReadString(sSecao, 'nProcesso', '');
      Servico.Valores.tribMun.tpImunidade := StrTotpImunidade(Ok, AINIRec.ReadString(sSecao, 'tpImunidade', ''));
      Servico.Valores.tribMun.pAliq := StringToFloatDef(AINIRec.ReadString(sSecao, 'pAliq', ''), 0);
      Servico.Valores.tribMun.tpRetISSQN := StrTotpRetISSQN(Ok, AINIRec.ReadString(sSecao, 'tpRetISSQN', ''));
    end;

    //Padrão Nacional
    sSecao := 'tribFederal';
    if AINIRec.SectionExists(sSecao) then
    begin
      Servico.Valores.tribFed.CST := StrToCST(Ok, AINIRec.ReadString(sSecao, 'CST', ''));
      Servico.Valores.tribFed.vBCPisCofins := StringToFloatDef(AINIRec.ReadString(sSecao, 'vBCPisCofins', ''), 0);
      Servico.Valores.tribFed.pAliqPis := StringToFloatDef(AINIRec.ReadString(sSecao, 'pAliqPis', ''), 0);
      Servico.Valores.tribFed.pAliqCofins := StringToFloatDef(AINIRec.ReadString(sSecao, 'pAliqCofins' ,''), 0);
      Servico.Valores.tribFed.vPis := StringToFloatDef(AINIRec.ReadString(sSecao, 'vPis', ''), 0);
      Servico.Valores.tribFed.vCofins := StringToFloatDef(AINIRec.ReadString(sSecao, 'vCofins', ''), 0);
      Servico.Valores.tribFed.tpRetPisCofins := StrTotpRetPisCofins(Ok, AINIRec.ReadString(sSecao, 'tpRetPisCofins', ''));
      Servico.Valores.tribFed.vRetCP := StringToFloatDef(AINIRec.ReadString(sSecao, 'vRetCP', ''), 0);
      Servico.Valores.tribFed.vRetIRRF := StringToFloatDef(AINIRec.ReadString(sSecao, 'vRetIRRF', ''), 0);
      Servico.Valores.tribFed.vRetCSLL := StringToFloatDef(AINIRec.ReadString(sSecao, 'vRetCSLL', ''), 0);
    end;

    //Padrão Nacional
    sSecao := 'totTrib';
    if AINIRec.SectionExists(sSecao) then
    begin
      Servico.Valores.totTrib.indTotTrib := StrToindTotTrib(Ok, AINIRec.ReadString(sSecao, 'indTotTrib', '0'));
      Servico.Valores.totTrib.pTotTribSN := StringToFloatDef(AINIRec.ReadString(sSecao, 'pTotTribSN', ''), 0);

      Servico.Valores.totTrib.vTotTribFed := StringToFloatDef(AINIRec.ReadString(sSecao, 'vTotTribFed', ''), 0);
      Servico.Valores.totTrib.vTotTribEst := StringToFloatDef(AINIRec.ReadString(sSecao, 'vTotTribEst', ''), 0);
      Servico.Valores.totTrib.vTotTribMun := StringToFloatDef(AINIRec.ReadString(sSecao, 'vTotTribMun', ''), 0);

      Servico.Valores.totTrib.pTotTribFed := StringToFloatDef(AINIRec.ReadString(sSecao, 'pTotTribFed', ''), 0);
      Servico.Valores.totTrib.pTotTribEst := StringToFloatDef(AINIRec.ReadString(sSecao, 'pTotTribEst', ''), 0);
      Servico.Valores.totTrib.pTotTribMun := StringToFloatDef(AINIRec.ReadString(sSecao, 'pTotTribMun', ''), 0);
    end;

    // Condição de Pagamento usado pelo provedor Betha versão 1 do Layout da ABRASF
    sSecao := 'CondicaoPagamento';
    if AINIRec.SectionExists(sSecao) then
    begin
      CondicaoPagamento.QtdParcela := AINIRec.ReadInteger(sSecao, 'QtdParcela', 0);
      CondicaoPagamento.Condicao := FProvider.StrToCondicaoPag(Ok, AINIRec.ReadString(sSecao, 'Condicao', 'A_VISTA'));

      // Provedor NFEletronica
      CondicaoPagamento.DataVencimento := AINIRec.ReadDate(sSecao, 'DataVencimento', Now);
      CondicaoPagamento.InstrucaoPagamento := AINIRec.ReadString(sSecao, 'InstrucaoPagamento', '');
      CondicaoPagamento.QtdParcela := AINIRec.ReadInteger(sSecao, 'CodigoVencimento', 0);
    end;

    i := 1;
    while true do
    begin
      sSecao := 'Parcelas' + IntToStrZero(i, 2);
      sFim := AINIRec.ReadString(sSecao, 'Parcela'  ,'FIM');

      if (sFim = 'FIM') then
        break;

      ItemParcelas := CondicaoPagamento.Parcelas.New;
      ItemParcelas.Parcela := sFim;
      ItemParcelas.DataVencimento := AINIRec.ReadDate(sSecao, 'DataVencimento', Now);
      ItemParcelas.Valor := StringToFloatDef(AINIRec.ReadString(sSecao, 'Valor', ''), 0);

      Inc(i);
    end;
  end;

  Result := True;
end;

function TNotaFiscal.LerNfseIni(AINIRec: TMemIniFile): Boolean;
var
  FProvider: IACBrNFSeXProvider;
  sSecao, sFim: string;
  Ok: Boolean;
  i: Integer;
  Item: TItemServicoCollectionItem;
  ItemDocDeducao: TDocDeducaoCollectionItem;
  ItemParcelas: TParcelasCollectionItem;
begin
  FProvider := TACBrNFSeX(FACBrNFSe).Provider;

  if not Assigned(FProvider) then
    raise EACBrNFSeException.Create(ERR_SEM_PROVEDOR);

  with FNFSe do
  begin

  end;

  Result := True;
end;

function TNotaFiscal.LerArqIni(const AIniString: string): Boolean;
var
  INIRec: TMemIniFile;
  TipoXML: string;
begin
  INIRec := TMemIniFile.Create('');

  try
    LerIniArquivoOuString(AIniString, INIRec);

    TipoXML := INIRec.ReadString('IdentificacaoNFSe', 'TipoXML', '');

    if (TipoXML = '') or (TipoXML = 'RPS') then
      LerRpsIni(INIRec)
    else
      LerNfseIni(INIRec);
  finally
    INIRec.Free;
  end;
end;

function TNotaFiscal.GerarNFSeIni: string;
var
  I: integer;
  sSecao: string;
  INIRec: TMemIniFile;
  IniNFSe: TStringList;
  FProvider: IACBrNFSeXProvider;
  fornec: TInfoPessoa;
begin
  Result:= '';
  FProvider := TACBrNFSeX(FACBrNFSe).Provider;

  INIRec := TMemIniFile.Create('');
  try
    with FNFSe do
    begin
      //Provedor Infisc - Layout Proprio
      sSecao:= 'IdentificacaoNFSe';

      if tpXML = txmlRPS then
        INIRec.WriteString(sSecao, 'TipoXML', 'RPS')
      else
        INIRec.WriteString(sSecao, 'TipoXML', 'NFSE');

      INIRec.WriteString(sSecao, 'Numero', Numero);
      INIRec.WriteString(sSecao, 'NumeroLote', NumeroLote);
      INIRec.WriteString(sSecao, 'StatusNFSe', StatusNFSeToStr(SituacaoNfse));

      //Adicionado para que a informação seja devolvida para quem usa a lib, não considerar na rotina de leitura.
      if CodigoVerificacao <> '' then
        INIRec.WriteString(sSecao, 'CodigoVerificacao', CodigoVerificacao);

      sSecao:= 'IdentificacaoRps';
      INIRec.WriteString(sSecao, 'SituacaoTrib', FProvider.SituacaoTribToStr(SituacaoTrib));
      INIRec.WriteString(sSecao, 'Producao', FProvider.SimNaoToStr(Producao));
      INIRec.WriteString(sSecao, 'Status', FProvider.StatusRPSToStr(StatusRps));
      INIRec.WriteString(sSecao, 'OutrasInformacoes', OutrasInformacoes);

      //Provedores CTA, ISSBarueri, ISSSDSF, ISSSaoPaulo, Simple e SmarAPD.
      INIRec.WriteString(sSecao, 'TipoTributacaoRps', FProvider.TipoTributacaoRPSToStr(TipoTributacaoRPS));

      // Provedores ISSDSF e Siat
      INIRec.WriteString(sSecao, 'SeriePrestacao', SeriePrestacao);
      INIRec.WriteString(sSecao, 'Numero', IdentificacaoRps.Numero);
      INIRec.WriteString(sSecao, 'Serie', IdentificacaoRps.Serie);
      INIRec.WriteString(sSecao, 'Tipo', FProvider.TipoRPSToStr(IdentificacaoRps.Tipo));

      if DataEmissao > 0 then
        INIRec.WriteDate(sSecao, 'DataEmissao', DataEmissao)
      else
        INIRec.WriteDate(sSecao, 'DataEmissao', Now);

      if Competencia > 0 then
        INIRec.WriteDate(sSecao, 'Competencia', Competencia)
      else
        IniRec.WriteDate(sSecao, 'Competencia', Now);

      INIRec.WriteString(sSecao, 'NaturezaOperacao', NaturezaOperacaoToStr(NaturezaOperacao));

      // Provedor Tecnos
      INIRec.WriteFloat(sSecao, 'PercentualCargaTributaria', PercentualCargaTributaria);
      INIRec.WriteFloat(sSecao, 'ValorCargaTributaria', ValorCargaTributaria);
      INIRec.WriteFloat(sSecao, 'PercentualCargaTributariaMunicipal', PercentualCargaTributariaMunicipal);
      INIRec.WriteFloat(sSecao, 'ValorCargaTributariaMunicipal', ValorCargaTributariaMunicipal);
      INIRec.WriteFloat(sSecao, 'PercentualCargaTributariaEstadual', PercentualCargaTributariaEstadual);
      INIRec.WriteFloat(sSecao, 'ValorCargaTributariaEstadual', ValorCargaTributariaEstadual);

      //Padrão Nacional
      INIRec.WriteString(sSecao, 'verAplic', verAplic);
      INIRec.WriteString(sSecao, 'tpEmit', tpEmitToStr(tpEmit));

      //Provedor Governa
      INIRec.WriteString(sSecao, 'RegRec', RegRecToStr(RegRec));

      INIRec.WriteString(sSecao, 'InformacoesComplementares', InformacoesComplementares);

      if RpsSubstituido.Numero <> '' then
      begin
        sSecao:= 'RpsSubstituido';
        INIRec.WriteString(sSecao, 'Numero', RpsSubstituido.Numero);
        INIRec.WriteString(sSecao, 'Serie', RpsSubstituido.Serie);
        INIRec.WriteString(sSecao, 'Tipo', FProvider.TipoRPSToStr(RpsSubstituido.Tipo));
      end;

      sSecao:= 'Prestador';
      INIRec.WriteString(sSecao, 'Regime', FProvider.RegimeEspecialTributacaoToStr(RegimeEspecialTributacao));
      INIRec.WriteString(sSecao, 'OptanteSN', FProvider.SimNaoToStr(OptanteSimplesNacional));
      INIRec.WriteString(sSecao, 'opSimpNac', OptanteSNToStr(OptanteSN));
      INIRec.WriteString(sSecao, 'IncentivadorCultural', FProvider.SimNaoToStr(IncentivadorCultural));
      INIRec.WriteString(sSecao, 'CNPJ', Prestador.IdentificacaoPrestador.CpfCnpj);
      INIRec.WriteString(sSecao, 'InscricaoMunicipal', Prestador.IdentificacaoPrestador.InscricaoMunicipal);
      INIRec.WriteString(sSecao, 'NIF', Prestador.IdentificacaoPrestador.NIF);
      INIRec.WriteString(sSecao, 'cNaoNIF', NaoNIFToStr(Prestador.IdentificacaoPrestador.cNaoNIF));
      INIRec.WriteString(sSecao, 'CAEPF', Prestador.IdentificacaoPrestador.CAEPF);

      // Para o provedor ISSDigital deve-se informar tambem:
      INIRec.WriteString(sSecao, 'RazaoSocial', Prestador.RazaoSocial);
      INIRec.WriteString(sSecao, 'NomeFantasia', Prestador.NomeFantasia);
      INIRec.WriteString(sSecao, 'Logradouro', Prestador.Endereco.Endereco);
      INIRec.WriteString(sSecao, 'Numero', Prestador.Endereco.Numero);
      INIRec.WriteString(sSecao, 'Complemento', Prestador.Endereco.Complemento);
      INIRec.WriteString(sSecao, 'Bairro', Prestador.Endereco.Bairro);
      INIRec.WriteString(sSecao, 'CodigoMunicipio', Prestador.Endereco.CodigoMunicipio);
      INIRec.WriteString(sSecao, 'UF',  Prestador.Endereco.UF);
      INIRec.WriteInteger(sSecao, 'CodigoPais', Prestador.Endereco.CodigoPais);
      INIRec.WriteString(sSecao, 'xPais', Prestador.Endereco.xPais);
      INIRec.WriteString(sSecao, 'CEP', Prestador.Endereco.CEP);
      INIRec.WriteString(sSecao, 'Telefone', Prestador.Contato.Telefone);
      INIRec.WriteString(sSecao, 'Email', Prestador.Contato.Email);

      // Para o provedor WebFisco
      if Prestador.Anexo <> '' then
        INIRec.WriteString(sSecao, 'Anexo', Prestador.Anexo);

      if Prestador.ValorReceitaBruta > 0 then
        INIRec.WriteFloat(sSecao, 'ValorReceitaBruta', Prestador.ValorReceitaBruta);

      sSecao:= 'Tomador';
      INIRec.WriteString(sSecao, 'Tipo', FProvider.TipoPessoaToStr(Tomador.IdentificacaoTomador.Tipo));
      INIRec.WriteString(sSecao, 'CNPJCPF', Tomador.IdentificacaoTomador.CpfCnpj);
      INIRec.WriteString(sSecao, 'InscricaoMunicipal', Tomador.IdentificacaoTomador.InscricaoMunicipal);
      INIRec.WriteString(sSecao, 'NIF', Tomador.IdentificacaoTomador.NIF);
      INIRec.WriteString(sSecao, 'cNaoNIF', NaoNIFToStr(Tomador.IdentificacaoTomador.cNaoNIF));
      INIRec.WriteString(sSecao, 'CAEPF', Tomador.IdentificacaoTomador.CAEPF);
      //Exigido pelo provedor Equiplano
      INIRec.WriteString(sSecao, 'InscricaoEstadual', Tomador.IdentificacaoTomador.InscricaoEstadual);
      INIRec.WriteString(sSecao, 'RazaoSocial', Tomador.RazaoSocial);

      INIRec.WriteString(sSecao, 'EnderecoInformado', FProvider.SimNaoOpcToStr(Tomador.Endereco.EnderecoInformado));
      INIRec.WriteString(sSecao, 'TipoLogradouro', Tomador.Endereco.TipoLogradouro);
      INIRec.WriteString(sSecao, 'Logradouro', Tomador.Endereco.Endereco);
      INIRec.WriteString(sSecao, 'Numero', Tomador.Endereco.Numero);
      INIRec.WriteString(sSecao, 'Complemento', Tomador.Endereco.Complemento);
      INIRec.WriteString(sSecao, 'Bairro', Tomador.Endereco.Bairro);
      INIRec.WriteString(sSecao, 'CodigoMunicipio', Tomador.Endereco.CodigoMunicipio);
      INIRec.WriteString(sSecao, 'xMunicipio', Tomador.Endereco.xMunicipio);
      INIRec.WriteString(sSecao, 'UF', Tomador.Endereco.UF);
      INIRec.WriteInteger(sSecao, 'CodigoPais', Tomador.Endereco.CodigoPais);
      INIRec.WriteString(sSecao, 'CEP', Tomador.Endereco.CEP);
      //Exigido pelo provedor Equiplano
      INIRec.WriteString(sSecao, 'xPais', Tomador.Endereco.xPais);
      INIRec.WriteString(sSecao, 'Telefone', Tomador.Contato.Telefone);
      INIRec.WriteString(sSecao, 'Email', Tomador.Contato.Email);
      INIRec.WriteString(sSecao, 'AtualizaTomador', FProvider.SimNaoToStr(Tomador.AtualizaTomador));
      INIRec.WriteString(sSecao, 'TomadorExterior', FProvider.SimNaoToStr(Tomador.TomadorExterior));

      if Intermediario.Identificacao.CpfCnpj <> '' then
      begin
        sSecao:= 'Intermediario';
        INIRec.WriteString(sSecao, 'CNPJCPF', Intermediario.Identificacao.CpfCnpj);
        INIRec.WriteString(sSecao, 'InscricaoMunicipal', Intermediario.Identificacao.InscricaoMunicipal);
        INIRec.WriteString(sSecao, 'NIF', Intermediario.Identificacao.NIF);
        INIRec.WriteString(sSecao, 'cNaoNIF', NaoNIFToStr(Intermediario.Identificacao.cNaoNIF));
        INIRec.WriteString(sSecao, 'CAEPF', Intermediario.Identificacao.CAEPF);
        INIRec.WriteString(sSecao, 'RazaoSocial', Intermediario.RazaoSocial);

        if (Intermediario.Endereco.Endereco <> '') or (Intermediario.Endereco.CEP <> '')then
        begin
          INIRec.WriteString(sSecao, 'Logradouro', Intermediario.Endereco.Endereco);
          INIRec.WriteString(sSecao, 'Numero', Intermediario.Endereco.Numero);
          INIRec.WriteString(sSecao, 'Complemento', Intermediario.Endereco.Complemento);
          INIRec.WriteString(sSecao, 'Bairro', Intermediario.Endereco.Bairro);
          INIRec.WriteString(sSecao, 'CodigoMunicipio', Intermediario.Endereco.CodigoMunicipio);
          INIRec.WriteString(sSecao, 'xMunicipio', Intermediario.Endereco.xMunicipio);
          INIRec.WriteString(sSecao, 'UF', Intermediario.Endereco.UF);
          INIRec.WriteInteger(sSecao, 'CodigoPais', Intermediario.Endereco.CodigoPais);
          INIRec.WriteString(sSecao, 'CEP', Intermediario.Endereco.CEP);
          INIRec.WriteString(sSecao, 'xPais', Intermediario.Endereco.xPais);
        end;

        INIRec.WriteString(sSecao, 'Telefone', Intermediario.Contato.Telefone);
        INIRec.WriteString(sSecao, 'Email', Intermediario.Contato.Email);
      end;

      if ConstrucaoCivil.CodigoObra <> '' then
      begin
        sSecao:= 'ConstrucaoCivil';
        INIRec.WriteString(sSecao, 'CodigoObra', ConstrucaoCivil.CodigoObra);
        INIRec.WriteString(sSecao, 'Art', ConstrucaoCivil.Art);
        //Padrão Nacional
        INIRec.WriteString(sSecao, 'inscImobFisc', ConstrucaoCivil.inscImobFisc);

        //Padrão Nacional
        if (ConstrucaoCivil.Endereco.Endereco <> '') or (ConstrucaoCivil.Endereco.CEP <> '') then
        begin
          INIRec.WriteString(sSecao, 'CEP', ConstrucaoCivil.Endereco.CEP);
          INIRec.WriteString(sSecao, 'xMunicipio', ConstrucaoCivil.Endereco.XMunicipio);
          INIRec.WriteString(sSecao, 'UF', ConstrucaoCivil.Endereco.UF);
          INIRec.WriteString(sSecao, 'Logradouro', ConstrucaoCivil.Endereco.Endereco);
          INIRec.WriteString(sSecao, 'Numero', ConstrucaoCivil.Endereco.Numero);
          INIRec.WriteString(sSecao, 'Complemento', ConstrucaoCivil.Endereco.Complemento);
          INIRec.WriteString(sSecao, 'Bairro', ConstrucaoCivil.Endereco.Bairro);
        end;
      end;

      sSecao:= 'Servico';
      INIRec.WriteString(sSecao, 'ItemListaServico', Servico.ItemListaServico);
      INIRec.WriteString(sSecao, 'CodigoCnae', Servico.CodigoCnae);
      INIRec.WriteString(sSecao, 'CodigoTributacaoMunicipio', Servico.CodigoTributacaoMunicipio);
      INIRec.WriteString(sSecao, 'Discriminacao', ChangeLineBreak(Servico.Discriminacao, FProvider.ConfigGeral.QuebradeLinha));
      INIRec.WriteString(sSecao, 'CodigoMunicipio', Servico.CodigoMunicipio);
      INIRec.WriteInteger(sSecao, 'CodigoPais', Servico.CodigoPais);
      INIRec.WriteString(sSecao, 'ExigibilidadeISS', FProvider.ExigibilidadeISSToStr(Servico.ExigibilidadeISS));
      INIRec.WriteInteger(sSecao, 'MunicipioIncidencia', Servico.MunicipioIncidencia);
      INIRec.WriteString(sSecao, 'UFPrestacao', Servico.UFPrestacao);
      INIRec.WriteString(sSecao, 'ResponsavelRetencao', FProvider.ResponsavelRetencaoToStr(Servico.ResponsavelRetencao));
      //Provedor ISSDSF
      INIRec.WriteString(sSecao, 'Operacao', OperacaoToStr(Servico.Operacao));
      INIRec.WriteString(sSecao, 'Tributacao', FProvider.TributacaoToStr(Servico.Tributacao));
      //Padrão Nacional e IssNet
      INIRec.WriteString(sSecao, 'CodigoNBS', Servico.CodigoNBS);
      INIRec.WriteString(sSecao, 'CodigoInterContr', Servico.CodigoInterContr);


      //Lista de Itens, xxx pode variar de 001-999
      for I := 0 to Servico.ItemServico.Count - 1 do
      begin
        sSecao:= 'Itens' + IntToStrZero(I + 1, 3);

        INIRec.WriteString(sSecao, 'Descricao', Servico.ItemServico.Items[I].Descricao);
        INIRec.WriteString(sSecao, 'CodServico', Servico.ItemServico.Items[I].CodServ);
        INIRec.WriteString(sSecao, 'codLCServico', Servico.ItemServico.Items[I].CodLCServ);
        INIRec.WriteString(sSecao, 'CodigoCnae', Servico.ItemServico.Items[I].CodigoCnae);
        INIRec.WriteString(sSecao, 'ItemListaServico', Servico.ItemServico.Items[I].ItemListaServico);
        INIRec.WriteFloat(sSecao, 'Quantidade', Servico.ItemServico.Items[I].Quantidade);
        INIRec.WriteFloat(sSecao, 'ValorUnitario', Servico.ItemServico.Items[I].ValorUnitario);
        INIRec.WriteFloat(sSecao, 'ValorDeducoes', Servico.ItemServico.Items[I].ValorDeducoes);
        INIRec.WriteFloat(sSecao, 'ValorIss', Servico.ItemServico.Items[I].ValorISS);
        INIRec.WriteFloat(sSecao, 'Aliquota', Servico.ItemServico.Items[I].Aliquota);
        INIRec.WriteFloat(sSecao, 'BaseCalculo', Servico.ItemServico.Items[I].BaseCalculo);
        INIRec.WriteFloat(sSecao, 'DescontoIncondicionado', Servico.ItemServico.Items[I].DescontoIncondicionado);
        INIRec.WriteFloat(sSecao, 'ValorTotal', Servico.ItemServico.Items[I].ValorTotal);
        INIRec.WriteString(sSecao, 'Tributavel', FProvider.SimNaoToStr(Servico.ItemServico.Items[I].Tributavel));
      end;

      //Padrão Nacional
      if (Servico.comExt.tpMoeda <> 0) or (Servico.comExt.vServMoeda > 0) then
      begin
        sSecao := 'ComercioExterior';
        INIRec.WriteString(sSecao, 'mdPrestacao', mdPrestacaoToStr(Servico.comExt.mdPrestacao));
        INIRec.WriteString(sSecao, 'vincPrest', vincPrestToStr(Servico.comExt.vincPrest));
        INIRec.WriteInteger(sSecao, 'tpMoeda', Servico.comExt.tpMoeda);
        INIRec.WriteFloat(sSecao, 'vServMoeda', Servico.comExt.vServMoeda);
        INIRec.WriteString(sSecao, 'mecAFComexP', mecAFComexPToStr(Servico.comExt.mecAFComexP));
        INIRec.WriteString(sSecao, 'mecAFComexT', mecAFComexTToStr(Servico.comExt.mecAFComexT));
        INIRec.WriteString(sSecao, 'movTempBens', MovTempBensToStr(Servico.comExt.movTempBens));
        INIRec.WriteString(sSecao, 'nDI', Servico.comExt.nDI);
        INIRec.WriteString(sSecao, 'nRE', Servico.comExt.nRE);
        INIRec.WriteInteger(sSecao, 'mdic', Servico.comExt.mdic);
      end;

      //Padrão Nacional
      if (Servico.Locacao.extensao <> '') or (Servico.Locacao.nPostes > 0)then
      begin
        sSecao := 'LocacaoSubLocacao';
        INIRec.WriteString(sSecao, 'categ', categToStr(Servico.Locacao.categ));
        INIRec.WriteString(sSecao, 'objeto', objetoToStr(Servico.Locacao.objeto));
        INIRec.WriteString(sSecao, 'extensao', Servico.Locacao.extensao);
        INIRec.WriteInteger(sSecao, 'nPostes', Servico.Locacao.nPostes);
      end;

      //Padrão Nacional
      if (Servico.Evento.xNome <> '') or (Servico.Evento.dtIni > 0) or (Servico.Evento.dtFim > 0)then
      begin
        sSecao := 'Evento';
        INIRec.WriteString(sSecao, 'xNome', Servico.Evento.xNome);
        INIRec.WriteDate(sSecao, 'dtIni', Servico.Evento.dtIni);
        INIRec.WriteDate(sSecao, 'dtFim', Servico.Evento.dtFim);
        INIRec.WriteString(sSecao, 'idAtvEvt', Servico.Evento.idAtvEvt);
        INIRec.WriteString(sSecao, 'CEP', Servico.Evento.Endereco.CEP);
        INIRec.WriteString(sSecao, 'xMunicipio', Servico.Evento.Endereco.xMunicipio);
        INIRec.WriteString(sSecao, 'UF', Servico.Evento.Endereco.UF);
        INIRec.WriteString(sSecao, 'Logradouro', Servico.Evento.Endereco.Endereco);
        INIRec.WriteString(sSecao, 'Complemento', Servico.Evento.Endereco.Complemento);
        INIRec.WriteString(sSecao, 'Bairro', Servico.Evento.Endereco.Bairro);
      end;

      //Padrão Nacional
      if (Servico.explRod.placa <> '') then
      begin
        sSecao := 'Rodoviaria';
        INIRec.WriteString(sSecao, 'categVeic', categVeicToStr(Servico.ExplRod.categVeic));
        INIRec.WriteInteger(sSecao, 'nEixos', Servico.ExplRod.nEixos);
        INIRec.WriteString(sSecao, 'rodagem', rodagemToStr(Servico.ExplRod.rodagem));
        INIRec.WriteString(sSecao, 'placa', Servico.ExplRod.placa);
        INIRec.WriteString(sSecao, 'sentido', Servico.ExplRod.sentido);
        INIRec.WriteString(sSecao, 'codAcessoPed', Servico.ExplRod.codAcessoPed);
        INIRec.WriteString(sSecao, 'codContrato', Servico.ExplRod.codContrato);
      end;

      if (Servico.infoCompl.idDocTec <> '') or (Servico.infoCompl.docRef <> '') or (Servico.infoCompl.xInfComp <> '') then
      begin
        sSecao := 'InformacoesComplementares';
        INIRec.WriteString(sSecao, 'idDocTec', Servico.infoCompl.idDocTec);
        INIRec.WriteString(sSecao, 'docRef', Servico.infoCompl.docRef);
        INIRec.WriteString(sSecao, 'xInfComp', Servico.infoCompl.xInfComp);
      end;

      sSecao:= 'Valores';
      INIRec.WriteFloat(sSecao, 'ValorServicos', Servico.Valores.ValorServicos);
      INIRec.WriteFloat(sSecao, 'ValorDeducoes', Servico.Valores.ValorDeducoes);
      INIRec.WriteFloat(sSecao, 'AliquotaDeducoes', Servico.Valores.AliquotaDeducoes);
      INIRec.WriteFloat(sSecao, 'ValorPis', Servico.Valores.ValorPis);
      INIRec.WriteFloat(sSecao, 'AliquotaPis', Servico.Valores.AliquotaPis);
      INIRec.WriteFloat(sSecao, 'ValorCofins', Servico.Valores.ValorCofins);
      INIRec.WriteFloat(sSecao, 'AliquotaCofins', Servico.Valores.AliquotaCofins);
      INIRec.WriteFloat(sSecao, 'ValorInss', Servico.Valores.ValorInss);
      INIRec.WriteFloat(sSecao, 'ValorIr', Servico.Valores.ValorIr);
      INIRec.WriteFloat(sSecao, 'ValorCsll', Servico.Valores.ValorCsll);
      INIRec.WriteString(sSecao, 'ISSRetido', FProvider.SituacaoTributariaToStr(Servico.Valores.IssRetido));
      INIRec.WriteFloat(sSecao, 'OutrasRetencoes', Servico.Valores.OutrasRetencoes);
      INIRec.WriteFloat(sSecao, 'DescontoIncondicionado', Servico.Valores.DescontoIncondicionado);
      INIRec.WriteFloat(sSecao, 'DescontoCondicionado', Servico.Valores.DescontoCondicionado);
      INIRec.WriteFloat(sSecao, 'BaseCalculo', Servico.Valores.BaseCalculo);
      INIRec.WriteFloat(sSecao, 'Aliquota', Servico.Valores.Aliquota);
      INIRec.WriteFloat(sSecao, 'AliquotaSN', Servico.Valores.AliquotaSN);
      INIRec.WriteFloat(sSecao, 'ValorIss', Servico.Valores.ValorIss);
      INIRec.WriteFloat(sSecao, 'ValorIssRetido', Servico.Valores.ValorIssRetido);
      INIRec.WriteFloat(sSecao, 'ValorLiquidoNfse', Servico.Valores.ValorLiquidoNfse);
      INIRec.WriteFloat(sSecao, 'ValorTotalNotaFiscal', Servico.Valores.ValorTotalNotaFiscal);
      INIRec.WriteFloat(sSecao, 'ValorRecebido', Servico.Valores.ValorRecebido);

      //Padrão Nacional
      for i := 0 to Servico.Valores.DocDeducao.Count - 1 do
      begin
        sSecao := 'DocumentosDeducoes' + IntToStrZero(i+1, 3);
        INIRec.WriteString(sSecao, 'chNFSe', Servico.Valores.DocDeducao[i].chNFSe);
        INIRec.WriteString(sSecao, 'chNFe', Servico.Valores.DocDeducao[i].chNFe);
        INIRec.WriteString(sSecao, 'nDoc', Servico.Valores.DocDeducao[i].nDoc);
        INIRec.WriteString(sSecao, 'tpDedRed', tpDedRedToStr(Servico.Valores.DocDeducao[i].tpDedRed));
        INIRec.WriteString(sSecao, 'xDescOutDed', Servico.Valores.DocDeducao[i].xDescOutDed);
        INIRec.WriteDate(sSecao, 'dtEmiDoc', Servico.Valores.DocDeducao[i].dtEmiDoc);
        INIRec.WriteFloat(sSecao, 'vDedutivelRedutivel', Servico.Valores.DocDeducao[i].vDedutivelRedutivel);
        INIRec.WriteFloat(sSecao, 'vDeducaoReducao', Servico.Valores.DocDeducao[i].vDeducaoReducao);
        INIRec.WriteString(sSecao, 'cMunNFSeMun', Servico.Valores.DocDeducao[i].NFSeMun.cMunNFSeMun);
        INIRec.WriteString(sSecao, 'nNFSeMun', Servico.Valores.DocDeducao[i].NFSeMun.nNFSeMun);
        INIRec.WriteString(sSecao, 'cVerifNFSeMun', Servico.Valores.DocDeducao[i].NFseMun.cVerifNFSeMun);
        INIRec.WriteString(sSecao, 'nNFS', Servico.Valores.DocDeducao[i].NFNFS.nNFS);
        INIRec.WriteString(sSecao, 'modNFS', Servico.Valores.DocDeducao[i].NFNFS.modNFS);
        INIRec.WriteString(sSecao, 'serieNFS', Servico.Valores.DocDeducao[i].NFNFS.serieNFS);

        if Servico.Valores.DocDeducao[i].fornec.Identificacao.CpfCnpj <> '' then
        begin
          sSecao := 'DocumentosDeducoesFornecedor' + IntToStrZero(i+1, 3);
          fornec := Servico.Valores.DocDeducao[i].fornec;

          INIRec.WriteString(sSecao, 'CNPJCPF', fornec.Identificacao.CpfCnpj);
          INIRec.WriteString(sSecao, 'InscricaoMunicipal', fornec.Identificacao.InscricaoMunicipal);
          INIRec.WriteString(sSecao, 'NIF', fornec.Identificacao.NIF);
          INIRec.WriteString(sSecao, 'cNaoNIF', NaoNIFToStr(fornec.Identificacao.cNaoNIF));
          INIRec.WriteString(sSecao, 'CAEEPF', fornec.Identificacao.CAEPF);

          INIRec.WriteString(sSecao, 'Logradouro', fornec.Endereco.Endereco);
          INIRec.WriteString(sSecao, 'Numero', fornec.Endereco.Numero);
          INIRec.WriteString(sSecao, 'Complemento', fornec.Endereco.Complemento);
          INIRec.WriteString(sSecao, 'Bairro', fornec.Endereco.Bairro);
          INIRec.WriteString(sSecao, 'CEP', fornec.Endereco.CEP);
          INIRec.WriteString(sSecao, 'xMunicipio', fornec.Endereco.xMunicipio);
          INIRec.WriteString(sSecao, 'UF', fornec.Endereco.UF);

          INIRec.WriteString(sSecao, 'Telefone', fornec.Contato.Telefone);
          INIRec.WriteString(sSecao, 'Email', fornec.Contato.Email);
        end;
      end;

      //Padrão Nacional
      if (Servico.Valores.tribMun.pAliq > 0) or (Servico.Valores.tribMun.pRedBCBM > 0) or
         (Servico.Valores.tribMun.vRedBCBM > 0) or (Servico.Valores.tribMun.cPaisResult > 0) then
      begin
        sSecao := 'tribMun';
        INIRec.WriteString(sSecao, 'tribISSQN', tribISSQNToStr(Servico.Valores.tribMun.tribISSQN));
        INIRec.WriteInteger(sSecao, 'cPaisResult', Servico.Valores.tribMun.cPaisResult);
        INIRec.WriteString(sSecao, 'tpBM', tpBMToStr(Servico.Valores.tribMun.tpBM));
        INIRec.WriteString(sSecao, 'nBM', Servico.Valores.TribMun.nBM);
        INIRec.WriteFloat(sSecao, 'vRedBCBM', Servico.Valores.tribMun.vRedBCBM);
        INIRec.WriteFloat(sSecao, 'pRedBCBM', Servico.Valores.tribMun.pRedBCBM);
        INIRec.WriteString(sSecao, 'tpSusp', tpSuspToStr(Servico.Valores.tribMun.tpSusp));
        INIRec.WriteString(sSecao, 'nProcesso', Servico.Valores.tribMun.nProcesso);
        INIRec.WriteString(sSecao, 'tpImunidade', tpImunidadeToStr(Servico.Valores.tribMun.tpImunidade));
        INIRec.WriteFloat(sSecao, 'pAliq', Servico.Valores.tribMun.pAliq);
        INIRec.WriteString(sSecao, 'tpRetISSQN', tpRetISSQNToStr(Servico.Valores.tribMun.tpRetISSQN));
      end;

      //Padrão Nacional
      if (Servico.Valores.tribFed.pAliqPis > 0) or (Servico.Valores.tribFed.pAliqCofins > 0) or
         (Servico.Valores.tribFed.vRetIRRF > 0) or (Servico.Valores.tribFed.vRetCP > 0) then
      begin
        sSecao := 'tribFederal';
        INIRec.WriteString(sSecao, 'CST', CSTToStr(Servico.Valores.tribFed.CST));
        INIRec.WriteFloat(sSecao, 'vBCPisCofins', Servico.Valores.tribFed.vBCPisCofins);
        INIRec.WriteFloat(sSecao, 'pAliqPis', Servico.Valores.tribFed.pAliqPis);
        INIRec.WriteFloat(sSecao, 'pAliqCofins', Servico.Valores.tribFed.pAliqCofins);
        INIRec.WriteFloat(sSecao, 'vPis', Servico.Valores.tribFed.vPis);
        INIRec.WriteFloat(sSecao, 'vCofins', Servico.Valores.tribFed.vCofins);
        INIRec.WriteString(sSecao, 'tpRetPisCofins', tpRetPisCofinsToStr(Servico.Valores.tribFed.tpRetPisCofins));
        INIRec.WriteFloat(sSecao, 'vRetCP', Servico.Valores.tribFed.vRetCP);
        INIRec.WriteFloat(sSecao, 'vRetIRRF', Servico.Valores.tribFed.vRetIRRF);
        INIRec.WriteFloat(sSecao, 'vRetCSLL', Servico.Valores.tribFed.vRetCSLL);
      end;

      sSecao := 'totTrib';
      INIRec.WriteString(sSecao, 'indTotTrib', indTotTribToStr(Servico.Valores.totTrib.indTotTrib));
      INIRec.WriteFloat(sSecao, 'pTotTribSN', Servico.Valores.totTrib.pTotTribSN);
      INIRec.WriteFloat(sSecao, 'vTotTribFed', Servico.Valores.totTrib.vTotTribFed);
      INIRec.WriteFloat(sSecao, 'vTotTribEst', Servico.Valores.totTrib.vTotTribEst);
      INIRec.WriteFloat(sSecao, 'vTotTribMun', Servico.Valores.totTrib.vTotTribMun);
      INIRec.WriteFloat(sSecao, 'pTotTribFed', Servico.Valores.totTrib.pTotTribFed);
      INIRec.WriteFloat(sSecao, 'pTotTribEst', Servico.Valores.totTrib.pTotTribEst);
      INIRec.WriteFloat(sSecao, 'pTotTribMun', Servico.Valores.totTrib.pTotTribMun);

      //Condição de Pagamento usado pelo provedor Betha versão 1 do Layout da ABRASF
      if CondicaoPagamento.QtdParcela > 0 then
      begin
        sSecao:= 'CondicaoPagamento';
        INIRec.WriteInteger(sSecao, 'QtdParcela', CondicaoPagamento.QtdParcela);
        INIRec.WriteString(sSecao, 'Condicao', FProvider.CondicaoPagToStr(CondicaoPagamento.Condicao));

        // Provedor NFEletronica
        if CondicaoPagamento.DataVencimento > 0 then
        begin
          INIRec.WriteDate(sSecao, 'DataVencimento', CondicaoPagamento.DataVencimento);
          INIRec.WriteString(sSecao, 'InstrucaoPagamento', CondicaoPagamento.InstrucaoPagamento);
          INIRec.WriteInteger(sSecao, 'CodigoVencimento', CondicaoPagamento.CodigoVencimento);
        end;

        //Lista de parcelas, xx pode variar de 01-99 (provedor Betha versão 1 do Layout da ABRASF)
        for I := 0 to CondicaoPagamento.Parcelas.Count - 1 do
        begin
          sSecao:= 'Parcelas' + IntToStrZero(I + 1, 2);

          INIRec.WriteString(sSecao, 'Parcela', CondicaoPagamento.Parcelas.Items[I].Parcela);
          INIRec.WriteDate(sSecao, 'DataVencimento', CondicaoPagamento.Parcelas.Items[I].DataVencimento);
          INIRec.WriteFloat(sSecao, 'Valor', CondicaoPagamento.Parcelas.Items[I].Valor);
        end;
      end;
    end;
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

function TNotaFiscal.LerXML(const AXML: string): Boolean;
var
  FProvider: IACBrNFSeXProvider;
  TipoXml: TtpXML;
  XmlTratado: string;
begin
  FProvider := TACBrNFSeX(FACBrNFSe).Provider;

  if not Assigned(FProvider) then
    raise EACBrNFSeException.Create(ERR_SEM_PROVEDOR);

  Result := FProvider.LerXML(AXML, FNFSe, TipoXml, XmlTratado);

  if TipoXml = txmlNFSe then
    FXmlNfse := XmlTratado
  else
    if TipoXml = txmlEspelho then
      FXmlEspelho := XmlTratado
    else
      FXmlRps := XmlTratado;
end;

procedure TNotaFiscal.SetXmlNfse(const Value: string);
begin
  LerXML(Value);
  FXmlNfse := Value;
end;

function TNotaFiscal.GravarXML(const NomeArquivo: string;
  const PathArquivo: string; aTipo: TtpXML): Boolean;
var
  ConteudoEhXml: Boolean;
begin
  if EstaVazio(FXmlRps) then
    GerarXML;

  {
    Tem provedor que é gerando um JSON em vez de XML e o método Gravar acaba
    incluindo na primeira linha do arquivo o encoding do XML.
    Para contornar isso a variável ConteudoEhXml recebe o valor false quando é
    um JSON e o método Gravar não inclui o encoding.
  }
  ConteudoEhXml := StringIsXML(FXmlRps);

  if aTipo = txmlNFSe then
  begin
    if EstaVazio(NomeArquivo) then
      FNomeArq := TACBrNFSeX(FACBrNFSe).GetNumID(NFSe) + '-nfse.xml'
    else
    begin
      FNomeArq := NomeArquivo;

      if ExtractFileExt(FNomeArq) = '' then
        FNomeArq := FNomeArq + '.xml';
    end;

    Result := TACBrNFSeX(FACBrNFSe).Gravar(FNomeArq, FXmlNfse, PathArquivo, ConteudoEhXml);
  end
  else
  begin
    FNomeArqRps := CalcularNomeArquivoCompleto(NomeArquivo, PathArquivo);
    Result := TACBrNFSeX(FACBrNFSe).Gravar(FNomeArqRps, FXmlRps, '', ConteudoEhXml);
  end;
end;

function TNotaFiscal.GravarStream(AStream: TStream): Boolean;
begin
  if EstaVazio(FXmlRps) then
    GerarXML;

  if EstaVazio(FXmlNfse) then
    FXmlNfse := FXmlRps;

  AStream.Size := 0;
  WriteStrToStream(AStream, AnsiString(FXmlNfse));
  Result := True;
end;

procedure TNotaFiscal.EnviarEmail(const sPara, sAssunto: string; sMensagem: TStrings;
  EnviaPDF: Boolean; sCC: TStrings; Anexos: TStrings; sReplyTo: TStrings;
  ManterPDFSalvo: Boolean);
var
  NomeArqTemp: string;
  AnexosEmail: TStrings;
  StreamNFSe: TMemoryStream;
begin
  if not Assigned(TACBrNFSeX(FACBrNFSe).MAIL) then
    raise EACBrNFSeException.Create('Componente ACBrMail não associado');

  AnexosEmail := TStringList.Create;
  StreamNFSe  := TMemoryStream.Create;
  try
    AnexosEmail.Clear;

    if Assigned(Anexos) then
      AnexosEmail.Assign(Anexos);

    with TACBrNFSeX(FACBrNFSe) do
    begin
      GravarStream(StreamNFSe);

      if (EnviaPDF) then
      begin
        if Assigned(DANFSE) then
        begin
          DANFSE.ImprimirDANFSEPDF(FNFSe);
          NomeArqTemp := DANFSE.ArquivoPDF;
          AnexosEmail.Add(NomeArqTemp);
        end;
      end;

      EnviarEmail( sPara, sAssunto, sMensagem, sCC, AnexosEmail, StreamNFSe,
                   NumID[FNFSe] +'-nfse.xml', sReplyTo);
    end;
  finally
    if not ManterPDFSalvo then
      DeleteFile(NomeArqTemp);

    AnexosEmail.Free;
    StreamNFSe.Free;
  end;
end;

function TNotaFiscal.GerarXML: string;
var
  FProvider: IACBrNFSeXProvider;
begin
  FProvider := TACBrNFSeX(FACBrNFSe).Provider;

  if not Assigned(FProvider) then
    raise EACBrNFSeException.Create(ERR_SEM_PROVEDOR);

  FProvider.GerarXml(NFSe, FXmlRps, FAlertas);
  Result := FXmlRps;
end;

function TNotaFiscal.GetXmlNfse: string;
begin
  Result := FXmlNfse;
  if Result = '' then
    Exit;

  if not XmlEhUTF8(Result) then
    Result := '<?xml version="1.0" encoding="UTF-8"?>' + Result;
end;

function TNotaFiscal.CalcularNomeArquivo: string;
var
  xID: string;
begin
  xID := TACBrNFSeX(FACBrNFSe).NumID[NFSe];

  if EstaVazio(xID) then
    raise EACBrNFSeException.Create('ID Inválido. Impossível Salvar XML');

  Result := xID + '-rps.xml';
end;

function TNotaFiscal.CalcularPathArquivo: string;
var
  Data: TDateTime;
begin
  with TACBrNFSeX(FACBrNFSe) do
  begin
    if Configuracoes.Arquivos.EmissaoPathNFSe then
      Data := FNFSe.DataEmissaoRps
    else
      Data := Now;

    Result := PathWithDelim(Configuracoes.Arquivos.GetPathRPS(Data,
      FNFSe.Prestador.IdentificacaoPrestador.CpfCnpj,
      FNFSe.Prestador.IdentificacaoPrestador.InscricaoEstadual));
  end;
end;

function TNotaFiscal.CalcularNomeArquivoCompleto(NomeArquivo: string;
  PathArquivo: string): string;
begin
  if EstaVazio(NomeArquivo) then
    NomeArquivo := CalcularNomeArquivo;

  if EstaVazio(PathArquivo) then
    PathArquivo := CalcularPathArquivo
  else
    PathArquivo := PathWithDelim(PathArquivo);

  Result := PathArquivo + NomeArquivo;
end;

{ TNotasFiscais }

constructor TNotasFiscais.Create(AOwner: TACBrDFe);
begin
  if not (AOwner is TACBrNFSeX) then
    raise EACBrNFSeException.Create('AOwner deve ser do tipo TACBrNFSeX');

  inherited Create();

  FACBrNFSe := TACBrNFSeX(AOwner);
  FConfiguracoes := TACBrNFSeX(FACBrNFSe).Configuracoes;
end;

function TNotasFiscais.New: TNotaFiscal;
begin
  Result := TNotaFiscal.Create(FACBrNFSe);
  Add(Result);
end;

function TNotasFiscais.Add(ANota: TNotaFiscal): Integer;
begin
  Result := inherited Add(ANota);
end;

function TNotasFiscais.GerarIni: string;
begin
  Result := '';

  if (Self.Count > 0) then
    Result := Self.Items[0].GerarNFSeIni;
end;

procedure TNotasFiscais.GerarNFSe;
var
  i: integer;
begin
  for i := 0 to Self.Count - 1 do
    Self.Items[i].GerarXML;
end;

function TNotasFiscais.GetItem(Index: integer): TNotaFiscal;
begin
  Result := TNotaFiscal(inherited Items[Index]);
end;

function TNotasFiscais.GetNamePath: string;
begin
  Result := 'NotaFiscal';
end;

procedure TNotasFiscais.VerificarDANFSE;
begin
  if not Assigned(TACBrNFSeX(FACBrNFSe).DANFSE) then
    raise EACBrNFSeException.Create('Componente DANFSE não associado.');
end;

procedure TNotasFiscais.Imprimir;
begin
  VerificarDANFSE;
  TACBrNFSeX(FACBrNFSe).DANFSE.ImprimirDANFSE(nil);
end;

procedure TNotasFiscais.ImprimirPDF;
begin
  VerificarDANFSE;

  TACBrNFSeX(FACBrNFSe).DANFSE.ImprimirDANFSEPDF;
end;

procedure TNotasFiscais.ImprimirPDF(AStream: TStream);
begin
  VerificarDANFSE;

  TACBrNFSeX(FACBrNFSe).DANFSE.ImprimirDANFSEPDF(AStream);
end;

procedure TNotasFiscais.Insert(Index: Integer; ANota: TNotaFiscal);
begin
  inherited Insert(Index, ANota);
end;

function TNotasFiscais.FindByRps(const ANumRPS: string): TNotaFiscal;
var
  AItem: TNotaFiscal;
  AItemIndex: Integer;
begin
  Result := nil;

  if Self.Count = 0 then Exit;

  if not Self.fIsSorted then
  begin
  {$IfDef HAS_SYSTEM_GENERICS}
    Sort(TComparer<TObject>.Construct(CompRpsPorNumero));
  {$Else}
    Sort(@CompRpsPorNumero);
  {$EndIf}
  end;

  AItem := TNotaFiscal.Create(FACBrNFSe);
  try
    AItem.NFSe.IdentificacaoRps.Numero := ANumRPS;
    {$IfDef HAS_SYSTEM_GENERICS}
     AItemIndex := FindObject(AItem, TComparer<TObject>.Construct(CompRpsPorNumero));
    {$Else}
     AItemIndex := FindObject(Pointer(AItem), @CompRpsPorNumero);
    {$EndIf}
  finally
    AItem.Free;
  end;

  if AItemIndex = -1 then Exit;

  Result := Self.Items[AItemIndex];
end;

function TNotasFiscais.FindByNFSe(const ANumNFSe: string): TNotaFiscal;
var
  AItem: TNotaFiscal;
  AItemIndex: Integer;
begin
  Result := nil;

  if Self.Count = 0 then Exit;

  if not Self.fIsSorted then
  begin
  {$IfDef HAS_SYSTEM_GENERICS}
    Sort(TComparer<TObject>.Construct(CompNFSePorNumero));
  {$Else}
    Sort(@CompNFSePorNumero);
  {$EndIf}
  end;

  AItem := TNotaFiscal.Create(FACBrNFSe);
  try
    AItem.NFSe.Numero := ANumNFSe;
    {$IfDef HAS_SYSTEM_GENERICS}
     AItemIndex := FindObject(AItem, TComparer<TObject>.Construct(CompNFSePorNumero));
    {$Else}
     AItemIndex := FindObject(Pointer(AItem), @CompNFSePorNumero);
    {$EndIf}
  finally
    AItem.Free;
  end;

  if AItemIndex = -1 then Exit;

  Result := Self.Items[AItemIndex];
end;

function TNotasFiscais.FindByCnpjCpfSerieRps(const CnpjCpf, Serie,
  ANumRPS: string): TNotaFiscal;
var
  AItem: TNotaFiscal;
  AItemIndex: Integer;
begin
  Result := nil;

  if Self.Count = 0 then Exit;

  if not Self.fIsSorted then
  begin
  {$IfDef HAS_SYSTEM_GENERICS}
    Sort(TComparer<TObject>.Construct(CompRpsPorCnpjCpfSerieNumero));
  {$Else}
    Sort(@CompRpsPorCnpjCpfSerieNumero);
  {$EndIf}
  end;

  AItem := TNotaFiscal.Create(FACBrNFSe);
  try
    AItem.NFSe.IdentificacaoRps.Numero := ANumRPS;
    AItem.NFSe.IdentificacaoRps.Serie := Serie;
    AItem.NFSe.Prestador.IdentificacaoPrestador.CpfCnpj := CnpjCpf;

    {$IfDef HAS_SYSTEM_GENERICS}
     AItemIndex := FindObject(AItem, TComparer<TObject>.Construct(CompRpsPorCnpjCpfSerieNumero));
    {$Else}
     AItemIndex := FindObject(Pointer(AItem), @CompRpsPorCnpjCpfSerieNumero);
    {$EndIf}
  finally
    AItem.Free;
  end;

  if AItemIndex = -1 then Exit;

  Result := Self.Items[AItemIndex];
end;

procedure TNotasFiscais.SetItem(Index: integer; const Value: TNotaFiscal);
begin
  inherited Items[Index] := Value;
end;

function TNotasFiscais.LoadFromFile(const CaminhoArquivo: string;
  AGerarNFSe: Boolean = True): Boolean;
var
  XmlUTF8: AnsiString;
  i, l: integer;
  MS: TMemoryStream;
begin
  MS := TMemoryStream.Create;
  try
    MS.LoadFromFile(CaminhoArquivo);

    XmlUTF8 := ReadStrFromStream(MS, MS.Size);
  finally
    MS.Free;
  end;

  l := Self.Count; // Indice da última nota já existente

  Result := LoadFromString(XmlUTF8, AGerarNFSe);

  if Result then
  begin
    // Atribui Nome do arquivo a novas notas inseridas //
    for i := l to Self.Count - 1 do
    begin
      if Pos('-rps.xml', CaminhoArquivo) > 0 then
        Self.Items[i].NomeArqRps := CaminhoArquivo
      else
        Self.Items[i].NomeArq := CaminhoArquivo;
    end;
  end;
end;

function TNotasFiscais.LoadFromIni(const AIniString: string): Boolean;
begin
  with Self.New do
    LerArqIni(AIniString);

  Result := Self.Count > 0;
end;

function TNotasFiscais.LoadFromLoteNfse(const CaminhoArquivo: string): Boolean;
var
  XMLStr: string;
  XMLUTF8: AnsiString;
  i, l: integer;
  MS: TMemoryStream;
  P, N, TamTag, j: Integer;
  aXml, aXmlLote: string;
  TagF: Array[1..15] of string;
  SL: TStringStream;
  IsFile: Boolean;

  function PrimeiraNFSe: Integer;
  begin
    TagF[01] := '<CompNfse>';
    TagF[02] := '<ComplNfse>';
    TagF[03] := '<NFS-e>';
    TagF[04] := '<Nfse>';
    TagF[05] := '<nfse>';             // Provedor IPM
    TagF[06] := '<Nota>';
    TagF[07] := '<NFe>';
    TagF[08] := '<tbnfd>';
    TagF[09] := '<nfs>';
    TagF[10] := '<nfeRpsNotaFiscal>'; // Provedor EL
    TagF[11] := '<notasFiscais>';     // Provedor EL
    TagF[12] := '<notaFiscal>';       // Provedor GIAP
    TagF[13] := '<NOTA>';             // Provedor AssessorPublico
    TagF[14] := '<NOTA_FISCAL>';      // Provedor ISSDSF
    TagF[15] := '<tcCompNfse>';       // Provedor ISSCuritiba

    j := 0;

    repeat
      inc(j);
      TamTAG := Length(TagF[j]) -1;
      Result := Pos(TagF[j], aXmlLote);
    until (j = High(TagF)) or (Result <> 0);
  end;

  function PosNFSe: Integer;
  begin
    TagF[01] := '</CompNfse>';
    TagF[02] := '</ComplNfse>';
    TagF[03] := '</NFS-e>';
    TagF[04] := '</Nfse>';
    TagF[05] := '</nfse>';             // Provedor IPM
    TagF[06] := '</Nota>';
    TagF[07] := '</NFe>';
    TagF[08] := '</tbnfd>';
    TagF[09] := '</nfs>';
    TagF[10] := '</nfeRpsNotaFiscal>'; // Provedor EL
    TagF[11] := '</notasFiscais>';     // Provedor EL
    TagF[12] := '</notaFiscal>';       // Provedor GIAP
    TagF[13] := '</NOTA>';             // Provedor AssessorPublico
    TagF[14] := '</NOTA_FISCAL>';      // Provedor ISSDSF
    TagF[15] := '</tcCompNfse>';       // Provedor ISSCuritiba

    j := 0;

    repeat
      inc(j);
      TamTAG := Length(TagF[j]) -1;
      Result := Pos(TagF[j], aXmlLote);
    until (j = High(TagF)) or (Result <> 0);
  end;

  function LimparXml(const XMLStr: string): string;
  begin
    Result := FaststringReplace(XMLStr, ' xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"', '', [rfReplaceAll]);
    Result := FaststringReplace(Result, ' xmlns:xsd="http://www.w3.org/2001/XMLSchema"', '', [rfReplaceAll]);
  end;
begin
  MS := TMemoryStream.Create;
  try
    IsFile := FilesExists(CaminhoArquivo);

    if (IsFile) then
      MS.LoadFromFile(CaminhoArquivo)
    else
    begin
      SL := TStringStream.Create(CaminhoArquivo);
      MS.LoadFromStream(SL);
      SL.Free;
    end;

    XMLUTF8 := ReadStrFromStream(MS, MS.Size);
  finally
    MS.Free;
  end;

  l := Self.Count; // Indice da última nota já existente

  XMLStr := LimparXml(XMLUTF8);

  aXmlLote := XMLStr;
  Result := False;
  P := PrimeiraNFSe;
  aXmlLote := copy(aXmlLote, P, length(aXmlLote));
  N := PosNFSe;

  while N > 0 do
  begin
    aXml := copy(aXmlLote, 1, N + TamTAG);
    aXmlLote := Trim(copy(aXmlLote, N + TamTAG + 1, length(aXmlLote)));

    Result := LoadFromString(aXml, False);

    N := PosNFSe;
  end;

  if Result then
  begin
    // Atribui Nome do arquivo a novas notas inseridas //
    for i := l to Self.Count - 1 do
    begin
      if Pos('-rps.xml', CaminhoArquivo) > 0 then
        Self.Items[i].NomeArqRps := CaminhoArquivo
      else
      begin
        if IsFile then
          Self.Items[i].NomeArq := CaminhoArquivo
        else
          Self.Items[i].NomeArq := TACBrNFSeX(FACBrNFSe).GetNumID(Items[i].NFSe) + '-nfse.xml';
      end;
    end;
  end;
end;

function TNotasFiscais.LoadFromStream(AStream: TStringStream;
  AGerarNFSe: Boolean = True): Boolean;
var
  AXML: AnsiString;
begin
  AStream.Position := 0;
  AXML := ReadStrFromStream(AStream, AStream.Size);

  Result := Self.LoadFromString(String(AXML), AGerarNFSe);
end;

function TNotasFiscais.LoadFromString(const AXMLString: string;
  AGerarNFSe: Boolean = True): Boolean;
begin
  with Self.New do
  begin
    LerXML(AXMLString);

    if AGerarNFSe then
      GerarXML;
  end;

  Result := Self.Count > 0;
end;

function TNotasFiscais.GravarXML(const PathNomeArquivo: string): Boolean;
var
  i: integer;
  NomeArq, PathArq : string;
begin
  Result := True;
  i := 0;

  while Result and (i < Self.Count) do
  begin
    PathArq := ExtractFilePath(PathNomeArquivo);
    NomeArq := ExtractFileName(PathNomeArquivo);
    Result := Self.Items[i].GravarXML(NomeArq, PathArq);
    Inc(i);
  end;
end;

end.
