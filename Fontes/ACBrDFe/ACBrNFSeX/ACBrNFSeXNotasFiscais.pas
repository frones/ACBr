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
  ACBrBase, ACBrDFe, ACBrNFSeXConfiguracoes, ACBrNFSeXClass, ACBrNFSeXConversao;

type

  { TNotaFiscal }

  TNotaFiscal = class(TCollectionItem)
  private
    FNFSe: TNFSe;
    FACBrNFSe: TACBrDFe;

    FAlertas: String;
    FNomeArq: String;
    FNomeArqRps: String;
    FConfirmada: Boolean;
    FXmlRps: String;
    FXmlNfse: String;
    FXmlEspelho: String;

    function CalcularNomeArquivo: String;
    function CalcularPathArquivo: String;
    procedure SetXmlNfse(const Value: String);
    function GetXmlNfse: String;
  public
    constructor Create(AOwner: TACBrDFe);
    destructor Destroy; override;

    procedure Imprimir;
    procedure ImprimirPDF; overload;
    procedure ImprimirPDF(AStream: TStream); overload;

    function LerXML(const AXML: String): Boolean;
    function LerArqIni(const AIniString: String): Boolean;
    function GerarNFSeIni: String;

    function GerarXML: String;
    function GravarXML(const NomeArquivo: String = '';
      const PathArquivo: String = ''; aTipo: TtpXML = txmlNFSe): Boolean;

    function GravarStream(AStream: TStream): Boolean;

    procedure EnviarEmail(const sPara, sAssunto: String; sMensagem: TStrings = nil;
      EnviaPDF: Boolean = True; sCC: TStrings = nil; Anexos: TStrings = nil;
      sReplyTo: TStrings = nil; ManterPDFSalvo: Boolean = True);

    property NomeArq: String    read FNomeArq    write FNomeArq;
    property NomeArqRps: String read FNomeArqRps write FNomeArqRps;

    function CalcularNomeArquivoCompleto(NomeArquivo: String = '';
      PathArquivo: String = ''): String;

    property NFSe: TNFSe read FNFSe;

    property XmlRps: String read FXmlRps write FXmlRps;
    property XmlNfse: String read GetXmlNfse write SetXmlNfse;
    property XmlEspelho: String read FXmlEspelho write FXmlEspelho;

    property Confirmada: Boolean read FConfirmada write FConfirmada;
    property Alertas: String     read FAlertas;

  end;

  { TNotasFiscais }

  TNotasFiscais = class(TACBrObjectList)
  private
    FTransacao: Boolean;
    FNumeroLote: String;
    FACBrNFSe: TACBrDFe;
    FConfiguracoes: TConfiguracoesNFSe;
    FXMLLoteOriginal: String;
    FXMLLoteAssinado: String;
    FAlertas: String;

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
    function FindByRps(ANumRPS: string): TNotaFiscal;
    function FindByNFSe(ANumNFSe: string): TNotaFiscal;
    function FindByCnpjCpfSerieRps(CnpjCpf, Serie, ANumRPS: string): TNotaFiscal;

    property Items[Index: integer]: TNotaFiscal read GetItem write SetItem; default;

    function GetNamePath: String;

    // Incluido o Parametro AGerarNFSe que determina se após carregar os dados da NFSe
    // para o componente, será gerado ou não novamente o XML da NFSe.
    function LoadFromFile(const CaminhoArquivo: String; AGerarNFSe: Boolean = True): Boolean;
    function LoadFromStream(AStream: TStringStream; AGerarNFSe: Boolean = True): Boolean;
    function LoadFromString(AXMLString: String; AGerarNFSe: Boolean = True): Boolean;
    function LoadFromIni(const AIniString: String): Boolean;
    function LoadFromLoteNfse(const CaminhoArquivo: String): Boolean;

    function GerarIni: String;
    function GravarXML(const PathNomeArquivo: String = ''): Boolean;

    property XMLLoteOriginal: String read FXMLLoteOriginal write FXMLLoteOriginal;
    property XMLLoteAssinado: String read FXMLLoteAssinado write FXMLLoteAssinado;
    property NumeroLote: String      read FNumeroLote      write FNumeroLote;
    property Transacao: Boolean      read FTransacao       write FTransacao;
    property Alertas: String         read FAlertas;

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
  synautil, IniFiles, StrUtilsEx,
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
  NumRps1, NumRps2: String;
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

function TNotaFiscal.LerArqIni(const AIniString: String): Boolean;
var
  INIRec: TMemIniFile;
  sSecao, sFim: String;
  Ok: Boolean;
  i: Integer;
  FProvider: IACBrNFSeXProvider;
begin
  FProvider := TACBrNFSeX(FACBrNFSe).Provider;

  if not Assigned(FProvider) then
    raise EACBrNFSeException.Create(ERR_SEM_PROVEDOR);

  INIRec := TMemIniFile.Create('');

  try
    LerIniArquivoOuString(AIniString, INIRec);

    with FNFSe do
    begin
      // Provedor Infisc - Layout Proprio
      sSecao := 'IdentificacaoNFSe';
      if INIRec.SectionExists(sSecao) then
      begin
        Numero := INIRec.ReadString(sSecao, 'Numero', '');
        cNFSe := GerarCodigoDFe(StrToIntDef(Numero, 0));
      end;

      sSecao := 'IdentificacaoRps';
      if INIRec.SectionExists(sSecao) then
      begin
        SituacaoTrib := FProvider.StrToSituacaoTrib(Ok, INIRec.ReadString(sSecao, 'SituacaoTrib', 'tp'));

        //Provedores CTA, ISSBarueri, ISSSDSF, ISSSaoPaulo, Simple e SmarAPD.
        if INIRec.ReadString(sSecao, 'TipoTributacaoRPS', 'FIM') <> 'FIM' then
          TipoTributacaoRPS := FProvider.StrToTipoTributacaoRPS(Ok, INIRec.ReadString(sSecao, 'TipoTributacaoRPS', ''));

        // Provedor AssessorPublico
        Situacao := INIRec.ReadInteger(sSecao, 'Situacao', 0);

        Producao := FProvider.StrToSimNao(Ok, INIRec.ReadString(sSecao, 'Producao', '1'));
        StatusRps := FProvider.StrToStatusRPS(Ok, INIRec.ReadString(sSecao, 'Status', '1'));
        OutrasInformacoes := INIRec.ReadString(sSecao, 'OutrasInformacoes', '');

        // Provedor ISSDSF e Siat
        SeriePrestacao := INIRec.ReadString(sSecao, 'SeriePrestacao', '');

        IdentificacaoRps.Numero := INIRec.ReadString(sSecao, 'Numero', '0');
        IdentificacaoRps.Serie := INIRec.ReadString(sSecao, 'Serie', '0');
        IdentificacaoRps.Tipo := FProvider.StrToTipoRPS(Ok, INIRec.ReadString(sSecao, 'Tipo', '1'));

        DataEmissao := INIRec.ReadDate(sSecao, 'DataEmissao', Now);
        Competencia := INIRec.ReadDate(sSecao, 'Competencia', Now);
        DataEmissaoRPS := INIRec.ReadDate(sSecao, 'DataEmissao', Now);
        Vencimento := INIRec.ReadDate(sSecao, 'Vencimento', Now);
        NaturezaOperacao := StrToNaturezaOperacao(Ok, INIRec.ReadString(sSecao, 'NaturezaOperacao', '0'));

        // Provedor Tecnos
        PercentualCargaTributaria := StringToFloatDef(INIRec.ReadString(sSecao, 'PercentualCargaTributaria', ''), 0);
        ValorCargaTributaria := StringToFloatDef(INIRec.ReadString(sSecao, 'ValorCargaTributaria', ''), 0);
        PercentualCargaTributariaMunicipal := StringToFloatDef(INIRec.ReadString(sSecao, 'PercentualCargaTributariaMunicipal', ''), 0);
        ValorCargaTributariaMunicipal := StringToFloatDef(INIRec.ReadString(sSecao, 'ValorCargaTributariaMunicipal', ''), 0);
        PercentualCargaTributariaEstadual := StringToFloatDef(INIRec.ReadString(sSecao, 'PercentualCargaTributariaEstadual', ''), 0);
        ValorCargaTributariaEstadual := StringToFloatDef(INIRec.ReadString(sSecao, 'ValorCargaTributariaEstadual', ''), 0);

        // Provedor PadraoNacional
        verAplic := INIRec.ReadString(sSecao, 'verAplic', 'ACBrNFSeX-1.00');
        tpEmit := StrTotpEmit(Ok, INIRec.ReadString(sSecao, 'tpEmit', '1'));

        //Provedor Governa
        RegRec := StrToRegRec(Ok, INIRec.ReadString(sSecao, 'RegRec', ''));

        InformacoesComplementares := INIRec.ReadString(sSecao, 'InformacoesComplementares', '');
      end;

      sSecao := 'RpsSubstituido';
      if INIRec.SectionExists(sSecao) then
      begin
        RpsSubstituido.Numero := INIRec.ReadString(sSecao, 'Numero', '');
        RpsSubstituido.Serie := INIRec.ReadString(sSecao, 'Serie', '');
        RpsSubstituido.Tipo := FProvider.StrToTipoRPS(Ok, INIRec.ReadString(sSecao, 'Tipo', '1'));
      end;

      sSecao := 'Prestador';
      if INIRec.SectionExists(sSecao) then
      begin
        RegimeEspecialTributacao := FProvider.StrToRegimeEspecialTributacao(Ok, INIRec.ReadString(sSecao, 'Regime', '0'));
        OptanteSimplesNacional := FProvider.StrToSimNao(Ok, INIRec.ReadString(sSecao, 'OptanteSN', '1'));
        OptanteSN := StrToOptanteSN(Ok, INIRec.ReadString(sSecao, 'opSimpNac', '2'));

        IncentivadorCultural := FProvider.StrToSimNao(Ok, INIRec.ReadString(sSecao, 'IncentivadorCultural', '1'));

        if INIRec.ReadString(sSecao, 'RegimeApuracaoSN', '') <> '' then
          RegimeApuracaoSN := StrToRegimeApuracaoSN(Ok, INIRec.ReadString(sSecao, 'RegimeApuracaoSN', '1'));

        with Prestador do
        begin
          IdentificacaoPrestador.CpfCnpj := INIRec.ReadString(sSecao, 'CNPJ', '');
          IdentificacaoPrestador.InscricaoMunicipal := INIRec.ReadString(sSecao, 'InscricaoMunicipal', '');

          IdentificacaoPrestador.Nif := INIRec.ReadString(sSecao, 'NIF', '');
          IdentificacaoPrestador.cNaoNIF := StrToNaoNIF(Ok, INIRec.ReadString(sSecao, 'cNaoNIF', '0'));
          IdentificacaoPrestador.CAEPF := INIRec.ReadString(sSecao, 'CAEPF', '');

          // Para o provedor ISSDigital deve-se informar também:
          cUF := UFparaCodigoUF(INIRec.ReadString(sSecao, 'UF', 'SP'));

          RazaoSocial := INIRec.ReadString(sSecao, 'RazaoSocial', '');
          NomeFantasia := INIRec.ReadString(sSecao, 'NomeFantasia', '');

          with Endereco do
          begin
            Endereco := INIRec.ReadString(sSecao, 'Logradouro', '');
            Numero := INIRec.ReadString(sSecao, 'Numero', '');
            Complemento := INIRec.ReadString(sSecao, 'Complemento', '');
            Bairro := INIRec.ReadString(sSecao, 'Bairro', '');
            CodigoMunicipio := INIRec.ReadString(sSecao, 'CodigoMunicipio', '');
            xMunicipio := INIRec.ReadString(sSecao, 'xMunicipio', '');
            UF := INIRec.ReadString(sSecao, 'UF', '');
            CodigoPais := INIRec.ReadInteger(sSecao, 'CodigoPais', 0);
            xPais := INIRec.ReadString(sSecao, 'xPais', '');
            CEP := INIRec.ReadString(sSecao, 'CEP', '');
          end;

          with Contato do
          begin
            Telefone := INIRec.ReadString(sSecao, 'Telefone', '');
            Email := INIRec.ReadString(sSecao, 'Email', '');
          end;
        end;
      end;

      sSecao := 'Tomador';
      if INIRec.SectionExists(sSecao) then
      begin
        with Tomador do
        begin
          with IdentificacaoTomador do
          begin
            Tipo := FProvider.StrToTipoPessoa(Ok, INIRec.ReadString(sSecao, 'Tipo', '1'));
            CpfCnpj := INIRec.ReadString(sSecao, 'CNPJCPF', '');
            InscricaoMunicipal := INIRec.ReadString(sSecao, 'InscricaoMunicipal', '');
            InscricaoEstadual := INIRec.ReadString(sSecao, 'InscricaoEstadual', '');

            Nif := INIRec.ReadString(sSecao, 'NIF', '');
            cNaoNIF := StrToNaoNIF(Ok, INIRec.ReadString(sSecao, 'cNaoNIF', '0'));
            CAEPF := INIRec.ReadString(sSecao, 'CAEPF', '');
          end;

          RazaoSocial := INIRec.ReadString(sSecao, 'RazaoSocial', '');

          with Endereco do
          begin
            TipoLogradouro := INIRec.ReadString(sSecao, 'TipoLogradouro', '');
            Endereco := INIRec.ReadString(sSecao, 'Logradouro', '');
            Numero := INIRec.ReadString(sSecao, 'Numero', '');
            Complemento := INIRec.ReadString(sSecao, 'Complemento', '');
            Bairro := INIRec.ReadString(sSecao, 'Bairro', '');
            CodigoMunicipio := INIRec.ReadString(sSecao, 'CodigoMunicipio', '');
            xMunicipio := INIRec.ReadString(sSecao, 'xMunicipio', '');
            UF := INIRec.ReadString(sSecao, 'UF', '');
            CodigoPais := INIRec.ReadInteger(sSecao, 'CodigoPais', 0);
            CEP := INIRec.ReadString(sSecao, 'CEP', '');
            // Provedor Equiplano é obrigatório o pais e IE
            xPais := INIRec.ReadString(sSecao, 'xPais', '');
          end;

          with Contato do
          begin
            Telefone := INIRec.ReadString(sSecao, 'Telefone', '');
            Email := INIRec.ReadString(sSecao, 'Email', '');
          end;

          AtualizaTomador := FProvider.StrToSimNao(Ok, INIRec.ReadString(sSecao, 'AtualizaTomador', '1'));
          TomadorExterior := FProvider.StrToSimNao(Ok, INIRec.ReadString(sSecao, 'TomadorExterior', '1'));
        end;
      end;

      sSecao := 'Intermediario';
      if INIRec.SectionExists(sSecao)then
      begin
        with Intermediario do
        begin
          with Identificacao do
          begin
            CpfCnpj := INIRec.ReadString(sSecao, 'CNPJCPF', '');
            InscricaoMunicipal := INIRec.ReadString(sSecao, 'InscricaoMunicipal', '');
            Nif := INIRec.ReadString(sSecao, 'NIF', '');
            cNaoNIF := StrToNaoNIF(Ok, INIRec.ReadString(sSecao, 'cNaoNIF', '0'));
            CAEPF := INIRec.ReadString(sSecao, 'CAEPF', '');
          end;

          RazaoSocial := INIRec.ReadString(sSecao, 'RazaoSocial', '');

          with Endereco do
          begin
            CodigoMunicipio := INIRec.ReadString(sSecao, 'CodigoMunicipio', '');
            CEP := INIRec.ReadString(sSecao, 'CEP', '');
            CodigoPais := INIRec.ReadInteger(sSecao, 'CodigoPais', 0);
            xMunicipio := INIRec.ReadString(sSecao, 'xMunicipio', '');
            UF := INIRec.ReadString(sSecao, 'UF', '');
            Endereco := INIRec.ReadString(sSecao, 'Logradouro', '');
            Numero := INIRec.ReadString(sSecao, 'Numero', '');
            Complemento := INIRec.ReadString(sSecao, 'Complemento', '');
            Bairro := INIRec.ReadString(sSecao, 'Bairro', '');
          end;

          with Contato do
          begin
            Telefone := INIRec.ReadString(sSecao, 'Telefone', '');
            Email := INIRec.ReadString(sSecao, 'Email', '');
          end;
        end;
      end;

      sSecao := 'ConstrucaoCivil';
      if INIRec.SectionExists(sSecao) then
      begin
        with ConstrucaoCivil do
        begin
          CodigoObra := INIRec.ReadString(sSecao, 'CodigoObra', '');
          Art := INIRec.ReadString(sSecao, 'Art', '');
          inscImobFisc := INIRec.ReadString(sSecao, 'inscImobFisc', '');

          with Endereco do
          begin
            CEP := INIRec.ReadString(sSecao, 'CEP', '');
            xMunicipio := INIRec.ReadString(sSecao, 'xMunicipio', '');
            UF := INIRec.ReadString(sSecao, 'UF', '');
            Endereco := INIRec.ReadString(sSecao, 'Logradouro', '');
            Numero := INIRec.ReadString(sSecao, 'Numero', '');
            Complemento := INIRec.ReadString(sSecao, 'Complemento', '');
            Bairro := INIRec.ReadString(sSecao, 'Bairro', '');

          end;
        end;
      end;

      with Servico do
      begin
        sSecao := 'Servico';
        if INIRec.SectionExists(sSecao) then
        begin
          ItemListaServico := INIRec.ReadString(sSecao, 'ItemListaServico', '');
          CodigoCnae := INIRec.ReadString(sSecao, 'CodigoCnae', '');
          CodigoTributacaoMunicipio := INIRec.ReadString(sSecao, 'CodigoTributacaoMunicipio', '');
          Discriminacao := INIRec.ReadString(sSecao, 'Discriminacao', '');
          CodigoMunicipio := INIRec.ReadString(sSecao, 'CodigoMunicipio', '');
          CodigoPais := INIRec.ReadInteger(sSecao, 'CodigoPais', 0);
          ExigibilidadeISS := FProvider.StrToExigibilidadeISS(Ok, INIRec.ReadString(sSecao, 'ExigibilidadeISS', '1'));
          MunicipioIncidencia := INIRec.ReadInteger(sSecao, 'MunicipioIncidencia', 0);
          UFPrestacao := INIRec.ReadString(sSecao, 'UFPrestacao', '');
          ResponsavelRetencao := FProvider.StrToResponsavelRetencao(Ok, INIRec.ReadString(sSecao, 'ResponsavelRetencao', ''));
          TipoLancamento := StrToTipoLancamento(Ok, INIRec.ReadString(sSecao, 'TipoLancamento', 'P'));

          // Provedor ISSDSF
          Operacao := StrToOperacao(Ok, INIRec.ReadString(sSecao, 'Operacao', ''));
          Tributacao := FProvider.StrToTributacao(Ok, INIRec.ReadString(sSecao, 'Tributacao', ''));
          // Provedor ISSSaoPaulo
          ValorTotalRecebido := StringToFloatDef(INIRec.ReadString(sSecao, 'ValorTotalRecebido', ''), 0);

          // Provedor IssNet e Padrão Nacional
          CodigoNBS := INIRec.ReadString(sSecao, 'CodigoNBS', '');
          CodigoInterContr := INIRec.ReadString(sSecao, 'CodigoInterContr', '');

          // Provedor SoftPlan
          CFPS := INIRec.ReadString(sSecao, 'CFPS', '');

          // Provedor Giap Informações sobre o Endereço da Prestação de Serviço
          with Endereco do
          begin
            Bairro := INIRec.ReadString(sSecao, 'Bairro', '');
            CEP := INIRec.ReadString(sSecao, 'CEP', '');
            xMunicipio := INIRec.ReadString(sSecao, 'xMunicipio', '');
            Complemento := INIRec.ReadString(sSecao, 'Complemento', '');
            Endereco := INIRec.ReadString(sSecao, 'Logradouro', '');
            Numero := INIRec.ReadString(sSecao, 'Numero', '');
            xPais := INIRec.ReadString(sSecao, 'xPais', '');
            UF := INIRec.ReadString(sSecao, 'UF', '');
          end;
        end;

        i := 1;
        while true do
        begin
          sSecao := 'Itens' + IntToStrZero(i, 3);
          sFim := INIRec.ReadString(sSecao, 'Descricao'  ,'FIM');

          if (sFim = 'FIM') then
            break;

          with ItemServico.New do
          begin
            Descricao := sFim;
            ItemListaServico := INIRec.ReadString(sSecao, 'ItemListaServico', '');
            CodServ := INIRec.ReadString(sSecao, 'CodServico', '');
            codLCServ := INIRec.ReadString(sSecao, 'codLCServico', '');
            CodigoCnae := INIRec.ReadString(sSecao, 'CodigoCnae', '');

            TipoUnidade := StrToUnidade(Ok, INIRec.ReadString(sSecao, 'TipoUnidade', '2'));
            Unidade := INIRec.ReadString(sSecao, 'Unidade', '');
            Quantidade := StringToFloatDef(INIRec.ReadString(sSecao, 'Quantidade', ''), 0);
            ValorUnitario := StringToFloatDef(INIRec.ReadString(sSecao, 'ValorUnitario', ''), 0);

            QtdeDiaria := StringToFloatDef(INIRec.ReadString(sSecao, 'QtdeDiaria', ''), 0);
            ValorTaxaTurismo := StringToFloatDef(INIRec.ReadString(sSecao, 'ValorTaxaTurismo', ''), 0);

            ValorDeducoes := StringToFloatDef(INIRec.ReadString(sSecao, 'ValorDeducoes', ''), 0);
            xJustDeducao := INIRec.ReadString(sSecao, 'xJustDeducao', '');

            AliqReducao := StringToFloatDef(INIRec.ReadString(sSecao, 'AliqReducao', ''), 0);
            ValorReducao := StringToFloatDef(INIRec.ReadString(sSecao, 'ValorReducao', ''), 0);

            ValorISS := StringToFloatDef(INIRec.ReadString(sSecao, 'ValorISS', ''), 0);
            Aliquota := StringToFloatDef(INIRec.ReadString(sSecao, 'Aliquota', ''), 0);
            BaseCalculo := StringToFloatDef(INIRec.ReadString(sSecao, 'BaseCalculo', ''), 0);
            DescontoIncondicionado := StringToFloatDef(INIRec.ReadString(sSecao, 'DescontoIncondicionado', ''), 0);
            DescontoCondicionado := StringToFloatDef(INIRec.ReadString(sSecao, 'DescontoCondicionado', ''), 0);

            AliqISSST := StringToFloatDef(INIRec.ReadString(sSecao, 'AliqISSST', ''), 0);
            ValorISSST := StringToFloatDef(INIRec.ReadString(sSecao, 'ValorISSST', ''), 0);

            ValorBCCSLL := StringToFloatDef(INIRec.ReadString(sSecao, 'ValorBCCSLL', ''), 0);
            AliqRetCSLL := StringToFloatDef(INIRec.ReadString(sSecao, 'AliqRetCSLL', ''), 0);
            ValorCSLL := StringToFloatDef(INIRec.ReadString(sSecao, 'ValorCSLL', ''), 0);

            ValorBCPIS := StringToFloatDef(INIRec.ReadString(sSecao, 'ValorBCPIS', ''), 0);
            AliqRetPIS := StringToFloatDef(INIRec.ReadString(sSecao, 'AliqRetPIS', ''), 0);
            ValorPIS := StringToFloatDef(INIRec.ReadString(sSecao, 'ValorPIS', ''), 0);

            ValorBCCOFINS := StringToFloatDef(INIRec.ReadString(sSecao, 'ValorBCCOFINS', ''), 0);
            AliqRetCOFINS := StringToFloatDef(INIRec.ReadString(sSecao, 'AliqRetCOFINS', ''), 0);
            ValorCOFINS := StringToFloatDef(INIRec.ReadString(sSecao, 'ValorCOFINS', ''), 0);

            ValorBCINSS := StringToFloatDef(INIRec.ReadString(sSecao, 'ValorBCINSS', ''), 0);
            AliqRetINSS := StringToFloatDef(INIRec.ReadString(sSecao, 'AliqRetINSS', ''), 0);
            ValorINSS := StringToFloatDef(INIRec.ReadString(sSecao, 'ValorINSS', ''), 0);

            ValorBCRetIRRF := StringToFloatDef(INIRec.ReadString(sSecao, 'ValorBCRetIRRF', ''), 0);
            AliqRetIRRF := StringToFloatDef(INIRec.ReadString(sSecao, 'AliqRetIRRF', ''), 0);
            ValorIRRF := StringToFloatDef(INIRec.ReadString(sSecao, 'ValorIRRF', ''), 0);

            ValorTotal := StringToFloatDef(INIRec.ReadString(sSecao, 'ValorTotal', ''), 0);

            Tributavel := FProvider.StrToSimNao(Ok, INIRec.ReadString(sSecao, 'Tributavel', '1'));

            // IPM
            TribMunPrestador := FProvider.StrToSimNao(Ok, INIRec.ReadString(sSecao, 'TribMunPrestador', '1'));
            CodMunPrestacao := INIRec.ReadString(sSecao, 'CodMunPrestacao', '');
            SituacaoTributaria := INIRec.ReadInteger(sSecao, 'SituacaoTributaria', 0);
            ValorISSRetido := StringToFloatDef(INIRec.ReadString(sSecao, 'ValorISSRetido', ''), 0);
          end;

          Inc(i);
        end;

        //Padrão Nacional
        sSecao := 'ComercioExterior';
        if INIRec.SectionExists(sSecao) then
        begin
          with comExt do
          begin
            mdPrestacao := StrTomdPrestacao(Ok, INIRec.ReadString(sSecao, 'mdPrestacao', '0'));
            vincPrest := StrTovincPrest(Ok, INIRec.ReadString(sSecao, 'vincPrest', '0'));
            tpMoeda := INIRec.ReadInteger(sSecao, 'tpMoeda', 0);
            vServMoeda := StringToFloatDef(INIRec.ReadString(sSecao, 'vServMoeda', '0'), 0);
            mecAFComexP := StrTomecAFComexP(Ok, INIRec.ReadString(sSecao, 'mecAFComexP', '00'));
            mecAFComexT := StrTomecAFComexT(Ok, INIRec.ReadString(sSecao, 'mecAFComexT', '00'));
            movTempBens := StrToMovTempBens(Ok, INIRec.ReadString(sSecao, 'movTempBens', '00'));
            nDI := INIRec.ReadString(sSecao, 'nDI', '');
            nRE := INIRec.ReadString(sSecao, 'nRE', '');
            mdic := INIRec.ReadInteger(sSecao, 'mdic', 0);
          end;
        end;

        //Padrão Nacional
        sSecao := 'LocacaoSubLocacao';
        if INIRec.SectionExists(sSecao) then
        begin
          with Locacao do
          begin
            categ := StrTocateg(Ok, INIRec.ReadString(sSecao, 'categ', '1'));
            objeto := StrToobjeto(Ok, INIRec.ReadString(sSecao, 'objeto', '1'));
            extensao := INIRec.ReadString(sSecao, 'extensao', '');
            nPostes := INIRec.ReadInteger(sSecao, 'nPostes', 0);
          end;
        end;

        //Padrão Nacional
        sSecao := 'Evento';
        if INIRec.SectionExists(sSecao) then
        begin
          with Evento do
          begin
            xNome := INIRec.ReadString(sSecao, 'xNome', '');
            dtIni := INIRec.ReadDate(sSecao, 'dtIni', Now);
            dtFim := INIRec.ReadDate(sSecao, 'dtFim', Now);
            idAtvEvt := INIRec.ReadString(sSecao, 'idAtvEvt', '');

            with Endereco do
            begin
              CEP := INIRec.ReadString(sSecao, 'CEP', '');
              xMunicipio := INIRec.ReadString(sSecao, 'xMunicipio', '');
              UF := INIRec.ReadString(sSecao, 'UF', '');
              Endereco := INIRec.ReadString(sSecao, 'Logradouro', '');
              Numero := INIRec.ReadString(sSecao, 'Numero', '');
              Complemento := INIRec.ReadString(sSecao, 'Complemento', '');
              Bairro := INIRec.ReadString(sSecao, 'Bairro', '');
            end;
          end;
        end;

        //Padrão Nacional
        sSecao := 'Rodoviaria';
        if INIRec.SectionExists(sSecao) then
        begin
          with explRod do
          begin
            categVeic := StrTocategVeic(Ok, INIRec.ReadString(sSecao, 'categVeic', '00'));
            nEixos := INIRec.ReadInteger(sSecao, 'nEixos', 0);
            rodagem := StrTorodagem(Ok, INIRec.ReadString(sSecao, 'rodagem', '1'));
            sentido := INIRec.ReadString(sSecao, 'sentido', '');
            placa := INIRec.ReadString(sSecao, 'placa', '');
            codAcessoPed := INIRec.ReadString(sSecao, 'codAcessoPed', '');
            codContrato := INIRec.ReadString(sSecao, 'codContrato', '');
          end;
        end;

        //Padrão Nacional
        sSecao := 'InformacoesComplementares';
        if INIRec.SectionExists(sSecao) then
        begin
          with infoCompl do
          begin
            idDocTec := INIRec.ReadString(sSecao, 'idDocTec', '');
            docRef := INIRec.ReadString(sSecao, 'docRef', '');
            xInfComp := INIRec.ReadString(sSecao, 'xInfComp', '');
          end;
        end;

        with Valores do
        begin
          sSecao := 'Valores';
          if INIRec.SectionExists(sSecao) then
          begin
            ValorServicos := StringToFloatDef(INIRec.ReadString(sSecao, 'ValorServicos', ''), 0);
            ValorDeducoes := StringToFloatDef(INIRec.ReadString(sSecao, 'ValorDeducoes', ''), 0);
            AliquotaDeducoes := StringToFloatDef(INIRec.ReadString(sSecao, 'AliquotaDeducoes', ''), 0);

            ValorPis := StringToFloatDef(INIRec.ReadString(sSecao, 'ValorPis', ''), 0);
            AliquotaPis := StringToFloatDef(INIRec.ReadString(sSecao, 'AliquotaPis', ''), 0);

            ValorCofins := StringToFloatDef(INIRec.ReadString(sSecao, 'ValorCofins', ''), 0);
            AliquotaCofins := StringToFloatDef(INIRec.ReadString(sSecao, 'AliquotaCofins', ''), 0);

            ValorInss := StringToFloatDef(INIRec.ReadString(sSecao, 'ValorInss', ''), 0);
            AliquotaInss := StringToFloatDef(INIRec.ReadString(sSecao, 'AliquotaInss', ''), 0);

            ValorIr := StringToFloatDef(INIRec.ReadString(sSecao, 'ValorIr', ''), 0);
            AliquotaIr := StringToFloatDef(INIRec.ReadString(sSecao, 'AliquotaIr', ''), 0);

            ValorCsll := StringToFloatDef(INIRec.ReadString(sSecao, 'ValorCsll', ''), 0);
            AliquotaCsll := StringToFloatDef(INIRec.ReadString(sSecao, 'AliquotaCsll', ''), 0);

            ISSRetido := FProvider.StrToSituacaoTributaria(Ok, INIRec.ReadString(sSecao, 'ISSRetido', '0'));

            OutrasRetencoes := StringToFloatDef(INIRec.ReadString(sSecao, 'OutrasRetencoes', ''), 0);
            DescontoIncondicionado := StringToFloatDef(INIRec.ReadString(sSecao, 'DescontoIncondicionado', ''), 0);
            DescontoCondicionado := StringToFloatDef(INIRec.ReadString(sSecao, 'DescontoCondicionado', ''), 0);

            BaseCalculo := StringToFloatDef(INIRec.ReadString(sSecao, 'BaseCalculo', ''), 0);
            Aliquota := StringToFloatDef(INIRec.ReadString(sSecao, 'Aliquota', ''), 0);
            AliquotaSN := StringToFloatDef(INIRec.ReadString(sSecao, 'AliquotaSN', ''), 0);
            ValorIss := StringToFloatDef(INIRec.ReadString(sSecao, 'ValorIss', ''), 0);
            ValorIssRetido := StringToFloatDef(INIRec.ReadString(sSecao, 'ValorIssRetido', ''), 0);

            ValorLiquidoNfse := StringToFloatDef(INIRec.ReadString(sSecao, 'ValorLiquidoNfse', ''), 0);

            //Padrão Nacional
            ValorRecebido := StringToFloatDef(INIRec.ReadString(sSecao, 'ValorRecebido', ''), 0);
          end;

          //Padrão Nacional
          i := 1;
          while true do
          begin
            sSecao := 'DocumentosDeducoes' + IntToStrZero(i, 3);

            if not INIRec.SectionExists(sSecao) then
              break;

            with DocDeducao.New do
            begin
              chNFSe := INIRec.ReadString(sSecao,'chNFSe', '');
              chNFe := INIRec.ReadString(sSecao, 'chNFe', '');
              nDocFisc := INIRec.ReadString(sSecao, 'nDocFisc', '');
              nDoc := INIRec.ReadString(sSecao, 'nDoc', '');
              tpDedRed := StrTotpDedRed(Ok, INIRec.ReadString(sSecao, 'tpDedRed', '1'));
              xDescOutDed := INIRec.ReadString(sSecao, 'xDescOutDed', '');
              dtEmiDoc := INIRec.ReadDate(sSecao, 'dtEmiDoc', Now);
              vDedutivelRedutivel := StringToFloatDef(INIRec.ReadString(sSecao, 'vDedutivelRedutivel', ''), 0);
              vDeducaoReducao := StringToFloatDef(INIRec.ReadString(sSecao, 'vDeducaoReducao', ''), 0);

              NFSeMun.cMunNFSeMun := INIRec.ReadString(sSecao, 'cMunNFSeMun', '');
              NFSeMun.nNFSeMun := INIRec.ReadString(sSecao, 'nNFSeMun', '');
              NFSeMun.cVerifNFSeMun := INIRec.ReadString(sSecao, 'cVerifNFSeMun', '');

              NFNFS.nNFS := INIRec.ReadString(sSecao, 'nNFS', '');
              NFNFS.modNFS := INIRec.ReadString(sSecao, 'modNFS', '');
              NFNFS.serieNFS := INIRec.ReadString(sSecao, 'serieNFS', '');

              sSecao := 'DocumentosDeducoesFornecedor' + IntToStrZero(i, 3);
              if INIRec.SectionExists(sSecao) then
              begin
                with fornec.Identificacao do
                begin
                  CpfCnpj := INIRec.ReadString(sSecao, 'CNPJCPF', '');
                  InscricaoMunicipal := INIRec.ReadString(sSecao, 'InscricaoMunicipal', '');
                  Nif := INIRec.ReadString(sSecao, 'NIF', '');
                  cNaoNIF := StrToNaoNIF(Ok, INIRec.ReadString(sSecao, 'cNaoNIF', '0'));
                  CAEPF := INIRec.ReadString(sSecao, 'CAEPF', '');
                end;

                with fornec.Endereco do
                begin
                  CEP := INIRec.ReadString(sSecao, 'CEP', '');
                  xMunicipio := INIRec.ReadString(sSecao, 'xMunicipio', '');
                  UF := INIRec.ReadString(sSecao, 'UF', '');
                  Endereco := INIRec.ReadString(sSecao, 'Logradouro', '');
                  Numero := INIRec.ReadString(sSecao, 'Numero', '');
                  Complemento := INIRec.ReadString(sSecao, 'Complemento', '');
                  Bairro := INIRec.ReadString(sSecao, 'Bairro', '');
                end;

                with fornec.Contato do
                begin
                  Telefone := INIRec.ReadString(sSecao, 'Telefone', '');
                  Email := INIRec.ReadString(sSecao, 'Email', '');
                end;
              end;
            end;

            Inc(i);
          end;

          //Padrão Nacional
          sSecao := 'tribMun';
          if INIRec.SectionExists(sSecao) then
          begin
            with tribMun do
            begin
              tribISSQN := StrTotribISSQN(Ok, INIRec.ReadString(sSecao, 'tribISSQN', '1'));
              cPaisResult := INIRec.ReadInteger(sSecao, 'cPaisResult', 0);
              tpBM := StrTotpBM(Ok, INIRec.ReadString(sSecao, 'tpBM', '1'));
              nBM := INIRec.ReadString(sSecao, 'nBM', '');
              vRedBCBM := StringToFloatDef(INIRec.ReadString(sSecao, 'vRedBCBM', ''), 0);
              pRedBCBM := StringToFloatDef(INIRec.ReadString(sSecao, 'pRedBCBM', ''), 0);
              tpSusp := StrTotpSusp(Ok, INIRec.ReadString(sSecao, 'tpSusp', ''));
              nProcesso := INIRec.ReadString(sSecao, 'nProcesso', '');
              tpImunidade := StrTotpImunidade(Ok, INIRec.ReadString(sSecao, 'tpImunidade', ''));
              pAliq := StringToFloatDef(INIRec.ReadString(sSecao, 'pAliq', ''), 0);
              tpRetISSQN := StrTotpRetISSQN(Ok, INIRec.ReadString(sSecao, 'tpRetISSQN', ''));
            end;
          end;

          //Padrão Nacional
          sSecao := 'tribFederal';
          if INIRec.SectionExists(sSecao) then
          begin
            with tribFed do
            begin
              CST := StrToCST(Ok, INIRec.ReadString(sSecao, 'CST', ''));
              vBCPisCofins := StringToFloatDef(INIRec.ReadString(sSecao, 'vBCPisCofins', ''), 0);
              pAliqPis := StringToFloatDef(INIRec.ReadString(sSecao, 'pAliqPis', ''), 0);
              pAliqCofins := StringToFloatDef(INIRec.ReadString(sSecao, 'pAliqCofins' ,''), 0);
              vPis := StringToFloatDef(INIRec.ReadString(sSecao, 'vPis', ''), 0);
              vCofins := StringToFloatDef(INIRec.ReadString(sSecao, 'vCofins', ''), 0);
              tpRetPisCofins := StrTotpRetPisCofins(Ok, INIRec.ReadString(sSecao, 'tpRetPisCofins', ''));
              vRetCP := StringToFloatDef(INIRec.ReadString(sSecao, 'vRetCP', ''), 0);
              vRetIRRF := StringToFloatDef(INIRec.ReadString(sSecao, 'vRetIRRF', ''), 0);
              vRetCSLL := StringToFloatDef(INIRec.ReadString(sSecao, 'vRetCSLL', ''), 0);
            end;
          end;

          //Padrão Nacional
          sSecao := 'totTrib';
          if INIRec.SectionExists(sSecao) then
          begin
            with totTrib do
            begin
              indTotTrib := StrToindTotTrib(Ok, INIRec.ReadString(sSecao, 'indTotTrib', '0'));
              pTotTribSN := StringToFloatDef(INIRec.ReadString(sSecao, 'pTotTribSN', ''), 0);

              vTotTribFed := StringToFloatDef(INIRec.ReadString(sSecao, 'vTotTribFed', ''), 0);
              vTotTribEst := StringToFloatDef(INIRec.ReadString(sSecao, 'vTotTribEst', ''), 0);
              vTotTribMun := StringToFloatDef(INIRec.ReadString(sSecao, 'vTotTribMun', ''), 0);

              pTotTribFed := StringToFloatDef(INIRec.ReadString(sSecao, 'pTotTribFed', ''), 0);
              pTotTribEst := StringToFloatDef(INIRec.ReadString(sSecao, 'pTotTribEst', ''), 0);
              pTotTribMun := StringToFloatDef(INIRec.ReadString(sSecao, 'pTotTribMun', ''), 0);
            end;
          end;

        end;
      end;

      // Condição de Pagamento usado pelo provedor Betha versão 1 do Layout da ABRASF
      sSecao := 'CondicaoPagamento';

      with CondicaoPagamento do
      begin
        if INIRec.SectionExists(sSecao) then
        begin
          QtdParcela := INIRec.ReadInteger(sSecao, 'QtdParcela', 0);
          Condicao := FProvider.StrToCondicaoPag(Ok, INIRec.ReadString(sSecao, 'Condicao', 'A_VISTA'));
        end;
        i := 1;
        while true do
        begin
          sSecao := 'Parcelas' + IntToStrZero(i, 2);
          sFim := INIRec.ReadString(sSecao, 'Parcela'  ,'FIM');

          if (sFim = 'FIM') then
            break;

          with Parcelas.New do
          begin
            Parcela := sFim;
            DataVencimento := INIRec.ReadDate(sSecao, 'DataVencimento', Now);
            Valor := StringToFloatDef(INIRec.ReadString(sSecao, 'Valor', ''), 0);
          end;

          Inc(i);
        end;
      end;
    end;

    Result := True;
  finally
    INIRec.Free;
  end;
end;

function TNotaFiscal.GerarNFSeIni: String;
var
  I: integer;
  sSecao: String;
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
      INIRec.WriteString(sSecao, 'Numero', Numero);

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
      INIRec.WriteDate(sSecao, 'DataEmissao', Now);
      INIRec.WriteDate(sSecao, 'Competencia', Now);
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
      INIRec.WriteString(sSecao, 'Discriminacao', Servico.Discriminacao);
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
        with Servico.ItemServico.Items[I] do
        begin
          INIRec.WriteString(sSecao, 'Descricao', Descricao);
          INIRec.WriteString(sSecao, 'CodServico', CodServ);
          INIRec.WriteString(sSecao, 'codLCServico', CodLCServ);
          INIRec.WriteString(sSecao, 'CodigoCnae', CodigoCnae);
          INIRec.WriteString(sSecao, 'ItemListaServico', ItemListaServico);
          INIRec.WriteFloat(sSecao, 'Quantidade', Quantidade);
          INIRec.WriteFloat(sSecao, 'ValorUnitario', ValorUnitario);
          INIRec.WriteFloat(sSecao, 'ValorDeducoes', ValorDeducoes);
          INIRec.WriteFloat(sSecao, 'ValorIss', ValorISS);
          INIRec.WriteFloat(sSecao, 'Aliquota', Aliquota);
          INIRec.WriteFloat(sSecao, 'BaseCalculo', BaseCalculo);
          INIRec.WriteFloat(sSecao, 'DescontoIncondicionado', DescontoIncondicionado);
          INIRec.WriteFloat(sSecao, 'ValorTotal', ValorTotal);
          INIRec.WriteString(sSecao, 'Tributavel', FProvider.SimNaoToStr(Tributavel));
        end;
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

        //Lista de parcelas, xx pode variar de 01-99 (provedor Betha versão 1 do Layout da ABRASF)
        for I := 0 to CondicaoPagamento.Parcelas.Count - 1 do
        begin
          sSecao:= 'Parcelas' + IntToStrZero(I + 1, 2);
          with CondicaoPagamento.Parcelas.Items[I] do
          begin
            INIRec.WriteString(sSecao, 'Parcela', Parcela);
            INIRec.WriteDate(sSecao, 'DataVencimento', DataVencimento);
            INIRec.WriteFloat(sSecao, 'Valor', Valor);
          end;
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

function TNotaFiscal.LerXML(const AXML: String): Boolean;
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

procedure TNotaFiscal.SetXmlNfse(const Value: String);
begin
  LerXML(Value);
  FXmlNfse := Value;
end;

function TNotaFiscal.GravarXML(const NomeArquivo: String;
  const PathArquivo: String; aTipo: TtpXML): Boolean;
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

procedure TNotaFiscal.EnviarEmail(const sPara, sAssunto: String; sMensagem: TStrings;
  EnviaPDF: Boolean; sCC: TStrings; Anexos: TStrings; sReplyTo: TStrings;
  ManterPDFSalvo: Boolean);
var
  NomeArqTemp: String;
  AnexosEmail:TStrings;
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

function TNotaFiscal.GerarXML: String;
var
  FProvider: IACBrNFSeXProvider;
begin
  FProvider := TACBrNFSeX(FACBrNFSe).Provider;

  if not Assigned(FProvider) then
    raise EACBrNFSeException.Create(ERR_SEM_PROVEDOR);

  FProvider.GerarXml(NFSe, FXmlRps, FAlertas);
  Result := FXmlRps;
end;

function TNotaFiscal.GetXmlNfse: String;
begin
  Result := FXmlNfse;
  if Result = '' then
    Exit;

  if not XmlEhUTF8(Result) then
    Result := '<?xml version="1.0" encoding="UTF-8"?>' + Result;
end;

function TNotaFiscal.CalcularNomeArquivo: String;
var
  xID: String;
begin
  xID := TACBrNFSeX(FACBrNFSe).NumID[NFSe];

  if EstaVazio(xID) then
    raise EACBrNFSeException.Create('ID Inválido. Impossível Salvar XML');

  Result := xID + '-rps.xml';
end;

function TNotaFiscal.CalcularPathArquivo: String;
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

function TNotaFiscal.CalcularNomeArquivoCompleto(NomeArquivo: String;
  PathArquivo: String): String;
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

function TNotasFiscais.GerarIni: String;
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

function TNotasFiscais.GetNamePath: String;
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

function TNotasFiscais.FindByRps(ANumRPS: string): TNotaFiscal;
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

function TNotasFiscais.FindByNFSe(ANumNFSe: string): TNotaFiscal;
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

function TNotasFiscais.FindByCnpjCpfSerieRps(CnpjCpf, Serie,
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

function TNotasFiscais.LoadFromFile(const CaminhoArquivo: String;
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

function TNotasFiscais.LoadFromIni(const AIniString: String): Boolean;
begin
  with Self.New do
    LerArqIni(AIniString);

  Result := Self.Count > 0;
end;

function TNotasFiscais.LoadFromLoteNfse(const CaminhoArquivo: String): Boolean;
var
  XMLStr: String;
  XMLUTF8: AnsiString;
  i, l: integer;
  MS: TMemoryStream;
  P, N, TamTag, j: Integer;
  aXml, aXmlLote: string;
  TagF: Array[1..15] of String;
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

function TNotasFiscais.LoadFromString(AXMLString: String;
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

function TNotasFiscais.GravarXML(const PathNomeArquivo: String): Boolean;
var
  i: integer;
  NomeArq, PathArq : String;
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
