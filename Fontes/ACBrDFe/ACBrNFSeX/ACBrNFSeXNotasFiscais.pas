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
      sReplyTo: TStrings = nil; ManterPDFSalvo: Boolean = True;
      sBCC: TStrings = nil);

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

function TNotaFiscal.LerArqIni(const AIniString: string): Boolean;
var
  FProvider: IACBrNFSeXProvider;
begin
  // O código abaixo se utiliza da Interface para ler o arquivo INI do RPS ou da NFS-e

  FProvider := TACBrNFSeX(FACBrNFSe).Provider;

  if not Assigned(FProvider) then
    raise EACBrNFSeException.Create(ERR_SEM_PROVEDOR);

  Result := FProvider.LerIni(AIniString, FNFSe);
end;

function TNotaFiscal.GerarNFSeIni: string;
var
  FProvider: IACBrNFSeXProvider;
begin
  // O código abaixo se utiliza da Interface para gerar o arquivo INI do RPS ou da NFS-e

  FProvider := TACBrNFSeX(FACBrNFSe).Provider;

  if not Assigned(FProvider) then
    raise EACBrNFSeException.Create(ERR_SEM_PROVEDOR);

  Result := FProvider.GerarIni(FNFSe);
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
  ManterPDFSalvo: Boolean; sBCC: TStrings);
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
                   NumID[FNFSe] +'-nfse.xml', sReplyTo, sBCC);
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
    TagF[08] := '<nfdok ';            // SmarAPD possui o atributo numeronfd
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
    TagF[08] := '</nfdok>';            // SmarAPD
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
