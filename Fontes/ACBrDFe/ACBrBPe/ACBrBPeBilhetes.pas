{******************************************************************************}
{ Projeto: Componente ACBrBPe                                                  }
{  Biblioteca multiplataforma de componentes Delphi para emissão de Bilhete de }
{ Passagem Eletrônica - BPe                                                    }
{                                                                              }
{ Direitos Autorais Reservados (c) 2017                                        }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do Projeto ACBr     }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr           }
{                                                                              }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{*******************************************************************************
|* Historico
|*
|* 20/06/2017: Italo Jurisato Junior
|*  - Doação do componente para o Projeto ACBr
*******************************************************************************}

{$I ACBr.inc}

unit ACBrBPeBilhetes;

interface

uses
  Classes, SysUtils, Dialogs, StrUtils,
  ACBrBPeConfiguracoes,
  pcnBPe, pcnBPeR, pcnBPeW, pcnConversao, pcnAuxiliar, pcnLeitor;

type

  { Bilhete }

  Bilhete = class(TCollectionItem)
  private
    FBPe: TBPe;
    FBPeW: TBPeW;
    FBPeR: TBPeR;

    FConfiguracoes: TConfiguracoesBPe;
    FXMLAssinado: String;
    FXMLOriginal: String;
    FAlertas: String;
    FErroValidacao: String;
    FErroValidacaoCompleto: String;
    FErroRegrasdeNegocios: String;
    FNomeArq: String;

    function GetConfirmada: Boolean;
    function GetProcessada: Boolean;
    function GetCancelada: Boolean;

    function GetMsg: String;
    function GetNumID: String;
    function GetXMLAssinado: String;
    procedure SetXML(AValue: String);
    procedure SetXMLOriginal(AValue: String);
    function ValidarConcatChave: Boolean;
    function CalcularNomeArquivo: String;
    function CalcularPathArquivo: String;
  public
    constructor Create(Collection2: TCollection); override;
    destructor Destroy; override;
    procedure Imprimir;
    procedure ImprimirPDF;

    procedure Assinar;
    procedure Validar;
    function VerificarAssinatura: Boolean;
    function ValidarRegrasdeNegocios: Boolean;

    function LerXML(AXML: String): Boolean;
    function LerArqIni(const AIniString: String): Boolean;

    function GerarXML: String;
    function GravarXML(NomeArquivo: String = ''; PathArquivo: String = ''): Boolean;

    function GerarTXT: String;
    function GravarTXT(NomeArquivo: String = ''; PathArquivo: String = ''): Boolean;

    function GravarStream(AStream: TStream): Boolean;

    procedure EnviarEmail(sPara, sAssunto: String; sMensagem: TStrings = nil;
      EnviaPDF: Boolean = True; sCC: TStrings = nil; Anexos: TStrings = nil;
      sReplyTo: TStrings = nil);

    property NomeArq: String read FNomeArq write FNomeArq;
    function CalcularNomeArquivoCompleto(NomeArquivo: String = '';
      PathArquivo: String = ''): String;

    property BPe: TBPe read FBPe;

    // Atribuir a "XML", faz o componente transferir os dados lido para as propriedades internas e "XMLAssinado"
    property XML: String         read FXMLOriginal   write SetXML;
    // Atribuir a "XMLOriginal", reflete em XMLAssinado, se existir a tag de assinatura
    property XMLOriginal: String read FXMLOriginal   write SetXMLOriginal;    // Sempre deve estar em UTF8
    property XMLAssinado: String read GetXMLAssinado write FXMLAssinado;      // Sempre deve estar em UTF8
    property Confirmada: Boolean read GetConfirmada;
    property Processada: Boolean read GetProcessada;
    property Cancelada: Boolean read GetCancelada;
    property Msg: String read GetMsg;
    property NumID: String read GetNumID;

    property Alertas: String read FAlertas;
    property ErroValidacao: String read FErroValidacao;
    property ErroValidacaoCompleto: String read FErroValidacaoCompleto;
    property ErroRegrasdeNegocios: String read FErroRegrasdeNegocios;

  end;

  { TBilhetes }

  TBilhetes = class(TOwnedCollection)
  private
    FACBrBPe: TComponent;
    FConfiguracoes: TConfiguracoesBPe;

    function GetItem(Index: integer): Bilhete;
    procedure SetItem(Index: integer; const Value: Bilhete);

    procedure VerificarDABPE;
  public
    constructor Create(AOwner: TPersistent; ItemClass: TCollectionItemClass);

    procedure GerarBPe;
    procedure Assinar;
    procedure Validar;
    function VerificarAssinatura(out Erros: String): Boolean;
    function ValidarRegrasdeNegocios(out Erros: String): Boolean;
    procedure Imprimir;
    procedure ImprimirCancelado;
    procedure ImprimirResumido;
    procedure ImprimirPDF;
    procedure ImprimirResumidoPDF;
    function Add: Bilhete;
    function Insert(Index: integer): Bilhete;

    property Items[Index: integer]: Bilhete read GetItem write SetItem; default;

    function GetNamePath: String; override;
    // Incluido o Parametro AGerarBPe que determina se após carregar os dados da BPe
    // para o componente, será gerado ou não novamente o XML da BPe.
    function LoadFromFile(CaminhoArquivo: String; AGerarBPe: Boolean = False): Boolean;
    function LoadFromStream(AStream: TStringStream; AGerarBPe: Boolean = False): Boolean;
    function LoadFromString(AXMLString: String; AGerarBPe: Boolean = False): Boolean;
    function LoadFromIni(AIniString: String): Boolean;

    function GravarXML(PathNomeArquivo: String = ''): Boolean;
    function GravarTXT(PathNomeArquivo: String = ''): Boolean;

    property ACBrBPe: TComponent read FACBrBPe;
  end;

implementation

uses
  dateutils, IniFiles,
  synautil,
  ACBrBPe, ACBrUtil, ACBrDFeUtil, pcnConversaoBPe;

{ Bilhete }

constructor Bilhete.Create(Collection2: TCollection);
begin
  inherited Create(Collection2);

  FBPe := TBPe.Create;
  FBPeW := TBPeW.Create(FBPe);
  FBPeR := TBPeR.Create(FBPe);
  FConfiguracoes := TACBrBPe(TBilhetes(Collection).ACBrBPe).Configuracoes;

  with TACBrBPe(TBilhetes(Collection).ACBrBPe) do
  begin
    FBPe.Ide.modelo := 63;
    FBPe.infBPe.Versao := VersaoBPeToDbl(Configuracoes.Geral.VersaoDF);

    FBPe.Ide.tpBPe := tbNormal;
    FBPe.Ide.verProc := 'ACBrBPe';
    FBPe.Ide.tpAmb := Configuracoes.WebServices.Ambiente;
    FBPe.Ide.tpEmis := Configuracoes.Geral.FormaEmissao;
  end;
end;

destructor Bilhete.Destroy;
begin
  FBPeW.Free;
  FBPeR.Free;
  FBPe.Free;
  inherited Destroy;
end;

procedure Bilhete.Imprimir;
begin
  with TACBrBPe(TBilhetes(Collection).ACBrBPe) do
  begin
    if not Assigned(DABPE) then
      raise EACBrBPeException.Create('Componente DABPE não associado.')
    else
      DABPE.ImprimirDABPE(BPe);
  end;
end;

procedure Bilhete.ImprimirPDF;
begin
  with TACBrBPe(TBilhetes(Collection).ACBrBPe) do
  begin
    if not Assigned(DABPE) then
      raise EACBrBPeException.Create('Componente DABPE não associado.')
    else
      DABPE.ImprimirDABPEPDF(BPe);
  end;
end;

procedure Bilhete.Assinar;
var
  XMLStr: String;
  XMLUTF8: AnsiString;
  Leitor: TLeitor;
begin
  with TACBrBPe(TBilhetes(Collection).ACBrBPe) do
  begin
    if not Assigned(SSL.AntesDeAssinar) then
      SSL.ValidarCNPJCertificado( BPe.Emit.CNPJ );
  end;

  // Gera novamente, para processar propriedades que podem ter sido modificadas
  XMLStr := GerarXML;

  // XML já deve estar em UTF8, para poder ser assinado //
  XMLUTF8 := ConverteXMLtoUTF8(XMLStr);

  with TACBrBPe(TBilhetes(Collection).ACBrBPe) do
  begin
    FXMLAssinado := SSL.Assinar(String(XMLUTF8), 'BPe', 'infBPe');
    // SSL.Assinar() sempre responde em UTF8...
    FXMLOriginal := FXMLAssinado;

    Leitor := TLeitor.Create;
    try
      leitor.Grupo := FXMLAssinado;
      BPe.signature.URI := Leitor.rAtributo('Reference URI=');
      BPe.signature.DigestValue := Leitor.rCampo(tcStr, 'DigestValue');
      BPe.signature.SignatureValue := Leitor.rCampo(tcStr, 'SignatureValue');
      BPe.signature.X509Certificate := Leitor.rCampo(tcStr, 'X509Certificate');
    finally
      Leitor.Free;
    end;

    with TACBrBPe(TBilhetes(Collection).ACBrBPe) do
    begin
      BPe.infBPeSupl.qrCodBPe := GetURLQRCode(BPe.Ide.cUF,
                                              BPe.Ide.tpAmb,
                                              BPe.infBPe.ID);

//      BPe.infBPeSupl.boardPassBPe := GetURLConsultaNFCe(BPe.Ide.cUF, BPe.Ide.tpAmb);

      GerarXML;
    end;

    if Configuracoes.Arquivos.Salvar and
       (not Configuracoes.Arquivos.SalvarApenasBPeProcessadas) then
    begin
      if NaoEstaVazio(NomeArq) then
        Gravar(NomeArq, FXMLAssinado)
      else
        Gravar(CalcularNomeArquivoCompleto(), FXMLAssinado);
    end;
  end;
end;

procedure Bilhete.Validar;
var
  Erro, AXML, DeclaracaoXML: String;
  BilheteEhValida: Boolean;
  ALayout: TLayOutBPe;
  VerServ: Real;
begin
  AXML := FXMLAssinado;
  if AXML = '' then
    AXML := XMLOriginal;

  with TACBrBPe(TBilhetes(Collection).ACBrBPe) do
  begin
    VerServ := FBPe.infBPe.Versao;
    ALayout := LayBPeRecepcao;

    // Extraindo apenas os dados da BPe (sem BPeProc)
    DeclaracaoXML := ObtemDeclaracaoXML(AXML);
    AXML := DeclaracaoXML + '<BPe xmlns' +
            RetornarConteudoEntre(AXML, '<BPe xmlns', '</BPe>') +
            '</BPe>';

    BilheteEhValida := SSL.Validar(AXML, GerarNomeArqSchema(ALayout, VerServ), Erro);

    if not BilheteEhValida then
    begin
      FErroValidacao := ACBrStr('Falha na validação dos dados do Bilhete: ') +
        IntToStr(BPe.Ide.nBP) + sLineBreak + FAlertas ;
      FErroValidacaoCompleto := FErroValidacao + sLineBreak + Erro;

      raise EACBrBPeException.CreateDef(
        IfThen(Configuracoes.Geral.ExibirErroSchema, ErroValidacaoCompleto,
        ErroValidacao));
    end;
  end;
end;

function Bilhete.VerificarAssinatura: Boolean;
var
  Erro, AXML, DeclaracaoXML: String;
  AssEhValida: Boolean;
begin
  AXML := FXMLAssinado;
  if AXML = '' then
    AXML := XMLOriginal;

  with TACBrBPe(TBilhetes(Collection).ACBrBPe) do
  begin

    // Extraindo apenas os dados do BPe (sem bpeProc)
    DeclaracaoXML := ObtemDeclaracaoXML(AXML);
    AXML := DeclaracaoXML + '<BPe xmlns' +
            RetornarConteudoEntre(AXML, '<BPe xmlns', '</BPe>') +
            '</BPe>';

    AssEhValida := SSL.VerificarAssinatura(AXML, Erro, 'infBPe');

    if not AssEhValida then
    begin
      FErroValidacao := ACBrStr('Falha na validação da assinatura do Bilhete: ') +
        IntToStr(BPe.Ide.nBP) + sLineBreak + Erro;
    end;
  end;

  Result := AssEhValida;
end;

function Bilhete.ValidarRegrasdeNegocios: Boolean;
var
  Erros: String;
  I: Integer;
  Agora: TDateTime;

  procedure GravaLog(AString: String);
  begin
    //DEBUG
    //Log := Log + FormatDateTime('hh:nn:ss:zzz',Now) + ' - ' + AString + sLineBreak;
  end;

  procedure AdicionaErro(const Erro: String);
  begin
    Erros := Erros + Erro + sLineBreak;
  end;

begin
  Agora := Now;
  GravaLog('Inicio da Validação');

  with TACBrBPe(TBilhetes(Collection).ACBrBPe) do
  begin
    Erros := '';

    GravaLog('Validar: 701-versão');
    if BPe.infBPe.Versao < 1.00 then
      AdicionaErro('701-Rejeição: Versão inválida');

    for I:=0 to BPe.autXML.Count-1 do
    begin
      GravaLog('Validar: 325-' + IntToStr(I) + '-CPF download');
      if (Length(Trim(OnlyNumber(BPe.autXML[I].CNPJCPF))) <= 11) and
        not ValidarCPF(BPe.autXML[I].CNPJCPF) then
        AdicionaErro('325-Rejeição: CPF autorizado para download inválido');

      GravaLog('Validar: 323-' + IntToStr(I) + '-CNPJ download');
      if (Length(Trim(OnlyNumber(BPe.autXML[I].CNPJCPF))) > 11) and
        not ValidarCNPJ(BPe.autXML[I].CNPJCPF) then
        AdicionaErro('323-Rejeição: CNPJ autorizado para download inválido');
    end;

  end;

  Result := EstaVazio(Erros);

  if not Result then
  begin
    Erros := ACBrStr('Erro(s) nas Regras de negócios do Bilhete: ' +
                     IntToStr(BPe.Ide.nBP) + sLineBreak + Erros);
  end;

  GravaLog('Fim da Validação. Tempo: ' +
           FormatDateTime('hh:nn:ss:zzz', Now - Agora) + sLineBreak +
           'Erros:' + Erros);

  //DEBUG
  //WriteToTXT('c:\temp\Bilhete.txt', Log);

  FErroRegrasdeNegocios := Erros;
end;

function Bilhete.LerXML(AXML: String): Boolean;
var
  XMLStr: String;
begin
  XMLOriginal := AXML;  // SetXMLOriginal() irá verificar se AXML está em UTF8

  { Verifica se precisa converter "AXML" de UTF8 para a String nativa da IDE.
    Isso é necessário, para que as propriedades fiquem com a acentuação correta }
  XMLStr := ParseText(AXML, True, XmlEhUTF8(AXML));

  FBPeR.Leitor.Arquivo := XMLStr;
  FBPeR.LerXml;

  Result := True;
end;

function Bilhete.GravarXML(NomeArquivo: String; PathArquivo: String): Boolean;
begin
  if EstaVazio(FXMLOriginal) then
    GerarXML;

  FNomeArq := CalcularNomeArquivoCompleto(NomeArquivo, PathArquivo);

  Result := TACBrBPe(TBilhetes(Collection).ACBrBPe).Gravar(FNomeArq, FXMLOriginal);
end;

function Bilhete.GravarTXT(NomeArquivo: String; PathArquivo: String): Boolean;
var
  ATXT: String;
begin
  FNomeArq := CalcularNomeArquivoCompleto(NomeArquivo, PathArquivo);
  ATXT := GerarTXT;
  Result := TACBrBPe(TBilhetes(Collection).ACBrBPe).Gravar(
    ChangeFileExt(FNomeArq, '.txt'), ATXT);
end;

function Bilhete.GravarStream(AStream: TStream): Boolean;
begin
  if EstaVazio(FXMLOriginal) then
    GerarXML;

  AStream.Size := 0;
  WriteStrToStream(AStream, AnsiString(FXMLOriginal));
  Result := True;
end;

procedure Bilhete.EnviarEmail(sPara, sAssunto: String; sMensagem: TStrings;
  EnviaPDF: Boolean; sCC: TStrings; Anexos: TStrings; sReplyTo: TStrings);
var
  NomeArq : String;
  AnexosEmail:TStrings;
  StreamBPe : TMemoryStream;
begin
  if not Assigned(TACBrBPe(TBilhetes(Collection).ACBrBPe).MAIL) then
    raise EACBrBPeException.Create('Componente ACBrMail não associado');

  AnexosEmail := TStringList.Create;
  StreamBPe := TMemoryStream.Create;
  try
    AnexosEmail.Clear;
    if Assigned(Anexos) then
      AnexosEmail.Assign(Anexos);

    with TACBrBPe(TBilhetes(Collection).ACBrBPe) do
    begin
      GravarStream(StreamBPe);

      if (EnviaPDF) then
      begin
        if Assigned(DABPE) then
        begin
          DABPE.ImprimirDABPEPDF(FBPe);
          NomeArq := PathWithDelim(DABPE.PathPDF) + NumID + '-bpe.pdf';
          AnexosEmail.Add(NomeArq);
        end;
      end;

      EnviarEmail( sPara, sAssunto, sMensagem, sCC, AnexosEmail, StreamBPe,
                   NumID + '-bpe.xml', sReplyTo);
    end;
  finally
    AnexosEmail.Free;
    StreamBPe.Free;
  end;
end;

function Bilhete.GerarXML: String;
var
  IdAnterior : String;
begin
  with TACBrBPe(TBilhetes(Collection).ACBrBPe) do
  begin
    IdAnterior := BPe.infBPe.ID;
    FBPeW.Gerador.Opcoes.FormatoAlerta  := Configuracoes.Geral.FormatoAlerta;
    FBPeW.Gerador.Opcoes.RetirarAcentos := Configuracoes.Geral.RetirarAcentos;
    FBPeW.Gerador.Opcoes.RetirarEspacos := Configuracoes.Geral.RetirarEspacos;
    FBPeW.Gerador.Opcoes.IdentarXML     := Configuracoes.Geral.IdentarXML;
    FBPeW.Opcoes.NormatizarMunicipios   := Configuracoes.Arquivos.NormatizarMunicipios;
    FBPeW.Opcoes.PathArquivoMunicipios  := Configuracoes.Arquivos.PathArquivoMunicipios;
    pcnAuxiliar.TimeZoneConf.Assign( Configuracoes.WebServices.TimeZoneConf );
  end;

  FBPeW.Opcoes.GerarTXTSimultaneamente := False;

  FBPeW.GerarXml;
  //DEBUG
  //WriteToTXT('c:\temp\Bilhete.xml', FBPeW.Gerador.ArquivoFormatoXML, False, False);

  XMLOriginal := FBPeW.Gerador.ArquivoFormatoXML;  // SetXMLOriginal() irá converter para UTF8

  { XML gerado pode ter nova Chave e ID, então devemos calcular novamente o
    nome do arquivo, mantendo o PATH do arquivo carregado }
  if (NaoEstaVazio(FNomeArq) and (IdAnterior <> FBPe.infBPe.ID)) then
    FNomeArq := CalcularNomeArquivoCompleto('', ExtractFilePath(FNomeArq));

  FAlertas := ACBrStr( FBPeW.Gerador.ListaDeAlertas.Text );
  Result := FXMLOriginal;
end;

function Bilhete.GerarTXT: String;
var
  IdAnterior : String;
begin
  with TACBrBPe(TBilhetes(Collection).ACBrBPe) do
  begin
    IdAnterior := BPe.infBPe.ID;
    FBPeW.Gerador.Opcoes.FormatoAlerta  := Configuracoes.Geral.FormatoAlerta;
    FBPeW.Gerador.Opcoes.RetirarAcentos := Configuracoes.Geral.RetirarAcentos;
    FBPeW.Gerador.Opcoes.RetirarEspacos := Configuracoes.Geral.RetirarEspacos;
    FBPeW.Gerador.Opcoes.IdentarXML     := Configuracoes.Geral.IdentarXML;
    FBPeW.Opcoes.NormatizarMunicipios   := Configuracoes.Arquivos.NormatizarMunicipios;
    FBPeW.Opcoes.PathArquivoMunicipios  := Configuracoes.Arquivos.PathArquivoMunicipios;
  end;

  FBPeW.Opcoes.GerarTXTSimultaneamente := True;

  FBPeW.GerarXml;
  XMLOriginal := FBPeW.Gerador.ArquivoFormatoXML;

  // XML gerado pode ter nova Chave e ID, então devemos calcular novamente o
  // nome do arquivo, mantendo o PATH do arquivo carregado
  if (NaoEstaVazio(FNomeArq) and (IdAnterior <> FBPe.infBPe.ID)) then
    FNomeArq := CalcularNomeArquivoCompleto('', ExtractFilePath(FNomeArq));

  FAlertas := FBPeW.Gerador.ListaDeAlertas.Text;
  Result := FBPeW.Gerador.ArquivoFormatoTXT;
end;

function Bilhete.CalcularNomeArquivo: String;
var
  xID: String;
  NomeXML: String;
begin
  xID := Self.NumID;

  if EstaVazio(xID) then
    raise EACBrBPeException.Create('ID Inválido. Impossível Salvar XML');

  NomeXML := '-bpe.xml';

  Result := xID + NomeXML;
end;

function Bilhete.CalcularPathArquivo: String;
var
  Data: TDateTime;
begin
  with TACBrBPe(TBilhetes(Collection).ACBrBPe) do
  begin
    if Configuracoes.Arquivos.EmissaoPathBPe then
      Data := FBPe.Ide.dhEmi
    else
      Data := Now;

    Result := PathWithDelim(Configuracoes.Arquivos.GetPathBPe(Data, FBPe.Emit.CNPJ));
  end;
end;

function Bilhete.CalcularNomeArquivoCompleto(NomeArquivo: String;
  PathArquivo: String): String;
var
  PathNoArquivo: String;
begin
  if EstaVazio(NomeArquivo) then
    NomeArquivo := CalcularNomeArquivo;

  PathNoArquivo := ExtractFilePath(NomeArquivo);
  if EstaVazio(PathNoArquivo) then
  begin
    if EstaVazio(PathArquivo) then
      PathArquivo := CalcularPathArquivo
    else
      PathArquivo := PathWithDelim(PathArquivo);
  end
  else
    PathArquivo := '';

  Result := PathArquivo + NomeArquivo;
end;

function Bilhete.ValidarConcatChave: Boolean;
var
  wAno, wMes, wDia: word;
  chaveBPe : String;
begin
  DecodeDate(BPe.ide.dhEmi, wAno, wMes, wDia);

  chaveBPe := 'BPe' + OnlyNumber(BPe.infBPe.ID);
  {(*}
  Result := not
    ((Copy(chaveBPe, 4, 2) <> IntToStrZero(BPe.Ide.cUF, 2)) or
    (Copy(chaveBPe, 6, 2)  <> Copy(FormatFloat('0000', wAno), 3, 2)) or
    (Copy(chaveBPe, 8, 2)  <> FormatFloat('00', wMes)) or
    (Copy(chaveBPe, 10, 14)<> PadLeft(OnlyNumber(BPe.Emit.CNPJ), 14, '0')) or
    (Copy(chaveBPe, 24, 2) <> IntToStrZero(BPe.Ide.modelo, 2)) or
    (Copy(chaveBPe, 26, 3) <> IntToStrZero(BPe.Ide.serie, 3)) or
    (Copy(chaveBPe, 29, 9) <> IntToStrZero(BPe.Ide.nBP, 9)) or
    (Copy(chaveBPe, 38, 1) <> TpEmisToStr(BPe.Ide.tpEmis)) or
    (Copy(chaveBPe, 39, 8) <> IntToStrZero(BPe.Ide.cBP, 8)));
  {*)}
end;

function Bilhete.GetConfirmada: Boolean;
begin
  Result := TACBrBPe(TBilhetes(Collection).ACBrBPe).CstatConfirmada(
    FBPe.procBPe.cStat);
end;

function Bilhete.GetProcessada: Boolean;
begin
  Result := TACBrBPe(TBilhetes(Collection).ACBrBPe).CstatProcessado(
    FBPe.procBPe.cStat);
end;

function Bilhete.GetCancelada: Boolean;
begin
  Result := TACBrBPe(TBilhetes(Collection).ACBrBPe).CstatCancelada(
    FBPe.procBPe.cStat);
end;

function Bilhete.GetMsg: String;
begin
  Result := FBPe.procBPe.xMotivo;
end;

function Bilhete.GetNumID: String;
begin
  Result := OnlyNumber(BPe.infBPe.ID);
end;

function Bilhete.GetXMLAssinado: String;
begin
  if EstaVazio(FXMLAssinado) then
    Assinar;

  Result := FXMLAssinado;
end;

procedure Bilhete.SetXML(AValue: String);
begin
  LerXML(AValue);
end;

procedure Bilhete.SetXMLOriginal(AValue: String);
var
  XMLUTF8: String;
begin
  { Garante que o XML informado está em UTF8, se ele realmente estiver, nada
    será modificado por "ConverteXMLtoUTF8"  (mantendo-o "original") }
  XMLUTF8 := ConverteXMLtoUTF8(AValue);

  FXMLOriginal := XMLUTF8;

  if XmlEstaAssinado(FXMLOriginal) then
    FXMLAssinado := FXMLOriginal
  else
    FXMLAssinado := '';
end;

function Bilhete.LerArqIni(const AIniString: String): Boolean;
var
  INIRec: TMemIniFile;
  sSecao, versao, sFim: String;
  OK: Boolean;
  I: Integer;
begin
  Result := False;

  INIRec := TMemIniFile.Create('');
  try
    LerIniArquivoOuString(AIniString, INIRec);

    with FBPe do
    begin
      infBPe.versao := StringToFloatDef(INIRec.ReadString('infBPe', 'versao', VersaoBPeToStr(FConfiguracoes.Geral.VersaoDF)),0);

      versao := FloatToString(infBPe.versao, '.', '#0.00');
      FConfiguracoes.Geral.VersaoDF := StrToVersaoBPe(OK, versao);

      Ide.tpAmb   := StrToTpAmb(OK, INIRec.ReadString( 'ide', 'tpAmb', TpAmbToStr(FConfiguracoes.WebServices.Ambiente)));
      Ide.modelo  := INIRec.ReadInteger('ide', 'mod', 63);
      Ide.serie   := INIRec.ReadInteger('ide', 'serie', 1);
      Ide.nBP     := INIRec.ReadInteger('ide', 'nBP', 0);
      Ide.cBP     := INIRec.ReadInteger('ide', 'cBP', 0);
      Ide.modal   := StrToModalBPe(OK, INIRec.ReadString('ide', 'modal', '1'));
      Ide.dhEmi   := StringToDateTime(INIRec.ReadString('ide', 'dhEmi', '0'));
      Ide.tpEmis  := StrToTpEmis(OK, INIRec.ReadString('ide', 'tpEmis', IntToStr(FConfiguracoes.Geral.FormaEmissaoCodigo)));
      Ide.verProc := INIRec.ReadString('ide', 'verProc', 'ACBrBPe');
      Ide.tpBPe   := StrTotpBPe(OK,INIRec.ReadString('ide', 'tpBPe', '0'));
      Ide.indPres := StrToPresencaComprador(OK, INIRec.ReadString('ide', 'indPres', '1'));
      Ide.UFIni   := INIRec.ReadString('ide', 'UFIni', '');
      Ide.cMunIni := INIRec.ReadInteger('ide', 'cMunIni', 0);
      Ide.UFFim   := INIRec.ReadString('ide', 'UFFim', '');
      Ide.cMunFim := INIRec.ReadInteger('ide', 'cMunFim', 0);
      Ide.dhCont  := StringToDateTime(INIRec.ReadString('ide', 'dhCont', '0'));
      Ide.xJust   := INIRec.ReadString('ide', 'xJust', '');

      //
      // Seção [emit] Emitente do BP-e
      //
      Emit.CNPJ  := INIRec.ReadString('emit', 'CNPJ', '');
      Emit.IE    := INIRec.ReadString('emit', 'IE', '');
      Emit.IEST  := INIRec.ReadString('emit', 'IEST', '');
      Emit.xNome := INIRec.ReadString('emit', 'xNome', '');
      Emit.xFant := INIRec.ReadString('emit', 'xFant', '');
      Emit.IM    := INIRec.ReadString('emit', 'IM', '');
      Emit.CNAE  := INIRec.ReadString('emit', 'CNAE', '');
      Emit.CRT   := StrToCRT(ok, INIRec.ReadString('emit', 'CRT', '3'));
      Emit.TAR   := INIRec.ReadString('emit', 'TAR', '');

      Emit.enderEmit.xLgr    := INIRec.ReadString('emit', 'xLgr', '');
      Emit.enderEmit.nro     := INIRec.ReadString('emit', 'nro', '');
      Emit.enderEmit.xCpl    := INIRec.ReadString('emit', 'xCpl', '');
      Emit.enderEmit.xBairro := INIRec.ReadString('emit', 'xBairro', '');
      Emit.enderEmit.cMun    := INIRec.ReadInteger('emit', 'cMun', 0);
      Emit.enderEmit.xMun    := INIRec.ReadString('emit', 'xMun', '');
      Emit.enderEmit.CEP     := INIRec.ReadInteger('emit', 'CEP', 0);
      Emit.enderEmit.UF      := INIRec.ReadString('emit', 'UF', '');

      ide.cUF := INIRec.ReadInteger('ide', 'cUF', UFparaCodigo(Emit.enderEmit.UF));

//          if Emit.enderEmit.cMun <= 0 then
//            Emit.enderEmit.cMun := ObterCodigoMunicipio(Emit.enderEmit.xMun, Emit.enderEmit.UF);

      Emit.enderEmit.fone    := INIRec.ReadString('emit', 'fone', '');
      Emit.enderEmit.Email   := INIRec.ReadString('emit', 'Email', '');

      //
      // Seção [comp] Comprador
      //
      Comp.xNome         := INIRec.ReadString('comp', 'xNome', '');
      Comp.CNPJCPF       := INIRec.ReadString('comp', 'CNPJCPF', '');
      Comp.idEstrangeiro := INIRec.ReadString('comp', 'idEstrangeiro', '');
      Comp.IE            := INIRec.ReadString('comp', 'IE', '');

      Comp.enderComp.xLgr    := INIRec.ReadString('comp', 'xLgr', '');
      Comp.enderComp.nro     := INIRec.ReadString('comp', 'nro', '');
      Comp.enderComp.xCpl    := INIRec.ReadString('comp', 'xCpl', '');
      Comp.enderComp.xBairro := INIRec.ReadString('comp', 'xBairro', '');
      Comp.enderComp.cMun    := INIRec.ReadInteger('comp', 'cMun', 0);
      Comp.enderComp.xMun    := INIRec.ReadString('comp', 'xMun', '');
      Comp.enderComp.CEP     := INIRec.ReadInteger('comp', 'CEP', 0);
      Comp.enderComp.UF      := INIRec.ReadString('comp', 'UF', '');

//          if Comp.enderComp.cMun <= 0 then
//            Comp.enderComp.cMun := ObterCodigoMunicipio(Comp.enderComp.xMun, Comp.enderComp.UF);

      Comp.EnderComp.cPais   := INIRec.ReadInteger('comp', 'cPais', 1058);
      Comp.EnderComp.xPais   := INIRec.ReadString('comp', 'xPais', 'BRASIL');
      Comp.enderComp.fone    := INIRec.ReadString('comp', 'fone', '');
      Comp.enderComp.Email   := INIRec.ReadString('comp', 'Email', '');

      //
      // Seção [Agencia] Agência que comercializou o BP-e
      //
      Agencia.xNome := INIRec.ReadString('Agencia', 'xNome', '');
      Agencia.CNPJ  := INIRec.ReadString('Agencia', 'CNPJ', '');

      Agencia.enderAgencia.xLgr    := INIRec.ReadString('Agencia', 'xLgr', '');
      Agencia.enderAgencia.nro     := INIRec.ReadString('Agencia', 'nro', '');
      Agencia.enderAgencia.xCpl    := INIRec.ReadString('Agencia', 'xCpl', '');
      Agencia.enderAgencia.xBairro := INIRec.ReadString('Agencia', 'xBairro', '');
      Agencia.enderAgencia.cMun    := INIRec.ReadInteger('Agencia', 'cMun', 0);
      Agencia.enderAgencia.xMun    := INIRec.ReadString('Agencia', 'xMun', '');
      Agencia.enderAgencia.CEP     := INIRec.ReadInteger('Agencia', 'CEP', 0);
      Agencia.enderAgencia.UF      := INIRec.ReadString('Agencia', 'UF', '');

//          if Agencia.enderAgencia.cMun <= 0 then
//            Agencia.enderAgencia.cMun := ObterCodigoMunicipio(Agencia.enderAgencia.xMun, Agencia.enderAgencia.UF);

      Agencia.enderAgencia.fone    := INIRec.ReadString('Agencia', 'fone', '');
      Agencia.enderAgencia.Email   := INIRec.ReadString('Agencia', 'Email', '');

      //
      // Seção [infBPeSub] Informações do BP-e Substituido
      //
      if INIRec.ReadString('infBPeSub', 'chBPe', '') <> '' then
      begin
        with infBPeSub do
        begin
          chBPe := INIRec.ReadString('infBPeSub', 'chBPe', '');
          tpSub := StrTotpSubstituicao(OK, INIRec.ReadString('infBPeSub', 'tpSub', '1'));
        end;
      end;

      //
      // Seção [infPassagem] Informações da Passagem
      //
      infPassagem.cLocOrig := INIRec.ReadString('infPassagem', 'cLocOrig', '');
      infPassagem.xLocOrig := INIRec.ReadString('infPassagem', 'xLocOrig', '');
      infPassagem.cLocDest := INIRec.ReadString('infPassagem', 'cLocDest', '');
      infPassagem.xLocDest := INIRec.ReadString('infPassagem', 'xLocDest', '');
      infPassagem.dhEmb    := StringToDateTime(INIRec.ReadString('infPassagem', 'dhEmb', '0'));
	     infPassagem.dhValidade    := StringToDateTime(INIRec.ReadString('infPassagem', 'dhValidade', '0'));

      //
      // Seção [infPassageiro] Informações do Passageiro
      //
      infPassagem.infPassageiro.xNome := INIRec.ReadString('infPassageiro', 'xNome', '');
      infPassagem.infPassageiro.CPF   := INIRec.ReadString('infPassageiro', 'CPF', '');
      infPassagem.infPassageiro.tpDoc := StrTotpDocumento(Ok, INIRec.ReadString('infPassageiro', 'tpDoc', '1'));
      infPassagem.infPassageiro.nDoc  := INIRec.ReadString('infPassageiro', 'nDoc', '');
      infPassagem.infPassageiro.dNasc := StringToDateTime(INIRec.ReadString('infPassageiro', 'dNasc', '0'));
      infPassagem.infPassageiro.fone  := INIRec.ReadString('infPassageiro', 'fone', '');
      infPassagem.infPassageiro.Email := INIRec.ReadString('infPassageiro', 'Email', '');

      //
      // Seção [infViagemxxx] Informações da Viagem
      //
      I := 1;
      while true do
      begin
        sSecao := 'infViagem' + IntToStrZero(I, 3);
        sFim   := INIRec.ReadString(sSecao, 'cPercurso', 'FIM');
        if (sFim = 'FIM') or (Length(sFim) <= 0) then
          break;

        with infViagem.Add do
        begin
          cPercurso    := sFim;
          xPercurso    := INIRec.ReadString(sSecao, 'xPercurso', '');
          tpViagem     := StrTotpViagem(Ok, INIRec.ReadString(sSecao, 'tpViagem', '00'));
          tpServ       := StrTotpServico(Ok, INIRec.ReadString(sSecao, 'tpServ', '1'));
          tpAcomodacao := StrTotpAcomodacao(Ok, INIRec.ReadString(sSecao, 'tpAcomodacao', '1'));
          tpTrecho     := StrTotpTrecho(Ok, INIRec.ReadString(sSecao, 'tpTrecho', '1'));
          dhViagem     := StringToDateTime(INIRec.ReadString(sSecao, 'dhViagem', '0'));
          dhConexao    := StringToDateTime(INIRec.ReadString(sSecao, 'dhConexao', '0'));
          Prefixo      := INIRec.ReadString(sSecao, 'Prefixo', '');
          Poltrona     := INIRec.ReadInteger(sSecao, 'Poltrona', 0);
          Plataforma   := INIRec.ReadString(sSecao, 'Plataforma', '');

          //
          // Informações da Travessia
          //
          if INIRec.ReadString(sSecao, 'tpVeiculo', '') <> '' then
          begin
            with infTravessia do
            begin
              tpVeiculo  := StrTotpVeiculo(Ok, INIRec.ReadString(sSecao, 'tpVeiculo', '01'));
              sitVeiculo := StrToSitVeiculo(Ok, INIRec.ReadString(sSecao, 'sitVeiculo', '01'));
            end;
          end;
        end;

        Inc(I);
      end;

      //
      // Seção [infValorBPe] Informações dos Valores do BP-e
      //
      infValorBPe.vBP        := StringToFloatDef(INIRec.ReadString('infValorBPe', 'vBP', ''), 0);
      infValorBPe.vDesconto  := StringToFloatDef(INIRec.ReadString('infValorBPe', 'vDesconto', ''), 0);
      infValorBPe.vPgto      := StringToFloatDef(INIRec.ReadString('infValorBPe', 'vPgto', ''), 0);
      infValorBPe.vTroco     := StringToFloatDef(INIRec.ReadString('infValorBPe', 'vTroco', ''), 0);
      infValorBPe.tpDesconto := StrTotpDesconto(Ok, INIRec.ReadString('infValorBPe', 'tpDesconto', '01'));
      infValorBPe.xDesconto  := INIRec.ReadString('infValorBPe', 'xDesconto', '');

      //
      // Seção [Compxxx] Componentes do Valor do BPe
      //
      I := 1;
      while true do
      begin
        sSecao := 'Comp' + IntToStrZero(I, 3);
        sFim   := INIRec.ReadString(sSecao, 'tpComp', 'FIM');
        if (sFim = 'FIM') or (Length(sFim) <= 0) then
          break;

        with infValorBPe.Comp.Add do
        begin
          tpComp := StrTotpComponente(Ok, sFim);
          vComp  := StringToFloatDef(INIRec.ReadString(sSecao, 'vComp', ''), 0);
        end;

        Inc(I);
      end;

      //
      // Seção [ICMS] Informacoes relativas aos Impostos
      //
      with Imp do
      begin
        sSecao := 'ICMS';
        sFim   := INIRec.ReadString(sSecao, 'CST', 'FIM');

        if (sFim <> 'FIM') then
        begin
          with ICMS do
          begin
            CST    := StrToCSTICMS(OK, sFim);
            pRedBC := StringToFloatDef(INIRec.ReadString(sSecao, 'pRedBC', ''), 0);
            vBC    := StringToFloatDef(INIRec.ReadString(sSecao, 'vBC', ''), 0);
            pICMS  := StringToFloatDef(INIRec.ReadString(sSecao, 'pICMS', ''), 0);
            vICMS  := StringToFloatDef(INIRec.ReadString(sSecao, 'vICMS', ''), 0);
            vCred  := StringToFloatDef(INIRec.ReadString(sSecao, 'vCred', ''), 0);

            pRedBCOutraUF := StringToFloatDef(INIRec.ReadString(sSecao, 'pRedBCOutraUF', ''), 0);
            vBCOutraUF    := StringToFloatDef(INIRec.ReadString(sSecao, 'vBCOutraUF', ''), 0);
            pICMSOutraUF  := StringToFloatDef(INIRec.ReadString(sSecao, 'pICMSOutraUF', ''), 0);
            vICMSOutraUF  := StringToFloatDef(INIRec.ReadString(sSecao, 'vICMSOutraUF', ''), 0);
          end;

          vTotTrib   := StringToFloatDef(INIRec.ReadString(sSecao, 'vTotTrib', ''), 0);
          infAdFisco := INIRec.ReadString(sSecao, 'infAdFisco', '');
        end;

        if StringToFloatDef(INIRec.ReadString('ICMSUFFim', 'pICMSInterPart', ''), 0) <> 0 then
        begin
          ICMSUFFim.vBCUFFim       := StringToFloatDef(INIRec.ReadString('ICMSUFFim', 'vBCUFFim', ''), 0);
          ICMSUFFim.pFCPUFFim      := StringToFloatDef(INIRec.ReadString('ICMSUFFim', 'pFCPUFFim', ''), 0);
          ICMSUFFim.pICMSUFFim     := StringToFloatDef(INIRec.ReadString('ICMSUFFim', 'pICMSUFFim', ''), 0);
          ICMSUFFim.pICMSInter     := StringToFloatDef(INIRec.ReadString('ICMSUFFim', 'pICMSInter', ''), 0);
          ICMSUFFim.pICMSInterPart := StringToFloatDef(INIRec.ReadString('ICMSUFFim', 'pICMSInterPart', ''), 0);
          ICMSUFFim.vFCPUFFim      := StringToFloatDef(INIRec.ReadString('ICMSUFFim', 'vFCPUFFim', ''), 0);
          ICMSUFFim.vICMSUFFim     := StringToFloatDef(INIRec.ReadString('ICMSUFFim', 'vICMSUFFim', ''), 0);
          ICMSUFFim.vICMSUFIni     := StringToFloatDef(INIRec.ReadString('ICMSUFFim', 'vICMSUFIni', ''), 0);
        end;
      end;

      //
      // Seção [Pagxx] Dados do Pagamento 01-10
      //
      I := 1 ;
      while true do
      begin
        sSecao := 'pag'+IntToStrZero(I,2) ;
        sFim   := INIRec.ReadString(sSecao, 'tpag', 'FIM');
        if (sFim = 'FIM') or (Length(sFim) <= 0) then
          break ;

        with pag.Add do
        begin
          tPag  := StrToFormaPagamento(OK, sFim);
          vPag  := StringToFloatDef(INIRec.ReadString(sSecao, 'vPag', ''), 0);

          tpIntegra := StrTotpIntegra(OK,INIRec.ReadString(sSecao, 'tpIntegra', ''));
          CNPJ      := INIRec.ReadString(sSecao, 'CNPJ', '');
          tBand     := StrToBandeiraCartao(OK, INIRec.ReadString(sSecao, 'tBand', '99'));
          cAut      := INIRec.ReadString(sSecao, 'cAut', '');
        end;

        Inc(I);
      end;

      //
      // Seção [auxXMLxxx] Autorizados para Download do XML do BPe
      //
      I := 1 ;
      while true do
      begin
        sSecao := 'autXML'+IntToStrZero(I,3) ;
        sFim   := OnlyNumber(INIRec.ReadString(sSecao, 'CNPJCPF', 'FIM'));
        if (sFim = 'FIM') or (Length(sFim) <= 0) then
          break ;

        with autXML.Add do
          CNPJCPF := sFim;

        Inc(I);
      end;

      //
      // Seção [infAdic] Informações Adicionais
      //
      InfAdic.infAdFisco := INIRec.ReadString('infAdic','infAdFisco', '');
      InfAdic.infCpl     := INIRec.ReadString('infAdic','infCpl', '');
    end;

    GerarXML;

    Result := True;
  finally
     INIRec.Free;
  end;
end;

{ TBilhetes }

constructor TBilhetes.Create(AOwner: TPersistent; ItemClass: TCollectionItemClass);
begin
  if not (AOwner is TACBrBPe) then
    raise EACBrBPeException.Create('AOwner deve ser do tipo TACBrBPe');

  inherited;

  FACBrBPe := TACBrBPe(AOwner);
  FConfiguracoes := TACBrBPe(FACBrBPe).Configuracoes;
end;


function TBilhetes.Add: Bilhete;
begin
  Result := Bilhete(inherited Add);
end;

procedure TBilhetes.Assinar;
var
  i: integer;
begin
  for i := 0 to Self.Count - 1 do
    Self.Items[i].Assinar;
end;

procedure TBilhetes.GerarBPe;
var
  i: integer;
begin
  for i := 0 to Self.Count - 1 do
    Self.Items[i].GerarXML;
end;

function TBilhetes.GetItem(Index: integer): Bilhete;
begin
  Result := Bilhete(inherited Items[Index]);
end;

function TBilhetes.GetNamePath: String;
begin
  Result := 'Bilhete';
end;

procedure TBilhetes.VerificarDABPE;
begin
  if not Assigned(TACBrBPe(FACBrBPe).DABPE) then
    raise EACBrBPeException.Create('Componente DABPE não associado.');
end;

procedure TBilhetes.Imprimir;
begin
  VerificarDABPE;
  TACBrBPe(FACBrBPe).DABPE.ImprimirDABPE(nil);
end;

procedure TBilhetes.ImprimirCancelado;
begin
  VerificarDABPE;
  TACBrBPe(FACBrBPe).DABPE.ImprimirDABPECancelado(nil);
end;

procedure TBilhetes.ImprimirResumido;
begin
  VerificarDABPE;
  TACBrBPe(FACBrBPe).DABPE.ImprimirDABPEResumido(nil);
end;

procedure TBilhetes.ImprimirPDF;
begin
  VerificarDABPE;
  TACBrBPe(FACBrBPe).DABPE.ImprimirDABPEPDF(nil);
end;

procedure TBilhetes.ImprimirResumidoPDF;
begin
  VerificarDABPE;
  TACBrBPe(FACBrBPe).DABPE.ImprimirDABPEResumidoPDF(nil);
end;

function TBilhetes.Insert(Index: integer): Bilhete;
begin
  Result := Bilhete(inherited Insert(Index));
end;

procedure TBilhetes.SetItem(Index: integer; const Value: Bilhete);
begin
  Items[Index].Assign(Value);
end;

procedure TBilhetes.Validar;
var
  i: integer;
begin
  for i := 0 to Self.Count - 1 do
    Self.Items[i].Validar;   // Dispara exception em caso de erro
end;

function TBilhetes.VerificarAssinatura(out Erros: String): Boolean;
var
  i: integer;
begin
  Result := True;
  Erros := '';

  if Self.Count < 1 then
  begin
    Erros := 'Nenhum BPe carregado';
    Result := False;
    Exit;
  end;

  for i := 0 to Self.Count - 1 do
  begin
    if not Self.Items[i].VerificarAssinatura then
    begin
      Result := False;
      Erros := Erros + Self.Items[i].ErroValidacao + sLineBreak;
    end;
  end;
end;

function TBilhetes.ValidarRegrasdeNegocios(out Erros: String): Boolean;
var
  i: integer;
begin
  Result := True;
  Erros := '';

  for i := 0 to Self.Count - 1 do
  begin
    if not Self.Items[i].ValidarRegrasdeNegocios then
    begin
      Result := False;
      Erros := Erros + Self.Items[i].ErroRegrasdeNegocios + sLineBreak;
    end;
  end;
end;

function TBilhetes.LoadFromFile(CaminhoArquivo: String;
  AGerarBPe: Boolean): Boolean;
var
  XMLUTF8: AnsiString;
  i, l: integer;
  MS: TMemoryStream;
begin
  MS := TMemoryStream.Create;
  try
    MS.LoadFromFile(CaminhoArquivo);
    XMLUTF8 := ReadStrFromStream(MS, MS.Size);
  finally
    MS.Free;
  end;

  l := Self.Count; // Indice da última Bilhete já existente
  Result := LoadFromString(String(XMLUTF8), AGerarBPe);

  if Result then
  begin
    // Atribui Nome do arquivo a novas Bilhetes inseridas //
    for i := l to Self.Count - 1 do
      Self.Items[i].NomeArq := CaminhoArquivo;
  end;
end;

function TBilhetes.LoadFromStream(AStream: TStringStream;
  AGerarBPe: Boolean): Boolean;
var
  AXML: AnsiString;
begin
  AStream.Position := 0;
  AXML := ReadStrFromStream(AStream, AStream.Size);

  Result := Self.LoadFromString(String(AXML), AGerarBPe);
end;

function TBilhetes.LoadFromString(AXMLString: String;
  AGerarBPe: Boolean): Boolean;
var
  ABPeXML, XMLStr: AnsiString;
  P, N: integer;

  function PosBPe: integer;
  begin
    Result := pos('</BPe>', XMLStr);
  end;

begin
  // Verifica se precisa Converter de UTF8 para a String nativa da IDE //
  XMLStr := ConverteXMLtoNativeString(AXMLString);

  N := PosBPe;
  while N > 0 do
  begin
    P := pos('</BPeProc>', XMLStr);

    if P <= 0 then
      P := pos('</procBPe>', XMLStr);  // BPe obtida pelo Portal da Receita

    if P > 0 then
    begin
      ABPeXML := copy(XMLStr, 1, P + 10);
      XMLStr := Trim(copy(XMLStr, P + 10, length(XMLStr)));
    end
    else
    begin
      ABPeXML := copy(XMLStr, 1, N + 6);
      XMLStr := Trim(copy(XMLStr, N + 6, length(XMLStr)));
    end;

    with Self.Add do
    begin
      LerXML(ABPeXML);

      if AGerarBPe then // Recalcula o XML
        GerarXML;
    end;

    N := PosBPe;
  end;

  Result := Self.Count > 0;
end;

function TBilhetes.LoadFromIni(AIniString: String): Boolean;
begin
  with Self.Add do
    LerArqIni(AIniString);

  Result := Self.Count > 0;
end;

function TBilhetes.GravarXML(PathNomeArquivo: String): Boolean;
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

function TBilhetes.GravarTXT(PathNomeArquivo: String): Boolean;
var
  SL: TStringList;
  ArqTXT: String;
  I: integer;
begin
  Result := False;
  SL := TStringList.Create;
  try
    SL.Clear;
    for I := 0 to Self.Count - 1 do
    begin
      ArqTXT := Self.Items[I].GerarTXT;
      SL.Add(ArqTXT);
    end;

    if SL.Count > 0 then
    begin
      // Inserindo cabeçalho //
      SL.Insert(0, 'BILHETE|' + IntToStr(Self.Count));

      // Apagando as linhas em branco //
      i := 0;
      while (i <= SL.Count - 1) do
      begin
        if SL[I] = '' then
          SL.Delete(I)
        else
          Inc(i);
      end;

      if EstaVazio(PathNomeArquivo) then
        PathNomeArquivo := PathWithDelim(
          TACBrBPe(FACBrBPe).Configuracoes.Arquivos.PathSalvar) + 'BPe.TXT';

      SL.SaveToFile(PathNomeArquivo);
      Result := True;
    end;
  finally
    SL.Free;
  end;
end;

end.
