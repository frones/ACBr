{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
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

unit ACBrDCeDeclaracoes;

interface

uses
  Classes,
	SysUtils, 
	StrUtils,
  ACBrDCeConfiguracoes,
  ACBrDCe.Classes,
	ACBrDCe.XmlReader,
	ACBrDCe.XmlWriter,
	pcnConversao;

type

  { Declaracao }

  Declaracao = class(TCollectionItem)
  private
    FDCe: TDCe;
    FDCeW: TDCeXmlWriter;
    FDCeR: TDCeXmlReader;

    FConfiguracoes: TConfiguracoesDCe;
    FXMLAssinado: String;
    FXMLOriginal: String;
    FAlertas: String;
    FErroValidacao: String;
    FErroValidacaoCompleto: String;
    FErroRegrasdeNegocios: String;
    FNomeArq: String;

    function GetConfirmado: Boolean;
    function GetProcessado: Boolean;
    function GetCancelado: Boolean;

    function GetMsg: String;
    function GetNumID: String;
    function GetXMLAssinado: String;
    procedure SetXML(const AValue: String);
    procedure SetXMLOriginal(const AValue: String);
    function ValidarConcatChave: Boolean;
    function CalcularNomeArquivo: String;
    function CalcularPathArquivo: String;
    function GetcStat: Integer;
  public
    constructor Create(Collection2: TCollection); override;
    destructor Destroy; override;
    procedure Imprimir;
    procedure ImprimirPDF;

    procedure Assinar;
    procedure Validar;
    function VerificarAssinatura: Boolean;
    function ValidarRegrasdeNegocios: Boolean;

    function LerXML(const AXML: String): Boolean;
    function LerArqIni(const AIniString: String): Boolean;
    function GerarDCeIni: String;

    function GerarXML: String;
    function GravarXML(const NomeArquivo: String = ''; const PathArquivo: String = ''): Boolean;

    function GravarStream(AStream: TStream): Boolean;

    procedure EnviarEmail(const sPara, sAssunto: String; sMensagem: TStrings = nil;
      EnviaPDF: Boolean = True; sCC: TStrings = nil; Anexos: TStrings = nil;
      sReplyTo: TStrings = nil);

    function CalcularNomeArquivoCompleto(NomeArquivo: String = '';
      PathArquivo: String = ''): String;

    property NomeArq: String read FNomeArq write FNomeArq;

    property DCe: TDCe read FDCe;

    // Atribuir a "XML", faz o componente transferir os dados lido para as propriedades internas e "XMLAssinado"
    property XML: String         read FXMLOriginal   write SetXML;
    // Atribuir a "XMLOriginal", reflete em XMLAssinado, se existir a tag de assinatura
    property XMLOriginal: String read FXMLOriginal   write SetXMLOriginal;
    property XMLAssinado: String read GetXMLAssinado write FXMLAssinado;

    property Confirmado: Boolean read GetConfirmado;
    property Processado: Boolean read GetProcessado;
    property Cancelado: Boolean  read GetCancelado;
    property cStat: Integer read GetcStat;
    property Msg: String read GetMsg;
    property NumID: String read GetNumID;

    property Alertas: String read FAlertas;
    property ErroValidacao: String read FErroValidacao;
    property ErroValidacaoCompleto: String read FErroValidacaoCompleto;
    property ErroRegrasdeNegocios: String read FErroRegrasdeNegocios;
  end;

  { TDeclaracoes }

  TDeclaracoes = class(TOwnedCollection)
  private
    FACBrDCe: TComponent;
    FConfiguracoes: TConfiguracoesDCe;

    function GetItem(Index: integer): Declaracao;
    procedure SetItem(Index: integer; const Value: Declaracao);

    procedure VerificarDADCe;
  public
    constructor Create(AOwner: TPersistent; ItemClass: TCollectionItemClass);

    procedure GerarDCe;
    procedure Assinar;
    procedure Validar;
    function VerificarAssinatura(out Erros: String): Boolean;
    function ValidarRegrasdeNegocios(out Erros: String): Boolean;
    procedure Imprimir;
    procedure ImprimirPDF;

    function Add: Declaracao;
    function Insert(Index: integer): Declaracao;

    property Items[Index: integer]: Declaracao read GetItem write SetItem; default;

    function GetNamePath: String; override;
    // Incluido o Parametro AGerarDCe que determina se após carregar os dados do DCe
    // para o componente, será gerado ou não novamente o XML do DCe.
    function LoadFromFile(const CaminhoArquivo: String; AGerarDCe: Boolean = False): Boolean;
    function LoadFromStream(AStream: TStringStream; AGerarDCe: Boolean = False): Boolean;
    function LoadFromString(const AXMLString: String; AGerarDCe: Boolean = False): Boolean;
    function LoadFromIni(const AIniString: String): Boolean;

    function GerarIni: String;
    function GravarXML(const PathNomeArquivo: String = ''): Boolean;

    property ACBrDCe: TComponent read FACBrDCe;
  end;

implementation

uses
  Dateutils, 
	IniFiles,
  synautil, 
  ACBrUtil.Base,
  ACBrUtil.Strings,
  ACBrUtil.XMLHTML,
  ACBrUtil.FilesIO,
  ACBrUtil.DateTime,
	ACBrXmlBase,
  ACBrXmlDocument,
  ACBrDCe,
	ACBrDFeUtil,
	ACBrDCe.Conversao;

{ Declaracao }

constructor Declaracao.Create(Collection2: TCollection);
begin
  inherited Create(Collection2);

  FDCe := TDCe.Create;
  FDCeW := TDCeXmlWriter.Create(FDCe);
  FDCeR := TDCeXmlReader.Create(FDCe);
  FConfiguracoes := TACBrDCe(TDeclaracoes(Collection).ACBrDCe).Configuracoes;

  FDCe.Ide.verProc := 'ACBrDCe';

  with TACBrDCe(TDeclaracoes(Collection).ACBrDCe) do
  begin
    FDCe.Ide.modelo := 99;
    FDCe.infDCe.Versao := VersaoDCeToDbl(Configuracoes.Geral.VersaoDF);
    FDCe.Ide.tpAmb := TACBrTipoAmbiente(Integer(Configuracoes.WebServices.Ambiente));
    FDCe.Ide.tpEmis := TACBrTipoEmissao(Integer(Configuracoes.Geral.FormaEmissao));
  end;
end;

destructor Declaracao.Destroy;
begin
  FDCeW.Free;
  FDCeR.Free;
  FDCe.Free;

  inherited Destroy;
end;

procedure Declaracao.Imprimir;
begin
  with TACBrDCe(TDeclaracoes(Collection).ACBrDCe) do
  begin
    if not Assigned(DACE) then
      raise EACBrDCeException.Create('Componente DACE não associado.')
    else
      DACE.ImprimirDACE(DCe);
  end;
end;

procedure Declaracao.ImprimirPDF;
begin
  with TACBrDCe(TDeclaracoes(Collection).ACBrDCe) do
  begin
    if not Assigned(DACE) then
      raise EACBrDCeException.Create('Componente DACE não associado.')
    else
      DACE.ImprimirDACEPDF(DCe);
  end;
end;

procedure Declaracao.Assinar;
var
  XMLStr: String;
  XMLUTF8: AnsiString;
  Document: TACBrXmlDocument;
  ANode: TACBrXmlNode;
begin
  with TACBrDCe(TDeclaracoes(Collection).ACBrDCe) do
  begin
    if not Assigned(SSL.AntesDeAssinar) then
      SSL.ValidarCNPJCertificado(DCe.Emit.CNPJCPF);
  end;

  // Gera novamente, para processar propriedades que podem ter sido modificadas
  XMLStr := GerarXML;

  // XML já deve estar em UTF8, para poder ser assinado //
  XMLUTF8 := ConverteXMLtoUTF8(XMLStr);

  with TACBrDCe(TDeclaracoes(Collection).ACBrDCe) do
  begin
    FXMLAssinado := SSL.Assinar(String(XMLUTF8), 'DCe', 'infDCe');
    // SSL.Assinar() sempre responde em UTF8...
    FXMLOriginal := FXMLAssinado;

    Document := TACBrXmlDocument.Create;
    try
      Document.LoadFromXml(FXMLOriginal);
      ANode := Document.Root;

      LerSignature(ANode.Childrens.FindAnyNs('Signature'), DCe.signature);
    finally
      FreeAndNil(Document);
    end;

    // Gera o QR-Code para adicionar no XML após ter a
    // assinatura, e antes de ser salvo.

    with TACBrDCe(TDeclaracoes(Collection).ACBrDCe) do
    begin
      if DCe.emit.idOutros <> '' then
        DCe.infDCeSupl.qrCode := GetURLQRCode(DCe.Ide.cUF,
          TpcnTipoAmbiente(Integer(DCe.Ide.tpAmb)),
          TpcnTipoEmissao(Integer(DCe.ide.tpEmis)), DCe.infDCe.ID, DCe.emit.idOutros,
          'O', DCe.infDCe.Versao)
      else
      begin
        if Length(DCe.emit.CNPJCPF) = 14 then
          DCe.infDCeSupl.qrCode := GetURLQRCode(DCe.Ide.cUF,
            TpcnTipoAmbiente(Integer(DCe.Ide.tpAmb)),
            TpcnTipoEmissao(Integer(DCe.ide.tpEmis)), DCe.infDCe.ID, DCe.emit.CNPJCPF,
            'J', DCe.infDCe.Versao)
        else
          DCe.infDCeSupl.qrCode := GetURLQRCode(DCe.Ide.cUF,
            TpcnTipoAmbiente(Integer(DCe.Ide.tpAmb)),
            TpcnTipoEmissao(Integer(DCe.ide.tpEmis)), DCe.infDCe.ID, DCe.emit.CNPJCPF,
            'F', DCe.infDCe.Versao);
      end;

      DCe.infDCeSupl.urlChave := GetURLConsulta(DCe.Ide.cUF,
                   TpcnTipoAmbiente(Integer(DCe.Ide.tpAmb)), DCe.infDCe.Versao);

      GerarXML;
    end;

    if Configuracoes.Arquivos.Salvar and
      (not Configuracoes.Arquivos.SalvarApenasDCeProcessados) then
    begin
      if NaoEstaVazio(NomeArq) then
        Gravar(NomeArq, FXMLAssinado)
      else
        Gravar(CalcularNomeArquivoCompleto(), FXMLAssinado);
    end;
  end;
end;

procedure Declaracao.Validar;
var
  Erro, AXML, AXMLModal, TagModal: String;
  DCeEhValida, ModalEhValido: Boolean;
  ALayout: TLayOutDCe;
begin
  AXML := FXMLAssinado;

  if AXML = '' then
    AXML := XMLOriginal;

  with TACBrDCe(TDeclaracoes(Collection).ACBrDCe) do
  begin
    ALayout := LayDCeRecepcao;

    // Extraindo apenas os dados da DCe (sem DCeProc)
    AXML := ObterDFeXML(AXML, 'DCe', ACBRDCe_NAMESPACE);

    if EstaVazio(AXML) then
    begin
      Erro := ACBrStr('DCe não encontrada no XML');
      DCeEhValida := False;
    end
    else
    begin
      DCeEhValida := SSL.Validar(AXML, GerarNomeArqSchema(ALayout, FDCe.infDCe.Versao), Erro);
    end;

    if not DCeEhValida then
    begin
      FErroValidacao := ACBrStr('Falha na validação dos dados do Declaracao: ') +
        IntToStr(DCe.Ide.nDC) + sLineBreak + FAlertas ;
      FErroValidacaoCompleto := FErroValidacao + sLineBreak + Erro;

      raise EACBrDCeException.CreateDef(
        IfThen(Configuracoes.Geral.ExibirErroSchema, ErroValidacaoCompleto,
        ErroValidacao));
    end;
  end;
end;

function Declaracao.VerificarAssinatura: Boolean;
var
  Erro, AXML: String;
  AssEhValida: Boolean;
begin
  AXML := XMLAssinado;

  with TACBrDCe(TDeclaracoes(Collection).ACBrDCe) do
  begin
    // Extraindo apenas os dados da DCe (sem DCeProc)
    AXML := ObterDFeXML(AXML, 'DCe', ACBRDCe_NAMESPACE);

    if EstaVazio(AXML) then
    begin
      Erro := ACBrStr('DCe não encontrada no XML');
      AssEhValida := False;
    end
    else
      AssEhValida := SSL.VerificarAssinatura(AXML, Erro, 'infDCe');

    if not AssEhValida then
    begin
      FErroValidacao := ACBrStr('Falha na validação da assinatura do Declaracao: ') +
        IntToStr(DCe.Ide.nDC) + sLineBreak + Erro;
    end;
  end;

  Result := AssEhValida;
end;

function Declaracao.ValidarRegrasdeNegocios: Boolean;
var
  Erros{, Log}: String;
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

  with TACBrDCe(TDeclaracoes(Collection).ACBrDCe) do
  begin
    Erros := '';

    GravaLog('Validar: 897-Código do documento: ' + IntToStr(DCe.Ide.nDC));
    if not ValidarCodigoDFe(DCe.Ide.cDC, DCe.Ide.nDC) then
      AdicionaErro('897-Rejeição: Código numérico em formato inválido ');

    GravaLog('Regra: G001 - Validar: 252-Ambiente');
    if (Integer(DCe.Ide.tpAmb) <> Integer(Configuracoes.WebServices.Ambiente)) then
      AdicionaErro('252-Rejeição: Tipo do ambiente do MDF-e difere do ambiente do Web Service');

    GravaLog('Regra: G002 - Validar 226-UF');
    if copy(IntToStr(DCe.Emit.EnderEmit.cMun), 1, 2) <> IntToStr(Configuracoes.WebServices.UFCodigo) then
      AdicionaErro('226-Rejeição: Código da UF do Emitente diverge da UF autorizadora');

    GravaLog('Regra: G003 - Validar 247-UF');
    if DCe.Emit.EnderEmit.UF <> Configuracoes.WebServices.UF then
      AdicionaErro('247-Rejeição: Sigla da UF do Emitente difere da UF do Web Service');

    GravaLog('Regra: G004 - Validar: 227-Chave de acesso');
    if not ValidarConcatChave then
      AdicionaErro('227-Rejeição: Chave de Acesso do Campo Id difere da concatenação dos campos correspondentes');

    GravaLog('Regra: G005 - Validar: 666-Ano da Chave');
    if Copy(DCe.infDCe.ID, 7, 2) < '12' then
      AdicionaErro('666-Rejeição: Ano da chave de acesso é inferior a 2012');

    // *************************************************************************
    // No total são 93 regras de validação, portanto faltam muitas para serem
    // acrescentadas nessa rotina.
  end;

  Result := EstaVazio(Erros);

  if not Result then
  begin
    Erros := ACBrStr('Erro(s) nas Regras de negócios do Declaracao: '+
                     IntToStr(DCe.Ide.nDC) + sLineBreak + Erros);
  end;

  GravaLog('Fim da Validação. Tempo: ' +
           FormatDateTime('hh:nn:ss:zzz', Now - Agora) + sLineBreak +
           'Erros:' + Erros);

  FErroRegrasdeNegocios := Erros;
end;

function Declaracao.LerXML(const AXML: String): Boolean;
var
  XMLStr: String;
begin
  XMLOriginal := AXML;  // SetXMLOriginal() irá verificar se AXML está em UTF8

  FDCeR.Arquivo := XMLOriginal;
  FDCeR.LerXml;

  Result := True;
end;

function Declaracao.GravarXML(const NomeArquivo: String; const PathArquivo: String): Boolean;
begin
  if EstaVazio(FXMLOriginal) then
    GerarXML;

  FNomeArq := CalcularNomeArquivoCompleto(NomeArquivo, PathArquivo);

  Result := TACBrDCe(TDeclaracoes(Collection).ACBrDCe).Gravar(FNomeArq, FXMLOriginal);
end;

function Declaracao.GravarStream(AStream: TStream): Boolean;
begin
  if EstaVazio(FXMLOriginal) then
    GerarXML;

  AStream.Size := 0;
  WriteStrToStream(AStream, AnsiString(FXMLOriginal));
  Result := True;
end;

procedure Declaracao.EnviarEmail(const sPara, sAssunto: String; sMensagem: TStrings;
  EnviaPDF: Boolean; sCC: TStrings; Anexos: TStrings; sReplyTo: TStrings);
var
  NomeArqTemp : String;
  AnexosEmail:TStrings;
  StreamDCe : TMemoryStream;
begin
  if not Assigned(TACBrDCe(TDeclaracoes(Collection).ACBrDCe).MAIL) then
    raise EACBrDCeException.Create('Componente ACBrMail não associado');

  AnexosEmail := TStringList.Create;
  StreamDCe := TMemoryStream.Create;
  try
    AnexosEmail.Clear;
    if Assigned(Anexos) then
      AnexosEmail.Assign(Anexos);

    with TACBrDCe(TDeclaracoes(Collection).ACBrDCe) do
    begin
      Self.GravarStream(StreamDCe);

      if (EnviaPDF) then
      begin
        if Assigned(DACE) then
        begin
          DACE.ImprimirDACEPDF(FDCe);
          NomeArqTemp := PathWithDelim(DACE.PathPDF) + NumID + '-DCe.pdf';
          AnexosEmail.Add(NomeArqTemp);
        end;
      end;

      EnviarEmail( sPara, sAssunto, sMensagem, sCC, AnexosEmail, StreamDCe,
                   NumID + '-DCe.xml', sReplyTo);
    end;
  finally
    AnexosEmail.Free;
    StreamDCe.Free;
  end;
end;

function Declaracao.GerarDCeIni: String;
var
  i, j, k, l, m: integer;
  sSecao: string;
  INIRec: TMemIniFile;
  IniDFe: TStringList;
begin
  Result := '';

  if not ValidarChave(DCe.infDCe.ID) then
    raise EACBrDCeException.Create('DCe Inconsistente para gerar INI. Chave Inválida.');

  INIRec := TMemIniFile.Create('');
  try
    (*
    with FDCe do
    begin
      INIRec.WriteInteger('ide', 'cUF', Ide.cUF);
      INIRec.WriteString('ide', 'tpEmit', TpEmitenteToStr(Ide.tpEmit));
      INIRec.WriteString('ide', 'tpTransp', TTransportadorToStr(Ide.tpTransp));
      INIRec.WriteString('ide', 'mod', Ide.modelo);
      INIRec.WriteInteger('ide', 'serie', Ide.serie);
      INIRec.WriteInteger('ide', 'nMDF', Ide.nMDF);
      INIRec.WriteInteger('ide', 'cMDF', Ide.cMDF);
      INIRec.WriteString('ide', 'modal', ModalToStr(Ide.modal));
      INIRec.WriteString('ide', 'dhEmi', DateToStr(Ide.dhEmi));
      INIRec.WriteString('ide', 'tpEmis', TpEmisToStr(Ide.tpEmis));
      INIRec.WriteString('ide', 'procEmi', procEmiToStr(Ide.procEmi));
      INIRec.WriteString('ide', 'verProc', Ide.verProc);
      INIRec.WriteString('ide', 'UFIni', Ide.UFIni);
      INIRec.WriteString('ide', 'UFFim', Ide.UFFim);
      INIRec.WriteString('ide', 'dhIniViagem', DateToStr(Ide.dhIniViagem));
      INIRec.WriteString('ide', 'tpTransp', TTransportadorToStr(Ide.tpTransp));
      INIRec.WriteString('ide', 'indCanalVerde', TindicadorToStr(Ide.indCanalVerde));
      INIRec.WriteString('ide', 'indCarregaPosterior', TindicadorToStr(Ide.indCarregaPosterior));

      for i := 0 to ide.infMunCarrega.Count - 1 do
      begin
        sSecao := 'CARR' + IntToStrZero(I + 1, 3);
        with ide.infMunCarrega.Items[i] do
        begin
          INIRec.WriteInteger(sSecao, 'cMunCarrega', cMunCarrega);
          INIRec.WriteString(sSecao, 'xMunCarrega', xMunCarrega);
        end;
      end;

      for i := 0 to ide.infPercurso.Count - 1 do
      begin
        sSecao := 'PERC' + IntToStrZero(I + 1, 3);
        with ide.infPercurso.Items[i] do
        begin
          INIRec.WriteString(sSecao, 'UFPer', UFPer);
        end;
      end;

      INIRec.WriteString('emit', 'CNPJ', Emit.CNPJCPF);
      INIRec.WriteString('emit', 'IE', Emit.IE);
      INIRec.WriteString('emit', 'xNome', Emit.xNome);
      INIRec.WriteString('emit', 'xFant', Emit.xFant);
      INIRec.WriteString('emit', 'xLgr', Emit.enderEmit.xLgr);
      INIRec.WriteString('emit', 'nro', Emit.enderEmit.nro);
      INIRec.WriteString('emit', 'xCpl', Emit.enderEmit.xCpl);
      INIRec.WriteString('emit', 'xBairro', Emit.enderEmit.xBairro);
      INIRec.WriteInteger('emit', 'cMun', Emit.enderEmit.cMun);
      INIRec.WriteString('emit', 'xMun', Emit.enderEmit.xMun);
      INIRec.WriteInteger('emit', 'CEP', Emit.enderEmit.CEP);
      INIRec.WriteString('emit', 'UF', Emit.enderEmit.UF);
      INIRec.WriteString('emit', 'fone', Emit.enderEmit.fone);
      INIRec.WriteString('emit', 'email', Emit.enderEmit.email);

      case Ide.modal of
        moRodoviario:
        begin
          INIRec.WriteString('Rodo', 'codAgPorto', Rodo.codAgPorto);

          INIRec.WriteString('infANTT', 'RNTRC', Rodo.infANTT.RNTRC);

          for i := 0 to rodo.infANTT.infCIOT.Count - 1 do
          begin
            sSecao := 'infCIOT' + IntToStrZero(I + 1, 3);
            with rodo.infANTT.infCIOT.Items[i] do
            begin
              INIRec.WriteString(sSecao, 'CIOT', CIOT);
              INIRec.WriteString(sSecao, 'CNPJCPF', CNPJCPF);
            end;

            INIRec.WriteString(sSecao, 'categCombVeic', categCombVeicToStr(Rodo.infANTT.valePed.categCombVeic));
          end;

          for i := 0 to rodo.infANTT.valePed.disp.Count - 1 do
          begin
            sSecao := 'valePed' + IntToStrZero(I + 1, 3);
            with rodo.infANTT.valePed.disp.Items[i] do
            begin
              INIRec.WriteString(sSecao, 'CNPJForn', CNPJForn);
              INIRec.WriteString(sSecao, 'CNPJPg', CNPJPg);
              INIRec.WriteString(sSecao, 'nCompra', nCompra);
              INIRec.WriteFloat(sSecao, 'vValePed', vValePed);
              INIRec.WriteString(sSecao, 'tpValePed', tpValePedToStr(tpValePed));
            end;
          end;

          for i := 0 to rodo.infANTT.infContratante.Count - 1 do
          begin
            sSecao := 'infContratante' + IntToStrZero(I + 1, 3);
            with rodo.infANTT.infContratante.Items[i] do
            begin
              INIRec.WriteString(sSecao, 'CNPJCPF', CNPJCPF);
              INIRec.WriteString(sSecao, 'idEstrangeiro', idEstrangeiro);
              INIRec.WriteString(sSecao, 'xNome', xNome);
            end;
          end;

          for i := 0 to rodo.infANTT.infPag.Count - 1 do
          begin
            sSecao := 'infPag' + IntToStrZero(I + 1, 3);
            with rodo.infANTT.infPag.Items[i] do
            begin
              INIRec.WriteString(sSecao, 'xNome', xNome);
              INIRec.WriteString(sSecao, 'idEstrangeiro', idEstrangeiro);
              INIRec.WriteString(sSecao, 'CNPJCPF', CNPJCPF);
              INIRec.WriteFloat(sSecao, 'vContrato', vContrato);
              INIRec.WriteString(sSecao, 'indAltoDesemp', indAltoDesempToStr(indAltoDesemp));
              INIRec.WriteString(sSecao, 'indPag', TindPagToStr(indPag));
              INIRec.WriteFloat(sSecao, 'vAdiant', vAdiant);

              for j := 0 to rodo.infANTT.infPag[I].Comp.Count - 1 do
              begin
                sSecao := 'Comp' + IntToStrZero(I + 1, 3) + IntToStrZero(J + 1, 3);
                with rodo.infANTT.infPag.Items[i].Comp.Items[j] do
                begin
                  INIRec.WriteString(sSecao, 'tpComp', TCompToStr(tpComp));
                  INIRec.WriteFloat(sSecao, 'vComp', vComp);
                  INIRec.WriteString(sSecao, 'xComp', xComp);
                end;
              end;

              for j := 0 to rodo.infANTT.infPag[I].infPrazo.Count - 1 do
              begin
                sSecao := 'infPrazo' + IntToStrZero(I + 1, 3) + IntToStrZero(J + 1, 3);
                with rodo.infANTT.infPag.Items[i].infPrazo.Items[j] do
                begin
                  INIRec.WriteInteger(sSecao, 'nParcela', nParcela);
                  INIRec.WriteString(sSecao, 'dVenc', DateToStr(dVenc));
                  INIRec.WriteFloat(sSecao, 'vParcela', vParcela);
                end;
              end;

              sSecao := 'infBanc' + IntToStrZero(I, 3);
              with rodo.infANTT.infPag.Items[i].infBanc do
              begin
                INIRec.WriteString(sSecao, 'PIX', PIX);
                INIRec.WriteString(sSecao, 'CNPJIPEF', CNPJIPEF);
                INIRec.WriteString(sSecao, 'codBanco', codBanco);
                INIRec.WriteString(sSecao, 'codAgencia', codAgencia);
              end;
            end;
          end;

          if (Rodo.veicTracao.placa <> '') then
          begin
            INIRec.WriteString('veicTracao', 'clInt', Rodo.veicTracao.cInt);
            INIRec.WriteString('veicTracao', 'placa', Rodo.veicTracao.placa);
            INIRec.WriteString('veicTracao', 'RENAVAN', Rodo.veicTracao.RENAVAM);
            INIRec.WriteInteger('veicTracao', 'tara', Rodo.veicTracao.tara);
            INIRec.WriteInteger('veicTracao', 'capKG', Rodo.veicTracao.capKG);
            INIRec.WriteInteger('veicTracao', 'capM3', Rodo.veicTracao.capM3);
            INIRec.WriteString('veicTracao', 'tpRod', TpRodadoToStr(Rodo.veicTracao.tpRod));
            INIRec.WriteString('veicTracao', 'tpCar', TpCarroceriaToStr(Rodo.veicTracao.tpCar));
            INIRec.WriteString('veicTracao', 'UF', Rodo.veicTracao.UF);
            // Dados do proprietário do veículo de Tração (Opcional) - Nível 2
            INIRec.WriteString('veicTracao', 'CNPJCPF', Rodo.veicTracao.prop.CNPJCPF);
            INIRec.WriteString('veicTracao', 'RNTRC', Rodo.veicTracao.prop.RNTRC);
            INIRec.WriteString('veicTracao', 'xNome', Rodo.veicTracao.prop.xNome);
            INIRec.WriteString('veicTracao', 'IE', Rodo.veicTracao.prop.IE);
            INIRec.WriteString('veicTracao', 'UFProp', Rodo.veicTracao.prop.UF);
            INIRec.WriteString('veicTracao', 'tpProp', TpPropToStr(Rodo.veicTracao.prop.tpProp));
          end;

          for i := 0 to Rodo.veicTracao.condutor.Count - 1 do
          begin
            sSecao := 'moto' + IntToStrZero(i + 1, 3);
            with rodo.veicTracao.condutor.Items[i] do
            begin
              IniRec.WriteString(sSecao, 'CPF', CPF);
              IniRec.WriteString(sSecao, 'xNome', xNome);
            end;
          end;

          for i := 0 to Rodo.veicReboque.Count - 1 do
          begin
            sSecao := 'reboque' + IntToStrZero(i + 1, 2);
            with rodo.veicReboque.Items[i] do
            begin
              INIRec.WriteString(sSecao, 'clInt', cInt);
              INIRec.WriteString(sSecao, 'placa', placa);
              INIRec.WriteString(sSecao, 'RENAVAN', RENAVAM);
              INIRec.WriteInteger(sSecao, 'tara', tara);
              INIRec.WriteInteger(sSecao, 'capKG', capKG);
              INIRec.WriteInteger(sSecao, 'capM3', capM3);
              INIRec.WriteString(sSecao, 'tpCar', TpCarroceriaToStr(tpCar));
              INIRec.WriteString(sSecao, 'UF', UF);
              // Dados do proprietário do veículo de Tração (Opcional) - Nível 2
              INIRec.WriteString(sSecao, 'CNPJCPF', prop.CNPJCPF);
              INIRec.WriteString(sSecao, 'RNTRC', prop.RNTRC);
              INIRec.WriteString(sSecao, 'xNome', prop.xNome);
              INIRec.WriteString(sSecao, 'IE', prop.IE);
              INIRec.WriteString(sSecao, 'UFProp', prop.UF);
              INIRec.WriteString(sSecao, 'tpProp', TpPropToStr(prop.tpProp));
            end;
          end;

          for i := 0 to Rodo.lacRodo.Count - 1 do
          begin
            sSecao := 'lacRodo' + IntToStrZero(i + 1, 3);
            with rodo.lacRodo.Items[i] do
            begin
              INIRec.WriteString(sSecao, 'nLacre', nLacre);
            end;
          end;
        end;

        moAereo:
        begin
          sSecao := 'aereo';
          INIRec.WriteString(sSecao, 'matr', Aereo.matr);
          INIRec.WriteString(sSecao, 'nVoo', Aereo.nVoo);
          INIRec.WriteString(sSecao, 'cAerEmb', Aereo.cAerEmb);
          INIRec.WriteString(sSecao, 'cAerDes', Aereo.cAerDes);
          INIRec.WriteString(sSecao, 'dVoo', DateToStr(Aereo.dVoo));
        end;

        moAquaviario:
        begin
          sSecao := 'aquav';
          INIRec.WriteString(sSecao, 'CNPJAgeNav', Aquav.CNPJAgeNav);
          INIRec.WriteString(sSecao, 'irin', Aquav.irin);
          INIRec.WriteString(sSecao, 'tpEmb', Aquav.tpEmb);
          INIRec.WriteString(sSecao, 'cEmbar', Aquav.cEmbar);
          INIRec.WriteString(sSecao, 'xEmbar', Aquav.xEmbar);
          INIRec.WriteString(sSecao, 'cPrtEmb', Aquav.cPrtEmb);
          INIRec.WriteString(sSecao, 'cPrtDest', Aquav.cPrtDest);
          INIRec.WriteString(sSecao, 'prtTrans', Aquav.prtTrans);
          INIRec.WriteString(sSecao, 'tpNav', TpNavegacaoToStr(Aquav.tpNav));

          for i := 0 to Aquav.infTermCarreg.Count - 1 do
          begin
            sSecao := 'infTermCarreg' + IntToStrZero(i + 1, 1);
            with Aquav.infTermCarreg.Items[i] do
            begin
              INIRec.WriteString(sSecao, 'cTermCarreg', cTermCarreg);
              INIRec.WriteString(sSecao, 'xTermCarreg', xTermCarreg);
            end;
          end;

          for i := 0 to Aquav.infTermDescarreg.Count - 1 do
          begin
            sSecao := 'infTermDescarreg' + IntToStrZero(i + 1, 1);
            with Aquav.infTermDescarreg.Items[i] do
            begin
              INIRec.WriteString(sSecao, 'cTermDescarreg', cTermDescarreg);
              INIRec.WriteString(sSecao, 'xTermDescarreg', xTermDescarreg);
            end;
          end;

          for i := 0 to Aquav.infEmbComb.Count - 1 do
          begin
            sSecao := 'infEmbComb' + IntToStrZero(i + 1, 2);
            with Aquav.infEmbComb.Items[i] do
            begin
              INIRec.WriteString(sSecao, 'cEmbComb', cEmbComb);
              INIRec.WriteString(sSecao, 'xBalsa', xBalsa);
            end;
          end;

          for i := 0 to Aquav.infUnidCargaVazia.Count - 1 do
          begin
            sSecao := 'infUnidCargaVazia' + IntToStrZero(i + 1, 3);
            with Aquav.infUnidCargaVazia.Items[i] do
            begin
              INIRec.WriteString(sSecao, 'idUnidCargaVazia', idUnidCargaVazia);
              INIRec.WriteString(sSecao, 'tpUnidCargaVazia', UnidCargaToStr(tpUnidCargaVazia));
            end;
          end;

          for i := 0 to Aquav.infUnidTranspVazia.Count - 1 do
          begin
            sSecao := 'infUnidTranspVazia' + IntToStrZero(i + 1, 3);
            with Aquav.infUnidTranspVazia.Items[i] do
            begin
              INIRec.WriteString(sSecao, 'idUnidTranspVazia', idUnidTranspVazia);
              INIRec.WriteString(sSecao, 'tpUnidTranspVazia', UnidTranspToStr(tpUnidTranspVazia));
            end;
          end;
        end;

        moFerroviario:
        begin
          sSecao := 'ferrov';
          INIRec.WriteString(sSecao, 'xPref', Ferrov.xPref);
          INIRec.WriteString(sSecao, 'dhTrem', DateToStr(Ferrov.dhTrem));
          INIRec.WriteString(sSecao, 'xOri', Ferrov.xOri);
          INIRec.WriteString(sSecao, 'xDest', Ferrov.xDest);
          INIRec.WriteInteger(sSecao, 'qVag', Ferrov.qVag);

          for i := 0 to Ferrov.vag.Count - 1 do
          begin
            sSecao := 'vag' + IntToStrZero(i + 1, 3);
            with Ferrov.vag.Items[i] do
            begin
              INIRec.WriteString(sSecao, 'serie', serie);
              INIRec.WriteInteger(sSecao, 'nVag', nVag);
              INIRec.WriteInteger(sSecao, 'nSeq', nSeq);
              INIRec.WriteFloat(sSecao, 'TU', TU);
              INIRec.WriteFloat(sSecao, 'pesoBC', pesoBC);
              INIRec.WriteFloat(sSecao, 'pesoR', pesoR);
              INIRec.WriteString(sSecao, 'tpVag', tpVag);
            end;
          end;
        end;
      end;

      for i := 0 to infDoc.infMunDescarga.Count - 1 do
      begin
        sSecao := 'DESC' + IntToStrZero(I + 1, 3);
        with infDoc.infMunDescarga.Items[i] do
        begin
          INIRec.WriteInteger(sSecao, 'cMunDescarga', cMunDescarga);
          INIRec.WriteString(sSecao, 'xMunDescarga', xMunDescarga);

          for j := 0 to infCTe.Count - 1 do
          begin
            sSecao := 'infCTe' + IntToStrZero(I + 1, 3) + IntToStrZero(j + 1, 3);
            with infCTe.Items[j] do
            begin
              INIRec.WriteString(sSecao, 'chCTe', chCTe);
              INIRec.WriteString(sSecao, 'SegCodBarra', SegCodBarra);
              INIRec.WriteString(sSecao, 'indReentrega', indReentrega);

              for k := 0 to peri.Count - 1 do
              begin
                sSecao := 'peri' + IntToStrZero(I + 1, 3) +
                                IntToStrZero(j + 1, 3) + IntToStrZero(k + 1, 3);
                with peri.Items[k] do
                begin
                  INIRec.WriteString(sSecao, 'nONU', nONU);
                  INIRec.WriteString(sSecao, 'xNomeAE', xNomeAE);
                  INIRec.WriteString(sSecao, 'xClaRisco', xClaRisco);
                  INIRec.WriteString(sSecao, 'grEmb', grEmb);
                  INIRec.WriteString(sSecao, 'qTotProd', qTotProd);
                  INIRec.WriteString(sSecao, 'qVolTipo', qVolTipo);
                end;
              end;

              sSecao := 'infEntregaParcial' + IntToStrZero(I + 1, 3) + IntToStrZero(j + 1, 3);
              with infEntregaParcial do
              begin
                INIRec.WriteFloat(sSecao, 'qtdTotal', qtdTotal);
                INIRec.WriteFloat(sSecao, 'qtdParcial', qtdParcial);
              end;

              for k := 0 to infUnidTransp.Count - 1 do
              begin
                sSecao := 'infUnidTransp' + IntToStrZero(I + 1, 3) +
                                IntToStrZero(j + 1, 3) + IntToStrZero(k + 1, 3);
                with infUnidTransp.Items[k] do
                begin
                  INIRec.WriteString(sSecao, 'idUnidTransp', idUnidTransp);
                  INIRec.WriteFloat(sSecao, 'qtdRat', qtdRat);
                  INIRec.WriteString(sSecao, 'tpUnidTransp', UnidTranspToStr(tpUnidTransp));

                  for l := 0 to lacUnidTransp.Count - 1 do
                  begin
                    sSecao := 'lacUnidTransp' + IntToStrZero(I + 1, 3) +
                               IntToStrZero(j + 1, 3) + IntToStrZero(k + 1, 3) +
                               IntToStrZero(l + 1, 3);
                    with lacUnidTransp.Items[l] do
                    begin
                      INIRec.WriteString(sSecao, 'nLacre', nLacre);
                    end;
                  end;

                  for l := 0 to infUnidCarga.Count - 1 do
                  begin
                    sSecao := 'infUnidCarga' + IntToStrZero(I + 1, 3) +
                               IntToStrZero(j + 1, 3) + IntToStrZero(k + 1, 3) +
                               IntToStrZero(l + 1, 3);
                    with infUnidCarga.Items[l] do
                    begin
                      INIRec.WriteString(sSecao, 'idUnidCarga', idUnidCarga);
                      INIRec.WriteFloat(sSecao, 'qtdRat', qtdRat);
                      INIRec.WriteString(sSecao, 'tpUnidCarga', UnidCargaToStr(tpUnidCarga));

                      for m := 0 to lacUnidCarga.Count - 1 do
                      begin
                        sSecao := 'lacUnidCarga' + IntToStrZero(I + 1, 3) +
                                   IntToStrZero(j + 1, 3) + IntToStrZero(k + 1, 3) +
                                   IntToStrZero(l + 1, 3) + IntToStrZero(m + 1, 3);
                        with lacUnidCarga.Items[m] do
                        begin
                          INIRec.WriteString(sSecao, 'nLacre', nLacre);
                        end;
                      end;
                    end;
                  end;
                end;
              end;
            end;
          end;

          for j := 0 to infDoc.infMunDescarga.Items[i].infNFe.Count - 1 do
          begin
            sSecao := 'infNFe' + IntToStrZero(I + 1, 3) + IntToStrZero(j + 1, 3);
            with infDoc.infMunDescarga.Items[i].infNFe.Items[j] do
            begin
              INIRec.WriteString(sSecao, 'chNFe', chNFe);
              INIRec.WriteString(sSecao, 'SegCodBarra', SegCodBarra);
              INIRec.WriteString(sSecao, 'indReentrega', indReentrega);

              for k := 0 to peri.Count - 1 do
              begin
                sSecao := 'peri' + IntToStrZero(I + 1, 3) +
                                IntToStrZero(j + 1, 3) + IntToStrZero(k + 1, 3);
                with peri.Items[k] do
                begin
                  INIRec.WriteString(sSecao, 'nONU', nONU);
                  INIRec.WriteString(sSecao, 'xNomeAE', xNomeAE);
                  INIRec.WriteString(sSecao, 'xClaRisco', xClaRisco);
                  INIRec.WriteString(sSecao, 'grEmb', grEmb);
                  INIRec.WriteString(sSecao, 'qTotProd', qTotProd);
                  INIRec.WriteString(sSecao, 'qVolTipo', qVolTipo);
                end;
              end;

              for k := 0 to infUnidTransp.Count - 1 do
              begin
                sSecao := 'infUnidTransp' + IntToStrZero(I + 1, 3) +
                                IntToStrZero(j + 1, 3) + IntToStrZero(k + 1, 3);
                with infUnidTransp.Items[k] do
                begin
                  INIRec.WriteString(sSecao, 'idUnidTransp', idUnidTransp);
                  INIRec.WriteFloat(sSecao, 'qtdRat', qtdRat);
                  INIRec.WriteString(sSecao, 'tpUnidTransp', UnidTranspToStr(tpUnidTransp));

                  for l := 0 to lacUnidTransp.Count - 1 do
                  begin
                    sSecao := 'lacUnidTransp' + IntToStrZero(I + 1, 3) +
                               IntToStrZero(j + 1, 3) + IntToStrZero(k + 1, 3) +
                               IntToStrZero(l + 1, 3);
                    with lacUnidTransp.Items[l] do
                    begin
                      INIRec.WriteString(sSecao, 'nLacre', nLacre);
                    end;
                  end;

                  for l := 0 to infUnidCarga.Count - 1 do
                  begin
                    sSecao := 'infUnidCarga' + IntToStrZero(I + 1, 3) +
                               IntToStrZero(j + 1, 3) + IntToStrZero(k + 1, 3) +
                               IntToStrZero(l + 1, 3);
                    with infUnidCarga.Items[l] do
                    begin
                      INIRec.WriteString(sSecao, 'idUnidCarga', idUnidCarga);
                      INIRec.WriteFloat(sSecao, 'qtdRat', qtdRat);
                      INIRec.WriteString(sSecao, 'tpUnidCarga', UnidCargaToStr(tpUnidCarga));

                      for m := 0 to lacUnidCarga.Count - 1 do
                      begin
                        sSecao := 'lacUnidCarga' + IntToStrZero(I + 1, 3) +
                                   IntToStrZero(j + 1, 3) + IntToStrZero(k + 1, 3) +
                                   IntToStrZero(l + 1, 3) + IntToStrZero(m + 1, 3);
                        with lacUnidCarga.Items[m] do
                        begin
                          INIRec.WriteString(sSecao, 'nLacre', nLacre);
                        end;
                      end;
                    end;
                  end;
                end;
              end;
            end;
          end;

          for j := 0 to infDoc.infMunDescarga.Items[i].infDCeTransp.Count - 1 do
          begin
            sSecao := 'infDCeTransp' + IntToStrZero(I + 1, 3) + IntToStrZero(j + 1, 3);
            with infDoc.infMunDescarga.Items[i].infDCeTransp.Items[j] do
            begin
              INIRec.WriteString(sSecao, 'chDCe', chDCe);
              INIRec.WriteString(sSecao, 'indReentrega', indReentrega);

              for k := 0 to peri.Count - 1 do
              begin
                sSecao := 'peri' + IntToStrZero(I + 1, 3) +
                                IntToStrZero(j + 1, 3) + IntToStrZero(k + 1, 3);
                with peri.Items[k] do
                begin
                  INIRec.WriteString(sSecao, 'nONU', nONU);
                  INIRec.WriteString(sSecao, 'xNomeAE', xNomeAE);
                  INIRec.WriteString(sSecao, 'xClaRisco', xClaRisco);
                  INIRec.WriteString(sSecao, 'grEmb', grEmb);
                  INIRec.WriteString(sSecao, 'qTotProd', qTotProd);
                  INIRec.WriteString(sSecao, 'qVolTipo', qVolTipo);
                end;
              end;

              for k := 0 to infUnidTransp.Count - 1 do
              begin
                sSecao := 'infUnidTransp' + IntToStrZero(I + 1, 3) +
                                IntToStrZero(j + 1, 3) + IntToStrZero(k + 1, 3);
                with infUnidTransp.Items[k] do
                begin
                  INIRec.WriteString(sSecao, 'idUnidTransp', idUnidTransp);
                  INIRec.WriteFloat(sSecao, 'qtdRat', qtdRat);
                  INIRec.WriteString(sSecao, 'tpUnidTransp', UnidTranspToStr(tpUnidTransp));

                  for l := 0 to lacUnidTransp.Count - 1 do
                  begin
                    sSecao := 'lacUnidTransp' + IntToStrZero(I + 1, 3) +
                               IntToStrZero(j + 1, 3) + IntToStrZero(k + 1, 3) +
                               IntToStrZero(l + 1, 3);
                    with lacUnidTransp.Items[l] do
                    begin
                      INIRec.WriteString(sSecao, 'nLacre', nLacre);
                    end;
                  end;

                  for l := 0 to infUnidCarga.Count - 1 do
                  begin
                    sSecao := 'infUnidCarga' + IntToStrZero(I + 1, 3) +
                               IntToStrZero(j + 1, 3) + IntToStrZero(k + 1, 3) +
                               IntToStrZero(l + 1, 3);
                    with infUnidCarga.Items[l] do
                    begin
                      INIRec.WriteString(sSecao, 'idUnidCarga', idUnidCarga);
                      INIRec.WriteFloat(sSecao, 'qtdRat', qtdRat);
                      INIRec.WriteString(sSecao, 'tpUnidCarga', UnidCargaToStr(tpUnidCarga));

                      for m := 0 to lacUnidCarga.Count - 1 do
                      begin
                        sSecao := 'lacUnidCarga' + IntToStrZero(I + 1, 3) +
                                   IntToStrZero(j + 1, 3) + IntToStrZero(k + 1, 3) +
                                   IntToStrZero(l + 1, 3) + IntToStrZero(m + 1, 3);
                        with lacUnidCarga.Items[m] do
                        begin
                          INIRec.WriteString(sSecao, 'nLacre', nLacre);
                        end;
                      end;
                    end;
                  end;
                end;
              end;
            end;
          end;
        end;
      end;

      for i := 0 to seg.Count - 1 do
      begin
        sSecao := 'seg' + IntToStrZero(I + 1, 3);
        with seg.Items[i] do
        begin
          INIRec.WriteString(sSecao, 'respSeg', RspSeguroDCeToStr(respSeg));
          INIRec.WriteString(sSecao, 'CNPJCPF', CNPJCPF);
          INIRec.WriteString(sSecao, 'xSeg', xSeg);
          INIRec.WriteString(sSecao, 'CNPJ', CNPJ);
          INIRec.WriteString(sSecao, 'nApol', nApol);

          for j := 0 to aver.Count - 1 do
          begin
            sSecao := 'aver' + IntToStrZero(I + 1, 3) + IntToStrZero(j + 1, 3);
            with aver.Items[j] do
            begin
              INIRec.WriteString(sSecao, 'nAver', nAver);
            end;
          end;
        end;
      end;

      sSecao := 'prodPred';
      with prodPred do
      begin
        INIRec.WriteString(sSecao, 'tpCarga', TCargaToStr(tpCarga));
        INIRec.WriteString(sSecao, 'xProd', xProd);
        INIRec.WriteString(sSecao, 'cEAN', cEAN);
        INIRec.WriteString(sSecao, 'NCM', NCM);

        sSecao := 'infLocalCarrega';
        with infLocalCarrega do
        begin
          INIRec.WriteInteger(sSecao, 'CEP', CEP);
          INIRec.WriteFloat(sSecao, 'latitude', latitude);
          INIRec.WriteFloat(sSecao, 'longitude', longitude);
        end;

        sSecao := 'infLocalDescarrega';
        with infLocalDescarrega do
        begin
          INIRec.WriteInteger(sSecao, 'CEP', CEP);
          INIRec.WriteFloat(sSecao, 'latitude', latitude);
          INIRec.WriteFloat(sSecao, 'longitude', longitude);
        end;
      end;

      sSecao := 'tot';
      with tot do
      begin
        INIRec.WriteInteger(sSecao, 'qCTe', qCTe);
        INIRec.WriteInteger(sSecao, 'qNFe', qNFe);
        INIRec.WriteInteger(sSecao, 'qDCe', qDCe);
        INIRec.WriteFloat(sSecao, 'vCarga', vCarga);
        INIRec.WriteFloat(sSecao, 'qCarga', qCarga);
        INIRec.WriteString(sSecao, 'cUnid', UnidMedToStr(cUnid));
      end;

      for i := 0 to lacres.Count - 1 do
      begin
        sSecao := 'lacres' + IntToStrZero(I + 1, 3);
        with lacres.Items[i] do
        begin
          INIRec.WriteString(sSecao, 'nLacre', nLacre);
        end;
      end;

      for i := 0 to autXML.Count - 1 do
      begin
        sSecao := 'autXML' + IntToStrZero(I + 1, 2);
        with autXML.Items[i] do
        begin
          INIRec.WriteString(sSecao, 'CNPJCPF', CNPJCPF);
        end;
      end;

      sSecao := 'infAdic';
      with infAdic do
      begin
        INIRec.WriteString(sSecao, 'infAdFisco', infAdFisco);
        INIRec.WriteString(sSecao, 'infCpl', infCpl);
      end;
    end;

    IniDFe := TStringList.Create;
    try
      INIRec.GetStrings(IniDFe);
      Result := StringReplace(IniDFe.Text, sLineBreak + sLineBreak, sLineBreak, [rfReplaceAll]);
    finally
      IniDFe.Free;
    end;
    *)
  finally
    INIRec.Free;
  end;
end;

function Declaracao.GerarXML: String;
var
  IdAnterior : String;
begin
  with TACBrDCe(TDeclaracoes(Collection).ACBrDCe) do
  begin
    IdAnterior := DCe.infDCe.ID;

    FDCeW.Opcoes.FormatoAlerta  := Configuracoes.Geral.FormatoAlerta;
    FDCeW.Opcoes.RetirarAcentos := Configuracoes.Geral.RetirarAcentos;
    FDCeW.Opcoes.RetirarEspacos := Configuracoes.Geral.RetirarEspacos;
    FDCeW.Opcoes.IdentarXML := Configuracoes.Geral.IdentarXML;
    FDCeW.Opcoes.NormatizarMunicipios  := Configuracoes.Arquivos.NormatizarMunicipios;
    FDCeW.Opcoes.PathArquivoMunicipios := Configuracoes.Arquivos.PathArquivoMunicipios;

    TimeZoneConf.Assign( Configuracoes.WebServices.TimeZoneConf );
    {
    FDCeW.ModeloDF := 99;
    FDCeW.VersaoDF := Configuracoes.Geral.VersaoDF;
    FDCeW.tpAmb := TACBrTipoAmbiente(Integer(Configuracoes.WebServices.Ambiente));
    FDCeW.tpEmis := TACBrTipoEmissao(Integer(Configuracoes.Geral.FormaEmissao));
    }
  end;

  FDCeW.GerarXml;

  XMLOriginal := FDCeW.Document.Xml;  // SetXMLOriginal() irá converter para UTF8

  { XML gerado pode ter nova Chave e ID, então devemos calcular novamente o
    nome do arquivo, mantendo o PATH do arquivo carregado }
  if (NaoEstaVazio(FNomeArq) and (IdAnterior <> FDCe.infDCe.ID)) then
    FNomeArq := CalcularNomeArquivoCompleto('', ExtractFilePath(FNomeArq));

  FAlertas := ACBrStr( FDCeW.ListaDeAlertas.Text );
  Result := FXMLOriginal;
end;

function Declaracao.CalcularNomeArquivo: String;
var
  xID: String;
begin
  xID := Self.NumID;

  if EstaVazio(xID) then
    raise EACBrDCeException.Create('ID Inválido. Impossível Salvar XML');

  Result := xID + '-DCe.xml';
end;

function Declaracao.CalcularPathArquivo: String;
var
  Data: TDateTime;
begin
  with TACBrDCe(TDeclaracoes(Collection).ACBrDCe) do
  begin
    if Configuracoes.Arquivos.EmissaoPathDCe then
      Data := FDCe.Ide.dhEmi
    else
      Data := Now;

    Result := PathWithDelim(Configuracoes.Arquivos.GetPathDCe(Data, FDCe.Emit.CNPJCPF, ''));
  end;
end;

function Declaracao.CalcularNomeArquivoCompleto(NomeArquivo: String;
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

function Declaracao.ValidarConcatChave: Boolean;
var
  wAno, wMes, wDia: word;
begin
  DecodeDate(DCe.ide.dhEmi, wAno, wMes, wDia);

  Result := not
    ((Copy(DCe.infDCe.ID, 5, 2) <> IntToStrZero(DCe.Ide.cUF, 2)) or
    (Copy(DCe.infDCe.ID, 7, 2)  <> Copy(FormatFloat('0000', wAno), 3, 2)) or
    (Copy(DCe.infDCe.ID, 9, 2)  <> FormatFloat('00', wMes)) or
    (Copy(DCe.infDCe.ID, 11, 14)<> PadLeft(OnlyNumber(DCe.Emit.CNPJCPF), 14, '0')) or
    (Copy(DCe.infDCe.ID, 25, 2) <> IntToStr(DCe.Ide.modelo)) or
    (Copy(DCe.infDCe.ID, 27, 3) <> IntToStrZero(DCe.Ide.serie, 3)) or
    (Copy(DCe.infDCe.ID, 30, 9) <> IntToStrZero(DCe.Ide.nDC, 9)) or
    (Copy(DCe.infDCe.ID, 39, 1) <> IntToStr(Integer(DCe.Ide.tpEmis))) or
    (Copy(DCe.infDCe.ID, 40, 8) <> IntToStrZero(DCe.Ide.cDC, 8)));
end;

function Declaracao.GetConfirmado: Boolean;
begin
  Result := TACBrDCe(TDeclaracoes(Collection).ACBrDCe).cStatConfirmado(
    FDCe.procDCe.cStat);
end;

function Declaracao.GetcStat: Integer;
begin
  Result := FDCe.procDCe.cStat;
end;

function Declaracao.GetProcessado: Boolean;
begin
  Result := TACBrDCe(TDeclaracoes(Collection).ACBrDCe).cStatProcessado(
    FDCe.procDCe.cStat);
end;

function Declaracao.GetCancelado: Boolean;
begin
  Result := TACBrDCe(TDeclaracoes(Collection).ACBrDCe).cStatCancelado(
    FDCe.procDCe.cStat);
end;

function Declaracao.GetMsg: String;
begin
  Result := FDCe.procDCe.xMotivo;
end;

function Declaracao.GetNumID: String;
begin
  Result := Trim(OnlyNumber(DCe.infDCe.ID));
end;

function Declaracao.GetXMLAssinado: String;
begin
  if EstaVazio(FXMLAssinado) then
    Assinar;

  Result := FXMLAssinado;
end;

procedure Declaracao.SetXML(const AValue: String);
begin
  LerXML(AValue);
end;

procedure Declaracao.SetXMLOriginal(const AValue: String);
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

function Declaracao.LerArqIni(const AIniString: String): Boolean;
var
  I, J, K, L, M: Integer;
  versao, sSecao, sFim: String;
  OK, GerarGrupo: boolean;
  INIRec : TMemIniFile;
//  SL: TStringList;
begin
  Result := False;

  INIRec := TMemIniFile.Create('');
  try
    LerIniArquivoOuString(AIniString, INIRec);
    (*
    with FDCe do
    begin
      OK := True;

      infDCe.versao := StringToFloatDef( INIRec.ReadString('infDCe','versao', VersaoDCeToStr(FConfiguracoes.Geral.VersaoDF)),0) ;
      versao         := FloatToString(infDCe.versao, '.', '#0.00');

      Ide.tpEmit  := StrToTpEmitente(OK, INIRec.ReadString('ide', 'tpEmit', '1'));
      Ide.modelo  := INIRec.ReadString('ide', 'mod', '58');

      FConfiguracoes.Geral.VersaoDF := StrToVersaoDCe(OK, versao);

      Ide.serie    := INIRec.ReadInteger('ide', 'serie', 1);
      Ide.nMDF     := INIRec.ReadInteger('ide', 'nMDF', 0);
      Ide.cMDF     := INIRec.ReadInteger('ide', 'cMDF', 0);
      Ide.modal    := StrToModal(OK, INIRec.ReadString('ide', 'modal', '1'));
      Ide.dhEmi    := StringToDateTime(INIRec.ReadString('ide', 'dhEmi', '0'));
      Ide.tpEmis   := StrToTpEmis(OK, INIRec.ReadString('ide', 'tpEmis', IntToStr(FConfiguracoes.Geral.FormaEmissaoCodigo)));
      Ide.procEmi  := StrToProcEmi(OK, INIRec.ReadString('ide', 'procEmi', '0'));
      Ide.verProc  := INIRec.ReadString('ide', 'verProc', 'ACBrDCe');
      Ide.UFIni    := INIRec.ReadString('ide', 'UFIni', '');
      Ide.UFFim    := INIRec.ReadString('ide', 'UFFim', '');
      Ide.tpTransp := StrToTTransportador(OK, INIRec.ReadString('ide', 'tpTransp', '1'));

      I := 1;
      while true do
      begin
        sSecao := 'CARR' + IntToStrZero(I, 3);
        sFim   := INIRec.ReadString(sSecao, 'xMunCarrega', 'FIM');

        if (sFim = 'FIM') or (Length(sFim) <= 0) then
          break;

        with Ide.infMunCarrega.New do
        begin
          cMunCarrega := INIRec.ReadInteger(sSecao, 'cMunCarrega', 0);
          xMunCarrega := sFim;
        end;

        Inc(I);
      end;

      I := 1;
      while true do
      begin
        sSecao := 'PERC' + IntToStrZero(I, 3);
        sFim   := INIRec.ReadString(sSecao, 'UFPer', 'FIM');

        if (sFim = 'FIM') or (Length(sFim) <= 0) then
          break;

        with Ide.infPercurso.New do
        begin
          UFPer := sFim;
        end;

        Inc(I);
      end;

      Ide.dhIniViagem   := StringToDateTime(INIRec.ReadString('ide', 'dhIniViagem', '0'));
      Ide.indCanalVerde := StrToTIndicador(Ok, INIRec.ReadString('ide', 'indCanalVerde', '0'));
      Ide.indCarregaPosterior := StrToTIndicador(Ok, INIRec.ReadString('ide', 'indCarregaPosterior', '0'));

      Emit.CNPJCPF := INIRec.ReadString('emit', 'CNPJCPF', INIRec.ReadString('emit', 'CNPJ', ''));
      Emit.IE      := INIRec.ReadString('emit', 'IE', '');
      Emit.xNome   := INIRec.ReadString('emit', 'xNome', '');
      Emit.xFant   := INIRec.ReadString('emit', 'xFant', '');

      Emit.enderEmit.xLgr    := INIRec.ReadString('emit', 'xLgr', '');
      Emit.enderEmit.nro     := INIRec.ReadString('emit', 'nro', '');
      Emit.enderEmit.xCpl    := INIRec.ReadString('emit', 'xCpl', '');
      Emit.enderEmit.xBairro := INIRec.ReadString('emit', 'xBairro', '');
      Emit.enderEmit.cMun    := INIRec.ReadInteger('emit', 'cMun', 0);
      Emit.enderEmit.xMun    := INIRec.ReadString('emit', 'xMun', '');
      Emit.enderEmit.CEP     := INIRec.ReadInteger('emit', 'CEP', 0);
      Emit.enderEmit.UF      := INIRec.ReadString('emit', 'UF', '');
      Emit.enderEmit.fone    := INIRec.ReadString('emit', 'fone', '');
      Emit.enderEmit.email   := INIRec.ReadString('emit', 'email', '');

      ide.cUF := INIRec.ReadInteger('ide', 'cUF', UFparaCodigo(Emit.enderEmit.UF));

      //*********************************************************************
      //
      // Modal Rodoviário
      //
      //*********************************************************************

      Rodo.codAgPorto := INIRec.ReadString('Rodo', 'codAgPorto', '');

      // Dados sobre Informações para Agencia Reguladora (Opcional) - Nível 1 - Versão 3.00

      GerarGrupo := (INIRec.ReadString('infANTT', 'RNTRC', '') <> '') or
                    INIRec.SectionExists('infCIOT001') or
                    INIRec.SectionExists('valePed001') or
                    INIRec.SectionExists('infContratante001');

      if GerarGrupo then
      begin
        rodo.infANTT.RNTRC := INIRec.ReadString('infANTT', 'RNTRC', '');

        // Dados do CIOT (Opcional) - Nível 2 - Versão 3.00

        I := 1;
        while true do
        begin
          sSecao := 'infCIOT' + IntToStrZero(I, 3);
          sFim   := INIRec.ReadString(sSecao, 'CNPJCPF', 'FIM');

          if sFim = 'FIM' then
            break;

          with rodo.infANTT.infCIOT.New do
          begin
            CIOT    := INIRec.ReadString(sSecao, 'CIOT', '');
            CNPJCPF := sFim;
          end;

          rodo.infANTT.valePed.categCombVeic := StrTocategCombVeic(OK, INIRec.ReadString(sSecao, 'categCombVeic', ''));

          Inc(I);
        end;

        // Dados do Vale Pedágio (Opcional) - Nível 2 - Versão 3.00

        I := 1;
        while true do
        begin
          sSecao := 'valePed' + IntToStrZero(I, 3);
          sFim   := INIRec.ReadString(sSecao, 'CNPJForn', 'FIM');

          if sFim = 'FIM' then
            break;

          with rodo.infANTT.valePed.disp.New do
          begin
            CNPJForn  := sFim;
            CNPJPg    := INIRec.ReadString(sSecao, 'CNPJPg', '');
            nCompra   := INIRec.ReadString(sSecao, 'nCompra', '');
            vValePed  := StringToFloatDef(INIRec.ReadString(sSecao, 'vValePed', ''), 0 );
            tpValePed := StrTotpValePed(OK, INIRec.ReadString(sSecao, 'tpValePed', ''));
          end;

          Inc(I);
        end;

        // Dados dos Contratantes (Opcional) - Nível 2 - Versão 3.00

        I := 1;
        while true do
        begin
          sSecao := 'infContratante' + IntToStrZero(I, 3);
          sFim   := INIRec.ReadString(sSecao, 'CNPJCPF', INIRec.ReadString(sSecao, 'idEstrangeiro', 'FIM'));

          if sFim = 'FIM' then
            break;

          with rodo.infANTT.infContratante.New do
          begin
            CNPJCPF       := INIRec.ReadString(sSecao, 'CNPJCPF', '');
            idEstrangeiro := INIRec.ReadString(sSecao, 'idEstrangeiro', '');
            xNome         := INIRec.ReadString(sSecao, 'xNome', '');
          end;

          Inc(I);
        end;

        sSecao := 'infPag001';

        if INIRec.SectionExists(sSecao) then
        begin
          I := 1;
          while true do
          begin
            sSecao := 'infPag' + IntToStrZero(I, 3);
            sFim   := INIRec.ReadString(sSecao, 'CNPJCPF', INIRec.ReadString(sSecao, 'idEstrangeiro', 'FIM'));

            if sFim = 'FIM' then
              break;

            with rodo.infANTT.infPag.New do
            begin
              xNome         := INIRec.ReadString(sSecao, 'xNome', '');
              idEstrangeiro := INIRec.ReadString(sSecao, 'idEstrangeiro', '');

              if idEstrangeiro = '' then
                CNPJCPF := INIRec.ReadString(sSecao, 'CNPJCPF', '');

              vContrato     := StringToFloatDef(INIRec.ReadString(sSecao, 'vContrato', ''), 0 );
              indAltoDesemp := StrToindAltoDesemp(ok, INIRec.ReadString(sSecao, 'indAltoDesemp', ''));
              indPag        := StrToTIndPag(ok, INIRec.ReadString(sSecao, 'indPag', '0'));
              vAdiant       := StringToFloatDef(INIRec.ReadString(sSecao, 'vAdiant', ''), 0 );

              J := 1;
              while true do
              begin
                sSecao := 'Comp' + IntToStrZero(I, 3) + IntToStrZero(J, 3);
                sFim   := INIRec.ReadString(sSecao, 'vComp', 'FIM');

                if sFim = 'FIM' then
                  break;

                with Comp.New do
                begin
                  tpComp := StrToTComp(ok, INIRec.ReadString(sSecao, 'tpComp', '01'));
                  vComp  := StringToFloatDef(INIRec.ReadString(sSecao, 'vComp', ''), 0 );
                  xComp  := INIRec.ReadString(sSecao, 'xComp', '');
                end;

                Inc(J);
              end;

              if indPag = ipPrazo then
              begin
                j := 1;
                while true do
                begin
                  sSecao := 'infPrazo' + IntToStrZero(I, 3) + IntToStrZero(J, 3);
                  sFim   := INIRec.ReadString(sSecao, 'vParcela', 'FIM');

                  if sFim = 'FIM' then
                    break;

                  with infPrazo.New do
                  begin
                    nParcela := INIRec.ReadInteger(sSecao, 'nParcela', 1);
                    dVenc    := StringToDateTime(INIRec.ReadString(sSecao, 'dVenc', '0'));
                    vParcela := StringToFloatDef(INIRec.ReadString(sSecao, 'vParcela', ''), 0 );
                  end;

                  Inc(J);
                end;
              end;

              sSecao := 'infBanc' + IntToStrZero(I, 3);

              if INIRec.SectionExists(sSecao) then
              begin
                with infBanc do
                begin
                  PIX := INIRec.ReadString(sSecao, 'PIX', '');

                  if PIX = '' then
                  begin
                    CNPJIPEF := INIRec.ReadString(sSecao, 'CNPJIPEF', '');

                    if CNPJIPEF = '' then
                    begin
                      codBanco   := INIRec.ReadString(sSecao, 'codBanco', '');
                      codAgencia := INIRec.ReadString(sSecao, 'codAgencia', '');
                    end;
                  end;
                end;
              end;
            end;
            Inc(I);
          end;
        end;
      end;

      // Dados do veículo de Tração (Obrigatório) - Nível 1
      if INIRec.ReadString('veicTracao', 'placa', '') <> '' then
      begin
        rodo.veicTracao.cInt    := INIRec.ReadString('veicTracao', 'cInt', '');
        rodo.veicTracao.placa   := INIRec.ReadString('veicTracao', 'placa', '');
        rodo.veicTracao.RENAVAM := INIRec.ReadString('veicTracao', 'RENAVAM', '');
        rodo.veicTracao.tara    := INIRec.ReadInteger('veicTracao', 'tara', 0);
        rodo.veicTracao.capKG   := INIRec.ReadInteger('veicTracao', 'capKG', 0);
        rodo.veicTracao.capM3   := INIRec.ReadInteger('veicTracao', 'capM3', 0);
        rodo.veicTracao.tpRod   := StrToTpRodado(OK, INIRec.ReadString('veicTracao', 'tpRod', '01'));
        rodo.veicTracao.tpCar   := StrToTpCarroceria(OK, INIRec.ReadString('veicTracao', 'tpCar', '00'));
        rodo.veicTracao.UF      := INIRec.ReadString('veicTracao', 'UF', '');
      end;

      // Dados do proprietário do veículo de Tração (Opcional) - Nível 2

      if INIRec.ReadString('veicTracao', 'CNPJCPF', '') <> '' then
      begin
        rodo.veicTracao.prop.CNPJCPF := INIRec.ReadString('veicTracao', 'CNPJCPF', '');
        rodo.veicTracao.prop.RNTRC   := INIRec.ReadString('veicTracao', 'RNTRC', '');
        rodo.veicTracao.prop.xNome   := INIRec.ReadString('veicTracao', 'xNome', '');
        rodo.veicTracao.prop.IE      := INIRec.ReadString('veicTracao', 'IE', 'ISENTO');
        rodo.veicTracao.prop.UF      := INIRec.ReadString('veicTracao', 'UFProp', '');
        rodo.veicTracao.prop.tpProp  := StrToTpProp(OK, INIRec.ReadString('veicTracao', 'tpProp', '0'));
      end;

      // Dados do Condudor do veículo de Tração (Obrigatório) - Nível 2

      I := 1;
      while true do
      begin
        sSecao := 'moto' + IntToStrZero(I, 3);
        sFim   := INIRec.ReadString(sSecao, 'xNome', 'FIM');

        if sFim = 'FIM' then
          break;

        with rodo.veicTracao.condutor.New do
        begin
          xNome := sFim;
          CPF   := INIRec.ReadString(sSecao, 'CPF', '');
        end;

        Inc(I);
      end;

      // Dados do veículo Reboque (Opcional) - Nível 1

      I := 1;
      while true do
      begin
        sSecao := 'reboque' + IntToStrZero(I, 2);
        sFim   := INIRec.ReadString(sSecao, 'placa', 'FIM');

        if sFim = 'FIM' then
          break;

        with rodo.veicReboque.New do
        begin
          cInt    := INIRec.ReadString(sSecao, 'cInt', '');
          placa   := sFim;
          RENAVAM := INIRec.ReadString(sSecao, 'RENAVAM', '');
          tara    := INIRec.ReadInteger(sSecao, 'tara', 0);
          capKG   := INIRec.ReadInteger(sSecao, 'capKG', 0);
          capM3   := INIRec.ReadInteger(sSecao, 'capM3', 0);
          tpCar   := StrToTpCarroceria(OK, INIRec.ReadString(sSecao, 'tpCar', '00'));
          UF      := INIRec.ReadString(sSecao, 'UF', '');

          // Dados do proprietário do veículo Reboque (Opcional) - Nível 2 - Versão 3.00

          if INIRec.ReadString(sSecao, 'CNPJCPF', '') <> '' then
          begin
            prop.CNPJCPF := INIRec.ReadString(sSecao, 'CNPJCPF', '');
            prop.RNTRC   := INIRec.ReadString(sSecao, 'RNTRC', '');
            prop.xNome   := INIRec.ReadString(sSecao, 'xNome', '');
            prop.IE      := INIRec.ReadString(sSecao, 'IE', '');
            prop.UF      := INIRec.ReadString(sSecao, 'UFProp', '');
            prop.tpProp  := StrToTpProp(OK, INIRec.ReadString(sSecao, 'tpProp', '0'));
          end;
        end;

        Inc(I);
      end;

      // Dados do Lacre (Opcional) - Nível 1

      I := 1;
      while true do
      begin
        sSecao := 'lacRodo' + IntToStrZero(I, 3);
        sFim   := INIRec.ReadString(sSecao, 'nLacre', 'FIM');

        if sFim = 'FIM' then
          break;

        with rodo.lacRodo.New do
        begin
          nLacre := sFim;
        end;

        Inc(I);
      end;

      //*********************************************************************
      //
      // Modal Aéreo
      //
      //*********************************************************************

      Aereo.nac := INIRec.ReadString('aereo', 'nac', '');
      if (Aereo.nac <> '') then
      begin
        Aereo.matr    := INIRec.ReadString('aereo', 'matr', '');
        Aereo.nVoo    := INIRec.ReadString('aereo', 'nVoo', '');
        Aereo.cAerEmb := INIRec.ReadString('aereo', 'cAerEmb', '');
        Aereo.cAerDes := INIRec.ReadString('aereo', 'cAerDes', '');
        Aereo.dVoo    := StringToDateTime(INIRec.ReadString('aereo', 'dVoo', '0'));
      end; // Fim do Aereoviário

      //*********************************************************************
      //
      // Modal Aquaviário
      //
      //*********************************************************************

      Aquav.CNPJAgeNav  := INIRec.ReadString('aquav', 'CNPJAgeNav', '');
      Aquav.irin        := INIRec.ReadString('aquav', 'irin', '');

      if ( (Aquav.CNPJAgeNav  <> '') or (Aquav.irin <> '') ) then
      begin
        Aquav.tpEmb    := INIRec.ReadString('aquav', 'tpEmb', '');
        Aquav.cEmbar   := INIRec.ReadString('aquav', 'cEmbar', '');
        Aquav.xEmbar   := INIRec.ReadString('aquav', 'xEmbar', '');
        Aquav.nViagem  := INIRec.ReadString('aquav', 'nViag', '');
        Aquav.cPrtEmb  := INIRec.ReadString('aquav', 'cPrtEmb', '');
        Aquav.cPrtDest := INIRec.ReadString('aquav', 'cPrtDest', '');

        //Campos MDF-e 3.0
        Aquav.prtTrans := INIRec.ReadString('aquav', 'prtTrans', '');
        Aquav.tpNav    := StrToTpNavegacao(OK, INIRec.ReadString('aquav', 'tpNav', '0') );

        I := 1;
        while true do
        begin
          sSecao := 'infTermCarreg' + IntToStrZero(I, 1);
          sFim   := INIRec.ReadString(sSecao, 'cTermCarreg', 'FIM');

          if sFim = 'FIM' then
            break;

          with Aquav.infTermCarreg.New do
          begin
            cTermCarreg := sFim;
            xTermCarreg := INIRec.ReadString(sSecao, 'xTermCarreg', '');
          end;

          inc(I);
        end;

        I := 1;
        while true do
        begin
          sSecao := 'infTermDescarreg' + IntToStrZero(I, 1);
          sFim   := INIRec.ReadString(sSecao, 'cTermDescarreg', 'FIM');

          if sFim = 'FIM' then
            break;

          with Aquav.infTermDescarreg.New do
          begin
            cTermDescarreg := sFim;
            xTermDescarreg := INIRec.ReadString(sSecao, 'xTermDescarreg', '');
          end;

          inc(I);
        end;

        I := 1;
        while true do
        begin
          sSecao := 'infEmbComb' + IntToStrZero(I, 2);
          sFim   := INIRec.ReadString(sSecao, 'cEmbComb', 'FIM');

          if sFim = 'FIM' then
            break;

          with Aquav.infEmbComb.New do
          begin
            cEmbComb := sFim;
            xBalsa   :=  INIRec.ReadString(sSecao, 'xBalsa', '');
          end;

          inc(I);
        end;

        I := 1;
        while true do
        begin
          sSecao := 'infUnidCargaVazia' + IntToStrZero(I, 3);
          sFim   := INIRec.ReadString(sSecao, 'idUnidCargaVazia', 'FIM');

          if sFim = 'FIM' then
            break;

          with Aquav.infUnidCargaVazia.New do
          begin
            idUnidCargaVazia := sFim;
            tpUnidCargaVazia := StrToUnidCarga(OK, INIRec.ReadString(sSecao, 'tpUnidCargaVazia', '1'));
          end;

          inc(I);
        end;

        I := 1;
        while true do
        begin
          sSecao := 'infUnidTranspVazia' + IntToStrZero(I, 3);
          sFim   := INIRec.ReadString(sSecao, 'idUnidTranspVazia', 'FIM');

          if sFim = 'FIM' then
            break;

          with Aquav.infUnidTranspVazia.New do
          begin
            idUnidTranspVazia := sFim;
            tpUnidTranspVazia := StrToUnidTransp (OK, INIRec.ReadString(sSecao, 'tpUnidTranspVazia', '1'));
          end;

          inc(I);
        end;
      end; // Fim do Aquaviário

      //*********************************************************************
      //
      // Modal Ferroviário
      //
      //*********************************************************************

      Ferrov.xPref  := INIRec.ReadString('ferrov', 'xPref', '');
      if (Ferrov.xPref <> '') then
      begin
        Ferrov.dhTrem := StringToDateTime(INIRec.ReadString('ferrov', 'dhTrem', '0'));
        Ferrov.xOri   := INIRec.ReadString('ferrov', 'xOri', '');
        Ferrov.xDest  := INIRec.ReadString('ferrov', 'xDest', '');
        Ferrov.qVag   := INIRec.ReadInteger('ferrov', 'qVag', 0);

        I := 1;
        while true do
        begin
          sSecao := 'vag' + IntToStrZero(I, 3);
          sFim   := INIRec.ReadString(sSecao, 'serie', 'FIM');

          if sFim = 'FIM' then
            break;

          with Ferrov.vag.New do
          begin
            serie := sFim;
            nVag  := INIRec.ReadInteger(sSecao, 'nVag', 0);
            nSeq  := INIRec.ReadInteger(sSecao, 'nSeq', 0);
            TU    := StringToFloatDef(INIRec.ReadString(sSecao, 'TU', ''), 0);

            //Campos MDF-e 3.0
            pesoBC := StringToFloatDef( INIRec.ReadString(sSecao, 'pesoBC', ''), 0);
            pesoR  := StringToFloatDef( INIRec.ReadString(sSecao, 'pesoR', ''), 0);
            tpVag  := INIRec.ReadString(sSecao, 'tpVag', '');
          end;

          inc(I);
        end;
      end; // Fim do Ferroviário

      I := 1;
      while true do
      begin
        sSecao := 'DESC' + IntToStrZero(I, 3);
        sFim   := INIRec.ReadString(sSecao, 'xMunDescarga', 'FIM');

        if (sFim = 'FIM') or (Length(sFim) <= 0) then
          break;

        with infDoc.infMunDescarga.New do
        begin
          cMunDescarga := INIRec.ReadInteger(sSecao, 'cMunDescarga', 0);
          xMunDescarga := sFim;

          J := 1;
          while true do
          begin
            sSecao := 'infCTe' + IntToStrZero(I, 3) + IntToStrZero(J, 3);
            sFim   := INIRec.ReadString(sSecao, 'chCTe', 'FIM');

            if sFim = 'FIM' then
              break;

            with infCTe.New do
            begin
              chCTe       := sFim;
              SegCodBarra := INIRec.ReadString(sSecao, 'SegCodBarra', '');
              indReentrega:= INIRec.ReadString(sSecao, 'indReentrega', '');

              K := 1;
              while true do
              begin
                sSecao := 'peri'+IntToStrZero(I,3)+IntToStrZero(J,3)+IntToStrZero(K,3);
                sFim   := INIRec.ReadString(sSecao,'nONU','FIM');

                if sFim = 'FIM' then
                  break;

                with peri.New do
                begin
                  nONU      := sFim;
                  xNomeAE   := INIRec.ReadString(sSecao,'xNomeAE','');
                  xClaRisco := INIRec.ReadString(sSecao,'xClaRisco','');
                  grEmb     := INIRec.ReadString(sSecao,'grEmb','');
                  qTotProd  := INIRec.ReadString(sSecao,'qTotProd','');
                  qVolTipo  := INIRec.ReadString(sSecao,'qVolTipo','');
                end;

                inc(K);
              end;

              sSecao := 'infEntregaParcial'+IntToStrZero(I,3)+IntToStrZero(J,3);

              if INIRec.SectionExists(sSecao) then
              begin
                infEntregaParcial.qtdTotal   := StringToFloatDef(INIRec.ReadString(sSecao, 'qtdTotal', ''), 0);
                infEntregaParcial.qtdParcial := StringToFloatDef(INIRec.ReadString(sSecao, 'qtdParcial', ''), 0);
              end;

              K := 1;
              while true do
              begin
                sSecao := 'infUnidTransp'+IntToStrZero(I,3)+IntToStrZero(J,3)+IntToStrZero(K,3);
                sFim   := INIRec.ReadString(sSecao,'idUnidTransp','FIM');

                if sFim = 'FIM' then
                  break;

                with infUnidTransp.New do
                begin
                  tpUnidTransp := StrToUnidTransp(OK,INIRec.ReadString(sSecao,'tpUnidTransp','1'));
                  idUnidTransp := sFim;
                  qtdRat       := StringToFloatDef( INIRec.ReadString(sSecao,'qtdRat',''),0);

                  L := 1;
                  while true do
                  begin
                    sSecao := 'lacUnidTransp'+IntToStrZero(I,3)+IntToStrZero(J,3)+IntToStrZero(K,3)+IntToStrZero(L,3);
                    sFim   := INIRec.ReadString(sSecao,'nLacre','FIM');

                    if sFim = 'FIM' then
                      break;

                    with lacUnidTransp.New do
                    begin
                      nLacre := sFim;
                    end;

                    inc(L);
                  end;

                  L := 1;
                  while true do
                  begin
                    sSecao := 'infUnidCarga'+IntToStrZero(I,3)+IntToStrZero(J,3)+IntToStrZero(K,3)+IntToStrZero(L,3);
                    sFim   := INIRec.ReadString(sSecao,'idUnidCarga','FIM');

                    if sFim = 'FIM' then
                      break;

                    with infUnidCarga.New do
                    begin
                      tpUnidCarga := StrToUnidCarga(OK,INIRec.ReadString(sSecao,'tpUnidCarga','1'));
                      idUnidCarga := sFim;
                      qtdRat      := StringToFloatDef( INIRec.ReadString(sSecao,'qtdRat',''),0);

                      M := 1;
                      while true do
                      begin
                        sSecao := 'lacUnidCarga'+IntToStrZero(I,3)+IntToStrZero(J,3)+IntToStrZero(K,3)+IntToStrZero(L,3)+IntToStrZero(M,3);
                        sFim   := INIRec.ReadString(sSecao,'nLacre','FIM');

                        if sFim = 'FIM' then
                          break;

                        with lacUnidCarga.New do
                        begin
                          nLacre := sFim;
                        end;

                        inc(M);
                      end;
                    end;

                    inc(L);
                  end;
                end;

                inc(K);
              end;
            end;

            Inc(J);
          end;

          J := 1;
          while true do
          begin
            sSecao := 'infNFe' + IntToStrZero(I, 3) + IntToStrZero(J, 3);
            sFim   := INIRec.ReadString(sSecao, 'chNFe', 'FIM');

            if sFim = 'FIM' then
              break;

            with infNFe.New do
            begin
              chNFe        := sFim;
              SegCodBarra  := INIRec.ReadString(sSecao, 'SegCodBarra', '');
              indReentrega := INIRec.ReadString(sSecao, 'indReentrega', '');

              K := 1;
              while true do
              begin
                sSecao := 'peri'+IntToStrZero(I,3)+IntToStrZero(J,3)+IntToStrZero(K,3);
                sFim   := INIRec.ReadString(sSecao,'nONU','FIM');

                if sFim = 'FIM' then
                  break;

                with peri.New do
                begin
                  nONU      := sFim;
                  xNomeAE   := INIRec.ReadString(sSecao,'xNomeAE','');
                  xClaRisco := INIRec.ReadString(sSecao,'xClaRisco','');
                  grEmb     := INIRec.ReadString(sSecao,'grEmb','');
                  qTotProd  := INIRec.ReadString(sSecao,'qTotProd','');
                  qVolTipo  := INIRec.ReadString(sSecao,'qVolTipo','');
                end;

                inc(K);
              end;

              K := 1;
              while true do
              begin
                sSecao := 'infUnidTransp'+IntToStrZero(I,3)+IntToStrZero(J,3)+IntToStrZero(K,3);
                sFim   := INIRec.ReadString(sSecao,'idUnidTransp','FIM');

                if sFim = 'FIM' then
                  break;

                with infUnidTransp.New do
                begin
                  tpUnidTransp := StrToUnidTransp(OK,INIRec.ReadString(sSecao,'tpUnidTransp','1'));
                  idUnidTransp := sFim;
                  qtdRat       := StringToFloatDef( INIRec.ReadString(sSecao,'qtdRat',''),0);

                  L := 1;
                  while true do
                  begin
                    sSecao := 'lacUnidTransp'+IntToStrZero(I,3)+IntToStrZero(J,3)+IntToStrZero(K,3)+IntToStrZero(L,3);
                    sFim   := INIRec.ReadString(sSecao,'nLacre','FIM');

                    if sFim = 'FIM' then
                      break;

                    with lacUnidTransp.New do
                    begin
                      nLacre := sFim;
                    end;

                    inc(L);
                  end;

                  L := 1;
                  while true do
                  begin
                    sSecao := 'infUnidCarga'+IntToStrZero(I,3)+IntToStrZero(J,3)+IntToStrZero(K,3)+IntToStrZero(L,3);
                    sFim   := INIRec.ReadString(sSecao,'idUnidCarga','FIM');

                    if sFim = 'FIM' then
                      break;

                    with infUnidCarga.New do
                    begin
                      tpUnidCarga := StrToUnidCarga(OK,INIRec.ReadString(sSecao,'tpUnidCarga','1'));
                      idUnidCarga := sFim;
                      qtdRat      := StringToFloatDef( INIRec.ReadString(sSecao,'qtdRat',''),0);

                      M := 1;
                      while true do
                      begin
                        sSecao := 'lacUnidCarga'+IntToStrZero(I,3)+IntToStrZero(J,3)+IntToStrZero(K,3)+IntToStrZero(L,3)+IntToStrZero(M,3);
                        sFim   := INIRec.ReadString(sSecao,'nLacre','FIM');

                        if sFim = 'FIM' then
                          break;

                        with lacUnidCarga.New do
                        begin
                          nLacre := sFim;
                        end;

                        inc(M);
                      end;
                    end;

                    inc(L);
                  end;
                end;

                inc(K);
              end;
            end;

            Inc(J);
          end;

          J := 1;
          while true do
          begin
            sSecao := 'infDCeTransp' + IntToStrZero(I, 3) + IntToStrZero(J, 3);
            sFim   := INIRec.ReadString(sSecao, 'chDCe', 'FIM');

            if sFim = 'FIM' then
              break;

            with infDCeTransp.New do
            begin
              chDCe := sFim;
              indReentrega:= INIRec.ReadString(sSecao, 'indReentrega', '');

              K := 1;
              while true do
              begin
                sSecao := 'peri'+IntToStrZero(I,3)+IntToStrZero(J,3)+IntToStrZero(K,3);
                sFim   := INIRec.ReadString(sSecao,'nONU','FIM');

                if sFim = 'FIM' then
                  break;

                with peri.New do
                begin
                  nONU      := sFim;
                  xNomeAE   := INIRec.ReadString(sSecao,'xNomeAE','');
                  xClaRisco := INIRec.ReadString(sSecao,'xClaRisco','');
                  grEmb     := INIRec.ReadString(sSecao,'grEmb','');
                  qTotProd  := INIRec.ReadString(sSecao,'qTotProd','');
                  qVolTipo  := INIRec.ReadString(sSecao,'qVolTipo','');
                end;

                inc(K);
              end;

              K := 1;
              while true do
              begin
                sSecao := 'infUnidTransp'+IntToStrZero(I,3)+IntToStrZero(J,3)+IntToStrZero(K,3);
                sFim   := INIRec.ReadString(sSecao,'idUnidTransp','FIM');

                if sFim = 'FIM' then
                  break;

                with infUnidTransp.New do
                begin
                  tpUnidTransp := StrToUnidTransp(OK,INIRec.ReadString(sSecao,'tpUnidTransp','1'));
                  idUnidTransp := sFim;
                  qtdRat       := StringToFloatDef( INIRec.ReadString(sSecao,'qtdRat',''),0);

                  L := 1;
                  while true do
                  begin
                    sSecao := 'lacUnidTransp'+IntToStrZero(I,3)+IntToStrZero(J,3)+IntToStrZero(K,3)+IntToStrZero(L,3);
                    sFim   := INIRec.ReadString(sSecao,'nLacre','FIM');

                    if sFim = 'FIM' then
                      break;

                    with lacUnidTransp.New do
                    begin
                      nLacre := sFim;
                    end;

                    inc(L);
                  end;

                  L := 1;
                  while true do
                  begin
                    sSecao := 'infUnidCarga'+IntToStrZero(I,3)+IntToStrZero(J,3)+IntToStrZero(K,3)+IntToStrZero(L,3);
                    sFim   := INIRec.ReadString(sSecao,'idUnidCarga','FIM');

                    if sFim = 'FIM' then
                      break;

                    with infUnidCarga.New do
                    begin
                      tpUnidCarga := StrToUnidCarga(OK,INIRec.ReadString(sSecao,'tpUnidCarga','1'));
                      idUnidCarga := sFim;
                      qtdRat      := StringToFloatDef( INIRec.ReadString(sSecao,'qtdRat',''),0);

                      M := 1;
                      while true do
                      begin
                        sSecao := 'lacUnidCarga'+IntToStrZero(I,3)+IntToStrZero(J,3)+IntToStrZero(K,3)+IntToStrZero(L,3)+IntToStrZero(M,3);
                        sFim   := INIRec.ReadString(sSecao,'nLacre','FIM');

                        if sFim = 'FIM' then
                          break;

                        with lacUnidCarga.New do
                        begin
                          nLacre := sFim;
                        end;

                        inc(M);
                      end;

                      inc(L);
                    end;
                  end;

                  inc(K);
                end;
              end;

              Inc(J);
            end;
          end;

          Inc(I);
        end;
      end;

      I := 1;
      while true do
      begin
        sSecao := 'seg' + IntToStrZero(I, 3);

        if (INIRec.ReadString(sSecao, 'xSeg', '') = '') and
           (INIRec.ReadString(sSecao, 'nApol', '') = '') and
           not INIRec.SectionExists('aver' + IntToStrZero(I, 3) + '001') then
          Break;

        with seg.New do
        begin
          respSeg :=  StrToRspSeguroDCe(OK, INIRec.ReadString(sSecao, 'respSeg', '1'));
          CNPJCPF := INIRec.ReadString(sSecao, 'CNPJCPF', '');
          xSeg    := INIRec.ReadString(sSecao, 'xSeg', '');
          CNPJ    := INIRec.ReadString(sSecao, 'CNPJ', '');
          nApol   := INIRec.ReadString(sSecao, 'nApol', '');

          J := 1;
          while true do
          begin
            sSecao := 'aver' + IntToStrZero(I, 3) + IntToStrZero(J, 3);
            sFim   := INIRec.ReadString(sSecao, 'nAver', 'FIM');

            if sFim = 'FIM' then
              break;

            with aver.New do
            begin
              nAver := sFim;
            end;

            Inc(J);
          end;
        end;

        Inc(I);
      end;

      sSecao := 'prodPred';

      if INIRec.SectionExists(sSecao) then
      begin
        DCe.prodPred.tpCarga := StrToTCarga(ok, INIRec.ReadString(sSecao, 'tpCarga', '01'));
        DCe.prodPred.xProd   := INIRec.ReadString(sSecao, 'xProd', '');
        DCe.prodPred.cEAN    := INIRec.ReadString(sSecao, 'cEAN', '');
        DCe.prodPred.NCM     := INIRec.ReadString(sSecao, 'NCM', '');

        sSecao := 'infLocalCarrega';

        if INIRec.SectionExists(sSecao) then
        begin
          DCe.prodPred.infLocalCarrega.CEP       := INIRec.ReadInteger(sSecao, 'CEP', 0);
          DCe.prodPred.infLocalCarrega.latitude  := StringToFloatDef( INIRec.ReadString(sSecao,'latitude',''),0);
          DCe.prodPred.infLocalCarrega.longitude := StringToFloatDef( INIRec.ReadString(sSecao,'longitude',''),0);
        end;

        sSecao := 'infLocalDescarrega';

        if INIRec.SectionExists(sSecao) then
        begin
          DCe.prodPred.infLocalDescarrega.CEP       := INIRec.ReadInteger(sSecao, 'CEP', 0);
          DCe.prodPred.infLocalDescarrega.latitude  := StringToFloatDef( INIRec.ReadString(sSecao,'latitude',''),0);
          DCe.prodPred.infLocalDescarrega.longitude := StringToFloatDef( INIRec.ReadString(sSecao,'longitude',''),0);
        end;
      end;

      tot.qCTe   := INIRec.ReadInteger('tot', 'qCTe', 0);
      tot.qCT    := INIRec.ReadInteger('tot', 'qCT', 0);
      tot.qNFe   := INIRec.ReadInteger('tot', 'qNFe', 0);
      tot.qNF    := INIRec.ReadInteger('tot', 'qNF', 0);
      tot.qDCe  := INIRec.ReadInteger('tot', 'qDCe', 0);
      tot.vCarga := StringToFloatDef(INIRec.ReadString('tot', 'vCarga', ''), 0);
      tot.cUnid  := StrToUnidMed(OK, INIRec.ReadString('tot', 'cUnid', '01'));
      tot.qCarga := StringToFloatDef(INIRec.ReadString('tot', 'qCarga', ''), 0);

      I := 1;
      while true do
      begin
        sSecao := 'lacres' + IntToStrZero(I, 3);
        sFim   := INIRec.ReadString(sSecao, 'nLacre', 'FIM');

        if sFim = 'FIM' then
          break;

        with lacres.New do
        begin
          nLacre := sFim;
        end;

        Inc(I);
      end;

      I := 1;
      while true do
      begin
        sSecao := 'autXML' + IntToStrZero(I, 2);
        sFim   := INIRec.ReadString(sSecao, 'CNPJCPF', 'FIM');
        if (sFim = 'FIM') or (Length(sFim) <= 0) then
        begin
          sSecao := 'autXML' + IntToStrZero(I, 3);
          sFim   := INIRec.ReadString(sSecao, 'CNPJCPF', 'FIM');
        end;
        if (sFim = 'FIM') or (Length(sFim) <= 0) then
          break;

        with autXML.New do
        begin
          CNPJCPF := sFim;
        end;

        Inc(I);
      end;

      infAdic.infCpl     := INIRec.ReadString('infAdic', 'infCpl', '');
      infAdic.infAdFisco := INIRec.ReadString('infAdic', 'infAdFisco', '');

      sSecao := 'infRespTec';
      if INIRec.SectionExists(sSecao) then
      begin
        with infRespTec do
        begin
          CNPJ     := INIRec.ReadString(sSecao, 'CNPJ', '');
          xContato := INIRec.ReadString(sSecao, 'xContato', '');
          email    := INIRec.ReadString(sSecao, 'email', '');
          fone     := INIRec.ReadString(sSecao, 'fone', '');
        end;
      end;
    end;
    *)
    GerarXML;

    Result := True;
  finally
    INIRec.Free;
  end;
end;

{ TDeclaracoes }

constructor TDeclaracoes.Create(AOwner: TPersistent; ItemClass: TCollectionItemClass);
begin
  if not (AOwner is TACBrDCe) then
    raise EACBrDCeException.Create('AOwner deve ser do tipo TACBrDCe');

  inherited Create(AOwner, ItemClass);

  FACBrDCe := TACBrDCe(AOwner);
  FConfiguracoes := TACBrDCe(FACBrDCe).Configuracoes;
end;

function TDeclaracoes.Add: Declaracao;
begin
  Result := Declaracao(inherited Add);
end;

procedure TDeclaracoes.Assinar;
var
  i: integer;
begin
  for i := 0 to Self.Count - 1 do
    Self.Items[i].Assinar;
end;

function TDeclaracoes.GerarIni: String;
begin
  Result := '';
  if (Self.Count > 0) then
    Result := Self.Items[0].GerarDCeIni;
end;

procedure TDeclaracoes.GerarDCe;
var
  i: integer;
begin
  for i := 0 to Self.Count - 1 do
    Self.Items[i].GerarXML;
end;

function TDeclaracoes.GetItem(Index: integer): Declaracao;
begin
  Result := Declaracao(inherited Items[Index]);
end;

function TDeclaracoes.GetNamePath: String;
begin
  Result := 'Declaracao';
end;

procedure TDeclaracoes.VerificarDADCe;
begin
  if not Assigned(TACBrDCe(FACBrDCe).DACE) then
    raise EACBrDCeException.Create('Componente DACE não associado.');
end;

procedure TDeclaracoes.Imprimir;
begin
  VerificarDADCe;
  TACBrDCe(FACBrDCe).DACE.ImprimirDACE(nil);
end;

procedure TDeclaracoes.ImprimirPDF;
begin
  VerificarDADCe;
  TACBrDCe(FACBrDCe).DACE.ImprimirDACEPDF(nil);
end;

function TDeclaracoes.Insert(Index: integer): Declaracao;
begin
  Result := Declaracao(inherited Insert(Index));
end;

procedure TDeclaracoes.SetItem(Index: integer; const Value: Declaracao);
begin
  Items[Index].Assign(Value);
end;

procedure TDeclaracoes.Validar;
var
  i: integer;
begin
  for i := 0 to Self.Count - 1 do
    Self.Items[i].Validar;   // Dispara exception em caso de erro
end;

function TDeclaracoes.VerificarAssinatura(out Erros: String): Boolean;
var
  i: integer;
begin
  Result := True;
  Erros := '';

  if Self.Count < 1 then
  begin
    Erros := 'Nenhum DCe carregado';
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

function TDeclaracoes.ValidarRegrasdeNegocios(out Erros: String): Boolean;
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

function TDeclaracoes.LoadFromFile(const CaminhoArquivo: String;
  AGerarDCe: Boolean): Boolean;
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

  l := Self.Count; // Indice do último Declaracao já existente
  Result := LoadFromString(String(XMLUTF8), AGerarDCe);

  if Result then
  begin
    // Atribui Nome do arquivo a novos Declaracoes inseridos //
    for i := l to Self.Count - 1 do
      Self.Items[i].NomeArq := CaminhoArquivo;
  end;
end;

function TDeclaracoes.LoadFromStream(AStream: TStringStream;
  AGerarDCe: Boolean): Boolean;
var
  AXML: AnsiString;
begin
  AStream.Position := 0;
  AXML := ReadStrFromStream(AStream, AStream.Size);

  Result := Self.LoadFromString(String(AXML), AGerarDCe);
end;

function TDeclaracoes.LoadFromString(const AXMLString: String;
  AGerarDCe: Boolean): Boolean;
var
  ADCeXML, XMLStr: AnsiString;
  P, N: integer;

  function PosDCe: integer;
  begin
    Result := pos('</DCe>', XMLStr);
  end;

begin
  // Verifica se precisa Converter de UTF8 para a String nativa da IDE //
  XMLStr := ConverteXMLtoNativeString(AXMLString);

  N := PosDCe;
  while N > 0 do
  begin
    P := pos('</DCeProc>', XMLStr);

    if P <= 0 then
      P := pos('</procDCe>', XMLStr);  // DCe obtido pelo Portal da Receita

    if P > 0 then
    begin
      ADCeXML := copy(XMLStr, 1, P + 11);
      XMLStr := Trim(copy(XMLStr, P + 11, length(XMLStr)));
    end
    else
    begin
      ADCeXML := copy(XMLStr, 1, N + 7);
      XMLStr := Trim(copy(XMLStr, N + 7, length(XMLStr)));
    end;

    with Self.Add do
    begin
      LerXML(ADCeXML);

      if AGerarDCe then // Recalcula o XML
        GerarXML;
    end;

    N := PosDCe;
  end;

  Result := Self.Count > 0;
end;

function TDeclaracoes.LoadFromIni(const AIniString: String): Boolean;
begin
  with Self.Add do
    LerArqIni(AIniString);

  Result := Self.Count > 0;
end;

function TDeclaracoes.GravarXML(const PathNomeArquivo: String): Boolean;
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
