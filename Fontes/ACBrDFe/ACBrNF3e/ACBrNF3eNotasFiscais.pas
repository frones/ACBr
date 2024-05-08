{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }
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

unit ACBrNF3eNotasFiscais;

interface

uses
  Classes, SysUtils, StrUtils,
  ACBrXmlBase,
  ACBrNF3eConfiguracoes, ACBrNF3eClass,
  ACBrNF3eXmlReader, ACBrNF3eXmlWriter;

type

  { TNotaFiscal }

  TNotaFiscal = class(TCollectionItem)
  private
    FNF3e: TNF3e;
    FNF3eW: TNF3eXmlWriter;
    FNF3eR: TNF3eXmlReader;
    FConfiguracoes: TConfiguracoesNF3e;
    FXMLAssinado: String;
    FXMLOriginal: String;
    FAlertas: String;
    FErroValidacao: String;
    FErroValidacaoCompleto: String;
    FErroRegrasdeNegocios: String;
    FNomeArq: String;

    function GetConfirmada: Boolean;
    function GetcStat: Integer;
    function GetProcessada: Boolean;
    function GetCancelada: Boolean;

    function GetMsg: String;
    function GetNumID: String;
    function GetXMLAssinado: String;
    procedure SetXML(const AValue: String);
    procedure SetXMLOriginal(const AValue: String);
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

    function LerXML(const AXML: String): Boolean;
    function LerArqIni(const AIniString: String): Boolean;
    function GerarNF3eIni: String;

    function GerarXML: String;
    function GravarXML(const NomeArquivo: String = ''; const PathArquivo: String = ''): Boolean;

    function GravarStream(AStream: TStream): Boolean;

    procedure EnviarEmail(const sPara, sAssunto: String; sMensagem: TStrings = nil;
      EnviaPDF: Boolean = True; sCC: TStrings = nil; Anexos: TStrings = nil;
      sReplyTo: TStrings = nil);

    property NomeArq: String read FNomeArq write FNomeArq;
    function CalcularNomeArquivoCompleto(NomeArquivo: String = '';
      PathArquivo: String = ''): String;

    property NF3e: TNF3e read FNF3e;

    // Atribuir a "XML", faz o componente transferir os dados lido para as propriedades internas e "XMLAssinado"
    property XML: String         read FXMLOriginal   write SetXML;
    // Atribuir a "XMLOriginal", reflete em XMLAssinado, se existir a tag de assinatura
    property XMLOriginal: String read FXMLOriginal   write SetXMLOriginal;    // Sempre deve estar em UTF8
    property XMLAssinado: String read GetXMLAssinado write FXMLAssinado;      // Sempre deve estar em UTF8
    property Confirmada: Boolean read GetConfirmada;
    property Processada: Boolean read GetProcessada;
    property Cancelada: Boolean read GetCancelada;
    property cStat: Integer read GetcStat;
    property Msg: String read GetMsg;
    property NumID: String read GetNumID;

    property Alertas: String read FAlertas;
    property ErroValidacao: String read FErroValidacao;
    property ErroValidacaoCompleto: String read FErroValidacaoCompleto;
    property ErroRegrasdeNegocios: String read FErroRegrasdeNegocios;

  end;

  { TNotasFiscais }

  TNotasFiscais = class(TOwnedCollection)
  private
    FACBrNF3e: TComponent;
    FConfiguracoes: TConfiguracoesNF3e;

    function GetItem(Index: integer): TNotaFiscal;
    procedure SetItem(Index: integer; const Value: TNotaFiscal);

    procedure VerificarDANF3e;
  public
    constructor Create(AOwner: TPersistent; ItemClass: TCollectionItemClass);

    procedure GerarNF3e;
    procedure Assinar;
    procedure Validar;
    function VerificarAssinatura(out Erros: String): Boolean;
    function ValidarRegrasdeNegocios(out Erros: String): Boolean;
    procedure Imprimir;
    procedure ImprimirCancelado;
    procedure ImprimirResumido;
    procedure ImprimirPDF;
    procedure ImprimirResumidoPDF;
    function Add: TNotaFiscal;
    function Insert(Index: integer): TNotaFiscal;

    property Items[Index: integer]: TNotaFiscal read GetItem write SetItem; default;

    function GetNamePath: String; override;
    // Incluido o Parametro AGerarNF3e que determina se após carregar os dados da NF3e
    // para o componente, será gerado ou não novamente o XML da NF3e.
    function LoadFromFile(const CaminhoArquivo: String; AGerarNF3e: Boolean = False): Boolean;
    function LoadFromStream(AStream: TStringStream; AGerarNF3e: Boolean = False): Boolean;
    function LoadFromString(const AXMLString: String; AGerarNF3e: Boolean = False): Boolean;
    function LoadFromIni(const AIniString: String): Boolean;

    function GerarIni: String;
    function GravarXML(const APathNomeArquivo: String = ''): Boolean;

    property ACBrNF3e: TComponent read FACBrNF3e;
  end;

implementation

uses
  dateutils, IniFiles,
  synautil,
  ACBrUtil.Base, ACBrUtil.Strings, ACBrUtil.XMLHTML, ACBrUtil.FilesIO,
  ACBrUtil.DateTime,
  ACBrDFeUtil,
  ACBrNF3e, ACBrNF3eConversao,
  ACBrXmlDocument;
{ NotaFiscal }

constructor TNotaFiscal.Create(Collection2: TCollection);
begin
  inherited Create(Collection2);

  FNF3e := TNF3e.Create;
  FNF3eW := TNF3eXmlWriter.Create(FNF3e);
  FNF3eR := TNF3eXmlReader.Create(FNF3e);

  FConfiguracoes := TACBrNF3e(TNotasFiscais(Collection).ACBrNF3e).Configuracoes;

  FNF3e.Ide.verProc := 'ACBrNF3e';

  with TACBrNF3e(TNotasFiscais(Collection).ACBrNF3e) do
  begin
    FNF3e.Ide.modelo := 66;
    FNF3e.infNF3e.Versao := VersaoNF3eToDbl(Configuracoes.Geral.VersaoDF);
    FNF3e.Ide.tpAmb := TACBrTipoAmbiente(Configuracoes.WebServices.Ambiente);
    FNF3e.Ide.tpEmis := TACBrTipoEmissao(Configuracoes.Geral.FormaEmissao);
  end;
end;

destructor TNotaFiscal.Destroy;
begin
  FNF3eW.Free;
  FNF3eR.Free;
  FNF3e.Free;

  inherited Destroy;
end;

procedure TNotaFiscal.Imprimir;
begin
  with TACBrNF3e(TNotasFiscais(Collection).ACBrNF3e) do
  begin
    if not Assigned(DANF3e) then
      raise EACBrNF3eException.Create('Componente DANF3e não associado.')
    else
      DANF3e.ImprimirDANF3e(NF3e);
  end;
end;

procedure TNotaFiscal.ImprimirPDF;
begin
  with TACBrNF3e(TNotasFiscais(Collection).ACBrNF3e) do
  begin
    if not Assigned(DANF3e) then
      raise EACBrNF3eException.Create('Componente DANF3e não associado.')
    else
      DANF3e.ImprimirDANF3ePDF(NF3e);
  end;
end;

procedure TNotaFiscal.Assinar;
var
  XMLStr: String;
  XMLUTF8: AnsiString;
  Document: TACBrXmlDocument;
  ANode, SignatureNode, ReferenceNode, X509DataNode: TACBrXmlNode;
begin
  with TACBrNF3e(TNotasFiscais(Collection).ACBrNF3e) do
  begin
    if not Assigned(SSL.AntesDeAssinar) then
      SSL.ValidarCNPJCertificado( NF3e.Emit.CNPJ );
  end;

  // Gera novamente, para processar propriedades que podem ter sido modificadas
  XMLStr := GerarXML;

  // XML já deve estar em UTF8, para poder ser assinado //
  XMLUTF8 := ConverteXMLtoUTF8(XMLStr);

  with TACBrNF3e(TNotasFiscais(Collection).ACBrNF3e) do
  begin
    FXMLAssinado := SSL.Assinar(String(XMLUTF8), 'NF3e', 'infNF3e');
    // SSL.Assinar() sempre responde em UTF8...
    FXMLOriginal := FXMLAssinado;

    Document := TACBrXmlDocument.Create;
    try
      Document.LoadFromXml(FXMLOriginal);
      ANode := Document.Root;

      if ANode <> nil then
      begin
        SignatureNode := ANode.Childrens.FindAnyNs('Signature');
        ReferenceNode := SignatureNode.Childrens.FindAnyNs('SignedInfo')
                                      .Childrens.FindAnyNs('Reference');
        X509DataNode :=  SignatureNode.Childrens.FindAnyNs('KeyInfo')
                                      .Childrens.FindAnyNs('X509Data');

        NF3e.signature.URI := ObterConteudoTag(ReferenceNode.Attributes.Items['URI']);
        NF3e.signature.DigestValue := ObterConteudoTag(ReferenceNode.Childrens.FindAnyNs('DigestValue'), tcStr);
        NF3e.signature.SignatureValue := ObterConteudoTag(SignatureNode.Childrens.FindAnyNs('SignatureValue'), tcStr);
        NF3e.signature.X509Certificate := ObterConteudoTag(X509DataNode.Childrens.FindAnyNs('X509Certificate'), tcStr);
      end;
    finally
      FreeAndNil(Document);
    end;

//    with TACBrNF3e(TNotasFiscais(Collection).ACBrNF3e) do
//    begin
      NF3e.infNF3eSupl.qrCodNF3e := GetURLQRCode(NF3e.Ide.cUF,
                                                 NF3e.Ide.tpAmb,
                                                 NF3e.Ide.tpEmis,
                                                 NF3e.infNF3e.ID,
                                                 NF3e.infNF3e.Versao);

      GerarXML;
//    end;

    if Configuracoes.Arquivos.Salvar then
    begin
      if NaoEstaVazio(NomeArq) then
        Gravar(NomeArq, FXMLAssinado)
      else
      begin
        NomeArq := CalcularNomeArquivoCompleto();
        Gravar(NomeArq, FXMLAssinado);
      end;
    end;
  end;
end;

procedure TNotaFiscal.Validar;
var
  Erro, AXML: String;
  NotaEhValida{, ok}: Boolean;
  ALayout: TLayOut;
  VerServ: Real;
//  cUF: Integer;
begin
  AXML := FXMLAssinado;
  if AXML = '' then
    AXML := XMLOriginal;

  with TACBrNF3e(TNotasFiscais(Collection).ACBrNF3e) do
  begin
    VerServ := FNF3e.infNF3e.Versao;
//    cUF     := FNF3e.Ide.cUF;

//    if EhAutorizacao( DblToVersaoNF3e(ok, VerServ), Modelo, cUF) then
//      ALayout := LayNF3eAutorizacao
//    else
      ALayout := LayNF3eRecepcao;

    // Extraindo apenas os dados da NF3e (sem nf3eProc)
    AXML := ObterDFeXML(AXML, 'NF3e', ACBRNF3e_NAMESPACE);

    if EstaVazio(AXML) then
    begin
      Erro := ACBrStr('NF3e não encontrada no XML');
      NotaEhValida := False;
    end
    else
      NotaEhValida := SSL.Validar(AXML, GerarNomeArqSchema(ALayout, VerServ), Erro);

    if not NotaEhValida then
    begin
      FErroValidacao := ACBrStr('Falha na validação dos dados da nota: ') +
        IntToStr(NF3e.Ide.nNF) + sLineBreak + FAlertas ;
      FErroValidacaoCompleto := FErroValidacao + sLineBreak + Erro;

      raise EACBrNF3eException.CreateDef(
        IfThen(Configuracoes.Geral.ExibirErroSchema, ErroValidacaoCompleto,
        ErroValidacao));
    end;
  end;
end;

function TNotaFiscal.VerificarAssinatura: Boolean;
var
  Erro, AXML: String;
  AssEhValida: Boolean;
begin
  AXML := FXMLAssinado;
  if AXML = '' then
    AXML := XMLOriginal;

  with TACBrNF3e(TNotasFiscais(Collection).ACBrNF3e) do
  begin
    // Extraindo apenas os dados da NF3e (sem nf3eProc)
    AXML := ObterDFeXML(AXML, 'NF3e', ACBRNF3e_NAMESPACE);

    if EstaVazio(AXML) then
    begin
      Erro := ACBrStr('NF3e não encontrada no XML');
      AssEhValida := False;
    end
    else
      AssEhValida := SSL.VerificarAssinatura(AXML, Erro, 'infNF3e');

    if not AssEhValida then
    begin
      FErroValidacao := ACBrStr('Falha na validação da assinatura da nota: ') +
        IntToStr(NF3e.Ide.nNF) + sLineBreak + Erro;
    end;
  end;

  Result := AssEhValida;
end;

function TNotaFiscal.ValidarRegrasdeNegocios: Boolean;
const
  SEM_GTIN = 'SEM GTIN';
var
  Erros: String;
  Inicio, Agora: TDateTime;

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
  Inicio := Now;
  Agora := IncMinute(Now, 5);  //Aceita uma tolerância de até 5 minutos, devido ao sincronismo de horário do servidor da Empresa e o servidor da SEFAZ.
  GravaLog('Inicio da Validação');

  with TACBrNF3e(TNotasFiscais(Collection).ACBrNF3e) do
  begin
    Erros := '';
    {
      Incluir as regras aqui
    }
  end;

  Result := EstaVazio(Erros);

  if not Result then
  begin
    Erros := ACBrStr('Erro(s) nas Regras de negócios da nota: '+
                     IntToStr(NF3e.Ide.nNF) + sLineBreak +
                     Erros);
  end;

  GravaLog('Fim da Validação. Tempo: '+FormatDateTime('hh:nn:ss:zzz', Now - Inicio)+sLineBreak+
           'Erros:' + Erros);

  //DEBUG
  //WriteToTXT('c:\temp\Notafiscal.txt', Log);

  FErroRegrasdeNegocios := Erros;
end;

function TNotaFiscal.LerXML(const AXML: String): Boolean;
begin
  XMLOriginal := AXML;

  FNF3eR.Arquivo := XMLOriginal;
  FNF3eR.LerXml;
  Result := True;
end;

function TNotaFiscal.LerArqIni(const AIniString: String): Boolean;
var
  INIRec: TMemIniFile;
  sSecao, sFim: string;
  OK: boolean;
  i, j: Integer;
  ItemgGrContrat: TgGrContratCollectionItem;
  ItemNFDet: TNFDetCollectionItem;
  ItemDet: TDetCollectionItem;
  ItemgMed: TgMedCollectionItem;
begin
  INIRec := TMemIniFile.Create('');
  try
    LerIniArquivoOuString(AIniString, INIRec);

    with FNF3e do
    begin
      infNF3e.versao := StringToFloatDef(INIRec.ReadString('infNF3e', 'versao', VersaoNF3eToStr(FConfiguracoes.Geral.VersaoDF)), 0);

      sSecao := 'ide';
      Ide.tpAmb := StrToTipoAmbiente(OK, INIRec.ReadString(sSecao, 'tpAmb', IntToStr(Integer(FConfiguracoes.WebServices.Ambiente))));
      Ide.modelo := INIRec.ReadInteger(sSecao, 'Modelo', 62);
      Ide.serie := INIRec.ReadInteger(sSecao, 'Serie', 1);
      Ide.nNF := INIRec.ReadInteger(sSecao, 'nNF', 0);
      Ide.cNF := INIRec.ReadInteger(sSecao, 'cNF', 0);
      Ide.dhEmi := StringToDateTime(INIRec.ReadString(sSecao, 'dhEmi', '0'));
      Ide.tpEmis := StrToTipoEmissao(OK, INIRec.ReadString(sSecao, 'tpEmis', IntToStr(FConfiguracoes.Geral.FormaEmissaoCodigo)));
      Ide.nSiteAutoriz := StrToSiteAutorizator(INIRec.ReadString(sSecao, 'nSiteAutoriz', '0'));
      Ide.finNF3e := StrToFinNF3e(INIRec.ReadString(sSecao, 'finNF3e', '0'));
      Ide.verProc := INIRec.ReadString(sSecao, 'verProc', 'ACBrNFCom');
//      Ide.indPrePago := StrToTIndicador(INIRec.ReadString(sSecao, 'indPrePago', '0'));
//      Ide.indCessaoMeiosRede := StrToTIndicador(INIRec.ReadString(sSecao, 'indCessaoMeiosRede', '0'));
//      Ide.indNotaEntrada := StrToTIndicador(INIRec.ReadString(sSecao, 'indNotaEntrada', '0'));
      Ide.dhCont := StringToDateTime(INIRec.ReadString(sSecao, 'dhCont', '0'));
      Ide.xJust := INIRec.ReadString(sSecao, 'xJust', '');

      sSecao := 'emit';
      Emit.CNPJ := INIRec.ReadString(sSecao, 'CNPJ', '');
      Emit.IE := INIRec.ReadString(sSecao, 'IE', '');
//      Emit.IEUFDest := INIRec.ReadString(sSecao, 'IEUFDest', '');
//      Emit.CRT := StrToCRT(INIRec.ReadString(sSecao, 'CRT', '3'));
      Emit.xNome := INIRec.ReadString(sSecao, 'xNome', '');
      Emit.xFant := INIRec.ReadString(sSecao, 'xFant', '');
      // Endereço do Emitente
      Emit.EnderEmit.xLgr := INIRec.ReadString(sSecao, 'xLgr', '');
      Emit.EnderEmit.nro := INIRec.ReadString(sSecao, 'nro', '');
      Emit.EnderEmit.xCpl := INIRec.ReadString(sSecao, 'xCpl', '');
      Emit.EnderEmit.xBairro := INIRec.ReadString(sSecao, 'xBairro', '');
      Emit.EnderEmit.cMun := INIRec.ReadInteger(sSecao, 'cMun', 0);
      Emit.EnderEmit.xMun := INIRec.ReadString(sSecao, 'xMun', '');
      Emit.EnderEmit.CEP := INIRec.ReadInteger(sSecao, 'CEP', 0);
      Emit.EnderEmit.UF := INIRec.ReadString(sSecao, 'UF', '');
      Emit.EnderEmit.fone := INIRec.ReadString(sSecao, 'fone', '');
      Emit.EnderEmit.email := INIRec.ReadString(sSecao, 'email', '');

      Ide.cUF := INIRec.ReadInteger(sSecao, 'cUF', UFparaCodigoUF(Emit.EnderEmit.UF));
      Ide.cMunFG := INIRec.ReadInteger(sSecao, 'cMunFG', Emit.EnderEmit.cMun);

      sSecao := 'dest';
      Dest.xNome := INIRec.ReadString(sSecao, 'xNome', '');
      Dest.CNPJCPF := INIRec.ReadString(sSecao, 'CNPJCPF', '');
      Dest.idOutros := INIRec.ReadString(sSecao, 'idOutros','');
      Dest.indIEDest := StrToindIEDest(INIRec.ReadString(sSecao, 'indIEDest', '1'));
      Dest.IE := INIRec.ReadString(sSecao, 'IE', '');
      Dest.IM := INIRec.ReadString(sSecao, 'IM', '');
      Dest.cNIS := INIRec.ReadString(sSecao, 'cNIS', '');
      Dest.NB := INIRec.ReadString(sSecao, 'NB', '');
      Dest.xNomeAdicional := INIRec.ReadString(sSecao, 'xNomeAdicional', '');
      // Endereço do Destinatario
      Dest.EnderDest.xLgr := INIRec.ReadString(sSecao, 'xLgr', '');
      Dest.EnderDest.nro := INIRec.ReadString(sSecao, 'nro', '');
      Dest.EnderDest.xCpl := INIRec.ReadString(sSecao, 'xCpl', '');
      Dest.EnderDest.xBairro := INIRec.ReadString(sSecao, 'xBairro', '');
      Dest.EnderDest.cMun := INIRec.ReadInteger(sSecao, 'cMun', 0);
      Dest.EnderDest.xMun := INIRec.ReadString(sSecao, 'xMun', '');
      Dest.EnderDest.CEP := INIRec.ReadInteger(sSecao, 'CEP', 0);
      Dest.EnderDest.UF := INIRec.ReadString(sSecao, 'UF', '');
      Dest.EnderDest.fone := INIRec.ReadString(sSecao, 'fone', '');
      Dest.EnderDest.email := INIRec.ReadString(sSecao, 'email', '');

      sSecao := 'acessante';
      acessante.idAcesso := INIRec.ReadString(sSecao, 'idAcesso', '');
      acessante.idCodCliente := INIRec.ReadString(sSecao, 'idCodCliente', '');
      acessante.tpAcesso := StrTotpAcesso(INIRec.ReadString(sSecao, 'tpAcesso', '0'));
      acessante.xNomeUC := INIRec.ReadString(sSecao, 'xNomeUC', '');
      acessante.tpClasse := StrTotpClasse(INIRec.ReadString(sSecao, 'tpClasse', '01'));
      acessante.tpSubClasse := StrTotpSubClasse(INIRec.ReadString(sSecao, 'tpSubClasse', '01'));
      acessante.tpFase := StrTotpFase(INIRec.ReadString(sSecao, 'tpFase', '1'));
      acessante.tpGrpTensao := StrTotpGrpTensao(INIRec.ReadString(sSecao, 'tpGrpTensao', '01'));
      acessante.tpModTar := StrTotpModTar(INIRec.ReadString(sSecao, 'tpModTar', '01'));
      acessante.latGPS := INIRec.ReadString(sSecao, 'latGPS', '');
      acessante.longGPS := INIRec.ReadString(sSecao, 'longGPS', '');
      acessante.codRoteiroLeitura := INIRec.ReadString(sSecao, 'codRoteiroLeitura', '');

      sSecao := 'gSub';
      gSub.chNF3e := INIRec.ReadString(sSecao, 'chNF3e', '');
      gSub.motSub := StrToMotSub(INIRec.ReadString(sSecao, 'motSub', '1'));
      gSub.CNPJ := INIRec.ReadString(sSecao, 'CNPJ', '');
//      gSub.Modelo := INIRec.ReadInteger(sSecao, 'Modelo', 0);
      gSub.Serie := INIRec.ReadString(sSecao, 'Serie', '');
      gSub.nNF := INIRec.ReadInteger(sSecao, 'nNF', 0);
      gSub.CompetEmis := StringToDateTime(INIRec.ReadString(sSecao, 'CompetEmis', '0'));
      gSub.CompetApur := StringToDateTime(INIRec.ReadString(sSecao, 'CompetApur', '0'));
      gSub.hash115 := INIRec.ReadString(sSecao, 'hash115', '');

      sSecao := 'gJudic';
      gJudic.chNF3e := INIRec.ReadString(sSecao, 'chNF3e', '');

      i := 1;
      while true do
      begin
        sSecao := 'gGrContrat' + IntToStrZero(i, 2);
        sFim := OnlyNumber(INIRec.ReadString(sSecao, 'qUnidContrat', 'FIM'));
        if (sFim = 'FIM') or (Length(sFim) <= 0) then
          break;

        ItemgGrContrat := gGrContrat.New;

        ItemgGrContrat.nContrat := INIRec.ReadInteger(sSecao, 'nContrat', 0);
        ItemgGrContrat.tpGrContrat := StrTotpGrContrat(INIRec.ReadString(sSecao, 'tpGrContrat', '1'));
        ItemgGrContrat.tpPosTar := StrTotpPosTar(INIRec.ReadString(sSecao, 'tpPosTar', '0'));
        ItemgGrContrat.qUnidContrat := StringToFloatDef(sFim, 0);

        Inc(i);
      end;

      i := 1;
      while true do
      begin
        sSecao := 'gMed' + IntToStrZero(i, 2);
        sFim := OnlyNumber(INIRec.ReadString(sSecao, 'idMedidor', 'FIM'));
        if (sFim = 'FIM') or (Length(sFim) <= 0) then
          break;

        ItemgMed := gMed.New;

        ItemgMed.nMed := INIRec.ReadInteger(sSecao, 'nMed', 0);
        ItemgMed.idMedidor := sFim;
        ItemgMed.dMedAnt := StringToDateTime(INIRec.ReadString(sSecao, 'dMedAnt', '0'));
        ItemgMed.dMedAtu := StringToDateTime(INIRec.ReadString(sSecao, 'dMedAtu', '0'));

        Inc(i);
      end;

      sSecao := 'gSCEE';
      gSCEE.tpPartComp := StrTotpPartComp(INIRec.ReadString(sSecao, 'tpPartComp', '1'));

      // as 3 seções abaixo são listas
      sSecao := 'gConsumidor';
      sSecao := 'gSaldo';
      sSecao := 'gTipoSaldo';

      i := 1;
      while true do
      begin
        sSecao := 'NFdet' + IntToStrZero(i, 2);
        if not INIRec.SectionExists(sSecao) then
          break;

        ItemNFDet := NFDet.New;

//        with NFDet.New do
//        begin
        ItemNFDet.chNF3eAnt := INIRec.ReadString(sSecao, 'chNF3eAnt', '');
        ItemNFDet.mod6HashAnt := INIRec.ReadString(sSecao, 'mod6HashAnt', '');

        j := 1;
        while true do
        begin
          sSecao := 'det' + IntToStrZero(i, 2) + IntToStrZero(j, 3);
          if not INIRec.SectionExists(sSecao) then
            break;

          ItemDet := ItemNFDet.Det.New;

          ItemDet.nItem := INIRec.ReadInteger(sSecao, 'nItem', 0);

          ItemDet.gAjusteNF3eAnt.tpAjuste:= StrTotpAjuste(INIRec.ReadString(sSecao, 'tpAjuste', '0'));
          ItemDet.gAjusteNF3eAnt.motAjuste:= StrToMotAjuste(INIRec.ReadString(sSecao, 'motAjuste', '1'));

          ItemDet.detItemAnt.nItemAnt := INIRec.ReadInteger(sSecao, 'nItemAnt', 0);
          ItemDet.detItemAnt.vItem := StringToFloatDef(INIRec.ReadString(sSecao, 'vItem', ''), 0);
          ItemDet.detItemAnt.qFaturada := StringToFloatDef(INIRec.ReadString(sSecao, 'qFaturada', ''), 0);
          ItemDet.detItemAnt.vProd := StringToFloatDef(INIRec.ReadString(sSecao, 'vProd', ''), 0);
          ItemDet.detItemAnt.cClass := INIRec.ReadInteger(sSecao, 'cClass', 0);
          ItemDet.detItemAnt.vBC := StringToFloatDef(INIRec.ReadString(sSecao, 'vBC', ''), 0);
          ItemDet.detItemAnt.pICMS := StringToFloatDef(INIRec.ReadString(sSecao, 'pICMS', ''), 0);
          ItemDet.detItemAnt.vICMS := StringToFloatDef(INIRec.ReadString(sSecao, 'vICMS', ''), 0);
          ItemDet.detItemAnt.vFCP := StringToFloatDef(INIRec.ReadString(sSecao, 'vFCP', ''), 0);
          ItemDet.detItemAnt.vBCST := StringToFloatDef(INIRec.ReadString(sSecao, 'vBCST', ''), 0);
          ItemDet.detItemAnt.vICMSST := StringToFloatDef(INIRec.ReadString(sSecao, 'vICMSST', ''), 0);
          ItemDet.detItemAnt.vFCPST := StringToFloatDef(INIRec.ReadString(sSecao, 'vFCPST', ''), 0);
          ItemDet.detItemAnt.vPIS := StringToFloatDef(INIRec.ReadString(sSecao, 'vPIS', ''), 0);
          ItemDet.detItemAnt.vPISEfet := StringToFloatDef(INIRec.ReadString(sSecao, 'vPISEfet', ''), 0);
          ItemDet.detItemAnt.vCOFINS := StringToFloatDef(INIRec.ReadString(sSecao, 'vCOFINS', ''), 0);
          ItemDet.detItemAnt.vCOFINSEfet := StringToFloatDef(INIRec.ReadString(sSecao, 'vCOFINSEfet', ''), 0);
          ItemDet.detItemAnt.indDevolucao := StrToTIndicador(INIRec.ReadString(sSecao, 'indDevolucao', '0'));

          ItemDet.detItemAnt.retTrib.vRetPIS := StringToFloatDef(INIRec.ReadString(sSecao, 'vRetPIS', ''), 0);
          ItemDet.detItemAnt.retTrib.vRetCOFINS := StringToFloatDef(INIRec.ReadString(sSecao, 'vRetCOFINS', ''), 0);
          ItemDet.detItemAnt.retTrib.vRetCSLL := StringToFloatDef(INIRec.ReadString(sSecao, 'vRetCSLL', ''), 0);
          ItemDet.detItemAnt.retTrib.vBCIRRF := StringToFloatDef(INIRec.ReadString(sSecao, 'vBCIRRF', ''), 0);
          ItemDet.detItemAnt.retTrib.vIRRF := StringToFloatDef(INIRec.ReadString(sSecao, 'vIRRF', ''), 0);

          Inc(j);
        end;
        (*
          chNFComAnt := INIRec.ReadString(sSecao, 'chNFComAnt', '');
          nItemAnt := INIRec.ReadInteger(sSecao, 'nItemAnt', 0);
          infAdProd := INIRec.ReadString(sSecao, 'infAdProd', '');

          Prod.cProd := INIRec.ReadString(sSecao, 'cProd', '');
          Prod.xProd := INIRec.ReadString(sSecao, 'xProd', '');
          Prod.cClass := INIRec.ReadString(sSecao, 'cClass', '');
          Prod.CFOP := INIRec.ReadInteger(sSecao, 'CFOP', 0);
          Prod.CNPJLD := INIRec.ReadString(sSecao, 'CNPJLD', '');
          Prod.uMed:= StrTouMed(INIRec.ReadString(sSecao, 'uMed', ''));
          Prod.qFaturada := StringToFloatDef(INIRec.ReadString(sSecao, 'qFaturada', ''), 0);
          Prod.vItem := StringToFloatDef(INIRec.ReadString(sSecao, 'vItem', ''), 0);
          Prod.vDesc := StringToFloatDef(INIRec.ReadString(sSecao, 'vDesc', ''), 0);
          Prod.vOutro := StringToFloatDef(INIRec.ReadString(sSecao, 'vOutro', ''), 0);
          Prod.vProd := StringToFloatDef(INIRec.ReadString(sSecao, 'vProd', ''), 0);
          Prod.dExpiracao := StringToDateTime(INIRec.ReadString(sSecao, 'dExpiracao', '0'));
          Prod.indDevolucao := StrToTIndicador(INIRec.ReadString(sSecao, 'indDevolucao', '0'));

          with Imposto do
          begin
            sSecao := 'ICMS' + IntToStrZero(i, 3);
            sFim := INIRec.ReadString(sSecao, 'CST', 'FIM');

            if ((sFim <> 'FIM') and (Length(sFim) > 0)) then
            begin
              indSemCST := StrToTIndicador(INIRec.ReadString(sSecao, 'indSemCST', '0'));

              with ICMS do
              begin
                CST := StrToCSTICMS(INIRec.ReadString(sSecao, 'CST', '00'));
                vBC := StringToFloatDef(INIRec.ReadString(sSecao, 'vBC', ''), 0);
                pICMS := StringToFloatDef(INIRec.ReadString(sSecao, 'pICMS', ''), 0);
                vICMS := StringToFloatDef(INIRec.ReadString(sSecao, 'vICMS', ''), 0);
                pFCP := StringToFloatDef(INIRec.ReadString(sSecao, 'pFCP', ''), 0);
                vFCP := StringToFloatDef(INIRec.ReadString(sSecao, 'vFCP', ''), 0);
                pRedBC := StringToFloatDef(INIRec.ReadString(sSecao, 'pRedBC', ''), 0);
                vICMSDeson := StringToFloatDef(INIRec.ReadString(sSecao, 'vICMSDeson', ''), 0);
                cBenef := INIRec.ReadString(sSecao,'cBenef', '');
              end;
            end;

            j := 1;
            while true do
            begin
              sSecao := 'ICMSUFDest' + IntToStrZero(i, 3) + IntToStrZero(j, 3);
              sFim := INIRec.ReadString(sSecao, 'vBCUFDest', 'FIM');
              if sFim = 'FIM' then
                break;

              with ICMSUFDest.New do
              begin
                cUFDest := INIRec.ReadInteger(sSecao, 'cUFDest', 0);
                vBCUFDest := StringToFloatDef(INIRec.ReadString(sSecao, 'vBCUFDest', ''), 0);
                pFCPUFDest := StringToFloatDef(INIRec.ReadString(sSecao, 'pFCPUFDest', ''), 0);
                pICMSUFDest := StringToFloatDef(INIRec.ReadString(sSecao, 'pICMSUFDest', ''), 0);
                pICMSInter := StringToFloatDef(INIRec.ReadString(sSecao, 'pICMSInter', ''), 0);
                vFCPUFDest := StringToFloatDef(INIRec.ReadString(sSecao, 'vFCPUFDest', ''), 0);
                vICMSUFDest := StringToFloatDef(INIRec.ReadString(sSecao, 'vICMSUFDest', ''), 0);
                vICMSUFEmi := StringToFloatDef(INIRec.ReadString(sSecao, 'vICMSUFEmi', ''), 0);
                cBenefUFDest := INIRec.ReadString(sSecao, 'cBenefUFDest', '');
              end;

              Inc(j);
            end;

            sSecao := 'PIS' + IntToStrZero(i, 3);
            sFim := INIRec.ReadString(sSecao, 'CST', 'FIM');

            if ((sFim <> 'FIM') and (Length(sFim) > 0)) then
            begin
              with PIS do
              begin
                CST := StrToCSTPIS(sFim);
                vBC := StringToFloatDef(INIRec.ReadString(sSecao,'vBC', ''), 0);
                pPIS := StringToFloatDef(INIRec.ReadString(sSecao,'pPIS', ''), 0);
                vPIS := StringToFloatDef(INIRec.ReadString(sSecao,'vPIS', ''), 0);
              end;
            end;

            sSecao := 'COFINS'+ IntToStrZero(i, 3);
            sFim := INIRec.ReadString(sSecao, 'CST', 'FIM');

            if ((sFim <> 'FIM') and (Length(sFim) > 0)) then
            begin
              with COFINS do
              begin
                CST := StrToCSTCOFINS(sFim);
                vBC := StringToFloatDef(INIRec.ReadString(sSecao,'vBC', ''), 0);
                pCOFINS := StringToFloatDef(INIRec.ReadString(sSecao,'pCOFINS', ''), 0);
                vCOFINS := StringToFloatDef(INIRec.ReadString(sSecao,'vCOFINS', ''), 0);
              end;
            end;

            sSecao := 'FUST'+ IntToStrZero(i, 3);
            sFim := INIRec.ReadString(sSecao, 'vBC', 'FIM');

            if ((sFim <> 'FIM') and (Length(sFim) > 0)) then
            begin
              with FUST do
              begin
                vBC := StringToFloatDef(sFim, 0);
                pFUST := StringToFloatDef(INIRec.ReadString(sSecao,'pFUST', ''), 0);
                vFUST := StringToFloatDef(INIRec.ReadString(sSecao,'vFUST', ''), 0);
              end;
            end;

            sSecao := 'FUNTTEL'+ IntToStrZero(i, 3);
            sFim := INIRec.ReadString(sSecao, 'vBC', 'FIM');

            if ((sFim <> 'FIM') and (Length(sFim) > 0)) then
            begin
              with FUNTTEL do
              begin
                vBC := StringToFloatDef(sFim, 0);
                pFUNTTEL := StringToFloatDef(INIRec.ReadString(sSecao,'pFUNTTEL', ''), 0);
                vFUNTTEL := StringToFloatDef(INIRec.ReadString(sSecao,'vFUNTTEL', ''), 0);
              end;
            end;

            sSecao := 'retTrib'+ IntToStrZero(i, 3);
            sFim := INIRec.ReadString(sSecao, 'vRetPIS', 'FIM');

            if ((sFim <> 'FIM') and (Length(sFim) > 0)) then
            begin
              with retTrib do
              begin
                vRetPIS := StringToFloatDef(sFim, 0);
                vRetCOFINS := StringToFloatDef(INIRec.ReadString(sSecao,'vRetCOFINS', ''), 0);
                vRetCSLL := StringToFloatDef(INIRec.ReadString(sSecao,'vRetCSLL', ''), 0);
                vBCIRRF := StringToFloatDef(INIRec.ReadString(sSecao,'vBCIRRF', ''), 0);
                vIRRF := StringToFloatDef(INIRec.ReadString(sSecao,'vIRRF', ''), 0);
              end;
            end;
          end;

          sSecao := 'gProcRef' + IntToStrZero(i, 3);
          sFim := INIRec.ReadString(sSecao, 'vItem', 'FIM');

          if ((sFim <> 'FIM') and (Length(sFim) > 0)) then
          begin
            with gProcRef do
            begin
              vItem := StringToFloatDef(INIRec.ReadString(sSecao,'vItem', ''), 0);
              qFaturada := INIRec.ReadInteger(sSecao,'qFaturada', 0);
              vProd := StringToFloatDef(INIRec.ReadString(sSecao,'vProd', ''), 0);
              vDesc := StringToFloatDef(INIRec.ReadString(sSecao,'vDesc', ''), 0);
              vOutro := StringToFloatDef(INIRec.ReadString(sSecao,'vOutro', ''), 0);
              indDevolucao := StrToTIndicador(INIRec.ReadString(sSecao, 'indDevolucao', '0'));
              vBC := StringToFloatDef(INIRec.ReadString(sSecao,'vBC', ''), 0);
              pICMS := StringToFloatDef(INIRec.ReadString(sSecao,'pICMS', ''), 0);
              vICMS := StringToFloatDef(INIRec.ReadString(sSecao,'vICMS', ''), 0);
              vPIS := StringToFloatDef(INIRec.ReadString(sSecao,'vPIS', ''), 0);
              vCOFINS := StringToFloatDef(INIRec.ReadString(sSecao,'vCOFINS', ''), 0);

              j := 1;
              while true do
              begin
                sSecao := 'gProc' + IntToStrZero(i, 3) + IntToStrZero(j, 2);
                sFim := INIRec.ReadString(sSecao, 'nProcesso', 'FIM');
                if sFim = 'FIM' then
                  break;

                with gProc.New do
                begin
                  tpProc := StrTotpProc(INIRec.ReadString(sSecao, 'tpProc', '0'));
                  nProcesso := sFim;
                end;

                Inc(j);
              end;
            end;
          end;

          sSecao := 'gRessarc' + IntToStrZero(i, 3);
          sFim := INIRec.ReadString(sSecao, 'dRef', 'FIM');

          if ((sFim <> 'FIM') and (Length(sFim) > 0)) then
          begin
            with gRessarc do
            begin
              tpRessarc := StrTotpRessarc(INIRec.ReadString(sSecao, 'tpRessarc', '0'));
              dRef := StringToDateTime(INIRec.ReadString(sSecao, 'dRef', '0'));
              nProcesso := INIRec.ReadString(sSecao,'nProcesso', '');
              nProtReclama := INIRec.ReadString(sSecao,'nProtReclama', '');
              xObs := INIRec.ReadString(sSecao,'xObs', '');
            end;
          end;
        end;
        *)
        Inc(i);
      end;

      (*
      sSecao := 'total';
      Total.vProd := StringToFloatDef(INIRec.ReadString(sSecao,'vProd', ''), 0);
      Total.vBC := StringToFloatDef(INIRec.ReadString(sSecao,'vBC', ''), 0);
      Total.vICMS := StringToFloatDef(INIRec.ReadString(sSecao,'vICMS', ''), 0);
      Total.vICMSDeson := StringToFloatDef(INIRec.ReadString(sSecao,'vICMSDeson', ''), 0);
      Total.vFCP := StringToFloatDef(INIRec.ReadString(sSecao,'vFCP', ''), 0);
      Total.vCOFINS := StringToFloatDef(INIRec.ReadString(sSecao,'vCOFINS', ''), 0);
      Total.vPIS := StringToFloatDef(INIRec.ReadString(sSecao,'vPIS', ''), 0);
      Total.vFUNTTEL := StringToFloatDef(INIRec.ReadString(sSecao,'vFUNTTEL', ''), 0);
      Total.vFUST := StringToFloatDef(INIRec.ReadString(sSecao,'vFUST', ''), 0);
      Total.vRetPIS := StringToFloatDef(INIRec.ReadString(sSecao,'vRetPIS', ''), 0);
      Total.vRetCOFINS := StringToFloatDef(INIRec.ReadString(sSecao,'vRetCOFINS', ''), 0);
      Total.vRetCSLL := StringToFloatDef(INIRec.ReadString(sSecao,'vRetCSLL', ''), 0);
      Total.vIRRF := StringToFloatDef(INIRec.ReadString(sSecao,'vIRRF', ''), 0);
      Total.vDesc := StringToFloatDef(INIRec.ReadString(sSecao,'vDesc', ''), 0);
      Total.vOutro := StringToFloatDef(INIRec.ReadString(sSecao,'vOutro', ''), 0);
      Total.vNF := StringToFloatDef(INIRec.ReadString(sSecao,'vNF', ''), 0);

      sSecao := 'gFidelidade';
      gFidelidade.qtdSaldoPts := INIRec.ReadString(sSecao, 'qtdSaldoPts', '');
      gFidelidade.dRefSaldoPts := StringToDateTime(INIRec.ReadString(sSecao, 'dRefSaldoPts', '0'));
      gFidelidade.qtdPtsResg := INIRec.ReadString(sSecao, 'qtdPtsResg', '');
      gFidelidade.dRefResgPts := StringToDateTime(INIRec.ReadString(sSecao, 'dRefResgPts', '0'));

      sSecao := 'gFat';
      gFat.CompetFat := StringToDateTime(INIRec.ReadString(sSecao, 'CompetFat', '0'));
      gFat.dVencFat := StringToDateTime(INIRec.ReadString(sSecao, 'dVencFat', '0'));
      gFat.dPerUsoIni := StringToDateTime(INIRec.ReadString(sSecao, 'dPerUsoIni', '0'));
      gFat.dPerUsoFim := StringToDateTime(INIRec.ReadString(sSecao, 'dPerUsoFim', '0'));
      gFat.codBarras := INIRec.ReadString(sSecao, 'codBarras', '');
      gFat.codDebAuto := INIRec.ReadString(sSecao, 'codDebAuto', '');
      gFat.codBanco := INIRec.ReadString(sSecao, 'codBanco', '');
      gFat.codAgencia := INIRec.ReadString(sSecao, 'codAgencia', '');
      // Endereço do Destinatario
      gFat.enderCorresp.xLgr := INIRec.ReadString(sSecao, 'xLgr', '');
      gFat.enderCorresp.nro := INIRec.ReadString(sSecao, 'nro', '');
      gFat.enderCorresp.xCpl := INIRec.ReadString(sSecao, 'xCpl', '');
      gFat.enderCorresp.xBairro := INIRec.ReadString(sSecao, 'xBairro', '');
      gFat.enderCorresp.cMun := INIRec.ReadInteger(sSecao, 'cMun', 0);
      gFat.enderCorresp.xMun := INIRec.ReadString(sSecao, 'xMun', '');
      gFat.enderCorresp.CEP := INIRec.ReadInteger(sSecao, 'CEP', 0);
      gFat.enderCorresp.UF := INIRec.ReadString(sSecao, 'UF', '');
      gFat.enderCorresp.fone := INIRec.ReadString(sSecao, 'fone', '');
      gFat.enderCorresp.email := INIRec.ReadString(sSecao, 'email', '');
      // Chave PIX
      gFat.gPIX.urlQRCodePIX := INIRec.ReadString(sSecao, 'urlQRCodePIX', '');

      sSecao := 'gFatCentral';
      gFatCentral.CNPJ := INIRec.ReadString(sSecao, 'CNPJ', '');
      gFatCentral.cUF := INIRec.ReadInteger(sSecao, 'cUF', 0);

      i := 1;
      while true do
      begin
        sSecao := 'autXML' + IntToStrZero(i, 2);
        sFim := OnlyNumber(INIRec.ReadString(sSecao, 'CNPJCPF', 'FIM'));
        if (sFim = 'FIM') or (Length(sFim) <= 0) then
          break;

        with autXML.New do
          CNPJCPF := sFim;

        Inc(i);
      end;

      sSecao := 'infAdic';
      InfAdic.infAdFisco := INIRec.ReadString(sSecao, 'infAdFisco', '');
      // Vai ser alterado pois é uma lista
      InfAdic.infCpl := INIRec.ReadString(sSecao,'infCpl', '');

      sSecao := 'infRespTec';
      if INIRec.SectionExists(sSecao) then
      begin
        with infRespTec do
        begin
          CNPJ := INIRec.ReadString(sSecao, 'CNPJ', '');
          xContato := INIRec.ReadString(sSecao, 'xContato', '');
          email := INIRec.ReadString(sSecao, 'email', '');
          fone := INIRec.ReadString(sSecao, 'fone', '');
        end;
      end;
      *)
    end;

    GerarXML;

    Result := True;
  finally
    INIRec.Free;
  end;
end;

function TNotaFiscal.GerarNF3eIni: String;
var
  INIRec: TMemIniFile;
  IniNF3e: TStringList;
//  I, J, K: integer;
//  sSecao: string;
begin
  Result := '';

  if not ValidarChave(NF3e.infNF3e.ID) then
    raise EACBrNF3eException.Create('NF3e Inconsistente para gerar INI. Chave Inválida.');

  INIRec := TMemIniFile.Create('');
  try
    with FNF3e do
    begin
      INIRec.WriteString('infNF3e', 'ID', infNF3e.ID);
      INIRec.WriteString('infNF3e', 'Versao', FloatToStr(infNF3e.Versao));
      INIRec.WriteInteger('Identificacao', 'cUF', Ide.cUF);
      INIRec.WriteInteger('Identificacao', 'Codigo', Ide.cNF);
      INIRec.WriteInteger('Identificacao', 'Modelo', Ide.modelo);
      INIRec.WriteInteger('Identificacao', 'Serie', Ide.serie);
      INIRec.WriteInteger('Identificacao', 'nNF', Ide.nNF);
      INIRec.WriteString('Identificacao', 'dhEmi', DateTimeToStr(Ide.dhEmi));
      INIRec.WriteInteger('Identificacao', 'cMunFG', Ide.cMunFG);
      INIRec.WriteString('Identificacao', 'tpAmb', TipoAmbienteToStr(Ide.tpAmb));
      INIRec.WriteString('Identificacao', 'tpemis', TipoEmissaoToStr(Ide.tpemis));
      INIRec.WriteString('Identificacao', 'finNF3e', FinNF3eToStr(Ide.finNF3e));
      INIRec.WriteString('Identificacao', 'verProc', Ide.verProc);
      INIRec.WriteString('Identificacao', 'dhCont', DateToStr(Ide.dhCont));
      INIRec.WriteString('Identificacao', 'xJust', Ide.xJust);
      {
      for I := 0 to Ide.NFref.Count - 1 do
      begin
        with Ide.NFref.Items[i] do
        begin
          sSecao := 'NFRef' + IntToStrZero(I + 1, 3);
          if trim(refNF3e) <> '' then
          begin
            INIRec.WriteString(sSecao, 'Tipo', 'NF3e');
            INIRec.WriteString(sSecao, 'refNF3e', refNF3e);
          end
          else if trim(RefNF.CNPJ) <> '' then
          begin
            INIRec.WriteString(sSecao, 'Tipo', 'NF');
            INIRec.WriteInteger(sSecao, 'cUF', RefNF.cUF);
            INIRec.WriteString(sSecao, 'AAMM', RefNF.AAMM);
            INIRec.WriteString(sSecao, 'CNPJ', RefNF.CNPJ);
            INIRec.WriteInteger(sSecao, 'Modelo', RefNF.modelo);
            INIRec.WriteInteger(sSecao, 'Serie', RefNF.serie);
            INIRec.WriteInteger(sSecao, 'nNF', RefNF.nNF);
          end
          else if trim(RefNFP.CNPJCPF) <> '' then
          begin
            INIRec.WriteString(sSecao, 'Tipo', 'NFP');
            INIRec.WriteInteger(sSecao, 'cUF', RefNFP.cUF);
            INIRec.WriteString(sSecao, 'AAMM', RefNFP.AAMM);
            INIRec.WriteString(sSecao, 'CNPJ', RefNFP.CNPJCPF);
            INIRec.WriteString(sSecao, 'IE', RefNFP.IE);
            INIRec.WriteString(sSecao, 'Modelo', RefNFP.modelo);
            INIRec.WriteInteger(sSecao, 'Serie', RefNFP.serie);
            INIRec.WriteInteger(sSecao, 'nNF', RefNFP.nNF);
          end
          else if trim(refCTe) <> '' then
          begin
            INIRec.WriteString(sSecao, 'Tipo', 'CTe');
            INIRec.WriteString(sSecao, 'reCTe', refCTe);
          end
          else if trim(RefECF.nCOO) <> '' then
          begin
            INIRec.WriteString(sSecao, 'Tipo', 'ECF');
            INIRec.WriteString(sSecao, 'modelo', ECFModRefToStr(RefECF.modelo));
            INIRec.WriteString(sSecao, 'nECF', RefECF.nECF);
            INIRec.WriteString(sSecao, 'nCOO', RefECF.nCOO);
          end;
        end;
      end;

      INIRec.WriteString('Emitente', 'CNPJCPF', Emit.CNPJCPF);
      INIRec.WriteString('Emitente', 'xNome', Emit.xNome);
      INIRec.WriteString('Emitente', 'xFant', Emit.xFant);
      INIRec.WriteString('Emitente', 'IE', Emit.IE);
      INIRec.WriteString('Emitente', 'IEST', Emit.IEST);
      INIRec.WriteString('Emitente', 'IM', Emit.IM);
      INIRec.WriteString('Emitente', 'CNAE', Emit.CNAE);
      INIRec.WriteString('Emitente', 'CRT', CRTToStr(Emit.CRT));
      INIRec.WriteString('Emitente', 'xLgr', Emit.EnderEmit.xLgr);
      INIRec.WriteString('Emitente', 'nro', Emit.EnderEmit.nro);
      INIRec.WriteString('Emitente', 'xCpl', Emit.EnderEmit.xCpl);
      INIRec.WriteString('Emitente', 'xBairro', Emit.EnderEmit.xBairro);
      INIRec.WriteInteger('Emitente', 'cMun', Emit.EnderEmit.cMun);
      INIRec.WriteString('Emitente', 'xMun', Emit.EnderEmit.xMun);
      INIRec.WriteString('Emitente', 'UF', Emit.EnderEmit.UF);
      INIRec.WriteInteger('Emitente', 'CEP', Emit.EnderEmit.CEP);
      INIRec.WriteInteger('Emitente', 'cPais', Emit.EnderEmit.cPais);
      INIRec.WriteString('Emitente', 'xPais', Emit.EnderEmit.xPais);
      INIRec.WriteString('Emitente', 'Fone', Emit.EnderEmit.fone);
      if Avulsa.CNPJ <> '' then
      begin
        INIRec.WriteString('Avulsa', 'CNPJ', Avulsa.CNPJ);
        INIRec.WriteString('Avulsa', 'xOrgao', Avulsa.xOrgao);
        INIRec.WriteString('Avulsa', 'matr', Avulsa.matr);
        INIRec.WriteString('Avulsa', 'xAgente', Avulsa.xAgente);
        INIRec.WriteString('Avulsa', 'fone', Avulsa.fone);
        INIRec.WriteString('Avulsa', 'UF', Avulsa.UF);
        INIRec.WriteString('Avulsa', 'nDAR', Avulsa.nDAR);
        INIRec.WriteString('Avulsa', 'dEmi', DateToStr(Avulsa.dEmi));
        INIRec.WriteFloat('Avulsa', 'vDAR', Avulsa.vDAR);
        INIRec.WriteString('Avulsa', 'repEmi', Avulsa.repEmi);
        INIRec.WriteString('Avulsa', 'dPag', DateToStr(Avulsa.dPag));
      end;
      if (Dest.idEstrangeiro <> EmptyStr) then
        INIRec.WriteString('Destinatario', 'idEstrangeiro', Dest.idEstrangeiro);
      INIRec.WriteString('Destinatario', 'CNPJCPF', Dest.CNPJCPF);
      INIRec.WriteString('Destinatario', 'xNome', Dest.xNome);
      INIRec.WriteString('Destinatario', 'indIEDest', indIEDestToStr(Dest.indIEDest));
      INIRec.WriteString('Destinatario', 'IE', Dest.IE);
      INIRec.WriteString('Destinatario', 'ISUF', Dest.ISUF);
      INIRec.WriteString('Destinatario', 'IM', Dest.IM);
      INIRec.WriteString('Destinatario', 'Email', Dest.Email);
      INIRec.WriteString('Destinatario', 'xLgr', Dest.EnderDest.xLgr);
      INIRec.WriteString('Destinatario', 'nro', Dest.EnderDest.nro);
      INIRec.WriteString('Destinatario', 'xCpl', Dest.EnderDest.xCpl);
      INIRec.WriteString('Destinatario', 'xBairro', Dest.EnderDest.xBairro);
      INIRec.WriteInteger('Destinatario', 'cMun', Dest.EnderDest.cMun);
      INIRec.WriteString('Destinatario', 'xMun', Dest.EnderDest.xMun);
      INIRec.WriteString('Destinatario', 'UF', Dest.EnderDest.UF);
      INIRec.WriteInteger('Destinatario', 'CEP', Dest.EnderDest.CEP);
      INIRec.WriteInteger('Destinatario', 'cPais', Dest.EnderDest.cPais);
      INIRec.WriteString('Destinatario', 'xPais', Dest.EnderDest.xPais);
      INIRec.WriteString('Destinatario', 'Fone', Dest.EnderDest.Fone);
      if Retirada.CNPJCPF <> '' then
      begin
        INIRec.WriteString('Retirada', 'CNPJCPF', Retirada.CNPJCPF);
        INIRec.WriteString('Retirada', 'xLgr', Retirada.xLgr);
        INIRec.WriteString('Retirada', 'nro', Retirada.nro);
        INIRec.WriteString('Retirada', 'xCpl', Retirada.xCpl);
        INIRec.WriteString('Retirada', 'xBairro', Retirada.xBairro);
        INIRec.WriteInteger('Retirada', 'cMun', Retirada.cMun);
        INIRec.WriteString('Retirada', 'xMun', Retirada.xMun);
        INIRec.WriteString('Retirada', 'UF', Retirada.UF);
      end;
      if Entrega.CNPJCPF <> '' then
      begin
        INIRec.WriteString('Entrega', 'CNPJCPF', Entrega.CNPJCPF);
        INIRec.WriteString('Entrega', 'xLgr', Entrega.xLgr);
        INIRec.WriteString('Entrega', 'nro', Entrega.nro);
        INIRec.WriteString('Entrega', 'xCpl', Entrega.xCpl);
        INIRec.WriteString('Entrega', 'xBairro', Entrega.xBairro);
        INIRec.WriteInteger('Entrega', 'cMun', Entrega.cMun);
        INIRec.WriteString('Entrega', 'xMun', Entrega.xMun);
        INIRec.WriteString('Entrega', 'UF', Entrega.UF);
      end;

      for I := 0 to Det.Count - 1 do
      begin
        with Det.Items[I] do
        begin
          sSecao := 'Produto' + IntToStrZero(I + 1, 3);
          INIRec.WriteInteger(sSecao, 'nItem', Prod.nItem);
          INIRec.WriteString(sSecao, 'infAdProd', infAdProd);
          INIRec.WriteString(sSecao, 'cProd', Prod.cProd);
          INIRec.WriteString(sSecao, 'cEAN', Prod.cEAN);
          INIRec.WriteString(sSecao, 'xProd', Prod.xProd);
          INIRec.WriteString(sSecao, 'NCM', Prod.NCM);
          INIRec.WriteString(sSecao, 'CEST', Prod.CEST);
          INIRec.WriteString(sSecao, 'indEscala', IndEscalaToStr(Prod.indEscala));
          INIRec.WriteString(sSecao, 'CNPJFab', Prod.CNPJFab);
          INIRec.WriteString(sSecao, 'cBenef', Prod.cBenef);
          INIRec.WriteString(sSecao, 'EXTIPI', Prod.EXTIPI);
          INIRec.WriteString(sSecao, 'CFOP', Prod.CFOP);
          INIRec.WriteString(sSecao, 'uCom', Prod.uCom);
          INIRec.WriteFloat(sSecao, 'qCom', Prod.qCom);
          INIRec.WriteFloat(sSecao, 'vUnCom', Prod.vUnCom);
          INIRec.WriteFloat(sSecao, 'vProd', Prod.vProd);
          INIRec.WriteString(sSecao, 'cEANTrib', Prod.cEANTrib);
          INIRec.WriteString(sSecao, 'uTrib', Prod.uTrib);
          INIRec.WriteFloat(sSecao, 'qTrib', Prod.qTrib);
          INIRec.WriteFloat(sSecao, 'vUnTrib', Prod.vUnTrib);
          INIRec.WriteFloat(sSecao, 'vFrete', Prod.vFrete);
          INIRec.WriteFloat(sSecao, 'vSeg', Prod.vSeg);
          INIRec.WriteFloat(sSecao, 'vDesc', Prod.vDesc);
          INIRec.WriteFloat(sSecao, 'vOutro', Prod.vOutro);
          INIRec.WriteString(sSecao, 'IndTot', indTotToStr(Prod.IndTot));
          INIRec.WriteString(sSecao, 'xPed', Prod.xPed);
          INIRec.WriteString(sSecao, 'nItemPed', Prod.nItemPed);
          INIRec.WriteString(sSecao, 'nFCI', Prod.nFCI);
          INIRec.WriteString(sSecao, 'nRECOPI', Prod.nRECOPI);
          INIRec.WriteFloat(sSecao, 'pDevol', pDevol);
          INIRec.WriteFloat(sSecao, 'vIPIDevol', vIPIDevol);
          INIRec.WriteFloat(sSecao, 'vTotTrib', Imposto.vTotTrib);
          for J := 0 to Prod.NVE.Count - 1 do
          begin
            if Prod.NVE.Items[J].NVE <> '' then
            begin
              with Prod.NVE.Items[J] do
              begin
                sSecao := 'NVE' + IntToStrZero(I + 1, 3) + IntToStrZero(J + 1, 3);
                INIRec.WriteString(sSecao, 'NVE', NVE);
              end;
            end
            else
              Break;
          end;

          for J := 0 to Prod.rastro.Count - 1 do
          begin
            if Prod.rastro.Items[J].nLote <> '' then
            begin
              with Prod.rastro.Items[J] do
              begin
                sSecao := 'Rastro' + IntToStrZero(I + 1, 3) + IntToStrZero(J + 1, 3);
                INIRec.WriteString(sSecao, 'nLote', nLote);
                INIRec.WriteFloat(sSecao, 'qLote', qLote);
                INIRec.WriteDateTime(sSecao, 'dFab', dFab);
                INIRec.WriteDateTime(sSecao, 'dVal', dVal);
                INIRec.WriteString(sSecao, 'cAgreg', cAgreg);
              end;
            end
            else
              Break;
          end;

          for J := 0 to Prod.DI.Count - 1 do
          begin
            if Prod.DI.Items[j].nDi <> '' then
            begin
              with Prod.DI.Items[j] do
              begin
                sSecao := 'DI' + IntToStrZero(I + 1, 3) + IntToStrZero(J + 1, 3);
                INIRec.WriteString(sSecao, 'nDi', nDi);
                INIRec.WriteString(sSecao, 'dDi', DateToStr(dDi));
                INIRec.WriteString(sSecao, 'xLocDesemb', xLocDesemb);
                INIRec.WriteString(sSecao, 'UFDesemb', UFDesemb);
                INIRec.WriteString(sSecao, 'dDesemb', DateToStr(dDesemb));
                INIRec.WriteString(sSecao, 'cExportador', cExportador);
                if (TipoViaTranspToStr(tpViaTransp) <> '') then
                begin
                  INIRec.WriteString(sSecao, 'tpViaTransp',
                    TipoViaTranspToStr(tpViaTransp));
                  if (tpViaTransp = tvMaritima) then
                    INIRec.WriteFloat(sSecao, 'vAFRMM', vAFRMM);
                end;
                if (TipoIntermedioToStr(tpIntermedio) <> '') then
                begin
                  INIRec.WriteString(sSecao, 'tpIntermedio',
                    TipoIntermedioToStr(tpIntermedio));
                  if not (tpIntermedio = tiContaPropria) then
                  begin
                    INIRec.WriteString(sSecao, 'CNPJ', CNPJ);
                    INIRec.WriteString(sSecao, 'UFTerceiro', UFTerceiro);
                  end;
                end;
                for K := 0 to adi.Count - 1 do
                begin
                  with adi.Items[K] do
                  begin
                    sSecao :=
                      'LADI' + IntToStrZero(I + 1, 3) + IntToStrZero(J + 1, 3) + IntToStrZero(K + 1, 3);
                    INIRec.WriteInteger(sSecao, 'nAdicao', nAdicao);
                    INIRec.WriteInteger(sSecao, 'nSeqAdi', nSeqAdi);
                    INIRec.WriteString(sSecao, 'cFabricante', cFabricante);
                    INIRec.WriteFloat(sSecao, 'vDescDI', vDescDI);
                    INIRec.WriteString(sSecao, 'nDraw', nDraw);
                  end;
                end;
              end;
            end
            else
              Break;
          end;
          for J := 0 to Prod.detExport.Count - 1 do
          begin
            if Prod.detExport.Items[j].nDraw <> '' then
            begin
              with Prod.detExport.Items[j] do
              begin
                sSecao := 'detExport' + IntToStrZero(I + 1, 3) + IntToStrZero(J + 1, 3);
                INIRec.WriteString(sSecao, 'nDraw', nDraw);
                INIRec.WriteString(sSecao, 'nRe', nRE);
                INIRec.WriteString(sSecao, 'chNF3e', chNF3e);
                INIRec.WriteFloat(sSecao, 'qExport', qExport);
              end;
            end;
          end;

          if (pDevol > 0) then
          begin
            sSecao := 'impostoDevol' + IntToStrZero(I + 1, 3);
            INIRec.WriteFloat(sSecao, 'pDevol', pDevol);
            INIRec.WriteFloat(sSecao, 'vIPIDevol', vIPIDevol);
          end;

          if Prod.veicProd.chassi <> '' then
          begin
            sSecao := 'Veiculo' + IntToStrZero(I + 1, 3);
            with Prod.veicProd do
            begin
              INIRec.WriteString(sSecao, 'tpOP', tpOPToStr(tpOP));
              INIRec.WriteString(sSecao, 'Chassi', chassi);
              INIRec.WriteString(sSecao, 'cCor', cCor);
              INIRec.WriteString(sSecao, 'xCor', xCor);
              INIRec.WriteString(sSecao, 'pot', pot);
              INIRec.WriteString(sSecao, 'Cilin', Cilin);
              INIRec.WriteString(sSecao, 'pesoL', pesoL);
              INIRec.WriteString(sSecao, 'pesoB', pesoB);
              INIRec.WriteString(sSecao, 'nSerie', nSerie);
              INIRec.WriteString(sSecao, 'tpComb', tpComb);
              INIRec.WriteString(sSecao, 'nMotor', nMotor);
              INIRec.WriteString(sSecao, 'CMT', CMT);
              INIRec.WriteString(sSecao, 'dist', dist);
              INIRec.WriteInteger(sSecao, 'anoMod', anoMod);
              INIRec.WriteInteger(sSecao, 'anoFab', anoFab);
              INIRec.WriteString(sSecao, 'tpPint', tpPint);
              INIRec.WriteInteger(sSecao, 'tpVeic', tpVeic);
              INIRec.WriteInteger(sSecao, 'espVeic', espVeic);
              INIRec.WriteString(sSecao, 'VIN', VIN);
              INIRec.WriteString(sSecao, 'condVeic', condVeicToStr(condVeic));
              INIRec.WriteString(sSecao, 'cMod', cMod);
              INIRec.WriteString(sSecao, 'cCorDENATRAN', cCorDENATRAN);
              INIRec.WriteInteger(sSecao, 'lota', lota);
              INIRec.WriteInteger(sSecao, 'tpRest', tpRest);
            end;
          end;
          for J := 0 to Prod.med.Count - 1 do
          begin
            sSecao := 'Medicamento' + IntToStrZero(I + 1, 3) + IntToStrZero(J + 1, 3);
            with Prod.med.Items[J] do
            begin
              if NF3e.infNF3e.Versao >= 4 then
              begin
                INIRec.WriteString(sSecao, 'cProdANVISA', cProdANVISA);
                INIRec.WriteString(sSecao, 'xMotivoIsencao', xMotivoIsencao);
              end;

              if NF3e.infNF3e.Versao < 4 then
              begin
                INIRec.WriteString(sSecao, 'nLote', nLote);
                INIRec.WriteFloat(sSecao, 'qLote', qLote);
                INIRec.WriteString(sSecao, 'dFab', DateToStr(dFab));
                INIRec.WriteString(sSecao, 'dVal', DateToStr(dVal));
              end;

              INIRec.WriteFloat(sSecao, 'vPMC', vPMC);
            end;
          end;
          for J := 0 to Prod.arma.Count - 1 do
          begin
            sSecao := 'Arma' + IntToStrZero(I + 1, 3) + IntToStrZero(J + 1, 3);
            with Prod.arma.Items[J] do
            begin
              INIRec.WriteString(sSecao, 'tpArma', tpArmaToStr(tpArma));
              INIRec.WriteString(sSecao, 'nSerie', nSerie);
              INIRec.WriteString(sSecao, 'nCano', nCano);
              INIRec.WriteString(sSecao, 'descr', descr);
            end;
          end;
          if (Prod.comb.cProdANP > 0) then
          begin
            sSecao := 'Combustivel' + IntToStrZero(I + 1, 3);
            with Prod.comb do
            begin
              INIRec.WriteInteger(sSecao, 'cProdANP', cProdANP);
              INIRec.WriteFloat(sSecao, 'pMixGN', pMixGN);
              INIRec.WriteString(sSecao, 'descANP', descANP);
              INIRec.WriteFloat(sSecao, 'pGLP', pGLP);
              INIRec.WriteFloat(sSecao, 'pGNn', pGNn);
              INIRec.WriteFloat(sSecao, 'pGNi', pGNi);
              INIRec.WriteFloat(sSecao, 'vPart', vPart);
              INIRec.WriteString(sSecao, 'CODIF', CODIF);
              INIRec.WriteFloat(sSecao, 'qTemp', qTemp);
              INIRec.WriteString(sSecao, 'UFCons', UFcons);
              sSecao := 'CIDE' + IntToStrZero(I + 1, 3);
              INIRec.WriteFloat(sSecao, 'qBCprod', CIDE.qBCprod);
              INIRec.WriteFloat(sSecao, 'vAliqProd', CIDE.vAliqProd);
              INIRec.WriteFloat(sSecao, 'vCIDE', CIDE.vCIDE);
              sSecao := 'encerrante' + IntToStrZero(I, 3);
              INIRec.WriteInteger(sSecao, 'nBico', encerrante.nBico);
              INIRec.WriteInteger(sSecao, 'nBomba', encerrante.nBomba);
              INIRec.WriteInteger(sSecao, 'nTanque', encerrante.nTanque);
              INIRec.WriteFloat(sSecao, 'vEncIni', encerrante.vEncIni);
              INIRec.WriteFloat(sSecao, 'vEncFin', encerrante.vEncFin);
              sSecao := 'ICMSComb' + IntToStrZero(I + 1, 3);
              INIRec.WriteFloat(sSecao, 'vBCICMS', ICMS.vBCICMS);
              INIRec.WriteFloat(sSecao, 'vICMS', ICMS.vICMS);
              INIRec.WriteFloat(sSecao, 'vBCICMSST', ICMS.vBCICMSST);
              INIRec.WriteFloat(sSecao, 'vICMSST', ICMS.vICMSST);
              if (ICMSInter.vBCICMSSTDest > 0) then
              begin
                sSecao := 'ICMSInter' + IntToStrZero(I + 1, 3);
                INIRec.WriteFloat(sSecao, 'vBCICMSSTDest', ICMSInter.vBCICMSSTDest);
                INIRec.WriteFloat(sSecao, 'vICMSSTDest', ICMSInter.vICMSSTDest);
              end;
              if (ICMSCons.vBCICMSSTCons > 0) then
              begin
                sSecao := 'ICMSCons' + IntToStrZero(I + 1, 3);
                INIRec.WriteFloat(sSecao, 'vBCICMSSTCons', ICMSCons.vBCICMSSTCons);
                INIRec.WriteFloat(sSecao, 'vICMSSTCons', ICMSCons.vICMSSTCons);
                INIRec.WriteString(sSecao, 'UFCons', ICMSCons.UFcons);
              end;
            end;
          end;
          with Imposto do
          begin
            sSecao := 'ICMS' + IntToStrZero(I + 1, 3);
            with ICMS do
            begin
              INIRec.WriteString(sSecao, 'orig', OrigToStr(ICMS.orig));
              INIRec.WriteString(sSecao, 'CST', CSTICMSToStr(CST));
              INIRec.WriteString(sSecao, 'CSOSN', CSOSNIcmsToStr(CSOSN));
              INIRec.WriteString(sSecao, 'modBC', modBCToStr(ICMS.modBC));
              INIRec.WriteFloat(sSecao, 'pRedBC', ICMS.pRedBC);
              INIRec.WriteFloat(sSecao, 'vBC', ICMS.vBC);
              INIRec.WriteFloat(sSecao, 'pICMS', ICMS.pICMS);
              INIRec.WriteFloat(sSecao, 'vICMS', ICMS.vICMS);
              INIRec.WriteFloat(sSecao, 'vBCFCP', ICMS.vBCFCP);
              INIRec.WriteFloat(sSecao, 'pFCP', ICMS.pFCP);
              INIRec.WriteFloat(sSecao, 'vFCP', ICMS.vFCP);
              INIRec.WriteString(sSecao, 'modBCST', modBCSTToStr(ICMS.modBCST));
              INIRec.WriteFloat(sSecao, 'pMVAST', ICMS.pMVAST);
              INIRec.WriteFloat(sSecao, 'pRedBCST', ICMS.pRedBCST);
              INIRec.WriteFloat(sSecao, 'vBCST', ICMS.vBCST);
              INIRec.WriteFloat(sSecao, 'pICMSST', ICMS.pICMSST);
              INIRec.WriteFloat(sSecao, 'vICMSST', ICMS.vICMSST);
              INIRec.WriteFloat(sSecao, 'vBCFCPST', ICMS.vBCFCPST);
              INIRec.WriteFloat(sSecao, 'pFCPST', ICMS.pFCPST);
              INIRec.WriteFloat(sSecao, 'vFCPST', ICMS.vFCPST);
              INIRec.WriteString(sSecao, 'UFST', ICMS.UFST);
              INIRec.WriteFloat(sSecao, 'pBCOp', ICMS.pBCOp);
              INIRec.WriteFloat(sSecao, 'vBCSTRet', ICMS.vBCSTRet);
              INIRec.WriteFloat(sSecao, 'pST', ICMS.pST);
              INIRec.WriteFloat(sSecao, 'vICMSSTRet', ICMS.vICMSSTRet);
              INIRec.WriteFloat(sSecao, 'vBCFCPSTRet', ICMS.vBCFCPSTRet);
              INIRec.WriteFloat(sSecao, 'pFCPSTRet', ICMS.pFCPSTRet);
              INIRec.WriteFloat(sSecao, 'vFCPSTRet', ICMS.vFCPSTRet);
              INIRec.WriteString(sSecao, 'motDesICMS', motDesICMSToStr(
                ICMS.motDesICMS));
              INIRec.WriteFloat(sSecao, 'pCredSN', ICMS.pCredSN);
              INIRec.WriteFloat(sSecao, 'vCredICMSSN', ICMS.vCredICMSSN);
              INIRec.WriteFloat(sSecao, 'vBCSTDest', ICMS.vBCSTDest);
              INIRec.WriteFloat(sSecao, 'vICMSSTDest', ICMS.vICMSSTDest);
              INIRec.WriteFloat(sSecao, 'vICMSDeson', ICMS.vICMSDeson);
              INIRec.WriteFloat(sSecao, 'vICMSOp', ICMS.vICMSOp);
              INIRec.WriteFloat(sSecao, 'pDif', ICMS.pDif);
              INIRec.WriteFloat(sSecao, 'vICMSDif', ICMS.vICMSDif);

              INIRec.WriteFloat(sSecao, 'pRedBCEfet', ICMS.pRedBCEfet);
              INIRec.WriteFloat(sSecao, 'vBCEfet', ICMS.vBCEfet);
              INIRec.WriteFloat(sSecao, 'pICMSEfet', ICMS.pICMSEfet);
              INIRec.WriteFloat(sSecao, 'vICMSEfet', ICMS.vICMSEfet);

              INIRec.WriteFloat(sSecao, 'vICMSSubstituto', ICMS.vICMSSubstituto);
            end;
            sSecao := 'ICMSUFDEST' + IntToStrZero(I + 1, 3);
            with ICMSUFDest do
            begin
              INIRec.WriteFloat(sSecao, 'vBCUFDest', vBCUFDest);
              INIRec.WriteFloat(sSecao, 'vBCFCPUFDest', vBCFCPUFDest);
              INIRec.WriteFloat(sSecao, 'pICMSUFDest', pICMSUFDest);
              INIRec.WriteFloat(sSecao, 'pICMSInter', pICMSInter);
              INIRec.WriteFloat(sSecao, 'pICMSInterPart', pICMSInterPart);
              INIRec.WriteFloat(sSecao, 'vICMSUFDest', vICMSUFDest);
              INIRec.WriteFloat(sSecao, 'vICMSUFRemet', vICMSUFRemet);
              INIRec.WriteFloat(sSecao, 'pFCPUFDest', pFCPUFDest);
              INIRec.WriteFloat(sSecao, 'vFCPUFDest', vFCPUFDest);
            end;
            if (IPI.vBC > 0) or (IPI.qUnid > 0) or
              (IPI.vIPI > 0) or (IPI.cEnq = '999') then
            begin
              sSecao := 'IPI' + IntToStrZero(I + 1, 3);
              with IPI do
              begin
                INIRec.WriteString(sSecao, 'CST', CSTIPIToStr(CST));
                INIRec.WriteString(sSecao, 'cEnq', cEnq);
                INIRec.WriteString(sSecao, 'clEnq', clEnq);
                INIRec.WriteString(sSecao, 'CNPJProd', CNPJProd);
                INIRec.WriteString(sSecao, 'cSelo', cSelo);
                INIRec.WriteInteger(sSecao, 'qSelo', qSelo);
                INIRec.WriteFloat(sSecao, 'vBC', vBC);
                INIRec.WriteFloat(sSecao, 'qUnid', qUnid);
                INIRec.WriteFloat(sSecao, 'vUnid', vUnid);
                INIRec.WriteFloat(sSecao, 'pIPI', pIPI);
                INIRec.WriteFloat(sSecao, 'vIPI', vIPI);
              end;
            end;
            if (II.vBc > 0) then
            begin
              sSecao := 'II' + IntToStrZero(I + 1, 3);
              with II do
              begin
                INIRec.WriteFloat(sSecao, 'vBc', vBc);
                INIRec.WriteFloat(sSecao, 'vDespAdu', vDespAdu);
                INIRec.WriteFloat(sSecao, 'vII', vII);
                INIRec.WriteFloat(sSecao, 'vIOF', vIOF);
              end;
            end;
            sSecao := 'PIS' + IntToStrZero(I + 1, 3);
            with PIS do
            begin
              INIRec.WriteString(sSecao, 'CST', CSTPISToStr(CST));
              if (CST = pis01) or (CST = pis02) then
              begin
                INIRec.WriteFloat(sSecao, 'vBC', PIS.vBC);
                INIRec.WriteFloat(sSecao, 'pPIS', PIS.pPIS);
                INIRec.WriteFloat(sSecao, 'vPIS', PIS.vPIS);
              end
              else if CST = pis03 then
              begin
                INIRec.WriteFloat(sSecao, 'qBCProd', PIS.qBCProd);
                INIRec.WriteFloat(sSecao, 'vAliqProd', PIS.vAliqProd);
                INIRec.WriteFloat(sSecao, 'vPIS', PIS.vPIS);
              end
              else if CST = pis99 then
              begin
                INIRec.WriteFloat(sSecao, 'vBC', PIS.vBC);
                INIRec.WriteFloat(sSecao, 'pPIS', PIS.pPIS);
                INIRec.WriteFloat(sSecao, 'qBCProd', PIS.qBCProd);
                INIRec.WriteFloat(sSecao, 'vAliqProd', PIS.vAliqProd);
                INIRec.WriteFloat(sSecao, 'vPIS', PIS.vPIS);
              end;
            end;
            if (PISST.vBc > 0) then
            begin
              sSecao := 'PISST' + IntToStrZero(I + 1, 3);
              with PISST do
              begin
                INIRec.WriteFloat(sSecao, 'vBc', vBc);
                INIRec.WriteFloat(sSecao, 'pPis', pPis);
                INIRec.WriteFloat(sSecao, 'qBCProd', qBCProd);
                INIRec.WriteFloat(sSecao, 'vAliqProd', vAliqProd);
                INIRec.WriteFloat(sSecao, 'vPIS', vPIS);
              end;
            end;
            sSecao := 'COFINS' + IntToStrZero(I + 1, 3);
            with COFINS do
            begin
              INIRec.WriteString(sSecao, 'CST', CSTCOFINSToStr(CST));
              if (CST = cof01) or (CST = cof02) then
              begin
                INIRec.WriteFloat(sSecao, 'vBC', COFINS.vBC);
                INIRec.WriteFloat(sSecao, 'pCOFINS', COFINS.pCOFINS);
                INIRec.WriteFloat(sSecao, 'vCOFINS', COFINS.vCOFINS);
              end
              else if CST = cof03 then
              begin
                INIRec.WriteFloat(sSecao, 'qBCProd', COFINS.qBCProd);
                INIRec.WriteFloat(sSecao, 'vAliqProd', COFINS.vAliqProd);
                INIRec.WriteFloat(sSecao, 'vCOFINS', COFINS.vCOFINS);
              end
              else if CST = cof99 then
              begin
                INIRec.WriteFloat(sSecao, 'vBC', COFINS.vBC);
                INIRec.WriteFloat(sSecao, 'pCOFINS', COFINS.pCOFINS);
                INIRec.WriteFloat(sSecao, 'qBCProd', COFINS.qBCProd);
                INIRec.WriteFloat(sSecao, 'vAliqProd', COFINS.vAliqProd);
                INIRec.WriteFloat(sSecao, 'vCOFINS', COFINS.vCOFINS);
              end;
            end;
            if (COFINSST.vBC > 0) then
            begin
              sSecao := 'COFINSST' + IntToStrZero(I + 1, 3);
              with COFINSST do
              begin
                INIRec.WriteFloat(sSecao, 'vBC', vBC);
                INIRec.WriteFloat(sSecao, 'pCOFINS', pCOFINS);
                INIRec.WriteFloat(sSecao, 'qBCProd', qBCProd);
                INIRec.WriteFloat(sSecao, 'vAliqProd', vAliqProd);
                INIRec.WriteFloat(sSecao, 'vCOFINS', vCOFINS);
              end;
            end;
            if (ISSQN.vBC > 0) then
            begin
              sSecao := 'ISSQN' + IntToStrZero(I + 1, 3);
              with ISSQN do
              begin
                INIRec.WriteFloat(sSecao, 'vBC', vBC);
                INIRec.WriteFloat(sSecao, 'vAliq', vAliq);
                INIRec.WriteFloat(sSecao, 'vISSQN', vISSQN);
                INIRec.WriteInteger(sSecao, 'cMunFG', cMunFG);
                INIRec.WriteString(sSecao, 'cListServ', cListServ);
                INIRec.WriteString(sSecao, 'cSitTrib', ISSQNcSitTribToStr(cSitTrib));
                INIRec.WriteFloat(sSecao, 'vDeducao', vDeducao);
                INIRec.WriteFloat(sSecao, 'vOutro', vOutro);
                INIRec.WriteFloat(sSecao, 'vDescIncond', vDescIncond);
                INIRec.WriteFloat(sSecao, 'vDescCond', vDescCond);
                INIRec.WriteFloat(sSecao, 'vISSRet', vISSRet);
                INIRec.WriteString(sSecao, 'indISS', indISSToStr( indISS ));
                INIRec.Writestring(sSecao, 'cServico', cServico);
                INIRec.WriteInteger(sSecao, 'cMun', cMun);
                INIRec.WriteInteger(sSecao, 'cPais', cPais);
                INIRec.WriteString(sSecao, 'nProcesso', nProcesso);
                INIRec.WriteString(sSecao, 'indIncentivo', indIncentivoToStr( indIncentivo ));
              end;
            end;
          end;
        end;
      end;

      INIRec.WriteFloat('Total', 'vBC', Total.ICMSTot.vBC);
      INIRec.WriteFloat('Total', 'vICMS', Total.ICMSTot.vICMS);
      INIRec.WriteFloat('Total', 'vICMSDeson', Total.ICMSTot.vICMSDeson);
      INIRec.WriteFloat('Total', 'vFCP', Total.ICMSTot.vFCP);
      INIRec.WriteFloat('Total', 'vICMSUFDest', Total.ICMSTot.vICMSUFDest);
      INIRec.WriteFloat('Total', 'vICMSUFRemet', Total.ICMSTot.vICMSUFRemet);
      INIRec.WriteFloat('Total', 'vFCPUFDest', Total.ICMSTot.vFCPUFDest);
      INIRec.WriteFloat('Total', 'vBCST', Total.ICMSTot.vBCST);
      INIRec.WriteFloat('Total', 'vST', Total.ICMSTot.vST);
      INIRec.WriteFloat('Total', 'vFCPST', Total.ICMSTot.vFCPST);
      INIRec.WriteFloat('Total', 'vFCPSTRet', Total.ICMSTot.vFCPSTRet);
      INIRec.WriteFloat('Total', 'vProd', Total.ICMSTot.vProd);
      INIRec.WriteFloat('Total', 'vFrete', Total.ICMSTot.vFrete);
      INIRec.WriteFloat('Total', 'vSeg', Total.ICMSTot.vSeg);
      INIRec.WriteFloat('Total', 'vDesc', Total.ICMSTot.vDesc);
      INIRec.WriteFloat('Total', 'vII', Total.ICMSTot.vII);
      INIRec.WriteFloat('Total', 'vIPI', Total.ICMSTot.vIPI);
      INIRec.WriteFloat('Total', 'vIPIDevol', Total.ICMSTot.vIPIDevol);
      INIRec.WriteFloat('Total', 'vPIS', Total.ICMSTot.vPIS);
      INIRec.WriteFloat('Total', 'vCOFINS', Total.ICMSTot.vCOFINS);
      INIRec.WriteFloat('Total', 'vOutro', Total.ICMSTot.vOutro);
      INIRec.WriteFloat('Total', 'vNF', Total.ICMSTot.vNF);
      INIRec.WriteFloat('Total', 'vTotTrib', Total.ICMSTot.vTotTrib);

      INIRec.WriteFloat('ISSQNtot', 'vServ', Total.ISSQNtot.vServ);
      INIRec.WriteFloat('ISSQNtot', 'vBC', Total.ISSQNTot.vBC);
      INIRec.WriteFloat('ISSQNtot', 'vISS', Total.ISSQNTot.vISS);
      INIRec.WriteFloat('ISSQNtot', 'vPIS', Total.ISSQNTot.vPIS);
      INIRec.WriteFloat('ISSQNtot', 'vCOFINS', Total.ISSQNTot.vCOFINS);
      INIRec.WriteDateTime('ISSQNtot', 'dCompet', Total.ISSQNTot.dCompet);
      INIRec.WriteFloat('ISSQNtot', 'vDeducao', Total.ISSQNTot.vDeducao);
      INIRec.WriteFloat('ISSQNtot', 'vOutro', Total.ISSQNTot.vOutro);
      INIRec.WriteFloat('ISSQNtot', 'vDescIncond', Total.ISSQNTot.vDescIncond);
      INIRec.WriteFloat('ISSQNtot', 'vDescCond', Total.ISSQNTot.vDescCond);
      INIRec.WriteFloat('ISSQNtot', 'vISSRet', Total.ISSQNTot.vISSRet);
      INIRec.WriteString('ISSQNtot', 'cRegTrib', RegTribISSQNToStr(
        Total.ISSQNTot.cRegTrib));

      INIRec.WriteFloat('retTrib', 'vRetPIS', Total.retTrib.vRetPIS);
      INIRec.WriteFloat('retTrib', 'vRetCOFINS', Total.retTrib.vRetCOFINS);
      INIRec.WriteFloat('retTrib', 'vRetCSLL', Total.retTrib.vRetCSLL);
      INIRec.WriteFloat('retTrib', 'vBCIRRF', Total.retTrib.vBCIRRF);
      INIRec.WriteFloat('retTrib', 'vIRRF', Total.retTrib.vIRRF);
      INIRec.WriteFloat('retTrib', 'vBCRetPrev', Total.retTrib.vBCRetPrev);
      INIRec.WriteFloat('retTrib', 'vRetPrev', Total.retTrib.vRetPrev);

      INIRec.WriteString('Transportador', 'modFrete', modFreteToStr(Transp.modFrete));
      INIRec.WriteString('Transportador', 'CNPJCPF', Transp.Transporta.CNPJCPF);
      INIRec.WriteString('Transportador', 'xNome', Transp.Transporta.xNome);
      INIRec.WriteString('Transportador', 'IE', Transp.Transporta.IE);
      INIRec.WriteString('Transportador', 'xEnder', Transp.Transporta.xEnder);
      INIRec.WriteString('Transportador', 'xMun', Transp.Transporta.xMun);
      INIRec.WriteString('Transportador', 'UF', Transp.Transporta.UF);
      INIRec.WriteFloat('Transportador', 'vServ', Transp.retTransp.vServ);
      INIRec.WriteFloat('Transportador', 'vBCRet', Transp.retTransp.vBCRet);
      INIRec.WriteFloat('Transportador', 'pICMSRet', Transp.retTransp.pICMSRet);
      INIRec.WriteFloat('Transportador', 'vICMSRet',
        Transp.retTransp.vICMSRet);
      INIRec.WriteString('Transportador', 'CFOP', Transp.retTransp.CFOP);
      INIRec.WriteInteger('Transportador', 'cMunFG', Transp.retTransp.cMunFG);
      INIRec.WriteString('Transportador', 'Placa', Transp.veicTransp.placa);
      INIRec.WriteString('Transportador', 'UFPlaca', Transp.veicTransp.UF);
      INIRec.WriteString('Transportador', 'RNTC', Transp.veicTransp.RNTC);
      INIRec.WriteString('Transportador', 'vagao', Transp.vagao);
      INIRec.WriteString('Transportador', 'balsa', Transp.balsa);

      for J := 0 to Transp.Reboque.Count - 1 do
      begin
        sSecao := 'Reboque' + IntToStrZero(J + 1, 3);
        with Transp.Reboque.Items[J] do
        begin
          INIRec.WriteString(sSecao, 'placa', placa);
          INIRec.WriteString(sSecao, 'UF', UF);
          INIRec.WriteString(sSecao, 'RNTC', RNTC);
        end;
      end;

      for I := 0 to Transp.Vol.Count - 1 do
      begin
        sSecao := 'Volume' + IntToStrZero(I + 1, 3);
        with Transp.Vol.Items[I] do
        begin
          INIRec.WriteInteger(sSecao, 'qVol', qVol);
          INIRec.WriteString(sSecao, 'esp', esp);
          INIRec.WriteString(sSecao, 'marca', marca);
          INIRec.WriteString(sSecao, 'nVol', nVol);
          INIRec.WriteFloat(sSecao, 'pesoL', pesoL);
          INIRec.WriteFloat(sSecao, 'pesoB', pesoB);

          for J := 0 to Lacres.Count - 1 do
          begin
            sSecao := 'Lacre' + IntToStrZero(I + 1, 3) + IntToStrZero(J + 1, 3);
            INIRec.WriteString(sSecao, 'nLacre', Lacres.Items[J].nLacre);
          end;
        end;
      end;

      INIRec.WriteString('Fatura', 'nFat', Cobr.Fat.nFat);
      INIRec.WriteFloat('Fatura', 'vOrig', Cobr.Fat.vOrig);
      INIRec.WriteFloat('Fatura', 'vDesc', Cobr.Fat.vDesc);
      INIRec.WriteFloat('Fatura', 'vLiq', Cobr.Fat.vLiq);

      for I := 0 to Cobr.Dup.Count - 1 do
      begin
        sSecao := 'Duplicata' + IntToStrZero(I + 1, 3);
        with Cobr.Dup.Items[I] do
        begin
          INIRec.WriteString(sSecao, 'nDup', nDup);
          INIRec.WriteString(sSecao, 'dVenc', DateToStr(dVenc));
          INIRec.WriteFloat(sSecao, 'vDup', vDup);
        end;
      end;

      for I := 0 to pag.Count - 1 do
      begin
        sSecao := 'pag' + IntToStrZero(I + 1, 3);
        with pag.Items[I] do
        begin
          INIRec.WriteString(sSecao, 'tPag', FormaPagamentoToStr(tPag));
          INIRec.WriteFloat(sSecao, 'vPag', vPag);
          INIRec.WriteString(sSecao, 'indPag', IndpagToStr(indPag));
          INIRec.WriteString(sSecao, 'tpIntegra', tpIntegraToStr(tpIntegra));
          INIRec.WriteString(sSecao, 'CNPJ', CNPJ);
          INIRec.WriteString(sSecao, 'tBand', BandeiraCartaoToStr(tBand));
          INIRec.WriteString(sSecao, 'cAut', cAut);
        end;
      end;
      INIRec.WriteFloat(sSecao, 'vTroco', pag.vTroco);

      INIRec.WriteString('DadosAdicionais', 'infAdFisco', InfAdic.infAdFisco);
      INIRec.WriteString('DadosAdicionais', 'infCpl', InfAdic.infCpl);

      for I := 0 to InfAdic.obsCont.Count - 1 do
      begin
        sSecao := 'InfAdic' + IntToStrZero(I + 1, 3);
        with InfAdic.obsCont.Items[I] do
        begin
          INIRec.WriteString(sSecao, 'xCampo', xCampo);
          INIRec.WriteString(sSecao, 'xTexto', xTexto);
        end;
      end;

      for I := 0 to InfAdic.obsFisco.Count - 1 do
      begin
        sSecao := 'ObsFisco' + IntToStrZero(I + 1, 3);
        with InfAdic.obsFisco.Items[I] do
        begin
          INIRec.WriteString(sSecao, 'xCampo', xCampo);
          INIRec.WriteString(sSecao, 'xTexto', xTexto);
        end;
      end;

      for I := 0 to InfAdic.procRef.Count - 1 do
      begin
        sSecao := 'procRef' + IntToStrZero(I + 1, 3);
        with InfAdic.procRef.Items[I] do
        begin
          INIRec.WriteString(sSecao, 'nProc', nProc);
          INIRec.WriteString(sSecao, 'indProc', indProcToStr(indProc));
        end;
      end;

      if (exporta.UFembarq <> '') or (exporta.UFSaidaPais <> '') then
      begin
        INIRec.WriteString('Exporta', 'UFembarq', exporta.UFembarq);
        INIRec.WriteString('Exporta', 'xLocEmbarq', exporta.xLocEmbarq);

        INIRec.WriteString('Exporta', 'UFSaidaPais', exporta.UFSaidaPais);
        INIRec.WriteString('Exporta', 'xLocExporta', exporta.xLocExporta);
        INIRec.WriteString('Exporta', 'xLocDespacho', exporta.xLocDespacho);
      end;

      if (compra.xNEmp <> '') then
      begin
        INIRec.WriteString('Compra', 'xNEmp', compra.xNEmp);
        INIRec.WriteString('Compra', 'xPed', compra.xPed);
        INIRec.WriteString('Compra', 'xCont', compra.xCont);
      end;

      INIRec.WriteString('cana', 'safra', cana.safra);
      INIRec.WriteString('cana', 'ref', cana.ref);
      INIRec.WriteFloat('cana', 'qTotMes', cana.qTotMes);
      INIRec.WriteFloat('cana', 'qTotAnt', cana.qTotAnt);
      INIRec.WriteFloat('cana', 'qTotGer', cana.qTotGer);
      INIRec.WriteFloat('cana', 'vFor', cana.vFor);
      INIRec.WriteFloat('cana', 'vTotDed', cana.vTotDed);
      INIRec.WriteFloat('cana', 'vLiqFor', cana.vLiqFor);

      for I := 0 to cana.fordia.Count - 1 do
      begin
        sSecao := 'forDia' + IntToStrZero(I + 1, 3);
        with cana.fordia.Items[I] do
        begin
          INIRec.WriteInteger(sSecao, 'dia', dia);
          INIRec.WriteFloat(sSecao, 'qtde', qtde);
        end;
      end;

      for I := 0 to cana.deduc.Count - 1 do
      begin
        sSecao := 'deduc' + IntToStrZero(I + 1, 3);
        with cana.deduc.Items[I] do
        begin
          INIRec.WriteString(sSecao, 'xDed', xDed);
          INIRec.WriteFloat(sSecao, 'vDed', vDed);
        end;
      end;

      INIRec.WriteString('procNF3e', 'tpAmb', TpAmbToStr(procNF3e.tpAmb));
      INIRec.WriteString('procNF3e', 'verAplic', procNF3e.verAplic);
      INIRec.WriteString('procNF3e', 'chNF3e', procNF3e.chNF3e);
      INIRec.WriteString('procNF3e', 'dhRecbto', DateTimeToStr(procNF3e.dhRecbto));
      INIRec.WriteString('procNF3e', 'nProt', procNF3e.nProt);
      INIRec.WriteString('procNF3e', 'digVal', procNF3e.digVal);
      INIRec.WriteString('procNF3e', 'cStat', IntToStr(procNF3e.cStat));
      INIRec.WriteString('procNF3e', 'xMotivo', procNF3e.xMotivo);
      }
    end;

  finally
    IniNF3e := TStringList.Create;
    try
      INIRec.GetStrings(IniNF3e);
      INIRec.Free;
      Result := StringReplace(IniNF3e.Text, sLineBreak + sLineBreak, sLineBreak, [rfReplaceAll]);
    finally
      IniNF3e.Free;
    end;
  end;

end;

function TNotaFiscal.GravarXML(const NomeArquivo: String; const PathArquivo: String): Boolean;
begin
  if EstaVazio(FXMLOriginal) then
    GerarXML;

  FNomeArq := CalcularNomeArquivoCompleto(NomeArquivo, PathArquivo);

  Result := TACBrNF3e(TNotasFiscais(Collection).ACBrNF3e).Gravar(FNomeArq, FXMLOriginal);
end;

function TNotaFiscal.GravarStream(AStream: TStream): Boolean;
begin
  if EstaVazio(FXMLOriginal) then
    GerarXML;

  AStream.Size := 0;
  WriteStrToStream(AStream, AnsiString(FXMLOriginal));

  Result := True;
end;

procedure TNotaFiscal.EnviarEmail(const sPara, sAssunto: String; sMensagem: TStrings;
  EnviaPDF: Boolean; sCC: TStrings; Anexos: TStrings; sReplyTo: TStrings);
var
  NomeArq_temp : String;
  AnexosEmail:TStrings;
  StreamNF3e : TMemoryStream;
begin
  if not Assigned(TACBrNF3e(TNotasFiscais(Collection).ACBrNF3e).MAIL) then
    raise EACBrNF3eException.Create('Componente ACBrMail não associado');

  AnexosEmail := TStringList.Create;
  StreamNF3e := TMemoryStream.Create;
  try
    AnexosEmail.Clear;

    if Assigned(Anexos) then
      AnexosEmail.Assign(Anexos);

    with TACBrNF3e(TNotasFiscais(Collection).ACBrNF3e) do
    begin
      Self.GravarStream(StreamNF3e);

      if (EnviaPDF) then
      begin
        if Assigned(DANF3e) then
        begin
          DANF3e.ImprimirDANF3ePDF(FNF3e);
          NomeArq_temp := PathWithDelim(DANF3e.PathPDF) + NumID + '-NF3e.pdf';
          AnexosEmail.Add(NomeArq_temp);
        end;
      end;

      EnviarEmail( sPara, sAssunto, sMensagem, sCC, AnexosEmail, StreamNF3e,
                   NumID +'-NF3e.xml', sReplyTo);
    end;
  finally
    AnexosEmail.Free;
    StreamNF3e.Free;
  end;
end;

function TNotaFiscal.GerarXML: String;
var
  IdAnterior : String;
begin
  with TACBrNF3e(TNotasFiscais(Collection).ACBrNF3e) do
  begin
    IdAnterior := NF3e.infNF3e.ID;

    FNF3eW.Opcoes.FormatoAlerta  := Configuracoes.Geral.FormatoAlerta;
    FNF3eW.Opcoes.RetirarAcentos := Configuracoes.Geral.RetirarAcentos;
    FNF3eW.Opcoes.RetirarEspacos := Configuracoes.Geral.RetirarEspacos;
    FNF3eW.Opcoes.IdentarXML := Configuracoes.Geral.IdentarXML;
    FNF3eW.Opcoes.NormatizarMunicipios  := Configuracoes.Arquivos.NormatizarMunicipios;
    FNF3eW.Opcoes.PathArquivoMunicipios := Configuracoes.Arquivos.PathArquivoMunicipios;

    TimeZoneConf.Assign( Configuracoes.WebServices.TimeZoneConf );

    {
      Ao gerar o XML as tags e atributos tem que ser exatamente os da configuração
    }
    {
    FNF3eW.VersaoDF := Configuracoes.Geral.VersaoDF;
    FNF3eW.ModeloDF := 62;
    FNF3eW.tpAmb := TACBrTipoAmbiente(Configuracoes.WebServices.Ambiente);
    FNF3eW.tpEmis := TACBrTipoEmissao(Configuracoes.Geral.FormaEmissao);
    }
    FNF3eW.idCSRT := Configuracoes.RespTec.IdCSRT;
    FNF3eW.CSRT   := Configuracoes.RespTec.CSRT;
  end;

  FNF3eW.GerarXml;
  //DEBUG
  //WriteToTXT('c:\temp\Notafiscal.xml', FNF3eW.Document.Xml, False, False);

  XMLOriginal := FNF3eW.Document.Xml;

  { XML gerado pode ter nova Chave e ID, então devemos calcular novamente o
    nome do arquivo, mantendo o PATH do arquivo carregado }
  if (NaoEstaVazio(FNomeArq) and (IdAnterior <> FNF3e.infNF3e.ID)) then
    FNomeArq := CalcularNomeArquivoCompleto('', ExtractFilePath(FNomeArq));

  FAlertas := ACBrStr( FNF3eW.ListaDeAlertas.Text );

  Result := FXMLOriginal;
end;

function TNotaFiscal.CalcularNomeArquivo: String;
var
  xID: String;
  NomeXML: String;
begin
  xID := Self.NumID;

  if EstaVazio(xID) then
    raise EACBrNF3eException.Create('ID Inválido. Impossível Salvar XML');

  NomeXML := '-NF3e.xml';

  Result := xID + NomeXML;
end;

function TNotaFiscal.CalcularPathArquivo: String;
var
  Data: TDateTime;
begin
  with TACBrNF3e(TNotasFiscais(Collection).ACBrNF3e) do
  begin
    if Configuracoes.Arquivos.EmissaoPathNF3e then
      Data := FNF3e.Ide.dhEmi
    else
      Data := Now;

    Result := PathWithDelim(Configuracoes.Arquivos.GetPathNF3e(Data, FNF3e.Emit.CNPJ));
  end;
end;

function TNotaFiscal.CalcularNomeArquivoCompleto(NomeArquivo: String;
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

function TNotaFiscal.ValidarConcatChave: Boolean;
var
  wAno, wMes, wDia: word;
  chaveNF3e : String;
begin
  DecodeDate(NF3e.ide.dhEmi, wAno, wMes, wDia);

  chaveNF3e := OnlyNumber(NF3e.infNF3e.ID);
  {(*}
  Result := not
    ((Copy(chaveNF3e, 1, 2) <> IntToStrZero(NF3e.Ide.cUF, 2)) or
    (Copy(chaveNF3e, 3, 2)  <> Copy(FormatFloat('0000', wAno), 3, 2)) or
    (Copy(chaveNF3e, 5, 2)  <> FormatFloat('00', wMes)) or
    (Copy(chaveNF3e, 7, 14)<> PadLeft(OnlyNumber(NF3e.Emit.CNPJ), 14, '0')) or
    (Copy(chaveNF3e, 21, 2) <> IntToStrZero(NF3e.Ide.modelo, 2)) or
    (Copy(chaveNF3e, 23, 3) <> IntToStrZero(NF3e.Ide.serie, 3)) or
    (Copy(chaveNF3e, 26, 9) <> IntToStrZero(NF3e.Ide.nNF, 9)) or
    (Copy(chaveNF3e, 35, 1) <> TipoEmissaoToStr(NF3e.Ide.tpEmis)) or
    (Copy(chaveNF3e, 36, 1) <> SiteAutorizadorToStr(NF3e.Ide.nSiteAutoriz)) or
    (Copy(chaveNF3e, 37, 7) <> IntToStrZero(NF3e.Ide.cNF, 7)));
  {*)}
end;

function TNotaFiscal.GetConfirmada: Boolean;
begin
  Result := TACBrNF3e(TNotasFiscais(Collection).ACBrNF3e).CstatConfirmada(
    FNF3e.procNF3e.cStat);
end;

function TNotaFiscal.GetcStat: Integer;
begin
  Result := FNF3e.procNF3e.cStat;
end;

function TNotaFiscal.GetProcessada: Boolean;
begin
  Result := TACBrNF3e(TNotasFiscais(Collection).ACBrNF3e).CstatProcessado(
    FNF3e.procNF3e.cStat);
end;

function TNotaFiscal.GetCancelada: Boolean;
begin
  Result := TACBrNF3e(TNotasFiscais(Collection).ACBrNF3e).CstatCancelada(
    FNF3e.procNF3e.cStat);
end;

function TNotaFiscal.GetMsg: String;
begin
  Result := FNF3e.procNF3e.xMotivo;
end;

function TNotaFiscal.GetNumID: String;
begin
  Result := OnlyNumber(NF3e.infNF3e.ID);
end;

function TNotaFiscal.GetXMLAssinado: String;
begin
  if EstaVazio(FXMLAssinado) then
    Assinar;

  Result := FXMLAssinado;
end;

procedure TNotaFiscal.SetXML(const AValue: String);
begin
  LerXML(AValue);
end;

procedure TNotaFiscal.SetXMLOriginal(const AValue: String);
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

{ TNotasFiscais }

constructor TNotasFiscais.Create(AOwner: TPersistent; ItemClass: TCollectionItemClass);
begin
  if not (AOwner is TACBrNF3e) then
    raise EACBrNF3eException.Create('AOwner deve ser do tipo TACBrNF3e');

  inherited Create(AOwner, ItemClass);

  FACBrNF3e := TACBrNF3e(AOwner);
  FConfiguracoes := TACBrNF3e(FACBrNF3e).Configuracoes;
end;

function TNotasFiscais.Add: TNotaFiscal;
begin
  Result := TNotaFiscal(inherited Add);
end;

procedure TNotasFiscais.Assinar;
var
  i: integer;
begin
  for i := 0 to Self.Count - 1 do
    Self.Items[i].Assinar;
end;

procedure TNotasFiscais.GerarNF3e;
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

procedure TNotasFiscais.VerificarDANF3e;
begin
  if not Assigned(TACBrNF3e(FACBrNF3e).DANF3e) then
    raise EACBrNF3eException.Create('Componente DANF3e não associado.');
end;

procedure TNotasFiscais.Imprimir;
begin
  VerificarDANF3e;
  TACBrNF3e(FACBrNF3e).DANF3e.ImprimirDANF3e(nil);
end;

procedure TNotasFiscais.ImprimirCancelado;
begin
  VerificarDANF3e;
  TACBrNF3e(FACBrNF3e).DANF3e.ImprimirDANF3eCancelado(nil);
end;

procedure TNotasFiscais.ImprimirResumido;
begin
  VerificarDANF3e;
  TACBrNF3e(FACBrNF3e).DANF3e.ImprimirDANF3eResumido(nil);
end;

procedure TNotasFiscais.ImprimirPDF;
begin
  VerificarDANF3e;
  TACBrNF3e(FACBrNF3e).DANF3e.ImprimirDANF3ePDF(nil);
end;

procedure TNotasFiscais.ImprimirResumidoPDF;
begin
  VerificarDANF3e;
  TACBrNF3e(FACBrNF3e).DANF3e.ImprimirDANF3eResumidoPDF(nil);
end;

function TNotasFiscais.Insert(Index: integer): TNotaFiscal;
begin
  Result := TNotaFiscal(inherited Insert(Index));
end;

procedure TNotasFiscais.SetItem(Index: integer; const Value: TNotaFiscal);
begin
  Items[Index].Assign(Value);
end;

procedure TNotasFiscais.Validar;
var
  i: integer;
begin
  for i := 0 to Self.Count - 1 do
    Self.Items[i].Validar;   // Dispara exception em caso de erro
end;

function TNotasFiscais.VerificarAssinatura(out Erros: String): Boolean;
var
  i: integer;
begin
  Result := True;
  Erros := '';

  if Self.Count < 1 then
  begin
    Erros := 'Nenhuma NF3e carregada';
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

function TNotasFiscais.ValidarRegrasdeNegocios(out Erros: String): Boolean;
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

function TNotasFiscais.LoadFromFile(const CaminhoArquivo: String;
  AGerarNF3e: Boolean): Boolean;
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

  l := Self.Count; // Indice da última nota já existente
  Result := LoadFromString(String(XMLUTF8), AGerarNF3e);

  if Result then
  begin
    // Atribui Nome do arquivo a novas notas inseridas //
    for i := l to Self.Count - 1 do
      Self.Items[i].NomeArq := CaminhoArquivo;
  end;
end;

function TNotasFiscais.LoadFromStream(AStream: TStringStream;
  AGerarNF3e: Boolean): Boolean;
var
  AXML: AnsiString;
begin
  AStream.Position := 0;
  AXML := ReadStrFromStream(AStream, AStream.Size);

  Result := Self.LoadFromString(String(AXML), AGerarNF3e);
end;

function TNotasFiscais.LoadFromString(const AXMLString: String;
  AGerarNF3e: Boolean): Boolean;
var
  ANF3eXML, XMLStr: AnsiString;
  P, N: integer;

  function PosNF3e: integer;
  begin
    Result := pos('</NF3e>', XMLStr);
  end;

begin
  // Verifica se precisa Converter de UTF8 para a String nativa da IDE //
  XMLStr := ConverteXMLtoNativeString(AXMLString);

  N := PosNF3e;
  while N > 0 do
  begin
    P := pos('</nf3eProc>', XMLStr);

    if P <= 0 then
      P := pos('</procNF3e>', XMLStr);  // NF3e obtida pelo Portal da Receita

    if P > 0 then
    begin
      ANF3eXML := copy(XMLStr, 1, P + 10);
      XMLStr := Trim(copy(XMLStr, P + 10, length(XMLStr)));
    end
    else
    begin
      ANF3eXML := copy(XMLStr, 1, N + 6);
      XMLStr := Trim(copy(XMLStr, N + 6, length(XMLStr)));
    end;

    with Self.Add do
    begin
      LerXML(ANF3eXML);

      if AGerarNF3e then // Recalcula o XML
        GerarXML;
    end;

    N := PosNF3e;
  end;

  Result := Self.Count > 0;
end;

function TNotasFiscais.LoadFromIni(const AIniString: String): Boolean;
begin
  with Self.Add do
    LerArqIni(AIniString);

  Result := Self.Count > 0;
end;

function TNotasFiscais.GerarIni: String;
begin
  Result := '';

  if (Self.Count > 0) then
    Result := Self.Items[0].GerarNF3eIni;
end;

function TNotasFiscais.GravarXML(const APathNomeArquivo: String): Boolean;
var
  i: integer;
  NomeArq, PathArq : String;
begin
  Result := True;
  i := 0;

  while Result and (i < Self.Count) do
  begin
    PathArq := ExtractFilePath(APathNomeArquivo);
    NomeArq := ExtractFileName(APathNomeArquivo);
    Result := Self.Items[i].GravarXML(NomeArq, PathArq);
    Inc(i);
  end;
end;

end.
