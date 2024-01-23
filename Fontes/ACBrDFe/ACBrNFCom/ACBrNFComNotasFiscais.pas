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

unit ACBrNFComNotasFiscais;

interface

uses
  Classes, SysUtils, StrUtils,
  ACBrXmlBase,
  ACBrNFComConfiguracoes, ACBrNFComClass,
  ACBrNFComXmlReader, ACBrNFComXmlWriter,
  pcnAuxiliar;

type

  { NotaFiscal }

  NotaFiscal = class(TCollectionItem)
  private
    FNFCom: TNFCom;
    FNFComW: TNFComXmlWriter;
    FNFComR: TNFComXmlReader;
    FConfiguracoes: TConfiguracoesNFCom;
    FXMLAssinado: string;
    FXMLOriginal: string;
    FAlertas: string;
    FErroValidacao: string;
    FErroValidacaoCompleto: string;
    FErroRegrasdeNegocios: string;
    FNomeArq: string;
    FNomeArqPDF: string;

    function GetConfirmada: Boolean;
    function GetcStat: Integer;
    function GetProcessada: Boolean;
    function GetCancelada: Boolean;

    function GetMsg: string;
    function GetNumID: string;
    function GetXMLAssinado: string;
    procedure SetXML(const AValue: string);
    procedure SetXMLOriginal(const AValue: string);
    function ValidarConcatChave: Boolean;
    function CalcularNomeArquivo: string;
    function CalcularPathArquivo: string;
  public
    constructor Create(Collection2: TCollection); override;
    destructor Destroy; override;
    procedure Imprimir;
    procedure ImprimirPDF;

    procedure Assinar;
    procedure Validar;
    function VerificarAssinatura: Boolean;
    function ValidarRegrasdeNegocios: Boolean;

    function LerXML(const AXML: string): Boolean;
    function LerArqIni(const AIniString: string): Boolean;
    function GerarNFComIni: string;

    function GerarXML: string;
    function GravarXML(const NomeArquivo: string = ''; const PathArquivo: string = ''): Boolean;

    function GravarStream(AStream: TStream): Boolean;

    procedure EnviarEmail(const sPara, sAssunto: string; sMensagem: TStrings = nil;
      EnviaPDF: Boolean = True; sCC: TStrings = nil; Anexos: TStrings = nil;
      sReplyTo: TStrings = nil);

    property NomeArq: string read FNomeArq write FNomeArq;
    property NomeArqPDF: string read FNomeArqPDF write FNomeArqPDF;
    function CalcularNomeArquivoCompleto(NomeArquivo: string = '';
      PathArquivo: string = ''): string;

    property NFCom: TNFCom read FNFCom;

    // Atribuir a "XML", faz o componente transferir os dados lido para as propriedades internas e "XMLAssinado"
    property XML: string         read FXMLOriginal   write SetXML;
    // Atribuir a "XMLOriginal", reflete em XMLAssinado, se existir a tag de assinatura
    property XMLOriginal: string read FXMLOriginal   write SetXMLOriginal;    // Sempre deve estar em UTF8
    property XMLAssinado: string read GetXMLAssinado write FXMLAssinado;      // Sempre deve estar em UTF8
    property Confirmada: Boolean read GetConfirmada;
    property Processada: Boolean read GetProcessada;
    property Cancelada: Boolean read GetCancelada;
    property cStat: Integer read GetcStat;
    property Msg: string read GetMsg;
    property NumID: string read GetNumID;

    property Alertas: string read FAlertas;
    property ErroValidacao: string read FErroValidacao;
    property ErroValidacaoCompleto: string read FErroValidacaoCompleto;
    property ErroRegrasdeNegocios: string read FErroRegrasdeNegocios;
  end;

  { TNotasFiscais }

  TNotasFiscais = class(TOwnedCollection)
  private
    FACBrNFCom: TComponent;
    FConfiguracoes: TConfiguracoesNFCom;

    function GetItem(Index: integer): NotaFiscal;
    procedure SetItem(Index: integer; const Value: NotaFiscal);

    procedure VerificarDANFCom;
  public
    constructor Create(AOwner: TPersistent; ItemClass: TCollectionItemClass);

    procedure GerarNFCom;
    procedure Assinar;
    procedure Validar;
    function VerificarAssinatura(out Erros: string): Boolean;
    function ValidarRegrasdeNegocios(out Erros: string): Boolean;
    procedure Imprimir;
    procedure ImprimirCancelado;
    procedure ImprimirResumido;
    procedure ImprimirPDF;
    procedure ImprimirResumidoPDF;
    function Add: NotaFiscal;
    function Insert(Index: integer): NotaFiscal;

    property Items[Index: integer]: NotaFiscal read GetItem write SetItem; default;

    function GetNamePath: string; override;
    // Incluido o Parametro AGerarNFCom que determina se após carregar os dados da NFCom
    // para o componente, será gerado ou não novamente o XML da NFCom.
    function LoadFromFile(const CaminhoArquivo: string; AGerarNFCom: Boolean = False): Boolean;
    function LoadFromStream(AStream: TStringStream; AGerarNFCom: Boolean = False): Boolean;
    function LoadFromString(const AXMLString: string; AGerarNFCom: Boolean = False): Boolean;
    function LoadFromIni(const AIniString: string): Boolean;

    function GerarIni: string;
    function GravarXML(const APathNomeArquivo: string = ''): Boolean;

    property ACBrNFCom: TComponent read FACBrNFCom;
  end;

implementation

uses
  dateutils, IniFiles,
  synautil,
  ACBrUtil.Base, ACBrUtil.Strings, ACBrUtil.XMLHTML, ACBrUtil.FilesIO,
  ACBrUtil.DateTime,
  ACBrDFeUtil, //ACBrDFeConversao,
  ACBrNFCom, ACBrNFComConversao,
  ACBrXmlDocument;

{ NotaFiscal }

constructor NotaFiscal.Create(Collection2: TCollection);
begin
  inherited Create(Collection2);

  FNFCom := TNFCom.Create;
  FNFComW := TNFComXmlWriter.Create(FNFCom);
  FNFComR := TNFComXmlReader.Create(FNFCom);

  FConfiguracoes := TACBrNFCom(TNotasFiscais(Collection).ACBrNFCom).Configuracoes;

  with TACBrNFCom(TNotasFiscais(Collection).ACBrNFCom) do
  begin
    FNFCom.infNFCom.Versao := VersaoNFComToDbl(Configuracoes.Geral.VersaoDF);

    FNFCom.Ide.modelo  := 62;
    FNFCom.Ide.verProc := 'ACBrNFCom';
    FNFCom.Ide.tpAmb   := TACBrTipoAmbiente(Configuracoes.WebServices.Ambiente);
    FNFCom.Ide.tpEmis  := TACBrTipoEmissao(Configuracoes.Geral.FormaEmissao);
  end;
end;

destructor NotaFiscal.Destroy;
begin
  FNFComW.Free;
  FNFComR.Free;
  FNFCom.Free;

  inherited Destroy;
end;

procedure NotaFiscal.Imprimir;
begin
  with TACBrNFCom(TNotasFiscais(Collection).ACBrNFCom) do
  begin
    if not Assigned(DANFCom) then
      raise EACBrNFComException.Create('Componente DANFCom não associado.')
    else
      DANFCom.ImprimirDANFCom(NFCom);
  end;
end;

procedure NotaFiscal.ImprimirPDF;
begin
  with TACBrNFCom(TNotasFiscais(Collection).ACBrNFCom) do
  begin
    if not Assigned(DANFCom) then
      raise EACBrNFComException.Create('Componente DANFCom não associado.')
    else
      DANFCom.ImprimirDANFComPDF(NFCom);
  end;
end;

procedure NotaFiscal.Assinar;
var
  XMLStr: string;
  XMLUTF8: AnsiString;
  Document: TACBrXmlDocument;
  ANode, SignatureNode, ReferenceNode: TACBrXmlNode;
begin
  with TACBrNFCom(TNotasFiscais(Collection).ACBrNFCom) do
  begin
    if not Assigned(SSL.AntesDeAssinar) then
      SSL.ValidarCNPJCertificado(NFCom.Emit.CNPJ);
  end;

  // Gera novamente, para processar propriedades que podem ter sido modificadas
  XMLStr := GerarXML;

  // XML já deve estar em UTF8, para poder ser assinado //
  XMLUTF8 := ConverteXMLtoUTF8(XMLStr);

  with TACBrNFCom(TNotasFiscais(Collection).ACBrNFCom) do
  begin
    FXMLAssinado := SSL.Assinar(String(XMLUTF8), 'NFCom', 'infNFCom');
    // SSL.Assinar() sempre responde em UTF8...
    FXMLOriginal := FXMLAssinado;

    Document := TACBrXmlDocument.Create;
    try
      try
        Document.LoadFromXml(FXMLOriginal);
        ANode := Document.Root;

        if ANode <> nil then
        begin
          SignatureNode := ANode.Childrens.FindAnyNs('Signature');
          ReferenceNode := SignatureNode.Childrens.FindAnyNs('SignedInfo').Childrens.FindAnyNs('Reference');

          NFCom.signature.URI := ObterConteudoTag(ReferenceNode.Attributes.Items['URI']);
          NFCom.signature.DigestValue := ObterConteudoTag(ReferenceNode.Childrens.FindAnyNs('DigestValue'), tcStr);
          NFCom.signature.SignatureValue := ObterConteudoTag(SignatureNode.Childrens.FindAnyNs('SignatureValue'), tcStr);
          NFCom.signature.X509Certificate := ObterConteudoTag(SignatureNode.Childrens.FindAnyNs('KeyInfo')
                                                                           .Childrens.FindAnyNs('X509Data')
                                                                           .Childrens.FindAnyNs('X509Certificate'), tcStr);
        end;
      except
        //Result := False;
      end;
    finally
      FreeAndNil(Document);
    end;

    with TACBrNFCom(TNotasFiscais(Collection).ACBrNFCom) do
    begin
      NFCom.infNFComSupl.qrCodNFCom := GetURLQRCode(NFCom.Ide.cUF,
                                                    NFCom.Ide.tpAmb,
                                                    NFCom.Ide.tpEmis,
                                                    NFCom.infNFCom.ID,
                                                    NFCom.infNFCom.Versao);

      GerarXML;
    end;

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

procedure NotaFiscal.Validar;
var
  Erro, AXML: string;
  NotaEhValida: Boolean;
  ALayout: TLayOutNFCom;
  VerServ: Real;
begin
  AXML := FXMLAssinado;

  if AXML = '' then
    AXML := XMLOriginal;

  with TACBrNFCom(TNotasFiscais(Collection).ACBrNFCom) do
  begin
    VerServ := FNFCom.infNFCom.Versao;
    ALayout := LayNFComRecepcao;

    // Extraindo apenas os dados da NFCom (sem nfComProc)
    AXML := ObterDFeXML(AXML, 'NFCom', ACBRNFCom_NAMESPACE);

    if EstaVazio(AXML) then
    begin
      Erro := ACBrStr('NFCom não encontrada no XML');
      NotaEhValida := False;
    end
    else
      NotaEhValida := SSL.Validar(AXML, GerarNomeArqSchema(ALayout, VerServ), Erro);

    if not NotaEhValida then
    begin
      FErroValidacao := ACBrStr('Falha na validação dos dados da nota: ') +
        IntToStr(NFCom.Ide.nNF) + sLineBreak + FAlertas;
      FErroValidacaoCompleto := FErroValidacao + sLineBreak + Erro;

      raise EACBrNFComException.CreateDef(
        IfThen(Configuracoes.Geral.ExibirErroSchema, ErroValidacaoCompleto,
        ErroValidacao));
    end;
  end;
end;

function NotaFiscal.VerificarAssinatura: Boolean;
var
  Erro, AXML: string;
  AssEhValida: Boolean;
begin
  AXML := FXMLAssinado;
  if AXML = '' then
    AXML := XMLOriginal;

  with TACBrNFCom(TNotasFiscais(Collection).ACBrNFCom) do
  begin
    // Extraindo apenas os dados da NFCom (sem nfComProc)
    AXML := ObterDFeXML(AXML, 'NFCom', ACBRNFCom_NAMESPACE);

    if EstaVazio(AXML) then
    begin
      Erro := ACBrStr('NFCom não encontrada no XML');
      AssEhValida := False;
    end
    else
      AssEhValida := SSL.VerificarAssinatura(AXML, Erro, 'infNFCom');

    if not AssEhValida then
    begin
      FErroValidacao := ACBrStr('Falha na validação da assinatura da nota: ') +
        IntToStr(NFCom.Ide.nNF) + sLineBreak + Erro;
    end;
  end;

  Result := AssEhValida;
end;

function NotaFiscal.ValidarRegrasdeNegocios: Boolean;
var
  Erros: string;
  Inicio, Agora: TDateTime;

  procedure GravaLog(AString: string);
  begin
    //DEBUG
//    Log := Log + FormatDateTime('hh:nn:ss:zzz', Now) + ' - ' + AString + sLineBreak;
  end;

  procedure AdicionaErro(const Erro: string);
  begin
    Erros := Erros + Erro + sLineBreak;
  end;

begin
  // Converte o DateTime do Sistema para o TimeZone configurado,
  // para evitar divergência de Fuso Horário.
  Inicio := DataHoraTimeZoneModoDeteccao(TACBrNFCom(TNotasFiscais(Collection).ACBrNFCom));
  // Aceita uma tolerância de até 5 minutos, devido ao sincronismo de horário
  // do servidor da Empresa e o servidor da SEFAZ.
  Agora := IncMinute(Inicio, 5);
  GravaLog('Inicio da Validação');

  with TACBrNFCom(TNotasFiscais(Collection).ACBrNFCom) do
  begin
    Erros := '';

    GravaLog('Validar: 703-Data hora');
    if (NFCom.Ide.dhEmi > Agora) then
      AdicionaErro('703-Rejeição: Data-Hora de Emissão posterior ao horário de recebimento');
    {
      Incluir as regras aqui
    }
  end;

  Result := EstaVazio(Erros);

  if not Result then
  begin
    Erros := ACBrStr('Erro(s) nas Regras de negócios da nota: '+
                     IntToStr(NFCom.Ide.nNF) + sLineBreak + Erros);
  end;

  GravaLog('Fim da Validação. Tempo: ' +
           FormatDateTime('hh:nn:ss:zzz', Now - Inicio) + sLineBreak +
           'Erros:' + Erros);

  //DEBUG
  //WriteToTXT('c:\temp\Notafiscal.txt', Log);

  FErroRegrasdeNegocios := Erros;
end;

function NotaFiscal.LerXML(const AXML: string): Boolean;
begin
  XMLOriginal := AXML;

  FNFComR.Arquivo := XMLOriginal;
  FNFComR.LerXml;
  Result := True;
end;

function NotaFiscal.LerArqIni(const AIniString: string): Boolean;
var
  INIRec: TMemIniFile;
  sSecao, sFim, nItemDet: string;
  OK: boolean;
  i, j: Integer;
begin
  Result := False;

  INIRec := TMemIniFile.Create('');
  try
    LerIniArquivoOuString(AIniString, INIRec);

    with FNFCom do
    begin
      infNFCom.versao := StringToFloatDef(INIRec.ReadString('infNFCom', 'versao', VersaoNFComToStr(FConfiguracoes.Geral.VersaoDF)), 0);

      sSecao := 'ide';
      Ide.tpAmb := StrToTipoAmbiente(OK, INIRec.ReadString(sSecao, 'tpAmb', IntToStr(Integer(FConfiguracoes.WebServices.Ambiente))));
      Ide.modelo := INIRec.ReadInteger(sSecao, 'Modelo', 62);
      Ide.serie := INIRec.ReadInteger(sSecao, 'Serie', 1);
      Ide.nNF := INIRec.ReadInteger(sSecao, 'nNF', 0);
      Ide.cNF := INIRec.ReadInteger(sSecao, 'cNF', 0);
      Ide.dhEmi := StringToDateTime(INIRec.ReadString(sSecao, 'dhEmi', '0'));
      Ide.tpEmis := StrToTipoEmissao(OK, INIRec.ReadString(sSecao, 'tpEmis', IntToStr(FConfiguracoes.Geral.FormaEmissaoCodigo)));
      Ide.nSiteAutoriz := StrToSiteAutorizator(INIRec.ReadString(sSecao, 'nSiteAutoriz', '0'));
      Ide.finNFCom := StrToFinNFCom(INIRec.ReadString(sSecao, 'finNFCom', '0'));
      Ide.tpFat := StrToTipoFaturamento(INIRec.ReadString(sSecao, 'tpFat', '0'));
      Ide.verProc := INIRec.ReadString(sSecao, 'verProc', 'ACBrNFCom');
      Ide.indPrePago := StrToTIndicador(INIRec.ReadString(sSecao, 'indPrePago', '0'));
      Ide.indCessaoMeiosRede := StrToTIndicador(INIRec.ReadString(sSecao, 'indCessaoMeiosRede', '0'));
      Ide.indNotaEntrada := StrToTIndicador(INIRec.ReadString(sSecao, 'indNotaEntrada', '0'));
      Ide.dhCont := StringToDateTime(INIRec.ReadString(sSecao, 'dhCont', '0'));
      Ide.xJust := INIRec.ReadString(sSecao, 'xJust', '');

      sSecao := 'emit';
      Emit.CNPJ := INIRec.ReadString(sSecao, 'CNPJ', '');
      Emit.IE := INIRec.ReadString(sSecao, 'IE', '');
      Emit.IEUFDest := INIRec.ReadString(sSecao, 'IEUFDest', '');
      Emit.CRT := StrToCRT(INIRec.ReadString(sSecao, 'CRT', '3'));
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

      Ide.cUF := INIRec.ReadInteger(sSecao, 'cUF', UFparaCodigo(Emit.EnderEmit.UF));
      Ide.cMunFG := INIRec.ReadInteger(sSecao, 'cMunFG', Emit.EnderEmit.cMun);

      sSecao := 'dest';
      Dest.xNome := INIRec.ReadString(sSecao, 'xNome', '');
      Dest.CNPJCPF := INIRec.ReadString(sSecao, 'CNPJCPF', '');
      Dest.idOutros := INIRec.ReadString(sSecao, 'idOutros','');
      Dest.indIEDest := StrToindIEDest(INIRec.ReadString(sSecao, 'indIEDest', '1'));
      Dest.IE := INIRec.ReadString(sSecao, 'IE', '');
      Dest.IM := INIRec.ReadString(sSecao, 'IM', '');
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

      sSecao := 'assinante';
      assinante.iCodAssinante := INIRec.ReadString(sSecao, 'iCodAssinante', '');
      assinante.tpAssinante := StrTotpAssinante(INIRec.ReadString(sSecao, 'tpAssinante', '1'));
      assinante.tpServUtil := StrTotpServUtil(INIRec.ReadString(sSecao, 'tpServUtil', '1'));
      assinante.nContrato := INIRec.ReadString(sSecao, 'nContrato', '');
      assinante.dContratoIni := StringToDateTime(INIRec.ReadString(sSecao, 'dContratoIni', '0'));
      assinante.dContratoFim := StringToDateTime(INIRec.ReadString(sSecao, 'dContratoFim', '0'));
      assinante.NroTermPrinc := INIRec.ReadString(sSecao, 'NroTermPrinc', '');
      assinante.cUFPrinc := INIRec.ReadInteger(sSecao, 'cUFPrinc', 0);

      i := 1;
      while true do
      begin
        sSecao := 'TermAdic' + IntToStrZero(i, 3);
        sFim := INIRec.ReadString(sSecao, 'NroTermAdic', 'FIM');
        if sFim = 'FIM' then
          break;

        with assinante.TermAdic.New do
        begin
          NroTermAdic := sFim;
          cUFAdic := INIRec.ReadInteger(sSecao, 'cUFAdic', 0);
        end;

        Inc(i);
      end;

      sSecao := 'gSub';
      gSub.chNFCom := INIRec.ReadString(sSecao, 'chNFCom', '');
      gSub.motSub := StrToMotSub(INIRec.ReadString(sSecao, 'motSub', '1'));
      gSub.gNF.CNPJ := INIRec.ReadString(sSecao, 'CNPJ', '');
      gSub.gNF.Modelo := INIRec.ReadInteger(sSecao, 'Modelo', 0);
      gSub.gNF.Serie := INIRec.ReadString(sSecao, 'Serie', '');
      gSub.gNF.nNF := INIRec.ReadInteger(sSecao, 'nNF', 0);
      gSub.gNF.CompetEmis := StringToDateTime(INIRec.ReadString(sSecao, 'CompetEmis', '0'));
      gSub.gNF.hash115 := INIRec.ReadString(sSecao, 'hash115', '');

      sSecao := 'gCofat';
      gCofat.chNFComLocal := INIRec.ReadString(sSecao, 'chNFComLocal', '');

      i := 1;
      while true do
      begin
        sSecao := 'det' + IntToStrZero(i, 3);
        nItemDet := INIRec.ReadString(sSecao, 'nItem', 'FIM');
        if nItemDet = 'FIM' then
          break;

        with Det.New do
        begin
          nItem := StrToIntDef(nItemDet, 0);
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

        Inc(i);
      end;

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
      // Vai ser pois é uma lista
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
    end;

    GerarXML;

    Result := True;
  finally
    INIRec.Free;
  end;
end;

function NotaFiscal.GerarNFComIni: string;
var
  INIRec: TMemIniFile;
  IniNFCom: TStringList;
//  I, J, K: integer;
//  sSecao: string;
begin
  {
    Atenção procurar seguir a risca a nomenclatura do manual
  }

  Result := '';

  if not ValidarChave(NFCom.infNFCom.ID) then
    raise EACBrNFComException.Create('NFCom Inconsistente para gerar INI. Chave Inválida.');

  INIRec := TMemIniFile.Create('');
  try
    with FNFCom do
    begin
      INIRec.WriteString('infNFCom', 'ID', infNFCom.ID);
      INIRec.WriteString('infNFCom', 'Versao', FloatToStr(infNFCom.Versao));
      INIRec.WriteInteger('ide', 'cUF', Ide.cUF);
      INIRec.WriteInteger('ide', 'Codigo', Ide.cNF);
      INIRec.WriteInteger('ide', 'Modelo', Ide.modelo);
      INIRec.WriteInteger('ide', 'Serie', Ide.serie);
      INIRec.WriteInteger('ide', 'nNF', Ide.nNF);
      INIRec.WriteString('ide', 'dhEmi', DateTimeToStr(Ide.dhEmi));
      INIRec.WriteInteger('ide', 'cMunFG', Ide.cMunFG);
//      INIRec.WriteString('ide', 'tpAmb', TpAmbToStr(Ide.tpAmb));
//      INIRec.WriteString('ide', 'tpemis', TpEmisToStr(Ide.tpemis));
      INIRec.WriteString('ide', 'finNFCom', FinNFComToStr(Ide.finNFCom));
      INIRec.WriteString('ide', 'verProc', Ide.verProc);
      INIRec.WriteString('ide', 'dhCont', DateToStr(Ide.dhCont));
      INIRec.WriteString('ide', 'xJust', Ide.xJust);
      {
      for I := 0 to Ide.NFref.Count - 1 do
      begin
        with Ide.NFref.Items[i] do
        begin
          sSecao := 'NFRef' + IntToStrZero(I + 1, 3);
          if trim(refNFCom) <> '' then
          begin
            INIRec.WriteString(sSecao, 'Tipo', 'NFCom');
            INIRec.WriteString(sSecao, 'refNFCom', refNFCom);
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
                INIRec.WriteString(sSecao, 'chNFCom', chNFCom);
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
              if NFCom.infNFCom.Versao >= 4 then
              begin
                INIRec.WriteString(sSecao, 'cProdANVISA', cProdANVISA);
                INIRec.WriteString(sSecao, 'xMotivoIsencao', xMotivoIsencao);
              end;

              if NFCom.infNFCom.Versao < 4 then
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
                INIRec.WriteString(sSecao, 'indISS', indISSToStr(indISS));
                INIRec.Writestring(sSecao, 'cServico', cServico);
                INIRec.WriteInteger(sSecao, 'cMun', cMun);
                INIRec.WriteInteger(sSecao, 'cPais', cPais);
                INIRec.WriteString(sSecao, 'nProcesso', nProcesso);
                INIRec.WriteString(sSecao, 'indIncentivo', indIncentivoToStr(indIncentivo));
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

      INIRec.WriteString('procNFCom', 'tpAmb', TpAmbToStr(procNFCom.tpAmb));
      INIRec.WriteString('procNFCom', 'verAplic', procNFCom.verAplic);
      INIRec.WriteString('procNFCom', 'chNFCom', procNFCom.chNFCom);
      INIRec.WriteString('procNFCom', 'dhRecbto', DateTimeToStr(procNFCom.dhRecbto));
      INIRec.WriteString('procNFCom', 'nProt', procNFCom.nProt);
      INIRec.WriteString('procNFCom', 'digVal', procNFCom.digVal);
      INIRec.WriteString('procNFCom', 'cStat', IntToStr(procNFCom.cStat));
      INIRec.WriteString('procNFCom', 'xMotivo', procNFCom.xMotivo);
      }
    end;

  finally
    IniNFCom := TStringList.Create;
    try
      INIRec.GetStrings(IniNFCom);
      INIRec.Free;
      Result := StringReplace(IniNFCom.Text, sLineBreak + sLineBreak, sLineBreak, [rfReplaceAll]);
    finally
      IniNFCom.Free;
    end;
  end;

end;

function NotaFiscal.GravarXML(const NomeArquivo: string; const PathArquivo: string): Boolean;
begin
  if EstaVazio(FXMLOriginal) then
    GerarXML;

  FNomeArq := CalcularNomeArquivoCompleto(NomeArquivo, PathArquivo);

  Result := TACBrNFCom(TNotasFiscais(Collection).ACBrNFCom).Gravar(FNomeArq, FXMLOriginal);
end;

function NotaFiscal.GravarStream(AStream: TStream): Boolean;
begin
  if EstaVazio(FXMLOriginal) then
    GerarXML;

  AStream.Size := 0;
  WriteStrToStream(AStream, AnsiString(FXMLOriginal));

  Result := True;
end;

procedure NotaFiscal.EnviarEmail(const sPara, sAssunto: string; sMensagem: TStrings;
  EnviaPDF: Boolean; sCC: TStrings; Anexos: TStrings; sReplyTo: TStrings);
var
  NomeArq_temp : string;
  AnexosEmail:TStrings;
  StreamNFCom : TMemoryStream;
begin
  if not Assigned(TACBrNFCom(TNotasFiscais(Collection).ACBrNFCom).MAIL) then
    raise EACBrNFComException.Create('Componente ACBrMail não associado');

  AnexosEmail := TStringList.Create;
  StreamNFCom := TMemoryStream.Create;
  try
    AnexosEmail.Clear;

    if Assigned(Anexos) then
      AnexosEmail.Assign(Anexos);

    with TACBrNFCom(TNotasFiscais(Collection).ACBrNFCom) do
    begin
      Self.GravarStream(StreamNFCom);

      if (EnviaPDF) then
      begin
        if Assigned(DANFCom) then
        begin
          DANFCom.ImprimirDANFComPDF(FNFCom);
          NomeArq_temp := PathWithDelim(DANFCom.PathPDF) + NumID + '-NFCom.pdf';
          AnexosEmail.Add(NomeArq_temp);
        end;
      end;

      EnviarEmail(sPara, sAssunto, sMensagem, sCC, AnexosEmail, StreamNFCom,
                   NumID +'-NFCom.xml', sReplyTo);
    end;
  finally
    AnexosEmail.Free;
    StreamNFCom.Free;
  end;
end;

function NotaFiscal.GerarXML: string;
var
  IdAnterior : string;
begin
  with TACBrNFCom(TNotasFiscais(Collection).ACBrNFCom) do
  begin
    IdAnterior := NFCom.infNFCom.ID;

    FNFComW.Opcoes.FormatoAlerta  := Configuracoes.Geral.FormatoAlerta;
    FNFComW.Opcoes.RetirarAcentos := Configuracoes.Geral.RetirarAcentos;
    FNFComW.Opcoes.RetirarEspacos := Configuracoes.Geral.RetirarEspacos;
    FNFComW.Opcoes.IdentarXML := Configuracoes.Geral.IdentarXML;
    FNFComW.Opcoes.NormatizarMunicipios  := Configuracoes.Arquivos.NormatizarMunicipios;
    FNFComW.Opcoes.PathArquivoMunicipios := Configuracoes.Arquivos.PathArquivoMunicipios;

    pcnAuxiliar.TimeZoneConf.Assign(Configuracoes.WebServices.TimeZoneConf);

    FNFComW.idCSRT := Configuracoes.RespTec.IdCSRT;
    FNFComW.CSRT   := Configuracoes.RespTec.CSRT;
  end;

  FNFComW.GerarXml;
  //DEBUG
  //WriteToTXT('c:\temp\Notafiscal.xml', FNFComW.Document.Xml, False, False);

  XMLOriginal := FNFComW.Document.Xml;

  { XML gerado pode ter nova Chave e ID, então devemos calcular novamente o
    nome do arquivo, mantendo o PATH do arquivo carregado }
  if (NaoEstaVazio(FNomeArq) and (IdAnterior <> FNFCom.infNFCom.ID)) then
    FNomeArq := CalcularNomeArquivoCompleto('', ExtractFilePath(FNomeArq));

  FAlertas := ACBrStr(FNFComW.ListaDeAlertas.Text);

  Result := FXMLOriginal;
end;

function NotaFiscal.CalcularNomeArquivo: string;
var
  xID: string;
  NomeXML: string;
begin
  xID := Self.NumID;

  if EstaVazio(xID) then
    raise EACBrNFComException.Create('ID Inválido. Impossível Salvar XML');

  NomeXML := '-NFCom.xml';

  Result := xID + NomeXML;
end;

function NotaFiscal.CalcularPathArquivo: string;
var
  Data: TDateTime;
begin
  with TACBrNFCom(TNotasFiscais(Collection).ACBrNFCom) do
  begin
    if Configuracoes.Arquivos.EmissaoPathNFCom then
      Data := FNFCom.Ide.dhEmi
    else
      Data := Now;

    Result := PathWithDelim(Configuracoes.Arquivos.GetPathNFCom(Data, FNFCom.Emit.CNPJ));
  end;
end;

function NotaFiscal.CalcularNomeArquivoCompleto(NomeArquivo: string;
  PathArquivo: string): string;
var
  PathNoArquivo: string;
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

function NotaFiscal.ValidarConcatChave: Boolean;
var
  wAno, wMes, wDia: word;
  chaveNFCom : string;
begin
  DecodeDate(NFCom.ide.dhEmi, wAno, wMes, wDia);

  chaveNFCom := OnlyNumber(NFCom.infNFCom.ID);
  {(*}
  Result := not
    ((Copy(chaveNFCom, 1, 2) <> IntToStrZero(NFCom.Ide.cUF, 2)) or
    (Copy(chaveNFCom, 3, 2)  <> Copy(FormatFloat('0000', wAno), 3, 2)) or
    (Copy(chaveNFCom, 5, 2)  <> FormatFloat('00', wMes)) or
    (Copy(chaveNFCom, 7, 14)<> PadLeft(OnlyNumber(NFCom.Emit.CNPJ), 14, '0')) or
    (Copy(chaveNFCom, 21, 2) <> IntToStrZero(NFCom.Ide.modelo, 2)) or
    (Copy(chaveNFCom, 23, 3) <> IntToStrZero(NFCom.Ide.serie, 3)) or
    (Copy(chaveNFCom, 26, 9) <> IntToStrZero(NFCom.Ide.nNF, 9)) or
    (Copy(chaveNFCom, 35, 1) <> TipoEmissaoToStr(NFCom.Ide.tpEmis)) or
    (Copy(chaveNFCom, 36, 1) <> SiteAutorizadorToStr(NFCom.Ide.nSiteAutoriz)) or
    (Copy(chaveNFCom, 37, 7) <> IntToStrZero(NFCom.Ide.cNF, 7)));
  {*)}
end;

function NotaFiscal.GetConfirmada: Boolean;
begin
  Result := TACBrNFCom(TNotasFiscais(Collection).ACBrNFCom).CstatConfirmada(
    FNFCom.procNFCom.cStat);
end;

function NotaFiscal.GetcStat: Integer;
begin
  Result := FNFCom.procNFCom.cStat;
end;

function NotaFiscal.GetProcessada: Boolean;
begin
  Result := TACBrNFCom(TNotasFiscais(Collection).ACBrNFCom).CstatProcessado(
    FNFCom.procNFCom.cStat);
end;

function NotaFiscal.GetCancelada: Boolean;
begin
  Result := TACBrNFCom(TNotasFiscais(Collection).ACBrNFCom).CstatCancelada(
    FNFCom.procNFCom.cStat);
end;

function NotaFiscal.GetMsg: string;
begin
  Result := FNFCom.procNFCom.xMotivo;
end;

function NotaFiscal.GetNumID: string;
begin
  Result := OnlyNumber(NFCom.infNFCom.ID);
end;

function NotaFiscal.GetXMLAssinado: string;
begin
  if EstaVazio(FXMLAssinado) then
    Assinar;

  Result := FXMLAssinado;
end;

procedure NotaFiscal.SetXML(const AValue: string);
begin
  LerXML(AValue);
end;

procedure NotaFiscal.SetXMLOriginal(const AValue: string);
var
  XMLUTF8: string;
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
  if not (AOwner is TACBrNFCom) then
    raise EACBrNFComException.Create('AOwner deve ser do tipo TACBrNFCom');

  inherited Create(AOwner, ItemClass);

  FACBrNFCom := TACBrNFCom(AOwner);
  FConfiguracoes := TACBrNFCom(FACBrNFCom).Configuracoes;
end;

function TNotasFiscais.Add: NotaFiscal;
begin
  Result := NotaFiscal(inherited Add);
end;

procedure TNotasFiscais.Assinar;
var
  i: integer;
begin
  for i := 0 to Self.Count - 1 do
    Self.Items[i].Assinar;
end;

procedure TNotasFiscais.GerarNFCom;
var
  i: integer;
begin
  for i := 0 to Self.Count - 1 do
    Self.Items[i].GerarXML;
end;

function TNotasFiscais.GetItem(Index: integer): NotaFiscal;
begin
  Result := NotaFiscal(inherited Items[Index]);
end;

function TNotasFiscais.GetNamePath: string;
begin
  Result := 'NotaFiscal';
end;

procedure TNotasFiscais.VerificarDANFCom;
begin
  if not Assigned(TACBrNFCom(FACBrNFCom).DANFCom) then
    raise EACBrNFComException.Create('Componente DANFCom não associado.');
end;

procedure TNotasFiscais.Imprimir;
begin
  VerificarDANFCom;
  TACBrNFCom(FACBrNFCom).DANFCom.ImprimirDANFCom(nil);
end;

procedure TNotasFiscais.ImprimirCancelado;
begin
  VerificarDANFCom;
  TACBrNFCom(FACBrNFCom).DANFCom.ImprimirDANFComCancelado(nil);
end;

procedure TNotasFiscais.ImprimirResumido;
begin
  VerificarDANFCom;
  TACBrNFCom(FACBrNFCom).DANFCom.ImprimirDANFComResumido(nil);
end;

procedure TNotasFiscais.ImprimirPDF;
begin
  VerificarDANFCom;
  TACBrNFCom(FACBrNFCom).DANFCom.ImprimirDANFComPDF(nil);
end;

procedure TNotasFiscais.ImprimirResumidoPDF;
begin
  VerificarDANFCom;
  TACBrNFCom(FACBrNFCom).DANFCom.ImprimirDANFComResumidoPDF(nil);
end;

function TNotasFiscais.Insert(Index: integer): NotaFiscal;
begin
  Result := NotaFiscal(inherited Insert(Index));
end;

procedure TNotasFiscais.SetItem(Index: integer; const Value: NotaFiscal);
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

function TNotasFiscais.VerificarAssinatura(out Erros: string): Boolean;
var
  i: integer;
begin
  Result := True;
  Erros := '';

  if Self.Count < 1 then
  begin
    Erros := 'Nenhuma NFCom carregada';
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

function TNotasFiscais.ValidarRegrasdeNegocios(out Erros: string): Boolean;
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

function TNotasFiscais.LoadFromFile(const CaminhoArquivo: string;
  AGerarNFCom: Boolean): Boolean;
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
  Result := LoadFromString(String(XMLUTF8), AGerarNFCom);

  if Result then
  begin
    // Atribui Nome do arquivo a novas notas inseridas //
    for i := l to Self.Count - 1 do
      Self.Items[i].NomeArq := CaminhoArquivo;
  end;
end;

function TNotasFiscais.LoadFromStream(AStream: TStringStream;
  AGerarNFCom: Boolean): Boolean;
var
  AXML: AnsiString;
begin
  AStream.Position := 0;
  AXML := ReadStrFromStream(AStream, AStream.Size);

  Result := Self.LoadFromString(String(AXML), AGerarNFCom);
end;

function TNotasFiscais.LoadFromString(const AXMLString: string;
  AGerarNFCom: Boolean): Boolean;
var
  ANFComXML, XMLStr: AnsiString;
  P, N: integer;

  function PosNFCom: integer;
  begin
    Result := pos('</NFCom>', XMLStr);
  end;

begin
  // Verifica se precisa Converter de UTF8 para a string nativa da IDE //
  XMLStr := ConverteXMLtoNativeString(AXMLString);

  N := PosNFCom;
  while N > 0 do
  begin
    P := pos('</NFComProc>', XMLStr);

    if P <= 0 then
      P := pos('</procNFCom>', XMLStr);  // NFCom obtida pelo Portal da Receita

    if P > 0 then
    begin
      ANFComXML := copy(XMLStr, 1, P + 11);
      XMLStr := Trim(copy(XMLStr, P + 11, length(XMLStr)));
    end
    else
    begin
      ANFComXML := copy(XMLStr, 1, N + 7);
      XMLStr := Trim(copy(XMLStr, N + 8, length(XMLStr)));
    end;

    with Self.Add do
    begin
      LerXML(ANFComXML);

      if AGerarNFCom then // Recalcula o XML
        GerarXML;
    end;

    N := PosNFCom;
  end;

  Result := Self.Count > 0;
end;

function TNotasFiscais.LoadFromIni(const AIniString: string): Boolean;
begin
  with Self.Add do
    LerArqIni(AIniString);

  Result := Self.Count > 0;
end;

function TNotasFiscais.GerarIni: string;
begin
  Result := '';

  if (Self.Count > 0) then
    Result := Self.Items[0].GerarNFComIni;
end;

function TNotasFiscais.GravarXML(const APathNomeArquivo: string): Boolean;
var
  i: integer;
  NomeArq, PathArq : string;
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
