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
  ACBrNFComXmlReader, ACBrNFComXmlWriter;

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
    property XML: string read FXMLOriginal write SetXML;
    // Atribuir a "XMLOriginal", reflete em XMLAssinado, se existir a tag de assinatura
    // Sempre deve estar em UTF8
    property XMLOriginal: string read FXMLOriginal write SetXMLOriginal;
    // Sempre deve estar em UTF8
    property XMLAssinado: string read GetXMLAssinado write FXMLAssinado;
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
  ACBrDFeUtil,
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
  ANode, SignatureNode, ReferenceNode, X509DataNode: TACBrXmlNode;
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
          ReferenceNode := SignatureNode.Childrens.FindAnyNs('SignedInfo')
                                        .Childrens.FindAnyNs('Reference');
          X509DataNode :=  SignatureNode.Childrens.FindAnyNs('KeyInfo')
                                        .Childrens.FindAnyNs('X509Data');

          NFCom.signature.URI := ObterConteudoTag(ReferenceNode.Attributes.Items['URI']);
          NFCom.signature.DigestValue := ObterConteudoTag(ReferenceNode.Childrens.FindAnyNs('DigestValue'), tcStr);
          NFCom.signature.SignatureValue := ObterConteudoTag(SignatureNode.Childrens.FindAnyNs('SignatureValue'), tcStr);
          NFCom.signature.X509Certificate := ObterConteudoTag(X509DataNode.Childrens.FindAnyNs('X509Certificate'), tcStr);
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

      Ide.cUF := INIRec.ReadInteger(sSecao, 'cUF', UFparaCodigoUF(Emit.EnderEmit.UF));
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
  sSecao: string;
  i, j: Integer;
begin
  Result := '';

  if not ValidarChave(NFCom.infNFCom.ID) then
    raise EACBrNFComException.Create('NFCom Inconsistente para gerar INI. Chave Inválida.');

  INIRec := TMemIniFile.Create('');
  try
    with FNFCom do
    begin
      INIRec.WriteString('infNFCom', 'ID', infNFCom.ID);
      INIRec.WriteString('infNFCom', 'Versao', FloatToStr(infNFCom.Versao));

      sSecao := 'ide';
      INIRec.WriteInteger(sSecao, 'cUF', Ide.cUF);
      INIRec.WriteString(sSecao, 'tpAmb', TipoAmbienteToStr(Ide.tpAmb));
      INIRec.WriteInteger(sSecao, 'Modelo', Ide.modelo);
      INIRec.WriteInteger(sSecao, 'Serie', Ide.serie);
      INIRec.WriteInteger(sSecao, 'nNF', Ide.nNF);
      INIRec.WriteInteger(sSecao, 'cNF', Ide.cNF);
      INIRec.WriteString(sSecao, 'dhEmi', DateTimeToStr(Ide.dhEmi));
      INIRec.WriteString(sSecao, 'tpemis', TipoEmissaoToStr(Ide.tpemis));
      INIRec.WriteString(sSecao, 'nSiteAutoriz', SiteAutorizadorToStr(Ide.nSiteAutoriz));
      INIRec.WriteInteger(sSecao, 'cMunFG', Ide.cMunFG);
      INIRec.WriteString(sSecao, 'finNFCom', FinNFComToStr(Ide.finNFCom));
      INIRec.WriteString(sSecao, 'tpFat', TipoFaturamentoToStr(Ide.tpFat));
      INIRec.WriteString(sSecao, 'verProc', Ide.verProc);
      INIRec.WriteString(sSecao, 'indPrePago', TIndicadorToStr(Ide.indPrePago));
      INIRec.WriteString(sSecao, 'indCessaoMeiosRede', TIndicadorToStr(Ide.indCessaoMeiosRede));
      INIRec.WriteString(sSecao, 'indNotaEntrada', TIndicadorToStr(Ide.indNotaEntrada));
      INIRec.WriteString(sSecao, 'dhCont', DateToStr(Ide.dhCont));
      INIRec.WriteString(sSecao, 'xJust', Ide.xJust);

      sSecao := 'emit';
      INIRec.WriteString(sSecao, 'CNPJ', Emit.CNPJ);
      INIRec.WriteString(sSecao, 'IE', Emit.IE);
      INIRec.WriteString(sSecao, 'IEUFDest', Emit.IEUFDest);
      INIRec.WriteString(sSecao, 'CRT', CRTToStr(Emit.CRT));
      INIRec.WriteString(sSecao, 'xNome', Emit.xNome);
      INIRec.WriteString(sSecao, 'xFant', Emit.xFant);
      // Endereço do Emitente
      INIRec.WriteString(sSecao, 'xLgr', Emit.EnderEmit.xLgr);
      INIRec.WriteString(sSecao, 'nro', Emit.EnderEmit.nro);
      INIRec.WriteString(sSecao, 'xCpl', Emit.EnderEmit.xCpl);
      INIRec.WriteString(sSecao, 'xBairro', Emit.EnderEmit.xBairro);
      INIRec.WriteInteger(sSecao, 'cMun', Emit.EnderEmit.cMun);
      INIRec.WriteString(sSecao, 'xMun', Emit.EnderEmit.xMun);
      INIRec.WriteInteger(sSecao, 'CEP', Emit.EnderEmit.CEP);
      INIRec.WriteString(sSecao, 'UF', Emit.EnderEmit.UF);
      INIRec.WriteString(sSecao, 'fone', Emit.EnderEmit.fone);
      INIRec.WriteString(sSecao, 'email', Emit.EnderEmit.email);

      sSecao := 'dest';
      INIRec.WriteString(sSecao, 'xNome', Dest.xNome);
      INIRec.WriteString(sSecao, 'CNPJCPF', Dest.CNPJCPF);
      INIRec.WriteString(sSecao, 'idOutros', Dest.idOutros);
      INIRec.WriteString(sSecao, 'indIEDest', indIEDestToStr(Dest.indIEDest));
      INIRec.WriteString(sSecao, 'IE', Dest.IE);
      INIRec.WriteString(sSecao, 'IM', Dest.IM);
      // Endereço do Destinatario
      INIRec.WriteString(sSecao, 'xLgr', Dest.EnderDest.xLgr);
      INIRec.WriteString(sSecao, 'nro', Dest.EnderDest.nro);
      INIRec.WriteString(sSecao, 'xCpl', Dest.EnderDest.xCpl);
      INIRec.WriteString(sSecao, 'xBairro', Dest.EnderDest.xBairro);
      INIRec.WriteInteger(sSecao, 'cMun', Dest.EnderDest.cMun);
      INIRec.WriteString(sSecao, 'xMun', Dest.EnderDest.xMun);
      INIRec.WriteInteger(sSecao, 'CEP', Dest.EnderDest.CEP);
      INIRec.WriteString(sSecao, 'UF', Dest.EnderDest.UF);
      INIRec.WriteString(sSecao, 'fone', Dest.EnderDest.fone);
      INIRec.WriteString(sSecao, 'email', Dest.EnderDest.email);

      sSecao := 'assinante';
      INIRec.WriteString(sSecao, 'iCodAssinante', assinante.iCodAssinante);
      INIRec.WriteString(sSecao, 'tpAssinante', tpAssinanteToStr(assinante.tpAssinante));
      INIRec.WriteString(sSecao, 'tpServUtil', tpServUtilToStr(assinante.tpServUtil));
      INIRec.WriteString(sSecao, 'nContrato', assinante.nContrato);
      INIRec.WriteString(sSecao, 'dContratoIni', DateTimeToStr(assinante.dContratoIni));
      INIRec.WriteString(sSecao, 'dContratoFim', DateTimeToStr(assinante.dContratoFim));
      INIRec.WriteString(sSecao, 'NroTermPrinc', assinante.NroTermPrinc);
      INIRec.WriteInteger(sSecao, 'cUFPrinc', assinante.cUFPrinc);

      for i := 0 to assinante.TermAdic.Count - 1 do
      begin
        sSecao := 'TermAdic' + IntToStrZero(i + 1, 3);

        with assinante.TermAdic.Items[i] do
        begin
          INIRec.WriteString(sSecao, 'NroTermAdic', NroTermAdic);
          INIRec.WriteInteger(sSecao, 'cUFAdic', cUFAdic);
        end;
      end;

      sSecao := 'gSub';
      INIRec.WriteString(sSecao, 'chNFCom', gSub.chNFCom);
      INIRec.WriteString(sSecao, 'motSub', MotSubToStr(gSub.motSub));
      INIRec.WriteString(sSecao, 'CNPJ', gSub.gNF.CNPJ);
      INIRec.WriteInteger(sSecao, 'Modelo', gSub.gNF.Modelo);
      INIRec.WriteString(sSecao, 'Serie', gSub.gNF.Serie);
      INIRec.WriteInteger(sSecao, 'nNF', gSub.gNF.nNF);
      INIRec.WriteString(sSecao, 'CompetEmis', DateTimeToStr(gSub.gNF.CompetEmis));
      INIRec.WriteString(sSecao, 'hash115', gSub.gNF.hash115);

      sSecao := 'gCofat';
      INIRec.WriteString(sSecao, 'chNFComLocal', gCofat.chNFComLocal);

      for i := 0 to Det.Count - 1 do
      begin
        sSecao := 'det' + IntToStrZero(i + 1, 3);

        with Det.Items[i] do
        begin
          INIRec.WriteInteger(sSecao, 'nItem', nItem);
          INIRec.WriteString(sSecao, 'chNFComAnt', chNFComAnt);
          INIRec.WriteInteger(sSecao, 'nItemAnt', nItemAnt);
          INIRec.WriteString(sSecao, 'infAdProd', infAdProd);
          // Informações do produto
          INIRec.WriteString(sSecao, 'cProd', Prod.cProd);
          INIRec.WriteString(sSecao, 'xProd', Prod.xProd);
          INIRec.WriteString(sSecao, 'cClass', Prod.cClass);
          INIRec.WriteInteger(sSecao, 'CFOP', Prod.CFOP);
          INIRec.WriteString(sSecao, 'CNPJLD', Prod.CNPJLD);
          INIRec.WriteString(sSecao, 'uMed', uMedToStr(Prod.uMed));
          INIRec.WriteFloat(sSecao, 'qFaturada', Prod.qFaturada);
          INIRec.WriteFloat(sSecao, 'vItem', Prod.vItem);
          INIRec.WriteFloat(sSecao, 'vDesc', Prod.vDesc);
          INIRec.WriteFloat(sSecao, 'vOutro', Prod.vOutro);
          INIRec.WriteFloat(sSecao, 'vProd', Prod.vProd);
          INIRec.WriteString(sSecao, 'dExpiracao', DateTimeToStr(Prod.dExpiracao));
          INIRec.WriteString(sSecao, 'indDevolucao', TIndicadorToStr(Prod.indDevolucao));
        end;

        sSecao := 'ICMS' + IntToStrZero(i + 1, 3);
        with Det.Items[i].Imposto do
        begin
          INIRec.WriteString(sSecao, 'CST', CSTICMSToStr(ICMS.CST));
          INIRec.WriteFloat(sSecao, 'vBC', ICMS.vBC);
          INIRec.WriteFloat(sSecao, 'pICMS', ICMS.pICMS);
          INIRec.WriteFloat(sSecao, 'vICMS', ICMS.vICMS);
          INIRec.WriteFloat(sSecao, 'pFCP', ICMS.pFCP);
          INIRec.WriteFloat(sSecao, 'vFCP', ICMS.vFCP);
          INIRec.WriteFloat(sSecao, 'pRedBC', ICMS.pRedBC);
          INIRec.WriteFloat(sSecao, 'vICMSDeson', ICMS.vICMSDeson);
          INIRec.WriteString(sSecao, 'cBenef', ICMS.cBenef);
        end;

        for j := 0 to Det.Items[i].Imposto.ICMSUFDest.Count - 1 do
        begin
          sSecao := 'ICMSUFDest' + IntToStrZero(i + 1, 3) + IntToStrZero(j + 1, 3);

          with Det.Items[i].Imposto.ICMSUFDest.Items[j] do
          begin
            INIRec.WriteInteger(sSecao, 'cUFDest', cUFDest);
            INIRec.WriteFloat(sSecao, 'vBCUFDest', vBCUFDest);
            INIRec.WriteFloat(sSecao, 'pFCPUFDest', pFCPUFDest);
            INIRec.WriteFloat(sSecao, 'pICMSUFDest', pICMSUFDest);
            INIRec.WriteFloat(sSecao, 'pICMSInter', pICMSInter);
            INIRec.WriteFloat(sSecao, 'vFCPUFDest', vFCPUFDest);
            INIRec.WriteFloat(sSecao, 'vICMSUFDest', vICMSUFDest);
            INIRec.WriteFloat(sSecao, 'vICMSUFEmi', vICMSUFEmi);
            INIRec.WriteString(sSecao, 'cBenefUFDest', cBenefUFDest);
          end;
        end;

        sSecao := 'PIS' + IntToStrZero(i + 1, 3);
        with Det.Items[i].Imposto do
        begin
          INIRec.WriteString(sSecao, 'CST', CSTPISToStr(PIS.CST));
          INIRec.WriteFloat(sSecao, 'vBC', PIS.vBC);
          INIRec.WriteFloat(sSecao, 'pPIS', PIS.pPIS);
          INIRec.WriteFloat(sSecao, 'vPIS', PIS.vPIS);
        end;

        sSecao := 'COFINS' + IntToStrZero(i + 1, 3);
        with Det.Items[i].Imposto do
        begin
          INIRec.WriteString(sSecao, 'CST', CSTCOFINSToStr(COFINS.CST));
          INIRec.WriteFloat(sSecao, 'vBC', COFINS.vBC);
          INIRec.WriteFloat(sSecao, 'pCOFINS', COFINS.pCOFINS);
          INIRec.WriteFloat(sSecao, 'vCOFINS', COFINS.vCOFINS);
        end;

        sSecao := 'FUST' + IntToStrZero(i + 1, 3);
        with Det.Items[i].Imposto do
        begin
          INIRec.WriteFloat(sSecao, 'vBC', FUST.vBC);
          INIRec.WriteFloat(sSecao, 'pFUST', FUST.pFUST);
          INIRec.WriteFloat(sSecao, 'vFUST', FUST.vFUST);
        end;

        sSecao := 'FUNTTEL' + IntToStrZero(i + 1, 3);
        with Det.Items[i].Imposto do
        begin
          INIRec.WriteFloat(sSecao, 'vBC', FUNTTEL.vBC);
          INIRec.WriteFloat(sSecao, 'pFUNTTEL', FUNTTEL.pFUNTTEL);
          INIRec.WriteFloat(sSecao, 'vFUNTTEL', FUNTTEL.vFUNTTEL);
        end;

        sSecao := 'retTrib' + IntToStrZero(i + 1, 3);
        with Det.Items[i].Imposto do
        begin
          INIRec.WriteFloat(sSecao, 'vRetPIS', retTrib.vRetPIS);
          INIRec.WriteFloat(sSecao, 'vRetCOFINS', retTrib.vRetCOFINS);
          INIRec.WriteFloat(sSecao, 'vRetCSLL', retTrib.vRetCSLL);
          INIRec.WriteFloat(sSecao, 'vBCIRRF', retTrib.vBCIRRF);
          INIRec.WriteFloat(sSecao, 'vIRRF', retTrib.vIRRF);
        end;

        sSecao := 'gProcRef' + IntToStrZero(i + 1, 3);
        with Det.Items[i] do
        begin
          INIRec.WriteFloat(sSecao, 'vItem', gProcRef.vItem);
          INIRec.WriteInteger(sSecao, 'qFaturada', gProcRef.qFaturada);
          INIRec.WriteFloat(sSecao, 'vProd', gProcRef.vProd);
          INIRec.WriteFloat(sSecao, 'vDesc', gProcRef.vDesc);
          INIRec.WriteFloat(sSecao, 'vOutro', gProcRef.vOutro);
          INIRec.WriteString(sSecao, 'indDevolucao', TIndicadorToStr(gProcRef.indDevolucao));
          INIRec.WriteFloat(sSecao, 'vBC', gProcRef.vBC);
          INIRec.WriteFloat(sSecao, 'pICMS', gProcRef.pICMS);
          INIRec.WriteFloat(sSecao, 'vICMS', gProcRef.vICMS);
          INIRec.WriteFloat(sSecao, 'vPIS', gProcRef.vPIS);
          INIRec.WriteFloat(sSecao, 'vCOFINS', gProcRef.vCOFINS);
        end;

        for j := 0 to Det.Items[i].gProcRef.gProc.Count - 1 do
        begin
          sSecao := 'gProc' + IntToStrZero(i + 1, 3) + IntToStrZero(j + 1, 3);

          with Det.Items[i].gProcRef.gProc.Items[j] do
          begin
            INIRec.WriteString(sSecao, 'tpProc', tpProcToStr(tpProc));
            INIRec.WriteString(sSecao, 'nProcesso', nProcesso);
          end;
        end;

        sSecao := 'gRessarc' + IntToStrZero(i + 1, 3);
        with Det.Items[i] do
        begin
          INIRec.WriteString(sSecao, 'tpRessarc', tpRessarcToStr(gRessarc.tpRessarc));
          INIRec.WriteString(sSecao, 'dRef', DateTimeToStr(gRessarc.dRef));
          INIRec.WriteString(sSecao, 'nProcesso', gRessarc.nProcesso);
          INIRec.WriteString(sSecao, 'nProtReclama', gRessarc.nProtReclama);
          INIRec.WriteString(sSecao, 'xObs', gRessarc.xObs);
        end;
      end;

      sSecao := 'total';
      INIRec.WriteFloat(sSecao, 'vProd', Total.vProd);
      INIRec.WriteFloat(sSecao, 'vBC', Total.vBC);
      INIRec.WriteFloat(sSecao, 'vICMS', Total.vICMS);
      INIRec.WriteFloat(sSecao, 'vICMSDeson', Total.vICMSDeson);
      INIRec.WriteFloat(sSecao, 'vFCP', Total.vFCP);
      INIRec.WriteFloat(sSecao, 'vCOFINS', Total.vCOFINS);
      INIRec.WriteFloat(sSecao, 'vPIS', Total.vPIS);
      INIRec.WriteFloat(sSecao, 'vFUNTTEL', Total.vFUNTTEL);
      INIRec.WriteFloat(sSecao, 'vFUST', Total.vFUST);
      INIRec.WriteFloat(sSecao, 'vRetPIS', Total.vRetPIS);
      INIRec.WriteFloat(sSecao, 'vRetCOFINS', Total.vRetCOFINS);
      INIRec.WriteFloat(sSecao, 'vRetCSLL', Total.vRetCSLL);
      INIRec.WriteFloat(sSecao, 'vIRRF', Total.vIRRF);
      INIRec.WriteFloat(sSecao, 'vDesc', Total.vDesc);
      INIRec.WriteFloat(sSecao, 'vOutro', Total.vOutro);
      INIRec.WriteFloat(sSecao, 'vNF', Total.vNF);

      sSecao := 'gFidelidade';
      INIRec.WriteString(sSecao, 'qtdSaldoPts', gFidelidade.qtdSaldoPts);
      INIRec.WriteString(sSecao, 'dRefSaldoPts', DateTimeToStr(gFidelidade.dRefSaldoPts));
      INIRec.WriteString(sSecao, 'qtdPtsResg', gFidelidade.qtdPtsResg);
      INIRec.WriteString(sSecao, 'dRefResgPts', DateTimeToStr(gFidelidade.dRefResgPts));

      sSecao := 'gFat';
      INIRec.WriteString(sSecao, 'CompetFat', DateTimeToStr(gFat.CompetFat));
      INIRec.WriteString(sSecao, 'dVencFat', DateTimeToStr(gFat.dVencFat));
      INIRec.WriteString(sSecao, 'dPerUsoIni', DateTimeToStr(gFat.dPerUsoIni));
      INIRec.WriteString(sSecao, 'dPerUsoFim', DateTimeToStr(gFat.dPerUsoFim));
      INIRec.WriteString(sSecao, 'codBarras', gFat.codBarras);
      INIRec.WriteString(sSecao, 'codDebAuto', gFat.codDebAuto);
      INIRec.WriteString(sSecao, 'codBanco', gFat.codBanco);
      INIRec.WriteString(sSecao, 'codAgencia', gFat.codAgencia);
      // Endereço do Destinatario
      INIRec.WriteString(sSecao, 'xLgr', gFat.enderCorresp.xLgr);
      INIRec.WriteString(sSecao, 'nro', gFat.enderCorresp.nro);
      INIRec.WriteString(sSecao, 'xCpl', gFat.enderCorresp.xCpl);
      INIRec.WriteString(sSecao, 'xBairro', gFat.enderCorresp.xBairro);
      INIRec.WriteInteger(sSecao, 'cMun', gFat.enderCorresp.cMun);
      INIRec.WriteString(sSecao, 'xMun', gFat.enderCorresp.xMun);
      INIRec.WriteInteger(sSecao, 'CEP', gFat.enderCorresp.CEP);
      INIRec.WriteString(sSecao, 'UF', gFat.enderCorresp.UF);
      INIRec.WriteString(sSecao, 'fone', gFat.enderCorresp.fone);
      INIRec.WriteString(sSecao, 'email', gFat.enderCorresp.email);
      // Chave PIX
      INIRec.WriteString(sSecao, 'urlQRCodePIX', gFat.gPIX.urlQRCodePIX);

      sSecao := 'gFatCentral';
      INIRec.WriteString(sSecao, 'CNPJ', gFatCentral.CNPJ);
      INIRec.WriteInteger(sSecao, 'cUF', gFatCentral.cUF);

      for i := 0 to autXML.Count - 1 do
      begin
        sSecao := 'autXML' + IntToStrZero(i + 1, 2);
        with autXML.Items[i] do
        begin
          INIRec.WriteString(sSecao, 'CNPJCPF', CNPJCPF);
        end;
      end;

      sSecao := 'infAdic';
      INIRec.WriteString(sSecao, 'infAdFisco', InfAdic.infAdFisco);
      // Vai ser alterado pois é uma lista
      INIRec.WriteString(sSecao, 'infCpl', InfAdic.infCpl);

      sSecao := 'infRespTec';
      INIRec.WriteString(sSecao, 'CNPJ', infRespTec.CNPJ);
      INIRec.WriteString(sSecao, 'xContato', infRespTec.xContato);
      INIRec.WriteString(sSecao, 'email', infRespTec.email);
      INIRec.WriteString(sSecao, 'fone', infRespTec.fone);

      sSecao := 'procNFCom';
      INIRec.WriteString(sSecao, 'tpAmb', TipoAmbienteToStr(procNFCom.tpAmb));
      INIRec.WriteString(sSecao, 'verAplic', procNFCom.verAplic);
      INIRec.WriteString(sSecao, 'chNFCom', procNFCom.chNFCom);
      INIRec.WriteString(sSecao, 'dhRecbto', DateTimeToStr(procNFCom.dhRecbto));
      INIRec.WriteString(sSecao, 'nProt', procNFCom.nProt);
      INIRec.WriteString(sSecao, 'digVal', procNFCom.digVal);
      INIRec.WriteString(sSecao, 'cStat', IntToStr(procNFCom.cStat));
      INIRec.WriteString(sSecao, 'xMotivo', procNFCom.xMotivo);
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

    TimeZoneConf.Assign(Configuracoes.WebServices.TimeZoneConf);

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
