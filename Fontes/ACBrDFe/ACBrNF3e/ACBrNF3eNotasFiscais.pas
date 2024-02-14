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

  { NotaFiscal }

  NotaFiscal = class(TCollectionItem)
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

    function GetItem(Index: integer): NotaFiscal;
    procedure SetItem(Index: integer; const Value: NotaFiscal);

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
    function Add: NotaFiscal;
    function Insert(Index: integer): NotaFiscal;

    property Items[Index: integer]: NotaFiscal read GetItem write SetItem; default;

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

constructor NotaFiscal.Create(Collection2: TCollection);
begin
  inherited Create(Collection2);

  FNF3e := TNF3e.Create;
  FNF3eW := TNF3eXmlWriter.Create(FNF3e);
  FNF3eR := TNF3eXmlReader.Create(FNF3e);

  FConfiguracoes := TACBrNF3e(TNotasFiscais(Collection).ACBrNF3e).Configuracoes;

  with TACBrNF3e(TNotasFiscais(Collection).ACBrNF3e) do
  begin
    FNF3e.infNF3e.Versao := VersaoNF3eToDbl(Configuracoes.Geral.VersaoDF);

    FNF3e.Ide.modelo  := 66;
    FNF3e.Ide.verProc := 'ACBrNF3e';
    FNF3e.Ide.tpAmb   := TACBrTipoAmbiente(Configuracoes.WebServices.Ambiente);
    FNF3e.Ide.tpEmis  := TACBrTipoEmissao(Configuracoes.Geral.FormaEmissao);
  end;
end;

destructor NotaFiscal.Destroy;
begin
  FNF3eW.Free;
  FNF3eR.Free;
  FNF3e.Free;

  inherited Destroy;
end;

procedure NotaFiscal.Imprimir;
begin
  with TACBrNF3e(TNotasFiscais(Collection).ACBrNF3e) do
  begin
    if not Assigned(DANF3e) then
      raise EACBrNF3eException.Create('Componente DANF3e não associado.')
    else
      DANF3e.ImprimirDANF3e(NF3e);
  end;
end;

procedure NotaFiscal.ImprimirPDF;
begin
  with TACBrNF3e(TNotasFiscais(Collection).ACBrNF3e) do
  begin
    if not Assigned(DANF3e) then
      raise EACBrNF3eException.Create('Componente DANF3e não associado.')
    else
      DANF3e.ImprimirDANF3ePDF(NF3e);
  end;
end;

procedure NotaFiscal.Assinar;
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
      except
        //Result := False;
      end;
    finally
      FreeAndNil(Document);
    end;

    with TACBrNF3e(TNotasFiscais(Collection).ACBrNF3e) do
    begin
      NF3e.infNF3eSupl.qrCodNF3e := GetURLQRCode(NF3e.Ide.cUF,
                                                 NF3e.Ide.tpAmb,
                                                 NF3e.Ide.tpEmis,
                                                 NF3e.infNF3e.ID,
                                                 NF3e.infNF3e.Versao);

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

function NotaFiscal.VerificarAssinatura: Boolean;
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

function NotaFiscal.ValidarRegrasdeNegocios: Boolean;
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

function NotaFiscal.LerXML(const AXML: String): Boolean;
begin
  XMLOriginal := AXML;

  FNF3eR.Arquivo := XMLOriginal;
  FNF3eR.LerXml;
  Result := True;
end;

function NotaFiscal.LerArqIni(const AIniString: String): Boolean;
var
  INIRec: TMemIniFile;
  sSecao, sFim, sProdID: String;
  OK: boolean;
  i: Integer;
//  SL     : TStringList;
//  J, K : Integer;
//  , sDINumber, sADINumber, sQtdVol,
//  sDupNumber, sAdittionalField, sType, sDay, sDeduc, sNVE, sCNPJCPF : String;
begin
  Result := False;

  INIRec := TMemIniFile.Create('');
  try
    LerIniArquivoOuString(AIniString, INIRec);

    with FNF3e do
    begin
      infNF3e.versao := StringToFloatDef( INIRec.ReadString('infNF3e','versao', VersaoNF3eToStr(FConfiguracoes.Geral.VersaoDF)), 0) ;

      sSecao      := IfThen( INIRec.SectionExists('Identificacao'), 'Identificacao', 'ide');
      Ide.cNF     := INIRec.ReadInteger( sSecao,'Codigo' ,INIRec.ReadInteger( sSecao,'cNF' ,0));
      Ide.modelo  := INIRec.ReadInteger( sSecao,'Modelo' ,INIRec.ReadInteger( sSecao,'mod' ,65));
      Ide.serie   := INIRec.ReadInteger( sSecao,'Serie'  ,1);
      Ide.nNF     := INIRec.ReadInteger( sSecao,'Numero' ,INIRec.ReadInteger( sSecao,'nNF' ,0));
      Ide.dhEmi   := StringToDateTime(INIRec.ReadString( sSecao,'Emissao',INIRec.ReadString( sSecao,'dhEmi',INIRec.ReadString( sSecao,'dhEmi','0'))));
      Ide.tpEmis  := StrToTipoEmissao( OK,INIRec.ReadString( sSecao,'tpEmis',IntToStr(FConfiguracoes.Geral.FormaEmissaoCodigo)));
      Ide.tpAmb   := StrToTipoAmbiente(OK, INIRec.ReadString(sSecao, 'tpAmb', IntToStr(Integer(FConfiguracoes.WebServices.Ambiente))));
      Ide.finNF3e := StrToFinNF3e( OK,INIRec.ReadString( sSecao,'Finalidade',INIRec.ReadString( sSecao,'finNF3e','0')));
      Ide.verProc := INIRec.ReadString(  sSecao, 'verProc' ,'ACBrNF3e');
      Ide.dhCont  := StringToDateTime(INIRec.ReadString( sSecao,'dhCont'  ,'0'));
      Ide.xJust   := INIRec.ReadString(  sSecao,'xJust' ,'' );

      {
      sSecao := IfThen( INIRec.SectionExists('Emitente'), 'Emitente', 'emit');
      Emit.CNPJCPF := INIRec.ReadString( sSecao,'CNPJ'    ,INIRec.ReadString( sSecao,'CNPJCPF', ''));
      Emit.xNome   := INIRec.ReadString( sSecao,'Razao'   ,INIRec.ReadString( sSecao,'xNome'  , ''));
      Emit.xFant   := INIRec.ReadString( sSecao,'Fantasia',INIRec.ReadString( sSecao,'xFant'  , ''));
      Emit.IE      := INIRec.ReadString( sSecao,'IE'  ,'');
      Emit.IEST    := INIRec.ReadString( sSecao,'IEST','');
      Emit.IM      := INIRec.ReadString( sSecao,'IM'  ,'');
      Emit.CNAE    := INIRec.ReadString( sSecao,'CNAE','');
      Emit.CRT     := StrToCRT(ok, INIRec.ReadString( sSecao,'CRT','3'));

      Emit.EnderEmit.xLgr := INIRec.ReadString( sSecao, 'Logradouro' ,INIRec.ReadString(  sSecao, 'xLgr', ''));
      if (INIRec.ReadString( sSecao,'Numero', '') <> '') or (INIRec.ReadString( sSecao, 'nro', '') <> '') then
        Emit.EnderEmit.nro := INIRec.ReadString( sSecao,'Numero', INIRec.ReadString( sSecao, 'nro', ''));

      if (INIRec.ReadString( sSecao, 'Complemento', '') <> '') or (INIRec.ReadString( sSecao, 'xCpl', '') <> '') then
        Emit.EnderEmit.xCpl := INIRec.ReadString( sSecao, 'Complemento', INIRec.ReadString( sSecao, 'xCpl', ''));

      Emit.EnderEmit.xBairro := INIRec.ReadString(  sSecao,'Bairro'     ,INIRec.ReadString(  sSecao,'xBairro',''));
      Emit.EnderEmit.cMun    := INIRec.ReadInteger( sSecao,'CidadeCod'  ,INIRec.ReadInteger( sSecao,'cMun'   ,0));
      Emit.EnderEmit.xMun    := INIRec.ReadString(  sSecao,'Cidade'     ,INIRec.ReadString(  sSecao,'xMun'   ,''));
      Emit.EnderEmit.UF      := INIRec.ReadString(  sSecao,'UF'         ,'');
      Emit.EnderEmit.CEP     := INIRec.ReadInteger( sSecao,'CEP'        ,0);
      Emit.EnderEmit.cPais   := INIRec.ReadInteger( sSecao,'PaisCod'    ,INIRec.ReadInteger( sSecao,'cPais'    ,1058));
      Emit.EnderEmit.xPais   := INIRec.ReadString(  sSecao,'Pais'       ,INIRec.ReadString(  sSecao,'xPais'    ,'BRASIL'));
      Emit.EnderEmit.fone    := INIRec.ReadString(  sSecao,'Fone'       ,'');

      Ide.cUF    := INIRec.ReadInteger( sSecao,'cUF'       ,UFparaCodigo(Emit.EnderEmit.UF));
      Ide.cMunFG := INIRec.ReadInteger( sSecao,'CidadeCod' ,INIRec.ReadInteger( sSecao,'cMunFG' ,Emit.EnderEmit.cMun));

      if INIRec.ReadString( 'Avulsa', 'CNPJ', '') <> '' then
      begin
        Avulsa.CNPJ    := INIRec.ReadString( 'Avulsa', 'CNPJ', '');
        Avulsa.xOrgao  := INIRec.ReadString( 'Avulsa', 'xOrgao', '');
        Avulsa.matr    := INIRec.ReadString( 'Avulsa', 'matr', '');
        Avulsa.xAgente := INIRec.ReadString( 'Avulsa', 'xAgente', '');
        Avulsa.fone    := INIRec.ReadString( 'Avulsa', 'fone', '');
        Avulsa.UF      := INIRec.ReadString( 'Avulsa', 'UF', '');
        Avulsa.nDAR    := INIRec.ReadString( 'Avulsa', 'nDAR', '');
        Avulsa.dEmi    := StringToDateTime(INIRec.ReadString( 'Avulsa', 'dEmi', '0'));
        Avulsa.vDAR    := StringToFloatDef(INIRec.ReadString( 'Avulsa', 'vDAR', ''), 0);
        Avulsa.repEmi  := INIRec.ReadString( 'Avulsa', 'repEmi','');
        Avulsa.dPag    := StringToDateTime(INIRec.ReadString( 'Avulsa', 'dPag', '0'));
      end;

      sSecao := IfThen( INIRec.SectionExists('Destinatario'),'Destinatario','dest');
      Dest.idEstrangeiro     := INIRec.ReadString(  sSecao,'idEstrangeiro','');
      Dest.CNPJCPF           := INIRec.ReadString(  sSecao,'CNPJ'       ,INIRec.ReadString(  sSecao,'CNPJCPF',INIRec.ReadString(  sSecao,'CPF','')));
      Dest.xNome             := INIRec.ReadString(  sSecao,'NomeRazao'  ,INIRec.ReadString(  sSecao,'xNome'  ,''));
      Dest.indIEDest         := StrToindIEDest(OK,INIRec.ReadString( sSecao,'indIEDest','1'));
      Dest.IE                := INIRec.ReadString(  sSecao,'IE'         ,'');
      Dest.ISUF              := INIRec.ReadString(  sSecao,'ISUF'       ,'');
      Dest.Email             := INIRec.ReadString(  sSecao,'Email'      ,'');

      Dest.EnderDest.xLgr := INIRec.ReadString(  sSecao, 'Logradouro' ,INIRec.ReadString( sSecao, 'xLgr', ''));
      if (INIRec.ReadString(sSecao, 'Numero', '') <> '') or (INIRec.ReadString(sSecao, 'nro', '') <> '') then
        Dest.EnderDest.nro := INIRec.ReadString(  sSecao, 'Numero', INIRec.ReadString(sSecao, 'nro', ''));

      if (INIRec.ReadString(sSecao, 'Complemento', '') <> '') or (INIRec.ReadString(sSecao, 'xCpl', '') <> '') then
        Dest.EnderDest.xCpl := INIRec.ReadString( sSecao, 'Complemento', INIRec.ReadString(sSecao,'xCpl',''));

      Dest.EnderDest.xBairro := INIRec.ReadString(  sSecao,'Bairro'     ,INIRec.ReadString(  sSecao,'xBairro',''));
      Dest.EnderDest.cMun    := INIRec.ReadInteger( sSecao,'CidadeCod'  ,INIRec.ReadInteger( sSecao,'cMun'   ,0));
      Dest.EnderDest.xMun    := INIRec.ReadString(  sSecao,'Cidade'     ,INIRec.ReadString(  sSecao,'xMun'   ,''));
      Dest.EnderDest.UF      := INIRec.ReadString(  sSecao,'UF'         ,'');
      Dest.EnderDest.CEP     := INIRec.ReadInteger( sSecao,'CEP'       ,0);
      Dest.EnderDest.cPais   := INIRec.ReadInteger( sSecao,'PaisCod'    ,INIRec.ReadInteger(sSecao,'cPais',1058));
      Dest.EnderDest.xPais   := INIRec.ReadString(  sSecao,'Pais'       ,INIRec.ReadString( sSecao,'xPais','BRASIL'));
      Dest.EnderDest.Fone    := INIRec.ReadString(  sSecao,'Fone'       ,'');

      sCNPJCPF := INIRec.ReadString( 'Retirada','CNPJ',INIRec.ReadString( 'Retirada','CPF',INIRec.ReadString( 'Retirada','CNPJCPF','')));
      if sCNPJCPF <> '' then
      begin
        Retirada.CNPJCPF := sCNPJCPF;
        Retirada.xNome   := INIRec.ReadString( 'Retirada','xNome','');
        Retirada.xLgr    := INIRec.ReadString( 'Retirada','xLgr','');
        Retirada.nro     := INIRec.ReadString( 'Retirada','nro' ,'');
        Retirada.xCpl    := INIRec.ReadString( 'Retirada','xCpl','');
        Retirada.xBairro := INIRec.ReadString( 'Retirada','xBairro','');
        Retirada.cMun    := INIRec.ReadInteger('Retirada','cMun',0);
        Retirada.xMun    := INIRec.ReadString( 'Retirada','xMun','');
        Retirada.UF      := INIRec.ReadString( 'Retirada','UF'  ,'');
        Retirada.CEP     := INIRec.ReadInteger('Retirada','CEP',0);
        Retirada.cPais   := INIRec.ReadInteger('Retirada','PaisCod',INIRec.ReadInteger('Retirada','cPais',1058));
        Retirada.xPais   := INIRec.ReadString( 'Retirada','Pais',INIRec.ReadString( 'Retirada','xPais','BRASIL'));
        Retirada.Fone    := INIRec.ReadString( 'Retirada','Fone','');
        Retirada.Email   := INIRec.ReadString( 'Retirada','Email','');
        Retirada.IE      := INIRec.ReadString( 'Retirada','IE'  ,'');
      end;

      sCNPJCPF := INIRec.ReadString(  'Entrega','CNPJ',INIRec.ReadString(  'Entrega','CPF',INIRec.ReadString(  'Entrega','CNPJCPF','')));
      if sCNPJCPF <> '' then
      begin
        Entrega.CNPJCPF := sCNPJCPF;
        Entrega.xNome   := INIRec.ReadString( 'Entrega','xNome','');
        Entrega.xLgr    := INIRec.ReadString(  'Entrega','xLgr','');
        Entrega.nro     := INIRec.ReadString(  'Entrega','nro' ,'');
        Entrega.xCpl    := INIRec.ReadString(  'Entrega','xCpl','');
        Entrega.xBairro := INIRec.ReadString(  'Entrega','xBairro','');
        Entrega.cMun    := INIRec.ReadInteger( 'Entrega','cMun',0);
        Entrega.xMun    := INIRec.ReadString(  'Entrega','xMun','');
        Entrega.UF      := INIRec.ReadString(  'Entrega','UF','');
        Entrega.CEP     := INIRec.ReadInteger('Entrega','CEP',0);
        Entrega.cPais   := INIRec.ReadInteger('Entrega','PaisCod',INIRec.ReadInteger('Entrega','cPais',1058));
        Entrega.xPais   := INIRec.ReadString( 'Entrega','Pais',INIRec.ReadString( 'Entrega','xPais','BRASIL'));
        Entrega.Fone    := INIRec.ReadString( 'Entrega','Fone','');
        Entrega.Email   := INIRec.ReadString( 'Entrega','Email','');
        Entrega.IE      := INIRec.ReadString( 'Entrega','IE'  ,'');
      end;
      }
      I := 1 ;
      while true do
      begin
        sSecao := 'autXML'+IntToStrZero(I,3) ;
        sFim     := OnlyNumber(INIRec.ReadString( sSecao ,'CNPJ',INIRec.ReadString(  sSecao,'CPF',INIRec.ReadString(  sSecao,'CNPJCPF','FIM'))));
        if (sFim = 'FIM') or (Length(sFim) <= 0) then
          break ;

//        with autXML.New do
//          CNPJCPF := sFim;

        Inc(I);
      end;

      I := 1 ;
      while true do
      begin
        sSecao := IfThen( INIRec.SectionExists('Produto'+IntToStrZero(I,3)), 'Produto', 'det');
        sSecao := sSecao+IntToStrZero(I,3) ;
        sProdID  := INIRec.ReadString(sSecao,'Codigo',INIRec.ReadString( sSecao,'cProd','FIM')) ;
        if sProdID = 'FIM' then
          break ;
        {
        with Det.New do
        begin
          Prod.nItem := I;
          infAdProd  := INIRec.ReadString(sSecao,'infAdProd','');

          Prod.cProd := INIRec.ReadString( sSecao,'Codigo'   ,INIRec.ReadString( sSecao,'cProd'   ,''));
          if (Length(INIRec.ReadString( sSecao,'EAN','')) > 0) or (Length(INIRec.ReadString( sSecao,'cEAN','')) > 0)  then
            Prod.cEAN := INIRec.ReadString( sSecao,'EAN'      ,INIRec.ReadString( sSecao,'cEAN'      ,''));

          Prod.xProd    := INIRec.ReadString( sSecao,'Descricao',INIRec.ReadString( sSecao,'xProd',''));
          Prod.NCM      := INIRec.ReadString( sSecao,'NCM'      ,'');
          Prod.CEST     := INIRec.ReadString( sSecao,'CEST'     ,'');
          Prod.indEscala:= StrToIndEscala(OK, INIRec.ReadString( sSecao,'indEscala' ,'') );
          Prod.CNPJFab  := INIRec.ReadString( sSecao,'CNPJFab'   ,'');
          Prod.cBenef   := INIRec.ReadString( sSecao,'cBenef'    ,'');
          Prod.EXTIPI   := INIRec.ReadString( sSecao,'EXTIPI'      ,'');
          Prod.CFOP     := INIRec.ReadString( sSecao,'CFOP'     ,'');
          Prod.uCom     := INIRec.ReadString( sSecao,'Unidade'  ,INIRec.ReadString( sSecao,'uCom'  ,''));
          Prod.qCom     := StringToFloatDef( INIRec.ReadString(sSecao,'Quantidade'   ,INIRec.ReadString(sSecao,'qCom'  ,'')) ,0) ;
          Prod.vUnCom   := StringToFloatDef( INIRec.ReadString(sSecao,'ValorUnitario',INIRec.ReadString(sSecao,'vUnCom','')) ,0) ;
          Prod.vProd    := StringToFloatDef( INIRec.ReadString(sSecao,'ValorTotal'   ,INIRec.ReadString(sSecao,'vProd' ,'')) ,0) ;

          if Length(INIRec.ReadString( sSecao,'cEANTrib','')) > 0 then
            Prod.cEANTrib      := INIRec.ReadString( sSecao,'cEANTrib'      ,'');

          Prod.uTrib     := INIRec.ReadString( sSecao,'uTrib'  , Prod.uCom);
          Prod.qTrib     := StringToFloatDef( INIRec.ReadString(sSecao,'qTrib'  ,''), Prod.qCom);
          Prod.vUnTrib   := StringToFloatDef( INIRec.ReadString(sSecao,'vUnTrib','') ,Prod.vUnCom) ;
          Prod.vFrete    := StringToFloatDef( INIRec.ReadString(sSecao,'vFrete','') ,0) ;
          Prod.vSeg      := StringToFloatDef( INIRec.ReadString(sSecao,'vSeg','') ,0) ;
          Prod.vDesc     := StringToFloatDef( INIRec.ReadString(sSecao,'ValorDesconto',INIRec.ReadString(sSecao,'vDesc','')) ,0) ;
          Prod.vOutro    := StringToFloatDef( INIRec.ReadString(sSecao,'vOutro','') ,0) ;
          Prod.IndTot    := StrToindTot(OK,INIRec.ReadString(sSecao,'indTot','1'));
          Prod.xPed      := INIRec.ReadString( sSecao,'xPed'    ,'');
          Prod.nItemPed  := INIRec.ReadString( sSecao,'nItemPed','');
          Prod.nFCI      := INIRec.ReadString( sSecao,'nFCI','');  //NF3e3
          Prod.nRECOPI   := INIRec.ReadString( sSecao,'nRECOPI','');  //NF3e3

          pDevol    := StringToFloatDef( INIRec.ReadString(sSecao,'pDevol','') ,0);
          vIPIDevol := StringToFloatDef( INIRec.ReadString(sSecao,'vIPIDevol','') ,0);

          Imposto.vTotTrib := StringToFloatDef( INIRec.ReadString(sSecao,'vTotTrib','') ,0) ;

          J := 1 ;
          while true do
          begin
            sSecao := 'NVE'+IntToStrZero(I,3)+IntToStrZero(J,3) ;
            sNVE     := INIRec.ReadString(sSecao,'NVE','') ;
            if (sNVE <> '') then
              Prod.NVE.New.NVE := sNVE
            else
              Break;

            Inc(J);
          end;

          J := 1 ;
          while true do
          begin
            sSecao  := 'rastro'+IntToStrZero(I,3)+IntToStrZero(J,3) ;
            sFim    := INIRec.ReadString(sSecao,'nLote','') ;
            if (sFim <> '') then
               with Prod.rastro.New do
               begin
                 nLote    := sFim;
                 qLote    := StringToFloatDef( INIRec.ReadString( sSecao,'qLote',''), 0 );
                 dFab     := StringToDateTime( INIRec.ReadString( sSecao,'dFab','0') );
                 dVal     := StringToDateTime( INIRec.ReadString( sSecao,'dVal','0') );
                 cAgreg   := INIRec.ReadString( sSecao,'cAgreg','');
               end
            else
               Break;
            Inc(J);
          end;

          J := 1 ;
          while true do
          begin
            sSecao  := 'DI'+IntToStrZero(I,3)+IntToStrZero(J,3) ;
            sDINumber := INIRec.ReadString(sSecao,'NumeroDI',INIRec.ReadString(sSecao,'nDi','')) ;

            if sDINumber <> '' then
            begin
              with Prod.DI.New do
              begin
                nDi         := sDINumber;
                dDi         := StringToDateTime(INIRec.ReadString(sSecao,'DataRegistroDI'  ,INIRec.ReadString(sSecao,'dDi'  ,'0')));
                xLocDesemb  := INIRec.ReadString(sSecao,'LocalDesembaraco',INIRec.ReadString(sSecao,'xLocDesemb',''));
                UFDesemb    := INIRec.ReadString(sSecao,'UFDesembaraco'   ,INIRec.ReadString(sSecao,'UFDesemb'   ,''));
                dDesemb     := StringToDateTime(INIRec.ReadString(sSecao,'DataDesembaraco',INIRec.ReadString(sSecao,'dDesemb','0')));

                tpViaTransp  := StrToTipoViaTransp(OK,INIRec.ReadString(sSecao,'tpViaTransp',''));
                vAFRMM       := StringToFloatDef( INIRec.ReadString(sSecao,'vAFRMM','') ,0) ;
                tpIntermedio := StrToTipoIntermedio(OK,INIRec.ReadString(sSecao,'tpIntermedio',''));
                CNPJ         := INIRec.ReadString(sSecao,'CNPJ','');
                UFTerceiro   := INIRec.ReadString(sSecao,'UFTerceiro','');

                cExportador := INIRec.ReadString(sSecao,'CodigoExportador',INIRec.ReadString(sSecao,'cExportador',''));

                K := 1 ;
                while true do
                begin
                  sSecao   := IfThen( INIRec.SectionExists('LADI'+IntToStrZero(I,3)+IntToStrZero(J,3)+IntToStrZero(K,3)), 'LADI', 'adi');
                  sSecao   := sSecao+IntToStrZero(I,3)+IntToStrZero(J,3)+IntToStrZero(K,3)  ;
                  sADINumber := INIRec.ReadString(sSecao,'NumeroAdicao',INIRec.ReadString(sSecao,'nAdicao','FIM')) ;
                  if (sADINumber = 'FIM') or (Length(sADINumber) <= 0) then
                    break;

                  with adi.New do
                  begin
                    nAdicao     := StrToInt(sADINumber);
                    nSeqAdi     := INIRec.ReadInteger( sSecao,'nSeqAdi',K);
                    cFabricante := INIRec.ReadString(  sSecao,'CodigoFabricante',INIRec.ReadString(  sSecao,'cFabricante',''));
                    vDescDI     := StringToFloatDef( INIRec.ReadString(sSecao,'DescontoADI',INIRec.ReadString(sSecao,'vDescDI','')) ,0);
                    nDraw       := INIRec.ReadString( sSecao,'nDraw','');
                  end;

                  Inc(K)
                end;
              end;
            end
            else
              Break;

            Inc(J);
          end;

          J := 1 ;
          while true do
          begin
            sSecao := 'detExport'+IntToStrZero(I,3)+IntToStrZero(J,3) ;
            sFim     := INIRec.ReadString(sSecao,'nRE','FIM');
            if (sFim = 'FIM') or (Length(sFim) <= 0) then
            begin
              sFim     := INIRec.ReadString(sSecao,'nDraw','FIM');
              if (sFim = 'FIM') or (Length(sFim) <= 0) then
                break ;
            end;

            with Prod.detExport.New do
            begin
              nDraw   := INIRec.ReadString( sSecao,'nDraw','');
              nRE     := INIRec.ReadString( sSecao,'nRE','');
              chNF3e   := INIRec.ReadString( sSecao,'chNF3e','');
              qExport := StringToFloatDef( INIRec.ReadString(sSecao,'qExport','') ,0);
            end;

            Inc(J);
          end;

          sSecao := 'impostoDevol'+IntToStrZero(I,3) ;
          sFim   := INIRec.ReadString( sSecao,'pDevol','FIM') ;
          if ((sFim <> 'FIM') and ( Length(sFim) > 0 ))  then
          begin
            pDevol    := StringToFloatDef( INIRec.ReadString(sSecao,'pDevol','') ,0);
            vIPIDevol := StringToFloatDef( INIRec.ReadString(sSecao,'vIPIDevol','') ,0);
          end;

          sSecao := IfThen( INIRec.SectionExists('Veiculo'+IntToStrZero(I,3)), 'Veiculo', 'veicProd');
          sSecao := sSecao+IntToStrZero(I,3) ;
          sFim     := INIRec.ReadString( sSecao,'Chassi','FIM') ;
          if ((sFim <> 'FIM') and ( Length(sFim) > 0 )) then
          begin
            with Prod.veicProd do
            begin
              tpOP    := StrTotpOP(OK,INIRec.ReadString( sSecao,'tpOP','0'));
              chassi  := sFim;
              cCor    := INIRec.ReadString( sSecao,'cCor'   ,'');
              xCor    := INIRec.ReadString( sSecao,'xCor'   ,'');
              pot     := INIRec.ReadString( sSecao,'pot'    ,'');
              Cilin   := INIRec.ReadString( sSecao,'CM3'    ,INIRec.ReadString( sSecao,'Cilin'  ,''));
              pesoL   := INIRec.ReadString( sSecao,'pesoL'  ,'');
              pesoB   := INIRec.ReadString( sSecao,'pesoB'  ,'');
              nSerie  := INIRec.ReadString( sSecao,'nSerie' ,'');
              tpComb  := INIRec.ReadString( sSecao,'tpComb' ,'');
              nMotor  := INIRec.ReadString( sSecao,'nMotor' ,'');
              CMT     := INIRec.ReadString( sSecao,'CMKG'   ,INIRec.ReadString( sSecao,'CMT'    ,''));
              dist    := INIRec.ReadString( sSecao,'dist'   ,'');
//             RENAVAM := INIRec.ReadString( sSecao,'RENAVAM','');
              anoMod  := INIRec.ReadInteger(sSecao,'anoMod' ,0);
              anoFab  := INIRec.ReadInteger(sSecao,'anoFab' ,0);
              tpPint  := INIRec.ReadString( sSecao,'tpPint' ,'');
              tpVeic  := INIRec.ReadInteger(sSecao,'tpVeic' ,0);
              espVeic := INIRec.ReadInteger(sSecao,'espVeic',0);
              VIN     := INIRec.ReadString( sSecao,'VIN'    ,'');
              condVeic := StrTocondVeic(OK,INIRec.ReadString( sSecao,'condVeic','1'));
              cMod    := INIRec.ReadString( sSecao,'cMod'   ,'');
              cCorDENATRAN := INIRec.ReadString( sSecao,'cCorDENATRAN','');
              lota    := INIRec.ReadInteger(sSecao,'lota'   ,0);
              tpRest  := INIRec.ReadInteger(sSecao,'tpRest' ,0);
            end;
          end;

          J := 1 ;
          while true do
          begin
            sSecao := IfThen( INIRec.SectionExists('Medicamento'+IntToStrZero(I,3)+IntToStrZero(J,3)), 'Medicamento', 'med');
            sSecao := sSecao+IntToStrZero(I,3)+IntToStrZero(J,3) ;
            sFim     := INIRec.ReadString(sSecao,'cProdANVISA','FIM') ;
            if (sFim = 'FIM') or (Length(sFim) <= 0) then
              break;

            with Prod.med.New do
            begin
              nLote := INIRec.ReadString(sSecao,'nLote','') ;
              cProdANVISA:=  sFim;
              xMotivoIsencao := INIRec.ReadString(sSecao,'xMotivoIsencao','') ;
              qLote := StringToFloatDef(INIRec.ReadString( sSecao,'qLote',''),0) ;
              dFab  := StringToDateTime(INIRec.ReadString( sSecao,'dFab','0')) ;
              dVal  := StringToDateTime(INIRec.ReadString( sSecao,'dVal','0')) ;
              vPMC  := StringToFloatDef(INIRec.ReadString( sSecao,'vPMC',''),0) ;
            end;

            Inc(J)
          end;

          J := 1 ;
          while true do
          begin
            sSecao := 'Arma'+IntToStrZero(I,3)+IntToStrZero(J,3) ;
            sFim     := INIRec.ReadString(sSecao,'nSerie','FIM') ;
            if (sFim = 'FIM') or (Length(sFim) <= 0) then
              break;

            with Prod.arma.New do
            begin
              tpArma := StrTotpArma(OK,INIRec.ReadString( sSecao,'tpArma','0')) ;
              nSerie := sFim;
              nCano  := INIRec.ReadString( sSecao,'nCano','') ;
              descr  := INIRec.ReadString( sSecao,'descr','') ;
            end;

            Inc(J)
          end;

          sSecao := IfThen( INIRec.SectionExists('Combustivel'+IntToStrZero(I,3)), 'Combustivel', 'comb');
          sSecao := sSecao+IntToStrZero(I,3) ;
          sFim     := INIRec.ReadString( sSecao,'cProdANP','FIM') ;
          if ((sFim <> 'FIM') and ( Length(sFim) > 0 )) then
          begin
            with Prod.comb do
            begin
              cProdANP := INIRec.ReadInteger( sSecao,'cProdANP',0) ;
              pMixGN   := StringToFloatDef(INIRec.ReadString( sSecao,'pMixGN',''),0) ;
              descANP  := INIRec.ReadString(  sSecao,'descANP'   ,'');
              pGLP     := StringToFloatDef( INIRec.ReadString( sSecao,'pGLP'   ,''), 0);
              pGNn     := StringToFloatDef( INIRec.ReadString( sSecao,'pGNn'   ,''), 0);
              pGNi     := StringToFloatDef( INIRec.ReadString( sSecao,'pGNi'   ,''), 0);
              vPart    := StringToFloatDef( INIRec.ReadString( sSecao,'vPart'  ,''), 0);
              CODIF    := INIRec.ReadString(  sSecao,'CODIF'   ,'') ;
              qTemp    := StringToFloatDef(INIRec.ReadString( sSecao,'qTemp',''),0) ;
              UFcons   := INIRec.ReadString( sSecao,'UFCons','') ;

              sSecao := 'CIDE'+IntToStrZero(I,3) ;
              CIDE.qBCprod   := StringToFloatDef(INIRec.ReadString( sSecao,'qBCprod'  ,''),0) ;
              CIDE.vAliqProd := StringToFloatDef(INIRec.ReadString( sSecao,'vAliqProd',''),0) ;
              CIDE.vCIDE     := StringToFloatDef(INIRec.ReadString( sSecao,'vCIDE'    ,''),0) ;

              sSecao := 'encerrante'+IntToStrZero(I,3) ;
              encerrante.nBico    := INIRec.ReadInteger( sSecao,'nBico'  ,0) ;
              encerrante.nBomba   := INIRec.ReadInteger( sSecao,'nBomba' ,0) ;
              encerrante.nTanque  := INIRec.ReadInteger( sSecao,'nTanque',0) ;
              encerrante.vEncIni  := StringToFloatDef(INIRec.ReadString( sSecao,'vEncIni',''),0) ;
              encerrante.vEncFin  := StringToFloatDef(INIRec.ReadString( sSecao,'vEncFin',''),0) ;

              sSecao := 'ICMSComb'+IntToStrZero(I,3) ;
              ICMS.vBCICMS   := StringToFloatDef(INIRec.ReadString( sSecao,'vBCICMS'  ,''),0) ;
              ICMS.vICMS     := StringToFloatDef(INIRec.ReadString( sSecao,'vICMS'    ,''),0) ;
              ICMS.vBCICMSST := StringToFloatDef(INIRec.ReadString( sSecao,'vBCICMSST',''),0) ;
              ICMS.vICMSST   := StringToFloatDef(INIRec.ReadString( sSecao,'vICMSST'  ,''),0) ;

              sSecao := 'ICMSInter'+IntToStrZero(I,3) ;
              sFim     := INIRec.ReadString( sSecao,'vBCICMSSTDest','FIM') ;
              if ((sFim <> 'FIM') and ( Length(sFim) > 0 )) then
              begin
                ICMSInter.vBCICMSSTDest := StringToFloatDef(sFim,0) ;
                ICMSInter.vICMSSTDest   := StringToFloatDef(INIRec.ReadString( sSecao,'vICMSSTDest',''),0) ;
              end;

              sSecao := 'ICMSCons'+IntToStrZero(I,3) ;
              sFim   := INIRec.ReadString( sSecao,'vBCICMSSTCons','FIM') ;
              if ((sFim <> 'FIM') and ( Length(sFim) > 0 )) then
              begin
                ICMSCons.vBCICMSSTCons := StringToFloatDef(sFim,0) ;
                ICMSCons.vICMSSTCons   := StringToFloatDef(INIRec.ReadString( sSecao,'vICMSSTCons',''),0) ;
                ICMSCons.UFcons        := INIRec.ReadString( sSecao,'UFCons','') ;
              end;
            end;
          end;

          with Imposto do
          begin
            sSecao := 'ICMS'+IntToStrZero(I,3) ;
            //sFim     := INIRec.ReadString( sSecao,'CST',INIRec.ReadString(sSecao,'CSOSN','FIM')) ;

            sFim     := INIRec.ReadString( sSecao,'CST','FIM') ;
            if (sFim = 'FIM') or ( Length(sFim) = 0 ) then
              sFim     := INIRec.ReadString(sSecao,'CSOSN','FIM');

            if ((sFim <> 'FIM') and ( Length(sFim) > 0 )) then
            begin
              with ICMS do
              begin
                ICMS.orig       := StrToOrig(     OK, INIRec.ReadString(sSecao,'Origem'    ,INIRec.ReadString(sSecao,'orig'    ,'0' ) ));
                CST             := StrToCSTICMS(  OK, INIRec.ReadString(sSecao,'CST'       ,'00'));
                CSOSN           := StrToCSOSNIcms(OK, INIRec.ReadString(sSecao,'CSOSN'     ,''  ));
                ICMS.modBC      := StrTomodBC(    OK, INIRec.ReadString(sSecao,'Modalidade',INIRec.ReadString(sSecao,'modBC','0' ) ));
                ICMS.pRedBC     := StringToFloatDef( INIRec.ReadString(sSecao,'PercentualReducao',INIRec.ReadString(sSecao,'pRedBC','')) ,0);
                ICMS.vBC        := StringToFloatDef( INIRec.ReadString(sSecao,'ValorBase',INIRec.ReadString(sSecao,'vBC'  ,'')) ,0);
                ICMS.pICMS      := StringToFloatDef( INIRec.ReadString(sSecao,'Aliquota' ,INIRec.ReadString(sSecao,'pICMS','')) ,0);
                ICMS.vICMS      := StringToFloatDef( INIRec.ReadString(sSecao,'Valor'    ,INIRec.ReadString(sSecao,'vICMS','')) ,0);
                ICMS.vBCFCP     := StringToFloatDef( INIRec.ReadString( sSecao,'ValorBaseFCP', INIRec.ReadString(sSecao,'vBCFCP','')) ,0) ;
                ICMS.pFCP       := StringToFloatDef( INIRec.ReadString( sSecao,'PercentualFCP', INIRec.ReadString(sSecao,'pFCP','')) ,0) ;
                ICMS.vFCP       := StringToFloatDef( INIRec.ReadString( sSecao,'ValorFCP', INIRec.ReadString(sSecao,'vFCP','')) ,0) ;
                ICMS.modBCST    := StrTomodBCST(OK, INIRec.ReadString(sSecao,'ModalidadeST',INIRec.ReadString(sSecao,'modBCST','0')));
                ICMS.pMVAST     := StringToFloatDef( INIRec.ReadString(sSecao,'PercentualMargemST' ,INIRec.ReadString(sSecao,'pMVAST' ,'')) ,0);
                ICMS.pRedBCST   := StringToFloatDef( INIRec.ReadString(sSecao,'PercentualReducaoST',INIRec.ReadString(sSecao,'pRedBCST','')) ,0);
                ICMS.vBCST      := StringToFloatDef( INIRec.ReadString(sSecao,'ValorBaseST',INIRec.ReadString(sSecao,'vBCST','')) ,0);
                ICMS.pICMSST    := StringToFloatDef( INIRec.ReadString(sSecao,'AliquotaST' ,INIRec.ReadString(sSecao,'pICMSST' ,'')) ,0);
                ICMS.vICMSST    := StringToFloatDef( INIRec.ReadString(sSecao,'ValorST'    ,INIRec.ReadString(sSecao,'vICMSST'    ,'')) ,0);
                ICMS.vBCFCPST   := StringToFloatDef( INIRec.ReadString( sSecao,'ValorBaseFCPST', INIRec.ReadString(sSecao,'vBCFCPST','')) ,0) ;
                ICMS.pFCPST     := StringToFloatDef( INIRec.ReadString( sSecao,'PercentualFCPST', INIRec.ReadString(sSecao,'pFCPST','')) ,0) ;
                ICMS.vFCPST     := StringToFloatDef( INIRec.ReadString( sSecao,'ValorFCPST', INIRec.ReadString(sSecao,'vFCPST','')) ,0) ;
                ICMS.UFST       := INIRec.ReadString(sSecao,'UFST'    ,'');
                ICMS.pBCOp      := StringToFloatDef( INIRec.ReadString(sSecao,'pBCOp'    ,'') ,0);
                ICMS.vBCSTRet   := StringToFloatDef( INIRec.ReadString(sSecao,'vBCSTRet','') ,0);
                ICMS.pST        := StringToFloatDef( INIRec.ReadString(sSecao,'pST','') ,0);
                ICMS.vICMSSTRet := StringToFloatDef( INIRec.ReadString(sSecao,'vICMSSTRet','') ,0);
                ICMS.vBCFCPSTRet:= StringToFloatDef( INIRec.ReadString( sSecao,'ValorBaseFCPSTRes', INIRec.ReadString(sSecao,'vBCFCPSTRet','')) ,0) ;
                ICMS.pFCPSTRet  := StringToFloatDef( INIRec.ReadString( sSecao,'PercentualFCPSTRet', INIRec.ReadString(sSecao,'pFCPSTRet','')) ,0) ;
                ICMS.vFCPSTRet  := StringToFloatDef( INIRec.ReadString( sSecao,'ValorFCPSTRet', INIRec.ReadString(sSecao,'vFCPSTRet','')) ,0) ;
                ICMS.motDesICMS := StrTomotDesICMS(OK, INIRec.ReadString(sSecao,'motDesICMS','0'));
                ICMS.pCredSN    := StringToFloatDef( INIRec.ReadString(sSecao,'pCredSN','') ,0);
                ICMS.vCredICMSSN:= StringToFloatDef( INIRec.ReadString(sSecao,'vCredICMSSN','') ,0);
                ICMS.vBCSTDest  := StringToFloatDef( INIRec.ReadString(sSecao,'vBCSTDest','') ,0);
                ICMS.vICMSSTDest:= StringToFloatDef( INIRec.ReadString(sSecao,'vICMSSTDest','') ,0);
                ICMS.vICMSDeson := StringToFloatDef( INIRec.ReadString(sSecao,'vICMSDeson','') ,0);
                ICMS.vICMSOp    := StringToFloatDef( INIRec.ReadString(sSecao,'vICMSOp','') ,0);
                ICMS.pDif       := StringToFloatDef( INIRec.ReadString(sSecao,'pDif','') ,0);
                ICMS.vICMSDif   := StringToFloatDef( INIRec.ReadString(sSecao,'vICMSDif','') ,0);

                ICMS.pRedBCEfet := StringToFloatDef( INIRec.ReadString(sSecao,'pRedBCEfet','') ,0);
                ICMS.vBCEfet    := StringToFloatDef( INIRec.ReadString(sSecao,'vBCEfet','') ,0);
                ICMS.pICMSEfet  := StringToFloatDef( INIRec.ReadString(sSecao,'pICMSEfet','') ,0);
                ICMS.vICMSEfet  := StringToFloatDef( INIRec.ReadString(sSecao,'vICMSEfet','') ,0);

                ICMS.vICMSSubstituto := StringToFloatDef( INIRec.ReadString(sSecao,'vICMSSubstituto','') ,0);
              end;
            end;

            sSecao := 'ICMSUFDest'+IntToStrZero(I,3);
            sFim     := INIRec.ReadString(sSecao,'vBCUFDest','FIM');
            if ((sFim <> 'FIM') and ( Length(sFim) > 0 )) then
            begin
              with ICMSUFDest do
              begin
                vBCUFDest      := StringToFloatDef(INIRec.ReadString(sSecao, 'vBCUFDest', ''), 0);
                vBCFCPUFDest   := StringToFloatDef( INIRec.ReadString(sSecao,'vBCFCPUFDest','') ,0);
                pFCPUFDest     := StringToFloatDef(INIRec.ReadString(sSecao, 'pFCPUFDest', ''), 0);
                pICMSUFDest    := StringToFloatDef(INIRec.ReadString(sSecao, 'pICMSUFDest', ''), 0);
                pICMSInter     := StringToFloatDef(INIRec.ReadString(sSecao, 'pICMSInter', ''), 0);
                pICMSInterPart := StringToFloatDef(INIRec.ReadString(sSecao, 'pICMSInterPart', ''), 0);
                vFCPUFDest     := StringToFloatDef(INIRec.ReadString(sSecao, 'vFCPUFDest', ''), 0);
                vICMSUFDest    := StringToFloatDef(INIRec.ReadString(sSecao, 'vICMSUFDest', ''), 0);
                vICMSUFRemet   := StringToFloatDef(INIRec.ReadString(sSecao, 'vICMSUFRemet', ''), 0);
              end;
            end;

            sSecao := 'IPI'+IntToStrZero(I,3) ;
            sFim   := INIRec.ReadString( sSecao,'CST','FIM') ;
            if ((sFim <> 'FIM') and ( Length(sFim) > 0 )) then
            begin
              with IPI do
              begin
                CST      := StrToCSTIPI(OK, INIRec.ReadString( sSecao,'CST','')) ;
                if OK then
                begin
                  clEnq    := INIRec.ReadString(  sSecao,'ClasseEnquadramento',INIRec.ReadString(  sSecao,'clEnq'   ,''));
                  CNPJProd := INIRec.ReadString(  sSecao,'CNPJProdutor'       ,INIRec.ReadString(  sSecao,'CNPJProd',''));
                  cSelo    := INIRec.ReadString(  sSecao,'CodigoSeloIPI'      ,INIRec.ReadString(  sSecao,'cSelo'   ,''));
                  qSelo    := INIRec.ReadInteger( sSecao,'QuantidadeSelos'    ,INIRec.ReadInteger( sSecao,'qSelo'   ,0));
                  cEnq     := INIRec.ReadString(  sSecao,'CodigoEnquadramento',INIRec.ReadString(  sSecao,'cEnq'    ,''));
                  vBC      := StringToFloatDef( INIRec.ReadString(sSecao,'ValorBase'   ,INIRec.ReadString(sSecao,'vBC'   ,'')) ,0);
                  qUnid    := StringToFloatDef( INIRec.ReadString(sSecao,'Quantidade'  ,INIRec.ReadString(sSecao,'qUnid' ,'')) ,0);
                  vUnid    := StringToFloatDef( INIRec.ReadString(sSecao,'ValorUnidade',INIRec.ReadString(sSecao,'vUnid' ,'')) ,0);
                  pIPI     := StringToFloatDef( INIRec.ReadString(sSecao,'Aliquota'    ,INIRec.ReadString(sSecao,'pIPI'  ,'')) ,0);
                  vIPI     := StringToFloatDef( INIRec.ReadString(sSecao,'Valor'       ,INIRec.ReadString(sSecao,'vIPI'  ,'')) ,0);
                end;
              end;
            end;

            sSecao := 'II'+IntToStrZero(I,3) ;
            sFim     := INIRec.ReadString( sSecao,'ValorBase',INIRec.ReadString( sSecao,'vBC','FIM')) ;
            if ((sFim <> 'FIM') and ( Length(sFim) > 0 )) then
            begin
              with II do
              begin
                vBc      := StringToFloatDef( INIRec.ReadString(sSecao,'ValorBase'          ,INIRec.ReadString(sSecao,'vBC'     ,'')) ,0);
                vDespAdu := StringToFloatDef( INIRec.ReadString(sSecao,'ValorDespAduaneiras',INIRec.ReadString(sSecao,'vDespAdu','')) ,0);
                vII      := StringToFloatDef( INIRec.ReadString(sSecao,'ValorII'            ,INIRec.ReadString(sSecao,'vII'     ,'')) ,0);
                vIOF     := StringToFloatDef( INIRec.ReadString(sSecao,'ValorIOF'           ,INIRec.ReadString(sSecao,'vIOF'    ,'')) ,0);
              end;
            end;

            sSecao := 'PIS'+IntToStrZero(I,3) ;
            sFim     := INIRec.ReadString( sSecao,'CST','FIM') ;
            if ((sFim <> 'FIM') and ( Length(sFim) > 0 )) then
            begin
              with PIS do
              begin
                CST :=  StrToCSTPIS(OK, INIRec.ReadString( sSecao,'CST',''));
                if OK then
                begin
                  PIS.vBC       := StringToFloatDef( INIRec.ReadString(sSecao,'ValorBase'    ,INIRec.ReadString(sSecao,'vBC'      ,'')) ,0);
                  PIS.pPIS      := StringToFloatDef( INIRec.ReadString(sSecao,'Aliquota'     ,INIRec.ReadString(sSecao,'pPIS'     ,'')) ,0);
                  PIS.qBCProd   := StringToFloatDef( INIRec.ReadString(sSecao,'Quantidade'   ,INIRec.ReadString(sSecao,'qBCProd'  ,'')) ,0);
                  PIS.vAliqProd := StringToFloatDef( INIRec.ReadString(sSecao,'ValorAliquota',INIRec.ReadString(sSecao,'vAliqProd','')) ,0);
                  PIS.vPIS      := StringToFloatDef( INIRec.ReadString(sSecao,'Valor'        ,INIRec.ReadString(sSecao,'vPIS'     ,'')) ,0);
                end;
              end;
            end;

            sSecao := 'PISST'+IntToStrZero(I,3) ;
            sFim     := INIRec.ReadString( sSecao,'ValorBase','F')+ INIRec.ReadString( sSecao,'Quantidade','IM') ;
            if (sFim = 'FIM') then
              sFim   := INIRec.ReadString( sSecao,'vBC','F')+ INIRec.ReadString( sSecao,'qBCProd','IM') ;

            if ((sFim <> 'FIM') and ( Length(sFim) > 0 )) then
            begin
              with PISST do
              begin
                vBc       := StringToFloatDef( INIRec.ReadString(sSecao,'ValorBase'    ,INIRec.ReadString(sSecao,'vBC'      ,'')) ,0);
                pPis      := StringToFloatDef( INIRec.ReadString(sSecao,'AliquotaPerc' ,INIRec.ReadString(sSecao,'pPis'     ,'')) ,0);
                qBCProd   := StringToFloatDef( INIRec.ReadString(sSecao,'Quantidade'   ,INIRec.ReadString(sSecao,'qBCProd'  ,'')) ,0);
                vAliqProd := StringToFloatDef( INIRec.ReadString(sSecao,'AliquotaValor',INIRec.ReadString(sSecao,'vAliqProd','')) ,0);
                vPIS      := StringToFloatDef( INIRec.ReadString(sSecao,'ValorPISST'   ,INIRec.ReadString(sSecao,'vPIS'     ,'')) ,0);
              end;
            end;

            sSecao := 'COFINS'+IntToStrZero(I,3) ;
            sFim     := INIRec.ReadString( sSecao,'CST','FIM') ;
            if ((sFim <> 'FIM') and ( Length(sFim) > 0 )) then
            begin
              with COFINS do
              begin
                CST := StrToCSTCOFINS(OK, INIRec.ReadString( sSecao,'CST',''));
                if OK then
                begin
                  COFINS.vBC       := StringToFloatDef( INIRec.ReadString(sSecao,'ValorBase'    ,INIRec.ReadString(sSecao,'vBC'      ,'')) ,0);
                  COFINS.pCOFINS   := StringToFloatDef( INIRec.ReadString(sSecao,'Aliquota'     ,INIRec.ReadString(sSecao,'pCOFINS'  ,'')) ,0);
                  COFINS.qBCProd   := StringToFloatDef( INIRec.ReadString(sSecao,'Quantidade'   ,INIRec.ReadString(sSecao,'qBCProd'  ,'')) ,0);
                  COFINS.vAliqProd := StringToFloatDef( INIRec.ReadString(sSecao,'ValorAliquota',INIRec.ReadString(sSecao,'vAliqProd','')) ,0);
                  COFINS.vCOFINS   := StringToFloatDef( INIRec.ReadString(sSecao,'Valor'        ,INIRec.ReadString(sSecao,'vCOFINS'  ,'')) ,0);
                end;
              end;
            end;

            sSecao := 'COFINSST'+IntToStrZero(I,3) ;
            sFim     := INIRec.ReadString( sSecao,'ValorBase','F')+ INIRec.ReadString( sSecao,'Quantidade','IM');
            if (sFim = 'FIM') then
              sFim   := INIRec.ReadString( sSecao,'vBC','F')+ INIRec.ReadString( sSecao,'qBCProd','IM') ;

            if ((sFim <> 'FIM') and ( Length(sFim) > 0 )) then
            begin
              with COFINSST do
              begin
                vBC       := StringToFloatDef( INIRec.ReadString(sSecao,'ValorBase'    ,INIRec.ReadString(sSecao,'vBC'      ,'')) ,0);
                pCOFINS   := StringToFloatDef( INIRec.ReadString(sSecao,'AliquotaPerc' ,INIRec.ReadString(sSecao,'pCOFINS'  ,'')) ,0);
                qBCProd   := StringToFloatDef( INIRec.ReadString(sSecao,'Quantidade'   ,INIRec.ReadString(sSecao,'qBCProd'  ,'')) ,0);
                vAliqProd := StringToFloatDef( INIRec.ReadString(sSecao,'AliquotaValor',INIRec.ReadString(sSecao,'vAliqProd','')) ,0);
                vCOFINS   := StringToFloatDef( INIRec.ReadString(sSecao,'ValorCOFINSST',INIRec.ReadString(sSecao,'vCOFINS'  ,'')) ,0);
              end;
            end;

            sSecao := 'ISSQN'+IntToStrZero(I,3) ;
            sFim     := INIRec.ReadString( sSecao,'ValorBase',INIRec.ReadString(sSecao,'vBC'   ,'FIM')) ;
            if (sFim = 'FIM') then
              sFim := INIRec.ReadString( sSecao,'vBC','FIM');

            if ((sFim <> 'FIM') and ( Length(sFim) > 0 )) then
            begin
              with ISSQN do
              begin
                if StringToFloatDef( INIRec.ReadString(sSecao,'ValorBase',INIRec.ReadString(sSecao,'vBC','')) ,0) > 0 then
                begin
                  vBC       := StringToFloatDef( INIRec.ReadString(sSecao,'ValorBase'   ,INIRec.ReadString(sSecao,'vBC'   ,'')) ,0);
                  vAliq     := StringToFloatDef( INIRec.ReadString(sSecao,'Aliquota'    ,INIRec.ReadString(sSecao,'vAliq' ,'')) ,0);
                  vISSQN    := StringToFloatDef( INIRec.ReadString(sSecao,'ValorISSQN'  ,INIRec.ReadString(sSecao,'vISSQN','')) ,0);
                  cMunFG    := StrToInt( INIRec.ReadString(sSecao,'MunicipioFatoGerador',INIRec.ReadString(sSecao,'cMunFG','')));
                  cListServ := INIRec.ReadString(sSecao,'CodigoServico',INIRec.ReadString(sSecao,'cListServ',''));
                  cSitTrib  := StrToISSQNcSitTrib( OK,INIRec.ReadString(sSecao,'cSitTrib','')) ;
                  vDeducao    := StringToFloatDef( INIRec.ReadString(sSecao,'ValorDeducao'   ,INIRec.ReadString(sSecao,'vDeducao'   ,'')) ,0);
                  vOutro      := StringToFloatDef( INIRec.ReadString(sSecao,'ValorOutro'   ,INIRec.ReadString(sSecao,'vOutro'   ,'')) ,0);
                  vDescIncond := StringToFloatDef( INIRec.ReadString(sSecao,'ValorDescontoIncondicional'   ,INIRec.ReadString(sSecao,'vDescIncond'   ,'')) ,0);
                  vDescCond   := StringToFloatDef( INIRec.ReadString(sSecao,'vDescontoCondicional'   ,INIRec.ReadString(sSecao,'vDescCond'   ,'')) ,0);
                  vISSRet     := StringToFloatDef( INIRec.ReadString(sSecao,'ValorISSRetido'   ,INIRec.ReadString(sSecao,'vISSRet'   ,'')) ,0);
                  indISS      := StrToindISS( OK,INIRec.ReadString(sSecao,'indISS','')) ;
                  cServico    := INIRec.ReadString(sSecao,'cServico','');
                  cMun        := INIRec.ReadInteger(sSecao,'cMun',0);
                  cPais       := INIRec.ReadInteger(sSecao,'cPais',1058);
                  nProcesso   := INIRec.ReadString(sSecao,'nProcesso','');
                  indIncentivo := StrToindIncentivo( OK,INIRec.ReadString(sSecao,'indIncentivo','')) ;
                end;
              end;
            end;
          end;
        end;
        }
        Inc( I ) ;
      end ;
      {
      Total.ICMSTot.vBC     := StringToFloatDef( INIRec.ReadString('Total','BaseICMS'     ,INIRec.ReadString('Total','vBC'     ,'')) ,0) ;
      Total.ICMSTot.vICMS   := StringToFloatDef( INIRec.ReadString('Total','ValorICMS'    ,INIRec.ReadString('Total','vICMS'   ,'')) ,0) ;
      Total.ICMSTot.vICMSDeson := StringToFloatDef( INIRec.ReadString('Total','vICMSDeson',''),0) ;
      Total.ICMSTot.vFCP       := StringToFloatDef( INIRec.ReadString('Total','ValorFCP',  INIRec.ReadString('Total','vFCP','')) ,0) ;
      Total.ICMSTot.vBCST   := StringToFloatDef( INIRec.ReadString('Total','BaseICMSSubstituicao' ,INIRec.ReadString('Total','vBCST','')) ,0) ;
      Total.ICMSTot.vST     := StringToFloatDef( INIRec.ReadString('Total','ValorICMSSubstituicao',INIRec.ReadString('Total','vST'  ,'')) ,0) ;
      Total.ICMSTot.vFCPST  := StringToFloatDef( INIRec.ReadString('Total','ValorFCPST',INIRec.ReadString('Total','vFCPST'  ,'')) ,0) ;
      Total.ICMSTot.vFCPSTRet:= StringToFloatDef( INIRec.ReadString('Total','ValorFCPSTRet',INIRec.ReadString('Total','vFCPSTRet'  ,'')) ,0) ;
      Total.ICMSTot.vProd   := StringToFloatDef( INIRec.ReadString('Total','ValorProduto' ,INIRec.ReadString('Total','vProd'  ,'')) ,0) ;
      Total.ICMSTot.vFrete  := StringToFloatDef( INIRec.ReadString('Total','ValorFrete'   ,INIRec.ReadString('Total','vFrete' ,'')) ,0) ;
      Total.ICMSTot.vSeg    := StringToFloatDef( INIRec.ReadString('Total','ValorSeguro'  ,INIRec.ReadString('Total','vSeg'   ,'')) ,0) ;
      Total.ICMSTot.vDesc   := StringToFloatDef( INIRec.ReadString('Total','ValorDesconto',INIRec.ReadString('Total','vDesc'  ,'')) ,0) ;
      Total.ICMSTot.vII     := StringToFloatDef( INIRec.ReadString('Total','ValorII'      ,INIRec.ReadString('Total','vII'    ,'')) ,0) ;
      Total.ICMSTot.vIPI    := StringToFloatDef( INIRec.ReadString('Total','ValorIPI'     ,INIRec.ReadString('Total','vIPI'   ,'')) ,0) ;
      Total.ICMSTot.vIPIDevol:= StringToFloatDef( INIRec.ReadString('Total','ValorIPIDevol',INIRec.ReadString('Total','vIPIDevol'  ,'')) ,0) ;
      Total.ICMSTot.vPIS    := StringToFloatDef( INIRec.ReadString('Total','ValorPIS'     ,INIRec.ReadString('Total','vPIS'   ,'')) ,0) ;
      Total.ICMSTot.vCOFINS := StringToFloatDef( INIRec.ReadString('Total','ValorCOFINS'  ,INIRec.ReadString('Total','vCOFINS','')) ,0) ;
      Total.ICMSTot.vOutro  := StringToFloatDef( INIRec.ReadString('Total','ValorOutrasDespesas',INIRec.ReadString('Total','vOutro','')) ,0) ;
      Total.ICMSTot.vNF     := StringToFloatDef( INIRec.ReadString('Total','ValorNota'    ,INIRec.ReadString('Total','vNF'    ,'')) ,0) ;
      Total.ICMSTot.vTotTrib:= StringToFloatDef( INIRec.ReadString('Total','vTotTrib'     ,''),0) ;
      Total.ICMSTot.vFCPUFDest  := StringToFloatDef( INIRec.ReadString('Total','vFCPUFDest',''),0);
      Total.ICMSTot.vICMSUFDest := StringToFloatDef( INIRec.ReadString('Total','vICMSUFDest',''),0);
      Total.ICMSTot.vICMSUFRemet:= StringToFloatDef( INIRec.ReadString('Total','vICMSUFRemet',''),0);

      Total.ISSQNtot.vServ  := StringToFloatDef( INIRec.ReadString('Total','ValorServicos',INIRec.ReadString('ISSQNtot','vServ','')) ,0) ;
      Total.ISSQNTot.vBC    := StringToFloatDef( INIRec.ReadString('Total','ValorBaseISS' ,INIRec.ReadString('ISSQNtot','vBC'  ,'')) ,0) ;
      Total.ISSQNTot.vISS   := StringToFloatDef( INIRec.ReadString('Total','ValorISSQN'   ,INIRec.ReadString('ISSQNtot','vISS' ,'')) ,0) ;
      Total.ISSQNTot.vPIS   := StringToFloatDef( INIRec.ReadString('Total','ValorPISISS'  ,INIRec.ReadString('ISSQNtot','vPIS' ,'')) ,0) ;
      Total.ISSQNTot.vCOFINS := StringToFloatDef( INIRec.ReadString('Total','ValorCONFINSISS',INIRec.ReadString('ISSQNtot','vCOFINS','')) ,0) ;
      Total.ISSQNtot.dCompet     := StringToDateTime(INIRec.ReadString('ISSQNtot','dCompet','0'));
      Total.ISSQNtot.vDeducao    := StringToFloatDef( INIRec.ReadString('ISSQNtot','vDeducao'   ,'') ,0) ;
      Total.ISSQNtot.vOutro      := StringToFloatDef( INIRec.ReadString('ISSQNtot','vOutro'   ,'') ,0) ;
      Total.ISSQNtot.vDescIncond := StringToFloatDef( INIRec.ReadString('ISSQNtot','vDescIncond'   ,'') ,0) ;
      Total.ISSQNtot.vDescCond   := StringToFloatDef( INIRec.ReadString('ISSQNtot','vDescCond'   ,'') ,0) ;
      Total.ISSQNtot.vISSRet     := StringToFloatDef( INIRec.ReadString('ISSQNtot','vISSRet'   ,'') ,0) ;
      Total.ISSQNtot.cRegTrib    := StrToRegTribISSQN( OK,INIRec.ReadString('ISSQNtot','cRegTrib','1')) ;

      Total.retTrib.vRetPIS    := StringToFloatDef( INIRec.ReadString('retTrib','vRetPIS'   ,'') ,0) ;
      Total.retTrib.vRetCOFINS := StringToFloatDef( INIRec.ReadString('retTrib','vRetCOFINS','') ,0) ;
      Total.retTrib.vRetCSLL   := StringToFloatDef( INIRec.ReadString('retTrib','vRetCSLL'  ,'') ,0) ;
      Total.retTrib.vBCIRRF    := StringToFloatDef( INIRec.ReadString('retTrib','vBCIRRF'   ,'') ,0) ;
      Total.retTrib.vIRRF      := StringToFloatDef( INIRec.ReadString('retTrib','vIRRF'     ,'') ,0) ;
      Total.retTrib.vBCRetPrev := StringToFloatDef( INIRec.ReadString('retTrib','vBCRetPrev','') ,0) ;
      Total.retTrib.vRetPrev   := StringToFloatDef( INIRec.ReadString('retTrib','vRetPrev'  ,'') ,0) ;

      sSecao := IfThen( INIRec.SectionExists('Transportador'), 'Transportador', 'transp');
      Transp.modFrete := StrTomodFrete(OK, INIRec.ReadString(sSecao,'FretePorConta',INIRec.ReadString(sSecao,'modFrete','0')));
      Transp.Transporta.CNPJCPF  := INIRec.ReadString(sSecao,'CNPJCPF'  ,'');
      Transp.Transporta.xNome    := INIRec.ReadString(sSecao,'NomeRazao',INIRec.ReadString(sSecao,'xNome',''));
      Transp.Transporta.IE       := INIRec.ReadString(sSecao,'IE'       ,'');
      Transp.Transporta.xEnder   := INIRec.ReadString(sSecao,'Endereco' ,INIRec.ReadString(sSecao,'xEnder',''));
      Transp.Transporta.xMun     := INIRec.ReadString(sSecao,'Cidade'   ,INIRec.ReadString(sSecao,'xMun',''));
      Transp.Transporta.UF       := INIRec.ReadString(sSecao,'UF'       ,'');

      Transp.retTransp.vServ    := StringToFloatDef( INIRec.ReadString(sSecao,'ValorServico',INIRec.ReadString(sSecao,'vServ'   ,'')) ,0) ;
      Transp.retTransp.vBCRet   := StringToFloatDef( INIRec.ReadString(sSecao,'ValorBase'   ,INIRec.ReadString(sSecao,'vBCRet'  ,'')) ,0) ;
      Transp.retTransp.pICMSRet := StringToFloatDef( INIRec.ReadString(sSecao,'Aliquota'    ,INIRec.ReadString(sSecao,'pICMSRet','')) ,0) ;
      Transp.retTransp.vICMSRet := StringToFloatDef( INIRec.ReadString(sSecao,'Valor'       ,INIRec.ReadString(sSecao,'vICMSRet','')) ,0) ;
      Transp.retTransp.CFOP     := INIRec.ReadString(sSecao,'CFOP'     ,'');
      Transp.retTransp.cMunFG   := INIRec.ReadInteger(sSecao,'CidadeCod',INIRec.ReadInteger(sSecao,'cMunFG',0));

      Transp.veicTransp.placa := INIRec.ReadString(sSecao,'Placa'  ,'');
      Transp.veicTransp.UF    := INIRec.ReadString(sSecao,'UFPlaca','');
      Transp.veicTransp.RNTC  := INIRec.ReadString(sSecao,'RNTC'   ,'');

      Transp.vagao := INIRec.ReadString( sSecao,'vagao','') ;
      Transp.balsa := INIRec.ReadString( sSecao,'balsa','') ;

      J := 1 ;
      while true do
      begin
        sSecao := 'Reboque'+IntToStrZero(J,3) ;
        sFim     := INIRec.ReadString(sSecao,'placa','FIM') ;
        if (sFim = 'FIM') or (Length(sFim) <= 0) then
          break;

        with Transp.Reboque.New do
        begin
          placa := sFim;
          UF    := INIRec.ReadString( sSecao,'UF'  ,'') ;
          RNTC  := INIRec.ReadString( sSecao,'RNTC','') ;
        end;

        Inc(J)
      end;

      I := 1 ;
      while true do
      begin
        sSecao := IfThen(INIRec.SectionExists('Volume'+IntToStrZero(I,3)), 'Volume', 'vol');
        sSecao := sSecao+IntToStrZero(I,3) ;
        sQtdVol  := INIRec.ReadString(sSecao,'Quantidade',INIRec.ReadString(sSecao,'qVol','FIM')) ;
        if (sQtdVol = 'FIM') or (Length(sQtdVol) <= 0)  then
          break ;

        with Transp.Vol.New do
        begin
          qVol  := StrToInt(sQtdVol);
          esp   := INIRec.ReadString( sSecao,'Especie'  ,INIRec.ReadString( sSecao,'esp'  ,''));
          marca := INIRec.ReadString( sSecao,'Marca'    ,'');
          nVol  := INIRec.ReadString( sSecao,'Numeracao',INIRec.ReadString( sSecao,'nVol'  ,''));
          pesoL := StringToFloatDef( INIRec.ReadString(sSecao,'PesoLiquido',INIRec.ReadString(sSecao,'pesoL','')) ,0) ;
          pesoB := StringToFloatDef( INIRec.ReadString(sSecao,'PesoBruto'  ,INIRec.ReadString(sSecao,'pesoB','')) ,0) ;

          J := 1;
          while true do
          begin
            sSecao := IfThen(INIRec.SectionExists('lacres'+IntToStrZero(I,3)+IntToStrZero(J,3)), 'lacres', 'Lacre');
            sSecao := sSecao+IntToStrZero(I,3)+IntToStrZero(J,3) ;
            sFim   := INIRec.ReadString(sSecao,'nLacre','FIM') ;
            if (sFim = 'FIM') or (Length(sFim) <= 0)  then
              break ;

            Lacres.New.nLacre := sFim;

            Inc(J);
          end;
        end;

        Inc(I);
      end;

      sSecao := IfThen(INIRec.SectionExists('Fatura'), 'Fatura', 'fat');
      Cobr.Fat.nFat  := INIRec.ReadString( sSecao,'Numero',INIRec.ReadString( sSecao,'nFat',''));
      Cobr.Fat.vOrig := StringToFloatDef( INIRec.ReadString(sSecao,'ValorOriginal',INIRec.ReadString(sSecao,'vOrig','')) ,0) ;
      Cobr.Fat.vDesc := StringToFloatDef( INIRec.ReadString(sSecao,'ValorDesconto',INIRec.ReadString(sSecao,'vDesc','')) ,0) ;
      Cobr.Fat.vLiq  := StringToFloatDef( INIRec.ReadString(sSecao,'ValorLiquido' ,INIRec.ReadString(sSecao,'vLiq' ,'')) ,0) ;

      I := 1 ;
      while true do
      begin
        sSecao   := IfThen(INIRec.SectionExists('Duplicata'+IntToStrZero(I,3)), 'Duplicata', 'dup');
        sSecao   := sSecao+IntToStrZero(I,3) ;
        sDupNumber := INIRec.ReadString(sSecao,'Numero',INIRec.ReadString(sSecao,'nDup','FIM')) ;
        if (sDupNumber = 'FIM') or (Length(sDupNumber) <= 0) then
          break ;

        with Cobr.Dup.New do
        begin
          nDup  := sDupNumber;
          dVenc := StringToDateTime(INIRec.ReadString( sSecao,'DataVencimento',INIRec.ReadString( sSecao,'dVenc','0')));
          vDup  := StringToFloatDef( INIRec.ReadString(sSecao,'Valor',INIRec.ReadString(sSecao,'vDup','')) ,0) ;
        end;

        Inc(I);
      end;

      I := 1 ;
      while true do
      begin
        sSecao := 'pag'+IntToStrZero(I,3) ;
        sFim     := INIRec.ReadString(sSecao,'tpag','FIM');
        if (sFim = 'FIM') or (Length(sFim) <= 0) then
          break ;

        with pag.New do
        begin
          tPag  := StrToFormaPagamento(OK,sFim);
          vPag  := StringToFloatDef( INIRec.ReadString(sSecao,'vPag','') ,0) ;
          indPag:= StrToIndpag(OK,INIRec.ReadString( sSecao,'indPag','0'));

          tpIntegra  := StrTotpIntegra(OK,INIRec.ReadString(sSecao,'tpIntegra',''));
          CNPJ  := INIRec.ReadString(sSecao,'CNPJ','');
          tBand := StrToBandeiraCartao(OK,INIRec.ReadString(sSecao,'tBand','99'));
          cAut  := INIRec.ReadString(sSecao,'cAut','');
        end;
        pag.vTroco:= StringToFloatDef( INIRec.ReadString(sSecao,'vTroco','') ,0) ;

        Inc(I);
      end;

      sSecao := IfThen(INIRec.SectionExists('DadosAdicionais'), 'DadosAdicionais', 'infAdic');
      InfAdic.infAdFisco := INIRec.ReadString( sSecao,'Fisco'      ,INIRec.ReadString( sSecao,'infAdFisco',''));
      InfAdic.infCpl     := INIRec.ReadString( sSecao,'Complemento',INIRec.ReadString( sSecao,'infCpl'    ,''));

      I := 1 ;
      while true do
      begin
        sSecao := IfThen(INIRec.SectionExists('obsCont'+IntToStrZero(I,3)), 'obsCont', 'InfAdic');
        sSecao     := sSecao+IntToStrZero(I,3) ;
        sAdittionalField := INIRec.ReadString(sSecao,'Campo',INIRec.ReadString(sSecao,'xCampo','FIM')) ;
        if (sAdittionalField = 'FIM') or (Length(sAdittionalField) <= 0) then
          break ;

        with InfAdic.obsCont.New do
        begin
          xCampo := sAdittionalField;
          xTexto := INIRec.ReadString( sSecao,'Texto',INIRec.ReadString( sSecao,'xTexto',''));
        end;

        Inc(I);
      end;

      I := 1 ;
      while true do
      begin
        sSecao := 'obsFisco'+IntToStrZero(I,3) ;
        sAdittionalField := INIRec.ReadString(sSecao,'Campo',INIRec.ReadString(sSecao,'xCampo','FIM')) ;
        if (sAdittionalField = 'FIM') or (Length(sAdittionalField) <= 0) then
          break ;

        with InfAdic.obsFisco.New do
        begin
          xCampo := sAdittionalField;
          xTexto := INIRec.ReadString( sSecao,'Texto',INIRec.ReadString( sSecao,'xTexto',''));
        end;

        Inc(I);
      end;

      I := 1 ;
      while true do
      begin
        sSecao := 'procRef'+IntToStrZero(I,3) ;
        sAdittionalField := INIRec.ReadString(sSecao,'nProc','FIM') ;
        if (sAdittionalField = 'FIM') or (Length(sAdittionalField) <= 0) then
          break ;

        with InfAdic.procRef.New do
        begin
          nProc := sAdittionalField;
          indProc := StrToindProc(OK,INIRec.ReadString( sSecao,'indProc','0'));
        end;

        Inc(I);
      end;

      sFim   := INIRec.ReadString( 'exporta','UFembarq',INIRec.ReadString( 'exporta','UFSaidaPais','FIM')) ;
      if ((sFim <> 'FIM') and ( Length(sFim) > 0 )) then
      begin
        exporta.UFembarq     := INIRec.ReadString( 'exporta','UFembarq','') ;;
        exporta.xLocEmbarq   := INIRec.ReadString( 'exporta','xLocEmbarq','');
        exporta.UFSaidaPais  := INIRec.ReadString( 'exporta','UFSaidaPais','');
        exporta.xLocExporta  := INIRec.ReadString( 'exporta','xLocExporta','');
        exporta.xLocDespacho := INIRec.ReadString( 'exporta','xLocDespacho','');
      end;

      if (INIRec.ReadString( 'compra','xNEmp','') <> '') or
         (INIRec.ReadString( 'compra','xPed' ,'') <> '') or
         (INIRec.ReadString( 'compra','xCont','') <> '') then
      begin
        compra.xNEmp := INIRec.ReadString( 'compra','xNEmp','');
        compra.xPed  := INIRec.ReadString( 'compra','xPed','');
        compra.xCont := INIRec.ReadString( 'compra','xCont','');
      end;

      cana.safra   := INIRec.ReadString( 'cana','safra','');
      cana.ref     := INIRec.ReadString( 'cana','ref'  ,'');
      cana.qTotMes := StringToFloatDef( INIRec.ReadString('cana','qTotMes','') ,0) ;
      cana.qTotAnt := StringToFloatDef( INIRec.ReadString('cana','qTotAnt','') ,0) ;
      cana.qTotGer := StringToFloatDef( INIRec.ReadString('cana','qTotGer','') ,0) ;
      cana.vFor    := StringToFloatDef( INIRec.ReadString('cana','vFor'   ,'') ,0) ;
      cana.vTotDed := StringToFloatDef( INIRec.ReadString('cana','vTotDed','') ,0) ;
      cana.vLiqFor := StringToFloatDef( INIRec.ReadString('cana','vLiqFor','') ,0) ;

      I := 1 ;
      while true do
      begin
        sSecao := 'forDia'+IntToStrZero(I,3) ;
        sDay     := INIRec.ReadString(sSecao,'dia','FIM') ;
        if (sDay = 'FIM') or (Length(sDay) <= 0) then
          break ;

        with cana.fordia.New do
        begin
          dia  := StrToInt(sDay);
          qtde := StringToFloatDef( INIRec.ReadString(sSecao,'qtde'   ,'') ,0) ;
        end;

        Inc(I);
      end;

      I := 1 ;
      while true do
      begin
        sSecao := 'deduc'+IntToStrZero(I,3) ;
        sDeduc   := INIRec.ReadString(sSecao,'xDed','FIM') ;
        if (sDeduc = 'FIM') or (Length(sDeduc) <= 0) then
          break ;

        with cana.deduc.New do
        begin
          xDed := sDeduc;
          vDed := StringToFloatDef( INIRec.ReadString(sSecao,'vDed'   ,'') ,0) ;
        end;

        Inc(I);
      end;

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
      }
    end;

    GerarXML;

    Result := True;
  finally
    INIRec.Free;
  end;
end;

function NotaFiscal.GerarNF3eIni: String;
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

function NotaFiscal.GravarXML(const NomeArquivo: String; const PathArquivo: String): Boolean;
begin
  if EstaVazio(FXMLOriginal) then
    GerarXML;

  FNomeArq := CalcularNomeArquivoCompleto(NomeArquivo, PathArquivo);

  Result := TACBrNF3e(TNotasFiscais(Collection).ACBrNF3e).Gravar(FNomeArq, FXMLOriginal);
end;

function NotaFiscal.GravarStream(AStream: TStream): Boolean;
begin
  if EstaVazio(FXMLOriginal) then
    GerarXML;

  AStream.Size := 0;
  WriteStrToStream(AStream, AnsiString(FXMLOriginal));

  Result := True;
end;

procedure NotaFiscal.EnviarEmail(const sPara, sAssunto: String; sMensagem: TStrings;
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

function NotaFiscal.GerarXML: String;
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

function NotaFiscal.CalcularNomeArquivo: String;
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

function NotaFiscal.CalcularPathArquivo: String;
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

function NotaFiscal.CalcularNomeArquivoCompleto(NomeArquivo: String;
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

function NotaFiscal.ValidarConcatChave: Boolean;
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

function NotaFiscal.GetConfirmada: Boolean;
begin
  Result := TACBrNF3e(TNotasFiscais(Collection).ACBrNF3e).CstatConfirmada(
    FNF3e.procNF3e.cStat);
end;

function NotaFiscal.GetcStat: Integer;
begin
  Result := FNF3e.procNF3e.cStat;
end;

function NotaFiscal.GetProcessada: Boolean;
begin
  Result := TACBrNF3e(TNotasFiscais(Collection).ACBrNF3e).CstatProcessado(
    FNF3e.procNF3e.cStat);
end;

function NotaFiscal.GetCancelada: Boolean;
begin
  Result := TACBrNF3e(TNotasFiscais(Collection).ACBrNF3e).CstatCancelada(
    FNF3e.procNF3e.cStat);
end;

function NotaFiscal.GetMsg: String;
begin
  Result := FNF3e.procNF3e.xMotivo;
end;

function NotaFiscal.GetNumID: String;
begin
  Result := OnlyNumber(NF3e.infNF3e.ID);
end;

function NotaFiscal.GetXMLAssinado: String;
begin
  if EstaVazio(FXMLAssinado) then
    Assinar;

  Result := FXMLAssinado;
end;

procedure NotaFiscal.SetXML(const AValue: String);
begin
  LerXML(AValue);
end;

procedure NotaFiscal.SetXMLOriginal(const AValue: String);
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

procedure TNotasFiscais.GerarNF3e;
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
