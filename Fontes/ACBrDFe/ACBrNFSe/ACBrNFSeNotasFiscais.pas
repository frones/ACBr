{******************************************************************************}
{ Projeto: Componente ACBrNFSe                                                 }
{  Biblioteca multiplataforma de componentes Delphi para emissão de Nota Fiscal}
{  de Serviço eletrônica - NFSe                                                }

{ Direitos Autorais Reservados (c) 2008 Wemerson Souto                         }
{                                       Daniel Simoes de Almeida               }
{                                       André Ferreira de Moraes               }

{ Colaboradores nesse arquivo:                                                 }

{  Você pode obter a última versão desse arquivo na pagina do Projeto ACBr     }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr           }


{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }

{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }

{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }

{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }

{******************************************************************************}

{$I ACBr.inc}

unit ACBrNFSeNotasFiscais;

interface

uses
  Classes, SysUtils, Dialogs, Forms, StrUtils,
  ACBrNFSeConfiguracoes, ACBrDFeUtil,
  pnfsNFSe, pnfsNFSeR, pnfsNFSeW, pcnConversao, pcnAuxiliar, pcnLeitor;

type

  { NotaFiscal }

  NotaFiscal = class(TCollectionItem)
  private
    FNFSe: TNFSe;
    FNFSeW: TNFSeW;
    FNFSeR: TNFSeR;

    FXML: String;
    FXMLAssinado: String;
    FXMLOriginal: String;
    FAlertas: String;
    FErroValidacao: String;
    FErroValidacaoCompleto: String;
    FErroRegrasdeNegocios: String;
    FNomeArq: String;

    function GetConfirmada: Boolean;
    function GetProcessada: Boolean;

    function GetMsg: String;
    function GetNumID: String;
    function GetXMLAssinado: String;
    function ValidarConcatChave: Boolean;
    function CalcularNomeArquivo: String;
    function CalcularPathArquivo: String;
    function CalcularNomeArquivoCompleto(NomeArquivo: String = '';
      PathArquivo: String = ''): String;

    procedure Assinar;
    procedure Validar;
  public
    constructor Create(Collection2: TCollection); override;
    destructor Destroy; override;
    procedure Imprimir;
    procedure ImprimirPDF;

    function VerificarAssinatura: Boolean;
    function ValidarRegrasdeNegocios: Boolean;

    function LerXML(AXML: AnsiString): Boolean;

    function GerarXML: String;
    function GravarXML(NomeArquivo: String = ''; PathArquivo: String = ''): Boolean;

    function GravarStream(AStream: TStream): Boolean;

    procedure EnviarEmail(sPara, sAssunto: String; sMensagem: TStrings = nil;
      EnviaPDF: Boolean = True; sCC: TStrings = nil; Anexos: TStrings = nil);

    property NomeArq: String read FNomeArq write FNomeArq;

    property NFSe: TNFSe read FNFSe;

    property XML: String read FXML;
    property XMLOriginal: String read FXMLOriginal write FXMLOriginal;
    property XMLAssinado: String read GetXMLAssinado;
    property Confirmada: Boolean read GetConfirmada;
    property Processada: Boolean read GetProcessada;
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
    FACBrNFSe: TComponent;
    FConfiguracoes: TConfiguracoesNFSe;

    function GetItem(Index: integer): NotaFiscal;
    procedure SetItem(Index: integer; const Value: NotaFiscal);

    procedure VerificarDANFSE;
    procedure Validar;
  public
    constructor Create(AOwner: TPersistent; ItemClass: TCollectionItemClass);

    procedure GerarNFSe;
    function VerificarAssinatura(out Erros: String): Boolean;
    function ValidarRegrasdeNegocios(out Erros: String): Boolean;

    procedure Assinar;
    procedure Imprimir;
    procedure ImprimirPDF;

    function Add: NotaFiscal;
    function Insert(Index: integer): NotaFiscal;

    property Items[Index: integer]: NotaFiscal read GetItem write SetItem; default;

    function GetNamePath: String; override;
    // Incluido o Parametro AGerarNFSe que determina se após carregar os dados da NFSe
    // para o componente, será gerado ou não novamente o XML da NFSe.
    function LoadFromFile(CaminhoArquivo: String; AGerarNFSe: Boolean = True): Boolean;
    function LoadFromStream(AStream: TStringStream; AGerarNFSe: Boolean = True): Boolean;
    function LoadFromString(AXMLString: String; AGerarNFSe: Boolean = True): Boolean;
    function GravarXML(PathNomeArquivo: String = ''): Boolean;

    property ACBrNFSe: TComponent read FACBrNFSe;
  end;

implementation

uses
  ACBrNFSe, ACBrUtil, pnfsConversao, synautil;

{ NotaFiscal }

constructor NotaFiscal.Create(Collection2: TCollection);
begin
  inherited Create(Collection2);
  FNFSe := TNFSe.Create;
  FNFSeW := TNFSeW.Create(FNFSe);
  FNFSeR := TNFSeR.Create(FNFSe);

  with TACBrNFSe(TNotasFiscais(Collection).ACBrNFSe) do
  begin
  (*
    FNFSe.Ide.tpNF := tnSaida;
    FNFSe.Ide.indPag := ipVista;
    FNFSe.Ide.verProc := 'ACBrNFSe';
    FNFSe.Ide.tpAmb := Configuracoes.WebServices.Ambiente;
    FNFSe.Ide.tpEmis := Configuracoes.Geral.FormaEmissao;

    if Assigned(DANFSE) then
      FNFSe.Ide.tpImp := DANFSE.TipoDANFSE;

    FNFSe.Emit.EnderEmit.xPais := 'BRASIL';
    FNFSe.Emit.EnderEmit.cPais := 1058;
    FNFSe.Emit.EnderEmit.nro := 'SEM NUMERO';
   *)
  end;
end;

destructor NotaFiscal.Destroy;
begin
  FNFSeW.Free;
  FNFSeR.Free;
  FNFSe.Free;
  inherited Destroy;
end;

procedure NotaFiscal.Imprimir;
begin
  with TACBrNFSe(TNotasFiscais(Collection).ACBrNFSe) do
  begin
    if not Assigned(DANFSE) then
      raise EACBrNFSeException.Create('Componente DANFSE não associado.')
    else
      DANFSE.ImprimirDANFSE(NFSe);
  end;
end;

procedure NotaFiscal.ImprimirPDF;
begin
  with TACBrNFSe(TNotasFiscais(Collection).ACBrNFSe) do
  begin
    if not Assigned(DANFSE) then
      raise EACBrNFSeException.Create('Componente DANFSE não associado.')
    else
      DANFSE.ImprimirDANFSEPDF(NFSe);
  end;
end;

procedure NotaFiscal.Assinar;
var
  XMLAss: String;
  ArqXML: String;
  Leitor: TLeitor;
begin
  ArqXML := GerarXML;

  // XML já deve estar em UTF8, para poder ser assinado //
  ArqXML := ConverteXMLtoUTF8(ArqXML);
  FXMLOriginal := ArqXML;

  with TACBrNFSe(TNotasFiscais(Collection).ACBrNFSe) do
  begin
    XMLAss := SSL.Assinar(ArqXML, 'NFSe', 'infNFSe');
    FXMLAssinado := XMLAss;

    // Remove header, pois podem existir várias Notas no XML //
    //TODO: Verificar se precisa
    //XMLAss := StringReplace(XMLAss, '<' + ENCODING_UTF8_STD + '>', '', [rfReplaceAll]);
    //XMLAss := StringReplace(XMLAss, '<' + XML_V01 + '>', '', [rfReplaceAll]);

    Leitor := TLeitor.Create;
    try
      leitor.Grupo := XMLAss;
      NFSe.signature.URI := Leitor.rAtributo('Reference URI=');
      NFSe.signature.DigestValue := Leitor.rCampo(tcStr, 'DigestValue');
      NFSe.signature.SignatureValue := Leitor.rCampo(tcStr, 'SignatureValue');
      NFSe.signature.X509Certificate := Leitor.rCampo(tcStr, 'X509Certificate');
    finally
      Leitor.Free;
    end;

    if Configuracoes.Geral.Salvar then
      Gravar(CalcularNomeArquivoCompleto(), XMLAss);

    if NaoEstaVazio(NomeArq) then
      Gravar(NomeArq, XMLAss);
  end;
end;

procedure NotaFiscal.Validar;
var
  Erro, AXML: String;
  NotaEhValida: Boolean;
  ALayout: TLayOutNFSe;
begin
  AXML := FXMLAssinado;

  if EstaVazio(AXML) then
  begin
    Assinar;
    AXML := FXMLAssinado;
  end;

  with TACBrNFSe(TNotasFiscais(Collection).ACBrNFSe) do
  begin
  (*
    if EhAutorizacao then
      ALayout := LayNFSeRetAutorizacao
    else
      ALayout := LayNFSeRetRecepcao;
  *)
    NotaEhValida := SSL.Validar(AXML, GerarNomeArqSchema(ALayout, 1.00 {FNFSe.infNFSe.Versao}), Erro);

    if not NotaEhValida then
    begin
      FErroValidacao := ACBrStr('Falha na validação dos dados da nota: ') +
        NFSe.IdentificacaoRps.Numero + sLineBreak + FAlertas ;
      FErroValidacaoCompleto := FErroValidacao + sLineBreak + Erro;

      raise EACBrNFSeException.CreateDef(
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
  AXML := FXMLOriginal;

  if EstaVazio(AXML) then
  begin
    if EstaVazio(FXMLAssinado) then
      Assinar;

    AXML := FXMLAssinado;
  end;

  with TACBrNFSe(TNotasFiscais(Collection).ACBrNFSe) do
  begin
    AssEhValida := SSL.VerificarAssinatura(AXML, Erro);

    if not AssEhValida then
    begin
      FErroValidacao := ACBrStr('Falha na validação da assinatura da nota: ') +
        NFSe.IdentificacaoRps.Numero + sLineBreak + Erro;
    end;
  end;

  Result := AssEhValida;
end;

function NotaFiscal.ValidarRegrasdeNegocios: Boolean;
var
  Erros: String;

  procedure AdicionaErro(const Erro: String);
  begin
    Erros := Erros + Erro + sLineBreak;
  end;

begin
  with TACBrNFSe(TNotasFiscais(Collection).ACBrNFSe) do
  begin
    Erros := '';
    (*
    if not ValidarConcatChave then  //A03-10
      AdicionaErro(
        '502-Rejeição: Erro na Chave de Acesso - Campo Id não corresponde à concatenação dos campos correspondentes');

    if copy(IntToStr(NFSe.Emit.EnderEmit.cMun), 1, 2) <>
      IntToStr(Configuracoes.WebServices.UFCodigo) then //B02-10
      AdicionaErro('226-Rejeição: Código da UF do Emitente diverge da UF autorizadora');

    if (NFSe.Ide.serie > 899) and  //B07-20
      (NFSe.Ide.tpEmis <> teSCAN) then
      AdicionaErro('503-Rejeição: Série utilizada fora da faixa permitida no SCAN (900-999)');

    if (NFSe.Ide.dEmi > now) then  //B09-10
      AdicionaErro('703-Rejeição: Data-Hora de Emissão posterior ao horário de recebimento');

    if ((now - NFSe.Ide.dEmi) > 30) then  //B09-20
      AdicionaErro('228-Rejeição: Data de Emissão muito atrasada');

    //GB09.02 - Data de Emissão posterior à 31/03/2011
    //GB09.03 - Data de Recepção posterior à 31/03/2011 e tpAmb (B24) = 2

    if not ValidarMunicipio(NFSe.Ide.cMunFG) then //B12-10
      AdicionaErro('270-Rejeição: Código Município do Fato Gerador: dígito inválido');

    if (UFparaCodigo(NFSe.Emit.EnderEmit.UF) <> StrToIntDef(
      copy(IntToStr(NFSe.Ide.cMunFG), 1, 2), 0)) then//GB12.1
      AdicionaErro('271-Rejeição: Código Município do Fato Gerador: difere da UF do emitente');

    if ((NFSe.Ide.tpEmis in [teSCAN, teSVCAN, teSVCRS]) and
      (Configuracoes.Geral.FormaEmissao = teNormal)) then  //B22-30
      AdicionaErro(
        '570-Rejeição: Tipo de Emissão 3, 6 ou 7 só é válido nas contingências SCAN/SVC');

    if ((NFSe.Ide.tpEmis <> teSCAN) and (Configuracoes.Geral.FormaEmissao = teSCAN))
    then  //B22-40
      AdicionaErro('571-Rejeição: Tipo de Emissão informado diferente de 3 para contingência SCAN');

    if ((Configuracoes.Geral.FormaEmissao in [teSVCAN, teSVCRS]) and
      (not (NFSe.Ide.tpEmis in [teSVCAN, teSVCRS]))) then  //B22-60
      AdicionaErro('713-Rejeição: Tipo de Emissão diferente de 6 ou 7 para contingência da SVC acessada');

    //B23-10
    if (NFSe.Ide.tpAmb <> Configuracoes.WebServices.Ambiente) then
      //B24-10
      AdicionaErro('252-Rejeição: Ambiente informado diverge do Ambiente de recebimento '
        + '(Tipo do ambiente da NF-e difere do ambiente do Web Service)');

    if (not (NFSe.Ide.procEmi in [peAvulsaFisco, peAvulsaContribuinte])) and
      (NFSe.Ide.serie > 889) then //B26-10
      AdicionaErro('266-Rejeição: Série utilizada fora da faixa permitida no Web Service (0-889)');

    if (NFSe.Ide.procEmi in [peAvulsaFisco, peAvulsaContribuinte]) and
      (NFSe.Ide.serie < 890) and (NFSe.Ide.serie > 899) then
      //B26-20
      AdicionaErro('451-Rejeição: Processo de emissão informado inválido');

    if (NFSe.Ide.procEmi in [peAvulsaFisco, peAvulsaContribuinte]) and
      (NFSe.Ide.tpEmis <> teNormal) then //B26-30
      AdicionaErro('370-Rejeição: Nota Fiscal Avulsa com tipo de emissão inválido');

    if (NFSe.Ide.tpEmis = teNormal) and ((NFSe.Ide.xJust > '') or
      (NFSe.Ide.dhCont <> 0)) then
      //B28-10
      AdicionaErro(
        '556-Justificativa de entrada em contingência não deve ser informada para tipo de emissão normal');

    if (NFSe.Ide.tpEmis in [teContingencia, teFSDA, teOffLine]) and
      (NFSe.Ide.xJust = '') then //B28-20
      AdicionaErro('557-A Justificativa de entrada em contingência deve ser informada');

    if (NFSe.Ide.dhCont > now) then //B28-30
      AdicionaErro('558-Rejeição: Data de entrada em contingência posterior a data de recebimento');

    if (NFSe.Ide.dhCont > 0) and ((now - NFSe.Ide.dhCont) > 30) then //B28-40
      AdicionaErro('559-Rejeição: Data de entrada em contingência muito atrasada');

    if (NFSe.Ide.modelo = 65) then  //Regras válidas apenas para NFC-e - 65
    begin
      if (NFSe.Ide.dEmi < now - StrToTime('00:05:00')) and
        (NFSe.Ide.tpEmis in [teNormal, teSCAN, teSVCAN, teSVCRS]) then
        //B09-40
        AdicionaErro('704-Rejeição: NFC-e com Data-Hora de emissão atrasada');

      if (NFSe.Ide.dSaiEnt <> 0) then  //B10-10
        AdicionaErro('705-Rejeição: NFC-e com data de entrada/saída');

      if (NFSe.Ide.tpNF = tnEntrada) then  //B11-10
        AdicionaErro('706-Rejeição: NFC-e para operação de entrada');

      if (NFSe.Ide.idDest <> doInterna) then  //B11-10
        AdicionaErro('707-NFC-e para operação interestadual ou com o exterior');

      if (not (NFSe.Ide.tpImp in [tiNFCe, tiNFCeA4, tiMsgEletronica])) then
        //B21-10
        AdicionaErro('709-Rejeição: NFC-e com formato de DANFSE inválido');

      if (NFSe.Ide.tpEmis = teOffLine) and
        (AnsiIndexStr(NFSe.Emit.EnderEmit.UF, ['SP']) <> -1) then  //B22-20
        AdicionaErro('712-Rejeição: NF-e com contingência off-line');

      if (NFSe.Ide.tpEmis = teSCAN) then //B22-50
        AdicionaErro('782-Rejeição: NFC-e não é autorizada pelo SCAN');

      if (NFSe.Ide.tpEmis in [teSVCAN, teSVCRS]) then  //B22-70
        AdicionaErro('783-Rejeição: NFC-e não é autorizada pela SVC');

      if (NFSe.Ide.finNFSe <> fnNormal) then  //B25-20
        AdicionaErro('715-Rejeição: Rejeição: NFC-e com finalidade inválida');

      if (NFSe.Ide.indFinal = cfNao) then //B25a-10
        AdicionaErro('716-Rejeição: NFC-e em operação não destinada a consumidor final');

      if (not (NFSe.Ide.indPres in [pcPresencial, pcEntregaDomicilio])) then
        //B25b-20
        AdicionaErro('717-Rejeição: NFC-e em operação não presencial');

      if (NFSe.Ide.indPres = pcEntregaDomicilio) and
        (AnsiIndexStr(NFSe.Emit.EnderEmit.UF, ['XX']) <> -1) then
        //B25b-30  Qual estado não permite entrega a domicílio?
        AdicionaErro('785-Rejeição: NFC-e com entrega a domicílio não permitida pela UF');

      if (NFSe.Ide.NFref.Count > 0) then  //BA01-10
        AdicionaErro('708-Rejeição: NFC-e não pode referenciar documento fiscal');

      if (NFSe.Emit.IEST > '') then  //C18-10
        AdicionaErro('718-Rejeição: NFC-e não deve informar IE de Substituto Tributário');
    end;

    if (NFSe.Ide.modelo = 55) then  //Regras válidas apenas para NF-e - 55
    begin
      if ((NFSe.Ide.dSaiEnt - now) > 30) then  //B10-20  - Facultativo
        AdicionaErro('504-Rejeição: Data de Entrada/Saída posterior ao permitido');

      if ((now - NFSe.Ide.dSaiEnt) > 30) then  //B10-30  - Facultativo
        AdicionaErro('505-Rejeição: Data de Entrada/Saída anterior ao permitido');

      if (NFSe.Ide.dSaiEnt < NFSe.Ide.dEmi) then
        //B10-40  - Facultativo
        AdicionaErro('506-Rejeição: Data de Saída menor que a Data de Emissão');

      if (NFSe.Ide.tpImp in [tiNFCe, tiMsgEletronica]) then  //B21-20
        AdicionaErro('710-Rejeição: NF-e com formato de DANFSE inválido');

      if (NFSe.Ide.tpEmis = teOffLine) then  //B22-10
        AdicionaErro('711-Rejeição: NF-e com contingência off-line');

      if (NFSe.Ide.finNFSe = fnComplementar) and (NFSe.Ide.NFref.Count = 0) then  //B25-30
        AdicionaErro('254-Rejeição: NF-e complementar não possui NF referenciada');

      if (NFSe.Ide.finNFSe = fnComplementar) and (NFSe.Ide.NFref.Count > 1) then  //B25-40
        AdicionaErro('255-Rejeição: NF-e complementar possui mais de uma NF referenciada');

      if (NFSe.Ide.finNFSe = fnComplementar) and (NFSe.Ide.NFref.Count = 1) and
        (((NFSe.Ide.NFref.Items[0].RefNF.CNPJ > '') and
        (NFSe.Ide.NFref.Items[0].RefNF.CNPJ <> NFSe.Emit.CNPJCPF)) or
        ((NFSe.Ide.NFref.Items[0].RefNFP.CNPJCPF > '') and
        (NFSe.Ide.NFref.Items[0].RefNFP.CNPJCPF <> NFSe.Emit.CNPJCPF))) then
        //B25-50
        AdicionaErro(
          '269-Rejeição: CNPJ Emitente da NF Complementar difere do CNPJ da NF Referenciada');

      if (NFSe.Ide.finNFSe = fnComplementar) and (NFSe.Ide.NFref.Count = 1) and
        //Testa pelo número para saber se TAG foi preenchida
        (((NFSe.Ide.NFref.Items[0].RefNF.nNF > 0) and
        (NFSe.Ide.NFref.Items[0].RefNF.cUF <> UFparaCodigo(
        NFSe.Emit.EnderEmit.UF))) or ((NFSe.Ide.NFref.Items[0].RefNFP.nNF > 0) and
        (NFSe.Ide.NFref.Items[0].RefNFP.cUF <> UFparaCodigo(
        NFSe.Emit.EnderEmit.UF))))
      then  //B25-60 - Facultativo
        AdicionaErro('678-Rejeição: NF referenciada com UF diferente da NF-e complementar');

      if (NFSe.Ide.finNFSe = fnDevolucao) and (NFSe.Ide.NFref.Count = 0) then
        //B25-70
        AdicionaErro('321-Rejeição: NF-e devolução não possui NF referenciada');

      if (NFSe.Ide.finNFSe = fnDevolucao) and (NFSe.Ide.NFref.Count > 1) then
        //B25-80
        AdicionaErro('322-Rejeição: NF-e devolução possui mais de uma NF referenciada');

      if (NFSe.Ide.indPres = pcEntregaDomicilio) then //B25b-10
        AdicionaErro('794-Rejeição: NF-e com indicativo de NFC-e com entrega a domicílio');
    end;
    *)
  end;

  Result := EstaVazio(Erros);

  if not Result then
  begin
    Erros := ACBrStr('Erro(s) nas Regras de negócios da nota: '+
                     NFSe.IdentificacaoRps.Numero + sLineBreak +
                     Erros);
  end;

  FErroRegrasdeNegocios := Erros;
end;

function NotaFiscal.LerXML(AXML: AnsiString): Boolean;
//var
//  Ok: Boolean;
begin
  Result := False;
  FNFSeR.Leitor.Arquivo := AXML;
  FNFSeR.LerXml;

  FXML := string(AXML);
  FXMLOriginal := FXML;
  Result := True;
end;

function NotaFiscal.GravarXML(NomeArquivo: String; PathArquivo: String): Boolean;
begin
  FNomeArq := CalcularNomeArquivoCompleto(NomeArquivo, PathArquivo);
  GerarXML;
  Result := TACBrNFSe(TNotasFiscais(Collection).ACBrNFSe).Gravar(FNomeArq, FXML);
end;

function NotaFiscal.GravarStream(AStream: TStream): Boolean;
begin
  Result := False;
  GerarXML;

  AStream.Size := 0;
  WriteStrToStream(AStream, AnsiString(FXML) );
  Result := True;
end;

procedure NotaFiscal.EnviarEmail(sPara, sAssunto: String; sMensagem: TStrings;
  EnviaPDF: Boolean; sCC: TStrings; Anexos: TStrings);
var
  NomeArq : String;
  AnexosEmail:TStrings;
  StreamNFSe : TMemoryStream;
begin
  if not Assigned(TACBrNFSe(TNotasFiscais(Collection).ACBrNFSe).MAIL) then
    raise EACBrNFSeException.Create('Componente ACBrMail não associado');

  AnexosEmail := TStringList.Create;
  StreamNFSe := TMemoryStream.Create;
  try
    AnexosEmail.Clear;
    if Assigned(Anexos) then
      AnexosEmail.Assign(Anexos);

    with TACBrNFSe(TNotasFiscais(Collection).ACBrNFSe) do
    begin
      GravarStream(StreamNFSe);

      if (EnviaPDF) then
      begin
        if Assigned(DANFSE) then
        begin
          DANFSE.ImprimirDANFSEPDF(FNFSe);
          NomeArq := PathWithDelim(DANFSE.PathPDF) + NumID + '-NFSe.pdf';
          AnexosEmail.Add(NomeArq);
        end;
      end;

      EnviarEmail( sPara, sAssunto, sMensagem, sCC, AnexosEmail, StreamNFSe,
                   NumID +'-NFSe.xml');
    end;
  finally
    AnexosEmail.Free;
    StreamNFSe.Free;
  end;
end;

function NotaFiscal.GerarXML: String;
begin
  with TACBrNFSe(TNotasFiscais(Collection).ACBrNFSe) do
  begin
    FNFSeW.Gerador.Opcoes.FormatoAlerta := Configuracoes.Geral.FormatoAlerta;
    FNFSeW.Gerador.Opcoes.RetirarAcentos := Configuracoes.Geral.RetirarAcentos;
  end;

  FNFSeW.GerarXml;
  FXML := FNFSeW.Gerador.ArquivoFormatoXML;
  FXMLAssinado := '';
  FAlertas := FNFSeW.Gerador.ListaDeAlertas.Text;
  Result := FXML;
end;

function NotaFiscal.CalcularNomeArquivo: String;
var
  xID: String;
begin
  xID := Self.NumID;

  if EstaVazio(xID) then
    raise EACBrNFSeException.Create('ID Inválido. Impossível Salvar XML');

  Result := xID + '-NFSe.xml';
end;

function NotaFiscal.CalcularPathArquivo: String;
var
  Data: TDateTime;
begin
  with TACBrNFSe(TNotasFiscais(Collection).ACBrNFSe) do
  begin
    if Configuracoes.Arquivos.EmissaoPathNFSe then
      Data := FNFSe.DataEmissaoRps
    else
      Data := Now;

    Result := PathWithDelim(Configuracoes.Arquivos.GetPathNFSe(Data, FNFSe.Prestador.Cnpj));
  end;
end;

function NotaFiscal.CalcularNomeArquivoCompleto(NomeArquivo: String;
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

function NotaFiscal.ValidarConcatChave: Boolean;
var
  wAno, wMes, wDia: word;
begin
  DecodeDate(NFSe.DataEmissaoRps, wAno, wMes, wDia);
(*
  Result := not
    ((Copy(NFSe.infNFSe.ID, 4, 2) <> IntToStrZero(NFSe.Ide.cUF, 2)) or
    (Copy(NFSe.infNFSe.ID, 6, 2)  <> Copy(FormatFloat('0000', wAno), 3, 2)) or
    (Copy(NFSe.infNFSe.ID, 8, 2)  <> FormatFloat('00', wMes)) or
    (Copy(NFSe.infNFSe.ID, 10, 14)<> PadLeft(OnlyNumber(NFSe.Emit.CNPJCPF), 14, '0')) or
    (Copy(NFSe.infNFSe.ID, 24, 2) <> IntToStrZero(NFSe.Ide.modelo, 2)) or
    (Copy(NFSe.infNFSe.ID, 26, 3) <> IntToStrZero(NFSe.Ide.serie, 3)) or
    (Copy(NFSe.infNFSe.ID, 29, 9) <> IntToStrZero(NFSe.Ide.nNF, 9)) or
    (Copy(NFSe.infNFSe.ID, 38, 1) <> TpEmisToStr(NFSe.Ide.tpEmis)) or
    (Copy(NFSe.infNFSe.ID, 39, 8) <> IntToStrZero(NFSe.Ide.cNF, 8)));
*)
 Result := True;
end;

function NotaFiscal.GetConfirmada: Boolean;
begin
//  Result := TACBrNFSe(TNotasFiscais(Collection).ACBrNFSe).CstatConfirmada(
//    FNFSe.procNFSe.cStat);
end;

function NotaFiscal.GetProcessada: Boolean;
begin
//  Result := TACBrNFSe(TNotasFiscais(Collection).ACBrNFSe).CstatProcessado(
//    FNFSe.procNFSe.cStat);
end;

function NotaFiscal.GetMsg: String;
begin
//  Result := FNFSe.procNFSe.xMotivo;
end;

function NotaFiscal.GetNumID: String;
begin
  Result := Trim(OnlyNumber(NFSe.InfID.ID));
end;

function NotaFiscal.GetXMLAssinado: String;
begin
  if EstaVazio(FXMLAssinado) then
    Assinar;

  Result := FXMLAssinado;
end;

{ TNotasFiscais }

constructor TNotasFiscais.Create(AOwner: TPersistent; ItemClass: TCollectionItemClass);
begin
  if not (AOwner is TACBrNFSe) then
    raise EACBrNFSeException.Create('AOwner deve ser do tipo TACBrNFSe');

  inherited;

  FACBrNFSe := TACBrNFSe(AOwner);
  FConfiguracoes := TACBrNFSe(FACBrNFSe).Configuracoes;
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

procedure TNotasFiscais.GerarNFSe;
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

procedure TNotasFiscais.VerificarDANFSE;
begin
  if not Assigned(TACBrNFSe(FACBrNFSe).DANFSE) then
    raise EACBrNFSeException.Create('Componente DANFSE não associado.');
end;

procedure TNotasFiscais.Imprimir;
begin
  VerificarDANFSE;
  TACBrNFSe(FACBrNFSe).DANFSE.ImprimirDANFSE(nil);
end;

procedure TNotasFiscais.ImprimirPDF;
begin
  VerificarDANFSE;
  TACBrNFSe(FACBrNFSe).DANFSE.ImprimirDANFSEPDF(nil);
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

function TNotasFiscais.LoadFromFile(CaminhoArquivo: String;
  AGerarNFSe: Boolean = True): Boolean;
var
  ArquivoXML: TStringList;
  XML: String;
  XMLOriginal: AnsiString;
  i: integer;
begin
  Result := False;
  ArquivoXML := TStringList.Create;
  try
    ArquivoXML.LoadFromFile(CaminhoArquivo);
    XMLOriginal := ArquivoXML.Text;

    // Converte de UTF8 para a String nativa da IDE //
    XML := DecodeToString(XMLOriginal, True);
    LoadFromString(XML, AGerarNFSe);

    for i := 0 to Self.Count - 1 do
      Self.Items[i].NomeArq := CaminhoArquivo;

    Result := True;
  finally
    ArquivoXML.Free;
  end;
end;

function TNotasFiscais.LoadFromStream(AStream: TStringStream;
  AGerarNFSe: Boolean = True): Boolean;
var
  XMLOriginal: AnsiString;
begin
  Result := False;
  AStream.Position := 0;
  XMLOriginal := ReadStrFromStream(AStream, AStream.Size);

  Result := Self.LoadFromString(String(XMLOriginal), AGerarNFSe);
end;

function TNotasFiscais.LoadFromString(AXMLString: String;
  AGerarNFSe: Boolean = True): Boolean;
var
  AXML: AnsiString;
  P, N: integer;

  function PosNFSe: integer;
  begin
    Result := pos('</NFSe>', AXMLString);
  end;

begin
  Result := False;
  N := PosNFSe;
  while N > 0 do
  begin
    P := pos('</NFSeProc>', AXMLString);
    if P > 0 then
    begin
      AXML := copy(AXMLString, 1, P + 5);
      AXMLString := Trim(copy(AXMLString, P + 10, length(AXMLString)));
    end
    else
    begin
      AXML := copy(AXMLString, 1, N + 5);
      AXMLString := Trim(copy(AXMLString, N + 6, length(AXMLString)));
    end;

    with Self.Add do
    begin
      LerXML(AXML);

      if AGerarNFSe then // Recalcula o XML
        GerarXML;
    end;

    N := PosNFSe;
  end;
end;

function TNotasFiscais.GravarXML(PathNomeArquivo: String): Boolean;
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
