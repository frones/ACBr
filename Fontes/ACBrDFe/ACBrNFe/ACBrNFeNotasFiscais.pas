{******************************************************************************}
{ Projeto: Componente ACBrNFe                                                  }
{  Biblioteca multiplataforma de componentes Delphi para emissão de Nota Fiscal}
{ eletrônica - NFe - http://www.nfe.fazenda.gov.br                             }

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

{*******************************************************************************
|* Historico
|*
|* 16/12/2008: Wemerson Souto
|*  - Doação do componente para o Projeto ACBr
|* 25/07/2009: Gilson Carmo
|*  - Envio do e-mail utilizando Thread
|* 24/09/2012: Italo Jurisato Junior
|*  - Alterações para funcionamento com NFC-e
*******************************************************************************}

{$I ACBr.inc}

unit ACBrNFeNotasFiscais;

interface

uses
  Classes, SysUtils, Dialogs, StrUtils,
  ACBrNFeConfiguracoes, ACBrDFeUtil,
  pcnNFe, pcnNFeR, pcnNFeW, pcnConversao, pcnAuxiliar, pcnLeitor;

type

  { NotaFiscal }

  NotaFiscal = class(TCollectionItem)
  private
    FNFe: TNFe;
    FNFeW: TNFeW;
    FNFeR: TNFeR;

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
  public
    constructor Create(Collection2: TCollection); override;
    destructor Destroy; override;
    procedure Imprimir;
    procedure ImprimirPDF;

    procedure Assinar;
    procedure Validar;
    function VerificarAssinatura: Boolean;
    function ValidarRegrasdeNegocios: Boolean;

    function LerXML(AXML: AnsiString): Boolean;

    function GerarXML: String;
    function GravarXML(NomeArquivo: String = ''; PathArquivo: String = ''): Boolean;

    function GerarTXT: String;
    function GravarTXT(NomeArquivo: String = ''; PathArquivo: String = ''): Boolean;

    function GravarStream(AStream: TStream): Boolean;

    procedure EnviarEmail(sPara, sAssunto: String; sMensagem: TStrings = nil;
      EnviaPDF: Boolean = True; sCC: TStrings = nil; Anexos: TStrings = nil);

    property NomeArq: String read FNomeArq write FNomeArq;

    property NFe: TNFe read FNFe;

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
    FACBrNFe: TComponent;
    FConfiguracoes: TConfiguracoesNFe;

    function GetItem(Index: integer): NotaFiscal;
    procedure SetItem(Index: integer; const Value: NotaFiscal);

    procedure VerificarDANFE;
  public
    constructor Create(AOwner: TPersistent; ItemClass: TCollectionItemClass);

    procedure GerarNFe;
    procedure Assinar;
    procedure Validar;
    function VerificarAssinatura(out Erros: String): Boolean;
    function ValidarRegrasdeNegocios(out Erros: String): Boolean;
    procedure Imprimir;
    procedure ImprimirResumido;
    procedure ImprimirPDF;
    procedure ImprimirResumidoPDF;
    function Add: NotaFiscal;
    function Insert(Index: integer): NotaFiscal;

    property Items[Index: integer]: NotaFiscal read GetItem write SetItem; default;

    function GetNamePath: String; override;
    // Incluido o Parametro AGerarNFe que determina se após carregar os dados da NFe
    // para o componente, será gerado ou não novamente o XML da NFe.
    function LoadFromFile(CaminhoArquivo: String; AGerarNFe: Boolean = True): Boolean;
    function LoadFromStream(AStream: TStringStream; AGerarNFe: Boolean = True): Boolean;
    function LoadFromString(AXMLString: String; AGerarNFe: Boolean = True): Boolean;
    function GravarXML(PathNomeArquivo: String = ''): Boolean;
    function GravarTXT(PathNomeArquivo: String = ''): Boolean;

    property ACBrNFe: TComponent read FACBrNFe;
  end;

implementation

uses
  ACBrNFe, ACBrUtil, pcnConversaoNFe, synautil;

{ NotaFiscal }

constructor NotaFiscal.Create(Collection2: TCollection);
begin
  inherited Create(Collection2);
  FNFe := TNFe.Create;
  FNFeW := TNFeW.Create(FNFe);
  FNFeR := TNFeR.Create(FNFe);

  with TACBrNFe(TNotasFiscais(Collection).ACBrNFe) do
  begin
    FNFe.Ide.modelo := StrToInt(ModeloDFToStr(Configuracoes.Geral.ModeloDF));
    FNFe.infNFe.Versao := VersaoDFToDbl(Configuracoes.Geral.VersaoDF);

    FNFe.Ide.tpNF := tnSaida;
    FNFe.Ide.indPag := ipVista;
    FNFe.Ide.verProc := 'ACBrNFe2';
    FNFe.Ide.tpAmb := Configuracoes.WebServices.Ambiente;
    FNFe.Ide.tpEmis := Configuracoes.Geral.FormaEmissao;

    if Assigned(DANFE) then
      FNFe.Ide.tpImp := DANFE.TipoDANFE;

    FNFe.Emit.EnderEmit.xPais := 'BRASIL';
    FNFe.Emit.EnderEmit.cPais := 1058;
    FNFe.Emit.EnderEmit.nro := 'SEM NUMERO';
  end;
end;

destructor NotaFiscal.Destroy;
begin
  FNFeW.Free;
  FNFeR.Free;
  FNFe.Free;
  inherited Destroy;
end;

procedure NotaFiscal.Imprimir;
begin
  with TACBrNFe(TNotasFiscais(Collection).ACBrNFe) do
  begin
    if not Assigned(DANFE) then
      raise EACBrNFeException.Create('Componente DANFE não associado.')
    else
      DANFE.ImprimirDANFE(NFe);
  end;
end;

procedure NotaFiscal.ImprimirPDF;
begin
  with TACBrNFe(TNotasFiscais(Collection).ACBrNFe) do
  begin
    if not Assigned(DANFE) then
      raise EACBrNFeException.Create('Componente DANFE não associado.')
    else
      DANFE.ImprimirDANFEPDF(NFe);
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

  with TACBrNFe(TNotasFiscais(Collection).ACBrNFe) do
  begin
    XMLAss := SSL.Assinar(ArqXML, 'NFe', 'infNFe');
    FXMLAssinado := XMLAss;

    // Remove header, pois podem existir várias Notas no XML //
    //TODO: Verificar se precisa
    //XMLAss := StringReplace(XMLAss, '<' + ENCODING_UTF8_STD + '>', '', [rfReplaceAll]);
    //XMLAss := StringReplace(XMLAss, '<' + XML_V01 + '>', '', [rfReplaceAll]);

    Leitor := TLeitor.Create;
    try
      leitor.Grupo := XMLAss;
      NFe.signature.URI := Leitor.rAtributo('Reference URI=');
      NFe.signature.DigestValue := Leitor.rCampo(tcStr, 'DigestValue');
      NFe.signature.SignatureValue := Leitor.rCampo(tcStr, 'SignatureValue');
      NFe.signature.X509Certificate := Leitor.rCampo(tcStr, 'X509Certificate');
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
  ALayout: TLayOut;
begin
  AXML := FXMLAssinado;

  if EstaVazio(AXML) then
  begin
    Assinar;
    AXML := FXMLAssinado;
  end;

  with TACBrNFe(TNotasFiscais(Collection).ACBrNFe) do
  begin
    if EhAutorizacao then
      ALayout := LayNfeAutorizacao
    else
      ALayout := LayNfeRecepcao;

    NotaEhValida := SSL.Validar(AXML, GerarNomeArqSchema(ALayout, FNFe.infNFe.Versao), Erro);

    if not NotaEhValida then
    begin
      FErroValidacao := ACBrStr('Falha na validação dos dados da nota: ') +
        IntToStr(NFe.Ide.nNF) + sLineBreak + FAlertas ;
      FErroValidacaoCompleto := FErroValidacao + sLineBreak + Erro;

      raise EACBrNFeException.CreateDef(
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

  with TACBrNFe(TNotasFiscais(Collection).ACBrNFe) do
  begin
    AssEhValida := SSL.VerificarAssinatura(AXML, Erro);

    if not AssEhValida then
    begin
      FErroValidacao := ACBrStr('Falha na validação da assinatura da nota: ') +
        IntToStr(NFe.Ide.nNF) + sLineBreak + Erro;
    end;
  end;

  Result := AssEhValida;
end;

function NotaFiscal.ValidarRegrasdeNegocios: Boolean;
var
  Erros: String;
  I: Integer;
  fsvTotTrib, fsvBC, fsvICMS, fsvICMSDeson, fsvBCST, fsvST, fsvProd, fsvFrete : Currency;
  fsvSeg, fsvDesc, fsvII, fsvIPI, fsvPIS, fsvCOFINS, fsvOutro, fsvServ, fsvNF, fsvTotPag : Currency;
  FaturamentoDireto, NFImportacao : Boolean;

  procedure AdicionaErro(const Erro: String);
  begin
    Erros := Erros + Erro + sLineBreak;
  end;

begin
  with TACBrNFe(TNotasFiscais(Collection).ACBrNFe) do
  begin
    Erros := '';

    if NFe.infNFe.Versao < 3.10 then
      AdicionaErro('701-Rejeição: Versão inválida');

    if not ValidarConcatChave then  //A03-10
      AdicionaErro(
        '502-Rejeição: Erro na Chave de Acesso - Campo Id não corresponde à concatenação dos campos correspondentes');

    if copy(IntToStr(NFe.Emit.EnderEmit.cMun), 1, 2) <>
      IntToStr(Configuracoes.WebServices.UFCodigo) then //B02-10
      AdicionaErro('226-Rejeição: Código da UF do Emitente diverge da UF autorizadora');

//    702-Rejeição: NFC-e não é aceita pela UF do Emitente

    if (NFe.Ide.serie > 899) and  //B07-20
      (NFe.Ide.tpEmis <> teSCAN) then
      AdicionaErro('503-Rejeição: Série utilizada fora da faixa permitida no SCAN (900-999)');

    if (NFe.Ide.dEmi > now) then  //B09-10
      AdicionaErro('703-Rejeição: Data-Hora de Emissão posterior ao horário de recebimento');

    if ((now - NFe.Ide.dEmi) > 30) then  //B09-20
      AdicionaErro('228-Rejeição: Data de Emissão muito atrasada');

    //GB09.02 - Data de Emissão posterior à 31/03/2011
    //GB09.03 - Data de Recepção posterior à 31/03/2011 e tpAmb (B24) = 2

    if not ValidarChave(NFe.infNFe.ID) then
      AdicionaErro('253-Rejeição: Digito Verificador da chave de acesso composta inválida');

    if not ValidarMunicipio(NFe.Ide.cMunFG) then //B12-10
      AdicionaErro('270-Rejeição: Código Município do Fato Gerador: dígito inválido');

    if (UFparaCodigo(NFe.Emit.EnderEmit.UF) <> StrToIntDef(
      copy(IntToStr(NFe.Ide.cMunFG), 1, 2), 0)) then//GB12.1
      AdicionaErro('271-Rejeição: Código Município do Fato Gerador: difere da UF do emitente');

    if ((NFe.Ide.tpEmis in [teSCAN, teSVCAN, teSVCRS]) and
      (Configuracoes.Geral.FormaEmissao = teNormal)) then  //B22-30
      AdicionaErro(
        '570-Rejeição: Tipo de Emissão 3, 6 ou 7 só é válido nas contingências SCAN/SVC');

    if ((NFe.Ide.tpEmis <> teSCAN) and (Configuracoes.Geral.FormaEmissao = teSCAN))
    then  //B22-40
      AdicionaErro('571-Rejeição: Tipo de Emissão informado diferente de 3 para contingência SCAN');

    if ((Configuracoes.Geral.FormaEmissao in [teSVCAN, teSVCRS]) and
      (not (NFe.Ide.tpEmis in [teSVCAN, teSVCRS]))) then  //B22-60
      AdicionaErro('713-Rejeição: Tipo de Emissão diferente de 6 ou 7 para contingência da SVC acessada');

    //B23-10
    if (NFe.Ide.tpAmb <> Configuracoes.WebServices.Ambiente) then
      //B24-10
      AdicionaErro('252-Rejeição: Ambiente informado diverge do Ambiente de recebimento '
        + '(Tipo do ambiente da NF-e difere do ambiente do Web Service)');

    if (not (NFe.Ide.procEmi in [peAvulsaFisco, peAvulsaContribuinte])) and
      (NFe.Ide.serie > 889) then //B26-10
      AdicionaErro('266-Rejeição: Série utilizada fora da faixa permitida no Web Service (0-889)');

    if (NFe.Ide.procEmi in [peAvulsaFisco, peAvulsaContribuinte]) and
      (NFe.Ide.serie < 890) and (NFe.Ide.serie > 899) then
      //B26-20
      AdicionaErro('451-Rejeição: Processo de emissão informado inválido');

    if (NFe.Ide.procEmi in [peAvulsaFisco, peAvulsaContribuinte]) and
      (NFe.Ide.tpEmis <> teNormal) then //B26-30
      AdicionaErro('370-Rejeição: Nota Fiscal Avulsa com tipo de emissão inválido');

    if (NFe.Ide.tpEmis = teNormal) and ((NFe.Ide.xJust > '') or
      (NFe.Ide.dhCont <> 0)) then
      //B28-10
      AdicionaErro(
        '556-Justificativa de entrada em contingência não deve ser informada para tipo de emissão normal');

    if (NFe.Ide.tpEmis in [teContingencia, teFSDA, teOffLine]) and
      (NFe.Ide.xJust = '') then //B28-20
      AdicionaErro('557-A Justificativa de entrada em contingência deve ser informada');

    if (NFe.Ide.dhCont > now) then //B28-30
      AdicionaErro('558-Rejeição: Data de entrada em contingência posterior a data de recebimento');

    if (NFe.Ide.dhCont > 0) and ((now - NFe.Ide.dhCont) > 30) then //B28-40
      AdicionaErro('559-Rejeição: Data de entrada em contingência muito atrasada');

    if not ValidarCNPJ(NFe.Emit.CNPJCPF) then
      AdicionaErro('207-Rejeição: CNPJ do emitente inválido');

    if not ValidarMunicipio(NFe.Emit.EnderEmit.cMun) then
      AdicionaErro('272-Rejeição: Código Município do Emitente: dígito inválido');

    if (UFparaCodigo(NFe.Emit.EnderEmit.UF) <> StrToIntDef(
      copy(IntToStr(NFe.Emit.EnderEmit.cMun), 1, 2), 0)) then
      AdicionaErro('273-Rejeição: Código Município do Emitente: difere da UF do emitente');

    if EstaVazio(NFe.Emit.IE) then
      AdicionaErro('229-Rejeição: IE do emitente não informada');

    if not ValidarIE(NFe.Emit.IE,NFe.Emit.EnderEmit.UF) then
      AdicionaErro('209-Rejeição: IE do emitente inválida ');

    if (Length(Trim(OnlyNumber(NFe.Dest.CNPJCPF))) >= 14) and
      not ValidarCNPJ(NFe.Dest.CNPJCPF) then
      AdicionaErro('208-Rejeição: CNPJ do emitente inválido');

    if (NFe.Retirada.UF = 'EX') and
       (NFe.Retirada.cMun <> 9999999) then
      AdicionaErro('513-Rejeição: Código Município do Local de Retirada deve ser 9999999 para UF retirada = "EX"');

    if (NFe.Retirada.UF <> 'EX') and
       NaoEstaVazio(NFe.Retirada.xMun) and
       not ValidarMunicipio(NFe.Retirada.cMun) then
      AdicionaErro('276-Rejeição: Código Município do Local de Retirada: dígito inválido');

    if (UFparaCodigo(NFe.Retirada.UF) <> StrToIntDef(
      copy(IntToStr(NFe.Retirada.cMun), 1, 2), 0)) then
      AdicionaErro('277-Rejeição: Código Município do Local de Retirada: difere da UF do Local de Retirada');

    if (NFe.Entrega.UF = 'EX') and
       (NFe.Entrega.cMun <> 9999999) then
      AdicionaErro('515-Rejeição: Código Município do Local de Entrega deve ser 9999999 para UF entrega = "EX"');

    if (NFe.Entrega.UF <> 'EX') and
       NaoEstaVazio(NFe.Entrega.xMun) and
       not ValidarMunicipio(NFe.Entrega.cMun) then
      AdicionaErro('278-Rejeição: Código Município do Local de Entrega: dígito inválido');

    if (UFparaCodigo(NFe.Entrega.UF) <> StrToIntDef(
      copy(IntToStr(NFe.Entrega.cMun), 1, 2), 0)) then
      AdicionaErro('279-Rejeição: Código Município do Local de Entrega: difere da UF do Local de Entrega');

    if NaoEstaVazio(Trim(NFe.Transp.Transporta.CNPJCPF)) and
       (Length(Trim(OnlyNumber(NFe.Transp.Transporta.CNPJCPF))) >= 14) and
       not ValidarCNPJ(NFe.Transp.Transporta.CNPJCPF) then
      AdicionaErro('542-Rejeição: CNPJ do Transportador inválido');

    if NaoEstaVazio(Trim(NFe.Transp.Transporta.CNPJCPF)) and
       (Length(Trim(OnlyNumber(NFe.Transp.Transporta.CNPJCPF))) <= 11) and
       not ValidarCPF(NFe.Transp.Transporta.CNPJCPF) then
      AdicionaErro('543-Rejeição: CPF do Transportador inválido');

    if NaoEstaVazio(Trim(NFe.Transp.Transporta.IE)) and
       EstaVazio(Trim(NFe.Transp.Transporta.UF)) then
      AdicionaErro('559-Rejeição: UF do Transportador não informada');

    if NaoEstaVazio(Trim(NFe.Transp.Transporta.IE)) and
       not ValidarIE(NFe.Transp.Transporta.IE,NFe.Transp.Transporta.UF) then
      AdicionaErro('544-Rejeição: IE do Transportador inválida');

    if (NFe.Ide.modelo = 65) then  //Regras válidas apenas para NFC-e - 65
    begin
      if (NFe.Ide.dEmi < now - StrToTime('00:05:00')) and
        (NFe.Ide.tpEmis in [teNormal, teSCAN, teSVCAN, teSVCRS]) then
        //B09-40
        AdicionaErro('704-Rejeição: NFC-e com Data-Hora de emissão atrasada');

      if (NFe.Ide.dSaiEnt <> 0) then  //B10-10
        AdicionaErro('705-Rejeição: NFC-e com data de entrada/saída');

      if (NFe.Ide.tpNF = tnEntrada) then  //B11-10
        AdicionaErro('706-Rejeição: NFC-e para operação de entrada');

      if (NFe.Ide.idDest <> doInterna) then  //B11-10
        AdicionaErro('707-NFC-e para operação interestadual ou com o exterior');

      if (not (NFe.Ide.tpImp in [tiNFCe, tiNFCeA4, tiMsgEletronica])) then
        //B21-10
        AdicionaErro('709-Rejeição: NFC-e com formato de DANFE inválido');

      if (NFe.Ide.tpEmis = teOffLine) and
        (AnsiIndexStr(NFe.Emit.EnderEmit.UF, ['SP']) <> -1) then  //B22-20
        AdicionaErro('712-Rejeição: NF-e com contingência off-line');

      if (NFe.Ide.tpEmis = teSCAN) then //B22-50
        AdicionaErro('782-Rejeição: NFC-e não é autorizada pelo SCAN');

      if (NFe.Ide.tpEmis in [teSVCAN, teSVCRS]) then  //B22-70
        AdicionaErro('783-Rejeição: NFC-e não é autorizada pela SVC');

      if (NFe.Ide.finNFe <> fnNormal) then  //B25-20
        AdicionaErro('715-Rejeição: Rejeição: NFC-e com finalidade inválida');

      if (NFe.Ide.indFinal = cfNao) then //B25a-10
        AdicionaErro('716-Rejeição: NFC-e em operação não destinada a consumidor final');

      if (not (NFe.Ide.indPres in [pcPresencial, pcEntregaDomicilio])) then
        //B25b-20
        AdicionaErro('717-Rejeição: NFC-e em operação não presencial');

      if (NFe.Ide.indPres = pcEntregaDomicilio) and
        (AnsiIndexStr(NFe.Emit.EnderEmit.UF, ['XX']) <> -1) then
        //B25b-30  Qual estado não permite entrega a domicílio?
        AdicionaErro('785-Rejeição: NFC-e com entrega a domicílio não permitida pela UF');

      if (NFe.Ide.NFref.Count > 0) then
        AdicionaErro('708-Rejeição: NFC-e não pode referenciar documento fiscal');

      if NaoEstaVazio(Trim(NFe.Emit.IEST)) then
        AdicionaErro('718-Rejeição: NFC-e não deve informar IE de Substituto Tributário');

      if (NFe.Ide.indPres = pcEntregaDomicilio) and
        EstaVazio(Trim(nfe.Entrega.xLgr)) then
        AdicionaErro('787-Rejeição: NFC-e de entrega a domicílio sem a identificação do destinatário');

      if (NFe.Dest.indIEDest <> inNaoContribuinte) then
        AdicionaErro('789-Rejeição: NFC-e para destinatário contribuinte de ICMS');

      if NaoEstaVazio(Trim(NFe.Dest.IE)) then
        AdicionaErro('729-Rejeição: NFC-e com informação da IE do destinatário');

      if NaoEstaVazio(Trim(NFe.Dest.ISUF)) then
        AdicionaErro('730-Rejeição: NFC-e com Inscrição Suframa');

      if (NFe.Transp.modFrete <> mfSemFrete) and
         (NFe.Ide.indPres <> pcEntregaDomicilio)then
        AdicionaErro('753-Rejeição: NFC-e com Frete');

      if (NFe.Ide.indPres <> pcEntregaDomicilio) and
         ((trim(NFe.Transp.Transporta.CNPJCPF) <> '') or
         (trim(NFe.Transp.Transporta.xNome) <> '') or
         (trim(NFe.Transp.Transporta.IE) <> '') or
         (trim(NFe.Transp.Transporta.xEnder) <> '') or
         (trim(NFe.Transp.Transporta.xMun) <> '') or
         (trim(NFe.Transp.Transporta.UF) <> '')) then
        AdicionaErro('754-Rejeição: NFC-e com dados do Transportador');

      if (NFe.Ide.indPres = pcEntregaDomicilio) and
         ((trim(NFe.Transp.Transporta.CNPJCPF) = '') or
         (trim(NFe.Transp.Transporta.xNome) = '')) then
        AdicionaErro('786-Rejeição: NFC-e de entrega a domicílio sem dados do Transportador');

      if (NFe.Transp.retTransp.vServ > 0) or
         (NFe.Transp.retTransp.vBCRet > 0) or
         (NFe.Transp.retTransp.pICMSRet > 0) or
         (NFe.Transp.retTransp.vICMSRet > 0) or
         (Trim(NFe.Transp.retTransp.CFOP) <> '') or
         (NFe.Transp.retTransp.cMunFG > 0) then
        AdicionaErro('755-Rejeição: NFC-e com dados de Retenção do ICMS no Transporte');

      if (Trim(NFe.Transp.veicTransp.placa) <> '') or
         (Trim(NFe.Transp.veicTransp.UF) <> '') or
         (Trim(NFe.Transp.veicTransp.RNTC) <> '') then
        AdicionaErro('756-Rejeição: NFC-e com dados do veículo de Transporte');

      if NFe.Transp.Reboque.Count > 0 then
        AdicionaErro('757-Rejeição: NFC-e com dados de Reboque do veículo de Transporte');

      if NaoEstaVazio(Trim(NFe.Transp.vagao)) then
        AdicionaErro('758-Rejeição: NFC-e com dados do Vagão de Transporte');

      if NaoEstaVazio(Trim(NFe.Transp.balsa)) then
        AdicionaErro('759-Rejeição: NFC-e com dados da Balsa de Transporte');

      if (Trim(nfe.Cobr.Fat.nFat) <> '') or
         (NFe.Cobr.Fat.vOrig > 0) or
         (NFe.Cobr.Fat.vDesc > 0) or
         (NFe.Cobr.Fat.vLiq > 0) or
         (NFe.Cobr.Dup.Count > 0) then
        AdicionaErro('760-Rejeição: NFC-e com dados de cobrança (Fatura, Duplicata)');

      if NFe.pag.Count <= 0 then
        AdicionaErro('769-Rejeição: NFC-e deve possuir o grupo de Formas de Pagamento');

      if Trim(NFe.compra.xNEmp) + Trim(NFe.compra.xPed) + Trim(NFe.compra.xCont) <> '' then
        AdicionaErro('762-Rejeição: NFC-e com dados de compras (Empenho, Pedido, Contrato)');

      if not(Trim(NFe.cana.safra) = '') or not(Trim(NFe.cana.ref) = '') or
         (NFe.cana.fordia.Count > 0) or (NFe.cana.deduc.Count > 0) then
        AdicionaErro('763-Rejeição: NFC-e com dados de aquisição de Cana');

    end;

    if (NFe.Ide.modelo = 55) then  //Regras válidas apenas para NF-e - 55
    begin
      if ((NFe.Ide.dSaiEnt - now) > 30) then  //B10-20  - Facultativo
        AdicionaErro('504-Rejeição: Data de Entrada/Saída posterior ao permitido');

      if ((now - NFe.Ide.dSaiEnt) > 30) then  //B10-30  - Facultativo
        AdicionaErro('505-Rejeição: Data de Entrada/Saída anterior ao permitido');

      if (NFe.Ide.dSaiEnt < NFe.Ide.dEmi) then
        //B10-40  - Facultativo
        AdicionaErro('506-Rejeição: Data de Saída menor que a Data de Emissão');

      if (NFe.Ide.tpImp in [tiNFCe, tiMsgEletronica]) then  //B21-20
        AdicionaErro('710-Rejeição: NF-e com formato de DANFE inválido');

      if (NFe.Ide.tpEmis = teOffLine) then  //B22-10
        AdicionaErro('711-Rejeição: NF-e com contingência off-line');

      if (NFe.Ide.finNFe = fnComplementar) and (NFe.Ide.NFref.Count = 0) then  //B25-30
        AdicionaErro('254-Rejeição: NF-e complementar não possui NF referenciada');

      if (NFe.Ide.finNFe = fnComplementar) and (NFe.Ide.NFref.Count > 1) then  //B25-40
        AdicionaErro('255-Rejeição: NF-e complementar possui mais de uma NF referenciada');

      if (NFe.Ide.finNFe = fnComplementar) and (NFe.Ide.NFref.Count = 1) and
        (((NFe.Ide.NFref.Items[0].RefNF.CNPJ > '') and
        (NFe.Ide.NFref.Items[0].RefNF.CNPJ <> NFe.Emit.CNPJCPF)) or
        ((NFe.Ide.NFref.Items[0].RefNFP.CNPJCPF > '') and
        (NFe.Ide.NFref.Items[0].RefNFP.CNPJCPF <> NFe.Emit.CNPJCPF))) then
        //B25-50
        AdicionaErro(
          '269-Rejeição: CNPJ Emitente da NF Complementar difere do CNPJ da NF Referenciada');

      if (NFe.Ide.finNFe = fnComplementar) and (NFe.Ide.NFref.Count = 1) and
        //Testa pelo número para saber se TAG foi preenchida
        (((NFe.Ide.NFref.Items[0].RefNF.nNF > 0) and
        (NFe.Ide.NFref.Items[0].RefNF.cUF <> UFparaCodigo(
        NFe.Emit.EnderEmit.UF))) or ((NFe.Ide.NFref.Items[0].RefNFP.nNF > 0) and
        (NFe.Ide.NFref.Items[0].RefNFP.cUF <> UFparaCodigo(
        NFe.Emit.EnderEmit.UF))))
      then  //B25-60 - Facultativo
        AdicionaErro('678-Rejeição: NF referenciada com UF diferente da NF-e complementar');

      if (NFe.Ide.finNFe = fnDevolucao) and (NFe.Ide.NFref.Count = 0) then
        //B25-70
        AdicionaErro('321-Rejeição: NF-e devolução não possui NF referenciada');

      if (NFe.Ide.finNFe = fnDevolucao) and (NFe.Ide.NFref.Count > 1) then
        //B25-80
        AdicionaErro('322-Rejeição: NF-e devolução possui mais de uma NF referenciada');

      if (NFe.Ide.indPres = pcEntregaDomicilio) then //B25b-10
        AdicionaErro('794-Rejeição: NF-e com indicativo de NFC-e com entrega a domicílio');

      if (NFe.Dest.CNPJCPF <> '') or
         (NFe.Dest.idEstrangeiro <> '') then
        AdicionaErro('719-Rejeição: NF-e sem a identificação do destinatário');

      if (Length(Trim(OnlyNumber(NFe.Dest.CNPJCPF))) <= 11) and
        not ValidarCPF(NFe.Dest.CNPJCPF) then
        AdicionaErro('237-Rejeição: CPF do destinatário inválido');

      if (nfe.Ide.idDest = doExterior) and
         (EstaVazio(Trim(NFe.Dest.idEstrangeiro))) then
        AdicionaErro('720-Rejeição: Na operação com Exterior deve ser informada tag idEstrangeiro');

      if (nfe.Ide.idDest = doInterestadual) and
         (EstaVazio(Trim(NFe.Dest.CNPJCPF))) then
        AdicionaErro('721-Rejeição: Operação interestadual deve informar CNPJ ou CPF');

      if (nfe.Ide.idDest = doInterna) and
         (NaoEstaVazio(Trim(NFe.Dest.idEstrangeiro))) and
         (NFe.Ide.indFinal <> cfConsumidorFinal)then
        AdicionaErro('723-Rejeição: Operação interna com idEstrangeiro informado deve ser para consumidor final');

      if EstaVazio(Trim(NFe.Dest.xNome)) then
        AdicionaErro('724-Rejeição: NF-e sem o nome do destinatário');

      if EstaVazio(Trim(NFe.Dest.EnderDest.xLgr)) then
        AdicionaErro('726-Rejeição: NF-e sem a informação de endereço do destinatário');

      if (NFe.Dest.EnderDest.UF <> 'EX') and
         not ValidarMunicipio(NFe.Dest.EnderDest.cMun) then
        AdicionaErro('509-Rejeição: Informado código de município diferente de "9999999" para operação com o exterior');

      if (nfe.Ide.idDest = doExterior) and
         (NFe.Dest.EnderDest.UF <> 'EX') then
        AdicionaErro('727-Rejeição: Operação com Exterior e UF diferente de EX');

      if (nfe.Ide.idDest = doInterestadual) and
         (NFe.Dest.EnderDest.UF = 'EX') then
        AdicionaErro('771-Rejeição: Operação Interestadual e UF de destino com EX');

      if (nfe.Ide.idDest = doInterestadual) and
         (NFe.Dest.EnderDest.UF = NFe.Emit.EnderEmit.UF) then
        AdicionaErro('772-Rejeição: Operação Interestadual e UF de destino igual à UF do emitente');

      if (nfe.Ide.idDest = doInterna) and
         (NFe.Dest.EnderDest.UF <> NFe.Emit.EnderEmit.UF) then
        AdicionaErro('773-Rejeição: Operação Interna e UF de destino difere da UF do emitente');

      if (NFe.Ide.idDest = doExterior) and
         (NFe.Dest.indIEDest <> inNaoContribuinte) then
        AdicionaErro('790-Rejeição: Operação com Exterior para destinatário Contribuinte de ICMS');

      if NFe.pag.Count > 0 then
        AdicionaErro('768-Rejeição: NF-e não deve possuir o grupo de Formas de Pagamento');
    end;

    for I:=0 to NFe.autXML.Count-1 do
    begin
      if (Length(Trim(OnlyNumber(NFe.autXML[I].CNPJCPF))) <= 11) and
        not ValidarCPF(NFe.autXML[I].CNPJCPF) then
        AdicionaErro('325-Rejeição: CPF autorizado para download inválido');

      if (Length(Trim(OnlyNumber(NFe.autXML[I].CNPJCPF))) > 11) and
        not ValidarCNPJ(NFe.autXML[I].CNPJCPF) then
        AdicionaErro('323-Rejeição: CNPJ autorizado para download inválido');
    end;

    fsvTotTrib := 0;
    fsvBC      := 0;
    fsvICMS    := 0;
    fsvICMSDeson    := 0;
    fsvBCST    := 0;
    fsvST      := 0;
    fsvProd    := 0;
    fsvFrete   := 0;
    fsvSeg     := 0;
    fsvDesc    := 0;
    fsvII      := 0;
    fsvIPI     := 0;
    fsvPIS     := 0;
    fsvCOFINS  := 0;
    fsvOutro   := 0;
    fsvServ    := 0;
    FaturamentoDireto := False;
    NFImportacao := False;


    for I:=0 to NFe.Det.Count-1 do
    begin
      if Length(Trim(NFe.Det[I].Prod.NCM)) < 8 then
        AdicionaErro('777-Rejeição: Obrigatória a informação do NCM completo');

      if (NFe.Ide.modelo = 65) then
      begin
        if (pos(OnlyNumber(NFe.Det[I].Prod.CFOP), 'XXXX,5101,5102,5103,5104,5115,5401,5403,5405,5653,5656,5667,5933') <= 0)  then
          AdicionaErro('725-Rejeição: NFC-e com CFOP inválido');

        if (NFe.Det[I].Prod.IndTot = itNaoSomaTotalNFe) then
          AdicionaErro('774-Rejeição: NFC-e com indicador de item não participante do total');

        if (NaoEstaVazio(NFe.Det[I].Prod.veicProd.chassi)) then
          AdicionaErro('736-Rejeição: NFC-e com grupo de Veículos novos');

        if (NFe.Det[I].Prod.med.Count > 0) then
          AdicionaErro('737-Rejeição: NFC-e com grupo de Medicamentos');

        if (NFe.Det[I].Prod.arma.Count > 0) then
          AdicionaErro('738-Rejeição: NFC-e com grupo de Armamentos');

        if (NaoEstaVazio(NFe.Det[I].Prod.comb.UFcons)) then
          AdicionaErro('739-Rejeição: NFC-e com grupo de Combustível');

        if (NaoEstaVazio(NFe.Det[I].Prod.nRECOPI)) then
          AdicionaErro('348-Rejeição: NFC-e com grupo RECOPI');

        if (NFe.Det[I].Imposto.ICMS.CST = cst50) then
          AdicionaErro('766-Rejeição: NFC-e com CST 50-Suspensão');

        if (NFe.Det[I].Imposto.ICMS.CST = cst51) then
          AdicionaErro('740-Rejeição: NFC-e com CST 51-Diferimento');

        if (NFe.Det[I].Imposto.ICMS.CST in [cstPart10,cstPart90]) then
          AdicionaErro('741-Rejeição: NFC-e com Partilha de ICMS entre UF');

        if ((NFe.Det[I].Imposto.IPI.cEnq  <> '') or
            (NFe.Det[I].Imposto.IPI.vBC   <> 0) or
            (NFe.Det[I].Imposto.IPI.qUnid <> 0) or
            (NFe.Det[I].Imposto.IPI.vUnid <> 0) or
            (NFe.Det[I].Imposto.IPI.pIPI  <> 0) or
            (NFe.Det[I].Imposto.IPI.vIPI  <> 0)) then
          AdicionaErro('742-Rejeição: NFC-e com grupo do IPI');

        if (NFe.Det[I].Imposto.II.vBc > 0) or
           (NFe.Det[I].Imposto.II.vDespAdu > 0) or
           (NFe.Det[I].Imposto.II.vII > 0) or
           (NFe.Det[I].Imposto.II.vIOF > 0) or
           (Copy(NFe.Det[I].Prod.CFOP,1,1) = '3') then
          AdicionaErro('743-Rejeição: NFC-e com grupo do II');

        if (NFe.Det[I].Imposto.PISST.vBc > 0) or
           (NFe.Det[I].Imposto.PISST.pPis > 0) or
           (NFe.Det[I].Imposto.PISST.qBCProd > 0) or
           (NFe.Det[I].Imposto.PISST.vAliqProd > 0) or
           (NFe.Det[I].Imposto.PISST.vPIS > 0) then
         AdicionaErro('746-Rejeição: NFC-e com grupo do PIS-ST');

        if (NFe.Det[I].Imposto.COFINSST.vBC > 0) or
           (NFe.Det[I].Imposto.COFINSST.pCOFINS > 0) or
           (NFe.Det[I].Imposto.COFINSST.qBCProd > 0) or
           (NFe.Det[I].Imposto.COFINSST.vAliqProd > 0) or
           (NFe.Det[I].Imposto.COFINSST.vCOFINS > 0) then
          AdicionaErro('749-Rejeição: NFC-e com grupo da COFINS-ST');

      end;

      if (NFe.Det[I].Imposto.ICMS.CST in [cst00,cst10,cst20,cst70]) and
         (NFe.Ide.finNFe = fnNormal) and
         (NFe.Det[I].Imposto.ICMS.vICMS <> (RoundABNT(NFe.Det[I].Imposto.ICMS.vBC * (NFe.Det[I].Imposto.ICMS.pICMS/100),-2)))then
        AdicionaErro('528-Rejeição: Valor do ICMS difere do produto BC e Alíquota');

      if (NFe.Det[I].Imposto.ICMS.motDesICMS = mdiSuframa) and
         (EstaVazio(NFe.Dest.ISUF))then
        AdicionaErro('625-Rejeição: Inscrição SUFRAMA deve ser informada na venda com isenção para ZFM');

      if EstaVazio(NFe.Emit.IM) and
        ((NFe.Det[I].Imposto.ISSQN.vBC > 0) or
         (NFe.Det[I].Imposto.ISSQN.vAliq > 0) or
         (NFe.Det[I].Imposto.ISSQN.vISSQN > 0) or
         (NFe.Det[I].Imposto.ISSQN.cMunFG > 0) or
         (NFe.Det[I].Imposto.ISSQN.cListServ <> '')) then
        AdicionaErro('530-Rejeição: Operação com tributação de ISSQN sem informar a Inscrição Municipal');

      if (NFe.Det[I].Imposto.ISSQN.cMunFG > 0) and
         not ValidarMunicipio(NFe.Det[I].Imposto.ISSQN.cMunFG) then
        AdicionaErro('287-Rejeição: Código Município do FG - ISSQN: dígito inválido');

      if NFe.Det.Items[I].Prod.IndTot = itSomaTotalNFe then
      begin
        fsvTotTrib := fsvTotTrib + NFe.Det.Items[I].Imposto.vTotTrib;
        fsvBC      := fsvBC + NFe.Det.Items[I].Imposto.ICMS.vBC;
        fsvICMS    := fsvICMS + NFe.Det.Items[I].Imposto.ICMS.vICMS;
        fsvICMSDeson := fsvICMSDeson + NFe.Det.Items[I].Imposto.ICMS.vICMSDeson;
        fsvBCST    := fsvBCST + NFe.Det.Items[I].Imposto.ICMS.vBCST;
        fsvST      := fsvST + NFe.Det.Items[I].Imposto.ICMS.vICMSST;
        fsvProd    := fsvProd + NFe.Det.Items[I].Prod.vProd;
        fsvFrete   := fsvFrete + NFe.Det.Items[I].Prod.vFrete;
        fsvSeg     := fsvSeg + NFe.Det.Items[I].Prod.vSeg;
        fsvDesc    := fsvDesc + NFe.Det.Items[I].Prod.vDesc;
        fsvII      := fsvII + NFe.Det.Items[I].Imposto.II.vII;
        fsvIPI     := fsvIPI + NFe.Det.Items[I].Imposto.IPI.vIPI;
        fsvPIS     := fsvPIS + NFe.Det.Items[I].Imposto.PIS.vPIS;
        fsvCOFINS  := fsvCOFINS + NFe.Det.Items[I].Imposto.COFINS.vCOFINS;
        fsvOutro   := fsvOutro + NFe.Det.Items[I].Prod.vOutro;
        fsvServ   := fsvServ + NFe.Det.Items[I].Imposto.ISSQN.vBC; //VERIFICAR
      end;

      if NFe.Det[I].Prod.veicProd.tpOP = toFaturamentoDireto then
        FaturamentoDireto := True;

      if Copy(NFe.Det[I].Prod.CFOP,1,1) = '3'then
        NFImportacao := True;
    end;

    if FaturamentoDireto then
      fsvNF := (fsvProd+fsvFrete+fsvSeg+fsvOutro+fsvII+fsvIPI+fsvServ)-(fsvDesc+fsvICMSDeson)
    else
      fsvNF := (fsvProd+fsvST+fsvFrete+fsvSeg+fsvOutro+fsvII+fsvIPI+fsvServ)-(fsvDesc+fsvICMSDeson);

    if (NFe.Total.ICMSTot.vBC <> fsvBC) then
      AdicionaErro('531-Rejeição: Total da BC ICMS difere do somatório dos itens');

    if (NFe.Total.ICMSTot.vICMS <> fsvICMS) then
      AdicionaErro('532-Rejeição: Total do ICMS difere do somatório dos itens');

    if (NFe.Total.ICMSTot.vICMSDeson <> fsvICMSDeson) then
      AdicionaErro('795-Rejeição: Total do ICMS desonerado difere do somatório dos itens');

    if (NFe.Total.ICMSTot.vBCST <> fsvBCST) then
      AdicionaErro('533-Rejeição: Total da BC ICMS-ST difere do somatório dos itens');

    if (NFe.Total.ICMSTot.vST <> fsvST) then
      AdicionaErro('534-Rejeição: Total do ICMS-ST difere do somatório dos itens');

    if (NFe.Total.ICMSTot.vProd <> fsvProd) then
      AdicionaErro('564-Rejeição: Total do Produto / Serviço difere do somatório dos itens');

    if (NFe.Total.ICMSTot.vFrete <> fsvFrete) then
      AdicionaErro('535-Rejeição: Total do Frete difere do somatório dos itens');

    if (NFe.Total.ICMSTot.vSeg <> fsvSeg) then
      AdicionaErro('536-Rejeição: Total do Seguro difere do somatório dos itens');

    if (NFe.Total.ICMSTot.vDesc <> fsvDesc) then
      AdicionaErro('537-Rejeição: Total do Desconto difere do somatório dos itens');

    if (NFe.Total.ICMSTot.vII <> fsvII) then
      AdicionaErro('601-Rejeição: Total do II difere do somatório dos itens');

    if (NFe.Total.ICMSTot.vIPI <> fsvIPI) then
      AdicionaErro('538-Rejeição: Total do IPI difere do somatório dos itens');

    if (NFe.Total.ICMSTot.vPIS <> fsvPIS) then
      AdicionaErro('602-Rejeição: Total do PIS difere do somatório dos itens sujeitos ao ICMS');

    if (NFe.Total.ICMSTot.vCOFINS <> fsvCOFINS) then
      AdicionaErro('603-Rejeição: Total da COFINS difere do somatório dos itens sujeitos ao ICMS');

    if (NFe.Total.ICMSTot.vOutro <> fsvOutro) then
      AdicionaErro('604-Rejeição: Total do vOutro difere do somatório dos itens');

    if not NFImportacao and
       (NFe.Total.ICMSTot.vNF <> fsvNF) then
    begin
      if (NFe.Total.ICMSTot.vNF <> (fsvNF+fsvICMSDeson)) then
        AdicionaErro('610-Rejeição: Total da NF difere do somatório dos Valores compõe o valor Total da NF.');
    end;

    if (NFe.Total.ICMSTot.vTotTrib <> fsvTotTrib) then
      AdicionaErro('685-Rejeição: Total do Valor Aproximado dos Tributos difere do somatório dos itens');

    if NFe.Ide.modelo = 65 then
    begin
      fsvTotPag := 0;
      for I:=0 to NFe.pag.Count-1 do
      begin
        fsvTotPag :=  fsvTotPag + NFe.pag[I].vPag;
      end;

      if (NFe.Total.ICMSTot.vNF <> fsvTotPag) then
        AdicionaErro('767-Rejeição: NFC-e com somatório dos pagamentos diferente do total da Nota Fiscal');
    end;


    //TODO: Regrar W01. Total da NF-e / ISSQN

  end;

  Result := EstaVazio(Erros);

  if not Result then
  begin
    Erros := ACBrStr('Erro(s) nas Regras de negócios da nota: '+
                     IntToStr(NFe.Ide.nNF) + sLineBreak +
                     Erros);
  end;

  FErroRegrasdeNegocios := Erros;
end;

function NotaFiscal.LerXML(AXML: AnsiString): Boolean;
//var
//  Ok: Boolean;
begin
  Result := False;
  FNFeR.Leitor.Arquivo := AXML;
  FNFeR.LerXml;

  FXML := string(AXML);
  FXMLOriginal := FXML;
  Result := True;
end;

function NotaFiscal.GravarXML(NomeArquivo: String; PathArquivo: String): Boolean;
begin
  FNomeArq := CalcularNomeArquivoCompleto(NomeArquivo, PathArquivo);
  GerarXML;
  Result := TACBrNFe(TNotasFiscais(Collection).ACBrNFe).Gravar(FNomeArq, FXML);
end;

function NotaFiscal.GravarTXT(NomeArquivo: String; PathArquivo: String): Boolean;
var
  ATXT: String;
begin
  FNomeArq := CalcularNomeArquivoCompleto(NomeArquivo, PathArquivo);
  ATXT := GerarTXT;
  Result := TACBrNFe(TNotasFiscais(Collection).ACBrNFe).Gravar(
    ChangeFileExt(FNomeArq, '.txt'), ATXT);
end;

function NotaFiscal.GravarStream(AStream: TStream): Boolean;
begin
  Result := False;
  GerarXML;

  AStream.Size := 0;
  WriteStrToStream(AStream, AnsiString(FXML));
  Result := True;
end;

procedure NotaFiscal.EnviarEmail(sPara, sAssunto: String; sMensagem: TStrings;
  EnviaPDF: Boolean; sCC: TStrings; Anexos: TStrings);
var
  NomeArq : String;
  AnexosEmail:TStrings;
  StreamNFe : TMemoryStream;
begin
  if not Assigned(TACBrNFe(TNotasFiscais(Collection).ACBrNFe).MAIL) then
    raise EACBrNFeException.Create('Componente ACBrMail não associado');

  AnexosEmail := TStringList.Create;
  StreamNFe := TMemoryStream.Create;
  try
    AnexosEmail.Clear;
    if Assigned(Anexos) then
      AnexosEmail.Assign(Anexos);

    with TACBrNFe(TNotasFiscais(Collection).ACBrNFe) do
    begin
      GravarStream(StreamNFe);

      if (EnviaPDF) then
      begin
        if Assigned(DANFE) then
        begin
          DANFE.ImprimirDANFEPDF(FNFe);
          NomeArq := PathWithDelim(DANFE.PathPDF) + NumID + '-nfe.pdf';
          AnexosEmail.Add(NomeArq);
        end;
      end;

      EnviarEmail( sPara, sAssunto, sMensagem, sCC, AnexosEmail, StreamNFe,
                   NumID +'-nfe.xml');
    end;
  finally
    AnexosEmail.Free;
    StreamNFe.Free;
  end;
end;

function NotaFiscal.GerarXML: String;
begin
  with TACBrNFe(TNotasFiscais(Collection).ACBrNFe) do
  begin
    FNFeW.Gerador.Opcoes.FormatoAlerta := Configuracoes.Geral.FormatoAlerta;
    FNFeW.Gerador.Opcoes.RetirarAcentos := Configuracoes.Geral.RetirarAcentos;
    FNFeW.Opcoes.GerarTXTSimultaneamente := False;
  end;

  FNFeW.GerarXml;
  FXML := FNFeW.Gerador.ArquivoFormatoXML;
  FXMLAssinado := '';
  FAlertas := FNFeW.Gerador.ListaDeAlertas.Text;
  Result := FXML;
end;

function NotaFiscal.GerarTXT: String;
begin
  with TACBrNFe(TNotasFiscais(Collection).ACBrNFe) do
  begin
//    NFe.infNFe.Versao := StringToFloat(LerVersaoDeParams(LayNfeRecepcao));
    FNFeW.Gerador.Opcoes.FormatoAlerta := Configuracoes.Geral.FormatoAlerta;
    FNFeW.Gerador.Opcoes.RetirarAcentos := Configuracoes.Geral.RetirarAcentos;
  end;

  FNFeW.Opcoes.GerarTXTSimultaneamente := True;

  FNFeW.GerarXml;
  FXML := FNFeW.Gerador.ArquivoFormatoXML;
  FAlertas := FNFeW.Gerador.ListaDeAlertas.Text;
  Result := FNFeW.Gerador.ArquivoFormatoTXT;
end;

function NotaFiscal.CalcularNomeArquivo: String;
var
  xID: String;
begin
  xID := Self.NumID;

  if EstaVazio(xID) then
    raise EACBrNFeException.Create('ID Inválido. Impossível Salvar XML');

  Result := xID + '-nfe.xml';
end;

function NotaFiscal.CalcularPathArquivo: String;
var
  Data: TDateTime;
begin
  with TACBrNFe(TNotasFiscais(Collection).ACBrNFe) do
  begin
    if Configuracoes.Arquivos.EmissaoPathNFe then
      Data := FNFe.Ide.dEmi
    else
      Data := Now;

    Result := PathWithDelim(Configuracoes.Arquivos.GetPathNFe(Data, FNFe.Emit.CNPJCPF, FNFe.Ide.modelo));
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
  DecodeDate(nfe.ide.dEmi, wAno, wMes, wDia);

  {(*}
  Result := not
    ((Copy(NFe.infNFe.ID, 4, 2) <> IntToStrZero(NFe.Ide.cUF, 2)) or
    (Copy(NFe.infNFe.ID, 6, 2)  <> Copy(FormatFloat('0000', wAno), 3, 2)) or
    (Copy(NFe.infNFe.ID, 8, 2)  <> FormatFloat('00', wMes)) or
    (Copy(NFe.infNFe.ID, 10, 14)<> PadLeft(OnlyNumber(NFe.Emit.CNPJCPF), 14, '0')) or
    (Copy(NFe.infNFe.ID, 24, 2) <> IntToStrZero(NFe.Ide.modelo, 2)) or
    (Copy(NFe.infNFe.ID, 26, 3) <> IntToStrZero(NFe.Ide.serie, 3)) or
    (Copy(NFe.infNFe.ID, 29, 9) <> IntToStrZero(NFe.Ide.nNF, 9)) or
    (Copy(NFe.infNFe.ID, 38, 1) <> TpEmisToStr(NFe.Ide.tpEmis)) or
    (Copy(NFe.infNFe.ID, 39, 8) <> IntToStrZero(NFe.Ide.cNF, 8)));
  {*)}
end;

function NotaFiscal.GetConfirmada: Boolean;
begin
  Result := TACBrNFe(TNotasFiscais(Collection).ACBrNFe).CstatConfirmada(
    FNFe.procNFe.cStat);
end;

function NotaFiscal.GetProcessada: Boolean;
begin
  Result := TACBrNFe(TNotasFiscais(Collection).ACBrNFe).CstatProcessado(
    FNFe.procNFe.cStat);
end;

function NotaFiscal.GetMsg: String;
begin
  Result := FNFe.procNFe.xMotivo;
end;

function NotaFiscal.GetNumID: String;
begin
  Result := Trim(OnlyNumber(NFe.infNFe.ID));
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
  if not (AOwner is TACBrNFe) then
    raise EACBrNFeException.Create('AOwner deve ser do tipo TACBrNFe');

  inherited;

  FACBrNFe := TACBrNFe(AOwner);
  FConfiguracoes := TACBrNFe(FACBrNFe).Configuracoes;
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

procedure TNotasFiscais.GerarNFe;
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

procedure TNotasFiscais.VerificarDANFE;
begin
  if not Assigned(TACBrNFe(FACBrNFe).DANFE) then
    raise EACBrNFeException.Create('Componente DANFE não associado.');
end;

procedure TNotasFiscais.Imprimir;
begin
  VerificarDANFE;
  TACBrNFe(FACBrNFe).DANFE.ImprimirDANFE(nil);
end;

procedure TNotasFiscais.ImprimirResumido;
begin
  VerificarDANFE;
  TACBrNFe(FACBrNFe).DANFE.ImprimirDANFEResumido(nil);
end;

procedure TNotasFiscais.ImprimirPDF;
begin
  VerificarDANFE;
  TACBrNFe(FACBrNFe).DANFE.ImprimirDANFEPDF(nil);
end;

procedure TNotasFiscais.ImprimirResumidoPDF;
begin
  VerificarDANFE;
  TACBrNFe(FACBrNFe).DANFE.ImprimirDANFEResumidoPDF(nil);
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
  AGerarNFe: Boolean = True): Boolean;
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
    LoadFromString(XML, AGerarNFe);

    for i := 0 to Self.Count - 1 do
      Self.Items[i].NomeArq := CaminhoArquivo;

    Result := True;
  finally
    ArquivoXML.Free;
  end;
end;

function TNotasFiscais.LoadFromStream(AStream: TStringStream;
  AGerarNFe: Boolean = True): Boolean;
var
  XMLOriginal: AnsiString;
begin
  Result := False;
  AStream.Position := 0;
  XMLOriginal := ReadStrFromStream(AStream, AStream.Size);

  Result := Self.LoadFromString(String(XMLOriginal), AGerarNFe);
end;

function TNotasFiscais.LoadFromString(AXMLString: String;
  AGerarNFe: Boolean = True): Boolean;
var
  AXML: AnsiString;
  P, N: integer;

  function PosNFe: integer;
  begin
    Result := pos('</NFe>', AXMLString);
  end;

begin
  Result := False;
  N := PosNFe;
  while N > 0 do
  begin
    P := pos('</nfeProc>', AXMLString);
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

      if AGerarNFe then // Recalcula o XML
        GerarXML;
    end;

    N := PosNFe;
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

function TNotasFiscais.GravarTXT(PathNomeArquivo: String): Boolean;
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
      SL.Insert(0, 'NOTA FISCAL|' + IntToStr(Self.Count));

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
          TACBrNFe(FACBrNFe).Configuracoes.Arquivos.PathSalvar) + 'NFe.TXT';

      SL.SaveToFile(PathNomeArquivo);
      Result := True;
    end;
  finally
    SL.Free;
  end;
end;


end.
