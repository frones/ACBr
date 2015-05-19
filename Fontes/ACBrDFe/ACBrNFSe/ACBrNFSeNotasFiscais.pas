{******************************************************************************}
{ Projeto: Componente ACBrNFSe                                                 }
{  Biblioteca multiplataforma de componentes Delphi                            }
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

{$I ACBr.inc}

unit ACBrNFSeNotasFiscais;

interface

uses
  Classes, Sysutils, Dialogs, Forms,
  ACBrNFSeConfiguracoes,
  ACBrNFSeDANFSeClass,
  smtpsend, ssl_openssl, mimemess, mimepart, // units para enviar email
  pnfsNFSe, pnfsNFSeR, pnfsNFSeW, pnfsConversao,
  pcnConversao, pcnAuxiliar, pcnLeitor;

type

 NotaFiscal = class(TCollectionItem)
  private
    FNFSe: TNFSe;
    FXML_Rps: AnsiString;
    FXML_Rps_Ass: AnsiString;
    FXML_LoteRps: AnsiString;
    FXML_LoteRps_Ass: AnsiString;
    FXML_NFSe: AnsiString;
    FConfirmada : Boolean;
    FMsg : AnsiString ;
    FAlertas: AnsiString;
    FNomeArq: String;
    FXML: AnsiString;
    function GetNFSeXML: AnsiString;
  public
    constructor Create(Collection2: TCollection); override;
    destructor Destroy; override;
    procedure Visualizar;
    procedure Imprimir;
    procedure ImprimirPDF;
    function SaveToFile(CaminhoArquivo: string = ''): boolean;
    function SaveToStream(Stream: TStringStream): boolean;
    procedure EnviarEmail(const sSmtpHost,
                                sSmtpPort,
                                sSmtpUser,
                                sSmtpPasswd,
                                sFrom,
                                sTo,
                                sAssunto: String;
                                sMensagem : TStrings;
                                SSL : Boolean;
                                EnviaPDF: Boolean = true;
                                sCC: TStrings = nil;
                                Anexos:TStrings=nil;
                                PedeConfirma: Boolean = False;
                                AguardarEnvio: Boolean = False;
                                NomeRemetente: String = '';
                                TLS: Boolean = True;
                                UsarThread: Boolean = True;
                                FormatoEmHTML: Boolean = False);
    property NFSe: TNFSe  read FNFSe write FNFSe;
    property XML_Rps: AnsiString read FXML_Rps write FXML_Rps;
    property XML_Rps_Ass: AnsiString read FXML_Rps_Ass write FXML_Rps_Ass;
    property XML_LoteRps: AnsiString read FXML_LoteRps write FXML_LoteRps;
    property XML_LoteRps_Ass: AnsiString read FXML_LoteRps_Ass write FXML_LoteRps_Ass;
    property XML_NFSe: AnsiString read FXML_NFSe write FXML_NFSe;
    property Confirmada: Boolean  read FConfirmada write FConfirmada;
    property Msg: AnsiString  read FMsg write FMsg;
    property Alertas: AnsiString read FAlertas write FAlertas;
    property NomeArq: String read FNomeArq write FNomeArq;
    property XML: AnsiString  read GetNFSeXML write FXML;
  end;

 TNotasFiscais = class(TOwnedCollection)
  private
    FConfiguracoes : TConfiguracoes;
    FACBrNFSe : TComponent;

    FNumeroLote: string;
    FTransacao: boolean;
    FXML_Lote: AnsiString;
    FXML_Lote_Ass: AnsiString;

    function GetItem(Index: Integer): NotaFiscal;
    procedure SetItem(Index: Integer; const Value: NotaFiscal);
  public
    constructor Create(AOwner: TPersistent; ItemClass: TCollectionItemClass);

    procedure GerarRPS;
    procedure Assinar(AssinaRPS: Boolean = True);
    function AssinarLoteRps(nLote:String; vLote: WideString; ASincrono: Boolean = False): WideString;
    procedure Valida;
    procedure Imprimir;
    procedure ImprimirPDF;
    function  Add: NotaFiscal;
    function Insert(Index: Integer): NotaFiscal;
    property Items[Index: Integer]: NotaFiscal read GetItem  write SetItem;
    property Configuracoes: TConfiguracoes read FConfiguracoes  write FConfiguracoes;

    function GetNamePath: string; override ;
    function LoadFromFile(CaminhoArquivo: string): boolean;
    function LoadFromStream(Stream: TStringStream): boolean;
    function LoadFromString(AString: string): boolean;
    function SaveToFile(PathArquivo: string = ''): boolean;

    property NumeroLote: String read FNumeroLote write FNumeroLote;
    property Transacao: Boolean read FTransacao write FTransacao;
    property XML_Lote: AnsiString  read FXML_Lote write FXML_Lote;
    property XML_Lote_Ass: AnsiString  read FXML_Lote_Ass write FXML_Lote_Ass;

    property ACBrNFSe : TComponent read FACBrNFSe ;
  end;

  TSendMailThread = class(TThread)
  private
    FException : Exception;
    // FOwner: NotaFiscal;
    procedure DoHandleException;
  public
    OcorreramErros: Boolean;
    Terminado: Boolean;
    smtp : TSMTPSend;
    sFrom : String;
    sTo : String;
    sCC : TStrings;
    slmsg_Lines : TStrings;
    constructor Create{(AOwner: NotaFiscal)};
    destructor Destroy; override;
  protected
    procedure Execute; override;
    procedure HandleException;
  end;

implementation

uses
 ACBrNFSe, ACBrUtil, ACBrDFeUtil, pcnGerador;

{ NotaFiscal }

constructor NotaFiscal.Create(Collection2: TCollection);
begin
 inherited Create(Collection2);

 FNFSe        := TNFSe.Create;
 FXML_RPS     := '';
 FXML_RPS_Ass := '';
 FNomeArq     := '';
end;

destructor NotaFiscal.Destroy;
begin
 FNFSe.Free;

 inherited Destroy;
end;

procedure NotaFiscal.EnviarEmail(const sSmtpHost,
                                       sSmtpPort,
                                       sSmtpUser,
                                       sSmtpPasswd,
                                       sFrom,
                                       sTo,
                                       sAssunto: String;
                                       sMensagem : TStrings;
                                       SSL : Boolean;
                                       EnviaPDF: Boolean = true;
                                       sCC: TStrings = nil;
                                       Anexos:TStrings=nil;
                                       PedeConfirma: Boolean = False;
                                       AguardarEnvio: Boolean = False;
                                       NomeRemetente: String = '';
                                       TLS: Boolean = True;
                                       UsarThread: Boolean = True;
                                       FormatoEmHTML: Boolean = False);

var
 NomeArq    : String;
 NomeArqPDF : String;
 NomeArqXML : String;
 AnexosEmail: TStrings;
 StreamNFSe : TStringStream;
begin
  AnexosEmail := TStringList.Create;
  StreamNFSe  := TStringStream.Create('');

  if TACBrNFSe( TNotasFiscais( Collection ).ACBrNFSe ).Configuracoes.Arquivos.NomeLongoNFSe
  then NomeArqXML := NotaUtil.GerarNomeNFSe(UFparaCodigo(Nfse.PrestadorServico.Endereco.UF),
                                            Nfse.DataEmissao,
                                            Nfse.PrestadorServico.IdentificacaoPrestador.Cnpj,
                                            StrToIntDef(Nfse.Numero, 0))
  else NomeArqXML := NFSe.Numero;

  try
    AnexosEmail.Clear;
    if Anexos <> nil then
      AnexosEmail.Text := Anexos.Text;
    if NomeArq <> '' then
     begin
       SaveToFile(NomeArq); // removido o comentario por ala em 02/04/2014,
       AnexosEmail.Add(NomeArq);
     end
    else
     begin
       SaveToStream(StreamNFSe);
     end;
    if (EnviaPDF) then
    begin
       if TACBrNFSe( TNotasFiscais( Collection ).ACBrNFSe ).DANFSE <> nil then
       begin
          TACBrNFSe( TNotasFiscais( Collection ).ACBrNFSe ).DANFSE.ImprimirDANFSEPDF(NFSe);

          // removido por ala NomeArqPDF := Trim(NomeArq);
          // removido por ala if NomeArqPDF <> ''
           // removido por ala then begin
             NomeArqPDF := StringReplace(NFSe.Numero, 'NFSe', '', [rfIgnoreCase]); // // removido o comentario por ala em 02/04/2014,
             NomeArqPDF := NomeArqXML;
             NomeArqPDF := PathWithDelim(TACBrNFSe( TNotasFiscais( Collection ).ACBrNFSe ).DANFSE.PathPDF) + NomeArqPDF + '.pdf';
           // removido o comentario por ala em 02/04/2014,  end
           // removido o comentario por ala em 02/04/2014,   else NomeArqPDF := StringReplace(NomeArqPDF, '-nfse.xml', '.pdf', [rfIgnoreCase]);

          AnexosEmail.Add(NomeArqPDF);
       end;
    end;
    TACBrNFSe( TNotasFiscais( Collection ).ACBrNFSe ).EnviaEmail(sSmtpHost,
                sSmtpPort,
                sSmtpUser,
                sSmtpPasswd,
                sFrom,
                sTo,
                sAssunto,
                sMensagem,
                SSL,
                sCC,
                AnexosEmail,
                PedeConfirma,
                AguardarEnvio,
                NomeRemetente,
                TLS,
                StreamNFSe,
//                copy(NFSe.Numero, (length(NFSe.Numero) - 44) + 1, 44) + '-NFSe.xml',
                NomeArqXML + '-nfse.xml',
                UsarThread,
                FormatoEmHTML);

// Mantive a linha NomeArqXML + '-nfse.xml', para que o desenvolvedor escolha a forma
// que o componente vai gerar o nome do arquivo xml da NFS-e
 
  finally
    AnexosEmail.Free;
    StreamNFSe.Free;
  end;
(*
 AnexosEmail := TStringList.Create;
 StreamNFSe  := TStringStream.Create('');

 if TACBrNFSe( TNotasFiscais( Collection ).ACBrNFSe ).Configuracoes.Arquivos.NomeLongoNFSe
  then NomeArqXML := NotaUtil.GerarNomeNFSe(UFparaCodigo(Nfse.PrestadorServico.Endereco.UF),
                                            Nfse.DataEmissao,
                                            Nfse.PrestadorServico.IdentificacaoPrestador.Cnpj,
                                            StrToIntDef(Nfse.Numero, 0))
  else NomeArqXML := NFSe.Numero;

 try
    AnexosEmail.Clear;
    if Anexos <> nil then
      AnexosEmail.Text := Anexos.Text;
    if NomeArq <> '' then
     begin
//       SaveToFile(NomeArq);
//       AnexosEmail.Add(NomeArq);
     end
    else
     begin
       SaveToStream(StreamNFSe);
     end;
    if (EnviaPDF) then
    begin
       if TACBrNFSe( TNotasFiscais( Collection ).ACBrNFSe ).DANFSE <> nil then
       begin
          TACBrNFSe( TNotasFiscais( Collection ).ACBrNFSe ).DANFSE.ImprimirDANFSEPDF(NFSe);

          NomeArqPDF := Trim(NomeArq);
          if NomeArqPDF <> ''
           then begin
//             NomeArqPDF := StringReplace(NFSe.Numero, 'NFSe', '', [rfIgnoreCase]);
             NomeArqPDF := NomeArqXML;
             NomeArqPDF := PathWithDelim(TACBrNFSe( TNotasFiscais( Collection ).ACBrNFSe ).DANFSE.PathPDF) + NomeArqPDF + '.pdf';
           end
           else NomeArqPDF := StringReplace(NomeArqPDF, '-nfse.xml', '.pdf', [rfIgnoreCase]);

          AnexosEmail.Add(NomeArqPDF);
       end;
    end;
    TACBrNFSe( TNotasFiscais( Collection ).ACBrNFSe ).EnviaEmail(sSmtpHost,
                sSmtpPort,
                sSmtpUser,
                sSmtpPasswd,
                sFrom,
                sTo,
                sAssunto,
                sMensagem,
                SSL,
                sCC,
                AnexosEmail,
                PedeConfirma,
                AguardarEnvio,
                NomeRemetente,
                TLS,
                StreamNFSe,
//                copy(NFSe.Numero, (length(NFSe.Numero) - 44) + 1, 44) + '-NFSe.xml',
                NomeArqXML + '-nfse.xml',
                UsarThread,
                FormatoEmHTML);
 finally
    AnexosEmail.Free;
    StreamNFSe.Free;
 end;
*)
(*
 m := TMimemess.create;

 ThreadSMTP := TSendMailThread.Create(Self);  // Não Libera, pois usa FreeOnTerminate := True ;
 StreamNFSe := TStringStream.Create('');
 try
  p := m.AddPartMultipart('mixed', nil);
  if sMensagem <> nil
   then m.AddPartText(sMensagem, p);
  SaveToStream(StreamNFSe);
  m.AddPartBinary(StreamNFSe, copy(NFSe.Numero, (length(NFSe.Numero) - 44) + 1, 44) + '-NFSe.xml', p);

  if (EnviaPDF)
   then begin
    if TACBrNFSe( TNotasFiscais( Collection ).ACBrNFSe ).DANFSE <> nil
     then begin
      TACBrNFSe( TNotasFiscais( Collection ).ACBrNFSe ).DANFSE.ImprimirDANFSEPDF(NFSe);
      // Alterado por Italo em 04/12/2012
      NomeArqPDF := Trim(NomeArq);
      if NomeArqPDF <> ''
       then begin
         NomeArqPDF := StringReplace(NFSe.Numero, 'NFSe', '', [rfIgnoreCase]);
         NomeArqPDF := PathWithDelim(TACBrNFSe( TNotasFiscais( Collection ).ACBrNFSe ).DANFSE.PathPDF) + NomeArqPDF + '.pdf';
       end
       else NomeArqPDF := StringReplace(NomeArqPDF, '-nfse.xml', '.pdf', [rfIgnoreCase]);

      m.AddPartBinaryFromFile(NomeArqPDF, p);
     end;
   end;

  if assigned(Anexos) then
   for i := 0 to Anexos.Count - 1 do
    begin
     m.AddPartBinaryFromFile(Anexos[i], p);
    end;

  m.header.tolist.add(sTo);

  if Trim(NomeRemetente) <> ''
   then m.header.From := Format('%s<%s>', [NomeRemetente, sFrom])
   else m.header.From := sFrom;

  m.header.subject := sAssunto;
  m.Header.ReplyTo := sFrom;
  if PedeConfirma
   then m.Header.CustomHeaders.Add('Disposition-Notification-To: '+sFrom);
  m.EncodeMessage;

  ThreadSMTP.sFrom := sFrom;
  ThreadSMTP.sTo   := sTo;
  if sCC <> nil
   then ThreadSMTP.sCC.AddStrings(sCC);
  ThreadSMTP.slmsg_Lines.AddStrings(m.Lines);

  ThreadSMTP.smtp.UserName   := sSmtpUser;
  ThreadSMTP.smtp.Password   := sSmtpPasswd;
  ThreadSMTP.smtp.TargetHost := sSmtpHost;

  if not EstaVazio( sSmtpPort )
   then ThreadSMTP.smtp.TargetPort := sSmtpPort; // Usa default

  ThreadSMTP.smtp.FullSSL := SSL;
  ThreadSMTP.smtp.AutoTLS := TLS;

  TACBrNFSe( TNotasFiscais( Collection ).ACBrNFSe ).SetStatus( stNFSeEmail );
  ThreadSMTP.Resume; // inicia a thread

  if AguardarEnvio
   then begin
    repeat
     Sleep(1000);
     Application.ProcessMessages;
    until ThreadSMTP.Terminado;
   end;

  TACBrNFSe( TNotasFiscais( Collection ).ACBrNFSe ).SetStatus( stNFSeIdle );
 finally
  m.free;
  StreamNFSe.Free;
 end;
 *)
end;

function NotaFiscal.GetNFSeXML: AnsiString;
var
 LocNFSeW : TNFSeW;
begin
 LocNFSeW := TNFSeW.Create(Self.NFSe);
 try
  LocNFSeW.Provedor      := TACBrNFSe( TNotasFiscais( Collection ).ACBrNFSe ).Configuracoes.WebServices.Provedor;
  LocNFSeW.Prefixo4      := TACBrNFSe( TNotasFiscais( Collection ).ACBrNFSe ).Configuracoes.WebServices.Prefixo4;
  LocNFSeW.Identificador := TACBrNFSe( TNotasFiscais( Collection ).ACBrNFSe ).Configuracoes.WebServices.Identificador;
  LocNFSeW.URL           := TACBrNFSe( TNotasFiscais( Collection ).ACBrNFSe ).Configuracoes.WebServices.URL;
  LocNFSeW.VersaoXML     := TACBrNFSe( TNotasFiscais( Collection ).ACBrNFSe ).Configuracoes.WebServices.VersaoXML;
  LocNFSeW.DefTipos      := TACBrNFSe( TNotasFiscais( Collection ).ACBrNFSe ).Configuracoes.WebServices.DefTipos;
  LocNFSeW.ServicoEnviar := TACBrNFSe( TNotasFiscais( Collection ).ACBrNFSe ).Configuracoes.WebServices.ServicoEnviar;
  LocNFSeW.QuebradeLinha := TACBrNFSe( TNotasFiscais( Collection ).ACBrNFSe ).Configuracoes.WebServices.QuebradeLinha;

  LocNFSeW.Gerador.Opcoes.FormatoAlerta  := TACBrNFSe( TNotasFiscais( Collection ).ACBrNFSe ).Configuracoes.Geral.FormatoAlerta;
  LocNFSeW.Gerador.Opcoes.RetirarAcentos := TACBrNFSe( TNotasFiscais( Collection ).ACBrNFSe ).Configuracoes.Geral.RetirarAcentos;

  LocNFSeW.GerarXml;
  Result := LocNFSeW.Gerador.ArquivoFormatoXML;
 finally
  LocNFSeW.Free;
 end;
end;

procedure NotaFiscal.Visualizar;
begin
 if not Assigned( TACBrNFSe( TNotasFiscais( Collection ).ACBrNFSe ).DANFSE )
  then raise Exception.Create('Componente DANFSE não associado.')
  else TACBrNFSe( TNotasFiscais( Collection ).ACBrNFSe ).DANFSE.VisualizarDANFSE(NFSe);
end;

procedure NotaFiscal.Imprimir;
begin
 if not Assigned( TACBrNFSe( TNotasFiscais( Collection ).ACBrNFSe ).DANFSE )
  then raise Exception.Create('Componente DANFSE não associado.')
  else TACBrNFSe( TNotasFiscais( Collection ).ACBrNFSe ).DANFSE.ImprimirDANFSE(NFSe);
end;

procedure NotaFiscal.ImprimirPDF;
begin
 if not Assigned( TACBrNFSe( TNotasFiscais( Collection ).ACBrNFSe ).DANFSE )
  then raise Exception.Create('Componente DANFSE não associado.')
  else TACBrNFSe( TNotasFiscais( Collection ).ACBrNFSe ).DANFSE.ImprimirDANFSEPDF(NFSe);
end;

function NotaFiscal.SaveToFile(CaminhoArquivo: string): boolean;
var
 LocNFSeW : TNFSeW;
begin
 try
  Result   := True;
  LocNFSeW := TNFSeW.Create(NFSe);
  try
   LocNFSeW.Provedor      := TACBrNFSe( TNotasFiscais( Collection ).ACBrNFSe ).Configuracoes.WebServices.Provedor;
   LocNFSeW.Prefixo4      := TACBrNFSe( TNotasFiscais( Collection ).ACBrNFSe ).Configuracoes.WebServices.Prefixo4;
   LocNFSeW.Identificador := TACBrNFSe( TNotasFiscais( Collection ).ACBrNFSe ).Configuracoes.WebServices.Identificador;
   LocNFSeW.URL           := TACBrNFSe( TNotasFiscais( Collection ).ACBrNFSe ).Configuracoes.WebServices.URL;
   LocNFSeW.VersaoXML     := TACBrNFSe( TNotasFiscais( Collection ).ACBrNFSe ).Configuracoes.WebServices.VersaoXML;
   LocNFSeW.DefTipos      := TACBrNFSe( TNotasFiscais( Collection ).ACBrNFSe ).Configuracoes.WebServices.DefTipos;
   LocNFSeW.ServicoEnviar := TACBrNFSe( TNotasFiscais( Collection ).ACBrNFSe ).Configuracoes.WebServices.ServicoEnviar;
   LocNFSeW.QuebradeLinha := TACBrNFSe( TNotasFiscais( Collection ).ACBrNFSe ).Configuracoes.WebServices.QuebradeLinha;

   LocNFSeW.Gerador.Opcoes.FormatoAlerta  := TACBrNFSe( TNotasFiscais( Collection ).ACBrNFSe ).Configuracoes.Geral.FormatoAlerta;
   LocNFSeW.Gerador.Opcoes.RetirarAcentos := TACBrNFSe( TNotasFiscais( Collection ).ACBrNFSe ).Configuracoes.Geral.RetirarAcentos;

   LocNFSeW.GerarXml;

   if EstaVazio(CaminhoArquivo)
    then begin
     if TACBrNFSe( TNotasFiscais( Collection ).ACBrNFSe ).Configuracoes.Arquivos.EmissaoPathNFSe then
       CaminhoArquivo := TACBrNFSe( TNotasFiscais( Collection ).ACBrNFSe ).Configuracoes.Arquivos.GetPathRPS(Self.NFSe.DataEmissao)
     else
       CaminhoArquivo := TACBrNFSe( TNotasFiscais( Collection ).ACBrNFSe ).Configuracoes.Arquivos.GetPathRPS(0);

    CaminhoArquivo := NotaUtil.PathWithDelim(CaminhoArquivo) + Self.NFSe.InfID.ID + '-Rps.xml';
    end;

   if EstaVazio(CaminhoArquivo) or not DirectoryExists(ExtractFilePath(CaminhoArquivo))
    then raise Exception.Create('Caminho Inválido: ' + CaminhoArquivo);

   LocNFSeW.Gerador.SalvarArquivo(CaminhoArquivo);
   NomeArq := CaminhoArquivo;
  finally
   LocNFSeW.Free;
  end;
 except
  raise;
  Result := False;
 end;
end;

function NotaFiscal.SaveToStream(Stream: TStringStream): boolean;
var
 LocNFSeW : TNFSeW;
begin
 try
  Result   := True;
  LocNFSeW := TNFSeW.Create(NFSe);
  try
  (*
   LocNFSeW.Provedor      := TACBrNFSe( TNotasFiscais( Collection ).ACBrNFSe ).Configuracoes.WebServices.Provedor;
   LocNFSeW.Prefixo4      := TACBrNFSe( TNotasFiscais( Collection ).ACBrNFSe ).Configuracoes.WebServices.Prefixo4;
   LocNFSeW.Identificador := TACBrNFSe( TNotasFiscais( Collection ).ACBrNFSe ).Configuracoes.WebServices.Identificador;
   LocNFSeW.URL           := TACBrNFSe( TNotasFiscais( Collection ).ACBrNFSe ).Configuracoes.WebServices.URL;
   LocNFSeW.VersaoXML     := TACBrNFSe( TNotasFiscais( Collection ).ACBrNFSe ).Configuracoes.WebServices.VersaoXML;
   LocNFSeW.DefTipos      := TACBrNFSe( TNotasFiscais( Collection ).ACBrNFSe ).Configuracoes.WebServices.DefTipos;
   LocNFSeW.ServicoEnviar := TACBrNFSe( TNotasFiscais( Collection ).ACBrNFSe ).Configuracoes.WebServices.ServicoEnviar;
   LocNFSeW.GerarXml;
   Stream.WriteString(LocNFSeW.Gerador.ArquivoFormatoXML);
   *)
   if XML_NFSe <> ''
    then Stream.WriteString(XML_NFSe)
    else if XML_RPS <> ''
          then Stream.WriteString(XML_RPS)
          else Stream.WriteString('');
  finally
   LocNFSeW.Free;
  end;
 except
//  raise;
  Result := False;
 end;
end;

{ TNotasFiscais }

function TNotasFiscais.Add: NotaFiscal;
begin
 Result := NotaFiscal(inherited Add);
end;

procedure TNotasFiscais.Assinar(AssinaRPS: Boolean = True);
var
 i              : Integer;
 vAssinada      : AnsiString;
 LocNFSeW       : TNFSeW;
 Leitor         : TLeitor;
 FMsg           : AnsiString;
 ALote          : Boolean;  // Alterado por Nilton Olher - 11/02/2015 => Controle se é em Lote
 CaminhoArquivo : String;
begin
  ALote := (Self.Count>1); // Alterado por Nilton Olher - 11/02/2015 => Controle se é em Lote
 for i := 0 to Self.Count-1 do
  begin
   LocNFSeW := TNFSeW.Create(Self.Items[i].NFSe);
   try
    LocNFSeW.Provedor      := self.Configuracoes.WebServices.Provedor;
    LocNFSeW.Prefixo4      := self.Configuracoes.WebServices.Prefixo4;
    LocNFSeW.Identificador := self.Configuracoes.WebServices.Identificador;
    LocNFSeW.URL           := self.Configuracoes.WebServices.URL;
    LocNFSeW.VersaoXML     := self.Configuracoes.WebServices.VersaoXML;
    LocNFSeW.DefTipos      := self.Configuracoes.WebServices.DefTipos;
    LocNFSeW.ServicoEnviar := self.Configuracoes.WebServices.ServicoEnviar;
    LocNFSeW.QuebradeLinha := self.Configuracoes.WebServices.QuebradeLinha;

    LocNFSeW.Gerador.Opcoes.FormatoAlerta  := self.Configuracoes.Geral.FormatoAlerta;
    LocNFSeW.Gerador.Opcoes.RetirarAcentos := self.Configuracoes.Geral.RetirarAcentos;

    LocNFSeW.GerarXml(ALote); // Alterado por Nilton Olher - 11/02/2015
    Self.Items[i].Alertas := LocNFSeW.Gerador.ListaDeAlertas.Text;
    Self.Items[i].XML_Rps := LocNFSeW.Gerador.ArquivoFormatoXML;

    if FConfiguracoes.WebServices.Salvar
     then begin
     if FConfiguracoes.Arquivos.EmissaoPathNFSe then
       CaminhoArquivo := FConfiguracoes.Arquivos.GetPathRPS(Self.Items[i].NFSe.DataEmissao)
     else
       CaminhoArquivo := FConfiguracoes.Arquivos.GetPathRPS(0);
       FConfiguracoes.Geral.Save(NotaUtil.PathWithDelim(CaminhoArquivo) + Self.Items[i].NFSe.InfID.ID+'-Rps2.xml', LocNFSeW.Gerador.ArquivoFormatoXML);
     end;

//    if self.Configuracoes.Certificados.AssinaRPS and AssinaRPS
    // Alterado por Italo em 28/11/2014
//    if self.Configuracoes.Certificados.AssinaRPS or AssinaRPS
    // Alterado por Italo em 09/01/2015
    if AssinaRPS
     then begin
      {$IFDEF ACBrNFSeOpenSSL}
        if not(NotaUtil.Assinar(LocNFSeW.Gerador.ArquivoFormatoXML,
                                FConfiguracoes.Certificados.Certificado,
                                FConfiguracoes.Certificados.Senha,
                                vAssinada, FMsg, False,
                                FConfiguracoes.WebServices.Prefixo3,
                                FConfiguracoes.WebServices.Prefixo4,
                                FConfiguracoes.WebServices.Provedor, ALote)) // Alterado por Nilton Olher - 11/02/2015
         then raise Exception.Create('Falha ao assinar Nota Fiscal de Serviço Eletrônica '+
                                     Self.Items[i].NFSe.IdentificacaoRps.Numero + FMsg);
      {$ELSE}
        // Alterado por Italo em 12/07/2012
        if not(NotaUtil.Assinar(LocNFSeW.Gerador.ArquivoFormatoXML,
                                FConfiguracoes.Certificados.GetCertificado,
                                vAssinada, FMsg, False,
                                FConfiguracoes.WebServices.Prefixo3,
                                FConfiguracoes.WebServices.Prefixo4,
                                FConfiguracoes.WebServices.Provedor, ALote)) // Alterado por Nilton Olher - 11/02/2015
         then raise Exception.Create('Falha ao assinar Nota Fiscal de Serviço Eletrônica '+
                                     Self.Items[i].NFSe.IdentificacaoRps.Numero + FMsg);
      {$ENDIF}

      vAssinada := StringReplace( vAssinada, '<'+ENCODING_UTF8_STD+'>', '', [rfReplaceAll] ) ;
      vAssinada := StringReplace( vAssinada, '<?xml version="1.0"?>', '', [rfReplaceAll] ) ;
     end
     else vAssinada := LocNFSeW.Gerador.ArquivoFormatoXML;

    Leitor       := TLeitor.Create;
    Leitor.Grupo := vAssinada;

    Self.Items[i].XML_Rps_Ass                    := vAssinada;
    Self.Items[i].NFSe.signature.URI             := Leitor.rAtributo('Reference URI=');
    Self.Items[i].NFSe.signature.DigestValue     := Leitor.rCampo(tcStr, 'DigestValue');
    Self.Items[i].NFSe.signature.SignatureValue  := Leitor.rCampo(tcStr, 'SignatureValue');
    Self.Items[i].NFSe.signature.X509Certificate := Leitor.rCampo(tcStr, 'X509Certificate');

    Leitor.Free;

//    if FConfiguracoes.Geral.Salvar
//     then
//    if FConfiguracoes.Arquivos.EmissaoPathNFSe then
//      CaminhoArquivo := FConfiguracoes.Arquivos.GetPathRPS(Self.Items[i].NFSe.DataEmissao)
//    else
//      CaminhoArquivo := FConfiguracoes.Arquivos.GetPathRPS(0);

//    FConfiguracoes.Geral.Save(NotaUtil.PathWithDelim(CaminhoArquivo) + Self.Items[i].NFSe.InfID.ID+'-Rps.xml', vAssinada);

    if NaoEstaVazio(Self.Items[i].NomeArq)
     then FConfiguracoes.Geral.Save(ExtractFileName(Self.Items[i].NomeArq), vAssinada, ExtractFilePath(Self.Items[i].NomeArq))
     else begin
       if FConfiguracoes.Arquivos.EmissaoPathNFSe then
         CaminhoArquivo := FConfiguracoes.Arquivos.GetPathRPS(Self.Items[i].NFSe.DataEmissao)
       else
         CaminhoArquivo := FConfiguracoes.Arquivos.GetPathRPS(0);

       FConfiguracoes.Geral.Save(NotaUtil.PathWithDelim(CaminhoArquivo) + Self.Items[i].NFSe.InfID.ID+'-Rps.xml', vAssinada);
     end;

   finally
    LocNFSeW.Free;
   end;
  end;
end;

function TNotasFiscais.AssinarLoteRps(nLote:String; vLote: WideString; ASincrono: Boolean = False): WideString;
var
 vAssinada : AnsiString;
// Leitor    : TLeitor;
 FMsg      : AnsiString;
begin
 try
  if FConfiguracoes.Certificados.AssinaLote
   then begin
    {$IFDEF ACBrNFSeOpenSSL}
      if not(NotaUtil.Assinar(vLote,
                              FConfiguracoes.Certificados.Certificado,
                              FConfiguracoes.Certificados.Senha,
                              vAssinada, FMsg, True,
                              FConfiguracoes.WebServices.Prefixo3,
                              FConfiguracoes.WebServices.Prefixo4,
                              FConfiguracoes.WebServices.Provedor,
                              ASincrono))
       then raise Exception.Create('Falha ao assinar o Lote de RPS, '+ nLote + FMsg);
    {$ELSE}

      // {Dalvan}
      if FConfiguracoes.WebServices.Provedor in [proPronim] then
        vLote :=StringReplace(vLote, ' xmlns="http://www.abrasf.org.br/ABRASF/arquivos/nfse.xsd"', '', [rfReplaceAll]);

      // Alterado por Italo em 24/07/2012
      if not(NotaUtil.Assinar(vLote,
                              FConfiguracoes.Certificados.GetCertificado,
                              vAssinada, FMsg, True,
                              FConfiguracoes.WebServices.Prefixo3,
                              FConfiguracoes.WebServices.Prefixo4,
                              FConfiguracoes.WebServices.Provedor,
                              ASincrono))
       then raise Exception.Create('Falha ao assinar o Lote de RPS, '+
                                     nLote + FMsg);
    {$ENDIF}

    vAssinada := StringReplace( vAssinada, '<'+ENCODING_UTF8_STD+'>', '', [rfReplaceAll] ) ;
    vAssinada := StringReplace( vAssinada, '<?xml version="1.0"?>', '', [rfReplaceAll] ) ;
   end
   else vAssinada := vLote;

//  Leitor       := TLeitor.Create;
//  Leitor.Grupo := vAssinada;

  Self.Items[0].XML_LoteRps     := vLote;
  Self.Items[0].XML_LoteRps_Ass := vAssinada;

//  Self.Items[i].NFSe.signature.URI             := Leitor.rAtributo('Reference URI=');
//  Self.Items[i].NFSe.signature.DigestValue     := Leitor.rCampo(tcStr, 'DigestValue');
//  Self.Items[i].NFSe.signature.SignatureValue  := Leitor.rCampo(tcStr, 'SignatureValue');
//  Self.Items[i].NFSe.signature.X509Certificate := Leitor.rCampo(tcStr, 'X509Certificate');

//  Leitor.Free;

//  if FConfiguracoes.Geral.Salvar
//   then FConfiguracoes.Geral.Save(FConfiguracoes.Geral.PathSalvar+IntToStr(nLote)+'-Lote.xml', vAssinada);

//  if NotaUtil.NaoEstaVazio(Self.Items[i].NomeArq)
//   then FConfiguracoes.Geral.Save(ExtractFileName(Self.Items[i].NomeArq), vAssinada, ExtractFilePath(Self.Items[i].NomeArq));

 finally
  Result := vAssinada;
 end;
end;

constructor TNotasFiscais.Create(AOwner: TPersistent;
  ItemClass: TCollectionItemClass);
begin
 if not (AOwner is TACBrNFSe )
  then raise Exception.Create( 'AOwner deve ser do tipo TNFSe.') ;

 inherited;

 FNumeroLote   := '';
 FXML_Lote     := '';
 FXML_Lote_Ass := '';
 FACBrNFSe     := TACBrNFSe( AOwner ) ;
end;

procedure TNotasFiscais.GerarRPS;
var
 i        : Integer;
 LocNFSeW : TNFSeW;
begin
 for i := 0 to Self.Count-1 do
  begin
   LocNFSeW := TNFSeW.Create(Self.Items[i].NFSe);
   try
    LocNFSeW.Provedor      := self.Configuracoes.WebServices.Provedor;
    LocNFSeW.Prefixo4      := self.Configuracoes.WebServices.Prefixo4;
    LocNFSeW.Identificador := self.Configuracoes.WebServices.Identificador;
    LocNFSeW.URL           := Self.Configuracoes.WebServices.URL;
    LocNFSeW.VersaoXML     := self.Configuracoes.WebServices.VersaoXML;
    LocNFSeW.DefTipos      := self.Configuracoes.WebServices.DefTipos;
    LocNFSeW.ServicoEnviar := Self.Configuracoes.WebServices.ServicoEnviar;
    LocNFSeW.QuebradeLinha := Self.Configuracoes.WebServices.QuebradeLinha;

    LocNFSeW.Gerador.Opcoes.FormatoAlerta  := Self.Configuracoes.Geral.FormatoAlerta;
    LocNFSeW.Gerador.Opcoes.RetirarAcentos := Self.Configuracoes.Geral.RetirarAcentos;

    LocNFSeW.GerarXml;
    Self.Items[i].XML_RPS := LocNFSeW.Gerador.ArquivoFormatoXML;
    Self.Items[i].Alertas := LocNFSeW.Gerador.ListaDeAlertas.Text;
   finally
    LocNFSeW.Free;
   end;
  end;
end;

function TNotasFiscais.GetItem(Index: Integer): NotaFiscal;
begin
 Result := NotaFiscal(inherited Items[Index]);
end;

function TNotasFiscais.GetNamePath: string;
begin
 Result := 'NotaFiscal';
end;

procedure TNotasFiscais.Imprimir;
begin
 if not Assigned( TACBrNFSe( FACBrNFSe ).DANFSE )
  then raise Exception.Create('Componente DANFSE não associado.')
  else TACBrNFSe( FACBrNFSe ).DANFSE.ImprimirDANFSE(nil);
end;

procedure TNotasFiscais.ImprimirPDF;
begin
 if not Assigned( TACBrNFSe( FACBrNFSe ).DANFSE )
  then raise Exception.Create('Componente DANFSE não associado.')
  else TACBrNFSe( FACBrNFSe ).DANFSE.ImprimirDANFSEPDF(nil);
end;

function TNotasFiscais.Insert(Index: Integer): NotaFiscal;
begin
 Result := NotaFiscal(inherited Insert(Index));
end;

function TNotasFiscais.LoadFromFile(CaminhoArquivo: string): boolean;
var
 LocNFSeR : TNFSeR;
 ArquivoXML: TStringList;
 XML : AnsiString;
 Tipo: Integer;
{$IFDEF DELPHI2009_UP}
 Encoding : TEncoding;
{$ENDIF}
 CodigoMunicipio, Prestador, RazaoSocial,
 CNPJ, IM, NumeroRPS, SerieRPS, TipoRPS: String;
 ok: Boolean;
begin
 try
  {$IFDEF DELPHI2009_UP}
    Encoding := NotaUtil.LoadXML(CaminhoArquivo);
  {$ENDIF}
  ArquivoXML := TStringList.Create;
  ArquivoXML.LoadFromFile(CaminhoArquivo {$IFDEF DELPHI2009_UP},Encoding{$ENDIF});
  Result := True;

  ArquivoXML.Text := StringReplace(StringReplace( ArquivoXML.Text, '&lt;', '<', [rfReplaceAll]), '&gt;', '>', [rfReplaceAll]);
  ArquivoXML.Text := NotaUtil.RetirarPrefixos(ArquivoXML.Text);

  if pos('</CompNfse>', ArquivoXML.Text) > 0
   then Tipo := 1
   else if pos('</ComplNfse>', ArquivoXML.Text) > 0
         then Tipo := 2
         else if pos('</Nfse>', ArquivoXML.Text) > 0
               then Tipo := 3
               else if pos('</ListaRps>', ArquivoXML.Text) > 0
                    then Tipo := 4
                    else if pos('</Rps>', ArquivoXML.Text) > 0
                         then Tipo := 5
                         else if pos('</listaRps>', ArquivoXML.Text) > 0
                              then Tipo := 6
                              else if pos('</NFS-e>', ArquivoXML.Text) > 0
                                    then Tipo := 7 // Infisc
                                    else Tipo := 0;

  case Tipo of
   1: begin
       while pos('</CompNfse>', ArquivoXML.Text) > 0 do
        begin
         XML             := copy(ArquivoXML.Text, 1, pos('</CompNfse>', ArquivoXML.Text) + 10);
         ArquivoXML.Text := Trim(copy(ArquivoXML.Text, pos('</CompNfse>', ArquivoXML.Text) + 11, length(ArquivoXML.Text)));
         LocNFSeR        := TNFSeR.Create(Self.Add.NFSe);
         try
          LocNFSeR.Leitor.Arquivo := XML;
          LocNFSeR.VersaoXML      := NotaUtil.VersaoXML(XML);
          LocNFSeR.TabServicosExt := self.Configuracoes.Arquivos.TabServicosExt;
          LocNFSeR.Provedor       := self.Configuracoes.WebServices.Provedor;
          LocNFSeR.LerXml;
          Items[Self.Count-1].XML_NFSe := LocNFSeR.Leitor.Arquivo;
          Items[Self.Count-1].NomeArq := CaminhoArquivo;
         finally
          LocNFSeR.Free;
         end;
        end;
      end;
   2: begin
       while pos('</ComplNfse>', ArquivoXML.Text) > 0 do
        begin
         XML             := copy(ArquivoXML.Text, 1, pos('</ComplNfse>', ArquivoXML.Text) + 11);
         ArquivoXML.Text := Trim(copy(ArquivoXML.Text, pos('</ComplNfse>', ArquivoXML.Text) + 12, length(ArquivoXML.Text)));
         LocNFSeR        := TNFSeR.Create(Self.Add.NFSe);
         try
          LocNFSeR.Leitor.Arquivo := XML;
          LocNFSeR.VersaoXML      := NotaUtil.VersaoXML(XML);
          LocNFSeR.TabServicosExt := self.Configuracoes.Arquivos.TabServicosExt;
          LocNFSeR.Provedor       := self.Configuracoes.WebServices.Provedor;
          LocNFSeR.LerXml;
          Items[Self.Count-1].XML_NFSe := LocNFSeR.Leitor.Arquivo;
          Items[Self.Count-1].NomeArq := CaminhoArquivo;
         finally
          LocNFSeR.Free;
         end;
        end;
      end;
   3: begin
      ArquivoXML.Text := '<CompNfse xmlns="http://www.abrasf.org.br/ABRASF/arquivos/">' +
                           ArquivoXML.Text +
                         '</CompNfse>';
      while pos('</CompNfse>', ArquivoXML.Text) > 0 do
       begin
        XML             := copy(ArquivoXML.Text, 1, pos('</CompNfse>', ArquivoXML.Text) + 10);
        ArquivoXML.Text := Trim(copy(ArquivoXML.Text, pos('</CompNfse>', ArquivoXML.Text) + 11, length(ArquivoXML.Text)));
        LocNFSeR        := TNFSeR.Create(Self.Add.NFSe);
        try
         LocNFSeR.Leitor.Arquivo := XML;
         LocNFSeR.VersaoXML      := NotaUtil.VersaoXML(XML);
         LocNFSeR.TabServicosExt := self.Configuracoes.Arquivos.TabServicosExt;
         LocNFSeR.Provedor       := self.Configuracoes.WebServices.Provedor;
         LocNFSeR.LerXml;
         Items[Self.Count-1].XML_NFSe := LocNFSeR.Leitor.Arquivo;
         Items[Self.Count-1].NomeArq := CaminhoArquivo;
        finally
         LocNFSeR.Free;
        end;
       end;
      end;
   4: begin
       if Trim(RazaoSocial) = '' then
          Prestador := Copy(ArquivoXML.Text,
                      Pos('<Prestador>', ArquivoXML.Text) + 11,
                      Pos('</Prestador>',ArquivoXML.Text) - (Pos('<Prestador>', ArquivoXML.Text) + 11));

          RazaoSocial := Copy(Prestador,
                      Pos('<RazaoSocial>', Prestador) + 13,
                      Pos('</RazaoSocial>',Prestador) - (Pos('<RazaoSocial>', Prestador) + 13));

       if pos('</InfDeclaracaoPrestacaoServico>',ArquivoXML.Text) > 0
        then begin
         while pos('</InfDeclaracaoPrestacaoServico>',ArquivoXML.Text) > 0 do
          begin
           XML             := copy(ArquivoXML.Text, 1, pos('</InfDeclaracaoPrestacaoServico>', ArquivoXML.Text) + 37);
           ArquivoXML.Text := Trim(copy(ArquivoXML.Text, pos('</InfDeclaracaoPrestacaoServico>', ArquivoXML.Text) + 38, length(ArquivoXML.Text)));
           LocNFSeR        := TNFSeR.Create(Self.Add.NFSe);
           try
            LocNFSeR.Leitor.Arquivo := XML;
            LocNFSeR.VersaoXML      := NotaUtil.VersaoXML(XML);
            LocNFSeR.TabServicosExt := self.Configuracoes.Arquivos.TabServicosExt;
            LocNFSeR.Provedor       := self.Configuracoes.WebServices.Provedor;
            LocNFSeR.LerXml;
            LocNFSeR.NFSe.PrestadorServico.RazaoSocial := RazaoSocial;
            Items[Self.Count-1].XML_Rps := LocNFSeR.Leitor.Arquivo;
            Items[Self.Count-1].NomeArq := CaminhoArquivo;
           finally
            LocNFSeR.Free;
           end;
          end;
        end
        else begin
         while pos('</Rps>',ArquivoXML.Text) > 0 do
          begin
           XML             := copy(ArquivoXML.Text, 1, pos('</Rps>', ArquivoXML.Text) + 5);
           ArquivoXML.Text := Trim(copy(ArquivoXML.Text, pos('</Rps>', ArquivoXML.Text) + 6, length(ArquivoXML.Text)));
           LocNFSeR        := TNFSeR.Create(Self.Add.NFSe);
           try
            LocNFSeR.Leitor.Arquivo := XML;
            LocNFSeR.VersaoXML      := NotaUtil.VersaoXML(XML);
            LocNFSeR.TabServicosExt := self.Configuracoes.Arquivos.TabServicosExt;
            LocNFSeR.Provedor       := self.Configuracoes.WebServices.Provedor;
            LocNFSeR.LerXml;
            Items[Self.Count-1].XML_Rps := LocNFSeR.Leitor.Arquivo;
            Items[Self.Count-1].NomeArq := CaminhoArquivo;
           finally
            LocNFSeR.Free;
           end;
          end;
        end;
      end;
   5: begin
       NumeroRPS := Copy(ArquivoXML.Text,
                    Pos('<Numero>', ArquivoXML.Text) + 8,
                    Pos('</Numero>',ArquivoXML.Text) - (Pos('<Numero>', ArquivoXML.Text) + 8));

       SerieRPS := Copy(ArquivoXML.Text,
                    Pos('<Serie>', ArquivoXML.Text) + 7,
                    Pos('</Serie>',ArquivoXML.Text) - (Pos('<Serie>', ArquivoXML.Text) + 7));

       TipoRPS := Copy(ArquivoXML.Text,
                    Pos('<Tipo>', ArquivoXML.Text) + 6,
                    Pos('</Tipo>',ArquivoXML.Text) - (Pos('<Tipo>', ArquivoXML.Text) + 6));

       CNPJ := Copy(ArquivoXML.Text,
                    Pos('<Prestador><CpfCnpj><Cnpj>', ArquivoXML.Text) + 26,
                    Pos('</Cnpj></CpfCnpj>',ArquivoXML.Text) - (Pos('<Prestador><CpfCnpj><Cnpj>', ArquivoXML.Text) + 26));

       if ((Length(CNPJ) <> 11) and (Length(CNPJ) <> 14)) then
          CNPJ:= Copy(ArquivoXML.Text,
                      Pos('<Prestador><CpfCnpj><Cpf>', ArquivoXML.Text) + 25,
                      Pos('</Cpf></CpfCnpj>',ArquivoXML.Text) - (Pos('<Prestador><CpfCnpj><Cpf>', ArquivoXML.Text) + 25));

       if ((Length(CNPJ) <> 11) and (Length(CNPJ) <> 14)) then
          CNPJ := Copy(ArquivoXML.Text,
                      Pos('<Prestador><Cnpj>', ArquivoXML.Text) + 17,
                      Pos('</Cnpj>',ArquivoXML.Text) - (Pos('<Prestador><Cnpj>', ArquivoXML.Text) + 17));

       if ((Length(CNPJ) <> 11) and (Length(CNPJ) <> 14)) then
          CNPJ:= Copy(ArquivoXML.Text,
                      Pos('<Prestador><Cpf>', ArquivoXML.Text) + 16,
                      Pos('</Cpf>',ArquivoXML.Text) - (Pos('<Prestador><Cpf>', ArquivoXML.Text) + 16));

       if ((Length(CNPJ) <> 11) and (Length(CNPJ) <> 14)) then
          CNPJ := Copy(ArquivoXML.Text,
                       Pos('<IdentificacaoPrestador><CpfCnpj>', ArquivoXML.Text) + 33,
                       Pos('</CpfCnpj>', ArquivoXML.Text) - (Pos('<IdentificacaoPrestador><CpfCnpj>', ArquivoXML.Text) + 33));

       IM := Copy(ArquivoXML.Text,
                  Pos('<InscricaoMunicipal>', ArquivoXML.Text) + 20,
                  Pos('</InscricaoMunicipal>',ArquivoXML.Text) - (Pos('<InscricaoMunicipal>', ArquivoXML.Text) + 20));

       CodigoMunicipio := Copy(ArquivoXML.Text,
                   Pos('<CodigoMunicipio>', ArquivoXML.Text) + 17,
                   Pos('</CodigoMunicipio>',ArquivoXML.Text) - (Pos('<CodigoMunicipio>', ArquivoXML.Text) + 17));

       if pos('</InfDeclaracaoPrestacaoServico>',ArquivoXML.Text) > 0
        then begin
         while pos('</InfDeclaracaoPrestacaoServico>',ArquivoXML.Text) > 0 do
          begin
           XML             := copy(ArquivoXML.Text, 1, pos('</InfDeclaracaoPrestacaoServico>', ArquivoXML.Text) + 37);
           ArquivoXML.Text := Trim(copy(ArquivoXML.Text, pos('</InfDeclaracaoPrestacaoServico>', ArquivoXML.Text) + 38, length(ArquivoXML.Text)));

           LocNFSeR        := TNFSeR.Create(Self.Add.NFSe);
           LocNFSeR.NFSe.Numero := NumeroRPS;
           LocNFSeR.NFSe.IdentificacaoRps.Numero := NumeroRPS;
           LocNFSeR.NFSe.IdentificacaoRps.Serie := SerieRPS;
           LocNFSeR.NFSe.IdentificacaoRps.Tipo := StrToTipoRPS(ok, TipoRPS);
           LocNFSeR.NFSe.Prestador.Cnpj:= CNPJ;
           LocNFSeR.NFSe.Prestador.InscricaoMunicipal:= IM;
           LocNFSeR.NFSe.PrestadorServico.Endereco.CodigoMunicipio:= CodigoMunicipio;
           try
            LocNFSeR.Leitor.Arquivo := XML;
            LocNFSeR.VersaoXML      := NotaUtil.VersaoXML(XML);
            LocNFSeR.TabServicosExt := self.Configuracoes.Arquivos.TabServicosExt;
            LocNFSeR.Provedor       := self.Configuracoes.WebServices.Provedor;
            LocNFSeR.LerXml;
            Items[Self.Count-1].XML_Rps := LocNFSeR.Leitor.Arquivo;
            Items[Self.Count-1].NomeArq := CaminhoArquivo;
           finally
            LocNFSeR.Free;
           end;
          end;
        end
        else begin
         while pos('</Rps>', ArquivoXML.Text) > 0 do
          begin
           XML             := copy(ArquivoXML.Text, 1, pos('</Rps>', ArquivoXML.Text) + 5);
           ArquivoXML.Text := Trim(copy(ArquivoXML.Text, pos('</Rps>',ArquivoXML.Text) + 6, length(ArquivoXML.Text)));

           LocNFSeR        := TNFSeR.Create(Self.Add.NFSe);
           LocNFSeR.NFSe.IdentificacaoRps.Numero := NumeroRPS;
           LocNFSeR.NFSe.IdentificacaoRps.Serie := SerieRPS;
           LocNFSeR.NFSe.IdentificacaoRps.Tipo := StrToTipoRPS(ok, TipoRPS);
           LocNFSeR.NFSe.Prestador.Cnpj:= CNPJ;
           LocNFSeR.NFSe.Prestador.InscricaoMunicipal:= IM;
           try
            LocNFSeR.Leitor.Arquivo := XML;
            LocNFSeR.VersaoXML      := NotaUtil.VersaoXML(XML);
            LocNFSeR.TabServicosExt := self.Configuracoes.Arquivos.TabServicosExt;
            LocNFSeR.Provedor       := self.Configuracoes.WebServices.Provedor;
            LocNFSeR.LerXml;
            Items[Self.Count-1].XML_Rps := LocNFSeR.Leitor.Arquivo;
            Items[Self.Count-1].NomeArq := CaminhoArquivo;
           finally
            LocNFSeR.Free;
           end;
          end;
        end;
      end;
   6: begin //Equiplano
        CNPJ:= Copy(ArquivoXML.Text,
                    Pos('<nrCnpj>', ArquivoXML.Text) + 8,
                    Pos('</nrCnpj>',ArquivoXML.Text) - (Pos('<nrCnpj>', ArquivoXML.Text) + 8));
        IM:= Copy(ArquivoXML.Text,
                  Pos('<nrInscricaoMunicipal>', ArquivoXML.Text) + 22,
                  Pos('</nrInscricaoMunicipal>',ArquivoXML.Text) - (Pos('<nrInscricaoMunicipal>', ArquivoXML.Text) + 22));

        while pos('</rps>',ArquivoXML.Text) > 0 do
          begin
            XML            := Copy(ArquivoXML.Text, Pos('<rps>', ArquivoXML.Text), Pos('</rps>', ArquivoXML.Text) + 5);
            ArquivoXML.Text:= Trim(Copy(ArquivoXML.Text, Pos('</rps>', ArquivoXML.Text) + 6, Length(ArquivoXML.Text)));
            LocNFSeR       := TNFSeR.Create(Self.Add.NFSe);
            //LocNFSeR.Provedor:= proEquiplano;
            LocNFSeR.NFSe.Prestador.Cnpj:= CNPJ;
            LocNFSeR.NFSe.Prestador.InscricaoMunicipal:= IM;
            try
              LocNFSeR.Leitor.Arquivo := XML;
              LocNFSeR.VersaoXML      := NotaUtil.VersaoXML(XML);
              LocNFSeR.TabServicosExt := self.Configuracoes.Arquivos.TabServicosExt;
              LocNFSeR.Provedor       := self.Configuracoes.WebServices.Provedor;
              LocNFSeR.LerXml;
              Items[Self.Count-1].XML_Rps := LocNFSeR.Leitor.Arquivo;
              Items[Self.Count-1].NomeArq := CaminhoArquivo;
            finally
              LocNFSeR.Free;
            end;
          end;
      end;
   7: begin //Infisc
        CNPJ:= Copy(ArquivoXML.Text,
                    Pos('<CNPJ>', ArquivoXML.Text) + 6,
                    Pos('</CNPJ>',ArquivoXML.Text) - (Pos('<CNPJ>', ArquivoXML.Text) + 6));
        IM:= Copy(ArquivoXML.Text,
                  Pos('<IM>', ArquivoXML.Text) + 4,
                  Pos('</IM>',ArquivoXML.Text) - (Pos('<IM>', ArquivoXML.Text) + 4));

        while pos('</NFS-e>',ArquivoXML.Text) > 0 do
          begin
            XML            := Copy(ArquivoXML.Text, Pos('<NFS-e>', ArquivoXML.Text), Pos('</NFS-e>', ArquivoXML.Text) + 7);
            ArquivoXML.Text:= ''; //Trim(Copy(ArquivoXML.Text, Pos('</rps>', ArquivoXML.Text) + 6, Length(ArquivoXML.Text)));
            LocNFSeR       := TNFSeR.Create(Self.Add.NFSe);
            LocNFSeR.Provedor:= proInfisc;
            LocNFSeR.NFSe.Prestador.Cnpj:= CNPJ;
            LocNFSeR.NFSe.Prestador.InscricaoMunicipal:= IM;
            try
              LocNFSeR.Leitor.Arquivo := XML;
              LocNFSeR.VersaoXML      := NotaUtil.VersaoXML(XML);
              LocNFSeR.TabServicosExt := self.Configuracoes.Arquivos.TabServicosExt;
              LocNFSeR.Provedor       := self.Configuracoes.WebServices.Provedor;
              LocNFSeR.LerXml;
              Items[Self.Count-1].XML_Rps := LocNFSeR.Leitor.Arquivo;
              Items[Self.Count-1].NomeArq := CaminhoArquivo;
            finally
              LocNFSeR.Free;
            end;
          end;
      end;
  end;

  if pos('</Notas>', ArquivoXML.Text)> 0 then begin
    while pos('</Notas>', ArquivoXML.Text) > 0 do begin
      XML             := copy(ArquivoXML.Text, 1, pos('</Notas>', ArquivoXML.Text) + 5);
      ArquivoXML.Text := Trim(copy(ArquivoXML.Text, pos('</Notas>',ArquivoXML.Text) + 6, length(ArquivoXML.Text)));
      LocNFSeR        := TNFSeR.Create(Self.Add.NFSe);
      try
       LocNFSeR.Leitor.Arquivo := XML;
       LocNFSeR.VersaoXML      := NotaUtil.VersaoXML(XML);
       LocNFSeR.TabServicosExt := self.Configuracoes.Arquivos.TabServicosExt;
       LocNFSeR.Provedor       := self.Configuracoes.WebServices.Provedor;
       LocNFSeR.LerXml;
       Items[Self.Count-1].XML_Rps := LocNFSeR.Leitor.Arquivo;
       Items[Self.Count-1].NomeArq := CaminhoArquivo;
      finally
       LocNFSeR.Free;
      end;
    end;
  end;

  ArquivoXML.Free;
 except
  raise;
  Result := False;
 end;
end;

function TNotasFiscais.LoadFromStream(Stream: TStringStream): boolean;
var
 LocNFSeR : TNFSeR;
 ArquivoXML: TStringList;
 XML : AnsiString;
 Tipo: Integer;
{$IFDEF DELPHI2009_UP}
 Encoding : TEncoding;
{$ENDIF}
 CodigoMunicipio, Prestador, RazaoSocial,
 CNPJ, IM, NumeroRPS, SerieRPS, TipoRPS: String;
 ok: Boolean;
begin
  try
   Result     := True;
   LocNFSeR   := TNFSeR.Create(Self.Add.NFSe);
   ArquivoXML := TStringList.Create;

   LocNFSeR.Leitor.CarregarArquivo(Stream);
   ArquivoXML.Text := LocNFSeR.Leitor.Arquivo;
   ArquivoXML.Text := StringReplace(StringReplace( ArquivoXML.Text, '&lt;', '<', [rfReplaceAll]), '&gt;', '>', [rfReplaceAll]);
   ArquivoXML.Text := NotaUtil.RetirarPrefixos(ArquivoXML.Text);

  if pos('</CompNfse>', ArquivoXML.Text) > 0
   then Tipo := 1
   else if pos('</ComplNfse>', ArquivoXML.Text) > 0
         then Tipo := 2
         else if pos('</Nfse>', ArquivoXML.Text) > 0
               then Tipo := 3
               else if pos('</ListaRps>', ArquivoXML.Text) > 0
                    then Tipo := 4
                    else if pos('</Rps>', ArquivoXML.Text) > 0
                         then Tipo := 5
                         else if pos('</listaRps>', ArquivoXML.Text) > 0
                              then Tipo := 6
                              else if pos('</RPS>', ArquivoXML.Text) > 0
                                then Tipo := 7 //issDSF
                                else if pos('</Nota>', ArquivoXML.Text) > 0
                                  then Tipo := 8 //issDSF
                                  else Tipo := 0;

  case Tipo of
   1: begin
       while pos('</CompNfse>', ArquivoXML.Text) > 0 do
        begin
         XML             := copy(ArquivoXML.Text, 1, pos('</CompNfse>', ArquivoXML.Text) + 10);
         ArquivoXML.Text := Trim(copy(ArquivoXML.Text, pos('</CompNfse>', ArquivoXML.Text) + 11, length(ArquivoXML.Text)));
         try
          LocNFSeR.Leitor.Arquivo := XML;
          LocNFSeR.VersaoXML      := NotaUtil.VersaoXML(XML);
          LocNFSeR.TabServicosExt := self.Configuracoes.Arquivos.TabServicosExt;
          LocNFSeR.Provedor       := self.Configuracoes.WebServices.Provedor;
          LocNFSeR.LerXml;
          Items[Self.Count-1].XML_NFSe := LocNFSeR.Leitor.Arquivo;
         finally
          LocNFSeR.Free;
         end;
        end;
      end;
   2: begin
       while pos('</ComplNfse>', ArquivoXML.Text) > 0 do
        begin
         XML             := copy(ArquivoXML.Text, 1, pos('</ComplNfse>', ArquivoXML.Text) + 11);
         ArquivoXML.Text := Trim(copy(ArquivoXML.Text, pos('</ComplNfse>', ArquivoXML.Text) + 12, length(ArquivoXML.Text)));
         try
          LocNFSeR.Leitor.Arquivo := XML;
          LocNFSeR.VersaoXML      := NotaUtil.VersaoXML(XML);
          LocNFSeR.TabServicosExt := self.Configuracoes.Arquivos.TabServicosExt;
          LocNFSeR.Provedor       := self.Configuracoes.WebServices.Provedor;
          LocNFSeR.LerXml;
          Items[Self.Count-1].XML_NFSe := LocNFSeR.Leitor.Arquivo;
         finally
          LocNFSeR.Free;
         end;
        end;
      end;
   3: begin
      ArquivoXML.Text := '<CompNfse xmlns="http://www.abrasf.org.br/ABRASF/arquivos/">' +
                           ArquivoXML.Text +
                         '</CompNfse>';
      while pos('</CompNfse>', ArquivoXML.Text) > 0 do
       begin
        XML             := copy(ArquivoXML.Text, 1, pos('</CompNfse>', ArquivoXML.Text) + 10);
        ArquivoXML.Text := Trim(copy(ArquivoXML.Text, pos('</CompNfse>', ArquivoXML.Text) + 11, length(ArquivoXML.Text)));
        try
         LocNFSeR.Leitor.Arquivo := XML;
         LocNFSeR.VersaoXML      := NotaUtil.VersaoXML(XML);
         LocNFSeR.TabServicosExt := self.Configuracoes.Arquivos.TabServicosExt;
         LocNFSeR.Provedor       := self.Configuracoes.WebServices.Provedor;
         LocNFSeR.LerXml;
         Items[Self.Count-1].XML_NFSe := LocNFSeR.Leitor.Arquivo;
        finally
         LocNFSeR.Free;
        end;
       end;
      end;
   4: begin
       if Trim(RazaoSocial) = '' then
          Prestador := Copy(ArquivoXML.Text,
                      Pos('<Prestador>', ArquivoXML.Text) + 11,
                      Pos('</Prestador>',ArquivoXML.Text) - (Pos('<Prestador>', ArquivoXML.Text) + 11));

          RazaoSocial := Copy(Prestador,
                      Pos('<RazaoSocial>', Prestador) + 13,
                      Pos('</RazaoSocial>',Prestador) - (Pos('<RazaoSocial>', Prestador) + 13));

       if pos('</InfDeclaracaoPrestacaoServico>',ArquivoXML.Text) > 0
        then begin
         while pos('</InfDeclaracaoPrestacaoServico>',ArquivoXML.Text) > 0 do
          begin
           XML             := copy(ArquivoXML.Text, 1, pos('</InfDeclaracaoPrestacaoServico>', ArquivoXML.Text) + 37);
           ArquivoXML.Text := Trim(copy(ArquivoXML.Text, pos('</InfDeclaracaoPrestacaoServico>', ArquivoXML.Text) + 38, length(ArquivoXML.Text)));
           try
            LocNFSeR.Leitor.Arquivo := XML;
            LocNFSeR.VersaoXML      := NotaUtil.VersaoXML(XML);
            LocNFSeR.TabServicosExt := self.Configuracoes.Arquivos.TabServicosExt;
            LocNFSeR.Provedor       := self.Configuracoes.WebServices.Provedor;
            LocNFSeR.LerXml;
            LocNFSeR.NFSe.PrestadorServico.RazaoSocial := RazaoSocial;
            Items[Self.Count-1].XML_Rps := LocNFSeR.Leitor.Arquivo;
           finally
            LocNFSeR.Free;
           end;
          end;
        end
        else begin
         while pos('</Rps>',ArquivoXML.Text) > 0 do
          begin
           XML             := copy(ArquivoXML.Text, 1, pos('</Rps>', ArquivoXML.Text) + 5);
           ArquivoXML.Text := Trim(copy(ArquivoXML.Text, pos('</Rps>', ArquivoXML.Text) + 6, length(ArquivoXML.Text)));
           try
            LocNFSeR.Leitor.Arquivo := XML;
            LocNFSeR.VersaoXML      := NotaUtil.VersaoXML(XML);
            LocNFSeR.TabServicosExt := self.Configuracoes.Arquivos.TabServicosExt;
            LocNFSeR.Provedor       := self.Configuracoes.WebServices.Provedor;
            LocNFSeR.LerXml;
            Items[Self.Count-1].XML_Rps := LocNFSeR.Leitor.Arquivo;
           finally
            LocNFSeR.Free;
           end;
          end;
        end;
      end;
   5: begin
       NumeroRPS := Copy(ArquivoXML.Text,
                    Pos('<Numero>', ArquivoXML.Text) + 8,
                    Pos('</Numero>',ArquivoXML.Text) - (Pos('<Numero>', ArquivoXML.Text) + 8));

       SerieRPS := Copy(ArquivoXML.Text,
                    Pos('<Serie>', ArquivoXML.Text) + 7,
                    Pos('</Serie>',ArquivoXML.Text) - (Pos('<Serie>', ArquivoXML.Text) + 7));

       TipoRPS := Copy(ArquivoXML.Text,
                    Pos('<Tipo>', ArquivoXML.Text) + 6,
                    Pos('</Tipo>',ArquivoXML.Text) - (Pos('<Tipo>', ArquivoXML.Text) + 6));

       CNPJ := Copy(ArquivoXML.Text,
                    Pos('<Prestador><CpfCnpj><Cnpj>', ArquivoXML.Text) + 26,
                    Pos('</Cnpj></CpfCnpj>',ArquivoXML.Text) - (Pos('<Prestador><CpfCnpj><Cnpj>', ArquivoXML.Text) + 26));

       if ((Length(CNPJ) <> 11) and (Length(CNPJ) <> 14)) then
          CNPJ:= Copy(ArquivoXML.Text,
                      Pos('<Prestador><CpfCnpj><Cpf>', ArquivoXML.Text) + 25,
                      Pos('</Cpf></CpfCnpj>',ArquivoXML.Text) - (Pos('<Prestador><CpfCnpj><Cpf>', ArquivoXML.Text) + 25));

       if ((Length(CNPJ) <> 11) and (Length(CNPJ) <> 14)) then
          CNPJ := Copy(ArquivoXML.Text,
                      Pos('<Prestador><Cnpj>', ArquivoXML.Text) + 17,
                      Pos('</Cnpj>',ArquivoXML.Text) - (Pos('<Prestador><Cnpj>', ArquivoXML.Text) + 17));

       if ((Length(CNPJ) <> 11) and (Length(CNPJ) <> 14)) then
          CNPJ:= Copy(ArquivoXML.Text,
                      Pos('<Prestador><Cpf>', ArquivoXML.Text) + 16,
                      Pos('</Cpf>',ArquivoXML.Text) - (Pos('<Prestador><Cpf>', ArquivoXML.Text) + 16));

       IM := Copy(ArquivoXML.Text,
                  Pos('<InscricaoMunicipal>', ArquivoXML.Text) + 20,
                  Pos('</InscricaoMunicipal>',ArquivoXML.Text) - (Pos('<InscricaoMunicipal>', ArquivoXML.Text) + 20));

       CodigoMunicipio := Copy(ArquivoXML.Text,
                   Pos('<CodigoMunicipio>', ArquivoXML.Text) + 17,
                   Pos('</CodigoMunicipio>',ArquivoXML.Text) - (Pos('<CodigoMunicipio>', ArquivoXML.Text) + 17));

       if pos('</InfDeclaracaoPrestacaoServico>',ArquivoXML.Text) > 0
        then begin
         while pos('</InfDeclaracaoPrestacaoServico>',ArquivoXML.Text) > 0 do
          begin
           XML             := copy(ArquivoXML.Text, 1, pos('</InfDeclaracaoPrestacaoServico>', ArquivoXML.Text) + 37);
           ArquivoXML.Text := Trim(copy(ArquivoXML.Text, pos('</InfDeclaracaoPrestacaoServico>', ArquivoXML.Text) + 38, length(ArquivoXML.Text)));

           LocNFSeR.NFSe.Numero := NumeroRPS;
           LocNFSeR.NFSe.IdentificacaoRps.Numero := NumeroRPS;
           LocNFSeR.NFSe.IdentificacaoRps.Serie := SerieRPS;
           LocNFSeR.NFSe.IdentificacaoRps.Tipo := StrToTipoRPS(ok, TipoRPS);
           LocNFSeR.NFSe.Prestador.Cnpj:= CNPJ;
           LocNFSeR.NFSe.Prestador.InscricaoMunicipal:= IM;
           LocNFSeR.NFSe.PrestadorServico.Endereco.CodigoMunicipio:= CodigoMunicipio;
           try
            LocNFSeR.Leitor.Arquivo := XML;
            LocNFSeR.VersaoXML      := NotaUtil.VersaoXML(XML);
            LocNFSeR.TabServicosExt := self.Configuracoes.Arquivos.TabServicosExt;
            LocNFSeR.Provedor       := self.Configuracoes.WebServices.Provedor;
            LocNFSeR.LerXml;
            Items[Self.Count-1].XML_Rps := LocNFSeR.Leitor.Arquivo;
           finally
            LocNFSeR.Free;
           end;
          end;
        end
        else begin
         while pos('</Rps>', ArquivoXML.Text) > 0 do
          begin
           XML             := copy(ArquivoXML.Text, 1, pos('</Rps>', ArquivoXML.Text) + 5);
           ArquivoXML.Text := Trim(copy(ArquivoXML.Text, pos('</Rps>',ArquivoXML.Text) + 6, length(ArquivoXML.Text)));

           LocNFSeR.NFSe.IdentificacaoRps.Numero := NumeroRPS;
           LocNFSeR.NFSe.IdentificacaoRps.Serie := SerieRPS;
           LocNFSeR.NFSe.IdentificacaoRps.Tipo := StrToTipoRPS(ok, TipoRPS);
           LocNFSeR.NFSe.Prestador.Cnpj:= CNPJ;
           LocNFSeR.NFSe.Prestador.InscricaoMunicipal:= IM;
           try
            LocNFSeR.Leitor.Arquivo := XML;
            LocNFSeR.VersaoXML      := NotaUtil.VersaoXML(XML);
            LocNFSeR.TabServicosExt := self.Configuracoes.Arquivos.TabServicosExt;
            LocNFSeR.Provedor       := self.Configuracoes.WebServices.Provedor;
            LocNFSeR.LerXml;
            Items[Self.Count-1].XML_Rps := LocNFSeR.Leitor.Arquivo;
           finally
            LocNFSeR.Free;
           end;
          end;
        end;
      end;
   6: begin //Equiplano
        CNPJ:= Copy(ArquivoXML.Text,
                    Pos('<nrCnpj>', ArquivoXML.Text) + 8,
                    Pos('</nrCnpj>',ArquivoXML.Text) - (Pos('<nrCnpj>', ArquivoXML.Text) + 8));
        IM:= Copy(ArquivoXML.Text,
                  Pos('<nrInscricaoMunicipal>', ArquivoXML.Text) + 22,
                  Pos('</nrInscricaoMunicipal>',ArquivoXML.Text) - (Pos('<nrInscricaoMunicipal>', ArquivoXML.Text) + 22));

        while pos('</rps>',ArquivoXML.Text) > 0 do
          begin
            XML            := Copy(ArquivoXML.Text, Pos('<rps>', ArquivoXML.Text), Pos('</rps>', ArquivoXML.Text) + 5);
            ArquivoXML.Text:= Trim(Copy(ArquivoXML.Text, Pos('</rps>', ArquivoXML.Text) + 6, Length(ArquivoXML.Text)));
            //LocNFSeR.Provedor:= proEquiplano;
            LocNFSeR.NFSe.Prestador.Cnpj:= CNPJ;
            LocNFSeR.NFSe.Prestador.InscricaoMunicipal:= IM;
            try
              LocNFSeR.Leitor.Arquivo := XML;
              LocNFSeR.VersaoXML      := NotaUtil.VersaoXML(XML);
              LocNFSeR.TabServicosExt := self.Configuracoes.Arquivos.TabServicosExt;
              LocNFSeR.Provedor       := self.Configuracoes.WebServices.Provedor;
              LocNFSeR.LerXml;
              Items[Self.Count-1].XML_Rps := LocNFSeR.Leitor.Arquivo;
            finally
              LocNFSeR.Free;
            end;
          end;
      end;
   7: begin //IssDSF
        while pos('</RPS>',ArquivoXML.Text) > 0 do
          begin
            XML            := Copy(ArquivoXML.Text, Pos('<RPS>', ArquivoXML.Text), Pos('</RPS>', ArquivoXML.Text) + 5);
            ArquivoXML.Text:= Trim(Copy(ArquivoXML.Text, Pos('</RPS>', ArquivoXML.Text) + 6, Length(ArquivoXML.Text)));
            try
              LocNFSeR.Leitor.Arquivo := XML;
              LocNFSeR.VersaoXML      := NotaUtil.VersaoXML(XML);
              LocNFSeR.TabServicosExt := self.Configuracoes.Arquivos.TabServicosExt;
              LocNFSeR.Provedor       := self.Configuracoes.WebServices.Provedor;
              LocNFSeR.LerXml;
              Items[Self.Count-1].XML_Rps := LocNFSeR.Leitor.Arquivo;
            finally
              LocNFSeR.Free;
            end;
          end;
      end;
   8: begin //IssDSF
        try
          LocNFSeR.Leitor.Arquivo := ArquivoXML.Text;
          LocNFSeR.Provedor:= proIssDSF;
          LocNFSeR.VersaoXML      := NotaUtil.VersaoXML(ArquivoXML.Text);
          LocNFSeR.TabServicosExt := self.Configuracoes.Arquivos.TabServicosExt;
          LocNFSeR.Provedor       := self.Configuracoes.WebServices.Provedor;
          LocNFSeR.LerXml;
          Items[Self.Count-1].XML_Rps := LocNFSeR.Leitor.Arquivo;
        finally
          LocNFSeR.Free;
        end;
      end;
  end;

  if pos('</Notas>', ArquivoXML.Text)> 0 then begin
    while pos('</Notas>', ArquivoXML.Text) > 0 do begin
      XML             := copy(ArquivoXML.Text, 1, pos('</Notas>', ArquivoXML.Text) + 5);
      ArquivoXML.Text := Trim(copy(ArquivoXML.Text, pos('</Notas>',ArquivoXML.Text) + 6, length(ArquivoXML.Text)));
      try
       LocNFSeR.Leitor.Arquivo := XML;
       LocNFSeR.VersaoXML      := NotaUtil.VersaoXML(XML);
       LocNFSeR.TabServicosExt := self.Configuracoes.Arquivos.TabServicosExt;
       LocNFSeR.Provedor       := self.Configuracoes.WebServices.Provedor;
       LocNFSeR.LerXml;
       Items[Self.Count-1].XML_Rps := LocNFSeR.Leitor.Arquivo;
      finally
       LocNFSeR.Free;
      end;
    end;
  end;

  ArquivoXML.Free;
 except
  raise;
  Result := False;
 end;

end;

function TNotasFiscais.LoadFromString(AString: string): boolean;
var
  XMLNFe: TStringStream;
begin
  try
    Result := True;

    XMLNFe := TStringStream.Create(AString);
    try
      XMLNFe.WriteString(AString);
      Result := LoadFromStream(XMLNFe);
    finally
      XMLNFe.Free;
    end;
  except
    Result := False;
  end;
end;

function TNotasFiscais.SaveToFile(PathArquivo: string): boolean;
var
 i              : integer;
 CaminhoArquivo : String;
begin
 Result := True;
 try
  for i := 0 to TACBrNFSe( FACBrNFSe ).NotasFiscais.Count-1 do
   begin
    if EstaVazio(PathArquivo)
     then PathArquivo := TACBrNFSe( FACBrNFSe ).Configuracoes.Geral.PathSalvar
     else PathArquivo := ExtractFilePath(PathArquivo);

    CaminhoArquivo := NotaUtil.PathWithDelim(PathArquivo) + (TACBrNFSe( FACBrNFSe ).NotasFiscais.NumeroLote)+'-LoteNFSe.xml';
    TACBrNFSe( FACBrNFSe ).NotasFiscais.Items[i].SaveToFile(CaminhoArquivo);
   end;
 except
  Result := False;
 end;
end;

procedure TNotasFiscais.SetItem(Index: Integer; const Value: NotaFiscal);
begin
 Items[Index].Assign(Value);
end;

procedure TNotasFiscais.Valida;
var
 i    : Integer;
 XML,
 FMsg : AnsiString;
begin
 for i := 0 to Self.Count-1 do
  begin

//   if pos('<Signature', Self.Items[i].XML_Rps_Ass {.XML_Rps}) = 0
//    then Assinar;

//   XML := '<Rps>' + RetornarConteudoEntre(Self.Items[i].XML_Rps_Ass, '<Rps', '</Rps>')+ '</Rps>';
   XML := Self.Items[i].XML_Rps;

   if not(NotaUtil.Valida(XML, FMsg,
                          Self.FConfiguracoes.Geral.PathSchemas,
                          Self.FConfiguracoes.WebServices.URL,
                          Self.FConfiguracoes.WebServices.ServicoEnviar))
    then raise Exception.Create('Falha na validação dos dados da nota ' +
                 Self.Items[i].NFSe.IdentificacaoRps.Numero + sLineBreak+Self.Items[i].Alertas+FMsg);
 end;
end;

{ TSendMailThread }

constructor TSendMailThread.Create{(AOwner: NotaFiscal)};
begin
 smtp        := TSMTPSend.Create;
 slmsg_Lines := TStringList.Create;
 sCC         := TStringList.Create;
 sFrom       := '';
 sTo         := '';

 FreeOnTerminate := True;

  inherited Create(True);
end;

destructor TSendMailThread.Destroy;
begin
 slmsg_Lines.Free;
 sCC.Free;
 smtp.Free;

  inherited;
end;

procedure TSendMailThread.DoHandleException;
begin
 // TACBrNFSe(TNotasFiscais(FOwner.GetOwner).ACBrNFSe).SetStatus( stNFSeIdle );

 // FOwner.Alertas := FException.Message;

 if FException is Exception
  then Application.ShowException(FException)
  else SysUtils.ShowException(FException, nil);
end;

procedure TSendMailThread.Execute;
var
 I: integer;
begin
  inherited;

 try
  Terminado := False;
  try
   if not smtp.Login()
    then raise Exception.Create('SMTP ERROR: Login:' + smtp.EnhCodeString+sLineBreak+smtp.FullResult.Text);

   if not smtp.MailFrom( sFrom, Length(sFrom))
    then raise Exception.Create('SMTP ERROR: MailFrom:' + smtp.EnhCodeString+sLineBreak+smtp.FullResult.Text);

   if not smtp.MailTo(sTo)
    then raise Exception.Create('SMTP ERROR: MailTo:' + smtp.EnhCodeString+sLineBreak+smtp.FullResult.Text);

   if (sCC <> nil)
    then begin
     for I := 0 to sCC.Count - 1 do
      begin
       if not smtp.MailTo(sCC.Strings[i])
        then raise Exception.Create('SMTP ERROR: MailTo:' + smtp.EnhCodeString+sLineBreak+smtp.FullResult.Text);
      end;
    end;

   if not smtp.MailData(slmsg_Lines)
    then raise Exception.Create('SMTP ERROR: MailData:' + smtp.EnhCodeString+sLineBreak+smtp.FullResult.Text);

   if not smtp.Logout()
    then raise Exception.Create('SMTP ERROR: Logout:' + smtp.EnhCodeString+sLineBreak+smtp.FullResult.Text);
  finally
   try
    smtp.Sock.CloseSocket;
   except
   end ;
   Terminado := True;
  end;
 except
  Terminado := True;
  HandleException;
 end;
end;

procedure TSendMailThread.HandleException;
begin
 FException := Exception(ExceptObject);
 try
  // Não mostra mensagens de EAbort
  if not (FException is EAbort)
   then Synchronize(DoHandleException);
 finally
  FException := nil;
 end;
end;

end.
