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

unit ACBrNFSe;

interface

uses
  Classes, Sysutils,
  pnfsNFSe, pnfsConversao,
{$IFDEF CLX}
  QDialogs,
{$ELSE}
  Dialogs,
{$ENDIF}
  ACBrNFSeNotasFiscais, ACBrNFSeConfiguracoes, ACBrNFSeWebServices,
  ACBrDFeUtil, ACBrUtil, ACBrNFSeUtil, ACBrNFSeDANFSeClass, Forms,
  smtpsend, ssl_openssl, mimemess, mimepart; // units para enviar email

const
  ACBrNFSe_VERSAO = '0.2.0';

type
  TACBrNFSeAboutInfo = (ACBrNFSeAbout);

  // Evento para gerar log das mensagens do Componente
  TACBrNFSeLog = procedure(const Mensagem : String) of object ;

  TACBrNFSe = class(TComponent)
  private
    fsAbout: TACBrNFSeAboutInfo;
    FDANFSe : TACBrNFSeDANFSeClass;
    FNotasFiscais: TNotasFiscais;
    FWebServices: TWebServices;
    FConfiguracoes: TConfiguracoes;
    FStatus : TStatusACBrNFSe;
    FOnStatusChange: TNotifyEvent;
    FOnGerarLog : TACBrNFSeLog;

    procedure SetDANFSe(const Value: TACBrNFSeDANFSeClass);
    procedure EnviaEmailThread(const sSmtpHost,
                                     sSmtpPort,
                                     sSmtpUser,
                                     sSmtpPasswd,
                                     sFrom,
                                     sTo,
                                     sAssunto: String;
                                     sMensagem: TStrings;
                                     SSL: Boolean;
                                     sCC,
                                     Anexos: TStrings;
                                     PedeConfirma,
                                     AguardarEnvio: Boolean;
                                     NomeRemetente: String;
                                     TLS: Boolean;
                                     StreamNFSe: TStringStream;
                                     NomeArq: String;
                                     HTML:Boolean = False);
    procedure EnviarEmailNormal(const sSmtpHost,
                                      sSmtpPort,
                                      sSmtpUser,
                                      sSmtpPasswd,
                                      sFrom,
                                      sTo,
                                      sAssunto: String;
                                      sMensagem: TStrings;
                                      SSL: Boolean;
                                      sCC,
                                      Anexos: TStrings;
                                      PedeConfirma,
                                      AguardarEnvio: Boolean;
                                      NomeRemetente: String;
                                      TLS: Boolean;
                                      StreamNFSe: TStringStream;
                                      NomeArq: String);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Enviar(ALote: Integer; Imprimir: Boolean = True): Boolean; overload;
    function Enviar(ALote: String; Imprimir: Boolean = True): Boolean; overload;
    function ConsultarSituacao(ACnpj, AInscricaoMunicipal, AProtocolo: String;
                               const ANumLote: String = ''): Boolean;
    function ConsultarLoteRps(ANumLote, AProtocolo: string; ACNPJ: String = ''; AInscricaoMunicipal: String = '';
                              ASenha: string = ''; AFraseSecreta: string ='';
                              Mes: Integer = 0; Ano: Integer = 0; ARazaoSocial: string = ''): Boolean;
    function ConsultarNFSeporRps(ANumero, ASerie, ATipo, ACnpj, AInscricaoMunicipal: String;
                                 ASenha: String = ''; AFraseSecreta: String = ''; ARazaoSocial: String = ''): Boolean;

    function ConsultarNFSe(ACnpj,
                           AInscricaoMunicipal: String;
                           ADataInicial,
                           ADataFinal: TDateTime;
                           ANumeroNFSe: String = '';
                           APagina: Integer = 1;
                           ASenha : String = '';
                           AFraseSecreta : String = '';
                           ACNPJTomador: String = '';
                           AIMTomador: String = '';
                           ANomeInter: String = '';
                           ACNPJInter: String = '';
                           AIMInter: String = '';
                           ASerie: String = ''): Boolean;

    function ConsultarSequencialRPS(ACidade, ACnpj, AInscricaoMunicipal, ASeriePrestacao: String):Boolean;
    function CancelarNFSe(ACodigoCancelamento: String): Boolean;
    function Gerar(ARps: Integer): Boolean;
    function LinkNFSe(ANumeroNFSe: Integer; ACodVerificacao, AInscricaoM: String): String;
    function GerarLote(ALote: Integer): Boolean; overload;
    function GerarLote(ALote: String): Boolean; overload;
    function EnviarSincrono(ALote: Integer; Imprimir: Boolean = True): Boolean; overload;
    function EnviarSincrono(ALote: String; Imprimir: Boolean = True): Boolean; overload;
    function SubstituirNFSe(ACodigoCancelamento, ANumeroNFSe: String): Boolean;

    property WebServices: TWebServices   read FWebServices  write FWebServices;
    property NotasFiscais: TNotasFiscais read FNotasFiscais write FNotasFiscais;
    property Status: TStatusACBrNFSe     read FStatus;
    procedure SetStatus( const stNewStatus : TStatusACBrNFSe );
    procedure EnviaEmail(const sSmtpHost,
                               sSmtpPort,
                               sSmtpUser,
                               sSmtpPasswd,
                               sFrom,
                               sTo,
                               sAssunto: String;
                               sMensagem : TStrings;
                               SSL : Boolean;
                               sCC: TStrings = nil;
                               Anexos:TStrings=nil;
                               PedeConfirma: Boolean = False;
                               AguardarEnvio: Boolean = False;
                               NomeRemetente: String = '';
                               TLS : Boolean = True;
                               StreamNFSe : TStringStream = nil;
                               NomeArq : String = '';
                               UsarThread: Boolean = True;
                               HTML: Boolean = False);
  published
    property Configuracoes: TConfiguracoes     read FConfiguracoes  write FConfiguracoes;
    property OnStatusChange: TNotifyEvent      read FOnStatusChange write FOnStatusChange;
    property DANFSe: TACBrNFSeDANFSeClass      read FDANFSe         write SetDANFSe;
    property AboutACBrNFSe: TACBrNFSeAboutInfo read fsAbout         write fsAbout stored false;
    property OnGerarLog: TACBrNFSeLog          read FOnGerarLog     write FOnGerarLog;
  end;

procedure ACBrAboutDialog;

implementation

procedure ACBrAboutDialog;
var
 Msg: String;
begin
 Msg := 'Componente ACBrNFSe'+#10+
        'Versão: '+ACBRNFSe_VERSAO+#10+#10+
        'Automação Comercial Brasil'+#10+#10+
        'http://acbr.sourceforge.net'+#10+#10+
        'Projeto Cooperar - PCN'+#10+#10+
        'http://www.projetocooperar.org/pcn/';

 MessageDlg(Msg ,mtInformation ,[mbOk],0);
end;

{ TACBrNFSe }

procedure TACBrNFSe.SetDANFSe(const Value: TACBrNFSeDANFSeClass);
var
 OldValue: TACBrNFSeDANFSeClass;
begin
 if Value <> FDANFSe
  then begin
   if Assigned(FDANFSe)
    then FDANFSe.RemoveFreeNotification(Self);

   OldValue := FDANFSe;  // Usa outra variavel para evitar Loop Infinito
   FDANFSe  := Value;    // na remoção da associação dos componentes

   if Assigned(OldValue)
    then if Assigned(OldValue.ACBrNFSe)
          then OldValue.ACBrNFSe := nil;

   if Value <> nil
    then begin
     Value.FreeNotification(self);
     Value.ACBrNFSe := self;
    end;
  end;
end;

procedure TACBrNFSe.Notification(AComponent: TComponent; Operation: TOperation);
begin
 inherited Notification(AComponent, Operation);

 if (Operation = opRemove) and (FDANFSe <> nil) and
    (AComponent is TACBrNFSeDANFSeClass)
  then FDANFSe := nil;
end;

constructor TACBrNFSe.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);

 FConfiguracoes      := TConfiguracoes.Create( self );
 FConfiguracoes.Name := 'Configuracoes';

 {$IFDEF COMPILER6_UP}
   FConfiguracoes.SetSubComponent( true ); { para gravar no DFM/XFM }
 {$ENDIF}

 FNotasFiscais               := TNotasFiscais.Create(Self, NotaFiscal);
 FNotasFiscais.Configuracoes := FConfiguracoes;
 FWebServices                := TWebServices.Create(Self);

 if FConfiguracoes.WebServices.Tentativas <= 0
  then FConfiguracoes.WebServices.Tentativas := 18;

 {$IFDEF ACBrNFSeOpenSSL}
   NotaUtil.InitXmlSec;
 {$ENDIF}

 FOnGerarLog := nil;
end;

destructor TACBrNFSe.Destroy;
begin
 {$IFDEF ACBrNFSeOpenSSL}
   NotaUtil.ShutDownXmlSec;
 {$ENDIF}

 FConfiguracoes.Free;
 FNotasFiscais.Free;
 FWebServices.Free;

 inherited destroy;
end;

function TACBrNFSe.Enviar(ALote: Integer; Imprimir: Boolean): Boolean;
begin
  Result := Enviar(IntToStr(ALote),Imprimir);
end;

function TACBrNFSe.Enviar(ALote: String; Imprimir: Boolean): Boolean;
var
  i: Integer;
begin
  if NotasFiscais.Count <= 0 then
  begin
    if Assigned(Self.OnGerarLog) then
      Self.OnGerarLog('ERRO: Nenhum RPS adicionado ao Lote');
    raise Exception.Create('ERRO: Nenhum RPS adicionado ao Lote');
    exit;
  end;

  if NotasFiscais.Count > 50 then
  begin
    if Assigned(Self.OnGerarLog) then
      Self.OnGerarLog('ERRO: Conjunto de RPS transmitidos (máximo de 50) excedido. Quantidade atual: '+IntToStr(NotasFiscais.Count));
    raise Exception.Create('ERRO: Conjunto de RPS transmitidos (máximo de 50) excedido. Quantidade atual: '+IntToStr(NotasFiscais.Count));
    exit;
  end;

  // Assina os Rps
  NotasFiscais.Assinar(FConfiguracoes.Certificados.AssinaRPS);

  Result := WebServices.Envia(ALote);

  if DANFSe <> nil then
  begin
    for i:= 0 to NotasFiscais.Count-1 do
    begin
      if NotasFiscais.Items[i].Confirmada and Imprimir then
        NotasFiscais.Items[i].Imprimir;
    end;
  end;
end;

function TACBrNFSe.ConsultarSituacao(ACnpj, AInscricaoMunicipal,
  AProtocolo: String; const ANumLote: String = ''): Boolean;
begin
 Result := WebServices.ConsultaSituacao(ACnpj, AInscricaoMunicipal, AProtocolo, ANumLote);
end;

function TACBrNFSe.ConsultarLoteRps(ANumLote, AProtocolo: String; ACNPJ: string = ''; AInscricaoMunicipal: string = '';
                                    ASenha: string = ''; AFraseSecreta: string ='';
                                    Mes: Integer = 0; Ano: Integer = 0; ARazaoSocial: string = ''): Boolean;
var
 aPath: String;
 wAno, wMes, wDia: Word;
begin
 // Alteri os parâmetros para receber Mes e Ano para Formatar o Path
 aPath := FConfiguracoes.Geral.PathSalvar;

 if (ACNPJ='') and (AInscricaoMunicipal='')
  then begin
   // Acrescentado por Endrigo Rodrigues
   if FConfiguracoes.Arquivos.PastaMensal
    then begin
     DecodeDate(Now, wAno, wMes, wDia);
     if Mes > 0 then
       wMes:= Mes;
     if Ano > 0 then
       wAno:= Ano;
     if Pos(IntToStr(wAno)+IntToStrZero(wMes,2),aPath) <= 0
      then aPath := PathWithDelim(aPath)+IntToStr(wAno)+IntToStrZero(wMes,2) + '\';
    end;

    // Alterado por Herbert Costa para quando o envio de lote não for
    //sincrono o arquivo tem outro nome
    if FilesExists(aPath+'Ger\'+ANumLote+'-env-lotS.xml') then
    begin
     // Alterado por Rodrigo Cantelli
     if FConfiguracoes.Arquivos.AdicionarLiteral then
        NotasFiscais.LoadFromFile(aPath+'Ger\'+ANumLote+'-env-lotS.xml')
      else
       if FConfiguracoes.Arquivos.Salvar then
         NotasFiscais.LoadFromFile(aPath+ANumLote+'-env-lotS.xml');
    end
    else
    begin
      if FConfiguracoes.Arquivos.AdicionarLiteral then
        NotasFiscais.LoadFromFile(aPath+'Ger\'+ANumLote+'-env-lot.xml')
      else
       if FConfiguracoes.Arquivos.Salvar then
         NotasFiscais.LoadFromFile(aPath+ANumLote+'-env-lot.xml');
    end;


   if NotasFiscais.Count <= 0
    then begin
     if Assigned(Self.OnGerarLog)
      then Self.OnGerarLog('ERRO: Nenhum RPS adicionado');
     raise Exception.Create('ERRO: Nenhum RPS adicionado');
     exit;
    end;
  end;

  if (Trim(Self.WebServices.ConsLote.NumeroLote) = '') then
    Self.WebServices.ConsLote.NumeroLote:= ANumLote;

  //obrigatorio passar a razao social para o provedor Tecnos
  if (FConfiguracoes.WebServices.Provedor in [proTecnos]) and (ARazaoSocial = '') then
    ARazaoSocial := NotasFiscais.Items[0].NFSe.PrestadorServico.RazaoSocial;

 Result := WebServices.ConsultaLoteRps(AProtocolo, ACNPJ, AInscricaoMunicipal, ASenha, AFraseSecreta, ARazaoSocial);
end;

function TACBrNFSe.ConsultarNFSeporRps(ANumero, ASerie, ATipo, ACnpj,
  AInscricaoMunicipal: String; ASenha: String = ''; AFraseSecreta: String = ''; ARazaoSocial: string = ''): Boolean;
begin
 if NotasFiscais.Count <= 0
  then begin
   if Assigned(Self.OnGerarLog)
    then Self.OnGerarLog('ERRO: Nenhum RPS adicionado');
   raise Exception.Create('ERRO: Nenhum RPS adicionado');
   exit;
  end;

 Result := WebServices.ConsultaNFSeporRps(ANumero, ASerie, ATipo, ACnpj,
                AInscricaoMunicipal, ASenha, AFraseSecreta, ARazaoSocial);
end;

function TACBrNFSe.ConsultarNFSe(ACnpj,
                                 AInscricaoMunicipal: String;
                                 ADataInicial,
                                 ADataFinal: TDateTime;
                                 ANumeroNFSe: string = '';
                                 APagina: Integer = 1;
                                 ASenha : String = '';
                                 AFraseSecreta : String = '';
                                 ACNPJTomador: String = '';
                                 AIMTomador: String = '';
                                 ANomeInter: String = '';
                                 ACNPJInter: String = '';
                                 AIMInter: String = '';
                                 ASerie: String = ''): Boolean;
begin
 Result := WebServices.ConsultaNFSe(ACnpj, AInscricaoMunicipal, ADataInicial,
                ADataFinal, ANumeroNFSe, APagina, ASenha, AFraseSecreta,
                ACNPJTomador, AIMTomador, ANomeInter, ACNPJInter, AIMInter, ASerie);
end;

function TACBrNFSe.CancelarNFSe(ACodigoCancelamento: String): Boolean;
begin
  if Self.NotasFiscais.Count = 0 then
   begin
      if Assigned(Self.OnGerarLog) then
         Self.OnGerarLog('ERRO: Nenhuma Nota Fiscal de Serviço Eletrônica Informada!');
      raise Exception.Create('Nenhuma Nota Fiscal de Serviço Eletrônica Informada!');
   end;

 Result := WebServices.CancelaNFSe(ACodigoCancelamento, True);
end;

procedure TACBrNFSe.SetStatus( const stNewStatus : TStatusACBrNFSe );
begin
 if ( stNewStatus <> FStatus )
  then begin
   FStatus := stNewStatus;
   if Assigned(fOnStatusChange)
    then FOnStatusChange(Self);
  end;
end;

function TACBrNFSe.Gerar(ARps: Integer): Boolean;
begin
  if NotasFiscais.Count <= 0 then
  begin
    if Assigned(Self.OnGerarLog) then
      Self.OnGerarLog('ERRO: Nenhum RPS adicionado');
    raise Exception.Create('ERRO: Nenhum RPS adicionado');
    exit;
  end;

  if NotasFiscais.Count > 1 then
  begin
    if Assigned(Self.OnGerarLog) then
      Self.OnGerarLog('ERRO: Conjunto de RPS transmitidos (máximo de 1) excedido. Quantidade atual: '+IntToStr(NotasFiscais.Count));
    raise Exception.Create('ERRO: Conjunto de RPS transmitidos (máximo de 1) excedido. Quantidade atual: '+IntToStr(NotasFiscais.Count));
    exit;
  end;

  // Assina os Rps
  NotasFiscais.Assinar(FConfiguracoes.Certificados.AssinaGerar);

  Result := WebServices.Gera(ARps);
end;

function TACBrNFSe.LinkNFSe(ANumeroNFSe: Integer;
  ACodVerificacao, AInscricaoM: String): String;
begin
 Result := WebServices.LinkNFSeGerada(ANumeroNFSe, ACodVerificacao, AInscricaoM);
end;

function TACBrNFSe.GerarLote(ALote: Integer): Boolean;
begin
  Result := GerarLote(IntToStr(ALote));
end;

function TACBrNFSe.GerarLote(ALote: String): Boolean;
begin
  if NotasFiscais.Count <= 0 then
  begin
    if Assigned(Self.OnGerarLog) then
      Self.OnGerarLog('ERRO: Nenhum RPS adicionado ao Lote');
    raise Exception.Create('ERRO: Nenhum RPS adicionado ao Lote');
    exit;
  end;

  if NotasFiscais.Count > 50 then
  begin
    if Assigned(Self.OnGerarLog) then
      Self.OnGerarLog('ERRO: Conjunto de RPS transmitidos (máximo de 50) excedido. Quantidade atual: '+IntToStr(NotasFiscais.Count));
    raise Exception.Create('ERRO: Conjunto de RPS transmitidos (máximo de 50) excedido. Quantidade atual: '+IntToStr(NotasFiscais.Count));
    exit;
  end;

// if (FConfiguracoes.WebServices.Provedor <> proPublica) and
//    (FConfiguracoes.WebServices.Provedor <> proSisPMJP) then
//   NotasFiscais.Assinar(True); // Assina os Rps

  // Assina os Rps
  NotasFiscais.Assinar(FConfiguracoes.Certificados.AssinaRPS);

  Result := WebServices.GeraLote(ALote);
end;

function TACBrNFSe.EnviarSincrono(ALote: Integer;
  Imprimir: Boolean): Boolean;
begin
  Result := EnviarSincrono(IntToStr(ALote));
end;

function TACBrNFSe.EnviarSincrono(ALote: String;
  Imprimir: Boolean): Boolean;
begin
  if NotasFiscais.Count <= 0 then
  begin
    if Assigned(Self.OnGerarLog) then
      Self.OnGerarLog('ERRO: Nenhum RPS adicionado ao Lote');
    raise Exception.Create('ERRO: Nenhum RPS adicionado ao Lote');
    exit;
  end;

  if NotasFiscais.Count > 50 then
  begin
    if Assigned(Self.OnGerarLog) then
      Self.OnGerarLog('ERRO: Conjunto de RPS transmitidos (máximo de 50) excedido. Quantidade atual: '+IntToStr(NotasFiscais.Count));
    raise Exception.Create('ERRO: Conjunto de RPS transmitidos (máximo de 50) excedido. Quantidade atual: '+IntToStr(NotasFiscais.Count));
    exit;
  end;

  // Assina os Rps
  NotasFiscais.Assinar(FConfiguracoes.Certificados.AssinaRPS);

  Result := WebServices.EnviaSincrono(ALote);
end;

function TACBrNFSe.ConsultarSequencialRPS(ACidade, ACnpj,
  AInscricaoMunicipal, ASeriePrestacao: String): Boolean;
begin
  Result := WebServices.ConsultaSequencialRPS(ACidade, ACnpj, AInscricaoMunicipal, ASeriePrestacao);
end;

procedure TACBrNFSe.EnviaEmailThread(const sSmtpHost, sSmtpPort, sSmtpUser,
  sSmtpPasswd, sFrom, sTo, sAssunto: String; sMensagem: TStrings;
  SSL: Boolean; sCC, Anexos: TStrings; PedeConfirma,
  AguardarEnvio: Boolean; NomeRemetente: String; TLS: Boolean;
  StreamNFSe: TStringStream; NomeArq: String; HTML: Boolean);
var
 ThreadSMTP : TSendMailThread;
 m:TMimemess;
 p: TMimepart;
 i: Integer;
begin
 m:=TMimemess.create;

 ThreadSMTP := TSendMailThread.Create ;  // Não Libera, pois usa FreeOnTerminate := True ;
 try
    p := m.AddPartMultipart('mixed', nil);
    if sMensagem <> nil then
    begin
       if HTML = true then
          m.AddPartHTML(sMensagem, p)
       else
          m.AddPartText(sMensagem, p);
    end;

    if StreamNFSe <> nil then
      m.AddPartBinary(StreamNFSe,NomeArq, p);

    if assigned(Anexos) then
      for i := 0 to Anexos.Count - 1 do
      begin
        m.AddPartBinaryFromFile(Anexos[i], p);
      end;

    m.header.tolist.add(sTo);

    if Trim(NomeRemetente) <> '' then
      m.header.From := Format('%s<%s>', [NomeRemetente, sFrom])
    else
      m.header.From := sFrom;

    m.header.subject:= sAssunto;
    m.Header.ReplyTo := sFrom;
    if PedeConfirma then
       m.Header.CustomHeaders.Add('Disposition-Notification-To: '+sFrom);
    m.EncodeMessage;

    ThreadSMTP.sFrom := sFrom;
    ThreadSMTP.sTo   := sTo;
    if sCC <> nil then
       ThreadSMTP.sCC.AddStrings(sCC);
    ThreadSMTP.slmsg_Lines.AddStrings(m.Lines);

    ThreadSMTP.smtp.UserName := sSmtpUser;
    ThreadSMTP.smtp.Password := sSmtpPasswd;

    ThreadSMTP.smtp.TargetHost := sSmtpHost;
    if not EstaVazio( sSmtpPort ) then     // Usa default
       ThreadSMTP.smtp.TargetPort := sSmtpPort;

    ThreadSMTP.smtp.FullSSL := SSL;
    ThreadSMTP.smtp.AutoTLS := TLS;

    // ThreadSMTP.smtp.StartTLS; ?

    if(TLS) then
      ThreadSMTP.smtp.StartTLS;

    SetStatus( stNFSeEmail );
    ThreadSMTP.Resume; // inicia a thread
    if AguardarEnvio then
    begin
      repeat
        Sleep(1000);
        Application.ProcessMessages;
      until ThreadSMTP.Terminado;
    end;
    SetStatus( stNFSeIdle );
 finally
    m.free;
 end;
end;

procedure TACBrNFSe.EnviarEmailNormal(const sSmtpHost, sSmtpPort,
  sSmtpUser, sSmtpPasswd, sFrom, sTo, sAssunto: String;
  sMensagem: TStrings; SSL: Boolean; sCC, Anexos: TStrings; PedeConfirma,
  AguardarEnvio: Boolean; NomeRemetente: String; TLS: Boolean;
  StreamNFSe: TStringStream; NomeArq: String);
var
  smtp: TSMTPSend;
  msg_lines: TStringList;
  m:TMimemess;
  p: TMimepart;
  I : Integer;
  CorpoEmail: TStringList;
begin
  SetStatus( stNFSeEmail );

  msg_lines := TStringList.Create;
  CorpoEmail := TStringList.Create;
  smtp := TSMTPSend.Create;
  m:=TMimemess.create;
  try
     p := m.AddPartMultipart('mixed', nil);
     if sMensagem <> nil then
     begin
        CorpoEmail.Text := sMensagem.Text;
        m.AddPartText(CorpoEmail, p);
     end;

     if StreamNFSe <> nil then
       m.AddPartBinary(StreamNFSe,NomeArq, p);

     if assigned(Anexos) then
     for i := 0 to Anexos.Count - 1 do
     begin
        m.AddPartBinaryFromFile(Anexos[i], p);
     end;

     m.header.tolist.add(sTo);

     if Trim(NomeRemetente) <> '' then
       m.header.From := Format('%s<%s>', [NomeRemetente, sFrom])
     else
       m.header.From := sFrom;

     m.header.subject := sAssunto;
     m.EncodeMessage;
     msg_lines.Add(m.Lines.Text);

     smtp.UserName := sSmtpUser;
     smtp.Password := sSmtpPasswd;

     smtp.TargetHost := sSmtpHost;
     smtp.TargetPort := sSmtpPort;

     smtp.FullSSL := SSL;
     // smtp.AutoTLS := SSL; ?
     smtp.AutoTLS := TLS;

     if(TLS) then
       smtp.StartTLS;

     if not smtp.Login then
       raise Exception.Create('SMTP ERROR: Login: ' + smtp.EnhCodeString+sLineBreak+smtp.FullResult.Text);

     if not smtp.MailFrom(sFrom, Length(sFrom)) then
       raise Exception.Create('SMTP ERROR: MailFrom: ' + smtp.EnhCodeString+sLineBreak+smtp.FullResult.Text);

     if not smtp.MailTo(sTo) then
       raise Exception.Create('SMTP ERROR: MailTo: ' + smtp.EnhCodeString+sLineBreak+smtp.FullResult.Text);

     if sCC <> nil then
     begin
       for I := 0 to sCC.Count - 1 do
       begin
         if not smtp.MailTo(sCC.Strings[i]) then
           raise Exception.Create('SMTP ERROR: MailTo: ' + smtp.EnhCodeString+sLineBreak+smtp.FullResult.Text);
       end;
     end;

     if not smtp.MailData(msg_lines) then
       raise Exception.Create('SMTP ERROR: MailData: ' + smtp.EnhCodeString+sLineBreak+smtp.FullResult.Text);

     if not smtp.Logout then
       raise Exception.Create('SMTP ERROR: Logout: ' + smtp.EnhCodeString+sLineBreak+smtp.FullResult.Text);
  finally
     msg_lines.Free;
     CorpoEmail.Free;
     smtp.Free;
     m.free;
     SetStatus( stNFSeIdle );
  end;
end;

procedure TACBrNFSe.EnviaEmail(const sSmtpHost, sSmtpPort, sSmtpUser,
  sSmtpPasswd, sFrom, sTo, sAssunto: String; sMensagem: TStrings;
  SSL: Boolean; sCC, Anexos: TStrings; PedeConfirma,
  AguardarEnvio: Boolean; NomeRemetente: String; TLS: Boolean;
  StreamNFSe: TStringStream; NomeArq: String; UsarThread, HTML: Boolean);
begin
  if UsarThread then
  begin
    EnviaEmailThread(
      sSmtpHost,
      sSmtpPort,
      sSmtpUser,
      sSmtpPasswd,
      sFrom,
      sTo,
      sAssunto,
      sMensagem,
      SSL,
      sCC,
      Anexos,
      PedeConfirma,
      AguardarEnvio,
      NomeRemetente,
      TLS,
      StreamNFSe,
      NomeArq,
      HTML
    );
  end
  else
  begin
    EnviarEmailNormal(
      sSmtpHost,
      sSmtpPort,
      sSmtpUser,
      sSmtpPasswd,
      sFrom,
      sTo,
      sAssunto,
      sMensagem,
      SSL,
      sCC,
      Anexos,
      PedeConfirma,
      AguardarEnvio,
      NomeRemetente,
      TLS,
      StreamNFSe,
      NomeArq
    );
  end;
end;

function TACBrNFSe.SubstituirNFSe(ACodigoCancelamento, ANumeroNFSe: String): Boolean;
begin
  if Self.NotasFiscais.Count = 0 then
   begin
      if Assigned(Self.OnGerarLog) then
         Self.OnGerarLog('ERRO: Nenhum RPS adicionado!');
      raise Exception.Create('ERRO: Nenhum RPS adicionado!');
   end;

 NotasFiscais.Assinar(True);

 Result := WebServices.SubstitiNFSe(ACodigoCancelamento, ANumeroNFSe);
end;

end.
