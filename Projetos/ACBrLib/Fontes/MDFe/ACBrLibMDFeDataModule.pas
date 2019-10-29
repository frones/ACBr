unit ACBrLibMDFeDataModule;

{$mode delphi}

interface

uses
  Classes, SysUtils, syncobjs,
  ACBrMDFe, ACBrMDFeDAMDFeRLClass, ACBrMail,
  ACBrLibComum, ACBrLibConfig, ACBrLibMailImport;

type

  { TLibMDFeDM }

  TLibMDFeDM = class(TDataModule)
    ACBrMDFe1: TACBrMDFe;
    ACBrMDFeDAMDFeRL1: TACBrMDFeDAMDFeRL;

    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    FACBrMail: TACBrMail;
    FLibMail: TACBrLibMail;

  protected
    FOwner: TACBrLib;
    FLock: TCriticalSection;

  public
    constructor Create(AOwner: TACBrLib); reintroduce;

    procedure CriarACBrMail;

    procedure AplicarConfiguracoes;
    procedure AplicarConfigMail;
    procedure GravarLog(AMsg: String; NivelLog: TNivelLog; Traduzir: Boolean = False);
    procedure Travar;
    procedure Destravar;
  end;

implementation

uses
  ACBrUtil, FileUtil,
  ACBrLibMDFeConfig, ACBrLibMDFeClass;

{$R *.lfm}

{ TLibMDFeDM }

procedure TLibMDFeDM.Create(AOwner: TACBrLib);
begin
  Inherited Create(nil);

  FOwner := AOwner;
end;

procedure TLibMDFeDM.DataModuleCreate(Sender: TObject);
begin
  FLock := TCriticalSection.Create;
  FACBrMail := Nil;
  FLibMail := Nil;
end;

procedure TLibMDFeDM.DataModuleDestroy(Sender: TObject);
begin
  FLock.Destroy;

  if Assigned(FLibMail) then
    FreeAndNil(FLibMail)
  else if Assigned(FACBrMail) then
    FreeAndNil(FACBrMail);
end;

procedure TLibMDFeDM.CriarACBrMail;
var
  NomeLib: String;
begin
  if Assigned(FLibMail) or Assigned(FACBrMail) then
    Exit;

  GravarLog('  CriarACBrMail', logCompleto);

  NomeLib := ApplicationPath + CACBrMailLIBName;
  if FileExists(NomeLib) then
  begin
    GravarLog('      Carregando MAIL de: ' + NomeLib, logCompleto);
    // Criando Classe para Leitura da Lib //
    FLibMail  := TACBrLibMail.Create(NomeLib, FOwner.Config.NomeArquivo, FOwner.Config.ChaveCrypt);
    FACBrMail := FLibMail.ACBrMail;
  end
  else
  begin
    GravarLog('     Criando MAIL Interno', logCompleto);
    FACBrMail := TACBrMail.Create(Nil);
  end;

  ACBrMDFe1.MAIL := FACBrMail;
end;

procedure TLibMDFeDM.AplicarConfiguracoes;
var
  pLibConfig: TLibMDFeConfig;
begin
  ACBrMDFe1.SSL.DescarregarCertificado;
  pLibConfig := TLibMDFeConfig(TACBrLibMDFe(FOwner).Config);
  ACBrMDFe1.Configuracoes.Assign(pLibConfig.MDFe);

  AplicarConfigMail;
end;

procedure TLibMDFeDM.AplicarConfigMail;
begin
  if Assigned(FLibMail) or (not Assigned(FACBrMail)) then
    Exit;

  with FACBrMail do
  begin
    Attempts := FOwner.Config.Email.Tentativas;
    SetTLS := FOwner.Config.Email.TLS;
    DefaultCharset := FOwner.Config.Email.Codificacao;
    From := FOwner.Config.Email.Conta;
    FromName := FOwner.Config.Email.Nome;
    SetSSL := FOwner.Config.Email.SSL;
    Host := FOwner.Config.Email.Servidor;
    IDECharset := FOwner.Config.Email.Codificacao;
    IsHTML := FOwner.Config.Email.IsHTML;
    Password := FOwner.Config.Email.Senha;
    Port := IntToStr(FOwner.Config.Email.Porta);
    Priority := FOwner.Config.Email.Priority;
    ReadingConfirmation := FOwner.Config.Email.Confirmacao;
    DeliveryConfirmation := FOwner.Config.Email.ConfirmacaoEntrega;
    TimeOut := FOwner.Config.Email.TimeOut;
    Username := FOwner.Config.Email.Usuario;
    UseThread := FOwner.Config.Email.SegundoPlano;
  end;
end;

procedure TLibMDFeDM.GravarLog(AMsg: String; NivelLog: TNivelLog; Traduzir: Boolean);
begin
  if Assigned(FOwner) then
    FOwner.GravarLog(AMsg, NivelLog, Traduzir);
end;

procedure TLibMDFeDM.Travar;
begin
  GravarLog('Travar', logParanoico);
  FLock.Acquire;
end;

procedure TLibMDFeDM.Destravar;
begin
  GravarLog('Destravar', logParanoico);
  FLock.Release;
end;

end.
