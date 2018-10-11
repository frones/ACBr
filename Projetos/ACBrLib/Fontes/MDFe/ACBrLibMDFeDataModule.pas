unit ACBrLibMDFeDataModule;

{$mode delphi}

interface

uses
  Classes, SysUtils, syncobjs,
  ACBrMDFe, ACBrMDFeDAMDFeRLClass, ACBrMail,
  ACBrLibConfig, ACBrLibMailImport;

type

  { TLibMDFeDM }

  TLibMDFeDM = class(TDataModule)
    ACBrMDFe1: TACBrMDFe;
    ACBrMDFeDAMDFeRL1: TACBrMDFeDAMDFeRL;

    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    FLock: TCriticalSection;
    FACBrMail: TACBrMail;

    FLibMail: TACBrLibMail;
  public
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
  ACBrLibMDFeConfig, ACBrLibComum, ACBrLibMDFeClass;

{$R *.lfm}

{ TLibMDFeDM }

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
    FLibMail  := TACBrLibMail.Create(NomeLib, pLib.Config.NomeArquivo, pLib.Config.ChaveCrypt);
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
  pLibConfig := TLibMDFeConfig(TACBrLibMDFe(pLib).Config);
  ACBrMDFe1.Configuracoes.Assign(pLibConfig.MDFeConfig);

  AplicarConfigMail;
end;

procedure TLibMDFeDM.AplicarConfigMail;
begin
  if Assigned(FLibMail) or (not Assigned(FACBrMail)) then
    Exit;

  with FACBrMail do
  begin
    Attempts := pLib.Config.Email.Tentativas;
    SetTLS := pLib.Config.Email.TLS;
    DefaultCharset := pLib.Config.Email.Codificacao;
    From := pLib.Config.Email.Conta;
    FromName := pLib.Config.Email.Nome;
    SetSSL := pLib.Config.Email.SSL;
    Host := pLib.Config.Email.Servidor;
    IDECharset := pLib.Config.Email.Codificacao;
    IsHTML := pLib.Config.Email.IsHTML;
    Password := pLib.Config.Email.Senha;
    Port := IntToStr(pLib.Config.Email.Porta);
    Priority := pLib.Config.Email.Priority;
    ReadingConfirmation := pLib.Config.Email.Confirmacao;
    DeliveryConfirmation := pLib.Config.Email.ConfirmacaoEntrega;
    TimeOut := pLib.Config.Email.TimeOut;
    Username := pLib.Config.Email.Usuario;
    UseThread := pLib.Config.Email.SegundoPlano;
  end;
end;

procedure TLibMDFeDM.GravarLog(AMsg: String; NivelLog: TNivelLog; Traduzir: Boolean);
begin
  if Assigned(pLib) then
    pLib.GravarLog(AMsg, NivelLog, Traduzir);
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
