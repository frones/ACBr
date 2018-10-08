unit ACBrLibMDFeDataModule;

{$mode delphi}

interface

uses
  Classes, SysUtils, syncobjs, FileUtil, ACBrMDFe, ACBrMDFeDAMDFeRLClass, ACBrMail,
  ACBrLibConfig;

type

  { TLibMDFeDM }

  TLibMDFeDM = class(TDataModule)
    ACBrMail1: TACBrMail;
    ACBrMDFe1: TACBrMDFe;
    ACBrMDFeDAMDFeRL1: TACBrMDFeDAMDFeRL;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);

  private
    FLock: TCriticalSection;

  public
    procedure AplicarConfiguracoes;
    procedure AplicarConfigMail;
    procedure GravarLog(AMsg: String; NivelLog: TNivelLog; Traduzir: Boolean = False);
    procedure Travar;
    procedure Destravar;
  end;

implementation

uses
  ACBrUtil,
  ACBrLibMDFeConfig, ACBrLibComum, ACBrLibMDFeClass;

{$R *.lfm}

{ TLibMDFeDM }

procedure TLibMDFeDM.DataModuleCreate(Sender: TObject);
begin
  FLock := TCriticalSection.Create;
end;

procedure TLibMDFeDM.DataModuleDestroy(Sender: TObject);
begin
  FLock.Destroy;
end;

procedure TLibMDFeDM.AplicarConfiguracoes;
var
  pLibConfig: TLibMDFeConfig;
begin
  ACBrMDFe1.SSL.DescarregarCertificado;
  pLibConfig := TLibMDFeConfig(TACBrLibMDFe(pLib).Config);
  ACBrMDFe1.Configuracoes.Assign(pLibConfig.MDFeConfig);

  if (TACBrLibMDFe(pLib).LibMail = nil) then
    AplicarConfigMail;
end;

procedure TLibMDFeDM.AplicarConfigMail;
begin
  with ACBrMail1 do
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
