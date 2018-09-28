unit ACBrLibCTeDataModule;

{$mode delphi}

interface

uses
  Classes, SysUtils, syncobjs, FileUtil, ACBrCTe, ACBrCTeDACTeRLClass, ACBrMail,
  ACBrLibConfig;

type

  { TLibCTeDM }

  TLibCTeDM = class(TDataModule)
    ACBrCTe1: TACBrCTe;
    ACBrCTeDACTeRL1: TACBrCTeDACTeRL;
    ACBrMail1: TACBrMail;
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
  ACBrLibCTeConfig, ACBrLibComum, ACBrLibCTeClass;

{$R *.lfm}

{ TLibCTeDM }

procedure TLibCTeDM.DataModuleCreate(Sender: TObject);
begin
  FLock := TCriticalSection.Create;
end;

procedure TLibCTeDM.DataModuleDestroy(Sender: TObject);
begin
  FLock.Destroy;
end;

procedure TLibCTeDM.AplicarConfiguracoes;
var
  pLibConfig: TLibCTeConfig;
begin
  ACBrCTe1.SSL.DescarregarCertificado;
  pLibConfig := TLibCTeConfig(TACBrLibCTe(pLib).Config);
  ACBrCTe1.Configuracoes.Assign(pLibConfig.CTeConfig);

  if (TACBrLibCTe(pLib).LibMail = nil) then
    AplicarConfigMail;
end;

procedure TLibCTeDM.AplicarConfigMail;
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

procedure TLibCTeDM.GravarLog(AMsg: String; NivelLog: TNivelLog; Traduzir: Boolean);
begin
  if Assigned(pLib) then
    pLib.GravarLog(AMsg, NivelLog, Traduzir);
end;

procedure TLibCTeDM.Travar;
begin
  GravarLog('Travar', logParanoico);
  FLock.Acquire;
end;

procedure TLibCTeDM.Destravar;
begin
  GravarLog('Destravar', logParanoico);
  FLock.Release;
end;

end.
