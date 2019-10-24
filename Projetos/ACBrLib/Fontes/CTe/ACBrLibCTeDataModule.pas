unit ACBrLibCTeDataModule;

{$mode delphi}

interface

uses
  Classes, SysUtils, syncobjs,
  ACBrLibConfig,
  ACBrCTe, ACBrCTeDACTeRLClass, ACBrMail,
  ACBrLibMailImport;

type

  { TLibCTeDM }

  TLibCTeDM = class(TDataModule)
    ACBrCTe1: TACBrCTe;
    ACBrCTeDACTeRL1: TACBrCTeDACTeRL;

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
    procedure ConfigurarImpressao(NomeImpressora: String = ''; GerarPDF: Boolean = False);
    procedure GravarLog(AMsg: String; NivelLog: TNivelLog; Traduzir: Boolean = False);
    procedure Travar;
    procedure Destravar;
  end;

implementation

uses
  ACBrUtil, FileUtil,
  ACBrLibCTeConfig, ACBrLibComum, ACBrLibCTeClass;

{$R *.lfm}

{ TLibCTeDM }

procedure TLibCTeDM.DataModuleCreate(Sender: TObject);
begin
  FLock := TCriticalSection.Create;
  FACBrMail := Nil;
  FLibMail := Nil;
end;

procedure TLibCTeDM.DataModuleDestroy(Sender: TObject);
begin
  FLock.Destroy;

  if Assigned(FLibMail) then
    FreeAndNil(FLibMail)
  else if Assigned(FACBrMail) then
    FreeAndNil(FACBrMail);
end;

procedure TLibCTeDM.CriarACBrMail;
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

  ACBrCTe1.MAIL := FACBrMail;
end;

procedure TLibCTeDM.AplicarConfiguracoes;
var
  pLibConfig: TLibCTeConfig;
begin
  ACBrCTe1.SSL.DescarregarCertificado;
  pLibConfig := TLibCTeConfig(TACBrLibCTe(pLib).Config);
  ACBrCTe1.Configuracoes.Assign(pLibConfig.CTe);

  AplicarConfigMail;
end;

procedure TLibCTeDM.AplicarConfigMail;
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

procedure TLibCTeDM.GravarLog(AMsg: String; NivelLog: TNivelLog; Traduzir: Boolean);
begin
  if Assigned(pLib) then
    pLib.GravarLog(AMsg, NivelLog, Traduzir);
end;

procedure TLibCTeDM.ConfigurarImpressao(NomeImpressora: String = ''; GerarPDF: Boolean = False);
var
  pLibConfig: TLibCTeConfig;
begin
  pLibConfig := TLibCTeConfig(pLib.Config);

  GravarLog('ConfigurarImpressao - Iniciado', logNormal);

   if ACBrCTe1.Conhecimentos.Count <= 0 then
     Exit;

   pLibConfig.DACTe.Apply(ACBrCTeDACTeRL1);

   if NaoEstaVazio(NomeImpressora) then
     ACBrCTeDACTeRL1.Impressora := NomeImpressora;

   if GerarPDF and not DirectoryExists(PathWithDelim(pLibConfig.DACTe.PathPDF))then
        ForceDirectories(PathWithDelim(pLibConfig.DACTe.PathPDF));

   GravarLog('ConfigurarImpressao - Feito', logNormal);
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
