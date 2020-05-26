unit ACBrLibGNReDataModule;

{$mode delphi}

interface

uses
  Classes, SysUtils, syncobjs,
  ACBrMail, ACBrGNRE2, ACBrGNReGuiaRLClass,
  ACBrLibConfig, ACBrLibMailImport;

type

  { TLibGNReDM }

  TLibGNReDM = class(TDataModule)
    ACBrGNRE1: TACBrGNRE;
    ACBrGNREGuiaRL1: TACBrGNREGuiaRL;

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
    procedure ConfigurarImpressao(GerarPDF: Boolean; NomeImpressora: String = ''; MostrarPreview: String = '');
    procedure GravarLog(AMsg: String; NivelLog: TNivelLog; Traduzir: Boolean = False);
    procedure Travar;
    procedure Destravar;
  end;

implementation

uses
  ACBrUtil, FileUtil,
  ACBrLibGNReConfig, ACBrLibComum, ACBrLibGNReClass;

{$R *.lfm}

{ TLibGNReDM }

procedure TLibGNReDM.DataModuleCreate(Sender: TObject);
begin
  FLock := TCriticalSection.Create;
  FACBrMail := Nil;
  FLibMail := Nil;
end;

procedure TLibGNReDM.DataModuleDestroy(Sender: TObject);
begin
  FLock.Destroy;

  if Assigned(FLibMail) then
    FreeAndNil(FLibMail)
  else if Assigned(FACBrMail) then
    FreeAndNil(FACBrMail);
end;

procedure TLibGNReDM.CriarACBrMail;
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

  ACBrGNRe1.MAIL := FACBrMail;
end;

procedure TLibGNReDM.AplicarConfiguracoes;
var
  pLibConfig: TLibGNReConfig;
begin
  ACBrGNRe1.SSL.DescarregarCertificado;
  pLibConfig := TLibGNReConfig(TACBrLibGNRe(pLib).Config);
  ACBrGNRe1.Configuracoes.Assign(pLibConfig.GNReConfig);

  AplicarConfigMail;
end;

procedure TLibGNReDM.AplicarConfigMail;
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

procedure TLibGNReDM.ConfigurarImpressao(GerarPDF: Boolean; NomeImpressora, MostrarPreview: String);
var
  pLibConfig: TLibGNReConfig;
begin
  GravarLog('ConfigurarImpressao - Iniciado', logNormal);

  pLibConfig := TLibGNReConfig(pLib.Config);
  ACBrGNRE1.GNREGuia := ACBrGNREGuiaRL1;

  with ACBrGNREGuiaRL1 do
  begin
    eMail := pLibConfig.GuiaConfig.eMail;
    Fax := pLibConfig.GuiaConfig.Fax;
    Impressora := pLibConfig.GuiaConfig.Impressora;
    MargemInferior := pLibConfig.GuiaConfig.MargemInferior;
    MargemSuperior := pLibConfig.GuiaConfig.MargemSuperior;
    MargemEsquerda := pLibConfig.GuiaConfig.MargemEsquerda;
    MargemDireita := pLibConfig.GuiaConfig.MargemDireita;
    MostrarPreview := pLibConfig.GuiaConfig.MostrarPreview;
    MostrarStatus := pLibConfig.GuiaConfig.MostrarStatus;
    NumCopias := pLibConfig.GuiaConfig.NumCopias;
    PathPDF := pLibConfig.GuiaConfig.PathPDF;
    PrintDialog := pLibConfig.GuiaConfig.PrintDialog;
    Sistema := pLibConfig.GuiaConfig.Sistema;
    Site := pLibConfig.GuiaConfig.Site;
    TamanhoPapel := pLibConfig.GuiaConfig.TamanhoPapel;
    Usuario := pLibConfig.GuiaConfig.Usuario;
  end;

  if GerarPDF then
  begin
    if (pLibConfig.GuiaConfig.PathPDF <> '') then
      if not DirectoryExists(PathWithDelim(pLibConfig.GuiaConfig.PathPDF))then
        ForceDirectories(PathWithDelim(pLibConfig.GuiaConfig.PathPDF));
  end;

  if NaoEstaVazio(NomeImpressora) then
    ACBrGNREGuiaRL1.Impressora := NomeImpressora;

  if NaoEstaVazio(MostrarPreview) then
    ACBrGNREGuiaRL1.MostrarPreview := StrToBoolDef(MostrarPreview, False);

  GravarLog('ConfigurarImpressao - Feito', logNormal);
end;

procedure TLibGNReDM.GravarLog(AMsg: String; NivelLog: TNivelLog; Traduzir: Boolean);
begin
  if Assigned(pLib) then
    pLib.GravarLog(AMsg, NivelLog, Traduzir);
end;

procedure TLibGNReDM.Travar;
begin
  GravarLog('Travar', logParanoico);
  FLock.Acquire;
end;

procedure TLibGNReDM.Destravar;
begin
  GravarLog('Destravar', logParanoico);
  FLock.Release;
end;

end.
