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
    FLock: TCriticalSection;

  public
    procedure CriarACBrMail;

    procedure AplicarConfiguracoes;
    procedure AplicarConfigMail;
    procedure ConfigurarImpressao(NomeImpressora: String = ''; GerarPDF: Boolean = False;
                                  Protocolo: String = ''; MostrarPreview: String = '');
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
  ACBrMDFe1.Configuracoes.Assign(pLibConfig.MDFe);

  AplicarConfigMail;
end;

procedure TLibMDFeDM.AplicarConfigMail;
begin
  if Assigned(FLibMail) then
  begin
    FLibMail.ConfigLer(pLib.Config.NomeArquivo);
    Exit;
  end;

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

procedure TLibMDFeDM.ConfigurarImpressao(NomeImpressora: String = ''; GerarPDF: Boolean = False;
                                         Protocolo: String = ''; MostrarPreview: String = '');
begin
  GravarLog('ConfigurarImpressao - Iniciado', logNormal);

  if ACBrMDFe1.Manifestos.Count > 0 then
  begin
    if (ACBrMDFe1.Manifestos.Items[0].MDFe.procMDFe.cStat in [101, 151, 155]) then
      ACBrMDFe1.DAMDFe.Cancelada := True
    else
      ACBrMDFe1.DAMDFe.Cancelada := False;
  end;

  TLibMDFeConfig(pLib.Config).DAMDFe.Apply(ACBrMDFeDAMDFeRL1);

  if NaoEstaVazio(NomeImpressora) then
    ACBrMDFe1.DAMDFe.Impressora := NomeImpressora;

  if NaoEstaVazio(MostrarPreview) then
    ACBrMDFe1.DAMDFe.MostraPreview := StrToBoolDef(MostrarPreview, False);

  if NaoEstaVazio(Protocolo) then
    ACBrMDFe1.DAMDFe.Protocolo := Protocolo
  else
    ACBrMDFe1.DAMDFe.Protocolo := '';

  if GerarPDF and not DirectoryExists(PathWithDelim(TLibMDFeConfig(pLib.Config).DAMDFe.PathPDF))then
        ForceDirectories(PathWithDelim(TLibMDFeConfig(pLib.Config).DAMDFe.PathPDF));

  GravarLog('ConfigurarImpressao - Feito', logNormal);
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
