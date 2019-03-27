unit ACBrLibNFeDataModule;

{$mode delphi}

interface

uses
  Classes, SysUtils, syncobjs,
  ACBrNFe, ACBrNFeDANFeRLClass, ACBrMail,
  ACBrPosPrinter, ACBrNFeDANFeESCPOS, ACBrDANFCeFortesFr,
  ACBrLibConfig,  ACBrLibMailImport, ACBrLibPosPrinterImport;

type

  { TLibNFeDM }

  TLibNFeDM = class(TDataModule)
    ACBrNFe1: TACBrNFe;
    ACBrNFeDANFCeFortes1: TACBrNFeDANFCeFortes;
    ACBrNFeDANFeESCPOS1: TACBrNFeDANFeESCPOS;
    ACBrNFeDANFeRL1: TACBrNFeDANFeRL;

    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    FLock: TCriticalSection;
    FACBrMail: TACBrMail;
    FACBrPosPrinter: TACBrPosPrinter;

    FLibMail: TACBrLibMail;
    FLibPosPrinter: TACBrLibPosPrinter;
  public
    procedure CriarACBrMail;
    procedure CriarACBrPosPrinter;

    procedure AplicarConfiguracoes;
    procedure AplicarConfigMail;
    procedure AplicarConfigPosPrinter;
    procedure ConfigurarImpressao(NomeImpressora: String = ''; GerarPDF: Boolean = False;
                                  Protocolo: String = ''; MostrarPreview: String = ''; MarcaDagua: String = '';
                                  ViaConsumidor: String = ''; Simplificado: String = '');
    procedure GravarLog(AMsg: String; NivelLog: TNivelLog; Traduzir: Boolean = False);
    procedure Travar;
    procedure Destravar;
  end;

implementation

uses
  pcnConversao,
  ACBrUtil, FileUtil, ACBrNFeDANFEClass,
  ACBrLibNFeConfig, ACBrLibComum, ACBrLibNFeClass;

{$R *.lfm}

{ TLibNFeDM }

procedure TLibNFeDM.DataModuleCreate(Sender: TObject);
begin
  FLock := TCriticalSection.Create;

  FACBrMail := Nil;
  FLibMail := Nil;
  FACBrPosPrinter := Nil;
  FLibPosPrinter := Nil;
end;

procedure TLibNFeDM.DataModuleDestroy(Sender: TObject);
begin
  FLock.Destroy;

  if Assigned(FLibMail) then
    FreeAndNil(FLibMail)
  else if Assigned(FACBrMail) then
    FreeAndNil(FACBrMail);

  if Assigned(FLibPosPrinter) then
    FreeAndNil(FLibPosPrinter)
  else if Assigned(FACBrPosPrinter) then
    FreeAndNil(FACBrPosPrinter);
end;

procedure TLibNFeDM.CriarACBrMail;
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

  ACBrNFe1.MAIL := FACBrMail;
end;

procedure TLibNFeDM.CriarACBrPosPrinter;
var
  NomeLib: String;
begin
  if Assigned(FLibPosPrinter) or Assigned(FACBrPosPrinter) then
    Exit;

  GravarLog('  CriarACBrPosPrinter', logCompleto);

  NomeLib := ApplicationPath + CACBrPosPrinterLIBName;
  if FileExists(NomeLib) then
  begin
    GravarLog('      Carregando PosPrinter de: ' + NomeLib, logCompleto);
    // Criando Classe para Leitura da Lib //
    FLibPosPrinter  := TACBrLibPosPrinter.Create(NomeLib, pLib.Config.NomeArquivo, pLib.Config.ChaveCrypt);
    FACBrPosPrinter := FLibPosPrinter.ACBrPosPrinter;
  end
  else
  begin
    GravarLog('     Criando PosPrinter Interno', logCompleto);
    FACBrPosPrinter := TACBrPosPrinter.Create(Nil);
  end;

  ACBrNFeDANFeESCPOS1.PosPrinter := FACBrPosPrinter;
end;

procedure TLibNFeDM.AplicarConfiguracoes;
var
  pLibConfig: TLibNFeConfig;
begin
  ACBrNFe1.SSL.DescarregarCertificado;
  pLibConfig := TLibNFeConfig(TACBrLibNFe(pLib).Config);
  ACBrNFe1.Configuracoes.Assign(pLibConfig.NFeConfig);

  AplicarConfigMail;
  AplicarConfigPosPrinter;
end;

procedure TLibNFeDM.AplicarConfigMail;
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

procedure TLibNFeDM.AplicarConfigPosPrinter;
begin
  if Assigned(FLibPosPrinter) then
  begin
    FLibPosPrinter.ConfigLer(pLib.Config.NomeArquivo);
    Exit;
  end;

  with FACBrPosPrinter do
  begin
    ArqLog := pLib.Config.PosPrinter.ArqLog;
    Modelo := TACBrPosPrinterModelo(pLib.Config.PosPrinter.Modelo);
    Porta := pLib.Config.PosPrinter.Porta;
    Device.TimeOut := pLib.Config.PosPrinter.TimeOut;
    PaginaDeCodigo := TACBrPosPaginaCodigo(pLib.Config.PosPrinter.PaginaDeCodigo);
    ColunasFonteNormal := pLib.Config.PosPrinter.ColunasFonteNormal;
    EspacoEntreLinhas := pLib.Config.PosPrinter.EspacoEntreLinhas;
    LinhasEntreCupons := pLib.Config.PosPrinter.LinhasEntreCupons;
    CortaPapel := pLib.Config.PosPrinter.CortaPapel;
    TraduzirTags := pLib.Config.PosPrinter.TraduzirTags;
    IgnorarTags := pLib.Config.PosPrinter.IgnorarTags;
    LinhasBuffer := pLib.Config.PosPrinter.LinhasBuffer;
    ControlePorta := pLib.Config.PosPrinter.ControlePorta;
    VerificarImpressora := pLib.Config.PosPrinter.VerificarImpressora;

    ConfigBarras.MostrarCodigo := pLib.Config.PosPrinter.BcMostrarCodigo;
    ConfigBarras.LarguraLinha := pLib.Config.PosPrinter.BcLarguraLinha;
    ConfigBarras.Altura := pLib.Config.PosPrinter.BcAltura;
    ConfigBarras.Margem := pLib.Config.PosPrinter.BcMargem;

    ConfigQRCode.Tipo := pLib.Config.PosPrinter.QrTipo;
    ConfigQRCode.LarguraModulo := pLib.Config.PosPrinter.QrLarguraModulo;
    ConfigQRCode.ErrorLevel := pLib.Config.PosPrinter.QrErrorLevel;

    ConfigLogo.IgnorarLogo := pLib.Config.PosPrinter.LgIgnorarLogo;
    ConfigLogo.KeyCode1 := pLib.Config.PosPrinter.LgKeyCode1;
    ConfigLogo.KeyCode2 := pLib.Config.PosPrinter.LgKeyCode2;
    ConfigLogo.FatorX := pLib.Config.PosPrinter.LgFatorX;
    ConfigLogo.FatorY := pLib.Config.PosPrinter.LgFatorY;

    ConfigGaveta.SinalInvertido := pLib.Config.PosPrinter.GvSinalInvertido;
    ConfigGaveta.TempoON := pLib.Config.PosPrinter.GvTempoON;
    ConfigGaveta.TempoOFF := pLib.Config.PosPrinter.GvTempoOFF;

    ConfigModoPagina.Largura := pLib.Config.PosPrinter.MpLargura;
    ConfigModoPagina.Altura := pLib.Config.PosPrinter.MpAltura;
    ConfigModoPagina.Esquerda := pLib.Config.PosPrinter.MpEsquerda;
    ConfigModoPagina.Topo := pLib.Config.PosPrinter.MpTopo;
    ConfigModoPagina.Direcao := TACBrPosDirecao(pLib.Config.PosPrinter.MpDirecao);
    ConfigModoPagina.EspacoEntreLinhas := pLib.Config.PosPrinter.MpEspacoEntreLinhas;

    Device.ParamsString := pLib.Config.PosPrinter.DeviceParams;
  end;
end;

procedure TLibNFeDM.ConfigurarImpressao(NomeImpressora: String; GerarPDF: Boolean = False;
  Protocolo: String = ''; MostrarPreview: String = ''; MarcaDagua: String = '';
  ViaConsumidor: String = ''; Simplificado: String = '');
var
  pLibConfig: TLibNFeConfig;
begin
  pLibConfig := TLibNFeConfig(pLib.Config);

  if ACBrNFe1.NotasFiscais.Count > 0 then
  begin
    if ACBrNFe1.NotasFiscais.Items[0].NFe.Ide.modelo = 65 then
    begin
      if (pLibConfig.DANFeConfig.NFCeConfig.TipoRelatorioBobina = tpFortes) or GerarPDF then
        ACBrNFe1.DANFE := ACBrNFeDANFCeFortes1
      else
        ACBrNFe1.DANFE := ACBrNFeDANFeESCPOS1;
    end
    else
    begin
      ACBrNFe1.DANFE := ACBrNFeDANFeRL1;
    end;

    pLibConfig.DANFeConfig.Assign(ACBrNFe1.DANFE);

    if (ACBrNFe1.NotasFiscais.Items[0].NFe.procNFe.cStat in [101, 151, 155]) then
      ACBrNFe1.DANFE.Cancelada := True
    else
      ACBrNFe1.DANFE.Cancelada := False;

    if GerarPDF and not DirectoryExists(PathWithDelim(pLibConfig.DANFeConfig.PathPDF))then
        ForceDirectories(PathWithDelim(pLibConfig.DANFeConfig.PathPDF));
  end;

  if NaoEstaVazio(NomeImpressora) then
    ACBrNFe1.DANFE.Impressora := NomeImpressora;

  if NaoEstaVazio(MostrarPreview) then
    ACBrNFe1.DANFE.MostraPreview := StrToBoolDef(MostrarPreview, False);

  if NaoEstaVazio(Simplificado) then
  begin
    if StrToBoolDef(Simplificado, False) then
      ACBrNFe1.DANFE.TipoDANFE := tiSimplificado;
  end;

  if NaoEstaVazio(Protocolo) then
    ACBrNFe1.DANFE.Protocolo := Protocolo
  else
    ACBrNFe1.DANFE.Protocolo := '';

  if ACBrNFe1.DANFE = ACBrNFeDANFeRL1 then
  begin
     if NaoEstaVazio(MarcaDagua) then
       ACBrNFeDANFeRL1.MarcaDagua := MarcaDagua
     else
       ACBrNFeDANFeRL1.MarcaDagua := '';
  end;

  if ACBrNFe1.DANFE is TACBrNFeDANFCEClass then
  begin
     if NaoEstaVazio(ViaConsumidor) then
       TACBrNFeDANFCEClass(ACBrNFe1.DANFE).ViaConsumidor := StrToBoolDef(ViaConsumidor, False);
  end;

  if ACBrNFe1.DANFE = ACBrNFeDANFeESCPOS1 then
  begin
    if not ACBrNFeDANFeESCPOS1.PosPrinter.ControlePorta then
    begin
      ACBrNFeDANFeESCPOS1.PosPrinter.Ativar;
      if not ACBrNFeDANFeESCPOS1.PosPrinter.Device.Ativo then
        ACBrNFeDANFeESCPOS1.PosPrinter.Device.Ativar;
    end;
  end;
end;

procedure TLibNFeDM.GravarLog(AMsg: String; NivelLog: TNivelLog; Traduzir: Boolean);
begin
  if Assigned(pLib) then
    pLib.GravarLog(AMsg, NivelLog, Traduzir);
end;

procedure TLibNFeDM.Travar;
begin
  GravarLog('Travar', logParanoico);
  FLock.Acquire;
end;

procedure TLibNFeDM.Destravar;
begin
  GravarLog('Destravar', logParanoico);
  FLock.Release;
end;

end.
