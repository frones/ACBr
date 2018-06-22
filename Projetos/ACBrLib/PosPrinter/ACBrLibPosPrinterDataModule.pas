unit ACBrLibPosPrinterDataModule;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, syncobjs,
  ACBrLibConfig, ACBrPosPrinter;

type

  { TLibPosPrinterDM }

  TLibPosPrinterDM = class(TDataModule)
    ACBrPosPrinter1: TACBrPosPrinter;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    FLock: TCriticalSection;

  public
    procedure AplicarConfiguracoes;
    procedure GravarLog(AMsg: String; NivelLog: TNivelLog; Traduzir: Boolean = False);
    procedure Travar;
    procedure Destravar;
  end;

implementation

uses
  ACBrUtil,
  ACBrLibPosPrinterConfig, ACBrLibComum, ACBrLibPosPrinterClass;

{$R *.lfm}

procedure TLibPosPrinterDM.DataModuleCreate(Sender: TObject);
begin
  FLock := TCriticalSection.Create;
end;

procedure TLibPosPrinterDM.DataModuleDestroy(Sender: TObject);
begin
  FLock.Destroy;
end;

procedure TLibPosPrinterDM.AplicarConfiguracoes;
var
  pLibConfig: TLibPosPrinterConfig;
begin
  pLibConfig := TLibPosPrinterConfig(TACBrLibPosPrinter(pLib).Config);

  with ACBrPosPrinter1 do
  begin
    ArqLog := pLibConfig.PosPrinterConfig.ArqLog;
    Modelo := pLibConfig.PosPrinterConfig.Modelo;
    Porta := pLibConfig.PosPrinterConfig.Porta;
    Device.TimeOut := pLibConfig.PosPrinterConfig.TimeOut;
    PaginaDeCodigo := pLibConfig.PosPrinterConfig.PaginaDeCodigo;
    ColunasFonteNormal := pLibConfig.PosPrinterConfig.ColunasFonteNormal;
    EspacoEntreLinhas := pLibConfig.PosPrinterConfig.EspacoEntreLinhas;
    LinhasEntreCupons := pLibConfig.PosPrinterConfig.LinhasEntreCupons;
    CortaPapel := pLibConfig.PosPrinterConfig.CortaPapel;
    TraduzirTags := pLibConfig.PosPrinterConfig.TraduzirTags;
    IgnorarTags := pLibConfig.PosPrinterConfig.IgnorarTags;
    LinhasBuffer := pLibConfig.PosPrinterConfig.LinhasBuffer;
    ControlePorta := pLibConfig.PosPrinterConfig.ControlePorta;
    VerificarImpressora := pLibConfig.PosPrinterConfig.VerificarImpressora;

    ConfigBarras.MostrarCodigo := pLibConfig.PosPrinterConfig.ConfigBarras.MostrarCodigo;
    ConfigBarras.LarguraLinha := pLibConfig.PosPrinterConfig.ConfigBarras.LarguraLinha;
    ConfigBarras.Altura := pLibConfig.PosPrinterConfig.ConfigBarras.Altura;
    ConfigBarras.Margem := pLibConfig.PosPrinterConfig.ConfigBarras.Margem;

    ConfigQRCode.Tipo := pLibConfig.PosPrinterConfig.ConfigQRCode.Tipo;
    ConfigQRCode.LarguraModulo := pLibConfig.PosPrinterConfig.ConfigQRCode.LarguraModulo;
    ConfigQRCode.ErrorLevel := pLibConfig.PosPrinterConfig.ConfigQRCode.ErrorLevel;

    ConfigLogo.IgnorarLogo := pLibConfig.PosPrinterConfig.ConfigLogo.IgnorarLogo;
    ConfigLogo.KeyCode1 := pLibConfig.PosPrinterConfig.ConfigLogo.KeyCode1;
    ConfigLogo.KeyCode2 := pLibConfig.PosPrinterConfig.ConfigLogo.KeyCode2;
    ConfigLogo.FatorX := pLibConfig.PosPrinterConfig.ConfigLogo.FatorX;
    ConfigLogo.FatorY := pLibConfig.PosPrinterConfig.ConfigLogo.FatorY;

    ConfigGaveta.SinalInvertido := pLibConfig.PosPrinterConfig.ConfigGaveta.SinalInvertido;
    ConfigGaveta.TempoON := pLibConfig.PosPrinterConfig.ConfigGaveta.TempoON;
    ConfigGaveta.TempoOFF := pLibConfig.PosPrinterConfig.ConfigGaveta.TempoOFF;

    ConfigModoPagina.Largura := pLibConfig.PosPrinterConfig.ConfigModoPagina.Largura;
    ConfigModoPagina.Altura := pLibConfig.PosPrinterConfig.ConfigModoPagina.Altura;
    ConfigModoPagina.Esquerda := pLibConfig.PosPrinterConfig.ConfigModoPagina.Esquerda;
    ConfigModoPagina.Topo := pLibConfig.PosPrinterConfig.ConfigModoPagina.Topo;
    ConfigModoPagina.Direcao := pLibConfig.PosPrinterConfig.ConfigModoPagina.Direcao;
    ConfigModoPagina.EspacoEntreLinhas := pLibConfig.PosPrinterConfig.ConfigModoPagina.EspacoEntreLinhas;

    Device.ParamsString := pLibConfig.PosPrinterConfig.DeviceParams;
  end;
end;

procedure TLibPosPrinterDM.GravarLog(AMsg: String; NivelLog: TNivelLog; Traduzir: Boolean);
begin
  if Assigned(pLib) then
    pLib.GravarLog(AMsg, NivelLog, Traduzir);
end;

procedure TLibPosPrinterDM.Travar;
begin
  GravarLog('Travar', logParanoico);
  FLock.Acquire;
end;

procedure TLibPosPrinterDM.Destravar;
begin
  GravarLog('Destravar', logParanoico);
  FLock.Release;
end;

end.

