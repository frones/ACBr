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
    Modelo := pLibConfig.PosPrinterConfig.Modelo;
    Porta := pLibConfig.PosPrinterConfig.Porta;
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

    ConfigBarras.Assign(pLibConfig.PosPrinterConfig.ConfigBarras);
    ConfigQRCode.Assign(pLibConfig.PosPrinterConfig.ConfigQRCode);
    ConfigLogo.Assign(pLibConfig.PosPrinterConfig.ConfigLogo);
    ConfigGaveta.Assign(pLibConfig.PosPrinterConfig.ConfigGaveta);

    Device.Baud := pLibConfig.DeviceConfig.Baud;
    Device.Data := pLibConfig.DeviceConfig.Data;
    Device.Parity := pLibConfig.DeviceConfig.Parity;
    Device.Stop := pLibConfig.DeviceConfig.Stop;
    Device.MaxBandwidth := pLibConfig.DeviceConfig.MaxBandwidth;
    Device.SendBytesCount := pLibConfig.DeviceConfig.SendBytesCount;
    Device.SendBytesInterval := pLibConfig.DeviceConfig.SendBytesInterval;
    Device.HandShake := pLibConfig.DeviceConfig.HandShake;
    Device.SoftFlow := pLibConfig.DeviceConfig.SoftFlow;
    Device.HardFlow := pLibConfig.DeviceConfig.HardFlow;
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

