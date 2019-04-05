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
  ACBrUtil, ACBrLibPosPrinterConfig, ACBrLibComum;

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
  pLibConfig := TLibPosPrinterConfig(pLib.Config);

  with ACBrPosPrinter1 do
  begin
    ArqLog := pLibConfig.PosPrinter.ArqLog;
    Modelo := TACBrPosPrinterModelo(pLibConfig.PosPrinter.Modelo);
    Porta := pLibConfig.PosPrinter.Porta;
    PaginaDeCodigo := TACBrPosPaginaCodigo(pLibConfig.PosPrinter.PaginaDeCodigo);
    ColunasFonteNormal := pLibConfig.PosPrinter.ColunasFonteNormal;
    EspacoEntreLinhas := pLibConfig.PosPrinter.EspacoEntreLinhas;
    LinhasEntreCupons := pLibConfig.PosPrinter.LinhasEntreCupons;
    CortaPapel := pLibConfig.PosPrinter.CortaPapel;
    TraduzirTags := pLibConfig.PosPrinter.TraduzirTags;
    IgnorarTags := pLibConfig.PosPrinter.IgnorarTags;
    LinhasBuffer := pLibConfig.PosPrinter.LinhasBuffer;
    ControlePorta := pLibConfig.PosPrinter.ControlePorta;
    VerificarImpressora := pLibConfig.PosPrinter.VerificarImpressora;
    TipoCorte := TACBrPosTipoCorte(pLibConfig.PosPrinter.TipoCorte);

    ConfigBarras.MostrarCodigo := pLibConfig.PosPrinter.BcMostrarCodigo;
    ConfigBarras.LarguraLinha := pLibConfig.PosPrinter.BcLarguraLinha;
    ConfigBarras.Altura := pLibConfig.PosPrinter.BcAltura;
    ConfigBarras.Margem := pLibConfig.PosPrinter.BcMargem;

    ConfigQRCode.Tipo := pLibConfig.PosPrinter.QrTipo;
    ConfigQRCode.LarguraModulo := pLibConfig.PosPrinter.QrLarguraModulo;
    ConfigQRCode.ErrorLevel := pLibConfig.PosPrinter.QrErrorLevel;

    ConfigLogo.IgnorarLogo := pLibConfig.PosPrinter.LgIgnorarLogo;
    ConfigLogo.KeyCode1 := pLibConfig.PosPrinter.LgKeyCode1;
    ConfigLogo.KeyCode2 := pLibConfig.PosPrinter.LgKeyCode2;
    ConfigLogo.FatorX := pLibConfig.PosPrinter.LgFatorX;
    ConfigLogo.FatorY := pLibConfig.PosPrinter.LgFatorY;

    ConfigGaveta.SinalInvertido := pLibConfig.PosPrinter.GvSinalInvertido;
    ConfigGaveta.TempoON := pLibConfig.PosPrinter.GvTempoON;
    ConfigGaveta.TempoOFF := pLibConfig.PosPrinter.GvTempoOFF;

    ConfigModoPagina.Largura := pLibConfig.PosPrinter.MpLargura;
    ConfigModoPagina.Altura := pLibConfig.PosPrinter.MpAltura;
    ConfigModoPagina.Esquerda := pLibConfig.PosPrinter.MpEsquerda;
    ConfigModoPagina.Topo := pLibConfig.PosPrinter.MpTopo;
    ConfigModoPagina.Direcao := TACBrPosDirecao(pLibConfig.PosPrinter.MpDirecao);
    ConfigModoPagina.EspacoEntreLinhas := pLibConfig.PosPrinter.MpEspacoEntreLinhas;

    pLibConfig.DeviceConfig.Assign(Device);
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

