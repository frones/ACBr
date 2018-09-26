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
  ACBrUtil, ACBrLibComum;

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
begin
  with ACBrPosPrinter1 do
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

