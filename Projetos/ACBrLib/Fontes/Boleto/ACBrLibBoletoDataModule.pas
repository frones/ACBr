unit ACBrLibBoletoDataModule;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ACBrBoleto, ACBrBoletoFCFortesFr, syncobjs,
  ACBrLibConfig;

type

  { TLibBoletoDM }

  TLibBoletoDM = class(TDataModule)
    ACBrBoleto1: TACBrBoleto;
    ACBrBoletoFCFortes1: TACBrBoletoFCFortes;

    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    FLock: TCriticalSection;

  public
    procedure GravarLog(AMsg: String; NivelLog: TNivelLog; Traduzir: Boolean = False);
    procedure Travar;
    procedure Destravar;

  end;

var
  LibBoletoDM: TLibBoletoDM;

implementation

uses
  ACBrUtil, ACBrLibComum, FileUtil;

{$R *.lfm}

{ TLibBoletoDM }

procedure TLibBoletoDM.DataModuleCreate(Sender: TObject);
begin
  FLock := TCriticalSection.Create;
  {FACBrMail := Nil;
  FLibMail := Nil;}
end;

procedure TLibBoletoDM.DataModuleDestroy(Sender: TObject);
begin
  FLock.Destroy;
  {if Assigned(FLibMail) then
    FreeAndNil(FLibMail)
  else if Assigned(FACBrMail) then
    FreeAndNil(FACBrMail); }
end;

procedure TLibBoletoDM.GravarLog(AMsg: String; NivelLog: TNivelLog;
  Traduzir: Boolean);
begin
  if Assigned(pLib) then
    pLib.GravarLog(AMsg, NivelLog, Traduzir);
end;

procedure TLibBoletoDM.Travar;
begin
  GravarLog('Travar', logParanoico);
  FLock.Acquire;
end;

procedure TLibBoletoDM.Destravar;
begin
  GravarLog('Destravar', logParanoico);
  FLock.Release;
end;

{ TLibBoletoDM }

end.

