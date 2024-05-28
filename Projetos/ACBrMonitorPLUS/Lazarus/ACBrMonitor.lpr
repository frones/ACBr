program ACBrMonitor;

{$mode objfpc}{$H+}
//{$IMAGEBASE $400000}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, sysutils,IniFiles,// this includes the LCL widgetset
  {$IFDEF MSWINDOWS}
   Windows, Dialogs,
  {$ENDIF}
  Forms, printer4lazarus, lazcontrols, lhelpcontrolpkg, UtilUnit, ACBrMonitor1,
  CmdUnit, ConfiguraSerial, DoACBrUnit, DoBALUnit, DoCHQUnit, DoDISUnit,
  DoECFBemafi32, DoECFObserver, DoECFUnit, DoETQUnit, DoGAVUnit, DoLCBUnit,
  Sobre, DoBoletoUnit, DoCEPUnit, DoIBGEUnit, DoEmailUnit, DoNcmUnit,
  DoACBrNFeUnit, DoACBrCTeUnit, DoACBrMDFeUnit, DoSATUnit, DoACBrGNReUnit,
  ACBrBoletoRelatorioRetorno, DoPosPrinterUnit, SelecionarCertificado,
  DoACBrDFeUnit, ACBrMonitorConsts, ACBrMonitorConfig, DoACBrReinfUnit,
  DoACBreSocialUnit, lazreportpdfexport, ACBrLibeSocialConsts,
  ACBrLibeSocialRespostas, ACBrLibMDFeConsts, ACBrLibMDFeRespostas,
  ACBrLibReinfConsts, ACBrLibReinfRespostas, ACBrLibNFeRespostas,
  ACBrLibNFeConsts, ACBrLibSATConsts, ACBrLibSATRespostas, ACBrLibCEPRespostas,
  ACBrLibCertUtils, ACBrLibDeviceUtils, ACBrLibConsultaCNPJConsts,
  ACBrLibConsultaCPFRespostas, DoCNPJUnit, DoCPFUnit, ACBrLibBoletoRespostas,
  DoACBrGTINUnit, DoACBrNFSeUnit, ACBrLibNCMsRespostas, ACBrLibNFSeRespostas,
  ACBrLibNFSeConsts, ACBrLibNFSeConfig;

{$R *.res}
var
  Ini : TIniFile;
  UmaInstancia : Boolean;
begin
  Ini := TIniFile.Create(  ExtractFilePath(Application.ExeName)+ 'ACBrMonitor.ini' ) ;
  try
     UmaInstancia := Ini.ReadBool('ACBrMonitor','Uma_Instancia',false);
  finally
     Ini.Free;
  end;
  //DeleteFile( 'c:\temp\heaptrclog.trc');
  //SetHeapTraceOutput( 'c:\temp\heaptrclog.trc');
  Application.Initialize;

  {$IFDEF MSWINDOWS}
   if UmaInstancia then
   begin
     CreateMutex(nil, True, 'ACBrMonitor');
     if GetLastError = ERROR_ALREADY_EXISTS then
     begin
        MessageDlg('ACBrMonitor','O programa ACBrMonitor já está em execução',
            mtError, [mbOK], 0);
        Application.Terminate;
        exit ;
     end;
   end;
  {$ENDIF}

  Application.CreateForm(TFrmACBrMonitor, FrmACBrMonitor) ;
  Application.Run;
end.

