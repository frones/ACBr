unit FormPrincipal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Buttons, Spin, ComCtrls, Grids, ExtDlgs, ACBrAbecsPinPad, Types;

const
  CSerialSection = 'Serial';
  CLogSection = 'Log';
  CPinPadSection = 'PinPad';

type

  { TfrMain }

  TfrMain = class(TForm)
    ACBrAbecsPinPad1: TACBrAbecsPinPad;
    ApplicationProperties1: TApplicationProperties;
    btActivate: TBitBtn;
    btCancel: TButton;
    btCEX: TButton;
    btCLO: TButton;
    btDSP: TButton;
    btCLX: TButton;
    btDEX: TButton;
    btDMF: TButton;
    btDSI: TButton;
    btDSPClear: TButton;
    btDEXClear: TButton;
    btGCD: TButton;
    btGIN: TButton;
    btGIX: TButton;
    btGKY: TButton;
    btLMF: TButton;
    btMediaLoad: TButton;
    btMNU: TButton;
    btRMC: TButton;
    btSendQRCode: TButton;
    btOPN: TButton;
    btReadParms: TBitBtn;
    btSaveParams: TBitBtn;
    btSearchSerialPorts: TSpeedButton;
    btSearchSerialPorts1: TSpeedButton;
    btSerial: TSpeedButton;
    btACBrPinPadCapabilities: TButton;
    btPaintQRCode: TButton;
    cbCEXVerifyICCRemoval: TCheckBox;
    cbCEXVerifyCTLSPresence: TCheckBox;
    cbCEXVerifyMagnetic: TCheckBox;
    cbCEXVerifyICCInsertion: TCheckBox;
    cbSecure: TCheckBox;
    cbxMsgAlign: TComboBox;
    cbxGCD: TComboBox;
    cbxPort: TComboBox;
    cbGIXAll: TCheckBox;
    cbMsgWordWrap: TCheckBox;
    cbMNUHotKey: TCheckBox;
    cbCEXVerifyKey: TCheckBox;
    edGIXValue: TEdit;
    edMNUTitle: TEdit;
    edLogFile: TEdit;
    edMediaLoad: TEdit;
    edQRCodeImgName: TEdit;
    edtCLOMsg1: TEdit;
    edtCLOMsg2: TEdit;
    edtRMCMsg1: TEdit;
    edtRMCMsg2: TEdit;
    edtDSPMsg1: TEdit;
    edtDSPMsg2: TEdit;
    gbCLX: TGroupBox;
    gbDSX: TGroupBox;
    gbConfig: TGroupBox;
    gbConfig1: TGroupBox;
    gbExponent: TGroupBox;
    gbGKY: TGroupBox;
    gbModulus: TGroupBox;
    gbCLO: TGroupBox;
    gbMNU: TGroupBox;
    gbCEX: TGroupBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    gbDSP: TGroupBox;
    gbGIN: TGroupBox;
    gbGIX: TGroupBox;
    gbACBrPinPadCapabilities: TGroupBox;
    gbGCD: TGroupBox;
    ImageList1: TImageList;
    imgMedia: TImage;
    imgQRCode: TImage;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    lbCLXMedias: TListBox;
    lbLMFMedias: TListBox;
    lbGIXParams: TListBox;
    mCLX: TMemo;
    mDEX: TMemo;
    mACBrPinPadCapabilities: TMemo;
    mCEXResponse: TMemo;
    mMNU: TMemo;
    mGIXResponse: TMemo;
    mGINResponse: TMemo;
    mExponent: TMemo;
    mLog: TMemo;
    mModulus: TMemo;
    mQRCode: TMemo;
    OpenPictureDialog1: TOpenPictureDialog;
    Panel1: TPanel;
    Panel2: TPanel;
    pGCDResponse: TPanel;
    pGKYResponse: TPanel;
    pMNUResponse: TPanel;
    pgMedia: TPageControl;
    pGINIDX: TPanel;
    pGIXParams: TPanel;
    pLMFMediaTitle: TPanel;
    pCLXMediaTitle: TPanel;
    pgCLX: TPageControl;
    pCommands: TPanel;
    pCancelar: TPanel;
    pConfigLogMsg: TPanel;
    pgcCommands: TPageControl;
    pKeys: TPanel;
    pLogs: TPanel;
    pMedia: TPanel;
    pMediaFile: TPanel;
    pMediaInfo: TPanel;
    pMediaLoad: TPanel;
    pMFButtons: TPanel;
    pQREGerado: TPanel;
    pQREMemo: TPanel;
    sbCleanMemoLog: TSpeedButton;
    sbGenerateKeys: TSpeedButton;
    sbMedia: TScrollBox;
    sbResponse: TStatusBar;
    sbShowLogFile: TSpeedButton;
    seMNUTimeOut: TSpinEdit;
    seGIN_ACQIDX: TSpinEdit;
    seLogLevel: TSpinEdit;
    seGCDTimeOut: TSpinEdit;
    seCEXTimeOut: TSpinEdit;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    tsImage: TTabSheet;
    tsQRCode: TTabSheet;
    tsCLOLines: TTabSheet;
    tsCLOMedia: TTabSheet;
    tsAskEvent: TTabSheet;
    tsDisplay: TTabSheet;
    tsClose: TTabSheet;
    tsConfig: TTabSheet;
    tsGIX: TTabSheet;
    tsMultimidia: TTabSheet;
    tsOpen: TTabSheet;
    procedure ACBrAbecsPinPad1EndCommand(Sender: TObject);
    procedure ACBrAbecsPinPad1StartCommand(Sender: TObject);
    procedure ACBrAbecsPinPad1WaitForResponse(var Cancel: Boolean);
    procedure ACBrAbecsPinPad1WriteLog(const ALogLine: String;
      var Tratado: Boolean);
    procedure ApplicationProperties1Exception(Sender: TObject; E: Exception);
    procedure btACBrPinPadCapabilitiesClick(Sender: TObject);
    procedure btActivateClick(Sender: TObject);
    procedure btCEXClick(Sender: TObject);
    procedure btCLOClick(Sender: TObject);
    procedure btCLXClick(Sender: TObject);
    procedure btDEXClick(Sender: TObject);
    procedure btDMFClick(Sender: TObject);
    procedure btDSIClick(Sender: TObject);
    procedure btDEXClearClick(Sender: TObject);
    procedure btDSPClearClick(Sender: TObject);
    procedure btDSPClick(Sender: TObject);
    procedure btGCDClick(Sender: TObject);
    procedure btGINClick(Sender: TObject);
    procedure btGIXClick(Sender: TObject);
    procedure btGKYClick(Sender: TObject);
    procedure btMediaLoadClick(Sender: TObject);
    procedure btLMFClick(Sender: TObject);
    procedure btMNUClick(Sender: TObject);
    procedure btOPNClick(Sender: TObject);
    procedure btReadParmsClick(Sender: TObject);
    procedure btSearchSerialPorts1Click(Sender: TObject);
    procedure btSearchSerialPortsClick(Sender: TObject);
    procedure btRMCClick(Sender: TObject);
    procedure btSaveParamsClick(Sender: TObject);
    procedure btSendQRCodeClick(Sender: TObject);
    procedure btSerialClick(Sender: TObject);
    procedure btCancelClick(Sender: TObject);
    procedure btPaintQRCodeClick(Sender: TObject);
    procedure cbGIXAllChange(Sender: TObject);
    procedure cbSecureChange(Sender: TObject);
    procedure cbMNUHotKeyChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure sbCleanMemoLogClick(Sender: TObject);
    procedure sbGenerateKeysClick(Sender: TObject);
    procedure sbShowLogFileClick(Sender: TObject);
  protected
    function ConfigFileName: String;
    procedure SaveParams;
    procedure ReadParams;

    procedure FindSerialPorts(AStringList: TStrings);
    procedure ShowResponseStatusBar;
    procedure AddStrToLog(const AStr: String);
    procedure ConfigPanelsCommands(IsActive: Boolean);
    procedure ConfigPanelCancel(IsCancel: Boolean);
    procedure ConfigACBrAbecsPinPad;

    procedure LoadMediaNames;
    procedure ShowMediaDimensions;
    function PP_StrToInt(const PP: String): Word;
  public

  end;

var
  frMain: TfrMain;

implementation

uses
  TypInfo, IniFiles, DateUtils, Math,
  configuraserial,
  ACBrImage, ACBrDelphiZXingQRCode,
  ACBrUtiL.FilesIO,
  ACBrUtil.Strings;

{$R *.lfm}

{ TfrMain }

procedure TfrMain.FormCreate(Sender: TObject);
var
  i: TACBrAbecsMsgAlign;
  j: TACBrAbecsMSGIDX;
begin
  pCancelar.Visible := False;
  ConfigPanelCancel(False);
  ConfigPanelsCommands(False);
  pgcCommands.ActivePageIndex := 0;
  pgCLX.ActivePageIndex := 0;
  pgMedia.ActivePageIndex := 0;
  FindSerialPorts(cbxPort.Items);

  cbxMsgAlign.Items.Clear;
  For i := Low(TACBrAbecsMsgAlign) to High(TACBrAbecsMsgAlign) do
    cbxMsgAlign.Items.Add( GetEnumName(TypeInfo(TACBrAbecsMsgAlign), integer(i) ) ) ;

  cbxGCD.Items.Clear;
  For j := Low(TACBrAbecsMSGIDX) to High(TACBrAbecsMSGIDX) do
    cbxGCD.Items.Add( GetEnumName(TypeInfo(TACBrAbecsMSGIDX), integer(j) ) ) ;

  ReadParams;
  ShowMediaDimensions;
  btPaintQRCode.Click;
end;

procedure TfrMain.sbCleanMemoLogClick(Sender: TObject);
begin
  mLog.Lines.Clear;
end;

procedure TfrMain.sbGenerateKeysClick(Sender: TObject);
begin
  // TODO: Generate new Keys
end;

procedure TfrMain.sbShowLogFileClick(Sender: TObject);
var
  AFileLog: String;
begin
  if pos(PathDelim,edLogFile.Text) = 0 then
    AFileLog := ApplicationPath + edLogFile.Text
  else
    AFileLog := edLogFile.Text;

  OpenURL( AFileLog );
end;

function TfrMain.ConfigFileName: String;
begin
  Result := ChangeFileExt( Application.ExeName,'.ini' ) ;
end;

procedure TfrMain.SaveParams;
Var
  ini: TIniFile ;
begin
  AddStrToLog('- SaveParams');

  ini := TIniFile.Create(ConfigFileName);
  try
    ini.WriteString(CSerialSection, 'Port', cbxPort.Text);
    ini.WriteString(CSerialSection,'ParamsString', ACBrAbecsPinPad1.Device.ParamsString);
    ini.WriteString(CLogSection, 'File', edLogFile.Text);
    ini.WriteInteger(CLogSection, 'Level', seLogLevel.Value);
    ini.WriteInteger(CPinPadSection, 'MsgAlign', cbxMsgAlign.ItemIndex);
    ini.WriteBool(CPinPadSection, 'MsgWordWrap', cbMsgWordWrap.Checked);
  finally
    ini.Free ;
  end ;
end;

procedure TfrMain.ReadParams;
Var
  ini: TIniFile ;
begin
  AddStrToLog('- ReadParams');

  ini := TIniFile.Create(ConfigFileName);
  try
    cbxPort.Text := ini.ReadString(CSerialSection, 'Port', '');
    ACBrAbecsPinPad1.Device.ParamsString := ini.ReadString(CSerialSection,'ParamsString', '');
    edLogFile.Text := ini.ReadString(CLogSection, 'File', '');
    seLogLevel.Value := ini.ReadInteger(CLogSection, 'Level', 2);
    cbxMsgAlign.ItemIndex := ini.ReadInteger(CPinPadSection, 'MsgAlign', 3);
    cbMsgWordWrap.Checked := ini.ReadBool(CPinPadSection, 'MsgWordWrap', True);
  finally
    ini.Free ;
  end ;
end;

procedure TfrMain.FindSerialPorts(AStringList: TStrings);
begin
  AStringList.Clear;
  ACBrAbecsPinPad1.Device.AcharPortasSeriais( AStringList );
  {$IfNDef MSWINDOWS}
   AStringList.Add('/dev/ttyS0') ;
   AStringList.Add('/dev/ttyUSB0') ;
  {$EndIf}
end;

procedure TfrMain.ShowResponseStatusBar;
begin
  sbResponse.Panels[0].Text := Format('STAT: %d', [ACBrAbecsPinPad1.Response.STAT]);
  sbResponse.Panels[1].Text := ReturnStatusCodeDescription(ACBrAbecsPinPad1.Response.STAT);
end;

procedure TfrMain.AddStrToLog(const AStr: String);
begin
  mLog.Lines.Add(AStr);
end;

procedure TfrMain.ConfigPanelsCommands(IsActive: Boolean);
var
  i: Integer;
begin
  if IsActive then
  begin
    btActivate.Caption := 'Desativar';
    btActivate.Tag := 1;
  end
  else
  begin
    btActivate.Caption := 'Ativar';
    btActivate.Tag := 0;
  end;

  for i := 1 to pgcCommands.PageCount-1 do
    pgcCommands.Pages[i].Enabled := IsActive;
end;

procedure TfrMain.ConfigPanelCancel(IsCancel: Boolean);
var
  i: Integer;
begin
  pCancelar.Visible := IsCancel;
  for i := 1 to pgcCommands.PageCount-1 do
    pgcCommands.Pages[i].Enabled := not IsCancel;
end;


procedure TfrMain.ConfigACBrAbecsPinPad;
begin
  ACBrAbecsPinPad1.LogFile := edLogFile.Text;
  ACBrAbecsPinPad1.LogLevel := seLogLevel.Value;
  ACBrAbecsPinPad1.Port := cbxPort.Text;
  ACBrAbecsPinPad1.MsgAlign := TACBrAbecsMsgAlign(cbxMsgAlign.ItemIndex);
  ACBrAbecsPinPad1.MsgWordWrap := cbMsgWordWrap.Checked;
end;

procedure TfrMain.LoadMediaNames;
var
  sl: TStringList;
begin
  ACBrAbecsPinPad1.LMF;

  sl := TStringList.Create;
  try
    ACBrAbecsPinPad1.Response.GetResponseFromTagValue(PP_MFNAME, sl);
    lbLMFMedias.Items.Assign(sl);
    if (sl.Count < 1) then
      pLMFMediaTitle.Caption := 'No media files'
    else
      pLMFMediaTitle.Caption := Format('%d media file(s)', [sl.Count]);

    pCLXMediaTitle.Caption := pLMFMediaTitle.Caption;
    lbCLXMedias.Items.Assign(sl);
  finally
    sl.Free;
  end;
end;

procedure TfrMain.ShowMediaDimensions;
begin
  pMediaInfo.Caption := Format('w:%d x h:%d', [imgMedia.Picture.Width, imgMedia.Picture.Height]);
end;

function TfrMain.PP_StrToInt(const PP: String): Word;
var
  s: String;
begin
  s := UpperCase(Trim(PP));

  if (s = 'PP_SERNUM') then
    Result := PP_SERNUM
  else if (s = 'PP_PARTNBR') then
    Result := PP_PARTNBR
  else if (s = 'PP_MODEL') then
    Result := PP_MODEL
  else if (s = 'PP_MNNAME') then
    Result := PP_MNNAME
  else if (s = 'PP_CAPAB') then
    Result := PP_CAPAB
  else if (s = 'PP_SOVER') then
    Result := PP_SOVER
  else if (s = 'PP_SPECVER') then
    Result := PP_SPECVER
  else if (s = 'PP_MANVERS') then
    Result := PP_MANVERS
  else if (s = 'PP_APPVERS') then
    Result := PP_APPVERS
  else if (s = 'PP_GENVERS') then
    Result := PP_GENVERS
  else if (s = 'PP_KRNLVER') then
    Result := PP_KRNLVER
  else if (s = 'PP_CTLSVER') then
    Result := PP_CTLSVER
  else if (s = 'PP_MCTLSVER') then
    Result := PP_MCTLSVER
  else if (s = 'PP_VCTLSVER') then
    Result := PP_VCTLSVER
  else if (s = 'PP_AECTLSVER') then
    Result := PP_AECTLSVER
  else if (s = 'PP_DPCTLSVER') then
    Result := PP_DPCTLSVER
  else if (s = 'PP_PUREVER') then
    Result := PP_PUREVER
  else if (s = 'PP_DSPTXTSZ') then
    Result := PP_DSPTXTSZ
  else if (s = 'PP_DSPGRSZ') then
    Result := PP_DSPGRSZ
  else if (s = 'PP_MFSUP') then
    Result := PP_MFSUP
  else if (s = 'PP_MKTDESP') then
    Result := PP_MKTDESP
  else if (s = 'PP_MKTDESD') then
    Result := PP_MKTDESD
  else if (s = 'PP_DKPTTDESP') then
    Result := PP_DKPTTDESP
  else if (s = 'PP_DKPTTDESD') then
    Result := PP_DKPTTDESD
  else if (s = 'PP_EVENT') then
    Result := PP_EVENT
  else if (s = 'PP_TRK1INC') then
    Result := PP_TRK1INC
  else if (s = 'PP_TRK2INC') then
    Result := PP_TRK2INC
  else if (s = 'PP_TRK3INC') then
    Result := PP_TRK3INC
  else if (s = 'PP_TRACK1') then
    Result := PP_TRACK1
  else if (s = 'PP_TRACK2') then
    Result := PP_TRACK2
  else if (s = 'PP_TRACK3') then
    Result := PP_TRACK3
  else if (s = 'PP_TRK1KSN') then
    Result := PP_TRK1KSN
  else if (s = 'PP_TRK2KSN') then
    Result := PP_TRK2KSN
  else if (s = 'PP_TRK3KSN') then
    Result := PP_TRK3KSN
  else if (s = 'PP_ENCPAN') then
    Result := PP_ENCPAN
  else if (s = 'PP_ENCPANKSN') then
    Result := PP_ENCPANKSN
  else if (s = 'PP_KSN') then
    Result := PP_KSN
  else if (s = 'PP_VALUE') then
    Result := PP_VALUE
  else if (s = 'PP_DATAOUT') then
    Result := PP_DATAOUT
  else if (s = 'PP_CARDTYPE') then
    Result := PP_CARDTYPE
  else if (s = 'PP_ICCSTAT') then
    Result := PP_ICCSTAT
  else if (s = 'PP_AIDTABINFO') then
    Result := PP_AIDTABINFO
  else if (s = 'PP_PAN') then
    Result := PP_PAN
  else if (s = 'PP_PANSEQNO') then
    Result := PP_PANSEQNO
  else if (s = 'PP_EMVDATA') then
    Result := PP_EMVDATA
  else if (s = 'PP_CHNAME') then
    Result := PP_CHNAME
  else if (s = 'PP_GOXRES') then
    Result := PP_GOXRES
  else if (s = 'PP_PINBLK') then
    Result := PP_PINBLK
  else if (s = 'PP_FCXRES') then
    Result := PP_FCXRES
  else if (s = 'PP_ISRESULTS') then
    Result := PP_ISRESULTS
  else if (s = 'PP_BIGRAND') then
    Result := PP_BIGRAND
  else if (s = 'PP_LABEL') then
    Result := PP_LABEL
  else if (s = 'PP_ISSCNTRY') then
    Result := PP_ISSCNTRY
  else if (s = 'PP_CARDEXP') then
    Result := PP_CARDEXP
  else if (s = 'PP_MFNAME') then
    Result := PP_MFNAME
  else if (s = 'PP_DEVTYPE') then
    Result := PP_DEVTYPE
  else if (s = 'PP_TLRMEM') then
    Result := PP_TLRMEM
  else if (s = 'PP_ENCKRAND') then
    Result := PP_ENCKRAND
  else if (s = 'PP_KSNTDESP00') then
    Result := PP_KSNTDESP00
  else if (s = 'PP_KSNTDESP63') then
    Result := PP_KSNTDESP63
  else if (s = 'PP_KSNTDESD00') then
    Result := PP_KSNTDESD00
  else if (s = 'PP_KSNTDESD63') then
    Result := PP_KSNTDESD63
  else if (s = 'PP_TABVER00') then
    Result := PP_TABVER00
  else if (s = 'PP_TABVER63') then
    Result := PP_TABVER63;
end;

procedure TfrMain.btSerialClick(Sender: TObject);
var
  frConfiguraSerial: TfrConfiguraSerial;
begin
  frConfiguraSerial := TfrConfiguraSerial.Create(self);
  try
    frConfiguraSerial.Device.Porta        := ACBrAbecsPinPad1.Device.Porta ;
    frConfiguraSerial.cmbPortaSerial.Text := cbxPort.Text ;
    frConfiguraSerial.Device.ParamsString := ACBrAbecsPinPad1.Device.ParamsString ;

    if frConfiguraSerial.ShowModal = mrOk then
    begin
      cbxPort.Text := frConfiguraSerial.cmbPortaSerial.Text ;
      ACBrAbecsPinPad1.Device.ParamsString := frConfiguraSerial.Device.ParamsString ;
    end ;
  finally
    FreeAndNil( frConfiguraSerial ) ;
  end ;
end;

procedure TfrMain.btSearchSerialPortsClick(Sender: TObject);
begin
  FindSerialPorts(cbxPort.Items);
  if (cbxPort.ItemIndex < 0) and (cbxPort.Items.Count>0) then
    cbxPort.ItemIndex := 0;
end;

procedure TfrMain.btActivateClick(Sender: TObject);
begin
  if not ACBrAbecsPinPad1.IsEnabled then
  begin
    SaveParams;
    ConfigACBrAbecsPinPad;
  end;

  ACBrAbecsPinPad1.IsEnabled := (btActivate.Tag = 0);
  ConfigPanelsCommands(ACBrAbecsPinPad1.IsEnabled);
  if ACBrAbecsPinPad1.IsEnabled then
  begin
    LoadMediaNames;
    pgcCommands.ActivePageIndex := 1;
  end;
end;

procedure TfrMain.ACBrAbecsPinPad1EndCommand(Sender: TObject);
begin
  ConfigPanelCancel(False);
  ShowResponseStatusBar;
end;

procedure TfrMain.ACBrAbecsPinPad1StartCommand(Sender: TObject);
begin
  if ACBrAbecsPinPad1.Command.IsBlocking then
    ConfigPanelCancel(True);
end;

procedure TfrMain.ACBrAbecsPinPad1WaitForResponse(var Cancel: Boolean);
begin
  Application.ProcessMessages;
  Cancel := not pCancelar.Visible;
end;

procedure TfrMain.ACBrAbecsPinPad1WriteLog(const ALogLine: String;
  var Tratado: Boolean);
begin
  AddStrToLog(ALogLine);
  Tratado := False;
end;

procedure TfrMain.ApplicationProperties1Exception(Sender: TObject; E: Exception);
begin
  AddStrToLog('');
  AddStrToLog('** '+E.ClassName+' **');
  AddStrToLog(E.Message);
  ShowResponseStatusBar;
end;

procedure TfrMain.btACBrPinPadCapabilitiesClick(Sender: TObject);
begin
  with ACBrAbecsPinPad1.PinPadCapabilities do
  begin
    mACBrPinPadCapabilities.Lines.Add('SerialNumber: '+SerialNumber);
    mACBrPinPadCapabilities.Lines.Add('PartNumber: '+PartNumber);
    mACBrPinPadCapabilities.Lines.Add('Model: '+Model);
    mACBrPinPadCapabilities.Lines.Add('Memory: '+Memory);
    mACBrPinPadCapabilities.Lines.Add('Manufacturer: '+Manufacturer);
    mACBrPinPadCapabilities.Lines.Add('SupportContactless: '+BoolToStr(SupportContactless, True));
    mACBrPinPadCapabilities.Lines.Add('DisplayIsGraphic: '+BoolToStr(DisplayIsGraphic, True));
    mACBrPinPadCapabilities.Lines.Add('DisplayIsColor: '+BoolToStr(DisplayIsColor, True));
    mACBrPinPadCapabilities.Lines.Add('SpecificationVersion: '+FloatToStr(SpecificationVersion));
    mACBrPinPadCapabilities.Lines.Add('DisplayTextModeDimensions.Rows: '+IntToStr(DisplayTextModeDimensions.Rows));
    mACBrPinPadCapabilities.Lines.Add('DisplayTextModeDimensions.Cols: '+IntToStr(DisplayTextModeDimensions.Cols));
    mACBrPinPadCapabilities.Lines.Add('DisplayGraphicPixels.Rows: '+IntToStr(DisplayGraphicPixels.Rows));
    mACBrPinPadCapabilities.Lines.Add('DisplayGraphicPixels.Cols: '+IntToStr(DisplayGraphicPixels.Cols));
    mACBrPinPadCapabilities.Lines.Add('MediaPNGisSupported: '+BoolToStr(MediaPNGisSupported, True));
    mACBrPinPadCapabilities.Lines.Add('MediaJPGisSupported: '+BoolToStr(MediaJPGisSupported, True));
    mACBrPinPadCapabilities.Lines.Add('MediaGIFisSupported: '+BoolToStr(MediaGIFisSupported, True));
  end;
end;

procedure TfrMain.btCEXClick(Sender: TObject);
var
  s: String;

  procedure AddResponseToLog(t: Word);
  var
    v: AnsiString;
  begin
    v := ACBrAbecsPinPad1.Response.GetResponseFromTagValue(t);
    if (Trim(v) = '') then
      Exit;

    mCEXResponse.Lines.Add(PP_ToStr(t)+' => '+v);
  end;
begin
  s := '';
  if cbCEXVerifyKey.Checked then
    s := s + 'Press Key'+CR;
  if cbCEXVerifyMagnetic.Checked then
    s := s + 'Swipe the card'+CR;
  if cbCEXVerifyICCInsertion.Checked then
    s := s + 'Insert card'+CR;
  if cbCEXVerifyICCRemoval.Checked then
    s := s + 'Remove card'+CR;
  if cbCEXVerifyCTLSPresence.Checked then
    s := s + 'Bring card closer'+CR;

  s := Trim(s);
  mCEXResponse.Lines.Add('------------------------------');
  mCEXResponse.Lines.Add(s);
  ACBrAbecsPinPad1.DEX(s);
  try
    ACBrAbecsPinPad1.CEX( cbCEXVerifyKey.Checked,
                          cbCEXVerifyMagnetic.Checked,
                          cbCEXVerifyICCInsertion.Checked,
                          cbCEXVerifyICCRemoval.Checked,
                          cbCEXVerifyCTLSPresence.Checked,
                          seCEXTimeOut.Value);
  except
    On E: EACBrAbecsPinPadTimeout do
    begin
      ACBrAbecsPinPad1.DEX('TIMEOUT');
      mCEXResponse.Lines.Add('* TIMEOUT *');
      Exit;
    end
    else
    begin
      ACBrAbecsPinPad1.DEX();
      raise;
    end;
  end;

  mCEXResponse.Lines.Add('');
  AddResponseToLog(PP_EVENT);
  AddResponseToLog(PP_VALUE);
  AddResponseToLog(PP_DATAOUT);
  AddResponseToLog(PP_CARDTYPE);
  AddResponseToLog(PP_PAN);
  AddResponseToLog(PP_PANSEQNO);
  AddResponseToLog(PP_CHNAME);
  AddResponseToLog(PP_LABEL);
  AddResponseToLog(PP_ISSCNTRY);
  AddResponseToLog(PP_CARDEXP);
  AddResponseToLog(PP_DEVTYPE);
  AddResponseToLog(PP_TRK1INC);
  AddResponseToLog(PP_TRK2INC);
  AddResponseToLog(PP_TRK3INC);

  s := Trim(ACBrAbecsPinPad1.Response.GetResponseFromTagValue(PP_EVENT));
  if (s <> '') then
    s := 'Event: '+s;
  ACBrAbecsPinPad1.DEX(s);
end;

procedure TfrMain.btDEXClick(Sender: TObject);
begin
  ACBrAbecsPinPad1.DEX(mDEX.Lines.Text);
end;

procedure TfrMain.btDMFClick(Sender: TObject);
var
  sl: TStringList;
  s: String;
  i: Integer;
begin
  if (lbLMFMedias.SelCount < 1) then
    raise Exception.Create('No Media selected');

  if (MessageDlg( 'DELETE MEDIA?',
                  Format('Delete %d Media file(s)',[lbLMFMedias.SelCount]),
                  mtConfirmation,
                  mbYesNo, 0, mbNo) <> mrYes) then
    Exit;

  s := '';
  for i := 0 to lbLMFMedias.Items.Count - 1 do
  begin
    if lbLMFMedias.Selected[i] then
    begin
      if (s = '') then
        s := lbLMFMedias.Items[i]
      else
        s := s + CR+LF + lbLMFMedias.Items[i];
    end;
  end;

  if (lbLMFMedias.SelCount = 1) then
    ACBrAbecsPinPad1.DMF(s)
  else
  begin
    sl := TStringList.Create;
    try
      sl.Text := s;
      ACBrAbecsPinPad1.DMF(sl);
    finally
      sl.Free;
    end;
  end;

  LoadMediaNames;
end;

procedure TfrMain.btDSIClick(Sender: TObject);
var
  i: Integer;
begin
  if (lbLMFMedias.SelCount < 1) then
    raise Exception.Create('No Media selected');

  for i := 0 to lbLMFMedias.Count-1 do
  begin
    if (lbLMFMedias.Selected[i]) then
      ACBrAbecsPinPad1.DSI(lbLMFMedias.Items[i])
  end;
end;

procedure TfrMain.btDEXClearClick(Sender: TObject);
begin
  ACBrAbecsPinPad1.DEX();
end;

procedure TfrMain.btDSPClearClick(Sender: TObject);
begin
  ACBrAbecsPinPad1.DSP();
end;

procedure TfrMain.btDSPClick(Sender: TObject);
begin
  ACBrAbecsPinPad1.DSP(edtDSPMsg1.Text, edtDSPMsg2.Text);
end;

procedure TfrMain.btGCDClick(Sender: TObject);
var
  s: String;
begin
  pGCDResponse.Caption := cbxGCD.Items[cbxGCD.ItemIndex];
  try
    s := ACBrAbecsPinPad1.GCD(TACBrAbecsMSGIDX(cbxGCD.ItemIndex), seGCDTimeOut.Value);
    pGCDResponse.Caption := s;
  except
    On E: EACBrAbecsPinPadTimeout do
      pGCDResponse.Caption := 'USER TIMEOUT';
    else
      raise;
  end;
end;

procedure TfrMain.btGINClick(Sender: TObject);
begin
  ACBrAbecsPinPad1.GIN(seGIN_ACQIDX.Value);
  mGINResponse.Lines.Text := ACBrAbecsPinPad1.Response.GetResponseData;
end;

procedure TfrMain.btGIXClick(Sender: TObject);
var
  i: Integer;
  p: Word;
  PP_DATA: Array of Word;
  sl: TStringList;
  s, n: String;

  procedure AddParam(AParam: Word);
  var
    l: Integer;
  begin
    l := Length(PP_DATA);
    SetLength(PP_DATA, l+1);
    PP_DATA[l] := AParam;
  end;

begin
  if (lbGIXParams.Count < 1) then
    ACBrAbecsPinPad1.GIX
  else
  begin
    for i := 0 to lbGIXParams.Count-1 do
    begin
      if (lbGIXParams.Selected[i]) then
       AddParam(PP_StrToInt(lbGIXParams.Items[i]));
    end;

    if (edGIXValue.Text <> '') then
    begin
      p := StrToIntDef(edGIXValue.Text, 0);
      if (p > 0) then
        AddParam(p);
    end;

    ACBrAbecsPinPad1.GIX(PP_DATA);
  end;

  mGIXResponse.Lines.Clear;
  sl := TStringList.Create;
  try
    ACBrAbecsPinPad1.Response.GetResponseAsValues(sl);
    for i := 0 to sl.Count-1 do
    begin
      n := sl.Names[i];
      p := StrToIntDef(n, -1);
      if (p > 0) then
        s := PP_ToStr(p)
      else
        s := n;

      mGIXResponse.Lines.Add(s+' => '+sl.Values[n]);
    end;
  finally
    sl.Free;
  end;
end;

procedure TfrMain.btGKYClick(Sender: TObject);
var
  i: Integer;
  s: String;
begin
  i := 0;
  s := 'PRESS FUNCTION KEY';
  pGKYResponse.Caption := s;
  ACBrAbecsPinPad1.DSP(s);

  try
    i := ACBrAbecsPinPad1.GKY;
  except
    On E: EACBrAbecsPinPadTimeout do
      pGKYResponse.Caption := 'USER TIMEOUT';
    else
      raise;
  end;

  s := Format('Key Number: %d',[i]);
  ACBrAbecsPinPad1.DSP(s);
  pGKYResponse.Caption := s;
end;

procedure TfrMain.btMediaLoadClick(Sender: TObject);
var
  ms: TMemoryStream;
  tini, tfim: TDateTime;
begin
  ms := TMemoryStream.Create;
  try
    imgMedia.Picture.SaveToStream(ms);
    try
      tini := Now;
      mLog.Lines.Add('Start Loading '+edMediaLoad.Text);
      mLog.Lines.BeginUpdate;
      ACBrAbecsPinPad1.LoadMedia( edMediaLoad.Text,
                                  ms,
                                  TACBrAbecsPinPadMediaType(imgMedia.Tag) );
    finally
      mLog.Lines.EndUpdate;
    end;

    tfim := Now;
    LoadMediaNames;
    ACBrAbecsPinPad1.DSI(edMediaLoad.Text);
    mLog.ScrollBy(0, mLog.Lines.Count);
    mLog.Lines.Add('Done Loading '+edMediaLoad.Text+', '+FormatFloat('##0.000',SecondSpan(tini,tfim))+' seconds' );
  finally
    ms.Free;
  end;
end;

procedure TfrMain.btLMFClick(Sender: TObject);
begin
  LoadMediaNames;
end;

procedure TfrMain.btMNUClick(Sender: TObject);
var
  s: String;
begin
  s := '';
  pMNUResponse.Caption := 'SELECT ON PINPAD';
  try
    s := ACBrAbecsPinPad1.MNU(mMNU.Lines,edMNUTitle.Text, seMNUTimeOut.Value);
  except
    On E: EACBrAbecsPinPadTimeout do
      pMNUResponse.Caption := 'USER TIMEOUT';
    else
      raise;
  end;

  pMNUResponse.Caption := s;
end;

procedure TfrMain.btOPNClick(Sender: TObject);
begin
  if not cbSecure.Checked then
    ACBrAbecsPinPad1.OPN
  else
    ACBrAbecsPinPad1.OPN( Trim(mModulus.Text), Trim(mExponent.Text) );
end;

procedure TfrMain.btRMCClick(Sender: TObject);
begin
  ACBrAbecsPinPad1.RMC(edtRMCMsg1.Text, edtRMCMsg2.Text);
end;

procedure TfrMain.btSaveParamsClick(Sender: TObject);
begin
  SaveParams;
end;

procedure TfrMain.btSendQRCodeClick(Sender: TObject);
var
  ms: TMemoryStream;
  tini, tfim: TDateTime;
  png: TPortableNetworkGraphic;
  qrsize: Integer;
begin
  ms := TMemoryStream.Create;
  png := TPortableNetworkGraphic.Create;
  try
    qrsize := Trunc(min( ACBrAbecsPinPad1.PinPadCapabilities.DisplayGraphicPixels.Cols,
                         ACBrAbecsPinPad1.PinPadCapabilities.DisplayGraphicPixels.Rows)) - 20;
    png.Width := qrsize;
    png.Height := qrsize;
    png.Canvas.StretchDraw(png.Canvas.ClipRect, imgQRCode.Picture.Bitmap);
    //png.SaveToFile('c:\temp\qrcode.png');
    png.SaveToStream(ms);
    imgQRCode.Picture.Assign(png);

    try
      tini := Now;
      mLog.Lines.Add('Start Loading '+edQRCodeImgName.Text);
      mLog.Lines.BeginUpdate;
      ACBrAbecsPinPad1.LoadMedia( edQRCodeImgName.Text, ms, mtPNG);
    finally
      mLog.Lines.EndUpdate;
    end;

    tfim := Now;
    LoadMediaNames;
    ACBrAbecsPinPad1.DSI(edQRCodeImgName.Text);
    mLog.ScrollBy(0, mLog.Lines.Count);
    mLog.Lines.Add('Done Loading '+edQRCodeImgName.Text+', '+FormatFloat('##0.000',SecondSpan(tini,tfim))+' seconds' );
  finally
    ms.Free;
    png.Free;
  end;
end;

procedure TfrMain.btReadParmsClick(Sender: TObject);
begin
  ReadParams;
end;

procedure TfrMain.btSearchSerialPorts1Click(Sender: TObject);
var
  filename, ext: String;
begin
  if OpenPictureDialog1.Execute then
  begin
    ext := LowerCase(ExtractFileExt(OpenPictureDialog1.FileName));
    filename := StringReplace(ExtractFileName(OpenPictureDialog1.FileName), ext, '', []);
    edMediaLoad.Text := ACBrAbecsPinPad1.FormatSPE_MFNAME( filename );
    imgMedia.Picture.Clear;
    imgMedia.Picture.LoadFromFile(OpenPictureDialog1.FileName);
    ShowMediaDimensions;
    if (ext = '.gif') then
      imgMedia.Tag := Integer(mtGIF)
    else if (ext = '.png') then
      imgMedia.Tag := Integer(mtPNG)
    else
      imgMedia.Tag := Integer(mtJPG);
  end;
end;

procedure TfrMain.btCancelClick(Sender: TObject);
begin
  ConfigPanelCancel(False);
end;

procedure TfrMain.btPaintQRCodeClick(Sender: TObject);
begin
  PintarQRCode(mQRCode.Lines.Text, imgQRCode.Picture.Bitmap, qrUTF8BOM);
end;

procedure TfrMain.cbGIXAllChange(Sender: TObject);
begin
  if (cbGIXAll.Checked) then
  begin
    lbGIXParams.ClearSelection;
    lbGIXParams.Enabled := False;
    edGIXValue.Enabled := False;
  end
  else
  begin
    lbGIXParams.Enabled := True;
    edGIXValue.Enabled := True;
  end;
end;

procedure TfrMain.cbSecureChange(Sender: TObject);
begin
  pKeys.Enabled := cbSecure.Checked;
end;

procedure TfrMain.cbMNUHotKeyChange(Sender: TObject);
var
  i: Integer;
  s: String;
begin
  if cbMNUHotKey.Checked then
  begin
    for i := 0 to min(mMNU.Lines.Count-1, 8) do
    begin
      s := mMNU.Lines[i];
      if (StrToIntDef(copy(s, 1, 1), 0) < 1) then
      begin
        s := IntToStr(i+1) + ' ' + s;
        mMNU.Lines[i] := s;
      end;
    end;
  end
  else
  begin
    for i := 0 to min(mMNU.Lines.Count-1, 8) do
    begin
      s := mMNU.Lines[i];
      if (StrToIntDef(copy(s, 1, 1), 0) > 0) then
      begin
        s := Trim(copy(s, 2, Length(s)));
        mMNU.Lines[i] := s;
      end;
    end;
  end;
end;

procedure TfrMain.btCLOClick(Sender: TObject);
begin
  ACBrAbecsPinPad1.CLO(edtCLOMsg1.Text, edtCLOMsg2.Text);
end;

procedure TfrMain.btCLXClick(Sender: TObject);
begin
  if (pgCLX.ActivePageIndex = 0) then
    ACBrAbecsPinPad1.CLX(mCLX.Lines.Text)
  else
  begin
    if (lbCLXMedias.ItemIndex >= 0) then
      ACBrAbecsPinPad1.CLX(lbCLXMedias.Items[lbCLXMedias.ItemIndex]);
  end;
end;

end.

