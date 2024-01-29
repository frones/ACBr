unit FormPrincipal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Buttons, Spin, ComCtrls, Grids, ExtDlgs, ACBrAbecsPinPad;

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
    btOPN: TButton;
    btReadParms: TBitBtn;
    btRMC: TButton;
    btSaveParams: TBitBtn;
    btSearchSerialPorts: TSpeedButton;
    btSearchSerialPorts1: TSpeedButton;
    btSerial: TSpeedButton;
    cbSecure: TCheckBox;
    cbxMsgAlign: TComboBox;
    cbxPort: TComboBox;
    cbGIXAll: TCheckBox;
    edGIXValue: TEdit;
    edLogFile: TEdit;
    edMediaLoad: TEdit;
    edtCLOMsg1: TEdit;
    edtCLOMsg2: TEdit;
    edtDSPMsg1: TEdit;
    edtDSPMsg2: TEdit;
    gbCLX: TGroupBox;
    gbDSX: TGroupBox;
    gbConfig: TGroupBox;
    gbConfig1: TGroupBox;
    gbExponent: TGroupBox;
    gbModulus: TGroupBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    gbDSP: TGroupBox;
    gbGIN: TGroupBox;
    gbGIX: TGroupBox;
    ImageList1: TImageList;
    imgMedia: TImage;
    Label1: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    lbCLXMedias: TListBox;
    lbLMFMedias: TListBox;
    lbGIXParams: TListBox;
    mCLX: TMemo;
    mDEX: TMemo;
    mGIXResponse: TMemo;
    mGINResponse: TMemo;
    mExponent: TMemo;
    mLog: TMemo;
    mModulus: TMemo;
    OpenPictureDialog1: TOpenPictureDialog;
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
    pMediaLoad: TPanel;
    pMFButtons: TPanel;
    sbCleanMemoLog: TSpeedButton;
    sbGenerateKeys: TSpeedButton;
    sbMedia: TScrollBox;
    sbResponse: TStatusBar;
    sbShowLogFile: TSpeedButton;
    seLogLevel: TSpinEdit;
    seGIN_ACQIDX: TSpinEdit;
    Splitter2: TSplitter;
    TabSheet1: TTabSheet;
    tsCLOLines: TTabSheet;
    tsCLOMedia: TTabSheet;
    tsAsk: TTabSheet;
    tsDisplay: TTabSheet;
    tsCLO: TTabSheet;
    tsConfig: TTabSheet;
    tsGIX: TTabSheet;
    tsLMF: TTabSheet;
    tsOPN: TTabSheet;
    procedure ACBrAbecsPinPad1EndCommand(Sender: TObject);
    procedure ACBrAbecsPinPad1StartCommand(Sender: TObject);
    procedure ACBrAbecsPinPad1WaitForResponse(var Cancel: Boolean);
    procedure ACBrAbecsPinPad1WriteLog(const ALogLine: String;
      var Tratado: Boolean);
    procedure ApplicationProperties1Exception(Sender: TObject; E: Exception);
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
    procedure btSerialClick(Sender: TObject);
    procedure btCancelClick(Sender: TObject);
    procedure cbGIXAllChange(Sender: TObject);
    procedure cbSecureChange(Sender: TObject);
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
    function PP_StrToInt(const PP: String): Word;
  public

  end;

var
  frMain: TfrMain;

implementation

uses
  TypInfo, IniFiles,
  configuraserial,
  ACBrUtiL.FilesIO,
  ACBrUtil.Strings;

{$R *.lfm}

{ TfrMain }

procedure TfrMain.FormCreate(Sender: TObject);
var
  i: TACBrAbecsMsgAlign;
begin
  pCancelar.Visible := False;
  pCancelar.Align := alClient;
  ConfigPanelCancel(False);
  ConfigPanelsCommands(False);
  pgcCommands.ActivePageIndex := 0;
  pgCLX.ActivePageIndex := 0;
  FindSerialPorts(cbxPort.Items);

  For i := Low(TACBrAbecsMsgAlign) to High(TACBrAbecsMsgAlign) do
    cbxMsgAlign.Items.Add( GetEnumName(TypeInfo(TACBrAbecsMsgAlign), integer(I) ) ) ;

  ReadParams;
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
  SaveParams;
  ConfigACBrAbecsPinPad;
  ACBrAbecsPinPad1.IsEnabled := (btActivate.Tag = 0);
  ConfigPanelsCommands(ACBrAbecsPinPad1.IsEnabled);
  if ACBrAbecsPinPad1.IsEnabled then
    pgcCommands.ActivePageIndex := 1;
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

procedure TfrMain.btCEXClick(Sender: TObject);
begin
  ACBrAbecsPinPad1.DSP('Pressione'+#13+'alguma tecla');
  ACBrAbecsPinPad1.CEX(True,False,False,False,False);
  ShowMessage('PP_EVENT: '+ACBrAbecsPinPad1.Response.GetResponseFromTagValue(PP_EVENT) + sLineBreak +
              'PP_TRK1INC: '+ACBrAbecsPinPad1.Response.GetResponseFromTagValue(PP_TRACK1) + sLineBreak +
              'PP_TRK2INC: '+ACBrAbecsPinPad1.Response.GetResponseFromTagValue(PP_TRACK2) + sLineBreak +
              'PP_TRK3INC: '+ACBrAbecsPinPad1.Response.GetResponseFromTagValue(PP_TRACK3) );

  ACBrAbecsPinPad1.DSP('Aproxime o cartao');
  ACBrAbecsPinPad1.CEX(False,False,False,False,True);
  ShowMessage('PP_EVENT: '+ACBrAbecsPinPad1.Response.GetResponseFromTagValue(PP_EVENT) + sLineBreak +
              'PP_TRK1INC: '+ACBrAbecsPinPad1.Response.GetResponseFromTagValue(PP_TRACK1) + sLineBreak +
              'PP_TRK2INC: '+ACBrAbecsPinPad1.Response.GetResponseFromTagValue(PP_TRACK2) + sLineBreak +
              'PP_TRK3INC: '+ACBrAbecsPinPad1.Response.GetResponseFromTagValue(PP_TRACK3) );

end;

procedure TfrMain.btDEXClick(Sender: TObject);
begin
  ACBrAbecsPinPad1.DEX(mDEX.Lines.Text);
end;

procedure TfrMain.btDMFClick(Sender: TObject);
var
  sl: TStringList;
  s: String;
begin
  if (lbLMFMedias.SelCount < 1) then
    raise Exception.Create('No Media selected');

  if (MessageDlg( 'DELETE MEDIA?',
                  Format('Delete %d Media file(s)',[lbLMFMedias.SelCount]),
                  mtConfirmation,
                  mbYesNo, 0, mbNo) <> mrYes) then
    Exit;

  s := lbLMFMedias.GetSelectedText;

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
  s := ACBrAbecsPinPad1.GCD(msgDataNascimentoDDMMAAAA, 60);
  ShowMessage(s);
  s := ACBrAbecsPinPad1.GCD($0025, 5, 5);
  ShowMessage(s);
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
begin
  ACBrAbecsPinPad1.DEX('Pressione'+#13+'alguma tecla'+#13+'de função');
  i := ACBrAbecsPinPad1.GKY;
  ACBrAbecsPinPad1.DSP('');
  ShowMessage(IntToStr(i));
end;

procedure TfrMain.btMediaLoadClick(Sender: TObject);
var
  ms: TMemoryStream;
begin
  ms := TMemoryStream.Create;
  try
    imgMedia.Picture.SaveToStream(ms);
    ACBrAbecsPinPad1.LoadMedia( edMediaLoad.Text,
                                ms,
                                TACBrAbecsPinPadMediaType(imgMedia.Tag) );
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
  s := ACBrAbecsPinPad1.MNU(['1 A VISTA','2 A PRAZO','3 FIADO'],'Forma de Pagamento');
  ShowMessage(s);
end;

procedure TfrMain.btOPNClick(Sender: TObject);
begin
  if not cbSecure.Checked then
    ACBrAbecsPinPad1.OPN
  else
    ACBrAbecsPinPad1.OPN( Trim(mModulus.Text), Trim(mExponent.Text) );

  LoadMediaNames;
end;

procedure TfrMain.btRMCClick(Sender: TObject);
begin
  ACBrAbecsPinPad1.RMC('OPERAÇÃO'+#13+'TERMINADA');
end;

procedure TfrMain.btSaveParamsClick(Sender: TObject);
begin
  SaveParams;
end;

procedure TfrMain.btReadParmsClick(Sender: TObject);
begin
  ReadParams;
end;

procedure TfrMain.btSearchSerialPorts1Click(Sender: TObject);
var
  ext: String;
begin
  if OpenPictureDialog1.Execute then
  begin
    imgMedia.Picture.Clear;
    imgMedia.Picture.LoadFromFile(OpenPictureDialog1.FileName);
    ext := LowerCase(ExtractFileExt(OpenPictureDialog1.FileName));
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
  ConfigPanelCancel(True);
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

