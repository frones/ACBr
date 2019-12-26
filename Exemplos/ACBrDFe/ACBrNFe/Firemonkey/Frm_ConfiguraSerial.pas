unit Frm_ConfiguraSerial;

interface

//** Converted with Mida 600     http://www.midaconverter.com - PROJETO.ACBR

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.IniFiles,
  Data.DB,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.Objects,
  FMX.Menus,
  FMX.Grid,
  FMX.ExtCtrls,
  FMX.ListBox,
  FMX.TreeView,
  FMX.Memo,
  FMX.TabControl,
  FMX.Layouts,
  FMX.Edit,
  FMX.Platform,
  FMX.Bind.DBEngExt,
  FMX.Bind.Editors,
  FMX.Bind.DBLinks,
  FMX.Bind.Navigator,
  Data.Bind.EngExt,
  Data.Bind.Components,
  Data.Bind.DBScope,
  Data.Bind.DBLinks,
  Datasnap.DBClient,
  Fmx.Bind.Grid,
  System.Rtti,
  System.Bindings.Outputs,
  Data.Bind.Grid,
  Fmx.StdCtrls,
  FMX.Header,
  FMX.Graphics, FMX.Controls.Presentation,
  ACBrDevice, FMX.ComboEdit;

type
  TfrmConfiguraSerial = class(TForm)
    Label5: TLabel;
    cmbBaudRate: TComboBox;
    Label6: TLabel;
    cmbDataBits: TComboBox;
    Label7: TLabel;
    cmbParity: TComboBox;
    Label11: TLabel;
    cmbStopBits: TComboBox;
    Label8: TLabel;
    cmbHandShaking: TComboBox;
    Label4: TLabel;
    cmbPortaSerial: TComboEdit;
    BitBtn1: TButton;
    BitBtn2: TButton;
    chHardFlow: TCheckBox;
    chSoftFlow: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure cmbPortaSerialChange(Sender: TObject);
    procedure cmbBaudRateChange(Sender: TObject);
    procedure cmbDataBitsChange(Sender: TObject);
    procedure cmbParityChange(Sender: TObject);
    procedure cmbStopBitsChange(Sender: TObject);
    procedure cmbHandShakingChange(Sender: TObject);
    procedure chHardFlowClick(Sender: TObject);
    procedure chSoftFlowClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure VerificaFlow ;
  public
    Device : TACBrDevice ;
  end;

var
  frmConfiguraSerial: TfrmConfiguraSerial;

implementation

{$R *.FMX}

{ TfrConfiguraSerial }

procedure TfrmConfiguraSerial.FormCreate(Sender: TObject);
begin
  Device := TACBrDevice.Create(self);
end;

procedure TfrmConfiguraSerial.FormDestroy(Sender: TObject);
begin
  Device.Free ;
end;

procedure TfrmConfiguraSerial.FormShow(Sender: TObject);
begin
  cmbBaudRate.ItemIndex    := cmbBaudRate.Items.IndexOf(IntToStr( Device.Baud )) ;
  cmbDataBits.ItemIndex    := cmbDataBits.Items.IndexOf(IntToStr( Device.Data )) ;
  cmbParity.ItemIndex      := Integer( Device.Parity ) ;
  cmbStopBits.ItemIndex    := Integer( Device.Stop ) ;
  chHardFlow.IsChecked        := Device.HardFlow ;
  chSoftFlow.IsChecked        := Device.SoftFlow ;
  cmbHandShaking.ItemIndex := Integer( Device.HandShake ) ;
end;

procedure TfrmConfiguraSerial.cmbPortaSerialChange(Sender: TObject);
begin
  Device.Porta := cmbPortaSerial.Text ;
end;

procedure TfrmConfiguraSerial.cmbBaudRateChange(Sender: TObject);
begin
  if cmbBaudRate.ItemIndex >= 0 then
    Device.Baud := StrToInt(cmbBaudRate.Selected.Text) ;
end;

procedure TfrmConfiguraSerial.cmbDataBitsChange(Sender: TObject);
begin
  if cmbDataBits.ItemIndex >= 0 then
    Device.Data := StrToInt(cmbDataBits.Selected.Text) ;
end;

procedure TfrmConfiguraSerial.cmbParityChange(Sender: TObject);
begin
  if cmbParity.ItemIndex >= 0 then
    Device.Parity := TACBrSerialParity( cmbParity.ItemIndex ) ;
end;

procedure TfrmConfiguraSerial.cmbStopBitsChange(Sender: TObject);
begin
  if cmbStopBits.ItemIndex >= 0 then
    Device.Stop := TACBrSerialStop( cmbStopBits.ItemIndex ) ;
end;

procedure TfrmConfiguraSerial.cmbHandShakingChange(Sender: TObject);
begin
  if cmbHandShaking.ItemIndex >= 0 then
  begin
    Device.HandShake := TACBrHandShake( cmbHandShaking.ItemIndex ) ;
    VerificaFlow ;
  end;
end;

procedure TfrmConfiguraSerial.chHardFlowClick(Sender: TObject);
begin
  Device.HardFlow := chHardFlow.IsChecked  ;
  VerificaFlow ;
end;

procedure TfrmConfiguraSerial.chSoftFlowClick(Sender: TObject);
begin
  Device.SoftFlow := chSoftFlow.IsChecked  ;
  VerificaFlow ;
end;

procedure TfrmConfiguraSerial.VerificaFlow;
begin
  cmbHandShaking.ItemIndex := Integer( Device.HandShake ) ;
  chHardFlow.IsChecked  := Device.HardFlow ;
  chSoftFlow.IsChecked  := Device.SoftFlow ;
end;

end.

